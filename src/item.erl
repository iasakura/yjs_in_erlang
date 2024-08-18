-module(item).

-export([new_item/8, integrate/3, len/1, is_deleted/1, splice/3, is_countable/1]).
-export_type([item/0]).

-import(util, [get_item_from_link/2]).

-include("../include/item.hrl").
-include("../include/branch.hrl").
-include("../include/constants.hrl").
-include("../include/transaction.hrl").
-include("../include/id.hrl").
-include("../include/store.hrl").

-type item() :: #item{}.

-spec len(item()) -> integer().
len(Item) -> Item#item.len.

-spec new_item(
    id:id(),
    option:option(id:id()),
    option:option(id:id()),
    option:option(id:id()),
    option:option(id:id()),
    type_ptr:type_ptr(),
    option:option(binary()),
    item_content:item_content()
) -> item().
new_item(
    Id,
    Left,
    Origin,
    Right,
    RightOrigin,
    Parent,
    ParentSub,
    Content
) ->
    Info =
        case item_content:countable(Content) of
            true -> ?ITEM_FLAG_COUNTABLE;
            _ -> 0
        end,
    Len = item_content:len(Content),
    RootName =
        case Parent of
            {{named, Name}} -> Name;
            _ -> undefined
        end,
    case Content of
        {type, Branch} ->
            Content2 = Branch#branch{
                name = RootName,
                item = {ok, Id}
            },
            #item{
                id = Id,
                len = Len,
                left = Left,
                origin = Origin,
                right = Right,
                right_origin = RightOrigin,
                parent = Parent,
                parent_sub = ParentSub,
                content = {type, Content2},
                info = Info,
                redone = undefined,
                moved = undefined
            };
        _ ->
            #item{
                id = Id,
                len = Len,
                left = Left,
                origin = Origin,
                right = Right,
                right_origin = RightOrigin,
                parent = Parent,
                parent_sub = ParentSub,
                content = Content,
                info = Info,
                redone = undefined,
                moved = undefined
            }
    end.

-spec last_id(item:item()) -> id:id().
last_id(Item) -> Item#item.id#id{clock = Item#item.id#id.clock + len(Item) - 1}.

-spec integrate(item(), transaction:transaction_mut(), integer()) -> boolean().
integrate(Item, Txn, Offset) ->
    % 1. Offset > 0 なら thisを諸々調整
    % - Item.idのclockをoffsetだけ増加
    % 2. ParentをName/Idを利用しない形へ修正
    % 3. Item.leftを計算
    % 4. ParentSubをitem.left/rightから再計算
    % 5. Item.right を更新
    % - item.leftがある-> Item.left.rightに、Item.left.rightをItemに更新
    % - ない -> Item.rightをParentの最左要素に更新
    % 6. Parentのlenを更新
    % 7. move周り
    % 8. Item.contentを登録
    % 9. txn.changedに登録
    % 10. parentが削除済みかチェック
    Store = Txn#transaction_mut.store,
    Item0 =
        case Offset > 0 of
            % WIP
            true ->
                begin
                    NewItem = Item#item{
                        id = Item#item.id#id{clock = Item#item.id#id.clock + Offset},
                        left = begin
                            Id = #id{
                                client = Item#item.id#id.client,
                                clock = Item#item.id#id.clock - 1
                            },
                            Slice = block_store:get_item_clean_end(Store#store.blocks, Id),
                            I = option:map(
                                fun(S) ->
                                    I = store:materialize(Store, S),
                                    I#item.id
                                end,
                                Slice
                            ),
                            I
                        end,
                        origin = option:map(
                            fun(I) -> last_id(I) end, get_item_from_link(Store, Item#item.left)
                        ),
                        content = begin
                            {ok, {_, Content2}} = item_content:split(Item#item.content, Offset),
                            Content2
                        end,
                        len = Item#item.len - Offset
                    },
                    NewItem
                end;
            false ->
                Item
        end,
    ParentOpt =
        case Item0#item.parent of
            {branch, Branch} -> {ok, Branch};
            _ -> undefined
        end,

    Left = get_item_from_link(Store, Item0#item.left),
    Right = get_item_from_link(Store, Item0#item.right),

    RightIsNullOrHasLeft =
        case Right of
            undefined -> true;
            {ok, Right0} -> get_item_from_link(Store, Right0#item.left) =/= undefined
        end,

    LeftHasOtherRightThanSelf =
        case Left of
            undefined ->
                false;
            {ok, Left0} ->
                get_item_from_link(Store, Left0#item.right) =/=
                    get_item_from_link(Store, Item#item.right)
        end,

    case ParentOpt of
        {ok, Parent} ->
            Item1 =
                if
                    (Left =/= undefined band RightIsNullOrHasLeft) orelse LeftHasOtherRightThanSelf ->
                        Left = compute_left(Store, Parent, Item0, Left, Right),
                        Item#item{left = option:map(fun(L) -> L#item.id end, Left)};
                    true ->
                        Item0
                end,
            Item2 = tweak_parent_sub(Store, Item1),
            reconnect_left_right(Store, Parent, Item2),
            adjust_length_of_parent(Store, Parent, Item2),
            % WIP: moved https://github.com/y-crdt/y-crdt/blob/04d82e86fec64cce0d363c2b93dd1310de05b9a1/yrs/src/block.rs#L678-L703

            integrate_content(Txn, Item2),
            transaction:add_changed_type(Txn, Parent, Item2#item.parent_sub),
            % WIP: is_linked(): https://github.com/y-crdt/y-crdt/blob/04d82e86fec64cce0d363c2b93dd1310de05b9a1/yrs/src/block.rs#L743-L750
            check_deleted(Store, Parent, Item2);
        _ ->
            false
    end.

-spec compute_left(
    store:store(),
    branch:branch(),
    item:item(),
    option:option(item:item()),
    option:option(item:item())
) -> option:option(item:item()).
compute_left(Store, ParentRef, This, Left, Right) ->
    O =
        case Left of
            {ok, Left0} ->
                get_item_from_link(Store, Left0#item.right);
            _ ->
                case This#item.parent_sub of
                    {ok, Sub} ->
                        Loop0 = fun Loop(O0) ->
                            case O0 of
                                {ok, This} ->
                                    case get_item_from_link(Store, This#item.left) of
                                        {ok, L} -> Loop({ok, L});
                                        undefined -> O0
                                    end;
                                undefined ->
                                    O0
                            end
                        end,
                        Loop0(maps:get(Sub, ParentRef#branch.map, undefined));
                    _ ->
                        get_item_from_link(Store, ParentRef#branch.start)
                end
        end,

    Loop1 = fun Loop(C, CurLeft, ItemBeforeOrigin, ConflictingItems) ->
        case C of
            undefined ->
                CurLeft;
            {ok, Item0} ->
                case This#item.id == option:map(fun(R) -> R#item.id end, Right) of
                    true ->
                        {ok, Item0};
                    false ->
                        ItemBeforeOrigin0 = sets:add_element(Item0#item.id, ItemBeforeOrigin),
                        ConflictingItems0 = sets:add_element(Item0#item.id, ConflictingItems),
                        if
                            % case 1.
                            This#item.origin == Item0#item.origin ->
                                if
                                    Item0#item.id#id.clock < This#item.id#id.clock ->
                                        Loop(
                                            get_item_from_link(Store, Item0#item.right),
                                            Item0,
                                            ItemBeforeOrigin0,
                                            #{}
                                        );
                                    This#item.right_origin == Item0#item.right_origin ->
                                        CurLeft;
                                    true ->
                                        Loop(
                                            get_item_from_link(Store, Item0#item.right),
                                            CurLeft,
                                            ItemBeforeOrigin0,
                                            ConflictingItems0
                                        )
                                end;
                            true ->
                                case get_item_from_link(Store, Item0#item.origin) of
                                    {ok, Origin} ->
                                        case sets:is_element(Origin#item.id, ItemBeforeOrigin0) of
                                            true ->
                                                case
                                                    not sets:is_element(
                                                        Origin#item.id, ConflictingItems0
                                                    )
                                                of
                                                    true ->
                                                        Loop(
                                                            get_item_from_link(
                                                                Store, Item0#item.right
                                                            ),
                                                            Item0,
                                                            ItemBeforeOrigin0,
                                                            #{}
                                                        );
                                                    false ->
                                                        CurLeft
                                                end;
                                            _ ->
                                                CurLeft
                                        end
                                end
                        end
                end
        end
    end,
    Loop1(O, Left, #{}, #{}).

-spec tweak_parent_sub(
    store:store(),
    item:item()
) -> item().
tweak_parent_sub(Store, Item) ->
    case Item#item.parent_sub of
        {ok, _} ->
            Item;
        _ ->
            case get_item_from_link(Store, Item#item.left) of
                {ok, ItemLeft} ->
                    case ItemLeft#item.parent_sub of
                        {ok, Sub} ->
                            Item#item{parent_sub = {ok, Sub}};
                        _ ->
                            case get_item_from_link(Store, Item#item.right) of
                                {ok, ItemRight} ->
                                    case ItemRight#item.parent_sub of
                                        {ok, Sub} -> Item#item{parent_sub = {ok, Sub}};
                                        _ -> Item
                                    end;
                                _ ->
                                    Item
                            end
                    end;
                _ ->
                    Item
            end
    end.

-spec reconnect_left_right(
    store:store(),
    branch:branch(),
    item:item()
) -> true.
reconnect_left_right(Store, Parent, This) ->
    case get_item_from_link(Store, This#item.left) of
        {ok, Left} ->
            store:put_item(
                Store,
                This#item{right = Left#item.right}
            ),
            store:put_item(
                Store,
                Left#item{right = {ok, This#item.id}}
            ),
            true;
        _ ->
            R =
                case This#item.parent_sub of
                    {ok, ParentSub} ->
                        Loop = fun Loop(CurR) ->
                            case CurR of
                                {ok, Item} ->
                                    case get_item_from_link(Store, Item#item.left) of
                                        {ok, L} ->
                                            Loop({ok, L});
                                        _ ->
                                            CurR
                                    end;
                                _ ->
                                    CurR
                            end
                        end,
                        Loop(maps:get(ParentSub, Parent#branch.map, undefined));
                    _ ->
                        Start = Parent#branch.start,
                        store:put_branch(Store, Parent#branch{start = {ok, This#item.id}}),
                        Start
                end,
            store:put_item(Store, This#item{right = R}),
            true
    end,
    case get_item_from_link(Store, This#item.right) of
        {ok, Right} ->
            store:put_item(
                Store,
                Right#item{left = {ok, This#item.id}}
            ),
            true;
        _ ->
            case This#item.parent_sub of
                {ok, ParentSubKey} ->
                    store:put_branch(
                        Store,
                        Parent#branch{map = maps:put(ParentSubKey, This#item.id, Parent#branch.map)}
                    ),
                    case get_item_from_link(Store, This#item.left) of
                        {ok, _Left2} ->
                            % WIP: support `weak` feature
                            % WIP: txn.delete(Left)
                            true;
                        _ ->
                            true
                    end;
                _ ->
                    true
            end
    end.

-spec is_deleted(item:item()) -> boolean().
is_deleted(Item) ->
    case Item#item.info band ?ITEM_FLAG_DELETED of
        0 -> false;
        _ -> true
    end.

-spec is_countable(item:item()) -> boolean().
is_countable(Item) ->
    case Item#item.info band ?ITEM_FLAG_COUNTABLE of
        0 -> false;
        _ -> true
    end.
-spec content_len(item:item()) -> integer().
content_len(Item) -> item_content:len(Item#item.content).

-spec adjust_length_of_parent(
    store:store(),
    branch:branch(),
    item:item()
) -> true.
adjust_length_of_parent(Store, Parent, This) ->
    case option:is_none(This#item.parent_sub) and is_deleted(This) of
        true ->
            case is_countable(This) of
                true ->
                    BlockLen = Parent#branch.block_len + This#item.len,
                    ContentLen = Parent#branch.content_len + content_len(This),
                    store:put_branch(Store, Parent#branch{
                        block_len = BlockLen, content_len = ContentLen
                    }),
                    true;
                _ ->
                    % WIP: support `weak` feature
                    true
            end;
        _ ->
            true
    end.

-spec integrate_content(transaction:transaction_mut(), item:item()) -> true.
integrate_content(TransactionMut, Item) ->
    ItemContent = Item#item.content,
    Store = TransactionMut#transaction_mut.store,
    case ItemContent of
        {deleted, Len} ->
            id_set:insert(TransactionMut#transaction_mut.delete_set, Item#item.id, Len),
            store:put_item(Store, Item#item{info = Item#item.info bor ?ITEM_FLAG_DELETED}),
            true;
        % wip: { type, Move }
        % wip: { type, Doc }
        {format, _} ->
            true;
        {type, Branch} ->
            _Ptr =
                case Item#item.info band ?ITEM_FLAG_DELETED of
                    0 -> store:put_branch(Store, Branch);
                    _ -> Branch
                end,
            true;
        _ ->
            true
    end.

-spec check_deleted(store:store(), branch:branch(), item:item()) -> boolean().
check_deleted(Store, Parent, Item) ->
    ParentDeleted =
        case Parent of
            {branch, Branch} ->
                case get_item_from_link(Store, Branch#branch.item) of
                    {ok, Item} -> is_deleted(Item);
                    _ -> false
                end;
            _ ->
                false
        end,
    ParentDeleted orelse
        (Item#item.parent_sub =/= undefined andalso
            option:is_some(get_item_from_link(Store, Item#item.right))).

% TODO: OffsetKind
-spec splice(store:store(), item(), integer()) -> option:option({item(), item()}).
splice(Store, Item, Offset) ->
    case Offset =:= 0 of
        true ->
            undefined;
        false ->
            Client = Item#item.id#id.client,
            Clock = Item#item.id#id.clock,
            {ok, {Content1, Content2}} = item_content:split(Item#item.content, Offset),
            New2 = Item#item{
                id = #id{client = Client, clock = Clock + Offset},
                len = item_content:len(Content2),
                content = Content2,
                left = {ok, Item#item.id}
            },
            New1 = Item#item{
                content = Content1,
                right = {ok, New2#item.id}
            },
            store:put_item(Store, New1),
            store:put_item(Store, New2),
            case Item#item.parent_sub of
                undefined ->
                    true;
                {ok, Sub} ->
                    case Item#item.right of
                        {ok, _} ->
                            true;
                        undefined ->
                            case Item#item.parent of
                                {branch, Branch} ->
                                    store:put_branch(Store, Branch#branch{
                                        map = maps:put(Sub, New2#item.id, Branch#branch.map)
                                    });
                                _ ->
                                    true
                            end
                    end
            end,
            {ok, {New1, New2}}
    end.

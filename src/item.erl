-module(item).

-export([new_item/8, integrate/3, length/1]).
-export_type([item/0]).

-include("../include/item.hrl").
-include("../include/branch.hrl").
-include("../include/constants.hrl").
-include("../include/transaction.hrl").
-include("../include/id.hrl").

-type item() :: #item{}.

-spec length(item()) -> integer().
length(Item) -> Item#item.len.

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

-spec get_item_from_link(store:store(), option:option(id:id())) -> option:option(item:item()).
get_item_from_link(Store, Link) ->
    case Link of
        {ok, Id} -> store:get_item(Store, Id);
        _ -> undefined
    end.

-spec integrate(item(), transaction:transaction_mut(), integer()) -> item().
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
            true -> Item;
            false -> Item
        end,
    Parent =
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

    Item1 =
        case Parent of
            {ok, ParentRef} ->
                if
                    (Left =/= undefined band -RightIsNullOrHasLeft) bor LeftHasOtherRightThanSelf ->
                        Left = compute_left(Store, ParentRef, Item0, Left, Right),
                        Item#item{left = option:map(fun(L) -> L#item.id end, Left)};
                    true ->
                        Item
                end
        end,

    % if this.parent_sub.is_none() {
    %     if let Some(item) = this.left.as_deref() {
    %         if item.parent_sub.is_some() {
    %             this.parent_sub = item.parent_sub.clone();
    %         } else if let Some(item) = this.right.as_deref() {
    %              this.parent_sub = item.parent_sub.clone();
    %         }
    %     }
    % }
    Item2 = tweak_parent_sub(Store, Item1),

    undefined.

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
    Branch:branch(),
    item:item()
) -> item().
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
            );
        _ ->
            R = case This#item.parent_sub of
                {ok, ParentSub} ->
                    fun Loop(CurR) ->
                        case CurR of
                            {ok, Item} ->
                                case get_item_from_link(Store, Item#item.left) of
                                    {ok, L} ->
                                        Loop({ok, L});
                                    _ ->
                                        CurR
                                end;
                            _ -> CurR
                        end
                    end,
                    Loop(maps:get(ParentSub, Parent#branch.map, undefined));
                _ ->
                    
            end,
    end.

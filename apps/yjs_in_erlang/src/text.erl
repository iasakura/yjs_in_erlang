-module(text).

-include_lib("kernel/include/logger.hrl").
-include("../include/records.hrl").
-include("../include/constants.hrl").

-export([get_string/1, insert/5, delete/4]).
-export_type([y_text/0]).

-type y_text() :: #y_text{}.

-spec get_string(y_text()) -> binary().
get_string(#y_text{store = Store, key = Key}) ->
    {ok, Branch} = store:get_branch(Store, Key),
    get_text(Branch#branch.start, Store, <<"">>).

-spec get_text(option:option(item_ptr:item_ptr()), store:store(), binary()) -> binary().
get_text(Start, Store, Acc) ->
    case Start of
        undefined ->
            Acc;
        {ok, S} ->
            case item_ptr:get_view(S) of
                undefined ->
                    Acc;
                {ok, Item} ->
                    begin
                        NewAcc =
                            case item:is_deleted(Item) of
                                false ->
                                    case Item#item.content of
                                        {string, Str} ->
                                            <<Acc/binary, Str/binary>>;
                                        _ ->
                                            Acc
                                    end;
                                true ->
                                    Acc
                            end,
                        get_text(Item#item.right, Store, NewAcc)
                    end
            end
    end.

-spec insert(
    transaction:transaction_mut(),
    id:id(),
    y_text(),
    integer(),
    binary()
) ->
    ok | error.
insert(_, _, _, _, <<>>) ->
    ok;
insert(Txn, #id{client = Id, clock = Clock}, #y_text{key = Key}, Pos, Str) ->
    {ok, Branch} = store:get_branch(transaction:get_store(Txn), Key),
    Start = Branch#branch.start,
    {ok, Origin} =
        case Pos of
            0 -> {ok, undefined};
            _ -> get_id_from_pos(undefined, Start, Pos - 1)
        end,
    ?LOG_DEBUG("Origin: ~p", [Origin]),
    RightOrigin =
        case util:get_item_from_link(transaction:get_store(Txn), Origin) of
            undefined -> option:map(fun(Ptr) -> item_ptr:get_id(Ptr) end, Start);
            {ok, Item} -> option:map(fun(Ptr) -> item_ptr:get_id(Ptr) end, Item#item.right)
        end,
    Item2 = #item{
        id = #id{client = Id, clock = Clock},
        len = byte_size(Str),
        left = undefined,
        right = undefined,
        origin = Origin,
        right_origin = RightOrigin,
        content = {string, Str},
        parent =
            case is_binary(Key) of
                true -> {named, Key};
                false -> {id, Key}
            end,
        redone = undefined,
        parent_sub = undefined,
        moved = undefined,
        info = ?ITEM_FLAG_COUNTABLE
    },
    % eqwalizer:ignore unbound rec update
    Update = #update{
        update_blocks = #{
            Id => [
                {item, Item2}
            ]
        },
        delete_set = #{}
    },
    transaction:apply_update(Txn, Update),
    ok.

-spec delete(
    transaction:transaction_mut(),
    y_text(),
    integer(),
    integer()
) ->
    ok | error.
delete(_, _, _, 0) ->
    ok;
delete(Txn, #y_text{key = Key}, Pos, Len) ->
    {ok, Branch} = store:get_branch(transaction:get_store(Txn), Key),
    ?LOG_DEBUG("Branch: ~p", [Branch]),
    {ok, IdRange} = get_id_set_from_range(
        transaction:get_store(Txn),
        undefined,
        Branch#branch.start,
        Pos,
        Len
    ),
    % eqwalizer:ignore unbound rec update
    Update = #update{
        update_blocks = #{},
        delete_set = IdRange
    },
    transaction:apply_update(Txn, Update).

-spec get_id_from_pos(option:option(id:id()), option:option(item_ptr:item_ptr()), integer()) ->
    option:option(id:id()).
get_id_from_pos(Prev, Start, Pos) ->
    ?LOG_DEBUG("get_id_from_pos: ~p, ~p, ~p", [Prev, Start, Pos]),
    case Pos of
        0 ->
            option:map(fun(S) -> item_ptr:get_id(S) end, Start);
        _ ->
            case Start of
                undefined ->
                    undefined;
                {ok, S} ->
                    case item_ptr:get_view(S) of
                        undefined ->
                            throw("unreachable");
                        {ok, Item} ->
                            ?LOG_DEBUG("Item: ~p", [Item]),
                            case item:is_deleted(Item) of
                                false ->
                                    case item:len(Item) > Pos of
                                        true ->
                                            {ok, #id{
                                                client = Item#item.id#id.client,
                                                clock = Item#item.id#id.clock + Pos
                                            }};
                                        false ->
                                            get_id_from_pos(
                                                {ok, Item#item.id},
                                                Item#item.right,
                                                Pos - item:len(Item)
                                            )
                                    end;
                                true ->
                                    get_id_from_pos(Prev, Item#item.right, Pos)
                            end
                    end
            end
    end.

-spec get_id_set_from_range(
    store:store(),
    option:option(id:id()),
    option:option(item_ptr:item_ptr()),
    integer(),
    integer()
) ->
    option:option(id_set:id_set()).
get_id_set_from_range(Store, Prev, Start, Pos, Len) ->
    ?LOG_DEBUG("get_id_set_from_range: ~p, ~p, ~p", [Prev, Start, Pos, Len]),
    case get_id_from_pos(Prev, Start, Pos) of
        undefined ->
            undefined;
        {ok, Id} ->
            get_id_set({ok, item_ptr:new(Store, Id)}, Len, id_set:new())
    end.

-spec get_id_set(option:option(item_ptr:item_ptr()), integer(), id_set:id_set()) ->
    option:option(id_set:id_set()).
get_id_set(ItemPtr, Len, IdSet) ->
    case ItemPtr of
        undefined ->
            case Len of
                0 ->
                    {ok, IdSet};
                _ ->
                    undefined
            end;
        {ok, ItemPtr2} ->
            case item_ptr:get_view(ItemPtr2) of
                undefined ->
                    throw("unreachable");
                {ok, Item} ->
                    case item:is_deleted(Item) of
                        true ->
                            get_id_set(Item#item.right, Len, IdSet);
                        false ->
                            case item:len(Item) > Len of
                                true ->
                                    {ok, id_set:insert(IdSet, Item#item.id, Len)};
                                false ->
                                    get_id_set(
                                        Item#item.right,
                                        Len - item:len(Item),
                                        id_set:insert(IdSet, Item#item.id, Len)
                                    )
                            end
                    end
            end
    end.
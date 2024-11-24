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
    ?LOG_DEBUG("Start: ~p", [Start]),
    {ok, Origin} =
        case Pos of
            0 ->
                {ok, undefined};
            _ ->
                case get_id_from_pos(Start, Pos - 1) of
                    undefined -> undefined;
                    {ok, Id2} -> {ok, {ok, Id2}}
                end
        end,
    ?LOG_DEBUG("Origin: ~p", [Origin]),
    case Origin of
        undefined ->
            ok;
        {ok, OriginId_} ->
            ?LOG_DEBUG("Origin ~p, Origin item: ~p", [
                OriginId_, store:get_item(transaction:get_store(Txn), OriginId_)
            ])
    end,
    RightOrigin = get_id_from_pos(Start, Pos),
    ?LOG_DEBUG("RightOrigin: ~p", [RightOrigin]),
    case RightOrigin of
        undefined ->
            ok;
        {ok, RightOriginId} ->
            ?LOG_DEBUG("RightOrigin ~p, RightOrigin item: ~p", [
                RightOriginId, store:get_item(transaction:get_store(Txn), RightOriginId)
            ])
    end,
    case Origin of
        undefined ->
            ok;
        {ok, OriginId} ->
            ?LOG_DEBUG("Origin item: ~p", [store:get_item(transaction:get_store(Txn), OriginId)])
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
    {ok, IdSet} = get_id_set_from_range(
        transaction:get_store(Txn),
        Branch#branch.start,
        Pos,
        Len
    ),
    ?LOG_DEBUG("IdSet: ~p", [IdSet]),
    % case IdSet of
    %     undefined ->
    %         ok;
    %     {ok, IdRange} ->
    %         maps:foreach(fun (Client, Range) ->
    %             ?LOG_DEBUG("Client: ~p, Range: ~p", [Client, Range]),

    %         end, IdSet)
    %     end,
    % eqwalizer:ignore unbound rec update
    Update = #update{
        update_blocks = #{},
        delete_set = IdSet
    },
    transaction:apply_update(Txn, Update).

-spec get_id_from_pos(option:option(item_ptr:item_ptr()), integer()) ->
    option:option(id:id()).
get_id_from_pos(Start, Pos) ->
    ?LOG_DEBUG("get_id_from_pos: ~p, ~p", [Start, Pos]),
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
                                        Item#item.right,
                                        Pos - item:len(Item)
                                    )
                            end;
                        true ->
                            get_id_from_pos(Item#item.right, Pos)
                    end
            end
    end.

-spec get_id_set_from_range(
    store:store(),
    option:option(item_ptr:item_ptr()),
    integer(),
    integer()
) ->
    option:option(id_set:id_set()).
get_id_set_from_range(Store, Start, Pos, Len) ->
    ?LOG_DEBUG("get_id_set_from_range: ~p, ~p, ~p", [Start, Pos, Len]),
    case get_id_from_pos(Start, Pos) of
        undefined ->
            undefined;
        {ok, Id} ->
            ?LOG_DEBUG("Id: ~p", [Id]),
            get_id_set({ok, item_ptr:new(Store, Id)}, Len, id_set:new())
    end.

-spec get_id_set(option:option(item_ptr:item_ptr()), integer(), id_set:id_set()) ->
    option:option(id_set:id_set()).
get_id_set(ItemPtr, Len, IdSet) ->
    ?LOG_DEBUG("get_id_set: ~p, ~p, ~p", [ItemPtr, Len, IdSet]),
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
                            Offset = (item_ptr:get_id(ItemPtr2))#id.clock - Item#item.id#id.clock,
                            case item:len(Item) > Len + Offset of
                                true ->
                                    {ok, id_set:insert(IdSet, item_ptr:get_id(ItemPtr2), Len)};
                                false ->
                                    get_id_set(
                                        Item#item.right,
                                        Len - item:len(Item) + Offset,
                                        id_set:insert(
                                            IdSet,
                                            item_ptr:get_id(ItemPtr2),
                                            item:len(Item) - Offset
                                        )
                                    )
                            end
                    end
            end
    end.

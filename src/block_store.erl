-module(block_store).

-export([
    new/0,
    put_item/2,
    get/2,
    get_item_clean_end/2,
    get_clock/2,
    get_client/2,
    find_pivot/2,
    get_state_vector/1
]).
-export_type([block_store/0, client_block_list/0]).

-include("../include/records.hrl").

-opaque block_store() :: ets:table().
-opaque client_block_list() :: ets:table().

-spec new() -> block_store().
new() -> ets:new(block_store, [set, {keypos, #block_store_item.client}]).

-spec add_client(block_store(), state_vector:client_id()) -> client_block_list().
add_client(BlockStore, Client) ->
    Table = ets:new(client_block_list, [set, {keypos, #client_block.start}]),
    ets:insert(BlockStore, #block_store_item{
        client = Client, table = Table
    }),
    Table.

-spec get_client(block_store(), state_vector:client_id()) -> option:option(client_block_list()).
get_client(BlockStore, Client) ->
    case ets:lookup(BlockStore, Client) of
        [] -> undefined;
        [{_, Table}] -> {ok, Table}
    end.

-spec get(block_store(), id:id()) -> option:option(block:block_cell()).
get(BlockStore, #id{client = Client} = Key) ->
    case ets:lookup(BlockStore, Client) of
        [] ->
            undefined;
        [{_, ClientBlockList}] ->
            case ets:lookup(ClientBlockList, Key) of
                [] -> undefined;
                [{_, Block}] -> {ok, Block}
            end
    end.

-spec get_item(block_store(), id:id()) -> option:option(item:item()).
get_item(Store, Id) ->
    case get(Store, Id) of
        {ok, {item, Item}} -> {ok, Item};
        undefined -> undefined
    end.

-spec put_item(block_store(), item:item()) -> true.
put_item(BlockStore, Item) ->
    #id{client = ClientId} = Item#item.id,
    Table =
        case ets:lookup(BlockStore, ClientId) of
            [{_, T}] ->
                T;
            [] ->
                add_client(BlockStore, ClientId)
        end,
    ets:insert(Table, Item).

-spec get_item_clean_end(block_store(), id:id()) -> option:option(item_slice:item_slice()).
get_item_clean_end(Store, Id) ->
    maybe
        {ok, Item} ?= get_item(Store, Id),
        BlockId = Item#item.id,
        Offset = Id#id.clock - BlockId#id.clock,
        {ok, #item_slice{item = Item, start = 0, end_ = Offset}}
    else
        _ -> undefined
    end.

-spec get_clock(block_store(), state_vector:client_id()) -> integer().
get_clock(BlockStore, Client) ->
    case ets:lookup(BlockStore, Client) of
        [{_, Table}] ->
            case ets:last(Table) of
                '$end_of_table' -> 0;
                Key -> Key
            end
    end.

-spec find_pivot(client_block_list(), integer()) -> option:option({integer(), block:block_cell()}).
find_pivot(Table, Clock) ->
    case ets:prev(Table, {Clock + 1}) of
        Key when is_integer(Key) ->
            case ets:lookup(Table, Key) of
                [] ->
                    throw("unreachable");
                [#client_block{cell = Block}] ->
                    case block:id_range(Block) of
                        {Start, Len} when Start =< Clock, Clock < Start + Len ->
                            {ok, {Start, Block}};
                        _ ->
                            undefined
                    end
            end;
        _ ->
            undefined
    end.

-spec get_state_vector(block_store()) -> state_vector:state_vector().
get_state_vector(BlockStore) ->
    ets:foldl(
        fun({ClientId, Table}, Acc) ->
            Clock =
                case ets:last(Table) of
                    '$end_of_table' -> 0;
                    Key -> Key
                end,
            maps:put(ClientId, Clock, Acc)
        end,
        state_vector:new(),
        BlockStore
    ).

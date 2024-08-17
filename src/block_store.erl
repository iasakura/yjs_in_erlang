-module(block_store).

-export([new/0, put_item/2, get/2, get_item_clean_end/2, get_clock/2, get_client/2, find_pivot/2]).
-export_type([block_store/0, client_block_list/0]).

-include("../include/records.hrl").

-opaque block_store() :: ets:table().
% -type client_block_list() :: #client_block_list{}.
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
        [{_, Table}] -> ets:foldl(fun(Item, Acc) -> max(Item#item.id#id.clock, Acc) end, 0, Table)
    end.

-spec find_pivot(client_block_list(), integer()) -> option:option({integer(), block:block_cell()}).
find_pivot(Table, Clock) ->
    case ets:next(Table, {Clock - 1}) of
        Key when is_integer(Key) ->
            case ets:lookup(Table, Key) of
                [] -> throw("unreachable");
                [#client_block{start = Start, cell = Block}] -> {ok, {Start, Block}};
                _ -> throw("y_erl invariant is broken: more than one item has the same clock")
            end;
        _ ->
            undefined
    end.

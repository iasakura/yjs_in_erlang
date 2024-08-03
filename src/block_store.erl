-module(block_store).

-export([new/0, put_item/2, get/2, get_item_clean_end/2]).
-export_type([block_store/0, item_slice/0]).

-include("../include/block_store.hrl").
-include("../include/id.hrl").
-include("../include/item.hrl").

-opaque block_store() :: ets:table().
% -type client_block_list() :: #client_block_list{}.
-type client_block_list() :: ets:table().
-type item_slice() :: #item_slice{}.

-spec new() -> block_store().
new() -> ets:new(block_store, [set, {keypos, #block_store_item.client}]).

-spec add_client(block_store(), state_vector:client_id()) -> client_block_list().
add_client(BlockStore, Client) ->
    Table = ets:new(client_block_list, [set, {keypos, #client_block.start}]),
    ets:insert(BlockStore, #block_store_item{
        client = Client, table = ets:new(client_block_list, [set])
    }),
    Table.

-spec get(block_store(), id:id()) -> option:option(block:block_cell()).
get(BlockStore, #id{client = Client} = Key) ->
    case ets:lookup(BlockStore, Client) of
        [] ->
            undefined;
        [{_, ClientBlockList}] ->
            case ets:lookup(ClientBlockList, Key) of
                [] -> undefined;
                [{_, Block}] -> Block
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

-spec get_item_clean_end(block_store(), id:id()) -> option:option(item_slice()).
get_item_clean_end(Store, Id) ->
    maybe
        {ok, Item} ?= get_item(Store, Id),
        BlockId = Item#item.id,
        Offset = Id#id.clock - BlockId#id.clock,
        {ok, #item_slice{item = Item, start = 0, end_ = Offset}}
    else
        _ -> undefined
    end.

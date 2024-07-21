-module(block_store).

-export([new/0, put_item/2, get/2]).
-export_type([block_store/0]).

-include("../include/block_store.hrl").
-include("../include/id.hrl").
-include("../include/item.hrl").

-opaque block_store() :: ets:table().
% -type client_block_list() :: #client_block_list{}.
-type client_block_list() :: ets:table().

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

-spec put_item(block_store(), item:item()) -> block_store().
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

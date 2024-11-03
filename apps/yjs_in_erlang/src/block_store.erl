-module(block_store).

-export([
    new/1,
    put_item/2,
    get/2,
    get_item_clean_start/2,
    get_item_clean_end/2,
    get_clock/2,
    get_client/2,
    find_pivot/2,
    get_state_vector/1,
    push_gc/2,
    get_all/1
]).
-export_type([block_store/0, client_block_list/0]).

-include("../include/records.hrl").

-record(block_store, {ets_manager :: ets_manager:ets_manager(), table :: ets:table()}).

-opaque block_store() :: #block_store{}.
-opaque client_block_list() :: ets:table().

-spec new(ets_manager:ets_manager()) -> block_store().
new(EtsManager) ->
    #block_store{
        ets_manager = EtsManager,
        table = ets_manager:new_ets(EtsManager, block_store, [
            public, ordered_set, {keypos, #block_store_item.client}
        ])
    }.

-spec add_client(block_store(), state_vector:client_id()) ->
    client_block_list().
add_client(BlockStore, Client) ->
    Table = ets_manager:new_ets(BlockStore#block_store.ets_manager, client_block_list, [
        public, ordered_set, {keypos, #client_block.start}
    ]),
    ets:insert(BlockStore#block_store.table, #block_store_item{
        client = Client, table = Table
    }),
    Table.

-spec get_client(block_store(), state_vector:client_id()) -> option:option(client_block_list()).
get_client(#block_store{table = BlockTable}, Client) ->
    case ets:lookup(BlockTable, Client) of
        [] -> undefined;
        [#block_store_item{table = Table}] -> {ok, Table}
    end.

-spec get(block_store(), id:id()) -> option:option(block:block_cell()).
get(BlockStore, #id{client = Client, clock = Clock}) ->
    case ets:lookup(BlockStore#block_store.table, Client) of
        [] ->
            undefined;
        [#block_store_item{table = ClientBlockList}] ->
            case find_pivot(ClientBlockList, Clock) of
                undefined -> undefined;
                {ok, {_, Block}} -> {ok, Block}
            end
    end.

-spec get_item(block_store(), id:id()) -> option:option(item:item()).
get_item(Store, Id) ->
    case get(Store, Id) of
        {ok, {block, Item}} -> {ok, Item};
        undefined -> undefined
    end.

-spec put_item(block_store(), item:item()) -> true.
put_item(BlockStore, Item) ->
    #id{client = ClientId} = Item#item.id,
    Table =
        case ets:lookup(BlockStore#block_store.table, ClientId) of
            [#block_store_item{table = T}] ->
                T;
            [] ->
                add_client(BlockStore, ClientId)
        end,
    ets:insert(Table, #client_block{start = Item#item.id#id.clock, cell = {block, Item}}).

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

-spec get_item_clean_start(block_store(), id:id()) -> option:option(item_slice:item_slice()).
get_item_clean_start(Store, Id) ->
    maybe
        {ok, Item} ?= get_item(Store, Id),
        BlockId = Item#item.id,
        Offset = Id#id.clock - BlockId#id.clock,
        {ok, #item_slice{item = Item, start = Offset, end_ = item:len(Item) - 1}}
    else
        _ -> undefined
    end.

-spec get_clock(block_store(), state_vector:client_id()) -> integer().
get_clock(BlockStore, Client) ->
    case ets:lookup(BlockStore#block_store.table, Client) of
        [#block_store_item{table = Table}] ->
            case ets:last(Table) of
                '$end_of_table' -> 0;
                Key -> Key
            end
    end.

-spec find_pivot(client_block_list(), integer()) -> option:option({integer(), block:block_cell()}).
find_pivot(Table, Clock) ->
    case ets:prev(Table, Clock + 1) of
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
        fun(#block_store_item{client = ClientId, table = Table}, Acc) ->
            Clock =
                case ets:last(Table) of
                    '$end_of_table' ->
                        0;
                    Key ->
                        case ets:lookup(Table, Key) of
                            [#client_block{cell = Block}] ->
                                {Start, Len} = block:id_range(Block),
                                Start + Len
                        end
                end,
            maps:put(ClientId, Clock, Acc)
        end,
        state_vector:new(),
        BlockStore#block_store.table
    ).

-spec push_gc(block_store(), update:block_range()) -> true.
push_gc(Store, Range) ->
    Id = Range#block_range.id,
    Gc = #gc{
        start = Id#id.clock,
        end_ = Id#id.clock + Range#block_range.len - 1
    },
    Table =
        case ets:lookup(Store#block_store.table, Id#id.client) of
            [#block_store_item{table = T}] ->
                T;
            [] ->
                add_client(Store, Id#id.client)
        end,
    ets:insert(Table, #client_block{start = Gc#gc.start, cell = {gc, Gc}}).

-spec get_all(block_store()) -> #{state_vector:client_id() => #{integer() => block:block_cell()}}.
get_all(BlockStore) ->
    ets:foldl(
        fun(X, Acc) ->
            #block_store_item{client = Client, table = Table} = X,
            maps:put(
                Client,
                ets:foldl(
                    fun(Y, Acc1) ->
                        #client_block{start = S, cell = BlockCell} = Y,
                        maps:put(S, BlockCell, Acc1)
                    end,
                    #{},
                    Table
                ),
                Acc
            )
        end,
        #{},
        BlockStore#block_store.table
    ).

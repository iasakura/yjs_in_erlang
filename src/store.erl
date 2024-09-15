-module(store).

-export([
    new/0,
    get_item/2,
    put_item/2,
    put_branch/2,
    materialize/2,
    delete_branch/2,
    get_state_vector/1,
    push_gc/2
]).

-export_type([store/0]).

-include("../include/records.hrl").

-type store() :: #store{}.

-spec new() -> store().
new() ->
    #store{
        types = #{},
        node_registry = node_registry:new(),
        blocks = block_store:new(),
        pending = undefined,
        pending_ds = undefined,
        subdocs = #{},
        parent = undefined,
        linked_by = #{}
    }.

-spec get_item(store(), id:id()) -> option:option(item:item()).
get_item(Store, Id) ->
    case block_store:get(Store#store.blocks, Id) of
        {ok, {item, Item}} -> {ok, Item};
        {ok, {gc, Gc}} -> throw(io_lib:format("WIP: GC: ~p", [Gc]));
        undefined -> undefined
    end.

-spec put_item(store(), item:item()) -> item:item().
put_item(Store, Item) ->
    block_store:put_item(Store#store.blocks, Item),
    Item.

-spec put_branch(store(), branch:branch()) -> branch:branch().
put_branch(Store, Branch) ->
    node_registry:put(Store#store.node_registry, Branch),
    Branch.

-spec delete_branch(store(), branch:branch()) -> branch:branch().
delete_branch(Store, Branch) ->
    node_registry:delete(Store#store.node_registry, Branch),
    Branch.

-spec materialize(store(), item_slice:item_slice()) -> item:item().
materialize(Store, Slice) ->
    Slice1 =
        case item_slice:adjacent_left(Slice) of
            true ->
                Slice;
            false ->
                case item:splice(Store, Slice#item_slice.item, Slice#item_slice.start) of
                    {ok, {_, NewItem}} ->
                        #item_slice{
                            item = NewItem,
                            start = 0,
                            end_ = Slice#item_slice.end_ - Slice#item_slice.start
                        };
                    undefined ->
                        Slice
                end
        end,
    Slice2 =
        case item_slice:adjacent_right(Slice1) of
            false ->
                case item:splice(Store, Slice#item_slice.item, item_slice:len(Slice1)) of
                    {ok, {NewItem2, _}} ->
                        #item_slice{
                            item = NewItem2,
                            start = 0,
                            end_ = Slice1#item_slice.end_ - Slice1#item_slice.start
                        };
                    undefined ->
                        Slice1
                end;
            true ->
                Slice1
        end,
    Slice2#item_slice.item.

-spec get_state_vector(store()) -> state_vector:state_vector().
get_state_vector(Store) ->
    block_store:get_state_vector(Store#store.blocks).

-spec push_gc(store(), update:block_range()) -> ok.
push_gc(Store, Range) ->
    throw("wip").

-spec repair(store(), item:item()) -> ok.
repair(Store, Item) ->
    throw("wip").
-module(store).

-export([
    new/0,
    get_item/2,
    put_item/2,
    get_branch/2,
    put_branch/2,
    materialize/2,
    delete_branch/2,
    get_state_vector/1,
    push_gc/2,
    repair/2,
    get_or_create_type/3,
    subscribe_update_v1/1
]).

-export_type([store/0]).

-include("../include/records.hrl").

-type store() :: #store{}.

-spec new() -> store().
new() ->
    EtsManager = ets_manager:new(),
    #store{
        types = types:new(EtsManager),
        node_registry = node_registry:new(EtsManager),
        blocks = block_store:new(EtsManager),
        pending = undefined,
        pending_ds = undefined,
        subdocs = #{},
        parent = undefined,
        linked_by = #{},
        ets_manager = ets_manager:new(),
        event_manager = event_manager:start_link()
    }.

-spec get_item(store(), id:id()) -> option:option(item:item()).
get_item(Store, Id) ->
    case block_store:get(Store#store.blocks, Id) of
        {ok, {block, Item}} -> {ok, Item};
        {ok, {gc, Gc}} -> throw(io_lib:format("WIP: GC: ~p", [Gc]));
        undefined -> undefined
    end.

-spec put_item(store(), item:item()) -> item:item().
put_item(Store, Item) ->
    block_store:put_item(Store#store.blocks, Item),
    Item.

-spec get_branch(store(), id:id() | binary()) -> option:option(branch:branch()).
get_branch(Store, Key) ->
    case node_registry:get_by(Store#store.node_registry, Key) of
        {ok, Branch} -> {ok, Branch};
        undefined -> undefined
    end.

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
                case item:splice(Store, Slice1#item_slice.item, item_slice:len(Slice1)) of
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

-spec push_gc(store(), update:block_range()) -> true.
push_gc(Store, Range) ->
    block_store:push_gc(Store#store.blocks, Range).

-spec repair(store(), item:item()) -> item:item().
repair(Store, Item) ->
    NewLeft =
        case Item#item.origin of
            {ok, Origin} ->
                Left = block_store:get_item_clean_end(Store#store.blocks, Origin),
                option:map(
                    fun(L) -> item_ptr:new(Store, (materialize(Store, L))#item.id) end, Left
                );
            undefined ->
                undefined
        end,
    NewRight =
        case Item#item.right_origin of
            {ok, OriginRight} ->
                Right = block_store:get_item_clean_start(Store#store.blocks, OriginRight),
                option:map(
                    fun(L) -> item_ptr:new(Store, (materialize(Store, L))#item.id) end, Right
                );
            undefined ->
                undefined
        end,
    Item0 = Item#item{left = NewLeft, right = NewRight},

    {NewParentSub, NewParent} =
        case Item0#item.parent of
            {unknown} ->
                case {Item0#item.left, Item0#item.right} of
                    {{ok, _} = Id, _} ->
                        case util:get_item_from_link(Id) of
                            undefined ->
                                {Item0#item.parent_sub, {unknown}};
                            {ok, LeftItem} ->
                                {LeftItem#item.parent_sub, LeftItem#item.parent}
                        end;
                    {_, {ok, _} = Id} ->
                        case util:get_item_from_link(Id) of
                            undefined -> {Item0#item.parent_sub, {unknown}};
                            {ok, RightItem} -> {RightItem#item.parent_sub, RightItem#item.parent}
                        end;
                    {_, _} ->
                        {Item0#item.parent_sub, {unknown}}
                end;
            {named, Name} ->
                get_or_create_type(Store, Name, {undefined}),
                {Item0#item.parent_sub, {named, Name}};
            {id, Id} ->
                case get_item(Store, Id) of
                    {ok, ParentItem} ->
                        case ParentItem#item.content of
                            {type, _} -> {ParentItem#item.parent_sub, {id, Id}};
                            {deleted, _} -> {ParentItem#item.parent_sub, {unknown}};
                            Other -> throw({"invalid parent", Id, Other})
                        end;
                    undefined ->
                        {Item0#item.parent_sub, {unknown}}
                end
        end,

    put_item(Store, Item0#item{parent = NewParent, parent_sub = NewParentSub}).

-spec put_type(store(), binary()) -> true.
put_type(Store, Name) ->
    types:put(Store#store.types, Name),
    true.

-spec get_or_create_type(store(), binary(), type_ref:type_ref()) -> types:branch().
get_or_create_type(Store, Name, TypeRef) ->
    Branch0 =
        case types:get(Store#store.types, Name) of
            {ok, _} -> node_registry:get_by(Store#store.node_registry, Name);
            undefined -> undefined
        end,
    case Branch0 of
        {ok, Branch} ->
            Branch;
        undefined ->
            Branch1 = branch:new_branch(TypeRef),
            Branch2 = Branch1#branch{name = {ok, Name}},
            put_branch(Store, Branch2),
            put_type(Store, Name),
            Branch1
    end.

-spec subscribe_update_v1(store()) -> ok.
subscribe_update_v1(Store) ->
    event_manager:subscribe(Store#store.event_manager, update_v1).

-module(store).

-export([
    new/0,
    get_item/2,
    put_item/2,
    put_branch/2,
    materialize/2,
    delete_branch/2,
    get_state_vector/1,
    push_gc/2,
    repair/2
]).

-export_type([store/0]).

-include("../include/records.hrl").

-type store() :: #store{}.

-spec new() -> store().
new() ->
    #store{
        types = types:new(),
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

-spec repair(store(), item:item()) -> ok.
repair(Store, Item) ->
    case Item#item.origin of
        {ok, Origin} ->
            Left = block_store:get_item_clean_end(Store#store.blocks, Origin),
            _ = option:map(fun(L) -> materialize(Store, L) end, Left),
            ok;
        undefined ->
            ok
    end,
    case Item#item.right_origin of
        {ok, OriginRight} ->
            Right = block_store:get_item_clean_start(Store#store.blocks, OriginRight),
            _ = option:map(fun(L) -> materialize(Store, L) end, Right),
            ok;
        undefined ->
            ok
    end,

    {NewParentSub, NewParent} =
        case Item#item.parent of
            {branch, Branch} ->
                {Item#item.parent_sub, {branch, Branch}};
            {unknown} ->
                case {Item#item.left, Item#item.right} of
                    {{ok, _} = Id, _} ->
                        case util:get_item_from_link(Store, Id) of
                            undefined -> {Item#item.parent_sub, {unknown}};
                            {ok, Item} -> {Item#item.parent_sub, Item#item.parent}
                        end;
                    {_, {ok, _} = Id} ->
                        case util:get_item_from_link(Store, Id) of
                            undefined -> {Item#item.parent_sub, {unknown}};
                            {ok, Item} -> {Item#item.parent_sub, Item#item.parent}
                        end;
                    {_, _} ->
                        {Item#item.parent_sub, {unknown}}
                end;
            {named, Name} ->
                Branch = get_or_create_type(Store, Name, {undefined}),
                {Item#item.parent_sub, {branch, Branch}};
            {id, Id} ->
                case get_item(Store, Id) of
                    {ok, Item} ->
                        case Item#item.content of
                            {type, Branch} -> {Item#item.parent_sub, {branch, Branch}};
                            {deleted, _} -> {Item#item.parent_sub, {unknown}};
                            Other -> throw({"invalid parent", Id, Other})
                        end;
                    undefined ->
                        {Item#item.parent_sub, {unknown}}
                end
        end,
    put_item(Store, Item#item{parent = NewParent, parent_sub = NewParentSub}),
    ok.

-spec put_type(store(), binary(), types:branch_ptr()) -> true.
put_type(Store, Name, Type) ->
    types:put(Store#store.types, Name, Type).

-spec get_or_create_type(store(), binary(), type_ref:type_ref()) -> types:branch_ptr().
get_or_create_type(Store, Name, TypeRef) ->
    case types:get(Store#store.types, Name) of
        {ok, Branch} ->
            Branch;
        undefined ->
            Branch0 = branch:new_branch(TypeRef),
            Branch1 = Branch0#branch{name = {ok, Name}},
            put_branch(Store, Branch1),
            put_type(Store, Name, Branch1),
            Branch1
    end.

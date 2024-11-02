-module(node_registry).

-export_type([node_registry/0]).
-export([new/1, get_by/2, put/2, delete/2, delete_by_id/2]).

-include("../include/node_registry.hrl").
-include("../include/branch.hrl").
-opaque node_registry() :: ets:table().

-spec new(ets_manager:ets_manager()) -> node_registry().
new(EtsManager) ->
    ets_manager:new_ets(EtsManager, node_registry, [
        public, ordered_set, {keypos, #node_registry_item.id}
    ]).

-spec get_by(node_registry(), binary() | id:id()) -> option:option(branch:branch()).
get_by(NodeRegistry, Name) ->
    case ets:lookup(NodeRegistry, Name) of
        [] -> undefined;
        [#node_registry_item{branch = Node}] -> {ok, Node}
    end.

-spec put(node_registry(), branch:branch()) -> true.
put(NodeRegistry, Branch) ->
    Id =
        case Branch#branch.name of
            {ok, Name} ->
                Name;
            undefined ->
                case Branch#branch.item of
                    {ok, ItemId} -> ItemId;
                    undefined -> throw("Branch must have a name or an item")
                end
        end,
    ets:insert(NodeRegistry, #node_registry_item{id = Id, branch = Branch}).

-spec delete_by_id(node_registry(), id:id() | binary()) -> true.
delete_by_id(NodeRegistry, Id) -> ets:delete(NodeRegistry, Id).

-spec delete(node_registry(), branch:branch()) -> true.
delete(NodeRegistry, Branch) ->
    Id =
        case Branch#branch.name of
            {ok, Name} ->
                Name;
            undefined ->
                case Branch#branch.item of
                    {ok, ItemId} -> ItemId;
                    undefined -> throw("Branch must have a name or an item")
                end
        end,
    delete_by_id(NodeRegistry, Id).

-module(types).

-export([
    new/0,
    put/3,
    get/2
]).
-export_type([types/0, branch_ptr/0, type_ptr/0]).

-include("../include/branch.hrl").

-type branch_ptr() :: #branch{}.

-type type_ptr() :: {unknown} | {branch, branch_ptr()} | {named, string()} | {id, id:id()}.

-type types() :: ets:table().

-record(types_entry, {name :: binary(), type :: branch_ptr()}).

-spec new() -> types().
new() -> ets:new(types, [set, {keypos, #types_entry.name}]).

-spec put(types(), binary(), branch_ptr()) -> true.
put(Types, Name, Type) -> ets:insert(Types, #types_entry{name = Name, type = Type}).

-spec get(types(), binary()) -> option:option(branch_ptr()).
get(Types, Name) ->
    case ets:lookup(Types, Name) of
        [] -> undefined;
        [{_, Entry}] -> {ok, Entry#types_entry.type}
    end.

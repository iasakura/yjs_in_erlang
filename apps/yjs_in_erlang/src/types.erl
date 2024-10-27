-module(types).

-export([
    new/0,
    put/2,
    get/2
]).
-export_type([types/0, branch/0]).

-include("../include/branch.hrl").

-type branch() :: #branch{}.

-opaque types() :: ets:table().

-record(types_entry, {name :: binary()}).

-spec new() -> types().
new() -> ets:new(types, [public, ordered_set, {keypos, #types_entry.name}]).

-spec put(types(), binary()) -> true.
put(Types, Name) -> ets:insert(Types, #types_entry{name = Name}).

-spec get(types(), binary()) -> option:option(binary()).
get(Types, Name) ->
    case ets:lookup(Types, Name) of
        [] -> undefined;
        [#types_entry{name = Name}] -> {ok, Name}
    end.

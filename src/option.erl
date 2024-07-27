-module(option).

-export([map/2, is_none/1]).
-export_type([option/1]).

-type option(T) :: {ok, T} | undefined.

-spec map(fun((T) -> U), option(T)) -> option(U).
map(F, {ok, X}) -> {ok, F(X)};
map(_, undefined) -> undefined.

-spec is_none(option(T)) -> boolean().
is_none(undefined) -> true;
is_none(_) -> false.

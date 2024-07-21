-module(option).

-export([map/2]).
-export_type([option/1]).

-type option(T) :: {ok, T} | undefined.

-spec map(fun((T) -> U), option(T)) -> option(U).
map(F, {ok, X}) -> {ok, F(X)};
map(_, undefined) -> undefined.
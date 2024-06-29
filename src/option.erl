-module(option).

-export_type([option/1]).

-type option(T) :: {ok, T} | undefined.

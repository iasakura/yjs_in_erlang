-module(fixed_int).

-export([read_u8/1]).

-spec read_u8(binary()) -> {integer(), binary()}.
read_u8(<<I:8, Rest/binary>>) -> {I, Rest}.

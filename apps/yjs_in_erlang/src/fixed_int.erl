-module(fixed_int).

-export([read_u8/1, read_f32/1, read_f64/1, read_i64/1]).

-spec read_u8(binary()) -> {integer(), binary()}.
read_u8(<<I:8, Rest/binary>>) -> {I, Rest}.

-spec read_f32(binary()) -> {float(), binary()}.
read_f32(<<Float:32/float-big, Rest/binary>>) ->
    {Float, Rest}.

-spec read_f64(binary()) -> {float(), binary()}.
read_f64(<<Float:64/float-big, Rest/binary>>) ->
    {Float, Rest}.

-spec read_i64(binary()) -> {integer(), binary()}.
read_i64(<<Int:64/integer-big, Rest/binary>>) ->
    {Int, Rest}.

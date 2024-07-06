-module(string).

-export([read_string/1, read_buf/1]).

-spec read_string(binary()) -> {binary(), binary()}.
read_string(Bin) -> read_buf(Bin).

-spec read_buf(binary()) -> {binary(), binary()}.
read_buf(Bin) ->
    {Len, RestLen} = var_int:decode_uint(Bin),
    read_exact(Len, RestLen).

-spec read_exact(non_neg_integer(), binary()) -> {binary(), binary()}.
read_exact(Len, Bin) ->
    <<Buf:Len/binary, Rest/binary>> = Bin,
    {Buf, Rest}.

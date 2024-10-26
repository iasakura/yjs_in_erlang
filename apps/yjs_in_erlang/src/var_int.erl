-module(var_int).

-export([encode_uint/1, decode_uint/1]).

-spec encode_uint(integer(), binary()) -> binary().
encode_uint(N, Acc) when N < 2#10000000 -> <<Acc/binary, N:8>>;
encode_uint(N, Acc) -> encode_uint(N bsr 7, <<Acc/binary, ((N band 2#01111111) bor 2#10000000):8>>).

-spec encode_uint(integer()) -> binary().
encode_uint(N) -> encode_uint(N, <<>>).

-spec decode_uint(binary()) -> {integer(), binary()}.
decode_uint(<<I:8, Rest/binary>>) when I < 2#10000000 -> {I, Rest};
decode_uint(<<I:8, Rest/binary>>) when I >= 2#10000000 ->
    {N, Rest1} = decode_uint(Rest),
    {(I band 2#01111111) bor (N bsl 7), Rest1}.

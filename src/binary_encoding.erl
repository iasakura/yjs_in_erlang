-module(binary_encoding).

-export([read_string/1, read_buf/1]).

-spec read_string(binary()) -> {binary(), binary()}.
read_string(Bin) ->
    {Len, B} = var_int:decode_uint(Bin),
    Loop = fun Loop(Rest, Acc) ->
        case Rest of
            0 ->
                {binary:part(B, Acc), binary:part(B, Acc, byte_size(Bin) - Acc)};
            _ ->
                CharLen =
                    case binary:at(Bin, Acc) band 16#FFFF of
                        0 ->
                            1;
                        _ ->
                            2
                    end,
                Loop(Rest - 1, Acc + CharLen)
        end
    end,
    BinaryLen = Loop(Len, 0),
    {binary:part(B, 0, BinaryLen), binary:part(B, BinaryLen)}.

-spec read_buf(binary()) -> {binary(), binary()}.
read_buf(Bin) ->
    {Len, Rest} = var_int:decode_uint(Bin),
    read_exact(Len, Rest).

-spec read_exact(non_neg_integer(), binary()) -> {binary(), binary()}.
read_exact(Len, Bin) ->
    <<Buf:Len/binary, Rest/binary>> = Bin,
    {Buf, Rest}.

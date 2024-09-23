-module(binary_encoding).

-export([read_string/1, read_buf/1]).

-spec read_string(binary()) -> {binary(), binary()}.
read_string(Bin) ->
    {Len, B} = var_int:decode_uint(Bin),
    Loop = fun Loop(Rest, Acc) ->
        case Rest of
            0 ->
                {binary:part(B, 0, Acc), binary:part(B, Acc, byte_size(B) - Acc)};
            _ ->
                Ch = binary:at(Bin, Acc),
                CharLen =
                    case Ch band 16#FFFF =:= Ch of
                        true ->
                            1;
                        false ->
                            2
                    end,
                Loop(Rest - CharLen, Acc + CharLen)
        end
    end,
    Loop(Len, 0).

-spec read_buf(binary()) -> {binary(), binary()}.
read_buf(Bin) ->
    {Len, Rest} = var_int:decode_uint(Bin),
    read_exact(Len, Rest).

-spec read_exact(non_neg_integer(), binary()) -> {binary(), binary()}.
read_exact(Len, Bin) ->
    <<Buf:Len/binary, Rest/binary>> = Bin,
    {Buf, Rest}.

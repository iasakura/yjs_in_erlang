-module(binary_encoding).

-include_lib("kernel/include/logger.hrl").

-export([read_string/1, read_buf/1]).

-spec read_string(binary()) -> {binary(), binary()}.
read_string(Bin) ->
    {Len, B} = var_int:decode_uint(Bin),
    ?LOG_DEBUG("Len: ~p, B: ~p", [Len, B]),
    Loop = fun Loop(Rest, RestLen, Acc) ->
        ?LOG_DEBUG("Rest: ~p, Acc: ~p", [Rest, Acc]),
        case RestLen of
            0 ->
                {Acc, Rest};
            _ ->
                case util:read_next_utf8_codepoint(Rest) of
                    undefined ->
                        throw({error, "invalid utf8 binary", Rest});
                    {ok, {C, L, Rest1}} ->
                        % Utf16Len =
                        %     case C >= 16#10000 of
                        %         true -> 2;
                        %         false -> 1
                        %     end,
                        Loop(
                            Rest1,
                            RestLen - L,
                            <<Acc/binary, (binary:part(Rest, 0, L))/binary>>
                        )
                end
        end
    end,
    Loop(B, Len, <<>>).

-spec read_buf(binary()) -> {binary(), binary()}.
read_buf(Bin) ->
    {Len, Rest} = var_int:decode_uint(Bin),
    read_exact(Len, Rest).

-spec read_exact(non_neg_integer(), binary()) -> {binary(), binary()}.
read_exact(Len, Bin) ->
    <<Buf:Len/binary, Rest/binary>> = Bin,
    {Buf, Rest}.

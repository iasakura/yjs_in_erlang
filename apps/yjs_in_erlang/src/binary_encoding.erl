-module(binary_encoding).

-include_lib("kernel/include/logger.hrl").

-export([decode_string/1, encode_string/1, decode_buf/1, encode_buf/1]).

% 多分いらない
-spec decode_string(binary()) -> {binary(), binary()}.
decode_string(Bin) ->
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
                    {ok, {_C, L, Rest1}} ->
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

% UTF-8バイナリのコードポイント数を取得する関数
-spec utf8_codepoint_length(binary()) -> integer().
utf8_codepoint_length(Binary) ->
    case unicode:characters_to_list(Binary) of
        % デコード成功時に文字数を取得
        Chars when is_list(Chars) -> length(Chars);
        % デコード失敗時は0を返す
        _ -> 0
    end.

% codepoint数を先頭に置く
-spec encode_string(binary()) -> binary().
encode_string(Str) ->
    <<(var_int:encode_uint(byte_size(Str)))/binary, Str/binary>>.

-spec decode_buf(binary()) -> {binary(), binary()}.
decode_buf(Bin) ->
    {Len, Rest} = var_int:decode_uint(Bin),
    decode_exact(Len, Rest).

-spec encode_buf(binary()) -> binary().
encode_buf(Bin) -> <<(var_int:encode_uint(byte_size(Bin)))/binary, Bin/binary>>.

-spec decode_exact(non_neg_integer(), binary()) -> {binary(), binary()}.
decode_exact(Len, Bin) ->
    <<Buf:Len/binary, Rest/binary>> = Bin,
    {Buf, Rest}.

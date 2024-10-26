-module(any).

-export([decode_any/1, encode_any/1]).
-export_type([any_type/0]).

-include_lib("kernel/include/logger.hrl").

-type any_type() ::
    null
    | undefined
    | {bool, boolean()}
    | {number, float()}
    | {big_int, integer()}
    | {string, binary()}
    | {buffer, binary()}
    | {array, [any_type()]}
    | {map, #{binary() => any_type()}}.

-spec decode_any(binary()) -> {any_type(), binary()}.
decode_any(Bin) ->
    {Type, Rest} = fixed_int:read_u8(Bin),
    ?LOG_DEBUG("Type: ~p", [Type]),
    case Type of
        127 ->
            {undefined, Rest};
        126 ->
            {null, Rest};
        125 ->
            {N, Rest0} = var_int:decode_uint(Rest),
            {{number, N}, Rest0};
        124 ->
            {F, Rest0} = fixed_int:read_f32(Rest),
            {{number, F}, Rest0};
        123 ->
            {F, Rest0} = fixed_int:read_f32(Rest),
            {{number, F}, Rest0};
        122 ->
            {N, Rest0} = fixed_int:read_i64(Rest),
            {{big_int, N}, Rest0};
        121 ->
            {{bool, false}, Rest};
        120 ->
            {{bool, true}, Rest};
        119 ->
            {S, Rest0} = binary_encoding:decode_string(Rest),
            {{string, S}, Rest0};
        118 ->
            {Len, Rest0} = var_int:decode_uint(Rest),
            Rec = fun Rec(N, Acc, Rest1) ->
                case N of
                    0 ->
                        {Acc, Rest1};
                    _ ->
                        {Key, Rest2} = binary_encoding:decode_string(Rest1),
                        {Item, Rest3} = decode_any(Rest2),
                        Rec(N - 1, maps:update(Key, Item, Acc), Rest3)
                end
            end,
            Rec(Len, #{}, Rest0);
        117 ->
            {Len, Rest0} = var_int:decode_uint(Rest),
            Rec = fun Rec(N, Acc, Rest1) ->
                case N of
                    0 ->
                        {lists:reverse(Acc), Rest1};
                    _ ->
                        {Item, Rest2} = decode_any(Rest1),
                        Rec(N - 1, [Item | Acc], Rest2)
                end
            end,
            Rec(Len, [], Rest0);
        116 ->
            {Buf, Rest0} = binary_encoding:decode_buf(Rest),
            {{buffer, Buf}, Rest0}
    end.

-spec encode_any(any_type()) -> binary().
encode_any(undefined) ->
    <<127>>;
encode_any(null) ->
    <<126>>;
encode_any({number, N}) when is_integer(N) ->
    <<125, (var_int:encode_uint(N))/binary>>;
encode_any({number, F}) when is_float(F) ->
    <<124, (fixed_int:write_f32(F))/binary>>;
encode_any({big_int, N}) ->
    <<122, (fixed_int:write_i64(N))/binary>>;
encode_any({bool, false}) ->
    <<121>>;
encode_any({bool, true}) ->
    <<120>>;
encode_any({string, S}) ->
    <<119, (binary_encoding:encode_string(S))/binary>>;
encode_any({map, Map}) ->
    Len = maps:size(Map),
    MapBin = [
        <<(binary_encoding:encode_string(Key))/binary, (encode_any(Value))/binary>>
     || {Key, Value} <- maps:to_list(Map)
    ],
    <<118, (var_int:encode_uint(Len))/binary, (list_to_binary(MapBin))/binary>>;
encode_any({array, Array}) ->
    Len = length(Array),
    ArrayBin = [encode_any(Value) || Value <- Array],
    <<117, (var_int:encode_uint(Len))/binary, (list_to_binary(ArrayBin))/binary>>;
encode_any({buffer, Buf}) ->
    <<116, (binary_encoding:encode_buf(Buf))/binary>>.

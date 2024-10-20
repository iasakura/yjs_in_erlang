-module(any).

-export([decode_any/1]).
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
    | {array, [any()]}
    | {map, #{binary() => any()}}.

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
            {S, Rest0} = binary_encoding:read_string(Rest),
            {{string, S}, Rest0};
        118 ->
            {Len, Rest0} = var_int:decode_uint(Rest),
            Rec = fun Rec(N, Acc, Rest1) ->
                case N of
                    0 ->
                        {Acc, Rest1};
                    _ ->
                        {Key, Rest2} = binary_encoding:read_string(Rest1),
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
            {Buf, Rest0} = binary_encoding:read_buf(Rest),
            {{buffer, Buf}, Rest0}
    end.

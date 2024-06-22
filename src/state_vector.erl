-module(state_vector).

-export_type([client_id/0, state_vector/0]).
-export([encode_state_vector/1, decode_state_vector/1]).

-type client_id() :: integer().
-type state_vector() :: #{client_id() => integer()}.

-spec encode_state_vector(state_vector()) -> binary().
encode_state_vector(SV) ->
    N = maps:size(SV),
    maps:fold(
        fun(ClientId, Clock, Acc) ->
            <<
                (var_int:encode_uint(ClientId))/binary,
                (var_int:encode_uint(Clock))/binary,
                Acc/binary
            >>
        end,
        <<(var_int:encode_uint(N))/binary>>,
        SV
    ).

-spec decode_state_vector(binary()) -> {state_vector(), binary()}.
decode_state_vector(Bin) ->
    {Len, Bin0} = var_int:decode_uint(Bin),
    Rec = fun Rec(N, Bin1, Acc) ->
        case N of
            0 ->
                {Acc, Bin1};
            _ ->
                {ClientId, Bin2} = var_int:decode_uint(Bin1),
                {Clock, Bin3} = var_int:decode_uint(Bin2),
                Rec(N - 1, Bin3, maps:put(ClientId, Clock, Acc))
        end
    end,
    Rec(Len, Bin0, #{}).

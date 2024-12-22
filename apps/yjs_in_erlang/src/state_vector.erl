-module(state_vector).

-include("../include/records.hrl").
-export_type([client_id/0, state_vector/0]).
-export([
    new/0,
    encode_client_id/1,
    encode_state_vector/1,
    decode_state_vector/1,
    set/3,
    set_min/3,
    set_max/3,
    get/2,
    contains/2,
    integer_to_client_id/1
]).

-opaque client_id() :: integer().
-type state_vector() :: #{client_id() => integer()}.

-spec new() -> state_vector().
new() -> #{}.

-spec integer_to_client_id(integer()) -> client_id().
integer_to_client_id(N) when N >= 0 -> N.

-spec encode_client_id(client_id()) -> binary().
encode_client_id(Id) -> var_int:encode_uint(Id).

-spec encode_state_vector(state_vector()) -> binary().
encode_state_vector(SV) ->
    N = maps:size(SV),
    maps:fold(
        fun(ClientId, Clock, Acc) ->
            <<
                Acc/binary,
                (var_int:encode_uint(ClientId))/binary,
                (var_int:encode_uint(Clock))/binary
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

-spec set(state_vector(), client_id(), integer()) -> state_vector().
set(SV, ClientId, Clock) ->
    maps:put(ClientId, Clock, SV).

-spec set_min(state_vector(), client_id(), integer()) -> state_vector().
set_min(SV, ClientId, Clock) ->
    maps:update_with(ClientId, fun(Clock0) -> min(Clock, Clock0) end, Clock, SV).

-spec set_max(state_vector(), client_id(), integer()) -> state_vector().
set_max(SV, ClientId, Clock) ->
    maps:update_with(ClientId, fun(Clock0) -> max(Clock, Clock0) end, Clock, SV).

-spec get(state_vector(), client_id()) -> integer().
get(SV, ClientId) ->
    maps:get(ClientId, SV, 0).

-spec contains(state_vector(), id:id()) -> boolean().
contains(SV, Id) ->
    Clock = get(SV, Id#id.client),
    Clock >= Id#id.clock.

-module(id).

-export([new/2, decode_id/1, encode_id/1]).

-export_type([id/0]).

-include("../include/id.hrl").

-type id() :: #id{}.

-spec new(state_vector:client_id(), integer()) -> id().
new(Client, Clock) ->
    #id{client = Client, clock = Clock}.

-spec decode_id(binary()) -> {id(), binary()}.
decode_id(Bin) ->
    {Client, Rest} = var_int:decode_uint(Bin),
    {Clock, Rest2} = var_int:decode_uint(Rest),
    {#id{client = state_vector:integer_to_client_id(Client), clock = Clock}, Rest2}.

-spec encode_id(id()) -> binary().
encode_id(#id{client = Client, clock = Clock}) ->
    <<(state_vector:encode_client_id(Client))/binary, (var_int:encode_uint(Clock))/binary>>.

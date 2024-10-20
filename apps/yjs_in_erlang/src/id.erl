-module(id).

-export([read_id/1]).

-export_type([id/0]).

-include("../include/id.hrl").

-type id() :: #id{}.

-spec read_id(binary()) -> {id(), binary()}.
read_id(Bin) ->
    {Client, Rest} = var_int:decode_uint(Bin),
    {Clock, Rest2} = var_int:decode_uint(Rest),
    {#id{client = state_vector:integer_to_client_id(Client), clock = Clock}, Rest2}.

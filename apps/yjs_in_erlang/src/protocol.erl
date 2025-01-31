-module(protocol).

-export([decode_sync_message/1, encode_sync_message/1]).
-export_type([sync_messages/0]).

-type sync_messages() ::
    {sync_step1, state_vector:state_vector()}
    | {sync_step2, update:update()}
    | {update, update:update()}.

-spec encode_sync_message(sync_messages()) -> binary().
encode_sync_message({sync_step1, StateVector}) ->
    SVBin = state_vector:encode_state_vector(StateVector),
    Len = byte_size(SVBin),
    <<0:8, 0:8, (var_int:encode_uint(Len))/binary, SVBin/binary>>;
encode_sync_message({sync_step2, Update}) ->
    UpdateBin = update:encode_update(Update),
    Len = byte_size(UpdateBin),
    <<0:8, 1:8, (var_int:encode_uint(Len))/binary, UpdateBin/binary>>;
encode_sync_message({update, Update}) ->
    UpdateBin = update:encode_update(Update),
    Len = byte_size(UpdateBin),
    <<0:8, 2:8, (var_int:encode_uint(Len))/binary, UpdateBin/binary>>.

-spec decode_sync_message(binary()) -> {sync_messages(), binary()}.
decode_sync_message(<<0:8, Bin/binary>>) ->
    {Len, Rest0} = var_int:decode_uint(Bin),
    Buf = binary:part(Rest0, 0, Len),
    {StateVector, _} = state_vector:decode_state_vector(Buf),
    {{sync_step1, StateVector}, binary:part(Rest0, Len, byte_size(Rest0) - Len)};
decode_sync_message(<<1:8, Bin/binary>>) ->
    {Len, Rest0} = var_int:decode_uint(Bin),
    Buf = binary:part(Rest0, 0, Len),
    {Update, _} = update:decode_update(Buf),
    {{sync_step2, Update}, binary:part(Rest0, Len, byte_size(Rest0) - Len)};
decode_sync_message(<<2:8, Bin/binary>>) ->
    {Len, Rest0} = var_int:decode_uint(Bin),
    Buf = binary:part(Rest0, 0, Len),
    {Update, _} = update:decode_update(Buf),
    {{update, Update}, binary:part(Rest0, Len, byte_size(Rest0) - Len)}.

-module(protocol).

-export([decode_sync_message/1]).
-export_type([sync_messages/0]).

-include("../include/records.hrl").

-type sync_messages() ::
    {sync_step1, state_vector:state_vector()}
    | {sync_step2, update:update()}
    | {update, update:update()}.

% -spec encode_sync_message(sync_messages()) -> binary().
% encode_sync_message({sync_step1, StateVector}) ->
%     <<0:8, (state_vector:encode_state_vector(StateVector))/binary>>;
% encode_sync_message({sync_step2, Update}) ->
%     <<1:8, update:encode_update(Update)>>;
% encode_sync_message({update, Update}) ->
%     <<2:8, update:encode_update(Update)>>.

-spec decode_sync_message(binary()) -> {sync_messages(), binary()}.
decode_sync_message(<<0:8, Bin/binary>>) ->
    {StateVector, Rest} = state_vector:decode_state_vector(Bin),
    {{sync_step1, StateVector}, Rest};
decode_sync_message(<<1:8, Bin/binary>>) ->
    {Update, Rest} = update:decode_update(Bin),
    {{sync_step2, Update}, Rest};
decode_sync_message(<<2:8, Bin/binary>>) ->
    {Update, Rest} = update:decode_update(Bin),
    {{update, Update}, Rest}.

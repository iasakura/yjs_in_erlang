-module(protocol).

-type sync_messages() ::
    {sync_step1, state_vector:state_vector()}
    | {sync_step2, update:update()}
    | {update, update:update()}.

-spec encode_sync_message(sync_messages()) -> binary().
encode_sync_message({sync_step1, StateVector}) ->
    <<0:8, (state_vector:encode_state_vector(StateVector))/binary>>;
encode_sync_message({sync_step2, Update}) ->
    <<1:8, update:encode_update(Update)>>;
encode_sync_message({update, Update}) ->
    <<2:8, update:encode_update(Update)>>.

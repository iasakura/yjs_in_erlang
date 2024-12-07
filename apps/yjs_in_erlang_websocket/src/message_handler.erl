-module(message_handler).

-export([handle_msg/2]).

-include("../include/records.hrl").

-spec handle_msg(protocol:sync_messages(), websocket_connection_manager:ws_local_state()) ->
    cowboy_websocket:commands().
handle_msg({sync_step1, StateVector}, #ws_local_state{doc = Doc}) ->
    Update = doc:get_update(Doc, StateVector),
    [{binary, protocol:encode_sync_message({sync_step2, Update})}];
handle_msg({sync_step2, Update}, #ws_local_state{manager = Manager, doc = Doc, doc_id = DocId}) ->
    Txn = transaction:new(Doc),
    transaction:apply_update(Txn, Update),
    % TODO: Avoid sending redundant updates to the same client
    % lists:foreach(fun(Client) -> Client ! {send, {sync_step2, Update}} end, OtherClients),
    websocket_connection_manager:broadcast(Manager, DocId, {send, {update, Update}}),
    [];
handle_msg({update, Update}, #ws_local_state{manager = Manager, doc = Doc, doc_id = DocId}) ->
    Txn = transaction:new(Doc),
    transaction:apply_update(Txn, Update),
    websocket_connection_manager:broadcast(Manager, DocId, {send, {update, Update}}),
    [].

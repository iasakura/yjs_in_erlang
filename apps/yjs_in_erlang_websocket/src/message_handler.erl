-module(message_handler).

-export([handle_msg/2]).

-include("../include/records.hrl").
-include_lib("kernel/include/logger.hrl").

-spec handle_msg(protocol:sync_messages(), websocket_connection_manager:ws_local_state()) ->
    cowboy_websocket:commands().
handle_msg({sync_step1, StateVector}, #ws_local_state{doc = Doc}) ->
    Update = doc:get_update(Doc, StateVector),
    [{binary, protocol:encode_sync_message({sync_step2, Update})}];
handle_msg({sync_step2, Update}, #ws_local_state{manager = Manager, doc = Doc, doc_id = DocId}) ->
    Txn = transaction:new(Doc),
    transaction:apply_update(Txn, Update),
    Clients = websocket_connection_manager:get_clients(Manager, DocId),
    OtherClients = [Client || Client <- Clients, Client /= self()],
    ?LOG_DEBUG("Send update to ~p", [OtherClients]),
    % TODO: Avoid sending redundant updates to the same client
    % lists:foreach(fun(Client) -> Client ! {send, {sync_step2, Update}} end, OtherClients),
    [];
handle_msg({update, Update}, #ws_local_state{manager = Manager, doc = Doc, doc_id = DocId}) ->
    Txn = transaction:new(Doc),
    transaction:apply_update(Txn, Update),
    Clients = websocket_connection_manager:get_clients(Manager, DocId),
    OtherClients = [Client || Client <- Clients, Client /= self()],
    ?LOG_DEBUG("Send update to ~p", [OtherClients]),
    lists:foreach(fun(Client) -> Client ! {send, {update, Update}} end, OtherClients),
    [].

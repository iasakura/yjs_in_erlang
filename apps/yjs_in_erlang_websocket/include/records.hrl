-record(ws_global_state, {
    docs :: #{binary() => websocket_connection_manager:ws_shared_doc()}
}).

-record(ws_shared_doc, {
    doc :: doc:doc(),
    clients :: [pid()]
}).

-record(ws_local_state, {
    manager :: websocket_connection_manager:ws_connection_manager(),
    doc :: doc:doc(),
    doc_id :: binary()
}).

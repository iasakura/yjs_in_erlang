-record(ws_global_state, {
    docs :: #{binary() => websocket_connection_manager:ws_shared_doc()},
    storage_module :: module()
}).

-record(ws_shared_doc, {
    doc_sup :: pid(),
    clients :: [websocket_connection_manager:client_info()]
}).

-record(client_info, {
    pid :: pid(),
    client_monitor_ref :: reference()
}).

-record(ws_local_state, {
    manager :: websocket_connection_manager:ws_connection_manager(),
    doc :: doc_server:doc(),
    monitor_ref :: reference(),
    doc_id :: binary()
}).

-module(message_handler).

-export([handle_msg/2]).

-spec handle_msg(protocol:sync_messages(), doc_server:doc()) -> [protocol:sync_messages()].
handle_msg({sync_step1, StateVector}, Doc) ->
    Update = doc_server:get_update(Doc, StateVector),
    [{sync_step2, Update}];
handle_msg({sync_step2, Update}, Doc) ->
    Txn = doc_server:new_transaction(Doc),
    transaction:apply_update(Txn, Update),
    transaction:commit(Txn),
    case doc_server:has_pendings(Doc) of
        true ->
            [{sync_step1, doc_server:get_state_vector(Doc)}];
        false ->
            []
    end;
handle_msg({update, Update}, Doc) ->
    Txn = doc_server:new_transaction(Doc),
    transaction:apply_update(Txn, Update),
    transaction:commit(Txn),
    case doc_server:has_pendings(Doc) of
        true ->
            [{sync_step1, doc_server:get_state_vector(Doc)}];
        false ->
            []
    end.

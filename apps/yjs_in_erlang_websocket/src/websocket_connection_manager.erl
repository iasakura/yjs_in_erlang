-module(websocket_connection_manager).

-export([
    new/0, get_or_create_doc/2, get_clients/2, disconnect/2
]).
-export_type([ws_connection_manager/0, ws_global_state/0, ws_shared_doc/0, ws_local_state/0]).

-include("../include/records.hrl").

-opaque ws_connection_manager() :: pid().

-type ws_global_state() :: #ws_global_state{}.

-type ws_shared_doc() :: #ws_shared_doc{}.

-type ws_local_state() :: #ws_local_state{}.

-spec new() -> ws_connection_manager().
new() ->
    spawn(fun() ->
        loop(#ws_global_state{docs = #{}})
    end).

-spec loop(ws_global_state()) -> ok.
loop(State) ->
    receive
        {From, {get_or_create_doc, DocId}} ->
            {Doc, NewState} = get_or_create_doc_impl(State, DocId, From),
            From ! {self(), Doc},
            loop(NewState);
        {From, {get_clients, DocId}} ->
            Clients = get_clients_impl(State, DocId),
            From ! {self(), Clients},
            loop(State);
        {From, {disconnect, DocId}} ->
            case maps:find(DocId, State#ws_global_state.docs) of
                error ->
                    loop(State);
                {ok, Doc} ->
                    NewDoc = Doc#ws_shared_doc{
                        clients = lists:delete(From, Doc#ws_shared_doc.clients)
                    },
                    NewState = State#ws_global_state{
                        docs = maps:put(DocId, NewDoc, State#ws_global_state.docs)
                    },
                    loop(NewState)
            end
    end.

-spec get_or_create_doc(ws_connection_manager(), binary()) -> doc:doc().
get_or_create_doc(Manager, DocId) ->
    Manager ! {self(), {get_or_create_doc, DocId}},
    receive
        {_, Doc} -> Doc
    end.

-spec get_clients(ws_connection_manager(), binary()) -> [pid()].
get_clients(Manager, DocId) ->
    Manager ! {self(), {get_clients, DocId}},
    receive
        {Manager, Clients} -> Clients
    end.

-spec get_or_create_doc_impl(ws_global_state(), binary(), pid()) -> {doc:doc(), ws_global_state()}.
get_or_create_doc_impl(State, DocId, From) ->
    case maps:find(DocId, State#ws_global_state.docs) of
        {ok, Doc} ->
            NewDoc = Doc#ws_shared_doc{clients = [From | Doc#ws_shared_doc.clients]},
            NewState = State#ws_global_state{
                docs = maps:put(DocId, NewDoc, State#ws_global_state.docs)
            },
            {NewDoc#ws_shared_doc.doc, NewState};
        error ->
            Doc = #ws_shared_doc{
                doc = doc:new(),
                clients = [From]
            },
            NewState = State#ws_global_state{
                docs = maps:put(DocId, Doc, State#ws_global_state.docs)
            },
            {Doc#ws_shared_doc.doc, NewState}
    end.

-spec get_clients_impl(ws_global_state(), binary()) -> [pid()].
get_clients_impl(State, DocId) ->
    case maps:find(DocId, State#ws_global_state.docs) of
        {ok, Doc} -> Doc#ws_shared_doc.clients;
        error -> []
    end.

-spec disconnect(ws_connection_manager(), binary()) -> [pid()].
disconnect(Manager, DocId) ->
    Manager ! {self(), {disconnect, DocId}},
    receive
        {Manager, Clients} -> Clients
    end.

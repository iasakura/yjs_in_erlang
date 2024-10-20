-module(websocket_connection_manager).

-export([
    new/0, get_or_create_doc/2
]).
-export_type([ws_connection_manager/0, ws_global_state/0, ws_shared_doc/0]).

-include("../include/records.hrl").

-opaque ws_connection_manager() :: pid().

-type ws_global_state() :: #ws_global_state{}.

-type ws_shared_doc() :: #ws_shared_doc{}.

-spec new() -> ws_connection_manager().
new() ->
    spawn(fun() ->
        loop(#ws_global_state{docs = #{}})
    end).

-spec loop(ws_global_state()) -> ok.
loop(State) ->
    receive
        {From, {get_or_create_doc, DocId}} ->
            {Doc, NewState} = get_or_create_doc_impl(State, DocId),
            From ! {self(), Doc},
            loop(NewState)
    end.

-spec get_or_create_doc(ws_connection_manager(), binary()) -> ws_shared_doc().
get_or_create_doc(Manager, DocId) ->
    Manager ! {self(), {get_or_create_doc, DocId}},
    receive
        {_, Doc} -> Doc
    end.

-spec get_or_create_doc_impl(ws_global_state(), binary()) -> {ws_shared_doc(), ws_global_state()}.
get_or_create_doc_impl(State, DocId) ->
    case maps:find(DocId, State#ws_global_state.docs) of
        {ok, Doc} ->
            NewDoc = Doc#ws_shared_doc{clients = [self() | Doc#ws_shared_doc.clients]},
            NewState = State#ws_global_state{
                docs = maps:put(DocId, NewDoc, State#ws_global_state.docs)
            },
            {NewDoc, NewState};
        error ->
            Doc = #ws_shared_doc{
                doc = doc:new(),
                clients = [self()]
            },
            NewState = State#ws_global_state{
                docs = maps:put(DocId, Doc, State#ws_global_state.docs)
            },
            {Doc, NewState}
    end.

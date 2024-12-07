-module(websocket_connection_manager).

-behaviour(gen_server).

-export([
    start_link/0, get_or_create_doc/2, get_clients/2, disconnect/2, broadcast/3
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export_type([ws_connection_manager/0, ws_global_state/0, ws_shared_doc/0, ws_local_state/0]).

-include("../include/records.hrl").
-include_lib("kernel/include/logger.hrl").

-opaque ws_connection_manager() :: pid().

-type ws_global_state() :: #ws_global_state{}.

-type ws_shared_doc() :: #ws_shared_doc{}.

-type ws_local_state() :: #ws_local_state{}.

%%% API
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_or_create_doc(ws_connection_manager(), binary()) -> doc:doc().
get_or_create_doc(Manager, DocId) ->
    gen_server:call(Manager, {get_or_create_doc, DocId}).

-spec get_clients(ws_connection_manager(), binary()) -> [pid()].
get_clients(Manager, DocId) ->
    gen_server:call(Manager, {get_clients, DocId}).

-spec broadcast(ws_connection_manager(), binary(), term()) -> ok.
broadcast(Manager, DocId, Update) ->
    gen_server:call(Manager, {broadcast, DocId, Update}).

-spec disconnect(ws_connection_manager(), binary()) -> ok.
disconnect(Manager, DocId) ->
    From = self(),
    gen_server:cast(Manager, {disconnect, DocId, From}).

%%% gen_server callbacks
-spec init([]) -> {ok, ws_global_state()}.
init([]) ->
    {ok, #ws_global_state{docs = #{}}}.

-spec handle_call(tuple(), {pid(), term()}, ws_global_state()) ->
    {reply, term(), ws_global_state()}.
handle_call({get_or_create_doc, DocId}, {From, _}, State) ->
    {Doc, NewState} = get_or_create_doc_impl(State, DocId, From),
    {reply, Doc, NewState};
handle_call({get_clients, DocId}, _From, State) ->
    Clients = get_clients_impl(State, DocId),
    {reply, Clients, State};
handle_call({broadcast, DocId, Msg}, From, State) ->
    Clients = get_clients_impl(State, DocId),
    OtherClients = [Client || Client <- Clients, Client /= From],
    ?LOG_DEBUG("Send update to ~p", [OtherClients]),
    lists:foreach(fun(Client) -> Client ! Msg end, OtherClients),
    {reply, ok, State}.

-spec handle_cast(tuple(), ws_global_state()) -> {noreply, ws_global_state()}.
handle_cast({disconnect, DocId, From}, State) ->
    NewState =
        case maps:find(DocId, State#ws_global_state.docs) of
            error ->
                State;
            {ok, Doc} ->
                NewDoc = Doc#ws_shared_doc{
                    clients = lists:delete(From, Doc#ws_shared_doc.clients)
                },
                State#ws_global_state{
                    docs = maps:put(DocId, NewDoc, State#ws_global_state.docs)
                }
        end,
    {noreply, NewState}.

-spec handle_info(term(), ws_global_state()) -> {noreply, ws_global_state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), ws_global_state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), ws_global_state(), term()) -> {ok, ws_global_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
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

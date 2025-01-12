-module(websocket_connection_manager).

-behaviour(gen_server).

-export([
    start_link/1, get_doc/2, get_or_create_doc/2, disconnect/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([ws_connection_manager/0, ws_global_state/0, ws_shared_doc/0, ws_local_state/0]).

-include_lib("kernel/include/logger.hrl").
-include("../include/records.hrl").

-type ws_connection_manager() :: pid().

-type ws_global_state() :: #ws_global_state{}.

-type client_info() :: #client_info{}.

-type ws_shared_doc() :: #ws_shared_doc{}.

-type ws_local_state() :: #ws_local_state{}.

%%% API
-spec start_link(module()) -> gen_server:start_ret().
start_link(StorageModule) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [StorageModule], []).

-spec get_or_create_doc(ws_connection_manager(), binary()) -> doc_server:doc().
get_or_create_doc(Manager, DocId) ->
    gen_server:call(Manager, {get_or_create_doc, DocId}).

-spec get_doc(ws_connection_manager(), binary()) -> option:option(doc_server:doc()).
get_doc(Manager, DocId) ->
    gen_server:call(Manager, {get_doc, DocId}).

-spec disconnect(ws_connection_manager(), binary()) -> ok.
disconnect(Manager, DocId) ->
    From = self(),
    gen_server:cast(Manager, {disconnect, DocId, From}).

%%% gen_server callbacks
-spec init([module()]) -> {ok, ws_global_state()}.
init([StorageModule]) ->
    {ok, #ws_global_state{
        docs = #{},
        storage_module = StorageModule
    }}.

-spec handle_call(tuple(), {pid(), term()}, ws_global_state()) ->
    {reply, term(), ws_global_state()}.
handle_call({get_or_create_doc, DocId}, {From, _}, State) ->
    {Doc, NewState} = get_or_create_doc_impl(State, DocId, From),
    {reply, Doc, NewState};
handle_call({get_doc, DocId}, _From, State) ->
    case maps:find(DocId, State#ws_global_state.docs) of
        error ->
            {reply, none, State};
        {ok, Doc} ->
            DocServer = doc_sup:get_child_doc(Doc#ws_shared_doc.doc_sup),
            {reply, {ok, DocServer}, State}
    end;
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

-spec handle_cast(tuple(), ws_global_state()) -> {noreply, ws_global_state()}.
handle_cast({disconnect, DocId, From}, State) ->
    NewState =
        case maps:find(DocId, State#ws_global_state.docs) of
            error ->
                State;
            {ok, Doc} ->
                {DeletedClients, Rest} = lists:splitwith(
                    fun(Client) -> Client#client_info.pid =:= From end, Doc#ws_shared_doc.clients
                ),
                lists:foreach(
                    fun(Client) ->
                        demonitor(Client#client_info.client_monitor_ref)
                    end,
                    DeletedClients
                ),
                case Rest of
                    [] ->
                        doc_sup:terminate(Doc#ws_shared_doc.doc_sup),
                        NewDocs = maps:remove(DocId, State#ws_global_state.docs),
                        State#ws_global_state{
                            docs = NewDocs
                        };
                    Clients ->
                        NewDoc = Doc#ws_shared_doc{clients = Clients},
                        State#ws_global_state{
                            docs = maps:put(DocId, NewDoc, State#ws_global_state.docs)
                        }
                end
        end,
    {noreply, NewState}.

-spec handle_info(term(), ws_global_state()) -> {noreply, ws_global_state()}.
handle_info({'DOWN', MonitorRef, process, Pid, _Reason}, State) ->
    % TODO: the following code takes linear time for documents and clients, please make faster.
    % Remove the terminated clients
    NewDoc = maps:map(
        fun(_, SharedDoc) ->
            NewClients = lists:filter(
                fun(Client) -> Client#client_info.client_monitor_ref =/= MonitorRef end,
                SharedDoc#ws_shared_doc.clients
            ),
            SharedDoc#ws_shared_doc{clients = NewClients}
        end,
        State#ws_global_state.docs
    ),
    {noreply, State#ws_global_state{docs = NewDoc}};
handle_info({'EXIT', From, _Reason}, State) ->
    TerminatedDocKeys = maps:filtermap(
        fun(_, SharedDoc) ->
            case SharedDoc#ws_shared_doc.doc_sup =:= From of
                true ->
                    {true, ok};
                false ->
                    false
            end
        end,
        State#ws_global_state.docs
    ),
    NewState = maps:fold(
        fun(Key, _, Acc) -> maps:remove(Key, Acc) end,
        State#ws_global_state.docs,
        TerminatedDocKeys
    ),
    {noreply, State#ws_global_state{docs = NewState}};
handle_info(Info, State) ->
    ?LOG_WARNING("Unexpected message: ~p~n", [Info]),
    {noreply, State}.

%%% Internal functions
-spec get_or_create_doc_impl(ws_global_state(), binary(), pid()) ->
    {doc_server:doc(), ws_global_state()}.
get_or_create_doc_impl(State, DocId, From) ->
    case maps:find(DocId, State#ws_global_state.docs) of
        {ok, Doc} ->
            ?LOG_INFO("Reusing an opened document: ~p~n", [DocId]),
            ClientMonitorRef = monitor(process, From),
            ClientInfo = #client_info{pid = From, client_monitor_ref = ClientMonitorRef},
            NewDoc = Doc#ws_shared_doc{clients = [ClientInfo | Doc#ws_shared_doc.clients]},
            NewState = State#ws_global_state{
                docs = maps:put(DocId, NewDoc, State#ws_global_state.docs)
            },
            DocServer = doc_sup:get_child_doc(NewDoc#ws_shared_doc.doc_sup),
            {DocServer, NewState};
        error ->
            ?LOG_INFO("Creating a new document: ~p~n", [DocId]),
            {ok, Sup} = doc_sup:start_link(DocId),
            process_flag(trap_exit, true),
            Doc = doc_sup:get_child_doc(Sup),
            ClientMonitorRef = monitor(process, From),
            ClientInfo = #client_info{pid = From, client_monitor_ref = ClientMonitorRef},
            SharedDoc = #ws_shared_doc{
                doc_sup = Sup,
                clients = [ClientInfo]
            },
            NewState = State#ws_global_state{
                docs = maps:put(DocId, SharedDoc, State#ws_global_state.docs)
            },
            {Doc, NewState}
    end.

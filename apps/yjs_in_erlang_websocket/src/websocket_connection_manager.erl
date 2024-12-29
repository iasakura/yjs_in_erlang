-module(websocket_connection_manager).

-behaviour(gen_server).

-export([
    start_link/1, get_or_create_doc/2, disconnect/2
]).
-export([init/1, handle_call/3, handle_cast/2]).
-export_type([ws_connection_manager/0, ws_global_state/0, ws_shared_doc/0, ws_local_state/0]).

-include_lib("kernel/include/logger.hrl").
-include("../include/records.hrl").

-opaque ws_connection_manager() :: pid().

-type ws_global_state() :: #ws_global_state{}.

-type ws_shared_doc() :: #ws_shared_doc{}.

-type ws_local_state() :: #ws_local_state{}.

%%% API
-spec start_link(module()) -> gen_server:start_ret().
start_link(StorageModule) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [StorageModule], []).

-spec get_or_create_doc(ws_connection_manager(), binary()) -> doc:doc().
get_or_create_doc(Manager, DocId) ->
    gen_server:call(Manager, {get_or_create_doc, DocId}).

-spec disconnect(ws_connection_manager(), binary()) -> ok.
disconnect(Manager, DocId) ->
    From = self(),
    gen_server:cast(Manager, {disconnect, DocId, From}).

%%% gen_server callbacks
-spec init([module()]) -> {ok, ws_global_state()}.
init([StorageModule]) ->
    {ok, #ws_global_state{docs = #{}, storage_module = StorageModule}}.

-spec handle_call(tuple(), {pid(), term()}, ws_global_state()) ->
    {reply, term(), ws_global_state()}.
handle_call({get_or_create_doc, DocId}, {From, _}, State) ->
    {Doc, NewState} = get_or_create_doc_impl(State, DocId, From),
    {reply, Doc, NewState}.

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

%%% Internal functions
-spec get_or_create_doc_impl(ws_global_state(), binary(), pid()) ->
    {doc:doc(), ws_global_state()}.
get_or_create_doc_impl(State, DocId, From) ->
    case maps:find(DocId, State#ws_global_state.docs) of
        {ok, Doc} ->
            NewDoc = Doc#ws_shared_doc{clients = [From | Doc#ws_shared_doc.clients]},
            NewState = State#ws_global_state{
                docs = maps:put(DocId, NewDoc, State#ws_global_state.docs)
            },
            {NewDoc#ws_shared_doc.doc, NewState};
        error ->
            Doc = doc:new(),
            StorageModule = State#ws_global_state.storage_module,
            Storage = StorageModule:start_link(Doc, DocId),
            ?LOG_DEBUG("Doc: ~p, Storage: ~p", [Doc, Storage]),
            SharedDoc = #ws_shared_doc{
                doc = Doc,
                storage = Storage,
                clients = [From]
            },
            NewState = State#ws_global_state{
                docs = maps:put(DocId, SharedDoc, State#ws_global_state.docs)
            },
            {Doc, NewState}
    end.

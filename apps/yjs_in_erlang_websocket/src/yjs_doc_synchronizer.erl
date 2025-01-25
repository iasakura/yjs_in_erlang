%% TODO: support synchronization on participating nodes
%% TODO: support periodic synchronization
-module(yjs_doc_synchronizer).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/1]).

-record(state, {
    doc :: doc_server:doc(),
    doc_id :: binary()
}).

init(DocId) ->
    {ok, Doc} = term_key_registerer:get({doc_server, DocId}),
    doc_server:subscribe_update_v1(Doc),
    {ok, #state{doc = Doc, doc_id = DocId}}.

handle_call(_Request, _From, State) ->
    ?LOG_WARNING("Unexpected request: ~p", [_Request]),
    {reply, {error, not_implemented}, State}.

handle_cast(Request, State) ->
    ?LOG_WARNING("Unexpected cast: ~p", [Request]),
    {noreply, State}.

handle_info({notify, update_v1, Update, _}, State) ->
    broadcast_msg(State, {sync, update_v1, Update}),
    {noreply, State};
handle_info({sync, update_v1, Update, _}, State) ->
    Txn = doc_server:new_transaction(State#state.doc),
    transaction:apply_update(Txn, Update),
    transaction:commit(Txn),
    {noreply, State};
handle_info(Request, State) ->
    ?LOG_WARNING("Unexpected message: ~p", [Request]),
    {noreply, State}.

broadcast_msg(State, Msg) ->
    lists:foreach(
        fun(Node) ->
            global:send({Node, yjs_doc_synchronizer, State#state.doc_id}, Msg)
        end,
        nodes()
    ).

-spec start_link(binary()) -> gen_server:start_ret().
start_link(DocId) ->
    gen_server:start_link({global, {node(), yjs_doc_synchronizer, DocId}}, ?MODULE, DocId, []).

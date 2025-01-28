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

handle_info({notify, update_v1, Update, Txn}, State) ->
    ?LOG_DEBUG("Notify update to remote: ~p", [Update]),
    case transaction:get_owner(Txn) =:= self() of
        true ->
            ok;
        false ->
            broadcast_msg(State, {sync, protocol:encode_sync_message({update, Update}), self()})
    end,
    {noreply, State};
handle_info({sync, MsgBin}, State) ->
    {Msg, <<>>} = protocol:decode_sync_message(MsgBin),
    Msgs = message_handler:handle_msg(Msg, State#state.doc),
    lists:foreach(
        fun(M) -> broadcast_msg(State, {sync, protocol:encode_sync_message(M)}) end, Msgs
    ),
    {noreply, State};
handle_info(Request, State) ->
    ?LOG_WARNING("Unexpected message: ~p", [Request]),
    {noreply, State}.

broadcast_msg(State, Msg) ->
    lists:foreach(
        fun(Node) ->
            case global:whereis_name({Node, yjs_doc_synchronizer, State#state.doc_id}) of
                undefined ->
                    ?LOG_WARNING("Node ~p is not available", [Node]),
                    ok;
                Pid ->
                    Pid ! Msg
            end
        end,
        nodes()
    ).

-spec start_link(binary()) -> gen_server:start_ret().
start_link(DocId) ->
    gen_server:start_link({global, {node(), yjs_doc_synchronizer, DocId}}, ?MODULE, DocId, []).

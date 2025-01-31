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
    init_sync(Doc, DocId),
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
handle_info({sync, <<0:8, MsgBin/binary>>, From}, State) ->
    ?LOG_DEBUG("Received sync message: ~p", [MsgBin]),
    {Msg, <<>>} = protocol:decode_sync_message(MsgBin),
    Msgs = message_handler:handle_msg(Msg, State#state.doc),
    lists:foreach(
        fun(M) -> From ! {sync, protocol:encode_sync_message(M), self()} end,
        Msgs
    ),
    {noreply, State};
handle_info({timeout, _, {sync_with_nodes, Nodes}}, State) ->
    {Next, Nodes2} =
        case Nodes of
            [] ->
                {undefined, nodes()};
            [Node | Rest] ->
                {{ok, Node}, Rest}
        end,
    case Next of
        undefined ->
            ok;
        {ok, Node2} ->
            send_sync_step1_to_node(State#state.doc_id, State#state.doc, Node2)
    end,
    erlang:start_timer(1000, self(), {sync_with_nodes, Nodes2}),
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

-spec init_sync(doc_server:doc(), binary()) -> reference().
init_sync(Doc, DocId) ->
    case nodes() of
        [] ->
            erlang:start_timer(1000, self(), {sync_with_nodes, []});
        [Node | Nodes] ->
            send_sync_step1_to_node(DocId, Doc, Node),
            erlang:start_timer(1000, self(), {sync_with_nodes, Nodes})
    end.

-spec send_sync_step1_to_node(binary(), doc_server:doc(), node()) -> ok.
send_sync_step1_to_node(DocId, Doc, Node) ->
    case global:whereis_name({Node, yjs_doc_synchronizer, DocId}) of
        undefined ->
            ok;
        Pid ->
            Pid !
                {sync,
                    protocol:encode_sync_message(
                        {sync_step1, doc_server:get_state_vector(Doc)}
                    ),
                    self()},
            ok
    end.

-spec start_link(binary()) -> gen_server:start_ret().
start_link(DocId) ->
    gen_server:start_link({global, {node(), yjs_doc_synchronizer, DocId}}, ?MODULE, DocId, []).

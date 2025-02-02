-module(websocket_handler).

-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include_lib("kernel/include/logger.hrl").
-include("../include/records.hrl").

-type ws_local_state() :: websocket_connection_manager:ws_local_state().

-spec init(cowboy_req:req(), {websocket_connection_manager:ws_connection_manager(), string()}) ->
    {cowboy_websocket, cowboy_req:req(), {
        websocket_connection_manager:ws_connection_manager(), binary(), string()
    }}
    | {ok, cowboy_req:req(), binary()}.
init(Req, {Manager, StoreDir}) ->
    {PeerAddress, PeerPort} = cowboy_req:peer(Req),
    ?LOG_INFO("Accessed from : ~p:~p~n", [PeerAddress, PeerPort]),
    Rest = cowboy_req:path_info(Req),
    case Rest of
        undefined ->
            Req1 = cowboy_req:reply(400, Req),
            {ok, Req1, <<>>};
        _ ->
            Room = lists:foldl(fun(X, Acc) -> <<Acc/binary, "_", X/binary>> end, <<>>, Rest),
            ?LOG_INFO("Manager: ~p, Room: ~p", [Manager, Room]),
            {cowboy_websocket, Req, {Manager, Room, StoreDir}}
    end.

-spec websocket_init({websocket_connection_manager:ws_connection_manager(), binary(), string()}) ->
    {cowboy_websocket:commands(), ws_local_state()}.
websocket_init({Manager, Room, StoreDir}) ->
    Doc = websocket_connection_manager:get_or_create_doc(Manager, Room, StoreDir),
    MonitorRef = monitor(process, Doc),
    doc_server:subscribe_update_v1(Doc),
    State = #ws_local_state{
        manager = Manager, doc = Doc, doc_id = Room, monitor_ref = MonitorRef
    },
    {
        [
            {binary,
                protocol:encode_sync_message(
                    {sync_step1, doc_server:get_state_vector(State#ws_local_state.doc)}
                )}
        ],
        State
    }.

-spec websocket_handle(
    ping | pong | {text | binary | ping | pong, binary()}, ws_local_state()
) ->
    {cowboy_websocket:commands(), ws_local_state()}.
% 0 means syncMessage
websocket_handle({binary, <<0:8, Msg/binary>>}, State) ->
    {YMsg, _} = protocol:decode_sync_message(Msg),
    Msgs = message_handler:handle_msg(YMsg, State#ws_local_state.doc),
    {[{binary, protocol:encode_sync_message(M)} || M <- Msgs], State};
% 0 means awarenessMessage
websocket_handle({binary, <<1:8, _>>}, State) ->
    % TODO: implement
    {[], State};
websocket_handle(_, State) ->
    {[], State}.

-spec websocket_info(any(), websocket_connection_manager:ws_local_state()) ->
    {cowboy_websocket:commands(), websocket_connection_manager:ws_local_state()}.
websocket_info({notify, update_v1, Update, _}, State) ->
    {[{binary, protocol:encode_sync_message({update, eqwalizer:dynamic_cast(Update)})}], State};
websocket_info({'DOWN', MonitorRef, process, Object, Info}, State) ->
    case MonitorRef =:= State#ws_local_state.monitor_ref of
        true ->
            exit({Object, Info}),
            {[], State};
        false ->
            {[], State}
    end;
websocket_info(_, State) ->
    {[], State}.

terminate(Reason, PartialReq, State) ->
    ?LOG_INFO("Terminating ~p ~p ~p", [Reason, PartialReq, State]),
    case State of
        #ws_local_state{manager = Manager, doc_id = DocId} ->
            websocket_connection_manager:disconnect(Manager, DocId);
        _ ->
            ok
    end.

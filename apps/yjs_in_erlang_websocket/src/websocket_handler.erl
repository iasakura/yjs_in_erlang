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

-spec init(cowboy_req:req(), websocket_connection_manager:ws_connection_manager()) ->
    {cowboy_websocket, cowboy_req:req(), {
        websocket_connection_manager:ws_connection_manager(), binary()
    }}
    | {ok, cowboy_req:req(), binary()}.
init(Req, Manager) ->
    {PeerAddress, PeerPort} = cowboy_req:peer(Req),
    io:format("Accessed from : ~p:~p~n", [PeerAddress, PeerPort]),
    Rest = cowboy_req:path_info(Req),
    case Rest of
        undefined ->
            Req1 = cowboy_req:reply(400, Req),
            {ok, Req1, <<>>};
        _ ->
            Room = lists:foldl(fun(X, Acc) -> <<Acc/binary, "/", X/binary>> end, <<>>, Rest),

            {cowboy_websocket, Req, {Manager, Room}}
    end.

-spec websocket_init({websocket_connection_manager:ws_connection_manager(), binary()}) ->
    {cowboy_websocket:commands(), ws_local_state()}.
websocket_init({Manager, Room}) ->
    Doc = websocket_connection_manager:get_or_create_doc(Manager, Room),
    {
        [{binary, protocol:encode_sync_message({sync_step1, doc:get_state_vector(Doc)})}],
        #ws_local_state{
            manager = Manager, doc = Doc, doc_id = Room
        }
    }.

-spec websocket_handle(
    ping | pong | {text | binary | ping | pong, binary()}, ws_local_state()
) ->
    {cowboy_websocket:commands(), ws_local_state()}.
% 0 means syncMessage
websocket_handle({binary, <<0:8, Msg/binary>>}, State) ->
    {YMsg, _} = protocol:decode_sync_message(Msg),
    ?LOG_DEBUG("syncMessage: ~p", [YMsg]),
    Msgs = message_handler:handle_msg(YMsg, State),
    {Msgs, State};
% 0 means awarenessMessage
websocket_handle({binary, <<1:8, _>>}, Doc) ->
    % TODO: implement
    {[], Doc};
websocket_handle(_, Doc) ->
    {[], Doc}.

-spec websocket_info(any(), websocket_connection_manager:ws_shared_doc()) ->
    {cowboy_websocket:commands(), websocket_connection_manager:ws_shared_doc()}.
websocket_info({notify, update_v1, Update, _}, State) ->
    ?LOG_DEBUG("notify update_v1: ~p", [Update]),
    % eqwalizer:ignore `Update`. Expression has type: term() Context expected type: protocol:sync_messages()
    {[{binary, protocol:encode_sync_message({update, Update})}], State};
websocket_info(_, State) ->
    {[], State}.

terminate(_, _, State) ->
    websocket_connection_manager:disconnect(
        State#ws_local_state.manager, State#ws_local_state.doc_id
    ).

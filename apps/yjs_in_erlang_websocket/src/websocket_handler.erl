-module(websocket_handler).

-import(goc_erlang, [eval/1, c/1, app/2, minus/2, v/2]).

-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-include_lib("kernel/include/logger.hrl").

-spec init(cowboy_req:req(), websocket_connection_manager:ws_connection_manager()) ->
    {cowboy_websocket, cowboy_req:req(), websocket_connection_manager:ws_shared_doc()}
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
            ?LOG_DEBUG("Create or get the room room: ~p from ~p~n", [Room, PeerAddress, PeerPort]),
            Doc = websocket_connection_manager:get_or_create_doc(Manager, Room),
            {cowboy_websocket, Req, Doc}
    end.

-spec websocket_init(websocket_connection_manager:ws_shared_doc()) ->
    {ok, websocket_connection_manager:ws_shared_doc()}.
websocket_init(Doc) -> {ok, Doc}.

-spec websocket_handle(
    ping | pong | {text | binary | ping | pong, binary()},
    websocket_connection_manager:ws_shared_doc()
) ->
    {cowboy_websocket:commands(), websocket_connection_manager:ws_shared_doc()}.
websocket_handle({binary, Msg}, Doc) ->
    {YMsg, _} = protocol:decode_sync_message(Msg),
    Msgs = message_handler:handle_msg(YMsg, Doc),
    {Msgs, Doc};
websocket_handle(_, Doc) ->
    {[], Doc}.

-spec websocket_info(any(), websocket_connection_manager:ws_shared_doc()) ->
    {cowboy_websocket:commands(), websocket_connection_manager:ws_shared_doc()}.
websocket_info({send, Update}, State) when is_binary(Update) ->
    {[{binary, Update}], State};
websocket_info(_, State) ->
    {[], State}.

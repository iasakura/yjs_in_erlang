%%%-------------------------------------------------------------------
%% @doc yjs_in_erlang_websocket public API
%% @end
%%%-------------------------------------------------------------------

-module(yjs_in_erlang_websocket_app).

-behaviour(application).

-export([start/2, stop/1]).

logger_init() ->
    logger:set_primary_config(#{
        level => debug
    }),
    logger:set_handler_config(
        default,
        formatter,
        {logger_formatter, #{
            template => [time, " ", file, ":", line, " ", level, ": ", msg, "\n"]
        }}
    ).

start(_StartType, _StartArgs) ->
    State = websocket_connection_manager:new(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws/[...]", websocket_handler, State}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 3000}], #{
        middlewares => [cors_middleware, cowboy_router, cowboy_handler],
        env => #{dispatch => Dispatch}
    }),
    logger_init(),
    yjs_in_erlang_websocket_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).

%% internal functions

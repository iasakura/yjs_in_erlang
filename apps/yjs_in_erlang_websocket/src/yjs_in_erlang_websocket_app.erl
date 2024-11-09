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
        {logger_color_formatter, #{
            template => [
                time,
                " ",
                color,
                "[",
                level,
                "]",
                {pid, [" ", pid, ""], ""},
                {mfa, [" ", mfa, ":", line], ""},
                ": ",
                msg,
                reset,
                "\n"
            ],
            colors =>
                #{
                    debug => "\e[0;38m",
                    info => "\e[1;37m",
                    notice => "\e[1;36m",
                    warning => "\e[1;33m",
                    error => "\e[1;31m",
                    critical => "\e[1;35m",
                    alert => "\e[1;44m",
                    emergency => "\e[1;41m"
                }
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
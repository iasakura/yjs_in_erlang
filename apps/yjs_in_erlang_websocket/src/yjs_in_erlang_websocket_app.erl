%%%-------------------------------------------------------------------
%% @doc yjs_in_erlang_websocket public API
%% @end
%%%-------------------------------------------------------------------

-module(yjs_in_erlang_websocket_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

start(_StartType, _StartArgs) ->
    websocket_connection_manager_sup:start_link(yjs_in_erlang_storage_file),
    Manager = whereis(websocket_connection_manager),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws/[...]", websocket_handler, Manager}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 3000}], #{
        middlewares => [cors_middleware, cowboy_router, cowboy_handler],
        env => #{dispatch => Dispatch}
    }),
    logger_init(),
    connect_to_nodes(),
    yjs_in_erlang_websocket_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).

connect_to_nodes() ->
    Nodes =
        case application:get_env(yjs_in_erlang_websocket, nodes) of
            {ok, Value} -> Value;
            undefined -> []
        end,
    ?LOG_INFO("cookie: ~p", [erlang:get_cookie()]),
    lists:foreach(
        fun(Node) ->
            Res =
                case Node of
                    Node when is_atom(Node) -> net_kernel:connect_node(Node);
                    Node when is_list(Node) ->
                        Atom = list_to_atom(Node),
                        net_kernel:connect_node(Atom)
                end,
            ?LOG_INFO("Connected to node ~p: ~p", [Node, Res])
        end,
        Nodes
    ).

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

-module(logging).
-export([init/0]).

init() ->
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

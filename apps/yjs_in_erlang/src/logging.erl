-module(logging).
-export([init/0]).

init() ->
    logger:set_primary_config(#{
        level => info
    }),
    logger:add_handler(to_file_handler, logger_std_h, #{
        config => #{file => "/tmp/debug.log"}, level => info
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

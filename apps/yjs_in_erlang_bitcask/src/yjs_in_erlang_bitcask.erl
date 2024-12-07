-module(yjs_in_erlang_bitcask).

-export([start/0]).

-spec start() -> ok.
start() ->
    Ref =
        case bitcask:open("./data", [read_write]) of
            R when is_reference(R) -> R;
            {error, E} -> throw(io_lib:format("Cannot open: ~p~n", [E]))
        end,
    bitcask:put(Ref, <<"key">>, <<"value">>),
    {ok, <<"value">>} = bitcask:get(Ref, <<"key">>),
    bitcask:close(Ref).

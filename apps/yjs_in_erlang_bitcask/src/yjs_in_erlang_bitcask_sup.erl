-module(yjs_in_erlang_bitcask_sup).

-behaviour(supervisor).

-export([init/1, get_storage_pid/1]).

init([Doc, DocId]) ->
    {ok,
        {
            #{
                strategy => one_for_one,
                intensity => 1,
                period => 5
            },
            [
                #{
                    id => yjs_in_erlang_bitcask,
                    start => {yjs_in_erlang_bitcask, start_link, [Doc, DocId]},
                    restart => transient
                }
            ]
        }}.

-spec get_storage_pid(pid()) -> pid().
get_storage_pid(Pid) ->
    case supervisor:which_children(Pid) of
        [{yjs_in_erlang_bitcask, Pid, _, _, _} | _] -> Pid;
        [] -> throw("no storage process")
    end.

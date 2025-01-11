-module(yjs_in_erlang_bitcask_sup).

-behaviour(supervisor).

-export([init/1, get_child_pid/1, start_link/1]).

init([DocId]) ->
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
                    start => {yjs_in_erlang_bitcask, start_link, [DocId]},
                    restart => transient
                }
            ]
        }}.

-spec get_child_pid(pid()) -> pid().
get_child_pid(Pid) ->
    case supervisor:which_children(Pid) of
        [{yjs_in_erlang_bitcask, Pid, _, _, _} | _] -> Pid;
        [] -> throw("no storage process")
    end.

-spec start_link(binary()) -> supervisor:startlink_ret().
start_link(DocId) ->
    supervisor:start_link(?MODULE, [DocId]).

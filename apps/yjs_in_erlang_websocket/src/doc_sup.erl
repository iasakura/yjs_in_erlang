-module(doc_sup).

-behaviour(supervisor).

-export([init/1, start_link/1, get_child_doc/1, get_child_storage/1, terminate/1]).

init([Key]) ->
    {ok,
        {#{strategy => rest_for_one, intencity => 1, period => 5}, [
            #{
                id => doc_server,
                start => {doc_server, start_link, [Key]},
                restart => permanent
            },
            #{
                id => doc_storage,
                start => {doc_storage, start_link, [Key]},
                restart => permanent
            }
        ]}}.

-spec get_child_doc(pid()) -> pid().
get_child_doc(Manager) ->
    find_child_by_id(Manager, doc_server).

-spec get_child_storage(pid()) -> pid().
get_child_storage(Manager) ->
    find_child_by_id(Manager, doc_storage).

-spec find_child_by_id(pid(), atom()) -> pid().
find_child_by_id(Supervisor, Id) ->
    Children = supervisor:which_children(Supervisor),
    case lists:keyfind(Id, 1, Children) of
        {Id, Pid, _Type, _Modules} when is_pid(Pid) ->
            %% Found the child
            Pid;
        false ->
            %% Child with the given id does not exist
            throw({error, not_found})
    end.

-spec start_link(binary()) -> supervisor:startlink_ret().
start_link(Key) ->
    supervisor:start_link(?MODULE, [Key]).

-spec terminate(pid()) -> ok.
terminate(Pid) ->
    exit(Pid, shutdown),
    ok.

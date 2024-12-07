-module(websocket_connection_manager_sup).

-behaviour(supervisor).

%% Callbacks for `supervisor`
-export([start_link/0, init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {{one_for_one, 5, 10}, [
            #{
                id => websocket_connection_manager,
                start => {websocket_connection_manager, start_link, []},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [websocket_connection_manager]
            }
        ]}}.

-module(websocket_connection_manager_sup).

-behaviour(supervisor).

%% Callbacks for `supervisor`
-export([start_link/1, init/1]).

-spec start_link(module()) -> supervisor:startlink_ret().
start_link(StorageModule) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [StorageModule]).

init([StorageModule]) ->
    {ok,
        {{one_for_one, 5, 10}, [
            #{
                id => websocket_connection_manager,
                start => {websocket_connection_manager, start_link, [StorageModule]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [websocket_connection_manager]
            }
        ]}}.

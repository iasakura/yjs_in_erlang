-module(storage_server).

-behaviour(gen_server).

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/1]).

-record(state, {
    inner_storage :: pid()
}).

init([Key]) ->
    Manager =
        case whereis(websocket_connection_manager) of
            ManagerPid when is_pid(ManagerPid) ->
                ManagerPid
        end,
    {ok, Doc} = websocket_connection_manager:get_doc(Manager, Key),
    {ok, Pid} = yjs_in_erlang_bitcask:start_link(Key),
    doc_server:subscribe_update_v1(Doc),
    {ok, #state{inner_storage = Pid}}.

handle_call(_Request, _From, _State) ->
    erlang:error(not_implemented).

handle_cast(_Request, _State) ->
    erlang:error(not_implemented).

%% just forward all requests to inner storage
handle_info(Request, State) ->
    State#state.inner_storage ! Request.

-spec start_link(binary()) -> gen_server:start_ret().
start_link(Key) ->
    gen_server:start_link(?MODULE, [Key], []).

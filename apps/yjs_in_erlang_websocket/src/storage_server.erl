-module(storage_server).

-behaviour(gen_server).

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/2]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    inner_storage :: pid()
}).

init({Key, StorageModule}) ->
    MayBeDoc = global:whereis_name({doc_server, Key}),
    Doc =
        case MayBeDoc of
            Pid when is_pid(Pid) ->
                ?LOG_INFO("Found existing doc ~p", [Pid]),
                Pid;
            _ ->
                exit({error, not_found})
        end,
    ?LOG_INFO("Subscribing to updates for doc ~p", [Doc]),
    {ok, Bitcask} = StorageModule:start_link(Key),
    doc_server:subscribe_update_v1(Doc),
    {ok, #state{inner_storage = Bitcask}}.

handle_call(_Request, _From, _State) ->
    erlang:error(not_implemented).

handle_cast(_Request, _State) ->
    erlang:error(not_implemented).

%% just forward all requests to inner storage
handle_info(Request, State) ->
    State#state.inner_storage ! Request,
    {noreply, State}.

-spec start_link(binary(), module()) -> gen_server:start_ret().
start_link(Key, StorageModule) ->
    gen_server:start_link(?MODULE, {Key, StorageModule}, []).

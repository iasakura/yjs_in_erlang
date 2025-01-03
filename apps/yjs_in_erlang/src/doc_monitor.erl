-module(doc_monitor).

-behavior(gen_server).

-type doc_monitor() :: pid().

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2, start_link/2, start_monitor/2]).
-export_type([doc_monitor/0]).

-record(state, {
    ets_manager :: pid(),
    event_manager :: pid()
}).

init([EtsManager, EventManager]) ->
    link(EtsManager),
    link(EventManager),
    {ok, #state{
        ets_manager = EtsManager,
        event_manager = EventManager
    }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

-spec start_link(ets_manager:ets_manager(), event_manager:event_manager()) ->
    gen_server:start_ret().
start_link(EtsManager, EventManager) ->
    gen_server:start_link(?MODULE, [EtsManager, EventManager], []).

-spec start_monitor(ets_manager:ets_manager(), event_manager:event_manager()) ->
    gen_server:start_mon_ret().
start_monitor(EtsManager, EventManager) ->
    gen_server:start_monitor(?MODULE, [EtsManager, EventManager], []).

-module(event_manager).

-behavior(gen_server).

-record(state, {
    event_manager :: pid()
}).

-include_lib("kernel/include/logger.hrl").

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, start_link/0, terminate/2]).
-export([subscribe/2, unsubscribe/2, has_subscribers/2, notify_update_v1/3]).
-export_type([event_manager/0]).

-opaque event_manager() :: pid().

-type event_target() :: update_v1.

-spec init(list()) -> {ok, #state{}}.
init([]) ->
    {ok, EventManager} = gen_event:start_link(),
    {ok, #state{event_manager = EventManager}}.

handle_call({subscribe, update_v1}, {From, _}, State) ->
    Ref = monitor(process, From),
    gen_event:add_handler(State#state.event_manager, {yjs_event_handler, Ref}, [
        Ref, From, true, []
    ]),
    {reply, {ok, Ref}, State};
handle_call({unsubscribe, update_v1, Ref}, {_From, _}, State) ->
    gen_event:delete_handler(State#state.event_manager, {yjs_event_handler, Ref}, []),
    demonitor(Ref, [flush]),
    {reply, ok, State};
handle_call({has_subscribers, Source}, _From, State) ->
    Handlers = gen_event:which_handlers(State#state.event_manager),
    {reply,
        lists:any(
            fun(Handler) ->
                case Handler of
                    {yjs_event_handler, Ref} ->
                        yjs_event_handler:is_subscribing(
                            State#state.event_manager, eqwalizer:dynamic_cast(Ref), Source
                        );
                    _ ->
                        false
                end
            end,
            Handlers
        ),
        State};
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

handle_cast({notify, update_v1, Update, Txn}, State) ->
    gen_event:notify(State#state.event_manager, {notify, update_v1, Update, Txn}),
    {noreply, State};
handle_cast({notify, node, Node, Txn}, State) ->
    gen_event:notify(State#state.event_manager, {notify, node, Node, Txn}),
    {noreply, State}.

% Monitor the subscriber process and remove the handler when it goes down
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    gen_event:delete_handler(State#state.event_manager, {yjs_event_handler, Ref}, []),
    {noreply, State};
handle_info(Info, State) ->
    ?LOG_DEBUG("Unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    gen_event:notify(State#state.event_manager, {notify, exit}),
    ok.

-spec start_link() -> event_manager().
start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

-spec subscribe(event_manager(), event_target()) -> {ok, reference()}.
subscribe(Manager, Event) ->
    gen_server:call(Manager, {subscribe, Event}).

-spec unsubscribe(event_manager(), event_target()) -> ok.
unsubscribe(Manager, Event) ->
    gen_server:call(Manager, {unsubscribe, Event}).

-spec has_subscribers(event_manager(), event_target()) -> boolean().
has_subscribers(Manager, Event) ->
    gen_server:call(Manager, {has_subscribers, Event}).

-spec notify_update_v1(event_manager(), update:update(), transaction:transaction_mut()) -> ok.
notify_update_v1(Manager, Update, Txn) ->
    gen_server:cast(Manager, {notify, update_v1, Update, Txn}).

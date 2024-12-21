-module(event_manager).

-behavior(gen_server).

-record(state, {
    update_v1_subscribers :: list(pid()),
    node_subscribers :: #{branch:branch() => list(pid())}
}).

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2, start/0]).
-export([subscribe/2, unsubscribe/2, has_subscribers/2, notify_update_v1/3]).
-export_type([event_manager/0]).

-opaque event_manager() :: pid().

-type event_target() :: update_v1.

-spec init(list()) -> {ok, #state{}}.
init(_Args) ->
    {ok, #state{update_v1_subscribers = [], node_subscribers = #{}}}.

handle_call({subscribe, update_v1}, {From, _}, State) ->
    {reply, ok, State#state{update_v1_subscribers = [From | State#state.update_v1_subscribers]}};
handle_call({unsubscribe, update_v1}, {From, _}, State) ->
    {reply, ok, State#state{
        update_v1_subscribers = lists:delete(From, State#state.update_v1_subscribers)
    }};
handle_call({has_subscribers, update_v1}, _From, State) ->
    {reply, length(State#state.update_v1_subscribers) > 0, State};
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

handle_cast({notify, update_v1, Update, Txn}, State) ->
    lists:foreach(
        fun(Pid) ->
            % only send to the processes other than the owner process
            case Pid =:= transaction:get_owner(Txn) of
                true -> ok;
                false -> Pid ! {notify, update_v1, Update, Txn}
            end
        end,
        State#state.update_v1_subscribers
    ),
    {noreply, State};
handle_cast({notify, node, Node, Txn}, State) ->
    maps:foreach(
        fun(_, Subs) ->
            lists:foreach(
                fun(Pid) ->
                    % only send to the processes other than the owner process
                    case Pid =:= transaction:get_owner(Txn) of
                        true -> ok;
                        false -> Pid ! {notify, node, Node, Txn}
                    end
                end,
                Subs
            )
        end,
        maps:filter(
            fun(N, _) ->
                N =:= Node
            end,
            State#state.node_subscribers
        )
    ),
    {noreply, State}.

-spec start() -> event_manager().
start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

-spec subscribe(event_manager(), event_target()) -> ok.
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

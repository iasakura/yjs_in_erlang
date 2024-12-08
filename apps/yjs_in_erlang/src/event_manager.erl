-module(event_manager).

-behavior(gen_server).

-record(state, {
    update_v1_subscribers :: list(pid()),
    node_subscribers :: #{branch:branch() => list(pid())}
}).

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2, start/0]).
-export_type([event_manager/0]).

-opaque event_manager() :: pid().

% -type event() ::
%     {update_v1, update:update()}
%     | {node, branch:branch(), transaction:transaction_mut()}.

-spec init(list()) -> {ok, #state{}}.
init(_Args) ->
    {ok, #state{update_v1_subscribers = [], node_subscribers = #{}}}.

handle_call({subscribe, update}, From, State) ->
    {reply, ok, State#state{update_v1_subscribers = [From | State#state.update_v1_subscribers]}};
handle_call({unsubscribe, update}, From, State) ->
    {reply, ok, State#state{
        update_v1_subscribers = lists:delete(From, State#state.update_v1_subscribers)
    }};
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

handle_cast({notify, update_v1, Update}, State) ->
    lists:foreach(
        fun(Pid) -> Pid ! {notify, update_v1, Update} end, State#state.update_v1_subscribers
    ),
    {noreply, State};
handle_cast({notify, node, Node, Txn}, State) ->
    maps:foreach(
        fun(_, Subs) ->
            lists:foreach(
                fun(Pid) ->
                    Pid ! {notify, node, Node, Txn}
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

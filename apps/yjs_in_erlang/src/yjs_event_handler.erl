-module(yjs_event_handler).

-behaviour(gen_event).

-record(state, {
    handler_ref :: reference(),
    subscriber :: pid(),
    update_v1_subscriber :: boolean(),
    node_subscribers :: [branch:branch()]
}).

%% Callbacks for `gen_server`
-export_type([event_manager/0]).
%% Callbacks for `gen_event`
-export([init/1, handle_event/2, handle_call/2]).
-export([is_subscribing/3]).

-opaque event_manager() :: pid().

-type event_source() :: update_v1 | {node, branch:branch()}.

%% callbacks for `gen_event`

init([Ref, ToPid, SubscribeUpdateV1, SubscribedNodes]) ->
    {ok, #state{
        handler_ref = Ref,
        subscriber = ToPid,
        update_v1_subscriber = SubscribeUpdateV1,
        node_subscribers = SubscribedNodes
    }}.

handle_event({notify, update_v1, Update, Txn}, State) ->
    case State#state.update_v1_subscriber of
        true -> State#state.subscriber ! {notify, update_v1, Update, Txn};
        false -> ok
    end,
    {ok, State};
handle_event({notify, node, Node, Txn}, State) ->
    % TODO: lightweight equality check
    case lists:member(Node, State#state.node_subscribers) of
        true -> State#state.subscriber ! {notify, node, Node, Txn};
        false -> ok
    end,
    {ok, State};
handle_event(exit, State) ->
    State#state.subscriber ! {exit, State#state.handler_ref},
    {stop, normal, State};
handle_event(_, State) ->
    {ok, State}.

handle_call({is_subscribing, Source}, State) ->
    Reply =
        case Source of
            update_v1 -> State#state.update_v1_subscriber;
            {node, Node} -> lists:member(Node, State#state.node_subscribers)
        end,
    {ok, Reply, State}.

%% API

-spec is_subscribing(pid(), reference(), event_source()) -> boolean().
is_subscribing(Manager, Ref, Source) ->
    eqwalizer:dynamic_cast(gen_event:call(Manager, {?MODULE, Ref}, {is_subscribing, Source})).

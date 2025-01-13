-module(yjs_in_erlang_storage).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, start_link/2]).

-record(state, {
    module :: module(),
    state :: term()
}).

-callback get_update(term()) -> update:update().
-callback on_update(update:update(), term()) -> ok.
-callback init(term()) -> {ok, term()}.

-spec init({module(), term()}) -> {ok, term()}.
init({Module, Arg}) ->
    {ok, InnerState} = Module:init(Arg),
    {ok, #state{module = Module, state = InnerState}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({notify, update_v1, Update, _}, State) ->
    Module = State#state.module,
    Module:on_update(State#state.state, Update),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

-spec start_link(module(), term()) -> gen_server:start_ret().
start_link(Module, Arg) ->
    gen_server:start_link(?MODULE, {Module, Arg}, []).

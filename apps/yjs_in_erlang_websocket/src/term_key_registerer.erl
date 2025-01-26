-module(term_key_registerer).

-behaviour(gen_server).

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/0, register/2, get/1, unregister/1]).

-include_lib("kernel/include/logger.hrl").

-record(entry, {
    key :: term(),
    ref :: reference(),
    pid :: pid()
}).

-record(ref_table_entry, {
    ref :: reference(),
    table_key :: term()
}).

-record(state, {
    table :: ets:table(),
    ref_table :: ets:table()
}).

init([]) ->
    Table = ets:new(term_key_registerer, [set, public, {keypos, #entry.key}]),
    RefTable = ets:new(term_key_registerer_ref, [set, public, {keypos, #ref_table_entry.ref}]),
    {ok, #state{table = Table, ref_table = RefTable}}.

handle_call({register, Key, Pid}, _From, State) ->
    Ref = monitor(process, Pid),
    ets:insert(State#state.table, #entry{key = Key, pid = Pid, ref = Ref}),
    ets:insert(State#state.ref_table, #ref_table_entry{ref = Ref, table_key = Key}),
    {reply, ok, State};
handle_call({get, Key}, _From, State) ->
    case ets:lookup(State#state.table, Key) of
        [] ->
            {reply, undefined, State};
        [#entry{pid = Pid}] ->
            {reply, {ok, Pid}, State}
    end;
handle_call({unregister, Key}, _From, State) ->
    case ets:lookup(State#state.table, Key) of
        [] ->
            {reply, false, State};
        [#entry{ref = Ref}] ->
            ets:delete(State#state.table, Key),
            ets:delete(State#state.ref_table, Ref),
            erlang:demonitor(Ref, [flush]),
            {reply, true, State}
    end.

handle_cast(Request, State) ->
    ?LOG_WARNING("Unexpected cast: ~p", [Request]),
    {noreply, State}.

handle_info({'DOWN', Ref, _, _, _}, State) ->
    case ets:lookup(State#state.ref_table, Ref) of
        [] ->
            {noreply, State};
        [#ref_table_entry{table_key = Key}] ->
            ets:delete(State#state.table, Key),
            ets:delete(State#state.ref_table, Ref),
            {noreply, State}
    end;
handle_info(_Request, State) ->
    {noreply, State}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register(term(), pid()) -> ok.
register(Key, Pid) ->
    gen_server:call(?MODULE, {register, Key, Pid}).

-spec get(term()) -> option:option(pid()).
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

-spec unregister(term()) -> boolean().
unregister(Key) ->
    gen_server:call(?MODULE, {unregister, Key}).

-module(doc_server).

-behaviour(gen_server).

-type doc() :: pid().

-include_lib("kernel/include/logger.hrl").
-include_lib("yjs_in_erlang/include/records.hrl").

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2, start_link/2]).
-export([
    get_or_create_text/2,
    transact_mut/1,
    get_update/2,
    subscribe_update_v1/1,
    get_state_vector/1,
    get_monitor/1,
    new_transaction/1
]).
-export_type([doc/0]).

-record(state, {
    doc :: doc:doc()
}).

init({Dir, StorageModule}) ->
    Update = StorageModule:get_update(Dir),
    Doc = doc:new(),
    Txn = doc:transact_mut(Doc),
    transaction:apply_update(Txn, Update),
    term_key_registerer:register({doc_server, Dir}, self()),
    {ok, #state{doc = Doc}}.

handle_call({get_or_create_text, Name}, _From, State) ->
    Doc = State#state.doc,
    {reply, doc:get_or_create_text(Doc, Name), State};
handle_call(transact_mut, _From, State) ->
    Doc = State#state.doc,
    {reply, doc:transact_mut(Doc), State};
handle_call({get_update, SV}, _From, State) ->
    Doc = State#state.doc,
    {reply, doc:get_update(Doc, SV), State};
handle_call(subscribe_update_v1, {From, _}, State) ->
    Doc = State#state.doc,
    doc:subscribe_update_v1(Doc, From),
    {reply, ok, State};
handle_call(get_state_vector, _From, State) ->
    Doc = State#state.doc,
    {reply, doc:get_state_vector(Doc), State};
handle_call(get_monitor, _From, State) ->
    Doc = State#state.doc,
    {reply, doc:get_monitor(Doc), State};
handle_call(new_transaction, {From, _}, State) ->
    Doc = State#state.doc,
    {reply, transaction:new(Doc, From), State};
handle_call(Request, _From, State) ->
    ?LOG_WARNING("Unknown request: ~p", [Request]),
    {reply, {error, {unknown_request, Request}}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

-spec start_link(term(), module()) -> gen_server:start_ret().
start_link(Key, StorageModule) ->
    gen_server:start_link(?MODULE, {Key, StorageModule}, []).

-spec get_or_create_text(pid(), binary()) -> text:y_text().
get_or_create_text(Server, Name) ->
    gen_server:call(Server, {get_or_create_text, Name}).

-spec transact_mut(pid()) -> transaction:transaction_mut().
transact_mut(Doc) ->
    gen_server:call(Doc, transact_mut).

-spec get_update(pid(), state_vector:state_vector()) -> update:update().
get_update(Doc, SV) ->
    gen_server:call(Doc, {get_update, SV}).

-spec subscribe_update_v1(pid()) -> ok.
subscribe_update_v1(Doc) ->
    gen_server:call(Doc, subscribe_update_v1).

-spec get_state_vector(pid()) -> state_vector:state_vector().
get_state_vector(Doc) ->
    gen_server:call(Doc, get_state_vector).

-spec get_monitor(pid()) -> pid().
get_monitor(Doc) ->
    gen_server:call(Doc, get_monitor).

-spec new_transaction(pid()) -> transaction:transaction_mut().
new_transaction(Doc) ->
    gen_server:call(Doc, new_transaction).

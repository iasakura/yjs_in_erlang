-module(yjs_in_erlang_bitcask).

-behavior(gen_server).

-include_lib("yjs_in_erlang/include/records.hrl").

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    bitcask_updates_ref :: reference(),
    bitcask_deletes_ref :: reference(),
    doc :: doc:doc()
}).

-type state() :: #state{}.

-spec init({doc:doc(), reference(), reference()}) -> {ok, state()}.
init({Doc, BitcaskItemsRef, BitcaskDeletesRef}) ->
    doc:subscribe_update_v1(Doc),
    {ok, #state{
        doc = Doc, bitcask_updates_ref = BitcaskItemsRef, bitcask_deletes_ref = BitcaskDeletesRef
    }}.

handle_call({get_updates, StateVector}, From, State) ->
    {reply, Update, State}.

handle_cast(Request, State) ->
    erlang:error(not_implemented).

handle_info({notify, update_v1, Update, _}, State) ->
    maps:foreach(
        fun(_, Blocks) ->
            lists:foreach(
                fun(Block) ->
                    case Block of
                        {item, Item} ->
                            Id = item:get_id(Item),
                            Key = id:encode_id(Id),
                            bitcask:put(State#state.bitcask_updates_ref, Key, {item, Item});
                        {gc, Range} ->
                            Id = Range#block_range.id,
                            Key = id:encode_id(Id),
                            bitcask:put(State#state.bitcask_updates_ref, Key, {gc, Range});
                        {skip, _} ->
                            ok
                    end
                end,
                Blocks
            )
        end,
        Update#update.update_blocks
    ),
    maps:foreach(
        fun(ClientId, Range) ->
            case Range of
                {continuous, Range} ->
                    Key = id:encode_id(id:new(ClientId, Range#range.start)),
                    bitcask:put(State#state.bitcask_deletes_ref, Key, Range);
                {fragmented, Ranges} ->
                    lists:foreach(
                        fun(R) ->
                            Key = id:encode_id(id:new(ClientId, R#range.start)),
                            bitcask:put(State#state.bitcask_deletes_ref, Key, R)
                        end,
                        Ranges
                    )
            end
        end,
        Update#update.delete_set
    ),
    {noreply, State}.

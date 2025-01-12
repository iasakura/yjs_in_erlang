-module(yjs_in_erlang_bitcask).

-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").
-include_lib("yjs_in_erlang/include/records.hrl").

-export([
    get_update/1, start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2
]).

-record(state, {
    bitcask_updates_ref :: reference(),
    bitcask_deletes_ref :: reference(),
    subscribe_ref :: option:option(reference())
}).

-type state() :: #state{}.

-spec start_link(binary()) -> gen_server:start_ret().
start_link(BitcaskDir) ->
    gen_server:start_link(?MODULE, BitcaskDir, []).

-spec get_update(binary()) -> update:update().
get_update(Prefix) ->
    State = get_state(Prefix),
    Update = get_updates(State, state_vector:new()),
    Update.

-spec get_state(binary()) -> state().
get_state(BitcaskDir) ->
    BitcaskItemsRef =
        case
            bitcask:open(binary_to_list(<<"./", BitcaskDir/binary, "/items">>), [
                read_write
            ])
        of
            {error, timeout} -> throw("timeout: open items db");
            Ref -> Ref
        end,
    BitcaskDeletesRef =
        case
            bitcask:open(binary_to_list(<<"./", BitcaskDir/binary, "/deletes">>), [
                read_write
            ])
        of
            {error, timeout} -> throw("timeout: open deletes db");
            Ref1 -> Ref1
        end,
    #state{
        bitcask_updates_ref = BitcaskItemsRef,
        bitcask_deletes_ref = BitcaskDeletesRef,
        subscribe_ref = undefined
    }.

-spec init(binary()) -> {ok, state()}.
init(BitcaskDir) ->
    {ok, get_state(BitcaskDir)}.

handle_call(get_doc, _From, State) ->
    Doc = doc:new(),
    Txn = transaction:new(Doc),
    Updates = get_updates(State, state_vector:new()),

    transaction:apply_update(Txn, Updates),
    transaction:commit(Txn),
    Ref = doc:subscribe_update_v1(Doc),
    Monitor = doc:get_monitor(Doc),
    monitor(process, Monitor),

    transaction:apply_update(Txn, Updates),
    {reply, Doc, State#state{subscribe_ref = Ref}}.

handle_cast(_Request, _State) ->
    erlang:error(not_implemented).

handle_info({notify, update_v1, Update, _}, State) ->
    maps:foreach(
        fun(_, Blocks) ->
            lists:foreach(
                fun(Block) ->
                    EncodedBlock = update:encode_block(Block),
                    case Block of
                        {item, Item} ->
                            Id = item:get_id(Item),
                            Key = id:encode_id(Id),
                            bitcask:put(
                                State#state.bitcask_updates_ref, Key, EncodedBlock
                            );
                        {gc, Range} ->
                            Id = Range#block_range.id,
                            Key = id:encode_id(Id),
                            bitcask:put(State#state.bitcask_updates_ref, Key, EncodedBlock);
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
                    Key = id:encode_id(id:new(ClientId, Range#range.start));
                {fragmented, Ranges} ->
                    case Ranges of
                        [] ->
                            Key = undefined;
                        [Range | _] ->
                            Key = id:encode_id(id:new(ClientId, Range#range.start))
                    end
            end,
            case Key of
                undefined ->
                    ok;
                _ ->
                    EncodedRanges = id_set:encode_id_range(Range),
                    bitcask:put(State#state.bitcask_deletes_ref, Key, EncodedRanges)
            end
        end,
        #{}
    ),
    {noreply, State};
handle_info({exit, Ref}, State) ->
    exit({error, io_lib:format("Exit doc of ~p", [Ref])}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc 型付けのためのダミー関数。モジュール化してもいいかも
-spec bitcask_fold(Ref, Fun, Acc) -> T | {error, term()} when
    Ref :: reference(),
    Fun :: fun((Key :: binary(), Value :: binary(), T) -> T),
    Acc :: T.
bitcask_fold(Ref, Fun, Acc) ->
    % eqwalizer:ignore
    bitcask:fold(Ref, Fun, Acc).

%% internal
-spec get_updates(state(), state_vector:state_vector()) -> update:update().
get_updates(State, _StateVector) ->
    BlockCarriers = bitcask_fold(
        State#state.bitcask_updates_ref,
        fun(Key, Value, Acc) ->
            {Id, <<>>} = id:decode_id(Key),
            {BlockCarrier, <<>>} = update:decode_block(Id, Value),
            case Id of
                undefined ->
                    Acc;
                _ ->
                    maps:update_with(
                        Id#id.client, fun(BCs) -> [BlockCarrier | BCs] end, [BlockCarrier], Acc
                    )
            end
        end,
        #{}
    ),
    BlockCarriers0 =
        case BlockCarriers of
            {error, BCTerm} -> throw(BCTerm);
            _ -> BlockCarriers
        end,
    DeleteCarriers = bitcask_fold(
        State#state.bitcask_deletes_ref,
        fun(Key, Value, Acc) ->
            {IdRange, <<>>} = id_set:decode_id_range(Value),
            {Id, <<>>} = id:decode_id(Key),
            maps:update_with(Id#id.client, fun(Ranges) -> [IdRange | Ranges] end, [IdRange], Acc)
        end,
        #{}
    ),
    DeleteSet0 =
        case DeleteCarriers of
            {error, DCTerm} -> throw(DCTerm);
            _ -> DeleteCarriers
        end,
    #update{
        update_blocks = BlockCarriers0,
        delete_set = DeleteSet0
    }.

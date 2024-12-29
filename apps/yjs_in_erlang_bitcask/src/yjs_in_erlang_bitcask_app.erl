-module(yjs_in_erlang_bitcask_app).

-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").
-include_lib("yjs_in_erlang/include/records.hrl").

%% Callbacks for `gen_server`
-export([
    get_updates/2, start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2
]).

-record(state, {
    bitcask_updates_ref :: reference(),
    bitcask_deletes_ref :: reference(),
    doc :: doc:doc()
}).

-type state() :: #state{}.

-spec start_link(doc:doc(), binary()) -> gen_server:start_ret().
start_link(Doc, BitcaskDir) ->
    gen_server:start_link(
        {local, ?MODULE}, ?MODULE, {Doc, BitcaskDir}, []
    ).

-spec get_updates(pid(), state_vector:state_vector()) -> update:update().
get_updates(Pid, StateVector) ->
    gen_server:call(Pid, {get_updates, StateVector}).

-spec init({doc:doc(), reference(), reference()}) -> {ok, state()}.
init({Doc, BitcaskDir}) ->
    BitcaskItemsRef = bitcask:open(binary_to_list(<<"./", BitcaskDir/binary, "/items">>), [
        read_write
    ]),
    BitcaskDeletesRef = bitcask:open(binary_to_list(<<"./", BitcaskDir/binary, "/deletes">>), [
        read_write
    ]),
    Txn = transaction:new(Doc),
    State = #state{
        doc = Doc, bitcask_updates_ref = BitcaskItemsRef, bitcask_deletes_ref = BitcaskDeletesRef
    },
    Update = get_updates_impl(State, state_vector:new()),
    ?LOG_DEBUG("Initial update: ~p", [Update]),
    transaction:apply_update(Txn, Update),
    doc:subscribe_update_v1(Doc),
    {ok, State}.

handle_call({get_updates, StateVector}, _From, State) ->
    Updates = get_updates_impl(State, StateVector),
    {reply, Updates, State}.

handle_cast(_Request, _State) ->
    erlang:error(not_implemented).

handle_info({notify, update_v1, Update, _}, State) ->
    ?LOG_DEBUG("notify update_v1: ~p", [Update]),
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
    {noreply, State}.

%% @doc 型付けのためのダミー関数。モジュール化してもいいかも
-spec bitcask_fold(Ref, Fun, Acc) -> T | {error, term()} when
    Ref :: reference(),
    Fun :: fun((Key :: binary(), Value :: binary(), T) -> T),
    Acc :: T.
bitcask_fold(Ref, Fun, Acc) ->
    % eqwalizer:ignore
    bitcask:fold(Ref, Fun, Acc).

-spec get_updates_impl(state(), state_vector:state_vector()) -> update:update().
get_updates_impl(State, _StateVector) ->
    ?LOG_DEBUG("State: ~p", [State]),
    BlockCarriers = bitcask_fold(
        State#state.bitcask_updates_ref,
        fun(Key, Value, Acc) ->
            ?LOG_DEBUG("Key: ~p, Value: ~p", [Key, Value]),
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
    ?LOG_DEBUG("BlockCarriers: ~p", [BlockCarriers]),
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

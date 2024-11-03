-module(update).

-export([new/0, decode_update/1, encode_update/1, integrate/2, merge_update/1]).
-export_type([update/0, pending_update/0, delete_set/0, block_range/0]).

-include("../include/records.hrl").
-include("../include/constants.hrl").
-include_lib("kernel/include/logger.hrl").

-type block_range() :: #block_range{}.

% -type ranges() :: [#range{}].

% -type id_set() :: #{state_vector:client_id() => ranges()}.

-type delete_set() :: id_set:id_set().

-type update() :: #update{}.

-type item() :: #item{}.

-type block_carrier() ::
    {item, item()}
    | {gc, block_range()}
    | {skip, block_range()}.

-type update_blocks() :: #{state_vector:client_id() => [block_carrier()]}.

-type pending_update() :: #pending_update{}.

-spec new() -> update().
new() ->
    #update{
        update_blocks = #{},
        delete_set = id_set:new()
    }.

-spec block_carrier_length(block_carrier()) -> integer().
block_carrier_length({item, Item}) -> item:len(Item);
block_carrier_length({gc, Range}) -> Range#block_range.len;
block_carrier_length({skip, Range}) -> Range#block_range.len.

-spec bc_id(block_carrier()) -> id:id().
bc_id({item, Item}) -> Item#item.id;
bc_id({gc, Range}) -> Range#block_range.id;
bc_id({skip, Range}) -> Range#block_range.id.

-spec bc_integrate(block_carrier(), transaction:transaction_mut(), integer()) -> boolean().
bc_integrate({item, Item}, Txn, Offset) -> item:integrate(Item, Txn, Offset);
bc_integrate({gc, _Range}, _Txn, _Offset) -> throw("wip: range.integrate");
bc_integrate({skip, _Range}, _Txn, _Offset) -> throw("wip: range.integrate").

-spec encode_update(update()) -> binary().
encode_update(Update) ->
    LenBin = maps:size(Update#update.update_blocks),
    UpdateBins = [
        begin
            Offset =
                case UpdateBlocks of
                    [] -> 0;
                    [First | _] -> (bc_id(First))#id.clock
                end,
            <<
                (var_int:encode_uint(length(UpdateBlocks)))/binary,
                (state_vector:encode_client_id(ClientId))/binary,
                (var_int:encode_uint(Offset))/binary,
                (encode_update_blocks(UpdateBlocks))/binary
            >>
        end
     || {ClientId, UpdateBlocks} <- maps:to_list(Update#update.update_blocks)
    ],
    UpdateBin = list_to_binary(UpdateBins),
    DeleteSetBin = id_set:encode_id_set(Update#update.delete_set),
    <<(var_int:encode_uint(LenBin))/binary, UpdateBin/binary, DeleteSetBin/binary>>.

-spec encode_update_blocks([block_carrier()]) -> binary().
encode_update_blocks(Blocks) ->
    BlockBins = [
        <<(encode_block(Block))/binary>>
     || Block <- Blocks
    ],
    <<(list_to_binary(BlockBins))/binary>>.

-spec encode_block(block_carrier()) -> binary().
encode_block({skip, #block_range{len = Len}}) ->
    Info = ?BLOCK_SKIP_REF_NUMBER,
    <<Info:8, (var_int:encode_uint(Len))/binary>>;
encode_block({gc, #block_range{len = Len}}) ->
    Info = ?BLOCK_GC_REF_NUMBER,
    <<Info:8, (var_int:encode_uint(Len))/binary>>;
encode_block({item, Item}) ->
    Info = item:encode_info(Item),
    OriginBin =
        case Item#item.origin of
            {ok, Origin} -> id:encode_id(Origin);
            _ -> <<>>
        end,
    RightOriginBin =
        case Item#item.right_origin of
            {ok, RightOrigin} -> id:encode_id(RightOrigin);
            _ -> <<>>
        end,
    ParentBin =
        case {Item#item.origin, Item#item.right_origin} of
            {undefined, undefined} ->
                case Item#item.parent of
                    {unknown} -> <<>>;
                    {named, Name} -> <<1:8, (binary_encoding:encode_string(Name))/binary>>;
                    {id, Pid} -> id:encode_id(Pid)
                end;
            _ ->
                <<>>
        end,
    ParentSubBin =
        case Item#item.parent_sub of
            {ok, ParentSub} -> binary_encoding:encode_string(ParentSub);
            _ -> <<>>
        end,
    ContentBin = item_content:encode(Item#item.content),
    <<Info/binary, OriginBin/binary, RightOriginBin/binary, ParentBin/binary, ParentSubBin/binary,
        ContentBin/binary>>.

-spec decode_block(id:id(), binary()) -> {block_carrier(), binary()}.
decode_block(Id, Bin) ->
    {Info, Rest} = fixed_int:read_u8(Bin),
    case Info of
        ?BLOCK_SKIP_REF_NUMBER ->
            {Len, Rest0} = var_int:decode_uint(Rest),
            {{skip, #block_range{id = Id, len = Len}}, Rest0};
        ?BLOCK_GC_REF_NUMBER ->
            {Len, Rest0} = var_int:decode_uint(Rest),
            {{gc, #block_range{id = Id, len = Len}}, Rest0};
        Info ->
            CantCopyParentInfo = Info band (?HAS_RIGHT_ORIGIN bor ?HAS_ORIGIN) =:= 0,
            ?LOG_DEBUG("Info: ~p, CantCopyParentInfo: ~p", [Info, CantCopyParentInfo]),
            {Origin, RestO} =
                case Info band ?HAS_ORIGIN of
                    0 ->
                        {undefined, Rest};
                    _ ->
                        {Id2, RestId} = id:decode_id(Rest),
                        {{ok, Id2}, RestId}
                end,
            {RightOrigin, RestRO} =
                case Info band ?HAS_RIGHT_ORIGIN of
                    0 ->
                        {undefined, RestO};
                    _ ->
                        {Id3, RestId2} = id:decode_id(RestO),
                        {{ok, Id3}, RestId2}
                end,
            {Parent, RestPA} =
                case CantCopyParentInfo of
                    true ->
                        case var_int:decode_uint(RestRO) of
                            {1, RestPI} ->
                                {Name, RestName} = binary_encoding:decode_string(RestPI),
                                {{named, Name}, RestName};
                            {_, RestPI} ->
                                {Pid, RestPID} = id:decode_id(RestPI),
                                {{id, Pid}, RestPID}
                        end;
                    _ ->
                        {{unknown}, RestRO}
                end,
            {ParentSub, RestPS} =
                if
                    CantCopyParentInfo and ((Info band ?HAS_PARENT_SUB) =/= 0) ->
                        {PSub, RestPSub} = binary_encoding:decode_string(RestPA),
                        {{ok, PSub}, RestPSub};
                    true ->
                        {undefined, RestPA}
                end,

            ?LOG_DEBUG("Parent: ~p, ParentSub: ~p", [Parent, ParentSub]),
            {Content, RestItem} = item_content:decode(RestPS, Info),
            Item = item:new_item(
                Id,
                Origin,
                RightOrigin,
                undefined,
                undefined,
                Parent,
                ParentSub,
                Content
            ),
            {{item, Item}, RestItem}
    end.

-spec decode_blocks(integer(), state_vector:client_id(), integer(), binary()) ->
    {[block_carrier()], binary()}.
decode_blocks(Len, ClientId, Clock, Bin) ->
    {Rest, Blocks, _} = lists:foldl(
        fun(_I, {Bin1, Blocks, C}) ->
            Id = #id{client = ClientId, clock = C},
            {Block, Rest} = decode_block(Id, Bin1),
            Size = block_carrier_length(Block),
            case Size > 0 of
                true -> {Rest, [Block | Blocks], C + Size};
                false -> {Rest, Blocks, C}
            end
        end,
        {Bin, [], Clock},
        lists:seq(0, Len - 1)
    ),
    {lists:reverse(Blocks), Rest}.

-spec decode_update(binary()) -> {update(), binary()}.
decode_update(Bin) ->
    {Len, Rest} = var_int:decode_uint(Bin),
    {Clients, Rest1} = lists:foldl(
        fun(_I, {Clients, B}) ->
            {BlocksLen, Rest0} = var_int:decode_uint(B),

            {ClientIdInt, Rest1} = var_int:decode_uint(Rest0),
            ClientId = state_vector:integer_to_client_id(ClientIdInt),
            {Clock, Rest2} = var_int:decode_uint(Rest1),
            {Blocks, Rest3} = decode_blocks(BlocksLen, ClientId, Clock, Rest2),
            {
                maps:update_with(ClientId, fun(V) -> lists:append(V, Blocks) end, Blocks, Clients),
                Rest3
            }
        end,
        {#{}, Rest},
        lists:seq(0, Len - 1)
    ),
    {DeleteSet, Rest2} = id_set:decode_id_set(Rest1),
    {#update{update_blocks = Clients, delete_set = DeleteSet}, Rest2}.

% - ClientIdの小さいほうから順にBlockを適用していく。
% - Blockを適用する際に、Blockが依存するBlockがLocalSVに存在しない場合は、そのBlockをUnappliedBlockStackに積む。
% - その後依存先のBlockのClientIdのBlockを先に処理する (依存先のclockは考えずに小さいほうから処理すればよい)
% - LocalSVがBlockをまだ受け入れられないならreturn_stackでMissingSV, Remainingを更新する
% - 受け入れ可能かつ依存もすでに適用済みなら適用する
-spec integrate_loop(
    transaction:transaction_mut(),
    update:update(),
    option:option(update:block_carrier()),
    [update:block_carrier()],
    [state_vector:client_id()],
    state_vector:state_vector(),
    state_vector:state_vector(),
    update:update_blocks(),
    [update:block_carrier()],
    store:store()
) -> option:option(update:pending_update()).
integrate_loop(
    Txn,
    Update,
    CurBlock,
    CurTarget,
    ClientBlockIds,
    LocalSV,
    MissingSV,
    Remaining,
    UnappliedBlockStack,
    Store
) ->
    ?LOG_DEBUG(
        "CurBlock: ~p, CurTarget: ~p, ClientBlockIds: ~p, LocalSV: ~p, MissingSV: ~p, Remaining: ~p, UnappliedBlockStack: ~p",
        [CurBlock, CurTarget, ClientBlockIds, LocalSV, MissingSV, Remaining, UnappliedBlockStack]
    ),
    ?LOG_DEBUG(
        "store: ~p", [block_store:get_all(Store#store.blocks)]
    ),
    case CurBlock of
        undefined ->
            case Remaining of
                [] ->
                    undefined;
                _ ->
                    {ok, #pending_update{
                        update = #update{
                            update_blocks = Remaining,
                            delete_set = #{}
                        },
                        missing = MissingSV
                    }}
            end;
        {ok, Block} ->
            case Block of
                {skip, _} ->
                    {NewBlock, NewStack, NewTarget, ClientBlockIds, NewUpdate} = next(
                        UnappliedBlockStack,
                        CurTarget,
                        ClientBlockIds,
                        Update
                    ),
                    integrate_loop(
                        Txn,
                        NewUpdate,
                        NewBlock,
                        NewTarget,
                        ClientBlockIds,
                        LocalSV,
                        MissingSV,
                        Remaining,
                        NewStack,
                        Store
                    );
                _ ->
                    Id = bc_id(Block),
                    case state_vector:contains(LocalSV, Id) of
                        true ->
                            case missing(Block, LocalSV) of
                                % 未適用の依存がある場合
                                {ok, Dep} ->
                                    % いったんBlockはStackに積んで、Depの依存を処理する
                                    NewStack = [Block | UnappliedBlockStack],
                                    case maps:get(Dep, Update#update.update_blocks, undefined) of
                                        % Update内にDepの依存がない場合, Remainingに退避する
                                        undefined ->
                                            NewMissingSV = state_vector:set_min(
                                                MissingSV, Dep, state_vector:get(LocalSV, Dep)
                                            ),
                                            {NewUpdateBlocks, NewRemaining} = return_stack(
                                                UnappliedBlockStack,
                                                Update#update.update_blocks,
                                                Remaining
                                            ),
                                            NewUpdate = Update#update{
                                                update_blocks = NewUpdateBlocks
                                            },
                                            NewStack2 = [],
                                            {NewBlock, NewStack3, NewTarget, NewClientBlockIds,
                                                NewUpdate2} = next(
                                                NewStack2,
                                                CurTarget,
                                                ClientBlockIds,
                                                NewUpdate
                                            ),
                                            integrate_loop(
                                                Txn,
                                                NewUpdate2,
                                                NewBlock,
                                                NewTarget,
                                                NewClientBlockIds,
                                                LocalSV,
                                                NewMissingSV,
                                                NewRemaining,
                                                NewStack3,
                                                Store
                                            );
                                        Blocks ->
                                            % 次はdepのlistに状態遷移する
                                            {NewCurBlock, NewClinetBlocks} =
                                                case Blocks of
                                                    [] -> {undefined, []};
                                                    [B | Bs] -> {{ok, B}, Bs}
                                                end,
                                            integrate_loop(
                                                Txn,
                                                Update#update{
                                                    update_blocks = maps:put(
                                                        Dep,
                                                        NewClinetBlocks,
                                                        Update#update.update_blocks
                                                    )
                                                },
                                                NewCurBlock,
                                                CurTarget,
                                                ClientBlockIds,
                                                LocalSV,
                                                MissingSV,
                                                Remaining,
                                                NewStack,
                                                Store
                                            )
                                    end;
                                undefined ->
                                    Offset = state_vector:get(LocalSV, Id#id.client) - Id#id.clock,
                                    case Offset =:= 0 orelse Offset < block_carrier_length(Block) of
                                        false ->
                                            % 適用済み
                                            {NewBlock, NewStack, NewTarget, ClientBlockIds,
                                                NewUpdate} = next(
                                                UnappliedBlockStack,
                                                CurTarget,
                                                ClientBlockIds,
                                                Update
                                            ),
                                            integrate_loop(
                                                Txn,
                                                NewUpdate,
                                                NewBlock,
                                                NewTarget,
                                                ClientBlockIds,
                                                LocalSV,
                                                MissingSV,
                                                Remaining,
                                                NewStack,
                                                Store
                                            );
                                        true ->
                                            % 未適用でかつ適用可能
                                            Client = Id#id.client,
                                            NewLocalSV = state_vector:set_max(
                                                LocalSV,
                                                Client,
                                                Id#id.clock + block_carrier_length(Block)
                                            ),
                                            NewBlock =
                                                case Block of
                                                    {item, Item} ->
                                                        {item, store:repair(Store, Item)};
                                                    _ ->
                                                        Block
                                                end,
                                            ShouldDelete = bc_integrate(NewBlock, Txn, Offset),
                                            DeleteItem =
                                                case ShouldDelete of
                                                    true ->
                                                        case NewBlock of
                                                            {item, I} -> {ok, I};
                                                            _ -> undefined
                                                        end;
                                                    false ->
                                                        undefined
                                                end,
                                            DeleteItem2 =
                                                case NewBlock of
                                                    {item, Item2} ->
                                                        case Item2#item.parent of
                                                            {unknown} ->
                                                                store:push_gc(Store, #block_range{
                                                                    id = Id,
                                                                    len = block_carrier_length(
                                                                        NewBlock
                                                                    )
                                                                }),
                                                                undefined;
                                                            _ ->
                                                                DeleteItem
                                                        end;
                                                    {gc, Gc} ->
                                                        store:push_gc(Store, Gc),
                                                        DeleteItem;
                                                    {skip, _} ->
                                                        DeleteItem
                                                end,
                                            case DeleteItem2 of
                                                {ok, B} ->
                                                    transaction:delete_item(Txn, B);
                                                _ ->
                                                    ok
                                            end,
                                            {NewBlock2, NewStack, NewTarget, NewClientBlockIds,
                                                NewUpdate2} = next(
                                                UnappliedBlockStack,
                                                CurTarget,
                                                ClientBlockIds,
                                                Update
                                            ),
                                            integrate_loop(
                                                Txn,
                                                NewUpdate2,
                                                NewBlock2,
                                                NewTarget,
                                                NewClientBlockIds,
                                                NewLocalSV,
                                                MissingSV,
                                                Remaining,
                                                NewStack,
                                                Store
                                            )
                                    end
                            end;
                        false ->
                            Id = bc_id(Block),
                            NewMissingSV = state_vector:set_min(
                                MissingSV, Id#id.client, Id#id.clock
                            ),
                            NewStack = [Block | UnappliedBlockStack],
                            {NewUpdateBlocks, NewRemaining} = return_stack(
                                NewStack, Update#update.update_blocks, Remaining
                            ),
                            NewUpdate = Update#update{
                                update_blocks = NewUpdateBlocks
                            },
                            NewStack2 = [],
                            {NewBlock, NewStack3, NewTarget, NewClientBlockIds, NewUpdate2} = next(
                                NewStack2, CurTarget, ClientBlockIds, NewUpdate
                            ),
                            integrate_loop(
                                Txn,
                                NewUpdate2,
                                NewBlock,
                                NewTarget,
                                NewClientBlockIds,
                                LocalSV,
                                NewMissingSV,
                                NewRemaining,
                                NewStack3,
                                Store
                            )
                    end
            end
    end.

-spec next(
    [update:block_carrier()],
    [update:block_carrier()],
    [state_vector:client_id()],
    update()
) ->
    {
        option:option(update:block_carrier()),
        [update:block_carrier()],
        [update:block_carrier()],
        [state_vector:client_id()],
        update()
    }.
next(UnappliedBlockStack, CurTarget, ClientBlockIds, Update) ->
    case UnappliedBlockStack of
        [Head | Rest] ->
            {{ok, Head}, Rest, CurTarget, ClientBlockIds, Update};
        [] ->
            case CurTarget of
                [Head | Rest] ->
                    {{ok, Head}, [], Rest, ClientBlockIds, Update};
                [] ->
                    case next_target(ClientBlockIds, Update) of
                        {found, {_, [Head | Rest]}, NewClientBlockIds, NewUpdate} ->
                            {{ok, Head}, [], Rest, NewClientBlockIds, NewUpdate};
                        {not_found, {NewClientBlockIds, NewUpdate}} ->
                            {undefined, [], [], NewClientBlockIds, NewUpdate}
                    end
            end
    end.

-spec next_target([state_vector:client_id()], update()) ->
    {found, {state_vector:client_id(), [update:block_carrier()]}, [state_vector:client_id()],
        update()}
    | {not_found, {[state_vector:client_id()], update()}}.
next_target([], Update) ->
    {not_found, {[], Update}};
next_target([Id | ClientBlockIds], Update) ->
    case maps:get(Id, Update#update.update_blocks, undefined) of
        undefined ->
            next_target(ClientBlockIds, Update);
        [] ->
            next_target(ClientBlockIds, Update);
        Blocks = [_ | _] ->
            {found, {Id, Blocks}, ClientBlockIds, Update}
    end.

% Stack内の全itemの適用をあきらめて、UpdateBlocksの中のitemのClientIdの要素をすべてRemainingに移動する
-spec return_stack(
    [block_carrier()],
    update_blocks(),
    update_blocks()
) -> {update_blocks(), update_blocks()}.
return_stack(
    Stack,
    UpdateBlocks,
    Remaining
) ->
    lists:foldl(
        fun(Block, {AccUpdateBlocks, AccRemaining}) ->
            Client = (bc_id(Block))#id.client,
            case maps:take(Client, AccUpdateBlocks) of
                error ->
                    {AccUpdateBlocks, maps:put(Client, [Block], AccRemaining)};
                {Value, AccCurTarget2} ->
                    {AccCurTarget2, maps:put(Client, [Block | Value], AccRemaining)}
            end
        end,
        {UpdateBlocks, Remaining},
        Stack
    ).

-spec integrate(update(), transaction:transaction_mut()) ->
    {option:option(pending_update()), option:option(update())}.
integrate(Update, Txn) ->
    RemainingBlocks = begin
        ?LOG_DEBUG("Update: ~p", [Update#update.update_blocks]),
        case Update#update.update_blocks of
            M when map_size(M) =:= 0 ->
                undefined;
            _ ->
                begin
                    Blocks = Update#update.update_blocks,
                    [CurrentClientId | ClientBlockIds] = lists:sort(maps:keys(Blocks)),
                    {CurTarget, CurBlock} =
                        case maps:get(CurrentClientId, Update#update.update_blocks, undefined) of
                            undefined ->
                                {[], undefined};
                            U ->
                                case U of
                                    [B | Rest] -> {Rest, {ok, B}};
                                    [] -> {[], undefined}
                                end
                        end,
                    Store = transaction:get_store(Txn),
                    integrate_loop(
                        Txn,
                        Update,
                        CurBlock,
                        CurTarget,
                        ClientBlockIds,
                        store:get_state_vector(Store),
                        state_vector:new(),
                        #{},
                        [],
                        Store
                    )
                end
        end
    end,
    DeleteSet = transaction:apply_delete(Txn, Update#update.delete_set),
    RemainingDs = #update{delete_set = DeleteSet, update_blocks = #{}},
    {RemainingBlocks, {ok, RemainingDs}}.

-spec missing(block_carrier(), state_vector:state_vector()) ->
    option:option(state_vector:client_id()).
missing({item, Item}, LocalSV) ->
    maybe
        undefined ?=
            case Item#item.origin of
                {ok, Origin} ->
                    case
                        (Origin#id.client =/= Item#item.id#id.client andalso
                            Origin#id.clock >= state_vector:get(LocalSV, Origin#id.client))
                    of
                        true -> {ok, Origin#id.client};
                        false -> undefined
                    end;
                _ ->
                    undefined
            end,
        undefined ?=
            case Item#item.right_origin of
                {ok, RightOrigin} ->
                    case
                        (RightOrigin#id.client =/= Item#item.id#id.client andalso
                            RightOrigin#id.clock >=
                                state_vector:get(LocalSV, RightOrigin#id.client))
                    of
                        true ->
                            {ok, RightOrigin#id.client};
                        false ->
                            undefined
                    end;
                _ ->
                    undefined
            end,
        undefined ?=
            case Item#item.parent of
                {branch, Parent} ->
                    case Parent#branch.item of
                        {ok, ParentItem} ->
                            case
                                (ParentItem#id.client =/= Item#item.id#id.client andalso
                                    ParentItem#id.clock >=
                                        state_vector:get(LocalSV, ParentItem#id.client))
                            of
                                true -> {ok, ParentItem#id.client};
                                false -> undefined
                            end;
                        _ ->
                            undefined
                    end;
                {id, Parent} ->
                    case
                        (Parent#id.client =/= Item#item.id#id.client andalso
                            Parent#id.clock >= state_vector:get(LocalSV, Parent#id.client))
                    of
                        true -> {ok, Parent#id.client};
                        false -> undefined
                    end;
                _ ->
                    undefined
            end,
        % TODO: Move & weak
        undefined
    else
        Found -> Found
    end;
missing(_, _) ->
    undefined.

-spec merge_update([update()]) -> update().
merge_update(Updates) ->
    DeleteSet = lists:foldl(
        fun(U, Acc) ->
            id_set:merge_id_set(U#update.delete_set, Acc)
        end,
        #{},
        Updates
    ),
    UpdateBlocks = lists:foldl(
        fun(U, Acc) ->
            maps:fold(
                fun(Client, Blocks, Acc1) ->
                    maps:update_with(
                        Client,
                        fun(V) -> lists:append(V, Blocks) end,
                        Blocks,
                        Acc1
                    )
                end,
                U#update.update_blocks,
                Acc
            )
        end,
        #{},
        Updates
    ),

    #update{update_blocks = UpdateBlocks, delete_set = DeleteSet}.

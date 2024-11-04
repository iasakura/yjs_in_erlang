-module(transaction).

-export([
    new/1,
    add_changed_type/3,
    apply_update/2,
    apply_delete/2,
    get_store/1,
    get_delete_set/1,
    delete_item/2
]).
-export_type([transaction_mut/0, subdocs/0]).

-include_lib("kernel/include/logger.hrl").
-include("../include/constants.hrl").
-include("../include/records.hrl").

-type subdocs() :: #subdocs{}.

-opaque transaction_mut() :: pid().

-type transaction_mut_state() :: #transaction_mut{}.

-spec new(doc:doc()) -> transaction_mut().
new(Doc) ->
    spawn(fun() -> transaction_loop(new_state(Doc)) end).

-spec new_state(doc:doc()) -> transaction_mut_state().
new_state(Doc) ->
    #transaction_mut{
        store = Doc#doc.store,
        before_state = block_store:get_state_vector(Doc#doc.store#store.blocks),
        after_state = state_vector:new(),
        merge_blocks = [],
        delete_set = id_set:new(),
        prev_moved = #{},
        changed = #{},
        changed_parent_types = [],
        subdocs = undefined,
        doc = Doc,
        committed = false
    }.

-spec transaction_loop(transaction_mut_state()) -> no_return().
transaction_loop(State) ->
    receive
        {Pid, apply_delete, Txn, DeleteSet} ->
            DeleteSet0 = internal_apply_delete(State, DeleteSet),
            ?LOG_DEBUG("DeleteSet: ~p", [DeleteSet0]),
            Pid ! {Txn, DeleteSet0},
            transaction_loop(State);
        {Pid, add_changed_type, Txn, Parent, ParentSub} ->
            NewState = internal_add_changed_type(State, Parent, ParentSub),
            Pid ! {Txn, ok},
            transaction_loop(NewState);
        {Pid, get_store} ->
            Pid ! {self(), State#transaction_mut.store},
            transaction_loop(State);
        {Pid, get_delete_set} ->
            Pid ! {self(), State#transaction_mut.delete_set},
            transaction_loop(State);
        {Pid, set_store, NewStore} ->
            Pid ! {self(), ok},
            transaction_loop(State#transaction_mut{store = NewStore});
        {Pid, delete_item, Item} ->
            {Res, NewState} = internal_delete_item(State, Item),
            Pid ! {self(), Res},
            transaction_loop(NewState)
    end.

-spec apply_delete(transaction_mut(), update:delete_set()) -> update:delete_set().
apply_delete(Txn, DeleteSet) ->
    Txn ! {self(), apply_delete, Txn, DeleteSet},
    receive
        {Txn, DeleteSet} -> DeleteSet
    end.

-spec add_changed_type(transaction_mut(), branch:branch(), option:option(binary())) -> ok.
add_changed_type(Txn, Parent, ParentSub) ->
    Txn ! {self(), add_changed_type, Txn, Parent, ParentSub},
    receive
        {Txn, ok} -> ok
    end.

-spec get_store(transaction_mut()) -> store:store().
get_store(Txn) ->
    Txn ! {self(), get_store},
    receive
        {Txn, Store} -> Store
    end.
-spec set_store(transaction_mut(), store:store()) -> ok.
set_store(Txn, Store) ->
    Txn ! {self(), set_store, Store},
    receive
        {Txn, ok} -> ok
    end.

-spec get_delete_set(transaction_mut()) -> update:delete_set().
get_delete_set(Txn) ->
    Txn ! {self(), get_delete_set},
    receive
        {Txn, DeleteSet} -> DeleteSet
    end.

-spec delete_item(transaction_mut(), item:item()) -> ok.
delete_item(Txn, Item) ->
    Txn ! {self(), delete_item, Item},
    receive
        {Txn, Res} -> Res
    end.

-spec apply_update(transaction_mut(), update:update()) -> ok.
apply_update(Txn, Update) ->
    {Remaining, RemainingDs} = update:integrate(Update, Txn),
    Store = get_store(Txn),

    % storeのpendingの計算
    NewPending =
        case Store#store.pending of
            undefined ->
                Retry = false,
                Remaining;
            {ok, Pending} ->
                case
                    % Storeのclockがpendingよりも進んでいるなら再適用してみる。
                    maps:find(
                        fun(Client, Clock) ->
                            Clock < block_store:get_clock(Store#store.blocks, Client)
                        end,
                        Pending#pending_update.missing
                    )
                of
                    {ok, _} -> Retry = true;
                    _ -> Retry = false
                end,

                NewPending0 =
                    case Remaining of
                        {ok, RemainingOk} ->
                            NewMissing = maps:fold(
                                fun(Client, Clock, Acc) ->
                                    state_vector:set_min(
                                        Acc,
                                        Client,
                                        Clock
                                    )
                                end,
                                Pending#pending_update.missing,
                                RemainingOk#pending_update.missing
                            ),
                            NewUpdate = update:merge_update([
                                Pending#pending_update.update, RemainingOk#pending_update.update
                            ]),
                            #pending_update{update = NewUpdate, missing = NewMissing};
                        undefined ->
                            Pending
                    end,
                {ok, NewPending0}
        end,

    NewPendingDs =
        case Store#store.pending_ds of
            undefined ->
                % eqwalizer:ignore: Unbound rec: update
                option:map(fun(U) -> U#update.delete_set end, RemainingDs);
            {ok, PendingDs} ->
                Ds2 = apply_delete(Txn, PendingDs),
                case RemainingDs of
                    % eqwalizer:ignore: Unbound rec: update
                    {ok, Ds1} -> {ok, id_set:merge_id_set(Ds1#update.delete_set, Ds2)};
                    undefined -> {ok, Ds2}
                end
        end,

    set_store(Txn, Store#store{pending = NewPending, pending_ds = NewPendingDs}),

    case Retry of
        false ->
            ok;
        true ->
            case NewPending of
                {ok, NewPendingOk} ->
                    NewStore = Store#store{pending = NewPending, pending_ds = NewPendingDs},
                    PendingDs2 =
                        case NewStore#store.pending_ds of
                            undefined -> id_set:new();
                            {ok, P} -> P
                        end,
                    % eqwalizer:ignore: Unbound rec: update
                    DsUpdate = #update{delete_set = PendingDs2, update_blocks = []},
                    ok = apply_update(Txn, NewPendingOk#pending_update.update),
                    ok = apply_update(Txn, DsUpdate),
                    ok;
                undefined ->
                    ok
            end
    end.

-spec internal_delete_item(transaction:transaction_mut_state(), item:item()) ->
    {boolean(), transaction:transaction_mut_state()}.
internal_delete_item(Txn, Item) ->
    Store = Txn#transaction_mut.store,
    case item:is_deleted(Item) of
        true ->
            {false, Txn};
        false ->
            case option:is_none(Item#item.parent_sub) andalso item:is_countable(Item) of
                false ->
                    true;
                true ->
                    case Item#item.parent of
                        {branch, Branch} ->
                            store:put_branch(Store, Branch#branch{
                                block_len = Branch#branch.block_len - Item#item.len,
                                content_len = Branch#branch.content_len - item:content_len(Item)
                            }),
                            true;
                        _ ->
                            true
                    end
            end,
            % item.mark_as_deleted()
            store:put_item(
                Store,
                Item#item{
                    info = Item#item.info bor ?ITEM_FLAG_DELETED
                }
            ),
            Txn2 = Txn#transaction_mut{
                delete_set = id_set:insert(
                    Txn#transaction_mut.delete_set, Item#item.id, Item#item.len
                )
            },
            Txn3 =
                case Item#item.parent of
                    {branch, Parent} ->
                        internal_add_changed_type(
                            Txn, Parent, Item#item.parent_sub
                        );
                    _ ->
                        Txn2
                end,
            Recurse =
                case Item#item.content of
                    {type, Type} ->
                        store:delete_branch(Store, Type),
                        Start = Type#branch.start,
                        Loop = fun Loop(Cur, Acc) ->
                            case Cur of
                                undefined ->
                                    lists:reverse(Acc);
                                {ok, C} ->
                                    Loop(C#item.right, [C#item.id | Acc])
                            end
                        end,
                        Loop(Start, []) ++ maps:values(Type#branch.map);
                    % TODO: support move & subdocs
                    _ ->
                        []
                end,
            NewTxn = lists:foldl(
                fun(I, CurTxn) ->
                    case store:get_item(Store, I) of
                        {ok, Item} ->
                            {_, NewTxn0} = internal_delete_item(CurTxn, Item),
                            NewTxn0;
                        undefined ->
                            CurTxn
                    end
                end,
                Txn3,
                Recurse
            ),
            {true, NewTxn}
    end.

-spec delete_by_range(
    transaction:transaction_mut_state(), block_store:client_block_list(), integer(), integer()
) ->
    [range:range()].
delete_by_range(Txn, Blocks, Clock, ClockEnd) ->
    Store = Txn#transaction_mut.store,
    %  TODO: use lookup
    case Clock >= ClockEnd of
        true ->
            [];
        false ->
            case block_store:find_pivot(Blocks, Clock) of
                undefined ->
                    [#range{start = Clock, end_ = ClockEnd}];
                {ok, {_, {block, Item}}} ->
                    case item:is_deleted(Item) of
                        true ->
                            delete_by_range(
                                Txn, Blocks, Item#item.id#id.clock + Item#item.len, ClockEnd
                            );
                        false ->
                            case Item#item.id#id.clock + item:len(Item) > ClockEnd of
                                false ->
                                    {_, NewTxn} = internal_delete_item(Txn, Item),
                                    delete_by_range(
                                        NewTxn,
                                        Blocks,
                                        Item#item.id#id.clock + Item#item.len,
                                        ClockEnd
                                    );
                                true ->
                                    case
                                        item:splice(
                                            Store, Item, Clock - Item#item.id#id.clock
                                        )
                                    of
                                        undefined ->
                                            internal_delete_item(Txn, Item),
                                            delete_by_range(
                                                Txn, Blocks, Clock + Item#item.len, ClockEnd
                                            );
                                        {ok, {NewItem1, NewItem2}} ->
                                            NewTxn0 = Txn#transaction_mut{
                                                merge_blocks = [
                                                    NewItem1#item.id
                                                    | Txn#transaction_mut.merge_blocks
                                                ]
                                            },
                                            {_, NewTxn1} = internal_delete_item(NewTxn0, NewItem1),
                                            delete_by_range(
                                                NewTxn1,
                                                Blocks,
                                                NewItem2#item.id#id.clock,
                                                ClockEnd
                                            )
                                    end
                            end
                    end
            end
    end.

-spec internal_apply_delete(transaction_mut_state(), update:delete_set()) -> update:delete_set().
internal_apply_delete(State, DeleteSet) ->
    Store = State#transaction_mut.store,
    maps:filtermap(
        fun(ClientId, Range) ->
            Ranges = id_set:id_range_to_list(Range),
            case block_store:get_client(Store#store.blocks, ClientId) of
                undefined ->
                    false;
                {ok, Blocks} ->
                    case
                        lists:flatmap(
                            fun(#range{start = Clock, end_ = ClockEnd}) ->
                                delete_by_range(State, Blocks, Clock, ClockEnd)
                            end,
                            Ranges
                        )
                    of
                        [] -> false;
                        [R] -> {true, {continuous, R}};
                        Rs -> {true, {fragmented, Rs}}
                    end
            end
        end,
        DeleteSet
    ).

-spec internal_add_changed_type(transaction_mut_state(), branch:branch(), option:option(binary())) ->
    transaction_mut_state().
internal_add_changed_type(Txn, Parent, ParentSub) ->
    Trigger =
        case util:get_item_from_link(Txn#transaction_mut.store, Parent#branch.item) of
            {ok, Item} ->
                #id{client = Client, clock = Clock} = Item#item.id,
                TxnClock = maps:get(Client, Txn#transaction_mut.before_state),
                Clock < TxnClock andalso not item:is_deleted(Item);
            _ ->
                true
        end,
    case Trigger of
        true ->
            ChangedSet = maps:get(
                {branch, Parent}, Txn#transaction_mut.changed, sets:new([{version, 2}])
            ),
            Added = sets:add_element(ParentSub, ChangedSet),
            Txn#transaction_mut{
                changed = maps:put(branch:get_type_ptr(Parent), Added, Txn#transaction_mut.changed)
            };
        _ ->
            Txn
    end.

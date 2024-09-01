-module(transaction).

-export([add_changed_type/3, apply_update/2, apply_delete/2]).
-export_type([transaction_mut/0, subdocs/0]).

-include("../include/records.hrl").
-include("../include/constants.hrl").

-type subdocs() :: #subdocs{}.

-type transaction_mut() :: #transaction_mut{}.

-spec apply_update(transaction_mut(), update:update()) -> transaction_mut().
apply_update(Transaction, Update) ->
    {Txn, Remaining, RemainingDs} = update:integrate(Update, Transaction),
    Store = Transaction#transaction_mut.store,

    % storeのpendingの計算
    NewPending =
        case Store#store.pending of
            undefined ->
                Retry = false,
                Remaining;
            {ok, Pending} ->
                case
                    maps:find(
                        fun(Client, Clock) ->
                            Clock < block_store:get_clock(Store#store.blocks, Client)
                        end,
                        Pending#pending_update.missing
                    )
                of
                    {ok, _} -> Retry = false;
                    _ -> Retry = true
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
                option:map(fun(U) -> U#update.delete_set end, RemainingDs);
            {ok, PendingDs} ->
                Ds2 = apply_delete(Txn, PendingDs),
                case {RemainingDs, Ds2} of
                    {undefined, undefined} -> undefined;
                    {undefined, Ds} -> {ok, Ds};
                    {Ds, undefined} -> {ok, Ds};
                    {{ok, Ds1}, {ok, Ds2}} -> {ok, id_set:merge_id_set(Ds1#update.delete_set, Ds2)}
                end
        end,

    case Retry of
        false ->
            ok;
        true ->
            NewStore = Store#store{pending = NewPending, pending_ds = NewPendingDs},
            PendingDs2 =
                case NewStore#store.pending_ds of
                    undefined -> id_set:new();
                    {ok, P} -> P
                end,
            DsUpdate = #update{delete_set = PendingDs2, update_blocks = []},
            apply_update(Transaction, NewPending#pending_update.update),
            apply_update(Transaction, DsUpdate)
    end,

    Transaction.

-spec delete_item(transaction:transaction_mut(), item:item()) -> transaction:transaction_mut().
delete_item(Txn, Item) ->
    Store = Txn#transaction_mut.store,
    case item:is_deleted(Item) of
        true ->
            Txn;
        false ->
            case option:is_none(Item#item.parent_sub) andalso item:is_countable(Item) of
                false ->
                    Txn;
                true ->
                    case Item#item.parent of
                        undefined ->
                            ok;
                        {branch, Branch} ->
                            store:put_branch(Store, Branch#branch{
                                block_len = Branch#branch.block_len - Item#item.len,
                                content_len = Branch#branch.content_len - item:content_len(Item)
                            })
                    end,
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
                            undefined ->
                                Txn2;
                            {branch, Parent} ->
                                transaction:add_changed_type(Txn, Parent, Item#item.parent_sub)
                        end,
                    Recurse =
                        case Item#item.content of
                            % TODO: suuport subdoc
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
                                {ok, Item} -> delete_item(CurTxn, Item);
                                undefined -> CurTxn
                            end
                        end,
                        Txn3,
                        Recurse
                    ),
                    NewTxn
            end
    end.

-spec delete_by_range(
    transaction:transaction_mut(), block_store:client_block_list(), integer(), integer()
) ->
    [range:range()].
delete_by_range(Txn, Blocks, Clock, ClockEnd) ->
    Store = Txn#transaction_mut.store,
    case block_store:find_pivot(Blocks, Clock) of
        undefined ->
            [];
        {ok, {Key, {block, Item}}} ->
            case item:is_deleted(Item) of
                true ->
                    delete_by_range(Txn, Blocks, Item#item.id#id.clock + Item#item.len, ClockEnd);
                false ->
                    case Key < Clock of
                        false ->
                            delete_item(Txn, Item),
                            delete_by_range(
                                Txn, Blocks, Item#item.id#id.clock + Item#item.len, ClockEnd
                            );
                        true ->
                            {ok, {NewItem1, NewItem2}} = item:splice(
                                Store, Item, Clock - Item#item.id#id.clock
                            ),
                            delete_item(Txn, NewItem1),
                            delete_by_range(
                                Txn,
                                Blocks,
                                NewItem2#item.id#id.clock,
                                ClockEnd
                            )
                    end
            end;
        _ ->
            []
    end.

-spec apply_delete(transaction_mut(), update:delete_set()) -> update:delete_set().
apply_delete(Txn, DeleteSet) ->
    Store = Txn#transaction_mut.store,
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
                                delete_by_range(Txn, Blocks, Clock, ClockEnd)
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

-spec add_changed_type(transaction_mut(), branch:branch(), option:option(binary())) ->
    transaction_mut().
add_changed_type(Txn, Parent, ParentSub) ->
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
                changed = maps:put({branch, Parent}, Added, Txn#transaction_mut.changed)
            };
        _ ->
            Txn
    end.

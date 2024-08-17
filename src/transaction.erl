-module(transaction).

-export([add_changed_type/3, apply_update/2]).
-export_type([transaction_mut/0, subdocs/0]).

-include("../include/records.hrl").

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
                                    maps:update_with(
                                        Client, fun(C) -> min(C, Clock) end, Clock, Acc
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
            NewStore = Store#store{pending = {ok, NewPending}, pending_ds = NewPendingDs},
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

-spec delete_by_range(block_store:client_block_list(), integer(), integer()) -> [range:range()].
delete_by_range(Blocks, Clock, ClockEnd) ->
    case block_store:find_pivot(Blocks, Clock) of
        undefined -> [];
        {ok, {Key, Item}} ->
            



-spec apply_delete(transaction_mut(), update:delete_set()) -> update:delete_set().
apply_delete(Txn, DeleteSet) ->
    Store = Txn#transaction_mut.store,
    maps:filtermap(
        fun(ClientId, Range) ->
            Ranges = id_set:id_range_to_list(Range),
            case block_store:get_client(Store#store.blocks, ClientId) of
                undefined ->
                    false;
                Blocks ->
                    case
                        lists:flatmap(
                            fun(#range{start = Clock, end_ = ClockEnd}) ->
                                delete_by_range(Blocks, Clock, ClockEnd)
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

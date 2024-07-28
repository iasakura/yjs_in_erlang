-module(transaction).

-export([add_changed_type/3]).
-export_type([transaction_mut/0, subdocs/0]).

-include("../include/transaction.hrl").
-include("../include/branch.hrl").
-include("../include/item.hrl").
-include("../include/id.hrl").

-type subdocs() :: #subdocs{}.

-type transaction_mut() :: #transaction_mut{}.

-spec apply_update(transaction_mut(), update:update()) -> transaction_mut().
apply_update(Transaction, Update) -> true.

-spec add_changed_type(transaction_mut(), branch:branch(), option:option(binary())) ->
    transaction_mut().
add_changed_type(Transaction, Parent, ParentSub) ->
    Trigger =
        case Parent#branch.item of
            {ok, Item} ->
                #id{client = Client, clock = Clock} = Item#item.id,
                TxnClock = maps:get(Transaction#transaction_mut.before_state, Client),
                Clock < TxnClock andalso not item:is_deleted(Item);
            _ ->
                true
        end,
    case Trigger of
        true ->
            ChangedSet = maps:get(
                {branch, Parent}, Transaction#transaction_mut.changed, sets:new()
            ),
            Added = sets:add_element(ParentSub, ChangedSet),
            Transaction#transaction_mut{
                changed = maps:put({branch, Parent}, Added, Transaction#transaction_mut.changed)
            };
        _ ->
            true
    end.

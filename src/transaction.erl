-module(transaction).

-export_type([transaction_mut/0, subdocs/0]).

-include("../include/transaction.hrl").

-type subdocs() :: #subdocs{}.

-type transaction_mut() :: #transaction_mut{}.

-spec apply_update(transaction_mut(), update:update()) -> transaction_mut().
apply_update(Transaction, Update) -> 1.

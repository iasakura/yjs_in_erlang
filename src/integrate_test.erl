-module(integrate_test).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/records.hrl").

setup() ->
    logging:init().

cleanup(_) -> ok.

transition_test_() ->
    [{setup, fun setup/0, fun cleanup/1, [fun integrate_test_case1/0]}].

integrate_test_case1() ->
    Doc = doc:new(),
    Txn = transaction:new(Doc),
    {Update, Rest} = update:decode_update(
        <<1, 1, 176, 249, 159, 198, 7, 0, 40, 1, 0, 4, 107, 101, 121, 66, 1, 119, 6, 118, 97, 108,
            117, 101, 66, 0>>
    ),
    transaction:apply_update(Txn, Update),
    ?LOG_DEBUG("store: ~p", [block_store:get_all(Doc#doc.store#store.blocks)]),
    ok.

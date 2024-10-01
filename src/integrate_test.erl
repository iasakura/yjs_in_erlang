-module(integrate_test).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/records.hrl").

setup() ->
    logging:init().

cleanup(_) -> ok.

transition_test_() ->
    [
        {setup, fun setup/0, fun cleanup/1, [
            %fun integrate_test_case1/0,
            fun integrate_test_case2/0
            %fun integrate_test_case3/0
        ]}
    ].

integrate_test_case1() ->
    Doc = doc:new(),
    Txn = transaction:new(Doc),
    {Update, <<"">>} = update:decode_update(
        <<1, 1, 176, 249, 159, 198, 7, 0, 40, 1, 0, 4, 107, 101, 121, 66, 1, 119, 6, 118, 97, 108,
            117, 101, 66, 0>>
    ),
    transaction:apply_update(Txn, Update),
    ?LOG_DEBUG("store: ~p", [block_store:get_all(Doc#doc.store#store.blocks)]),
    ok.

integrate_test_case2() ->
    {ok, BinaryContent} = file:read_file("tests/test1.bin"),
    Doc = doc:new(),
    Txn = transaction:new(Doc),
    {Update, <<"">>} = update:decode_update(BinaryContent),
    transaction:apply_update(Txn, Update),
    ?LOG_DEBUG("store: ~p", [block_store:get_all(Doc#doc.store#store.blocks)]),
    Text = doc:get_or_create_text(Doc, <<"text">>),
    ?assertEqual(
        <<16#30, 16#42, 16#30, 16#44, 16#30, 16#46, 16#61, 16#62, 16#63, 16#30, 16#48, 16#30,
            16#4a>>,
        text:get_string(Text)
    ),
    ok.

integrate_test_case3() ->
    {ok, A} = file:read_file("tests/test2-a.bin"),
    {ok, B} = file:read_file("tests/test2-b.bin"),
    {UpdateA, <<"">>} = update:decode_update(A),
    {UpdateB, <<"">>} = update:decode_update(B),
    Doc = doc:new(),
    Txn = transaction:new(Doc),
    transaction:apply_update(Txn, UpdateA),
    transaction:apply_update(Txn, UpdateB),
    ?LOG_DEBUG("store: ~p", [block_store:get_all(Doc#doc.store#store.blocks)]),
    Text = doc:get_or_create_text(Doc, <<"text">>),
    ?assertEqual(<<"abcxyz">>, text:get_string(Text)),
    ok.

-module(transaction_test).

-include("../include/records.hrl").
-include_lib("eunit/include/eunit.hrl").

transaction_test() ->
    logging:init(),
    Doc = doc:new(),
    YText = doc:get_or_create_text(Doc, <<"text">>),
    YTxn = doc:transact_mut(Doc),
    doc:subscribe_update_v1(Doc),

    ClientId = state_vector:integer_to_client_id(0),

    text:insert(YTxn, id:new(ClientId, 0), YText, 0, <<"test">>),

    transaction:commit(YTxn),

    receive
        {notify, update_v1, Update} ->
            ?assertEqual(maps:size(Update#update.update_blocks), 1),
            #{0 := [{item, Block}]} = Update#update.update_blocks,
            ?assertEqual(Block#item.content, {string, <<"test">>})
    after 5000 -> throw("timeout")
    end.

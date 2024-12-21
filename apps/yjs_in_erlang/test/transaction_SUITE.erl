-module(transaction_SUITE).

-include("../include/records.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, transaction_tests/1]).

all() -> [transaction_tests].

transaction_tests(_Config) ->
    logging:init(),
    Doc = doc:new(),
    YText = doc:get_or_create_text(Doc, <<"text">>),
    YTxn1 = doc:transact_mut(Doc),
    doc:subscribe_update_v1(Doc),

    ClientId1 = state_vector:integer_to_client_id(0),
    ClientId2 = state_vector:integer_to_client_id(1),
    ClientId3 = state_vector:integer_to_client_id(2),

    text:insert(YTxn1, id:new(ClientId1, 0), YText, 0, <<"before0">>),
    text:insert(YTxn1, id:new(ClientId2, 0), YText, 0, <<"before1">>),
    text:insert(YTxn1, id:new(ClientId3, 0), YText, 0, <<"before2">>),

    transaction:commit(YTxn1),

    YTxn2 = doc:transact_mut(Doc),

    text:insert(YTxn2, id:new(ClientId1, 7), YText, 0, <<"after0">>),
    text:insert(YTxn2, id:new(ClientId2, 7), YText, 0, <<"after1">>),
    text:insert(YTxn2, id:new(ClientId3, 7), YText, 0, <<"after2">>),

    transaction:commit(YTxn2),

    receive
        {notify, update_v1, Update, YTxn1} ->
            3 = maps:size(Update#update.update_blocks),
            #{0 := [{item, Block0}], 1 := [{item, Block1}], 2 := [{item, Block2}]} =
                Update#update.update_blocks,
            {string, <<"before0">>} = Block0#item.content,
            {string, <<"before1">>} = Block1#item.content,
            {string, <<"before2">>} = Block2#item.content
    after 5000 -> throw("timeout")
    end,

    receive
        {notify, update_v1, UpdateAfter, YTxn2} ->
            3 = maps:size(UpdateAfter#update.update_blocks),
            #{0 := [{item, BlockAfter0}], 1 := [{item, BlockAfter1}], 2 := [{item, BlockAfter2}]} =
                UpdateAfter#update.update_blocks,
            {string, <<"after0">>} = BlockAfter0#item.content,
            {string, <<"after1">>} = BlockAfter1#item.content,
            {string, <<"after2">>} = BlockAfter2#item.content
    after 5000 -> throw("timeout")
    end.

-module(ytext_test).

-include_lib("eunit/include/eunit.hrl").
% -include("../include/records.hrl").

setup() ->
    logging:init().

cleanup(_) -> ok.

ytext_test_() ->
    [
        {setup, fun setup/0, fun cleanup/1, [
            fun test_case1/0,
            fun test_case2/0,
            fun test_case3/0,
            fun test_case4/0,
            fun test_case5/0,
            fun test_case6/0,
            fun test_case7/0
        ]}
    ].

test_case1() ->
    Doc = doc:new(),
    Text = doc:get_or_create_text(Doc, <<"text">>),
    YTxn = doc:transact_mut(Doc),
    ClientId = state_vector:integer_to_client_id(0),
    text:insert(YTxn, id:new(ClientId, 0), Text, 0, <<"01">>),
    text:insert(YTxn, id:new(ClientId, 2), Text, 1, <<"2">>),
    text:insert(YTxn, id:new(ClientId, 3), Text, 1, <<"3">>),
    ?assertEqual(<<"0321">>, text:get_string(Text)).

test_case2() ->
    Doc = doc:new(),
    Text = doc:get_or_create_text(Doc, <<"text">>),
    YTxn = doc:transact_mut(Doc),
    ClientId = state_vector:integer_to_client_id(0),
    text:insert(YTxn, id:new(ClientId, 0), Text, 0, <<"01">>),
    text:delete(YTxn, Text, 1, 1),
    text:insert(YTxn, id:new(ClientId, 2), Text, 1, <<"2">>),
    ?assertEqual(<<"02">>, text:get_string(Text)).

test_case3() ->
    Doc = doc:new(),
    Text = doc:get_or_create_text(Doc, <<"text">>),
    YTxn = doc:transact_mut(Doc),
    ClientId = state_vector:integer_to_client_id(0),
    text:insert(YTxn, id:new(ClientId, 0), Text, 0, <<"01">>),
    text:delete(YTxn, Text, 0, 1),
    ?assertEqual(<<"1">>, text:get_string(Text)).

test_case4() ->
    Doc = doc:new(),
    Text = doc:get_or_create_text(Doc, <<"text">>),
    YTxn = doc:transact_mut(Doc),
    ClientId = state_vector:integer_to_client_id(0),
    text:insert(YTxn, id:new(ClientId, 0), Text, 0, <<"0">>),
    text:insert(YTxn, id:new(ClientId, 1), Text, 0, <<"12">>),
    text:delete(YTxn, Text, 1, 1),
    ?assertEqual(<<"10">>, text:get_string(Text)).

test_case5() ->
    Doc = doc:new(),
    Text = doc:get_or_create_text(Doc, <<"text">>),
    YTxn = doc:transact_mut(Doc),
    ClientId = state_vector:integer_to_client_id(0),
    text:insert(YTxn, id:new(ClientId, 0), Text, 0, <<"0">>),
    text:insert(YTxn, id:new(ClientId, 1), Text, 0, <<"123">>),
    text:delete(YTxn, Text, 2, 1),
    ?assertEqual(<<"120">>, text:get_string(Text)).

test_case6() ->
    Doc = doc:new(),
    Text = doc:get_or_create_text(Doc, <<"text">>),
    YTxn = doc:transact_mut(Doc),
    ClientId = state_vector:integer_to_client_id(0),
    text:insert(YTxn, id:new(ClientId, 0), Text, 0, <<"01">>),
    text:insert(YTxn, id:new(ClientId, 2), Text, 1, <<"2">>),
    text:delete(YTxn, Text, 0, 2),
    ?assertEqual(<<"1">>, text:get_string(Text)).

test_case7() ->
    Doc = doc:new(),
    Text = doc:get_or_create_text(Doc, <<"text">>),
    YTxn = doc:transact_mut(Doc),
    ClientId = state_vector:integer_to_client_id(0),
    text:insert(YTxn, id:new(ClientId, 0), Text, 0, <<"0">>),
    text:insert(YTxn, id:new(ClientId, 1), Text, 0, <<"12">>),
    text:delete(YTxn, Text, 2, 1),
    ?assertEqual(<<"12">>, text:get_string(Text)).

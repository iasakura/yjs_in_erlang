-module(update_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/records.hrl").

setup() ->
    logging:init().

cleanup(_) -> ok.

transition_test_() ->
    [
        {setup, fun setup/0, fun cleanup/1, [
            fun decode_update_test_case1/0, fun encode_update_test_case1/0
        ]}
    ].

decode_update_test_case1() ->
    Res = update:decode_update(
        <<1, 1, 176, 249, 159, 198, 7, 0, 40, 1, 0, 4, 107, 101, 121, 66, 1, 119, 6, 118, 97, 108,
            117, 101, 66, 0>>
    ),
    Expected = {
        #update{
            update_blocks = #{
                state_vector:integer_to_client_id(2026372272) =>
                    [
                        {item, #item{
                            id = #id{
                                client = state_vector:integer_to_client_id(2026372272), clock = 0
                            },
                            len = 1,
                            left = undefined,
                            right = undefined,
                            origin = undefined,
                            right_origin = undefined,
                            content = {any, [{string, <<"valueB">>}]},
                            parent = {named, <<>>},
                            redone = undefined,
                            parent_sub = {ok, <<"keyB">>},
                            moved = undefined,
                            info = 2
                        }}
                    ]
            },
            delete_set = #{}
        },
        <<>>
    },
    ?assertEqual(
        Expected,
        Res
    ).

encode_update_test_case1() ->
    Update = #update{
        update_blocks = #{
            state_vector:integer_to_client_id(2026372272) =>
                [
                    {item, #item{
                        id = #id{client = state_vector:integer_to_client_id(2026372272), clock = 0},
                        len = 1,
                        left = undefined,
                        right = undefined,
                        origin = undefined,
                        right_origin = undefined,
                        content = {any, [{string, <<"valueB">>}]},
                        parent = {named, <<>>},
                        redone = undefined,
                        parent_sub = {ok, <<"keyB">>},
                        moved = undefined,
                        info = 2
                    }}
                ]
        },
        delete_set = #{}
    },
    Res = update:encode_update(Update),
    Expected =
        <<1, 1, 176, 249, 159, 198, 7, 0, 40, 1, 0, 4, 107, 101, 121, 66, 1, 119, 6, 118, 97, 108,
            117, 101, 66, 0>>,
    ?assertEqual(
        Expected,
        Res
    ).

-module(update_test).

-include("../include/update.hrl").
-include("../include/item.hrl").
-include("../include/id.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
    logging:init().

cleanup(_) -> ok.

transition_test_() ->
    [{setup, fun setup/0, fun cleanup/1, [fun decode_update_test_case1/0]}].

decode_update_test_case1() ->
    Res = update:decode_update(
        <<1, 1, 176, 249, 159, 198, 7, 0, 40, 1, 0, 4, 107, 101, 121, 66, 1, 119, 6, 118, 97, 108,
            117, 101, 66, 0>>
    ),
    ?assertEqual(
        {
            #update{
                update_blocks = #{
                    20736530432 =>
                        [
                            {item, #item{
                                id = #id{client = 20736530432, clock = 0},
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
        Res
    ).

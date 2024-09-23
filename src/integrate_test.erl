-module(integrate_test).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    logging:init().

cleanup(_) -> ok.

transition_test_() ->
    [{setup, fun setup/0, fun cleanup/1, [fun integrate_test_case1/0]}].

integrate_test_case1() ->
    Doc = doc:new(),
    ok.

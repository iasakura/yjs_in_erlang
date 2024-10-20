%%%-------------------------------------------------------------------
%% @doc y_erl public API
%% @end
%%%-------------------------------------------------------------------

-module(y_erl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    y_erl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

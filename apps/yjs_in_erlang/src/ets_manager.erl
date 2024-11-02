-module(ets_manager).

-opaque ets_manager() :: pid().

-export([
    new/0,
    new_ets/3
]).
-export_type([ets_manager/0]).

-spec new() -> ets_manager().
new() ->
    spawn(fun() ->
        loop()
    end).

-spec loop() -> no_return().
loop() ->
    receive
        {From, {new_ets, Name, Options}} ->
            Table = ets:new(Name, Options),
            From ! {self(), Table},
            loop()
    end.

% copied from ets.erl
-spec new_ets(Manager, Name, Options) -> ets:table() when
    Manager :: ets_manager(),
    Name :: atom(),
    Options :: [Option],
    Option ::
        Type
        | Access
        | named_table
        | {keypos, Pos}
        | {heir, Pid :: pid(), HeirData}
        | {heir, none}
        | Tweaks,
    Type :: ets:table_type(),
    Access :: ets:table_access(),
    WriteConcurrencyAlternative :: boolean() | auto,
    Tweaks ::
        {write_concurrency, WriteConcurrencyAlternative}
        | {read_concurrency, boolean()}
        | {decentralized_counters, boolean()}
        | compressed,
    Pos :: pos_integer(),
    HeirData :: term().

new_ets(Manager, Name, Options) ->
    Manager ! {self(), {new_ets, Name, Options}},
    receive
        {Manager, Table} -> Table
    end.

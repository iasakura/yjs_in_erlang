-module(block).

-export([id_range/1]).
-export_type([block_cell/0]).

-include("../include/records.hrl").

-type gc() :: #gc{}.

-type block_cell() :: {gc, gc()} | {block, item:item()}.

-spec id_range(block_cell()) -> {integer(), integer()}.
id_range({block, Item}) -> {Item#item.id#id.clock, Item#item.len};
id_range({gc, Gc}) -> {Gc#gc.start, Gc#gc.end_ - Gc#gc.start}.

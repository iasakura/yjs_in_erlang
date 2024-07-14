-module(block).

-export_type([block_cell/0]).

-include("../include/block.hrl").

-type gc() :: #gc{}.

-type block_cell() :: {gc, gc()} | {block, item:item()}.

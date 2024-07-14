-module(block_store).

-export_type([block_store/0]).

-include("../include/block_store.hrl").

-type block_store() :: #block_store{}.

-type client_block_list() :: #client_block_list{}.

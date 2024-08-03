% -record(block_store, {clients :: #{state_vector:client_id() => block_store:client_block_list()}}).

-record(block_store_item, {client :: state_vector:client_id(), table :: ets:table()}).

-record(client_block_cell, {list :: [block:block_cell()]}).

-record(client_block, {start :: integer(), cell :: block:block_cell()}).

-record(item_slice, {item :: item:item(), start :: integer(), end_ :: integer()}).

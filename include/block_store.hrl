-record(block_store, {clients :: #{state_vector:client_id() => block_store:client_block_list()}}).

-record(client_block_list, {list :: [block:block_cell()]}).

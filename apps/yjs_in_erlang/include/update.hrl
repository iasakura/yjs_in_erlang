-record(update, {update_blocks :: update:update_blocks(), delete_set :: update:delete_set()}).

-record(pending_update, {update :: update:update(), missing :: state_vector:state_vector()}).

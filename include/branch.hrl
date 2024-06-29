% WIP
-record(branch, {
    start :: option:option(id:id()),
    map :: #{string() => id:id()},
    item :: option:option(id:id()),
    name :: option:option(string()),
    block_len :: integer(),
    content_len :: integer(),
    type_ref :: type_ref:type_ref()
    % observers
    % deep_observers
}).

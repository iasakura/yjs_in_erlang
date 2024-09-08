-record(branch, {
    start :: option:option(id:id()),
    map :: #{binary() => id:id()},
    item :: option:option(id:id()),
    name :: option:option(binary()),
    block_len :: integer(),
    content_len :: integer(),
    type_ref :: type_ref:type_ref()
    % TODO: observers
    % observers
    % deep_observers
}).

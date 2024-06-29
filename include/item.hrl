-record(item, {
    id :: id:id(),
    len :: integer(),
    left :: option:option(id:id()),
    right :: option:option(id:id()),
    origin :: option:option(id:id()),
    right_origin :: option:option(id:id()),
    content :: item_content:item_content(),
    parent :: type_ptr:type_ptr(),
    redone :: option:option(id:id()),
    parent_sub :: option:option(string()),
    moved :: option:option(id:id()),
    info :: item_flags:item_flags()
}).

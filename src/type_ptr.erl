-module(type_ptr).

-export_type([type_ptr/0]).

-type type_ptr() ::
    {unknown} | {branch, branch:branch()} | {named, binary()} | {id, id:id()}.

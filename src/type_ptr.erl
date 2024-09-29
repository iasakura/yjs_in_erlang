-module(type_ptr).

-export_type([type_ptr/0]).

-type type_ptr() ::
    {unknown}
    % | {branch, branch:branch()} % branchに書き換えがあったときに検出できないため削除
    | {named, binary()}
    | {id, id:id()}.

-module(types).

-export_type([branch_ptr/0, type_ptr/0]).

-include("../include/branch.hrl").

-type branch_ptr() :: #branch{}.

-type type_ptr() :: {unknown} | {branch, branch_ptr()} | {named, string()} | {id, id:id()}.

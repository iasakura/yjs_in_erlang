-module(branch).

-export([new_branch/1]).
-export_type([branch/0]).

-include("../include/branch.hrl").

-type branch() :: #branch{}.

-spec new_branch(type_ref:type_ref()) -> branch().
new_branch(TypeRef) ->
    #branch{
        start = undefined,
        map = #{},
        item = undefined,
        name = undefined,
        block_len = 0,
        content_len = 0,
        type_ref = TypeRef
    }.

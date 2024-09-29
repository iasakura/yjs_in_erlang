-module(branch).

-export([new_branch/1, get_type_ptr/1]).
-export_type([branch/0]).

-include("../include/records.hrl").

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

-spec get_type_ptr(branch()) -> type_ptr:type_ptr().
get_type_ptr(Branch) ->
    case Branch#branch.item of
        undefined ->
            case Branch#branch.name of
                {ok, Name} -> {named, Name};
                undefined -> throw("branch has no key")
            end;
        {ok, Id} ->
            {id, Id}
    end.

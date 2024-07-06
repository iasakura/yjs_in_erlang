-module(item_content).

-export([countable/1, decode/2, len/1]).
-export_type([item_content/0]).

-include("../include/constants.hrl").

-type item_content() ::
    %    {any, any_type()} |
    {binary, binary()}
    | {deleted, integer()}
    % {doc, option:option(doc()), doc()}
    % | {json, [json()]}
    % {embed, any_type()} |
    % | {format, string(), any_type()}
    % wip: correct?
    | {string, binary()}
    | {type, branch:branch()}.
% | {move, move()}.

-spec countable(item_content()) -> boolean().
countable({binary, _}) -> true;
countable({deleted, _}) -> false;
countable({string, _}) -> true;
countable({type, _}) -> true.

-spec len(item_content()) -> integer().
len({delete, D}) -> D;
len({string, S}) -> byte_size(S);
len(_) -> 1.

-spec decode(binary(), integer()) -> {item_content(), binary()}.
decode(Bin, RefNum) ->
    case RefNum band 2#1111 of
        ?BLOCK_ITEM_BINARY_REF_NUMBER ->
            {Buf, Rest} = string:read_buf(Bin),
            {{binary, Buf}, Rest};
        ?BLOCK_ITEM_DELETED_REF_NUMBER ->
            {Len, Rest} = var_int:decode_uint(Bin),
            {{deleted, Len}, Rest};
        ?BLOCK_ITEM_STRING_REF_NUMBER ->
            {Buf, Rest} = string:read_string(Bin),
            {{string, Buf}, Rest};
        ?BLOCK_ITEM_TYPE_REF_NUMBER ->
            {TypeRef, Rest} = type_ref:decode_type_ref(Bin),
            {{type, branch:new_branch(TypeRef)}, Rest}
    end.

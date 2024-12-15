-module(item_content).

-export([countable/1, decode/2, encode/1, len/1, split/2, get_ref_number/1]).
-export_type([item_content/0]).

-include("../include/constants.hrl").
-include("../include/records.hrl").
-include_lib("kernel/include/logger.hrl").

-type item_content() ::
    {any, [any:any_type()]}
    | {binary, binary()}
    | {deleted, integer()}
    % {doc, option:option(doc()), doc()}
    % | {json, [json()]}
    % {embed, any_type()} |
    % | {format, string(), any_type()}
    | {string, binary()}
    | {type, branch:branch()}.
% | {move, move()}.

-spec countable(item_content()) -> boolean().
countable({binary, _}) -> true;
countable({deleted, _}) -> false;
countable({string, _}) -> true;
countable({type, _}) -> true;
countable({any, _}) -> true.

-spec len(item_content()) -> integer().
len({deleted, D}) -> D;
len({string, S}) -> util:compute_utf16_length(S);
% todo: any / json
len(_) -> 1.

-spec decode(binary(), integer()) -> {item_content(), binary()}.
decode(Bin, RefNum) ->
    % Show RefNum in hex format.

    case RefNum band 2#1111 of
        ?BLOCK_ITEM_BINARY_REF_NUMBER ->
            {Buf, Rest} = binary_encoding:decode_buf(Bin),
            {{binary, Buf}, Rest};
        ?BLOCK_ITEM_DELETED_REF_NUMBER ->
            {Len, Rest} = var_int:decode_uint(Bin),
            {{deleted, Len}, Rest};
        ?BLOCK_ITEM_STRING_REF_NUMBER ->
            {Buf, Rest} = binary_encoding:decode_string(Bin),
            {{string, Buf}, Rest};
        ?BLOCK_ITEM_TYPE_REF_NUMBER ->
            {TypeRef, Rest} = type_ref:decode_type_ref(Bin),
            {{type, branch:new_branch(TypeRef)}, Rest};
        ?BLOCK_ITEM_ANY_REF_NUMBER ->
            {Len, Rest} = var_int:decode_uint(Bin),
            Loop = fun Loop(N, Acc, Rest0) ->
                case N of
                    Len ->
                        {lists:reverse(Acc), Rest0};
                    _ ->
                        {Item, Rest1} = any:decode_any(Rest0),
                        Loop(N + 1, [Item | Acc], Rest1)
                end
            end,
            {Values, Rest1} = Loop(0, [], Rest),
            {{any, Values}, Rest1}
    end.

-spec encode(item_content()) -> binary().
encode({binary, Bin}) ->
    binary_encoding:encode_buf(Bin);
encode({deleted, Len}) ->
    var_int:encode_uint(Len);
encode({string, Str}) ->
    binary_encoding:encode_string(Str);
encode({type, Type}) ->
    type_ref:encode_type_ref(Type#branch.type_ref);
encode({any, Values}) ->
    Len = length(Values),
    ValueBins = [any:encode_any(Value) || Value <- Values],
    <<(var_int:encode_uint(Len))/binary, (list_to_binary(ValueBins))/binary>>.

-spec split(item_content(), integer()) -> option:option({item_content(), item_content()}).
split({any, Value}, Offset) ->
    {Left, Right} = lists:split(Offset, Value),
    {ok, {{any, Left}, {any, Right}}};
split({string, String}, Offset) ->
    UTF16Offset = util:compute_utf16_offset(String, Offset),
    Left = binary:part(String, 0, UTF16Offset),
    Right = binary:part(String, UTF16Offset, byte_size(String) - UTF16Offset),
    {ok, {{string, Left}, {string, Right}}};
split({deleted, Len}, Offset) ->
    case Len >= Offset of
        true -> {ok, {{deleted, Offset}, {deleted, Len - Offset}}};
        false -> undefined
    end;
split(_, _) ->
    undefined.

-spec get_ref_number(item_content()) -> integer().
get_ref_number({binary, _}) -> ?BLOCK_ITEM_BINARY_REF_NUMBER;
get_ref_number({deleted, _}) -> ?BLOCK_ITEM_DELETED_REF_NUMBER;
get_ref_number({string, _}) -> ?BLOCK_ITEM_STRING_REF_NUMBER;
get_ref_number({type, _}) -> ?BLOCK_ITEM_TYPE_REF_NUMBER;
get_ref_number({any, _}) -> ?BLOCK_ITEM_ANY_REF_NUMBER.

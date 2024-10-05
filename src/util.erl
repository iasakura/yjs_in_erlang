-module(util).

-export([
    get_item_from_link/2, compute_utf16_offset/2, compute_utf16_length/1, read_next_utf8_codepoint/1
]).

-spec get_item_from_link(store:store(), option:option(id:id())) -> option:option(item:item()).
get_item_from_link(Store, Link) ->
    case Link of
        {ok, Id} -> store:get_item(Store, Id);
        _ -> undefined
    end.

-spec read_next_utf8_codepoint(binary()) ->
    option:option({non_neg_integer(), non_neg_integer(), binary()}).
read_next_utf8_codepoint(<<2#0:1, Codepoint:7, Rest/binary>>) ->
    {ok, {Codepoint, 1, Rest}};
read_next_utf8_codepoint(<<2#110:3, Part1:5, 2#10:2, Part2:6, Rest/binary>>) ->
    Codepoint = (Part1 bsl 6) bor Part2,
    {ok, {Codepoint, 2, Rest}};
read_next_utf8_codepoint(<<2#1110:4, Part1:4, 2#10:2, Part2:6, 2#10:2, Part3:6, Rest/binary>>) ->
    Codepoint = (Part1 bsl 12) bor (Part2 bsl 6) bor Part3,
    {ok, {Codepoint, 3, Rest}};
read_next_utf8_codepoint(
    <<2#11110:5, Part1:3, 2#10:2, Part2:6, 2#10:2, Part3:6, 2#10:2, Part4:6, Rest/binary>>
) ->
    Codepoint = (Part1 bsl 18) bor (Part2 bsl 12) bor (Part3 bsl 6) bor Part4,
    {ok, {Codepoint, 4, Rest}};
read_next_utf8_codepoint(_) ->
    undefined.

-spec compute_utf16_offset(binary(), non_neg_integer()) -> non_neg_integer().
compute_utf16_offset(Bin, Offset) ->
    compute_utf16_offset(Bin, Offset, 0).

-spec compute_utf16_offset(binary(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
compute_utf16_offset(_, 0, Acc) ->
    Acc;
compute_utf16_offset(Bin, Offset, Acc) ->
    case read_next_utf8_codepoint(Bin) of
        {ok, {C, L, Rest}} ->
            Utf16Len =
                case C >= 16#10000 of
                    true -> 2;
                    false -> 1
                end,
            compute_utf16_offset(Rest, Offset - Utf16Len, Acc + L);
        undefined ->
            throw({error, "invalid utf8 binary", Bin})
    end.

-spec compute_utf16_length(binary()) -> non_neg_integer().
compute_utf16_length(Binary) ->
    compute_utf16_length(Binary, 0).

-spec compute_utf16_length(binary(), non_neg_integer()) -> non_neg_integer().
compute_utf16_length(<<>>, Acc) ->
    Acc;
compute_utf16_length(Bin, Acc) ->
    case read_next_utf8_codepoint(Bin) of
        {ok, {C, _, Rest}} ->
            Utf16Len =
                case C >= 16#10000 of
                    true -> 2;
                    false -> 1
                end,
            compute_utf16_length(Rest, Acc + Utf16Len);
        undefined ->
            throw({error, "invalid utf8 binary", Bin})
    end.

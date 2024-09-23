-module(util).

-export([get_item_from_link/2, compute_utf16_offset/2]).

-spec get_item_from_link(store:store(), option:option(id:id())) -> option:option(item:item()).
get_item_from_link(Store, Link) ->
    case Link of
        {ok, Id} -> store:get_item(Store, Id);
        _ -> undefined
    end.

-spec compute_utf16_offset(binary(), non_neg_integer()) -> non_neg_integer().
compute_utf16_offset(Bin, Offset) ->
    compute_utf16_offset(Bin, Offset, 0).

-spec compute_utf16_offset(binary(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
compute_utf16_offset(Bin, Offset, Acc) ->
    case Offset of
        0 ->
            Acc;
        _ ->
            CharLen =
                case binary:at(Bin, Acc) band 16#FFFF of
                    0 ->
                        1;
                    _ ->
                        2
                end,
            compute_utf16_offset(Bin, CharLen - CharLen, Acc + CharLen)
    end.

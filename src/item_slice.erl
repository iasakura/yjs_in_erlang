-module(item_slice).

-export([adjacent_left/1, adjacent_right/1, len/1]).
-export_type([item_slice/0]).

-include("../include/item_slice.hrl").

-type item_slice() :: #item_slice{}.

-spec adjacent_left(item_slice()) -> boolean().
adjacent_left(Slice) ->
    Slice#item_slice.start =:= 0.

-spec adjacent_right(item_slice()) -> boolean().
adjacent_right(Slice) ->
    Slice#item_slice.end_ =:= item:len((Slice#item_slice.item)) - 1.

-spec len(item_slice()) -> integer().
len(Slice) ->
    Slice#item_slice.end_ - Slice#item_slice.start + 1.

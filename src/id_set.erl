-module(id_set).

-export([insert/3]).

-export_type([delete_set/0]).

-include("../include/id.hrl").
-include("../include/range.hrl").

-type id_range() :: {continuous, range:range()} | {fragmented, [range:range()]}.

-type id_set() :: #{state_vector:client_id() => id_range()}.

-type delete_set() :: id_set().

-spec id_range_push(id_range(), integer(), integer()) -> id_range().
id_range_push({continuous, #range{start = RangeStart, end_ = RangeEnd}}, Start, End) ->
    case RangeEnd >= Start of
        true ->
            case RangeStart > End of
                true ->
                    {fragmented, [
                        #range{start = Start, end_ = End},
                        #range{start = RangeStart, end_ = RangeEnd}
                    ]};
                false ->
                    {continuous, #range{start = min(RangeStart, Start), end_ = max(RangeEnd, End)}}
            end;
        false ->
            {fragmented, [
                #range{start = Start, end_ = End}, #range{start = RangeStart, end_ = RangeEnd}
            ]}
    end;
id_range_push({fragmented, Ranges}, Start, End) ->
    case Ranges of
        [] ->
            {continuous, #range{start = Start, end_ = End}};
        _ ->
            % リストの長さを求める
            Len = length(Ranges),
            {L1, [Last]} = lists:split(Len - 1, Ranges),
            case range:try_join(Last, #range{start = Start, end_ = End}) of
                {ok, NewLast} -> {fragmented, L1 ++ [NewLast]};
                undefined -> {fragmented, Ranges ++ [#range{start = Start, end_ = End}]}
            end
    end.

-spec insert(id_set(), id:id(), integer()) -> id_set().
insert(IdSet, Id, Len) ->
    maps:update_with(
        Id#id.client,
        fun(V) -> id_range_push(V, Id#id.clock, Id#id.clock + Len) end,
        {continuous, #range{start = Id#id.clock, end_ = Id#id.clock + Len}},
        IdSet
    ).

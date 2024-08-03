-module(range).

-export_type([range/0]).

-include("../include/range.hrl").

-type range() :: #range{}.

-spec disjoint(range(), range()) -> boolean().
disjoint(R1, R2) ->
    R1#range.end_ < R2#range.start orelse R2#range.end_ < R1#range.start.

-spec try_join(range(), range()) -> option:option(range()).
try_join(R1, R2) ->
    case disjoint(R1, R2) of
        true ->
            undefined;
        false ->
            {ok, #range{
                start = min(R1#range.start, R2#range.start),
                end_ = max(R1#range.end_, R2#range.end_)
            }}
    end.

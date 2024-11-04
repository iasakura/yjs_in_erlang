-module(range).

-export([decode_range/1, encode_range/1, try_join/2]).
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

-spec encode_range(range()) -> binary().
encode_range(#range{start = Start, end_ = End}) ->
    <<(var_int:encode_uint(Start))/binary, (var_int:encode_uint(End - Start))/binary>>.

-spec decode_range(binary()) -> {range(), binary()}.
decode_range(Bin) ->
    {Start, Bin1} = var_int:decode_uint(Bin),
    {Len, Bin2} = var_int:decode_uint(Bin1),
    {{range, Start, Start + Len}, Bin2}.

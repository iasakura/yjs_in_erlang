-module(id_set).

-export([
    new/0,
    insert/3,
    encode_id_set/1,
    decode_id_set/1,
    decode_id_range/1,
    encode_id_range/1,
    merge_id_set/2,
    id_range_to_list/1,
    id_range_push/3,
    subtract_id_set/2
]).

-export_type([id_set/0, id_range/0]).

-include("../include/id.hrl").
-include("../include/range.hrl").

-type id_range() :: {continuous, range:range()} | {fragmented, [range:range()]}.

-type id_set() :: #{state_vector:client_id() => id_range()}.

% -type delete_set() :: id_set().

-spec new() -> id_set().
new() -> #{}.

-spec id_range_to_list(id_range()) -> [range:range()].
id_range_to_list({continuous, Range}) -> [Range];
id_range_to_list({fragmented, Ranges}) -> Ranges.

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

-spec encode_id_range(id_range()) -> binary().
encode_id_range({continuous, R}) ->
    LenBin = var_int:encode_uint(1),
    RBin = range:encode_range(R),
    <<LenBin/binary, RBin/binary>>;
encode_id_range({fragmented, Ranges}) ->
    LenBin = var_int:encode_uint(length(Ranges)),
    RangesBin = lists:foldl(
        fun(R, Acc) ->
            <<Acc/binary, (range:encode_range(R))/binary>>
        end,
        <<>>,
        Ranges
    ),
    <<LenBin/binary, RangesBin/binary>>.

-spec decode_id_range(binary()) -> {id_range(), binary()}.
decode_id_range(Bin) ->
    {Len, Bin0} = var_int:decode_uint(Bin),
    case Len of
        1 ->
            {R, Rest} = range:decode_range(Bin0),
            {{continuous, R}, Rest};
        _ ->
            Rec = fun Rec(N, Bin1, Acc) ->
                case N of
                    0 ->
                        {Acc, Bin1};
                    _ ->
                        {R, Bin2} = range:decode_range(Bin1),
                        Rec(N - 1, Bin2, [R | Acc])
                end
            end,
            {Ranges, Rest} = Rec(Len, Bin0, []),
            {{fragmented, Ranges}, Rest}
    end.

-spec encode_id_set(id_set()) -> binary().
encode_id_set(IdSet) ->
    LenBin = var_int:encode_uint(maps:size(IdSet)),
    Bin = maps:fold(
        fun(ClientId, Ranges, Acc) ->
            <<Acc/binary, (state_vector:encode_client_id(ClientId))/binary,
                (encode_id_range(Ranges))/binary>>
        end,
        <<>>,
        IdSet
    ),
    <<LenBin/binary, Bin/binary>>.

-spec decode_id_set(binary()) -> {id_set:id_set(), binary()}.
decode_id_set(Bin) ->
    {Len, Bin0} = var_int:decode_uint(Bin),
    Rec = fun Rec(N, Bin1, Acc) ->
        case N of
            0 ->
                {Acc, Bin1};
            _ ->
                {ClientId, Bin2} = var_int:decode_uint(Bin1),
                {Ranges, Bin3} = decode_id_range(Bin2),
                Rec(N - 1, Bin3, maps:put(ClientId, Ranges, Acc))
        end
    end,
    Rec(Len, Bin0, #{}).

-spec merge_id_range(id_range(), id_range()) -> id_range().
merge_id_range({continuous, R1}, {continuous, R2}) ->
    case R1#range.end_ < R2#range.start orelse R2#range.end_ < R1#range.start of
        true ->
            {fragmented, [R1, R2]};
        false ->
            {continuous, #range{
                start = min(R1#range.start, R2#range.start),
                end_ = max(R1#range.end_, R2#range.end_)
            }}
    end;
merge_id_range({continuous, R1}, {fragmented, Ranges}) ->
    {fragmented, [R1 | Ranges]};
merge_id_range({fragmented, Ranges}, {continuous, R1}) ->
    {fragmented, [R1 | Ranges]};
merge_id_range({fragmented, R1}, {fragmented, R2}) ->
    {fragmented, R1 ++ R2}.

-spec merge_id_set(id_set(), id_set()) -> id_set().
merge_id_set(D1, D2) ->
    maps:merge_with(
        fun(_K, V1, V2) ->
            merge_id_range(V1, V2)
        end,
        D1,
        D2
    ).

-spec subtract_id_range(id_range(), id_range()) -> id_range().
subtract_id_range({continuous, R1}, {continuous, R2}) ->
    case range:subtract(R1, R2) of
        [R] -> {continuous, R};
        Rs -> {fragmented, Rs}
    end;
subtract_id_range({continuous, R1}, {fragmented, Ranges}) ->
    case
        lists:foldl(
            fun(R, Acc) ->
                lists:flatmap(fun(Range1) -> range:subtract(Range1, R) end, Acc)
            end,
            [R1],
            Ranges
        )
    of
        [R] -> {continuous, R};
        Rs -> {fragmented, Rs}
    end;
subtract_id_range({fragmented, Ranges1}, {continuous, R2}) ->
    case lists:flatmap(fun(R) -> range:subtract(R, R2) end, Ranges1) of
        [R] -> {continuous, R};
        Rs -> {fragmented, Rs}
    end;
subtract_id_range({fragmented, Ranges1}, {fragmented, Ranges2}) ->
    case
        lists:foldl(
            fun(R, Acc) ->
                lists:flatmap(fun(R1) -> range:subtract(R1, R) end, Acc)
            end,
            Ranges1,
            Ranges2
        )
    of
        [R] -> {continuous, R};
        Rs -> {fragmented, Rs}
    end.

-spec subtract_id_set(id_set(), id_set()) -> id_set().
subtract_id_set(D1, D2) ->
    maps:filtermap(
        fun(_K, V) ->
            case V of
                {fragmented, []} -> false;
                _ -> true
            end
        end,
        maps:merge_with(
            fun(_K, V1, V2) ->
                subtract_id_range(V1, V2)
            end,
            D1,
            D2
        )
    ).

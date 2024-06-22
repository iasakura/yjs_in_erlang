-module(update).

-export_type([update/0]).

-include("../include/range.hrl").
-include("../include/update.hrl").

-type ranges() :: [#range{}].

-type id_set() :: #{state_vector:client_id() => ranges()}.

-type delete_set() :: id_set().

-type update() :: #update{}.

-spec encode_ranges(ranges()) -> binary().
encode_ranges(Ranges) ->
    L = length(Ranges),
    lists:foldl(
        fun({Start, End}, Acc) ->
            <<(var_int:encode_uint(Start))/binary, (var_int:encode_uint(End))/binary, Acc/binary>>
        end,
        var_int:encode_uint(L),
        Ranges
    ).

-spec encode_id_set(id_set()) -> binary().
encode_id_set(IdSet) ->
    N = maps:size(IdSet),
    maps:fold(
        fun(ClientId, Ranges, Acc) ->
            <<(var_int:encode_uint(ClientId))/binary, (encode_ranges(Ranges))/binary, Acc/binary>>
        end,
        <<(var_int:encode_uint(N))/binary>>,
        IdSet
    ).

-spec encode_delete_set(delete_set()) -> binary().
encode_delete_set(DeleteSet) ->
    encode_id_set(DeleteSet).


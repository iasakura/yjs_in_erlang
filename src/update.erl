-module(update).

-export([decode_update/1]).
-export_type([update/0, pending_update/0]).

-include("../include/range.hrl").
-include("../include/update.hrl").
-include("../include/id.hrl").
-include("../include/block_range.hrl").
-include("../include/item.hrl").
-include("../include/constants.hrl").
-include_lib("kernel/include/logger.hrl").

-type block_range() :: #block_range{}.

-type ranges() :: [#range{}].

-type id_set() :: #{state_vector:client_id() => ranges()}.

-type delete_set() :: id_set().

-type update() :: #update{}.

-type item() :: #item{}.

-type block_carrier() ::
    {item, item()}
    | {gc, block_range()}
    | {skip, block_range()}.

-type update_blocks() :: #{state_vector:client_id() => [block_carrier()]}.

-type pending_update() :: #pending_update{}.

-spec block_carrier_length(block_carrier()) -> integer().
block_carrier_length({item, Item}) -> item:len(Item);
block_carrier_length({gc, Range}) -> Range#block_range.len;
block_carrier_length({skip, Range}) -> Range#block_range.len.

% -spec encode_ranges(ranges()) -> binary().
% encode_ranges(Ranges) ->
%     L = length(Ranges),
%     lists:foldl(
%         fun({Start, End}, Acc) ->
%             <<(var_int:encode_uint(Start))/binary, (var_int:encode_uint(End))/binary, Acc/binary>>
%         end,
%         var_int:encode_uint(L),
%         Ranges
%     ).

-spec decode_ranges(binary()) -> {ranges(), binary()}.
decode_ranges(Bin) ->
    {Len, Bin0} = var_int:decode_uint(Bin),
    Rec = fun Rec(N, Bin1, Acc) ->
        case N of
            0 ->
                {Acc, Bin1};
            _ ->
                {Start, Bin2} = var_int:decode_uint(Bin1),
                {End, Bin3} = var_int:decode_uint(Bin2),
                Rec(N - 1, Bin3, [#range{start = Start, end_ = End} | Acc])
        end
    end,
    Rec(Len, Bin0, []).

% -spec encode_id_set(id_set()) -> binary().
% encode_id_set(IdSet) ->
%     N = maps:size(IdSet),
%     maps:fold(
%         fun(ClientId, Ranges, Acc) ->
%             <<(var_int:encode_uint(ClientId))/binary, (encode_ranges(Ranges))/binary, Acc/binary>>
%         end,
%         <<(var_int:encode_uint(N))/binary>>,
%         IdSet
%     ).

-spec decode_id_set(binary()) -> {id_set(), binary()}.
decode_id_set(Bin) ->
    {Len, Bin0} = var_int:decode_uint(Bin),
    Rec = fun Rec(N, Bin1, Acc) ->
        case N of
            0 ->
                {Acc, Bin1};
            _ ->
                {ClientId, Bin2} = var_int:decode_uint(Bin1),
                {Ranges, Bin3} = decode_ranges(Bin2),
                Rec(N - 1, Bin3, maps:put(ClientId, Ranges, Acc))
        end
    end,
    Rec(Len, Bin0, #{}).

% -spec encode_delete_set(delete_set()) -> binary().
% encode_delete_set(DeleteSet) ->
%     encode_id_set(DeleteSet).

-spec decode_delete_set(binary()) -> {delete_set(), binary()}.
decode_delete_set(Bin) -> decode_id_set(Bin).

-spec decode_block(id:id(), binary()) -> {block_carrier(), binary()}.
decode_block(Id, Bin) ->
    {Info, Rest} = fixed_int:read_u8(Bin),
    case Info of
        ?BLOCK_SKIP_REF_NUMBER ->
            {Len, Rest0} = var_int:decode_uint(Rest),
            {{type = skip, #block_range{id = Id, len = Len}}, Rest0};
        ?BLOCK_GC_REF_NUMBER ->
            {Len, Rest0} = var_int:decode_uint(Rest),
            {{type = gcc, #block_range{id = Id, len = Len}}, Rest0};
        Info ->
            CantCopyParentInfo = Info band (?HAS_RIGHT_ORIGIN bor ?HAS_ORIGIN) =:= 0,
            {Origin, RestO} =
                case Info band ?HAS_ORIGIN of
                    0 ->
                        {undefined, Rest};
                    _ ->
                        {Id, RestId} = id:read_id(Rest),
                        {{ok, Id}, RestId}
                end,
            {RightOrigin, RestRO} =
                case Info band ?HAS_RIGHT_ORIGIN of
                    0 ->
                        {undefined, RestO};
                    _ ->
                        {Id, RestId2} = id:read_id(RestO),
                        {{ok, Id}, RestId2}
                end,
            {Parent, RestPA} =
                case CantCopyParentInfo of
                    true ->
                        case var_int:decode_uint(RestRO) of
                            {1, RestPI} ->
                                {Name, RestName} = binary_encoding:read_string(RestPI),
                                {{named, Name}, RestName};
                            {_, RestPI} ->
                                {Pid, RestPID} = id:read_id(RestPI),
                                {{id, Pid}, RestPID}
                        end;
                    _ ->
                        {{unknown}, RestRO}
                end,
            {ParentSub, RestPS} =
                if
                    CantCopyParentInfo and ((Info band ?HAS_PARENT_SUB) =/= 0) ->
                        {PSub, RestPSub} = binary_encoding:read_string(RestPA),
                        {{ok, PSub}, RestPSub};
                    true ->
                        {undefined, RestPA}
                end,
            ?LOG_DEBUG("Parent: ~p, ParentSub: ~p", [Parent, ParentSub]),
            {Content, RestItem} = item_content:decode(RestPS, Info),
            Item = item:new_item(
                Id,
                undefined,
                Origin,
                undefined,
                RightOrigin,
                Parent,
                ParentSub,
                Content
            ),
            {{item, Item}, RestItem}
    end.

-spec decode_blocks(integer(), state_vector:client_id(), integer(), binary()) ->
    {[block_carrier()], binary()}.
decode_blocks(Len, ClientId, Clock, Bin) ->
    {Rest, Blocks, _} = lists:foldl(
        fun(_I, {Bin1, Blocks, C}) ->
            Id = #id{client = ClientId, clock = C},
            {Block, Rest} = decode_block(Id, Bin1),
            Size = block_carrier_length(Block),
            case Size > 0 of
                true -> {Rest, [Block | Blocks], C + Size};
                false -> {Rest, Blocks, C}
            end
        end,
        {Bin, [], Clock},
        lists:seq(0, Len - 1)
    ),
    {lists:reverse(Blocks), Rest}.

-spec decode_update(binary()) -> {update(), binary()}.
decode_update(Bin) ->
    {Len, Rest} = var_int:decode_uint(Bin),
    {Clients, Rest1} = lists:foldl(
        fun(_I, {Clients, B}) ->
            {BlocksLen, Rest0} = var_int:decode_uint(B),

            {ClientId, Rest1} = var_int:decode_uint(Rest0),
            {Clock, Rest2} = var_int:decode_uint(Rest1),
            {Blocks, Rest3} = decode_blocks(BlocksLen, ClientId, Clock, Rest2),
            {
                maps:update_with(ClientId, fun(V) -> lists:append(V, Blocks) end, Blocks, Clients),
                Rest3
            }
        end,
        {#{}, Rest},
        lists:seq(0, Len - 1)
    ),
    {DeleteSet, Rest2} = decode_delete_set(Rest1),
    {#update{update_blocks = Clients, delete_set = DeleteSet}, Rest2}.

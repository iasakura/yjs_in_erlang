-module(text).

-include("../include/records.hrl").
-include("../include/constants.hrl").

-export([get_string/1, insert/5, delete/4]).
-export_type([y_text/0]).

-type y_text() :: #y_text{}.

-spec get_string(y_text()) -> binary().
get_string(#y_text{store = Store, branch = Branch}) ->
    get_text(Branch#branch.start, Store, <<"">>).

-spec get_text(option:option(item_ptr:item_ptr()), store:store(), binary()) -> binary().
get_text(Start, Store, Acc) ->
    case Start of
        undefined ->
            Acc;
        {ok, S} ->
            case item_ptr:get_view(S) of
                undefined ->
                    Acc;
                {ok, Item} ->
                    begin
                        NewAcc =
                            case item:is_deleted(Item) of
                                false ->
                                    case Item#item.content of
                                        {string, Str} ->
                                            <<Acc/binary, Str/binary>>;
                                        _ ->
                                            Acc
                                    end;
                                true ->
                                    Acc
                            end,
                        get_text(Item#item.right, Store, NewAcc)
                    end
            end
    end.

-spec insert(
    transaction:transaction_mut(),
    id:id(),
    y_text(),
    integer(),
    binary()
) ->
    ok | error.
insert(Txn, #id{client = Id, clock = Clock}, #y_text{branch = Branch}, Pos, Str) ->
    Start = Branch#branch.start,
    {ok, {Origin, RightOrigin}} = get_id_from_pos(undefined, Start, Pos),
    % eqwalizer:ignore update...
    Update = #update{
        update_blocks = #{
            Id => [
                {item, #item{
                    id = #id{client = Id, clock = Clock},
                    len = byte_size(Str),
                    left = undefined,
                    right = undefined,
                    origin = Origin,
                    right_origin = RightOrigin,
                    content = {string, Str},
                    parent = Branch#branch.item,
                    redone = undefined,
                    parent_sub = undefined,
                    moved = undefined,
                    info =
                        (case option:is_some(Origin) of
                            true -> ?HAS_ORIGIN;
                            false -> 0
                        end) bor
                            case option:is_none(RightOrigin) of
                                true -> ?HAS_RIGHT_ORIGIN;
                                false -> 0
                            end bor 0 bor item_content:get_ref_number({string, Str})
                }}
            ]
        },
        delete_set = #{}
    },
    transaction:apply_update(Txn, Update),
    ok.

-spec delete(
    transaction:transaction_mut(),
    y_text(),
    integer(),
    integer()
) ->
    ok | error.
delete(Txn, #y_text{branch = Branch}, Pos, Len) ->
    {ok, #id{client = Id, clock = Clock}} = get_id_from_pos(undefined, Branch#branch.start, Pos),
    % eqwalizer:ignore update...
    Update = #update{
        update_blocks = #{},
        delete_set = #{Id => {continuous, #range{start = Clock, end_ = Clock + Len - 1}}}
    },
    transaction:apply_update(Txn, Update).

-spec get_id_from_pos(option:option(id:id()), option:option(item_ptr:item_ptr()), integer()) ->
    option:option(option:option(id:id())).
get_id_from_pos(Prev, Start, Pos) ->
    case Start of
        undefined ->
            case Pos of
                0 -> {ok, Prev};
                _ -> undefined
            end;
        {ok, S} ->
            case item_ptr:get_view(S) of
                undefined ->
                    throw("unreachable");
                {ok, Item} ->
                    case item:is_deleted(Item) of
                        false ->
                            case item:len(Item) > Pos of
                                true ->
                                    {ok,
                                        {ok, #id{
                                            client = Item#item.id#id.client,
                                            clock = Item#item.id#id.clock + Pos
                                        }}};
                                false ->
                                    get_id_from_pos(
                                        {ok, Item#item.id},
                                        Item#item.right,
                                        Pos - item:len(Item)
                                    )
                            end;
                        true ->
                            get_id_from_pos(Prev, Item#item.right, Pos)
                    end
            end
    end.

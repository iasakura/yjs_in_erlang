-module(doc).

-export([new/0, get_or_create_text/2, transact_mut/1, get_update/2]).
-export_type([doc/0]).

-include("../include/records.hrl").

-type doc() :: #doc{}.

-spec new() -> doc().
new() ->
    #doc{
        store = store:new()
    }.

-spec get_or_create_text(doc(), binary()) -> text:y_text().
get_or_create_text(Doc, Str) ->
    Branch = store:get_or_create_type(Doc#doc.store, Str, {text}),
    #y_text{store = Doc#doc.store, branch = Branch}.

-spec transact_mut(doc()) -> transaction:transaction_mut().
transact_mut(Doc) ->
    transaction:new(Doc).

% TODO: use StateVector for not to send redundant data
-spec get_update(doc(), state_vector:state_vector()) -> update:update().
get_update(Doc, _StateVector) ->
    Store = Doc#doc.store,
    Blocks = Store#store.blocks,
    AllBlocksMap = block_store:get_all(Blocks),
    UpdateBlocks = maps:map(
        fun(ClientId, BS) ->
            lists:map(
                fun({_K, V}) ->
                    case V of
                        {block, B} ->
                            {item, B};
                        {gc, G} ->
                            {gc, #block_range{
                                id = #id{
                                    client = ClientId,
                                    clock = G#gc.start
                                },
                                len = G#gc.end_ - G#gc.start
                            }}
                    end
                end,
                maps:to_list(BS)
            )
        end,
        AllBlocksMap
    ),
    DeleteSet = maps:map(
        fun(_ClientId, BS) ->
            lists:foldl(
                fun({_K, V}, Acc) ->
                    case V of
                        {block, Item} ->
                            case item:is_deleted(Item) of
                                true ->
                                    Start = Item#item.id#id.clock,
                                    id_set:id_range_push(Acc, Start, Start + Item#item.len);
                                false ->
                                    Acc
                            end;
                        {gc, G} ->
                            id_set:id_range_push(
                                Acc,
                                G#gc.start,
                                G#gc.end_
                            )
                    end
                end,
                {fragmented, []},
                maps:to_list(BS)
            )
        end,
        AllBlocksMap
    ),
    % eqwalizer:ignore: Unbound rec: update
    #update{
        update_blocks = UpdateBlocks,
        delete_set = DeleteSet
    }.

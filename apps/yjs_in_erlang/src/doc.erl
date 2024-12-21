-module(doc).

-export([
    new/0,
    get_or_create_text/2,
    transact_mut/1,
    get_update/2,
    subscribe_update_v1/1,
    get_state_vector/1
]).
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
    store:get_or_create_type(Doc#doc.store, Str, {text}),
    #y_text{store = Doc#doc.store, key = Str}.

-spec transact_mut(doc()) -> transaction:transaction_mut().
transact_mut(Doc) ->
    transaction:new(Doc).

% TODO: use StateVector for not to send redundant data
-spec get_update(doc(), state_vector:state_vector()) -> update:update().
get_update(Doc, StateVector) ->
    Store = Doc#doc.store,
    Blocks = Store#store.blocks,
    UpdateBlocks = block_store:blocks_from(StateVector, Blocks),
    DeleteSet = maps:map(
        fun(_ClientId, BS) ->
            lists:foldl(
                fun(V, Acc) ->
                    case V of
                        {item, Item} ->
                            case item:is_deleted(Item) of
                                true ->
                                    Start = Item#item.id#id.clock,
                                    id_set:id_range_push(Acc, Start, Start + Item#item.len);
                                false ->
                                    Acc
                            end;
                        {gc, G} ->
                            Start = G#block_range.id#id.clock,
                            id_set:id_range_push(
                                Acc,
                                Start,
                                Start + G#block_range.len
                            )
                    end
                end,
                {fragmented, []},
                BS
            )
        end,
        UpdateBlocks
    ),
    #update{
        update_blocks = UpdateBlocks,
        delete_set = DeleteSet
    }.

-spec subscribe_update_v1(doc()) -> ok.
subscribe_update_v1(Doc) ->
    store:subscribe_update_v1(Doc#doc.store).

-spec get_state_vector(doc()) -> state_vector:state_vector().
get_state_vector(Doc) ->
    store:get_state_vector(Doc#doc.store).

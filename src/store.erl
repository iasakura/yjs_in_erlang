-module(store).

-export([get_item/2]).

-export_type([store/0]).

-include("../include/store.hrl").
-include("../include/id.hrl").
-include("../include/item.hrl").
-include("../include/block_store.hrl").

-type store() :: #store{}.

-spec get_item(store(), id:id()) -> option:option(item:item()).
get_item(Store, Id) ->
    case block_store:get(Store#store.blocks, Id) of
        {ok, {item, Item}} -> {ok, Item};
        {ok, {gc, Gc}} -> throw(io_lib:format("WIP: GC: ~p", [Gc]));
        undefined -> {error, not_found}
    end.

-spec put_item(store(), item:item()) -> store().
put_item(Store, Item) ->
    block_store:put_item(Store#store.blocks, {item, Item}).

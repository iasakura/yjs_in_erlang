-module(item_ptr).

-export([new/2, get_id/1, get_view/1, update/2, next/1]).
-export_type([item_ptr/0]).

-include("../include/records.hrl").

-type item_ptr() :: {store:store(), id:id()}.

-spec new(store:store(), id:id()) -> item_ptr().
new(Store, Id) -> {Store, Id}.

-spec get_id(item_ptr()) -> id:id().
get_id({_, Id}) -> Id.

-spec get_view(item_ptr()) -> option:option(item:item()).
get_view({Store, Id}) -> store:get_item(Store, Id).

-spec update(item_ptr(), item:item()) -> ok.
update({Store, Id}, Item) ->
    store:put_item(Store, Item#item{id = Id}),
    ok.

% TODO: ここにあるべき？
-spec next(item_ptr()) -> option:option(item_ptr()).
next({Store, Id}) ->
    case store:get_item(Store, Id) of
        {ok, Item} ->
            NextId = #id{client = Id#id.client, clock = Id#id.clock + Item#item.len},
            case option:is_some(store:get_item(Store, NextId)) of
                true ->
                    {ok, {Store, NextId}};
                false ->
                    undefined
            end;
        undefined ->
            undefined
    end.

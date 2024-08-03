-module(util).

-export([get_item_from_link/2]).

-spec get_item_from_link(store:store(), option:option(id:id())) -> option:option(item:item()).
get_item_from_link(Store, Link) ->
    case Link of
        {ok, Id} -> store:get_item(Store, Id);
        _ -> undefined
    end.

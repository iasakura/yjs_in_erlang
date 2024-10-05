-module(text).

-include("../include/records.hrl").

-export([get_string/1]).
-export_type([y_text/0]).

-type y_text() :: #y_text{}.

-spec get_string(y_text()) -> binary().
get_string(#y_text{store = Store, branch = Branch}) ->
    get_text(Branch#branch.start, Store, <<"">>).

-spec get_text(option:option(id:id()), store:store(), binary()) -> binary().
get_text(Start, Store, Acc) ->
    case Start of
        undefined ->
            Acc;
        {ok, S} ->
            case store:get_item(Store, S) of
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

-module(doc).

-export([new/0, get_or_create_text/2]).
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

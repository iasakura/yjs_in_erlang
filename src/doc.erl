-module(doc).

-export([new/0]).
-export_type([doc/0]).

-include("../include/doc.hrl").

-type doc() :: #doc{}.

% -opaque tag(A) :: ok | A.

-spec new() -> doc().
new() ->
    #doc{
        store = store:new()
    }.

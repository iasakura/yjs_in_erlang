-module(yjs_in_erlang_crdt_benchmark).

-export([]).

-spec bench(fileName) -> ok.
bench(fileName) ->
    % fileNameからJSONで読み出し
    {ok, Bin} = file:read_file(fileName),
    % JSONをパース
    Json = json:decode(Bin),
    ok.
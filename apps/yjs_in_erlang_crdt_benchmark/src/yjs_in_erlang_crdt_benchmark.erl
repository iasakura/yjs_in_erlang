-module(yjs_in_erlang_crdt_benchmark).

-export([bench/1]).

-spec bench(fileName) -> ok.
bench(fileName) ->
    % fileNameからJSONで読み出し
    {ok, Bin} = file:read_file(fileName),
    % JSONをパース
    Json = jsone:decode(Bin, []),
    StartContent = maps:get("startContent", Json),
    EndContent = maps:get("endContent", Json),
    Txns = maps:get("txns", Json),
    run(StartContent, EndContent, Txns),
    ok.

-spec run(binary(), binary(), [#{binary() => term()}]) -> ok.
run(StartContent, EndContent, Txns) ->
    Doc = doc:new(),
    Str = doc:get_or_create_text(Doc, <<"content">>),
    YTxn = doc:transact_mut(Doc),
    Id = 0,
    text:insert(YTxn, id:new(Id, clock), Str, 0, StartContent),
    Clock = util:compute_utf16_length(Str),
    lists:foldl(
        fun(Txn, Acc) ->
            Patches = maps:get("patches", Txn),
            lists:foldl(
                fun(Patch, InAcc) ->
                    [InsertPos, DeleteNum, InsertStr] = Patch,
                    text:delete(YTxn, Str, InsertPos, DeleteNum),
                    text:insert(YTxn, id:new(Id, InAcc), Str, InsertPos, InsertStr),
                    InAcc + util:compute_utf16_length(InsertStr)
                end,
                Acc,
                Patches
            )
        end,
        Clock,
        Txns
    ),
    case text:get_string(Str) == EndContent of
        true -> io:format("OK~n");
        false -> io:format("NG~n")
    end,
    ok.

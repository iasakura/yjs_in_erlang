-module(yjs_in_erlang_crdt_benchmark).

-include_lib("kernel/include/logger.hrl").

-export([bench/0, bench/1]).

-spec decompress_gzip_data(binary()) -> binary().
decompress_gzip_data(BinaryData) ->
    Zlib = zlib:open(),
    % 31 indicates gzip decoding
    ok = zlib:inflateInit(Zlib, 31),
    DecompressedData = zlib:inflate(Zlib, BinaryData),
    ok = zlib:inflateEnd(Zlib),
    zlib:close(Zlib),
    iolist_to_binary(DecompressedData).

-spec bench() -> ok.
bench() ->
    logging:init(),
    bench(
        <<"apps/yjs_in_erlang_crdt_benchmark/editing-traces/sequential_traces/clownschool_flat.json.gz">>
    ).

-spec bench(binary()) -> ok.
bench(FileName) ->
    % fileNameからJSONで読み出し
    {ok, Bin} = file:read_file(FileName),
    % JSONをパース
    JsonBin = decompress_gzip_data(Bin),
    Json = jsone:decode(JsonBin, []),
    StartContent = maps:get(<<"startContent">>, Json),
    EndContent = maps:get(<<"endContent">>, Json),
    Txns = maps:get(<<"txns">>, Json),
    run(StartContent, EndContent, Txns),
    ok.

-spec run(binary(), binary(), [#{binary() => term()}]) -> ok.
run(StartContent, EndContent, Txns) ->
    Doc = doc:new(),
    Str = doc:get_or_create_text(Doc, <<"content">>),
    YTxn = doc:transact_mut(Doc),
    Id = state_vector:integer_to_client_id(0),
    text:insert(YTxn, id:new(Id, 0), Str, 0, StartContent),
    Clock = util:compute_utf16_length(StartContent),
    lists:foldl(
        fun(Txn, Acc) ->
            Patches = maps:get(<<"patches">>, Txn),
            lists:foldl(
                fun(Patch, InAcc) ->
                    [InsertPos, DeleteNum, InsertStr] = Patch,
                    ?LOG_INFO("InsertPos: ~p, DeleteNum: ~p, InsertStr: ~p", [
                        InsertPos, DeleteNum, InsertStr
                    ]),
                    text:delete(YTxn, Str, InsertPos, DeleteNum),
                    text:insert(YTxn, id:new(Id, InAcc), Str, InsertPos, InsertStr),
                    InAcc + util:compute_utf16_length(InsertStr)
                end,
                Acc,
                eqwalizer:dynamic_cast(Patches)
            )
        end,
        Clock,
        Txns
    ),
    case text:get_string(Str) =:= EndContent of
        true -> io:format("OK~n");
        false -> io:format("NG~n")
    end,
    ok.

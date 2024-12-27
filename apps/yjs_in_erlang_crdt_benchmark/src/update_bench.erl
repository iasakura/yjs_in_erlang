-module(update_bench).

-export([bench/0, bench/2]).

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
        <<"apps/yjs_in_erlang_crdt_benchmark/build_histories_to_yjs_updates/clownschool_flat.update.bin">>,
        <<"apps/yjs_in_erlang_crdt_benchmark/editing-traces/sequential_traces/clownschool_flat.json.gz">>
    ).

-spec bench(binary(), binary()) -> ok.
bench(FileName1, FileName2) ->
    % fileNameからJSONで読み出し
    {ok, Updates} = file:read_file(FileName1),
    {ok, Bin} = file:read_file(FileName2),
    % JSONをパース
    JsonBin = decompress_gzip_data(Bin),
    Json = jsone:decode(JsonBin, []),
    EndContent = maps:get(<<"endContent">>, Json),
    run(EndContent, Updates),
    ok.

-spec run(binary(), binary()) -> ok.
run(EndContent, Updates) ->
    Doc = doc:new(),
    Str = doc:get_or_create_text(Doc, <<"content">>),
    YTxn = doc:transact_mut(Doc),
    Loop = fun Loop(Rest) ->
        case Rest of
            <<>> ->
                ok;
            _ ->
                {UpdateBin, Rest1} = binary_encoding:decode_buf(Rest),
                % ?LOG_INFO("UpdateBin: ~p", [UpdateBin]),
                {Update, <<>>} = update:decode_update(UpdateBin),
                transaction:apply_update(YTxn, Update),
                Loop(Rest1)
        end
    end,
    Loop(Updates),
    case text:get_string(Str) =:= EndContent of
        true -> io:format("OK~n");
        false -> io:format("NG~n,~p~n,~p~n", [text:get_string(Str), EndContent])
    end.

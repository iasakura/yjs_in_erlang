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
    TxnLen = length(Txns),
    lists:foldl(
        fun(Txn, {N, Acc}) ->
            if
                N rem 100 =:= 0 ->
                    ?LOG_INFO("#~p / ~p", [
                        N, TxnLen
                    ]);
                true ->
                    ok
            end,
            Patches = maps:get(<<"patches">>, Txn),
            Next = lists:foldl(
                fun(Patch, InAcc) ->
                    % PrevText = text:get_string(Str),
                    [Pos, DeleteNum, InsertStr] = Patch,
                    % ?LOG_INFO("InsertPos: ~p, DeleteNum: ~p, InsertStr: ~p", [
                    %     Pos, DeleteNum, InsertStr
                    % ]),
                    text:delete(YTxn, Str, Pos, DeleteNum),
                    text:insert(YTxn, id:new(Id, InAcc), Str, Pos, InsertStr),
                    % DEBUG
                    % case unicode:characters_to_list(PrevText, utf8) of
                    %     S when is_list(S) -> CodePoints = S;
                    %     _ ->
                    %         throw("invalid text"),
                    %         % unreachable
                    %         CodePoints = []
                    % end,
                    % case unicode:characters_to_binary(lists:sublist(CodePoints, Pos)) of
                    %     Bin when is_binary(Bin) ->
                    %         BytePos = byte_size(Bin);
                    %     _ ->
                    %         throw("invalid text"),
                    %         BytePos = 0
                    % end,
                    % case unicode:characters_to_binary(lists:sublist(CodePoints, Pos + DeleteNum)) of
                    %     Bin0 when is_binary(Bin0) ->
                    %         DeleteBytePos = byte_size(Bin0);
                    %     _ ->
                    %         throw("invalid text"),
                    %         DeleteBytePos = 0
                    % end,
                    % Expected = <<
                    %     (binary:part(PrevText, 0, BytePos))/binary,
                    %     InsertStr/binary,
                    %     (binary:part(PrevText, DeleteBytePos, byte_size(PrevText) - DeleteBytePos))/binary
                    % >>,
                    % NewText = text:get_string(Str),
                    % ?LOG_INFO("~p, ~p", [
                    %     Expected, NewText
                    % ]),
                    % case Expected =/= NewText of
                    %     true ->
                    %         throw(io_lib:format("assert failure: ~p =/= ~p", [Expected, NewText]));
                    %     false ->
                    %         ok
                    % end,
                    InAcc + util:compute_utf16_length(InsertStr)
                end,
                Acc,
                eqwalizer:dynamic_cast(Patches)
            ),
            {N + 1, Next}
        end,
        {0, Clock},
        Txns
    ),
    case text:get_string(Str) =:= EndContent of
        true -> io:format("OK~n");
        false -> io:format("NG~n")
    end,
    ok.

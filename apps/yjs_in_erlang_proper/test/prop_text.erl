-module(prop_text).
-include_lib("proper/include/proper.hrl").

run_with_timeout(F, Timeout) ->
    Parent = self(),
    %% テストプロセスを生成
    TestPid = spawn(fun() ->
        Result = proper:quickcheck(F(), []),
        Parent ! {done, Result}
    end),
    %% タイムアウト設定
    receive
        {done, Result} -> Result
    after Timeout ->
        %% タイムアウト時のエラー処理
        throw(timeout)
    end.

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(
        Type,
        edits(),
        begin
            run_with_timeout(
                fun() ->
                    Doc = new(),
                    Text = doc:get_or_create_text(Doc, <<"text">>),
                    YTxn = doc:transact_mut(Doc),
                    Id = state_vector:integer_to_client_id(0),
                    {Res, _, _} = lists:foldl(
                        fun({Pos, DeleteNum, InsertStr}, {Acc, PrevText, Cnt}) ->
                            P = Pos rem (byte_size(PrevText) + 1),
                            DNum = min(byte_size(PrevText) - P, DeleteNum),
                            S = binary:list_to_bin(InsertStr),
                            case Acc of
                                false ->
                                    {false, PrevText, Cnt};
                                true ->
                                    text:delete(YTxn, Text, P, DNum),
                                    text:insert(YTxn, id:new(Id, Cnt), Text, P, S),
                                    Actual = text:get_string(Text),
                                    Expected = <<
                                        (binary:part(PrevText, 0, P))/binary,
                                        S/binary,
                                        (binary:part(
                                            PrevText, P + DNum, byte_size(PrevText) - P - DNum
                                        ))/binary
                                    >>,
                                    {Expected =:= Actual, Expected, Cnt + byte_size(S)}
                            end
                        end,
                        {true, <<"">>, 0},
                        Type
                    ),
                    Res
                end,
                1000
            )
        end
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
% boolean(_) -> true.
new() -> doc:new().

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
edits() ->
    list(
        % 位置、削除数、文字列
        {non_neg_integer(), non_neg_integer(), ascii_string()}
    ).

ascii_string() ->
    % 表示可能文字のみ
    list(choose(32, 126)).

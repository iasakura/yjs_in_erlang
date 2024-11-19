-module(prop_text).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(
        Type,
        mytype(),
        begin
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
mytype() ->
    list(
        % 位置、削除数、文字列
        {non_neg_integer(), non_neg_integer(), ascii_string()}
    ).

ascii_string() ->
    % 表示可能文字のみ
    list(choose(32, 126)).

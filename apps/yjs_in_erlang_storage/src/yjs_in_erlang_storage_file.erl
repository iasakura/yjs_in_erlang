-module(yjs_in_erlang_storage_file).

-behavior(yjs_in_erlang_storage).

-include_lib("kernel/include/logger.hrl").

-export([init/1, get_update/1, on_update/2]).
-export([start_link/1]).

-record(state, {
    filepath :: binary()
}).

init(FilePath) ->
    {ok, #state{filepath = FilePath}}.

on_update(State, Update) ->
    ?LOG_DEBUG("on_update: ~p", [State]),
    {ok, File} = file:open(State#state.filepath, [append]),
    file:write(File, update:encode_update(Update)),
    file:close(File).

get_update(FilePath) ->
    case file:read_file(FilePath) of
        {error, enoent} ->
            update:new();
        {ok, Bin} ->
            begin
                Loop = fun Loop(Acc, Bin0) ->
                    case Bin0 of
                        <<>> ->
                            lists:reverse(Acc);
                        _ ->
                            {Update, Rest} = update:decode_update(Bin0),
                            Loop([Update | Acc], Rest)
                    end
                end,
                X = update:merge_update(Loop([], Bin)),
                ?LOG_DEBUG("get_update: ~p", [X]),
                X
            end
    end.

start_link(FilePath) ->
    yjs_in_erlang_storage:start_link(?MODULE, FilePath).

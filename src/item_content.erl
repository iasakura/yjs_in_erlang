-module(item_content).

-export_type([item_content/0]).

-type item_content() ::
    %    {any, any_type()} |
    {binary, binary()}
    | {deleted, integer()}
    % {doc, option:option(doc()), doc()}
    % | {json, [json()]}
    % {embed, any_type()} |
    % | {format, string(), any_type()}
    % wip: correct?
    | {string, binary()}
    | {type, branch:branch()}.
% | {move, move()}.

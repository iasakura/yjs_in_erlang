-module(item).

-export([new_item/8, length/1]).
-export_type([item/0]).

-include("../include/item.hrl").
-include("../include/branch.hrl").
-include("../include/constants.hrl").

-type item() :: #item{}.

-spec length(item()) -> integer().
length(Item) -> Item#item.len.

-spec new_item(
    id:id(),
    option:option(id:id()),
    option:option(id:id()),
    option:option(id:id()),
    option:option(id:id()),
    type_ptr:type_ptr(),
    option:option(binary()),
    item_content:item_content()
) -> item().
new_item(
    Id,
    Left,
    Origin,
    Right,
    RightOrigin,
    Parent,
    ParentSub,
    Content
) ->
    Info =
        case item_content:countable(Content) of
            true -> ?ITEM_FLAG_COUNTABLE;
            _ -> 0
        end,
    Len = item_content:len(Content),
    RootName =
        case Parent of
            {{named, Name}} -> Name;
            _ -> undefined
        end,
    case Content of
        {type, Branch} ->
            Content2 = Branch#branch{
                name = RootName,
                item = {ok, Id}
            },
            #item{
                id = Id,
                len = Len,
                left = Left,
                origin = Origin,
                right = Right,
                right_origin = RightOrigin,
                parent = Parent,
                parent_sub = ParentSub,
                content = {type, Content2},
                info = Info,
                redone = undefined,
                moved = undefined
            };
        _ ->
            #item{
                id = Id,
                len = Len,
                left = Left,
                origin = Origin,
                right = Right,
                right_origin = RightOrigin,
                parent = Parent,
                parent_sub = ParentSub,
                content = Content,
                info = Info,
                redone = undefined,
                moved = undefined
            }
    end.

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

-spec integrate(item(), transaction:transaction_mut(), integer()) -> item().
integrate(Item, Txn, Offset) ->
    % 1. Offset > 0 なら thisを諸々調整
    % - Item.idのclockをoffsetだけ増加
    % 2. ParentをName/Idを利用しない形へ修正
    % 3. Item.leftを計算
    % 4. ParentSubをitem.left/rightから再計算
    % 5. Item.right を更新
    % - item.leftがある-> Item.left.rightに、Item.left.rightをItemに更新
    % - ない -> Item.rightをParentの最左要素に更新
    % 6. Parentのlenを更新
    % 7. move周り
    % 8. Item.contentを登録
    % 9. txn.changedに登録
    % 10. parentが削除済みかチェック
    1.

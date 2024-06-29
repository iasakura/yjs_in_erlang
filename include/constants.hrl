%  Bit flag used to identify [Item::GC].
-define(BLOCK_GC_REF_NUMBER, 0).

%  Bit flag used to identify items with content of type [ItemContent::Deleted].
-define(BLOCK_ITEM_DELETED_REF_NUMBER, 1).

%  Bit flag used to identify items with content of type [ItemContent::JSON].
-define(BLOCK_ITEM_JSON_REF_NUMBER, 2).

%  Bit flag used to identify items with content of type [ItemContent::Binary].
-define(BLOCK_ITEM_BINARY_REF_NUMBER, 3).

%  Bit flag used to identify items with content of type [ItemContent::String].
-define(BLOCK_ITEM_STRING_REF_NUMBER, 4).

%  Bit flag used to identify items with content of type [ItemContent::Embed].
-define(BLOCK_ITEM_EMBED_REF_NUMBER, 5).

%  Bit flag used to identify items with content of type [ItemContent::Format].
-define(BLOCK_ITEM_FORMAT_REF_NUMBER, 6).

%  Bit flag used to identify items with content of type [ItemContent::Number].
-define(BLOCK_ITEM_TYPE_REF_NUMBER, 7).

%  Bit flag used to identify items with content of type [ItemContent::Any].
-define(BLOCK_ITEM_ANY_REF_NUMBER, 8).

%  Bit flag used to identify items with content of type [ItemContent::Doc].
-define(BLOCK_ITEM_DOC_REF_NUMBER, 9).

%  Bit flag used to identify [Item::Skip].
-define(BLOCK_SKIP_REF_NUMBER, 10).

%  Bit flag used to identify items with content of type [ItemContent::Move].
-define(BLOCK_ITEM_MOVE_REF_NUMBER, 11).

%  Bit flag used to tell if encoded item has right origin defined.
-define(HAS_RIGHT_ORIGIN, 2#01000000).

%  Bit flag used to tell if encoded item has left origin defined.
-define(HAS_ORIGIN, 2#10000000).

%  Bit flag used to tell if encoded item has a parent subtitle defined. Subtitles are used only
%  for blocks which act as map-like types entries.
-define(HAS_PARENT_SUB, 2#00100000).

% Bit flag (9st bit) for item that is linked by Weak Link references
-define(ITEM_FLAG_LINKED, 2#0001_0000_0000).

% Bit flag (4th bit) for a marked item - not used atm.
-define(ITEM_FLAG_MARKED, 2#0000_1000).

% Bit flag (3rd bit) for a tombstoned (deleted) item.
-define(ITEM_FLAG_DELETED, 2#0000_0100).

% Bit flag (2nd bit) for an item, which contents are considered countable.
-define(ITEM_FLAG_COUNTABLE, 2#0000_0010).

% Bit flag (1st bit) used for an item which should be kept - not used atm.
-define(ITEM_FLAG_KEEP, 2#0000_0001).

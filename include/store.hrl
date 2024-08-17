% pub struct Store {
%     pub(crate) options: Options,

%     /// Root types (a.k.a. top-level types). These types are defined by users at the document level,
%     /// they have their own unique names and represent core shared types that expose operations
%     /// which can be called concurrently by remote peers in a conflict-free manner.
%     pub(crate) types: HashMap<Arc<str>, Arc<Branch>>,

%     /// Registry of all alive nodes in the document store.
%     pub(crate) node_registry: HashSet<BranchPtr>,

%     /// A block store of a current document. It represent all blocks (inserted or tombstoned
%     /// operations) integrated - and therefore visible - into a current document.
%     pub(crate) blocks: BlockStore,

%     /// A pending update. It contains blocks, which are not yet integrated into `blocks`, usually
%     /// because due to issues in update exchange, there were some missing blocks that need to be
%     /// integrated first before the data from `pending` can be applied safely.
%     pub(crate) pending: Option<PendingUpdate>,

%     /// A pending delete set. Just like `pending`, it contains deleted ranges of blocks that have
%     /// not been yet applied due to missing blocks that prevent `pending` update to be integrated
%     /// into `blocks`.
%     pub(crate) pending_ds: Option<DeleteSet>,

%     pub(crate) subdocs: HashMap<DocAddr, Doc>,

%     pub(crate) events: Option<Box<StoreEvents>>,

%     /// Pointer to a parent block - present only if a current document is a sub-document of another
%     /// document.
%     pub(crate) parent: Option<ItemPtr>,

%     /// Dependencies between items and weak links pointing to these items.
%     pub(crate) linked_by: HashMap<ItemPtr, HashSet<BranchPtr>>,
% }

-record(store, {
    types :: #{binary() => branch:branch()},
    node_registry :: node_registry:node_registry(),
    blocks :: block_store:block_store(),
    pending :: option:option(update:pending_update()),
    pending_ds :: option:option(update:delete_set()),
    subdocs :: #{reference() => doc:doc()},
    % events :: option:option(store_events:store_events()),
    parent :: option:option(id:id()),
    linked_by :: #{id:id() => sets:set(branch:branch())}
}).

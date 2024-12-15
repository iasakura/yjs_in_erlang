-record(subdocs, {
    added :: #{reference() => doc:doc()},
    removed :: #{reference() => doc:doc()},
    loaded :: #{reference() => doc:doc()}
}).

% /// Read-write transaction. It can be used to modify an underlying state of the corresponding [Doc].
% /// Read-write transactions require an exclusive access to document store - only one such
% /// transaction can be present per [Doc] at the same time (read-only [Transaction]s are not allowed
% /// to coexists at the same time as well).
% ///
% /// This transaction type stores the information about all of the changes performed in its scope.
% /// These will be used during [TransactionMut::commit] call to optimize metadata of incoming updates,
% /// triggering necessary event callbacks etc. For performance reasons it's preferred to batch as
% /// many updates as possible using the same transaction.
% ///
% /// In Yrs transactions are always auto-committing all of their changes when dropped. Rollbacks are
% /// not supported (if some operations needs to be undone, this can be achieved using [UndoManager])
% pub struct TransactionMut<'doc> {
%     pub(crate) store: AtomicRefMut<'doc, Store>,
%     /// State vector of a current transaction at the moment of its creation.
%     pub(crate) before_state: StateVector,
%     /// Current state vector of a transaction, which includes all performed updates.
%     pub(crate) after_state: StateVector,
%     /// ID's of the blocks to be merged.
%     pub(crate) merge_blocks: Vec<ID>,
%     /// Describes the set of deleted items by ids.
%     pub(crate) delete_set: DeleteSet,
%     /// We store the reference that last moved an item. This is needed to compute the delta
%     /// when multiple ContentMove move the same item.
%     pub(crate) prev_moved: HashMap<ItemPtr, ItemPtr>,
%     /// All types that were directly modified (property added or child inserted/deleted).
%     /// New types are not included in this Set.
%     pub(crate) changed: HashMap<TypePtr, HashSet<Option<Arc<str>>>>,
%     pub(crate) changed_parent_types: Vec<BranchPtr>,
%     pub(crate) subdocs: Option<Box<Subdocs>>,
%     pub(crate) origin: Option<Origin>,
%     doc: Doc,
%     committed: bool,
% }

-record(transaction_mut, {
    store :: store:store(),
    before_state :: state_vector:state_vector(),
    after_state :: state_vector:state_vector(),
    % TODO: support merge
    merge_blocks :: [id:id()],
    delete_set :: update:delete_set(),
    prev_moved :: #{id:id() => id:id()},
    changed :: #{type_ptr:type_ptr() => sets:set(option:option(binary()))},
    changed_parent_types :: [branch:branch()],
    subdocs :: option:option(transaction:subdocs()),
    % origin:: option:option(origin:origin()),
    doc :: doc:doc(),
    committed :: boolean(),
    owner :: pid()
}).

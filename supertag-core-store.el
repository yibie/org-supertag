;;; org-supertag/store.el --- Core data storage and atomic update for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file implements the central data store for the Org-Supertag
;; data-centric architecture. It provides a single source of truth for all
;; application state and ensures atomic updates and consistent change notifications.

;;; Code:

(require 'cl-lib)
(require 'ht) ; Ensures `ht` API availability
(require 'supertag-core-notify) ; For supertag-core-notify-handle-change and supertag-emit-event
(require 'supertag-core-state) ; For supertag--transaction-record-old-value

(declare-function supertag-mark-dirty "supertag-core-persistence")

;;; --- Core Data Store ---

(defvar supertag--store nil ; Initialize to nil, will be loaded by supertag-load-store
  "The central hash table for all application state.
Data is stored in a tree-like structure using nested hash tables.")

(defconst supertag--store-collections
  '(:nodes
    :tags
    :relations
    :embeds
    ;; Legacy nested field values (node -> tag -> field)
    :fields
    ;; Global field model (new)
    :field-definitions          ; field-id -> field plist
    :tag-field-associations     ; tag-id -> ordered list of association plists
    :field-values               ; node-id -> field-id -> value
    :boards                     ; board-id -> board plist (whiteboard layouts)
    :meta)
  "Canonical root collections maintained in `supertag--store'.")

(defconst supertag--canonical-collections
  '(:nodes :tags :relations :embeds :field-definitions)
  "Collections expected to contain entity plists keyed by identifier.")

(defconst supertag--not-found (make-symbol "supertag-not-found")
  "Sentinel used to signal missing entries during path resolution.")

(defun supertag-store--put-and-notify (collection id data &optional emit-event-p)
  "Internal helper to store DATA under COLLECTION/ID and optionally emit event."
  (let* ((bucket (supertag-store-get-collection collection))
         (existing (gethash id bucket supertag--not-found))
         (existed-p (not (eq existing supertag--not-found)))
         (canonical (supertag--normalize-entity data)))
    ;; Transaction seam: record the pre-write value (or "did not exist") the
    ;; first time this path is touched in the active transaction, so
    ;; `supertag-with-transaction' can roll it back on error. No-op when no
    ;; transaction is active.
    (supertag--transaction-record-old-value
     (list collection id) existed-p (if existed-p existing nil))
    (puthash id canonical bucket)
    (when emit-event-p
      (supertag-emit-event :store-changed (list collection id) nil canonical))
    canonical))

(defun supertag--ensure-store ()
  "Ensure `supertag--store' exists and has canonical collections."
  (unless (hash-table-p supertag--store)
    (setq supertag--store (ht-create)))
  (dolist (collection supertag--store-collections)
    (let ((bucket (gethash collection supertag--store 'missing)))
      (unless (hash-table-p bucket)
        (puthash collection (ht-create) supertag--store))))
  supertag--store)

(defun supertag--normalize-entity (value)
  "Return VALUE converted to canonical plist when it is a hash table."
  (cond
   ((hash-table-p value)
    (let (plist)
      (maphash (lambda (k v)
                 (setq plist (plist-put plist k v)))
               value)
      plist))
   (t value)))

(defun supertag-store-get-collection (collection)
  "Return hash table for COLLECTION, creating it if necessary."
  (supertag--ensure-store)
  (let ((bucket (gethash collection supertag--store 'missing)))
    (unless (hash-table-p bucket)
      (setq bucket (ht-create))
      (puthash collection bucket supertag--store))
    bucket))

(defun supertag-store-get-entity (collection id)
  "Return entity plist stored under COLLECTION keyed by ID."
  (gethash id (supertag-store-get-collection collection)))

(defun supertag-store-put-entity (collection id data &optional emit-event-p)
  "Store DATA under COLLECTION/ID.
When EMIT-EVENT-P is non-nil, emit :store-changed notification."
  (supertag-store--put-and-notify collection id data emit-event-p))

(defun supertag-store-remove-entity (collection id)
  "Remove entity under COLLECTION/ID. Returns removed value or nil."
  (let* ((bucket (supertag-store-get-collection collection))
         (old (and bucket (gethash id bucket))))
    (when old
      (supertag--transaction-record-old-value (list collection id) t old)
      (remhash id bucket)
      (supertag-emit-event :store-changed (list collection id) old nil))
    old))

(defun supertag--coerce-store-table (data)
  "Coerce DATA into a hash table representation of the store."
  (cond
   ((hash-table-p data)
    data)
   ((null data)
    (ht-create))
   ;; Property list layout: (:nodes <val> :tags <val> ...)
   ((and (listp data) (keywordp (car data)))
    (let ((table (ht-create))
          (cursor data))
      (while cursor
        (let ((key (pop cursor))
              (value (pop cursor)))
          (puthash key value table)))
      table))
   ;; Association list layout: ((:nodes . <val>) (:tags . <val>))
   ((and (listp data) (consp (car data)))
    (let ((table (ht-create)))
      (dolist (cell data table)
        (puthash (car cell) (cdr cell) table))))
   (t
    (let ((table (ht-create)))
      (puthash :data data table)
      table))))

(defun supertag--normalize-collection-value (collection value)
  "Normalize VALUE stored under COLLECTION into canonical hash tables/plists."
  (let ((bucket
         (cond
          ((hash-table-p value)
           value)
          ((null value)
           (ht-create))
          ;; Allow plist-based buckets: (id1 plist1 id2 plist2 ...)
          ((and (listp value) (not (null value)) (keywordp (car value)))
           (let ((table (ht-create))
                 (cursor value))
             (while cursor
               (let ((entry-key (pop cursor))
                     (entry-val (pop cursor)))
                 (puthash entry-key entry-val table)))
             table))
          ;; Alist buckets: ((id . plist) ...)
          ((and (listp value) (consp (car value)))
           (let ((table (ht-create)))
             (dolist (cell value table)
               (puthash (car cell) (cdr cell) table))))
          (t
           (let ((table (ht-create)))
             (puthash :value value table)
             table)))))
    (when (memq collection supertag--canonical-collections)
      (maphash
       (lambda (entity-id entity)
         (when (hash-table-p entity)
           (puthash entity-id (supertag--normalize-entity entity) bucket)))
       bucket))
    bucket))

(defun supertag-store-normalize! (&optional data)
  "Normalize DATA (or the live store) into canonical hash-table/plist layout.
Returns the normalized hash table and updates `supertag--store' when DATA is nil."
  ;; This function is deprecated due to its complexity and potential for data corruption.
  ;; It now acts as a pass-through to maintain API compatibility while preventing issues.
  (or data supertag--store))

;;; --- Global Field Collections (opt-in scaffolding) ---

(defun supertag-store-put-field-definition (field-id data &optional emit-event-p)
  "Store global field definition DATA under FIELD-ID."
  (let ((result (supertag-store--put-and-notify :field-definitions field-id data emit-event-p)))
    (when (fboundp 'supertag--maybe-rebuild-global-field-caches)
      (supertag--maybe-rebuild-global-field-caches))
    result))

(defun supertag-store-get-field-definition (field-id)
  "Fetch global field definition for FIELD-ID."
  (gethash field-id (supertag-store-get-collection :field-definitions)))

(defun supertag-store-remove-field-definition (field-id)
  "Remove global field definition FIELD-ID."
  (let ((old (supertag-store-remove-entity :field-definitions field-id)))
    (when (and old (fboundp 'supertag--maybe-rebuild-global-field-caches))
      (supertag--maybe-rebuild-global-field-caches))
    old))

(defun supertag-store-put-tag-field-associations (tag-id associations &optional emit-event-p)
  "Store ASSOCIATIONS (ordered list) for TAG-ID."
  (let ((result (supertag-store--put-and-notify :tag-field-associations tag-id associations emit-event-p)))
    (when (fboundp 'supertag--maybe-rebuild-global-field-caches)
      (supertag--maybe-rebuild-global-field-caches))
    result))

(defun supertag-store-get-tag-field-associations (tag-id)
  "Fetch field association list for TAG-ID."
  (gethash tag-id (supertag-store-get-collection :tag-field-associations)))

(defun supertag-store-remove-tag-field-associations (tag-id)
  "Remove field associations for TAG-ID."
  (let ((old (supertag-store-remove-entity :tag-field-associations tag-id)))
    (when (and old (fboundp 'supertag--maybe-rebuild-global-field-caches))
      (supertag--maybe-rebuild-global-field-caches))
    old))

(defun supertag-store-put-field-value (node-id field-id value &optional emit-event-p)
  "Set VALUE for FIELD-ID on NODE-ID."
  (let* ((root (supertag-store-get-collection :field-values))
         (node-table (gethash node-id root)))
    (unless (hash-table-p node-table)
      ;; This node's field-value bucket doesn't exist yet. Record that fact
      ;; *before* creating it so a rollback that undoes every field written
      ;; under it also removes the whole bucket again, instead of leaving an
      ;; empty shell that would inflate the :field-values entity count.
      (supertag--transaction-record-old-value (list :field-values node-id) nil nil)
      (setq node-table (ht-create))
      (puthash node-id node-table root))
    (let* ((had-value (ht-contains? node-table field-id))
           (old-value (and had-value (gethash field-id node-table))))
      (supertag--transaction-record-old-value
       (list :field-values node-id field-id) had-value old-value))
    (puthash field-id value node-table)
    (when emit-event-p
      (supertag-emit-event :store-changed (list :field-values node-id field-id) nil value))
    value))

(defun supertag-store-get-field-value (node-id field-id &optional default)
  "Get VALUE for FIELD-ID on NODE-ID, or DEFAULT if missing."
  (let* ((root (supertag-store-get-collection :field-values))
         (node-table (and (hash-table-p root) (gethash node-id root))))
    (if (and (hash-table-p node-table)
             (ht-contains? node-table field-id))
        (gethash field-id node-table)
      default)))

(defun supertag-store-remove-field-value (node-id field-id)
  "Remove FIELD-ID value for NODE-ID. Returns removed value or nil."
  (let* ((root (supertag-store-get-collection :field-values))
         (node-table (and (hash-table-p root) (gethash node-id root))))
    (when (and (hash-table-p node-table)
               (ht-contains? node-table field-id))
      (let ((old (gethash field-id node-table)))
        (supertag--transaction-record-old-value (list :field-values node-id field-id) t old)
        (remhash field-id node-table)
        (supertag-emit-event :store-changed (list :field-values node-id field-id) old nil)
        old))))

;;; --- Legacy Nested Field Collection (:fields) ---
;;
;; `:fields' stores node-id -> tag-id -> field-name -> value as three
;; levels of nested (real, `equal'-test) hash tables — never plists. This
;; is the one collection shape `supertag--normalize-entity' must NEVER be
;; allowed to touch: it unconditionally flattens a hash-table VALUE into a
;; plist, which is exactly correct for the ordinary entity collections it
;; was designed for (:nodes, :tags, ...) but would corrupt a per-node field
;; table. The functions below are the transactional seam for this
;; collection, mirroring `supertag-store-put-field-value'/
;; `supertag-store-remove-field-value' for `:field-values': they record a
;; rollback marker the first time each level (node table, tag table, field
;; slot) is touched in an active transaction, using path shapes
;; `(:fields NODE-ID)', `(:fields NODE-ID TAG-ID)', and
;; `(:fields NODE-ID TAG-ID FIELD-NAME)' — all understood by dedicated
;; arms in `supertag--transaction-restore-entry' (supertag-core-transform.el)
;; that restore by direct `puthash'/`remhash' on the live hash tables,
;; bypassing `supertag-store-put-entity'/`supertag--normalize-entity'
;; entirely.

(defun supertag--legacy-field-coerce-table (data)
  "Coerce DATA into a hash table for the legacy `:fields' collection.
Handles the shapes that may still linger from older on-disk formats:
hash tables (returned as-is), alists, flat plist-style key/value lists,
and nil. Anything else is stored under a `:value' key so data is never
silently dropped."
  (cond
   ((hash-table-p data) data)
   ((null data) (ht-create))
   ((and (listp data) (consp (car data)))
    (let ((table (ht-create)))
      (dolist (cell data table)
        (puthash (car cell) (cdr cell) table))))
   ((and (listp data) (zerop (% (length data) 2)))
    (let ((table (ht-create))
          (cursor data))
      (while cursor
        (let ((key (pop cursor))
              (value (pop cursor)))
          (puthash key value table)))
      table))
   (t
    (let ((table (ht-create)))
      (puthash :value data table)
      table))))

(defun supertag-store-put-legacy-field (node-id tag-id field-name value)
  "Set VALUE for FIELD-NAME under TAG-ID for NODE-ID in the legacy
nested `:fields' collection, creating the per-node and per-tag hash
tables on demand. Records a rollback marker at each level the first
time it is touched in an active transaction (see the Commentary above
this section). Returns VALUE."
  (let* ((fields-root (supertag-store-get-collection :fields))
         (raw-node (gethash node-id fields-root))
         (node-table
          (cond
           ((hash-table-p raw-node) raw-node)
           (raw-node
            ;; Legacy list-shaped data: migrate to a hash table, recording
            ;; the original value so a rollback restores it verbatim.
            (supertag--transaction-record-old-value (list :fields node-id) t raw-node)
            (let ((table (supertag--legacy-field-coerce-table raw-node)))
              (puthash node-id table fields-root)
              table))
           (t
            (supertag--transaction-record-old-value (list :fields node-id) nil nil)
            (let ((table (ht-create)))
              (puthash node-id table fields-root)
              table)))))
    (let* ((raw-tag (gethash tag-id node-table))
           (tag-table
            (cond
             ((hash-table-p raw-tag) raw-tag)
             (raw-tag
              (supertag--transaction-record-old-value (list :fields node-id tag-id) t raw-tag)
              (let ((table (supertag--legacy-field-coerce-table raw-tag)))
                (puthash tag-id table node-table)
                table))
             (t
              (supertag--transaction-record-old-value (list :fields node-id tag-id) nil nil)
              (let ((table (ht-create)))
                (puthash tag-id table node-table)
                table)))))
      (let* ((had-value (ht-contains? tag-table field-name))
             (old-value (and had-value (gethash field-name tag-table))))
        (supertag--transaction-record-old-value
         (list :fields node-id tag-id field-name) had-value old-value))
      (puthash field-name value tag-table)
      value)))

(defun supertag-store-remove-legacy-field (node-id tag-id field-name)
  "Remove FIELD-NAME under TAG-ID for NODE-ID from the legacy nested
`:fields' collection. Returns the removed value, or nil when absent.
Records a rollback marker before mutating, mirroring
`supertag-store-put-legacy-field'. Never creates node/tag tables that
do not already exist."
  (let* ((fields-root (supertag-store-get-collection :fields))
         (raw-node (gethash node-id fields-root))
         (node-table (cond ((hash-table-p raw-node) raw-node)
                            (raw-node (supertag--legacy-field-coerce-table raw-node)))))
    (when node-table
      (let* ((raw-tag (gethash tag-id node-table))
             (tag-table (cond ((hash-table-p raw-tag) raw-tag)
                               (raw-tag (supertag--legacy-field-coerce-table raw-tag)))))
        (when (and tag-table (ht-contains? tag-table field-name))
          (let ((old (gethash field-name tag-table)))
            (supertag--transaction-record-old-value
             (list :fields node-id tag-id field-name) t old)
            (remhash field-name tag-table)
            old))))))

(defun supertag-store-remove-legacy-tag-fields (node-id tag-id)
  "Remove the complete legacy field table for NODE-ID and TAG-ID.
Returns the removed table, or nil when absent.  The table-level write is
recorded by the active transaction so a failed migration restores it."
  (let* ((fields-root (supertag-store-get-collection :fields))
         (node-table (gethash node-id fields-root))
         (tag-table (and (hash-table-p node-table)
                         (gethash tag-id node-table))))
    (when tag-table
      (supertag--transaction-record-old-value
       (list :fields node-id tag-id) t tag-table)
      (remhash tag-id node-table)
      tag-table)))

;; Direct storage is now the default and only mode for optimal performance
;; This hybrid architecture combines old system performance with new system features

(defun supertag-store-direct-set (collection id data)
  "Directly set data in the store using old-style format.
COLLECTION is the collection name (:nodes, :tags, :relations, :embeds).
ID is the entity ID. DATA is the plist data."
  ;; Ensure store is initialized
  (supertag-store-put-entity collection id data t))

(defun supertag-store-direct-get (collection id)
  "Directly get data from the store using old-style format.
COLLECTION is the collection name. ID is the entity ID."
  (supertag-store-get-entity collection id))

;;; --- Canonical Path Resolution ---

(defun supertag--resolve-path (container path)
  "Traverse CONTAINER following PATH and return the located value or `supertag--not-found'.
CONTAINER may be a hash table, plist, or alist. PATH is a list of keys."
  (if (null path)
      container
    (let ((key (car path))
          (rest (cdr path)))
      (cond
       ;; Nothing left to traverse: missing
       ((null container)
        supertag--not-found)
       ;; Hash-table navigation
       ((hash-table-p container)
        (let ((value (gethash key container supertag--not-found)))
          (if (eq value supertag--not-found)
              supertag--not-found
            (if rest
                (supertag--resolve-path value rest)
              value))))
       ;; Keyword lookup in plist
       ((and (listp container) (keywordp key))
        (let ((cell (plist-member container key)))
          (if cell
              (let ((value (cadr cell)))
                (if rest
                    (supertag--resolve-path value rest)
                  value))
            supertag--not-found)))
       ;; Association list lookup (string/symbol keys)
       ((and (listp container) (consp (car container)))
        (let ((cell (assoc key container)))
          (if cell
              (let ((value (cdr cell)))
                (if rest
                    (supertag--resolve-path value rest)
                  value))
            supertag--not-found)))
       ;; Fallback: treat as flat key/value list (\"key\" value ...)
       ((listp container)
        (let ((cursor container)
              (found supertag--not-found))
          (while (and cursor (not (eq found supertag--not-found)))
            (let ((entry-key (car cursor))
                  (entry-val (cadr cursor)))
              (when (equal entry-key key)
                (setq found entry-val))
              (setq cursor (cddr cursor))))
          (if (eq found supertag--not-found)
              supertag--not-found
            (if rest
                (supertag--resolve-path found rest)
              found))))
       (t
        supertag--not-found)))))

;;; --- Change Notification ---

(defun supertag--notify-change (path old-value new-value)
  "Trigger a change notification for PATH with OLD-VALUE and NEW-VALUE.
This function acts as a bridge to the actual notification system in `supertag-core-notify.el`."
  ;; This function assumes `supertag-core-notify-handle-change` is defined elsewhere.
  ;; It's a forward declaration to break circular dependency.
  (when (fboundp 'supertag-core-notify-handle-change)
    (supertag-core-notify-handle-change path old-value new-value)))

;;; --- Public API for Data Storage ---

(defun supertag-get (path &optional default)
  "Get data from the store by PATH.
PATH is a list of keys (e.g., '(:nodes \"123\" :tags)).
Supports mixed structures: hash-tables and plists."
  ;; Ensure store is initialized
  (supertag--ensure-store)
  (let ((resolved (supertag--resolve-path supertag--store path)))
    (if (eq resolved supertag--not-found)
        default
      resolved)))

(defun supertag-update (path value)
  "Atomically update a value in the central store at PATH.
 PATH is a list of keys. Returns the old value.
 Triggers change notifications unless suppressed.
 Stores plist values directly.

In canonical mode PATH must reference either a collection (:nodes) or
a collection entity (:nodes \"id\")."
  ;; Ensure store is initialized
  (supertag--ensure-store)

  (unless (and (listp path) path)
    (error "PATH must be a non-empty list, got: %S" path))

  (pcase path
    (`(,collection)
     (let* ((old-value (supertag-get path supertag--not-found))
            (normalized (supertag--normalize-collection-value collection value)))
       (if (and (not (eq old-value supertag--not-found))
                (equal old-value normalized))
           old-value
         (let ((old (if (eq old-value supertag--not-found) nil old-value)))
           (supertag--transaction-record-old-value
            path (not (eq old-value supertag--not-found)) old)
           (puthash collection normalized supertag--store)
           (supertag--notify-change path old normalized)
           (supertag-emit-event :store-changed path old normalized))
         (if (eq old-value supertag--not-found) nil old-value))))
    (`(,collection ,id)
     (let* ((canonical (supertag--normalize-entity value))
            (old-value (supertag-store-get-entity collection id)))
       (if (equal old-value canonical)
           old-value
         (supertag-store-put-entity collection id canonical)
         (supertag--notify-change path old-value canonical)
         (supertag-emit-event :store-changed path old-value canonical)
         old-value)))
    (_
     (error "Canonical store update only supports collection/entity paths, got: %S" path))))

(defun supertag-delete (path)
  "Atomically delete a value from the central store at PATH."
  ;; Ensure store is initialized
  (supertag--ensure-store)

  (unless (and (listp path) path)
    (error "PATH must be a non-empty list, got: %S" path))

  (pcase path
    (`(,collection)
     (let ((existing (supertag-get path supertag--not-found)))
       (if (eq existing supertag--not-found)
           nil
         (let ((cleared (supertag--normalize-collection-value collection nil)))
           (supertag--transaction-record-old-value path t existing)
           (puthash collection cleared supertag--store)
           (supertag--notify-change path existing nil)
           (supertag-emit-event :store-changed path existing nil)
           existing))))
    (`(,collection ,id)
     (let ((old-value (supertag-store-remove-entity collection id)))
       (when old-value
         (supertag--notify-change path old-value nil))
       old-value))
    (_
     (error "Canonical store delete only supports collection/entity paths, got: %S" path))))

(defun supertag-store-clear ()
  "Clear the entire data store.
This is primarily intended for testing and system resets."
  (interactive)
  (setq supertag--store (ht-create))
  ;; Clear relation indexes if loaded
  (when (fboundp 'supertag-index-rebuild-relations)
    (supertag-index-rebuild-relations))
  (message "Supertag store has been cleared."))

;;; --- Unified Commit Pipeline ---

(defvar supertag-before-operation-hook nil
  "Hook run before `supertag-ops-commit`.
Each function receives a plist containing at least
`:operation', `:collection', `:id', `:path', `:context', and `:previous'.")

(defvar supertag-after-operation-hook nil
  "Hook run after `supertag-ops-commit`.
Each function receives a plist containing the commit payload plus
`:current' and `:changed' keys.")

(defun supertag-ops-commit (&rest spec)
  "Execute a datastore mutation described by SPEC and broadcast a unified event.
Required keys in SPEC:
- :operation — keyword describing the logical action (:create, :update, :delete, ...).
- :perform   — thunk that performs the mutation (must be non-nil unless :new or :result supplied).

Optional keys:
- :collection — top-level store collection (e.g., :nodes).
- :id         — entity identifier within COLLECTION.
- :path       — explicit path used for event payloads when COLLECTION/ID is not enough.
- :context    — arbitrary metadata passed through to hooks and listeners.
- :previous   — precomputed previous value (otherwise derived from store when possible).
- :new        — explicitly provide resulting value (skips post-fetch).
- :result     — fallback return value when no collection is associated.
- :force-event — emit events even when :previous and :new compare equal.
- :suppress-mark-dirty — inhibit automatic dirty flag toggling.

Returns the updated entity when available, otherwise falls back to :result or :previous."
  (let* ((operation (plist-get spec :operation))
         (perform (plist-get spec :perform))
         (collection (plist-get spec :collection))
         (entity-id (plist-get spec :id))
         (explicit-path (plist-get spec :path))
         (context (plist-get spec :context)))
    (unless operation
      (error "supertag-ops-commit requires :operation"))
    (unless (or (functionp perform)
                (plist-member spec :new)
                (plist-member spec :result))
      (error "supertag-ops-commit requires :perform or :new/:result"))
    (let* ((path (or explicit-path
                     (when collection
                       (if entity-id
                           (list collection entity-id)
                         (list collection)))))
           (previous (if (plist-member spec :previous)
                         (plist-get spec :previous)
                       (when (and collection entity-id)
                         (supertag-store-get-entity collection entity-id))))
           (before-payload (list :operation operation
                                 :collection collection
                                 :id entity-id
                                 :path path
                                 :context context
                                 :previous previous)))
      (run-hook-with-args 'supertag-before-operation-hook before-payload)
      (let* ((result (cond
                      ((functionp perform) (funcall perform))
                      ((plist-member spec :result) (plist-get spec :result))
                      (t nil)))
             (current (cond
                       ((plist-member spec :new) (plist-get spec :new))
                       ((and collection entity-id)
                        (supertag-store-get-entity collection entity-id))
                       ((plist-member spec :result) (plist-get spec :result))
                       (t result)))
             (changed (or (plist-get spec :force-event)
                          (not (equal previous current))))
             (event (plist-put (plist-put (copy-sequence before-payload)
                                          :current current)
                               :changed changed)))
        (when (and changed
                   (not (plist-get spec :suppress-mark-dirty))
                   (fboundp 'supertag-mark-dirty))
          (supertag-mark-dirty))
        (when changed
          (let ((event-payload (plist-put event :result result)))
            (supertag-emit-event :store-committed event-payload)
            ;; Backward compatible signal for existing subscribers.
            (when path
              (supertag-emit-event :store-changed path previous current))))
        (run-hook-with-args 'supertag-after-operation-hook event)
        (cond
         ((plist-member spec :return) (plist-get spec :return))
         (current current)
         (result result)
         (t previous))))))

(provide 'supertag-core-store)

;;; supertag-core-store.el ends here

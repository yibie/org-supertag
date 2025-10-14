;;; org-supertag/store.el --- Core data storage and atomic update for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file implements the central data store for the Org-Supertag
;; data-centric architecture. It provides a single source of truth for all
;; application state and ensures atomic updates and consistent change notifications.

;;; Code:

(require 'cl-lib)
(require 'ht) ; Ensures `ht` API availability
(require 'supertag-core-notify) ; For supertag-core-notify-handle-change and supertag-emit-event

(declare-function supertag-mark-dirty "supertag-core-persistence")

;;; --- Core Data Store ---

(defvar supertag--store nil ; Initialize to nil, will be loaded by supertag-load-store
  "The central hash table for all application state.
Data is stored in a tree-like structure using nested hash tables.")

(defconst supertag--store-collections
  '(:nodes :tags :relations :embeds :fields :meta)
  "Canonical root collections maintained in `supertag--store'.")

(defconst supertag--canonical-collections
  '(:nodes :tags :relations :embeds)
  "Collections expected to contain entity plists keyed by identifier.")

(defconst supertag--not-found (make-symbol "supertag-not-found")
  "Sentinel used to signal missing entries during path resolution.")

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
  (let ((bucket (supertag-store-get-collection collection))
        (canonical (supertag--normalize-entity data)))
    (puthash id canonical bucket)
    (when emit-event-p
      (supertag-emit-event :store-changed (list collection id) nil canonical))
    canonical))

(defun supertag-store-remove-entity (collection id)
  "Remove entity under COLLECTION/ID. Returns removed value or nil."
  (let* ((bucket (supertag-store-get-collection collection))
         (old (and bucket (gethash id bucket))))
    (when old
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
          ;; Flat key/value list with string or symbol keys: (id1 plist1 id2 plist2 ...)
          ((and (listp value) (zerop (% (length value) 2)))
           (let ((table (ht-create))
                 (cursor value))
             (while cursor
               (let ((entry-key (pop cursor))
                     (entry-val (pop cursor)))
                 (puthash entry-key entry-val table)))
             table))
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
  (let* ((raw (or data supertag--store))
         (table (supertag--coerce-store-table raw)))
    (maphash
     (lambda (collection value)
       (when (memq collection supertag--store-collections)
         (puthash collection
                  (supertag--normalize-collection-value collection value)
                  table)))
     table)
    (dolist (collection supertag--store-collections)
      (unless (gethash collection table)
        (puthash collection
                 (supertag--normalize-collection-value collection nil)
                 table)))
    (when (null data)
      (setq supertag--store table))
    table))

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
         (puthash collection normalized supertag--store)
         (let ((old (if (eq old-value supertag--not-found) nil old-value)))
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

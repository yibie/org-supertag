;;; org-supertag/store.el --- Core data storage and atomic update for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file implements the central data store for the Org-Supertag
;; data-centric architecture. It provides a single source of truth for all
;; application state and ensures atomic updates and consistent change notifications.

;;; Code:

(require 'ht)     ; For hash-table operations (ht-create, ht-get, ht-set, ht-remove)
(require 'supertag-core-notify) ; For supertag-core-notify-handle-change and supertag-emit-event

;;; --- Core Data Store ---

(defvar supertag--store nil ; Initialize to nil, will be loaded by supertag-load-store
  "The central hash table for all application state.
Data is stored in a tree-like structure using nested hash tables.")

(defvar supertag--store-indexes nil
  "The index hash table for fast query performance.
Contains multiple sub-indexes:
  :tags - Hash table mapping tag names to lists of node IDs
  :words - Inverted index mapping words to lists of node IDs
  :fields - Hash table mapping field keys to value->node-id mappingswojt
  :dates - Hash table for date-based queries")

;; Direct storage is now the default and only mode for optimal performance
;; This hybrid architecture combines old system performance with new system features

(defun supertag-store-direct-set (collection id data)
  "Directly set data in the store using old-style format.
COLLECTION is the collection name (:nodes, :tags, :relations, :embeds).
ID is the entity ID. DATA is the plist data."
  ;; Ensure store is initialized
  (unless (hash-table-p supertag--store)
    (setq supertag--store (ht-create)))
  
  (let ((collection-table (or (gethash collection supertag--store)
                              (let ((new-table (ht-create)))
                                (puthash collection new-table supertag--store)
                                new-table)))
        (old-data (when (and (hash-table-p supertag--store)
                            (gethash collection supertag--store))
                    (gethash id (gethash collection supertag--store)))))
    (puthash id data collection-table)
    ;; Update indexes for nodes
    (when (eq collection :nodes)
      (supertag--update-indexes-for-node id old-data data))
    ;; Emit change event
    (supertag-emit-event :store-changed (list collection id) nil data)
    data))

(defun supertag-store-direct-get (collection id)
  "Directly get data from the store using old-style format.
COLLECTION is the collection name. ID is the entity ID."
  ;; Ensure store is initialized
  (unless (hash-table-p supertag--store)
    (setq supertag--store (ht-create)))
  
  (let ((collection-table (gethash collection supertag--store)))
    (when collection-table
      (gethash id collection-table))))

;;; --- Internal Helpers for Nested Hash Table Access ---

(defun supertag--get-nested-ht (table path &optional default)
  "Get a value from a nested hash table at PATH.
TABLE is the hash table. PATH is a list of keys (e.g., '(:nodes \"123\" :title)).
Returns DEFAULT if path not found."
  (if (null path)
      table
    (let ((key (car path))
          (rest (cdr path)))
      (if rest
          (let ((subtable (gethash key table 'not-found)))
            (if (eq subtable 'not-found)
                default
              (supertag--get-nested-ht subtable rest default))) ; Fixed: added missing 'rest' parameter
        (gethash key table default)))))

(defun supertag--set-nested-ht (table path value)
  "Set a VALUE in a nested hash table at PATH.
TABLE is the hash table. PATH is a list of keys. Creates intermediate hash tables if necessary."
  (if (null (cdr path))
      (puthash (car path) value table)
    (let* ((key (car path))
           (subtable (gethash key table)))
      (unless (hash-table-p subtable)
        (setq subtable (ht-create))
        (puthash key subtable table))
      (supertag--set-nested-ht subtable (cdr path) value)))
  table)

(defun supertag--remove-nested-ht (table path)
  "Remove a value from a nested hash table at PATH.
Returns t if a value was removed, nil otherwise."
  (when table
    (let ((key (car path))
          (rest (cdr path)))
      (if rest
          (let ((subtable (gethash key table)))
            (when (hash-table-p subtable)
              (supertag--remove-nested-ht subtable rest)))
        (remhash key table)))))

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
  "Get data from the store by PATH."
  ;; Ensure store is initialized
  (unless (hash-table-p supertag--store)
    (setq supertag--store (ht-create)))
  
  (let ((value supertag--store))
    (catch 'supertag-get-early-exit
      (dolist (key path value) ; `value` is what dolist returns if loop completes
        (unless (hash-table-p value)
          (throw 'supertag-get-early-exit default))
        (setq value (gethash key value))))))

(defun supertag-update (path value)
  "Atomically update a value in the central store at PATH.
 PATH is a list of keys. Returns the old value.
 Triggers change notifications unless suppressed.
 Stores plist values directly."
  ;; Ensure store is initialized
  (unless (hash-table-p supertag--store)
    (setq supertag--store (ht-create)))
  
  (let ((old-value (supertag-get path))
        (full-old-node-data nil))
    ;; If we are updating any part of a node, capture its full state BEFORE the change.
    (when (and (>= (length path) 2) (eq (car path) :nodes))
      (setq full-old-node-data (supertag-get (list :nodes (cadr path)))))

    (when (not (equal old-value value))
      (supertag--set-nested-ht supertag--store path value)
      ;; If a node was updated, trigger a full re-index for that node
      ;; using the before and after states.
      (when full-old-node-data
        (let* ((node-id (cadr path))
               (full-new-node-data (supertag-get (list :nodes node-id))))
          (supertag--update-indexes-for-node node-id full-old-node-data full-new-node-data)))
      (supertag--notify-change path old-value value)
      ;; Emit a generic store-changed event for persistence layer to listen to
      (supertag-emit-event :store-changed path old-value value))
    old-value))

(defun supertag-delete (path)
  "Atomically delete a value from the central store at PATH."
  ;; Ensure store is initialized
  (unless (hash-table-p supertag--store)
    (setq supertag--store (ht-create)))
  
  (let ((old-value (supertag-get path)))
    (when old-value ; Only do something if a value actually exists
      (supertag--remove-nested-ht supertag--store path)
      ;; Update indexes for nodes
      (when (and (= (length path) 2) (eq (car path) :nodes))
        (supertag--update-indexes-for-node (cadr path) old-value nil))
      (supertag--notify-change path old-value nil)
      (supertag-emit-event :store-changed path old-value nil))
    old-value))

(defun supertag-store-clear ()
  "Clear the entire data store and all indexes.
This is primarily intended for testing and system resets."
  (interactive)
  (setq supertag--store (ht-create))
  (setq supertag--store-indexes (ht-create))
  (supertag--ensure-indexes-initialized) ; Re-initialize the basic index structure
  (message "Supertag store and indexes have been cleared."))

;;; --- Index Management ---

(defun supertag--ensure-indexes-initialized ()
  "Ensure all indexes are properly initialized."
  (unless supertag--store-indexes
    (setq supertag--store-indexes (ht-create))
    (puthash :tags (ht-create) supertag--store-indexes)
    (puthash :words (ht-create) supertag--store-indexes)
    (puthash :fields (ht-create) supertag--store-indexes)
    (puthash :dates (ht-create) supertag--store-indexes)))

(defun supertag--update-indexes-for-node (node-id old-data new-data)
  "Update all indexes for a node change.
NODE-ID is the node identifier.
OLD-DATA is the previous node data (nil if creating).
NEW-DATA is the new node data (nil if deleting)."
  (supertag--ensure-indexes-initialized)
  
  ;; Remove old data from indexes
  (when old-data
    (supertag--remove-from-tag-index node-id old-data)
    (supertag--remove-from-word-index node-id old-data)
    (supertag--remove-from-date-index node-id old-data))
  
  ;; Add new data to indexes
  (when new-data
    (supertag--add-to-tag-index node-id new-data)
    (supertag--add-to-word-index node-id new-data)
    (supertag--add-to-date-index node-id new-data)))

(defun supertag--add-to-tag-index (node-id node-data)
  "Add node to tag index."
  (let ((tags (plist-get node-data :tags))
        (tag-index (gethash :tags supertag--store-indexes)))
    (when tags
      (dolist (tag tags)
        (let ((nodes-list (gethash tag tag-index)))
          (unless (member node-id nodes-list)
            (puthash tag (cons node-id nodes-list) tag-index)))))))

(defun supertag--remove-from-tag-index (node-id node-data)
  "Remove node from tag index."
  (let ((tags (plist-get node-data :tags))
        (tag-index (gethash :tags supertag--store-indexes)))
    (when tags
      (dolist (tag tags)
        (let ((nodes-list (gethash tag tag-index)))
          (when nodes-list
            (puthash tag (remove node-id nodes-list) tag-index)))))))

(defun supertag--add-to-word-index (node-id node-data)
  "Add node to word index for full-text search."
  (let ((title (plist-get node-data :title))
        (content (plist-get node-data :content))
        (word-index (gethash :words supertag--store-indexes)))
    ;; Index words from title
    (when title
      (supertag--index-words-from-text title node-id word-index))
    ;; Index words from content
    (when content
      (supertag--index-words-from-text content node-id word-index))))

(defun supertag--remove-from-word-index (node-id node-data)
  "Remove node from word index."
  (let ((title (plist-get node-data :title))
        (content (plist-get node-data :content))
        (word-index (gethash :words supertag--store-indexes)))
    ;; Remove words from title
    (when title
      (supertag--unindex-words-from-text title node-id word-index))
    ;; Remove words from content
    (when content
      (supertag--unindex-words-from-text content node-id word-index))))

(defun supertag--add-to-date-index (node-id node-data)
  "Add node to date index."
  (let ((created-at (plist-get node-data :created-at))
        (modified-at (plist-get node-data :modified-at))
        (date-index (gethash :dates supertag--store-indexes)))
    (when created-at
      (let ((created-nodes (gethash :created-at date-index)))
        (unless (member node-id created-nodes)
          (puthash :created-at (cons (cons node-id created-at) created-nodes) date-index))))
    (when modified-at
      (let ((modified-nodes (gethash :modified-at date-index)))
        (unless (member node-id modified-nodes)
          (puthash :modified-at (cons (cons node-id modified-at) modified-nodes) date-index))))))

(defun supertag--remove-from-date-index (node-id node-data)
  "Remove node from date index."
  (let ((date-index (gethash :dates supertag--store-indexes)))
    (let ((created-nodes (gethash :created-at date-index)))
      (when created-nodes
        (puthash :created-at (cl-remove node-id created-nodes :key #'car) date-index)))
    (let ((modified-nodes (gethash :modified-at date-index)))
      (when modified-nodes
        (puthash :modified-at (cl-remove node-id modified-nodes :key #'car) date-index)))))

(defun supertag--index-words-from-text (text node-id word-index)
  "Extract and index words from TEXT for NODE-ID."
  (when (stringp text)
    ;; Simple word extraction: split by non-word characters and index
    (let ((words (split-string (downcase text) "[^[:word:]]+" t)))
      (dolist (word words)
        (when (> (length word) 2) ; Only index words longer than 2 characters
          (let ((nodes-list (gethash word word-index)))
            (unless (member node-id nodes-list)
              (puthash word (cons node-id nodes-list) word-index))))))))

(defun supertag--unindex-words-from-text (text node-id word-index)
  "Remove words from TEXT for NODE-ID from word index."
  (when (stringp text)
    (let ((words (split-string (downcase text) "[^[:word:]]+" t)))
      (dolist (word words)
        (when (> (length word) 2)
          (let ((nodes-list (gethash word word-index)))
            (when nodes-list
              (puthash word (remove node-id nodes-list) word-index))))))))

;;; --- Index Query Functions ---

(defun supertag-index-get-nodes-by-tag (tag-name)
  "Fast query: get all nodes with TAG-NAME using tag index."
  (supertag--ensure-indexes-initialized)
  (let ((tag-index (gethash :tags supertag--store-indexes)))
    (let ((result (or (gethash tag-name tag-index) '())))
      ;;(message "Supertag Store Debug: Tag '%s' index lookup returned %d nodes: %s" tag-name (length result) result)
      result)))

(defun supertag-index-get-nodes-by-word (word)
  "Fast query: get all nodes containing WORD using word index."
  (supertag--ensure-indexes-initialized)
  (let ((word-index (gethash :words supertag--store-indexes)))
    (or (gethash (downcase word) word-index) '())))

(defun supertag-index-get-nodes-by-date-range (start-time end-time &optional date-field)
  "Fast query: get all nodes created/modified within date range using date index.
DATE-FIELD can be :created-at or :modified-at (default :created-at)."
  (supertag--ensure-indexes-initialized)
  (let* ((field (or date-field :created-at))
         (date-index (gethash :dates supertag--store-indexes))
         (date-entries (gethash field date-index))
         (matching-nodes '()))
    (dolist (entry date-entries)
      (let ((node-id (car entry))
            (node-time (cdr entry)))
        (when (and node-time (listp node-time) (>= (length node-time) 2))
          ;; For 'after' queries: node-time should be LATER than start-time
          ;; For 'before' queries: node-time should be EARLIER than end-time
          ;; Fix: For 'after' queries, we want nodes with node-time > start-time
          ;; Fix: For 'before' queries, we want nodes with node-time < end-time
          (let ((start-check (or (null start-time) (time-less-p start-time node-time)))
                (end-check (or (null end-time) (time-less-p node-time end-time))))
            ;;(message "Supertag Store Debug: Checking node %s with time %s" node-id node-time)
            ;;(message "Supertag Store Debug: Start time: %s, End time: %s" start-time end-time)
            ;;(message "Supertag Store Debug: Start check: %s, End check: %s" start-check end-check)
            (when (and start-check end-check)
              ;;(message "Supertag Store Debug: Node %s matches date range" node-id)
              (push node-id matching-nodes))))))
    ;;(message "Supertag Store Debug: Date range query returned %d nodes: %s" (length matching-nodes) matching-nodes)
    matching-nodes))

(defun supertag--rebuild-all-indexes ()
  "Rebuild all indexes from scratch based on current store data.
Useful for initialization or after major data changes."
  (supertag--ensure-indexes-initialized)
  
  ;; Clear all indexes
  (puthash :tags (ht-create) supertag--store-indexes)
  (puthash :words (ht-create) supertag--store-indexes)
  (puthash :fields (ht-create) supertag--store-indexes)
  (puthash :dates (ht-create) supertag--store-indexes)
  
  ;; Rebuild from current data
  (let ((nodes-collection (gethash :nodes supertag--store)))
    (when (hash-table-p nodes-collection)
      (maphash
       (lambda (node-id node-data)
         (supertag--update-indexes-for-node node-id nil node-data))
       nodes-collection))))

;;; --- High-performance API Query Functions (for internal modules) ---

(defun supertag-find-nodes-by-tag (tag-name)
  "Find all nodes that have TAG-NAME using index for optimal performance.
TAG-NAME is the name of the tag to search for.
Returns a list of (node-id . node-data) pairs."
  (let ((node-ids (supertag-index-get-nodes-by-tag tag-name))
        (results '()))
    (dolist (node-id node-ids)
      (when-let ((node-data (supertag-store-direct-get :nodes node-id)))
        (push (cons node-id node-data) results)))
    (nreverse results)))

(defun supertag-find-nodes-by-file (file-path)
  "Find all nodes located in FILE-PATH.
This version uses a single, robust pass to find nodes, normalizing
file paths before comparison to ensure correctness.
Returns a list of (node-id . node-data) pairs."
  (let ((nodes-collection (supertag-get '(:nodes)))
        (found-nodes '())
        ;; Normalize the target path once at the beginning for efficiency.
        (normalized-query-path (expand-file-name file-path)))
    (when (hash-table-p nodes-collection)
      (maphash
       (lambda (id node-data)
         ;; Safely extract :file and ensure it's a string
         (when-let* ((node-file (and node-data (plist-get node-data :file)))
                     ((stringp node-file)))
           ;; Normalize both paths before comparing
           (let ((normalized-node-file (expand-file-name node-file)))
             (when (equal normalized-node-file normalized-query-path)
               (push (cons id node-data) found-nodes)))))
       nodes-collection))
    (nreverse found-nodes)))

(defun supertag-find-nodes-by-title (title-pattern)
  "Find all nodes whose title matches TITLE-PATTERN using optimized search.
TITLE-PATTERN is a regular expression string.
Returns a list of (node-id . node-data) pairs."
  (let ((all-nodes (supertag-query '(:nodes)))
        (results '()))
    (dolist (node-pair all-nodes)
      (let ((node-id (car node-pair))
            (node-data (cdr node-pair)))
        (when (string-match-p title-pattern (plist-get node-data :title))
          (push (cons node-id node-data) results))))
    (nreverse results)))

(defun supertag-find-nodes (predicates)
  "Find nodes using multiple predicates with optimized filtering.
PREDICATES is a list of predicate functions. Each predicate function receives (id . data) and returns t or nil.
Returns a list of (node-id . node-data) pairs that satisfy all predicates."
  (let ((all-nodes (supertag-query '(:nodes)))
        (results '()))
    (dolist (node-pair all-nodes)
      (let ((id (car node-pair))
            (data (cdr node-pair)))
        (when (cl-every (lambda (pred) (funcall pred id data)) predicates)
          (push (cons id data) results))))
    (nreverse results)))

(defun supertag-index-node-has-tag-p (node-id tag-name)
  "Check if a node has a specific tag using index for optimal performance.
NODE-ID is the node identifier.
TAG-NAME is the tag name to check.
Returns t if the node has the tag, nil otherwise."
  (let ((node-tags (supertag-index-get-nodes-by-tag tag-name)))
    (member node-id node-tags)))

(provide 'supertag-core-store)

;;; supertag-core-store.el ends here
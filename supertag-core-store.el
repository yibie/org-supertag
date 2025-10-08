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
  "Get a value from a nested hash table or plist/alist at PATH.
TABLE is the hash table. PATH is a list of keys (e.g., '(:nodes \"123\" :title)).
Handles mixed structures: hash-tables at top levels, plists/alists at entity level.
Returns DEFAULT if path not found."
  (if (null path)
      table
    (let ((key (car path))
          (rest (cdr path)))
      (if rest
          (let ((current (gethash key table 'not-found)))
            (cond
             ;; Not found
             ((eq current 'not-found)
              default)
             ;; Found a hash-table, recurse into it
             ((hash-table-p current)
              (supertag--get-nested-ht current rest default))
             ;; Found a list (could be plist or alist), try both
             ((and (listp current) (not (null current)))
              (let ((next-key (car rest)))
                (if (keywordp next-key)
                    ;; Keyword key: use plist-get
                    (plist-get current next-key)
                  ;; String key: check if it's alist or fake plist
                  (if (consp (car current))
                      ;; Standard alist: (("key" . "val"))
                      (cdr (assoc next-key current))
                    ;; Fake plist: ("key" "val") - manual search
                    (let ((temp current)
                          (found nil))
                      (while (and temp (not found))
                        (when (equal (car temp) next-key)
                          (setq found (cadr temp)))
                        (setq temp (cddr temp)))
                      (or found default))))))
             ;; Unknown type
             (t
              default)))
        ;; Last key: just get it
        (gethash key table default)))))

(defun supertag--set-nested-ht (table path value)
  "Set a VALUE in a nested hash table or plist/alist at PATH.
TABLE is the hash table. PATH is a list of keys.
Handles mixed structures: hash-tables at top levels, plists/alists at entity level.
Example: (:nodes \"id\" :tags) where node data is a plist."
  (if (null (cdr path))
      ;; Last key: just set it
      (puthash (car path) value table)
    ;; More keys to traverse
    (let* ((key (car path))
           (current (gethash key table))
           (rest-path (cdr path)))
      (cond
       ;; If current value is a hash-table, recurse into it
       ((hash-table-p current)
        (supertag--set-nested-ht current rest-path value))
       
      ;; If current value is a list (plist or alist), update it
      ((and (listp current) (not (null current)))
       (let ((next-key (car rest-path)))
         ;; Check if it's a plist (keyword key) or alist (string key)
         (if (keywordp next-key)
             ;; Use plist-put for keyword keys
             (let ((updated-plist (plist-put (copy-sequence current) next-key value)))
               (puthash key updated-plist table))
           ;; String key: convert to alist if needed, then update
           (let* ((alist (if (and (consp (car current))) 
                            current  ; Already an alist
                          ;; Convert fake plist ("key" "val") to alist (("key" . "val"))
                          (when (zerop (mod (length current) 2))
                            (let ((result '()))
                              (while current
                                (push (cons (car current) (cadr current)) result)
                                (setq current (cddr current)))
                              (nreverse result)))))
                  (existing (assoc next-key alist)))
             (if existing
                 ;; Update existing entry
                 (let ((updated-alist (cons (cons next-key value)
                                           (delq existing (copy-sequence alist)))))
                   (puthash key updated-alist table))
               ;; Add new entry
               (let ((updated-alist (cons (cons next-key value) (or alist '()))))
                 (puthash key updated-alist table)))))))
       
       ;; If current value is nil or not a known type, decide what to create
       (t
        (if (= (length rest-path) 1)
            ;; Only one level left: create a plist or alist based on key type
            (let ((next-key (car rest-path)))
              (if (keywordp next-key)
                  ;; Create plist for keyword keys
                  (let ((new-plist (list next-key value)))
                    (puthash key new-plist table))
                ;; Create alist for string keys
                (let ((new-alist (list (cons next-key value))))
                  (puthash key new-alist table))))
          ;; More levels: create a hash-table
          (let ((subtable (ht-create)))
            (puthash key subtable table)
            (supertag--set-nested-ht subtable rest-path value)))))))
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
  "Get data from the store by PATH.
PATH is a list of keys (e.g., '(:nodes \"123\" :tags)).
Supports mixed structures: hash-tables and plists."
  ;; Ensure store is initialized
  (unless (hash-table-p supertag--store)
    (setq supertag--store (ht-create)))
  
  ;; Use the nested helper that handles both hash-tables and plists
  (supertag--get-nested-ht supertag--store path default))

(defun supertag-update (path value)
  "Atomically update a value in the central store at PATH.
 PATH is a list of keys. Returns the old value.
 Triggers change notifications unless suppressed.
 Stores plist values directly."
  ;; Ensure store is initialized
  (unless (hash-table-p supertag--store)
    (setq supertag--store (ht-create)))
  
  (let ((old-value (supertag-get path)))
    (when (not (equal old-value value))
      (supertag--set-nested-ht supertag--store path value)
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
    ;; The `when old-value` check was preventing deletion of nil-valued keys.
    ;; We now attempt removal regardless and check the return value of the
    ;; removal function, which correctly indicates if a key existed and was removed.
    (when (supertag--remove-nested-ht supertag--store path)
      (supertag--notify-change path old-value nil)
      (supertag-emit-event :store-changed path old-value nil))
    old-value))

(defun supertag-store-clear ()
  "Clear the entire data store.
This is primarily intended for testing and system resets."
  (interactive)
  (setq supertag--store (ht-create))
  (message "Supertag store has been cleared."))



(provide 'supertag-core-store)

;;; supertag-core-store.el ends here

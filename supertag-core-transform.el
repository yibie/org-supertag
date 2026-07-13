;;; org-supertag/transform.el --- Core data transformation mechanism for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file implements the central data transformation mechanism for the
;; Org-Supertag data-centric architecture. It provides a functional,
;; atomic, and consistent way to modify the application state.

;;; Code:

(require 'cl-lib) ; For cl-loop, cl-find, etc.
(require 'ht) ; Ensures `ht` API availability
(require 'supertag-core-store) ; Depends on supertag-get and supertag-update
(require 'supertag-core-state) ; For shared state variables
(require 'supertag-core-notify) ; For supertag--notify-change

;;; --- Core Transform Function ---

(defun supertag-transform (path fn &rest args)
  "Transform data at PATH using function FN.
PATH is a list of keys (e.g., '(:nodes \"123\" :tags)).
FN is a function that receives the current value as its first argument,
and ARGS as subsequent arguments. Returns the transformed value.

Canonical store mode restricts PATH to collection or collection-entity locations."
  (unless (and (listp path) path)
    (error "PATH must be a non-empty list, got: %S" path))
  (when (> (length path) 2)
    (error "Canonical transform only supports collection/entity paths, got: %S" path))
  (let* ((current-value (supertag-get path))
         (new-value (apply fn current-value args)))
    ;; Update store with new value directly.
    ;; Validation is handled by specific ops functions in hybrid architecture.
    ;; Rollback recording (when a transaction is active) happens at the
    ;; store-level seam inside `supertag-update' itself (which ultimately
    ;; calls `supertag-store-put-entity' / puthash on the collection), so
    ;; there is nothing to log here — see `supertag--transaction-record-old-value'.
    (supertag-update path new-value)

    ;; Return new value
    new-value))

;;; --- Batch Transform ---

(defun supertag-batch-transform (transforms)
  "Execute multiple transformations in a batch, ensuring atomicity.
TRANSFORMS is a list of (path fn args...) tuples.
Notifications are suppressed until all transformations are complete."
  (let ((results '()))
    (supertag-core-state-with-suppressed-notifications
     (dolist (transform transforms)
       (let ((path (nth 0 transform))
             (fn (nth 1 transform))
             (args (nthcdr 2 transform)))
         (let ((new-value (apply #'supertag-transform path fn args)))
           (push (cons path new-value) results))))
     (supertag--notify-batch-changes)) ; Call batch notification after all transforms
    (nreverse results)))

;;; --- Transaction Support ---

(defun supertag--transaction-restore-entry (entry)
  "Undo one recorded ENTRY of the form (PATH EXISTED-P OLD-VALUE).
Dispatches on the shape of PATH:
- (:field-values NODE-ID FIELD-ID) — a single field value.
- (COLLECTION ID) — a canonical entity (also covers the :field-values
  \"this node's bucket didn't exist yet\" marker, which always has
  EXISTED-P nil and therefore only ever takes the remove branch, so it
  never risks flattening a per-node field hash table into a plist via
  `supertag--normalize-entity').
- (COLLECTION) — a whole-collection replace/clear."
  (let ((path (nth 0 entry))
        (existed-p (nth 1 entry))
        (old-value (nth 2 entry)))
    (cond
     ((= (length path) 3)
      (let ((node-id (nth 1 path))
            (field-id (nth 2 path)))
        (if existed-p
            (supertag-store-put-field-value node-id field-id old-value)
          (supertag-store-remove-field-value node-id field-id))))
     ((= (length path) 2)
      (let ((collection (nth 0 path))
            (id (nth 1 path)))
        (if existed-p
            (supertag-store-put-entity collection id old-value)
          (supertag-store-remove-entity collection id))))
     ((= (length path) 1)
      (if existed-p
          (supertag-update path old-value)
        (supertag-delete path)))
     (t
      (error "supertag--transaction-restore-entry: unsupported path shape %S" path)))))

(defun supertag--transaction-rollback (log)
  "Undo every change recorded in LOG, most-recently-touched path first.
LOG is a list of (PATH EXISTED-P OLD-VALUE) entries as produced by
`supertag--transaction-record-old-value' — since entries are pushed as they
are first recorded, LOG is already in the correct (reverse chronological)
order for `dolist' to walk directly. Restoration itself must not be treated
as new transactional writes, so the active-transaction flag is bound to nil
for the duration."
  (let ((supertag--transaction-active nil)
        (supertag--transaction-seen nil))
    (dolist (entry log)
      (supertag--transaction-restore-entry entry))))

(defmacro supertag-with-transaction (&rest body)
  "Execute BODY within a transaction.
If an error occurs during BODY execution, every path touched during the
transaction — directly, or transitively via automation actions triggered
synchronously by those writes — is restored to its exact pre-transaction
value: entities created during the transaction are removed again, and
entities deleted during the transaction are resurrected with their original
value. This works because every low-level store mutation primitive
(`supertag-store-put-entity', `supertag-store-remove-entity',
`supertag-store-put-field-value', `supertag-store-remove-field-value', and
the whole-collection replace/clear paths in `supertag-update'/`supertag-delete')
calls `supertag--transaction-record-old-value' before mutating, which is a
no-op unless a transaction is active.

Nesting: invoking `supertag-with-transaction' while one is already active
simply runs BODY inline so its changes join the *enclosing* transaction's
log — there is no separate commit, rollback, or notification flush for the
inner call; only the outermost transaction commits or rolls back.

Notifications are suppressed until the (outermost) transaction commits, at
which point exactly one batch notification flush happens."
  (declare (indent 0))
  `(if supertag--transaction-active
       ;; Already inside a transaction: just run BODY so it joins the
       ;; enclosing transaction's log instead of starting/ending its own.
       (progn ,@body)
     (let ((supertag--transaction-active t) ; Flag for transaction
           (supertag--transaction-log '()) ; Log for rollback
           (supertag--transaction-seen nil) ; Dedup set: first-touch only
           (supertag--tx-success nil)
           result) ; Variable to capture the result
       (unwind-protect
           (progn
             (setq result (supertag-core-state-with-suppressed-notifications
                           (progn ,@body)))
             (setq supertag--tx-success t)
             ;; Commit transaction: notify all pending changes
             (when (fboundp 'supertag--notify-batch-changes)
               (supertag--notify-batch-changes))
             result) ; Return the result
         ;; Cleanup: roll back on error, then always reset transaction state.
         (unless supertag--tx-success
           (supertag--transaction-rollback supertag--transaction-log))
         (setq supertag--transaction-active nil)
         (setq supertag--transaction-log nil)
         (setq supertag--transaction-seen nil)))))

;;; --- Path Pattern Matching ---

(defun supertag-transform-pattern (pattern fn &rest args)
  "Apply transformation function FN to all paths matching PATTERN.
PATTERN can contain wildcards (e.g., '(:nodes * :tags)).
Returns a list of (path . new-value) pairs for each transformation."
  (let ((matching-paths (supertag--find-matching-paths pattern)) ; Helper to be implemented in store.el or query.el
        (transforms '()))
    (dolist (path matching-paths)
      (push (list path fn args) transforms))
    (supertag-batch-transform transforms)))

;;; --- Internal Helper for Path Matching ---

(defun supertag--find-matching-paths (pattern)
  "Find all data paths matching PATTERN in the supertag--store.
PATTERN can contain wildcards (e.g., '(:nodes * :tags)).
Returns a list of matching paths."
  (require 'supertag-core-store) ; Ensure supertag--store is available
  (let ((matches '()))
    (supertag--traverse-store-matches supertag--store pattern '() matches)
    matches))

(defun supertag--traverse-store-matches (store pattern current-path matches)
  "Recursively traverse STORE to find paths matching PATTERN.
STORE is the current hash table being traversed.
PATTERN is the remaining pattern to match.
CURRENT-PATH is the path accumulated so far.
MATCHES is the list to collect matching paths."
  (if (null pattern)
      ;; Pattern exhausted, current-path is a match
      (push (nreverse current-path) matches)
    (let ((key (car pattern))
          (rest-pattern (cdr pattern)))
      (cond
       ;; Wildcard match: match all keys at this level
       ((eq key '*)
        (maphash
         (lambda (k v)
           (when (hash-table-p v)
             (supertag--traverse-store-matches
              v rest-pattern (cons k current-path) matches)))
         store))

       ;; Exact match: continue with specific key
       (t
        (let ((value (gethash key store)))
          (when (and value (hash-table-p value))
             (supertag--traverse-store-matches
              value rest-pattern (cons key current-path) matches))))))))

(defun supertag-transform-extract-inline-tags (content-string)
  "Extract all #tags from a CONTENT-STRING using a regex."
  (let ((tags '()))
    (when content-string
      (with-temp-buffer
        (insert content-string)
        (goto-char (point-min))
        (while (re-search-forward "#\\([^[:space:]#]+\\)" nil t)
          (push (match-string 1) tags))))
    (nreverse tags)))

(provide 'supertag-core-transform)

;;; org-supertag/transform.el ends here

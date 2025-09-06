;;; org-supertag/transform.el --- Core data transformation mechanism for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file implements the central data transformation mechanism for the
;; Org-Supertag data-centric architecture. It provides a functional,
;; atomic, and consistent way to modify the application state.

;;; Code:

(require 'cl-lib) ; For cl-loop, cl-find, etc.
(require 'ht)     ; For hash-table operations (ht-create, ht-get, ht-set, ht-remove)
(require 'supertag-core-store) ; Depends on supertag-get and supertag-update
(require 'supertag-core-state) ; For shared state variables
(require 'supertag-core-notify) ; For supertag--notify-change

;;; --- Core Transform Function ---

(defun supertag-transform (path fn &rest args)
  "Transform data at PATH using function FN.
PATH is a list of keys (e.g., '(:nodes \"123\" :tags)).
FN is a function that receives the current value as its first argument,
and ARGS as subsequent arguments.
Returns the new, transformed value."
  (let* ((current-value (supertag-get path))
       (new-value (apply fn current-value args)))
  ;; Update store with new value directly.
  ;; Validation is handled by specific ops functions in hybrid architecture.
  (supertag-update path new-value)

  ;; If in a transaction, log the change for potential rollback
  (when supertag--transaction-active
    (push (list path current-value new-value) supertag--transaction-log))

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

(defmacro supertag-with-transaction (&rest body)
  "Execute BODY within a transaction.
If an error occurs during BODY execution, all changes will be rolled back.
Notifications are suppressed until the transaction commits."
  (declare (indent 0))
  `(let ((supertag--transaction-active t) ; Flag for transaction
         (supertag--transaction-log '()) ; Log for rollback
         result) ; Variable to capture the result
     (unwind-protect
         (progn
           (setq result (supertag-core-state-with-suppressed-notifications
                         (progn ,@body)))
           ;; Commit transaction: notify all pending changes
           (when (fboundp 'supertag--notify-batch-changes)
             (supertag--notify-batch-changes))
           result) ; Return the result
       ;; Ensure flags are reset in cleanup
       (setq supertag--transaction-active nil)
       (setq supertag--transaction-log nil))))

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

(provide 'supertag-core-transform)

;;; org-supertag/transform.el ends here
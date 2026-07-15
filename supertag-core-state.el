;;; org-supertag/core/state.el --- Core state variables for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file defines core state variables used across multiple modules
;; in the Org-Supertag data-centric architecture, particularly for
;; managing batch operations and transactions.

;;; Code:

(require 'cl-lib) ; For cl-loop, cl-find, etc.
(require 'ht) ; Provides basic `ht` operations when library missing

;;; --- Emacs 28 compatibility ---
;; `plistp' was added in Emacs 29; Package-Requires declares 28.1. This
;; file sits at the bottom of the require chain, so the polyfill is in
;; place before any caller. (Caught by CI's 28.2 matrix: every rescan
;; skipped every file with "void function plistp".)
(unless (fboundp 'plistp)
  (defun plistp (object)
    "Non-nil if OBJECT is a valid plist (even-length proper list)."
    (let ((len (proper-list-p object)))
      (and len (zerop (% len 2))))))

;;; --- Shared Core State Variables ---

(defvar supertag--suppress-notifications nil
  "If non-nil, suppress change notifications. Used for batch operations and transactions.")

(defvar supertag--pending-changes nil
  "List of changes to be notified when notifications are unsuppressed.
Each element is a list: (path old-value new-value).")

(defvar supertag--transaction-active nil
  "Flag indicating if a transaction is currently active.")

(defvar supertag--transaction-log nil
  "Log of changes made within the active transaction, for rollback.
Each element is a list: (PATH EXISTED-P OLD-VALUE), recorded the *first*
time PATH is touched during the transaction (see
`supertag--transaction-record-old-value'), and pushed so the head of the
list is always the most recently touched path. EXISTED-P is nil when PATH
had no entity before the transaction (so rollback must delete PATH again
rather than restore a value); otherwise OLD-VALUE is the pre-transaction
value to restore verbatim.")

(defvar supertag--transaction-seen nil
  "Hash table (equal-keyed) of paths already recorded in the current
transaction's rollback log, or nil when no transaction is active. Ensures
`supertag--transaction-record-old-value' only records the *original*
pre-transaction value the first time a path is touched — later writes to
the same path within the same transaction must not overwrite that snapshot
with an intermediate value.")

(defun supertag--transaction-record-old-value (path existed-p old-value)
  "Record OLD-VALUE for PATH the first time it is touched in this transaction.
No-op unless `supertag--transaction-active' is non-nil. Meant to be called
by the low-level store mutation primitives (`supertag-core-store.el') right
before they mutate PATH, so `supertag-with-transaction' can restore every
touched path to its true pre-transaction value on error.

EXISTED-P distinguishes an update (PATH already had OLD-VALUE, so rollback
restores it) from a creation (PATH did not exist, so rollback deletes it
again). OLD-VALUE is deep-copied with `copy-tree' so later in-place mutation
of the live entity plist cannot corrupt the recorded snapshot."
  (when supertag--transaction-active
    (unless (hash-table-p supertag--transaction-seen)
      (setq supertag--transaction-seen (make-hash-table :test 'equal)))
    (unless (gethash path supertag--transaction-seen)
      (puthash path t supertag--transaction-seen)
      (push (list path existed-p (copy-tree old-value)) supertag--transaction-log))))

;;; --- Macro for Managing Suppressed Notifications ---

(defmacro supertag-core-state-with-suppressed-notifications (&rest body)
  "Execute BODY with notifications suppressed.
Ensures proper cleanup of notification state even if an error occurs."
  (declare (indent 0))
  `(let ((supertag--suppress-notifications t)
         (supertag--pending-changes '()))
     (unwind-protect
         (progn ,@body)
       ;; Ensure notifications are re-enabled and pending changes cleared
       (setq supertag--suppress-notifications nil)
       (setq supertag--pending-changes '()))))

(provide 'supertag-core-state)

;;; org-supertag/core/state.el ends here

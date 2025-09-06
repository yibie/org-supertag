;;; org-supertag/core/state.el --- Core state variables for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file defines core state variables used across multiple modules
;; in the Org-Supertag data-centric architecture, particularly for
;; managing batch operations and transactions.

;;; Code:

(require 'cl-lib) ; For cl-loop, cl-find, etc.
(require 'ht)     ; For hash-table operations

;;; --- Shared Core State Variables ---

(defvar supertag--suppress-notifications nil
  "If non-nil, suppress change notifications. Used for batch operations and transactions.")

(defvar supertag--pending-changes nil
  "List of changes to be notified when notifications are unsuppressed.
Each element is a list: (path old-value new-value).")

(defvar supertag--transaction-active nil
  "Flag indicating if a transaction is currently active.")

(defvar supertag--transaction-log nil
  "Log of changes made within a transaction for rollback purposes.
Each element is a list: (path old-value new-value).")

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

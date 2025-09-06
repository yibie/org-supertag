;;; org-supertag/core/notify.el --- Change notification system for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file implements the change notification system for the Org-Supertag
;; data-centric architecture. It provides mechanisms for notifying subscribers
;; about data changes, especially in batch operations.

;;; Code:

(require 'cl-lib) ; For cl-delete
(require 'ht)                ; For hash-table operations
(require 'supertag-core-state) ; For supertag--subscribers, supertag--suppress-notifications, supertag--pending-changes

;;; --- Change Notification System ---

(defvar supertag--subscribers (ht-create)
  "Hash table to store subscribers for data path changes.
Key: data path (list of keys)
Value: list of callback functions")

(defun supertag-subscribe (event-type callback)
  "Subscribe to a specific EVENT-TYPE.
EVENT-TYPE can be a data path (list of keys) or a generic keyword (e.g., :store-changed).
CALLBACK will be called with arguments relevant to the event.
Returns a function to unsubscribe."
  (let ((callbacks (gethash event-type supertag--subscribers)))
    (puthash event-type (cons callback callbacks) supertag--subscribers)
    ;; Return an unsubscribe function
    (lambda ()
      (puthash event-type (cl-delete callback (gethash event-type supertag--subscribers)) supertag--subscribers))))

(defun supertag-emit-event (event-type &rest args)
  "Emit a generic event.
EVENT-TYPE is a keyword (e.g., :store-changed).
ARGS are the arguments to pass to the event handlers."
  (let ((callbacks (gethash event-type supertag--subscribers)))
    (when callbacks
      (dolist (callback callbacks)
        ;; 直接调用回调函数，不捕获错误
        (apply callback args)))))

(defun supertag-notify (event-type &rest args)
  "Notify subscribers about an event.
This is a wrapper around supertag-emit-event for backward compatibility."
  (apply 'supertag-emit-event event-type args))

;;; --- Core Change Handler (called by supertag-store) ---

(defun supertag-core-notify-handle-change (path old-value new-value)
  "Handle a single data change notification.
This function is called by `supertag-store` after an update.
It manages pending changes for batch operations and dispatches notifications."
  (when (and (not (bound-and-true-p supertag--suppress-notifications))
             (not (equal old-value new-value))) ; Only notify if value actually changed
    (let ((callbacks (gethash path supertag--subscribers)))
      (when callbacks
        (dolist (callback callbacks)
          (funcall callback path old-value new-value)))))
  (when (bound-and-true-p supertag--suppress-notifications)
    (push (list path old-value new-value) supertag--pending-changes)))

;;; --- Batch Notification ---

(defun supertag--notify-batch-changes ()
  "Notify all pending changes in a batch.
This function is called after a batch operation or transaction commits.
It iterates through supertag--pending-changes and dispatches notifications."
  (setq supertag--suppress-notifications nil) ; Ensure notifications are re-enabled
  (dolist (change (nreverse supertag--pending-changes)) ; Notify in order
    (let ((path (nth 0 change))
          (old-value (nth 1 change))
          (new-value (nth 2 change)))
      (supertag-core-notify-handle-change path old-value new-value)))
  (setq supertag--pending-changes '())) ; Clear pending changes after notification

(provide 'supertag-core-notify)
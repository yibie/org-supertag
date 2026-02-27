;;; org-supertag/core/async.el --- Asynchronous task queue for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This module implements an asynchronous job queue for Org-Supertag.
;; It allows heavy operations (like file parsing and database sync) to be
;; performed in the background using idle timers, preventing UI freezes.
;;
;; Inspired by Vulpea's async architecture.

;;; Code:

(require 'cl-lib)

(defgroup supertag-async nil
  "Asynchronous processing settings for Org-Supertag."
  :group 'supertag)

(defcustom supertag-async-idle-delay 0.5
  "Seconds of idle time to wait before processing the next job in the queue.
Lower values make sync faster but might interfere with typing.
Higher values ensure Emacs is truly idle."
  :type 'number
  :group 'supertag-async)

(defcustom supertag-async-batch-size 1
  "Number of files to process in a single idle cycle.
Keep this low (1-3) to maintain responsiveness."
  :type 'integer
  :group 'supertag-async)

;;; Variables

(defvar supertag-async--queue '()
  "List of items (usually file paths) waiting to be processed.
Ordered from oldest to newest.")

(defvar supertag-async--timer nil
  "The active idle timer, or nil if not running.")

(defvar supertag-async--processor-fn nil
  "The function to call for each item in the queue.
Must accept a single argument (the item).")

;;; Core Functions

(defun supertag-async-init (processor-fn)
  "Initialize the async system with a PROCESSOR-FN.
PROCESSOR-FN is a function that takes one argument (the item to process)."
  (setq supertag-async--processor-fn processor-fn)
  (setq supertag-async--queue '())
  (supertag-async--ensure-timer))

(defun supertag-async-enqueue (item)
  "Add ITEM to the processing queue.
If ITEM is already in the queue, it is moved to the end (re-prioritized).
Returns the new queue length."
  ;; Remove if exists (deduplicate)
  (setq supertag-async--queue (delete item supertag-async--queue))
  ;; Add to end
  (setq supertag-async--queue (append supertag-async--queue (list item)))
  ;; Ensure timer is running
  (supertag-async--ensure-timer)
  (length supertag-async--queue))

(defun supertag-async-queue-size ()
  "Return the number of items currently in the queue."
  (length supertag-async--queue))

(defun supertag-async-clear ()
  "Clear all pending jobs."
  (setq supertag-async--queue '()))

;;; Internal Timer Logic

(defun supertag-async--ensure-timer ()
  "Start the idle timer if it's not already running and there is work to do."
  (when (and supertag-async--queue
             (not supertag-async--timer))
    (setq supertag-async--timer
          (run-with-idle-timer
           supertag-async-idle-delay
           nil ;; Run once (we will re-schedule if more work remains)
           #'supertag-async--worker))))

(defun supertag-async--worker ()
  "Process the next batch of items from the queue."
  (setq supertag-async--timer nil) ;; Timer has fired, so it's gone

  (when (and supertag-async--queue supertag-async--processor-fn)
    (condition-case err
        (let ((count 0))
          ;; Process batch
          (while (and supertag-async--queue
                      (< count supertag-async-batch-size))
            (let ((item (pop supertag-async--queue)))
              (when item
                (funcall supertag-async--processor-fn item)
                (cl-incf count))))

          ;;(message "Async worker processed %d items. Remaining: %d"
          ;;         count (length supertag-async--queue))
          )
      (error
       (message "Error in supertag async worker: %s" (error-message-string err))))

    ;; If work remains, re-schedule
    (when supertag-async--queue
      (supertag-async--ensure-timer))))

(provide 'supertag-core-async)

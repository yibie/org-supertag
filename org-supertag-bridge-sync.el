;;; org-supertag-bridge-sync.el --- Independent background sync to Python backend -*- lexical-binding: t; -*-

;;; Commentary:
;; This module handles the periodic, independent synchronization of the
;; Elisp database (`org-supertag-db`) to the Python backend.
;;
;; It operates on its own timer, completely decoupled from the file-level
;; synchronization managed by `org-supertag-sync.el`.
;;
;; Core Responsibilities:
;; 1.  Maintain a long-interval timer (e.g., once or twice a day).
;; 2.  When triggered, package the entire Elisp DB into a snapshot.
;; 3.  Asynchronously send this snapshot to the Python backend's
;;     `bulk_process_snapshot` endpoint.
;; 4.  Provide clear user feedback upon completion, including a summary
;;     of processed data.
;; 5.  Manage a lock to prevent concurrent sync processes.

;;; Code:

(require 'org-supertag-db)
(require 'org-supertag-bridge)

;;; Customization

(defgroup org-supertag-bridge-sync nil
  "Settings for the periodic Elisp-to-Python DB synchronization."
  :group 'org-supertag)

(defcustom org-supertag-bridge-sync-interval (* 12 3600)
  "Interval in seconds for automatic background DB synchronization to Python.
The default is 43200 seconds (12 hours)."
  :type 'integer
  :group 'org-supertag-bridge-sync)

;;; Variables

(defvar-local org-supertag-bridge-sync--timer nil
  "The timer for the periodic background DB sync.")

(defvar-local org-supertag-bridge-sync--in-progress-p nil
  "A flag to prevent concurrent background sync runs.
This acts as a lock.")

;;; Core Functions

(defun org-supertag-bridge-sync--package-snapshot ()
  "Package the entire Elisp DB into a single snapshot dictionary."
  (let ((nodes '())
        (links '()))
    ;; Convert hash table objects to list format
    (maphash (lambda (id node-data)
               (when (and node-data (plist-get node-data :type))
                 (if (eq (plist-get node-data :type) :node)
                     ;; Node format: [ID, TITLE, CONTENT, TAGS, FILE_PATH, MODIFIED_AT]
                     (let ((title (or (plist-get node-data :title) ""))
                           (content (or (plist-get node-data :content) 
                                       (plist-get node-data :raw-value) ""))
                           (tags (plist-get node-data :tags))
                           (file-path (plist-get node-data :file-path))
                           (modified-at (plist-get node-data :modified-at)))
                       (push (list id title content tags file-path modified-at) nodes)))))
             org-supertag-db--object)
    
    ;; Convert hash table links to list format  
    (maphash (lambda (id link-data)
               (when (and link-data (plist-get link-data :type))
                 (if (eq (plist-get link-data :type) :link)
                     ;; Link format: [LINK_ID, TYPE, FROM_ID, TO_ID, PROPERTIES, MODIFIED_AT]
                     (let ((link-type (or (plist-get link-data :link-type) "unknown"))
                           (from-id (plist-get link-data :from-id))
                           (to-id (plist-get link-data :to-id))
                           (properties (plist-get link-data :properties))
                           (modified-at (plist-get link-data :modified-at)))
                       (push (list id link-type from-id to-id properties modified-at) links)))))
             org-supertag-db--link)
    
    ;; Return in the new expected format
    (list nodes links (current-time-string))))

(defun org-supertag-bridge-sync--handle-completion (result)
  "Callback function to handle the result of the background sync.
RESULT is the summary dictionary returned from Python."
  (let* ((summary (format "[org-supertag] Background sync complete. Processed: %d nodes added, %d updated, %d deleted."
                          (or (plist-get result :nodes_added) 0)
                          (or (plist-get result :nodes_updated) 0)
                          (or (plist-get result :nodes_deleted) 0)))
         (errors (plist-get result :errors)))
    (message summary)
    (when errors
      (warn "[org-supertag] Background sync encountered %d errors. See *Python EPC* buffer for details." (length errors))))
  ;; Unlock the sync process
  (setq org-supertag-bridge-sync--in-progress-p nil))

;;;###autoload
(defun org-supertag-bridge-sync-run ()
  "Run the background DB synchronization to the Python backend.
This function is intended to be called by a timer or manually."
  (interactive)
  (if (not (and (boundp 'org-supertag-bridge--ready-p)
                org-supertag-bridge--ready-p))
      (message "[org-supertag] Python bridge not ready. Skipping background sync.")
    (if org-supertag-bridge-sync--in-progress-p
        (message "[org-supertag] Background sync is already in progress. Skipping.")
      (setq org-supertag-bridge-sync--in-progress-p t)
      (message "[org-supertag] Starting background DB sync to Python...")
      (let ((snapshot (org-supertag-bridge-sync--package-snapshot)))
        (org-supertag-bridge-call-async
         "bulk_process_snapshot"
         #'org-supertag-bridge-sync--handle-completion
         snapshot)))))

;;; Timer Management

;;;###autoload
(defun org-supertag-bridge-sync-start ()
  "Start the automatic background DB synchronization timer."
  (interactive)
  (if (timerp org-supertag-bridge-sync--timer)
      (message "Background sync timer is already running.")
    (setq org-supertag-bridge-sync--timer
          (run-with-timer 0 org-supertag-bridge-sync-interval #'org-supertag-bridge-sync-run))
    (message "Automatic background DB sync started (interval: %d seconds)." org-supertag-bridge-sync-interval)))

;;;###autoload
(defun org-supertag-bridge-sync-stop ()
  "Stop the automatic background DB synchronization timer."
  (interactive)
  (if (timerp org-supertag-bridge-sync--timer)
      (progn
        (cancel-timer org-supertag-bridge-sync--timer)
        (setq org-supertag-bridge-sync--timer nil)
        (message "Automatic background DB sync stopped."))
    (message "Background sync timer is not running.")))

;;;###autoload
(defun org-supertag-bridge-sync-restart ()
  "Restart the automatic background DB synchronization timer."
  (interactive)
  (org-supertag-bridge-sync-stop)
  (org-supertag-bridge-sync-start))

(provide 'org-supertag-bridge-sync)

;;; org-supertag-bridge-sync.el ends here
;;; org-supertag-background-sync.el --- Background sync engine for org-supertag -*- lexical-binding: t; -*-

;; Background sync engine: responsible for periodically syncing Elisp data to Python backend
;; Uses hash-based incremental detection mechanism, not timestamp

;;; Commentary:
;; 
;; This module implements background sync decoupled from org-supertag-sync.el:
;; - org-supertag-sync.el: file → Elisp DB (high frequency, lightweight)
;; - org-supertag-background-sync.el: Elisp DB → Python (low frequency, heavyweight)
;;
;; Key features:
;; 1. Hash-based incremental detection (inspired by org-supertag-sync.el)
;; 2. State management, prevent concurrent running
;; 3. Detailed sync reports and debug information
;; 4. 12-hour sync interval

;;; Code:

(require 'org-supertag-db)
(require 'org-supertag-bridge)
(require 'org-supertag-api)
(require 'ht) ;; Ensure ht is required

;; === Configuration variables ===

(defcustom org-supertag-background-sync-interval (* 12 3600) ; 12 hours
  "Background sync interval (seconds)."
  :type 'integer
  :group 'org-supertag)

(defcustom org-supertag-background-sync-auto-start t
  "Whether to automatically start background sync on startup."
  :type 'boolean
  :group 'org-supertag)

;; === Runtime variables ===

(defvar org-supertag-background-sync--timer nil
  "Background sync timer.")

(defvar org-supertag-background-sync--state :idle
  "Background sync state. Possible values:
- :idle            - Idle state
- :waiting-backend - Waiting for Python backend to be ready
- :syncing         - Syncing")

(defvar org-supertag-background-sync--last-sync-time nil
  "Last successful sync time.")

(defvar org-supertag-background-sync--last-sync-hashes (make-hash-table :test 'equal)
  "Last sync hashes for each object.
Key: object ID
Value: hash string")

(defvar org-supertag-background-sync--stats 
  '(:synced-nodes 0 :synced-tags 0 :synced-links 0 :total-objects 0)
  "Last sync statistics.")

(defvar org-supertag-background-sync--backend-check-timer nil
  "Timer for checking Python backend status.")

(defvar org-supertag-background-sync--backend-check-interval 5
  "Interval for checking Python backend status (seconds).")

(defvar org-supertag-background-sync--hash-file
  (expand-file-name "sync_hashes.json" org-supertag-data-directory)
  "Hash record persistence file path.")

;; === Hash Persistence ===

(defun org-supertag-background-sync--save-hashes ()
  "Save the current sync hashes to the hash file in JSON format."
  (interactive)
  (require 'json)
  (let ((temp-file (make-temp-file "sync-hashes-json-")))
    (with-temp-buffer
      (insert (json-encode org-supertag-background-sync--last-sync-hashes))
      (write-file temp-file nil))
    (rename-file temp-file org-supertag-background-sync--hash-file t)
    (message "[background-sync] Hashes saved to %s" org-supertag-background-sync--hash-file)))

(defun org-supertag-background-sync--load-hashes ()
  "Load sync hashes from the JSON hash file if it exists."
  (interactive)
  (if (file-exists-p org-supertag-background-sync--hash-file)
      (progn
        (require 'json)
        (let* ((json-string (with-temp-buffer
                              (insert-file-contents org-supertag-background-sync--hash-file)
                              (buffer-string)))
               ;; json-read-from-string returns an alist for JSON objects
               (data-alist (json-read-from-string json-string)))
          (clrhash org-supertag-background-sync--last-sync-hashes)
          (dolist (pair data-alist)
            (puthash (car pair) (cdr pair) org-supertag-background-sync--last-sync-hashes))
          (message "[background-sync] Hashes loaded from JSON file %s. Total: %d"
                   org-supertag-background-sync--hash-file
                   (hash-table-count org-supertag-background-sync--last-sync-hashes))))
    (message "[background-sync] JSON hash file not found. Starting with empty hash table.")))

;; === Hash calculation functions ===

(defun org-supertag-background-sync--calculate-object-hash (props)
  "Calculate object hash from its property list."
  (when (plistp props)
    (let ((hash-content (format "%S" props)))
      (secure-hash 'sha1 hash-content))))

(defun org-supertag-background-sync--get-changed-objects ()
  "Get all changed objects (nodes, tags, links) since the last sync.
This function iterates over both `org-supertag-db--object` and
`org-supertag-db--link` to build a complete picture of the changes."
  (let ((nodes-to-upsert '())
        (tags-to-upsert '())
        (links-to-upsert '())
        (ids-to-delete '())
        (current-ids (make-hash-table :test 'equal))
        (changed-count 0))

    ;; 1. Find created/updated objects from the main object table
    (maphash
     (lambda (id props)
       (puthash id t current-ids) ; Track current IDs
       (let ((current-hash (org-supertag-background-sync--calculate-object-hash props))
             (last-hash (gethash id org-supertag-background-sync--last-sync-hashes)))
         (unless (and current-hash last-hash (string= current-hash last-hash))
           (cl-incf changed-count)
           (let ((type (plist-get props :type)))
             (cond
              ((eq type :node) (push props nodes-to-upsert))
              ((eq type :tag) (push props tags-to-upsert))
              (t nil))))))
     org-supertag-db--object)

    ;; 2. Find created/updated links from the link table
    (maphash
     (lambda (id props)
       (puthash id t current-ids) ; Also track link IDs
       (let ((current-hash (org-supertag-background-sync--calculate-object-hash props))
             (last-hash (gethash id org-supertag-background-sync--last-sync-hashes)))
         (unless (and current-hash last-hash (string= current-hash last-hash))
           (cl-incf changed-count)
           (push props links-to-upsert))))
     org-supertag-db--link)

    ;; 3. Find deleted objects by comparing old hashes with current IDs
    (maphash
     (lambda (id _last-hash)
       (unless (gethash id current-ids)
         (cl-incf changed-count)
         (push id ids-to-delete)))
     org-supertag-background-sync--last-sync-hashes)

    (message "[background-sync] checked %d objects, found %d changes"
             (+ (hash-table-count org-supertag-db--object)
                (hash-table-count org-supertag-db--link))
             changed-count)

    (list nodes-to-upsert tags-to-upsert links-to-upsert ids-to-delete)))

;; === Main sync function ===

(defun org-supertag-background-sync--do-sync ()
  "Perform a full background sync operation."
  (setq org-supertag-background-sync--state :syncing)
  (condition-case-unless-debug err
      (let* ((start-time (current-time))
             (changes (org-supertag-background-sync--get-changed-objects))
             (nodes-to-upsert (nth 0 changes))
             (tags-to-upsert (nth 1 changes))
             (links-to-upsert (nth 2 changes))
             (ids-to-delete (nth 3 changes))
             (total-changed (+ (length nodes-to-upsert)
                               (length tags-to-upsert)
                               (length links-to-upsert)
                               (length ids-to-delete))))
        (if (= total-changed 0)
            (progn
              (message "[background-sync] No changes detected. Sync complete.")
              (setq org-supertag-background-sync--state :idle))
          (progn
            (org-supertag-background-sync--start-progress total-changed)
            (let* ((nodes-data (org-supertag-background-sync--prepare-nodes-for-python nodes-to-upsert))
                   (tags-data (org-supertag-background-sync--prepare-tags-for-python tags-to-upsert))
                   (links-data (org-supertag-background-sync--prepare-links-for-python links-to-upsert))
                   ;; Build an alist instead of a hash-table for reliable serialization.
                   (snapshot-data `(("nodes" . ,nodes-data)
                                    ("links" . ,links-data)
                                    ("ids_to_delete" . ,ids-to-delete)
                                    ("sync_timestamp" . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))))
              ;; We must use the designated API function which handles correct payload wrapping.
              (org-supertag-api-bulk-process-snapshot
               snapshot-data
               (lambda (result)
                 (let ((end-time (current-time)))
                   (if (and result (equal (plist-get result :status) "success"))
                       (progn
                         (org-supertag-background-sync--update-hashes nodes-to-upsert tags-to-upsert links-to-upsert ids-to-delete)
                         (setq org-supertag-background-sync--stats
                               (list :synced-nodes (length nodes-to-upsert)
                                     :synced-tags (length tags-to-upsert)
                                     :synced-links (length links-to-upsert)
                                     :deleted-count (length ids-to-delete)
                                     :total-objects total-changed))
                         (setq org-supertag-background-sync--last-sync-time end-time)
                         (message "[background-sync] Sync successful."))
                     (message "[background-sync] sync failed: %s" (or (plist-get result :message) "Unknown error")))
                   (org-supertag-background-sync--finish-progress)
                   (setq org-supertag-background-sync--state :idle))))))))
    (error
     (setq org-supertag-background-sync--state :idle)
     (org-supertag-background-sync--finish-progress)
     (message "[background-sync] error during sync: %s" (error-message-string err)))))

(defun org-supertag-background-sync--deep-prepare-for-python (data)
  "Recursively convert Elisp data (plist, alist) to a pure hash table, for safe sending to Python.
This handles nested structures and special data types like time objects."
  (cond
   ;; Emacs internal time list (e.g., (26701 18355 90563 0)) -> ISO 8601 string
   ((and (listp data) (numberp (car data)) (>= (length data) 3))
    (format-time-string "%Y-%m-%dT%H:%M:%SZ" data t))
   ;; alist (e.g., '((a . 1) (b . 2))) -> hash-table
   ((and (listp data) (consp data) (consp (car data)))
    (let ((table (make-hash-table :test 'equal)))
      (dolist (pair data)
        (when (consp pair)
          (puthash (car pair) (org-supertag-background-sync--deep-prepare-for-python (cdr pair)) table)))
      table))
   ;; plist (e.g., '(:a 1 :b 2)) -> hash-table
   ((and (plistp data) (or (null data) (symbolp (car data))))
    (let ((table (make-hash-table :test 'equal)))
      (while data
        (let ((key (pop data))
              (value (pop data)))
          (puthash (symbol-name key) (org-supertag-background-sync--deep-prepare-for-python value) table)))
      table))
   ;; list -> list (with values processed)
   ((listp data)
    (mapcar #'org-supertag-background-sync--deep-prepare-for-python data))
   ;; Handle symbols explicitly to prevent sending them raw over EPC.
   ((symbolp data)
    ;; `t` and `nil` are handled correctly as booleans by the bridge.
    ;; Other symbols must be converted to their string representation.
    (if (memq data '(t nil))
        data
      (symbol-name data)))
   ;; other types -> return as is
   (t data)))

(defun org-supertag-background-sync--prepare-nodes-for-python (nodes)
  "Prepare node data for sending to Python, using deep conversion."
  (mapcar
   (lambda (node-props)
     (org-supertag-background-sync--deep-prepare-for-python node-props))
   nodes))

(defun org-supertag-background-sync--prepare-tags-for-python (tags)
  "Prepare tag data for sending to Python, using deep conversion."
  (mapcar
   (lambda (tag-props)
     (org-supertag-background-sync--deep-prepare-for-python tag-props))
   tags))

(defun org-supertag-background-sync--prepare-links-for-python (links)
  "Prepare link data for sending to Python, using deep conversion."
  (mapcar
   (lambda (link-props)
     (org-supertag-background-sync--deep-prepare-for-python link-props))
   links))

(defun org-supertag-background-sync--update-hashes (nodes-upserted tags-upserted links-upserted ids-deleted)
  "Update hash records for synchronized objects and save them."
  ;; Update hashes for upserted nodes
  (dolist (props nodes-upserted)
    (let* ((id (plist-get props :id))
           (hash (org-supertag-background-sync--calculate-object-hash props)))
      (when id (puthash id hash org-supertag-background-sync--last-sync-hashes))))
  
  ;; Update hashes for upserted tags
  (dolist (props tags-upserted)
    (let* ((id (plist-get props :id))
           (hash (org-supertag-background-sync--calculate-object-hash props)))
      (when id (puthash id hash org-supertag-background-sync--last-sync-hashes))))
  
  ;; Update hashes for upserted links
  (dolist (props links-upserted)
    (let* ((id (plist-get props :id))
           (hash (org-supertag-background-sync--calculate-object-hash props)))
      (when id (puthash id hash org-supertag-background-sync--last-sync-hashes))))
  
  ;; Remove hashes for deleted objects
  (dolist (id ids-deleted)
    (remhash id org-supertag-background-sync--last-sync-hashes))
  
  ;; Persist the updated hashes to the file
  (org-supertag-background-sync--save-hashes))

(defun org-supertag-background-sync--ensure-data-directory ()
  "Ensure data directory exists."
  (unless (file-exists-p org-supertag-data-directory)
    (make-directory org-supertag-data-directory t)))

;; === Timer management ===

(defun org-supertag-background-sync--python-ready-p ()
  "Check if Python backend is ready."
  (and (featurep 'org-supertag-bridge)
       (boundp 'org-supertag-bridge--ready-p)
       org-supertag-bridge--ready-p
       (boundp 'org-supertag-bridge--python-epc-manager)
       org-supertag-bridge--python-epc-manager
       (fboundp 'org-supertag-bridge-epc-live-p)
       (org-supertag-bridge-epc-live-p org-supertag-bridge--python-epc-manager)))

(defun org-supertag-background-sync--try-start-backend ()
  "Try to start Python backend (if needed)."
  (when (and (not (org-supertag-background-sync--python-ready-p))
             (fboundp 'org-supertag-init-vectorization))
    (condition-case err
        (org-supertag-init-vectorization)
      (error
       (org-supertag-background-sync--log-debug "Failed to start Python backend: %s" (error-message-string err))))))

(defun org-supertag-background-sync--wait-for-backend ()
  "Wait for Python backend to be ready."
  (setq org-supertag-background-sync--state :waiting-backend)
  
  ;; Try to start Python backend (if not already started)
  (org-supertag-background-sync--try-start-backend)
  
  (setq org-supertag-background-sync--backend-check-timer
        (run-with-timer org-supertag-background-sync--backend-check-interval
                        org-supertag-background-sync--backend-check-interval
                        #'org-supertag-background-sync--check-backend)))

(defun org-supertag-background-sync--check-backend ()
  "Check Python backend status timer callback."
  (if (org-supertag-background-sync--python-ready-p)
      (progn
        ;; Backend ready, stop checking and start sync
        (when org-supertag-background-sync--backend-check-timer
          (cancel-timer org-supertag-background-sync--backend-check-timer)
          (setq org-supertag-background-sync--backend-check-timer nil))
        (org-supertag-background-sync--start-sync-timer))
    ;; Continue waiting
    (org-supertag-background-sync--log-debug "Still waiting for Python backend to be ready...")))

(defun org-supertag-background-sync--start-sync-timer ()
  "Start actual sync timer."
  (setq org-supertag-background-sync--state :idle)
  (setq org-supertag-background-sync--timer
        (run-with-timer 0 ; Start immediately for the first sync
                        org-supertag-background-sync-interval
                        #'org-supertag-background-sync--timer-function))
  (org-supertag-background-sync--log-debug "Background sync started, interval %d hours" (/ org-supertag-background-sync-interval 3600)))

(defun org-supertag-background-sync--timer-function ()
  "Timer callback function."
  (when (eq org-supertag-background-sync--state :idle)
    ;; Here Python backend should be ready, but just to be safe, check again
    (if (org-supertag-background-sync--python-ready-p)
        (org-supertag-background-sync--do-sync)
      ;; If backend is not available, re-enter waiting state
      ;; (message "[background-sync] Python backend connection lost, re-waiting...")
      (org-supertag-background-sync--wait-for-backend))))

(defun org-supertag-background-sync-start ()
  "Start background sync service.
This function now assumes Python bridge is already ready when called."
  (interactive)
  (if (timerp org-supertag-background-sync--timer)
      (message "[background-sync] Service is already running.")
    (progn
      (message "[background-sync] Starting background sync service (interval: %ds)..." org-supertag-background-sync-interval)
      (setq org-supertag-background-sync--state :idle)
      (org-supertag-background-sync--load-hashes)
      (setq org-supertag-background-sync--timer
            (run-with-timer 0 org-supertag-background-sync-interval
                            #'org-supertag-background-sync--trigger-sync)))))

(defun org-supertag-background-sync-stop ()
  "Stop background sync service."
  (interactive)
  (when (timerp org-supertag-background-sync--timer)
    (cancel-timer org-supertag-background-sync--timer)
    (setq org-supertag-background-sync--timer nil)
    (setq org-supertag-background-sync--state :idle)
    (message "[background-sync] Service stopped.")))

(defun org-supertag-background-sync--trigger-sync ()
  "Trigger sync entry point, check state."
  (if (eq org-supertag-background-sync--state :idle)
      (org-supertag-background-sync--do-sync)
    (message "[background-sync] Skipping sync run because service is not idle (state: %s)."
             org-supertag-background-sync--state)))

(defun org-supertag-background-sync-restart ()
  "Restart background sync."
  (interactive)
  (org-supertag-background-sync-stop)
  (org-supertag-background-sync-start))

;;; Manual sync and status query

(defun org-supertag-background-sync-run-now ()
  "Run background sync immediately."
  (interactive)
  (cond 
   ((eq org-supertag-background-sync--state :syncing)
    (message "[background-sync] Already running, please wait..."))
   
   ((eq org-supertag-background-sync--state :waiting-backend)
    (message "[background-sync] Waiting for Python backend to be ready, please wait..."))
   
   ((not (org-supertag-background-sync--python-ready-p))
    (message "[background-sync] Python backend not ready. Please start Python backend: M-x org-supertag-bridge-start-process"))
   
   (t 
    (org-supertag-background-sync--do-sync))))

(defun org-supertag-background-sync-status ()
  "Display background sync status."
  (interactive)
  (let* ((python-ready (org-supertag-background-sync--python-ready-p))
         (timer-status (cond 
                       (org-supertag-background-sync--timer "Sync timer running")
                       (org-supertag-background-sync--backend-check-timer "Waiting for backend timer running")
                       (t "已停止")))
         (status-msg
          (format "Background sync status:
- State: %s
- Timer: %s
- Python backend: %s
- Sync interval: %d hours
- Last sync: %s
- Last sync stats: nodes %d, tags %d, links %d, total %d
- Hash records: %d"
                  org-supertag-background-sync--state
                  timer-status
                  (if python-ready "Ready" "Not ready")
                  (/ org-supertag-background-sync-interval 3600)
                  (if org-supertag-background-sync--last-sync-time
                      (format-time-string "%Y-%m-%d %H:%M:%S" org-supertag-background-sync--last-sync-time)
                    "Never synced")
                  (plist-get org-supertag-background-sync--stats :synced-nodes)
                  (plist-get org-supertag-background-sync--stats :synced-tags)
                  (plist-get org-supertag-background-sync--stats :synced-links)
                  (plist-get org-supertag-background-sync--stats :total-objects)
                  (hash-table-count org-supertag-background-sync--last-sync-hashes))))
    (message "%s" status-msg)
    status-msg))

;;; 初始化

(defun org-supertag-background-sync-reset-hashes ()
  "Reset all hash records, force full sync next time."
  (interactive)
  (clrhash org-supertag-background-sync--last-sync-hashes)
  (setq org-supertag-background-sync--last-sync-time nil)
  ;; (message "[background-sync] Hash records reset, full sync will be triggered next time")
  )

(defun org-supertag-background-sync-initialize ()
  "Initialize background sync system."
  ;; Load history hash records
  (org-supertag-background-sync--load-hashes)
  
  (when org-supertag-background-sync-auto-start
    (org-supertag-background-sync-start)))

;; This module is now initialized directly by org-supertag.el
;; No automatic hooks to avoid duplicate initialization

;;; Debug functions

(defun org-supertag-background-sync--log-debug (msg &rest args)
  "Output debug information, only show when debug is enabled."
  (when (and (boundp 'org-supertag-debug) org-supertag-debug)
    (apply #'message (concat "[background-sync-debug] " msg) args)))

(defun org-supertag-background-sync-debug-hashes (&optional limit)
  "Display current hash records (for debugging).
LIMIT: Maximum number of records to display, default 10."
  (interactive "P")
  (let ((count 0)
        (limit (or limit 10)))
    (message "Hash records (maximum %d):" limit)
    (maphash 
     (lambda (id hash)
       (when (< count limit)
         (message "  %s: %s" id (substring hash 0 8))
         (cl-incf count)))
     org-supertag-background-sync--last-sync-hashes)
    (message "Total hash records: %d" (hash-table-count org-supertag-background-sync--last-sync-hashes))))

;;; Progress handling

(defvar org-supertag-background-sync--current-progress nil
  "Current sync progress information, format (current total start-time).")

(defun org-supertag-background-sync--update-progress (current total)
  "Update sync progress. Called by Python backend."
  (when org-supertag-background-sync--current-progress
    (let* ((start-time (nth 2 org-supertag-background-sync--current-progress))
           (elapsed (float-time (time-subtract (current-time) start-time)))
           (percentage (if (> total 0) (/ (* current 100.0) total) 0))
           (eta (if (and (> current 0) (> total current))
                    (/ (* elapsed (- total current)) current)
                  0)))
      
      (setq org-supertag-background-sync--current-progress (list current total start-time))
      
      ;; (message "[background-sync] Progress: %d/%d (%.1f%%) - Elapsed time %.1fs%s"
      ;;          current total percentage elapsed
      ;;          (if (> eta 0) (format " - 预计剩余 %.1fs" eta) ""))
      )))

(defun org-supertag-background-sync--start-progress (total)
  "Start progress tracking."
  (setq org-supertag-background-sync--current-progress 
        (list 0 total (current-time)))
  ;; (message "[background-sync] Starting sync of %d objects..." total)
  )

(defun org-supertag-background-sync--finish-progress ()
  "Finish progress tracking."
  (setq org-supertag-background-sync--current-progress nil))

(when org-supertag-background-sync-auto-start
  (add-hook 'org-supertag-bridge-ready-hook #'org-supertag-background-sync-start))

;; Load hashes on startup, after all functions are defined.
(org-supertag-background-sync--load-hashes)

(provide 'org-supertag-background-sync)
;;; org-supertag-background-sync.el ends here 

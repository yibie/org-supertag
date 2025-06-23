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

(defcustom org-supertag-background-sync-mode 'interval
  "Background sync mode.
- 'interval: Run at fixed intervals (default, uses org-supertag-background-sync-interval)
- 'scheduled: Run at specific times daily
- 'manual: Only run when manually triggered"
  :type '(choice (const :tag "Interval-based sync" interval)
                 (const :tag "Scheduled sync" scheduled)
                 (const :tag "Manual sync only" manual))
  :group 'org-supertag)

(defcustom org-supertag-background-sync-schedule '("09:00" "18:00")
  "Scheduled sync times in HH:MM format.
Only used when `org-supertag-background-sync-mode' is 'scheduled.
Example: '(\"09:00\" \"18:00\") for sync at 9 AM and 6 PM daily."
  :type '(repeat string)
  :group 'org-supertag)

;; === Runtime variables ===

(defvar org-supertag-background-sync--timer nil
  "Background sync timer.")

(defvar org-supertag-background-sync--schedule-timer nil
  "Scheduled sync timer (for daily checks).")

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
  (org-supertag-background-sync--ensure-data-directory)
  (let ((temp-file (make-temp-file "sync-hashes-json-")))
    (with-temp-buffer
      (insert (json-encode org-supertag-background-sync--last-sync-hashes))
      (write-file temp-file nil))
    (rename-file temp-file org-supertag-background-sync--hash-file t)
    (message "[background-sync] Hashes saved to %s" org-supertag-background-sync--hash-file)))

(defun org-supertag-background-sync--load-hashes ()
  "Load sync hashes from the JSON hash file if it exists.
If the file doesn't exist, automatically create a baseline hash file
from current database state to enable incremental sync."
  (interactive)
  (if (file-exists-p org-supertag-background-sync--hash-file)
      (progn
        (require 'json)
        (let* ((json-string (with-temp-buffer
                              (insert-file-contents org-supertag-background-sync--hash-file)
                              (buffer-string)))
               ;; Use json-parse-string with string keys to avoid symbol conversion
               (data-hash (condition-case nil
                            (json-parse-string json-string)
                          (error
                           ;; Fallback to old method if json-parse-string not available
                           (json-read-from-string json-string)))))
          (clrhash org-supertag-background-sync--last-sync-hashes)
          (cond
           ;; If we got a hash table from json-parse-string, iterate directly
           ((hash-table-p data-hash)
            (maphash (lambda (key value)
                       (puthash key value org-supertag-background-sync--last-sync-hashes))
                     data-hash))
           ;; If we got an alist from json-read-from-string, convert symbol keys to strings
           ((listp data-hash)
            (dolist (pair data-hash)
              (let ((key (if (symbolp (car pair)) 
                            (symbol-name (car pair))
                          (car pair))))
                (puthash key (cdr pair) org-supertag-background-sync--last-sync-hashes)))))
          (message "[background-sync] Hashes loaded from JSON file %s. Total: %d"
                   org-supertag-background-sync--hash-file
                   (hash-table-count org-supertag-background-sync--last-sync-hashes))))
    ;; Hash file doesn't exist - create baseline from current database state
    (progn
      (message "[background-sync] JSON hash file not found. Creating baseline from current database state...")
      (org-supertag-background-sync--create-baseline-hashes)
      (message "[background-sync] Baseline created with %d hash records. Incremental sync now enabled."
               (hash-table-count org-supertag-background-sync--last-sync-hashes)))))

(defun org-supertag-background-sync--create-baseline-hashes ()
  "Create baseline hashes from current database state.
This scans all objects and links in the current database and calculates
their hashes to establish a sync baseline. Essential for enabling 
incremental sync when no hash file exists."
  (clrhash org-supertag-background-sync--last-sync-hashes)
  
  ;; Helper function to extract ID from properties (consistent with get-changed-objects)
  (cl-flet ((extract-id (props)
             (or (plist-get props :id)
                 (plist-get props :node-id)
                 (plist-get props :tag-id)
                 (plist-get props :link-id)
                 ;; For links, try source/target as fallback
                 (when (eq (plist-get props :type) :link)
                   (format "%s->%s" 
                           (or (plist-get props :source) "")
                           (or (plist-get props :target) ""))))))
    
    (let ((processed-objects 0)
          (processed-links 0)
          (failed-objects 0))
      
      ;; 1. Process all objects in the main object table
      (maphash
       (lambda (id props)
         (let ((actual-id (extract-id props)))
           (if actual-id
               (condition-case err
                   (let ((hash (org-supertag-background-sync--calculate-object-hash props)))
                     (when hash
                       (puthash actual-id hash org-supertag-background-sync--last-sync-hashes)
                       (cl-incf processed-objects)))
                 (error
                  (cl-incf failed-objects)
                  (message "[background-sync] Failed to hash object %s: %s" 
                           actual-id (error-message-string err))))
             (cl-incf failed-objects))))
       org-supertag-db--object)
      
      ;; 2. Process all links in the link table
      (maphash
       (lambda (id props)
         (let ((actual-id (extract-id props)))
           (if actual-id
               (condition-case err
                   (let ((hash (org-supertag-background-sync--calculate-object-hash props)))
                     (when hash
                       (puthash actual-id hash org-supertag-background-sync--last-sync-hashes)
                       (cl-incf processed-links)))
                 (error
                  (cl-incf failed-objects)
                  (message "[background-sync] Failed to hash link %s: %s" 
                           actual-id (error-message-string err))))
             (cl-incf failed-objects))))
       org-supertag-db--link)
      
      ;; 3. Save the baseline to file
      (org-supertag-background-sync--save-hashes)
      
      ;; 4. Report results
      (message "[background-sync] Baseline creation completed:")
      (message "  Objects processed: %d" processed-objects)
      (message "  Links processed: %d" processed-links)
      (when (> failed-objects 0)
        (message "  Failed to process: %d" failed-objects))
      (message "  Total baseline hashes: %d" (hash-table-count org-supertag-background-sync--last-sync-hashes))
      
      ;; Return summary
      (list :processed-objects processed-objects
            :processed-links processed-links
            :failed-objects failed-objects
            :total-hashes (hash-table-count org-supertag-background-sync--last-sync-hashes)))))

;; === Hash calculation functions ===

(defun org-supertag-background-sync--calculate-object-hash (props)
  "Calculate object hash from its property list.
Excludes time-sensitive and position-sensitive fields to ensure stable hashes."
  (when (plistp props)
    (let ((stable-props '())
          ;; Fields to exclude from hash calculation.
          (excluded-fields '(:modified-at :created-at :last-modified 
                            :timestamp :update-time :sync-time
                            ;; Exclude positional fields
                            :pos :begin :contents-begin :contents-end :olp
                            ;; File path is essential, but other transient fields should be excluded.
                            )))
      ;; Build a new plist with only stable fields
      (let ((remaining props))
        (while remaining
          (let ((key (car remaining))
                (value (cadr remaining)))
            (unless (memq key excluded-fields)
              (setq stable-props (append stable-props (list key value))))
            (setq remaining (cddr remaining)))))
      ;; Calculate hash from stable properties only
      (let ((hash-content (format "%S" stable-props)))
        (secure-hash 'sha1 hash-content)))))

(defun org-supertag-background-sync--get-changed-objects ()
  "Get all changed objects (nodes, tags, links) since the last sync.
This function iterates over both `org-supertag-db--object` and
`org-supertag-db--link` to build a complete picture of the changes."
  ;; Helper function to extract ID from properties (same as in update-hashes)
  (cl-flet ((extract-id (props)
             (or (plist-get props :id)
                 (plist-get props :node-id)
                 (plist-get props :tag-id)
                 (plist-get props :link-id)
                 ;; For links, try source/target as fallback
                 (when (eq (plist-get props :type) :link)
                   (format "%s->%s" 
                           (or (plist-get props :source) "")
                           (or (plist-get props :target) ""))))))
    
    (let ((nodes-to-upsert '())
          (tags-to-upsert '())
          (links-to-upsert '())
          (ids-to-delete '())
          (current-ids (make-hash-table :test 'equal))
          (changed-count 0))

      ;; 1. Find created/updated objects from the main object table
      (maphash
       (lambda (id props)
         (let ((actual-id (extract-id props)))
           (when actual-id
             (puthash actual-id t current-ids) ; Track current IDs using extracted ID
             (let ((current-hash (org-supertag-background-sync--calculate-object-hash props))
                   (last-hash (gethash actual-id org-supertag-background-sync--last-sync-hashes)))
               (unless (and current-hash last-hash (string= current-hash last-hash))
                 (cl-incf changed-count)
                 (let ((type (plist-get props :type)))
                   (cond
                    ((eq type :node) (push props nodes-to-upsert))
                    ((eq type :tag) (push props tags-to-upsert))
                    (t nil))))))))
       org-supertag-db--object)

      ;; 2. Find created/updated links from the link table
      (maphash
       (lambda (id props)
         (let ((actual-id (extract-id props)))
           (when actual-id
             (puthash actual-id t current-ids) ; Also track link IDs using extracted ID
             (let ((current-hash (org-supertag-background-sync--calculate-object-hash props))
                   (last-hash (gethash actual-id org-supertag-background-sync--last-sync-hashes)))
               (unless (and current-hash last-hash (string= current-hash last-hash))
                 (cl-incf changed-count)
                 (push props links-to-upsert))))))
       org-supertag-db--link)

      ;; 3. Find deleted objects by comparing old hashes with current IDs
      (maphash
       (lambda (id _last-hash)
         (unless (gethash id current-ids)
           (cl-incf changed-count)
           (push id ids-to-delete)))
       org-supertag-background-sync--last-sync-hashes)

      (message "[background-sync] checked %d objects, found %d changes: %d nodes, %d tags, %d links, %d deletions"
               (+ (hash-table-count org-supertag-db--object)
                  (hash-table-count org-supertag-db--link))
               changed-count
               (length nodes-to-upsert)
               (length tags-to-upsert)
               (length links-to-upsert)
               (length ids-to-delete))

      (list nodes-to-upsert tags-to-upsert links-to-upsert ids-to-delete))))

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
                   (message "[background-sync] Callback received, result status: %s" 
                            (if result (plist-get result :status) "nil"))
                   (if (and result (equal (plist-get result :status) "success"))
                       (progn
                         (message "[background-sync] Updating hashes for %d nodes, %d tags, %d links, deleting %d" 
                                  (length nodes-to-upsert) (length tags-to-upsert) 
                                  (length links-to-upsert) (length ids-to-delete))
                         (org-supertag-background-sync--update-hashes nodes-to-upsert tags-to-upsert links-to-upsert ids-to-delete)
                         (message "[background-sync] Hash update completed, total hashes: %d" 
                                  (hash-table-count org-supertag-background-sync--last-sync-hashes))
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
   ((and (listp data) (not (null data)) (symbolp (car data)) (evenp (length data)))
    (let ((table (make-hash-table :test 'equal)))
      (while data
        (let ((key (pop data))
              (value (pop data)))
          (puthash (substring (symbol-name key) 1) (org-supertag-background-sync--deep-prepare-for-python value) table)))
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
  ;; Helper function to extract ID from properties
  (cl-flet ((extract-id (props)
             (or (plist-get props :id)
                 (plist-get props :node-id)
                 (plist-get props :tag-id)
                 (plist-get props :link-id)
                 ;; For links, try source/target as fallback
                 (when (eq (plist-get props :type) :link)
                   (format "%s->%s" 
                           (or (plist-get props :source) "")
                           (or (plist-get props :target) ""))))))
    
    ;; Update hashes for upserted nodes
    (dolist (props nodes-upserted)
      (let* ((id (extract-id props))
             (hash (org-supertag-background-sync--calculate-object-hash props)))
        (when (and id hash)
          (puthash id hash org-supertag-background-sync--last-sync-hashes))))
    
    ;; Update hashes for upserted tags
    (dolist (props tags-upserted)
      (let* ((id (extract-id props))
             (hash (org-supertag-background-sync--calculate-object-hash props)))
        (when (and id hash)
          (puthash id hash org-supertag-background-sync--last-sync-hashes))))
    
    ;; Update hashes for upserted links
    (dolist (props links-upserted)
      (let* ((id (extract-id props))
             (hash (org-supertag-background-sync--calculate-object-hash props)))
        (when (and id hash)
          (puthash id hash org-supertag-background-sync--last-sync-hashes))))
    
    ;; Remove hashes for deleted objects
    (dolist (id ids-deleted)
      (remhash id org-supertag-background-sync--last-sync-hashes))
    
    ;; Persist the updated hashes to the file
    (org-supertag-background-sync--save-hashes)))

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

(defun org-supertag-background-sync-start (&optional force)
  "Start background sync service.
This function now assumes Python bridge is already ready when called.
If FORCE is non-nil or called interactively, ignore auto-start setting."
  (interactive "P")
  (let ((should-start (or force (called-interactively-p 'any) org-supertag-background-sync-auto-start)))
    (if (not should-start)
        (message "[background-sync] Auto-start is disabled (org-supertag-background-sync-auto-start is nil)")
      (cond
       ;; Interval mode (traditional behavior)
       ((eq org-supertag-background-sync-mode 'interval)
        (if (timerp org-supertag-background-sync--timer)
            (message "[background-sync] Interval service is already running.")
          (progn
            (message "[background-sync] Starting interval sync service (interval: %ds)..." org-supertag-background-sync-interval)
            (setq org-supertag-background-sync--state :idle)
            (org-supertag-background-sync--load-hashes)
            (setq org-supertag-background-sync--timer
                  (run-with-timer 0 org-supertag-background-sync-interval
                                  #'org-supertag-background-sync--trigger-sync)))))
       
       ;; Scheduled mode
       ((eq org-supertag-background-sync-mode 'scheduled)
        (if (timerp org-supertag-background-sync--schedule-timer)
            (message "[background-sync] Scheduled service is already running.")
          (progn
            (message "[background-sync] Starting scheduled sync service (times: %s)..." 
                     (mapconcat 'identity org-supertag-background-sync-schedule ", "))
            (setq org-supertag-background-sync--state :idle)
            (org-supertag-background-sync--load-hashes)
            (org-supertag-background-sync--start-schedule-timer))))
       
       ;; Manual mode
       ((eq org-supertag-background-sync-mode 'manual)
        (message "[background-sync] Manual mode - no automatic sync will be started. Use M-x org-supertag-background-sync-run-now to sync."))
       
       (t
        (message "[background-sync] Unknown sync mode: %s" org-supertag-background-sync-mode))))))

(defun org-supertag-background-sync-stop ()
  "Stop background sync service."
  (interactive)
  (let ((stopped-services '()))
    (when (timerp org-supertag-background-sync--timer)
      (cancel-timer org-supertag-background-sync--timer)
      (setq org-supertag-background-sync--timer nil)
      (push "continuous" stopped-services))
    (when (timerp org-supertag-background-sync--schedule-timer)
      (cancel-timer org-supertag-background-sync--schedule-timer)
      (setq org-supertag-background-sync--schedule-timer nil)
      (push "scheduled" stopped-services))
    (setq org-supertag-background-sync--state :idle)
    (if stopped-services
        (message "[background-sync] Stopped %s service(s)." (mapconcat 'identity stopped-services " and "))
      (message "[background-sync] No services were running."))))

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

(defun org-supertag-background-sync-toggle-auto-start ()
  "Toggle automatic background sync startup."
  (interactive)
  (setq org-supertag-background-sync-auto-start 
        (not org-supertag-background-sync-auto-start))
  (message "[background-sync] Auto-start %s. %s"
           (if org-supertag-background-sync-auto-start "enabled" "disabled")
           (if org-supertag-background-sync-auto-start
               "Background sync will start automatically when bridge is ready."
             "Use M-x org-supertag-background-sync-start to start manually.")))

;;; === Scheduled Sync Functions ===

(defun org-supertag-background-sync--start-schedule-timer ()
  "Start the scheduled sync timer that checks every minute."
  (setq org-supertag-background-sync--schedule-timer
        (run-with-timer 60 60 #'org-supertag-background-sync--check-schedule)))

(defun org-supertag-background-sync--check-schedule ()
  "Check if current time matches any scheduled sync time."
  (let ((current-time (format-time-string "%H:%M")))
    (when (member current-time org-supertag-background-sync-schedule)
      (message "[background-sync] Scheduled sync triggered at %s" current-time)
      (org-supertag-background-sync--trigger-sync))))

(defun org-supertag-background-sync--parse-time-string (time-str)
  "Parse time string 'HH:MM' and return (hour minute) or nil if invalid."
  (when (and time-str (string-match "^\\([0-9][0-9]\\):\\([0-9][0-9]\\)$" time-str))
    (let ((hour (string-to-number (match-string 1 time-str)))
          (minute (string-to-number (match-string 2 time-str))))
      (when (and (<= 0 hour 23) (<= 0 minute 59))
        (list hour minute)))))

(defun org-supertag-background-sync--next-scheduled-time ()
  "Calculate the next scheduled sync time."
  (let* ((now (current-time))
         (current-hour (string-to-number (format-time-string "%H" now)))
         (current-minute (string-to-number (format-time-string "%M" now)))
         (today-times '())
         (tomorrow-times '()))
    
    ;; Parse all scheduled times
    (dolist (time-str org-supertag-background-sync-schedule)
      (when-let ((parsed (org-supertag-background-sync--parse-time-string time-str)))
        (let ((hour (nth 0 parsed))
              (minute (nth 1 parsed)))
          (if (or (> hour current-hour)
                  (and (= hour current-hour) (> minute current-minute)))
              (push (list hour minute) today-times)
            (push (list hour minute) tomorrow-times)))))
    
    ;; Find the next time
    (let ((next-time (or (car (sort today-times 
                                   (lambda (a b) 
                                     (or (< (car a) (car b))
                                         (and (= (car a) (car b)) 
                                              (< (cadr a) (cadr b)))))))
                         (car (sort tomorrow-times 
                                   (lambda (a b) 
                                     (or (< (car a) (car b))
                                         (and (= (car a) (car b)) 
                                              (< (cadr a) (cadr b))))))))))
      (when next-time
        (let ((next-hour (nth 0 next-time))
              (next-minute (nth 1 next-time)))
          (if (member next-time today-times)
              (format "Today %02d:%02d" next-hour next-minute)
            (format "Tomorrow %02d:%02d" next-hour next-minute)))))))

(defun org-supertag-background-sync-set-mode (mode)
  "Set background sync mode interactively."
  (interactive (list (intern (completing-read "Sync mode: " 
                                              '("interval" "scheduled" "manual") 
                                              nil t))))
  (setq org-supertag-background-sync-mode mode)
  (message "[background-sync] Mode set to %s. Restart service to apply changes." mode))

(defun org-supertag-background-sync-set-schedule (times)
  "Set scheduled sync times interactively."
  (interactive "sScheduled times (space-separated HH:MM): ")
  (let ((time-list (split-string times)))
    (setq org-supertag-background-sync-schedule time-list)
    (message "[background-sync] Schedule set to: %s. Restart service to apply changes." 
             (mapconcat 'identity time-list ", "))))

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
                       ((and org-supertag-background-sync--timer 
                             org-supertag-background-sync--schedule-timer)
                        "Both continuous and scheduled timers running")
                       (org-supertag-background-sync--timer "Continuous timer running")
                       (org-supertag-background-sync--schedule-timer "Scheduled timer running")
                       (org-supertag-background-sync--backend-check-timer "Waiting for backend timer running")
                       (t "Stopped")))
         (mode-info (cond
                    ((eq org-supertag-background-sync-mode 'interval)
                     (format "Interval (every %d hours)" 
                             (/ org-supertag-background-sync-interval 3600)))
                    ((eq org-supertag-background-sync-mode 'scheduled)
                     (format "Scheduled (times: %s, next: %s)"
                             (mapconcat 'identity org-supertag-background-sync-schedule ", ")
                             (or (org-supertag-background-sync--next-scheduled-time) "None")))
                    ((eq org-supertag-background-sync-mode 'manual)
                     "Manual (no automatic sync)")
                    (t (format "Unknown mode: %s" org-supertag-background-sync-mode))))
         (status-msg
          (format "Background sync status:
- State: %s
- Mode: %s
- Timer: %s
- Auto-start: %s
- Python backend: %s
- Last sync: %s
- Last sync stats: nodes %d, tags %d, links %d, total %d
- Hash records: %d"
                  org-supertag-background-sync--state
                  mode-info
                  timer-status
                  (if org-supertag-background-sync-auto-start "Enabled" "Disabled")
                  (if python-ready "Ready" "Not ready")
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

(defun org-supertag-background-sync-rebuild-baseline ()
  "Rebuild baseline hashes from current database state.
This is useful after database recovery or when you want to re-establish
the sync baseline. All objects will be considered 'synced' in their current state."
  (interactive)
  (when (yes-or-no-p "Rebuild sync baseline from current database state? This will mark all current objects as 'synced'. ")
    (message "[background-sync] Rebuilding baseline from current database state...")
    (let ((result (org-supertag-background-sync--create-baseline-hashes)))
      (message "[background-sync] Baseline rebuild completed:")
      (message "  Total objects processed: %d" 
               (+ (plist-get result :processed-objects)
                  (plist-get result :processed-links)))
      (message "  Total hashes created: %d" (plist-get result :total-hashes))
      (when (> (plist-get result :failed-objects) 0)
        (message "  Failed to process: %d objects" (plist-get result :failed-objects)))
      (message "  Incremental sync is now enabled with new baseline")
      result)))

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

;; Hook registration moved to org-supertag.el to avoid duplication
;; (when org-supertag-background-sync-auto-start
;;   (add-hook 'org-supertag-bridge-ready-hook #'org-supertag-background-sync-start))

;; Load hashes on startup, after all functions are defined.
(org-supertag-background-sync--load-hashes)

(provide 'org-supertag-background-sync)
;;; org-supertag-background-sync.el ends here 

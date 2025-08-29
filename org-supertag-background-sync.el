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
(require 'org-supertag-scheduler)

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

(defcustom org-supertag-tag-refresh-time "03:00"
  "Daily time (HH:MM) to refresh STALE tag embeddings via embedding/refresh_stale_tags."
  :type 'string
  :group 'org-supertag)

;; === Knowledge cycle configuration ===

(defcustom org-supertag-knowledge-cycle-interval 300
  "Interval (in seconds) for triggering the backend knowledge extraction cycle.
This controls how often Emacs 调用 `knowledge/run_cycle` 以及队列检查逻辑。
建议值：300 (5 分钟) 到 1800 (30 分钟) 之间。"
  :type 'integer
  :group 'org-supertag)

;; === Runtime variables ===

(defvar org-supertag-background-sync--registered-task-ids nil
  "A list of task IDs registered with the central scheduler.")

(defvar org-supertag-background-sync--phase :idle
  "Background sync phase. Possible values:
- :idle            - Idle
- :waiting-backend - Waiting for Python backend to be ready
- :embedding       - Phase 1: Syncing embeddings and metadata
- :reasoning       - Phase 2: Inferring relations for nodes")

(defvar org-supertag-background-sync--timer nil
  "Timer for interval-based background sync.")

(defvar org-supertag-background-sync--schedule-timer nil
  "Timer for scheduled background sync.")

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

;;------------------------------------------------------------------------------ 
;; Hash Persistence 
;;------------------------------------------------------------------------------

(defun org-supertag-background-sync--save-hashes ()
  "Save the current sync hashes to the hash file in JSON format.
Only writes the file if the hash table is not empty to avoid creating
empty or invalid JSON files."
  (interactive)
  (unless (hash-table-empty-p org-supertag-background-sync--last-sync-hashes)
    (require 'json)
    (org-supertag-background-sync--ensure-data-directory)
    (let ((temp-file (make-temp-file "sync-hashes-json-")))
      (with-temp-buffer
        (insert (json-encode org-supertag-background-sync--last-sync-hashes))
        (write-file temp-file nil))
      (rename-file temp-file org-supertag-background-sync--hash-file t)
      (message "[background-sync] Hashes saved to %s" org-supertag-background-sync--hash-file))))

(defun org-supertag-background-sync--load-hashes ()
  "Load sync hashes from the JSON hash file if it exists.
If the file doesn't exist, automatically create a baseline hash file
from current database state to enable incremental sync."
  (interactive)
  (if (file-exists-p org-supertag-background-sync--hash-file)
      (let ((json-string (with-temp-buffer
                           (insert-file-contents org-supertag-background-sync--hash-file)
                           (buffer-string))))
        (if (or (null json-string) (string-empty-p (string-trim json-string)))
            (progn
              (message "[background-sync] Hash file is empty. Creating baseline...")
              (org-supertag-background-sync--create-baseline-hashes))
          (require 'json)
          (let* ((data-hash (condition-case nil
                              (json-parse-string json-string)
                            (error
                             (json-read-from-string json-string)))))
            (clrhash org-supertag-background-sync--last-sync-hashes)
            (cond
             ((hash-table-p data-hash)
              (maphash (lambda (key value)
                         (puthash key value org-supertag-background-sync--last-sync-hashes))
                       data-hash))
             ((listp data-hash)
              (dolist (pair data-hash)
                (let ((key (if (symbolp (car pair))
                              (symbol-name (car pair))
                            (car pair))))
                  (puthash key (cdr pair) org-supertag-background-sync--last-sync-hashes)))))
            (message "[background-sync] Hashes loaded from JSON file %s. Total: %d"
                     org-supertag-background-sync--hash-file
                     (hash-table-count org-supertag-background-sync--last-sync-hashes)))))
    ;; Hash file doesn't exist - check if database has content before creating baseline
    (progn
      (message "[background-sync] JSON hash file not found.")
      (if (and (boundp 'org-supertag-db--object)
               (hash-table-p org-supertag-db--object)
               (> (hash-table-count org-supertag-db--object) 0))
          (progn
            (message "[background-sync] Database has content. Creating baseline from current database state...")
            (org-supertag-background-sync--create-baseline-hashes)
            (message "[background-sync] Baseline created with %d hash records. Incremental sync now enabled."
                     (hash-table-count org-supertag-background-sync--last-sync-hashes)))
        (progn
          (message "[background-sync] Database is empty or not loaded. Hash baseline will be created when database has content.")
          (clrhash org-supertag-background-sync--last-sync-hashes))))))

;;;###autoload
(defun org-supertag-background-sync-regenerate-baseline ()
  "Manually regenerate the baseline hash file from current database state.
This is useful when the hash file is missing or corrupted, or when you want to
reset the incremental sync baseline."
  (interactive)
  (if (and (boundp 'org-supertag-db--object)
           (hash-table-p org-supertag-db--object)
           (> (hash-table-count org-supertag-db--object) 0))
      (progn
        (message "[background-sync] Regenerating baseline from current database state...")
        (org-supertag-background-sync--create-baseline-hashes)
        (message "[background-sync] Baseline regenerated successfully with %d hash records."
                 (hash-table-count org-supertag-background-sync--last-sync-hashes)))
    (message "[background-sync] Cannot regenerate baseline: database is empty or not loaded.")))

;;;###autoload
(defun org-supertag-background-sync-check-hash-status ()
  "Check the status of the hash file and provide recommendations."
  (interactive)
  (let ((hash-file-exists (file-exists-p org-supertag-background-sync--hash-file))
        (hash-count (hash-table-count org-supertag-background-sync--last-sync-hashes))
        (db-objects (if (and (boundp 'org-supertag-db--object)
                            (hash-table-p org-supertag-db--object))
                       (hash-table-count org-supertag-db--object)
                     0))
        (db-links (if (and (boundp 'org-supertag-db--link)
                          (hash-table-p org-supertag-db--link))
                     (hash-table-count org-supertag-db--link)
                   0)))
    
    (with-current-buffer (get-buffer-create "*org-supertag-hash-status*")
      (erase-buffer)
      (insert "=== org-supertag Background Sync Hash Status ===\n\n")
      
      (insert (format "Hash file path: %s\n" org-supertag-background-sync--hash-file))
      (insert (format "Hash file exists: %s\n" (if hash-file-exists "Yes" "No")))
      (insert (format "In-memory hash count: %d\n" hash-count))
      (insert (format "Database objects: %d\n" db-objects))
      (insert (format "Database links: %d\n\n" db-links))
      
      (cond
       ;; Case 1: Everything looks good
       ((and hash-file-exists (> hash-count 0) (> db-objects 0))
        (insert "✅ Status: GOOD\n")
        (insert "Hash file exists and contains data. Incremental sync is working properly.\n"))
       
       ;; Case 2: Hash file missing but database has content
       ((and (not hash-file-exists) (> db-objects 0))
        (insert "⚠️  Status: HASH FILE MISSING\n")
        (insert "Database has content but hash file is missing.\n")
        (insert "Recommendation: Run M-x org-supertag-background-sync-regenerate-baseline\n"))
       
       ;; Case 3: Hash file exists but empty
       ((and hash-file-exists (= hash-count 0) (> db-objects 0))
        (insert "⚠️  Status: HASH FILE EMPTY\n")
        (insert "Hash file exists but contains no data.\n")
        (insert "Recommendation: Run M-x org-supertag-background-sync-regenerate-baseline\n"))
       
       ;; Case 4: Database is empty
       ((= db-objects 0)
        (insert "ℹ️  Status: DATABASE EMPTY\n")
        (insert "Database has no content. Hash baseline will be created automatically\n")
        (insert "when database is populated with nodes.\n"))
       
       ;; Case 5: Mismatch between hash count and database content
       ((and (> hash-count 0) (> db-objects 0) 
             (> (abs (- hash-count (+ db-objects db-links))) 10))
        (insert "⚠️  Status: HASH/DATABASE MISMATCH\n")
        (insert "Significant difference between hash count and database content.\n")
        (insert "This may indicate the hash file is outdated.\n")
        (insert "Recommendation: Run M-x org-supertag-background-sync-regenerate-baseline\n"))
       
       ;; Default case
       (t
        (insert "❓ Status: UNKNOWN\n")
        (insert "Unable to determine status. Please check manually.\n")))
      
      (insert "\n=== Actions ===\n")
      (insert "• M-x org-supertag-background-sync-regenerate-baseline - Regenerate hash baseline\n")
      (insert "• M-x org-supertag-background-sync-reset-and-force-resync - Reset and force full resync\n")
      (insert "• M-x org-supertag-background-sync-status - Check background sync status\n")
      
      (display-buffer (current-buffer)))
    
    (message "Hash status displayed in *org-supertag-hash-status* buffer")))

;; Hash calculation functions 
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
                 ;; 修复链接 ID 提取逻辑
                 (when (or (eq (plist-get props :type) :link)
                          (eq (plist-get props :type) :tag-relation)
                          (string-match-p "relation" (format "%s" (plist-get props :type))))
                   (let ((source (plist-get props :source))
                         (target (plist-get props :target)))
                     (when (and source target)
                       (format "%s->%s" source target))))
                 ;; 如果以上都失败，尝试使用哈希表的 key 作为 ID
                 ;; 这在 maphash 中会作为第一个参数传入
                 nil))) ; 在 maphash 中，我们需要使用传入的 id 参数
    
    (let ((processed-objects 0)
          (processed-links 0)
          (failed-objects 0))
      
      ;; 1. Process all objects in the main object table
      (maphash
       (lambda (id props)
         ;; Only process objects that are not :metadata
         (unless (eq (plist-get props :type) :metadata)
           (let ((actual-id (or (extract-id props) id)))
             (when actual-id
               (puthash actual-id t current-ids)
               (let* ((type-key (when (string-match "^:\\([^:]+\\):" id)
                                  (intern (concat ":" (match-string 1 id)))))
                      (props-with-type (if type-key
                                           (plist-put (copy-sequence props) :type type-key)
                                         props))
                      (current-hash (org-supertag-background-sync--calculate-object-hash props-with-type))
                      (last-hash (gethash actual-id org-supertag-background-sync--last-sync-hashes)))
                 (unless (and current-hash last-hash (string= current-hash last-hash))
                   (cl-incf changed-count)
                   (push props-with-type links-to-upsert)
                   ;; 同步其关联实体
                   (when-let ((source-id (plist-get props-with-type :from)))
                     (unless (gethash source-id entities-to-upsert)
                       (when-let ((entity-data (org-supertag-db-get source-id)))
                         (puthash source-id entity-data entities-to-upsert))))
                   (when-let ((target-id (plist-get props-with-type :to)))
                     (unless (gethash target-id entities-to-upsert)
                       (when-let ((entity-data (org-supertag-db-get target-id)))
                         (puthash target-id entity-data entities-to-upsert)))))))))
       org-supertag-db--object)
      
      ;; 2. Process all links in the link table
      (maphash
       (lambda (id props)
         (unless (eq (plist-get props :type) :metadata)
           (let ((actual-id (or (extract-id props) id)))
             (when actual-id
               (puthash actual-id t current-ids)
               (let* ((type-key (when (string-match "^:\\([^:]+\\):" id)
                                  (intern (concat ":" (match-string 1 id)))))
                      (props-with-type (if type-key
                                           (plist-put (copy-sequence props) :type type-key)
                                         props))
                      (current-hash (org-supertag-background-sync--calculate-object-hash props-with-type))
                      (last-hash (gethash actual-id org-supertag-background-sync--last-sync-hashes)))
                 (unless (and current-hash last-hash (string= current-hash last-hash))
                   (cl-incf changed-count)
                   (push props-with-type links-to-upsert)
                   (when-let ((source-id (plist-get props-with-type :from)))
                     (unless (gethash source-id entities-to-upsert)
                       (when-let ((entity-data (org-supertag-db-get source-id)))
                         (puthash source-id entity-data entities-to-upsert))))
                   (when-let ((target-id (plist-get props-with-type :to)))
                     (unless (gethash target-id entities-to-upsert)
                       (when-let ((entity-data (org-supertag-db-get target-id)))
                         (puthash target-id entity-data entities-to-upsert)))))))))
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
            :total-hashes (hash-table-count org-supertag-background-sync--last-sync-hashes)))))))


(defun org-supertag-background-sync-create-baseline ()
  "Create baseline hashes from current database state."
  (interactive)
  (org-supertag-background-sync--create-baseline-hashes))  

(defun org-supertag-background-sync--calculate-object-hash (props)
  "Calculate object hash based on its type and core content fields.
For `:node`, hash is based on `:title`, `:content`, and `:tags`.
For `:tag`, hash is based on `:id`.
For links, hash is based on their core properties like `:type`, `:from`, and `:to`.
This ensures the hash is stable and only changes when meaningful data is modified."
  (when (plistp props)
    (let* ((type (plist-get props :type))
           (stable-props
            (pcase type
              (:node
               (let (plist)
                 (when-let ((title (plist-get props :title)))
                   (setq plist (plist-put plist :title title)))
                 (when-let ((content (plist-get props :content)))
                   (setq plist (plist-put plist :content content)))
                 (when-let ((tags (plist-get props :tags)))
                   ;; Sort tags to ensure hash is stable regardless of order.
                   (setq plist (plist-put plist :tags (sort (copy-sequence tags) #'string<))))
                 plist))
              (:tag
               (let (plist)
                 (when-let ((id (plist-get props :id)))
                   (setq plist (plist-put plist :id id)))
                 plist))
              ;; Handle all known link types.
              ((or :node-tag :tag-ref :tag-tag :relation-group :relation-member)
               (let (plist)
                 (setq plist (plist-put plist :type type))
                 (when-let ((from (plist-get props :from)))
                   (setq plist (plist-put plist :from from)))
                 (when-let ((to (plist-get props :to)))
                   (setq plist (plist-put plist :to to)))
                 plist))
              (:node-field
               (let (plist)
                 (setq plist (plist-put plist :type type))
                 (when-let ((from (plist-get props :from)))
                   (setq plist (plist-put plist :from from)))
                 (when-let ((to (plist-get props :to)))
                   (setq plist (plist-put plist :to to)))
                 (when-let ((value (plist-get props :value)))
                   (setq plist (plist-put plist :value value)))
                 (when-let ((tag-id (plist-get props :tag-id)))
                   (setq plist (plist-put plist :tag-id tag-id)))
                 plist))
              ;; Default/fallback for any other types.
              (_
               (let (plist)
                 (when-let ((id (plist-get props :id)))
                   (setq plist (plist-put plist :id id)))
                 plist)))))
      ;; Only calculate hash if we have stable properties to work with.
      (when stable-props
        (let ((hash-content (format "%S" stable-props)))
          (secure-hash 'sha1 hash-content))))))

(defun org-supertag-background-sync--is-alist-p (data)
  "Check if DATA is likely an alist (a list of cons pairs).
This is a dependency-free replacement for `(every #'consp data)`."
  (and (listp data)
       (let ((res t))
         (dolist (x data)
           (unless (consp x) (setq res nil)))
         res)))

(defun org-supertag-background-sync--deep-prepare-for-python (data)
  "Recursively normalize any Lisp data into a clean structure for JSON serialization.
The primary goal is to convert any object-like structure (hash-table, plist)
into an association list (alist) with string keys, which `json-encode`
correctly serializes into a JSON object."
  (cond
   ;; --- Base Cases for Atoms ---
   ((null data) nil)
   ((stringp data) data)
   ((numberp data) data)
   ((eq data t) t)

   ;; --- Recursive Cases ---
   ;; Hash-table: convert to alist and recurse.
   ((hash-table-p data)
    (org-supertag-background-sync--deep-prepare-for-python (ht->alist data)))

   ;; Symbol: convert to string (keywords are stripped of ':').
   ((symbolp data)
    (if (keywordp data)
        (substring (symbol-name data) 1)
      (symbol-name data)))

   ;; List-like structures (the most complex case).
   ((consp data)
    (cond
     ;; Heuristic for Plist: a list starting with a keyword.
     ;; This must be checked before other list types.
     ((and (keywordp (car data)) (plistp data))
      (let (alist)
        (while (and data (cdr data)) ; Safe for odd-length plists
          (push (cons (pop data) (pop data)) alist))
        ;; After converting plist to alist, recurse on the alist.
        (org-supertag-background-sync--deep-prepare-for-python (nreverse alist))))

     ;; Heuristic for Alist: a list where each element is a cons pair.
     ;; This is the target format for JSON objects.
     ((org-supertag-background-sync--is-alist-p data)
      (mapcar (lambda (pair)
                (cons (org-supertag-background-sync--deep-prepare-for-python (car pair))
                      (org-supertag-background-sync--deep-prepare-for-python (cdr pair))))
              data))

     ;; Proper list of other items.
     ((listp data)
      (mapcar #'org-supertag-background-sync--deep-prepare-for-python data))

     ;; Fallback for improper lists or dotted pairs.
     (t
      (cons (org-supertag-background-sync--deep-prepare-for-python (car data))
            (org-supertag-background-sync--deep-prepare-for-python (cdr data))))))

   ;; --- Fallback for any other data type ---
   (t data)))

;; ---------------------------------------------------------------------------
;; Incremental change detection
;; ---------------------------------------------------------------------------
(defun org-supertag-background-sync--get-changed-objects ()
  "Scan DB, compare with baseline hashes, and return (ENTITIES LINKS IDS-TO-DELETE).

ENTITIES     - list of node/tag property plists that need to be upserted.
LINKS        - list of link property plists that need to be upserted.
IDS-TO-DELETE - list of IDs that existed in previous baseline but are now missing.

The function skips any object whose `:type` is `:metadata`.
It also ensures each link plist contains both `:link-id` and `:id` so that
subsequent hash-update logic can extract identifiers reliably."
  (let* ((entities-to-upsert (make-hash-table :test 'equal)) ; id -> props
         (links-to-upsert   '())
         (current-ids       (make-hash-table :test 'equal)))

    ;; Helper to extract stable ID from props or fallback to key supplied by maphash
    (cl-labels
        ((extract-id
          (id-from-hash props)
          (or (plist-get props :id)
              (plist-get props :node-id)
              (plist-get props :tag-id)
              (plist-get props :link-id)
              id-from-hash))
         (ensure-entity-upsert
          (entity-id)
          (when (and entity-id (not (gethash entity-id entities-to-upsert)))
            (when-let ((entity-data (org-supertag-db-get entity-id)))
              (puthash entity-id entity-data entities-to-upsert)))))

      ;; ----------------------------- Objects (nodes / tags) ---------------
      (when (hash-table-p org-supertag-db--object)
        (maphash
         (lambda (id props)
           (unless (eq (plist-get props :type) :metadata)
             (let* ((actual-id (extract-id id props))
                    (current-hash (org-supertag-background-sync--calculate-object-hash props))
                    (baseline-hash (and actual-id (gethash actual-id org-supertag-background-sync--last-sync-hashes))))
               (when actual-id (puthash actual-id t current-ids))
               ;; New object or hash changed ⇒ upsert
               (unless (and current-hash baseline-hash (string= current-hash baseline-hash))
                 (puthash actual-id props entities-to-upsert)))))
         org-supertag-db--object))

      ;; ----------------------------- Links ---------------------------------
      (when (hash-table-p org-supertag-db--link)
        (maphash
         (lambda (id props)
           (unless (eq (plist-get props :type) :metadata)
             ;; Prepare a copy with explicit IDs for later steps
             (let* ((props-copy (copy-sequence props))
                    (_ (setq props-copy (plist-put props-copy :link-id id)))
                    (_ (setq props-copy (plist-put props-copy :id id)))
                    (actual-id id)
                    (current-hash (org-supertag-background-sync--calculate-object-hash props-copy))
                    (baseline-hash (gethash actual-id org-supertag-background-sync--last-sync-hashes)))
               (puthash actual-id t current-ids)
               ;; Changed or new link
               (unless (and current-hash baseline-hash (string= current-hash baseline-hash))
                 (push props-copy links-to-upsert)
                 ;; Also enqueue the related entities for upsert to keep hashes fresh
                 (ensure-entity-upsert (plist-get props-copy :from))
                 (ensure-entity-upsert (plist-get props-copy :to))))))
         org-supertag-db--link))

      ;; ----------------------------- Deletions -----------------------------
      (let (ids-to-delete)
        (maphash (lambda (baseline-id _hash)
                   (unless (gethash baseline-id current-ids)
                     (push baseline-id ids-to-delete)))
                 org-supertag-background-sync--last-sync-hashes)

        ;; Return three lists: entities, links, deletions
        (list (ht-values entities-to-upsert)
              (nreverse links-to-upsert)
              ids-to-delete)))))


;; ---------------------------------------------------------------------------
;; Main sync function
;; ---------------------------------------------------------------------------

(defun org-supertag-background-sync--finish-sync (&optional status)
  "Finalizes the sync process, resetting state and logging completion.
This function is now more robust to prevent type errors in logging."
  (message "[background-sync] Finishing sync process with status: %s."
           (cond
            ((null status) "completed")
            ((symbolp status) (symbol-name status))
            ((stringp status) status)
            ;; Use %S for any other type to prevent errors.
            (t (format "%S" status))))
  (org-supertag-background-sync--finish-progress)
  (setq org-supertag-background-sync--phase :idle)
  t) ;; Explicitly return t to avoid returning :idle to the scheduler.

(defun org-supertag-background-sync--handle-reasoning-result (result)
  "Callback to handle the result of a reasoning cycle.
If more nodes were processed, it triggers the next cycle.
Otherwise, it finalizes the entire sync process."
  (if (and result (equal (plist-get result :status) "success"))
      (let* ((stage-a-raw (plist-get result :stage_a_processed))
             (stage-b-raw (plist-get result :stage_b_processed))
             (total-raw (or (plist-get result :total_processed) ; Use new key
                            (plist-get result :processed_count))) ; Fallback
             (stage-a (if (numberp stage-a-raw) stage-a-raw (string-to-number (format "%s" (or stage-a-raw 0)))))
             (stage-b (if (numberp stage-b-raw) stage-b-raw (string-to-number (format "%s" (or stage-b-raw 0)))))
             (total (if (numberp total-raw) total-raw (string-to-number (format "%s" (or total-raw 0))))))
        (message "[background-sync::reasoning] Cycle complete. Processed %d nodes (Stage A: %d, Stage B: %d)." total stage-a stage-b)
        (if (> total 0)
            ;; More nodes to process, continue the cycle.
            (progn
              (message "[background-sync::reasoning] More nodes to process, triggering next cycle...")
              (org-supertag-background-sync--trigger-reasoning-cycle))
          ;; No more nodes to process, finish the entire sync.
          (progn
            (message "[background-sync::reasoning] All nodes processed. Finalizing sync.")
            (setq org-supertag-background-sync--last-sync-time (current-time))
            (org-supertag-background-sync--finish-sync :success))))
    ;; Reasoning phase failed.
    (message "[background-sync::reasoning] Reasoning cycle failed: %s" (or (plist-get result :message) "Unknown error"))
    (org-supertag-background-sync--finish-sync :reasoning-failed)))

(defun org-supertag-background-sync--trigger-reasoning-cycle ()
  "Initiates one cycle of the relation inference process."
  (message "[background-sync::reasoning] Triggering relation inference cycle...")
  (setq org-supertag-background-sync--phase :reasoning)
  (org-supertag-bridge-call-async "knowledge/run_cycle"
                                 nil
                                 #'org-supertag-background-sync--handle-reasoning-result))

(defun org-supertag-background-sync--do-sync ()
  "Perform a full, two-phase background sync operation: embedding then reasoning."
  (setq org-supertag-background-sync--phase :embedding)
  (let* ((start-time (current-time))
         (changes (org-supertag-background-sync--get-changed-objects))
         (entities-to-upsert (nth 0 changes))
         (links-to-upsert (nth 1 changes))
         (ids-to-delete (nth 2 changes)))

    (let ((total-changed (+ (length entities-to-upsert)
                            (length links-to-upsert)
                            (length ids-to-delete))))
      (if (= total-changed 0)
          (progn
            (message "[background-sync] No changes detected. Sync complete.")
            (org-supertag-background-sync--finish-sync :no-changes)
            nil) ;; Explicitly return nil for success, as the scheduler expects.
        (progn
          (message "[background-sync::embedding] Starting embedding sync for %d changes." total-changed)
          (org-supertag-background-sync--start-progress total-changed)
          (let* ((snapshot-data (org-supertag-background-sync--prepare-snapshot-for-contract 
                                entities-to-upsert links-to-upsert ids-to-delete)))
            (unless (org-supertag-background-sync--validate-snapshot-data snapshot-data)
              (error "[background-sync] Snapshot data validation failed"))
            
            (message "[background-sync] Data validation passed, sending to backend...")
             (org-supertag-api-sync-bulk-process
              snapshot-data
             (lambda (result)
               (message "[background-sync::embedding] Callback received, result status: %s"
                        (if result (plist-get result :status) "nil"))
               (if (and result (equal (plist-get result :status) "success"))
                   (progn
                     (message "[background-sync::embedding] Updating hashes for %d entities, %d links, deleting %d"
                              (length entities-to-upsert) (length links-to-upsert) (length ids-to-delete))
                     (org-supertag-background-sync--update-hashes entities-to-upsert links-to-upsert ids-to-delete)
                     (setq org-supertag-background-sync--stats
                           (list :synced-entities (length entities-to-upsert)
                                 :synced-links (length links-to-upsert)
                                 :deleted-count (length ids-to-delete)
                                 :total-objects total-changed))
                     (message "[background-sync::embedding] Phase 1 (Embedding) successful. Proceeding to Phase 2 (Reasoning).")
                     (org-supertag-background-sync--trigger-reasoning-cycle))
                 (message "[background-sync::embedding] sync failed: %s" (or (plist-get result :message) "Unknown error"))
                 (org-supertag-background-sync--finish-sync :embedding-failed))))))))))

;; ---------------------------------------------------------------------------
;; Data contract preparation functions
;; ---------------------------------------------------------------------------
(defun org-supertag-background-sync--prepare-entity-for-contract (entity-props)
  "Prepare a single entity for the data contract.
Ensure the entity contains the required fields: id, type, and other standard fields."
  (let* ((entity-type (or (plist-get entity-props :type)
                         (plist-get entity-props 'type)))
         (entity-id (or (plist-get entity-props :id)
                       (plist-get entity-props :node-id)
                       (plist-get entity-props :tag-id)
                       (plist-get entity-props 'id)
                       (plist-get entity-props 'node-id)
                       (plist-get entity-props 'tag-id)))
         (title (or (plist-get entity-props :title)
                   (plist-get entity-props :name)
                   (plist-get entity-props 'title)
                   (plist-get entity-props 'name)))
         (content (or (plist-get entity-props :content)
                     (plist-get entity-props 'content)))
         (file-path (or (plist-get entity-props :file-path)
                       (plist-get entity-props 'file-path)))
         (pos (or (plist-get entity-props :pos)
                 (plist-get entity-props 'pos)))
         (properties (or (plist-get entity-props :properties)
                        (plist-get entity-props 'properties)))
         (modified-at (or (plist-get entity-props :modified-at)
                         (plist-get entity-props 'modified-at)
                         (format-time-string "%Y-%m-%dT%H:%M:%S"))))

    ;; Normalize fields
    (setq entity-id (org-supertag-background-sync--normalize-link-field entity-id "id"))
    (setq title (org-supertag-background-sync--normalize-link-field title "title"))
    
    ;; Validate required fields
    (unless entity-id
      (error "Entity missing required 'id' field: %S" entity-props))
    (unless entity-type
      (error "Entity missing required 'type' field: %S" entity-props))
    
    ;; Normalize type field
    (setq entity-type 
          (cond
           ((or (eq entity-type :node) (string= entity-type "node")) "node")
           ((or (eq entity-type :tag) (string= entity-type "tag")) "tag")
           (t (error "Invalid entity type %S, must be 'node' or 'tag'" entity-type))))

    ;; Build entity data according to the data contract
    (let ((entity-data `((id . ,entity-id)
                        (type . ,entity-type))))
      
      ;; Add optional fields (only add if non-empty)
      (when title
        (setq entity-data (append entity-data `((title . ,title)))))
      (when content
        (setq entity-data (append entity-data `((content . ,content)))))
      (when file-path
        (setq entity-data (append entity-data `((file_path . ,file-path)))))
      (when pos
        (setq entity-data (append entity-data `((pos . ,pos)))))
      (when modified-at
        (setq entity-data (append entity-data `((modified_at . ,modified-at)))))
      
      ;; Process properties field
      (when properties
        (let ((normalized-props
               (cond
                ;; If already an alist, use it directly
                ((and (listp properties) 
                      (org-supertag-background-sync--is-alist-p properties))
                 properties)
                ;; If a plist, convert to an alist
                ((plistp properties)
                 (let (alist)
                   (while (and properties (cdr properties))
                     (let ((key (pop properties))
                           (value (pop properties)))
                       (push (cons (cond
                                   ((keywordp key) (substring (symbol-name key) 1))
                                   ((symbolp key) (symbol-name key))
                                   ((stringp key) key)
                                   (t (format "%s" key)))
                                 value) alist)))
                   (nreverse alist)))
                ;; For other cases, try to use directly
                (t properties))))
          (setq entity-data (append entity-data `((properties . ,normalized-props))))))
      
      entity-data)))

(defun org-supertag-background-sync--normalize-link-field (field field-name)
  "Recursively clean and normalize a field to ensure it's a valid string ID.
This function is the single gatekeeper for sanitizing data before it's
sent to Python. It handles various data types, including symbols, numbers,
and malformed, nested list structures from completion frameworks."
  (cond
   ;; Base case: Already a clean string.
   ((stringp field) field)
   ;; Base case: Nil, normalize to empty string.
   ((null field) "")
   ;; Base case: A symbol, convert to its name.
   ((symbolp field) (symbol-name field))
   ;; Base case: A number, convert to string.
   ((numberp field) (number-to-string field))

   ;; Recursive case: A list, which is often a malformed structure.
   ((listp field)
    (if (null field)
        ""
      ;; The core assumption is that the actual text is the first element
      ;; of some list, which may itself be nested. We recursively process
      ;; the first element of the list until we hit a base case (a non-list atom).
      (let ((car (car field)))
        (message "[background-sync] Normalizing list in '%s', processing first element: %S" field-name car)
        (org-supertag-background-sync--normalize-link-field car field-name))))

   ;; Final fallback for any other unexpected data type.
   (t
    (message "[background-sync] Warning: Invalid type '%s' for field '%s', converting to string representation."
             (type-of field) field-name)
    (format "%S" field))))

(defun org-supertag-background-sync--prepare-link-for-contract (link-props)
  "Prepare a single link for the data contract.
Ensure the link contains the required fields: source, target, and optional type field."
  (let* ((source (or (plist-get link-props :source)
                    (plist-get link-props 'source)
                    (plist-get link-props :from)
                    (plist-get link-props 'from)))
         (target (or (plist-get link-props :target)
                    (plist-get link-props 'target)
                    (plist-get link-props :to)
                    (plist-get link-props 'to)))
         (link-type-raw (or (plist-get link-props :type)
                           (plist-get link-props 'type)
                           "REF_TO"))
         (link-type (cond ((eq link-type-raw :node-tag) "HAS_TAG") ; Convert :node-tag to "HAS_TAG" for Python
                         ((symbolp link-type-raw) (symbol-name link-type-raw))
                         ((stringp link-type-raw) link-type-raw)
                         (t "REF_TO"))))

    ;; Clean and normalize source and target fields
    (setq source (org-supertag-background-sync--normalize-link-field source "source"))
    (setq target (org-supertag-background-sync--normalize-link-field target "target"))

    ;; Validate required fields
    (unless source
      (error "Link missing required 'source' field: %S" link-props))
    (unless target
      (error "Link missing required 'target' field: %S" link-props))
    (unless (stringp source)
      (error "Link 'source' must be a string, got %s: %S" (type-of source) link-props))
    (unless (stringp target)
      (error "Link 'target' must be a string, got %s: %S" (type-of target) link-props))

    ;; Build link data according to the data contract
    `((source . ,source)
      (target . ,target)
      (type . ,link-type))))

(defun org-supertag-background-sync--prepare-snapshot-for-contract (entities links ids-to-delete)
  "Prepare a complete snapshot of data according to the data contract.
Ensure the data structure matches the expected format for the Python backend."
  (let ((prepared-entities '())
        (prepared-links '())
        (valid-ids-to-delete '()))

    ;; Prepare entity data
    (dolist (entity entities)
      ;; Skip metadata-type entities
      (let ((entity-type (or (plist-get entity :type)
                           (plist-get entity 'type))))
        (unless (eq entity-type :metadata)
          (condition-case err
              (let ((prepared-entity (org-supertag-background-sync--prepare-entity-for-contract entity)))
                (push prepared-entity prepared-entities))
            (error
             (message "[background-sync] Failed to prepare entity %S: %s" 
                      entity (error-message-string err)))))))

    ;; Prepare link data
    (dolist (link links)
      ;; Skip links with nil source or target
      (let ((source (or (plist-get link :source)
                       (plist-get link 'source)
                       (plist-get link :from)
                       (plist-get link 'from)))
            (target (or (plist-get link :target)
                       (plist-get link 'target)
                       (plist-get link :to)
                       (plist-get link 'to))))
        (when (and source target)  ; Only process valid links
          (condition-case err
              (let ((prepared-link (org-supertag-background-sync--prepare-link-for-contract link)))
                (push prepared-link prepared-links))
            (error
             (message "[background-sync] Failed to prepare link %S: %s" 
                      link (error-message-string err)))))))

    ;; Validate deletion ID list
    (dolist (id ids-to-delete)
      (if (and id (stringp id) (not (string-empty-p (string-trim id))))
          (push (string-trim id) valid-ids-to-delete)
        (message "[background-sync] Invalid deletion ID: %S" id)))

    ;; Build final snapshot data
    (let ((snapshot `((entities . ,(nreverse prepared-entities))
                     (links . ,(nreverse prepared-links))
                     (ids_to_delete . ,(nreverse valid-ids-to-delete)))))
      
      (message "[background-sync] Prepared snapshot: %d entities, %d links, %d deletions"
               (length prepared-entities)
               (length prepared-links)
               (length valid-ids-to-delete))
      
      snapshot)))

(defun org-supertag-background-sync--validate-entity-data (entity)
  "Validate entity data against the data contract in Elisp."
  (let ((id (alist-get 'id entity))
        (type (alist-get 'type entity)))
    (and id
         (stringp id)
         (not (string-empty-p (string-trim id)))
         type
         (member type '("node" "tag")))))

(defun org-supertag-background-sync--validate-link-data (link)
  "Validate link data against the data contract in Elisp."
  (let ((source (alist-get 'source link))
        (target (alist-get 'target link)))
    (and source
         (stringp source)
         (not (string-empty-p (string-trim source)))
         target
         (stringp target)
         (not (string-empty-p (string-trim target))))))

(defun org-supertag-background-sync--validate-snapshot-data (snapshot)
  "Validate snapshot data against the data contract in Elisp."
  (let ((entities (alist-get 'entities snapshot))
        (links (alist-get 'links snapshot))
        (ids-to-delete (alist-get 'ids_to_delete snapshot)))
    (and (listp entities)
         (listp links)
         (listp ids-to-delete)
         (cl-every #'org-supertag-background-sync--validate-entity-data entities)
         (cl-every #'org-supertag-background-sync--validate-link-data links)
         (cl-every (lambda (id) (and (stringp id) (not (string-empty-p (string-trim id))))) ids-to-delete))))

;; ---------------------------------------------------------------------------
;; Update hashes
;; ---------------------------------------------------------------------------

(defun org-supertag-background-sync--update-hashes (entities-upserted links-upserted ids-deleted)
  "Update hash records for synchronized objects and save them."
  ;; Helper function to extract ID from properties, now consistent with get-changed-objects
  (cl-flet ((extract-id (props)
             (or (plist-get props :id)
                 (plist-get props :node-id)
                 (plist-get props :tag-id)
                 (plist-get props :link-id)
                 ;; This logic reconstructs a link ID from its properties.
                 ;; It's crucial that the `props` has a :type field for this to work.
                 (when-let ((type (plist-get props :type)))
                   (when (memq type '(:node-tag :node-field :tag-ref :tag-tag :relation-group :relation-member))
                     (when-let ((from (plist-get props :from))
                                (to (plist-get props :to)))
                       (format "%s:%s->%s" type from to)))))))
    
    ;; Update hashes for upserted entities (nodes and tags)
    (dolist (props entities-upserted)
      (let* ((id (extract-id props))
             (hash (org-supertag-background-sync--calculate-object-hash props)))
        (if (and id hash)
            (progn
              (puthash id hash org-supertag-background-sync--last-sync-hashes)
              (message "[background-sync] Updated hash for entity: %s" id))
          (message "[background-sync] Failed to update hash for entity, ID or hash missing: %S" props))))
    
    ;; Update hashes for upserted links
    (dolist (props links-upserted)
      (let* ((id (extract-id props))
             (hash (org-supertag-background-sync--calculate-object-hash props)))
        (if (and id hash)
            (progn
              (puthash id hash org-supertag-background-sync--last-sync-hashes)
              (message "[background-sync] Updated hash for link: %s" id))
          (message "[background-sync] Failed to update hash for link, ID or hash missing: %S" props))))
    
    ;; Remove hashes for deleted objects
    (dolist (id ids-deleted)
      (when (remhash id org-supertag-background-sync--last-sync-hashes)
        (message "[background-sync] Removed hash for deleted object: %s" id)))
    
    ;; Persist the updated hashes to the file
    (org-supertag-background-sync--save-hashes)))

(defun org-supertag-background-sync--ensure-data-directory ()
  "Ensure data directory exists."
  (unless (file-exists-p org-supertag-data-directory)
    (make-directory org-supertag-data-directory t)))

;; ---------------------------------------------------------------------------
;; Timer management
;; ---------------------------------------------------------------------------

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
  (setq org-supertag-background-sync--phase :waiting-backend)
  
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
  (setq org-supertag-background-sync--phase :idle)
  (setq org-supertag-background-sync--timer
        (run-with-timer 0 ; Start immediately for the first sync
                        org-supertag-background-sync-interval
                        #'org-supertag-background-sync--timer-function))
  (org-supertag-background-sync--log-debug "Background sync started, interval %d hours" (/ org-supertag-background-sync-interval 3600)))

(defun org-supertag-background-sync--timer-function ()
  "Timer callback function."
  (when (eq org-supertag-background-sync--phase :idle)
    ;; Here Python backend should be ready, but just to be safe, check again
    (if (org-supertag-background-sync--python-ready-p)
        (org-supertag-background-sync--check-queue-and-sync)
      ;; If backend is not available, re-enter waiting state
      ;; (message "[background-sync] Python backend connection lost, re-waiting...")
      (org-supertag-background-sync--wait-for-backend))))

(defun org-supertag-background-sync--check-queue-and-sync ()
  "Check the queue status and decide whether to sync."
  (org-supertag-bridge-call-async "knowledge/get_queue_status"
                                 nil
                                 #'org-supertag-background-sync--handle-queue-status-result))

(defun org-supertag-background-sync--handle-queue-status-result (result)
  "Handle the result of the queue status check and decide the next action."
  (if (and result (equal (plist-get result :status) "success"))
      (let* ((pending-count (or (plist-get result :pending)   ; New backend field
                                (plist-get result :pending_count) ; Old field
                                0))
             (next-delay (* 5 60))) ; Default to 5 minutes if not otherwise set
        (message "[background-sync] Pending nodes in queue: %d" pending-count)
        (cond
         ;; If there's a large backlog, sync immediately and schedule the next check soon.
         ((> pending-count 500)
          (message "[background-sync] Large backlog detected. Syncing now and will re-check in 1 minute.")
          (setq next-delay 60) ; 1 minute
          (org-supertag-background-sync--do-sync))
         
         ;; If there's a medium backlog, sync and schedule a check sooner than the default.
         ((> pending-count 50)
          (message "[background-sync] Medium backlog detected. Syncing now and will re-check in 15 minutes.")
          (setq next-delay (* 15 60)) ; 15 minutes
          (org-supertag-background-sync--do-sync))
         
         ;; If there's a small backlog, sync and use the default interval.
         ((> pending-count 0)
          (message "[background-sync] Small backlog detected. Syncing now. (Queue: %d)" pending-count)
          (setq next-delay org-supertag-knowledge-cycle-interval)
          (org-supertag-background-sync--do-sync))
         
         ;; If the queue is empty, just schedule the next check at the default interval.
         (t
          (message "[background-sync] Queue is empty. Scheduling next check.")
          (setq next-delay org-supertag-knowledge-cycle-interval)))
        
        ;; Cancel any existing timer and set a new one with the calculated delay.
        (when (timerp org-supertag-background-sync--timer)
          (cancel-timer org-supertag-background-sync--timer))
        (setq org-supertag-background-sync--timer
              (run-with-timer next-delay nil #'org-supertag-background-sync--check-queue-and-sync))
        (message "[background-sync] Next queue check scheduled in %d seconds." next-delay))
    
    (message "[background-sync] Failed to get queue status: %s" (or (plist-get result :message) "Unknown error"))))


(defun org-supertag-background-sync-start (&optional force)
  "Start background sync service by registering tasks with the central scheduler.
If FORCE is non-nil or called interactively, ignore auto-start setting."
  (interactive "P")
  ;; First, stop any existing tasks to ensure a clean slate.
  (org-supertag-background-sync-stop)
  (let ((should-start (or force (called-interactively-p 'any) org-supertag-background-sync-auto-start)))
    (if (not should-start)
        (message "[background-sync] Auto-start is disabled.")
      (progn
        (org-supertag-background-sync--load-hashes)
        (pcase org-supertag-background-sync-mode
          ('interval
           (message "[background-sync] Registering interval task (interval: %ds)..." org-supertag-background-sync-interval)
           (let ((task-id 'background-sync-interval))
             (org-supertag-scheduler-register-task
              task-id
              :interval
              #'org-supertag-background-sync-run-now
              :interval org-supertag-background-sync-interval)
             (setq org-supertag-background-sync--registered-task-ids (list task-id))))
          ('scheduled
           (message "[background-sync] Registering scheduled tasks (times: %s)..." (mapconcat 'identity org-supertag-background-sync-schedule ", "))
           (setq org-supertag-background-sync--registered-task-ids nil)
           (dolist (time-str org-supertag-background-sync-schedule)
             (let ((task-id (intern (format "background-sync-scheduled-%s" time-str))))
               (org-supertag-scheduler-register-task
                task-id
                :daily
                #'org-supertag-background-sync-run-now
                :time time-str)
               (push task-id org-supertag-background-sync--registered-task-ids))))
          ('manual
           (message "[background-sync] Manual mode. No tasks registered.")))
        ;; Register daily tag embedding refresh task (runs after other tasks)
        (let ((task-id 'tag-embedding-refresh-daily))
          (org-supertag-scheduler-register-task
           task-id
           :daily
           #'org-supertag-background-sync-refresh-stale-tags
           :time org-supertag-tag-refresh-time)
          (push task-id org-supertag-background-sync--registered-task-ids))))))

(defun org-supertag-background-sync-stop ()
  "Stop background sync service by deregistering its tasks from the scheduler.
This function will only print a message if tasks were actually deregistered."
  (interactive)
  (when org-supertag-background-sync--registered-task-ids
    (dolist (task-id org-supertag-background-sync--registered-task-ids)
      (org-supertag-scheduler-deregister-task task-id))
    (setq org-supertag-background-sync--registered-task-ids nil)
    (message "[background-sync] Tasks deregistered.")))

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

;; ---------------------------------------------------------------------------
;; Manual sync and status query
;; ---------------------------------------------------------------------------

(defun org-supertag-background-sync-run-now ()
  "Run background sync immediately."
  (interactive)
  (cond
   ((not (eq org-supertag-background-sync--phase :idle))
    (message "[background-sync] Already running, please wait (current phase: %s)..." (symbol-name org-supertag-background-sync--phase)))

   ((eq org-supertag-background-sync--phase :waiting-backend)
    (message "[background-sync] Waiting for Python backend to be ready, please wait..."))

   ((not (org-supertag-background-sync--python-ready-p)) (message "[background-sync] Python backend not ready. Please start Python backend: M-x org-supertag-bridge-start-process"))
   ((not (and (boundp 'org-supertag-bridge-enable-ai)
              org-supertag-bridge-enable-ai))
    (message "[background-sync] AI services disabled, skipping background sync.")
    nil)
   (t (org-supertag-background-sync--do-sync))))

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
- Last sync stats: entities %d, links %d, total %d
- Hash records: %d"
                  (symbol-name org-supertag-background-sync--phase)
                  mode-info
                  timer-status
                  (if org-supertag-background-sync-auto-start "Enabled" "Disabled")
                  (if python-ready "Ready" "Not ready")
                  (if org-supertag-background-sync--last-sync-time
                      (format-time-string "%Y-%m-%d %H:%M:%S" org-supertag-background-sync--last-sync-time)
                    "Never synced")
                  (plist-get org-supertag-background-sync--stats :synced-entities)
                  (plist-get org-supertag-background-sync--stats :synced-links)
                  (plist-get org-supertag-background-sync--stats :total-objects)
                  (hash-table-count org-supertag-background-sync--last-sync-hashes))))
    (message "%s" status-msg)
    status-msg))

;; ---------------------------------------------------------------------------
;; Scheduled Sync Functions
;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------
;; Progress tracking functions
;; ---------------------------------------------------------------------------

(defvar org-supertag-background-sync--progress-total 0
  "Total number of items to process in current sync.")

(defvar org-supertag-background-sync--progress-current 0
  "Current number of items processed in current sync.")

(defun org-supertag-background-sync--start-progress (total)
  "Start progress tracking for sync operation with TOTAL items."
  (setq org-supertag-background-sync--progress-total total)
  (setq org-supertag-background-sync--progress-current 0)
  (message "[background-sync] Starting sync progress tracking for %d items" total))

(defun org-supertag-background-sync--finish-progress ()
  "Finish progress tracking for sync operation."
  (setq org-supertag-background-sync--progress-total 0)
  (setq org-supertag-background-sync--progress-current 0)
  (message "[background-sync] Progress tracking completed"))

(defun org-supertag-background-sync--update-progress (processed)
  "Update progress tracking with PROCESSED items."
  (setq org-supertag-background-sync--progress-current processed)
  (when (> org-supertag-background-sync--progress-total 0)
    (let ((percentage (/ (* 100.0 processed) org-supertag-background-sync--progress-total)))
      (message "[background-sync] Progress: %d/%d (%.1f%%)" 
               processed org-supertag-background-sync--progress-total percentage))))

;; ---------------------------------------------------------------------------  
;; Reset and force resync functions
;; ---------------------------------------------------------------------------

(defun org-supertag-background-sync-reset-and-force-resync ()
  "Reset all hash records and force a full resync.
This will delete all existing hash records, ensuring that all objects
are considered changed during the next sync."
  (interactive)
  (when (y-or-n-p "This will reset all sync status and force a full resync. Continue?")
    (message "[background-sync] Starting reset and force resync...")
    
    ;; 1. Clear hash table in memory
    (clrhash org-supertag-background-sync--last-sync-hashes)
    (message "[background-sync] Cleared hash table in memory")
    
    ;; 2. Delete hash file
    (when (file-exists-p org-supertag-background-sync--hash-file)
      (delete-file org-supertag-background-sync--hash-file)
      (message "[background-sync] Deleted hash file: %s" org-supertag-background-sync--hash-file))
    
    ;; 3. Reset sync time
    (setq org-supertag-background-sync--last-sync-time nil)
    (message "[background-sync] Reset sync time")
    
    ;; 4. Reset sync phase
    (setq org-supertag-background-sync--phase :idle)
    (message "[background-sync] Reset sync phase")
    
    ;; 5. Clear statistics
    (setq org-supertag-background-sync--stats nil)
    (message "[background-sync] Cleared statistics")
    
    ;; 6. Immediately start a full sync
    (message "[background-sync] Starting full sync...")
    (org-supertag-background-sync-run-now)
    
    (message "[background-sync] Reset and force resync started")))

(defun org-supertag-background-sync-refresh-stale-tags ()
  "Call backend RPC to refresh embeddings for STALE tags if AI services are enabled."
  (interactive)
  (if (and (boundp 'org-supertag-bridge-enable-ai) org-supertag-bridge-enable-ai)
      (progn
        (message "[background-sync] Triggering backend refresh of STALE tag embeddings...")
        (org-supertag-bridge-call-async
         "embedding/refresh_stale_tags"
         nil
         (lambda (result)
           (message "[background-sync] refresh_stale_tags result: %S" result))))
    (message "[background-sync] AI services disabled, skipping stale tag refresh.")))


(provide 'org-supertag-background-sync)
;;; org-supertag-background-sync.el ends here 

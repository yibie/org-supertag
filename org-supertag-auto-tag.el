;;; org-supertag-auto-tag.el --- Auto-tagging system -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Auto-tagging system provides:
;; 1. Background silent scanning: Periodically scans the entire database to generate tag suggestions for untagged notes.
;; 2. Batch processing interface: Provides a unified interface to review and apply all generated tag suggestions.
;;
;; Workflow:
;; 1. When `org-supertag-auto-tag-mode` is enabled, it starts a background timer based on configuration.
;; 2. The timer periodically calls `org-supertag-auto-tag-silent-scan-and-generate`.
;; 3. This function scans the database to find all untagged nodes and submits them in batch to the Python backend.
;; 4. Suggestions returned from backend are stored in `org-supertag-auto-tag--suggestion-queue`.
;; 5. Users can open the review interface anytime via `org-supertag-auto-tag-batch-add` command.

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-supertag-bridge)
(require 'org-supertag-api)
(require 'org-supertag-tag)
(require 'org-supertag-inline)
(require 'org-supertag-node)
(require 'org-supertag-db)
(require 'cl-lib)

;;; === Core Configuration ===

(defgroup org-supertag-auto-tag nil
  "Auto-tagging system configuration"
  :group 'org-supertag)

(defcustom org-supertag-auto-tag-enable-silent-scan t
  "If non-nil, automatically scan and generate suggestions for untagged nodes in background."
  :type 'boolean
  :group 'org-supertag-auto-tag)

(defcustom org-supertag-auto-tag-silent-scan-interval 7200
  "Time interval (seconds) for background silent scanning of untagged nodes, default 10 minutes."
  :type 'integer
  :group 'org-supertag-auto-tag)

(defcustom org-supertag-auto-tag-batch-min-content-length 10
  "Minimum content length requirement for batch processing nodes."
  :type 'integer
  :group 'org-supertag-auto-tag)

(defcustom org-supertag-auto-tag-batch-enable-limit t
  "If non-nil, limit the number of nodes processed per background scan."
  :type 'boolean
  :group 'org-supertag-auto-tag)

(defcustom org-supertag-auto-tag-batch-max-nodes-per-run 10
  "When limit is enabled, maximum number of nodes processed per background scan."
  :type 'integer
  :group 'org-supertag-auto-tag)

(defcustom org-supertag-auto-tag-enable-daily-reminder t
  "If non-nil, show notification at specified time to remind user to review tag suggestions."
  :type 'boolean
  :group 'org-supertag-auto-tag)

(defcustom org-supertag-auto-tag-daily-reminder-time "17:00"
  "Daily reminder time for reviewing tag suggestions (HH:MM format)."
  :type 'string
  :group 'org-supertag-auto-tag)

;;; === Global Variables ===

(defvar org-supertag-auto-tag--suggestion-queue '()
  "Global queue for storing tag suggestions. Each element is a plist representing a suggestion.")

(defvar org-supertag-auto-tag--node-content-cache (make-hash-table :test 'equal)
  "Cache node content to avoid duplicate IO.")

(defvar org-supertag-auto-tag--silent-scan-timer nil
  "Timer for background silent scanning.")

(defvar org-supertag-auto-tag--reminder-timer nil
  "Timer for daily reminders.")

(defvar org-supertag-auto-tag--last-prompt-date nil
  "Date (YYYY-MM-DD) of last reminder prompt, to avoid duplicate reminders.")

;;; === Auto Mode and Background Scanning ===

(defun org-supertag-auto-tag-start-silent-scan ()
  "Start background silent scan timer.
This function should be called after Python bridge is ready."
  (interactive)
  (when (and org-supertag-auto-tag-enable-silent-scan
             (not (timerp org-supertag-auto-tag--silent-scan-timer)))
    (message "Auto-tag: Background silent scan service started, running every %d seconds."
             org-supertag-auto-tag-silent-scan-interval)
    (setq org-supertag-auto-tag--silent-scan-timer
          (run-with-timer 5 org-supertag-auto-tag-silent-scan-interval
                          'org-supertag-auto-tag-silent-scan-and-generate))))

(defun org-supertag-auto-tag-stop-silent-scan ()
  "Stop background silent scan timer."
  (interactive)
  (when (timerp org-supertag-auto-tag--silent-scan-timer)
    (cancel-timer org-supertag-auto-tag--silent-scan-timer)
    (setq org-supertag-auto-tag--silent-scan-timer nil)
    (message "Auto-tag: Background silent scan service stopped.")))

(defun org-supertag-auto-tag-start-reminder-timer ()
  "Start daily reminder timer."
  (interactive)
  (when (and org-supertag-auto-tag-enable-daily-reminder
             (not (timerp org-supertag-auto-tag--reminder-timer)))
    (message "Auto-tag: Daily reminder enabled, will remind at %s." org-supertag-auto-tag-daily-reminder-time)
    (setq org-supertag-auto-tag--reminder-timer
          (run-with-timer 60 60 'org-supertag-auto-tag--check-and-prompt-for-review))))

(defun org-supertag-auto-tag-stop-reminder-timer ()
  "Stop daily reminder timer."
  (interactive)
  (when (timerp org-supertag-auto-tag--reminder-timer)
    (cancel-timer org-supertag-auto-tag--reminder-timer)
    (setq org-supertag-auto-tag--reminder-timer nil)
    (message "Auto-tag: Daily reminder disabled.")))

(defun org-supertag-auto-tag--check-and-prompt-for-review ()
  "Check current time and prompt user to review tags if conditions are met."
  (let ((today (format-time-string "%Y-%m-%d")))
    (when (and org-supertag-auto-tag-enable-daily-reminder
               (not (seq-empty-p org-supertag-auto-tag--suggestion-queue))
               (string= (format-time-string "%H:%M") org-supertag-auto-tag-daily-reminder-time)
               (not (string= org-supertag-auto-tag--last-prompt-date today)))
      (setq org-supertag-auto-tag--last-prompt-date today)
      (when (yes-or-no-p (format "Auto-tag: %d suggestions pending, review now?"
                                 (length org-supertag-auto-tag--suggestion-queue)))
        (org-supertag-auto-tag-batch-add)))))

;;;###autoload
(define-minor-mode org-supertag-auto-tag-mode
  "Enable auto-tagging for org-supertag.
This mode itself doesn't start any process, it's just a switch.
Actual background scanning is started by `org-supertag-auto-tag-start-silent-scan`."
  :init-value nil
  :lighter " ST-Auto"
  :group 'org-supertag
  (if org-supertag-auto-tag-mode
      ;; Logic when mode is enabled (if needed), but we choose to put startup logic
      ;; in an externally callable function to ensure correct timing.
      (message "Org SuperTag Auto-Tag Mode enabled.")
    ;; When mode is disabled, stop timers
    (org-supertag-auto-tag-stop-silent-scan)
    (org-supertag-auto-tag-stop-reminder-timer)))

(defun org-supertag-auto-tag-silent-scan-and-generate ()
  "Scan database for all untagged nodes and generate tag suggestions in batch.
If `org-supertag-auto-tag-batch-enable-limit` is t,
process at most `org-supertag-auto-tag-batch-max-nodes-per-run` nodes per run."
  (interactive)
  (let* ((all-nodes (org-supertag-node-get-all))
         (untagged-nodes-all (seq-filter
                              (lambda (node-id)
                                (seq-empty-p (org-supertag-node-get-tags node-id)))
                              all-nodes))
         (untagged-nodes (if (and org-supertag-auto-tag-batch-enable-limit
                                  (> (length untagged-nodes-all) org-supertag-auto-tag-batch-max-nodes-per-run))
                             (seq-take untagged-nodes-all org-supertag-auto-tag-batch-max-nodes-per-run)
                           untagged-nodes-all)))
    (if (seq-empty-p untagged-nodes)
        (message "Auto-tag: No untagged nodes found.")
      (progn
        (message "Auto-tag: Found %d untagged nodes (total %d), batch generating suggestions..."
                 (length untagged-nodes)
                 (length untagged-nodes-all))
        (org-supertag-auto-tag--batch-extract-and-send-content untagged-nodes)))))

(defun org-supertag-auto-tag--batch-extract-and-send-content (node-ids)
  "Extract content from given node IDs and send asynchronously to the backend.
The data is structured according to the unified data contract."
  (let ((nodes-to-process '()))
    ;; 1. Create a simplified data structure that EPC can properly serialize
    (dolist (node-id node-ids)
      (when-let* ((node-data (org-supertag-db-get node-id))
                  (content (org-supertag-auto-tag--get-node-content node-data)))
        (when (>= (length content) org-supertag-auto-tag-batch-min-content-length)
          ;; Use list of pairs instead of alist - EPC serializes this as Python dict
          (let ((node-dict `(("id" ,node-id)
                            ("content" ,content))))
            (push node-dict nodes-to-process)))))

    ;; 2. Only send data to backend when there are eligible nodes.
    (if nodes-to-process
        (progn
          (message "Auto-tag: Preparing to send %d eligible nodes to backend for processing..." (length nodes-to-process))
          ;; Construct the final payload using list format for proper EPC serialization  
          (let* ((reversed-nodes (reverse nodes-to-process))
                 (model-config (org-supertag-api--get-model-config-for-tagging))
                 ;; Use list format instead of alist - EPC serializes this as Python dict
                 (payload `(("nodes" ,reversed-nodes)
                           ("model_config" ,model-config))))
            
            ;; 详细的数据收集和调试输出
            (message "=== ELISP DEBUG: DATA COLLECTION (LIST FORMAT) ===")
            (message "Total nodes to process: %d" (length reversed-nodes))
            (message "Model config: %S" model-config)
            
            ;; 打印前5个节点的详细信息
            (let ((node-count 0))
              (dolist (node reversed-nodes)
                (when (< node-count 5)
                  (message "--- Node %d ---" (1+ node-count))
                  (message "Node structure type: %s" (type-of node))
                  (message "Node is list with length: %d" (length node))
                  (dolist (pair node)
                    (when (listp pair)
                      (message "  %S => %S (type: %s)" (car pair) (cadr pair) (type-of (cadr pair)))))
                  (cl-incf node-count))))
            
            (message "--- Final Payload Structure ---")
            (message "Payload type: %s" (type-of payload))
            (message "Payload is list with length: %d" (length payload))
            (dolist (top-pair payload)
              (when (listp top-pair)
                (message "  Top-level %S => type: %s" (car top-pair) (type-of (cadr top-pair)))
                (when (string= (car top-pair) "nodes")
                  (message "    Nodes count: %d" (length (cadr top-pair))))
                (when (string= (car top-pair) "model_config")
                  (message "    Model config: %S" (cadr top-pair)))))
            (message "=== END ELISP DEBUG ===")
            
            ;; (message "Auto-tag DEBUG: Sending payload with %d nodes to API layer." (length reversed-nodes))
            ;; The API layer will wrap this payload in a list before sending.
            (org-supertag-api-batch-generate-tags
             payload
             #'org-supertag-auto-tag--batch-handle-completion)))
      
      ;; If no eligible nodes, just print a message, do nothing.
      (message "Auto-tag: All untagged nodes' content is too short or cannot be extracted, skipped."))))

(defun org-supertag-auto-tag--get-node-content (node-data)
  "Extract content for analysis from node data, including title and body."
  (let* ((file-path (plist-get node-data :file-path))
         (pos (plist-get node-data :pos))
         (title (plist-get node-data :title)))
    (or
     (when (and file-path pos (file-exists-p file-path))
       (with-temp-buffer
         (insert-file-contents file-path)
         (org-mode)
         (goto-char pos)
         (when (org-at-heading-p)
           (let* ((element (org-element-at-point))
                  (contents-begin (org-element-property :contents-begin element))
                  (contents-end (org-element-property :contents-end element)))
             (if (and contents-begin contents-end)
                 (let ((raw-content (buffer-substring-no-properties contents-begin contents-end)))
                   ;; Remove drawers from content
                   (concat title "\n\n" 
                          (if (fboundp 'org-supertag-db--remove-drawers-from-content)
                              (org-supertag-db--remove-drawers-from-content raw-content)
                            raw-content)))
               title)))))
     title
     "")))

(defun org-supertag-auto-tag--batch-handle-completion (result)
  "Handle batch processing completion callback, unify suggestions into main suggestion queue."
  (let* ((all-suggestions (plist-get result :suggestions))
         (failed-count (or (plist-get result :failed_count) 0))
         (new-suggestions-count 0))

    (when all-suggestions
      (dolist (node-suggestion-group all-suggestions)
        (let* ((node-id (plist-get node-suggestion-group :node_id))
               (suggestions-for-node (plist-get node-suggestion-group :suggestions))
               (existing-tags (org-supertag-node-get-tags node-id)))

          (dolist (suggestion-data suggestions-for-node)
            (let ((tag-name (plist-get suggestion-data :tag_name)))
              ;; Check if tag already exists on node
              (unless (member tag-name existing-tags)
                (let ((new-suggestion
                       (list :node-id node-id
                             :tag-name (plist-get suggestion-data :tag_name)
                             :confidence (plist-get suggestion-data :confidence)
                             :reasoning (plist-get suggestion-data :reasoning))))
                  (cl-pushnew new-suggestion org-supertag-auto-tag--suggestion-queue :test 'equal)
                  (cl-incf new-suggestions-count))))))))

    (message "Auto-tag: Background processing completed: %d new suggestions, %d failed nodes."
             new-suggestions-count failed-count)

    (when (> new-suggestions-count 0)
      (message "Auto-tag: New suggestions added to queue, use M-x org-supertag-auto-tag-batch-add to review."))))

;;; === Batch Tag Addition Interface ===

(defvar org-supertag-auto-tag--batch-nodes nil "Current batch processing node list")
(defvar org-supertag-auto-tag--batch-all-suggestions nil "All node suggestions hash table (node-id -> suggestions)")
(defvar org-supertag-auto-tag--batch-all-selected nil "All node selected tags hash table (node-id -> selected-tags)")

(defun org-supertag-auto-tag--batch-next-node ()
  "Move to the next node in the batch add buffer."
  (interactive)
  (end-of-line)
  ;; If not at end of buffer, move to next line to start search
  (unless (eobp) (forward-char 1))
  (if (re-search-forward "^\\[[0-9]+/[0-9]+\\]" nil t)
      (progn
        (beginning-of-line))
    ;; If not found, wrap around to the top
    (goto-char (point-min))
    (when (re-search-forward "^\\[[0-9]+/[0-9]+\\]" nil t)
      (beginning-of-line))))

(defun org-supertag-auto-tag--batch-prev-node ()
  "Move to the previous node in the batch add buffer."
  (interactive)
  (beginning-of-line)
  ;; If not at beginning of buffer, move to previous line to start search
  (unless (bobp) (forward-char -1))
  (if (re-search-backward "^\\[[0-9]+/[0-9]+\\]" nil t)
      (progn
        (beginning-of-line))
    ;; If not found, wrap around to the bottom
    (goto-char (point-max))
    (when (re-search-backward "^\\[[0-9]+/[0-9]+\\]" nil t)
      (beginning-of-line))))

(defvar org-supertag-auto-tag-batch-add-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "j") 'org-supertag-auto-tag--batch-next-node)
    (define-key map (kbd "k") 'org-supertag-auto-tag--batch-prev-node)
    (define-key map (kbd "SPC") 'org-supertag-auto-tag--batch-toggle-tag)
    (define-key map (kbd "RET") 'org-supertag-auto-tag--batch-apply-current-tag)
    (define-key map (kbd "A") 'org-supertag-auto-tag--batch-apply-all-selected)
    (define-key map (kbd "M") 'org-supertag-auto-tag--batch-add-manual-tag-to-node)
    (define-key map (kbd "g") 'org-supertag-auto-tag--batch-refresh-display)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Batch tag addition mode key mapping")

(define-derived-mode org-supertag-auto-tag-batch-add-mode special-mode "Batch-Add"
  "Batch tag addition mode"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (use-local-map org-supertag-auto-tag-batch-add-mode-map))

;;;###autoload
(defun org-supertag-auto-tag-batch-add ()
  "Open batch tag addition interface, process all pending suggestions in queue."
  (interactive)
  (if (seq-empty-p org-supertag-auto-tag--suggestion-queue)
      (message "Auto-tag: Suggestions queue is empty, no suggestions to process.")
    (let* ((suggestions-by-node (make-hash-table :test 'equal))
           (nodes-to-process '())
           (node-ids-processed (make-hash-table :test 'equal)))

      (dolist (suggestion org-supertag-auto-tag--suggestion-queue)
        (let ((node-id (plist-get suggestion :node-id)))
          (push suggestion (gethash node-id suggestions-by-node '()))))

      (maphash (lambda (node-id _suggestions)
                 (unless (gethash node-id node-ids-processed)
                   (when-let ((node-data (org-supertag-db-get node-id)))
                     (push (cons node-id node-data) nodes-to-process))
                   (puthash node-id t node-ids-processed)))
               suggestions-by-node)
      
      (let ((buffer (get-buffer-create "*Org Supertag Batch Add*")))
        (with-current-buffer buffer
          (org-supertag-auto-tag-batch-add-mode)
          (setq org-supertag-auto-tag--batch-nodes (nreverse nodes-to-process)
                org-supertag-auto-tag--batch-all-suggestions suggestions-by-node
                org-supertag-auto-tag--batch-all-selected (make-hash-table :test 'equal))
          
          (let ((inhibit-read-only t))
            (org-supertag-auto-tag--batch-refresh-display)))
        (pop-to-buffer buffer)))))

(defun org-supertag-auto-tag--batch-refresh-display ()
  "Refresh batch tag addition interface display"
  (let ((inhibit-read-only t)
        (current-tag-info (org-supertag-auto-tag--batch-get-current-tag)))
    (erase-buffer)
    (org-supertag-auto-tag--batch-insert-compact-display)
    (when current-tag-info
      (goto-char (point-min))
      (when (re-search-forward (format "^\\s-*[✓□] %s (" (regexp-quote (car current-tag-info))) nil t)
        (beginning-of-line)))))

(defun org-supertag-auto-tag--batch-insert-compact-display ()
  "Insert compact display for all nodes"
  (insert (propertize (format "Batch Tag Addition [Total %d nodes]\n" 
                             (length org-supertag-auto-tag--batch-nodes)) 
                     'face 'org-level-1))
  (insert "\n")
  
  (let ((node-index 0))
    (dolist (node-entry org-supertag-auto-tag--batch-nodes)
      (let* ((node-id (car node-entry))
             (node-data (cdr node-entry))
             (title (plist-get node-data :title))
             (file-path (plist-get node-data :file-path))
             (suggestions (gethash node-id org-supertag-auto-tag--batch-all-suggestions))
             (selected-tags (gethash node-id org-supertag-auto-tag--batch-all-selected))
             (progress (format "[%d/%d]" (1+ node-index) (length org-supertag-auto-tag--batch-nodes))))

        ;; Diagnostic messages for missing data
        (unless title
          (message "Auto-tag Diagnostics: Node ID '%s' is missing a title." node-id))
        (unless file-path
          (message "Auto-tag Diagnostics: Node ID '%s' is missing a file-path." node-id))

        (when (> node-index 0)
          (insert "\n" (propertize (make-string 80 ?═) 'face 'org-meta-line) "\n\n"))
        
        (insert (propertize (format "%s 📄 %s" progress (or title "Untitled Node")) 'face 'bold) "\n")
        (insert (propertize (format "    %s" (if file-path (file-name-nondirectory file-path) "<No File>")) 'face 'org-meta-line) "\n\n")
        
        (let ((content (org-supertag-auto-tag--get-node-content node-data)))
          (when content
            (let ((content-lines (split-string content "\n" t)))
              (dotimes (i (min 2 (length content-lines)))
                (let ((line (nth i content-lines)))
                  (when (and line (> (length (string-trim line)) 0))
                    (insert "    " (substring line 0 (min (length line) 76))
                            (if (> (length line) 76) "..." "") "\n")))))
            (insert "\n")))

        (insert (propertize "    🏷️ Suggested Tags:\n" 'face 'org-level-2))
        
        (if (null suggestions)
            (insert "        No suggestions or generating...\n")
          (dolist (suggestion suggestions)
            (let* ((tag-name (plist-get suggestion :tag-name))
                   (confidence (plist-get suggestion :confidence))
                   (selected-p (member tag-name selected-tags))
                   (confidence-color (cond ((>= confidence 0.8) 'success)
                                           ((>= confidence 0.6) 'warning)
                                           (t 'error)))
                   (line-start (point)))
              (insert (format "        %s %s " (if selected-p "✓" "□") (propertize tag-name 'face 'bold)))
              (insert (propertize (format "(%.2f)" confidence) 'face confidence-color) "\n")
              (put-text-property line-start (point) 'tag-name tag-name)
              (put-text-property line-start (point) 'node-id node-id))))
        (insert "\n")
        (cl-incf node-index))))
  
  (insert (propertize "j/k: node switch n/p: up/down move SPC: toggle select RET: apply current A: apply all M: manual add q: quit" 
                     'face 'org-meta-line)))

(defun org-supertag-auto-tag--batch-get-current-node-id ()
  "Get current cursor node ID"
  (get-text-property (point) 'node-id))

(defun org-supertag-auto-tag--batch-get-current-tag ()
  "Get current cursor tag name and node ID"
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "^\\s-*[✓□] \\(.+?\\) (" (line-end-position) t)
      (let ((tag-name (match-string-no-properties 1))
            (node-id (get-text-property (point) 'node-id)))
        (when node-id (cons tag-name node-id))))))

(defun org-supertag-auto-tag--batch-toggle-tag ()
  "Toggle current cursor tag selection state"
  (interactive)
  (when-let* ((tag-info (org-supertag-auto-tag--batch-get-current-tag))
              (tag-name (car tag-info))
              (node-id (cdr tag-info)))
    (let ((selected-tags (gethash node-id org-supertag-auto-tag--batch-all-selected)))
      (if (member tag-name selected-tags)
          (puthash node-id (remove tag-name selected-tags) org-supertag-auto-tag--batch-all-selected)
        (puthash node-id (cons tag-name selected-tags) org-supertag-auto-tag--batch-all-selected))
      (let ((inhibit-read-only t))
        (org-supertag-auto-tag--batch-refresh-display)))))

(defun org-supertag-auto-tag--batch-apply-current-tag ()
  "Apply current cursor tag"
  (interactive)
  (when-let* ((tag-info (org-supertag-auto-tag--batch-get-current-tag))
              (tag-name (car tag-info))
              (node-id (cdr tag-info)))
    (org-supertag-auto-tag--apply-tag-to-node node-id tag-name)
    (message "Auto-tag: Applied tag %s" tag-name)
    (let ((inhibit-read-only t))
      (org-supertag-auto-tag--batch-refresh-display))))

(defun org-supertag-auto-tag--batch-apply-all-selected ()
  "Apply all selected tags and close the window if any were applied."
  (interactive)
  (let ((total-applied 0))
    (maphash (lambda (node-id selected-tags)
               (when selected-tags
                 (dolist (tag-name selected-tags)
                   (org-supertag-auto-tag--apply-tag-to-node node-id tag-name)
                   (cl-incf total-applied))
                 (puthash node-id nil org-supertag-auto-tag--batch-all-selected)))
             org-supertag-auto-tag--batch-all-selected)

    (if (> total-applied 0)
        (progn
          (message "Auto-tag: Applied %d tags. Closing window." total-applied)
          (quit-window))
      (message "Auto-tag: No tags were selected to apply."))))

(defun org-supertag-auto-tag--batch-add-manual-tag-to-node ()
  "Add manual tag to current cursor node"
  (interactive)
  (when-let* ((node-id (org-supertag-auto-tag--batch-get-current-node-id))
              (tag-name (read-string "Add tag to current node: ")))
    (when (and tag-name (> (length (string-trim tag-name)) 0))
      (org-supertag-auto-tag--apply-tag-to-node node-id (string-trim tag-name))
      (message "Added tag: %s" tag-name)
      (let ((inhibit-read-only t))
        (org-supertag-auto-tag--batch-refresh-display)))))

(defun org-supertag-auto-tag--apply-tag-to-node (node-id tag-name)
  "Apply tag to node (database and file)."
  (let* ((tag-result (org-supertag-inline--ensure-tag tag-name))
         (tag-id (plist-get tag-result :tag-id)))
    (org-supertag-node-db-add-tag node-id tag-id)
    (org-supertag-inline-insert-tag-for-node node-id tag-name)
    
    ;; Remove processed suggestions from queue
    (setq org-supertag-auto-tag--suggestion-queue
          (seq-remove (lambda (s)
                        (and (equal (plist-get s :node-id) node-id)
                             (equal (plist-get s :tag-name) tag-name)))
                      org-supertag-auto-tag--suggestion-queue)))
  (message "Applied tag '%s' to node" tag-name))

(defun org-supertag-auto-tag-get-tags-from-llm (nodes callback)
  "Send text chunks in NODES to LLM and get tags asynchronously."
  (let* ((note-ids (mapcar (lambda (node) (plist-get node :id)) nodes))
         (payload (list
                   (list
                    '("nodes" . nodes)
                    '("note_ids" . note-ids)))))
    (org-supertag-bridge-call-async
     "autotag/get_tags"
     payload
     (lambda (result)
       (when (and result (not (plist-get result :error)))
         (funcall callback result))
       (unless result
         (message "Auto-tagging returned no result."))))))

(defun org-supertag-auto-tag-get-tags-from-llm-sync (nodes)
  "Send text chunks in NODES to LLM and get tags synchronously."
  (let* ((note-ids (mapcar (lambda (node) (plist-get node :id)) nodes))
         (payload (list
                   (list
                    '("nodes" . nodes)
                    '("note_ids" . note-ids)))))
    (org-supertag-bridge-call-sync
     "autotag/get_tags"
     payload
     (lambda (result)
       (when (and result (not (plist-get result :error)))
         (funcall result))
       (unless result
         (message "Auto-tagging returned no result."))))))

(provide 'org-supertag-auto-tag)

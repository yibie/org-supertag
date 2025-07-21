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
(require 'org-supertag-scheduler)
(require 'cl-lib)

;;; === Core Configuration ===

(defgroup org-supertag-auto-tag nil
  "Auto-tagging system configuration"
  :group 'org-supertag)

(defcustom org-supertag-auto-tag-enable-silent-scan t
  "If non-nil, automatically scan and generate suggestions for untagged nodes in background."
  :type 'boolean
  :group 'org-supertag-auto-tag)

(defcustom org-supertag-auto-tag-scan-daily-time "02:30"
  "The time of day (HH:MM format) to run the silent background scan for untagged nodes."
  :type 'string
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

(defcustom org-supertag-auto-tag-provider 'ollama
  "The AI provider to use for auto-tagging.
Supported values match the Python backend's LLMClient configuration."
  :type '(choice (const :tag "Ollama" ollama)
                 (const :tag "OpenAI" openai)
                 (const :tag "Google" google))
  :group 'org-supertag-auto-tag)

(defcustom org-supertag-auto-tag-model "hf.co/unsloth/gemma-3-4b-it-GGUF:latest"
  "The model name to use for auto-tagging."
  :type 'string
  :group 'org-supertag-auto-tag)

(defcustom org-supertag-auto-tag-temperature 0.7
  "The sampling temperature for the auto-tagging model (0.0 - 2.0)."
  :type 'float
  :group 'org-supertag-auto-tag)

(defcustom org-supertag-auto-tag-max-tokens 2048
  "The maximum number of tokens for the auto-tagging model's response."
  :type 'integer
  :group 'org-supertag-auto-tag)

(defcustom org-supertag-auto-tag-endpoint "http://localhost:11434"
  "The API endpoint for the auto-tagging provider.
This is particularly important for local providers like Ollama."
  :type 'string
  :group 'org-supertag-auto-tag)

;;----------------------------------------------------------------------
;; Global Variables
;;----------------------------------------------------------------------

(defvar org-supertag-auto-tag--suggestion-queue '()
  "Global queue for storing tag suggestions. Each element is a plist representing a suggestion.")

(defvar org-supertag-auto-tag--node-content-cache (make-hash-table :test 'equal)
  "Cache node content to avoid duplicate IO.")

(defvar org-supertag-auto-tag--last-prompt-date nil
  "Date (YYYY-MM-DD) of last reminder prompt, to avoid duplicate reminders.")

;;----------------------------------------------------------------------
;; Auto Mode and Background Scanning
;;----------------------------------------------------------------------

(defun org-supertag-auto-tag--prompt-for-review ()
  "Prompt user to review tags if conditions are met."
  (when (and org-supertag-auto-tag-enable-daily-reminder
             (not (seq-empty-p org-supertag-auto-tag--suggestion-queue)))
    (when (yes-or-no-p (format "Auto-tag: %d suggestions pending, review now?"
                               (length org-supertag-auto-tag--suggestion-queue)))
      ;; This function must be called from a timer, so it's safe
      ;; to assume we can pop up a window.
      (with-current-buffer (get-buffer-create "*Org Supertag Batch Add*")
        (org-supertag-auto-tag-batch-add)))))

;;;###autoload
(define-minor-mode org-supertag-auto-tag-mode
  "Enable auto-tagging for org-supertag.
This mode itself doesn't start any process, it's just a switch.
This mode registers and deregisters the background tasks with the central scheduler."
  :init-value nil
  :lighter " ST-Auto"
  :group 'org-supertag
  (if org-supertag-auto-tag-mode
      ;; When mode is enabled, register tasks with the scheduler.
      (progn
        (when org-supertag-auto-tag-enable-silent-scan
          (org-supertag-scheduler-register-task
           'auto-tag-silent-scan
           :daily
           #'org-supertag-auto-tag-silent-scan-and-generate
           :time org-supertag-auto-tag-scan-daily-time))
        (when org-supertag-auto-tag-enable-daily-reminder
          (org-supertag-scheduler-register-task
           'auto-tag-daily-reminder
           :daily
           #'org-supertag-auto-tag--prompt-for-review
           :time org-supertag-auto-tag-daily-reminder-time))
        (message "Org SuperTag Auto-Tag Mode enabled."))
    ;; When mode is disabled, deregister tasks.
    (org-supertag-scheduler-deregister-task 'auto-tag-silent-scan)
    (org-supertag-scheduler-deregister-task 'auto-tag-daily-reminder)
    (message "Org SuperTag Auto-Tag Mode disabled.")))

(defun org-supertag-auto-tag-silent-scan-and-generate ()
  "Scan database for untagged nodes and generate tag suggestions in batch.
This function now iterates through all potential nodes to find a batch
of nodes that meet the minimum content length requirement, avoiding getting
stuck on short-content nodes."
  (interactive)
  (message "[Auto-tag] Silent scan called at %s" (format-time-string "%H:%M:%S.%3N"))
  (let* ((all-nodes (org-supertag-node-get-all))
         (untagged-nodes-all (seq-filter
                              (lambda (node-id)
                                (seq-empty-p (org-supertag-node-get-tags node-id)))
                              all-nodes))
         (processed-node-ids (delete-dups (mapcar (lambda (s) (plist-get s :node-id)) org-supertag-auto-tag--suggestion-queue)))
         (candidate-nodes (seq-remove (lambda (node-id) (member node-id processed-node-ids))
                                      untagged-nodes-all))
         (eligible-nodes-payload '())
         (limit (if org-supertag-auto-tag-batch-enable-limit
                    org-supertag-auto-tag-batch-max-nodes-per-run
                  (length candidate-nodes))))

    ;; Iterate through candidates to build a batch of eligible nodes
    (let ((remaining-candidates candidate-nodes))
      (while (and remaining-candidates (< (length eligible-nodes-payload) limit))
        (let* ((node-id (car remaining-candidates))
               (node-data (org-supertag-db-get node-id)))
          (when-let* ((content (org-supertag-auto-tag--get-node-content node-data)))
            (let* ((title (plist-get node-data :title))
                   (combined (concat (or title "") " " (or content ""))))
              (when (>= (length (string-trim combined)) org-supertag-auto-tag-batch-min-content-length)
                (push `(("id" . ,node-id) ("content" . ,combined)) eligible-nodes-payload)))))
        (setq remaining-candidates (cdr remaining-candidates))))

    (if (seq-empty-p eligible-nodes-payload)
        (message "Auto-tag: No new untagged nodes with sufficient content found to process.")
      (progn
        (message "Auto-tag: Found %d eligible untagged nodes to process. Sending to backend..." (length eligible-nodes-payload))
        (let* ((nodes-to-send (reverse eligible-nodes-payload))
               (model-config (org-supertag-auto-tag--get-model-config))
               (payload `(("nodes" ,nodes-to-send)
                         ("model_config" ,model-config))))
          (org-supertag-api-batch-generate-tags
           (list payload)
           #'org-supertag-auto-tag--batch-handle-completion))))))

(defun org-supertag-auto-tag--get-model-config ()
  "Get model configuration for auto-tagging from this module's custom variables.
This ensures auto-tagging is self-contained and not dependent on other modules like `org-supertag-ai`."
  (list
   (cons 'provider org-supertag-auto-tag-provider)
   (cons 'model org-supertag-auto-tag-model)
   (cons 'temperature org-supertag-auto-tag-temperature)
   (cons 'max_tokens org-supertag-auto-tag-max-tokens)
   (cons 'endpoint org-supertag-auto-tag-endpoint)))

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

(defun org-supertag-auto-tag--batch-handle-completion (results)
  "Handle batch processing completion callback from Python backend.
RESULTS is a vector of plists, each representing a suggestion for a node."
  (message "Auto-tag: Received %d suggestions from backend." (length results))
  (let ((new-suggestions-count 0))
    (dolist (suggestion results)
      (let ((node-id (plist-get suggestion :node_id))
            (tags (plist-get suggestion :tags)))
        (when (and node-id tags)
          ;; Check if a suggestion for this node already exists to avoid duplicates.
          (unless (cl-some (lambda (q-item) (string= (plist-get q-item :node-id) node-id))
                           org-supertag-auto-tag--suggestion-queue)
            (let* ((node-data (org-supertag-db-get node-id))
                   (content (or (gethash node-id org-supertag-auto-tag--node-content-cache)
                                (org-supertag-auto-tag--get-node-content node-data))))
              (when content
                ;; Add to suggestion queue with a consistent structure
                (push (list :node-id node-id
                            :content content
                            :tags tags)
                      org-supertag-auto-tag--suggestion-queue)
                (puthash node-id content org-supertag-auto-tag--node-content-cache)
                (cl-incf new-suggestions-count)))))))
    (if (> new-suggestions-count 0)
        (message "Auto-tag: Added %d new suggestions to the queue. Total pending: %d."
                 new-suggestions-count
                 (length org-supertag-auto-tag--suggestion-queue))
      (message "Auto-tag: No new suggestions were added (already in queue or invalid format)."))))

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

      ;; Correctly build the suggestions hash table using the :tags key
      (dolist (item org-supertag-auto-tag--suggestion-queue)
        (let ((node-id (plist-get item :node-id))
              (tags (plist-get item :tags)))
          ;; Only store the actual list of tag suggestion plists
          (puthash node-id tags suggestions-by-node)))

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
      (when (re-search-forward (format "^\\s-*\\(?:[✓□]\\) %s (" (regexp-quote (car current-tag-info))) nil t)
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
             (progress (format "[%d/%d]" (1+ node-index) (length org-supertag-auto-tag--batch-nodes)))
             (start (point)))

        ;; Diagnostic messages for missing data
        (unless title
          (message "Auto-tag Diagnostics: Node ID '%s' is missing a title." node-id))
        (unless file-path
          (message "Auto-tag Diagnostics: Node ID '%s' is missing a file-path." node-id))

        (when (> node-index 0)
          (insert "\n" (propertize "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" 'face 'org-meta-line) "\n\n"))
        
        (let ((node-start (point)))
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
          (if (or (not suggestions) (seq-empty-p suggestions))
              (insert "        No suggestions or generating...\n")
            (dolist (tag-name suggestions)
              (when (stringp tag-name) ; Add safeguard
                (let* ((selected-p (member tag-name selected-tags))
                       (line-start (point)))
                  (insert (format "        %s %s\n"
                                  (if selected-p "✓" "□")
                                  (propertize tag-name 'face 'bold)))
                  (put-text-property line-start (point) 'tag-name tag-name)
                  (put-text-property line-start (point) 'node-id node-id)))))
          (insert "\n")
          (add-text-properties node-start (point) `(node-id ,node-id)))
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
    (when (re-search-forward "^\\s-*\\(?:[✓□]\\) \\(.+?\\)\\s-*$" (line-end-position) t)
      (let ((tag-name (string-trim (match-string-no-properties 1)))
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
      (let ((pos (point))
            (win-start (window-start)))
        (org-supertag-auto-tag--batch-refresh-current-node)
        ;; 恢复光标和窗口位置
        (goto-char pos)
        (set-window-start (selected-window) win-start)))))

(defun org-supertag-auto-tag--batch-apply-current-tag ()
  "Apply current cursor tag"
  (interactive)
  (when-let* ((tag-info (org-supertag-auto-tag--batch-get-current-tag))
              (tag-name (car tag-info))
              (node-id (cdr tag-info)))
    (org-supertag-auto-tag--apply-multiple-tags-to-node node-id (list tag-name))
    (message "Auto-tag: Applied tag %s" tag-name)
    (let ((inhibit-read-only t))
      (org-supertag-auto-tag--batch-refresh-display))))

(defun org-supertag-auto-tag--batch-apply-all-selected ()
  "Apply all selected tags and close the window if any were applied."
  (interactive)
  (let ((total-applied 0)
        (tags-by-node (make-hash-table :test 'equal)))
    
    ;; Group tags by node for batch processing
    (maphash (lambda (node-id selected-tags)
               (when selected-tags
                 (puthash node-id selected-tags tags-by-node)))
             org-supertag-auto-tag--batch-all-selected)
    
    ;; Apply tags in batches per node
    (maphash (lambda (node-id selected-tags)
               (when selected-tags
                 (org-supertag-auto-tag--apply-multiple-tags-to-node node-id selected-tags)
                 (setq total-applied (+ total-applied (length selected-tags)))
                 (puthash node-id nil org-supertag-auto-tag--batch-all-selected)))
             tags-by-node)

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
      (org-supertag-auto-tag--apply-multiple-tags-to-node node-id (list (string-trim tag-name)))
      (message "Added tag: %s" tag-name)
      (let ((inhibit-read-only t))
        (org-supertag-auto-tag--batch-refresh-display)))))

(defun org-supertag-auto-tag--apply-tag-to-node (node-id tag-name)
  "Apply tag to node, ensuring it's fully registered in the database and file.
This function now supports both single tag and batch tag application."
  (message "Applying tag '%s' to node '%s'..." tag-name node-id)
  ;; 1. Sanitize the tag name to get a valid ID.
  (let ((tag-id (org-supertag-sanitize-tag-name tag-name)))
    ;; 2. Check if the tag object exists in the database. If not, create it.
    (unless (org-supertag-tag-get tag-id)
      (message "Tag '%s' not found in DB, creating it." tag-id)
      (org-supertag-tag--create tag-id))
    
    ;; 3. Link the node to the tag in the database.
    (message "Linking node '%s' to tag '%s'." node-id tag-id)
    (org-supertag-node-db-add-tag node-id tag-id)
    
    ;; 4. Insert the tag visually into the Org file.
    (message "Inserting tag '%s' into file for node '%s'." tag-name node-id)
    (org-supertag-inline-insert-tag-for-autotag node-id tag-name)
    
    ;; 5. Remove the corresponding suggestion from the queue after applying.
    (setq org-supertag-auto-tag--suggestion-queue
          (cl-remove-if
           (lambda (q-item)
             (string= (plist-get q-item :node-id) node-id))
           org-supertag-auto-tag--suggestion-queue))
    (message "Tag '%s' applied and all suggestions for this node removed from queue." tag-name)))

(defun org-supertag-auto-tag--apply-multiple-tags-to-node (node-id tag-names)
  "Apply multiple tags to node with improved formatting.
NODE-ID is the node identifier.
TAG-NAMES is a list of tag names to apply.
This function ensures all tags are properly formatted and spaced."
  (when (and node-id tag-names)
    (message "Applying %d tags to node '%s'..." (length tag-names) node-id)
    
    ;; 1. Process all tags in the database first
    (dolist (tag-name tag-names)
      (let ((tag-id (org-supertag-sanitize-tag-name tag-name)))
        ;; Ensure tag exists in database
        (unless (org-supertag-tag-get tag-id)
          (message "Tag '%s' not found in DB, creating it." tag-id)
          (org-supertag-tag--create tag-id))
        
        ;; Link node to tag in database
        (org-supertag-node-db-add-tag node-id tag-id)))
    
    ;; 2. Insert all tags visually into the Org file with proper formatting
    (let ((found-location (org-supertag-find-node-location node-id)))
      (if found-location
          (let ((pos (car found-location))
                (file-path (cdr found-location)))
            (with-current-buffer (find-file-noselect file-path)
              (save-excursion
                (goto-char pos)
                (org-back-to-heading t)
                ;; Use the smart positioning logic for tag insertion
                (org-supertag-inline--smart-position-for-insertion)
                ;; Insert all tags with proper formatting
                (org-supertag-inline--insert-multiple-tags-for-autotag tag-names)
                ;; Save the buffer to ensure tags are written to file
                (save-buffer)
                (message "Auto-inserted %d tags for node %s at %s" (length tag-names) node-id file-path))))
        (error "Could not find node with ID: %s" node-id)))
    
    ;; 3. Remove suggestions from the queue
    (setq org-supertag-auto-tag--suggestion-queue
          (cl-remove-if
           (lambda (q-item)
             (string= (plist-get q-item :node-id) node-id))
           org-supertag-auto-tag--suggestion-queue))
    
    (message "Applied %d tags to node %s: %s" 
             (length tag-names) 
             node-id 
             (mapconcat #'identity tag-names ", "))))

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

(defun org-supertag-auto-tag--apply-suggestion-for-node (node-id tag-suggestions)
  "Apply selected tag suggestions to a specific node.
This function now correctly registers the tag in the database."
  (dolist (suggestion tag-suggestions)
    (let ((tag-name (plist-get suggestion :tag-name)))
      (when (and (stringp tag-name) (not (string-empty-p tag-name)))
        (message "Auto-tag: Applying suggestion '%s' to node '%s'" tag-name node-id)
        
        ;; --- Database Registration Logic ---
        ;; 1. Sanitize and ensure the tag object exists in the database.
        (let ((sanitized-tag-name (org-supertag-sanitize-tag-name tag-name)))
          (unless (org-supertag-tag-get sanitized-tag-name)
            (org-supertag-tag--create sanitized-tag-name))
          
          ;; 2. Link the node to the tag in the database.
          (org-supertag-node-db-add-tag node-id sanitized-tag-name))

        ;; --- UI/File Update Logic ---
        ;; 3. Visually add the tag to the Org file as an inline tag.
        (org-supertag-inline-tag-add (list tag-name) node-id)))))

(defun org-supertag-auto-tag--get-all-suggestions-for-node (node-id)
  "Get all suggestions for a given NODE-ID from the suggestion-queue."
  (seq-filter (lambda (s)
                (and (equal (plist-get s :node-id) node-id)
                     (stringp (plist-get s :tag-name))))
              org-supertag-auto-tag--suggestion-queue))

(defun org-supertag-auto-tag--batch-refresh-current-node ()
  "Only refresh the display of the current node, not the entire buffer."
  (let* ((node-id (org-supertag-auto-tag--batch-get-current-node-id)))
    (when node-id
      (save-excursion
        (let ((inhibit-read-only t))
          (let* ((start (previous-single-property-change (point) 'node-id nil (point-min)))
                 (end (next-single-property-change (point) 'node-id nil (point-max))))
            (when (and start end)
              (goto-char start)
              (delete-region start end)
              (let* ((node-entry (assoc node-id org-supertag-auto-tag--batch-nodes))
                     (node-data (cdr node-entry))
                     (title (plist-get node-data :title))
                     (file-path (plist-get node-data :file-path))
                     (suggestions (gethash node-id org-supertag-auto-tag--batch-all-suggestions))
                     (selected-tags (gethash node-id org-supertag-auto-tag--batch-all-selected))
                     (progress (format "[%d/%d]"
                                       (1+ (cl-position node-entry org-supertag-auto-tag--batch-nodes :test #'equal))
                                       (length org-supertag-auto-tag--batch-nodes)))
                     (node-start (point)))
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
                (if (or (not suggestions) (seq-empty-p suggestions))
                    (insert "        No suggestions or generating...\n")
                  (dolist (tag-name suggestions)
                    (when (stringp tag-name) ; Add safeguard
                      (let* ((selected-p (member tag-name selected-tags))
                             (line-start (point)))
                        (insert (format "        %s %s\n"
                                        (if selected-p "✓" "□")
                                        (propertize tag-name 'face 'bold)))
                        (put-text-property line-start (point) 'tag-name tag-name)
                        (put-text-property line-start (point) 'node-id node-id)))))
                (insert "\n")
                (add-text-properties node-start (point) `(node-id ,node-id))))))))))


;;; === Suggestion UI Mode ===

(defvar org-supertag-auto-tag--suggestion-node-id nil "Current suggestion node ID")
(defvar org-supertag-auto-tag--suggestion-tags nil "Current suggestion tags")
(defvar org-supertag-auto-tag--suggestion-selected nil "Selected tags for current suggestion")

(defvar org-supertag-auto-tag-suggestion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "SPC") 'org-supertag-auto-tag--suggestion-toggle-tag)
    (define-key map (kbd "RET") 'org-supertag-auto-tag--suggestion-apply-current-tag)
    (define-key map (kbd "A") 'org-supertag-auto-tag--suggestion-apply-all-selected)
    (define-key map (kbd "M") 'org-supertag-auto-tag--suggestion-add-manual-tag)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Suggestion UI mode key mapping")

(define-derived-mode org-supertag-auto-tag-suggestion-mode special-mode "Suggestion-UI"
  "Suggestion UI mode for single node tag suggestions"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (use-local-map org-supertag-auto-tag-suggestion-mode-map))

(defun org-supertag-auto-tag--suggestion-insert-compact-display ()
  "Insert compact display for single node suggestion"
  (let* ((node-data (org-supertag-db-get org-supertag-auto-tag--suggestion-node-id))
         (title (plist-get node-data :title))
         (file-path (plist-get node-data :file-path))
         (content (org-supertag-auto-tag--get-node-content node-data)))
    
    (insert (propertize "Tag Suggestion Interface\n" 'face 'org-level-1))
    (insert "\n")
    
    ;; Node information
    (insert (propertize (format "📄 %s" (or title "Untitled Node")) 'face 'bold) "\n")
    (insert (propertize (format "    %s" (if file-path (file-name-nondirectory file-path) "<No File>")) 'face 'org-meta-line) "\n\n")
    
    ;; Content preview
    (when content
      (let ((content-lines (split-string content "\n" t)))
        (dotimes (i (min 3 (length content-lines)))
          (let ((line (nth i content-lines)))
            (when (and line (> (length (string-trim line)) 0))
              (insert "    " (substring line 0 (min (length line) 76))
                      (if (> (length line) 76) "..." "") "\n")))))
      (insert "\n"))
    
    ;; Suggested tags
    (insert (propertize "    🏷️ Suggested Tags:\n" 'face 'org-level-2))
    (if (or (not org-supertag-auto-tag--suggestion-tags) (seq-empty-p org-supertag-auto-tag--suggestion-tags))
        (insert "        No suggestions available.\n")
      (dolist (tag-name org-supertag-auto-tag--suggestion-tags)
        (when (stringp tag-name)
          (let* ((selected-p (member tag-name org-supertag-auto-tag--suggestion-selected))
                 (line-start (point)))
            (insert (format "        %s %s\n"
                            (if selected-p "✓" "□")
                            (propertize tag-name 'face 'bold)))
            (put-text-property line-start (point) 'tag-name tag-name)
            (put-text-property line-start (point) 'node-id org-supertag-auto-tag--suggestion-node-id)))))
    
    (insert "\n")
    (insert (propertize "SPC: toggle select RET: apply current A: apply all M: manual add q: quit" 
                       'face 'org-meta-line))))

(defun org-supertag-auto-tag--suggestion-get-current-tag ()
  "Get current cursor tag name"
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "^\\s-*\\(?:[✓□]\\) \\(.+?\\)\\s-*$" (line-end-position) t)
      (string-trim (match-string-no-properties 1)))))

(defun org-supertag-auto-tag--suggestion-toggle-tag ()
  "Toggle current cursor tag selection state"
  (interactive)
  (when-let* ((tag-name (org-supertag-auto-tag--suggestion-get-current-tag)))
    (if (member tag-name org-supertag-auto-tag--suggestion-selected)
        (setq org-supertag-auto-tag--suggestion-selected (remove tag-name org-supertag-auto-tag--suggestion-selected))
      (push tag-name org-supertag-auto-tag--suggestion-selected))
    (let ((pos (point))
          (win-start (window-start)))
      (let ((inhibit-read-only t))
        (org-supertag-auto-tag--suggestion-refresh-display))
      (goto-char pos)
      (set-window-start (selected-window) win-start))))

(defun org-supertag-auto-tag--suggestion-apply-current-tag ()
  "Apply current cursor tag"
  (interactive)
  (when-let* ((tag-name (org-supertag-auto-tag--suggestion-get-current-tag)))
    (org-supertag-auto-tag--apply-multiple-tags-to-node org-supertag-auto-tag--suggestion-node-id (list tag-name))
    (message "Auto-tag: Applied tag %s" tag-name)
    (let ((inhibit-read-only t))
      (org-supertag-auto-tag--suggestion-refresh-display))))

(defun org-supertag-auto-tag--suggestion-apply-all-selected ()
  "Apply all selected tags and close the window if any were applied."
  (interactive)
  (if org-supertag-auto-tag--suggestion-selected
      (progn
        (org-supertag-auto-tag--apply-multiple-tags-to-node org-supertag-auto-tag--suggestion-node-id org-supertag-auto-tag--suggestion-selected)
        (message "Auto-tag: Applied %d tags. Closing window." (length org-supertag-auto-tag--suggestion-selected))
        (quit-window))
    (message "Auto-tag: No tags were selected to apply.")))

(defun org-supertag-auto-tag--suggestion-add-manual-tag ()
  "Add manual tag to current node"
  (interactive)
  (when-let* ((tag-name (read-string "Add tag to current node: ")))
    (when (and tag-name (> (length (string-trim tag-name)) 0))
      (org-supertag-auto-tag--apply-multiple-tags-to-node org-supertag-auto-tag--suggestion-node-id (list (string-trim tag-name)))
      (message "Added tag: %s" tag-name)
      (let ((inhibit-read-only t))
        (org-supertag-auto-tag--suggestion-refresh-display)))))

(defun org-supertag-auto-tag--suggestion-refresh-display ()
  "Refresh suggestion UI display"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-supertag-auto-tag--suggestion-insert-compact-display)))

;;;###autoload
(defun org-supertag-auto-tag-show-suggestion-ui (suggestions)
  "Show tag SUGGESTIONS to the user with a compact interface similar to batch mode.
This function now provides a rich interface with keyboard navigation and visual feedback.
SUGGESTIONS is a list of plists, where each plist has :node_id and :tags."
  (interactive)
  (when suggestions
    ;; Assuming suggestions are for the current node, and there's only one suggestion in the list.
    (let* ((suggestion (car suggestions))
           (node-id (plist-get suggestion :node_id))
           (tags (plist-get suggestion :tags)))
      (when (and node-id tags)
        (let ((buffer (get-buffer-create "*Org Supertag Suggestion*")))
          (with-current-buffer buffer
            (org-supertag-auto-tag-suggestion-mode)
            (setq org-supertag-auto-tag--suggestion-node-id node-id
                  org-supertag-auto-tag--suggestion-tags tags
                  org-supertag-auto-tag--suggestion-selected '())
            
            (let ((inhibit-read-only t))
              (org-supertag-auto-tag--suggestion-refresh-display))
            
            (pop-to-buffer buffer)))))))

(provide 'org-supertag-auto-tag)

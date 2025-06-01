;;; org-supertag-sim.el --- Semantic similarity support for org-supertag -*- lexical-binding: t; -*-

;; Author: Your Name
;; Keywords: outlines, org-mode, tags
;; Package-Requires: ((emacs "28.1") (org-supertag "0.1"))

;;; Commentary:
;; Provides semantic 1similarity support for org-supertag.
;; Uses sim-tag.py as the backend to provide tag similarity calculation and recommendation functionality.

;;; Code:

(require 'json)
(require 'org-supertag-db)
(require 'org-supertag-sim-epc)  

(defgroup org-supertag-sim nil
  "Semantic similarity support for org-supertag."
  :group 'org-supertag)

(defcustom org-supertag-sim-vector-file
  (org-supertag-data-file "supertag_vector.db")
  "Tag vector storage file path."
  :type 'file
  :group 'org-supertag-sim)

(defcustom org-supertag-sim-sync-interval 3600
  "Vector library synchronization interval (seconds)."
  :type 'integer
  :group 'org-supertag-sim)

(defvar org-supertag-sim--initialized nil
  "Flag indicating whether the system has been initialized.")

(defvar org-supertag-sim--sync-timer nil
  "Timer for periodic synchronization.")

(defvar org-supertag-sim-tag-select-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "SPC") #'org-supertag-sim-toggle-tag-selection)
    (define-key map (kbd "RET") #'org-supertag-sim-toggle-tag-selection)
    (define-key map (kbd "C-c C-c") #'org-supertag-sim-apply-selected-tags)
    (define-key map (kbd "C-c C-k") #'org-supertag-sim-cancel-tag-selection)
    (define-key map (kbd "q") #'org-supertag-sim-cancel-tag-selection)
    (define-key map (kbd "a") #'org-supertag-sim-select-all-tags)
    (define-key map (kbd "A") #'org-supertag-sim-unselect-all-tags)
    (define-key map (kbd "r") #'org-supertag-sim-regenerate-tags)
    map)
  "Tag select mode keybindings.")

(define-derived-mode org-supertag-sim-tag-select-mode special-mode "OrgSuperTag-TagSelect"
  "Major mode for tag selection in org-supertag."
  :group 'org-supertag
  (setq-local buffer-read-only t))

(defun org-supertag-sim--get-all-tags ()
  "Get the names and IDs of all tags."
  (let ((tags '()))
    (dolist (tag (org-supertag-db-find-by-type :tag))
      (when-let* ((tag-props (org-supertag-db-get tag))
                  (tag-name (plist-get tag-props :name)))
        (push (cons tag tag-name) tags)))
    tags))

(defun org-supertag-sim--ensure-db-file ()
  "Ensure the database file exists and return its path and tag data."
  (unless (boundp 'org-supertag-db-file)
    (error "Database file path is not defined"))
  (let* ((db-file org-supertag-db-file)
         (tags (org-supertag-db-find-by-type :tag)))
    ;; Ensure the database file exists
    (unless (file-exists-p db-file)
      (org-supertag-db-save))
    
    ;; Generate tag data directly in the format expected by Python:
    ;; a list of [tag-id, tag-name] pairs
    (let ((tag-data
           (delq nil
                 (mapcar (lambda (tag)
                          (when-let* ((props (org-supertag-db-get tag))
                                      (tag-name (plist-get props :name)))
                            ;; Return as a simple list [id name] which will be transmitted correctly
                            (list tag tag-name)))
                        tags))))
      ;; Log the format being used
      (message "Prepared %d tags for sync in format: %S" 
               (length tag-data) 
               (when (> (length tag-data) 0) (car tag-data)))
      (list db-file tag-data))))

(defun org-supertag-sim--ensure-vector-dir ()
  "Ensure the vector file directory exists."
  (let ((vector-dir (file-name-directory org-supertag-sim-vector-file)))
    (unless (file-exists-p vector-dir)
      (make-directory vector-dir t))
    vector-dir))

(defun org-supertag-sim-init ()
  "Initialize the tag similarity engine, including EPC server and Ollama service.
As a unified entry point, it will initialize the entire system, including the underlying EPC service and similarity engine."
  (interactive)
  (message "Initializing tag similarity system...")
  (condition-case err
      (progn
        (unless (org-supertag-sim-epc-server-alive-p)
          (org-supertag-sim-epc-start-server)
          (sit-for 1))
        (when (org-supertag-sim-epc-server-alive-p)
          (org-supertag-db-on 'entity:created #'org-supertag-sim--on-tag-created)
          (org-supertag-db-on 'entity:removed #'org-supertag-sim--on-tag-removed)
          (org-supertag-sim--start-sync-timer)
          (setq org-supertag-sim--initialized t)
          (message "Tag similarity system initialized")
          ;; 注册EPC连接建立的hook，立即执行初始同步
          (add-hook 'org-supertag-sim-epc-connection-established-hook 
                   #'org-supertag-sim--sync-library)
          ;; 延迟执行初始同步，等待EPC连接建立
          (run-with-timer 3 nil #'org-supertag-sim--sync-library)))
    (error
     (message "Tag similarity system initialization failed: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim--on-tag-created (tag &rest _)
  "Handle tag creation events.
TAG is the created tag information."
  (when org-supertag-sim--initialized
    (let ((tag-props (org-supertag-db-get tag)))
      (when (eq (plist-get tag-props :type) :tag)
        (org-supertag-sim--update-tag tag)))))

(defun org-supertag-sim--on-tag-removed (tag-id &rest rest-args)
  "Handle tag removal events.
TAG-ID is the ID of the removed tag."
  (message "org-supertag-sim--on-tag-removed received: tag-id=%s, rest=%S" tag-id rest-args)
  (when org-supertag-sim--initialized
    ;; Assuming the tag type is known or irrelevant for removal
    (org-supertag-sim--remove-tag tag-id)))

(defun org-supertag-sim--update-tag (tag)
  "Update the vector of a single tag.
TAG is the tag information."
  (when org-supertag-sim--initialized
    (let ((tag-props (org-supertag-db-get tag)))
      (condition-case err
          (epc:call-deferred org-supertag-sim-epc-manager
                            'update_tag
                            (list (list tag (plist-get tag-props :name))))
        (error
         (message "Update tag vector error: %s" (error-message-string err)))))))

(defun org-supertag-sim--remove-tag (tag-id)
  "Remove a tag from the vector library.
TAG-ID is the tag ID."
  (when org-supertag-sim--initialized
    (condition-case err
        (epc:call-deferred org-supertag-sim-epc-manager
                          'remove_tag
                          (list tag-id))
      (error
       (message "Delete tag vector error: %s" (error-message-string err))))))

(defun org-supertag-sim--sync-library ()
  "Synchronize the tag vector library."
  (when org-supertag-sim--initialized
    (let* ((db-info (org-supertag-sim--ensure-db-file))
           (db-file (car db-info))
           (tag-data (cadr db-info)))
      
      ;; 添加调试日志
      (message "Synchronizing tag vector library: %d tags" (length tag-data))
      
      (if (org-supertag-sim-epc-manager-live-p)
          (condition-case err
              (epc:call-deferred org-supertag-sim-epc-manager
                               'sync_library
                               (list db-file tag-data))
            (error
             (message "Sync tag library error (during EPC call): %s" (error-message-string err))))
        ;; 如果EPC管理器未准备好，等待一段时间后重试
        (message "EPC manager not ready for sync, will retry in 3 seconds...")
        (run-with-timer 3 nil
                       (lambda ()
                         (when (org-supertag-sim-epc-manager-live-p)
                           (message "Retrying tag library synchronization...")
                           (org-supertag-sim--sync-library))))))))

(defun org-supertag-sim--start-sync-timer ()
  "Start the periodic synchronization timer."
  (when org-supertag-sim--sync-timer
    (cancel-timer org-supertag-sim--sync-timer))
  (setq org-supertag-sim--sync-timer
        (run-with-timer 
         org-supertag-sim-sync-interval
         org-supertag-sim-sync-interval
         #'org-supertag-sim--sync-library)))

(defun org-supertag-sim--ensure-initialized ()
  "Ensure the system is initialized, return a deferred object.
If the system is not initialized, it will try to initialize automatically.

Returns:
- A deferred object that will be resolved when the system is initialized
- nil if the system is already initialized"
  (if (and org-supertag-sim--initialized 
           (org-supertag-sim-epc-server-alive-p)
           (org-supertag-sim-epc-manager-live-p))
      ;; 如果系统已经初始化且连接正常，直接返回成功
      (deferred:next (lambda () t))
    ;; 否则，尝试初始化
    (deferred:$
      (deferred:try
        (deferred:$
          (deferred:next
            (lambda ()
              (message "System not initialized, initializing...")
              ;; 添加初始化超时保护
              (let ((start-time (current-time)))
                (condition-case err
                    (progn
                      (org-supertag-sim-init)
                      ;; 检查初始化是否超时（10秒）
                      (when (> (float-time (time-subtract (current-time) start-time)) 10)
                        (error "Initialization timeout after 10 seconds")))
                  (error
                   (message "Initialization failed: %s" (error-message-string err))
                   (signal (car err) (cdr err)))))))
          (deferred:nextc it
            (lambda (_)
              ;; 再次检查初始化状态，带超时
              (let ((max-attempts 5)
                    (attempt 0))
                (while (and (< attempt max-attempts)
                            (not (and org-supertag-sim--initialized
                                      (org-supertag-sim-epc-server-alive-p)
                                      (org-supertag-sim-epc-manager-live-p))))
                  (sit-for 0.5)
                  (setq attempt (1+ attempt)))
                
                (if (and org-supertag-sim--initialized
                         (org-supertag-sim-epc-server-alive-p)
                         (org-supertag-sim-epc-manager-live-p))
                    t
                  (error "System initialization failed: EPC server not ready after %d attempts" max-attempts))))))
        :catch
        (lambda (err)
          (message "Initialization error: %s" (error-message-string err))
          ;; 重置初始化状态，避免无限循环
          (setq org-supertag-sim--initialized nil)
          nil)))))

(defun org-supertag-sim-find-similar (tag-name &optional top-k callback)
  "Find tags similar to the specified tag.
TAG-NAME is the tag name.
TOP-K is the number of similar tags to return, default is 5.
CALLBACK is an optional callback function that receives the result as a parameter."
  (let ((buffer (current-buffer))
        (limit (or top-k 5)))
    (message "Searching for similar tags for '%s'..." tag-name)
    (deferred:$
      (deferred:try
        (deferred:$
          (org-supertag-sim--ensure-initialized)
          (deferred:nextc it
            (lambda (_)
              (message "Calling EPC method 'find_similar' with tag '%s' and limit %d" tag-name limit)
              (epc:call-deferred org-supertag-sim-epc-manager
                                'find_similar
                                (list tag-name limit))))
          (deferred:nextc it
            (lambda (response)
              (message "Received response: %S" response)
              (let ((status (plist-get response :status))
                    (result (plist-get response :result)))
                (message "Status: %s, Result type: %s, Result length: %d" 
                         status 
                         (type-of result)
                         (if (listp result) (length result) 0))
                (if (string= status "success")
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        ;; 简化处理 - 直接将[tag, score]转换为(tag . score)
                        (let ((formatted-result
                               (mapcar (lambda (item)
                                         (if (listp item)
                                             (cons (nth 0 item) (float (nth 1 item)))
                                           (cons (format "%s" item) 1.0)))
                                     result)))
                          (message "Formatted %d results: %S" (length formatted-result) formatted-result)
                          (if callback
                              (progn
                                (message "Calling callback with %d results" (length formatted-result))
                                (message "Callback type: %s" (type-of callback))
                                (condition-case err
                                    (funcall callback formatted-result)
                                  (error
                                   (message "Error in callback: %s" (error-message-string err))
                                   (message "Callback error stack: %s" 
                                            (with-output-to-string (backtrace))))))
                            ;; Display results only when there is no callback
                            (message "Found %d similar tags: %S" 
                                     (length formatted-result)
                                     (mapcar #'car formatted-result)))
                          formatted-result)))
                  (error "Failed to find similar tags: %s" 
                         (or (plist-get response :message) "Unknown error")))))))
        :catch
        (lambda (err)
          (message "Failed to find similar tags: %s" (error-message-string err))
          (message "Error stack: %s" (with-output-to-string (backtrace)))
          nil)))))

(defun org-supertag-sim-search-tags (query-tags &optional weights callback)
  "Search for tag combinations.
QUERY-TAGS is the query tag list.
WEIGHTS is the weight list.
CALLBACK is an optional callback function that receives the result as a parameter."
  (org-supertag-sim--ensure-initialized)
  (let ((buffer (current-buffer)))
    (message "Searching for tag combinations...")
    (condition-case err
        (epc:call-deferred org-supertag-sim-epc-manager
                          'search_tags
                          (list query-tags weights))
      (deferred:nextc it
        (lambda (result)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (if callback
                  (funcall callback result)
                (message "Found %d related tags" (length result)))))))
      (error
       (message "Failed to search tags: %s" (error-message-string err))
       nil))))

(defun org-supertag-sim-extract-entities (text &optional callback)
  "Extract named entities from TEXT asynchronously.
Optional CALLBACK will be called with the results."
  (org-supertag-sim--ensure-initialized)
  (let ((buffer (current-buffer)))
    (message "Analyzing entities...")
    (condition-case err
        (epc:call-deferred org-supertag-sim-epc-manager
                          'extract_entities
                          (list text))
      (deferred:nextc it
        (lambda (result)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (if callback
                  (funcall callback result)
                ;; 默认处理
                (if result
                    (progn
                      (message "Found %d entities:" (length result))
                      (dolist (entity result)
                        (let ((entity-text (cdr (assoc 'entity entity)))
                              (type (cdr (assoc 'type entity)))
                              (start (cdr (assoc 'start entity)))
                              (end (cdr (assoc 'end entity))))
                          (message "  %s [%s] (%d-%d)" entity-text type start end))))
                  (message "No entities found")))))))
      (error
       (message "Failed to extract entities: %s" (error-message-string err))
       nil))))

(defun org-supertag-sim-suggest-tags-from-text (text &optional callback)
  "According to the text content, generate tag suggestions.
Optional CALLBACK will be called with the results."
  (let ((buffer (current-buffer)))
    (message "Analyzing text content...")
    
    ;; Build JSON request data
    (let ((request-data (list :content text)))
      
      ;; Record request information
      (message "Text length: %d" (length text))
      
      (deferred:$
        (deferred:try
          (deferred:$
            ;; Ensure the system is initialized first
            (org-supertag-sim--ensure-initialized)
            (deferred:nextc it
              (lambda (_)
                ;; Ensure the text is not empty
                (if (string-empty-p text)
                    (error "Text content is empty")
                  ;; Use JSON format to send the request
                  (epc:call-deferred org-supertag-sim-epc-manager
                                  'suggest_tags_json
                                  (list (json-encode request-data))))))
            (deferred:nextc it
              (lambda (response)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (let ((status (plist-get response :status))
                          (result (plist-get response :result))
                          (error-msg (plist-get response :message)))
                      (cond
                       ;; Success case
                       ((string= status "success")
                        (if callback
                            (funcall callback result)
                          (message "Found %d related tags" (length result))))
                       ;; Error case
                       (t
                        (message "Failed to generate tag suggestions: %s" 
                                 (or error-msg "Unknown error"))
                        (if callback (funcall callback nil)))))))))
          :catch
          (lambda (err)
            (message "Failed to generate tag suggestions: %s" (error-message-string err))
            (when callback (funcall callback nil)))))))))

(defun org-supertag-sim-toggle-tag-selection ()
  "Toggle the selection state of the current tag."
  (interactive)
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (when (looking-at "^\\([[:space:]]*\\)\\(\\[[ X]\\]\\) \\(.*\\)$")
      (let* ((tag-name (match-string-no-properties 3))
             (current-state (match-string-no-properties 2))
             (new-state (if (string= current-state "[X]") "[ ]" "[X]")))
        (replace-match (concat (match-string 1) new-state " " tag-name))
        (forward-line 1)))))

(defun org-supertag-sim-apply-selected-tags ()
  "Apply all selected tags to the current node.
This function will:
1. Collect selected tags from the selection buffer
2. Ensure each tag exists (create if necessary)
3. Apply tags through org-supertag system to establish proper relationships
4. Record Co-occurrence relationships between the selected tags
5. Insert tags visually in inline format"
  (interactive)
  (let* ((context (buffer-local-value 'org-supertag-sim--select-context (current-buffer)))
         (source-buffer (plist-get context :source-buffer))
         (node-id (plist-get context :node-id))
         (selected-tags '())
         (tag-ids '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:space:]]*\\)\\(\\[X\\]\\) \\(.*\\)$" nil t)
        (push (match-string-no-properties 3) selected-tags)))
    
    (if (null selected-tags)
        (message "No tags selected")
      ;; Apply tags through org-supertag system
      (message "Applying %d selected tags..." (length selected-tags))
      (with-current-buffer source-buffer
        (save-excursion
          ;; 1. Ensure each tag exists and get their IDs
          (dolist (tag-name selected-tags)
            (condition-case err
                (let* ((sanitized-name (org-supertag-sanitize-tag-name tag-name))
                       (tag-id (if (org-supertag-tag-exists-p sanitized-name)
                                  sanitized-name
                                (org-supertag-tag-create sanitized-name))))
                  (when tag-id
                    (push tag-id tag-ids)))
              (error
               (message "Error creating tag '%s': %s" tag-name (error-message-string err)))))
          
          ;; 2. Apply each tag through org-supertag system
          (when tag-ids
            (dolist (tag-id tag-ids)
              (condition-case err
                  (let ((org-supertag-tag-apply-skip-headline t)
                        (org-supertag-force-node-id node-id))
                    (org-supertag-tag-apply tag-id))
                (error
                 (message "Error applying tag '%s': %s" tag-id (error-message-string err)))))
            
            ;; 3. Move to correct position and insert visual representation
            (org-back-to-heading t)
            (org-end-of-meta-data t)
            
            ;; Ensure we are at the beginning of a line
            (unless (bolp)
              (insert "\n"))
            
            ;; Insert all tags on the same line with proper spacing
            (let ((tag-line (mapconcat (lambda (tag) (concat "#" tag)) selected-tags " ")))
              (insert tag-line)
              ;; Add a newline after the tags to maintain proper formatting
              (insert "\n"))
            
            (message "Successfully applied %d tags: %s" 
                     (length selected-tags)
                     (mapconcat (lambda (tag)
                                (propertize (concat "#" tag) 'face 'font-lock-keyword-face))
                              selected-tags " ")))))
    (quit-window t))))

(defun org-supertag-sim-cancel-tag-selection ()
  "Cancel the tag selection operation."
  (interactive)
  (message "Tag selection canceled")
  (quit-window t))

(defun org-supertag-sim-select-all-tags ()
  "Select all tags."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:space:]]*\\)\\(\\[[ X]\\]\\) \\(.*\\)$" nil t)
        (replace-match (concat (match-string 1) "[X] " (match-string 3))))
      (message "All tags selected"))))

(defun org-supertag-sim-unselect-all-tags ()
  "Unselect all tags."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:space:]]*\\)\\(\\[[ X]\\]\\) \\(.*\\)$" nil t)
        (replace-match (concat (match-string 1) "[ ] " (match-string 3))))
      (message "All tags unselected"))))

(defun org-supertag-sim-auto-tag-node ()
  "According to the content of the current node, automatically suggest and apply tags.
Use semantic analysis to extract relevant tags from the node content, and display them in a dedicated buffer,
allowing users to select and apply the tags they need by pressing the space key."
  (interactive)
  (require 'org-supertag-inline)

  (unless (and (boundp 'org-supertag-sim-epc-manager)
               (boundp 'org-supertag-sim--initialized))
    (user-error "Similarity system not loaded. Please ensure org-supertag-sim is properly initialized"))
  
  (when (and org-supertag-sim-epc-manager
             (not (org-supertag-sim-epc-server-alive-p)))
    (user-error "EPC server is not running. Please restart with `org-supertag-sim-epc-emergency-restart`"))
  
  (let* ((node-title (org-get-heading t t t t))
         (content (org-get-entry))
         (node-id (org-id-get-create))
         ;; Combine title and content for better semantic understanding
         (full-text (concat node-title "\n\n" content))
         (progress-reporter (make-progress-reporter "Analyzing content..." 0 100)))
    
    (when (> (length full-text) 10000)
      (when (not (yes-or-no-p "Content is very long (%d characters). Continue? " (length full-text)))
        (user-error "Analysis cancelled by user")))
    
    ;; Display initial progress
    (progress-reporter-update progress-reporter 10)
    
    ;; Temporarily display a prompt
    (run-with-timer 0.5 nil (lambda () 
                             (progress-reporter-update progress-reporter 30)
                             (message "Generating tag suggestions...")))
    
    (let ((timeout-timer nil)
          (callback-called nil))
      (setq timeout-timer
            (run-with-timer 60 nil
                           (lambda ()
                             (unless callback-called
                               (setq callback-called t)
                               (progress-reporter-done progress-reporter)
                               (message "Tag suggestion timeout. Please check EPC server status.")))))
      
      ;; Tag generation is asynchronous
      (org-supertag-sim-suggest-tags-from-text
       full-text
       (lambda (suggested-tags)
         (unless callback-called
           (setq callback-called t)
           (when timeout-timer
             (cancel-timer timeout-timer))
           ;; Update progress
           (progress-reporter-update progress-reporter 80)
           (progress-reporter-done progress-reporter)
           (if suggested-tags
               ;; Display the tag selection interface in a dedicated buffer
               (let* ((select-buffer (get-buffer-create "*Org SuperTag Selection*"))
                      (source-buffer (current-buffer)))
                 (with-current-buffer select-buffer
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (org-supertag-sim-tag-select-mode)
                     
                     ;; Store context information
                     (setq-local org-supertag-sim--select-context
                                 (list :node-id node-id
                                       :node-title node-title
                                       :source-buffer source-buffer))

                     (insert (format "Suggested %d tags for the node:\n" (length suggested-tags)))
                     (insert "──────────────────────────────────────────────\n")
                     (insert (format "Node: %s\n\n" node-title))
                     (dolist (tag suggested-tags)
                       (insert (format "[ ] %s\n" tag)))
                     (insert "\nOperation instructions:\n")
                     (insert "Space/Enter: Select/Unselect tag  n/p: Move up/down\n")
                     (insert "a: Select all  A: Unselect all\n")
                     (insert "C-c C-c: Apply selected tags  C-c C-k/q: Cancel\n")))
                 (select-window 
                  (display-buffer select-buffer
                                  '((display-buffer-below-selected)
                                    (window-height . fit-window-to-buffer)
                                    (preserve-size . (nil . t))
                                    (select . t))))
                 (with-current-buffer select-buffer
                   (goto-char (point-min))
                   (re-search-forward "^\\[ \\]" nil t)
                   (beginning-of-line)))
             (message "No suitable tag suggestions found"))))))))

(defun org-supertag-sim-regenerate-tags ()
  "Regenerate tag suggestions for the current node."
  (interactive)
  (let* ((context (buffer-local-value 'org-supertag-sim--select-context (current-buffer)))
         (source-buffer (plist-get context :source-buffer))
         (select-buffer (current-buffer)))
    (when source-buffer
      ;; First close the current selection buffer
      (quit-window)
      ;; Then regenerate tags in the source buffer
      (with-current-buffer source-buffer
        (org-supertag-sim-auto-tag-node)))))


(defun org-supertag-sim-safe-auto-tag-node ()
  "Safe version of org-supertag-sim-auto-tag-node with comprehensive error handling.
This function includes all the safety checks and will not cause Emacs to freeze."
  (interactive)
  
  ;; Comprehensive pre-flight checks
  (cond
   ;; Check if similarity system is even loaded
   ((not (featurep 'org-supertag-sim-epc))
    (user-error "Similarity system not loaded. Please ensure org-supertag-sim is installed and loaded"))
   
   ;; Check if we're in an org buffer
   ((not (derived-mode-p 'org-mode))
    (user-error "This command can only be used in org-mode buffers"))
   
   ;; Check if we're in a heading
   ((not (org-at-heading-p))
    (user-error "Please position the cursor on an org heading"))
   
   ;; Check system state
   ((not org-supertag-sim--initialized)
    (when (yes-or-no-p "Similarity system not initialized. Initialize now? ")
      (org-supertag-sim-init))
    (unless org-supertag-sim--initialized
      (user-error "System initialization failed")))
   
   ;; Check EPC server
   ((not (org-supertag-sim-epc-server-alive-p))
    (when (yes-or-no-p "EPC server not running. Start it now? ")
      (org-supertag-sim-epc-restart)
      (sit-for 2))
    (unless (org-supertag-sim-epc-server-alive-p)
      (user-error "EPC server failed to start")))
   
   ;; All checks passed, proceed with tag suggestion
   (t
    (message "All checks passed. Running safe tag suggestion...")
    (org-supertag-sim-auto-tag-node))))

(defun org-supertag-sim-force-sync-library ()
  "Force synchronization of the tag vector library.
This function will manually trigger the synchronization process
to ensure that all tags in the database are properly vectorized."
  (interactive)
  
  ;; First check if the system is initialized
  (if (not org-supertag-sim--initialized)
      (when (yes-or-no-p "Similarity system not initialized. Initialize now? ")
        (org-supertag-sim-init)))
  
  ;; Now check if EPC server is running
  (if (not (org-supertag-sim-epc-server-alive-p))
      (when (yes-or-no-p "EPC server not running. Start it now? ")
        (org-supertag-sim-epc-restart)
        (sit-for 2)))
  
  ;; Verify system is ready
  (unless (and org-supertag-sim--initialized (org-supertag-sim-epc-server-alive-p))
    (user-error "System is not ready. Please initialize it first"))
  
  ;; Trigger synchronization
  (message "Forcing tag vector library synchronization...")
  (deferred:$
    (org-supertag-sim--sync-library)
    (deferred:nextc it
      (lambda (result)
        (let ((status (plist-get result :status))
              (data (plist-get result :result)))
          (if (string= status "success")
              (progn
                (message "Tag vector library synchronized successfully: %s"
                         (format "Total: %d, Added: %d, Updated: %d, Removed: %d"
                                 (cdr (assoc 'total_processed data))
                                 (cdr (assoc 'added data))
                                 (cdr (assoc 'updated data))
                                 (cdr (assoc 'removed data))))
                data)
            (message "Synchronization failed: %s" (plist-get result :message))
            nil)))))
  t)

(defun org-supertag-sim-debug-find-similar (tag-name)
  "Debug function to directly test finding similar tags for TAG-NAME.
This bypasses the callback mechanism and directly logs the results."
  (interactive 
   (list (completing-read "Tag name: " (org-supertag-get-all-tags) nil t)))
  (message "Directly testing find_similar for tag: %s" tag-name)
  
  ;; Make sure the system is initialized
  (unless org-supertag-sim--initialized
    (message "Initializing system first...")
    (org-supertag-sim-init))
  
  ;; Directly call the EPC function and log the result
  (deferred:$
    (deferred:try
      (deferred:$
        (epc:call-deferred org-supertag-sim-epc-manager
                         'find_similar
                         (list tag-name 10))
        (deferred:nextc it
          (lambda (response)
            (message "Direct EPC call result:")
            (message "Response: %S" response)
            (let ((status (plist-get response :status))
                  (result (plist-get response :result)))
              (message "Status: %s" status)
              (message "Result: %S" result)
              
              ;; Process each result item
              (when (and (string= status "success") result)
                (dolist (item result)
                  (message "Item: %S (type: %s)" item (type-of item))))
              
              ;; Add to recommendations cache directly for testing
              (when (and (string= status "success") result)
                (message "Adding results directly to recommendations cache")
                (let* ((tag-id (org-supertag-tag-get-id-by-name tag-name))
                       (formatted-result
                        (mapcar (lambda (item)
                                  (cond
                                   ;; Format: [tag score]
                                   ((and (listp item) (= (length item) 2))
                                    (cons (nth 0 item) (float (nth 1 item))))
                                   ;; Other formats
                                   (t (cons (format "%s" (if (listp item) (car item) item)) 1.0))))
                                result)))
                  (when tag-id
                    (puthash tag-id formatted-result org-supertag-relation--recommendations-cache)
                    (message "Recommendations cached: %S" formatted-result))))
              response))))
      :catch
      (lambda (err)
        (message "Error in direct EPC call: %s" (error-message-string err))
        nil))))

(defun org-supertag-sim-quick-test (tag-name)
  "Run a quick direct test of the similar tags functionality.
This function bypasses most of the complexity and directly creates test results."
  (interactive
   (list (completing-read "Tag name: " (org-supertag-get-all-tags) nil t)))
  
  (message "Running quick test for %s" tag-name)
  
  ;; Create mock results
  (let* ((test-results (list 
                        (list "test_tag_1" 0.95)
                        (list "test_tag_2" 0.85)
                        (list "test_tag_3" 0.75)
                        (list "test_tag_4" 0.65)
                        (list "test_tag_5" 0.55)))
         (formatted-results
          (mapcar (lambda (item) (cons (car item) (cadr item))) test-results))
         (tag-id (org-supertag-tag-get-id-by-name tag-name)))
    
    ;; Store in cache
    (when tag-id
      (message "Storing test results in cache for %s" tag-id)
      (puthash tag-id formatted-results org-supertag-relation--recommendations-cache)
      
      ;; Open relation management if needed
      (unless (get-buffer org-supertag-relation-manage--buffer-name)
        (org-supertag-relation--show-management-interface tag-id))
      
      ;; Force refresh
      (with-current-buffer org-supertag-relation-manage--buffer-name
        (when (eq org-supertag-relation--current-tag tag-id)
          (let ((inhibit-read-only t))
            (message "Forcing display refresh")
            (org-supertag-relation--refresh-display)))))
    
    ;; Return the formatted results
    formatted-results))

(defun org-supertag-sim-test-async-epc (tag-name)
  "Test EPC communication asynchronously without timeout issues."
  (interactive
   (list (completing-read "Tag name: " (org-supertag-get-all-tags) nil t)))
  
  (message "Testing async EPC call for '%s'..." tag-name)
  
  ;; Make sure we're initialized
  (unless org-supertag-sim--initialized
    (org-supertag-sim-init))
  
  ;; Make direct async call using deferred API
  (deferred:$
    (deferred:try
      (deferred:$
        (epc:call-deferred org-supertag-sim-epc-manager
                          'find_similar
                          (list tag-name 10))
        (deferred:nextc it
          (lambda (response)
            (message "✅ Async EPC call successful!")
            (message "Response: %S" response)
            
            ;; Process the response if successful
            (let ((status (plist-get response :status))
                  (result (plist-get response :result)))
              (if (string= status "success")
                  (progn
                    (message "Found %d similar tags" (length result))
                    ;; Store in cache for testing
                    (let* ((tag-id (org-supertag-tag-get-id-by-name tag-name))
                           (formatted-result
                            (mapcar (lambda (item)
                                      (cons (nth 0 item) (float (nth 1 item))))
                                    result)))
                      (when tag-id
                        (puthash tag-id formatted-result 
                                org-supertag-relation--recommendations-cache)
                        (message "Results cached for tag %s" tag-id)
                        
                        ;; Trigger UI refresh if relation management is open
                        (when (get-buffer org-supertag-relation-manage--buffer-name)
                          (with-current-buffer org-supertag-relation-manage--buffer-name
                            (when (eq org-supertag-relation--current-tag tag-id)
                              (let ((inhibit-read-only t))
                                (org-supertag-relation--refresh-display))))))))
                (message "❌ Server returned error: %s" 
                         (plist-get response :message))))
            response))))
      :catch
      (lambda (err)
        (message "❌ Async EPC call failed: %s" (error-message-string err))
        nil)))

(defun org-supertag-sim-test-safe-echo ()
  "Test EPC echo/ping safely to avoid hanging."
  (interactive)
  (message "Testing safe EPC ping (synchronous call)...")

  (unless (org-supertag-sim-epc-manager-live-p)
    (message "EPC manager not live, attempting to initialize...")
    (org-supertag-sim-init) ;; This should start/connect to the server
    (sit-for 2) ;; Give it a moment to initialize
    (unless (org-supertag-sim-epc-manager-live-p)
      (message "❌ EPC manager still not live after init attempt.")
      (error "EPC manager not live")))

  (condition-case err
      (let ((result (org-supertag-sim-epc-call-method 'ping)))
        (message "✅ EPC Ping call raw result: %S" result)
        ;; Check if the result is a plist and has the expected structure
        (if (and (listp result) (plist-get result :status))
            (let ((status (plist-get result :status))
                  (response-data (plist-get result :result)))
              (if (string= status "success")
                  (message "✅ Ping successful: %s" response-data)
                (message "❌ Ping failed (server-side): %s" (plist-get result :message))))
          (message "ℹ️ Ping response was not in expected plist format: %S" result)))
    (error
     (message "❌ EPC Ping call failed (Lisp error): %S" err)
     (signal (car err) (cdr err)))))

(provide 'org-supertag-sim)

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
  (org-supertag-data-file "tag_vectors.json")
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
    
    (let ((tag-data
           (delq nil
                 (mapcar (lambda (tag)
                          (when-let* ((props (org-supertag-db-get tag)))
                            ;; Only keep tag ID and name
                            (list
                             (cons "id" tag)
                             (cons "name" (plist-get props :name)))))
                        tags))))
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
        (unless (org-supertag-sim-epc-server-running-p)
          (org-supertag-sim-epc-start-server)
          (sit-for 1))
        (when (org-supertag-sim-epc-server-running-p)
          (org-supertag-sim-epc-init)

          (unless org-supertag-sim-epc-initialized
            (error "EPC service initialization failed"))
          (org-supertag-db-on 'entity:created #'org-supertag-sim--on-tag-created)
          (org-supertag-db-on 'entity:removed #'org-supertag-sim--on-tag-removed)
          (org-supertag-sim--start-sync-timer)
          (setq org-supertag-sim--initialized t)
          (message "Tag similarity system initialized")))
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
                            (list (list (cons "id" tag)
                                      (cons "name" (plist-get tag-props :name)))))
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
      (condition-case err
          (epc:call-deferred org-supertag-sim-epc-manager
                            'sync_library
                            (list db-file tag-data))
        (error
         (message "Sync tag library error: %s" (error-message-string err)))))))

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
  (if (and org-supertag-sim--initialized org-supertag-sim-epc-initialized)
      ;; If both systems are initialized, return success directly
      (deferred:next (lambda () t))
    ;; Otherwise, try to initialize
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
                      ;; 检查初始化是否超时（30秒）
                      (when (> (float-time (time-subtract (current-time) start-time)) 30)
                        (error "Initialization timeout after 30 seconds")))
                  (error
                   (message "Initialization failed: %s" (error-message-string err))
                   (signal (car err) (cdr err)))))))
          (deferred:nextc it
            (lambda (_)
              ;; Check initialization status again with timeout
              (let ((max-attempts 10)
                    (attempt 0))
                (while (and (< attempt max-attempts)
                           (not (and org-supertag-sim--initialized org-supertag-sim-epc-initialized)))
                  (sit-for 0.5)
                  (setq attempt (1+ attempt)))
                
                (if (and org-supertag-sim--initialized org-supertag-sim-epc-initialized)
                    t
                  (error "System initialization failed: EPC server not ready after %d attempts" max-attempts))))))
        :catch
        (lambda (err)
          (message "Initialization error: %s" (error-message-string err))
          ;; 重置初始化状态，避免无限循环
          (setq org-supertag-sim--initialized nil
                org-supertag-sim-epc-initialized nil)
          nil)))))

(defun org-supertag-sim-find-similar (tag-name &optional top-k callback)
  "Find tags similar to the specified tag.
TAG-NAME is the tag name.
TOP-K is the number of similar tags to return, default is 5.
CALLBACK is an optional callback function that receives the result as a parameter."
  (let ((buffer (current-buffer))
        (limit (or top-k 5)))
    (message "Searching for similar tags...")
    (deferred:$
      (deferred:try
        (deferred:$
          (org-supertag-sim--ensure-initialized)
          (deferred:nextc it
            (lambda (_)
              (epc:call-deferred org-supertag-sim-epc-manager
                                'find_similar
                                (list tag-name limit))))
          (deferred:nextc it
            (lambda (response)
              (let ((status (plist-get response :status))
                    (result (plist-get response :result)))
                (if (string= status "success")
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (let ((formatted-result
                               (mapcar (lambda (item)
                                       (cons (car item)
                                             (float (if (listp (cdr item))
                                                      (car (cdr item))
                                                    (cdr item)))))
                                     result)))
                          (if callback
                              (funcall callback formatted-result)
                            ;; Display results only when there is no callback
                            (message "Found %d similar tags" (length formatted-result)))
                          formatted-result)))
                  (error "Failed to find similar tags: %s" 
                         (or (plist-get response :message) "Unknown error")))))))
        :catch
        (lambda (err)
          (message "Failed to find similar tags: %s" (error-message-string err))
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
2. Move to the correct position in the source buffer (after properties drawer)
3. Insert tags in inline format (#tag) on a new line after the properties drawer
4. Ensure proper line formatting and spacing"
  (interactive)
  (let* ((context (buffer-local-value 'org-supertag-sim--select-context (current-buffer)))
         (source-buffer (plist-get context :source-buffer))
         (node-id (plist-get context :node-id))
         (selected-tags '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:space:]]*\\)\\(\\[X\\]\\) \\(.*\\)$" nil t)
        (push (match-string-no-properties 3) selected-tags)))
    (if (null selected-tags)
        (message "No tags selected")
      ;; Apply tags
      (message "Applying %d selected tags..." (length selected-tags))
      (with-current-buffer source-buffer
        (save-excursion
          ;; 1. Move to the correct position - after the properties drawer
          (org-back-to-heading t)
          (org-end-of-meta-data t)
          
          ;; 2. Ensure we are at the beginning of a line
          (unless (bolp)
            (insert "\n"))
          
          ;; 3. Insert all tags on the same line with proper spacing
          (let ((tag-line (mapconcat (lambda (tag) (concat "#" tag)) selected-tags " ")))
            (insert tag-line)
            ;; Add a newline after the tags to maintain proper formatting
            (insert "\n"))
          
          (message "Successfully applied %d tags: %s" 
                   (length selected-tags)
                   (mapconcat (lambda (tag)
                              (propertize (concat "#" tag) 'face 'font-lock-keyword-face))
                            selected-tags " ")))))
    (quit-window t)))

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
             (not (org-supertag-sim-epc-server-running-p)))
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

(defun org-supertag-sim-diagnose ()
  "Diagnose the current state of the similarity system.
Provides detailed information about system status and potential issues."
  (interactive)
  (let ((result-buffer (get-buffer-create "*Org SuperTag Sim Diagnosis*")))
    (with-current-buffer result-buffer
      (erase-buffer)
      (insert "=== Org SuperTag Similarity System Diagnosis ===\n\n")
      
      ;; Check basic system state
      (insert "1. Basic System State:\n")
      (insert (format "   - org-supertag-sim--initialized: %s\n" 
                     (if (boundp 'org-supertag-sim--initialized) 
                         org-supertag-sim--initialized 
                         "UNBOUND")))
      (insert (format "   - org-supertag-sim-epc-initialized: %s\n" 
                     (if (boundp 'org-supertag-sim-epc-initialized) 
                         org-supertag-sim-epc-initialized 
                         "UNBOUND")))
      (insert (format "   - org-supertag-sim-epc-manager: %s\n" 
                     (if (boundp 'org-supertag-sim-epc-manager) 
                         (if org-supertag-sim-epc-manager "EXISTS" "NIL")
                         "UNBOUND")))
      
      ;; Check EPC server status
      (insert "\n2. EPC Server Status:\n")
      (condition-case err
          (if (and (boundp 'org-supertag-sim-epc-manager)
                   org-supertag-sim-epc-manager)
              (if (org-supertag-sim-epc-server-running-p)
                  (insert "   - Server Status: RUNNING\n")
                (insert "   - Server Status: STOPPED\n"))
            (insert "   - Server Status: NOT INITIALIZED\n"))
        (error 
         (insert (format "   - Server Status: ERROR - %s\n" (error-message-string err)))))
      
      ;; Check Python environment
      (insert "\n3. Python Environment:\n")
      (condition-case err
          (let ((python-path (if (boundp 'org-supertag-sim-epc-python-path)
                                org-supertag-sim-epc-python-path
                               "UNSET")))
            (insert (format "   - Python Path: %s\n" python-path))
            (insert (format "   - Python Executable Exists: %s\n" 
                           (if (and (stringp python-path) (executable-find python-path))
                               "YES" "NO"))))
        (error 
         (insert (format "   - Python Environment: ERROR - %s\n" (error-message-string err)))))
      
      ;; Check dependencies
      (insert "\n4. Dependencies:\n")
      (insert (format "   - epc package loaded: %s\n" (if (featurep 'epc) "YES" "NO")))
      (insert (format "   - deferred package available: %s\n" (if (fboundp 'deferred:$) "YES" "NO")))
      (insert (format "   - json package available: %s\n" (if (fboundp 'json-encode) "YES" "NO")))
      
      ;; Provide recommendations
      (insert "\n5. Recommendations:\n")
      (cond
       ;; Everything seems fine
       ((and (boundp 'org-supertag-sim--initialized) org-supertag-sim--initialized
             (boundp 'org-supertag-sim-epc-initialized) org-supertag-sim-epc-initialized
             (org-supertag-sim-epc-server-running-p))
        (insert "   ✓ System appears to be working correctly\n"))
       
       ;; EPC server issues
       ((and (boundp 'org-supertag-sim-epc-manager) org-supertag-sim-epc-manager
             (not (org-supertag-sim-epc-server-running-p)))
        (insert "   ⚠ EPC server is not running\n")
        (insert "   → Try: M-x org-supertag-sim-epc-emergency-restart\n"))
       
       ;; Initialization issues
       ((or (not (boundp 'org-supertag-sim--initialized))
            (not org-supertag-sim--initialized))
        (insert "   ⚠ System not initialized\n")
        (insert "   → Try: M-x org-supertag-sim-emergency-reset\n"))
       
       ;; General issues
       (t
        (insert "   ⚠ Multiple issues detected\n")
        (insert "   → Try: M-x org-supertag-sim-emergency-reset\n")))
      
      (insert "\n6. Emergency Commands:\n")
      (insert "   - M-x org-supertag-sim-emergency-reset    : Complete system reset\n")
      (insert "   - M-x org-supertag-sim-epc-emergency-restart : Restart EPC server\n")
      (insert "   - M-x org-supertag-sim-epc-force-kill     : Force kill all processes\n")
      (insert "   - M-x org-supertag-sim-epc-show-log       : View detailed logs\n")
      
      (special-mode))
    (display-buffer result-buffer)))

(defun org-supertag-sim-emergency-reset ()
  "Emergency reset of the entire similarity system.
This function will:
1. Stop all running processes
2. Reset all state variables
3. Restart the system from scratch"
  (interactive)
  (when (yes-or-no-p "This will reset the entire similarity system. Continue? ")
    (message "Performing emergency reset...")
    
    ;; Step 1: Stop everything
    (condition-case err
        (progn
          ;; Cancel any running timers
          (when (and (boundp 'org-supertag-sim--sync-timer) org-supertag-sim--sync-timer)
            (cancel-timer org-supertag-sim--sync-timer)
            (setq org-supertag-sim--sync-timer nil))
          
          ;; Stop EPC server
          (when (featurep 'org-supertag-sim-epc)
            (org-supertag-sim-epc-force-kill))
          
          ;; Reset state variables
          (setq org-supertag-sim--initialized nil)
          (when (boundp 'org-supertag-sim-epc-initialized)
            (setq org-supertag-sim-epc-initialized nil))
          (when (boundp 'org-supertag-sim-epc-manager)
            (setq org-supertag-sim-epc-manager nil))
          
          (message "System state reset completed"))
      (error
       (message "Error during reset: %s" (error-message-string err))))
    
    ;; Step 2: Wait and restart
    (sit-for 2)
    (message "Restarting similarity system...")
    
    (condition-case err
        (progn
          ;; Try to reinitialize
          (org-supertag-sim-init)
          (message "Emergency reset completed successfully"))
      (error
       (message "Error during restart: %s" (error-message-string err))
       (message "You may need to restart Emacs completely")))))

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
   ((not (and (boundp 'org-supertag-sim--initialized)
              (boundp 'org-supertag-sim-epc-initialized)))
    (when (yes-or-no-p "Similarity system not initialized. Initialize now? ")
      (org-supertag-sim-emergency-reset))
    (unless (and org-supertag-sim--initialized org-supertag-sim-epc-initialized)
      (user-error "System initialization failed")))
   
   ;; Check EPC server
   ((not (org-supertag-sim-epc-server-running-p))
    (when (yes-or-no-p "EPC server not running. Start it now? ")
      (org-supertag-sim-epc-emergency-restart)
      (sit-for 2))
    (unless (org-supertag-sim-epc-server-running-p)
      (user-error "EPC server failed to start")))
   
   ;; All checks passed, proceed with tag suggestion
   (t
    (message "All checks passed. Running safe tag suggestion...")
    (org-supertag-sim-auto-tag-node))))

(provide 'org-supertag-sim)

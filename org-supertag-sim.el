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
              (org-supertag-sim-init)))
          (deferred:nextc it
            (lambda (_)
              ;; Check initialization status again
              (if (and org-supertag-sim--initialized org-supertag-sim-epc-initialized)
                  t
                (error "System initialization failed: EPC server not ready")))))
        :catch
        (lambda (err)
          (message "Initialization error: %s" (error-message-string err))
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
2. Move to the correct position in the source buffer
3. Insert tags in inline format (#tag) after the properties drawer
4. Ensure proper spacing between tags"
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
          ;; 1. Move to the correct position
          (org-back-to-heading t)
          (org-end-of-meta-data t)
          ;; 2. Insert tags with proper spacing
          (dolist (tag selected-tags)
            (insert "#" tag " "))
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
  (let* ((node-title (org-get-heading t t t t))
         (content (org-get-entry))
         (node-id (org-id-get-create))
         ;; Combine title and content for better semantic understanding
         (full-text (concat node-title "\n\n" content))
         (progress-reporter (make-progress-reporter "Analyzing content..." 0 100)))
    
    ;; Display initial progress
    (progress-reporter-update progress-reporter 10)
    
    ;; Temporarily display a prompt
    (run-with-timer 0.5 nil (lambda () 
                             (progress-reporter-update progress-reporter 30)
                             (message "Generating tag suggestions...")))
    
    ;; Tag generation is asynchronous
    (org-supertag-sim-suggest-tags-from-text
     full-text
     (lambda (suggested-tags)
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
         (message "No suitable tag suggestions found"))))))

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

(provide 'org-supertag-sim)

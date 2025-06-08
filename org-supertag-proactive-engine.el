;;; org-supertag-proactive-engine.el --- Proactive analysis engine for org-supertag -*- lexical-binding: t -*-

(require 'org-supertag-db)
(require 'org-supertag-api)
(require 'org-supertag-ui-conversation)

(defgroup org-supertag-proactive-engine nil
  "Settings for the proactive intelligence engine."
  :group 'org-supertag)

(defcustom org-supertag-proactive-engine-enabled t
  "Enable the proactive engine to analyze content automatically."
  :type 'boolean
  :group 'org-supertag-proactive-engine)

(defcustom org-supertag-proactive-engine-idle-delay 2.5
  "Idle time in seconds before triggering proactive analysis."
  :type 'float
  :group 'org-supertag-proactive-engine)

(defvar org-supertag-proactive-engine--timer nil
  "Timer for proactive analysis.")
(defvar org-supertag-proactive-engine--last-node-id nil
  "The ID of the node the cursor was last in.")
(defvar org-supertag-proactive-engine--last-tick nil
  "The buffer-modified-tick of the last check.")

(defun org-supertag-proactive-engine--trigger-analysis ()
  "Decide which analysis to run based on buffer state."
  (when (and (derived-mode-p 'org-mode)
             org-supertag-proactive-engine--last-node-id)
    (let ((is-modified (and org-supertag-proactive-engine--last-tick
                            (/= (buffer-modified-tick) org-supertag-proactive-engine--last-tick))))
      (if is-modified
          (org-supertag-proactive-engine--run-conceptual-resonance)
        (org-supertag-proactive-engine--run-knowledge-archaeology)))))

(defun org-supertag-proactive-engine--run-conceptual-resonance ()
  "Run conceptual resonance analysis on the current node.
This now involves sending the full node context to the backend for
graph construction, vectorization, and analysis."
  (message "Proactive Engine: Analyzing current node context...")
  (let* ((node-id org-supertag-proactive-engine--last-node-id)
         (node-props (org-supertag-db-get node-id))
         (content (plist-get node-props :content))
         (tags (plist-get node-props :tags)))
    (when content
      (let ((context-data `(:event_type "user_idle"
                           :node_id ,node-id
                           :buffer_content ,content
                           :current_tags ,tags
                           :dialogue_mode "normal" ;; Or get from a variable
                           :timestamp ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))))
        (org-supertag-api-analyze-node-context
         context-data
         (lambda (result)
           (when (and result (plist-get result :insight))
             (let* ((insight (plist-get result :insight))
                    (resonant-title (plist-get result :resonant_note_title))
                    (formatted-text (format "✨ *Context Analyzed*\n\n%s\n\n*Referenced:* `%s`" insight resonant-title)))
               (org-supertag-ui-conversation-add-message "ai" formatted-text)))))))))

(defun org-supertag-proactive-engine--run-knowledge-archaeology ()
  "Run knowledge archaeology on the current node's concept."
  (message "Proactive Engine: Running Knowledge Archaeology...")
  (let* ((node-id org-supertag-proactive-engine--last-node-id)
         (title (plist-get (org-supertag-db-get node-id) :title)))
    (when title
      (org-supertag-api-knowledge-archaeology-dig
       title
       (lambda (result)
         (when result
           (let* ((formatted-list (mapcar (lambda (node)
                                            (format "- *%s* (%s)"
                                                    (or (plist-get node :title) "No Title")
                                                    (format-time-string "%Y-%m-%d" (plist-get node :document_date))))
                                          result))
                  (formatted-text (format " digging into the history of *%s*...\n\n%s"
                                          title
                                          (string-join formatted-list "\n"))))
             (org-supertag-ui-conversation-add-message "ai" formatted-text))))))))

(defun org-supertag-proactive-engine--post-command-hook-function ()
  "Hook function to monitor cursor position and buffer modifications."
  (when (and org-supertag-proactive-engine-enabled
             (derived-mode-p 'org-mode)
             (buffer-file-name)
             (org-supertag--org-file-p (buffer-file-name)))
    (let ((current-node-id (org-id-get)))
      (when (and current-node-id (not (equal current-node-id org-supertag-proactive-engine--last-node-id)))
        (setq org-supertag-proactive-engine--last-node-id current-node-id)
        (setq org-supertag-proactive-engine--last-tick (buffer-modified-tick))
        
        (when org-supertag-proactive-engine--timer
          (cancel-timer org-supertag-proactive-engine--timer))
        
        (setq org-supertag-proactive-engine--timer
              (run-with-idle-timer
               org-supertag-proactive-engine-idle-delay
               nil
               'org-supertag-proactive-engine--trigger-analysis))))))

;;;###autoload
(defun org-supertag-proactive-engine-activate ()
  "Activate the proactive analysis engine."
  (interactive)
  (add-hook 'post-command-hook 'org-supertag-proactive-engine--post-command-hook-function nil t)
  (when (called-interactively-p 'any)
    (message "Org SuperTag Proactive Engine Activated.")))

;;;###autoload
(defun org-supertag-proactive-engine-deactivate ()
  "Deactivate the proactive analysis engine."
  (interactive)
  (remove-hook 'post-command-hook 'org-supertag-proactive-engine--post-command-hook-function)
  (when org-supertag-proactive-engine--timer
    (cancel-timer org-supertag-proactive-engine--timer)
    (setq org-supertag-proactive-engine--timer nil))
  (message "Org SuperTag Proactive Engine Deactivated."))

(defun org-supertag-proactive-engine--analyze-context ()
  "Gather context and send it to the backend for analysis."
  (let* ((node-id (org-id-get))
         (context-data
          `((:node_id . ,node-id)
            (:file_path . ,(buffer-file-name))
            (:buffer_content . ,(buffer-string))
            (:current_tags . ,(org-get-tags))
            (:cursor_pos . ,(point))
            (:title . ,(org-get-heading t t t t)))))
    (message "Proactive Engine: Sending context for node %s to backend..." node-id)
    (org-supertag-api-analyze-node-context
     context-data
     (lambda (result)
       (when (and result (plist-get result :insight))
         (let* ((insight (plist-get result :insight))
                (resonant-title (plist-get result :resonant_note_title))
                (formatted-text (format "✨ *Context Analyzed*\n\n%s\n\n*Referenced:* `%s`" insight resonant-title)))
           (org-supertag-ui-conversation-add-message "ai" formatted-text)))))))

(provide 'org-supertag-proactive-engine) 
;; -*- lexical-binding: t; -*-
;;; org-supertag-view-chat.el --- Chat View for org-supertag

;;; Commentary:
;; Chat View for org-supertag.

;;; Code:

(require 'cl-lib)
(require 'org-supertag-bridge)
(require 'org-supertag-api)
(require 'org-supertag-node)
(require 'org-supertag-db)
(require 'org-supertag-query)

(defcustom org-supertag-view-chat-buffer-name "*Org SuperTag Chat View*"
  "The name of the buffer for the chat view."
  :type 'string
  :group 'org-supertag)

(defcustom org-supertag-view-chat-lang "English"
  "The language used in chat view"
  :type 'string
  :group 'org-supertag)

;; --- Faces ---
(defface org-supertag-chat-label-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for labels like 'Analysis:' or 'User:'."
  :group 'org-supertag)

(defface org-supertag-chat-prompt-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the input prompt '>'."
  :group 'org-supertag)

;; --- Buffer-Local Variables ---
(defvar-local org-supertag-view-chat--conversation-history nil)
;; Marker for where the next response will be inserted / prompt begins
(defvar-local org-supertag-view-chat--response-start-marker nil)
;; Marker for the beginning of current prompt line (char just after '> ')
(defvar-local org-supertag-view-chat--prompt-start nil)
(defvar-local org-supertag-view-chat--current-command nil
  "Current active chat command (e.g. 'tags', 'expand', etc., nil for default mode).")

;; --- Core Functions ---
(defun org-supertag-view-chat--md-to-org (md-string)
  "Convert a markdown string to an org-mode formatted string."
  (if (and md-string (not (string-empty-p md-string)))
      (with-temp-buffer
        (insert md-string)
        (goto-char (point-min))
        
        ;; Basic markdown to org conversions
        
        ;; Headers: # title -> ** title, ## title -> *** title, etc. 
        (while (re-search-forward "^\\(#+\\) " nil t) 
          (let ((header-level (match-string 1)))
            (when header-level 
              (replace-match (concat (make-string (1+ (length header-level)) ?*) " ")))))
        
        ;; Inline code: `code` -> =code= (handle before other transformations)
        (goto-char (point-min))
        (while (re-search-forward "`\\([^`]*?\\)`" nil t)
          (let ((code-text (match-string 1)))
            (when code-text  
              (replace-match (concat "=" code-text "=")))))
        
        ;; Bold: **bold** -> *bold* (handle before single asterisk)
        (goto-char (point-min))
        (while (re-search-forward "\\*\\*\\([^*]+?\\)\\*\\*" nil t) 
          (let ((bold-text (match-string 1)))
            (when bold-text  
              (replace-match (concat "*" bold-text "*")))))
        
        ;; Italics: *italic* -> /italic/ (handle after bold)
        (goto-char (point-min))
        (while (re-search-forward "\\*\\([^*]+?\\)\\*" nil t) 
          (let ((italic-text (match-string 1)))
            (when (and italic-text 
                       (not (string-match "^\\s-*$" italic-text)))  
              (replace-match (concat "/" italic-text "/")))))
        
        ;; Ordered lists: 1. item -> 1. item (preserve numbering)
        (goto-char (point-min))
        (while (re-search-forward "^\\([ \t]*\\)\\([0-9]+\\)\\. \\(.*\\)$" nil t)
          (let ((indent (match-string 1))
                (number (match-string 2))
                (list-text (match-string 3)))
            (when (and indent number list-text)  
              (replace-match (concat indent number ". " list-text)))))
        
        ;; Unordered lists: - item -> - item, * item -> - item, + item -> - item
        (goto-char (point-min))
        (while (re-search-forward "^\\([ \t]*\\)\\([-*+]\\) \\(.*\\)$" nil t)
          (let ((indent (match-string 1))
                (marker (match-string 2))
                (list-text (match-string 3)))
            (when (and indent marker list-text)  
              (replace-match (concat indent "- " list-text)))))
        
        ;; Links: [text](url) -> [[url][text]]
        (goto-char (point-min))
        (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
          (let ((link-text (match-string 1))
                (link-url (match-string 2)))
            (when (and link-text link-url)  
              (replace-match (concat "[[" link-url "][" link-text "]]")))))
        
        ;; Horizontal rules: --- -> -----
        (goto-char (point-min))
        (while (re-search-forward "^\\(-{3,}\\|\\*{3,}\\)$" nil t)
          (replace-match "-----"))
        
        ;; Normalize multiple newlines to prevent excessive spacing
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil t)
          (replace-match "\n\n"))
        
        (buffer-string))
    ""))

;; --- Prompt with command ---
(defun org-supertag-view-chat--insert-prompt ()
  "Insert an org headline as the input prompt at the end of the buffer, showing current command if any."
  (with-current-buffer (get-buffer-create org-supertag-view-chat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (let ((headline
             (if org-supertag-view-chat--current-command
                 (format "* User [%s mode]: " org-supertag-view-chat--current-command)
               "* User: ")))
        (insert (propertize headline 'face 'org-supertag-chat-prompt-face))
        (setq org-supertag-view-chat--prompt-start (point-marker))))))

;; --- Async Callback ---
(defun org-supertag-view-chat--handle-response (result)
  "Handle asynchronous response from the RAG backend.
RESULT is the plist returned from the Python backend."
  (with-current-buffer (get-buffer-create org-supertag-view-chat-buffer-name)
    (let ((inhibit-read-only t)
          (response-content nil)
          (history nil))

      ;; Parse the plist from the handler
      ;; Handle both direct results and wrapped results from _run_async
      (let ((actual-result (if (and (listp result) (plist-get result :result))
                               (plist-get result :result)
                             result)))
        (setq response-content (or (plist-get actual-result :answer) 
                                   (plist-get actual-result :response)
                                   (plist-get actual-result :analysis)))
        (setq history (plist-get actual-result :conversation-history)))

      ;; Go to the response start marker to clear the "Fetching..." message
      (when org-supertag-view-chat--response-start-marker
        (goto-char org-supertag-view-chat--response-start-marker)
        (delete-region (point) (point-max)))

      ;; Insert the content if we have a response
      (when response-content
        (setq org-supertag-view-chat--conversation-history history)
        ;; push assistant message to history
        (push (list :role "assistant" :content response-content)
              org-supertag-view-chat--conversation-history)
        (insert "** Assistant\n")
        (insert (format "%s\n" (org-supertag-view-chat--md-to-org response-content)))
        (insert "\n")

        ;; Context block
        (when-let ((sources (plist-get result :source_nodes)))
          (when (and (listp sources) (> (length sources) 0))
            ;; Deduplicate by id
            (let* ((unique-sources
                    (cl-remove-duplicates sources :key (lambda (s) (plist-get s :id)) :test #'equal)))
              ;; Insert org-mode context block
              (insert "*** Context\n")
              (let ((content-start (point)))
                (dolist (src unique-sources)
                  (let* ((id (plist-get src :id))
                         (title (or (plist-get src :title) "Untitled"))
                         (snippet (or (plist-get src :snippet) "No snippet")))
                    (when (and id title snippet)  
                      (insert (format "- "))
                      (insert-text-button title
                                          'action (lambda (_btn) (org-supertag-view-chat--open-node id))
                                          'follow-link t
                                          'help-echo (format "Open node %s" id)
                                          'face 'org-link)
                      (insert (format ": %s\n" snippet))))))
              ;; Fold the context block at the point
              (save-excursion
                ;; Search for the Context title
                (forward-line -1)
                (while (and (not (looking-at "^\\*\\*\\* Context")) (not (bobp)))
                  (forward-line -1))
                (when (looking-at "^\\*\\*\\* Context")
                  (when (fboundp 'org-fold-hide-subtree)
                    (org-fold-hide-subtree))))))

        ;; Finalize buffer state
        (insert "\n\n")
        (when org-supertag-view-chat--response-start-marker
          (put-text-property org-supertag-view-chat--response-start-marker (point) 'read-only t)))

      ;; Always insert the next prompt
      (org-supertag-view-chat--insert-prompt)))))

(defun org-supertag-view-chat--open-node (id)
  "Corresponding to the node ID, jump to the org buffer and locate the node."
  (interactive "sNode ID: ")
  (when (and id (stringp id)) 
    (let ((marker (org-id-find id t)))
      (if marker
          (progn
            (switch-to-buffer (marker-buffer marker))
            (goto-char marker)
            (org-show-entry)
            (message "Jumped to node %s" id))
        (message "Node %s not found" id)))))

;; --- Sending Logic ---
;; Prompt begins with ">" followed by optional spaces.
(defconst org-supertag-view-chat--prompt-regexp "^> *")

(defun org-supertag-view-chat--extract-latest-input ()
  "Return text after the last prompt line (* User: ...)."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "^\\* User.*?:[ ]*\\(.*\\)$" nil t)
      (let ((input-text (match-string 1)))
        (string-trim (or input-text ""))))))

(defun org-supertag-view-chat--current-input ()
  "Return current prompt line user text (string without surrounding spaces).
Falls back to regex search when marker is missing or the captured
text is empty. This prevents the first input cannot be sent issue when
the prompt marker is unexpectedly nil or misplaced."
  (let* ((txt (when (and org-supertag-view-chat--prompt-start
                          (marker-position org-supertag-view-chat--prompt-start))
                (save-excursion
                  (goto-char org-supertag-view-chat--prompt-start)
                  (string-trim (buffer-substring-no-properties (point) (line-end-position)))))))
    (when (or (not txt) (string-empty-p txt))
      (save-excursion
        (beginning-of-line)
        (when (looking-at "^\\* User.*?:[ ]*\\(.*\\)$")
          (let ((input-text (match-string 1)))
            (setq txt (string-trim (or input-text "")))))))
    (if (and txt (not (string-empty-p txt)))
        txt
      (org-supertag-view-chat--extract-latest-input))))

;; -----------------------------------------------------------------------------
;; Main send input function
;; -----------------------------------------------------------------------------
;; --- User commands ---
(defvar org-supertag-view-chat--user-commands (make-hash-table :test 'equal)
  "Store all user-defined chat commands, command name -> prompt content.")

(defconst org-supertag-view-chat--command-dir
  (expand-file-name "command/" (or (bound-and-true-p org-supertag-data-directory)
                                    (expand-file-name "org-supertag/" user-emacs-directory)))
  "Path to the custom command prompt files. One .prompt file per command.")

(defun org-supertag-view-chat--load-user-commands ()
  "Load all custom command prompt files into hash-table."
  (clrhash org-supertag-view-chat--user-commands)
  (when (file-directory-p org-supertag-view-chat--command-dir)
    (dolist (file (directory-files org-supertag-view-chat--command-dir t "\\.prompt$"))
      (let ((name (file-name-base file)))
        (with-temp-buffer
          (insert-file-contents file)
          (puthash name (buffer-string) org-supertag-view-chat--user-commands))))))

;; Load on startup
(org-supertag-view-chat--load-user-commands)

;; /define <name> "multi-line prompt, containing $input variable"
(defun org-supertag-view-chat--define-command (name prompt)
  "Define a new command, persist the prompt to a file and load it into the hash-table."
  (unless (file-directory-p org-supertag-view-chat--command-dir)
    (make-directory org-supertag-view-chat--command-dir t))
  (let ((file (expand-file-name (concat name ".prompt") org-supertag-view-chat--command-dir)))
    (with-temp-file file
      (insert prompt)))
  (puthash name prompt org-supertag-view-chat--user-commands)
  (message "Defined command /%s" name))

;; /commands show all commands and their content
(defun org-supertag-view-chat--list-commands ()
  "Show all available commands and their prompt content."
  (interactive)
  (let ((msg (with-temp-buffer
               (insert "Commands:\n")
               (insert "/create-question\n")
               (maphash (lambda (k v)
                          (insert (format "/%s\n%s\n---\n" k v)))
                        org-supertag-view-chat--user-commands)
               (buffer-string))))
    (message "%s" msg)))

(defun org-supertag-view-chat--get-all-command-names ()
  "Return a list of all available command names, without the leading slash."
  (let ((cmds '("define" "commands" "default" "reset"))) ; Meta commands
    ;; Add built-in commands
    (dolist (cmd org-supertag-view-chat--builtin-commands)
      (push (car cmd) cmds))
    ;; Add user-defined commands
    (maphash (lambda (k _v) (push k cmds))
             org-supertag-view-chat--user-commands)
    ;; Sort commands for better user experience
    (sort (delete-dups cmds) 'string<)))


(defun org-supertag-view-chat--show-available-commands ()
  "Show a brief list of available commands in the minibuffer."
  (interactive)
  (let* ((commands (org-supertag-view-chat--get-all-command-names))
         (command-list (mapconcat (lambda (cmd) (concat "/" cmd)) commands " ")))
    (message "Available commands: %s" command-list)))

(defun org-supertag-view-chat--select-command-simple ()
  "Simple command selection using completing-read."
  (interactive)
  (let* ((p (point))
         (prompt-start-pos (and org-supertag-view-chat--prompt-start
                               (marker-position org-supertag-view-chat--prompt-start))))
    (if (and prompt-start-pos (>= p prompt-start-pos))
        (let* ((commands (org-supertag-view-chat--get-all-command-names))
               (selected (completing-read "Select command: " commands nil t)))
          (when (and selected (not (string-empty-p selected)))
            (insert "/" selected " ")
            (message "Command '/%s' inserted." selected)))
      (message "Not in prompt area"))))

(defun org-supertag-view-chat--smart-slash ()
  "Smart slash: insert slash and optionally show command menu."
  (interactive)
  (let* ((p (point))
         (prompt-start-pos (and org-supertag-view-chat--prompt-start
                               (marker-position org-supertag-view-chat--prompt-start))))
    (if (and prompt-start-pos (>= p prompt-start-pos))
        (progn
          (insert "/")
          ;; Ask if user wants to see commands
          (when (y-or-n-p "Show available commands? ")
            (delete-char -1) ; Remove the slash we just inserted
            (org-supertag-view-chat--select-command-simple)))
      ;; Not in prompt area, just insert slash
      (insert "/"))))

;; Parse /define command, return (name . prompt) or nil
(defun org-supertag-view-chat--parse-define (input)
  "Parse /define command input."
  (when (and input (stringp input))  
    (cond
     ;; Pattern 1: /define name "prompt content"
     ((string-match "^/define\\s-+\\([a-zA-Z0-9_-]+\\\)\\s-+\"\\(.*\\)\"$" input)
      (let ((name (match-string 1 input))
            (prompt (match-string 2 input)))
        (cons name (or prompt ""))))
     ;; Pattern 2: /define name (without quotes, empty prompt)
     ((string-match "^/define\\s-+\\([a-zA-Z0-9_-]+\\\)\\s-*$" input)
      (let ((name (match-string 1 input)))
        (cons name "")))
     ;; Pattern 3: /define "name" "prompt" (both quoted)
     ((string-match "^/define\\s-+\"\\([a-zA-Z0-9_-]+\\\)\"\\s-+\"\\(.*\\)\"$" input)
      (let ((name (match-string 1 input))
            (prompt (match-string 2 input)))
        (cons name (or prompt ""))))
     (t nil))))

(defun org-supertag-view-chat--validate-define-syntax (input)
  "Validate /define command syntax and provide helpful error messages."
  (cond
   ((string-match "^/define\\s-*$" input)
    "Error: /define requires a command name. Usage: /define <name> \"<prompt>\"")
   ((string-match "^/define\\s-+\\([a-zA-Z0-9_-]+\\\)\\s-+[^\"]*$" input)
    "Error: Prompt must be quoted. Usage: /define <name> \"<prompt>\"")
   ((string-match "^/define\\s-+\\([^a-zA-Z0-9_-]\\\)" input)
    "Error: Command name must contain only letters, numbers, underscores, and hyphens.")
   (t nil)))

;; Only a built-in command /create-question
(defconst org-supertag-view-chat--builtin-commands
  '(("create-question" . "Please list all important questions related to $input in $lang.")))

;; When switch prompt, display content
(defun org-supertag-view-chat--show-current-command-prompt ()
  "Display current command prompt in chat buffer"
  (when org-supertag-view-chat--current-command
    (let ((prompt (cond
                    ((and org-supertag-view-chat--current-command
                          (assoc org-supertag-view-chat--current-command org-supertag-view-chat--builtin-commands))
                     (cdr (assoc org-supertag-view-chat--current-command org-supertag-view-chat--builtin-commands)))
                    ((and org-supertag-view-chat--current-command
                          (gethash org-supertag-view-chat--current-command org-supertag-view-chat--user-commands))
                     (gethash org-supertag-view-chat--current-command org-supertag-view-chat--user-commands))
                    (t nil))))
      (when prompt
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (propertize (format "[Prompt for /%s]:\n%s\n" org-supertag-view-chat--current-command prompt)
                              'face 'font-lock-comment-face)))))))

;; Modify send-input main flow, support /define, /commands, command switch, variable replacement
(defun org-supertag-view-chat-send-input ()
  "Send the current prompt line's text to the backend.
Handles both regular chat queries and special /commands."
  (interactive)
  (cl-block org-supertag-view-chat-send-input
    (let* ((raw (org-supertag-view-chat--current-input))
           (lang (symbol-value 'org-supertag-view-chat-lang))
           (input (when raw (string-trim (substring-no-properties raw)))))
      
      ;; Debug: show what input we're processing
      (message "Processing input: %S" input)
      
      ;; /define
      (when (string-match "^/define" input)
        (let ((error-msg (org-supertag-view-chat--validate-define-syntax input)))
          (when error-msg
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert (propertize (format "[System] %s\n" error-msg) 'face 'font-lock-warning-face)))
            (org-supertag-view-chat--insert-prompt)
            (setq org-supertag-view-chat--prompt-start nil)
            (cl-return-from org-supertag-view-chat-send-input))))
        
      (let ((define-pair (org-supertag-view-chat--parse-define input)))
        (when define-pair
          (message "Define command detected: %S" define-pair)
          (org-supertag-view-chat--define-command (car define-pair) (cdr define-pair))
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (propertize (format "[System] Command /%s defined\n" (car define-pair)) 'face 'font-lock-comment-face)))
          (org-supertag-view-chat--insert-prompt)
          (setq org-supertag-view-chat--prompt-start nil)
          (cl-return-from org-supertag-view-chat-send-input)))
        
      ;; /commands
      (when (string= input "/commands")
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (propertize "[System] Available commands:\n" 'face 'font-lock-comment-face))
          (insert "/create-question\n")
          (insert "/define <name> \"<prompt>\"\n")
          (maphash (lambda (k v)
                     (insert (format "/%s\n%s\n---\n" k v)))
                   org-supertag-view-chat--user-commands))
        (org-supertag-view-chat--insert-prompt)
        (setq org-supertag-view-chat--prompt-start nil)
        (cl-return-from org-supertag-view-chat-send-input))
        
      ;; 命令处理：切换模式 + 立即执行
      (let ((cmd-pair (org-supertag-view-chat--parse-command input)))
        (when cmd-pair
          (let ((cmd (car cmd-pair))
                (args (cdr cmd-pair)))
            ;; 跳过已处理的特殊命令
            (unless (member cmd '("define" "commands"))
              ;; 检查是否为有效命令
              (let ((is-builtin (assoc cmd org-supertag-view-chat--builtin-commands))
                    (is-user-defined (gethash cmd org-supertag-view-chat--user-commands))
                    (is-meta (member cmd '("default" "reset"))))
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (unless (bolp) (insert "\n"))
                  (insert (propertize "[System] " 'face 'font-lock-comment-face))
                  (cond
                   (is-meta
                    (setq org-supertag-view-chat--current-command nil)
                    (insert "Switched back to default chat mode\n"))
                   ((or is-builtin is-user-defined)
                    (setq org-supertag-view-chat--current-command cmd)
                    (if (and args (not (string-empty-p args)))
                        (insert (format "Switched to command: %s and executing with input: %s\n" cmd args))
                      (insert (format "Switched to command: %s\n" cmd)))
                    (org-supertag-view-chat--show-current-command-prompt))
                   (t
                    (insert (format "Unknown command: %s\n" cmd)))))
                
                ;; 如果有参数且是有效命令，立即执行
                (when (and (or is-builtin is-user-defined)
                          args 
                          (not (string-empty-p args)))
                  ;; 使用参数作为输入立即执行命令
                  (org-supertag-view-chat--execute-command-with-input cmd args lang)
                  (cl-return-from org-supertag-view-chat-send-input))
                
                (org-supertag-view-chat--insert-prompt)
                (setq org-supertag-view-chat--prompt-start nil)
                (cl-return-from org-supertag-view-chat-send-input))))
      ;; 正常输入流程
      (when (and input (not (string-empty-p input)))
        (let ((inhibit-read-only t))
          (let ((prompt-line-start
                 (save-excursion 
                   (beginning-of-line)
                   (if (looking-at org-supertag-view-chat--prompt-regexp)
                       (point)
                     (when (re-search-backward org-supertag-view-chat--prompt-regexp nil t)
                       (match-beginning 0))))))
            (if prompt-line-start
                (progn
                  (goto-char prompt-line-start)
                  (delete-region prompt-line-start (line-end-position))
                  (insert "\n"))  ;; 先换行，确保 Assistant is thinking... 单独成行
              (goto-char (point-max))
              (unless (bolp) (insert "\n")))
            (setq org-supertag-view-chat--response-start-marker (point-marker))
            (insert (propertize "Assistant is thinking..." 'face 'italic)))
          ;; Variable replacement when sending
          (let* ((prompt (cond
                          ((and org-supertag-view-chat--current-command
                                (assoc org-supertag-view-chat--current-command org-supertag-view-chat--builtin-commands))
                           (cdr (assoc org-supertag-view-chat--current-command org-supertag-view-chat--builtin-commands)))
                          ((and org-supertag-view-chat--current-command
                                (gethash org-supertag-view-chat--current-command org-supertag-view-chat--user-commands))
                           (gethash org-supertag-view-chat--current-command org-supertag-view-chat--user-commands))
                          (t nil)))
                 (final-input (if (and prompt (string-match-p "\\$input\\|\\$lang" prompt))
                                  (let ((temp-input (if (string-match-p "\\$input" prompt)
                                                       (replace-regexp-in-string "\\$input" input prompt)
                                                     prompt)))
                                    (if (string-match-p "\\$lang" temp-input)
                                        (replace-regexp-in-string "\\$lang" lang temp-input)
                                      temp-input))
                                input))
                 (payload `(("query" . ,final-input)
                            ("query_text" . ,final-input)
                            ("lang" . ,lang)
                            ("command" . ,org-supertag-view-chat--current-command)
                            ("history" . ,org-supertag-view-chat--conversation-history))))
            ;;(message "[Chat] Sending payload: %S" payload)
            (org-supertag-bridge-call-async
             "rag/query"
             (list payload)
             #'org-supertag-view-chat--handle-response))
          (push (list :role "user" :content input) org-supertag-view-chat--conversation-history))
        (setq org-supertag-view-chat--prompt-start nil)
        (unless (and input (not (string-empty-p input)))
          (message "[SuperTag Chat] No input detected on current prompt line."))))))))

;; Invisibility spec for context blocks
(add-to-invisibility-spec 'org-st-chat-context)


(defun org-supertag-view-chat--toggle-context (button)
  "Toggle visibility of the context overlay attached to BUTTON, update arrow."
  (let ((ov (button-get button 'context-overlay)))
    (when (overlayp ov)
      (let* ((currently-hidden (overlay-get ov 'invisible))
             (new-hidden (if currently-hidden nil 'org-st-chat-context))
             (label-text (button-get button 'label-text))
             (new-label (if currently-hidden
                            (replace-regexp-in-string "^▸" "▾" label-text)
                          (replace-regexp-in-string "^▾" "▸" label-text))))
        (overlay-put ov 'invisible new-hidden)
        ;; Update button label
        (cond
         ;; If Emacs version supports button-label-set
         ((fboundp 'button-label-set)
          (button-label-set button new-label)
          (button-put button 'label-text new-label))
         (t
          ;; Compatible with old version: delete and rebuild button text
          (let ((start (button-start button))
                (end (button-end button)))
            (let ((inhibit-read-only t))
              (delete-region start end)
              (goto-char start)
              (insert-text-button new-label
                                  'action #'org-supertag-view-chat--toggle-context
                                  'follow-link t
                                  'label-text new-label
                                  'context-overlay ov)))))))))

;; --- Buffer Setup & UI Commands ---

;;;###autoload
(defun org-supertag-view-chat-open ()
  "Open or switch to the Org SuperTag Chat buffer."
  (interactive)
  (let* ((buffer (get-buffer-create org-supertag-view-chat-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'org-supertag-view-chat-mode)
          (org-supertag-view-chat-mode))
        (unless (org-supertag-bridge-ready-p)
          (message "[SuperTag Chat] Starting Python backend…")
          (org-supertag-bridge-ensure-ready 10))
        (goto-char (point-max))
        (when (= (point-min) (point-max))
          (insert (propertize "#+TITLE: Welcome to SuperTag Chat\n")))
        (org-supertag-view-chat--insert-prompt))))
  (display-buffer org-supertag-view-chat-buffer-name)
  (select-window (get-buffer-window org-supertag-view-chat-buffer-name)))

(defvar org-supertag-view-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-supertag-view-chat-send-input)
    (define-key map (kbd "RET") #'org-supertag-view-chat-send-input)
    ;; Use C-c / for command selection
    (define-key map (kbd "C-c /") #'org-supertag-view-chat--select-command-simple)
    ;; Or use / with a different approach
    (define-key map (kbd "/") #'org-supertag-view-chat--smart-slash)
    (define-key map (kbd "C-c C-h") #'org-supertag-view-chat--show-available-commands)
    map)
  "Keymap for org-supertag-view-chat-mode.")

;; Major Mode definition with keymap
(define-derived-mode org-supertag-view-chat-mode org-mode "Org-ST-Chat"
  "Major mode for the chat view conversation."
  :group 'org-supertag
  (setq-local truncate-lines t)
  (setq-local org-hide-leading-stars t)
  ;; Make buffer read-only by default, we toggle it when editing prompt
  (read-only-mode -1)
  (use-local-map org-supertag-view-chat-mode-map)
  ;; Disable company mode in chat buffer to avoid conflicts
  (when (featurep 'company)
    (company-mode -1))
  ;; Also disable any global company mode
  (setq-local company-backends nil))

;; ;; --- Context Management ---
;; (defvar-local org-supertag-chat--current-node-context nil
;;   "Current org node context for Chat View commands.
;; Structure: (title content tags id filepath)")

;; (defun org-supertag-chat--collect-node-context ()
;;   "Silently collect current org node context and return it as a plist.
;; This should be called from the original Org buffer."
;;   (message "[Chat Debug] collect-node-context called.")
;;     (when (and (derived-mode-p 'org-mode) (org-at-heading-p))
;;     (message "[Chat Debug] At heading: t")
;;     (let* ((title (org-get-heading t t))
;;            (content (org-get-entry))
;;            (tags (org-get-tags))
;;            (id (org-id-get))
;;            (filepath (buffer-file-name)))
;;       (message "[Chat Debug] Collected - title: %S, content: %S, tags: %S, id: %S, filepath: %S"
;;                title content tags id filepath)
;;       (list :title title
;;             :content content
;;             :tags tags
;;             :id id
;;             :filepath filepath))))

;; (defun org-supertag-chat--use-current-node-p ()
  ;;   "Determine if current node context should be used."
;;   (let ((result (and org-supertag-chat--current-node-context
;;                       (equal (plist-get org-supertag-chat--current-node-context :filepath)
;;                              (buffer-file-name)))))
;;     (message "[Chat Debug] use-current-node-p returning: %S" result)
;;     result))

;; --- Command System ---
(defun org-supertag-view-chat--parse-command (input)
  "Parse command input, return (command . args) or nil."
  (when (and input (stringp input)) 
    (if (string-match "^/\\([a-zA-Z0-9_-]+\\)\\(?:\\s-+\\(.*\\)\\)?" input)
        (let ((cmd (match-string 1 input))
              (args (string-trim (or (match-string 2 input) ""))))
          (cons (or cmd "") args)))))

(defun org-supertag-view-chat--execute-command-with-input (cmd input lang)
  "Execute command CMD with INPUT, similar to normal chat flow."
  (let ((inhibit-read-only t))
    ;; 显示执行状态
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (setq org-supertag-view-chat--response-start-marker (point-marker))
    (insert (propertize "Assistant is thinking..." 'face 'italic))
    
    ;; 获取命令提示词
    (let* ((prompt (cond
                    ((assoc cmd org-supertag-view-chat--builtin-commands)
                     (cdr (assoc cmd org-supertag-view-chat--builtin-commands)))
                    ((gethash cmd org-supertag-view-chat--user-commands)
                     (gethash cmd org-supertag-view-chat--user-commands))
                    (t nil)))
           (final-input (if (and prompt (string-match-p "\\$input\\|\\$lang" prompt))
                           (let ((temp-input (if (string-match-p "\\$input" prompt)
                                               (replace-regexp-in-string "\\$input" input prompt)
                                             prompt)))
                             (if (string-match-p "\\$lang" temp-input)
                                 (replace-regexp-in-string "\\$lang" lang temp-input)
                               temp-input))
                         input))
           (payload `(("query" . ,final-input)
                     ("query_text" . ,final-input)
                     ("lang" . ,lang)
                     ("command" . ,cmd)
                     ("history" . ,org-supertag-view-chat--conversation-history))))
      
      ;; 发送请求
      (org-supertag-bridge-call-async
       "rag/query"
       (list payload)
       #'org-supertag-view-chat--handle-response)
      
      ;; 添加到对话历史
      (push (list :role "user" :content input) org-supertag-view-chat--conversation-history))))

;; Only a built-in command /create-question
(defconst org-supertag-view-chat--builtin-commands
  '(("create-question" . "Please list all important questions related to $input in $lang.")))

;; When switch prompt, display content
(defun org-supertag-view-chat--show-current-command-prompt ()
  "Display current command prompt in chat buffer"
  (when org-supertag-view-chat--current-command
    (let ((prompt (cond
                    ((and org-supertag-view-chat--current-command
                          (assoc org-supertag-view-chat--current-command org-supertag-view-chat--builtin-commands))
                     (cdr (assoc org-supertag-view-chat--current-command org-supertag-view-chat--builtin-commands)))
                    ((and org-supertag-view-chat--current-command
                          (gethash org-supertag-view-chat--current-command org-supertag-view-chat--user-commands))
                     (gethash org-supertag-view-chat--current-command org-supertag-view-chat--user-commands))
                    (t nil))))
      (when prompt
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (propertize (format "[Prompt for /%s]:\n%s\n" org-supertag-view-chat--current-command prompt)
                              'face 'font-lock-comment-face)))))))

;; --- Enhanced Send Logic ---
(defun org-supertag-view-chat--handle-command-response (response)
  "Handle response from commands and insert into buffer."
  (with-current-buffer (get-buffer-create org-supertag-view-chat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char org-supertag-view-chat--response-start-marker)
      (delete-region (point) (point-max))
      (insert "** Assistant\n")
      (insert (format "%s\n" (org-supertag-view-chat--md-to-org (or response ""))))
      (insert "\n")
      (put-text-property org-supertag-view-chat--response-start-marker (point) 'read-only t)
      (org-supertag-view-chat--insert-prompt))))

;; --- Save conversation ---
(defgroup org-supertag-chat nil
  "Chat View configuration for enhanced AI interactions."
  :group 'org-supertag)

(defcustom org-supertag-chat-default-save-method 'ask
  "Default save method for conversations."
  :type '(choice (const :tag "Ask each time" ask)
                 (const :tag "New file" new-file)
                 (const :tag "Append to node" append-node)
                 (const :tag "New subnode" new-node))
  :group 'org-supertag-chat)

(defcustom org-supertag-chat-save-directory
  (expand-file-name "chat-notes/" user-emacs-directory)
  "Directory for saving Chat View conversations."
  :type 'directory
  :group 'org-supertag-chat)

(defun org-supertag-view-chat--format-conversation (conversation)
  "Format conversation for org-mode display."
  (let ((formatted (replace-regexp-in-string "^" "> " (or conversation ""))))
    (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" formatted)))

(defun org-supertag-view-chat--save-as-new-file (conversation title)
  "Save conversation as new org file."
  (when (and conversation title (stringp conversation) (stringp title))  ; 确保参数存在且为字符串
    (let ((filename (expand-file-name (concat title ".org") org-supertag-chat-save-directory)))
      (make-directory org-supertag-chat-save-directory t)
      (with-current-buffer (find-file-noselect filename)
        (insert (format "#+TITLE: %s\n#+DATE: %s\n#+TAGS: ai-conversation\n\n"
                        title (format-time-string "%Y-%m-%d")))
        (insert "* AI Conversation\n\n")
        (insert (org-supertag-view-chat--format-conversation conversation))
        (save-buffer)
        (message "Conversation saved to: %s" filename)))))

(defun org-supertag-view-chat--save-append-to-node (conversation)
  "Append conversation to current org headline."
  (when (and conversation (stringp conversation) (org-at-heading-p))  ; 确保参数存在且为字符串
    (save-excursion
      (org-end-of-subtree t)
      (insert "\n\n* AI Assistant Conversation\n")
      (insert (format "#+CAPTION: Generated %s\n"
                      (format-time-string "%Y-%m-%d %H:%M")))
      (insert (org-supertag-view-chat--format-conversation conversation))
      (message "Conversation appended to current node"))))

(defun org-supertag-view-chat--save-as-subnode (conversation title)
  "Create new subnode under current headline."
  (when (and conversation title (stringp conversation) (stringp title) (org-at-heading-p))  ; 确保参数存在且为字符串
    (save-excursion
      (org-end-of-subtree t)
      (insert "\n")
      (org-insert-heading)
      (insert title)
      (org-set-property "DATE" (format-time-string "%Y-%m-%d"))
      (org-set-property "TAGS" "ai-conversation")
      (insert "\n")
      (insert (org-supertag-view-chat--format-conversation conversation))
      (message "Conversation saved as subnode: %s" title))))

(defun org-supertag-view-chat--save-conversation (conversation)
  "Save Chat View conversation with user choice of method."
  (interactive "sConversation: ")
  (when (and conversation (stringp conversation))  ; 确保参数存在且为字符串
    (let* ((title (read-string "Conversation title: "
                               (format "Chat-%s" (format-time-string "%Y%m%d"))))
           (choice (completing-read "Save conversation as: "
                                    '("new file" "append to current node" "create new subnode")
                                    nil t)))
      (pcase choice
        ("new file" (org-supertag-view-chat--save-as-new-file conversation title))
        ("append to current node" (org-supertag-view-chat--save-append-to-node conversation))
        ("create new subnode" (org-supertag-view-chat--save-as-subnode conversation title))))))

(defun org-supertag-view-chat--quick-save (conversation)
  "Save using default method without prompting."
  (when (and conversation (stringp conversation))  ; 确保参数存在且为字符串
    (let ((method (if (eq org-supertag-chat-default-save-method 'ask)
                      (completing-read "Save as: "
                                       '("new file" "append to current node" "create new subnode") nil t)
                    org-supertag-chat-default-save-method)))
      (pcase method
        ("new file" (org-supertag-view-chat--save-as-new-file
                     conversation (format "Chat-%s" (format-time-string "%Y%m%d"))))
        ("append to current node" (org-supertag-view-chat--save-append-to-node conversation))
        ("create new subnode" (org-supertag-view-chat--save-as-subnode
                              conversation (format "AI-Conversation-%s" (format-time-string "%Y%m%d"))))))))

(define-key org-supertag-view-chat-mode-map (kbd "C-c C-s")
  (lambda ()
    (interactive)
    (let ((conversation (buffer-substring-no-properties (point-min) (point-max))))
      (when (and conversation (stringp conversation))  ; 确保参数存在且为字符串
        (org-supertag-view-chat--quick-save conversation)))))

(provide 'org-supertag-view-chat)

;;; org-supertag-view-chat.el ends here

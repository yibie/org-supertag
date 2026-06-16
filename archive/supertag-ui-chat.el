;;; supertag-ui-chat.el --- Chat View for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a user-facing chat view for interacting with the
;; org-supertag AI backend. It is part of the UI layer in the new architecture.
;; This is a full-featured version including the command and session-saving systems.

;;; Code:

(require 'cl-lib)
(require 'supertag-rag)

(defgroup supertag-chat nil
  "Chat View configuration for enhanced AI interactions."
  :group 'org-supertag)

(defcustom supertag-ui-chat-buffer-name "*SuperTag Chat*"
  "The name of the buffer for the chat view."
  :type 'string
  :group 'supertag-chat)

(defcustom supertag-ui-chat-lang "English"
  "The language used in chat view"
  :type 'string
  :group 'supertag-chat)

(defcustom supertag-ui-chat-default-save-method 'ask
  "Default save method for conversations."
  :type '(choice (const :tag "Ask each time" ask)
                 (const :tag "New file" new-file)
                 (const :tag "Append to node" append-node)
                 (const :tag "New subnode" new-node))
  :group 'supertag-chat)

(defcustom supertag-ui-chat-save-directory
  (expand-file-name "chat-notes/" (or (bound-and-true-p supertag-data-directory)
                                     (expand-file-name "org-supertag/" user-emacs-directory)))
  "Directory for saving Chat View conversations."
  :type 'directory
  :group 'supertag-chat)


;; --- Faces ---
(defface supertag-chat-label-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for labels like 'Analysis:' or 'User:'."
  :group 'supertag-chat)

(defface supertag-chat-prompt-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the input prompt '>'"
  :group 'supertag-chat)

;; --- Buffer-Local Variables ---
(defvar-local supertag-ui-chat--conversation-history nil)
(defvar-local supertag-ui-chat--response-start-marker nil)
(defvar-local supertag-ui-chat--prompt-start nil)
(defvar-local supertag-ui-chat--current-command nil
  "Current active chat command (e.g. 'tags', 'expand', etc., nil for default mode).")
(defvar-local supertag-ui-chat--turn-finalized nil
  "Flag to ensure finalization logic runs only once per turn.")
(defvar-local supertag-ui-chat--current-response-parts nil
  "A list to accumulate response chunks in a streaming conversation.")

;; --- Command System Variables ---
(defvar supertag-ui-chat--user-commands (make-hash-table :test 'equal)
  "Store all user-defined chat commands, command name -> prompt content.")

(defconst supertag-ui-chat--command-dir
  (expand-file-name "command/" (or (bound-and-true-p supertag-data-directory)
                                    (expand-file-name "org-supertag/" user-emacs-directory)))
  "Path to the custom command prompt files. One .prompt file per command.")

(defconst supertag-ui-chat--builtin-commands
  '(("create-question" . "Please list all important questions related to $input in $lang.")
    ("rag" . "Answer the question using the local knowledge base: $input"))
  "Alist of built-in commands and their prompts.")


;; --- Core Functions ---
(defun supertag-ui-chat--md-to-org (md-string)
  "Convert a markdown string to an org-mode formatted string."
  (if (and md-string (not (string-empty-p md-string)))
      (with-temp-buffer
        (insert md-string)
        (goto-char (point-min))
        ;; Headers: # title -> * title, ## title -> ** title
        (while (re-search-forward "^\\(#+\\)\\s-+" nil t)
          (replace-match (concat (make-string (length (match-string 1)) ?*) " ")))
        ;; Bold: **bold** -> *bold*
        (goto-char (point-min))
        (while (re-search-forward "\\*\\*\\([^ \n*][^*]*[^ \n*]\\)\\*\\*" nil t)
          (replace-match (concat "*" (match-string 1) "*")))
        ;; Italics: *italic* or _italic_ -> /italic/
        (goto-char (point-min))
        (while (re-search-forward "\\(?:\\*\\|_\\)\\([^ \n*_][^*_]*[^ \n*_]\\)\\(?:\\*\\|_\\)" nil t)
          (replace-match (concat "/" (match-string 1) "/")))
        ;; Inline code: `code` -> =code=
        (goto-char (point-min))
        (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
          (replace-match (concat "=" (match-string 1) "=")))
        ;; Code blocks: ```language\ncode\n``` -> #+BEGIN_SRC language\ncode\n#+END_SRC
        (goto-char (point-min))
        (while (re-search-forward "^```\\([a-zA-Z0-9]+\\)?\n\\([\\s\\S]*?\\)\n```" nil t)
          (let ((lang (or (match-string 1) ""))
                (code (match-string 2)))
            (replace-match (concat "#+BEGIN_SRC " lang "\n" code "\n#+END_SRC"))))
        ;; Unordered lists: * item -> - item
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*\\* " nil t)
          (replace-match "- "))
        ;; Links: [text](url) -> [[url][text]]
        (goto-char (point-min))
        (while (re-search-forward "\\[\\([^\\]]+\\)\\](\\([^)]+\\))" nil t)
          (replace-match (concat "[[" (match-string 2) "][" (match-string 1) "]]")))
        ;; Horizontal rules: --- -> -----
        (goto-char (point-min))
        (while (re-search-forward "^---\\s-*$" nil t)
          (replace-match "-----"))
        (buffer-string))
    ""))

(defun supertag-ui-chat--insert-prompt ()
  "Insert an org headline as the input prompt at the end of the buffer."
  (with-current-buffer (get-buffer-create supertag-ui-chat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (let ((headline
             (if supertag-ui-chat--current-command
                 (format "* User [%s mode]: " supertag-ui-chat--current-command)
               "* User: ")))
        (insert (propertize headline 'face 'supertag-chat-prompt-face))
        (setq supertag-ui-chat--prompt-start (point-marker))))))

(defun supertag-ui-chat--stream-rag-result (chunk)
  "Process a chunk from the RAG streaming response, update UI and accumulate response."
  (with-current-buffer (get-buffer-create supertag-ui-chat-buffer-name)
    (let ((inhibit-read-only t))
      (push chunk supertag-ui-chat--current-response-parts)
      (when (and supertag-ui-chat--response-start-marker (marker-position supertag-ui-chat--response-start-marker))
        (goto-char supertag-ui-chat--response-start-marker)
        (delete-region (point) (line-end-position))
        (insert "\n** Assistant\n")
        (setq supertag-ui-chat--response-start-marker nil))
      (goto-char (point-max))
      (insert (supertag-ui-chat--md-to-org chunk)))))
      
(defun supertag-ui-chat--process-rag-result (answer)
  "Finalize the RAG response processing.
This function is called ONCE after the entire response has been streamed."
  (with-current-buffer (get-buffer-create supertag-ui-chat-buffer-name)
    (let ((inhibit-read-only t)
          (response-content (if (and answer (not (string-empty-p answer)))
                                answer
                              "Assistant did not provide a response.")))
      ;; If streaming failed, clear the status message here
      (when (and supertag-ui-chat--response-start-marker (marker-position supertag-ui-chat--response-start-marker))
        (goto-char supertag-ui-chat--response-start-marker)
        (delete-region (point) (line-end-position))
        (insert "\n** Assistant\n")
        (insert (supertag-ui-chat--md-to-org response-content)))

      ;; 1. Update conversation history with the full response
      (setq supertag-ui-chat--conversation-history
            (cons (list :role "assistant" :content response-content)
                  supertag-ui-chat--conversation-history))

      ;; 2. Insert a newline for spacing before the next prompt
      (insert "\n")
      (supertag-ui-chat--insert-prompt))))

(defun supertag-ui-chat--open-node (id)
  "Jump to the org node corresponding to the given ID."
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

(defun supertag-ui-chat--current-input ()
  "Return current prompt line user text."
  (let ((txt (when (and supertag-ui-chat--prompt-start (marker-position supertag-ui-chat--prompt-start))
               (save-excursion
                 (goto-char supertag-ui-chat--prompt-start)
                 (string-trim (buffer-substring-no-properties (point) (line-end-position)))))))
    (if (and txt (not (string-empty-p txt)))
        txt
      (save-excursion
        (goto-char (point-max))
        (when (re-search-backward "^\* User.*?:[ ]*\(.*\)$" nil t)
          (string-trim (or (match-string 1) "")))))))

;; --- Command System ---

(defun supertag-ui-chat--load-user-commands ()
  "Load all custom command prompt files into hash-table."
  (clrhash supertag-ui-chat--user-commands)
  (when (file-directory-p supertag-ui-chat--command-dir)
    (dolist (file (directory-files supertag-ui-chat--command-dir t "\.prompt$"))
      (let ((name (file-name-base file)))
        (with-temp-buffer
          (insert-file-contents file)
          (puthash name (buffer-string) supertag-ui-chat--user-commands))))))

(defun supertag-ui-chat--define-command (name prompt)
  "Define a new command, persist the prompt to a file and load it."
  (unless (file-directory-p supertag-ui-chat--command-dir)
    (make-directory supertag-ui-chat--command-dir t))
  (let ((file (expand-file-name (concat name ".prompt") supertag-ui-chat--command-dir)))
    (with-temp-buffer
      (insert prompt)
      (write-file file)))
  (puthash name prompt supertag-ui-chat--user-commands)
  (message "Defined command /%s" name))

(defun supertag-ui-chat--list-commands ()
  "Show all available commands and their prompt content."
  (interactive)
  (let ((msg (with-temp-buffer
               (insert "Commands:\n")
               (insert "/define <name> \"<prompt>\"\n")
               (insert "/commands\n")
               (insert "/reset\n")
               (insert "/rag <question> - Answer question using local knowledge base\n")
               (maphash (lambda (k v)
                          (insert (format "/%s\n%s\n---\n" k v)))
                        supertag-ui-chat--user-commands)
               (buffer-string))))
    (message "%s" msg)))

(defun supertag-ui-chat--get-all-command-names ()
  "Return a list of all available command names, withoupt the leading slash."
  (let ((cmds '("define" "commands" "reset" "rag"))) ; Meta commands
    (dolist (cmd supertag-ui-chat--builtin-commands)
      (push (car cmd) cmds))
    (maphash (lambda (k _v) (push k cmds))
             supertag-ui-chat--user-commands)
    (sort (delete-dups cmds) 'string<)))

(defun supertag-ui-chat--select-command-simple ()
  "Simple command selection using completing-read."
  (interactive)
  (let* ((p (point))
         (prompt-start-pos (and supertag-ui-chat--prompt-start (marker-position supertag-ui-chat--prompt-start))))
    (if (and prompt-start-pos (>= p prompt-start-pos))
        (let* ((commands (supertag-ui-chat--get-all-command-names))
               (selected (completing-read "Select command: " commands nil t)))
          (when (and selected (not (string-empty-p selected)))
            (insert "/" selected " ")
            (message "Command '/%s' inserted." selected)))
      (message "Not in prompt area"))))

(defun supertag-ui-chat--update-status (message)
  "Update the status message in the chat buffer (e.g., 'Assistant is thinking...')."
  (with-current-buffer (get-buffer-create supertag-ui-chat-buffer-name)
    (let ((inhibit-read-only t))
      (when (and supertag-ui-chat--response-start-marker (marker-position supertag-ui-chat--response-start-marker))
        (goto-char supertag-ui-chat--response-start-marker)
        (delete-region (point) (line-end-position))
        (insert (propertize message 'face 'italic))))))

(defun supertag-ui-chat--smart-slash ()
  "Smart slash: insert slash and optionally show command menu."
  (interactive)
  (let* ((p (point))
         (prompt-start-pos (and supertag-ui-chat--prompt-start (marker-position supertag-ui-chat--prompt-start))))
    (if (and prompt-start-pos (>= p prompt-start-pos))
        (progn
          (insert "/")
          (when (y-or-n-p "Show available commands? ")
            (delete-char -1)
            (supertag-ui-chat--select-command-simple)))
      (insert "/"))))

(defun supertag-ui-chat--parse-define (input)
  "Parse /define command input."
  (when (and input (stringp input))
    (cond
     ((string-match "^/define\\s-+\\([a-zA-Z0-9_-]+\\)\\s-+\"\\(.*\\)\"$" input)
      (cons (match-string 1 input) (or (match-string 2 input) "")))
     ((string-match "^/define\\s-+\\([a-zA-Z0-9_-]+\\)\\s-*$" input)
      (cons (match-string 1 input) ""))
     (t nil))))

(defun supertag-ui-chat--parse-command (input)
  "Parse command input, return (command . args) or nil."
  (when (and input (stringp input))
    (if (string-match "^/\\([a-zA-Z0-9_-]+\\)\\(?:\\s-+\\(.*\\)\\)?" input)
        (cons (or (match-string 1 input) "") (string-trim (or (match-string 2 input) "")))
      nil)))

;; --- Main Send Logic ---
(defun supertag-ui-chat-send-input ()
  "Send the current prompt line's text to the RAG backend."
  (interactive)
  (let ((input (supertag-ui-chat--current-input)))
    (when (and input (> (length input) 0))
      (let ((inhibit-read-only t))
        ;; Finalize the user's input line
        (put-text-property supertag-ui-chat--prompt-start (line-end-position) 'read-only t)
        (setq supertag-ui-chat--prompt-start nil) ; Invalidate marker to prevent re-sending
        ;; Show "Thinking..." message
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (setq supertag-ui-chat--response-start-marker (point-marker))
        (insert (propertize "Assistant is thinking..." 'face 'italic)))

      (let* ((cmd-pair (supertag-ui-chat--parse-command input))
             (command (car-safe cmd-pair))
             (final-input (if cmd-pair (cdr cmd-pair) input)))

        ;; 1. Handle local meta-commands that don't call the RAG engine
        (cond
         ((member command '("define" "commands" "reset"))
          (pcase command
            ("define" (when-let ((define-pair (supertag-ui-chat--parse-define input)))
                        (supertag-ui-chat--define-command (car define-pair) (cdr define-pair))))
            ("commands" (supertag-ui-chat--list-commands))
            ("reset" (setq supertag-ui-chat--current-command nil) (message "Switched to default chat mode.")))
          (supertag-ui-chat--process-rag-result nil)) ; Refresh prompt

         ;; 2. All other inputs are sent to the RAG engine
         (t
          (let ((mode (if (string= command "rag") :rag-only :smart)))
            (supertag-rag-ask final-input
                              :mode mode
                              :callback #'supertag-ui-chat--process-rag-result
                              :stream-callback #'supertag-ui-chat--stream-rag-result
                              :feedback-callback #'supertag-ui-chat--update-status))))))))

;; --- Save Conversation ---
(defun supertag-ui-chat--format-conversation (conversation)
  "Format conversation for org-mode display."
  (let ((formatted (replace-regexp-in-string "^" "> " (or conversation ""))))
    (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" formatted)))

(defun supertag-ui-chat--save-as-new-file (conversation title)
  "Save conversation as new org file."
  (when (and conversation title (stringp conversation) (stringp title))
    (let ((filename (expand-file-name (concat title ".org") supertag-ui-chat-save-directory)))
      (make-directory supertag-ui-chat-save-directory t)
      (with-current-buffer (find-file-noselect filename)
        (insert (format "#+TITLE: %s\n#+DATE: %s\n#+TAGS: ai-conversation\n\n" 
                        title (format-time-string "%Y-%m-%d")))
        (insert "* AI Conversation\n\n")
        (insert (supertag-ui-chat--format-conversation conversation))
        (save-buffer)
        (message "Conversation saved to: %s" filename)))))

(defun supertag-ui-chat--save-append-to-node (conversation)
  "Append conversation to current org headline."
  (when (and conversation (stringp conversation) (org-at-heading-p))
    (save-excursion
      (org-end-of-subtree t)
      (insert "\n\n* AI Assistant Conversation\n")
      (insert (format "#+CAPTION: Generated %s\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert (supertag-ui-chat--format-conversation conversation))
      (message "Conversation appended to current node"))))

(defun supertag-ui-chat--save-as-subnode (conversation title)
  "Create new subnode under current headline."
  (when (and conversation title (stringp conversation) (stringp title) (org-at-heading-p))
    (save-excursion
      (org-end-of-subtree t)
      (insert "\n")
      (org-insert-heading)
      (insert title)
      (org-set-property "DATE" (format-time-string "%Y-%m-%d"))
      (org-set-property "TAGS" "ai-conversation")
      (insert "\n")
      (insert (supertag-ui-chat--format-conversation conversation))
      (message "Conversation saved as subnode: %s" title))))

(defun supertag-ui-chat--save-conversation ()
  "Save Chat View conversation with user choice of method."
  (interactive)
  (let ((conversation (buffer-substring-no-properties (point-min) (point-max))))
    (when (and conversation (stringp conversation))
      (let* ((title (read-string "Conversation title: " (format "Chat-%s" (format-time-string "%Y%m%d"))))
             (choice (completing-read "Save conversation as: "
                                      '("new file" "append to current node" "create new subnode")
                                      nil t)))
        (pcase choice
          ("new file" (supertag-ui-chat--save-as-new-file conversation title))
          ("append to current node" (supertag-ui-chat--save-append-to-node conversation))
          ("create new subnode" (supertag-ui-chat--save-as-subnode conversation title)))))))

;; --- UI Commands and Mode Definition ---

(defvar supertag-ui-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'supertag-ui-chat-send-input)
    (define-key map (kbd "RET") #'supertag-ui-chat-send-input)
    (define-key map (kbd "C-c /") #'supertag-ui-chat--select-command-simple)
    (define-key map (kbd "/") #'supertag-ui-chat--smart-slash)
    (define-key map (kbd "C-c C-h") #'supertag-ui-chat--list-commands)
    (define-key map (kbd "C-c C-s") #'supertag-ui-chat--save-conversation)
    map)
  "Keymap for supertag-ui-chat-mode.")

(define-derived-mode supertag-ui-chat-mode org-mode "ST-Chat"
  "Major mode for the chat view conversation."
  :group 'org-supertag
  (setq-local truncate-lines t)
  (setq-local org-hide-leading-stars t)
  (read-only-mode -1)
  (use-local-map supertag-ui-chat-mode-map)
  (when (featurep 'company)
    (company-mode -1))
  (setq-local company-backends nil))

;;;###autoload
(defun supertag-chat ()
  "Open or switch to the SuperTag Chat buffer."
  (interactive)
  (supertag-ui-chat--load-user-commands) ; Load commands every time chat is opened
  (let* ((buffer (get-buffer-create supertag-ui-chat-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'supertag-ui-chat-mode)
          (supertag-ui-chat-mode))
        (goto-char (point-max))
        (when (= (point-min) (point-max))
          (insert (propertize "#+TITLE: Welcome to SuperTag Chat\n" 'face 'font-lock-title-face)))
        (supertag-ui-chat--insert-prompt))))
  (display-buffer supertag-ui-chat-buffer-name)
  (select-window (get-buffer-window supertag-ui-chat-buffer-name)))

(provide 'supertag-ui-chat)

;;; supertag-ui-chat.el ends here

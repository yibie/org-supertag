;;; org-supertag-view-chat.el --- Chat view for org-supertag -*- lexical-binding: t; -*-

(require 'org-supertag-api)
(require 'org-supertag-node)
(require 'org-supertag-db)
(require 'org-supertag-query)
(require 'org-supertag-bridge)

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
        ;; Headers: # title -> * title
        (while (re-search-forward "^#+ \\(.*\\)" nil t) 
          (replace-match (concat (make-string (length (match-string 1)) ?*) " " (match-string 1))))
        
        ;; Bold: **bold** -> *bold*
        (while (re-search-forward "\\*\\*\\(.*?\\)\\*\\*" nil t) 
          (replace-match "*\\1*"))
        
        ;; Italics: *italic* -> /italic/
        (while (re-search-forward "\\*\\(.*?\\)\\*" nil t) 
          (replace-match "/\\1/"))
        
        ;; Inline code: `code` -> =code= (handle before other transformations)
        (goto-char (point-min))
        (while (re-search-forward "`\\([^`]*?\\)`" nil t)
          (replace-match "=\\1="))
        
        ;; Lists: - item -> - item (ensure proper spacing)
        (goto-char (point-min))
        (while (re-search-forward "^\\([ \t]*\\)[*-+] \\(.*\\)$" nil t)
          (replace-match (concat (match-string 1) "- \\2")))
        
        ;; Links: [text](url) -> [[url][text]]
        (goto-char (point-min))
        (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
          (replace-match "[[\\2][\\1]]"))
        
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
                 (format "* User [%s]:" org-supertag-view-chat--current-command)
               "* User:")))
        (insert (propertize headline 'face 'org-supertag-chat-prompt-face))
        (insert "\n"))))

;; --- Async Callback ---
(defun org-supertag-view-chat--handle-response (result)
  "Handle asynchronous response from the RAG backend.
RESULT is the plist returned from the Python backend."
  (with-current-buffer (get-buffer-create org-supertag-view-chat-buffer-name)
    (let ((inhibit-read-only t)
          (response-content nil)
          (history nil)
          (status "error")
          (error-message "No valid response from backend."))

      ;; Check for bridge/EPC error first
      (if (and (listp result) (eq (car result) :error))
          (setq error-message (format "Backend Error: %s" (cadr result)))
        ;; Otherwise, parse the plist from the handler
        ;; Handle both direct results and wrapped results from _run_async
        (let ((actual-result (if (and (listp result) (plist-get result :result))
                                 (plist-get result :result)
                               result)))
          (setq response-content (or (plist-get actual-result :answer) 
                                     (plist-get actual-result :response)
                                     (plist-get actual-result :analysis)))
          (setq history (plist-get actual-result :conversation-history))
          (setq status (or (plist-get actual-result :status) "success"))
          (when (plist-get actual-result :error)
            (setq status "error")
            (setq error-message (plist-get actual-result :error))))

      ;; Go to the response start marker to clear the "Fetching..." message
      (goto-char org-supertag-view-chat--response-start-marker)
      (delete-region (point) (point-max))

      ;; Debug: log raw result
      ;; (message "[Chat Debug] Raw result: %S" result)
      ;; (message "[Chat Debug] Result keys: %S" (when (and (listp result) (evenp (length result))) 
      ;;                                          (cl-loop for (key val) on result by #'cddr collect key)))
      ;; (let ((actual-result (if (and (listp result) (plist-get result :result))
      ;;                          (plist-get result :result)
      ;;                        result)))
      ;;   (message "[Chat Debug] Actual result: %S" actual-result)
      ;;   (message "[Chat Debug] Actual result keys: %S" (when (and (listp actual-result) (evenp (length actual-result)))
      ;;                                                    (cl-loop for (key val) on actual-result by #'cddr collect key)))
      ;;   (message "[Chat Debug] Answer: %S" (plist-get actual-result :answer))
      ;;   (message "[Chat Debug] Response: %S" (plist-get actual-result :response))
      ;;   (message "[Chat Debug] Analysis: %S" (plist-get actual-result :analysis)))
      ;; (message "[Chat Debug] Response content: %S" response-content)
      ;; (message "[Chat Debug] Status: %S" status)

      ;; Insert the content. This is the primary conditional logic.2
      (if (and (string= status "success") response-content)
          ;; --- THEN: SUCCESS CASE ---
          (progn
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
                        (insert (format "- "))
                        (insert-text-button title
                                            'action (lambda (_btn) (org-supertag-view-chat--open-node id))
                                            'follow-link t
                                            'help-echo (format "Open node %s" id)
                                            'face 'org-link)
                        (insert (format ": %s\n" snippet))))
                    ;; Create overlay for context lines only
                    (let ((ov (make-overlay content-start (point))))
                      (overlay-put ov 'invisible 'org-st-chat-context)
                      (overlay-put ov 'intangible nil)))
                  ;; 自动收起 Context 区块
                  (require 'org)
                  (run-at-time
                   0.1 nil
                   (lambda ()
                     (with-current-buffer (current-buffer)
                       (save-excursion
                         (goto-char (point-min))
                         (when (re-search-forward "^\\*\\*\\* Context" nil t)
                           (when (fboundp 'org-fold-hide-subtree)
                             (org-fold-hide-subtree))))))))
                  ))

            ;; Finalize buffer state for success
            (insert "\n\n")
            (put-text-property org-supertag-view-chat--response-start-marker (point) 'read-only t)
            (org-supertag-view-chat--insert-prompt))

        ;; --- ELSE: ERROR CASE ---
        (progn
          (insert (propertize "--- ERROR ---" 'face 'font-lock-warning-face))
          (insert (format "\n%s\n\nRaw Response:\n%S" error-message result))
          ;; Finalize buffer state for error
          (insert "\n\n")
          (put-text-property org-supertag-view-chat--response-start-marker (point) 'read-only t)
          (org-supertag-view-chat--insert-prompt)))))))

(defun org-supertag-view-chat--open-node (id)
  "Corresponding to the node ID, jump to the org buffer and locate the node."
  (interactive "sNode ID: ")
  (let ((marker (org-id-find id t)))
    (if marker
        (progn
          (switch-to-buffer (marker-buffer marker))
          (goto-char marker)
          (org-show-entry)
          (message "Jumped to node %s" id))
      (message "Node %s not found" id))))

;; --- Sending Logic ---
;; Prompt begins with ">" followed by optional spaces.
(defconst org-supertag-view-chat--prompt-regexp "^> *")

(defun org-supertag-view-chat--extract-latest-input ()
  "Return text after the last prompt line (> )."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward org-supertag-view-chat--prompt-regexp nil t)
      (let ((start (match-end 0)))
        (string-trim (buffer-substring-no-properties start (line-end-position)))))))

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
        (when (looking-at "^> *\\(.*\\)$")
          (setq txt (string-trim (match-string 1))))))
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

;; Parse /define command, return (name . prompt) or nil
(defun org-supertag-view-chat--parse-define (input)
  (when (string-match "^/define\\s-+\\([a-zA-Z0-9_-]+\\)\\s-+\"\(.*\)\"$" input)
    (let ((name (match-string 1 input))
          (prompt (match-string 2 input)))
      (cons name prompt))))

;; Only a built-in command /create-question
(defconst org-supertag-view-chat--builtin-commands
  '(("create-question" . "Please list all important questions related to $input.")))

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
      (message "[Chat Debug] send-input called. Raw input: %S"
               (substring-no-properties (or raw "")))
      ;; /define
      (let ((define-pair (org-supertag-view-chat--parse-define input)))
        (when define-pair
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
          (maphash (lambda (k v)
                     (insert (format "/%s\n%s\n---\n" k v)))
                   org-supertag-view-chat--user-commands))
        (org-supertag-view-chat--insert-prompt)
        (setq org-supertag-view-chat--prompt-start nil)
        (cl-return-from org-supertag-view-chat-send-input))
      ;; /command 切换
      (let ((cmd-pair (org-supertag-view-chat--parse-command input)))
        (when cmd-pair
          (let ((cmd (car cmd-pair)))
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert (propertize "[System] " 'face 'font-lock-comment-face))
              (cond
               ((member cmd '("default" "reset"))
                (setq org-supertag-view-chat--current-command nil)
                (insert "Switched back to default chat mode\n"))
               ((or (assoc cmd org-supertag-view-chat--builtin-commands)
                    (gethash cmd org-supertag-view-chat--user-commands))
                (setq org-supertag-view-chat--current-command cmd)
                (insert (format "Switched to command: %s\n" cmd))
                (org-supertag-view-chat--show-current-command-prompt))
               (t
                (insert (format "Unknown command: %s\n" cmd)))))
          (org-supertag-view-chat--insert-prompt)
          (setq org-supertag-view-chat--prompt-start nil)
          (cl-return-from org-supertag-view-chat-send-input)))
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
            (when prompt-line-start
              (goto-char prompt-line-start)
              (delete-region prompt-line-start (line-end-position))
              (insert "* User\n")
              (insert (format "%s\n" input))
              (insert "\n")))
          (setq org-supertag-view-chat--response-start-marker (point-marker))
          (insert (propertize "Assistant is thinking..." 'face 'italic))
          ;; Variable replacement when sending
          (let* ((prompt (cond
                          ((and org-supertag-view-chat--current-command
                                (assoc org-supertag-view-chat--current-command org-supertag-view-chat--builtin-commands))
                           (cdr (assoc org-supertag-view-chat--current-command org-supertag-view-chat--builtin-commands)))
                          ((and org-supertag-view-chat--current-command
                                (gethash org-supertag-view-chat--current-command org-supertag-view-chat--user-commands))
                           (gethash org-supertag-view-chat--current-command org-supertag-view-chat--user-commands))
                          (t nil)))
                 (final-input (if (and prompt (string-match-p "\\$input" prompt))
                                  (replace-regexp-in-string "\\$input" input prompt)
                                input))
                 (payload `(("query" . ,final-input)
                            ("query_text" . ,final-input)
                            ("lang" . ,lang)
                            ("command" . ,org-supertag-view-chat--current-command)
                            ("history" . ,org-supertag-view-chat--conversation-history))))
            (org-supertag-bridge-call-async
             "rag/query"
             (list payload)
             #'org-supertag-view-chat--handle-response))
          (push (list :role "user" :content input) org-supertag-view-chat--conversation-history))
        (setq org-supertag-view-chat--prompt-start nil)
        (unless (and input (not (string-empty-p input)))
          (message "[SuperTag Chat] No input detected on current prompt line.")))))))

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
  (use-local-map org-supertag-view-chat-mode-map))

;; ;; --- Context Management ---
;; (defvar-local org-supertag-chat--current-node-context nil
;;   "Current org node context for Chat View commands.
;; Structure: (title content tags id filepath)")

;; (defun org-supertag-chat--collect-node-context ()
;;   "Silently collect current org node context and return it as a plist.
;; This should be called from the original Org buffer."
;;   (message "[Chat Debug] collect-node-context called.")
;;   (when (and (derived-mode-p 'org-mode) (org-at-heading-p))
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
  "Parse Chat View input for special commands."
  (message "[Chat Debug] parse-command input: %S" input)
  (when (string-match "^/\\([a-z-]+\\)\\(?:\\s-+\\(.*\\)\\)?" input)
    (let ((cmd (match-string 1 input))
          (args (string-trim (or (match-string 2 input) "")))) ; Ensure args is a string, not nil
      (message "[Chat Debug] parse-command matched - cmd: %S, args: %S" cmd args)
      (cons cmd args))))

;; --- Enhanced Send Logic ---
(defun org-supertag-view-chat--handle-command-response (response)
  "Handle response from commands and insert into buffer."
  (with-current-buffer (get-buffer-create org-supertag-view-chat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char org-supertag-view-chat--response-start-marker)
      (delete-region (point) (point-max))
      (insert "** Assistant\n")
      (insert (format "%s\n" (org-supertag-view-chat--md-to-org response)))
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
  (let ((formatted (replace-regexp-in-string "^" "> " conversation)))
    (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" formatted)))

(defun org-supertag-view-chat--save-as-new-file (conversation title)
  "Save conversation as new org file."
  (let ((filename (expand-file-name (concat title ".org") org-supertag-chat-save-directory)))
    (make-directory org-supertag-chat-save-directory t)
    (with-current-buffer (find-file-noselect filename)
      (insert (format "#+TITLE: %s\n#+DATE: %s\n#+TAGS: ai-conversation\n\n"
                      title (format-time-string "%Y-%m-%d")))
      (insert "* AI Conversation\n\n")
      (insert (org-supertag-view-chat--format-conversation conversation))
      (save-buffer)
      (message "Conversation saved to: %s" filename))))

(defun org-supertag-view-chat--save-append-to-node (conversation)
  "Append conversation to current org headline."
  (when (org-at-heading-p)
    (save-excursion
      (org-end-of-subtree t)
      (insert "\n\n* AI Assistant Conversation\n")
      (insert (format "#+CAPTION: Generated %s\n"
                      (format-time-string "%Y-%m-%d %H:%M")))
      (insert (org-supertag-view-chat--format-conversation conversation))
      (message "Conversation appended to current node"))))

(defun org-supertag-view-chat--save-as-subnode (conversation title)
  "Create new subnode under current headline."
  (when (org-at-heading-p)
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
  (let* ((title (read-string "Conversation title: "
                             (format "Chat-%s" (format-time-string "%Y%m%d"))))
         (choice (completing-read "Save conversation as: "
                                  '("new file" "append to current node" "create new subnode")
                                  nil t)))
    (pcase choice
      ("new file" (org-supertag-view-chat--save-as-new-file conversation title))
      ("append to current node" (org-supertag-view-chat--save-append-to-node conversation))
      ("create new subnode" (org-supertag-view-chat--save-as-subnode conversation title)))))

(defun org-supertag-view-chat--quick-save (conversation)
  "Save using default method without prompting."
  (let ((method (if (eq org-supertag-chat-default-save-method 'ask)
                    (completing-read "Save as: "
                                     '("new file" "append to current node" "create new subnode") nil t)
                  org-supertag-chat-default-save-method)))
    (pcase method
      ("new file" (org-supertag-view-chat--save-as-new-file
                   conversation (format "Chat-%s" (format-time-string "%Y%m%d"))))
      ("append to current node" (org-supertag-view-chat--save-append-to-node conversation))
      ("create new subnode" (org-supertag-view-chat--save-as-subnode
                            conversation (format "AI-Conversation-%s" (format-time-string "%Y%m%d")))))))

(define-key org-supertag-view-chat-mode-map (kbd "C-c C-s")
  (lambda ()
    (interactive)
    (let ((conversation (buffer-substring-no-properties (point-min) (point-max))))
      (org-supertag-view-chat--quick-save conversation))))

(provide 'org-supertag-view-chat)

;;; org-supertag-view-chat.el ends here

;;; org-supertag-view-chat.el --- Chat view for org-supertag -*- lexical-binding: t; -*-

(require 'org-supertag-bridge)
(require 'org-supertag-node)
(require 'org-supertag-db)
(require 'org-supertag-query)
(require 'org-supertag-node) ; Ensure node functions are available

(defcustom org-supertag-view-chat-buffer-name "*Org SuperTag Chat View*"
  "The name of the buffer for the chat view."
  :type 'string
  :group 'org-supertag)

;; --- Faces for Richer UI ---
(defface org-supertag-chat-label-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for labels like 'Analysis:' or 'User:'."
  :group 'org-supertag)

(defface org-supertag-chat-prompt-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the input prompt '>'."
  :group 'org-supertag)

(defvar-local org-supertag-view-chat--conversation-history nil
  "Buffer-local variable to store the conversation history.")

(defvar-local org-supertag-view-chat--source-node-id nil
  "The ID of the node that initiated the chat view session.")

;; --- MD to Org Conversion (Adapted from user's code) ---
(defun org-supertag-view-chat--md-to-org (md-string)
  "Convert a markdown string to an org-mode formatted string."
  (if (not (string-empty-p md-string))
      (with-temp-buffer
        (insert md-string)
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil t) (replace-match "\n\n"))
        (goto-char (point-min))
        (let ((code-blocks nil) (counter 0) block-start block-end lang content placeholder)
          (save-match-data
            (while (re-search-forward "```\\(.*?\\)\\(?:\n\\|\\s-\\)\\(\\(?:.\\|\n\\)*?\\)```" nil t)
              (setq lang (match-string 1) content (match-string 2)
                    block-start (match-beginning 0) block-end (match-end 0)
                    placeholder (format "CODE_BLOCK_PLACEHOLDER_%d" counter))
              (push (list placeholder lang content) code-blocks)
              (delete-region block-start block-end)
              (goto-char block-start)
              (insert placeholder)
              (setq counter (1+ counter))))
          (save-match-data (goto-char (point-min)) (while (re-search-forward "^\\([ \t]*\\)[*-+] \\(.*\\)$" nil t) (replace-match (concat (match-string 1) "- \\2"))))
          (save-match-data (goto-char (point-min)) (while (re-search-forward "\\*\\*\\([^ ]\\(.*?\\)[^ ]\\)\\*\\*" nil t) (replace-match "*\\1*")))
          (save-match-data (goto-char (point-min)) (while (re-search-forward "\\([ \n]\\)_\\([^ ].*?[^ ]\\)_\\([ \n]\\)" nil t) (replace-match "\\1/\\2/\\3")))
          (save-match-data (goto-char (point-min)) (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t) (replace-match "[[\\2][\\1]]")))
          (save-match-data (goto-char (point-min)) (while (re-search-forward "`\\(.*?\\)`" nil t) (replace-match "=\\1=")))
          (save-match-data (goto-char (point-min)) (while (re-search-forward "^\\(-{3,}\\|\\*{3,}\\)$" nil t) (replace-match "-----")))
          (save-match-data (goto-char (point-min)) (while (re-search-forward "!\\[.*?\\](\\(.*?\\))" nil t) (replace-match "[[\\1]]")))
          (save-match-data (goto-char (point-min)) (while (re-search-forward "^\\(#+\\) " nil t) (replace-match (make-string (length (match-string 1)) ?*) nil nil nil 1)))
          (save-match-data (dolist (block (nreverse code-blocks))
                             (let ((placeholder (nth 0 block)) (lang (nth 1 block)) (content (nth 2 block)))
                               (goto-char (point-min))
                               (when (search-forward placeholder nil t)
                                 (replace-match (format "#+begin_src %s\n%s#+end_src" lang content) t t))))))
        (buffer-string))
    ""))

(defun org-supertag-view-chat--insert-prompt ()
  "Insert the input prompt."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (propertize "> " 'face 'org-supertag-chat-prompt-face))))

(defun org-supertag-view-chat-insert-conversation ()
  "Format and insert the conversation into the source org node.
If a region is active in the chat buffer, insert only the selected text.
Otherwise, insert the entire conversation."
  (interactive)
  (let* ((chat-buffer (get-buffer org-supertag-view-chat-buffer-name))
         (source-node-id org-supertag-view-chat--source-node-id))
    (when (and chat-buffer source-node-id)
      (let* ((conversation-text
              (with-current-buffer chat-buffer
                (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (buffer-substring-no-properties (point-min) (point-max)))))
             (cleaned-text (string-trim (string-remove-suffix "\n> " conversation-text)))
             (formatted-text (format "\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" cleaned-text))
             (node-props (org-supertag-db-get source-node-id))
             (node-file (plist-get node-props :file-path))
             (node-location (and node-file (org-supertag-find-node-location source-node-id node-file))))
        ;; (message "Debug: node-id=%s, node-file=%s, node-location=%s"
        ;;          source-node-id node-file node-location)
        (when node-location
          (with-current-buffer (find-file-noselect node-file)
            (goto-char (car node-location))
            (goto-char (org-entry-end-position))
            (insert formatted-text)
            (save-buffer)
            (message "Conversation inserted into node %s in file %s"
                     source-node-id
                     (file-name-nondirectory node-file)))
          (kill-buffer chat-buffer))))))

(defun org-supertag-view-chat-send ()
  "Send the user's input to the backend to continue the conversation."
  (interactive)
  (with-current-buffer (get-buffer-create org-supertag-view-chat-buffer-name)
    (let ((inhibit-read-only t))
      (let* ((end (point-max))
             (prompt-pos (or (save-excursion (re-search-backward "\n> " nil t))
                             (save-excursion (re-search-backward "^> " nil t)))))
        (if prompt-pos
            (let* ((prompt-start (+ prompt-pos (if (eq prompt-pos (point-min)) 1 3))) ; Adjust start based on which search succeeded
                   (prompt (buffer-substring-no-properties prompt-start end))
                   (payload `(("conversation-history" . ,org-supertag-view-chat--conversation-history)
                              ("prompt" . ,prompt))))
              ;; Make the sent prompt read-only
              (put-text-property prompt-start end 'read-only t)
              (goto-char end)
              (insert "\n\n")
              (let ((fetching-pos (point)))
                (insert (propertize "Fetching response..." 'face 'italic))
                (put-text-property fetching-pos (point) 'read-only t))
              (org-supertag-bridge-call-async "rag/continue_conversation" payload
                                              #'org-supertag-view-chat--insert-response))
          (message "Could not find prompt to send."))))))

(defun org-supertag-view-chat--insert-response (result)
  "Callback function to insert a continued conversation response."
  (with-current-buffer (get-buffer-create org-supertag-view-chat-buffer-name)
    (let ((inhibit-read-only t))
      (setq org-supertag-view-chat--conversation-history (plist-get result :conversation-history))
      (when (search-backward "Fetching response..." nil t) (replace-match ""))
      (goto-char (point-max))
      (let ((response-org (org-supertag-view-chat--md-to-org (plist-get result :response))))
        (insert (propertize "Assistant:" 'face 'org-supertag-chat-label-face))
        (insert "\n")
        (insert response-org)
        (insert "\n"))
      ;; Make everything up to this point read-only
      (put-text-property (point-min) (point) 'read-only t)
      (org-supertag-view-chat--insert-prompt))))

(defun org-supertag-view-chat--insert-analysis (result)
  "Callback function to insert the analysis result into the chat buffer."
  (with-current-buffer (get-buffer-create org-supertag-view-chat-buffer-name)
    (let ((inhibit-read-only t))
      (setq org-supertag-view-chat--conversation-history (plist-get result :conversation-history))
      (when (search-backward "Fetching analysis from backend..." nil t) (replace-match ""))
      (goto-char (point-max))
      (let ((analysis-org (org-supertag-view-chat--md-to-org (plist-get result :analysis))))
        (insert (propertize "Analysis:" 'face 'org-supertag-chat-label-face))
        (insert "\n")
        (insert analysis-org)
        (insert "\n"))
      ;; Make everything up to this point read-only
      (put-text-property (point-min) (point) 'read-only t)
      (org-supertag-view-chat--insert-prompt))))

;;;###autoload
(define-derived-mode org-supertag-view-chat-mode text-mode "Org-ST-Chat"
  "Major mode for the chat view conversation."
  :group 'org-supertag
  :map (let ((map (make-sparse-keymap)))
         (define-key map (kbd "C-c C-c") #'org-supertag-view-chat-send)
         ;; (define-key map (kbd "C-c C-i") #'org-supertag-view-chat-insert-conversation)
         map)
  ;; (setq-local buffer-read-only t) ; Buffer is writable by default, managed by text properties
  (setq-local truncate-lines t)
  (setq-local header-line-format
        (propertize " Org SuperTag Chat View " 'face '(:weight bold))))

(defun org-supertag-view-chat-show (node-id)
  "Show the chat view window for a given NODE-ID, and fetch analysis."
  (let ((buffer (get-buffer-create org-supertag-view-chat-buffer-name))
        (node-props (org-supertag-db-get node-id)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-supertag-view-chat-mode)
        (setq org-supertag-view-chat--source-node-id node-id)
        (insert (propertize (format "Analyzing node: %s" node-id) 'face 'font-lock-comment-face))
        (insert "\n\n")
        (insert (propertize "Fetching analysis from backend..." 'face 'italic))))
    (display-buffer buffer)
    (when node-props
      ;; Convert plist to alist for Python compatibility
      (let ((payload-alist '()))
        (dotimes (i (/ (length node-props) 2))
          (let ((key (symbol-name (nth (* 2 i) node-props)))
                (value (nth (1+ (* 2 i)) node-props)))
            (push (cons key value) payload-alist)))
        ;; Pass the alist wrapped in a list
        (org-supertag-bridge-call-async "rag/analyze_note" (list payload-alist)
                                        #'org-supertag-view-chat--insert-analysis)))))

(defun org-supertag-advice-for-narrow-to-subtree (&rest _)
  "Advice for `org-toggle-narrow-to-subtree`."
  (when (buffer-narrowed-p)
    (save-excursion
      (goto-char (point-min))
      (when (org-at-heading-p)
        (when-let ((node-id (org-id-get)))
          (if (org-supertag-db-exists-p node-id)
              (org-supertag-view-chat-show node-id)
            (message "Org-supertag node does not exist in DB.")))))))

(advice-add 'org-toggle-narrow-to-subtree :after #'org-supertag-advice-for-narrow-to-subtree)

(provide 'org-supertag-view-chat)
;;; org-supertag-view-chat.el ends here 

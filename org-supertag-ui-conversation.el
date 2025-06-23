;;; org-supertag-ui-conversation.el --- UI for conversational features -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the user interface for the conversational AI,
;; including asking questions, changing modes, and viewing history.

;;; Code:

(require 'org-supertag-api)
(require 'org-supertag-ui-sidebar)
(require 'org-supertag-ui-archaeology) ;; For the /d slash command

(defvar org-supertag-ui-conversation-available-modes '("Normal" "Socratic")
  "A list of available conversational AI modes.")

(defvar org-supertag-ui-conversation--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "/") 'org-supertag-ui-conversation-handle-slash-command)
    map)
  "Keymap for the RAG dialogue input area.")


;; --- Conversational AI Modes ---

(defun org-supertag-ui-conversation-set-mode ()
  "Interactively set the conversational AI mode for the current session."
  (interactive)
  (let* ((session-id (with-current-buffer (org-supertag-ui-sidebar--get-buffer)
                       (org-supertag-ui-sidebar-get-session-id)))
         (mode (completing-read "Set Dialogue Mode: " org-supertag-ui-conversation-available-modes nil t nil nil "Normal")))
    (when (and mode (member mode org-supertag-ui-conversation-available-modes))
      (org-supertag-api-set-dialogue-mode
       session-id (downcase mode)
       (lambda (response)
         (let ((message
                (if (and (listp response) (eq (car response) :error))
                    (format "[SYSTEM_ERROR]: Could not set mode - %s" (plist-get response :message))
                  (format "[SYSTEM]: Dialogue mode set to %s." mode))))
           (org-supertag-ui-sidebar-render :dialogue (propertize (concat message "\n") 'face 'italic))))))))

(defun org-supertag-ui-conversation-handle-slash-command ()
  "Handle slash commands entered in the dialogue window."
  (interactive)
  (let* ((command-char (read-char "Slash command: "))
         (command-str (downcase (string command-char))))
    (cond
     ((string= command-str "m") ; /m for mode
      (call-interactively 'org-supertag-ui-conversation-set-mode))
     ((string= command-str "d") ; /d for dig
      (call-interactively 'org-supertag-ui-archaeology-dig))
     (t (message "Unknown slash command: /%c" command-char)))))


;; --- RAG Query UI ---

(defun org-supertag-ui-conversation--format-query-response (response query-string)
  "Format the RESPONSE from an API query and append it to the sidebar."
  (let* ((is-error (and (listp response) (eq (car response) :error)))
         (ai-text (if is-error
                      (plist-get response :message)
                    (if (and (listp response) (plist-get response :answer))
                        (plist-get response :answer)
                      (format "%S" response))))
         (metadata (unless is-error (plist-get response :metadata)))
         (hint (and metadata (plist-get metadata :hint)))
         (mode (and metadata (plist-get metadata :mode)))
         (formatted-message
          (propertize (format "[AI - %s]: %s\n" (or mode "response") ai-text) 'face 'font-lock-function-name-face)))

    (org-supertag-ui-sidebar-render :dialogue formatted-message)

    (when (and hint (not (string-empty-p hint)))
      (let* ((separator (propertize (format "\n%s\n" (make-string 35 ?─)) 'face 'font-lock-comment-face))
             (hint-header (propertize "💡 Hint from your notes:\n" 'face '(:inherit font-lock-comment-face :weight bold)))
             (hint-body (propertize (format "%s\n" hint) 'face 'font-lock-string-face)))
        (org-supertag-ui-sidebar-render :dialogue (concat separator hint-header hint-body))))))

(defun org-supertag-ui-conversation-ask-question (query-string)
  "Send QUERY-STRING to the RAG backend and display in the sidebar."
  (interactive (list (read-string "RAG Query: ")))
  (org-supertag-ui-sidebar-show)
  (let* ((session-id (with-current-buffer (org-supertag-ui-sidebar--get-buffer)
                       (org-supertag-ui-sidebar-get-session-id)))
         (user-message (propertize (format "[USER]: %s\n" query-string) 'face 'bold)))
    (org-supertag-ui-sidebar-render :dialogue user-message)

    (org-supertag-api-ask-question
     query-string session-id
     (lambda (response)
       (org-supertag-ui-conversation--format-query-response response query-string)))))


;; --- Dialogue History UI ---

(defun org-supertag-ui-conversation--format-history (history-response)
  "Format the HISTORY-RESPONSE for display."
  (if (null history-response)
      (insert (propertize "[AI]: No dialogue history data received.\n" 'face 'italic))
    (let ((turns (if (listp history-response) (plist-get history-response :turns) nil)))
      (if (or (null turns) (seq-empty-p turns))
          (insert (propertize "[AI]: No dialogue turns in history record.\n" 'face 'italic))
        (dolist (turn turns)
          (let* ((speaker (plist-get turn :speaker))
                 (text (plist-get turn :text))
                 (face (if (string= speaker "user") 'bold 'font-lock-function-name-face)))
            (insert (propertize (format "[%s]: %s\n" (upcase speaker) text) 'face face))))))))

(defun org-supertag-ui-conversation-show-history (&optional count)
  "Retrieve and display dialogue history in the sidebar's dialogue window."
  (interactive "P")
  (org-supertag-ui-sidebar-show)
  (let ((session-id (with-current-buffer (org-supertag-ui-sidebar--get-buffer)
                      (org-supertag-ui-sidebar-get-session-id))))
    (org-supertag-ui-sidebar-render
     :dialogue (lambda () (insert (propertize "[AI]: Fetching dialogue history...\n" 'face 'italic))) t)

    (org-supertag-api-get-dialogue-history
     session-id (if (integerp count) count nil)
     (lambda (response)
       (org-supertag-ui-sidebar-render
        :dialogue
        (lambda ()
          (if (and (listp response) (eq (car response) :error))
              (insert (propertize (format "[AI_ERROR]: Could not retrieve history - %s\n" (plist-get response :message)) 'face 'error))
            (org-supertag-ui-conversation--format-history response)))
        t)))))


(provide 'org-supertag-ui-conversation)
;;; org-supertag-ui-conversation.el ends here 
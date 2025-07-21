;;; org-supertag-smart-companion.el --- Smart Knowledge Companion for org-supertag -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Yibie

;; Author: Yibie
;; Keywords: org-mode, tags, ai, knowledge-companion
;; Version: 1.0.0

;;; Commentary:

;; This module provides an intelligent knowledge companion that analyzes
;; tag context and provides smart suggestions to help users discover
;; related concepts and expand their knowledge graph.

;; Key features:
;; - Seamless integration with existing tag operations
;; - Unobtrusive, context-aware suggestions
;; - Intelligent analysis of tag relationships
;; - Progressive disclosure of information
;; - Sync scope awareness (only suggests tags for files in org-supertag-sync scope)
;; - Non-intrusive notification system (shows hints in echo area)

;; Usage:
;; - C-c t s - Show tag suggestions for current node
;; - M-x org-supertag-smart-companion-suggest-here - Manually trigger suggestions
;; - M-x org-supertag-smart-companion-toggle - Toggle companion functionality
;; - M-x org-supertag-smart-companion-toggle-quiet-mode - Toggle quiet mode

;; Configuration:
;; - org-supertag-smart-companion-sync-scope-only - When t, only suggests tags for files in sync scope
;; - org-supertag-smart-companion-auto-popup - When t, automatically shows suggestion window
;; - org-supertag-smart-companion-quiet-mode - When t, reduces visual notifications

;;; Code:

(require 'org-supertag-api)
(require 'org-supertag-bridge)
(require 'org-supertag-node)
(require 'org-supertag-auto-tag)
(require 'org-supertag-sync)

;;; Customization

(defgroup org-supertag-smart-companion nil
  "Smart knowledge companion for org-supertag."
  :group 'org-supertag
  :prefix "org-supertag-smart-companion-")

(defcustom org-supertag-smart-companion-enabled t
  "Whether to enable the smart knowledge companion functionality."
  :type 'boolean
  :group 'org-supertag-smart-companion)

(defcustom org-supertag-smart-companion-delay 2.0
  "Delay in seconds before analyzing tag context after tag addition."
  :type 'number
  :group 'org-supertag-smart-companion)

(defcustom org-supertag-smart-companion-context-size 5
  "Number of related concepts to analyze for context."
  :type 'integer
  :group 'org-supertag-smart-companion)

(defcustom org-supertag-smart-companion-suggestion-threshold 0.5
  "Minimum confidence threshold for showing suggestions."
  :type 'number
  :group 'org-supertag-smart-companion)

(defcustom org-supertag-smart-companion-auto-popup nil
  "Whether to automatically popup the suggestion window when suggestions are found."
  :type 'boolean
  :group 'org-supertag-smart-companion)

(defcustom org-supertag-smart-companion-quiet-mode nil
  "When enabled, reduces visual and audible notifications to be less intrusive."
  :type 'boolean
  :group 'org-supertag-smart-companion)

(defcustom org-supertag-smart-companion-idle-interval 10.0
  "Interval in seconds for idle detection. Longer intervals are less intrusive."
  :type 'number
  :group 'org-supertag-smart-companion)

(defcustom org-supertag-smart-companion-session-limit 3
  "Maximum number of suggestions per session to avoid overwhelming the user."
  :type 'integer
  :group 'org-supertag-smart-companion)

(defcustom org-supertag-smart-companion-sync-scope-only t
  "When non-nil, only provide tag suggestions for files in org-supertag-sync scope.
When nil, provide suggestions for all org files regardless of sync status."
  :type 'boolean
  :group 'org-supertag-smart-companion)

(defcustom org-supertag-smart-companion-suggest-chat-on-active-node t
  "When non-nil, proactively suggest opening the Chat View on active nodes."
  :type 'boolean
  :group 'org-supertag-smart-companion)

;;; Faces

(defface org-supertag-smart-companion-hint-face
  '((t :inherit shadow :weight normal))
  "Face for smart companion hint messages."
  :group 'org-supertag-smart-companion)

(defface org-supertag-smart-companion-suggestion-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for smart companion suggestions."
  :group 'org-supertag-smart-companion)

;;; Variables

(defvar-local org-supertag-smart-companion--last-suggestion nil
  "The most recent suggestion data from the smart companion.")

(defvar-local org-supertag-smart-companion--analysis-timer nil
  "Timer for delayed analysis after tag addition.")

(defvar org-supertag-smart-companion--suggestion-buffer "*Org SuperTag Smart Suggestions*"
  "Buffer name for displaying detailed suggestions.")

(defvar org-supertag-smart-companion--analysis-status nil
  "Current analysis status: nil, 'analyzing, 'completed, 'error.")

(defvar org-supertag-smart-companion--status-message-timer nil
  "Timer for status message display.")

(defvar org-supertag-smart-companion--suggestion-position nil
  "Position where suggestions were generated, for applying tags.")

(defvar org-supertag-smart-companion--session-suggestion-count 0
  "Number of suggestions made in current session to avoid overwhelming the user.")

(defvar org-supertag-smart-companion--last-suggestion-time nil
  "Time of last suggestion to avoid repeated suggestions.")

(defvar org-supertag-smart-companion--suggested-nodes nil
  "List of node IDs that have already been suggested to avoid repetition.")

(defvar org-supertag-smart-companion--last-suggestion-type nil
  "The type of the last suggestion made (:tag or :chat).")

;;; Core Functions

(defun org-supertag-smart-companion--analyze-tag-async (tag-name node-id)
  "Asynchronously analyze tag context for TAG-NAME at NODE-ID."
  (when (and org-supertag-smart-companion-enabled
             (org-supertag-bridge-ready-p))
    (setq org-supertag-smart-companion--analysis-status 'analyzing)
    (org-supertag-smart-companion--show-status-briefly "🔍 Analyzing tag context...")
    (let ((payload `(("tag_name" . ,tag-name)
                     ("node_id" . ,node-id)
                     ("context_size" . ,org-supertag-smart-companion-context-size))))
      (org-supertag-bridge-call-async
       "smart_companion/analyze_tag_context"
       (list payload)
       #'org-supertag-smart-companion--handle-suggestion))))

(defun org-supertag-smart-companion--show-status-briefly (message &optional duration)
  "Show a brief status MESSAGE for DURATION seconds (default 1.5)."
  (when org-supertag-smart-companion--status-message-timer
    (cancel-timer org-supertag-smart-companion--status-message-timer))
  (let ((propertized-message (propertize message 'face 'org-supertag-smart-companion-hint-face)))
    (message propertized-message)
    (setq org-supertag-smart-companion--status-message-timer
          (run-with-timer (or duration 5.0) nil
                          (lambda ()
                            (when (equal (current-message) propertized-message)
                              (message nil)))))))

(defun org-supertag-smart-companion--show-tag-suggestion-hint (count)
  "Show a gentle hint about COUNT tag suggestions."
  (let ((msg (format "💡 Found %d tag suggestions for this node. Press C-c t s to show." count)))
    (org-supertag-smart-companion--show-status-briefly msg)))

(defun org-supertag-smart-companion--handle-tag-suggestions (results)
  "Handle tag suggestions from auto-tag backend and display them."
  (message "Smart Companion: Processing tag suggestion results...")
  (when results
    (setq org-supertag-smart-companion--last-suggestion results)
    (let ((count (length results)))
      (when (> count 0)
        (org-supertag-smart-companion--show-tag-suggestion-hint count)))))

;;; Smart Context Detection

(defun org-supertag-smart-companion--at-headline-p ()
  "Check if cursor is at org headline or in subtree."
  (and (derived-mode-p 'org-mode)
       (save-excursion
         (condition-case nil
             (progn
               (org-back-to-heading t)
               t)
           (error nil)))))

(defun org-supertag-smart-companion--has-inline-tags-p ()
  "Check if current headline's subtree contains inline tags."
  (save-excursion
    (org-back-to-heading t)
    (let ((subtree-end (save-excursion (org-end-of-subtree t t) (point))))
      (re-search-forward "#[a-zA-Z0-9_@%一-龥-]+" subtree-end t))))

(defun org-supertag-smart-companion--has-sufficient-content-p ()
  "Check if current node has sufficient content for tag suggestions."
  (save-excursion
    (org-back-to-heading t)
    (let* ((node-data (list :file-path (buffer-file-name)
                           :pos (point)
                           :title (org-get-heading t t t t)))
           (content (org-supertag-auto-tag--get-node-content node-data)))
      (and content
           (>= (length content) org-supertag-auto-tag-batch-min-content-length)))))

(defun org-supertag-smart-companion--should-suggest-p (context)
  "Determine if suggestions should be provided based on anti-distraction logic."
  (let* ((current-time (current-time))
         (node-id (plist-get context :node-id))
         (time-since-last (if org-supertag-smart-companion--last-suggestion-time
                             (float-time (time-subtract current-time
                                                       org-supertag-smart-companion--last-suggestion-time))
                           most-positive-fixnum)))
    (and
     (< org-supertag-smart-companion--session-suggestion-count
        org-supertag-smart-companion-session-limit)
     (> time-since-last 30.0)
     (not (member node-id org-supertag-smart-companion--suggested-nodes))
     (or (not org-supertag-smart-companion-quiet-mode)
         (> time-since-last 120.0)))))

(defun org-supertag-smart-companion--provide-chat-suggestion (context)
  "Provide a chat suggestion for the given CONTEXT."
  (let ((node-id (plist-get context :node-id)))
    (setq org-supertag-smart-companion--last-suggestion-time (current-time))
    (setq org-supertag-smart-companion--session-suggestion-count
          (1+ org-supertag-smart-companion--session-suggestion-count))
    (setq org-supertag-smart-companion--last-suggestion-type :chat)
    (when node-id
      (push node-id org-supertag-smart-companion--suggested-nodes))
    (org-supertag-smart-companion--show-status-briefly
     "💡 Smart Companion has ideas for this node. Press C-c t s to explore.")))

(defun org-supertag-smart-companion--provide-context-suggestions (context)
  "Provide tag suggestions based on context for untagged nodes."
  (setq org-supertag-smart-companion--last-suggestion-type :tag)
  (setq org-supertag-smart-companion--last-suggestion-time (current-time))
  (setq org-supertag-smart-companion--session-suggestion-count
        (1+ org-supertag-smart-companion--session-suggestion-count))
  (let ((node-id (plist-get context :node-id)))
    (when node-id
      (push node-id org-supertag-smart-companion--suggested-nodes)))
  (unless org-supertag-smart-companion-quiet-mode
    (org-supertag-smart-companion--show-status-briefly "🔍 Analyzing content for tag suggestions..."))
  (org-supertag-smart-companion--suggest-tags-for-current-headline))

(defun org-supertag-smart-companion--check-and-suggest ()
  "Check current context and provide suggestions if appropriate."
  (when (and org-supertag-smart-companion-enabled
             (derived-mode-p 'org-mode))
    (let ((context (org-supertag-smart-companion--analyze-current-context)))
      (when (org-supertag-smart-companion--should-suggest-p context)
        (cond
         ((plist-get context :should-suggest-chat)
          (org-supertag-smart-companion--provide-chat-suggestion context))
         ((plist-get context :should-suggest)
          (org-supertag-smart-companion--provide-context-suggestions context)))))))

(defun org-supertag-smart-companion--analyze-current-context ()
  "Analyze current context to determine if suggestions should be provided."
  (save-excursion
    (let* ((at-headline (org-supertag-smart-companion--at-headline-p))
           (node-id (when at-headline
                      (org-back-to-heading t)
                      (org-entry-get nil "ID")))
           (existing-tags (when node-id (org-supertag-node-get-tags node-id)))
           (has-inline-tags (when at-headline
                              (org-supertag-smart-companion--has-inline-tags-p)))
           (is-untagged (and at-headline
                             (not existing-tags)
                             (not has-inline-tags)))
           (has-sufficient-content (when at-headline
                                     (org-supertag-smart-companion--has-sufficient-content-p)))
           (file-in-sync-scope (and buffer-file-name
                                    (fboundp 'org-supertag-sync--in-sync-scope-p)
                                    (org-supertag-sync--in-sync-scope-p buffer-file-name)))
           (enforce-sync-scope org-supertag-smart-companion-sync-scope-only)
           (is-active-node (and at-headline node-id existing-tags has-sufficient-content)))
      (list :at-headline at-headline
            :node-id node-id
            :existing-tags existing-tags
            :has-inline-tags has-inline-tags
            :is-untagged is-untagged
            :has-sufficient-content has-sufficient-content
            :file-in-sync-scope file-in-sync-scope
            :should-suggest (and is-untagged
                                 has-sufficient-content
                                 (or (not enforce-sync-scope)
                                     file-in-sync-scope))
            :should-suggest-chat (and is-active-node
                                      org-supertag-smart-companion-suggest-chat-on-active-node
                                      (or (not enforce-sync-scope)
                                          file-in-sync-scope))))))

(defun org-supertag-smart-companion--suggest-tags-for-current-headline ()
  "Suggest tags for current headline using auto-tag infrastructure."
  (save-excursion
    (org-back-to-heading t)
    (let* ((node-id (org-id-get-create))
           (node-data (list :file-path (buffer-file-name)
                           :pos (point)
                           :title (org-get-heading t t t t)))
           (content (org-supertag-auto-tag--get-node-content node-data)))
      (when (and content
                 (>= (length content) org-supertag-auto-tag-batch-min-content-length))
        (let* ((node-dict `(("id" ,node-id)
                           ("content" ,content)))
               (model-config (org-supertag-auto-tag--get-model-config))
               (payload `(("nodes" ,(list node-dict))
                         ("model_config" ,model-config))))
          (setq org-supertag-smart-companion--suggestion-position
                (list :file (buffer-file-name) :pos (point)))
          (org-supertag-api-batch-generate-tags
           (list payload)
           #'org-supertag-smart-companion--handle-tag-suggestions))))))

;;; Legacy Tag Addition Hook (保留兼容性)

(defun org-supertag-smart-companion--on-tag-added (tag-name)
  "Handle tag addition event for TAG-NAME with intelligent analysis."
  (when org-supertag-smart-companion-enabled
    (run-with-timer 1.0 nil
                    (lambda ()
                      (org-supertag-smart-companion--check-and-suggest)))))

;;; Interactive Commands

;;;###autoload
(defun org-supertag-smart-companion-show-suggestions ()
  "Show the last suggestion, whether it was for tags or chat."
  (interactive)
  (cond
   ((eq org-supertag-smart-companion--last-suggestion-type :chat)
    (require 'org-supertag-view-chat)
    (org-supertag-view-chat-open))
   ((eq org-supertag-smart-companion--last-suggestion-type :tag)
    (if org-supertag-smart-companion--last-suggestion
        (org-supertag-auto-tag-show-suggestion-ui org-supertag-smart-companion--last-suggestion)
      (message "No tag suggestions available. Run M-x org-supertag-smart-companion-suggest-here to get new suggestions.")))
   (t (message "No suggestions available at the moment."))))

;;;###autoload
(defun org-supertag-smart-companion-suggest-here ()
  "Analyze current location and provide intelligent tag suggestions for untagged nodes."
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (message "Smart Companion: Can only be used in org-mode")
    (let ((context (org-supertag-smart-companion--analyze-current-context)))
      (message "Smart Companion DEBUG: at-headline=%s, node-id=%s, is-untagged=%s, has-sufficient-content=%s, file-in-sync-scope=%s"
               (plist-get context :at-headline)
               (plist-get context :node-id)
               (plist-get context :is-untagged)
               (plist-get context :has-sufficient-content)
               (plist-get context :file-in-sync-scope))
      (cond
       ((not (plist-get context :at-headline))
        (message "Smart Companion: Please move to an org headline or its subtree"))
       ((not (plist-get context :is-untagged))
        (message "Smart Companion: This node already has tags. Smart Companion suggests tags for untagged nodes."))
       ((not (plist-get context :has-sufficient-content))
        (message "Smart Companion: Node content is too short for meaningful tag suggestions"))
       ((and org-supertag-smart-companion-sync-scope-only
             (not (plist-get context :file-in-sync-scope)))
        (message "Smart Companion: Current file is not in org-supertag-sync scope (set org-supertag-smart-companion-sync-scope-only to nil to override)"))
       (t
        (message "Smart Companion: Analyzing node content for tag suggestions...")
        (org-supertag-smart-companion--provide-context-suggestions context))))))

;;;###autoload
(defun org-supertag-smart-companion-toggle ()
  "Toggle the smart companion functionality."
  (interactive)
  (setq org-supertag-smart-companion-enabled
        (not org-supertag-smart-companion-enabled))
  (message "Smart Companion %s"
           (if org-supertag-smart-companion-enabled "enabled" "disabled")))

;;;###autoload
(defun org-supertag-smart-companion-reset-session ()
  "Reset Smart Companion session counters and suggested node tracking."
  (interactive)
  (setq org-supertag-smart-companion--session-suggestion-count 0
        org-supertag-smart-companion--last-suggestion-time nil
        org-supertag-smart-companion--suggested-nodes nil)
  (message "Smart Companion session reset. All nodes can receive suggestions again."))

;;;###autoload
(defun org-supertag-smart-companion-toggle-quiet-mode ()
  "Toggle quiet mode for Smart Companion to reduce distractions."
  (interactive)
  (setq org-supertag-smart-companion-quiet-mode
        (not org-supertag-smart-companion-quiet-mode))
  (message "Smart Companion quiet mode %s"
           (if org-supertag-smart-companion-quiet-mode "enabled" "disabled")))

;;; Integration with org-supertag-tag

(defun org-supertag-smart-companion--advice-tag-add (orig-fun &rest args)
  "Advice function to hook into tag addition for smart companion analysis."
  (let ((result (apply orig-fun args)))
    (when (and result org-supertag-smart-companion-enabled)
      (run-with-timer 0.1 nil
                      (lambda ()
                        (org-supertag-smart-companion--analyze-recent-tag-addition))))
    result))

(defun org-supertag-smart-companion--analyze-recent-tag-addition ()
  "Analyze the most recently added tag for smart companion suggestions."
  (when (derived-mode-p 'org-mode)
    (let* ((node-id (org-id-get))
           (tags (when node-id (org-supertag-node-get-tags node-id))))
      (when (and tags (> (length tags) 0))
        (let ((trigger-tag (car tags)))
          (org-supertag-smart-companion--on-tag-added trigger-tag))))))

;;; Key Bindings

(defun org-supertag-smart-companion-setup-keybindings (&optional prefix)
  "Optionally set up key bindings for smart companion."
  (interactive)
  (define-key org-mode-map (kbd "C-c t s") 'org-supertag-smart-companion-show-suggestions))

;;; Setup and Activation

;;;###autoload
(defun org-supertag-smart-companion-setup ()
  "Set up the smart companion functionality."
  (interactive)
  (advice-add 'org-supertag-inline-add :around
              #'org-supertag-smart-companion--advice-tag-add)
  (when org-supertag-smart-companion-enabled
    (org-supertag-smart-companion--setup-idle-detection))
  (org-supertag-smart-companion-setup-keybindings)
  (message "Smart Companion functionality has been set up. Use C-c t s to show suggestions."))

(defun org-supertag-smart-companion--setup-idle-detection ()
  "Set up idle detection to provide suggestions at appropriate times."
  (run-with-idle-timer org-supertag-smart-companion-idle-interval t
                       (lambda ()
                         (when (and org-supertag-smart-companion-enabled
                                    (derived-mode-p 'org-mode))
                           (org-supertag-smart-companion--check-and-suggest)))))

;;;###autoload
(defun org-supertag-smart-companion-cleanup ()
  "Clean up the smart companion functionality."
  (interactive)
  (advice-remove 'org-supertag-inline-add
                 #'org-supertag-smart-companion--advice-tag-add)
  (when org-supertag-smart-companion--analysis-timer
    (cancel-timer org-supertag-smart-companion--analysis-timer)
    (setq org-supertag-smart-companion--analysis-timer nil))
  (when (get-buffer org-supertag-smart-companion--suggestion-buffer)
    (kill-buffer org-supertag-smart-companion--suggestion-buffer))
  (message "Smart Companion functionality has been cleaned up."))



(provide 'org-supertag-smart-companion)

;;; org-supertag-smart-companion.el ends here
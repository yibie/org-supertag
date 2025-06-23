;;; org-supertag-ui.el --- Main entry point for the org-supertag UI -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is the main entry point for all org-supertag user interfaces.
;; It loads all the individual UI modules and provides a single, unified
;; command to refresh the entire intelligent sidebar.

;;; Code:

(require 'org-supertag-ui-sidebar)
(require 'org-supertag-ui-conversation)
(require 'org-supertag-ui-ner)
(require 'org-supertag-ui-archaeology)
(require 'org-supertag-ui-memory)
(require 'org-supertag-proactive-engine)
(require 'org-supertag-util)

;;;###autoload
(defun org-supertag-ui-refresh-sidebar ()
  "Refresh the entire intelligent sidebar with context from the current node.
This is the main, unified entry point for the UI."
  (interactive)
  (org-supertag-ui-sidebar-show) ; Ensure the sidebar is visible first

  ;; Clear all sections by rendering a loading message
  (dolist (section-cons org-supertag-ui-sidebar-sections)
      (org-supertag-ui-sidebar-render
       (car section-cons)
       (lambda () (insert (propertize "  (Loading...)\n" 'face 'italic)))
       t)) ; t means clear the section

  ;; Asynchronously call the refresh/show functions from each module
  ;; Note: These functions are asynchronous (they take callbacks),
  ;; so they will not block each other.
  
  ;; 1. Conversation & Associated Nodes (part of conversation now)
  ;; This will ask a default question based on context
  (let ((context-query (org-supertag-util-get-node-content-for-llm)))
    (org-supertag-ui-conversation-ask-question (format "Summarize this note and find related content: %s" context-query)))

  ;; 2. NER Features
  (org-supertag-ui-ner-suggest-tags)
  (org-supertag-ui-ner-discover-relationships)

  ;; 3. Memory Candidates
  (org-supertag-ui-memory-show-candidates)

  ;; 4. Proactive Engine
  (org-supertag-proactive-engine-activate)

  (message "Intelligent Sidebar refresh initiated."))


(provide 'org-supertag-ui)
;;; org-supertag-ui.el ends here
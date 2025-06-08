;;; org-supertag-ui-memory.el --- UI for memory synthesis features -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the user interface for reviewing and managing
;; candidate memories synthesized by the MemorySynthesizer.

;;; Code:

(require 'org-supertag-api)
(require 'org-supertag-ui-sidebar)

;; --- UI Formatting ---

(defun org-supertag-ui-memory--format-candidates (candidates)
  "Format memory CANDIDATES for display in the sidebar.
This function is designed to be called by `org-supertag-ui-sidebar-render`."
  (if (seq-empty-p candidates)
      (insert "  (No pending memory candidates to review.)\n")
    (dolist (candidate candidates)
      (let* ((candidate-id (plist-get candidate :candidate_id))
             (proposed-item (plist-get candidate :proposed_item))
             (item-content (format "%S" (plist-get proposed-item :content))) ; Pretty-print content
             (justification (plist-get candidate :justification))
             (confidence (plist-get candidate :confidence_score)))

        (insert (propertize (format "  - Proposed Memory (Confidence: %.2f):\n" confidence)
                            'face '(:weight bold :foreground "orange")))
        (insert (format "    Content: %s\n" item-content))
        (insert (format "    Justification: %s\n" justification))
        (insert "    ")
        ;; Accept button
        (insert (propertize "[Accept]" 'face '(:foreground "green" :underline t)
                            'action (lambda (_)
                                      (org-supertag-api-process-candidate-memory candidate-id "accept")
                                      (message "Accepted memory candidate: %s" candidate-id)
                                      (org-supertag-ui-memory-show-candidates))
                            'follow-link t
                            'help-echo "Click to accept and store this memory."))
        (insert " ")
        ;; Reject button
        (insert (propertize "[Reject]" 'face '(:foreground "red" :underline t)
                            'action (lambda (_)
                                      (org-supertag-api-process-candidate-memory candidate-id "reject")
                                      (message "Rejected memory candidate: %s" candidate-id)
                                      (org-supertag-ui-memory-show-candidates))
                            'follow-link t
                            'help-echo "Click to reject this memory candidate."))
        (insert "\n\n")))))

;;;###autoload
(defun org-supertag-ui-memory-show-candidates ()
  "Fetch and display pending memory candidates for user review in the sidebar."
  (interactive)
  (let ((response (org-supertag-api-get-candidate-memories)))
    (org-supertag-ui-sidebar-show) ; Ensure sidebar is visible
    (org-supertag-ui-sidebar-render
     :memory
     (lambda ()
       (if (and (eq (car response) 'success)
                (plist-get (cdr response) :result))
           (let ((candidates (plist-get (cdr response) :result)))
             (org-supertag-ui-memory--format-candidates candidates))
         (insert (propertize "Error fetching memory candidates.\n" 'face '(:foreground "red")))
         (insert (format "%S" response))))
     t))) ; t means clear the section

(provide 'org-supertag-ui-memory)
;;; org-supertag-ui-memory.el ends here 
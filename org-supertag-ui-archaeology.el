;;; org-supertag-ui-archaeology.el --- UI for knowledge archaeology features -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the user interface for the Knowledge Archaeology
;; feature, allowing users to \"dig\" for the history of a concept.

;;; Code:

(require 'org-supertag-api)
(require 'org-supertag-ui-sidebar)
(require 'seq)

;; --- UI Formatting ---

(defun org-supertag-ui-archaeology--format-results (response)
  "Format the archaeology dig RESPONSE into a string for display."
  (let ((results (if (and (listp response) (eq (car response) 'success))
                     (plist-get (cdr response) :result)
                   nil)))
    (if (or (null results) (seq-empty-p results))
        (insert (propertize "No relevant historical notes found for this topic." 'face 'italic))
      (dolist (node-info results)
        (let ((title (plist-get node-info :title))
              (date (plist-get node-info :document_date)))
          (insert (propertize (format "▪ %s (%s)\n" (or title "Untitled") (or date "No Date"))
                              'face 'default)))))))

;;;###autoload
(defun org-supertag-ui-archaeology-dig ()
  "Interactively perform a knowledge archaeology dig and display results."
  (interactive)
  (let ((query (read-string "Knowledge Archaeology - Dig for: ")))
    (unless (string-empty-p query)
      (org-supertag-ui-sidebar-show) ; Ensure sidebar is visible
      (org-supertag-ui-sidebar-render
       :archaeology
       (lambda ()
         (insert (propertize (format "Digging for '%s'..." query) 'face 'italic)))
       t) ; Clear section and show placeholder

      (org-supertag-api-knowledge-archaeology-dig
       query
       (lambda (response)
         (org-supertag-ui-sidebar-render
          :archaeology
          (lambda ()
            (if (and (listp response) (eq (car response) :error))
                (insert (propertize (format "Error during dig: %s" (plist-get response :message)) 'face 'error))
              (org-supertag-ui-archaeology--format-results response)))
          t))))))


(provide 'org-supertag-ui-archaeology)
;;; org-supertag-ui-archaeology.el ends here 
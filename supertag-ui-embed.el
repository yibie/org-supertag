;;; supertag-ui-embed.el --- UI functions for embed blocks -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides UI-related functions for embed blocks in org-supertag.
;; It handles finding blocks, generating content, and UI operations.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'supertag-core-store)

(defun supertag-ui-embed-find-block-by-source (source-id)
  "Find embed block by source ID using regex search.
SOURCE-ID: The source node ID to find.
Returns (start . end) for the entire block (including begin/end markers), or nil if not found."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+begin_embed:\\s-+%s"
                                    (regexp-quote source-id)) nil t)
      (let ((start (match-beginning 0)))
        (when (re-search-forward "^#\\+end_embed" nil t)
          (cons start (match-end 0)))))))

(defun supertag-ui-embed-get-inner-block-region (source-id)
  "Get the inner content region of an embed block, excluding begin/end markers.
SOURCE-ID: The source node ID
Returns (start . end) for the inner content region, or nil if not found."
  (let ((block-region (supertag-ui-embed-find-block-by-source source-id)))
    (when block-region
      (save-excursion
        (goto-char (car block-region)) ; Go to the start of the #+begin_embed line
        (forward-line 1) ; Move past the #+begin_embed line
        (let ((inner-start (point)))
          (goto-char (cdr block-region)) ; Go to the end of the #+end_embed line
          (re-search-backward "^#\\+end_embed" (car block-region) t) ; Find the start of the #+end_embed line
          (let ((inner-end (point)))
            (when (>= inner-end inner-start)
              (cons inner-start inner-end))))))))

(defun supertag-ui-embed--filter-embed-markers (content)
  "Remove embed block markers and ID properties from content to prevent nested embeds and ID conflicts.
CONTENT: The content string to filter"
  (let ((filtered-content content))
    ;; Remove embed block begin markers
    (setq filtered-content
          (replace-regexp-in-string "^#\\+begin_embed:.*$" "" filtered-content))
    ;; Remove embed block end markers
    (setq filtered-content
          (replace-regexp-in-string "^#\\+end_embed.*$" "" filtered-content))
    ;; Remove ID properties to prevent conflicts
    (setq filtered-content
          (replace-regexp-in-string "^[ \t]*:ID:[ \t]+[a-zA-Z0-9-]+[ \t]*$" "" filtered-content))
    ;; Clean up multiple consecutive newlines
    (setq filtered-content
          (replace-regexp-in-string "\n\n\n+" "\n\n" filtered-content))
    ;; Trim leading and trailing whitespace but preserve internal structure
    (string-trim filtered-content)))

(cl-defun supertag-ui-embed-generate-node-content (node-id)
  "Generate embed content for a single node.
NODE-ID: The node identifier to embed"
  (let* ((node-data (supertag-get (list :nodes node-id))))
    (unless node-data
      (cl-return-from supertag-ui-embed-generate-node-content
        "** Error: Node not found in DB **"))
    
    (let* ((title (plist-get node-data :title))
           (content (plist-get node-data :content))
           (level (or (plist-get node-data :level) 1))
           (heading-prefix (make-string level ?*)))
      
      ;; Generate heading and content
      (let* ((heading-line (format "%s %s" heading-prefix (or title "Untitled")))
             (content-text (if (and content (not (string-empty-p (string-trim content))))
                               content
                             ""))
             (full-content (if (string-empty-p content-text)
                               heading-line
                             (concat heading-line "\n" content-text)))
             (normalized-content (replace-regexp-in-string "\n*\\'" "\n" full-content))
             (filtered-content (supertag-ui-embed--filter-embed-markers normalized-content))
             (clean-content (string-trim-right filtered-content)))
        
        ;; Ensure content ends with exactly one newline
        (if (string-suffix-p "\n" clean-content)
            clean-content
          (concat clean-content "\n"))))))

(provide 'supertag-ui-embed)
;;; supertag-ui-embed.el ends here

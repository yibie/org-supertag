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
NODE-ID: The node identifier to embed.
Returns ONLY the content part, without the headline."
  (let* ((node-data (supertag-get (list :nodes node-id))))
    (unless node-data
      (cl-return-from supertag-ui-embed-generate-node-content
        "** Error: Node not found in DB **"))
    
    (let* ((content (plist-get node-data :content)))
      ;; Return only the content, filtered and cleaned
      (let* ((content-text (if (and content (not (string-empty-p (string-trim content))))
                               content
                             ""))
             (normalized-content (replace-regexp-in-string "\n*\\'" "\n" content-text))
             (filtered-content (supertag-ui-embed--filter-embed-markers normalized-content))
             (clean-content (string-trim-right filtered-content)))
        
        ;; Ensure content ends with exactly one newline if not empty
        (if (string-empty-p clean-content)
            ""
          (if (string-suffix-p "\n" clean-content)
              clean-content
            (concat clean-content "\n")))))))

(defun supertag-ui-embed-get-link-at-point ()
  "Get the org-link at point and extract its ID.
Returns a plist with :id, :description, :begin, and :end, or nil if not on a link."
  (let ((element (org-element-context)))
    (when (eq (org-element-type element) 'link)
      (let* ((link-type (org-element-property :type element))
             (path (org-element-property :path element))
             (begin (org-element-property :begin element))
             (end (org-element-property :end element))
             (contents-begin (org-element-property :contents-begin element))
             (contents-end (org-element-property :contents-end element))
             (description (when (and contents-begin contents-end)
                           (buffer-substring-no-properties contents-begin contents-end))))
        (when (string= link-type "id")
          (list :id path
                :description description
                :begin begin
                :end end))))))

(defun supertag-ui-embed--link-to-block ()
  "Internal function to convert the org-link at point to an embed block.
The link must be an id: link pointing to a node in the database.
This is an internal function and should not be called directly by users."
  (let ((link-info (supertag-ui-embed-get-link-at-point)))
    (unless link-info
      (user-error "Point is not on an org id: link"))
    
    (let* ((node-id (plist-get link-info :id))
           (begin (plist-get link-info :begin))
           (end (plist-get link-info :end))
           (node-data (supertag-get (list :nodes node-id))))
      
      (unless node-data
        (user-error "Node %s not found in database" node-id))
      
      ;; Generate embed block content
      (let ((embed-content (supertag-ui-embed-generate-node-content node-id))
            (title (plist-get node-data :title)))
        (unless embed-content
          (user-error "Failed to generate embed content for node %s" node-id))
        
        ;; Replace the link with embed block
        (delete-region begin end)
        (goto-char begin)
        ;; Include title in the begin_embed line for reference
        (insert (format "#+begin_embed: %s [%s]\n" node-id (or title "Untitled")))
        (insert embed-content)
        (unless (string-suffix-p "\n" embed-content)
          (insert "\n"))
        (insert "#+end_embed\n")
        
        (message "Converted link to embed block for node %s" node-id)))))

(defun supertag-ui-embed--insert-block ()
  "Internal function to select a node and insert an embed block at point.
This allows you to directly insert an embed block without first creating a link.
It will prompt you to select a node from the database and then insert the
embed block at the current position. This is an internal function and should
not be called directly by users."
  (require 'supertag-services-ui)
  
  ;; Use the existing node selection UI with cache for better performance
  (let ((node-id (supertag-ui-select-node "Insert embed block for node: " t)))
    (unless node-id
      (user-error "No node selected"))
    
    ;; Verify node exists in database
    (let ((node-data (supertag-get (list :nodes node-id))))
      (unless node-data
        (user-error "Node %s not found in database" node-id))
      
      ;; Generate embed block content
      (let ((embed-content (supertag-ui-embed-generate-node-content node-id))
            (title (plist-get node-data :title)))
        (unless embed-content
          (user-error "Failed to generate embed content for node %s" node-id))
        
        ;; Insert embed block at current position
        (let ((start-pos (point)))
          ;; Ensure we're at the beginning of a line
          (unless (bolp)
            (insert "\n"))
          
          ;; Include title in the begin_embed line for reference
          (insert (format "#+begin_embed: %s [%s]\n" node-id (or title "Untitled")))
          (insert embed-content)
          (unless (string-suffix-p "\n" embed-content)
            (insert "\n"))
          (insert "#+end_embed\n")
          
          (message "Inserted embed block for node %s" node-id))))))

(provide 'supertag-ui-embed)
;;; supertag-ui-embed.el ends here

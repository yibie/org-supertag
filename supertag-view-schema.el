;;; supertag-view-schema.el --- A UI for viewing the tag and field schema -*- lexical-binding: t; -*--

;;; Commentary:
;; This file provides a dedicated, interactive buffer for viewing the
;; entire schema of tags, fields, and their relationships.

;;; Code:

(require 'cl-lib)
(require 'supertag-services-query)

;;; --- Data Gathering and Structuring ---

(defun supertag-schema--ensure-plist (data)
  "Ensure DATA is a plist, converting from a hash-table if necessary."
  (if (hash-table-p data)
      (let (plist)
        (maphash (lambda (k v) (setq plist (plist-put plist k v))) data)
        plist)
    data))

(defun supertag-schema--build-tree ()
  "Build a hierarchical tree of all tags using a robust, functional approach."
  (let* ((all-tags-alist (supertag-query :tags))
         (tags-by-id (make-hash-table :test 'equal))
         (children-by-id (make-hash-table :test 'equal))
         (root-ids '()))

    ;; Pass 1: Ingest all data, normalize it, and store by ID.
    (dolist (pair all-tags-alist)
      (let* ((id (car pair))
             (data (cdr pair))
             (plist-data (supertag-schema--ensure-plist data)))
        (when id
          (puthash id (cl-copy-list plist-data) tags-by-id))))

    ;; Pass 2: Determine children and roots based on the :extends property.
    (maphash
     (lambda (id tag-plist)
       (let ((parent-ids (plist-get tag-plist :extends)))
         (if (and parent-ids (listp parent-ids) (not (null parent-ids)))
             ;; This tag has parents. For each parent, add this tag's ID
             ;; to the parent's list of children.
             (dolist (parent-id parent-ids)
               (when (gethash parent-id tags-by-id) ; Ensure parent exists
                 (push id (gethash parent-id children-by-id))))
           ;; This tag has no valid parents, so it's a root.
           (push id root-ids))))
     tags-by-id)

    ;; Pass 3: Recursively build the final tree structure from the maps.
    (cl-labels ((build-node (id)
                   (let* ((tag-plist (gethash id tags-by-id))
                          (child-ids (sort (gethash id children-by-id) #'string<))
                          ;; Recursively build the nodes for all children.
                          (children (mapcar #'build-node child-ids)))
                     ;; Return a new, clean plist with the :children key correctly set.
                     (plist-put (cl-copy-list tag-plist) :children children))))
      ;; Build the tree starting from the sorted, unique list of root IDs.
      (mapcar #'build-node (sort (cl-delete-duplicates (cl-copy-list root-ids)) #'string<)))))


;;; --- Rendering ---

(defun supertag-schema--format-field (field-def)
  "Format a single field definition into a display string."
  (let* ((name (plist-get field-def :name))
         (type (plist-get field-def :type))
         (options (plist-get field-def :options))
         (type-str (if type (format "(type: %s)" (substring (symbol-name type) 1)) "(type: string)")))
    (if (and (eq type :options) options)
        (format "- %s %s %s" name type-str options)
      (format "- %s %s" name type-str))))

(defun supertag-schema--render-tag-node (tag-node &optional level)
  "Recursively render a tag node and its children into the buffer."
  (let* ((level (or level 0))
         (indent (make-string (* 2 level) ?	))
         (tag-id (plist-get tag-node :id))
         (fields (plist-get tag-node :fields))
         (children (plist-get tag-node :children))
         (extends (plist-get tag-node :extends))
         (extends-str (if extends (format "(extends: %s)" extends) "")))
    ;; Render the tag itself
    (insert (format "%s[+] %s %s\n" indent tag-id extends-str))

    ;; Render fields if any
    (when fields
      (insert (format "%s    Fields:\n" indent))
      (dolist (field fields)
        (insert (format "%s      %s\n" indent (supertag-schema--format-field field)))))

    ;; Render children recursively
    (dolist (child children)
      (supertag-schema--render-tag-node child (1+ level)))))

(defun supertag-schema--render ()
  "Render the entire schema tree into the current buffer."
  (let ((tag-tree (supertag-schema--build-tree)))
    (erase-buffer)
    (insert "Supertag Schema\n")
    (insert "=================\n\n")
    (insert "Tags:\n")
    (dolist (root-tag tag-tree)
      (supertag-schema--render-tag-node root-tag))))


;;; --- Major Mode and User Command ---

(define-derived-mode supertag-schema-view-mode special-mode "Schema"
  "A major mode for viewing the Org-Supertag schema."
  (setq-local buffer-read-only t)
  (setq-local revert-buffer-function #'(lambda (&rest _) (supertag-schema--render))))

;;;###autoload
(defun supertag-schema-view ()
  "Create and display a buffer showing the entire tag and field schema."
  (interactive)
  (let ((buffer (get-buffer-create "*Supertag Schema*")))
    (with-current-buffer buffer
      ;; Render the content FIRST, while the buffer is still writable.
      (supertag-schema--render)
      ;; Set the major mode AFTER rendering is complete.
      (supertag-schema-view-mode))
    (pop-to-buffer buffer)))

(provide 'supertag-view-schema)

;;; supertag-view-schema.el ends here

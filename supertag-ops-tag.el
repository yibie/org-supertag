;;; org-supertag/ops/tag.el --- Tag operations for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides standardized operations for Tag entities in the
;; Org-Supertag data-centric architecture. All operations leverage
;; the core transform mechanism and adhere to the defined schema.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'supertag-core-store)
(require 'supertag-core-schema)
(require 'supertag-ops-relation)
(require 'supertag-ops-node)
(require 'supertag-ops-schema)

;;; --- Internal Helper ---

(defun supertag--validate-tag-data (data)
  "Strict validation for tag data. Fails fast on any inconsistency.
Implements immediate error reporting as preferred by the user."
  (unless (plist-get data :id)
    (error "Tag missing required :id field: %S" data))
  (unless (plist-get data :name)
    (error "Tag missing required :name field: %S" data))
  ;; Validate time format compliance (Emacs native format)
  (when-let ((created-at (plist-get data :created-at)))
    (unless (and (listp created-at) (= (length created-at) 4))
      (error "Tag :created-at must use Emacs time format, got: %S" created-at)))
  (when-let ((modified-at (plist-get data :modified-at)))
    (unless (and (listp modified-at) (= (length modified-at) 4))
      (error "Tag :modified-at must use Emacs time format, got: %S" modified-at))))

(defun supertag--ensure-plist (data)
  "Ensure DATA is in plist format, converting from hash table if necessary."
  (if (hash-table-p data)
      (let ((plist '()))
        (maphash (lambda (k v)
                   (setq plist (plist-put plist k v)))
                 data)
        plist)
    data))

(defun supertag--normalize-tag-extends (tag)
  "Normalize the :extends field in TAG plist to a sanitized tag id or nil."
  (if (plist-member tag :extends)
      (let* ((ext (plist-get tag :extends))
             (normalized (and (stringp ext)
                               (not (string-empty-p ext))
                               (supertag-sanitize-tag-name ext))))
        (plist-put tag :extends normalized))
    tag))

;; ID generation is now handled by supertag-id-utils.el

;;; --- Tag Operations ---

;; 3.1 Basic Operations

(defun supertag-tag-create (props)
  "Create a new tag using the unified commit system.
PROPS is a plist of tag properties.
Returns the created tag data."
  (let* ((name (plist-get props :name))
         (id (or (plist-get props :id)
                 (when name (supertag-sanitize-tag-name name))))
         (extends (let ((ext (plist-get props :extends)))
                    (when (and (stringp ext) (not (string-empty-p ext)))
                      (supertag-sanitize-tag-name ext))))
         (existing-tag (supertag-tag-get id)))
    ;; Check if tag exists
    (if existing-tag
        (progn
          (message "Tag '%s' already exists, returning existing tag." id)
          existing-tag)
      ;; If tag does not exist, create it
      (let* ((final-props `(:id ,id
                             :name ,name
                             :type :tag
                             :extends ,extends
                             :fields ,(or (plist-get props :fields) '())
                             :created-at ,(current-time)
                             :modified-at ,(current-time)))
             (normalized-props (supertag--normalize-tag-extends final-props)))
        ;; Use unified commit system
        (supertag-ops-commit
         :operation :create
         :collection :tags
         :id id
         :new normalized-props
         :perform (lambda ()
                    (supertag-store-put-entity :tags id normalized-props)
                    (supertag-ops-schema-rebuild-cache)
                    normalized-props))))))

(defun supertag-tag-get (id)
  "Get tag data.
ID is the unique identifier of the tag.
Returns tag data, or nil if it does not exist."
  (supertag-store-get-entity :tags id))

(defun supertag-tag-update (id updater)
  "Update tag data using the unified commit system.
ID is the unique identifier of the tag.
UPDATER is a function that receives the current tag data and returns the updated data.
Returns the updated tag data."
  (let ((previous (supertag-tag-get id)))
    (when previous
      ;; Convert hash table to plist if necessary
      (let* ((original-plist (supertag--ensure-plist previous))
             (copy-for-update (copy-tree original-plist)))
        (supertag-ops-commit
         :operation :update
         :collection :tags
         :id id
         :previous original-plist
         :perform (lambda ()
                    (let ((updated-tag (funcall updater copy-for-update)))
                      (when updated-tag
                        (if (equal updated-tag original-plist)
                            original-plist
                          (let* ((normalized-tag (supertag--normalize-tag-extends updated-tag))
                                 (final-tag (plist-put normalized-tag :modified-at (current-time))))
                            (supertag--validate-tag-data final-tag)
                            (supertag-store-put-entity :tags id final-tag)
                            (supertag-ops-schema-rebuild-cache)
                            final-tag))))))))))

(defun supertag-tag-delete (id)
  "Delete a tag using the unified commit system.
ID is the unique identifier of the tag.
Returns the deleted tag data."
  (let ((previous (supertag-tag-get id)))
    (when previous
      (supertag-ops-commit
       :operation :delete
       :collection :tags
       :id id
       :previous previous
       :perform (lambda ()
                  (supertag-store-remove-entity :tags id)
                  (supertag-ops-schema-rebuild-cache)
                  nil))))) 

(defun supertag-ops-delete-tag-everywhere (tag-name)
  "Delete a tag and all its uses from the database and all org files.
This is a non-interactive, high-level operation. It finds all nodes
with TAG-NAME, cleans up all database relations, and then removes
the tag text from the source files.
Returns the number of instances removed from files."
  (when (and tag-name (not (string-empty-p tag-name)))
    (let* ((nodes-with-tag (supertag-find-nodes-by-tag tag-name))
           (files (delete-dups (mapcar (lambda (node-pair)
                                         (let ((node (cdr node-pair)))
                                           (plist-get node :file)))
                                       nodes-with-tag))))

      ;; Clean up relations and node properties in a single loop
      (dolist (node-pair nodes-with-tag)
        (let* ((node-id (car node-pair))
               (relations (supertag-relation-find-between node-id tag-name :node-tag)))
          (dolist (rel relations)
            (supertag-relation-delete (plist-get rel :id)))
          (supertag-node-remove-tag node-id tag-name)))

      ;; Delete the tag definition itself
      (supertag-tag-delete tag-name)

      ;; Remove tag text from all associated files
      (require 'supertag-view-helper)
      (let ((total-deleted (supertag-view-helper-remove-tag-text-from-files tag-name files)))
        (message "Tag '%s' completely deleted. Removed %d instances from files."
                 tag-name (or total-deleted 0))
        total-deleted))))

(defun supertag-ops-add-tag-to-node (node-id tag-id &key create-if-needed)
  "High-level operation to add a tag to a node.
This non-interactive function ensures the tag exists (creating it
if CREATE-IF-NEEDED is non-nil) and then creates the node-tag
relationship. It also updates the node's :tags property to ensure
index consistency.

It does NOT modify the buffer.
Returns t if the relationship was created or already exists, nil otherwise."
  (when (and node-id (not (string-empty-p tag-id)))
    ;; 1. Ensure tag definition exists.
    (when (and create-if-needed (not (supertag-tag-get tag-id)))
      (supertag-tag-create `(:name ,tag-id :id ,tag-id)))

    ;; 2. If tag exists, create the relationship and update node.
    (if-let ((raw-tag (supertag-tag-get tag-id)))
        (let ((tag (supertag--ensure-plist raw-tag))) ; Ensure we have a plist
          ;; Update the node's :tags property through the node CRUD API.
          (supertag-node-add-tag node-id tag-id)

          ;; Avoid creating duplicate relations
          (unless (supertag-relation-find-between node-id (plist-get tag :id) :node-tag)
            (supertag-relation-create `(:type :node-tag :from ,node-id :to ,(plist-get tag :id))))
          t)
      nil)))

;; 3.2 Field Operations

(defun supertag-tag-add-field (tag-id field-def)
  "Add a field definition to a tag.
TAG-ID is the unique identifier of the tag.
FIELD-DEF is a plist of the field definition.
Returns the updated tag data."
  (supertag-tag-update tag-id
    (lambda (tag)
      (when tag
        (let* ((fields-list (or (plist-get tag :fields) '()))
               (field-name (plist-get field-def :name))
               (existing-field (cl-find field-name fields-list :key (lambda (f) (plist-get f :name)) :test 'equal))
               (new-fields (if existing-field
                               ;; If field exists, update its definition
                               (mapcar (lambda (f)
                                         (if (equal (plist-get f :name) field-name)
                                             field-def
                                           f))
                                       fields-list)
                             ;; If field does not exist, add it
                             (append fields-list (list field-def)))))
          (plist-put tag :fields new-fields))))))

(defun supertag-tag-define-field (tag-id field-name type &optional properties)
  "Define or update a field for a tag with explicit TYPE and PROPERTIES.
This is a high-level wrapper that constructs the field-def and calls
`supertag-tag-add-field`. It enforces the rule that an :options
type must be accompanied by an :options property.

For :options type, PROPERTIES should include :options '(...).
Example: (supertag-tag-define-field \"task\" \"Priority\" :options '(:options (\"High\" \"Medium\" \"Low\")))"
  (let ((field-def (append `(:name ,field-name :type ,type) properties)))
    (supertag-tag-add-field tag-id field-def)))

(defun supertag-tag-remove-field (tag-id field-name)
  "Remove a field definition from a tag.
TAG-ID is the unique identifier of the tag.
FIELD-NAME is the name of the field to remove.
Returns the updated tag data."
  (message "DEBUG: Got tag object: %S" (supertag-tag-get tag-id))
  (supertag-tag-update tag-id
    (lambda (tag)
      (when tag
        (let* ((fields-list (or (plist-get tag :fields) '()))
               (new-fields (cl-remove field-name fields-list :key (lambda (f) (plist-get f :name)) :test 'equal)))
          (plist-put tag :fields new-fields))))))

(defun supertag-tag-list-missing-fields ()
  "List all tags that have nil :fields or :extends properties.
Returns a list of tag IDs that may have lost their field definitions."
  (interactive)
  (let ((tags-table (supertag-store-get-collection :tags))
        (missing-fields '())
        (missing-extends '())
        (total-tags 0))
    (maphash
     (lambda (tag-id tag-data)
       (cl-incf total-tags)
       (let ((fields (plist-get tag-data :fields))
             (extends (plist-get tag-data :extends)))
         (when (null fields)
           (push tag-id missing-fields))
         (when (null extends)
           (push tag-id missing-extends))))
     tags-table)
    
    (let ((report (format "=== Tag Field Status Report ===\nTotal tags: %d\nTags with nil :fields: %d\nTags with nil :extends: %d\n\nTags missing :fields:\n%s\n"
                         total-tags
                         (length missing-fields)
                         (length missing-extends)
                         (mapconcat #'identity (nreverse missing-fields) "\n"))))
      (message "%s" report)
      (with-current-buffer (get-buffer-create "*Supertag Missing Fields*")
        (erase-buffer)
        (insert report)
        (goto-char (point-min))
        (display-buffer (current-buffer)))
      (list :missing-fields missing-fields
            :missing-extends missing-extends))))

(defun supertag-tag-rename-field (tag-id old-name new-name)
  "Rename a field definition in a tag.
TAG-ID is the unique identifier of the tag.
OLD-NAME is the current name of the field.
NEW-NAME is the new name for the field.
Returns the updated tag data."
  (supertag-tag-update tag-id
    (lambda (tag)
      (when tag
        (let* ((fields-list (or (plist-get tag :fields) '()))
               (field-to-rename (cl-find old-name fields-list :key (lambda (f) (plist-get f :name)) :test 'equal)))
          (when field-to-rename
            (let* ((updated-field (plist-put field-to-rename :name new-name))
                   (new-fields (mapcar (lambda (f)
                                         (if (equal (plist-get f :name) old-name)
                                             updated-field
                                           f))
                                       fields-list)))
              (plist-put tag :fields new-fields))))))))

(defun supertag-tag-rename (old-id new-id)
  "Rename a tag from OLD-ID to NEW-ID across the entire system.
This is a complex operation that updates the tag definition,
all child tags that extend it, all node-tag relationships,
and the tag text in all relevant Org files."
  (interactive "sRename tag from: \nsRename tag to: ")
  (let ((sanitized-new-id (supertag-sanitize-tag-name new-id)))
    (when (supertag-tag-get sanitized-new-id)
      (error "Tag '%s' already exists. Cannot rename." sanitized-new-id))

    ;; 0. Find all affected nodes and files BEFORE any changes
    (let* ((nodes-with-tag (supertag-find-nodes-by-tag old-id))
           (files (delete-dups (mapcar (lambda (node-pair)
                                         (let ((node (cdr node-pair)))
                                           (plist-get node :file)))
                                       nodes-with-tag))))

      ;; 1. Get old tag data and create the new one
      (let ((old-tag-data (supertag-tag-get old-id)))
        (unless old-tag-data
          (error "Tag '%s' not found." old-id))
        (let ((new-tag-props (copy-tree (supertag--ensure-plist old-tag-data))))
          (setq new-tag-props (plist-put new-tag-props :id sanitized-new-id))
          (setq new-tag-props (plist-put new-tag-props :name sanitized-new-id))
          (supertag-tag-create new-tag-props)))

      ;; 2. Update all child tags that extend the old tag
      (let ((all-tags (mapcar #'cdr (supertag-query :tags))))
        (dolist (tag-data all-tags)
          (let* ((tag-plist (supertag--ensure-plist tag-data))
                 (parent-id (plist-get tag-plist :extends)))
            (when (equal parent-id old-id)
              (supertag--set-tag-parent (plist-get tag-plist :id) sanitized-new-id)))))

      ;; 3. Update all node-tag relations
      (let ((relations-to-update (supertag-relation-find-by-to old-id :node-tag)))
        (dolist (rel relations-to-update)
          (supertag-relation-update (plist-get rel :id)
            (lambda (r) (plist-put r :to sanitized-new-id)))))

      ;; 4. Delete the old tag definition
      (supertag-tag-delete old-id)

      ;; 5. Update the text in all relevant Org files
      (require 'supertag-view-helper)
      (let ((total-renamed (supertag-view-helper-rename-tag-text-in-files old-id sanitized-new-id files)))
        (message "Tag renamed from '%s' to '%s' (%s total instances)." 
                 old-id sanitized-new-id total-renamed))

      ;; 6. Rebuild schema cache to reflect all changes
      (supertag-ops-schema-rebuild-cache)
      sanitized-new-id)))

(defun supertag--set-tag-parent (child-id parent-id)
  "Set CHILD-ID to extend PARENT-ID, rebuilding schema cache."
  (let ((child (supertag-tag-get child-id))
        (parent (supertag-tag-get parent-id)))
    (unless child
      (error "Child tag '%s' does not exist" child-id))
    (unless parent
      (error "Parent tag '%s' does not exist" parent-id))
    (let* ((normalized-child (plist-get (supertag--ensure-plist child) :id))
           (normalized-parent (supertag-sanitize-tag-name parent-id)))
      (when (string= normalized-child normalized-parent)
        (error "Tag '%s' cannot extend itself" normalized-child))
    (supertag-tag-update child-id
      (lambda (tag)
        (when tag
          (plist-put tag :extends normalized-parent)))))))

(defun supertag--clear-parent (child-id)
  "Clear the parent (extends) relationship for CHILD-ID."
  (unless (supertag-tag-get child-id)
    (error "Child tag '%s' does not exist" child-id))
  (supertag-tag-update child-id
    (lambda (tag)
      (when tag
        (plist-put tag :extends nil)))))

(defun supertag-tag-get-field (tag-id field-name)
  "Get a field definition from a tag.
TAG-ID is the unique identifier of the tag.
FIELD-NAME is the name of the field.
Returns the field definition, or nil if not found."
  (let ((all-fields (supertag-tag-get-all-fields tag-id)))
    (when all-fields
      (cl-find field-name all-fields :key (lambda (f) (plist-get f :name)) :test #'equal)))) 

(defun supertag-tag-move-field-up (tag-id field-name)
  "Move a field definition up in the tag's field list.
TAG-ID is the unique identifier of the tag.
FIELD-NAME is the name of the field to move up.
Returns the updated tag data."
  (supertag-tag-update tag-id
    (lambda (tag)
      (when tag
        (let* ((fields-list (or (plist-get tag :fields) '()))
               (field-index (cl-position field-name fields-list :key (lambda (f) (plist-get f :name)) :test 'equal)))
          (if (and field-index (> field-index 0))
              ;; Move field up by swapping with the previous field
              (let* ((new-fields (copy-sequence fields-list))
                     (prev-index (1- field-index)))
                (cl-rotatef (nth prev-index new-fields) (nth field-index new-fields))
                (plist-put tag :fields new-fields))
            ;; If field is not found or already at the top, return unchanged
            tag))))))

(defun supertag-tag-move-field-down (tag-id field-name)
  "Move a field definition down in the tag's field list.
TAG-ID is the unique identifier of the tag.
FIELD-NAME is the name of the field to move down.
Returns the updated tag data."
  (supertag-tag-update tag-id
    (lambda (tag)
      (when tag
        (let* ((fields-list (or (plist-get tag :fields) '()))
               (field-index (cl-position field-name fields-list :key (lambda (f) (plist-get f :name)) :test 'equal)))
          (if (and field-index (< field-index (1- (length fields-list))))
              ;; Move field down by swapping with the next field
              (let* ((new-fields (copy-sequence fields-list))
                     (next-index (1+ field-index)))
                (cl-rotatef (nth field-index new-fields) (nth next-index new-fields))
                (plist-put tag :fields new-fields))
            ;; If field is not found or already at the bottom, return unchanged
            tag))))))

(defun supertag-tag-get-all-fields (tag-id)
  "Get all field definitions for a tag.
TAG-ID is the unique identifier of the tag.
Returns a list of field definition plists, or empty list if tag not found."
  (let ((resolved (supertag-ops-schema-get-resolved-tag tag-id)))
    (cond
     ((and resolved (plist-get resolved :fields))
      (plist-get resolved :fields))
     (t
      (let ((tag (supertag-tag-get tag-id)))
        (unless tag
          (message "Warning: Tag '%s' not found, returning empty field list." tag-id)
          (cl-return-from supertag-tag-get-all-fields '()))
        (let ((plist-tag (supertag--ensure-plist tag)))
          (or (plist-get plist-tag :fields) '())))))))

(defun supertag-sanitize-tag-name (name)
  "Sanitize a string into a valid tag name.
Removes leading/trailing whitespace, a leading '#', and converts
internal whitespace to single underscores."
  (if (or (null name) (string-empty-p name))
      (error "Tag name cannot be empty")
    (let* ((clean-name (substring-no-properties name))
           (trimmed (string-trim clean-name))
           (no-hash (if (string-prefix-p "#" trimmed)
                        (substring trimmed 1)
                      trimmed))
           (sanitized (replace-regexp-in-string "\\s-+" "_" no-hash)))
      (if (string-empty-p sanitized)
          (error "Invalid tag name: %s" name)
        sanitized))))

(provide 'supertag-ops-tag)

;;; org-supertag/ops/tag.el ends here

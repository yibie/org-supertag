;;; org-supertag/ops/tag.el --- Tag operations for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides standardized operations for Tag entities in the
;; Org-Supertag data-centric architecture. All operations leverage
;; the core transform mechanism and adhere to the defined schema.

;;; Code:

(require 'cl-lib)
(require 'supertag-core-store)    ; For supertag-get, supertag-update
(require 'supertag-core-schema)   ; For validation functions
(require 'supertag-core-transform) ; For supertag-transform
(require 'supertag-ops-relation) ; For relation operations

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

;; ID generation is now handled by supertag-id-utils.el

;;; --- Tag Operations ---

;; 3.1 Basic Operations

(defun supertag-tag-create (props)
  "Create a new tag using hybrid architecture.
PROPS is a plist of tag properties.
Returns the created tag data."
  (let* ((name (plist-get props :name))
         (id (or (plist-get props :id)
                 (when name (supertag-sanitize-tag-name name))))
         (existing-tag (supertag-tag-get id)))
    ;; Check if tag exists
    (if existing-tag
        (progn
          (message "Tag '%s' already exists, returning existing tag." id)
          existing-tag)
      ;; If tag does not exist, create it
      (let* ((final-props (plist-put props :id id))
             (final-props (plist-put final-props :type :tag))
             (final-props (plist-put final-props :created-at (current-time)))
             (final-props (plist-put final-props :modified-at (current-time))))
        ;; Strict validation (fail-fast principle)
        (supertag--validate-tag-data final-props)
        ;; Direct storage for optimal performance
        (supertag-store-direct-set :tags id final-props)
        final-props))))

(defun supertag-tag-get (id)
  "Get tag data.
ID is the unique identifier of the tag.
Returns tag data, or nil if it does not exist."
  (supertag-get (list :tags id)))

(defun supertag-tag-update (id updater)
  "Update tag data using hybrid architecture.
ID is the unique identifier of the tag.
UPDATER is a function that receives the current tag data and returns the updated data.
Returns the updated tag data."
  (let ((tag (supertag-tag-get id)))
    (when tag
      ;; Convert hash table to plist if necessary
      (when (hash-table-p tag)
        (let ((plist-tag '()))
          (maphash (lambda (k v)
                     (setq plist-tag (plist-put plist-tag k v)))
                   tag)
          (setq tag plist-tag)))
      
      (let ((updated-tag (funcall updater tag)))
        (when updated-tag
          ;; Add/update timestamps
          (let ((final-tag (plist-put updated-tag :modified-at (current-time))))
            ;; Strict validation (fail-fast principle)
            (supertag--validate-tag-data final-tag)
            ;; Direct storage for optimal performance
            (supertag-store-direct-set :tags id final-tag)
            final-tag))))))

(defun supertag-tag-delete (id)
  "Delete a tag.
ID is the unique identifier of the tag.
Returns the deleted tag data."
  (let ((tag (supertag-tag-get id)))
    (when tag
      (supertag-update (list :tags id) nil)
      ;; TODO: Clean up related relations (will be implemented in ops/relation.el)
      ;; TODO: Remove this tag from all nodes (will be implemented in ops/node.el)
      tag)))

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
    (if-let ((tag (supertag-tag-get tag-id)))
        (progn
          ;; Update the node's :tags property to trigger index update
          (let ((node (supertag-node-get node-id)))
            (when node
              (let ((current-tags (or (plist-get node :tags) '())))
                (unless (member tag-id current-tags)
                  ;; Add tag to node's tags list
                  (let ((updated-node (plist-put node :tags (cons tag-id current-tags))))
                    (supertag-store-direct-set :nodes node-id updated-node))))))
          
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

(defun supertag-tag-get-field (tag-id field-name)
  "Get a field definition from a tag.
TAG-ID is the unique identifier of the tag.
FIELD-NAME is the name of the field.
Returns the field definition, or nil if not found."
  (let ((tag (supertag-tag-get tag-id)))
    (when tag
      ;; Convert hash table to plist if necessary
      (when (hash-table-p tag)
        (let ((plist-tag '()))
          (maphash (lambda (k v)
                     (setq plist-tag (plist-put plist-tag k v)))
                   tag)
          (setq tag plist-tag)))
      
      (let ((fields (plist-get tag :fields)))
        (cl-find field-name fields :key (lambda (f) (plist-get f :name)) :test 'equal))))) 

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
              (let ((new-fields (copy-sequence fields-list)))
                (let ((temp (nth field-index new-fields)))
                  (setf (nth field-index new-fields) (nth (1- field-index) new-fields))
                  (setf (nth (1- field-index) new-fields) temp))
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
              (let ((new-fields (copy-sequence fields-list)))
                (let ((temp (nth field-index new-fields)))
                  (setf (nth field-index new-fields) (nth (1+ field-index) new-fields))
                  (setf (nth (1+ field-index) new-fields) temp))
                (plist-put tag :fields new-fields))
            ;; If field is not found or already at the bottom, return unchanged
            tag))))))

;; 3.3 Inheritance Operations

(defun supertag-tag-add-extends (tag-id parent-tag-id)
  "Add a tag inheritance relationship.
TAG-ID is the unique identifier of the tag.
PARENT-TAG-ID is the unique identifier of the parent tag.
Returns the updated tag data."
  (supertag-transform
   (list :tags tag-id :extends)
   (lambda (extends)
     (let ((extends-list (or extends '())))
       (if (member parent-tag-id extends-list)
           extends-list
         (cons parent-tag-id extends-list))))))

(defun supertag-tag-remove-extends (tag-id parent-tag-id)
  "Remove a tag inheritance relationship.
TAG-ID is the unique identifier of the tag.
PARENT-TAG-ID is the unique identifier of the parent tag.
Returns the updated tag data."
  (supertag-transform
   (list :tags tag-id :extends)
   (lambda (extends)
     (remove parent-tag-id (or extends '())))))

(defun supertag-tag-get-all-fields (tag-id &optional visited-tags)
  "Recursively get all field definitions for a tag, including inherited ones.
TAG-ID is the unique identifier of the tag.
VISITED-TAGS is an internal parameter to detect circular dependencies.
Returns a list of field definition plists, or empty list if tag not found."
  (let ((tag (supertag-tag-get tag-id)))
    ;; Return empty list if tag doesn't exist (defensive programming)
    (unless tag
      (message "Warning: Tag '%s' not found, returning empty field list." tag-id)
      (cl-return-from supertag-tag-get-all-fields '()))

    ;; Convert hash table to plist if necessary
    (when (hash-table-p tag)
      (let ((plist-tag '()))
        (maphash (lambda (k v)
                   (setq plist-tag (plist-put plist-tag k v)))
                 tag)
        (setq tag plist-tag)))

    ;; Circular dependency detection
    (when (member tag-id visited-tags)
      (error "Circular dependency detected in tag inheritance for tag '%s'." tag-id))

    (let ((fields (or (plist-get tag :fields) '()))
          (extends (plist-get tag :extends))
          (all-fields ()))

      ;; Add own fields first
      (dolist (field fields)
        (push field all-fields))

      ;; Recursively get fields from parent tags
      (when extends
        (dolist (parent-id (if (listp extends) extends (list extends))) ; Ensure extends is a list
          (let ((parent-fields (supertag-tag-get-all-fields parent-id (cons tag-id visited-tags))))
            (dolist (p-field parent-fields)
              ;; Add parent field only if not already defined by current tag
              (unless (cl-find (plist-get p-field :name) all-fields :key (lambda (f) (plist-get f :name)) :test 'equal)
                (push p-field all-fields))))))
      (nreverse all-fields)))) ; Return in original order

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

;;; org-supertag-tag.el --- Tag relation management for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides tag relationship management functionality
;; Core principle: Connect entities through relationships using type, from, and to

;;; Code:

(require 'cl-lib) ; For cl-loop, cl-find, etc.
(require 'ht)     ; For hash-table operations
(require 'org-supertag-db)
(require 'org-supertag-node)
(require 'org-supertag-field)
;; Utilities
(require 'cl-lib) ; For cl-return-from used in early exit

;;----------------------------------------------------------------------
;; Tag Name Operation
;;----------------------------------------------------------------------

(defun org-supertag-sanitize-tag-name (name)
  "Convert a name into a valid tag name.
NAME is the name to convert. It removes leading/trailing whitespace,
a leading '#', removes all text properties, and converts internal
whitespace sequences to single underscores."
  (if (or (null name) (string-empty-p name))
      (error "Tag name cannot be empty")
    (let* ((clean-name (substring-no-properties name))
           (trimmed (string-trim clean-name))
           (no-hash (if (string-prefix-p "#" trimmed)
                        (substring trimmed 1)
                      trimmed))
           (sanitized (replace-regexp-in-string "\s-+" "_" no-hash)))
      (if (string-empty-p sanitized)
          (error "Invalid tag name: %s" name)
        sanitized))))

(defun org-supertag-tag-exists-p (tag-name)
  "Check if a tag exists.
TAG-NAME is the name of the tag to check"
  (let ((sanitized-name (org-supertag-sanitize-tag-name tag-name)))
    (org-supertag-tag-get sanitized-name)))

;;----------------------------------------------------------------------
;; Tag Base Operation
;;----------------------------------------------------------------------

(defun org-supertag-tag--parse-name (tag-name)
  "Parse tag name to determine if it's an extension.
TAG-NAME: The tag name to parse

Returns (BASE-TAG . SPECIFIC-NAME) if it's an extension,
nil if it's a base tag.

Examples:
  (org-supertag-tag--parse-name \"task\") => nil
  (org-supertag-tag--parse-name \"task_design\") => (\"task\" . \"design\")"
  (when-let* ((parts (split-string tag-name "_" t))
              ((> (length parts) 1)))
    (cons (car parts) (mapconcat #'identity (cdr parts) "_"))))

(defun org-supertag-tag--create (tag-name &rest props)
  "Create a new tag.
TAG-NAME: Name of the tag
PROPS: Additional properties including:
- :fields      List of field definitions
- :behaviors   List of behaviors"
  (unless (string-to-multibyte tag-name)
    (error "Tag name '%s' contains invalid characters" tag-name))
  (let ((sanitized-name (org-supertag-sanitize-tag-name tag-name)))
    ;; If the tag already exists, do NOT overwrite it. Simply return its ID.
    (when (org-supertag-tag-get sanitized-name)
      (cl-return-from org-supertag-tag-create sanitized-name))

    ;; Tag does not exist yet – proceed with normal creation logic.
    (let* ((parsed-name (org-supertag-tag--parse-name sanitized-name))
           (base-tag-name (car parsed-name))
           (new-fields (plist-get props :fields))
           (base-tag (and base-tag-name
                          (org-supertag-tag-exists-p base-tag-name)
                          (org-supertag-tag-get base-tag-name)))
           (fields (or (if base-tag
                           (org-supertag-tag--project-fields
                            base-tag
                            new-fields
                            (plist-get props :field-defaults))
                         new-fields)
                       '()))
           (base-props (list :type :tag
                             :id sanitized-name
                             :name sanitized-name
                             :fields fields
                             :extend-from base-tag-name
                             :behaviors (plist-get props :behaviors)
                             :created-at (current-time))))
      (org-supertag-db-add sanitized-name base-props))
    sanitized-name))

(defun org-supertag-tag--project-fields (base-tag new-fields defaults)
  "Project fields from base tag and merge with new fields.
BASE-TAG: The base tag entity
NEW-FIELDS: Additional fields to add
DEFAULTS: Plist of field default value overrides"
  (let* ((base-fields (plist-get base-tag :fields))
         result)
    (dolist (field base-fields)
      (let* ((field-name (plist-get field :name))
             (default-override (plist-get defaults field-name))
             (projected-field
              (append 
               (list :projected-from (plist-get base-tag :id))
               (if default-override
                   (plist-put (copy-sequence field) 
                              :default default-override)
                 (copy-sequence field)))))
        (unless (plist-get projected-field :type)
          (error "Field '%s' from base tag '%s' has no type defined"
                 field-name (plist-get base-tag :id)))
        (push projected-field result)))
    
    (dolist (field new-fields)
      (let ((field-name (plist-get field :name)))
        (unless field-name
          (error "New field must have a name"))
        (unless (plist-get field :type)
          (error "New field '%s' must have a type defined" field-name))
        
        (when (cl-find field-name result
                       :key (lambda (f) (plist-get f :name))
                       :test #'equal)
          (error "Field '%s' already exists in base tag '%s'"
                 field-name (plist-get base-tag :id)))
        (push (copy-sequence field) result)))
    
    (nreverse result)))

(defun org-supertag-tag-get-base (tag-name)
  "Get base tag name if TAG-NAME is an extension, nil otherwise."
  (car (org-supertag-tag--parse-name tag-name)))

(defun org-supertag-tag-get-extensions (base-tag-name)
  "Get all extension tags for BASE-TAG-NAME."
  (let (extensions)
    (maphash
     (lambda (id entity)
       (when (and (eq (plist-get entity :type) :tag)
                  (equal (org-supertag-tag-get-base id) base-tag-name))
         (push id extensions)))
     org-supertag-db--object)
    extensions))

(defun org-supertag-tag-get (tag-name)
  "Get tag definition.
TAG-NAME is the name of the tag to retrieve.
Returns the tag entity if found and is a valid tag type,
otherwise returns nil."
  (let ((entity (org-supertag-db-get tag-name)))
    (when (and entity 
               (eq (plist-get entity :type) :tag))
      entity)))

(defun org-supertag-get-all-tags ()
  "Get a list of all defined tags."
  (let ((tags '()))
    (maphash
     (lambda (id entity)
       (when (eq (plist-get entity :type) :tag)
         (push (plist-get entity :name) tags)))
     org-supertag-db--object)
    (delete-dups tags)))

(defun org-supertag-tag-get-id-by-name (name)
  "Return the ID for a given tag NAME.
In the current system, the sanitized name is the ID.
This function is provided for compatibility."
  (org-supertag-sanitize-tag-name name))

;;----------------------------------------------------------------------
;; Tag-Node Relation Operation
;;----------------------------------------------------------------------

(defvar org-supertag-tag-apply-skip-headline nil
  "When non-nil, `org-supertag-tag-apply' will not add the tag to headline tags.")

(defvar org-supertag-tag-apply-skip-duplicate-id nil
  "When non-nil, `org-supertag-tag-apply' will be extra careful not to create duplicate IDs.")

(defvar org-supertag-force-node-id nil
  "When non-nil, force `org-supertag-tag-apply' to use this specific node ID instead of creating one.")

(defvar org-supertag-skip-text-insertion nil
  "When non-nil, skip text insertion when applying tags, only establish the relationship.")

(defcustom org-supertag-inherit-parent-tags t
  "When non-nil, automatically inherit all parent node tags to the child node (database only)."
  :type 'boolean
  :group 'org-supertag)

(defun org-supertag-tag--inherit-parent-tags (node-id)
  "Inherit all parent node tags to NODE-ID when `org-supertag-inherit-parent-tags` is non-nil.
Only creates :node-tag links in database; no inline text insertion."  
  (when (and org-supertag-inherit-parent-tags node-id)
    (when-let* ((parent-tags (org-supertag-node-get-parent-tags node-id)))
      (dolist (ptag parent-tags)
        (unless (member ptag (org-supertag-node-get-tags node-id))
          (org-supertag-node-db-add-tag node-id ptag))))))

;; In org-supertag-tag-apply, after establishing the primary tag relationship, call inherit function.
(defun org-supertag-tag-apply (tag-id)
  "Apply tag to the node at current position.
This is now the single entry point for applying a tag. It handles
database relations, behavior execution, and finally text insertion."
  (message "DEBUG: org-supertag-tag-apply called with tag-id=%s" tag-id)
  (let* ((tag (org-supertag-db-get tag-id))
         (node-id (or org-supertag-force-node-id (org-entry-get nil "ID"))))
    
    (setq node-id (or node-id (org-id-get-create)))
    (message "DEBUG: applying to node-id=%s" node-id)

    ;; Prevent applying a tag that already exists on the node.
    (when (member tag-id (org-supertag-node-get-tags node-id))
      (user-error "Tag '%s' is already applied to this node" tag-id))
    
    (unless tag
      (error "Tag %s not found" tag-id))
    (unless (eq (plist-get tag :type) :tag)
      (error "Invalid tag type for %s" tag-id))
    (unless (org-supertag-db-get node-id)
      (org-supertag-node-sync-at-point))
    
    (org-supertag-node-db-add-tag node-id tag-id)
    ;; inherit parent tags, database only
    (org-supertag-tag--inherit-parent-tags node-id)
    
    ;; Text insertion is now handled here, controlled by the same variable.
    (unless org-supertag-skip-text-insertion
      (org-supertag-inline-insert-tag tag-id))

    (when-let* ((fields (org-supertag-get-all-fields-for-tag tag-id)))
      (dolist (field fields)
        (let* ((field-name (plist-get field :name))
               (field-type (plist-get field :type))
               (type-def (org-supertag-get-field-type field-type))
               (initial-value (org-supertag-field-get-initial-value field)))
          (unless type-def
            (error "Invalid field type: %s" field-type))

          (when-let* ((formatter (plist-get type-def :formatter))
                      (formatted-value (if formatter
                                           (funcall formatter initial-value field)
                                         (format "%s" initial-value))))
            (org-supertag-tag--set-field-value 
             tag-id node-id field-name initial-value)))))
        
    (when (featurep 'org-supertag-relation)
      (org-supertag-relation-record-cooccurrence node-id tag-id))
    
    (run-hook-with-args 'org-supertag-after-tag-apply-hook node-id)
    (org-supertag-behavior--on-tag-change node-id tag-id :add)
    (org-supertag-behavior--apply-styles node-id)
    
    node-id))

(defun org-supertag-tag--remove (tag-id node-id)
  "Remove a tag from a node.
TAG-ID: The tag identifier
NODE-ID: The node identifier"
  (org-supertag-db-remove-link :node-tag node-id tag-id)
  (let ((tag (org-supertag-tag-get tag-id)))
    (dolist (field-def (org-supertag-get-all-fields-for-tag tag-id))
      (let ((field-name (plist-get field-def :name)))
        (org-supertag-field-delete-value node-id field-name tag-id))))
  ;; Clean up cooccurrence relations when removing a tag
  (when (featurep 'org-supertag-relation)
    (org-supertag-relation-unrecord-cooccurrence node-id tag-id)))

;;----------------------------------------------------------------------
;; Tag User Command (Now data-layer functions)
;;----------------------------------------------------------------------

(defun org-supertag-tag--delete-at-all (tag-id)
  "Delete a tag completely from the database.
This is the data-layer function. For interactive use, see `org-supertag-inline-delete-all`.
TAG-ID: The tag identifier to delete."
  (when (yes-or-no-p (format "Delete tag '%s' from DB? This is a data-only operation. " tag-id))
    (let ((nodes (org-supertag-db-get-tag-nodes tag-id))
          (tag (org-supertag-tag-get tag-id)))
      
      (unless tag
        (error "Tag %s not found" tag-id))
      (unless (eq (plist-get tag :type) :tag)
        (error "Invalid tag type for %s" tag-id))

      (dolist (node-id nodes)
        (when-let* ((pos (condition-case nil (org-id-find node-id t) (error nil))))
          (org-with-point-at pos
            (dolist (field (org-supertag-get-all-fields-for-tag tag-id))
              (let ((field-name (plist-get field :name)))
                (org-delete-property field-name))))))
      
      (let ((links-to-remove '()))
        (maphash (lambda (link-id props)
                   (when (or (equal (plist-get props :from) tag-id)
                             (equal (plist-get props :to) tag-id)
                             (equal (plist-get props :tag-id) tag-id))
                     (push link-id links-to-remove)))
                 org-supertag-db--link)
        
        (dolist (link-id links-to-remove)
          (when-let* ((link-props (gethash link-id org-supertag-db--link)))
            (let ((link-type (plist-get link-props :type))
                  (from (plist-get link-props :from))
                  (to (plist-get link-props :to)))
              (org-supertag-db-remove-link link-type from to)))))
      
      ;; Clean up cooccurrence relations for the deleted tag
      (when (featurep 'org-supertag-relation)
        (dolist (node-id nodes)
          (org-supertag-relation-unrecord-cooccurrence node-id tag-id)))
      
      (org-supertag-db-remove-object tag-id)
      (org-supertag-db--mark-dirty)
      (org-supertag-db-save)
      
      (run-hook-with-args 'org-supertag-after-tag-delete-hook tag-id)
      
      (message "Deleted tag '%s' and removed it from %d nodes" tag-id (length nodes)))))

(defcustom org-supertag-after-tag-delete-hook nil
  "Hook run after a tag is deleted.
The hook functions are called with one argument:
- tag-id: The ID of the deleted tag"
  :type 'hook
  :group 'org-supertag)

(defun org-supertag-tag--set-field-value (tag-id node-id field-name value)
  "Helper function to set a field value in the database."
  (let* ((tag (org-supertag-tag-get tag-id))
         (field-def (cl-find field-name (org-supertag-get-all-fields-for-tag tag-id)
                             :key (lambda (f) (plist-get f :name))
                             :test #'string=)))
    (when field-def
      (org-supertag-field-set-value node-id field-name value tag-id))))

(defun org-supertag-tag-add-field (tag-id field-def)
  "Add a new field definition to a tag.
TAG-ID is the ID of the tag.
FIELD-DEF is the new field definition plist."
  (when-let ((tag (org-supertag-db-get tag-id)))
    (let* ((fields (plist-get tag :fields))
           (new-fields (append fields (list field-def)))
           (new-tag (plist-put (copy-sequence tag) :fields new-fields)))
      (org-supertag-db-add tag-id new-tag)
      (message "Field added to tag '%s'." tag-id))))

(defun org-supertag-tag-rename-field (tag-id old-name new-name)
  "Rename a field in a tag's definition.
TAG-ID is the ID of the tag.
OLD-NAME is the current name of the field.
NEW-NAME is the new name for the field."
  (when-let ((tag (org-supertag-db-get tag-id)))
    (let* ((fields (plist-get tag :fields))
           (new-fields (mapcar (lambda (f)
                                 (if (string= (plist-get f :name) old-name)
                                     (plist-put (copy-sequence f) :name new-name)
                                   f))
                               fields))
           (new-tag (plist-put (copy-sequence tag) :fields new-fields)))
      (org-supertag-db-add tag-id new-tag)
      (message "Field '%s' renamed to '%s' in tag '%s'." old-name new-name tag-id))))

(defun org-supertag-tag--remove-field (tag-id field-name)
  "Remove a field from a tag's definition.
TAG-ID is the ID of the tag.
FIELD-NAME is the name of the field to remove.
Returns t if successful, nil otherwise."
  (when-let ((tag (org-supertag-db-get tag-id)))
    (let* ((fields (plist-get tag :fields))
           (new-fields (cl-remove-if (lambda (f)
                                       (string= (plist-get f :name) field-name))
                                     fields))
           (new-tag (plist-put (copy-sequence tag) :fields new-fields)))
      (org-supertag-db-add tag-id new-tag)
      (message "Field '%s' removed from tag '%s'." field-name tag-id)
      t)))

(defun org-supertag-tag--update-field-definition (tag-id field-name new-field-def)
  "Update the complete definition of a field in a tag.
TAG-ID is the ID of the tag.
FIELD-NAME is the name of the field to update.
NEW-FIELD-DEF is the new field definition.
Returns t if successful, nil otherwise."
  (when-let ((tag (org-supertag-db-get tag-id))
             (fields (plist-get tag :fields)))
    (let* ((new-fields (mapcar (lambda (f)
                                 (if (string= (plist-get f :name) field-name)
                                     new-field-def
                                   f))
                               fields))
           (new-tag (plist-put (copy-sequence tag) :fields new-fields)))
      (org-supertag-db-add tag-id new-tag)
      (message "Field '%s' definition updated in tag '%s'." field-name tag-id)
      t)))

(defun org-supertag-tag--create-field-definition ()
  "Interactively create a new field definition.
Returns a field definition plist, or nil if cancelled."
  (let* ((field-name (read-string "Field name: "))
         (field-type-choices (org-supertag-get-field-types))
         (field-type-str (completing-read "Field type: "
                                          (mapcar #'car field-type-choices)))
         (field-type (cdr (assoc field-type-str field-type-choices)))
         (field-def (list :name field-name :type field-type)))
    ;; For options type, ask for the options
    (when (eq field-type 'options)
      (let* ((options-input (read-string "Options (comma separated): "))
             (options-list (split-string options-input "," t "[ \t\n\r]+")))
        (setq field-def (plist-put field-def :options options-list))))
    field-def))

(defun org-supertag-tag--rename (old-name new-name)
  "Rename a tag in the database.
This is a data-layer function. Use `org-supertag-inline-rename` for interactive use.
OLD-NAME: The current name of the tag.
NEW-NAME: The new name for the tag."
  (unless (string-to-multibyte new-name)
    (error "New tag name '%s' contains invalid characters" new-name))
  (let* ((old-tag (org-supertag-tag-get old-name))
         (new-tag-name (org-supertag-sanitize-tag-name new-name)))
    (when (org-supertag-tag-get new-tag-name)
      (error "Tag %s already exists" new-tag-name))
    (unless old-tag
      (error "Tag %s not found" old-name))
    (org-supertag-db-rename old-name new-tag-name)))

(defun org-supertag-get-all-tags-with-prefix (prefix)
  "Get all tags starting with a specific prefix."
  (let ((tags (org-supertag-get-all-tags))
        (result '()))
    (dolist (tag tags result)
      (when (string-prefix-p prefix tag)
        (push tag result)))))

(defun org-supertag-tag-select-by-prefix (prefix)
  "Select a tag from a list of tags starting with a specific prefix.
PREFIX: The prefix to filter tags by."
  (interactive
   (list (read-string "Prefix: ")))
  (let* ((tags (org-supertag-get-all-tags-with-prefix prefix))
         (selected-tag (completing-read "Select tag: " tags nil t)))
    (message "Selected tag: %s" selected-tag)))

;;----------------------------------------------------------------------
;; Tag Extension System
;;----------------------------------------------------------------------

(defvar org-supertag--tag-all-fields-cache (make-hash-table :test 'equal)
  "Cache for `org-supertag-get-all-fields-for-tag` results.")

(defun org-supertag-get-all-fields-for-tag (tag-id &optional visited-tags)
  "Recursively get all fields for a TAG-ID, including inherited ones.
TAG-ID is the ID of the tag (string).
VISITED-TAGS is an optional list of tags already visited in the current
inheritance chain to detect circular dependencies.

Returns a list of field definition plists.
Signals an error if a circular dependency is detected."
  ;; Use a catch block for early return (e.g., cache hit)
  (catch 'org-supertag-get-all-fields-for-tag-return
    (unless (stringp tag-id)
      (error "Invalid tag-id: %S. Must be a string." tag-id))

    ;; 1. Check cache
    (let ((cached-fields (gethash tag-id org-supertag--tag-all-fields-cache)))
      (when cached-fields
        (message "DEBUG: Cache hit for tag %s" tag-id)
        (throw 'org-supertag-get-all-fields-for-tag-return cached-fields)))

    ;; 2. Circular dependency detection
    ;; Use a simple loop to check for circular dependencies
    (let ((found nil))
      (dolist (visited-tag visited-tags)
        (when (equal tag-id visited-tag)
          (setq found t)))
      (when found
        (error "Circular dependency detected in tag inheritance chain for tag: %S. Path: %S"
               tag-id (nreverse (cons tag-id visited-tags)))))

    (let* ((tag-props (org-supertag-db-get tag-id))
           (own-fields (plist-get tag-props :fields))
           (parent-tag-id (plist-get tag-props :extends))
           (all-fields-ht (make-hash-table :test 'equal)))

      ;; Recursively get parent fields
      (when parent-tag-id
        (message "DEBUG: Tag %s extends %s. Recursing..." tag-id parent-tag-id)
        (let ((parent-fields (org-supertag-get-all-fields-for-tag
                              parent-tag-id
                              (cons tag-id visited-tags))))
          (dolist (field parent-fields)
            (puthash (plist-get field :name) field all-fields-ht))))

      ;; Add own fields, potentially overriding parent fields
      (dolist (field own-fields)
        (puthash (plist-get field :name) field all-fields-ht))

      ;; Convert hash table values back to a list
      (let ((final-fields '()))
        (maphash (lambda (_key value)
                   (push value final-fields))
                 all-fields-ht)
        ;; Reverse to maintain original order (or a consistent order)
        (setq final-fields (nreverse final-fields))

        ;;(message "DEBUG: Calculated fields for %s: %S" tag-id final-fields)
        ;; 3. Store in cache
        (puthash tag-id final-fields org-supertag--tag-all-fields-cache)
        final-fields))))

(defun org-supertag-clear-tag-fields-cache (&rest _args)
  "Clear the tag fields cache.
This function should be called when any tag definition changes."
  (message "DEBUG: Clearing tag fields cache.")
  (clrhash org-supertag--tag-all-fields-cache))

;; Helper to get all tag IDs for completion
(defun org-supertag--get-all-tag-ids ()
  "Get a list of all tag IDs (names) in the database."
  (org-supertag-db-find-by-type :tag))

(defun org-supertag-tag-set-extends ()
  "Set or clear the inheritance relationship for a supertag.
This allows a tag to inherit fields from another parent tag.
A circular dependency check is performed before saving."
  (interactive)
  (let* ((child-tag-id (completing-read "Set extends for tag: "
                                       (org-supertag--get-all-tag-ids)
                                       nil t))
         (parent-tag-id (completing-read "Extend from tag (leave empty to clear): "
                                        (cons "" (org-supertag--get-all-tag-ids))
                                        nil t))
         (child-tag-props (org-supertag-db-get child-tag-id)))

    (unless child-tag-props
      (error "Tag '%s' not found." child-tag-id))

    ;; Normalize parent-tag-id (empty string means nil)
    (when (string-empty-p parent-tag-id)
      (setq parent-tag-id nil))

    ;; Check for self-extension
    (when (equal child-tag-id parent-tag-id)
      (error "A tag cannot extend itself: %S" child-tag-id))

    ;; Construct proposed new properties
    (let ((new-child-tag-props child-tag-props))
      (if parent-tag-id
          (setq new-child-tag-props (plist-put new-child-tag-props :extends parent-tag-id))
        (setq new-child-tag-props (plist-delete new-child-tag-props :extends)))

      ;; Perform circular dependency check using a dry run of field calculation
      (condition-case err
          (progn
            ;; Temporarily update the tag in memory for the check
            ;; This is a bit hacky, but avoids saving to DB before check
            ;; and leverages the existing circular dependency detection in
            ;; org-supertag-get-all-fields-for-tag.
            (let ((org-supertag-db--object (ht-copy org-supertag-db--object))) ;; Work on a copy
              (ht-set! org-supertag-db--object child-tag-id new-child-tag-props)
              ;; Try to get fields for the child tag with the new parent
              ;; This will trigger the circular dependency check
              (org-supertag-get-all-fields-for-tag child-tag-id))

            ;; If no error, proceed to save
            (org-supertag-db-add child-tag-id new-child-tag-props)
            (message "Tag '%s' now extends '%s'." child-tag-id (or parent-tag-id "nothing")))
        (error
         (message "Error setting inheritance: %s" (error-message-string err))
         (error "Inheritance not set due to error: %s" (error-message-string err)))))))

;; Add a listener to clear the cache when a tag entity is updated
(org-supertag-db-on 'entity:updated
                    (lambda (type id old-props new-props)
                      (when (eq type :tag)
                        (message "DEBUG: Tag entity '%s' updated. Clearing tag fields cache." id)
                        (org-supertag-clear-tag-fields-cache))))

;; Add a listener to clear the cache when a tag entity is created
(org-supertag-db-on 'entity:created
                    (lambda (type id props)
                      (when (eq type :tag)
                        (message "DEBUG: Tag entity '%s' created. Clearing tag fields cache." id)
                        (org-supertag-clear-tag-fields-cache))))

;; Add a listener to clear the cache when a tag entity is removed
(org-supertag-db-on 'entity:removed
                    (lambda (type id entity removed-data)
                      (when (eq type :tag)
                        (message "DEBUG: Tag entity '%s' removed. Clearing tag fields cache." id)
                        (org-supertag-clear-tag-fields-cache))))

;; Ensure cache is cleared on Emacs startup after DB load
(add-hook 'org-supertag-db-after-load-hook #'org-supertag-clear-tag-fields-cache)

(provide 'org-supertag-tag)

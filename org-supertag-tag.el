;;; org-supertag-tag.el --- Tag relation management for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides tag relationship management functionality
;; Core principle: Connect entities through relationships using type, from, and to

;;; Code:

(require 'org-supertag-db)
(require 'org-supertag-node)
(require 'org-supertag-field)

;;----------------------------------------------------------------------
;; Tag Name Operation
;;----------------------------------------------------------------------

(defun org-supertag-sanitize-tag-name (name)
  "Convert a name into a valid tag name.
NAME is the name to convert. It removes leading/trailing whitespace
and converts internal whitespace sequences to single underscores."
  (if (or (null name) (string-empty-p name))
      (error "Tag name cannot be empty")
    (let* ((trimmed (string-trim name))
           (sanitized (replace-regexp-in-string "\\s-+" "_" trimmed)))
      (if (string-empty-p sanitized)
          (error "Invalid tag name: %s" name)
        sanitized))))

(defun org-supertag-tag-exists-p (tag-name)
  "Check if a tag exists.
TAG-NAME is the name of the tag to check"
  (let ((sanitized-name (org-supertag-sanitize-tag-name tag-name)))
    (org-supertag-tag-get sanitized-name)))

(defun org-supertag-node-get-parent-tags (node-id)
  "Get the parent tags of the node.
NODE-ID: The ID of the current node"
  (save-excursion
    (when-let* ((pos (org-id-find node-id t)))
      (goto-char pos)
      (when (org-up-heading-safe)  
        (when-let ((parent-id (org-id-get)))  
          (org-supertag-node-get-tags parent-id))))))

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
  (let* ((sanitized-name (org-supertag-sanitize-tag-name tag-name))
         (parsed-name (org-supertag-tag--parse-name sanitized-name))
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
    (org-supertag-db-add sanitized-name base-props)
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
    
    ;; Text insertion is now handled here, controlled by the same variable.
    (unless org-supertag-skip-text-insertion
      (org-supertag-inline-insert-tag tag-id))

    (when-let* ((fields (plist-get tag :fields)))
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
    (dolist (field-def (plist-get tag :fields))
      (org-supertag-field-remove-value field-def node-id tag-id))))

;;----------------------------------------------------------------------
;; Tag User Command (Now data-layer functions)
;;----------------------------------------------------------------------

(defun org-supertag-tag--delete-at-all (tag-id)
  "Delete a tag completely from the database.
This is the data-layer function. For interactive use, see `org-supertag-inline-delete-all`.
TAG-ID: The tag identifier to delete."
  (when (yes-or-no-p (format "Delete tag '%s' from DB? This is a data-only operation. " tag-id))
    (let ((nodes (org-supertag-db-get-nodes-by-tag tag-id))
          (tag (org-supertag-tag-get tag-id)))
      
      (unless tag
        (error "Tag %s not found" tag-id))
      (unless (eq (plist-get tag :type) :tag)
        (error "Invalid tag type for %s" tag-id))

      (dolist (node-id nodes)
        (when-let* ((pos (condition-case nil (org-id-find node-id t) (error nil))))
          (org-with-point-at pos
            (dolist (field (plist-get tag :fields))
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

;;----------------------------------------------------------------------
;; Preset Tag
;;----------------------------------------------------------------------

(defcustom org-supertag-preset-tags
  '(("project" . ((:name "status"
                    :type options 
                    :options ("planning" "active" "on-hold" "completed" "cancelled")
                    :description "Project status")
                   (:name "priority"
                    :type options
                    :options ("high" "medium" "low") 
                    :description "Priority level")
                   (:name "deadline"
                    :type date
                    :description "Due date")
                   (:name "owner"
                    :type string
                    :description "Project owner")))
    ("task" . ((:name "status"
                :type options
                :options ("todo" "in-progress" "done" "cancelled")
                :description "Task status")
               (:name "priority" 
                :type options
                :options ("A" "B" "C")
                :description "Priority level")
               (:name "due"
                :type date
                :description "Due date")
               (:name "assignee"
                :type string
                :description "Assigned to")))
    ("person" . ((:name "role"
                  :type string
                  :description "Role")
                 (:name "email"
                  :type string
                  :description "Email address")
                 (:name "phone"
                  :type string
                  :description "Phone number")))
    ("meeting" . ((:name "date"
                   :type date
                   :description "Meeting date")
                  (:name "attendees"
                   :type string
                   :description "Attendees")
                  (:name "location"
                   :type string
                   :description "Location")))
    ("place" . ((:name "address"
                 :type string
                 :description "Address")
                (:name "category"
                 :type options
                 :options ("office" "home" "public" "other")
                 :description "Place type")))
    ("company" . ((:name "industry"
                   :type string
                   :description "Industry")
                  (:name "website"
                   :type string
                   :description "Website")
                  (:name "contact"
                   :type string
                   :description "Contact person")))
    ("note" . ((:name "category"
                :type options
                :options ("idea" "reference" "summary" "other")
                :description "Note type")
               (:name "source"
                :type string
                :description "Source"))))
  "Default preset tags with their field definitions."
  :type '(repeat (cons (string :tag "Tag name")
                      (repeat (plist :options ((:name string)
                                              (:type (choice (const string)
                                                             (const options)
                                                             (const date)))
                                              (:options (repeat string))
                                              (:description string))))))
  :group 'org-supertag
  :set (lambda (sym val) (set-default sym val)))

(defun org-supertag-get-preset-fields (tag-name)
  "Get predefined fields for a tag.
TAG-NAME is the name of the tag."
  (when-let* ((preset (assoc tag-name org-supertag-preset-tags)))
    (let ((fields (cdr preset)))
      (cl-loop for field in fields
               collect (let* ((name (plist-get field :name))
                              (type (plist-get field :type))
                              (options (plist-get field :options))
                              (description (plist-get field :description)))
                         (unless (and name type)
                           (error "Invalid field definition in preset tag %s: missing name or type" tag-name))
                         (append
                          (list :name name
                                :type type)
                          (when description
                            (list :description description))
                          (when (and options (eq type 'options))
                            (list :options options))))))))

(defun org-supertag-tag--set-field-value (tag-id node-id field-name value)
  "Helper function to set a field value in the database."
  (let* ((tag (org-supertag-tag-get tag-id))
         (field-def (cl-find field-name (plist-get tag :fields)
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

(defun org-supertag-set-field-and-value (tag field value)
  "Set field and value for a tag.
This is a convenience function for interactive use."
  (interactive
   (let* ((all-tags (org-supertag-get-all-tags))
          (tag (completing-read "Select tag: " all-tags nil t))
          (fields (mapcar (lambda (f) (plist-get f :name))
                          (plist-get (org-supertag-tag-get tag) :fields)))
          (field (completing-read "Select field: " fields nil t))
          (value (read-string (format "Enter value for %s: " field))))
     (list tag field value)))
  (org-supertag-tag--set-field-value tag (org-id-get) field value))

(provide 'org-supertag-tag)
;;; org-supertag-tag.el --- Tag relation management for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides tag relationship management functionality
;; Core principle: Connect entities through relationships using type, from, and to

;;----------------------------------------------------------------------
;; Tag Name Operation
;;----------------------------------------------------------------------

(defun org-supertag-sanitize-tag-name (name)
  "Convert a name into a valid tag name.
NAME is the name to convert
- Remove leading '#' characters
- Convert spaces to underscores
- Validate non-empty"
  (if (or (null name) (string-empty-p name))
      (error "Tag name cannot be empty")
    (let* ((trimmed (string-trim name))
           (without-hash (replace-regexp-in-string "^#+" "" trimmed))
           (sanitized (replace-regexp-in-string "\\s-+" "_" without-hash)))
      (message "Debug sanitize-tag-name: input=%s output=%s" name sanitized)
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

(defun org-supertag-tag-create (tag-name &rest props)
  "Create a new tag.
TAG-NAME: Name of the tag
PROPS: Additional properties including:
- :fields      List of field definitions
- :behaviors   List of behaviors"
  (let* ((sanitized-name (org-supertag-sanitize-tag-name tag-name))
         ;; Check if this is an extension tag
         (parsed-name (org-supertag-tag--parse-name sanitized-name))
         (base-tag-name (car parsed-name))
         ;; Get new fields
         (new-fields (plist-get props :fields))
         ;; If it's an extension, get base tag fields
         (base-tag (and base-tag-name 
                       (org-supertag-tag-exists-p base-tag-name)
                       (org-supertag-tag-get base-tag-name)))
         ;; Combine fields - ensure it's never nil, default to empty list
         (fields (or (if base-tag
                        ;; Project fields from base tag
                        (org-supertag-tag--project-fields 
                         base-tag
                         new-fields
                         (plist-get props :field-defaults))
                      ;; Otherwise just use new fields
                      new-fields)
                    '())) ; Default to empty list if nil
         ;; Ensure type is set
         (base-props (list :type :tag
                          :id sanitized-name
                          :name sanitized-name
                          :fields fields
                          :extend-from base-tag-name
                          :behaviors (plist-get props :behaviors)
                          :created-at (current-time))))
    
    (message "Creating tag with fields: %S" fields)
    (org-supertag-db-add sanitized-name base-props)
    sanitized-name))

(defun org-supertag-tag--project-fields (base-tag new-fields defaults)
  "Project fields from base tag and merge with new fields.
BASE-TAG: The base tag entity
NEW-FIELDS: Additional fields to add
DEFAULTS: Plist of field default value overrides"
  (let* ((base-fields (plist-get base-tag :fields))
         result)
    ;; Process base fields
    (dolist (field base-fields)
      (let* ((field-name (plist-get field :name))
             (default-override (plist-get defaults field-name))
             (projected-field
              (append 
               (list :projected-from (plist-get base-tag :id))  ; Mark as projected
               (if default-override
                   (plist-put (copy-sequence field) 
                            :default default-override)
                 (copy-sequence field)))))  ; Always copy field
        (unless (plist-get projected-field :type)
          (error "Field '%s' from base tag '%s' has no type defined"
                 field-name (plist-get base-tag :id)))
        (push projected-field result)))
    
    ;; Add new fields
    (dolist (field new-fields)
      (let ((field-name (plist-get field :name)))
        ;; Validate new field
        (unless field-name
          (error "New field must have a name"))
        (unless (plist-get field :type)
          (error "New field '%s' must have a type defined" field-name))
        
        ;; Check for field name conflicts
        (when (cl-find field-name result
                      :key (lambda (f) (plist-get f :name))
                      :test #'equal)
          (error "Field '%s' already exists in base tag '%s'"
                 field-name (plist-get base-tag :id)))
        (push (copy-sequence field) result)))  ; Always copy field
    
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
  "Apply tag to the node at current position."
  (let* ((tag (org-supertag-db-get tag-id))
         ;; Use forced ID if provided
         (node-id (or org-supertag-force-node-id 
                      ;; Otherwise check ID in headline properties
                      (org-entry-get nil "ID"))))
    
    ;; Only log debug info when debugging variable is set
    (when org-supertag-tag-apply-skip-duplicate-id
      (message "org-supertag-tag-apply: Initial node-id=%s (forced=%s)" 
               node-id org-supertag-force-node-id))
    
    ;; Only create new ID if none exists and we're not forcing a specific ID
    (setq node-id (or node-id (org-id-get-create)))
    
    (message "Applying tag: %s to node: %s" tag-id node-id)
    ;; Validation
    (unless tag
      (error "Tag %s not found" tag-id))
    (unless (eq (plist-get tag :type) :tag)
      (error "Invalid tag type for %s" tag-id))
    (unless (org-supertag-db-get node-id)
      (org-supertag-node-sync-at-point))
    
    ;; Link node and tag
    (org-supertag-node-db-add-tag node-id tag-id)
    
    ;; Apply fields (skip if we're only establishing the relationship)
    (unless org-supertag-skip-text-insertion
      (when-let* ((fields (plist-get tag :fields)))
        (message "Processing fields for tag %s: %S" tag-id fields)
        (dolist (field fields)
          (let* ((field-name (plist-get field :name))
                 (field-type (plist-get field :type))
                 (type-def (org-supertag-get-field-type field-type))
                 (initial-value (org-supertag-field-get-initial-value field)))
            (message "Processing field: name=%s type=%s initial=%S" 
                     field-name field-type initial-value)
            (unless type-def
              (error "Invalid field type: %s" field-type))

            (when-let* ((formatter (plist-get type-def :formatter))
                        (formatted-value (if formatter
                                           (funcall formatter initial-value field)
                                         (format "%s" initial-value))))
              (message "Setting field %s = %s" field-name formatted-value)
              ;; (org-set-property field-name formatted-value)
              (org-supertag-tag--set-field-value 
               tag-id node-id field-name initial-value))))))
        ;; Record tag relationships
    (when (featurep 'org-supertag-relation)
      (org-supertag-relation-record-cooccurrence node-id tag-id))
    
    ;; Behavior Active
    (run-hook-with-args 'org-supertag-after-tag-apply-hook node-id)
    (org-supertag-behavior--on-tag-change node-id tag-id :add)
    (org-supertag-behavior--apply-styles node-id)
    
    ;; Return node-id
    node-id))


(defun org-supertag-tag--remove (tag-id node-id)
  "Remove a tag from a node.
TAG-ID: The tag identifier
NODE-ID: The node identifier

This function:
1. Removes the tag-node relationship
2. Removes all associated field values"
  ;; 1. Remove tag-node relationship
  (org-supertag-db-remove-link :node-tag node-id tag-id)
  ;; 2. Remove associated field values
  (let ((tag (org-supertag-tag-get tag-id)))
    (dolist (field-def (plist-get tag :fields))
      (org-supertag-field-remove-value field-def node-id tag-id))))


(defun org-supertag-tag-change-tag ()
3  "Change an existing tag to another existing tag on current node."
  (interactive)
  (let* ((node-id (org-id-get))
         (current-tags (org-supertag-node-get-tags node-id))
         (source-tag (completing-read "Select tag to change: "
                                      current-tags nil t))
         (target-tag (read-string "Change to new tag name: ")))
    (when (and (not (string-empty-p source-tag))
               (not (string-empty-p target-tag)))
      (org-supertag-tag-remove source-tag)
      (org-supertag-tag-add-tag target-tag)
      (message "Changed tag '%s' to '%s'" source-tag target-tag))))



;;----------------------------------------------------------------------
;; Tag User Command
;;----------------------------------------------------------------------

(defun org-supertag-tag-add-tag (tag-name)
  "Add a tag to the current node."
  (interactive
   (list (completing-read "Tag to add: " (org-supertag-get-all-tags) nil t)))
  (let* ((node-id (org-id-get-create))
         (sanitized-name (org-supertag-sanitize-tag-name tag-name)))
    (unless (member sanitized-name (org-supertag-node-get-tags node-id))
      (org-supertag-node-db-add-tag node-id sanitized-name)
      (org-supertag-inline-insert-tag sanitized-name)
      (message "Tag '%s' added." sanitized-name))))

(defun org-supertag-tag-remove ()
  "Remove tag from current node."
  (interactive
   (let* ((node-id (org-id-get))
          (tags (org-supertag-node-get-tags node-id)))
     (list (completing-read "Remove tag: " tags nil t))))
  (let* ((tag-id (car (last (org-supertag-node-get-tags (org-id-get)))))
         (node-id (org-id-get)))
    (org-supertag-db-remove-link :node-tag node-id tag-id)
    (org-supertag-inline-remove-tag-at-point tag-id)
    (message "Removed tag '%s'" tag-id)))

(defun org-supertag-tag-delete-at-all (tag-id)
  "Delete a tag completely, including removing it from all nodes.
TAG-ID: The tag identifier to delete.
This will:
1. Remove the tag from all nodes that have it
2. Delete all field values associated with this tag
3. Delete all database entries related to this tag
4. Delete the tag definition from the database"
  (interactive (list (completing-read "Tag to delete: " (org-supertag-get-all-tags))))
  (when (yes-or-no-p (format "Delete tag '%s'? This will remove it from all nodes. " tag-id))
    (let ((nodes (org-supertag-tag-get-nodes tag-id))
          (tag (org-supertag-tag-get tag-id)))
      
      ;; Validation
      (unless tag
        (error "Tag %s not found" tag-id))
      (unless (eq (plist-get tag :type) :tag)
        (error "Invalid tag type for %s" tag-id))

      ;; 1. First remove tag from all nodes in org buffers
      (message "Removing tag from %d nodes in buffers..." (length nodes))
      (dolist (node-id nodes)
        ;; Remove from org buffer first
        (when-let* ((pos (condition-case nil
                           (org-id-find node-id t)
                         (error nil))))
          (org-with-point-at pos
            ;; Remove all field properties
            (dolist (field (plist-get tag :fields))
              (let ((field-name (plist-get field :name)))
                (org-delete-property field-name)))
            ;; ;; Remove the tag from org tags
            ;; (let* ((current-tags (org-get-tags))
            ;;        (new-tags (delete (concat "#" tag-id) current-tags)))
            ;;   (org-set-tags new-tags))
            ))))
      
      ;; 2. Find and collect all relationships to be removed
      (message "Collecting tag relationships...")
      (let ((links-to-remove '()))
        ;; Find all tag links (node-tag, field values, etc.)
        (maphash (lambda (link-id props)
                   (when (or (equal (plist-get props :from) tag-id)
                             (equal (plist-get props :to) tag-id)
                             (equal (plist-get props :tag-id) tag-id))
                     (push link-id links-to-remove)))
                 org-supertag-db--link)
        
        ;; Remove all collected links using proper API
        (message "Removing %d tag relationships..." (length links-to-remove))
        (dolist (link-id links-to-remove)
          (when-let* ((link-props (gethash link-id org-supertag-db--link)))
            (let ((link-type (plist-get link-props :type))
                  (from (plist-get link-props :from))
                  (to (plist-get link-props :to)))
              (org-supertag-db-remove-link link-type from to)))))
      
      ;; 3. Remove the tag using proper database API
      (message "Removing tag entity from database...")
      (message "DEBUG: Before DB remove, tag exists: %s" (org-supertag-db-exists-p tag-id))
      (org-supertag-db-remove-object tag-id)
      (message "DEBUG: After DB remove, tag exists: %s" (org-supertag-db-exists-p tag-id))
      (message "DEBUG: Is DB dirty? %s" org-supertag-db--dirty)
      
      ;; 4. Force database save to persist changes
      (message "Saving database...")
      (org-supertag-db--mark-dirty)
      (org-supertag-db-save)
      
      ;; 5. Run hooks if defined
      (when (boundp 'org-supertag-after-tag-delete-hook)
        (run-hook-with-args 'org-supertag-after-tag-delete-hook tag-id))
      
      (message "Deleted tag '%s' and removed it from %d nodes" tag-id (length nodes))))

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
  "Default preset tags with their field definitions.
Users can override this by setting it in their config files.

Format:
((tag-name . field-definitions) ...)

Each field definition is a plist with properties:
- :name        Field name
- :type        Field type (string, options, date, etc)
- :options     List of options (for options type)
- :description Field description

Example user configuration:
(setq org-supertag-preset-tags
      (append org-supertag-preset-tags
              '((\"book\" . ((:name \"status\"
                             :type options
                             :options (\"reading\" \"completed\" \"want-to-read\")
                             :description \"Reading status\")
                            (:name \"author\"
                             :type string
                             :description \"Author\"))))))"
  :type '(repeat (cons (string :tag "Tag name")
                      (repeat (plist :options ((:name string)
                                              (:type (choice (const string)
                                                            (const options)
                                                            (const date)))
                                              (:options (repeat string))
                                              (:description string))))))
  :group 'org-supertag
  :set (lambda (sym val)
         (set-default sym val)
         ;; Optionally add hook to refresh tag cache if needed
         ))

(defun org-supertag-get-preset-fields (tag-name)
  "Get predefined fields for a tag.
TAG-NAME is the name of the tag.

Returns a list of field definitions with the following properties:
- :name        Field name (required)
- :type        Field type (required)
- :options     List of options (for options type)
- :description Field description (optional)

Example return value:
((:name \"status\"
  :type options
  :options (\"todo\" \"in-progress\" \"done\")
  :description \"Task status\")
 (:name \"priority\"
  :type options
  :options (\"high\" \"medium\" \"low\")))"
  (when-let* ((preset (assoc tag-name org-supertag-preset-tags)))
    (let ((fields (cdr preset)))
          ;; Validate and normalize field definitions
          (cl-loop for field in fields
                   collect (let* ((name (plist-get field :name))
                                 (type (plist-get field :type))
                                (options (plist-get field :options))
                                (description (plist-get field :description)))
                                
                                ;; Ensure required fields are present
                                (unless (and name type)
                                  (error "Invalid field definition in preset tag %s: missing name or type" tag-name))
                                
                                ;; Build normalized field definition
                                (append
                                 (list :name name
                                       :type type)
                                 ;; Add optional properties
                                 (when description
                                   (list :description description))
                                 (when (and options (eq type 'options))
                                   (list :options options))))))))


(defun org-supertag-tag--set-field-value (tag-id node-id field-name value)
  "Helper function to set a field value in the database.
TAG-ID: The ID of the tag.
NODE-ID: The ID of the node.
FIELD-NAME: The name of the field.
VALUE: The value to set."
  (let* ((tag (org-supertag-tag-get tag-id))
         (field-def (cl-find field-name (plist-get tag :fields)
                             :key (lambda (f) (plist-get f :name))
                             :test #'string=)))
    (when field-def
      ;; Call the centralized function to set the field value.
      (org-supertag-field-set-value node-id field-name value tag-id))))

(defun org-supertag-tag-remove (tag-id)
  "Remove tag from the node at current position."
  (interactive
   (let* ((node-id (org-id-get))
          (tags (org-supertag-node-get-tags node-id)))
     (list (completing-read "Remove tag: " tags nil t))))
  (let* ((tag (org-supertag-db-get tag-id))
         (node-id (org-id-get)))
    ;; Validation
    (unless tag
      (error "Tag %s not found" tag-id))
    (unless (eq (plist-get tag :type) :tag)
      (error "Invalid tag type for %s" tag-id))
    
    (org-supertag-node-db-remove-tag node-id tag-id)
    
    (message "Tag '%s' removed from node %s" tag-id node-id)))

(defun org-supertag-tag-add-field (tag-id field-def)
  "Add a new field definition to a tag.
TAG-ID is the ID of the tag.
FIELD-DEF is the new field definition plist (e.g., '(:name \"new-field\" :type 'string))."
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
      ;; Note: This does not automatically migrate existing values from the old field name.
      ;; That would require a more complex migration step.
      (message "Field '%s' renamed to '%s' in tag '%s'." old-name new-name tag-id))))

(defun org-supertag-tag-rename (old-name new-name)
  "Rename a tag.
OLD-NAME: The current name of the tag.
NEW-NAME: The new name for the tag."
  (interactive
   (list (read-string "Old tag name: ")
         (read-string "New tag name: ")))
  (let* ((old-tag (org-supertag-tag-get old-name))
         (new-tag (org-supertag-tag-get new-name)))
    (when (and old-tag new-tag)
      (error "Tag %s already exists" new-name))
    (org-supertag-db-rename old-name new-name)
    (message "Renamed tag '%s' to '%s'" old-name new-name)))

(defun org-supertag-tag-delete-at-all (tag-id)
  "Delete tag at all nodes."
  (interactive
   (list (completing-read "Tag to delete at all nodes: " (org-supertag-get-all-tags) nil t)))
  (let ((nodes (org-supertag-db-get-nodes-by-tag tag-id)))
    (dolist (node-id nodes)
      (org-supertag-db-remove-link :node-tag node-id tag-id))
    (message "Deleted tag '%s' from %d nodes" tag-id (length nodes))))

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
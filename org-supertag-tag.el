;;; org-supertag-tag.el --- Tag relation management for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides tag relationship management functionality
;; Core principle: Connect entities through relationships using type, from, and to

(require 'org-supertag-tag-company)
(require 'org-supertag-field-operation)

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
         ;; Combine fields
         (fields (if base-tag
                    ;; Project fields from base tag
                    (org-supertag-tag--project-fields 
                     base-tag
                     new-fields
                     (plist-get props :field-defaults))
                  ;; Otherwise just use new fields
                  new-fields))
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

;;----------------------------------------------------------------------
;; Tag-Node Relation Operation
;;----------------------------------------------------------------------

(defvar org-supertag-tag-apply-skip-headline nil
  "When non-nil, `org-supertag-tag-apply' will not add the tag to headline tags.")

(defun org-supertag-tag-apply (tag-id)
  "Apply tag to the node at current position."
  (let* ((tag (org-supertag-db-get tag-id))
         (node-id (org-id-get-create)))
    (message "Applying tag: %s to node: %s" tag-id node-id)
    ;; Validation
    (unless tag
      (error "Tag %s not found" tag-id))
    (unless (eq (plist-get tag :type) :tag)
      (error "Invalid tag type for %s" tag-id))
    (unless (org-supertag-db-get node-id)
      (org-supertag--create-node node-id))
    
    ;; Link node and tag
    (org-supertag-node-db-add-tag node-id tag-id)
    ;; Apply fields
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
            (org-set-property field-name formatted-value)
            (org-supertag-tag--set-field-value 
             tag-id node-id field-name initial-value)))))
    
    ;; Add tag to node tags (unless skipped for inline tags)
    (unless org-supertag-tag-apply-skip-headline
      (let ((tags (org-get-tags)))
        (org-set-tags (cons (concat "#" tag-id) tags))))
    
    ;; Record tag relationships
    (when (featurep 'org-supertag-relation)
      (org-supertag-relation-record-cooccurrence node-id tag-id))
    
    ;; Behavior Active
    (run-hook-with-args 'org-supertag-after-tag-apply-hook node-id)
    (org-supertag-behavior--on-tag-change node-id tag-id :add)
    (org-supertag-behavior--apply-styles node-id)
    
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
  "Change an existing tag to another existing tag on current node.
This function:
1. Lists current node's tags for selection
2. Lists available existing tags (excluding current node's tags) as target
3. Removes old tag and applies new tag"
  (interactive)
  (let* ((node-id (org-id-get))
         (current-tags (org-supertag-node-get-tags node-id)))
    
    ;; Validation
    (unless node-id
      (error "Not on a valid node"))
    (unless current-tags
      (error "No tags on current node"))
    
    ;; Select source tag to change
    (let* ((source-tag (completing-read "Select tag to change: " 
                                      current-tags nil t))
           ;; Get all existing tags except current node's tags
           (all-tags (cl-set-difference 
                     (org-supertag-get-all-tags)
                     current-tags
                     :test #'string=))
           ;; Ensure we have available tags to change to
           (_ (unless all-tags
                (error "No other existing tags available")))
           ;; Select target tag
           (target-tag (completing-read 
                       (format "Change '%s' to: " source-tag)
                       all-tags nil t)))
      
      ;; Execute tag change
      (save-excursion
        ;; 1. Remove old tag
        (org-supertag-tag--remove source-tag node-id)
        ;; Remove from org tags
        (let* ((current-org-tags (org-get-tags))
               (new-org-tags (delete (concat "#" source-tag) 
                                   current-org-tags)))
          (org-set-tags new-org-tags))
        
        ;; 2. Apply new tag
        (org-supertag-tag-apply target-tag))
      
      (message "Changed tag '%s' to '%s'" source-tag target-tag))))



;;----------------------------------------------------------------------
;; Tag User Command
;;----------------------------------------------------------------------

(defun org-supertag-tag-add-tag (tag-name)
  "Add a tag to the current headline.
TAG-NAME can be an existing tag or a new tag name.
Will prevent duplicate tag application.

Special input format:
- Normal input: show completion with existing and preset tags
- Ending with #: directly create new tag without completion"
  (interactive
   (let* ((all-tags (org-supertag-get-all-tags))
          (preset-names (mapcar #'car org-supertag-preset-tags))
          ;; Remove any existing preset tags from all-tags to avoid duplicates
          (user-tags (cl-remove-if (lambda (tag) (member tag preset-names)) all-tags))
          (candidates (delete-dups
                      (append 
                       ;; Format preset tags with [P] prefix
                       (mapcar (lambda (name) 
                               (format "[P] %s" name))
                             preset-names)
                       ;; Regular tags as is
                       user-tags)))
          (input (completing-read
                 "Enter tag name (TAB: complete, end with #: direct create): "
                 candidates nil nil)))
     (list
      (cond
       ((string-prefix-p "[P] " input)
        (substring input 4))  ; Remove [P] prefix
       (t input)))))
  
  (when tag-name  
    (let* ((node-id (org-id-get))
           ;; Check if input ends with # and remove it
           (direct-create (string-suffix-p "#" tag-name))
           (tag-name-clean (if direct-create
                              (substring tag-name 0 -1)
                            tag-name))
           (sanitized-name (org-supertag-sanitize-tag-name tag-name-clean))
           (current-tags (org-supertag-node-get-tags node-id)))
      (message "Adding tag: %s" sanitized-name)
      ;; Check for duplicate tag
      (when (member sanitized-name current-tags)
        (user-error "Tag '%s' is already applied to this node" sanitized-name))
      
      (let* ((existing-tag (org-supertag-tag-get sanitized-name))
             ;; Get or create the tag
             (tag-id
              (cond
               ;; If tag exists, use it
               (existing-tag
                sanitized-name)
               ;; If it's a preset tag, create with preset fields
               ;; Otherwise create a new empty tag
               (t
                ;; If direct creation or confirmed, create tag
                (if (or direct-create
                        (y-or-n-p (format "Create new tag '%s'? " sanitized-name)))
                    (org-supertag-tag-create sanitized-name)
                  (user-error "Tag creation cancelled"))))))
        ;; Apply the tag
        (org-supertag-tag-apply tag-id)
        ;; Skip immediate field value input for preset tags
        (message "Tag '%s' applied. Use `org-supertag-tag-edit-fields' to edit field values." tag-id)))))

(defun org-supertag-tag-batch-add-tag (tag-name)
  "Batch add tags to selected headlines.
TAG-NAME is the name of the tag to add."
  (interactive
   (list (completing-read "Select tag: "
                         (append 
                          (org-supertag-get-all-tags)
                          (mapcar #'car org-supertag-preset-tags)))))
  (let* ((ast (org-element-parse-buffer))
         (headlines '())
         (selected-headlines '())
         (tag (org-supertag-tag-get tag-name)))
    
    ;; Collect all headlines without ID
    (org-element-map ast 'headline
      (lambda (headline)
        (unless (org-element-property :ID headline)
          (push headline headlines))))
    
    ;; Build choices list
    (let ((choices (append 
                   '(("Finish" . :finish))  ; Add finish option
                   (mapcar (lambda (hl)
                            (cons (org-element-property :raw-value hl)
                                 hl))
                          headlines))))
      
      ;; Loop until user selects Finish
      (while (when-let* ((title (completing-read 
                                (format "Select headline (%d selected): "
                                       (length selected-headlines))
                                (mapcar #'car choices)
                                nil t))
                         (choice (cdr (assoc title choices))))
                (unless (eq choice :finish)
                  ;; Add to selected list
                  (push choice selected-headlines)
                  ;; Remove selected option
                  (setq choices (assoc-delete-all title choices))
                  t)))  ; Continue loop unless Finish is selected
      
      ;; Apply tags to all selected headlines
      (when selected-headlines
        (dolist (hl selected-headlines)
          (save-excursion
            (goto-char (org-element-property :begin hl))
            (org-supertag-tag-add-tag tag-name)))
        (message "Added tag %s to %d headlines" 
                 tag-name
                 (length selected-headlines))))))

(defun org-supertag-tag-remove ()
  "Remove tag from current node.
This function:
1. Removes tag from org tags
2. Calls internal function to remove tag data"
  (interactive)
  (let* ((node-id (org-id-get))
         (tags (org-supertag-node-get-tags node-id))
         (tag-id (completing-read "Select tag to remove: " tags nil t)))
    (unless node-id
      (error "Not on a valid node"))
    (unless tags
      (error "No tags on current node"))
    ;; 1. Remove from org tags
    (let* ((current-tags (org-get-tags))
           (tag-name (concat "#" tag-id))
           (new-tags (delete tag-name current-tags)))
      (org-set-tags new-tags))
    ;; 2. Remove tag data using internal function
    (org-supertag-tag--remove tag-id node-id)
    (message "Removed tag '%s' and its fields" tag-id)))

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
            ;; Remove the tag from org tags
            (let* ((current-tags (org-get-tags))
                   (new-tags (delete (concat "#" tag-id) current-tags)))
              (org-set-tags new-tags)))))
      
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


(defalias 'org-supertag-set-field-and-value 'org-supertag-tag-edit-fields)

(provide 'org-supertag-tag)

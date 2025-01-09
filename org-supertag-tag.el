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

(defun org-supertag-tag-create (tag-name &rest props)
  "Create a new tag.
TAG-NAME: Name of the tag
PROPS: Additional properties"
  (let* ((sanitized-name (org-supertag-sanitize-tag-name tag-name))
         (fields (or (plist-get props :fields) '()))
         (base-props (list :type :tag
                          :id sanitized-name
                          :name sanitized-name
                          :fields fields
                          :behaviors (plist-get props :behaviors)
                          :created-at (current-time))))
    (message "Creating tag with fields: %S" fields)
    (org-supertag-db-add sanitized-name base-props)
    (let ((saved-tag (org-supertag-db-get sanitized-name)))
      (message "Saved tag: %S" saved-tag)
      (unless saved-tag
        (error "Failed to create tag: %s" sanitized-name)))
    sanitized-name))

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
    (when-let ((fields (plist-get tag :fields)))
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
    
    ;; Add tag to node tags
    (let ((tags (org-get-tags)))
      (org-set-tags (cons (concat "#" tag-id) tags)))
    
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
;; Tag Field Operation
;;----------------------------------------------------------------------

(defun org-supertag--sanitize-field-name (name)
  "Convert field name to valid property name.
NAME is the original field name.

This function:
1. Trims leading/trailing whitespace
2. Replaces spaces with underscores
3. Converts to uppercase"
  (let ((sanitized (replace-regexp-in-string
                   "\\s-+" "_"  ; Replace spaces with underscores
                   (upcase (string-trim name)))))  ; Convert to uppercase and trim
    sanitized))

(defun org-supertag-tag-get-nodes (tag-id)
  "Get all node IDs associated with a tag.
TAG-ID: The tag identifier (without 'tag:' prefix)

Returns a list of node IDs that have this tag attached."
  (let (result)
    (maphash
     (lambda (link-id props)
       (message "Checking link: %s with props: %s" link-id props)
       (when (and (string-prefix-p ":node-tag:" link-id)
                  (equal (plist-get props :to) tag-id))  ; Direct comparison, no prefix handling
         (push (plist-get props :from) result)))
     org-supertag-db--link)
    (delete-dups result)))

(defun org-supertag-tag-get-fields (tag-id)
  "Get list of fields defined for a tag.
TAG-ID: The tag identifier"
  (plist-get (org-supertag-tag-get tag-id) :fields))

(defun org-supertag-tag-add-field (tag-id field-def)
  "Add a field to a tag.
TAG-ID: The tag identifier
FIELD-DEF: The field definition plist"
  (let* ((tag (org-supertag-db-get tag-id))
         (fields (plist-get tag :fields))
         (new-fields (append fields (list field-def)))
         (new-tag (plist-put (copy-sequence tag) :fields new-fields)))
    (org-supertag-db-add tag-id new-tag)))

(defun org-supertag-tag-get-field-value (tag field-name)
  "Get field value from TAG with FIELD-NAME.
TAG is the tag entity
FIELD-NAME is the name of the field to get"
  (when-let* ((fields (plist-get tag :fields))
              (field (cl-find field-name fields
                             :key (lambda (f) (plist-get f :name))
                             :test #'equal)))
    (plist-get field :value)))

(defun org-supertag-tag--set-field-value (tag-id node-id field-name value)
  "Set field value for TAG-ID on NODE-ID.
FIELD-NAME is the name of the field
VALUE is the value to set"
  (let ((tag (org-supertag-tag-get tag-id)))
    (when tag
      ;; Update field value in tag definition
      (let* ((fields (plist-get tag :fields))
             (field (cl-find field-name fields
                            :key (lambda (f) (plist-get f :name))
                            :test #'equal)))
        (when field
          (setf (plist-get field :value) value)
          ;; Update tag in database
          (org-supertag-db-add tag-id tag)
          ;; Run hook
          (run-hook-with-args 'org-supertag-node-field-updated-hook
                             node-id field-name value))))))

;;----------------------------------------------------------------------
;; Tag User Command
;;----------------------------------------------------------------------

(defun org-supertag-tag-add-tag (tag-name)
  "Add a tag to the current headline.
TAG-NAME can be an existing tag or a new tag name.
Will prevent duplicate tag application."
  (interactive
   (let* ((all-tags (org-supertag-get-all-tags))
          (preset-names (mapcar #'car org-supertag-preset-tags))
          (candidates (delete-dups
                      (append all-tags
                             (mapcar (lambda (name) 
                                     (concat "Preset: " name))
                                   preset-names))))
          (input (completing-read-default
                 "Enter tag name (TAB: complete): "
                 ;; Custom Compeleting function
                 (lambda (string pred action)
                   (if (eq action 'metadata)
                       '(metadata (category . org-supertag-tag))
                     (let ((exact-match (member string candidates)))
                       (cond
                        ((eq action 'lambda) ; exact match test
                         (member string candidates))
                        ((eq action t)       ; all matches
                         (cl-remove-if-not
                          (lambda (candidate)
                            (string-prefix-p string candidate t))
                          candidates))
                        (t string)))))       ; return input string
                 nil nil)))  ; 允许输入新标签
     (list
      (cond
       ((member input candidates) input)
       ((string-prefix-p "Preset: " input)
        (substring input (length "Preset: ")))
       (t input)))))
  
  (when tag-name  
    (let* ((node-id (org-id-get))
           (sanitized-name (org-supertag-sanitize-tag-name tag-name))
           (current-tags (org-supertag-node-get-tags node-id)))
      (message "Adding tag: %s" sanitized-name)
      ;; Check for duplicate tag
      (when (member sanitized-name current-tags)
        (user-error "Tag '%s' is already applied to this node" sanitized-name))
      
      (let* ((existing-tag (org-supertag-tag-get sanitized-name))
             (preset-fields (org-supertag-get-preset-fields sanitized-name))
             ;; Get or create the tag
             (tag-id
              (cond
               (existing-tag
                sanitized-name)
               (preset-fields
                (progn
                  (message "Creating new tag from preset with fields: %S" preset-fields)
                  (org-supertag-tag-create sanitized-name :fields preset-fields)))
               (t
                (if (y-or-n-p (format "Create new tag '%s'? " sanitized-name))
                    (org-supertag-tag-create sanitized-name)
                  (user-error "Tag creation cancelled"))))))
        ;; Apply the tag
        (org-supertag-tag-apply tag-id)))))

(defun org-supertag-tag-set-field-value ()
  "Set field value for tags on current node.
This function allows interactive editing of field values for all tags
attached to the current node. It displays a list of fields with their
current values and lets the user select and modify them one by one."
  (interactive)
  (let* ((node-id (org-id-get))
         ;; Get all tags for the node
         (tags (org-supertag-node-get-tags node-id))
         ;; Collect field information for all tags
         (tag-fields (cl-loop for tag-id in tags
                             for tag = (org-supertag-tag-get tag-id)
                             when tag
                             collect (cons tag-id 
                                         (plist-get tag :fields))))
         ;; Build list of all fields
         (all-fields (cl-loop for (tag-id . fields) in tag-fields
                             when fields
                             append (mapcar (lambda (field)
                                            (list :tag-id tag-id
                                                  :field field
                                                  :current-value (org-entry-get nil 
                                                                             (plist-get field :name))))
                                          fields)))
         ;; Display and edit options
         (field-values
          (cl-loop for field-info in all-fields
                   for tag-id = (plist-get field-info :tag-id)
                   for field = (plist-get field-info :field)
                   for field-name = (plist-get field :name)
                   for current-value = (plist-get field-info :current-value)
                   collect
                   (cons
                    (format "[%s] %s (current: %s)" tag-id field-name 
                           (or current-value "unset"))
                    (cons tag-id field)))))
    ;; Let user select fields to edit
    (while (when-let* ((choice (completing-read "Select field to edit (C-g to finish): "
                                              (mapcar #'car field-values)
                                              nil t))
                       (field-info (cdr (assoc choice field-values)))
                       (tag-id (car field-info))
                       (field (cdr field-info)))
             ;; Set the field value
             (org-supertag-field-set-value field
                                          (org-supertag-field-read-value field)
                                          node-id
                                          tag-id)
             t))))

(defun org-supertag-tag-set-field-and-value ()
  "Set field values for tags on the current node.
This function allows setting field values for tags attached to the current node.
It provides options to:
1. Edit existing fields
2. Add new fields (create new or copy existing)
3. Add preset fields from templates"
  (interactive)
  (let* ((node-id (org-id-get))
         (tags (org-supertag-node-get-tags node-id)))
    
    (unless node-id
      (error "Not on a valid node"))
    
    (when (null tags)
      (when (y-or-n-p "No tags on current node. Add a tag?")
        (call-interactively #'org-supertag-tag-add-tag)
        ;; Refresh tags list
        (setq tags (org-supertag-node-get-tags node-id))))
    
    (unless tags
      (user-error "At least one tag is required to set fields"))
    
    ;; Collect all existing fields for reuse
    (let* ((existing-fields
            (cl-loop for tag-id in tags
                     for tag = (org-supertag-tag-get tag-id)
                     append
                     (cl-loop for field in (plist-get tag :fields)
                             for field-name = (plist-get field :name)
                             for field-type = (plist-get field :type)
                             collect (cons (format "[%s] %s (type: %s)"
                                                 tag-id
                                                 field-name
                                                 field-type)
                                         (list :name field-name
                                               :type field-type)))))
           (_ (message "Collected existing fields: %S" existing-fields))
           ;; Build choices list
           (choices
            (cl-loop for tag-id in tags
                     for tag = (org-supertag-tag-get tag-id)
                     append
                     ;; Existing fields
                     (cl-loop for field in (plist-get tag :fields)
                             for field-name = (plist-get field :name)
                             for field-type = (plist-get field :type)
                             for current-value = (org-supertag-field-get-value 
                                                field node-id tag-id)
                             collect
                             (cons (format "[%s] %s (type: %s, current: %s)"
                                         tag-id field-name field-type
                                         (or current-value "unset"))
                                       (list :type :existing
                                            :tag-id tag-id
                                            :field field)))
                     ;; New field options
                     collect (cons (format "[%s] + Create new field" tag-id)
                                 (list :type :new-create
                                      :tag-id tag-id))
                     collect (cons (format "[%s] + Copy existing field" tag-id)
                                 (list :type :new-copy
                                      :tag-id tag-id))
                     ;; Preset fields
                     append
                     (cl-loop for preset in org-supertag-preset-fields
                             for preset-name = (car preset)
                             for preset-desc = (plist-get (cdr preset) :description)
                             collect
                             (cons (format "[%s] + Preset: %s (%s)"
                                         tag-id preset-name preset-desc)
                                   (list :type :preset
                                        :tag-id tag-id
                                        :preset-name preset-name))))))
      ;; Loop until user presses C-g
      (while t
        (let* ((choice (completing-read "Select field: " choices nil t))
               (choice-data (cdr (assoc choice choices))))
          
          (pcase (plist-get choice-data :type)
            ;; Existing field
            (:existing
             (let ((field (plist-get choice-data :field))
                   (tag-id (plist-get choice-data :tag-id)))
               (message "Setting value for field '%s' (type: %s) in tag '%s'"
                        (plist-get field :name)
                        (plist-get field :type)
                        tag-id)
               (org-supertag-field-set-value 
                field
                (org-supertag-field-read-value field)
                node-id
                tag-id)))
            ;; Create new field
            (:new-create
             (let* ((tag-id (plist-get choice-data :tag-id))
                    (field-name (read-string "Field name: "))
                    (type-choices
                     (mapcar (lambda (type-def)
                              (let ((type-name (car type-def))
                                    (type-spec (cdr type-def)))
                                (format "%s - %s"
                                        (symbol-name type-name)
                                        (plist-get type-spec :description))))
                            org-supertag-field-types))
                    (field-type-choice
                     (completing-read 
                      (format "Field type for '%s': " field-name)
                      type-choices
                      nil t))
                    (field-type (intern (car (split-string field-type-choice " - "))))
                    (field-def (list :name field-name
                                   :type field-type)))
               (message "Adding new field '%s' (type: %s) to tag '%s'"
                        field-name field-type tag-id)
               (org-supertag-tag-add-field tag-id field-def)
               (org-supertag-field-set-value 
                field-def
                (org-supertag-field-read-value field-def)
                node-id
                tag-id)))
            ;; Copy existing field
            (:new-copy
             (let* ((tag-id (plist-get choice-data :tag-id))
                    (field-choice
                     (completing-read 
                      "Select field to copy: "
                      (mapcar #'car existing-fields)
                      nil t))
                    (field-template (cdr (assoc field-choice existing-fields)))
                    ;; 只使用名称和类型创建新字段
                    (field-def (list :name (read-string 
                                          "Field name (press Enter to keep original): "
                                          (plist-get field-template :name))
                                   :type (plist-get field-template :type)))
                    ;; 验证字段定义
                    (_ (unless (and (plist-get field-def :name)
                                  (plist-get field-def :type))
                         (error "Invalid field definition: missing name or type"))))
               (message "Copying field definition '%s' (type: %s) to tag '%s'"
                        (plist-get field-def :name)
                        (plist-get field-def :type)
                        tag-id)
               (org-supertag-tag-add-field tag-id field-def)
               (org-supertag-field-set-value 
                field-def
                (org-supertag-field-read-value field-def)
                node-id
                tag-id)))
            ;; Preset field
            (:preset
             (let* ((tag-id (plist-get choice-data :tag-id))
                    (preset-name (plist-get choice-data :preset-name))
                    (field-def (org-supertag-get-preset-field preset-name)))
               (message "Adding preset field '%s' (type: %s) to tag '%s'"
                        preset-name
                        (plist-get field-def :type)
                        tag-id)
               (org-supertag-tag-add-field tag-id field-def)
               (org-supertag-field-set-value 
                field-def
                (org-supertag-field-read-value field-def)
                node-id
                tag-id)))))))))

(defun org-supertag-tag-batch-add-tag (tag-name)
  "Batch add tags to selected headlines.
TAG-NAME is the name of the tag to add"
  (interactive
   (list (completing-read "Select tag: "
                         (append 
                          (org-supertag-get-all-tags)
                          (mapcar #'car org-supertag-preset-tags)))))  ; Only get preset tag names
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

;;----------------------------------------------------------------------
;; Preset Tag
;;----------------------------------------------------------------------

(defvar org-supertag-preset-tags
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
                :options ("todo" "in-progress" "blocked" "done" "cancelled")
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
- :type        Field type (string, options, date, number, etc)
- :options     List of options (for options type)
- :description Field description

Example user configuration:
(setq org-supertag-preset-tags
      '((\"book\" . ((name \"status\"
                     :type options
                     :options (\"reading\" \"completed\" \"want-to-read\")
                     :description \"Reading status\")
                    (:name \"rating\"
                     :type number
                     :min 1
                     :max 5
                     :description \"Book rating\")
                    (:name \"author\"
                     :type string
                     :description \"Author\")))))")

(defun org-supertag-get-preset-fields (tag-name)
  "Get predefined fields for a tag.
TAG-NAME is the name of the tag."
  (message "Getting preset fields for tag: %s" tag-name)
  (when-let ((preset (assoc tag-name org-supertag-preset-tags)))
    (let ((fields (cdr preset)))
      (message "Found preset fields: %S" fields)
      fields)))

(defun org-supertag-tag-edit-preset ()
  "Edit preset tag definitions interactively.
This function allows:
1. View current preset tag definitions
2. Edit fields of existing preset tags
3. Add new preset tags
4. Remove preset tags"
  (interactive)
  (let* ((choices
          (append
           ;; View definitions
           '(("View preset definitions" . :view))
           ;; Add new preset
           '(("Add new preset tag" . :new))
           ;; Edit existing presets
           (mapcar (lambda (preset)
                    (cons (format "Edit '%s'" (car preset))
                          (car preset)))
                  org-supertag-preset-tags)))
         (choice (completing-read "Select action: " 
                                (mapcar #'car choices)
                                nil t))
         (action (cdr (assoc choice choices))))
    (pcase action
      ;; View definitions
      (:view
       (with-current-buffer (get-buffer-create "*Org Supertag Presets*")
         (erase-buffer)
         (insert "Org Supertag Preset Definitions:\n\n")
         (dolist (preset org-supertag-preset-tags)
           (let ((tag-name (car preset))
                 (fields (cdr preset)))
             (insert (format "* %s\n" tag-name))
             (dolist (field fields)
               (insert (format "  - %s (%s)\n"
                             (plist-get field :name)
                             (plist-get field :type))))))
         (org-mode)
         (display-buffer (current-buffer))))
      
      ;; Add new preset
      (:new
       (let* ((tag-name (read-string "Tag name: "))
              (fields '()))
         ;; Add fields interactively
         (while (y-or-n-p "Add field? ")
           (let* ((field-name (read-string "Field name: "))
                  (field-type (completing-read 
                             "Field type: "
                             '("string" "options" "date" "number")
                             nil t))
                  (field-def
                   (list :name field-name
                         :type (intern field-type))))
             ;; Add options for options type
             (when (string= field-type "options")
               (let ((options
                      (split-string
                       (read-string "Options (comma separated): ")
                       "," t "[ \t]+")))
                 (setq field-def 
                       (plist-put field-def :options options))))
             ;; Add description
             (let ((desc (read-string "Description (optional): ")))
               (when (not (string-empty-p desc))
                 (setq field-def 
                       (plist-put field-def :description desc))))
             (push field-def fields)))
         ;; Add new preset
         (customize-save-variable
          'org-supertag-preset-tags
          (cons (cons tag-name (nreverse fields))
                org-supertag-preset-tags))))
      ;; Edit existing preset
      ((pred stringp)
       (let* ((tag-name action)
              (current-fields (cdr (assoc tag-name org-supertag-preset-tags)))
              (field-choices
               (append
                '(("Add new field" . :new)
                  ("Remove tag" . :remove))
                (mapcar (lambda (field)
                         (cons (format "Edit '%s'" 
                                     (plist-get field :name))
                               field))
                       current-fields)))
              (field-choice
               (completing-read 
                (format "Edit '%s': " tag-name)
                (mapcar #'car field-choices)
                nil t))
              (field-action (cdr (assoc field-choice field-choices))))
         (pcase field-action
           ;; Add new field
           (:new
            (let* ((field-name (read-string "Field name: "))
                   (field-type (completing-read 
                              "Field type: "
                              '("string" "options" "date" "number")
                              nil t))
                   (field-def
                    (list :name field-name
                          :type (intern field-type))))
              ;; Add options for options type
              (when (string= field-type "options")
                (let ((options
                       (split-string
                        (read-string "Options (comma separated): ")
                        "," t "[ \t]+")))
                  (setq field-def 
                        (plist-put field-def :options options))))
              ;; Add description
              (let ((desc (read-string "Description (optional): ")))
                (when (not (string-empty-p desc))
                  (setq field-def 
                        (plist-put field-def :description desc))))
              ;; Update preset
              (customize-save-variable
               'org-supertag-preset-tags
               (cons (cons tag-name 
                          (append current-fields (list field-def)))
                     (assoc-delete-all 
                      tag-name org-supertag-preset-tags)))))
           ;; Remove tag
           (:remove
            (when (yes-or-no-p 
                   (format "Remove preset tag '%s'? " tag-name))
              (customize-save-variable
               'org-supertag-preset-tags
               (assoc-delete-all tag-name org-supertag-preset-tags))))
           
           ;; Edit field
           ((pred listp)
            (let* ((field field-action)
                   (field-name (plist-get field :name))
                   (field-type (plist-get field :type))
                   (actions
                    '(("Edit options" . :options)
                      ("Edit description" . :description)
                      ("Remove field" . :remove)))
                   (action
                    (intern
                     (completing-read 
                      (format "Edit field '%s' (%s): "
                              field-name field-type)
                      actions
                      nil t))))
              (pcase action
                (:options
                 (when (eq field-type 'options)
                   (let ((new-options
                          (split-string
                           (read-string "New options (comma separated): ")
                           "," t "[ \t]+")))
                     (setq field
                           (plist-put field :options new-options))
                     (customize-save-variable
                      'org-supertag-preset-tags
                      (cons (cons tag-name
                                 (mapcar (lambda (f)
                                         (if (equal (plist-get f :name)
                                                  field-name)
                                             field
                                           f))
                                       current-fields))
                            (assoc-delete-all 
                             tag-name org-supertag-preset-tags))))))
                (:description
                 (let ((new-desc
                        (read-string "New description: "
                                    (plist-get field :description))))
                   (setq field
                         (plist-put field :description new-desc))
                   (customize-save-variable
                    'org-supertag-preset-tags
                    (cons (cons tag-name
                               (mapcar (lambda (f)
                                       (if (equal (plist-get f :name)
                                                field-name)
                                           field
                                         f))
                                     current-fields))
                          (assoc-delete-all 
                           tag-name org-supertag-preset-tags)))))
                (:remove
                 (when (yes-or-no-p 
                        (format "Remove field '%s'? " field-name))
                   (customize-save-variable
                    'org-supertag-preset-tags
                    (cons (cons tag-name
                               (remove field current-fields))
                          (assoc-delete-all 
                           tag-name org-supertag-preset-tags))))))))))))))

(defun org-supertag--parse-preset-field (field-def)
  "Parse preset field definition.
FIELD-DEF is the preset field definition"
  (let* ((name (car field-def))
         (props (cdr field-def)))
    (message "Parsing preset field: %s with props: %S" name props)
    (let ((parsed-field
           (append 
            (list :name name
                  :type (plist-get props :type))
            ;; 可选属性
            (when-let ((desc (plist-get props :description)))
              (list :description desc))
            (when-let ((values (plist-get props :values)))
              (list :options values))
            (when-let ((min (plist-get props :min)))
              (list :min min))
            (when-let ((max (plist-get props :max)))
              (list :max max))
            (when-let ((sep (plist-get props :separator)))
              (list :separator sep)))))
      (message "Parsed field result: %S" parsed-field)
      parsed-field)))



(provide 'org-supertag-tag)

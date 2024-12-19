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
    (org-supertag-tag-db-get sanitized-name)))


;;----------------------------------------------------------------------
;; Tag Base Operation
;;----------------------------------------------------------------------

(defun org-supertag-tag-create (tag-name &rest props)
  "Create a new tag.
TAG-NAME: Name of the tag
PROPS: Additional properties"
  (let* ((sanitized-name (org-supertag-sanitize-tag-name tag-name))
         (fields (or (plist-get props :fields) '()))  ; 先获取字段
         (base-props (list :type :tag
                          :id sanitized-name
                          :name sanitized-name
                          :fields fields      ; 使用获取的字段
                          :created-at (current-time))))
    
    (message "Debug create-tag: props=%S" props)
    (message "Debug create-tag: fields=%S" fields)
    (message "Debug create-tag: base-props=%S" base-props)
    (org-supertag-db-add sanitized-name base-props)
    (message "Debug create-tag: 保存后的标签=%S" 
             (org-supertag-db-get sanitized-name))
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

;;----------------------------------------------------------------------
;; Tag-Node Relation Operation
;;----------------------------------------------------------------------

(defun org-supertag-tag-apply (tag-id)
  "Apply tag to the node at current position.
TAG-ID is the tag identifier.

This function will:
1. Validate the tag exists and is valid
2. Create node ID if needed 
3. Link the tag to the node
4. Initialize tag fields with default values
5. Add tag to org tags"
  (let* ((tag (org-supertag-db-get tag-id))
         (node-id (org-id-get-create)))
    ;; Ensure tag exists
    (unless tag
      (error "Tag %s not found" tag-id))
    ;; Ensure it's a valid tag
    (unless (eq (plist-get tag :type) :tag)
      (error "Invalid tag type for %s" tag-id))
    ;; Create node if needed
    (unless (org-supertag-db-get node-id)
      (org-supertag--create-node node-id))
    ;; Create tag association
    (org-supertag-db-link 
     :node-tag 
     node-id 
     tag-id 
     (list :created-at (current-time)))
    ;; Initialize fields
    (when-let ((fields (plist-get tag :fields)))
      (dolist (field fields)
        (let* ((field-name (plist-get field :name))
               (field-type (plist-get field :type))
               ;; Set initial value based on field type
               (initial-value 
                (pcase field-type
                  ('options 
                   (car (plist-get field :options)))  ; Use first option as default
                  ('date 
                   (format-time-string "%Y-%m-%d"))   ; Use current date
                  ('string 
                   nil)                               ; Use nil instead of empty string
                  (_ nil))))                          ; Default to nil
          ;; Set property only if initial value exists
          (when initial-value
            (org-set-property field-name initial-value))
          ;; Initialize field value in database
          (org-supertag-tag--set-field-value 
           tag-id node-id field-name initial-value))))
    ;; Add tag to org tags
    (let ((tags (org-get-tags)))
      (org-set-tags (cons (concat "#" tag-id) tags)))
    ;; Return node ID
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
  (let ((tag (org-supertag-tag-db-get tag-id)))
    (dolist (field-def (plist-get tag :fields))
      (org-supertag-field-remove-value field-def node-id tag-id))))

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
  (plist-get (org-supertag-tag-db-get tag-id) :fields))

(defun org-supertag-tag-add-field (tag-id field-def)
  "Add a field to a tag.
TAG-ID: The tag identifier
FIELD-DEF: The field definition plist"
  (let* ((tag (org-supertag-db-get tag-id))
         (fields (plist-get tag :fields))
         (new-fields (append fields (list field-def)))
         (new-tag (plist-put (copy-sequence tag) :fields new-fields)))
    (org-supertag-db-add tag-id new-tag)))

(defun org-supertag-tag--set-field-value (tag-id node-id field-name value)
  "Internal function to set a value for a tag field.
TAG-ID: The tag identifier
NODE-ID: The node identifier 
FIELD-NAME: Name of the field
VALUE: Value to set for the field"
  (message "DEBUG: Setting field value - Tag: %s, Node: %s, Field: %s, Value: %s"
           tag-id node-id field-name value)
  (let* ((tag (org-supertag-db-get tag-id))
         (fields (plist-get tag :fields))
         (field-def (cl-find field-name fields 
                            :key (lambda (f) (plist-get f :name))
                            :test #'equal)))
    (if (not field-def)
        (error "Field %s not found in tag %s" field-name tag-id)
      (org-supertag-field-set-value field-def value node-id tag-id))))

;;----------------------------------------------------------------------
;; Tag User Command
;;----------------------------------------------------------------------

(defun org-supertag-tag-add-tag (tag-name)
  "Add a tag to the current headline.
TAG-NAME can be an existing tag or a new tag name"
  (interactive
   (let* ((existing-tags (org-supertag-db-find-by-type :tag))  ; Get all existing tags
          (tag-choices
           (delete-dups
            (append
             ;; 1. Existing tags
             (mapcar (lambda (tag-id)
                      (let ((tag (org-supertag-db-get tag-id)))
                        (plist-get tag :name)))
                    existing-tags)
             ;; 2. Preset tags (unused)
             (cl-remove-if
              (lambda (preset-name)
                (member preset-name
                        (mapcar (lambda (tag-id)
                                (plist-get (org-supertag-db-get tag-id) :name))
                              existing-tags)))
              org-supertag-preset-tags)))))
     (list
      (completing-read "Select or enter new tag name: " tag-choices nil nil))))
  
  (let* ((sanitized-name (org-supertag-sanitize-tag-name tag-name))
         (preset-fields (org-supertag-get-preset-fields sanitized-name)))
    
    ;; Get or create the tag
    (let ((tag-id
           (if (org-supertag-tag-get sanitized-name)
               sanitized-name  ; If tag exists, use name as ID
             ;; If new tag, create it
             (org-supertag-tag-create sanitized-name 
                                    :fields preset-fields))))
      
      ;; Apply the tag
      (org-supertag-tag-apply tag-id))))

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
                       (field-def (cdr field-info)))
             ;; Set the field value
             (org-supertag-field-set-value field-def
                                          (org-supertag-field-read-value field-def)
                                          node-id
                                          tag-id)
             t))))

(defun org-supertag-tag-set-field-and-value ()
  "Set field values for tags on the current node.
This function allows setting field values for tags attached to the current node.
It provides options to:
1. Edit existing fields
2. Add new fields 
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
    
    ;; Build choices list
    (let* ((choices
            (cl-loop for tag-id in tags
                     for tag = (org-supertag-tag-get tag-id)
                     append
                     ;; Existing fields
                     (cl-loop for field in (plist-get tag :fields)
                             for field-name = (plist-get field :name)
                             for current-value = (org-supertag-field-get-value 
                                                field node-id tag-id)
                             collect
                             (cons (format "[%s] %s (current: %s)"
                                         tag-id field-name 
                                         (or current-value "unset"))
                                   (list :type :existing
                                        :tag-id tag-id
                                        :field field)))
                     ;; New field option
                     collect (cons (format "[%s] + Add new field" tag-id)
                                 (list :type :new
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
               (org-supertag-field-set-value 
                field
                (org-supertag-field-read-value field)
                node-id
                tag-id)))
            ;; New field
            (:new
             (let* ((tag-id (plist-get choice-data :tag-id))
                    (field-name (read-string "Field name: "))
                    (field-type (completing-read 
                               "Field type: "
                               org-supertag-field-types
                               nil t))
                    (field-def (list :name field-name
                                    :type (intern field-type))))
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
   (list (completing-read "选择或输入标签: "
                         (append 
                          (org-supertag-get-all-tags)
                          org-supertag-preset-tags)))
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
                 (length selected-headlines)))))))


(defun org-supertag-tag-remove ()
  "Remove tag association and its field values from current node."
  (interactive)
  (let* ((node-id (org-id-get))
         (tags (org-supertag-node-get-tags node-id))
         (tag-id (completing-read "Select tag to remove: " tags nil t)))
    
    ;; Remove tag using internal function
    (org-supertag-tag--remove tag-id node-id)
    
    (message "Removed tag '%s' association and its field values" tag-id)))
;;----------------------------------------------------------------------
;; Preset Tag
;;----------------------------------------------------------------------

(defconst org-supertag-preset-tags
  '("project" "task" "person" "meeting" "place" "company" "note")
  "List of predefined tags.")

(defconst org-supertag-preset-tag-fields
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
                :description "Source")))))

(defun org-supertag-get-preset-fields (tag-name)
  "Get predefined fields for a tag.
TAG-NAME is the name of the tag."
  (cdr (assoc tag-name org-supertag-preset-tag-fields)))

(provide 'org-supertag-tag)

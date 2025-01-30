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

(defun org-supertag-tag-remove-field (tag-id field-name)
  "Remove a field from a tag.
TAG-ID: The tag identifier
FIELD-NAME: The name of the field to remove"
  (interactive
   (let* ((node-id (org-id-get))
          (tags (org-supertag-node-get-tags node-id))
          (tag-id (completing-read "Select tag: " tags nil t))
          (tag (org-supertag-tag-get tag-id))
          (fields (plist-get tag :fields))
          (field-name (completing-read 
                      "Select field to remove: "
                      (mapcar (lambda (field)
                              (plist-get field :name))
                             fields)
                      nil t)))
     (list tag-id field-name)))
  
  (when (yes-or-no-p (format "Remove field '%s' from tag '%s'? This will remove all values of this field." 
                            field-name tag-id))
    (let* ((tag (org-supertag-db-get tag-id))
           (fields (plist-get tag :fields))
           ;; Remove field from fields list
           (new-fields (cl-remove-if (lambda (field)
                                     (equal (plist-get field :name)
                                            field-name))
                                   fields))
           ;; Update tag
           (new-tag (plist-put (copy-sequence tag) :fields new-fields)))
      ;; Update tag in database
      (org-supertag-db-add tag-id new-tag)
      ;; Remove field values from all nodes
      (let ((nodes (org-supertag-tag-get-nodes tag-id)))
        (dolist (node-id nodes)
          ;; Remove from database
          (org-supertag-field-db-delete-value node-id field-name tag-id)
          ;; Remove from org buffer
          (when-let ((pos (condition-case nil
                             (org-id-find node-id t)
                           (error nil))))
            (org-with-point-at pos
              (org-delete-property field-name)))))
      (message "Removed field '%s' from tag '%s'" field-name tag-id))))

(defun org-supertag-tag-batch-set-fields (tag-id)
  "Batch set field values for a tag.
TAG-ID: The tag identifier"
  (interactive
   (let* ((node-id (org-id-get))
          (tags (org-supertag-node-get-tags node-id))
          (tag-id (completing-read "Select tag: " tags nil t)))
     (list tag-id)))
  
  (let* ((tag (org-supertag-tag-get tag-id))
         (fields (plist-get tag :fields))
         (nodes (org-supertag-tag-get-nodes tag-id))
         (field-values (make-hash-table :test 'equal)))
    
    ;; Collect field values
    (dolist (field fields)
      (let* ((field-name (plist-get field :name))
             (field-type (plist-get field :type))
             (value (org-supertag-field-read-value field)))
        (puthash field-name value field-values)))
    
    ;; Apply values to all nodes
    (dolist (node-id nodes)
      (dolist (field fields)
        (let* ((field-name (plist-get field :name))
               (value (gethash field-name field-values)))
          (org-supertag-field-set-value field value node-id tag-id))))
    
    (message "Batch set field values for tag '%s' completed." tag-id)))

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
                 "Enter tag name (TAB: complete): "
                 candidates nil nil)))
     (list
      (cond
       ((string-prefix-p "[P] " input)
        (substring input 4))  ; Remove [P] prefix
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
               ;; If tag exists, use it
               (existing-tag
                sanitized-name)
               ;; If it's a preset tag, create with preset fields
               (preset-fields
                (progn
                  (message "Creating new tag from preset with fields: %S" preset-fields)
                  (org-supertag-tag-create sanitized-name :fields preset-fields)))
               ;; Otherwise create a new empty tag
               (t
                (if (y-or-n-p (format "Create new tag '%s'? " sanitized-name))
                    (org-supertag-tag-create sanitized-name)
                  (user-error "Tag creation cancelled"))))))
        ;; Apply the tag
        (org-supertag-tag-apply tag-id)
        ;; Skip immediate field value input for preset tags
        (message "Tag '%s' applied. Use `org-supertag-tag-set-field-and-value' to set field values." tag-id)))))

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

(defun org-supertag-tag-delete (tag-id)
  "Delete a tag completely, including removing it from all nodes.
TAG-ID: The tag identifier to delete.
This will:
1. Remove the tag from all nodes that have it
2. Delete all field values associated with this tag
3. Delete the tag definition from the database"
  (interactive (list (completing-read "Tag to delete: " (org-supertag-get-all-tags))))
  (when (yes-or-no-p (format "Delete tag '%s'? This will remove it from all nodes. " tag-id))
    (let ((nodes (org-supertag-tag-get-nodes tag-id))
          (tag (org-supertag-db-get tag-id)))
      ;; Validation
      (unless tag
        (error "Tag %s not found" tag-id))
      (unless (eq (plist-get tag :type) :tag)
        (error "Invalid tag type for %s" tag-id))

      ;; Remove tag from all nodes
      (dolist (node-id nodes)
        ;; Remove tag link from node
        (org-supertag-node-db-remove-tag node-id tag-id)
        ;; Remove all field values for this tag
        (dolist (field (plist-get tag :fields))
          (let ((field-name (plist-get field :name)))
            ;; Remove from database
            (org-supertag-field-db-delete-value node-id field-name tag-id)
            ;; Remove from org buffer
            (when-let ((pos (condition-case nil
                              (org-id-find node-id t)
                            (error nil))))
              (org-with-point-at pos
                (org-delete-property field-name))))))

      ;; Delete tag from database
      (org-supertag-db-remove-object tag-id)
      (message "Deleted tag '%s' and removed it from %d nodes" tag-id (length nodes)))))

;;----------------------------------------------------------------------
;; Field Edit Mode
;;----------------------------------------------------------------------  

(defun org-supertag-tag-set-field-and-value ()
  "Set field values for tags on the current node using a table interface."
  (interactive)
  (org-supertag-tag-edit-fields-table))

(define-derived-mode org-supertag-field-edit-mode special-mode "OrgSuperTag-FieldEdit"
  "Major mode for editing org-supertag fields."
  (setq-local buffer-read-only nil)
  
  ;; 定义按键绑定
  (let ((map org-supertag-field-edit-mode-map))
    (define-key map (kbd "n") #'org-supertag-field-edit-next-field)
    (define-key map (kbd "p") #'org-supertag-field-edit-prev-field)
    (define-key map (kbd "RET") #'org-supertag-field-edit-complete-field)
    (define-key map (kbd "C-c C-c") #'org-supertag-field-edit-save)
    (define-key map (kbd "C-c C-k") #'org-supertag-field-edit-cancel)))

(defun org-supertag-tag--insert-tag-fields (tag-id source-buffer source-point)
  "Insert fields for a tag.
TAG-ID is the tag identifier.
SOURCE-BUFFER is the buffer containing the org node.
SOURCE-POINT is the point in SOURCE-BUFFER."
  (let* ((tag (org-supertag-tag-get tag-id))
         (fields (plist-get tag :fields))
         ;; 1. Calculate the maximum field name length
         (max-name-length
          (if fields
              (apply #'max
                    (mapcar (lambda (field)
                             (length (plist-get field :name)))
                           fields))
              0))
         ;; 2. Calculate the maximum type name length
         (max-type-length
          (if fields
              (apply #'max
                    (mapcar (lambda (field)
                             (length (symbol-name (plist-get field :type))))
                           fields))
              0)))

    (insert (format "Tag: %s\n" tag-id))
    (insert "──────────────────────────────────────────────\n")
    (if fields
        (progn
          (dolist (field fields)
            (let* ((field-name (plist-get field :name))
                   (field-type (plist-get field :type))
                   (current-value 
                    (with-current-buffer source-buffer
                      (save-excursion
                        (goto-char source-point)
                        (org-entry-get (point) field-name nil t))))
                   ;; 3. Use fixed width formatting for fields
                   (formatted-line
                    (format (format "  %%-%ds [%%-%ds]: %%s\n" 
                                  max-name-length 
                                  max-type-length)
                           field-name
                           (symbol-name field-type)
                           (or current-value ""))))
              (insert formatted-line)))
          (insert "  [Press 'a' to add, 'e' to edit, 'd' to delete fields]\n"))
      ;; If no fields defined
      (insert "  No fields defined\n")
      (insert "  [Press 'a' to add fields]\n"))))

(defun org-supertag-tag--add-field (tag-id)
  "Add a new field to TAG-ID."
  (let* ((field-name (read-string "Field name: "))
         (all-types (mapcar #'car org-supertag-field-types))
         (type-help
          (concat
           "Available types:\n"
           (mapconcat
            (lambda (type)
              (let ((desc (plist-get (cdr (assq type org-supertag-field-types))
                                   :description)))
                (format "  %-12s - %s" type desc)))
            all-types
            "\n")))
         (field-type
          (minibuffer-with-setup-hook
              (lambda ()
                (let ((inhibit-read-only t))
                  (with-current-buffer (get-buffer-create "*Minibuf Help*")
                    (erase-buffer)
                    (insert type-help)
                    (display-buffer (current-buffer)
                                  '((display-buffer-in-side-window)
                                    (side . bottom)
                                    (window-height . fit-window-to-buffer))))))
            (intern
             (completing-read 
              "Field type: "
              (mapcar #'symbol-name all-types)
              nil t))))
         (field-def
          (list :name field-name
                :type field-type)))
    ;; 清理帮助 buffer
    (when-let ((help-buf (get-buffer "*Minibuf Help*")))
      (kill-buffer help-buf))
    ;; Add options for options type
    (when (eq field-type 'options)
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
    ;; Add field to tag
    (org-supertag-tag-add-field tag-id field-def)))

(defun org-supertag-tag--refresh-field-table (context)
  "Refresh the field table display.
CONTEXT is the edit context plist containing:
- :node-id        The node ID
- :node-title     The node title
- :tags           List of tags
- :source-buffer  The source buffer
- :source-point   The point in source buffer"
  (let ((inhibit-read-only t)
        (current-point (point)))
    (erase-buffer)
    ;; Header
    (insert (format "Fields for Node: %s\n" 
                    (plist-get context :node-title)))
    ;; Fields
    (dolist (tag-id (plist-get context :tags))
      (org-supertag-tag--insert-tag-fields 
       tag-id
       (plist-get context :source-buffer)
       (plist-get context :source-point)))
    ;; Help text
    (insert "\nCommands:\n")
    (insert "n: Next field            p: Previous field\n")
    (insert "RET: Edit field value    a: Add field\n")
    (insert "e: Edit field            d: Delete field\n")
    (insert "C-c C-c: Save           C-c C-k: Cancel\n")
    ;; Restore point
    (goto-char (min current-point (point-max)))))

(defun org-supertag-tag--edit-field (tag-id field-name)
  "Edit an existing field in TAG-ID with FIELD-NAME."
  (let* ((tag (org-supertag-tag-get tag-id))
         (fields (plist-get tag :fields))
         (field (cl-find field-name fields
                        :key (lambda (f) (plist-get f :name))
                        :test #'equal))
         (action (completing-read 
                 "Action: "
                 '("Edit type" "Edit options" "Edit description")
                 nil t)))
    (pcase action
      ("Edit type"
       (let* ((all-types (mapcar #'car org-supertag-field-types))
              (type-help
               (concat
                "Available types:\n"
                (mapconcat
                 (lambda (type)
                   (let ((desc (plist-get (cdr (assq type org-supertag-field-types))
                                        :description)))
                     (format "  %-12s - %s" type desc)))
                 all-types
                 "\n")))
              (new-type
               (minibuffer-with-setup-hook
                   (lambda ()
                     (let ((inhibit-read-only t))
                       (with-current-buffer (get-buffer-create "*Minibuf Help*")
                         (erase-buffer)
                         (insert type-help)
                         (display-buffer (current-buffer)
                                       '((display-buffer-in-side-window)
                                         (side . bottom)
                                         (window-height . fit-window-to-buffer))))))
                 (intern
                  (completing-read 
                   "New type: "
                   (mapcar #'symbol-name all-types)
                   nil t)))))
         ;; 清理帮助 buffer
         (when-let ((help-buf (get-buffer "*Minibuf Help*")))
           (kill-buffer help-buf))
         
         (setq field (plist-put field :type new-type))
         ;; 如果新类型是 options，添加选项
         (when (eq new-type 'options)
           (let ((options
                  (split-string
                   (read-string "Options (comma separated): ")
                   "," t "[ \t]+")))
             (setq field (plist-put field :options options))))
         ;; 更新字段列表
         (let ((new-fields (cl-substitute field
                                        (cl-find field-name fields
                                                :key (lambda (f) (plist-get f :name))
                                                :test #'equal)
                                        fields
                                        :test #'equal)))
           ;; 更新 tag 定义
           (setq tag (plist-put tag :fields new-fields))
           (org-supertag-db-add tag-id tag))))
      
      ("Edit options"
       (when (eq (plist-get field :type) 'options)
         (let ((new-options
                (split-string
                 (read-string "New options (comma separated): "
                            (mapconcat #'identity 
                                     (plist-get field :options)
                                     ","))
                 "," t "[ \t]+")))
           (setq field (plist-put field :options new-options))
           ;; 更新字段列表
           (let ((new-fields (cl-substitute field
                                          (cl-find field-name fields
                                                  :key (lambda (f) (plist-get f :name))
                                                  :test #'equal)
                                          fields
                                          :test #'equal)))
             ;; 更新 tag 定义
             (setq tag (plist-put tag :fields new-fields))
             (org-supertag-db-add tag-id tag)))))
      
      ("Edit description"
       (let ((new-desc (read-string "New description: "
                                   (plist-get field :description)))
         (setq field (plist-put field :description new-desc))
         ;; 更新字段列表
         (let ((new-fields (cl-substitute field
                                        (cl-find field-name fields
                                                :key (lambda (f) (plist-get f :name))
                                                :test #'equal)
                                        fields
                                        :test #'equal)))
           ;; 更新 tag 定义
           (setq tag (plist-put tag :fields new-fields))
           (org-supertag-db-add tag-id tag))))))))

(defun org-supertag-tag--goto-first-field ()
  "Move cursor to the first field in the OrgSuperTag-FieldEdit buffer.
If no field exists, move to the first line after the separator line."
  (let ((field-buffer (get-buffer "*Org SuperTag Fields*")))
    (when field-buffer
      (with-current-buffer field-buffer
        (goto-char (point-min))
        ;; Find first separator line
        (if (re-search-forward "^─+$" nil t)
            (progn
              ;; Try to find first field
              (forward-line 1)
              (if (looking-at "^  \\([^[]+\\)\\[\\([^]]+\\)\\]: \\(.*\\)$")
                  (beginning-of-line)
                ;; If no field, stay at current line
                t))
          ;; If no separator found, go to beginning
          (goto-char (point-min)))))))

(defun org-supertag-tag-edit-fields-table ()
  "Edit fields of current node's tags in a table format."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (source-point (point))
         (node-id (org-id-get))
         (tags (org-supertag-node-get-tags node-id))
         (node-title (org-get-heading t t t t))
         (edit-buffer (get-buffer-create "*Org SuperTag Fields*")))
    
    ;; Prepare the edit buffer
    (with-current-buffer edit-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-supertag-field-edit-mode)
        
        ;; Store context
        (setq-local org-supertag--edit-context
                    (list :node-id node-id
                          :node-title node-title
                          :tags tags
                          :source-buffer source-buffer
                          :source-point source-point))
        
        ;; Initial content
        (insert (format "Fields for Node: %s\n" node-title))
        (dolist (tag-id tags)
          (org-supertag-tag--insert-tag-fields tag-id source-buffer source-point))
        
        ;; Add help text
        (insert "\nCommands:\n")
        (insert "TAB: Next field          S-TAB: Previous field\n")
        (insert "RET: Edit field value    a: Add fields\n")
        (insert "e: Edit field            d: Delete field\n")
        (insert "C-c C-c: Save           C-c C-k: Cancel\n")
        
        ;; Add key bindings for field operations
        (define-key org-supertag-field-edit-mode-map
          (kbd "a")
          (lambda ()
            (interactive)
            (let ((tag-id (save-excursion
                           (re-search-backward "^Tag: \\(.+\\)$" nil t)
                           (match-string 1))))
              (org-supertag-tag--add-field tag-id)
              (org-supertag-tag--refresh-field-table org-supertag--edit-context))))
        
        (define-key org-supertag-field-edit-mode-map
          (kbd "e")
          (lambda ()
            (interactive)
            (save-excursion
              (beginning-of-line)
              (when (looking-at "^  \\([^ ]+\\) \\[")
                (let* ((field-name (match-string 1))
                       (tag-id (save-excursion
                                (re-search-backward "^Tag: \\(.+\\)$" nil t)
                                (match-string 1))))
                  (org-supertag-tag--edit-field tag-id field-name)
                  (org-supertag-tag--refresh-field-table org-supertag--edit-context))))))
        
        (define-key org-supertag-field-edit-mode-map
          (kbd "d")
          (lambda ()
            (interactive)
            (save-excursion
              (beginning-of-line)
              (when (looking-at "^  \\([^ ]+\\) \\[")
                (let* ((field-name (match-string 1))
                       (tag-id (save-excursion
                                (re-search-backward "^Tag: \\(.+\\)$" nil t)
                                (match-string 1))))
                  (when (yes-or-no-p (format "Delete field '%s'? " field-name))
                    (org-supertag-tag-remove-field tag-id field-name)
                    (org-supertag-tag--refresh-field-table org-supertag--edit-context)))))))))
    
    ;; Display buffer below current buffer and position cursor
    (let ((window (display-buffer edit-buffer
                                '((display-buffer-below-selected)
                                  (window-height . fit-window-to-buffer)
                                  (preserve-size . (nil . t))
                                  (select . t)))))  
      (select-window window)
      ;; 2. Set the cursor position
      (goto-char (point-min))
      (re-search-forward "^─+$" nil t)
      (forward-line 1))))

(defun org-supertag-field-edit-next-field ()
  "Move to next editable field."
  (interactive)
  (forward-line)
  (when (looking-at "^$")
    (forward-line)))

(defun org-supertag-field-edit-prev-field ()
  "Move to previous editable field."
  (interactive)
  (forward-line -1)
  (when (looking-at "^$")
    (forward-line -1)))

(defun org-supertag-field-edit-complete-field ()
  "Edit the current field value."
  (interactive)
  (when (looking-at "^  \\([^[]+\\)\\[\\([^]]+\\)\\]: \\(.*\\)$")
    (let* ((field-name (match-string 1))
           (field-type (match-string 2))
           (current-value (match-string 3))
           (tag-id (save-excursion
                    (re-search-backward "^Tag: \\(.+\\)$" nil t)
                    (match-string 1)))
           (tag (org-supertag-tag-get tag-id))
           (field (cl-find (string-trim field-name)
                          (plist-get tag :fields)
                          :key (lambda (f) (plist-get f :name))
                          :test #'equal))
           (new-value (org-supertag-field-read-value field)))
      (when new-value
        (save-excursion
          (beginning-of-line)
          (when (re-search-forward ": .*$" nil t)
            (replace-match (format ": %s" new-value))))))))

(defun org-supertag-field-edit-save ()
  "Save all field values and close the edit buffer."
  (interactive)
  (let* ((context org-supertag--edit-context)
         (node-id (plist-get context :node-id))
         (source-buffer (plist-get context :source-buffer))
         (source-point (plist-get context :source-point)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^  \\([^[]+\\)\\[\\([^]]+\\)\\]: \\(.*\\)$" nil t)
        (let ((field-name (string-trim (match-string 1)))
              (new-value (string-trim (match-string 3))))
          (unless (string-empty-p new-value)
            (with-current-buffer source-buffer
              (save-excursion
                (goto-char source-point)
                (org-set-property field-name new-value)))))))
    (quit-window t)))

(defun org-supertag-field-edit-cancel ()
  "Cancel editing and close the buffer."
  (interactive)
  (when (yes-or-no-p "Cancel editing? Changes will be lost.")
    (quit-window t)))

;;------------------------------------------------------------
;; Company Completion
;;------------------------------------------------------------

;;(require 'company)

(defvar org-supertag-company-prefix-regexp
  "#\\([[:word:]:-]*\\)"
  "Regexp to match the prefix for company completion.")

(defun org-supertag-company--make-candidate (tag-name)
  "Create a company candidate from TAG-NAME."
  (let* ((tag (org-supertag-tag-get tag-name))
         (fields (plist-get tag :fields))
         (field-count (length fields)))
    (propertize tag-name
                'tag tag
                'help-echo (format "SuperTag with %d field(s)" field-count))))

(defun org-supertag-company--candidates (prefix)
  "Get completion candidates for PREFIX."
  (let* ((prefix-no-hash (if (> (length prefix) 1)
                            (org-supertag-sanitize-tag-name (substring prefix 1))
                          ""))
         (all-tags (org-supertag-get-all-tags)))
    (cl-loop for tag-name in all-tags
             when (string-prefix-p prefix-no-hash tag-name t)
             collect (org-supertag-company--make-candidate tag-name))))

(defun org-supertag-company--post-completion (candidate)
  "Handle post-completion actions for CANDIDATE."
  (let* ((tag (get-text-property 0 'tag candidate))
         (tag-name (plist-get tag :name)))
    ;; Delete the completion prefix including #
    (delete-region (- (point) (length candidate) 1) (point))
    ;; Apply tag
    (org-supertag-tag-add-tag tag-name)))

;;;###autoload
(defun org-supertag-company-backend (command &optional arg &rest ignored)
  "Company backend for org-supertag completion.
COMMAND, ARG and IGNORED are standard arguments for company backends."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'org-supertag-company-backend))
    (prefix (and (eq major-mode 'org-mode)
                 (save-excursion
                   (when (looking-back org-supertag-company-prefix-regexp
                                     (line-beginning-position))
                     (match-string-no-properties 0)))))
    (candidates (org-supertag-company--candidates arg))
    (post-completion (org-supertag-company--post-completion arg))
    (annotation (when-let* ((tag (get-text-property 0 'tag arg))
                           (fields (plist-get tag :fields)))
                 (format " [%d fields]" (length fields))))
    (quickhelp-string (get-text-property 0 'help-echo arg))
    (sorted t)))

;;;###autoload
(defun org-supertag-setup-completion ()
  "Setup company completion for org-supertag."
  (when (and (eq major-mode 'org-mode)
             (featurep 'company))
    (add-to-list 'company-backends 'org-supertag-company-backend)))

(add-hook 'org-mode-hook #'org-supertag-setup-completion)

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
  (when-let ((preset (assoc tag-name org-supertag-preset-tags)))
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
              (current-fields (cdr (assoc tag-name org-supertag-preset-tags))
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
                           (read-string "New options (comma separated): "
                            (mapconcat #'identity 
                                     (plist-get field :options)
                                     ","))
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
                                             (plist-get field :description)))
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
                           tag-name org-supertag-preset-tags))))))))))))))))

(provide 'org-supertag-tag)

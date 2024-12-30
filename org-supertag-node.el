;;; org-supertag-node.el --- Node management for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides core node functionality including:
;; - Node structure definition
;; - Node operations (create, move, copy, delete)
;; - Node monitoring
;; - Node reference relationships
;; Implementation notes:
;; - Remember to implement org-supertag-node--handle-delete and org-supertag-node--handle-create
;; - Use org-supertag-node-cache
;; - Node updates require two layers: 1. Data layer 2. Entity layer

(require 'org)
(require 'org-element)
(require 'org-id)
(require 'org-supertag-db)
(require 'org-supertag-query)

(defconst org-supertag-node--id-property-regexp
  (rx ":ID:" (1+ " ") (group (1+ (not (any " \t\n")))))
  "Regular expression matching ID property.")

;;------------------------------------------------------------------------------
;; Database Operations
;;------------------------------------------------------------------------------    

(defun org-supertag-node-db-create (id props)
  "Create a new node in the database.
ID is the unique node identifier
PROPS are the node properties

Notes:
1. Validates required properties
2. Adds system properties
3. Creates node record
4. Triggers creation event"
  ;; Validate required properties
  (unless (plist-get props :title)
    (error "Node must have a title"))
  
  ;; Build complete node properties
  (let ((node-props (append
                    (list :id id
                          :type :node
                          :created-at (current-time))
                    props)))
    ;; Store node
    (org-supertag-db-add id node-props)
    ;; Trigger event
    (run-hook-with-args 'org-supertag-node-created-hook id node-props)
    ;; Return node properties
    node-props))

(defun org-supertag-node-db-update (id props)
  "Update an existing node in the database.
ID is the unique node identifier
PROPS are the properties to update"
  (when-let ((node (org-supertag-db-get id)))
    (let ((new-props nil))
      ;; 1. Preserve creation time
      (setq new-props (list :created-at (plist-get node :created-at)))
      ;; 2. Add modification time
      (setq new-props (plist-put new-props :modified-at (current-time)))
      ;; 3. Clean input properties (remove system props)
      (let ((clean-props (copy-sequence props)))
        (while (or (plist-member clean-props :created-at)
                  (plist-member clean-props :modified-at))
          (setq clean-props (org-plist-delete 
                           (org-plist-delete clean-props :created-at)
                           :modified-at)))
        ;; 4. Merge properties
        (setq new-props (append new-props clean-props)))
      ;; 5. Update database
      (org-supertag-db-add id new-props)
      ;; 6. Trigger event
      (org-supertag-emit 'node:updated id new-props)
      ;; 7. Return updated properties
      new-props)))

(defun org-supertag-node-db-exists-p (id)
  "Check if node exists in database.
ID is the unique node identifier

Returns:
- t   if node exists and type is :node
- nil if node doesn't exist or type isn't :node"
  (when-let ((node (org-supertag-db-get id)))
    (eq (plist-get node :type) :node)))

(defun org-supertag-node-db-get-tags (id)
  "Get all tags for a node.
ID is the unique node identifier

Returns:
List of tag IDs, or nil if node doesn't exist"
  (when (org-supertag-node-db-exists-p id)
    (mapcar #'cdr  ; Get tag IDs
            (org-supertag-db-get-links-by-type :node-tag
                                              :from id))))

(defun org-supertag-node-db-add-tag (node-id tag-id)
  "Add a tag to a node.
NODE-ID is the node identifier
TAG-ID is the tag identifier"
  (when (and (org-supertag-node-db-exists-p node-id)
             (org-supertag-db-exists-p tag-id))
    ;; Add relationship, note parameter order
    (org-supertag-db-link :node-tag 
                         node-id 
                         tag-id 
                         ;; Add relationship properties
                         `(:created-at ,(current-time)))
    ;; Trigger event
    (run-hook-with-args 'org-supertag-node-tag-added-hook
                        node-id tag-id)))

(defun org-supertag-node-db--get-candidates ()
  "Get list of all referenceable node candidates.

Returns:
Association list of ((title . id) ...) where:
- title is the node title
- id is the unique node identifier

Notes:
1. Traverses all nodes in database
2. Filters for type :node entities
3. Builds title and ID associations"
  (let (candidates)
    (maphash (lambda (id node)
               (when (eq (plist-get node :type) :node)
                 (push (cons (plist-get node :title) id)
                       candidates)))
             org-supertag-db--object)
    ;; Sort by title
    (sort candidates
          (lambda (a b)
            (string< (car a) (car b))))))

(defun org-supertag-node-get-tags (node-id)
  "Get list of all tag IDs associated with a node.
NODE-ID is the node identifier"
  (let ((result nil))
    (maphash
     (lambda (link-id props)
       (when (and (string-prefix-p ":node-tag:" link-id)
                  (equal (plist-get props :from) node-id))
         (push (plist-get props :to) result)))
     org-supertag-db--link)
    (nreverse result)))

(defun org-supertag-node-get-tag (node-id)
  "Get tag ID for NODE-ID.
Returns the first tag ID found for the node."
  (when-let* ((node-tags (org-supertag-node-get-tags node-id)))
    (car node-tags)))

;;------------------------------------------------------------------------------
;; Node Field Relations
;;------------------------------------------------------------------------------

(defun org-supertag-node-db-set-field (node-id field-id value)
  "设置节点的字段值.
NODE-ID 是节点ID
FIELD-ID 是字段ID
VALUE 是字段值

说明：
1. 创建/更新字段关系
2. 存储字段值
3. 触发字段更新事件"
    ;; 添加/更新字段关系
    (org-supertag-db-link
     :type :node-field
     :from node-id
     :to field-id
     :value value)
    ;; 清除缓存
    (org-supertag-db--cache-remove 'query 
                                  (format "node-fields:%s" node-id))
    ;; 触发事件
    (run-hook-with-args 'org-supertag-node-field-updated-hook
                        node-id field-id value))

 (defun org-supertag-node-db-get-fields (node-id)
  "获取节点的所有字段值.
NODE-ID 是节点ID

返回值：
字段值的列表，每个元素是 (field-id . value)"
  (or (org-supertag-db--cache-get 'query 
                                 (format "node-fields:%s" node-id))
      (when (org-supertag-node-db-exists-p node-id)
        (let ((fields (org-supertag-db-get-links-by-type 
                      :node-field :from node-id)))
          (org-supertag-db--cache-set 'query 
                                     (format "node-fields:%s" node-id)
                                     fields)
          fields))))                       

;;------------------------------------------------------------------------------
;; Operation Functions
;;------------------------------------------------------------------------------    

(defun org-supertag-node-sync-at-point ()
  "Synchronize node data at current point.
Returns:
- Node ID if successful
- nil if no valid node found"
  (let ((existing-id (org-id-get)))
    (if existing-id
        ;; Update existing node
        (when (org-supertag-node-db-exists-p existing-id)
          (org-supertag-db-add-node-at-point)
          existing-id)
      ;; Create new node
      (let ((new-id (org-id-get-create)))
        (org-supertag-db-add-node-at-point)
        new-id))))

;; ID Location Tracking
(defvar org-supertag-node--id-locations-timer nil
  "Timer for periodic ID location updates.")

(defvar org-supertag-node--modified-files nil
  "List of files with potentially modified ID locations.")

(defun org-supertag-node--track-file-modification ()
  "Track current file for ID location updates."
  (when-let ((file (buffer-file-name)))
    (add-to-list 'org-supertag-node--modified-files file)))

(defun org-supertag-node--setup-id-tracking ()
  "Setup enhanced ID location tracking."
  ;; Initialize org-id system
  (require 'org-id)
  
  ;; Load existing locations
  (condition-case err
      (progn
        (unless org-id-locations
          (org-id-locations-load))
        ;; Convert to hash table if needed
        (unless (hash-table-p org-id-locations)
          (setq org-id-locations (org-id-alist-to-hash (or org-id-locations nil)))))
    (error
     (message "Failed to load org-id-locations: %s" (error-message-string err))
     (setq org-id-locations (make-hash-table :test 'equal))
     (setq org-id-files nil)))
  
  ;; Track file modifications
  (add-hook 'before-save-hook #'org-supertag-node--track-file-modification nil t)
  
  ;; Setup periodic updates
  (unless org-supertag-node--id-locations-timer
    (setq org-supertag-node--id-locations-timer
          (run-with-idle-timer 300 t #'org-supertag-node--update-id-locations)))
  
  ;; Update on save
  (add-hook 'after-save-hook #'org-supertag-node--check-and-update-ids nil t))

(defun org-supertag-node--cleanup-id-tracking ()
  "Cleanup ID tracking resources."
  (when org-supertag-node--id-locations-timer
    (cancel-timer org-supertag-node--id-locations-timer)
    (setq org-supertag-node--id-locations-timer nil))
  (setq org-supertag-node--modified-files nil)
  (remove-hook 'before-save-hook #'org-supertag-node--track-file-modification t)
  (remove-hook 'after-save-hook #'org-supertag-node--check-and-update-ids t))

(defun org-supertag-node--check-and-update-ids ()
  "Check and update ID locations in current file."
  (when (and org-id-track-globally
             (derived-mode-p 'org-mode))
    (let ((file (buffer-file-name))
          (changed nil)
          (file-ids nil))
      ;; Ensure org-id-locations exists
      (unless org-id-locations
        (setq org-id-locations (make-hash-table :test 'equal)))
      ;; Scan current file for IDs
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-supertag-node--id-property-regexp nil t)
         (let* ((id (match-string-no-properties 1))
                (stored-file (gethash id org-id-locations)))
           ;; Check if ID location has changed
           (when (or (null stored-file)
                    (not (equal stored-file file)))
             (setq changed t)
             (puthash id file org-id-locations)))))
      ;; If changes found, save locations
      (when changed
        (let ((alist (org-id-hash-to-alist org-id-locations)))
          (setq org-id-files (mapcar 'car alist))
          (with-temp-file org-id-locations-file
            (let ((print-length nil)
                  (print-level nil))
              (print alist (current-buffer)))))))))

(defun org-supertag-node--update-id-locations ()
  "Update ID locations for modified files."
  (when (and org-id-track-globally org-supertag-node--modified-files)
    (let ((files (copy-sequence org-supertag-node--modified-files)))
      (setq org-supertag-node--modified-files nil)
      (org-id-update-id-locations files))))

(defun org-supertag-node--after-refile-update-ids ()
  "Update ID locations after refile operations."
  (when org-id-track-globally
    (let* ((current-file (buffer-file-name))
           (target-file (buffer-file-name (marker-buffer org-refile-target-location))))
      (when (and current-file target-file
                 (not (equal current-file target-file)))
        (org-id-update-id-locations (list current-file target-file))))))

(defun org-supertag-node-get-props ()
  "Get properties of current node."
  (org-supertag-db--parse-node-at-point))

(defun org-supertag-node-sync-display ()
  "Synchronize node display state.

Process:
1. Ensure node has ID property
2. Update fields in properties drawer
3. Update tag display
4. Update todo state display

Notes:
- Keeps display state consistent with database
- Does not trigger database updates
- Only updates display-related properties"
  (when-let ((node-id (org-id-get)))
    (when-let ((node (org-supertag-db-get node-id)))
      ;; Update properties drawer
      (org-set-property "ID" node-id)
      ;; Update other display states
      (let ((tags (plist-get node :tags))
            (todo (plist-get node :todo)))
        (when tags
          (org-set-tags tags))
        (when todo
          (org-todo todo))))))

(defun org-supertag-node-move (node-id target-file &optional target-level)
  "Move node to target file.
NODE-ID is the node identifier
TARGET-FILE is the target file path
TARGET-LEVEL is the target heading level

Returns:
- t if move successful
- nil if move failed"
  (when-let* ((content (org-supertag-get-node-content node-id))
              (source-file (plist-get (org-supertag-db-get node-id) :file-path)))
    ;; 1. Delete node from source
    (when (org-supertag-delete-node-content node-id)
      ;; 2. Insert to target and update database
      (with-current-buffer (find-file-noselect target-file)
        (save-excursion
          (let ((adjusted-content 
                 (org-supertag-adjust-node-level content target-level)))
            (insert adjusted-content "\n")
            (forward-line -1)
            ;; 3. Update node database entry
            (org-supertag-update-node-db node-id target-file)
            ;; 4. Update ID locations if cross-file move
            (when (and org-id-track-globally
                      (not (equal source-file target-file)))
              (org-id-update-id-locations (list source-file target-file)))
            ;; 5. Save both files
            (save-buffer)
            (when (and source-file
                      (not (equal source-file target-file)))
              (with-current-buffer (find-file-noselect source-file)
                (save-buffer)))
            t))))))

;;------------------------------------------------------------------------------
;; Node Commands 
;;------------------------------------------------------------------------------    

(defun org-supertag-node-create ()
  "Create a new supertag node at current position.

Use cases:
1. Convert regular org heading to supertag node
2. Ensure heading has no associated node ID

Prerequisites:
1. Cursor must be on a heading
2. Heading must not already have a node ID"
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Cursor must be on a heading"))
  (when (org-id-get)
    (user-error "Heading already has a node"))
  (org-supertag-node-sync-at-point))

(defun org-supertag--create-node (node-id)
  "Create a new supertag node with given ID.
NODE-ID: The node identifier"
  (let ((node-data (list :id node-id
                        :type :node
                        :title (org-get-heading t t t t)
                        :file-path (buffer-file-name)
                        :pos (point)
                        :olp (org-get-outline-path)
                        :level (org-outline-level)
                        :created-at (current-time))))
    (org-supertag-db-add node-id node-data)
    node-id))

(defun org-supertag-node-get-props-at-point ()
  "Get properties of node at current point.
Returns:
- Property list if valid node
- nil if not a valid node"
  (when (org-at-heading-p)
    (let* ((element (org-element-at-point))
           (id (org-id-get))
           (title (org-get-heading t t t t))
           (level (org-current-level))
           (olp (org-get-outline-path)))
      (when (and id title)
        (list :type :node
              :id id
              :title title
              :file-path (buffer-file-name)
              :pos (point)
              :level level
              :olp olp)))))

(defun org-supertag-node-update ()
  "Update node at current position.

Use cases:
1. Manual update after node content changes
2. Force node data synchronization

Prerequisites:
1. Cursor must be on a heading
2. Heading must have an associated node ID

Returns:
- Node ID on success
- Throws error on failure"
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Cursor must be on a heading"))
  (let ((id (org-id-get)))
    (unless id
      (user-error "Heading has no associated node"))
    ;; Get node properties
    (let ((props (org-supertag-node-get-props-at-point)))
      (unless props
        (error "Unable to get node properties"))
      ;; Update database
      (condition-case err
          (progn
            (org-supertag-node-db-update id props)
            (message "Node updated successfully: %s" id)
            id)
        (error
         (message "Node update failed: %s" (error-message-string err))
         (signal (car err) (cdr err)))))))

(defun org-supertag-node-db-update (id props)
  "Update or create a node in the database.
ID is the node identifier
PROPS are the node properties

Returns:
- Updated properties on success
- Throws error on failure"
  (let* ((existing-node (org-supertag-db-get id))
         (is-new (not existing-node))
         (new-props (org-supertag-db--normalize-props
                    (append
                     ;; Basic properties
                     (list :type :node
                           :id id)
                     ;; Timestamps
                     (if is-new
                         (list :created-at (current-time)
                               :modified-at (current-time))
                       (list :created-at (plist-get existing-node :created-at)
                             :modified-at (current-time)))
                     ;; User provided properties
                     props))))
    
    ;; Validate required properties
    (unless (plist-get new-props :title)
      (error "Missing required property: title"))
    (unless (plist-get new-props :file-path)
      (error "Missing required property: file-path"))
    ;; Update database
    (org-supertag-db-add id new-props)
    ;; Trigger appropriate hook
    (run-hook-with-args 
     (if is-new
         'org-supertag-node-created-hook
       'org-supertag-node-updated-hook)
     id new-props)
    ;; Log operation
    (message "%s node: %s" 
             (if is-new "Created new" "Updated")
             id)
    ;; Return updated properties
    new-props))


(defun org-supertag-node-remove-tag (node-id tag-id)
  "Remove tag association from a node.
NODE-ID is the node identifier
TAG-ID is the tag identifier"
  (let ((link-id (format ":node-tag:%s->%s" node-id tag-id)))
    ;; Remove association from database
    (remhash link-id org-supertag-db--link)
    ;; Clear caches
    (org-supertag-db--cache-remove 'query (format "node-tags:%s" node-id))
    (org-supertag-db--cache-remove 'query (format "tag-nodes:%s" tag-id))
    ;; Schedule save
    (org-supertag-db-save)))


;;------------------------------------------------------------------------------
;; Node Relations 
;;------------------------------------------------------------------------------    

(defun org-supertag-node--in-node-p ()
  "Check if current position is within a valid org-supertag node.
Valid node requirements:
1. Current position can reach a heading
2. Heading has an ID property
3. ID exists in database

Returns:
- (id . title) if in valid node
- nil if not in valid node, with appropriate message"
  (condition-case nil
      (save-excursion
        (org-back-to-heading t)
        (let ((id (org-entry-get nil "ID"))
              (title (org-get-heading t t t t)))
          (cond
           ;; No ID
           ((null id)
            (message "Current node not created, use org-supertag-node-create command")
            nil)
           ;; ID not in database
           ((not (org-supertag-node-db-exists-p id))
            (message "Node ID not registered in database, use org-supertag-node-create command")
            nil)
           ;; Valid node
           (t (cons id title)))))
    ;; Not under any heading
    (error 
     (message "Current position not within any node")
     nil)))

(defun org-supertag-node-db-add-reference (from-id to-id)
  "Add reference relationship between nodes.
FROM-ID is the source node
TO-ID is the target node

Notes:
1. Checks node existence
2. Creates reference association
3. Triggers event"
  (when (and (org-supertag-node-db-exists-p from-id)
             (org-supertag-node-db-exists-p to-id))
    ;; Add reference relationship
    (org-supertag-db-link
     :type :node-ref
     :from from-id
     :to to-id)
    ;; Clear cache
    (org-supertag-db--cache-remove 'query 
                                  (format "node-refs:%s" from-id))
    ;; Trigger event
    (run-hook-with-args 'org-supertag-node-reference-added-hook
                       from-id to-id)))

;; 3. Relationship queries
(defun org-supertag-node-db-get-reference (node-id &optional direction)
  "Get node reference relationships.
NODE-ID is the node identifier
DIRECTION specifies reference direction:
  - 'to   get nodes referenced by this node
  - 'from get nodes referencing this node
  - nil   get all related references

Returns:
List of node IDs"
  (or (org-supertag-db--cache-get 'query 
                                 (format "node-refs:%s:%s" node-id direction))
      (when-let ((node (org-supertag-db-get node-id)))
        (let ((refs (pcase direction
                     ('to (plist-get node :refs-to))
                     ('from (plist-get node :refs-from))
                     (_ (append (plist-get node :refs-to)
                              (plist-get node :refs-from))))))
          (org-supertag-db--cache-set 'query 
                                     (format "node-refs:%s:%s" node-id direction)
                                     refs)
          refs))))



;; 4. Relationship cleanup
(defun org-supertag-node-db-remove-reference (from-id to-id)
  "Remove reference relationship between nodes.
FROM-ID is the source node
TO-ID is the target node"
  (when-let* ((from-node (org-supertag-db-get from-id))
              (refs-to (plist-get from-node :refs-to)))
    ;; Use database reference handling mechanism
    (org-supertag-db--handle-refs-updated 
     from-id 
     (delete to-id refs-to))))

;;------------------------------------------------------------------------------
;; Node Reference Functions
;;------------------------------------------------------------------------------  


(defun org-supertag-node--insert-reference (node-id)
  "Insert node reference at current position.
NODE-ID is the referenced node's identifier"
  (let* ((target-node (org-supertag-db-get node-id))
         (target-title (plist-get target-node :title))
         (link-text (format "[[id:%s][%s]]" node-id target-title)))
    ;; Insert link at current position
    (insert link-text)))


(defun org-supertag-node-add-reference ()
  "Add node reference.
Insert reference to selected node at current position and update relationships."
  (interactive)
  (unless (org-supertag-node--in-node-p)
    (user-error "Must be within a node"))
  (when-let* ((candidates (org-supertag-node-db--get-candidates))
              (selected (completing-read
                        "Reference node: "
                        (mapcar #'car candidates)
                        nil t))
              (target-id (cdr (assoc selected candidates))))
    ;; 1. Insert reference
    (org-supertag-node--insert-reference target-id)
    ;; 2. Update reference relationships
    (org-supertag-db--parse-node-all-ref)
    (message "Added reference to node '%s'" selected)))



(defun org-supertag-node-remove-reference (node-id)
  "Remove reference to specified node.
NODE-ID is the referenced node identifier.
Uses org-element to parse and locate links for accurate removal."
  (interactive 
   (let* ((refs (org-supertag-node--collect-reference-id-title))
          (selected (completing-read 
                    "Remove reference: "
                    (mapcar #'car refs)
                    nil t)))
     (list (cdr (assoc selected refs)))))
  
  (let ((start (org-entry-beginning-position))
        (end (org-entry-end-position))
        (removed 0))
    (save-excursion
      ;; 1. Use org-element to parse and remove links
      (goto-char start)
      (while (< (point) end)
        (let ((element (org-element-context)))
          (when (and (eq (org-element-type element) 'link)
                    (equal (org-element-property :type element) "id")
                    (equal (org-element-property :path element) node-id))
            (delete-region (org-element-property :begin element)
                          (org-element-property :end element))
            (cl-incf removed)))
        (forward-char)))

    (org-supertag-db--parse-node-all-ref)
    
    (message "Removed %d references" removed)))

(defun org-supertag-node--collect-reference-id-title ()
  "Collect all reference information in current node.
Returns: ((title . id) ...)"
  (let ((refs nil)
        (start (org-entry-beginning-position))
        (end (org-entry-end-position)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward org-link-any-re end t)
        (let* ((link (org-element-context))
               (type (org-element-property :type link))
               (path (org-element-property :path link)))
          (when (and (equal type "id")
                    (org-uuidgen-p path))
            (when-let* ((node (org-supertag-db-get path))
                       (title (plist-get node :title)))
              (push (cons title path) refs))))))
    (nreverse refs)))

;;------------------------------------------------------------------------------
;; Event Hooks
;;------------------------------------------------------------------------------    

(defvar org-supertag-node-created-hook nil
  "Hook run after node creation.
Arguments: (id props)")

(defvar org-supertag-node-updated-hook nil
  "Hook run after node update.
Arguments: (id props)")

(defvar org-supertag-node-deleted-hook nil
  "Hook run after node deletion.
Arguments: (id)")

(defvar org-supertag-node-tag-added-hook nil
  "Hook run after tag is added to node.
Arguments: (node-id tag-id)")

(defvar org-supertag-node-field-updated-hook nil
  "Hook run after node field update.
Arguments: (node-id field-id value)")

(defvar org-supertag-node-reference-added-hook nil
  "Hook run after node reference is added.
Arguments: (from-id to-id)")

(defvar org-supertag-node-reference-removed-hook nil
  "Hook run after node reference is removed.
Arguments: (from-id to-id)")



(provide 'org-supertag-node)
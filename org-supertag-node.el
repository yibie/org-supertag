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
ID is the node identifier
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
  "Set field value for a node.
NODE-ID is the node identifier
FIELD-ID is the field identifier
VALUE is the field value

Notes:
1. Creates/updates field relationship
2. Stores field value
3. Triggers field update event"
    ;; Add/update field relationship
    (org-supertag-db-link
     :type :node-field
     :from node-id
     :to field-id
     :value value)
    ;; Clear cache
    (org-supertag-db--cache-remove 'query 
                                  (format "node-fields:%s" node-id))
    ;; Trigger event
    (run-hook-with-args 'org-supertag-node-field-updated-hook
                        node-id field-id value))

(defun org-supertag-node-db-get-fields (node-id)
  "Get all field values for a node.
NODE-ID is the node identifier

Returns:
List of field values, each element is (field-id . value)"
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
  "Synchronize current node with database."
  (when (org-at-heading-p)
    (let* ((node-id (org-id-get-create))
           (old-node (org-supertag-db-get node-id))
           (title (org-get-heading t t t t))
           (file-path (buffer-file-name))
           (pos (point))
           (level (org-outline-level))
           (olp (org-get-outline-path))
           ;; 获取节点内容
           (content (save-excursion
                     (let* ((begin (progn 
                                   (org-end-of-meta-data t)
                                   (point)))
                            (end (org-entry-end-position)))
                       (buffer-substring-no-properties begin end)))))
      
      ;; 更新所有属性，包括内容
      (let ((props (list :type :node
                        :id node-id
                        :title title
                        :file-path file-path
                        :pos pos
                        :level level
                        :olp olp
                        :content content  ;; 添加内容
                        :ref-to (when old-node 
                                (plist-get old-node :ref-to))
                        :ref-from (when old-node 
                                  (plist-get old-node :ref-from))
                        :ref-count (when old-node
                                   (plist-get old-node :ref-count))
                        :created-at (or (and old-node 
                                          (plist-get old-node :created-at))
                                     (current-time)))))
        
        ;; Update database
        (org-supertag-db-add node-id props)))))

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


(defun org-supertag-node-move (node-id target-file &optional target-level target-point)
  "Move node to target file.
NODE-ID is the node identifier
TARGET-FILE is the target file path
TARGET-LEVEL is the target heading level
TARGET-POINT is buffer position to insert (nil means end of file)

Returns:
- t if move successful
- nil if move failed"
  (when-let* ((content (org-supertag-get-node-content node-id)))
    ;; 1. Delete node from source
    (when (org-supertag-delete-node-content node-id)
      ;; 2. Insert to target and update database
      (with-current-buffer (find-file-noselect target-file)
        (save-excursion
          ;; Move to target position
          (if target-point
              (goto-char target-point)
            (goto-char (point-max)))
          
          ;; Ensure we're at the beginning of a line
          (beginning-of-line)
          
          ;; Insert with proper level adjustment
          (let ((adjusted-content 
                 (if (and target-level (> target-level 0))
                     (condition-case err
                         (org-supertag-adjust-node-level content target-level)
                       (error
                        (message "Level adjustment failed: %s" (error-message-string err))
                        content))
                   content)))
            ;; Ensure proper spacing before insertion
            (unless (or (bobp) (looking-back "\n\n" 2))
              (insert "\n"))
            
            ;; Insert content
            (let ((insert-point (point)))
              (insert adjusted-content)
              
              ;; Ensure proper spacing after insertion
              (unless (looking-at "\n")
                (insert "\n"))
              
              ;; Move back to inserted heading and sync node
              (goto-char insert-point)
              (when (re-search-forward (format "^[ \t]*:ID:[ \t]+%s[ \t]*$" node-id) nil t)
                (org-back-to-heading t)
                (org-supertag-node-sync-at-point))
              
              ;; Save buffer
              (save-buffer)
              t)))))))

(defun org-supertag-adjust-node-level (content target-level)
  "Adjust heading level of node content.
CONTENT is the node content string
TARGET-LEVEL is the desired heading level (integer)

Returns adjusted content string with proper heading levels."
  (with-temp-buffer
    (org-mode)  ; 启用 org-mode 以确保正确处理
    (insert content)
    (message "Original content:\n%s" content)  ; 调试信息
    (goto-char (point-min))
    
    (when (and target-level (> target-level 0))
      (message "Adjusting to level %d" target-level)  ; 调试信息
      ;; 获取当前节点的层级
      (when (re-search-forward "^\\(\\*+\\)\\( .*\\)" nil t)  ; 修改正则表达式以包含标题文本
        (let* ((current-level (length (match-string 1)))
               (level-diff (- target-level current-level)))
          (message "Current level: %d, Level diff: %d" 
                  current-level level-diff)  ; 调试信息
          
          (unless (zerop level-diff)
            (goto-char (point-min))
            ;; 调整所有标题的层级
            (while (re-search-forward "^\\(\\*+\\)\\( .*\\)" nil t)  ; 修改正则表达式以包含标题文本
              (let* ((stars (match-string 1))
                     (text (match-string 2))  ; 保存标题文本
                     (current-stars-len (length stars))
                     (new-level (+ current-stars-len level-diff))
                     (new-stars (make-string (max 1 new-level) ?*)))
                (message "Adjusting heading from level %d to %d" 
                        current-stars-len new-level)  ; 调试信息
                (replace-match (concat new-stars text))))))))  ; 保持标题文本不变
    
    (let ((result (buffer-string)))
      (message "Adjusted content:\n%s" result)  ; 调试信息
      result)))

(defun org-supertag-move-node-and-link (node-id target-file &optional target-level)
  "Move node to target file and leave a reference link at original location.
NODE-ID is the node identifier
TARGET-FILE is the target file path
TARGET-LEVEL is the target heading level

Returns:
- t if move successful
- nil if move failed"
  (interactive
   (let* ((node-id (org-id-get)))
     (unless node-id
       (user-error "Current position is not in a node"))
     (unless (org-supertag-db-get node-id)
       (user-error "Current node is not registered in database"))
     (list node-id 
           (read-file-name "Move to file: "))))
  
  (when-let* ((node (org-supertag-db-get node-id))
              (title (plist-get node :title))
              (source-file (plist-get node :file-path))
              (source-pos (point)))
    
    ;; 1. Get target position and level adjustment
    (let* ((insert-pos (org-supertag-query--get-insert-position target-file))
           (target-pos (car insert-pos))
           (level-adj (or target-level (cdr insert-pos))))
      
      ;; 2. Store the reference link content
      (let ((reference-content (format "[[id:%s][%s]]\n" node-id title)))
        
        ;; 3. Move the node
        (with-current-buffer (find-file-noselect target-file)
          (save-excursion
            (goto-char target-pos)
            (when (org-supertag-node-move node-id target-file level-adj)
              ;; 4. Insert reference at original position
              (with-current-buffer (find-file-noselect source-file)
                (save-excursion
                  (goto-char source-pos)
                  (insert reference-content)
                  (save-buffer)))
              
              ;; 5. Save target file
              (save-buffer)
              (message "Node moved and reference link created")
              t)))))))

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

(defun org-supertag-node-delete ()
  "Delete current node and all its associated data.
This includes:
1. Delete the org headline
2. Remove associated tags
3. Remove associated fields (properties)
4. Clean up database entries
5. Remove all references to this node"
  (interactive)
  (let ((node-id (org-id-get)))
    (unless node-id
      (user-error "Current position is not in a node"))
    (unless (org-supertag-db-get node-id)
      (user-error "Current node is not registered in database"))
    
    (when (yes-or-no-p "Really delete this node and all its data? ")
      ;; 1. Get node info before deletion
      (let* ((node (org-supertag-db-get node-id))
             (tags (org-supertag-node-get-tags node-id))
             (ref-from (plist-get node :ref-from)))
        
        ;; 2. Remove all tags
        (dolist (tag-id tags)
          (org-supertag-tag--remove tag-id node-id))
        
        ;; 3. Remove all references to this node
        (dolist (ref-node-id ref-from)
          (when-let* ((ref-file (plist-get (org-supertag-db-get ref-node-id) :file-path)))
            (with-current-buffer (find-file-noselect ref-file)
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward (format "\\[\\[id:%s\\]\\[[^]]*\\]\\]" node-id) nil t)
                  (delete-region (match-beginning 0) (match-end 0)))
                (save-buffer)))))
        
        ;; 4. Delete the headline content including properties
        (org-back-to-heading t)
        (let* ((element (org-element-at-point))
               (begin (org-element-property :begin element))
               (end (org-element-property :end element)))
          ;; Delete the entire heading including properties
          (delete-region begin end)
          (when (looking-at "\n") (delete-char 1)))
        
        ;; 5. Remove from database
        (remhash node-id org-supertag-db--object)
        
        ;; 6. Clear database caches
        (org-supertag-db--cache-remove 'entity node-id)
        (org-supertag-db--cache-remove 'query (format "type:%s" :node))
        (org-supertag-db--cache-remove 'query (format "node-tags:%s" node-id))
        (org-supertag-db--cache-remove 'query (format "node-fields:%s" node-id))
        (org-supertag-db--cache-remove 'query (format "node-refs:%s" node-id))
        
        ;; 7. Save changes
        (save-buffer)
        (org-supertag-db-save)
        
        ;; 8. Run hooks
        (run-hook-with-args 'org-supertag-node-deleted-hook node-id)
        
        (message "Node deleted: %s" node-id)))))

(defun org-supertag-node--ensure-id-system ()
  "Ensure org-id system is properly initialized."
  (require 'org-id)
  (unless (and (boundp 'org-id-locations)
               (hash-table-p org-id-locations))
    (setq org-id-locations (make-hash-table :test 'equal))
    (when (file-exists-p org-id-locations-file)
      (org-id-locations-load))))

(defun org-supertag--create-node (node-id)
  "Create a new node with NODE-ID."
  (org-supertag-node--ensure-id-system)
  (let ((props (org-supertag-db--parse-node-at-point)))
    (org-supertag-db-add node-id props)))

(defun org-supertag-node-get-props-at-point ()
  "Get node properties at current point.
Returns property list if successful, nil if failed."
  (condition-case err
      (progn
        (unless (org-at-heading-p)
          (error "Not at a heading"))
        ;; 使用 org-supertag-db--parse-node-at-point 获取完整属性
        (org-supertag-db--parse-node-at-point))
    (error
     (message "Failed to get node properties: %s" (error-message-string err))
     nil)))

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
  "Update node in database.
ID is the node identifier
PROPS is the property list to update with"
  (unless (and id props)
    (error "Invalid input: id=%s props=%s" id props))
  ;; 直接使用 org-supertag-db-add 更新数据库
  (org-supertag-db-add id props))


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
  "Insert node reference at current position and update relationships.
NODE-ID is the referenced node's identifier"
  (let* ((target-node (org-supertag-db-get node-id))
         (target-title (plist-get target-node :title))
         (link-text (format "[[id:%s][%s]]" node-id target-title)))
    ;; 1. 插入链接
    (insert link-text)
    ;; 2. 更新引用关系
    (let* ((current-node-id (org-id-get))
           (current-node (org-supertag-db-get current-node-id)))
      
      ;; 2.1 更新当前节点的 ref-to
      (let* ((current-refs (or (plist-get current-node :ref-to) nil))
        (org-supertag-db-add 
         current-node-id
         (append
          (list :type :node)
          (plist-put 
           (plist-put current-node :ref-to current-refs)
           :ref-count (length current-refs)))))
      
      ;; 2.2 更新目标节点的 ref-from
      (let* ((target-refs (or (plist-get target-node :ref-from) nil))
        (org-supertag-db-add 
         node-id
         (append
          (list :type :node)
          (plist-put 
           (plist-put target-node :ref-from target-refs)
           :ref-count (length target-refs))))))))))


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
NODE-ID is the referenced node identifier."
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
    ;; 1. delete link
    (save-excursion
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
    ;; 2. update reference
    (when (> removed 0)
      (let* ((current-node-id (org-id-get))
             (current-node (org-supertag-db-get current-node-id))
             (target-node (org-supertag-db-get node-id)))
        ;; 2.1 update current node ref-to
        (let* ((current-refs (plist-get current-node :ref-to))
               (new-refs (delete node-id current-refs)))
          (org-supertag-db-add 
           current-node-id
           (append
            (list :type :node)
            (plist-put 
             (plist-put current-node :ref-to new-refs)
             :ref-count (length new-refs)))))        
        ;; 2.2 update target node ref-from
        (let* ((target-refs (plist-get target-node :ref-from))
               (new-target-refs (delete current-node-id target-refs)))
          (org-supertag-db-add 
           node-id
           (append
            (list :type :node)
            (plist-put 
             (plist-put target-node :ref-from new-target-refs)
             :ref-count (length new-target-refs)))))))
    
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


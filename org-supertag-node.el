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
  (when-let* ((node (org-supertag-db-get id)))
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
  (when-let* ((node (org-supertag-db-get id)))
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

(defun org-supertag-node-db-remove-tag (node-id tag-id)
  "Remove a tag from a node.
NODE-ID is the node identifier
TAG-ID is the tag identifier"
  (let ((link-id (format ":node-tag:%s->%s" node-id tag-id))
        (removed nil))
    ;; Remove association from database
    (when (ht-contains-p org-supertag-db--link link-id)
      (remhash link-id org-supertag-db--link)
      (setq removed t))
    ;; Clear caches if something was removed
    (when removed
      (org-supertag-db--cache-remove 'query (format "node-tags:%s" node-id))
      (org-supertag-db--cache-remove 'query (format "tag-nodes:%s" tag-id))
      ;; Schedule save
      (org-supertag-db-save))
    removed))

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
  "Get list of all valid tag IDs associated with a node.
This function now filters out tags that don't have a corresponding
tag definition in the database, preventing dangling links from being returned.
NODE-ID is the node identifier"
  (let ((result nil))
    (maphash
     (lambda (link-id props)
       (when (and (string-prefix-p ":node-tag:" link-id)
                  (equal (plist-get props :from) node-id))
         (let ((tag-id (plist-get props :to)))
           ;; Verify that the tag actually exists before adding it.
           (when (org-supertag-tag-get tag-id)
             (push tag-id result)))))
     org-supertag-db--link)
    (nreverse result)))

(defun org-supertag-node-get-tag (node-id)
  "Get tag ID for NODE-ID.
Returns the first tag ID found for the node."
  (when-let* ((node-tags (org-supertag-node-get-tags node-id)))
    (car node-tags)))

(defun org-supertag-node-get-parent-tags (node-id)
  "Get the parent tags of the node.
NODE-ID: The ID of the current node"
  (save-excursion
    (message "DEBUG: org-supertag-node-get-parent-tags called with node-id: %s" node-id)
    (when-let* ((pos (org-id-find node-id t)))
      (goto-char pos)
      (message "DEBUG: Child node (id: %s) found at pos %s" node-id pos)
      (if (org-up-heading-safe)
          (let* ((parent-id (org-id-get))
                 (parent-tags (when parent-id (org-supertag-node-get-tags parent-id))))
            (message "DEBUG: Parent ID found: %s" parent-id)
            (message "DEBUG: Parent tags from DB: %S" parent-tags)
            parent-tags)
        ;; If no parent heading is found, log and return nil explicitly to avoid
        ;; propagating a string value that breaks callers expecting a list.
        (progn
          (message "DEBUG: org-up-heading-safe returned nil. No parent found.")
          nil)))))


(defun org-supertag-node--ensure-sync ()
  "Ensure current node is properly synced with database.
If point is in a heading or its content area, create/sync the node.
Returns node ID if successful, nil otherwise."
  (save-excursion
    (condition-case nil
        (progn
          ;; Try to move to the parent heading if we're in content
          (unless (org-at-heading-p)
            (org-back-to-heading t))
          ;; Now we're at a heading, check for existing node
          (let ((node-id (org-id-get)))
            (if node-id
                ;; If node exists, ensure it's synced
                (org-supertag-node-sync-at-point)
              ;; If no node exists, create one
              (org-supertag-node-sync-at-point))))
      ;; If we can't find a parent heading
      (error nil))))

(defun org-supertag-node-get-all ()
  "Get a list of all node IDs from the database."
  (let ((nodes '()))
    (maphash (lambda (id props)
               (when (eq (plist-get props :type) :node)
                 (push id nodes)))
             org-supertag-db--object)
    (nreverse nodes)))

;;------------------------------------------------------------------------------
;; Hashing Functions
;;------------------------------------------------------------------------------

(defun org-supertag-node-content-hash (node-id)
  "Calculate a content-based MD5 hash for a given node-id.
This hash reflects changes in the node's title, content, tags, and field values.
It excludes metadata like creation/modification times, file path, and position."
  (condition-case err
      (let* ((node-props (org-supertag-db-get node-id))
             (hash-string-parts '()))

        (unless node-props
          (message "Warning: Node %s not found for content hash calculation." node-id)
          (return-from org-supertag-node-content-hash nil))

        ;; 1. Include Title
        (when-let ((title (plist-get node-props :title)))
          (push (format "T:%s" title) hash-string-parts))

        ;; 2. Include Content
        (when-let ((content (plist-get node-props :content)))
          (push (format "C:%s" content) hash-string-parts))

        ;; 3. Include Tags (sorted for consistent hash)
        (when-let ((tags (plist-get node-props :tags)))
          (push (format "G:%s" (mapconcat #'identity (sort tags #'string<) ",")) hash-string-parts))

        ;; 4. Include Field Values (sorted by field-name for consistent hash)
        ;;    We need to iterate through all node-field links for this node.
        (let ((node-fields '()))
          (cl-loop for link in (org-supertag-db-find-links :node-field node-id nil)
                   do (push (cons (plist-get link :to) (plist-get link :value)) node-fields))
          ;; Sort fields by name before concatenating
          (setq node-fields (sort node-fields (lambda (a b) (string< (car a) (car b)))))
          (dolist (field-pair node-fields)
            (push (format "F:%s=%s" (car field-pair) (cdr field-pair)) hash-string-parts)))

        ;; Combine all parts into a single string and calculate MD5 hash
        (md5 (mapconcat #'identity (sort hash-string-parts #'string<) "|")))
    (error
     (message "Error calculating content hash for node %s: %s" node-id (error-message-string err))
     nil)))

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
;; Operation Functions (Internal)
;;------------------------------------------------------------------------------    

(defun org-supertag-node-sync-at-point ()
  "Synchronize current node with database, including full tag registration."
  (when (org-at-heading-p)
    (let* ((node-id (org-id-get-create))
           (old-node (org-supertag-db-get node-id))
           (node-data (org-supertag-db--parse-node-at-point))
           (refs-from (when old-node (plist-get old-node :ref-from)))
           (props (append node-data
                         (list :ref-from (or refs-from nil)
                               :created-at (or (and old-node (plist-get old-node :created-at))
                                               (current-time)))))
           (headline-tags (plist-get props :tags)))
      
      (message "Syncing node %s. Tags from file: %S" node-id headline-tags)
      
      ;; 1. Update/create the node object itself.
      (org-supertag-db-add node-id props)
      
      ;; 2. Process and register all tags found in the headline.
      (if (null headline-tags)
          (message "No headline tags to process for node %s." node-id)
        (dolist (tag-name headline-tags)
          (let ((sanitized-tag (org-supertag-sanitize-tag-name tag-name)))
            (message "Processing tag: %s (sanitized: %s)" tag-name sanitized-tag)
            ;; Ensure tag object exists
            (unless (org-supertag-tag-get sanitized-tag)
              (org-supertag-tag--create sanitized-tag))
            ;; Ensure link exists
            (org-supertag-node-db-add-tag node-id sanitized-tag))))
      
      ;; Return node ID
      node-id)))

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
  (when-let* ((node-id (org-id-get)))
    (when-let* ((node (org-supertag-db-get node-id)))
      ;; Update properties drawer
      (org-set-property "ID" node-id)
      ;; Update other display states
      (let ((tags (plist-get node :tags))
            (todo (plist-get node :todo)))
        ;; (when tags
        ;;   (org-set-tags tags))
        (when todo
          (org-todo todo))))))

(defun org-supertag-get-node-content (node-id)
  "Get complete node content including subtree.
NODE-ID is the node identifier.
Returns the complete node content as string, or nil if not found."
  (message "[Get Content] Starting for node: %s" node-id)
  (when-let* ((node (org-supertag-db-get node-id))
              (file (plist-get node :file-path)))
    (message "[Get Content] Found node in database, file: %s" file)
    (if (file-exists-p file)
        (progn
          (message "[Get Content] File exists, attempting to get content")
          (with-current-buffer (find-file-noselect file)
            (org-with-wide-buffer
             (message "[Get Content] Searching for node ID in file")
             (when (org-find-entry-with-id node-id)
               (message "[Get Content] Found node entry")
               (org-back-to-heading t)
               (let* ((element (org-element-at-point))
                      (begin (org-element-property :begin element))
                      (end (org-element-property :end element))
                      (content (buffer-substring-no-properties begin end)))
                 (message "[Get Content] Got content from %d to %d (%d chars)" 
                         begin end (length content))
                 (if (string-suffix-p "\n" content)
                     content
                   (concat content "\n")))))))
      (message "[Get Content] File does not exist: %s" file)
      nil)))

(defun org-supertag-node--move-internal (node-id target-file &optional target-level target-point)
  "Move node to target file.
NODE-ID is the node identifier
TARGET-FILE is the target file path
TARGET-LEVEL is the target heading level
TARGET-POINT is buffer position to insert (nil means end of file)

Returns:
- t if move successful
- nil if move failed"
  (message "[Move] Starting node move: ID=%s to file=%s level=%s point=%s" 
           node-id target-file target-level target-point)
  
  (when-let* ((content (org-supertag-get-node-content node-id)))
    (message "[Move] Got node content: %d chars" (length content))
    
    ;; 使用 condition-case 包装整个移动过程
    (condition-case err
        (progn
          ;; 1. Delete node from source
          (unless (org-supertag-delete-node-content node-id)
            (error "Failed to delete source node"))
          
          ;; 2. Insert to target and update database
          (with-current-buffer (find-file-noselect target-file)
            (save-excursion
              ;; Move to target position
              (if target-point
                  (progn
                    (goto-char target-point)
                    (message "[Move] Moving to specified point: %d" target-point))
                (progn
                  (goto-char (point-max))
                  (message "[Move] Moving to end of file")))
              
              ;; Ensure we're at the beginning of a line
              (beginning-of-line)
              (message "[Move] Current point after positioning: %d" (point))
              
              ;; Temporarily disable folding
              (let ((org-fold-core-style 'overlays))  
                ;; Insert with proper level adjustment
                (let ((adjusted-content 
                       (if (and target-level (> target-level 0))
                           (progn
                             (message "[Move] Adjusting content to level %d" target-level)
                             (org-supertag-adjust-node-level content target-level))
                         content)))
                  
                  (message "[Move] Content ready for insertion (%d chars)" 
                          (length adjusted-content))
                  
                  ;; Ensure proper spacing before insertion
                  (unless (or (bobp) (looking-back "\n\n" 2))
                    (insert "\n"))
                  
                  ;; Insert content
                  (let ((insert-point (point)))
                    (message "[Move] Inserting content at point %d" insert-point)
                    (insert adjusted-content)
                    
                    ;; Ensure proper spacing after insertion
                    (unless (looking-at "\n")
                      (insert "\n"))
                    
                    ;; 立即保存缓冲区，避免搜索未保存内容
                    (save-buffer)
                    
                    ;; 使用更可靠的方法查找和同步节点 - 避免直接搜索
                    (message "[Move] Syncing inserted node...")
                    (widen)
                    (let ((marker (org-id-find-id-in-file node-id (buffer-file-name))))
                      (if marker
                          (progn
                            (goto-char (cdr marker))
                            (when (org-at-heading-p)
                              (message "[Move] Found heading at point %d" (point))
                              ;; 确保可见
                              (org-show-context)
                              (org-show-subtree)
                              ;; 同步节点
                              (org-supertag-node-sync-at-point)
                              (message "[Move] Node move completed successfully")
                              t))
                        (message "[Move] Warning: Could not find ID in target file, but insertion completed")
                        ;; 尝试返回到插入点并找到标题
                        (goto-char insert-point)
                        (when (re-search-forward "^\\*+ " nil t)
                          (beginning-of-line)
                          (when (org-at-heading-p)
                            (message "[Move] Found heading using fallback method")
                            (org-id-get-create)
                            (org-supertag-node-sync-at-point)))
                        t)))))))
          t)  ; 成功完成返回 t
      
      (error
       (message "[Move] Error during move: %s" (error-message-string err))
       nil))))  ; 发生错误返回 nil

(defun org-supertag-delete-node-content (node-id)
  "Delete node content from source file. (Internal helper)
NODE-ID is the node identifier

Returns:
- t if deletion successful
- nil if node not found or deletion failed"
  (message "Attempting to delete node: %s" node-id)
  (when-let* ((node (org-supertag-db-get node-id))
              (source-file (plist-get node :file-path))
              ((file-exists-p source-file))
              (loc (org-supertag-find-node-location node-id source-file)))
    (message "Found node location in %s at %s" source-file loc)
    (with-current-buffer (find-file-noselect source-file)
      (org-with-wide-buffer
       (goto-char (car loc))
       (if (org-at-heading-p)
           (let* ((element (org-element-at-point))
                  (begin (org-element-property :begin element))
                  (end (org-element-property :end element)))
             (message "Deleting region from %d to %d" begin end)
             (delete-region begin end)
             (when (looking-at "\n") 
               (delete-char 1))
             (save-buffer)
             t)
                    (message "Not at heading after moving to location")
           nil)))))

;;------------------------------------------------------------------------------
;; Interactive Commands
;;------------------------------------------------------------------------------

(defun org-supertag-node-move ()
  "Interactive command to move current node to another location.

The command will:
1. Check if current position is in a valid node, if not create one
2. Ask user to select target file
3. Ask user to select insert position in target file
4. Move the node to target location"
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Must be on a heading"))
  
  ;; Get or create node ID and ensure it's synced
  (let ((node-id (or (org-id-get)
                     (progn 
                       (org-supertag-node-create)
                       (org-id-get)))))
    (message "Moving node with ID: %s" node-id)
    (org-supertag-node-sync-at-point)
    (unless (org-supertag-db-get node-id)
      (user-error "Current node not found in database"))
    ;; Get target file and position
    (let* ((target-file (read-file-name "Move to file: "))
           (insert-pos (org-supertag-query--get-insert-position target-file))
           (target-pos (car insert-pos))
           (level-adj (cdr insert-pos)))
            (message "Target file: %s, position: %s, level: %s"
               target-file target-pos level-adj)
      (if (org-supertag-node--move-internal node-id target-file level-adj target-pos)
          (message "Node moved successfully")
        (message "Failed to move node")))))

(defun org-supertag-find-file-content-start ()
  "Find the position where actual content should start in current buffer.
This skips over the org-mode file header (#+TITLE:, #+AUTHOR:, etc.)
and returns the position where new content should be inserted."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (looking-at "^#\\+\\|^$"))
      (forward-line 1))
    (point)))

(defun org-supertag-adjust-node-level (content target-level)
  "Adjust heading level of node content.
CONTENT is the node content string
TARGET-LEVEL is the desired heading level (integer)

Returns adjusted content string with proper heading levels."
  (with-temp-buffer
    (org-mode)
    (insert content)
    (goto-char (point-min))
    (when (and target-level (> target-level 0))
      ;; Find and get current level
      (when (re-search-forward "^\\(\\*+\\)\\( .*\\)" nil t)
        (let* ((current-level (length (match-string 1)))
          (level-diff (- target-level current-level)))
          (unless (zerop level-diff)
            (goto-char (point-min))
            ;; Adjust all heading levels
            (while (re-search-forward "^\\(\\*+\\)\\( .*\\)" nil t)
              (let* ((stars (match-string 1))
                     (text (match-string 2))
                     (current-stars-len (length stars))
                     (new-level (+ current-stars-len level-diff))
                     (new-stars (make-string (max 1 new-level) ?*)))
                (replace-match (concat new-stars text))))))))
    (buffer-string)))

(defun org-supertag--move-node-and-link (node-id target-file &optional target-level)
  "Internal function to move a node and create link.
NODE-ID is the ID of the node to move.
TARGET-FILE is the destination file path.
TARGET-LEVEL is the optional target heading level."

  ;; 1. 获取源节点信息
  (let* ((node (org-supertag-db-get node-id))
         (source-file (or (plist-get node :file-path)
                          (buffer-file-name)))
         (source-pos (or (plist-get node :pos)
                         (point)))
         (source-element (with-current-buffer (find-file-noselect source-file)
                           (save-excursion
                             (goto-char source-pos)
                             (org-element-at-point))))
         (title (org-element-property :raw-value source-element))
         (content (buffer-substring (org-element-property :contents-begin source-element)
                                    (org-element-property :contents-end source-element)))
         (properties (org-element-property :PROPERTY source-element))
         (level (or target-level
                    (org-element-property :level source-element)
                    1)))

    ;; 2. 在目标位置创建完整节点
    (with-current-buffer (find-file-noselect target-file)
      (save-excursion
        (let* ((insert-pos (org-supertag-query--get-insert-position target-file))
               (target-pos (car insert-pos)))
          (goto-char target-pos)
          ;; 插入完整节点内容
          (insert (make-string level ?*)
                  " " title "\n"
                  content "\n")
          (save-buffer))))

    ;; 3. 修改源节点为链接
    (with-current-buffer (find-file-noselect source-file)
      (save-excursion
        (goto-char source-pos)
        (org-narrow-to-subtree)
        (delete-region (point) (point-max))
        (beginning-of-line)  ;; 移动到行头
        (delete-region (point) (line-end-position))  ;; 清理从行头到行末的内容
        (insert (make-string level ?*)
                " " (org-link-make-string (concat "id:" node-id) title) "\n")
        (widen)
        (save-buffer)))))

(defun org-supertag-move-node-and-link ()
  "Interactive command to move a node and create link.
Prompts user for node and target file from a list of Org files."
  (interactive)
  
  ;; 1. 获取当前节点信息
  (unless (org-id-get)
    (org-id-get-create))
  (let* ((node-id (org-id-get))
         (title (org-get-heading t t t t))
         (target-file (completing-read
                       "Move node to file: "
                       (org-supertag--get-org-file-list)  ;; 获取 Org 文件列表
                       nil t))  ;; 强制用户选择一个文件
         (target-level (when current-prefix-arg
                         (read-number "Target level: " (org-outline-level)))))

    ;; 2. 调用内部函数
    (org-supertag--move-node-and-link node-id target-file target-level)
    
    ;; 3. 显示成功消息
    (message "Moved node '%s' to %s" title target-file)))

(defun org-supertag--get-org-file-list ()
  "Return a list of Org files in the specified directory.
The directory is determined by `org-directory' or `default-directory'."
  (let* ((dir (or (if (boundp 'org-directory)
                      org-directory
                    default-directory)))
         (files (directory-files-recursively dir "\.org$" t)))  ;; 添加 t 参数以确保返回完整路径
    ;; 过滤掉备份文件和隐藏文件
    (seq-filter (lambda (file)
                  (and (not (string-match-p "/\\.#" file))  ;; 排除备份文件
                       (not (string-match-p "/\\.org$" file))  ;; 排除隐藏文件
                       (file-regular-p file)))  ;; 确保是文件而不是目录
                files)))

;;------------------------------------------------------------------------------
;; Node Commands 
;;------------------------------------------------------------------------------    

(defun org-supertag-node-create ()
  "Create a new supertag node.

Two use cases:
1. At blank position - Create new heading with ID
2. At existing heading without ID - Add ID to heading

Returns:
- Node ID on success
- Throws error on failure"
  (interactive)
  (let ((at-heading (org-at-heading-p)))
    (cond
     ;; Case 1: At existing heading with ID
     ((and at-heading (org-id-get))
      (user-error "Heading already has a node"))
     
     ;; Case 2: At existing heading without ID
     (at-heading
      (let* ((node-id (org-id-get-create))
             (props (org-supertag-db--parse-node-at-point)))
        (unless props
          (error "Failed to parse node properties"))
        ;; Add node to database
        (org-supertag-db-add node-id props)
        ;; Return node ID
        node-id))
     
     ;; Case 3: At blank position
     (t
      (let* ((title (read-string "Node title: "))
             ;; Ensure we're at beginning of line
             (_ (beginning-of-line))
             ;; Get current level or default to 1
             (current-level (if (org-at-heading-p)
                              (org-outline-level)
                            1)))
        ;; Insert new heading
        (insert (make-string current-level ?*) " " title "\n")
        ;; Move back to heading
        (forward-line -1)
        ;; Create node
        (let* ((node-id (org-id-get-create))
               (props (org-supertag-db--parse-node-at-point)))
          (unless props
            (error "Failed to parse node properties"))
          ;; Add node to database
          (org-supertag-db-add node-id props)
          ;; Return node ID
          node-id))))))

(defun org-supertag-node-delete ()
  "Delete current node and all its associated data."
  (interactive)
  (if-let* ((node-id (org-supertag-node--ensure-sync)))
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
          ;; 5. Remove from database using proper function
          (org-supertag-db-remove-object node-id)
          ;; 6. Save changes
          (save-buffer)
          ;; 7. Run hooks
          (run-hook-with-args 'org-supertag-node-deleted-hook node-id)
          (message "Node deleted: %s" node-id)))
    (user-error "No node found at current position")))

(defun org-supertag-node--ensure-id-system ()
  "Ensure org-id system is properly initialized."
  (require 'org-id)
  (unless (and (boundp 'org-id-locations)
               (hash-table-p org-id-locations))
    (setq org-id-locations (make-hash-table :test 'equal))
    (when (file-exists-p org-id-locations-file)
      (org-id-locations-load))))


(defun org-supertag-node-update ()
  "Update node at current position.
Uses org-supertag-node-sync-at-point to perform a complete node synchronization."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Must be on a heading to update node"))
  
  (let ((node-id (org-id-get)))
    (unless node-id
      (message "No node ID found, creating new node...")
      (setq node-id (org-id-get-create)))
    
    (if-let* ((sync-result (org-supertag-node--ensure-sync)))
        (progn
          (message "Node updated successfully: %s" node-id)
          node-id)
      (user-error "Failed to sync node. Please ensure:\n1. You're on a valid heading\n2. The heading is not archived or commented\n3. The node ID is valid"))))

(defun org-supertag-node-at-valid-heading-p ()
  "Check if point is at a valid heading for tag operations.
Returns t if:
1. Point is at a heading
2. Heading is not commented out
3. Heading is not archived
4. Heading is not in COMMENT tree"
  (let ((at-heading (org-at-heading-p)))
    (message "Checking heading validity:")
    (message "  At heading: %s" (if at-heading "yes" "no"))
    (when at-heading
      (message "  Heading text: '%s'" (org-get-heading t t t t))
      (message "  Point: %d" (point))
      (message "  In commented heading: %s" 
               (if (org-in-commented-heading-p) "yes" "no"))
      (message "  In archived heading: %s"
               (if (org-in-archived-heading-p) "yes" "no")))
    at-heading))

(defun org-supertag-node--format-path (file-path olp title &optional props)
  "Format node path for display.
FILE-PATH is the path to the file
OLP is the outline path list
TITLE is the node title
PROPS is optional property list for additional info"
  (let* ((file-name (if file-path 
                        (file-name-nondirectory file-path)
                      "<no file>"))
         (clean-title (or title "<no title>"))
         ;; 如果最后一个 olp 和标题相同，则不重复显示
         (filtered-olp (if (and olp (string= (car (last olp)) clean-title))
                          (butlast olp)
                        olp))
         (path-parts (append (list file-name)
                           filtered-olp
                           (list clean-title))))
    (mapconcat #'identity 
               (remove nil path-parts)
               " / ")))

(defun org-supertag-node--collect-nodes ()
  "Collect all nodes with their paths.
Returns a list of (node-id . display-string) pairs."
  (let ((nodes '())
        (display-counts (make-hash-table :test 'equal)))
    ;; First pass: count occurrences of each display string
    (maphash
     (lambda (id props)
       (when (eq (plist-get props :type) :node)
         (let* ((file-path (plist-get props :file-path))
                (title (or (plist-get props :title) "<untitled>"))
                (olp (plist-get props :olp))
                ;; 确保所有字符串属性都是纯文本
                (clean-title (if (stringp title)
                               (substring-no-properties title)
                             "<invalid title>"))
                (display (org-supertag-node--format-path file-path olp clean-title props)))
           (puthash display
                    (1+ (gethash display display-counts 0))
                    display-counts))))
     org-supertag-db--object)
    
    ;; Second pass: create nodes list with unique display strings
    (maphash
     (lambda (id props)
       (when (eq (plist-get props :type) :node)
         (let* ((file-path (plist-get props :file-path))
                (title (or (plist-get props :title) "<untitled>"))
                (olp (plist-get props :olp))
                ;; 确保所有字符串属性都是纯文本
                (clean-title (if (stringp title)
                               (substring-no-properties title)
                             "<invalid title>"))
                (base-display (org-supertag-node--format-path file-path olp clean-title props))
                ;; If display string is not unique, append node ID
                (display (if (> (gethash base-display display-counts) 1)
                           (format "%s [%s]" base-display id)
                         base-display)))
           (push (cons id display) nodes))))
     org-supertag-db--object)
    
    ;; 按显示字符串排序
    (sort nodes (lambda (a b) 
                 (string< (cdr a) (cdr b))))))

;;;###autoload
(defun org-supertag-node-find ()
  "Find and jump to a node using completion.
Displays nodes with their full paths in format:
filename / outline-path / title"
  (interactive)
  (let* ((nodes (org-supertag-node--collect-nodes))
         (candidates (mapcar #'cdr nodes))
         (choice (completing-read "Find node: " candidates nil t))
         (node-id (car (rassoc choice nodes))))
    
    (if node-id
        (if (org-supertag-query--find-node node-id)
            (message "Jumped to node: %s" choice)
          (message "Error: Could not find node location"))
      (message "No matching node found"))))
;;;###autoload
(defun org-supertag-node-find-other-window ()
  "Like `org-supertag-node-find' but display node in other window."
  (interactive)
  (let* ((nodes (org-supertag-node--collect-nodes))
         (candidates (mapcar #'cdr nodes))
         (choice (completing-read "Find node: " candidates nil t))
         (node-id (car (rassoc choice nodes))))
    
    (when node-id
      (when-let* ((props (org-supertag-db-get node-id))
                  (file-path (plist-get props :file-path))
                  ((file-exists-p file-path)))
        ;; Open file in other window
        (find-file-other-window file-path)
        ;; Find and show node
        (widen)
        (when-let* ((marker (org-id-find-id-in-file node-id file-path)))
          (goto-char (cdr marker))
          (org-show-entry)
          (org-show-children)
          (recenter)
          (message "Jumped to node: %s" choice))))))


;;------------------------------------------------------------------------------
;; Node Location Functions
;;------------------------------------------------------------------------------

(defun org-supertag-find-node-location (node-id &optional file-path)
  "Find the location of a node by ID.
NODE-ID: The node identifier to find
FILE-PATH: Optional file path to search in (if nil, searches in all files)

Returns:
- (point . file-path) if node found
- nil if node not found"
  (let* ((node (org-supertag-db-get node-id))
         (target-file (or file-path (plist-get node :file-path))))
    (when (and target-file (file-exists-p target-file))
      (with-current-buffer (find-file-noselect target-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (let ((found nil))
           (org-map-entries
            (lambda ()
              (when (equal (org-id-get) node-id)
                (setq found (point))))
            t nil)
           (when found
             (cons found target-file))))))))

(defun org-supertag-goto-node (node-id)
  "Go to a node by ID using database information.
NODE-ID: The node identifier to navigate to

Returns:
- t if successfully navigated
- nil if node not found"
  (if-let ((location (org-supertag-find-node-location node-id)))
      (let ((pos (car location))
            (file (cdr location)))
        (switch-to-buffer (find-file-noselect file))
        (widen)
        (goto-char pos)
        (org-show-context)
        (recenter)
        t)
    (message "Node with ID %s not found in any file." node-id)
    nil))

;;------------------------------------------------------------------------------
;; Node Relations 
;;------------------------------------------------------------------------------    

(defun org-supertag-node--in-node-p ()
  "Check if current position is within a valid org-supertag node."
  (when-let* ((node-id (org-supertag-node--ensure-sync)))
    (cons node-id (org-get-heading t t t t))))

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
(let ((cache-key (format "node-refs:%s:%s" node-id direction)))                                                                                    
    (or (org-supertag-db--cache-get 'query cache-key)                                                                                               
        (let ((results '())))                                                                                                                       
          (cond                                                                                                                                     
           ((eq direction 'to)                                                                                                                      
            (setq results (mapcar (lambda (link) (plist-get link :to))                                                                              
                                  (org-supertag-db-find-links :node-node :from node-id))))                                                           
           ((eq direction 'from)                                                                                                                    
            (setq results (mapcar (lambda (link) (plist-get link :from))                                                                            
                                  (org-supertag-db-find-links :node-node :to node-id))))                                                            
           (t                                                                                                                                       
            (let ((to-links (mapcar (lambda (link) (plist-get link :to))                                                                            
                                    (org-supertag-db-find-links :node-node :from node-id)))                                                         
                  (from-links (mapcar (lambda (link) (plist-get link :from))                                                                        
                                      (org-supertag-db-find-links :node-node :to node-id))))                                                        
              (setq results (delete-duplicates (append to-links from-links))))))                                                                    
          (org-supertag-db--cache-set 'query cache-key results)                                                                                     
          results)))                                                                                                                                
                       

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
    ;; 1. Insert link
    (insert link-text)
    
    ;; 2. Update reference relationships
    (let* ((current-node-id (org-id-get))
           (current-node (org-supertag-db-get current-node-id)))
      
      ;; 2.1 Update current node's ref-to
      (let* ((current-refs (or (plist-get current-node :ref-to) nil)))
        (setq current-refs (cl-adjoin node-id current-refs :test #'string=))
        (org-supertag-db-add 
         current-node-id
         (append current-node  
                (list :ref-to current-refs
                      :ref-count (length current-refs)))))
      
      ;; 2.2 Update target node's ref-from
      (let* ((target-refs (or (plist-get target-node :ref-from) nil)))
        (setq target-refs (cl-adjoin current-node-id target-refs :test #'string=))
        (org-supertag-db-add 
         node-id
         (append target-node  
                (list :ref-from target-refs
                      :ref-count (length target-refs))))))))


(defun org-supertag-node--find-by-title (title)
  "Find node by exact title match.
TITLE is the title to search for.
Returns node-id if found, nil otherwise."
  (let ((result nil))
    (maphash
     (lambda (id props)
       (when (and (eq (plist-get props :type) :node)
                  (string= (plist-get props :title) title))
         (setq result id)))
     org-supertag-db--object)
    result))

(defun org-supertag-node-add-reference ()
  "Add reference to current node.
If region is active:
  1. Checks if node with same title exists
  2. If exists, uses that node
  3. If not, creates a new node from selected text
  4. Replaces selection with a link to the node
If no region:
  1. Insert reference to an existing node
In both cases:
  - Creates node if current position is in a heading or its content
  - Updates reference relationships"
  (interactive)
  (let ((current-node-id (or (org-id-get)
                            (org-supertag-node--ensure-sync))))
    (unless current-node-id
      (user-error "Must be within a heading or its content area"))
    
    (if (use-region-p)
        ;; Handle selected text
        (let* ((source-buffer (current-buffer))
               (beg (copy-marker (region-beginning)))
               (end (copy-marker (region-end)))
               (selected-text (buffer-substring-no-properties beg end))
               ;; Check if node with same title exists
               (existing-node-id (org-supertag-node--find-by-title selected-text)))
          
          (if existing-node-id
              ;; If exists, use existing node
              (progn
                (with-current-buffer source-buffer
                  (delete-region beg end)
                  (goto-char beg)
                  (org-supertag-node--insert-reference existing-node-id))
                (message "Using existing node: %s" selected-text))
            
            ;; If not, create new node
            (let ((target-file (completing-read
                              "Create reference in file: "
                              (org-supertag--get-org-file-list)
                              nil t)))
              
              (unwind-protect
                  (with-current-buffer (find-file-noselect target-file)
                    (save-excursion
                      (goto-char (point-max))
                      ;; Ensure empty line at end of file
                      (unless (bolp) (insert "\n"))
                      (unless (looking-back "\n\n" (- (point) 2))
                        (insert "\n"))
                      
                      ;; Insert new node
                      (insert "* " selected-text "\n")
                      ;; Ensure new node is at the end of file
                      (forward-line -1)
                      ;; Create node and get ID
                      (let ((new-node-id (org-supertag-node-create)))
                        ;; Return to original buffer and replace selected text with link
                        (with-current-buffer source-buffer
                          (delete-region beg end)
                          (goto-char beg)
                          (org-supertag-node--insert-reference new-node-id))
                        
                        ;; Save all changes
                        (save-buffer)
                        (with-current-buffer source-buffer
                          (save-buffer))
                        
                        (message "Created reference node: %s" selected-text))))
                ;; Clean up markers
                (set-marker beg nil)
                (set-marker end nil))))
          
          ;; Ensure markers are cleaned up (in case of early return)
          (set-marker beg nil)
          (set-marker end nil))
      
      ;; Handle case with no selected text - add reference to existing node
      (let* ((nodes (org-supertag-node--collect-nodes))
             (candidates (mapcar #'cdr nodes))
             (choice (completing-read "Reference node: " candidates nil t))
             (node-id (car (rassoc choice nodes))))
        
        (when node-id
          ;; Insert reference
          (org-supertag-node--insert-reference node-id)
          ;; Update reference relationships
          (message "Added reference to: %s" choice))))))

(defun org-supertag-node--collect-reference-id-title ()
  "Collect all reference information in current node.
Returns: ((title . id) ...)"
  (let ((refs nil)
        (current-node-id (org-id-get)))
    (when current-node-id
      (dolist (ref-node-id (org-supertag-node-db-get-reference current-node-id 'to))
        (when-let* ((node (org-supertag-db-get ref-node-id))
                     (title (plist-get node :title)))
          (push (cons title ref-node-id) refs))))
    (nreverse refs)))

(defun org-supertag-node-remove-reference (node-id)
  "Remove reference to specified node."
  (interactive 
   (let* ((current-id (org-supertag-node--ensure-sync))
          (refs (when current-id (org-supertag-node--collect-reference-id-title)))
          (selected (completing-read 
                    "Remove reference: "
                    (mapcar #'car refs)
                    nil t)))
     (list (cdr (assoc selected refs)))))
  (let ((current-node-id (org-supertag-node--ensure-sync)))
    (when (and current-node-id node-id)
      (org-supertag-db-unlink :node-node current-node-id node-id)
      (message "Removed reference from %s to %s" current-node-id node-id))))

(defun org-supertag-node-db-update-reference (from-id to-id)
  "Update reference relationship in database.
FROM-ID: Source node ID
TO-ID: Target node ID"
  (when (and (org-supertag-node-db-exists-p from-id)
             (org-supertag-node-db-exists-p to-id))
    ;; 1. Update reference relationship in database
    (org-supertag-db-link :type :node-ref
                         :from from-id
                         :to to-id)
    
    ;; 2. Clear related caches
    (org-supertag-db--cache-remove 'query 
                                  (format "node-refs:%s" from-id))
    (org-supertag-db--cache-remove 'query 
                                  (format "node-refs:%s" to-id))
    
    ;; 3. Trigger reference update event
    (run-hook-with-args 'org-supertag-node-reference-updated-hook
                       from-id to-id)))

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

(defun org-supertag-check-and-fix-ids ()
  "Check and fix ID-related issues in org-supertag.
This function will:
1. Update ID locations database
2. Check for missing IDs
3. Report problematic entries"
  (interactive)
  (let ((missing-ids nil)
        (total-ids 0)
        (problematic-files nil))
    
    ;; 1. Update ID locations
    (message "Updating ID locations...")
    (org-id-update-id-locations)
    
    ;; 2. Check database entries
    (message "Checking database entries...")
    (maphash
     (lambda (id props)
       (cl-incf total-ids)
       (when (and (eq (plist-get props :type) :node)
                  (not (org-id-find id 'marker)))
         (push id missing-ids)
         (when-let* ((file (plist-get props :file-path)))
           (push file problematic-files))))
     org-supertag-db--object)
    
    ;; 3. Report results
    (with-output-to-temp-buffer "*Org-Supertag ID Check*"
      (princ "Org-Supertag ID Check Results\n")
      (princ "===========================\n\n")
      
      (princ (format "Total IDs checked: %d\n" total-ids))
      (princ (format "Missing IDs: %d\n\n" (length missing-ids)))
      
      (when missing-ids
        (princ "Missing ID Details:\n")
        (princ "-------------------\n")
        (dolist (id missing-ids)
          (when-let* ((props (org-supertag-db-get id)))
            (princ (format "ID: %s\n" id))
            (princ (format "  Title: %s\n" (or (plist-get props :title) "[No title]")))
            (princ (format "  File: %s\n" (or (plist-get props :file-path) "[No file]")))
            (princ "\n"))))
      
      (when problematic-files
        (princ "\nProblematic Files:\n")
        (princ "-----------------\n")
        (dolist (file (delete-dups problematic-files))
          (princ (format "%s\n" file))))
      
      (princ "\nRecommended Actions:\n")
      (princ "------------------\n"
      (princ "1. Check if problematic files still exist\n")
      (princ "2. Run org-id-update-id-locations\n")
      (princ "3. Consider removing invalid entries from the database\n")
      
      (when missing-ids
        (princ "\nTo remove invalid entries, evaluate:\n")
        (princ "(dolist (id '")
        (prin1 missing-ids (current-buffer))
        (princ ") (org-supertag-db-remove id))"))))))

(provide 'org-supertag-node)

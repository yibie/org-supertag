;;; org-supertag-field-operation.el --- Field operations for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides field operation functionality for org-supertag
;; Core principle: Manage field values and field editing interface

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-supertag-db)
(require 'org-supertag-field)



;;----------------------------------------------------------------------
;; Field Value Entity Operations
;;----------------------------------------------------------------------

(defun org-supertag-field-set-value (field-def value node-id tag-id)
  "Set field value for node.
FIELD-DEF is the field definition
VALUE is the field value
NODE-ID is the target node ID
TAG-ID is the tag ID"
  (condition-case err
      (let* ((field-name (plist-get field-def :name))
             (sanitized-name (org-supertag--sanitize-field-name field-name))
             (field-type (plist-get field-def :type))
             (type-def (org-supertag-get-field-type field-type))
             (processed-value
              (progn
                (message "Debug - Setting field: name=%S, type=%S, initial-value=%S" 
                         sanitized-name field-type value)
                (if (and type-def value)
                    (progn
                      (message "Debug - Processing value with type-def: %S" type-def)
                      (when-let* ((validator (plist-get type-def :validator)))
                        (message "Debug - Validating with %S" validator)
                        (unless (funcall validator value)
                          (error "Value validation failed for %s: %S" sanitized-name value)))
                      (if-let* ((formatter (plist-get type-def :formatter)))
                          (progn
                            (message "Debug - Formatting with %S" formatter)
                            (funcall formatter value field-def))
                        value))
                  (progn
                    (message "Debug - Using raw value (no processing): %S" value)
                    value)))))
        
        ;; Use sanitized name for all operations
        (unless (string= field-name sanitized-name)
          (setq field-name sanitized-name)
          (setq field-def (copy-sequence field-def))
          (setf (plist-get field-def :name) sanitized-name))
        
        (message "Debug - Final processed value: %S" processed-value)
        
        (when (eq field-type 'tag-reference)
          (message "Debug - Processing tag-reference: value=%S, processed=%S" 
                   value processed-value)
          (when-let* ((old-value (org-supertag-field-get-value field-def node-id tag-id)))
            (org-supertag-node-db-remove-reference node-id old-value))
          (when processed-value
            (org-supertag-node-db-add-reference node-id processed-value)))
        
        (org-supertag-db-set-field-value node-id field-name processed-value tag-id)
        
        (when-let* ((pos (condition-case nil
                           (org-id-find node-id t)
                         (error nil))))
          (org-with-point-at pos
            (org-set-property field-name processed-value))))
    (error
     (message "Error in field set-value operation: %s"
              (error-message-string err))
     (signal (car err) (cdr err)))))

(defun org-supertag-field-validate (field value)
  "Validate field value.
FIELD: Field definition
VALUE: Value to validate
Returns t if valid, otherwise throws error"
  (let* ((name (plist-get field :name))
         (type (plist-get field :type))
         (required (plist-get field :required))
         (type-def (org-supertag-get-field-type type))
         (validator (plist-get type-def :validator)))
    (message "Debug - Validating field: name=%S, type=%S, value=%S" name type value)
    
    (when (and required (null value))
      (error "Field '%s' is required" name))
    
    (if (null value)
        t
      (progn
        (when (and (eq type 'string) (not (stringp value)))
          (error "Field '%s' requires string type, got %S" name (type-of value)))
        
        (when validator
          (condition-case err
              (if (not (funcall validator value))
                (error "Value '%s' does not meet validation requirements" value))
            (error
             (error "Error validating field '%s': %s" name (error-message-string err)))))
        t))))

(defun org-supertag-field-get-value (field-def node-id tag-id)
  "Get field value.
FIELD-DEF: field definition
NODE-ID: node ID
TAG-ID: tag ID"
  (let* ((field-name (plist-get field-def :name))
         (type (plist-get field-def :type))
         (type-spec (org-supertag-get-field-type type))
         (value (org-supertag-db-get-field-value node-id field-name tag-id)))
    (when (and value
               (plist-get type-spec :formatter))
      (setq value (funcall (plist-get type-spec :formatter) value field-def)))
    value))

(defun org-supertag-field-remove-value (field-def node-id tag-id)
  "Remove field value.
FIELD-DEF: field definition
NODE-ID: node ID
TAG-ID: tag ID"
  (let ((field-name (plist-get field-def :name)))
    (org-supertag-db-remove-link :node-field node-id field-name
                                `(:tag-id ,tag-id))
    (org-with-point-at (org-id-find node-id t)
      (org-entry-delete nil field-name))))

(defun org-supertag-field-batch-set-value (field-def-list value-list node-id tag-id)
  "Set multiple field values in batch.
FIELD-DEF-LIST: List of field definitions to set
VALUE-LIST: List of values corresponding to the fields
NODE-ID: ID of the target node
TAG-ID: ID of the associated tag"
  (cl-loop for field-def in field-def-list
           for value in value-list
           do (org-supertag-field-set-value field-def value node-id tag-id)))

(defun org-supertag-field-find-nodes (field-name value &optional tag-id)
  "Find nodes that have a specific field value.
FIELD-NAME: Name of the field to search
VALUE: Value to match against
TAG-ID: Optional tag ID to limit search scope

Returns a list of nodes that match the criteria."
  (let ((links (org-supertag-db-get-links :node-field)))
    (cl-remove-if-not
     (lambda (link)
       (and (equal (car link) field-name)  ; Match field name
            (equal (plist-get (cadr link) :value) value)  ; Match value
            (or (null tag-id)  ; If no tag ID specified
                (equal (plist-get (cadr link) :tag-id) tag-id)))) ; Match tag ID
     links)))

;;----------------------------------------------------------------------
;; Field Edit Interface 
;;----------------------------------------------------------------------
(defvar-local org-supertag--pending-operations nil
  "存储待执行的操作。
格式为 ((operation . args) ...)
operation 可以是 'remove-field, 'add-field, 'edit-field 等。")

(defun org-supertag-tag--refresh-field-table (context)
  "Refresh the field table display.
CONTEXT is the edit context plist containing:
- :node-id        The node ID
- :node-title     The node title
- :tags           List of tags
- :source-buffer  The source buffer
- :source-point   The point in source buffer"
  (let ((inhibit-read-only t)
        (current-point (point))
        (current-values (org-supertag-tag--collect-current-values)))
    (erase-buffer)
    ;; 显示待执行操作的提示
    (when org-supertag--pending-operations
      (insert "Pending operations:\n")
      (dolist (op org-supertag--pending-operations)
        (pcase (car op)
          ('remove-field
           (insert (format "  - Remove field '%s'\n" (cddr op))))
          (_ (insert (format "  - Unknown operation: %S\n" op)))))
      (insert "\n"))
    
    ;; 插入常规内容
    (insert (format "Fields for Node: %s\n" 
                    (plist-get context :node-title)))
    ;; 插入字段表
    (dolist (tag-id (plist-get context :tags))
      (org-supertag-tag--insert-tag-fields tag-id
                                          (plist-get context :source-buffer)
                                          (plist-get context :source-point)))
    
    ;; 帮助文本
    (insert "\nCommands:\n")
    (insert "n: Next field    p: Previous field    RET: Edit field\n")
    (insert "C-c C-c: Save and apply changes    C-c C-k: Cancel\n")
    
    ;; 恢复字段值和光标位置
    (org-supertag-tag--restore-field-values current-values)
    (goto-char (min current-point (point-max)))))

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
         ;; 标准化字段名
         (sanitized-field-def 
          (let ((field-name (plist-get field-def :name)))
            (setq field-def 
                  (plist-put (copy-sequence field-def)
                            :name 
                            (org-supertag--sanitize-field-name field-name)))))
         (new-fields (append fields (list sanitized-field-def)))
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

(defun org-supertag-tag--queue-remove-field (tag-id field-name)
  "将字段删除操作加入队列。
TAG-ID: 标签ID
FIELD-NAME: 字段名称"
  (push `(remove-field . (,tag-id . ,field-name))
        org-supertag--pending-operations)
  (message "Field '%s' marked for removal. Press C-c C-c to apply changes." 
           field-name))

;;----------------------------------------------------------------------
;; Field Edit Mode
;;----------------------------------------------------------------------  

(defun org-supertag-tag--add-field (tag-id)
  "Add a new field to TAG-ID."
  (let* ((raw-field-name (read-string "Field name: "))
         (sanitized-name (org-supertag--sanitize-field-name raw-field-name))
         ;; 显示标准化后的名称
         (_ (message "Field name will be standardized to: %s" sanitized-name))
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
          (list :name sanitized-name  ; 使用标准化的名称
                :type field-type)))

    (when-let* ((help-buf (get-buffer "*Minibuf Help*")))
      (kill-buffer help-buf))
    
    ;; 添加 options 类型的选项
    (when (eq field-type 'options)
      (let ((options
             (split-string
              (read-string "Options (comma separated): ")
              "," t "[ \t]+")))
        (setq field-def 
              (plist-put field-def :options options))))
    
    ;; 添加描述
    (let ((desc (read-string "Description (optional): ")))
      (when (not (string-empty-p desc))
        (setq field-def 
              (plist-put field-def :description desc))))
    
    ;; 添加字段到标签
    (org-supertag-tag-add-field tag-id field-def)))

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

         (when-let* ((help-buf (get-buffer "*Minibuf Help*")))
           (kill-buffer help-buf))
         
         (setq field (plist-put field :type new-type))
         (when (eq new-type 'options)
           (let ((options
                  (split-string
                   (read-string "Options (comma separated): ")
                   "," t "[ \t]+")))
             (setq field (plist-put field :options options))))
         ;; Update field value in tag definition
         (let ((new-fields (cl-substitute field
                                        (cl-find field-name fields
                                                :key (lambda (f) (plist-get f :name))
                                                :test #'equal)
                                        fields
                                        :test #'equal)))
           ;; Update tag in database
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
           ;; Update field value in tag definition
           (let ((new-fields (cl-substitute field
                                          (cl-find field-name fields
                                                  :key (lambda (f) (plist-get f :name))
                                                  :test #'equal)
                                          fields
                                          :test #'equal)))
             ;; Update tag in database
             (setq tag (plist-put tag :fields new-fields))
             (org-supertag-db-add tag-id tag)))))
      
      ("Edit description"
       (let ((new-desc (read-string "New description: "
                                   (plist-get field :description))))
         (setq field (plist-put field :description new-desc))
         ;; Update field value in tag definition
         (let ((new-fields (cl-substitute field
                                        (cl-find field-name fields
                                                :key (lambda (f) (plist-get f :name))
                                                :test #'equal)
                                        fields
                                        :test #'equal)))
           ;; Update tag in database
           (setq tag (plist-put tag :fields new-fields))
           (org-supertag-db-add tag-id tag)))))))

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
    (let* ((field-name (string-trim (match-string 1)))
           (field-type (match-string 2))
           (current-value (match-string 3))
           ;; Fix tag ID extraction to only get the tag name
           (tag-id (save-excursion
                    (beginning-of-line)
                    (re-search-backward "^Tag: \\([^ \n]+\\)" nil t)
                    (match-string-no-properties 1)))
           (tag (org-supertag-tag-get tag-id)))
      
      (unless tag
        (error "Tag '%s' not found" tag-id))
      
      (let ((field (cl-find field-name 
                           (plist-get tag :fields)
                           :key (lambda (f) (plist-get f :name))
                           :test #'equal)))
        ;; Validate field exists and has a type defined
        (unless field
          (error "Field '%s' not found in tag '%s'" field-name tag-id))
        (unless (plist-get field :type)
          (error "Field '%s' has no type defined" field-name))
        
        ;; Read new value
        (when-let* ((new-value (org-supertag-field-read-value field)))
          (save-excursion
            (beginning-of-line)
            (when (re-search-forward ": .*$" nil t)
              (replace-match (format ": %s" new-value)))))))))

(defun org-supertag-field-edit-save ()
  "Save all field values and close the edit buffer."
  (interactive)
  (let* ((context org-supertag--edit-context)
         (source-buffer (plist-get context :source-buffer))
         (source-point (plist-get context :source-point))
         (modified-buffers (make-hash-table :test 'equal))
         (node-id (plist-get context :node-id))
         (tags (plist-get context :tags)))
    
    ;; 1. 收集需要修改的缓冲区并禁用 org-mode 功能
    (dolist (op org-supertag--pending-operations)
      (pcase (car op)
        ('remove-field
         (let* ((tag-id (cadr op))
                (field-name (cddr op))
                (nodes (org-supertag-tag-get-nodes tag-id)))
           (dolist (node-id nodes)
             (when-let* ((marker (org-id-find node-id t))
                        (buf (if (markerp marker)
                               (marker-buffer marker)
                             (current-buffer))))
               (puthash (buffer-file-name buf) buf modified-buffers)))))))
    
    ;; 2. 禁用 org-mode 功能
    (maphash (lambda (_file buf)
               (with-current-buffer buf
                 (setq-local org-element-use-cache nil)))
             modified-buffers)
    
    ;; 3. 执行待处理操作
    (dolist (op org-supertag--pending-operations)
      (pcase (car op)
        ('remove-field
         (let* ((tag-id (cadr op))
                (field-name (cddr op))
                (tag (org-supertag-db-get tag-id))
                (fields (plist-get tag :fields))
                (new-fields (cl-remove-if 
                           (lambda (field)
                             (equal (plist-get field :name) field-name))
                           fields))
                (new-tag (plist-put (copy-sequence tag) :fields new-fields)))
           ;; 更新数据库
           (org-supertag-db-add tag-id new-tag)
           
           ;; 更新文件
           (dolist (node-id (org-supertag-tag-get-nodes tag-id))
             (org-supertag-db-delete-field-value node-id field-name tag-id)
             (when-let* ((marker (org-id-find node-id t)))
               (with-current-buffer (if (markerp marker)
                                      (marker-buffer marker)
                                    (current-buffer))
                 (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char (if (markerp marker)
                                  (marker-position marker)
                                marker))
                     (org-delete-property field-name))))))))))
    
    ;; 4. 保存当前显示的字段值
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^  \\([^[]+\\)\\[\\([^]]+\\)\\]: \\(.*\\)$" nil t)
        (let* ((field-name (string-trim (match-string 1)))
               (field-type (match-string 2))
               (new-value (string-trim (match-string 3)))
               ;; 获取当前字段所属的标签ID
               (tag-id (save-excursion
                        (re-search-backward "^Tag: \\([^ \n]+\\)" nil t)
                        (match-string-no-properties 1)))
               ;; 获取标签和字段定义
               (tag (when tag-id (org-supertag-tag-get tag-id)))
               (field (when tag
                       (cl-find field-name
                               (plist-get tag :fields)
                               :key (lambda (f) (plist-get f :name))
                               :test #'equal))))
          (unless (string-empty-p new-value)
            ;; 更新 org 文件
            (with-current-buffer source-buffer
              (save-excursion
                (goto-char source-point)
                (org-set-property field-name new-value)))
            ;; 更新数据库
            (when (and tag-id field)
              (org-supertag-db-set-field-value node-id field-name new-value tag-id))))))
    
    ;; 5. 保存并重新加载修改过的缓冲区
    (maphash (lambda (_file buf)
               (with-current-buffer buf
                 (save-buffer)
                 ;; 重新启用缓存
                 (setq-local org-element-use-cache t)
                 ;; 重新加载 org-mode
                 (when (derived-mode-p 'org-mode)
                   (org-mode))))
             modified-buffers)
    
    ;; 6. 清理并退出
    (setq org-supertag--pending-operations nil)
    (quit-window t)))

(defun org-supertag-field-edit-cancel ()
  "Cancel editing and close the buffer."
  (interactive)
  (when (yes-or-no-p "Cancel editing? Changes will be lost.")
    (setq org-supertag--pending-operations nil)
    (quit-window t)))

(defvar org-supertag-field-edit-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Basic navigation and editing
    (define-key map (kbd "n") #'org-supertag-field-edit-next-field)
    (define-key map (kbd "p") #'org-supertag-field-edit-prev-field)
    (define-key map (kbd "RET") #'org-supertag-field-edit-complete-field)
    (define-key map (kbd "C-c C-c") #'org-supertag-field-edit-save)
    (define-key map (kbd "C-c C-k") #'org-supertag-field-edit-cancel)
    
    ;; Field operations
    (define-key map (kbd "a")
      (lambda ()
        (interactive)
        (let ((tag-id (save-excursion
                       (re-search-backward "^Tag: \\([^ \n]+\\)" nil t)
                       (match-string-no-properties 1))))
          (when tag-id
            (org-supertag-tag--add-field tag-id)
            (org-supertag-tag--refresh-field-table org-supertag--edit-context)))))
    
    (define-key map (kbd "e")
      (lambda ()
        (interactive)
        (save-excursion
          (beginning-of-line)
          (when (looking-at "^  \\([^ ]+\\) \\[")
            (let* ((field-name (match-string 1))
                   (tag-id (save-excursion
                            (re-search-backward "^Tag: \\([^ \n]+\\)" nil t)
                            (match-string-no-properties 1))))
              (when (and tag-id field-name)
                (org-supertag-tag--edit-field tag-id field-name)
                (org-supertag-tag--refresh-field-table org-supertag--edit-context)))))))
    
    (define-key map (kbd "d")
      (lambda ()
        (interactive)
        (org-supertag-field-edit-mode-remove-field)))
    map))

(define-derived-mode org-supertag-field-edit-mode special-mode "OrgSuperTag-FieldEdit"
  "Major mode for editing org-supertag fields."
  (setq-local buffer-read-only nil))

(defun org-supertag-tag--collect-current-values ()
  "Collect current field values from the edit buffer.
Returns an alist of (field-name . value) pairs."
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^  \\([^[]+\\)\\[\\([^]]+\\)\\]: \\(.*\\)$" nil t)
        (let ((field-name (string-trim (match-string 1)))
              (value (string-trim (match-string 3))))
          (push (cons field-name value) result))))
    result))

(defun org-supertag-tag--restore-field-values (values)
  "Restore field values in the edit buffer.
VALUES is an alist of (field-name . value) pairs."
  (when values
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^  \\([^[]+\\)\\[\\([^]]+\\)\\]: " nil t)
        (let* ((field-name (string-trim (match-string 1)))
               (saved-value (cdr (assoc field-name values))))
          (when saved-value
            (insert saved-value)))))))

(defun org-supertag-tag--insert-tag-fields (tag-id source-buffer source-point)
  "Insert fields for a tag."
  (let* ((tag (org-supertag-tag-get tag-id))
         (base-tag (org-supertag-tag-get-base tag-id))
         (fields (plist-get tag :fields))
         (max-name-length 
          (if fields
              (apply #'max 
                     (mapcar (lambda (f) 
                             (length (plist-get f :name)))
                            fields))
            0))
         (max-type-length
          (if fields
              (apply #'max 
                     (mapcar (lambda (f)
                             (length (symbol-name (plist-get f :type))))
                            fields))
            0)))
    (insert (format "Tag: %s" tag-id))
    (when base-tag
      (insert (format " (extends %s)" base-tag)))
    (insert "\n")
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

(defun org-supertag-tag-edit-fields ()
  "Edit fields of current node's tags in a table format.
Creates a dedicated buffer showing all fields of the node's tags in a table view,
allowing for easy editing of field values."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (source-point (point))
         (node-id (org-id-get))
         (tags (org-supertag-node-get-tags node-id))
         (edit-buffer (get-buffer-create "*Org SuperTag Fields*"))
         (node-title (org-get-heading t t t t)))
    
    ;; Validate we have a node and tags
    (unless node-id
      (error "Not on a valid node"))
    (unless tags
      (error "No tags on current node"))
    
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
        (org-supertag-tag--refresh-field-table org-supertag--edit-context)))
    
    ;; Display buffer and position cursor
    (let ((window (display-buffer edit-buffer
                                '((display-buffer-below-selected)
                                  (window-height . fit-window-to-buffer)
                                  (preserve-size . (nil . t))
                                  (select . t)))))
      (select-window window)
      (org-supertag-tag--goto-first-field))))

(defun org-supertag-field-edit-mode-remove-field ()
  "Queue the removal of the field at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    ;; 1. 首先尝试匹配已经标记为删除的行
    ;; 2. 然后尝试匹配普通字段行
    ;; 3. 使用更宽松的空白匹配
    (when (or (looking-at "^.*\\[TO BE REMOVED\\].*\\([^ \n[]+\\).*\\[\\([^]]+\\)\\]")
              (looking-at "^[[:blank:]]*\\([^ \n[]+\\)[[:blank:]]*\\[\\([^]]+\\)\\]"))
      (let* ((field-name (match-string-no-properties 1))
             (tag-id (save-excursion
                      (re-search-backward "^Tag: \\([^ \n]+\\)" nil t)
                      (match-string-no-properties 1))))
        (when (and tag-id field-name)
          ;; 检查字段是否已标记为删除
          (unless (save-excursion
                   (beginning-of-line)
                   (looking-at ".*\\[TO BE REMOVED\\]"))
            (when (yes-or-no-p (format "Mark field '%s' for removal? " field-name))
              (org-supertag-tag--queue-remove-field tag-id field-name)
              (let ((inhibit-read-only t))
                (beginning-of-line)
                ;; 保存当前行的内容
                (let ((line-content (buffer-substring (line-beginning-position)
                                                    (line-end-position))))
                  ;; 删除当前行
                  (delete-region (line-beginning-position)
                               (line-end-position))
                  ;; 插入带有删除标记的行
                  (insert "  [TO BE REMOVED] " line-content))
                ;; 移动到下一个可能的字段
                (forward-line)
                (while (and (not (eobp))
                           (not (looking-at "[[:blank:]]*\\([^ \n[]+\\)[[:blank:]]*\\["))
                           (not (looking-at ".*\\[TO BE REMOVED\\].*\\["))
                           (not (looking-at "^Tag:")))
                  (forward-line))))))))))


(provide 'org-supertag-field-operation)
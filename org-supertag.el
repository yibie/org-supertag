 ;;; org-supertag.el --- Supertag system for Org mode -*- lexical-binding: t; -*-

;;; Commentary:
;; 为 Org mode 提供超级标签系统
;; 支持标签继承、组合、引用和互斥关系
;; 提供字段系统和节点管理

;;; Code:

;; 基础依赖
(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-id)

;; 核心系统
(require 'org-supertag-base)
(require 'org-supertag-db)
(require 'org-supertag-api)
(require 'org-supertag-types)

;; 功能模块
(require 'org-supertag-field)
(require 'org-supertag-tag)
(require 'org-supertag-node)

;; 扩展功能
(require 'org-supertag-perf)
(require 'org-supertag-sync)


;;; 自定义选项

(defgroup org-supertag nil
  "Org Supertag customization group."
  :group 'org)

(defcustom org-supertag-auto-sync t
  "是否自动同步节点变更."
  :type 'boolean
  :group 'org-supertag)

(defcustom org-supertag-data-directory (expand-file-name "org-supertag" user-emacs-directory)
  "数据存储目录."
  :type 'directory
  :group 'org-supertag)

(defcustom org-supertag-templates-file
  (expand-file-name "templates.el" org-supertag-data-directory)
  "模板数据文件路径."
  :type 'file
  :group 'org-supertag)

(defcustom org-supertag-field-types
  '(string property list date choice number enum time datetime
    email url reference options integer duration checkbox rating
    progress file directory color person telephone org-link password)
  "支持的字段类型列表."
  :type '(repeat symbol)
  :group 'org-supertag)


(defcustom org-supertag-tag-presets
  '(("task" . (
      (:name "title" :type string :required t)
      (:name "status" :type options :options ("TODO" "DONE"))
      (:name "priority" :type options :options ("A" "B" "C"))
    ))
    ("note" . (
      (:name "title" :type string :required t)
      (:name "tags" :type list)
    )))
  "预定义的标签及其字段定义."
  :type '(alist :key-type string :value-type sexp)
  :group 'org-supertag)

;;; 内部变量

(defvar org-supertag--initialized nil
  "标记系统是否已初始化.")

(defun org-supertag-init ()
  "初始化超级标签系统."
  (interactive)
  (unless org-supertag--initialized
    ;; 1. 确保数据目录存在
    (org-supertag-ensure-data-directory)
    
    ;; 2. 初始化数据库核心表
    (setq org-supertag-db--entities (ht-create)
          org-supertag-db--relations (ht-create)
          org-supertag-db--field-values (ht-create))
    
    ;; 3. 加载已有数据
    (condition-case err
        (when (file-exists-p org-supertag-db-file)
          (org-supertag-db-load)
          (message "Database loaded successfully"))
      (error
       (message "Error loading database: %S" err)))
    
    ;; 4. 设置钩子
    (when org-supertag-auto-sync
      (add-hook 'org-after-todo-state-change-hook 
               #'org-supertag-sync-node-at-point)
      (add-hook 'org-after-tags-change-hook 
               #'org-supertag-sync-node-at-point)
      (add-hook 'org-after-promote-entry-hook 
               #'org-supertag-sync-node-at-point)
      (add-hook 'org-after-demote-entry-hook 
               #'org-supertag-sync-node-at-point))
    
    ;; 5. 验证数据库状态
    (if (org-supertag-db-ready-p)
        (progn
          (setq org-supertag--initialized t)
          (message "Org Supertag system initialized successfully"))
      (error "Failed to initialize database"))))

(defun org-supertag-save ()
  "保存超级标签系统数据."
  (interactive)
  (org-supertag-with-timer "save"
    (when org-supertag--initialized
      (let ((db-file (expand-file-name "db.el" org-supertag-data-directory)))
        (org-supertag-db-save db-file)
        (message "Org Supertag data saved to %s" db-file)))))

(defun org-supertag-shutdown ()
  "关闭超级标签系统."
  (interactive)
  (when org-supertag--initialized
    ;; 保存数据
    (org-supertag-save)
    
    ;; 移除钩子
    (remove-hook 'org-after-todo-state-change-hook #'org-supertag-sync-node-at-point)
    (remove-hook 'org-after-tags-change-hook #'org-supertag-sync-node-at-point)
    (remove-hook 'org-after-promote-entry-hook #'org-supertag-sync-node-at-point)
    (remove-hook 'org-after-demote-entry-hook #'org-supertag-sync-node-at-point)
    
    ;; 重置状态
    (setq org-supertag--initialized nil)
    (message "Org Supertag system shutdown.")))

;;;###autoload
(define-minor-mode org-supertag-mode
  "Toggle Org Supertag mode.
启用对 supertag 链接的支持."
  :lighter " SuperTag"
  :group 'org-supertag
  (if org-supertag-mode
      (add-hook 'org-open-at-point-functions #'org-supertag-try-visit-link nil t)
    (remove-hook 'org-open-at-point-functions #'org-supertag-try-visit-link t)))

;;------------------------------------------------------------------------------ 
;; Interactive Commands
;;------------------------------------------------------------------------------ 

;;;###autoload
(defun org-supertag-add-tag-at-point ()
  "为当前位置添加标签."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "必须在标题处"))
  
  (message "Debug - 开始添加标签")
  (let* ((all-choices
          (append
           ;; 预定义标签
           (mapcar (lambda (preset)
                    (cons (format "%s [预设]" (car preset))
                          (list :preset (car preset))))
                  org-supertag-tag-presets)
           ;; 已存在的标签
           (mapcar (lambda (tag)
                    (cons (format "%s [已存在]" tag)
                          (list :existing tag)))
                  (org-supertag-find-entities :tag))
           ;; 新建标签选项
           '(("+ 新建标签" . (:new)))))
         (_ (message "Debug - 可用选项: %S" all-choices))
         (choice (completing-read "选择标签: "
                                (mapcar #'car all-choices)
                                nil t))
         (_ (message "Debug - 选择了: %s" choice))
         (tag-info (cdr (assoc choice all-choices)))
         (_ (message "Debug - 标签信息: %S" tag-info)))
    
      (pcase (car tag-info)
      ;; 使用预设标签
        (:preset
          (let* ((tag-name (cadr tag-info))
              (fields (alist-get tag-name org-supertag-tag-presets
                              nil nil #'string=)))
        (message "Debug - 使用预设标签: %s" tag-name)
        (org-supertag-tag-create tag-name :fields fields)
        (org-supertag--apply-tag tag-name)))
      
      ;; 使用已存在的标签
      (:existing
      (let ((tag-name (cadr tag-info)))
        (message "Debug - 使用已存在标签: %s" tag-name)
        (org-supertag--apply-tag tag-name)))
      
      ;; 创建新标签
      (:new
      (let* ((tag-name (read-string "标签名称: "))
              (fields-and-values (org-supertag--field-interactive-edit))
              (fields (car fields-and-values))
              (values (cdr fields-and-values)))
        (message "Debug - 创建新标签: %s" tag-name)
        (message "Debug - 字段定义: %S" fields)
        (message "Debug - 字段值: %S" values)
        (org-supertag-tag-create tag-name :fields fields)
        (org-supertag--apply-tag tag-name values))))))

;;;###autoload
(defun org-supertag-remove-tag-at-point ()
  "从当前位置移除标签."
  (interactive)
  (message "Debug - 开始移除标签...")
  (unless (org-at-heading-p)
    (user-error "必须在标题处"))
  (let* ((element (org-element-at-point))
         (node-id (org-id-get))
         (_ (message "Debug - 当前节点 ID: %s" node-id)))
    ;; 确保有节点 ID
    (unless node-id
      (user-error "当前标题不是节点（没有 ID）"))
    ;; 获取当前节点的标签
    (let* ((current-tags (org-supertag-get-node-tags node-id))
           (_ (message "Debug - 当前标签: %S" current-tags))
           (selected-tags (completing-read-multiple
                         "选择要移除的标签: "
                         current-tags)))
      (when selected-tags
        (message "Debug - 要移除的标签: %S" selected-tags)
        ;; 更新数据层
        (message "Debug - 更新数据层...")
        (dolist (tag selected-tags)
          (message "Debug - 移除标签: %s" tag)
          (org-supertag-tag-remove-from-node node-id tag))
        ;; 更新实例层
        (message "Debug - 更新实例层...")
        (let* ((title (org-element-property :raw-value element))
               (_ (message "Debug - 当前标题: %s" title))
               ;; 移除标签
               (tag-pattern (regexp-opt (mapcar (lambda (tag) (concat "#" tag))
                                              selected-tags)))
               (new-title (replace-regexp-in-string 
                          (concat "\\s-*" tag-pattern "\\s-*") 
                          " " 
                          title))
               (new-title (string-trim new-title)))
          (message "Debug - 新标题: %s" new-title)
          (org-edit-headline new-title)
          ;; 移除相关的属性
          (message "Debug - 处理标签属性...")
          (dolist (tag selected-tags)
            (when-let* ((tag-entity (org-supertag-get-entity tag))
                        (fields (plist-get tag-entity :fields)))
              (message "Debug - 标签 %s 的字段定义: %S" tag fields)
              (dolist (field fields)
                (let ((field-name (plist-get field :name)))
                  (message "Debug - 移除字段: %s" field-name)
                  (org-entry-delete nil field-name))))
          (message "Debug - 标签移除完成")))))))

;;;###autoload
(defun org-supertag-add-field-at-point ()
  "为当前节点添加一字段."
  (interactive)
  ;; 确保在节点上
  (let ((node-id (org-id-get)))
    (unless node-id
      (user-error "当前标题不是节点（没有 ID）"))
    
    ;; 获取当前节点关联的所有标签的字段
    (let* ((tags (org-supertag-get-node-tags node-id))
           (available-fields '()))
      
      ;; 收集所有标签的字段
      (dolist (tag tags)
        (when-let ((tag-fields (org-supertag-get-tag-fields tag)))
          (dolist (field tag-fields)
            (push (plist-get field :name) available-fields))))
      
      (setq available-fields (delete-dups available-fields))
      (message "Debug - 可用字段: %S" available-fields)
      
      (if (null available-fields)
          (user-error "没有可用的字段，请先为标签定义字段")
        (let ((selected-field (completing-read "选择要添加的字段: " available-fields)))
          (when selected-field
            (message "Debug - 添加字段: %s" selected-field)
            (org-entry-put nil selected-field "")
            (message "Debug - 字段添加完成")))))))

;;;###autoload
(defun org-supertag-remove-field-at-point ()
  "从当前节点移除字段."
  (interactive)
  (message "Debug - 开始移除字段...")
  (unless (org-at-heading-p)
    (user-error "必须在标题处"))
  
  (let* ((node-id (org-id-get)))
    ;; 确保有节点 ID
    (unless node-id
      (user-error "当前标题不是节点（没有 ID）"))
    
    ;; 获取当前节点的所有字段
    (let* ((current-fields (org-entry-properties nil))
           (field-names (mapcar #'car current-fields))
           (_ (message "Debug - 当前字段: %S" field-names))
           (selected-field (completing-read "选择要移除的字段: " field-names)))
      
      (when selected-field
        (message "Debug - 移除字段: %s" selected-field)
        ;; 移除字段属性
        (org-entry-delete nil selected-field)
        (message "Debug - 字段移除完成")))))

;;;###autoload
(defun org-supertag-define-tag-fields ()
  "为标签定义字段模板."
  (interactive)
  (let* ((tag-name (completing-read "选择标签: " 
                                  (mapcar #'car (org-supertag-find-entities :tag))
                                  nil t))
         (fields nil)
         (template-id (org-supertag-get-linked tag-name :tag-template)))
    
    (while (y-or-n-p "添加字段")
      (let* ((field-name (read-string "字段名称: "))
             (field-type (completing-read 
                         "字段类型: "
                         (mapcar #'symbol-name org-supertag-field-types)
                         nil t))
             (field-props
              `(:type ,(intern (concat ":" field-type))
                :name ,field-name)))
        
        ;; 根据字段类型添加特殊属性
        (pcase (intern field-type)
          ('enum
           (let ((values
                  (split-string
                   (read-string "输入枚举值 (用逗号分隔): ")
                   "," t "[ \t\n]+")))
             (when values
               (setq field-props 
                     (plist-put field-props :values 
                               (mapcar #'intern values))))))
          
          ('property
           (let ((org-name (read-string "Org 属性名称: " field-name)))
             (setq field-props 
                   (plist-put field-props :org-name org-name))))
          
          ('drawer
           (let ((drawer-name (read-string "抽屉名称: " (upcase field-name))))
             (setq field-props 
                   (plist-put field-props :org-name drawer-name)))))
        
        ;; 添加验证器（如果需要）
        (when (y-or-n-p "添加自定义验证？")
          (let ((validator-form
                 (read--expression "输入验证函数 (接受一个参数): ")))
            (setq field-props
                  (plist-put field-props :validate 
                            (eval `(lambda (value) ,validator-form))))))
        
        ;; 创建字段定义
        (let ((field-def (org-supertag-field-create field-name field-props)))
          (push field-def fields))))
    
    (when fields
      ;; 更新标签的字段定义
      (org-supertag-update-entity 
       tag-name 
       (list :type :tag
             :fields (nreverse fields)))

      ;; 如果标签没有关联的模板，则创建新模板
      (unless template-id
        (let* ((template (make-org-supertag-template
                         :id (org-supertag--generate-id)
                         :tag-name tag-name
                         :display-name tag-name
                         :fields (vconcat fields))))
          ;; 保存模板并建立关联
          (org-supertag-create-template template)
          (org-supertag-link :tag-template tag-name (org-supertag-template-id template))
          (org-supertag-link :template-tag (org-supertag-template-id template) tag-name)
          (message "已为标签 #%s 创建模板" tag-name)))
      
      (message "标签 #%s 的字段已更新" tag-name))))

;;;###autoload
(defun org-supertag-edit-tag-fields ()
  "Edit tag field definitions."
  (interactive)
  (let* ((tag-name (completing-read "Select tag: " 
                                  (org-supertag-get-all-tags)
                                  nil t))
         (current-fields (org-supertag-get-tag-fields tag-name)))
    
    (when current-fields
      (let* ((field-to-edit (completing-read 
                            "Select field to edit: "
                            (mapcar (lambda (f) (plist-get f :name)) 
                                    current-fields)
                            nil t))
             (field-def (org-supertag-get-field-definition tag-name field-to-edit))
             (action (completing-read "Select action: " '("Edit Type" "Edit Value") nil t))
             (new-props (copy-sequence field-def)))
        
        (cond
         ((string= action "Edit Type")
          (let* ((type-choices (org-supertag-get-field-types))
                 (type-choice (completing-read "Field type: "
                                               (mapcar #'car type-choices)
                                               nil t
                                               (car (rassoc (plist-get field-def :type)
                                                            type-choices))))
                 (type-sym (cdr (assoc type-choice type-choices))))
            (setq new-props (plist-put new-props :type type-sym))))
         
         ((string= action "Edit Value")
          (let* ((type (plist-get field-def :type))
                 (reader (plist-get (org-supertag-get-field-type type) :reader))
                 (new-value (funcall reader (format "Enter new value for %s: " field-to-edit))))
            ;; 更新数据库的字段定义
            (setq new-props (plist-put new-props :value new-value))
            ;; 更新所有使用该签的节点的属性值
            (dolist (node-id (org-supertag-get-nodes-with-tag tag-name))
              (message "Debug - Updating node %s with new value for %s: %s" 
                      node-id field-to-edit new-value)
              (when-let ((marker (org-id-find node-id t)))
                (message "Debug - Found node at marker: %s" marker)
                (save-excursion
                  (with-current-buffer (marker-buffer marker)
                    (goto-char marker)
                    (org-entry-put nil field-to-edit new-value)
                    (message "Debug - Updated property at point: %s" (point)))))))))
        
        ;; Update field definition using API
        (when (org-supertag-update-field tag-name field-to-edit new-props)
          (message "Field %s updated" field-to-edit))))))

;;------------------------------------------------------------------------------ 
;; Reference
;;------------------------------------------------------------------------------ 

(defun org-supertag-add-reference ()
  "交互式插入标签或节点引用，支持自动补全."
  (interactive)
  ;; 确保系统已初始化
  (unless org-supertag--initialized
    (message "Initializing org-supertag system...")
    (org-supertag-init))
  (let* ((type (completing-read "引用类型: " '("标签" "节点")))
         ;; 获取可选项
         (choices
          (if (string= type "标签")
              ;; 标签选项
              (org-supertag-get-all-tags)
            ;; 节点选项 - 使用标题作为选项
            (mapcar (lambda (node-id)
                     (let ((full-title (org-with-point-at (org-id-find node-id t)
                                       (org-get-heading t t t t))))
                       ;; 保存完整标题和 ID 的关联
                       (cons (cons (truncate-string-to-width 
                                  (or full-title "Untitled") 
                                  120 nil nil "...")
                             full-title)
                             node-id)))
                   (org-supertag-get-all-node-ids))))
         ;; 选择实体
         (choice (completing-read 
                 (if (string= type "标签") 
                     "选择标签: " 
                   "选择节点: ")
                 (if (string= type "标签")
                     choices
                   (mapcar (lambda (c) (car (car c))) choices))))
         ;; 获取实际的实体ID和完整标题
         (entity-id (if (string= type "标签")
                       choice
                     (cdr (assoc (cons choice (cdr (assoc choice 
                       (mapcar (lambda (c) 
                         (cons (car (car c)) (cdr (car c))))
                       choices))))
                       choices))))
         ;; 获取字段列表
         (fields (org-supertag-get-entity-fields-to-reference entity-id))
         (field-input (completing-read "字段 (直接回车跳过): " fields))
         (field (unless (string-empty-p field-input) field-input))
         ;; 获取可能的字段值
         (values (when field 
                  (org-supertag-get-field-values entity-id field)))
         (value-input (when field
                       (completing-read "值 (直接回车跳过): " values)))
         (value (unless (string-empty-p value-input) value-input))
         ;; 构建链接
         (link-target
          (if (string= type "标签")
              (format "*#%s%s%s" 
                      entity-id
                      (if field (concat "." field) "")
                      (if value (concat "=" value) ""))
            (format "id:%s%s%s"
                    entity-id
                    (if field (concat "." field) "")
                    (if value (concat "=" value) ""))))
         ;; 获取描述文本
         (description
          (if (string= type "标签")
              entity-id  ; 标签使用标签名作为默认描述
            ;; 使用完整标题作为描述
            (cdr (assoc choice 
                       (mapcar (lambda (c) 
                         (cons (car (car c)) (cdr (car c))))
                       choices))))))
    
    ;; 插入链接
    (insert (format "[[%s][%s]]" link-target description))))

(defun org-supertag-try-visit-link ()
  "尝试访问 supertag 链接.
返回 t 如果成功处理了链接，nil 如果不是 supertag 链接."
  (when (org-in-regexp org-link-any-re)
    (let* ((context (org-element-context))
           (type (org-element-property :type context))
           (path (org-element-property :path context)))
      ;; 处理模糊链接
      (when (and (equal type "fuzzy")
                 (string-match "^\\*\\(#\\|@\\)" path))
        (org-supertag-follow-link (substring path 1) nil)
        t))))

(defun org-supertag-follow-link (path _arg)
  "处理链接的点击事件.
PATH 是链接路径
_ARG 是可选参数"
  (when (string-match "\\(?:#\\|@\\)\\([^.=]+\\)\\(?:\\.\\([^=]+\\)\\(?:=\\([^]\n]+\\)\\)?\\)?" path)
    (let* ((entity-id (match-string 1 path))
           (field (match-string 2 path))
           (value (match-string 3 path)))
      (cond
       ;; 标签引用
       ((string-prefix-p "#" path)
        (org-supertag-visit-tag entity-id field value))
       ;; 节点引用
       ((string-prefix-p "@" path)
        (org-supertag-visit-node entity-id field value))
       (t nil)))))

(defun org-supertag-visit-tag (tag-name &optional field value)
  "访问标签引用."
  (cond
   ;; 有字段值
   ((and field value)
    (org-supertag-show-tag-field-value tag-name field value))
   ;; 只有字段
   (field
    (org-supertag-show-tag-field tag-name field))
   ;; 只有标签
   (t
    (org-supertag-show-tag tag-name))))

(defun org-supertag-visit-node (node-id &optional field value)
  "访问节点引用."
  (cond
   ;; 有字段值
   ((and field value)
    (org-supertag-show-node-field-value node-id field value))
   ;; 只有字段
   (field
    (org-supertag-show-node-field node-id field))
   ;; 只有节点
   (t
    (org-id-goto node-id))))

;; 注册链接类型
(org-link-set-parameters
 "*"  ; 使用 * 作为链接类型
 :follow #'org-supertag-follow-link
 :face 'org-link)



(provide 'org-supertag)

;;; org-supertag.el ends here 
 

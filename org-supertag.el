 ;;; org-supertag.el --- Supertag system for Org mode -*- lexical-binding: t; -*-

;;; Commentary:
;; 为 Org mode 提供超级标签系统
;; 支持标签继承、组合、引用和互斥关系
;; 提供字段系统和节点管理

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-id)

(require 'org-supertag-base)
(require 'org-supertag-db)
(require 'org-supertag-api)
(require 'org-supertag-types)
(require 'org-supertag-tag)
(require 'org-supertag-perf)
(require 'org-supertag-node)
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

;;; 核数

(defun org-supertag-init ()
  "初始化超级标签系统."
  (interactive)
  (org-supertag-with-timer "init"
    (unless org-supertag--initialized
      ;; 确保数据目录存在
      (unless (file-exists-p org-supertag-data-directory)
        (make-directory org-supertag-data-directory t))
      
      ;; 载数据
      (let ((db-file (expand-file-name "db.el" org-supertag-data-directory)))
        (when (file-exists-p db-file)
          (org-supertag-db-load db-file)))
      
      ;; 设置钩子
      (when org-supertag-auto-sync
        (add-hook 'org-after-todo-state-change-hook #'org-supertag-sync-node-at-point)
        (add-hook 'org-after-tags-change-hook #'org-supertag-sync-node-at-point)
        (add-hook 'org-after-promote-entry-hook #'org-supertag-sync-node-at-point)
        (add-hook 'org-after-demote-entry-hook #'org-supertag-sync-node-at-point))
      
      ;; 标记为已初始化
      (setq org-supertag--initialized t)
      (message "Org Supertag system initialized."))))

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
  "Toggle Org Supertag mode."
  :lighter " OrgST"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c t t") #'org-supertag-add-tag-at-point)
            (define-key map (kbd "C-c t r") #'org-supertag-remove-tag-at-point)
            (define-key map (kbd "C-c t f") #'org-supertag-define-tag-fields)
            (define-key map (kbd "C-c t e") #'org-supertag-edit-tag-fields)
            (define-key map (kbd "C-c t s") #'org-supertag-sync-node-at-point)
            (define-key map (kbd "C-c t n") #'org-supertag-create-tag-template)
            (define-key map (kbd "C-c t e") #'org-supertag-edit-tag-template)
            (define-key map (kbd "C-c t a") #'org-supertag-add-tag)
            map)
  (if org-supertag-mode
      (progn
        ;; 确保必要的模块已加载
        (require 'org-supertag-node)
        (require 'org-supertag-sync)

        
        (org-supertag-init)
        ;; 添加节点监控钩子
        (add-hook 'org-after-refile-insert-hook #'org-supertag-node--after-change-hook nil t)
        (add-hook 'before-change-functions #'org-supertag-node--before-change-hook nil t)
        (add-hook 'after-change-functions #'org-supertag-node--after-change-hook nil t)
        (add-hook 'before-save-hook #'org-supertag-save nil t))
    (org-supertag-shutdown)
    ;; 移除节点监控钩子
    (remove-hook 'org-after-refile-insert-hook #'org-supertag-node--after-change-hook t)
    (remove-hook 'before-change-functions #'org-supertag-node--before-change-hook t)
    (remove-hook 'after-change-functions #'org-supertag-node--after-change-hook t)
    (remove-hook 'before-save-hook #'org-supertag-save t)))

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
           ;; 预设标签
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
              (fields (org-supertag--field-interactive-edit)))
         (message "Debug - 创建新标签: %s" tag-name)
         (message "Debug - 字段定义: %S" fields)
         (org-supertag-tag-create tag-name :fields fields)
         (org-supertag--apply-tag tag-name))))))




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
  "为当前节点添加一个字段."
  (interactive)
  ;; 确保在节点上
  (let ((node-id (org-id-get)))
    (unless node-id
      (user-error "当前标题不是节点（没有 ID）"))
    
    (let* ((available-fields (org-supertag-get-all-fields))
           (_ (message "Debug - 可用字段: %S" available-fields)))
      (if (null available-fields)
          (user-error "没有可用的字段，请先创建模板")
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
            ;; 更新数据库中的字段定义
            (setq new-props (plist-put new-props :value new-value))
            ;; 更新所有使用该标签的节点的属性值
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

(defun org-supertag--get-template-choices ()
  "获取所有可用的标签模板选项."
  (let* ((preset-choices
          (mapcar (lambda (template)
                   (cons (format "%s [预设]" (car template))
                         (list :preset (car template))))
                 org-supertag-preset-templates))
         (custom-templates
          (mapcar (lambda (template)
                   (cons (format "%s [自定义]" 
                               (org-supertag-template-tag-name template))
                         (list :custom 
                               (org-supertag-template-id template))))
                 (org-supertag-find-templates-by-type))))
    (append preset-choices
            custom-templates
            '(("+ 新建标签" . (:new))))))

(defun org-supertag--apply-template (tag-name fields)
  "应用模板到当前节点.
TAG-NAME 是标签名称
FIELDS 是字段列表，可以是 plist 或 alist"
  (let ((node-id (org-id-get-create)))
    ;; 添加标签关系
    (org-supertag-tag-add-to-node node-id tag-name)
    
    ;; 更新标题，添加标签
    (let* ((element (org-element-at-point))
           (title (org-element--property :raw-value element nil nil))
           (new-title (concat title " #" tag-name)))
      (org-edit-headline new-title))
    
    ;; 应用字段
    (org-supertag--apply-tag-fields tag-name)))
    



(provide 'org-supertag)
;;; org-supertag.el ends here 
 

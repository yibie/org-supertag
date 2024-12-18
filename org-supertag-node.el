;;; org-supertag-node.el --- Node management for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供节点的核心功能，包括：
;; - 节点结构定义
;; - 节点操作（创建、移动、复制、删除）
;; - 节点监控
;; - 节点引用关系
;; 节点实现方法：别忘了 org-supertag-node--handle-delete 和 org-supertag-node--handle-create
;; 和 org-supertag-node-cache
;; 节点每次更新，都要更新两层：1. 数据层，2. 实体层


(require 'org)
(require 'org-element)
(require 'org-id)
(require 'org-supertag-db)
(require 'org-supertag-query)

;;------------------------------------------------------------------------------
;; Database Operations - 数据库操作函数
;;------------------------------------------------------------------------------    

(defun org-supertag-node-db-create (id props)
  "在数据库中创建新节点.
ID 是节点唯一标识
PROPS 是节点属性

说明：
1. 验证必要属性
2. 添加系统属性
3. 创建节点记录
4. 触发创建事件"
  ;; 验证必要属性
  (unless (plist-get props :title)
    (error "节点必须有标题"))
  
  ;; 构建完整的节点属性
  (let ((node-props (append
                    (list :id id
                          :type :node
                          :created-at (current-time))
                    props)))
    ;; 存储节点
    (org-supertag-db-add id node-props)
    ;; 触发事件
    (run-hook-with-args 'org-supertag-node-created-hook id node-props)
    ;; 返回节点属性
    node-props))

(defun org-supertag-node-db-update (id props)
  "更新数据库中的节点.
ID 是节点唯一标识
PROPS 是要更新的属性"
  (when-let ((node (org-supertag-db-get id)))
    (let ((new-props nil))
      ;; 1. 保留原有的创建时间
      (setq new-props (list :created-at (plist-get node :created-at)))
      ;; 2. 添加修改时间
      (setq new-props (plist-put new-props :modified-at (current-time)))
      ;; 3. 清理输入属性(移除系统属性)
      (let ((clean-props (copy-sequence props)))
        (while (or (plist-member clean-props :created-at)
                  (plist-member clean-props :modified-at))
          (setq clean-props (org-plist-delete 
                           (org-plist-delete clean-props :created-at)
                           :modified-at)))
        ;; 4. 合并属性
        (setq new-props (append new-props clean-props)))
      ;; 5. 更新数据库
      (org-supertag-db-add id new-props)
      ;; 6. 触发事件
      (org-supertag-emit 'node:updated id new-props)
      ;; 7. 返回更新后的属性
      new-props)))

(defun org-supertag-node-db-exists-p (id)
  "检查节点是否存在于数据库中.
ID 是节点唯一标识

返回值：
- t   节点存在且类型为 :node
- nil 节点不存在或类型不是 :node"
  (when-let ((node (org-supertag-db-get id)))
    (eq (plist-get node :type) :node)))

(defun org-supertag-node-db-get-tags (id)
  "获取节点的所有标签.
ID 是节点唯一标识

返回值：
标签ID列表，如果节点不存在返回 nil"
  (when (org-supertag-node-db-exists-p id)
    (mapcar #'cdr  ; 获取标签ID
            (org-supertag-db-get-links-by-type :node-tag
                                              :from id))))

(defun org-supertag-node-db-add-tag (node-id tag-id)
  "为节点添加标签.
NODE-ID 是节点ID
TAG-ID 是标签ID"
  (when (and (org-supertag-node-db-exists-p node-id)
             (org-supertag-db-exists-p tag-id))
    ;; 添加关系，注意参数顺序
    (org-supertag-db-link :node-tag 
                         node-id 
                         tag-id 
                         ;; 添加关系属性
                         `(:created-at ,(current-time)))
    ;; 触发事件
    (run-hook-with-args 'org-supertag-node-tag-added-hook
                        node-id tag-id)))

(defun org-supertag-node-db--get-candidates ()
  "获取所有可引用节点的候选列表.

返回值：
((title . id) ...) 形式的关联列表，其中：
- title 是节点标题
- id 是节点唯一标识

说明：
1. 遍历数据库中的所有节点
2. 过滤出类型为 :node 的实体
3. 构建标题和ID的关联"
  (let (candidates)
    (maphash (lambda (id node)
               (when (eq (plist-get node :type) :node)
                 (push (cons (plist-get node :title) id)
                       candidates)))
             org-supertag-db--object)
    ;; 按标题排序
    (sort candidates
          (lambda (a b)
            (string< (car a) (car b))))))

(defun org-supertag-node-get-tags (node-id)
  "获取节点关联的所有标签ID列表.
NODE-ID 是节点的ID"
  (let ((result nil))
    (maphash
     (lambda (link-id props)
       (when (and (string-prefix-p ":node-tag:" link-id)
                  (equal (plist-get props :from) node-id))
         (push (plist-get props :to) result)))
     org-supertag-db--link)
    (nreverse result)))
;;------------------------------------------------------------------------------
;; Node Field Relations
;;------------------------------------------------------------------------------

(defun org-supertag-node-db-set-field (node-id field-id value)
  "设置节点的字段���.
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
;; Operation Funtions
;;------------------------------------------------------------------------------    

(defun org-supertag-node-sync-at-point ()
  "同步当前位置的节点数据."
  (let ((existing-id (org-id-get)))
    (if existing-id
        ;; 更新已存在的节点
        (when (org-supertag-node-db-exists-p existing-id)
          ;; 直接使用 org-supertag-db-add-node-at-point
          (org-supertag-db-add-node-at-point)
          existing-id)
      ;; 创建新节点
      (let ((new-id (org-id-get-create)))
        (org-supertag-db-add-node-at-point)
        new-id))))

(defun org-supertag-node-get-props ()
  "获取当前节点的属性."
  (org-supertag-db--parse-node-at-point))

(defun org-supertag-node-sync-display ()
  "同步节点的显示状态.

处理流程：
1. 确保节点有ID属性
2. 更新属性抽屉中的字段
3. 更新标签显示
4. 更新任务状态显示

说明：
- 保持显示状态与数据库一致
- 不触发数据库更新
- 仅更新显示相关的属性"
  (when-let ((node-id (org-id-get)))
    (when-let ((node (org-supertag-db-get node-id)))
      ;; 更新属性抽屉
      (org-set-property "ID" node-id)
      ;; 更新其他显示状态
      (let ((tags (plist-get node :tags))
            (todo (plist-get node :todo)))
        (when tags
          (org-set-tags tags))
        (when todo
          (org-todo todo))))))




;;------------------------------------------------------------------------------
;; Node Commands 
;;------------------------------------------------------------------------------    

(defun org-supertag-node-create ()
  "在当前位置创建一个新的 supertag 节点.

使用场景：
1. 用户想要将普通的 org 标题转换为 supertag 节点
2. 确保标题没有关联的节点ID

前置条件：
1. 光标必须在标题处
2. 标题不能已经有节点ID"
  (interactive)
  (unless (org-at-heading-p)
    (user-error "光标必须在标题处"))
  (when (org-id-get)
    (user-error "该标题已经有一个节点"))
  (org-supertag-node-sync-at-point))

(defun org-supertag--create-node (node-id)
  "根据ID创建一个新的 supertag 节点.
NODE-ID: 节点ID"
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
  "获取当前位置节点的属性.
返回值：
- 节点属性列表
- nil 如果不是有效节点"
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
  "更新当前位置的节点.

使用场景：
1. 用户修改了节点内容后手动更新
2. 需要强制同步节点数据

前置条件：
1. 光标必须在标题处
2. 标题必须有关联的节点ID

返回值：
- 成功时返回节点ID
- 失败时抛出错误"
  (interactive)
  (unless (org-at-heading-p)
    (user-error "光标必须在标题处"))
  
  (let ((id (org-id-get)))
    (unless id
      (user-error "该标题没有关联的节点"))
    
    ;; 获取节点属性
    (let ((props (org-supertag-node-get-props-at-point)))
      (unless props
        (error "无法获取节点属性"))
      
      ;; 更新数据库
      (condition-case err
          (progn
            (org-supertag-node-db-update id props)
            (message "节点更新成功: %s" id)
            id)
        (error
         (message "节点更新失败: %s" (error-message-string err))
         (signal (car err) (cdr err)))))))

(defun org-supertag-node-db-update (id props)
  "更新或创建数据库中的节点.
ID 是节点ID
PROPS 是节点属性

返回值：
- 成功时返回更新后的属性
- 失败时抛出错误"
  (let* ((existing-node (org-supertag-db-get id))
         (is-new (not existing-node))
         (new-props (org-supertag-db--normalize-props
                    (append
                     ;; 基本属性
                     (list :type :node
                           :id id)
                     ;; 时间戳
                     (if is-new
                         (list :created-at (current-time)
                               :modified-at (current-time))
                       (list :created-at (plist-get existing-node :created-at)
                             :modified-at (current-time)))
                     ;; 用户提供的属性
                     props))))
    
    ;; 验证必要属性
    (unless (plist-get new-props :title)
      (error "Missing required property: title"))
    (unless (plist-get new-props :file-path)
      (error "Missing required property: file-path"))
    ;; 更新数据库
    (org-supertag-db-add id new-props)
    ;; 触发相应的钩子
    (run-hook-with-args 
     (if is-new
         'org-supertag-node-created-hook
       'org-supertag-node-updated-hook)
     id new-props)
    ;; 记录操作
    (message "%s node: %s" 
             (if is-new "Created new" "Updated")
             id)
    ;; 返回更新后的属性
    new-props))


(defun org-supertag-node-remove-tag (node-id tag-id)
  "从节点移除标签关联.
NODE-ID 是节点ID
TAG-ID 是标签ID"
  (let ((link-id (format ":node-tag:%s->%s" node-id tag-id)))
    ;; 从数据库中移除关联
    (remhash link-id org-supertag-db--link)
    ;; 清除缓存
    (org-supertag-db--cache-remove 'query (format "node-tags:%s" node-id))
    (org-supertag-db--cache-remove 'query (format "tag-nodes:%s" tag-id))
    ;; 安排保存
    (org-supertag-db-save)))


;;------------------------------------------------------------------------------
;; Node Relations 
;;------------------------------------------------------------------------------    

(defun org-supertag-node--in-node-p ()
  "检查当前位置是否在有效的 org-supertag 节点内.
有效节点需满足：
1. 当前位置能回到标题
2. 标题有 ID 属性
3. ID 在数据库中存在对应记录

返回值：
- (id . title) 如果在有效节点内
- nil 如果不在有效节点内，并提供相应提示"
  (condition-case nil
      (save-excursion
        (org-back-to-heading t)
        (let ((id (org-entry-get nil "ID"))
              (title (org-get-heading t t t t)))
          (cond
           ;; 没有 ID
           ((null id)
            (message "当前节点未创建，请使用 org-supertag-node-create 命令创建")
            nil)
           ;; ID 不在数据库中
           ((not (org-supertag-node-db-exists-p id))
            (message "节点 ID 未在数据库中注册，请使用 org-supertag-node-create 命令创建")
            nil)
           ;; 有效节点
           (t (cons id title)))))
    ;; 不在任何标题下
    (error 
     (message "当前位置不在任何节点内")
     nil)))

(defun org-supertag-node-db-add-reference (from-id to-id)
  "添加节点间的引用关系.
FROM-ID 是引用源节点
TO-ID 是被引用节点

说明：
1. 检查节点存在性
2. 创建引用关联
3. 触发事件"
  (when (and (org-supertag-node-db-exists-p from-id)
             (org-supertag-node-db-exists-p to-id))
    ;; 添加引用关系
    (org-supertag-db-link
     :type :node-ref
     :from from-id
     :to to-id)
    ;; 清除缓存
    (org-supertag-db--cache-remove 'query 
                                  (format "node-refs:%s" from-id))
    ;; 触发事件
    (run-hook-with-args 'org-supertag-node-reference-added-hook
                       from-id to-id)))

;; 3. 关系查询
(defun org-supertag-node-db-get-reference (node-id &optional direction)
  "获取节点的引用关系.
NODE-ID 是节点ID
DIRECTION 是引用方向:
  - 'to   获取该节点引用的其他节点
  - 'from 获取引用该节点的其他节点
  - nil   获取所有相关引用

返回值：
节点ID列表"
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



;; 4. 关系清理
(defun org-supertag-node-db-remove-reference (from-id to-id)
  "移除节点间的引用关系.
FROM-ID 是引用源节点
TO-ID 是被引用节点"
  (when-let* ((from-node (org-supertag-db-get from-id))
              (refs-to (plist-get from-node :refs-to)))
    ;; 使用数据库的引用处理机制
    (org-supertag-db--handle-refs-updated 
     from-id 
     (delete to-id refs-to))))

;;------------------------------------------------------------------------------
;; 节点引用功能
;;------------------------------------------------------------------------------  


(defun org-supertag-node--insert-reference (node-id)
  "在当前位置插入节点引用.
NODE-ID 是被引用节点的ID"
  (let* ((target-node (org-supertag-db-get node-id))
         (target-title (plist-get target-node :title))
         (link-text (format "[[id:%s][%s]]" node-id target-title)))
    ;; 直接在当前位置插入链接
    (insert link-text)))


(defun org-supertag-node-add-reference ()
  "添加节点引用.
在当前位置插入对选定节点的引用，并更新引用关系."
  (interactive)
  (unless (org-supertag-node--in-node-p)
    (user-error "必须在节点内"))
  (when-let* ((candidates (org-supertag-node-db--get-candidates))
              (selected (completing-read
                        "引用节点: "
                        (mapcar #'car candidates)
                        nil t))
              (target-id (cdr (assoc selected candidates))))
    ;; 1. 插入引用
    (org-supertag-node--insert-reference target-id)
    ;; 2. 更新引用关系
    (org-supertag-db--parse-node-all-ref)
    (message "已添加对节点 '%s' 的引用" selected)))



(defun org-supertag-node-remove-reference (node-id)
  "移除对指定节点的引用.
NODE-ID 是要移除的引用节点ID.
使用 org-element 解析和定位链接，确保准确删除."
  (interactive 
   (let* ((refs (org-supertag-node--collect-reference-id-title))
          (selected (completing-read 
                    "移除引用: "
                    (mapcar #'car refs)
                    nil t)))
     (list (cdr (assoc selected refs)))))
  
  (let ((start (org-entry-beginning-position))
        (end (org-entry-end-position))
        (removed 0))
    (save-excursion
      ;; 1. 使用 org-element 解析和删除链接
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
    
    (message "已移除 %d 处引用" removed)))

(defun org-supertag-node--collect-reference-id-title ()
  "收集当前节点中的所有引用信息.
返回格式: ((title . id) ...)"
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
;; Event Hooks - 事件钩子
;;------------------------------------------------------------------------------    


(defvar org-supertag-node-created-hook nil
  "节点创建后的钩子.
参数: (id props)")

(defvar org-supertag-node-updated-hook nil
  "节点更新后的钩子.
参数: (id props)")

(defvar org-supertag-node-deleted-hook nil
  "节点删除后的钩子.
参数: (id)")

(defvar org-supertag-node-tag-added-hook nil
  "节点添加标签后的钩子.
参数: (node-id tag-id)")


(defvar org-supertag-node-field-updated-hook nil
  "节点字段更新后的钩子.
参数: (node-id field-id value)")

(defvar org-supertag-node-reference-added-hook nil
  "节点引用关系添加后的钩子.
参数: (from-id to-id)")

(defvar org-supertag-node-reference-removed-hook nil
  "节点引用关系移除后的钩子.
参数: (from-id to-id)")

(provide 'org-supertag-node)
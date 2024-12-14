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
PROPS 是要更新的属性

说明：
1. 合并现有属性
2. 更新修改时间
3. 保存更新
4. 触发更新事件"
  (when-let ((node (org-supertag-db-get id)))
    ;; 合并属性，保留原有的系统属性
    (let ((new-props (append
                     (list :modified-at (current-time))
                     props
                     (list :id id
                           :type :node
                           :created-at (plist-get node :created-at)))))
      ;; 更新节点
      (org-supertag-db-update-node-at-point id new-props)
      ;; 触发事件
      (run-hook-with-args 'org-supertag-node-updated-hook id new-props)
      ;; 返回更新后的属性
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
TAG-ID 是标签ID

说明：
1. 检查节点和标签是否存在
2. 创建关联关系
3. 触发标签添加事件"
  (when (and (org-supertag-node-db-exists-p node-id)
             (org-supertag-db-exists-p tag-id))
    ;; 添加关系
    (org-supertag-db-link
     :type :node-tag
     :from node-id
     :to tag-id)
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
;; Operation Handlers - 操作处理函数
;;------------------------------------------------------------------------------    

(defun org-supertag-node-sync-at-point ()
  "同步当前位置的节点数据."
  (let ((existing-id (org-id-get)))
    (if existing-id
        ;; 更新已存在的节点
        (when (org-supertag-node-db-exists-p existing-id)
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
;; User Interactive Commands - 用户交互命令
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

(defun org-supertag-node-update ()
  "更新当前位置的节点.

使用场景：
1. 用户修改了节点内容后手动更新
2. 需要强制同步节点数据

前置条件：
1. 光标必须在标题处
2. 标题必须有关联的节点ID"
  (interactive)
  (unless (org-at-heading-p)
    (user-error "光标必须在标题处"))
  (unless (org-id-get)
    (user-error "该标题没有关联的节点"))
  (org-supertag-node-sync-at-point))

(defun org-supertag-node-add-tags ()
  "为当前节点添加标签.

使用场景：
1. 用户想要给节点添加标签
2. 通过补全界面选择已有标签

前置条件：
1. 光标必须在节点标题处
2. 节点必须已存在"
  (interactive)
  (when-let ((node-id (org-id-get)))
    (let* ((available-tags (mapcar #'car (org-supertag-find-object :tag)))
           (selected-tags (completing-read-multiple
                          "Tags: "
                          available-tags)))
      (dolist (tag selected-tags)
        (org-supertag-add-tag node-id tag)))))

(defun org-supertag-node-set-field ()
  "为当前节点设置字段值.

使用场景：
1. 用户想要设置节点的字段值
2. 通过补全界面选择字段并输入值

前置条件：
1. 光标必须在节点标题处
2. 节点必须已存在"
  (interactive)
  (when-let ((node-id (org-id-get)))
    (let* ((available-fields (mapcar #'car (org-supertag-find-object :field)))
           (field (completing-read
                  "Field: "
                  available-fields))
           (value (read-string "Value: ")))
      (org-supertag-set-field node-id field value))))


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
    
    ;; 2. 更新数据库中的引用关系
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

;;------------------------------------------------------------------------------
;; 测试
;;------------------------------------------------------------------------------    

(defun org-supertag-node-test ()
  "测试节点操作.

测试项目：
1. 基础操作：创建、获取、更新节点
2. 引用操作：添加、获取引用
3. 标签操作：添加、获取标签"
  (interactive)
  ;; 1. 测试基础操作
  (let ((node-id (org-supertag-node-sync-at-point)))
    (message "1. 基础操作测试:")
    (message "创建节点: %s" node-id)
    
    (let ((node (org-supertag-db-get node-id)))
      (message "节点数据: %S" node))
    
    (org-supertag-node-update-at-point)
    (message "节点已更新\n")

    ;; 2. 测试引用操作
    (message "2. 引用操作测试:")
    ;; 创建一个临时节点作为引用目标
    (save-excursion
      (org-insert-heading)
      (insert "引用测试节点")
      (let ((ref-id (org-supertag-node-sync-at-point)))
        ;; 添加引用
        (org-supertag-node-db-add-reference node-id ref-id)
        (message "添加引用: %s -> %s" node-id ref-id)
        ;; 获取引用
        (let ((refs (org-supertag-node-db-get-reference node-id 'to)))
          (message "节点引用列表: %S\n" refs))))

    ;; 3. 测试标签操作
    (message "3. 标签操作测试:")
    ;; 添加标签
    (let ((tag-name "test-tag"))
      (org-set-tags (list tag-name))
      (org-supertag-node-update-at-point)
      ;; 获取标签
      (let ((tags (org-supertag-node-db-get-tags node-id)))
        (message "节点标签列表: %S" tags)))))

(defun org-supertag-node-test-id ()
  "测试节点 ID 的获取.
测试场景：
1. 在当前节点获取 ID
2. 在目标节点获取 ID
3. 验证引用关系"
  (interactive)
  (save-excursion
    ;; 1. 获取当前节点信息
    (org-back-to-heading t)
    (let* ((current-id (org-id-get))
           (current-title (org-get-heading t t t t))
           ;; 获取候选节点列表
           (candidates (org-supertag-node-db--get-candidates)))
      
      (message "当前节点: %s (%s)" current-title current-id)
      
      ;; 2. 显示可引用的节点列表
      (message "可引用节点列表:")
      (dolist (candidate candidates)
        (message "  - %s (%s)" (car candidate) (cdr candidate)))
      
      ;; 3. 模拟引用过程
      (let* ((selected-title "test") ; 假设选择了 "test" 节点
             (selected-id (cdr (assoc selected-title candidates))))
        (message "\n准备添加引用:")
        (message "  从: %s (%s)" current-title current-id)
        (message "  到: %s (%s)" selected-title selected-id)
        
        ;; 4. 验证目标节点
        (when selected-id
          (org-with-point-at (org-id-find selected-id t)
            (let ((actual-id (org-id-get))
                  (actual-title (org-get-heading t t t t)))
              (message "\n验证目标节点:")
              (message "  期望 ID: %s" selected-id)
              (message "  实际 ID: %s" actual-id)
              (message "  标题: %s" actual-title))))))))    

(defun org-supertag-node-db-check-state ()
  "检查数据库状态."
  (interactive)
  (message "\n=== 数据库状态检查 ===")
  
  ;; 1. 检查文件
  (message "\n文件状态:")
  (message "  路径: %s" org-supertag-db-file)
  (message "  存在: %s" (file-exists-p org-supertag-db-file))
  (when (file-exists-p org-supertag-db-file)
    (message "  大小: %s bytes" 
             (file-attribute-size (file-attributes org-supertag-db-file))))
  
  ;; 2. 检查内存数据
  (message "\n内存数据:")
  (if (and (boundp 'org-supertag-db--object)
           (boundp 'org-supertag-db--link))
      (progn
        (message "  对象数量: %d" (ht-size org-supertag-db--object))
        (message "  关系数量: %d" (ht-size org-supertag-db--link)))
    (message "  数据库未初始化"))
  
  ;; 3. 检查缓存 - 使用正确的缓存系统函数
  (message "\n缓存状态:")
  (if (boundp 'org-supertag-db--cache)
      (message "  缓存条目: %d" (ht-size org-supertag-db--cache))
    (message "  缓存系统未初始化"))
  
  ;; 4. 检查自动保存
  (message "\n自动保存状态:")
  (message "  定时器活动: %s" 
           (if org-supertag-db--auto-save-timer "是" "否"))
  (message "  保存间隔: %s" 
           (if org-supertag-db-auto-save-interval
               (format "%d秒" org-supertag-db-auto-save-interval)
             "禁用")))
        
(provide 'org-supertag-node)

;;; org-supertag-tests.el --- Tests for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供完整的测试套件

;;; Code:

(require 'ert)
(require 'org-supertag)
(require 'org-supertag-db)
(require 'org-supertag-api)
(require 'org-supertag-query)
(require 'org-supertag-group)
(require 'org-supertag-cache)
(require 'org-supertag-sync)
(require 'org-supertag-ui)

;; 测试辅助函数
(defmacro org-supertag-with-test-env (&rest body)
  "在干净的测试环境中执行测试代码."
  `(let ((org-supertag-db--entities (ht-create))
         (org-supertag-db--relations (ht-create))
         (org-supertag-db--field-values (ht-create)))
     (org-supertag-cache-clear)
     ,@body))

;; 基础 CRUD 测试
(ert-deftest org-supertag-test-basic-crud ()
  "测试基本的 CRUD 操作."
  (org-supertag-with-test-env
   ;; 测试创建
   (should (org-supertag-create-node "node1" '(:title "Test Node")))
   (should (org-supertag-create-tag "tag1" '(:name "Important")))
   (should (org-supertag-create-field "priority" '(:type :number)))
   
   ;; 测试读取
   (should (org-supertag-db-exists-p "node1"))
   (should (equal (plist-get (org-supertag-db-get "node1") :title) "Test Node"))
   
   ;; 测试更新
   (org-supertag-db-put "node1" '(:type :node :title "Updated Node"))
   (should (equal (plist-get (org-supertag-db-get "node1") :title) "Updated Node"))
   
   ;; 测试删除
   (should (org-supertag-db-remove "node1"))
   (should-not (org-supertag-db-exists-p "node1"))))

;; 关系操作测试
(ert-deftest org-supertag-test-relations ()
  "测试标签和字段关系操作."
  (org-supertag-with-test-env
   ;; 准备测试数据
   (org-supertag-create-node "node1" '(:type :node))
   (org-supertag-create-tag "tag1" '(:type :tag))
   (org-supertag-create-field "field1" '(:type :field))
   
   ;; 测试添加标签
   (should (org-supertag-add-tag "node1" "tag1"))
   (should (member "tag1" (org-supertag-get-node-tags "node1")))
   
   ;; 测试设置字段
   (should (org-supertag-set-field "node1" "field1" "value1"))
   (should (equal (org-supertag-db-get-field-value "field1" "node1") "value1"))
   
   ;; 测试移除标签
   (should (org-supertag-remove-tag "node1" "tag1"))
   (should-not (member "tag1" (org-supertag-get-node-tags "node1")))))

;; 查询功能测试
(ert-deftest org-supertag-test-queries ()
  "测试查询功能."
  (org-supertag-with-test-env
   ;; 准备测试数
   (org-supertag-create-node "node1" '(:type :node))
   (org-supertag-create-node "node2" '(:type :node))
   (org-supertag-create-tag "tag1" '(:type :tag))
   (org-supertag-create-field "field1" '(:type :field))
   
   ;; 设置关系和值
   (org-supertag-add-tag "node1" "tag1")
   (message "Debug - Setting field for node1")
   (should (org-supertag-set-field "node1" "field1" "value1"))
   
   ;; 验证关系和值
   (message "Debug - Relations: %S" (ht-items org-supertag-db--relations))
   (message "Debug - Field values: %S" (ht-items org-supertag-db--field-values))
   
   ;; 测试按标签查询
   (should (equal (org-supertag-find-nodes-by-tag "tag1") '("node1")))
   
   ;; 测试按字段值查询
   (let ((nodes (org-supertag-find-nodes-by-field "field1" "value1")))
     (message "Debug - Found nodes by field: %S" nodes)
     (should (equal nodes '("node1"))))
   
   ;; 测试实体查询
   (should (= (length (org-supertag-find-entities :node)) 2))))

;; 标签组测试
(ert-deftest org-supertag-test-groups ()
  "测试标签组功能."
  (org-supertag-with-test-env
   ;; 创建组和标签
   (let ((group (org-supertag-create-group "group1" "Test Group")))
     (message "Created group: %S" (org-supertag-db-get "group1"))
     (should group))
   
   (let ((tag1 (org-supertag-create-tag "tag1" '(:type :tag))))
     (message "Created tag1: %S" (org-supertag-db-get "tag1"))
     (should tag1))
   
   (let ((tag2 (org-supertag-create-tag "tag2" '(:type :tag))))
     (message "Created tag2: %S" (org-supertag-db-get "tag2"))
     (should tag2))
   
   ;; 测试添加标��到组
   (let ((result1 (org-supertag-add-tag-to-group "tag1" "group1")))
     (message "Add tag1 to group result: %S" result1)
     (should result1))
   
   (let ((result2 (org-supertag-add-tag-to-group "tag2" "group1")))
     (message "Add tag2 to group result: %S" result2)
     (should result2))
   
   ;; 检查关系表的状态
   (message "Relations table: %S" 
            (ht-items org-supertag-db--relations))
   
   ;; 测试获取组内标签
   (let ((tags (org-supertag-get-group-tags "group1")))
     (message "Group tags: %S" tags)
     (should (equal (length tags) 2)))
   
   ;; 测试移除标签
   (should (org-supertag-remove-tag-from-group "tag1" "group1"))
   (should (equal (length (org-supertag-get-group-tags "group1")) 1))))

;; 缓存测试
(ert-deftest org-supertag-test-cache ()
  "测试缓存功能."
  (org-supertag-with-test-env
   ;; 准备测试数据
   (org-supertag-create-node "node1" '(:type :node))
   (org-supertag-create-tag "tag1" '(:type :tag))
   (org-supertag-add-tag "node1" "tag1")
   
   ;; 测试缓存获取
   (should (org-supertag-cache-get-node-tags "node1"))
   
   ;; 测试缓存失效
   (org-supertag-cache-invalidate-node "node1")
   (should-not (gethash "node1" org-supertag-cache--node-tags))
   
   ;; 测试缓存清理
   (org-supertag-cache-get-node-tags "node1")
   (org-supertag-cache-clear)
   (should-not (gethash "node1" org-supertag-cache--node-tags))))

;; 批量操作测试
(ert-deftest org-supertag-test-batch-operations ()
  "测试批量操作功能."
  (org-supertag-with-test-env
   ;; 准备测试数据
   (org-supertag-create-node "node1" '(:type :node))
   (org-supertag-create-tag "tag1" '(:type :tag))
   (org-supertag-create-tag "tag2" '(:type :tag))
   
   ;; 创建字段
   (should (org-supertag-create-field "field1" '(:type :field)))
   (should (org-supertag-create-field "field2" '(:type :field)))
   
   ;; 测试批量添加标签
   (should (org-supertag-batch-add-tags "node1" '("tag1" "tag2")))
   (should (equal (length (org-supertag-get-node-tags "node1")) 2))
   
   ;; 测试批量设置字段
   (should (org-supertag-batch-set-fields "node1" 
                                         '(("field1" . "value1")
                                           ("field2" . "value2"))))
   (should (equal (org-supertag-db-get-field-value "field1" "node1") "value1"))
   (should (equal (org-supertag-db-get-field-value "field2" "node1") "value2"))))

;; 节点系统测试
(ert-deftest org-supertag-test-node-system ()
  "测试节点系统的核心功能."
  (org-supertag-with-test-env
   ;; 测��节点创建和元数据
   (let ((props '(:title "Test Node"
                 :description "Test Description"
                 :number 1
                 :status :active)))
     ;; 创建节点
     (should (org-supertag-node-create "node1" props))
     
     ;; 测试元数据获取和更新
     (let ((metadata (org-supertag-node-get-metadata "node1")))
       (should metadata)
       (should (equal (plist-get metadata :title) "Test Node"))
       (should (equal (plist-get metadata :description) "Test Description"))
       (should (equal (plist-get metadata :number) 1))
       (should (equal (plist-get metadata :status) :active)))
     
     ;; 测试元数据更新
     (should (org-supertag-node-update-metadata "node1" 
                                               '(:number 2 
                                                 :status :archived)))
     (let ((updated-metadata (org-supertag-node-get-metadata "node1")))
       (should updated-metadata)
       (should (equal (plist-get updated-metadata :number) 2))
       (should (equal (plist-get updated-metadata :status) :archived)))
     
     ;; 测试大纲结构
     (with-temp-buffer
       ;; 确保 org-mode 完全初始化
       (message "Debug - Setting up org-mode...")
       (let ((org-inhibit-startup t))
         (org-mode))
       (setq-local org-startup-folded nil)
       
       ;; 设置临时文件名
       (message "Debug - Creating temp file...")
       (setq buffer-file-name (make-temp-file "org-supertag-test" nil ".org"))
       
       ;; 创建结构
       (message "Debug - Inserting outline structure...")
       (insert "* Parent Node\n")
       (insert "** Child Node\n")
       
       ;; 确保 org-element 缓存已更新
       (message "Debug - Resetting org-element cache...")
       (org-element-cache-reset)
       
       ;; 同步父节点
       (message "Debug - Syncing parent node...")
       (goto-char (point-min))
       (let* ((parent-id (org-id-get-create))
              (parent-level (org-outline-level))
              (_ (message "Debug - Parent level: %d" parent-level))
              (sync-result (org-supertag-sync-node-at-point)))
         (message "Debug - Parent ID: %s, Level: %d, Sync result: %s" 
                  parent-id parent-level sync-result)
         (should sync-result)
         
         ;; 同步子节点
         (message "Debug - Syncing child node...")
         (org-next-visible-heading 1)
         (let* ((child-id (org-id-get-create))
                (child-level (org-outline-level))
                (_ (message "Debug - Child level: %d" child-level))
                (sync-result (org-supertag-sync-node-at-point)))
           (message "Debug - Child ID: %s, Level: %d, Sync result: %s" 
                    child-id child-level sync-result)
           (should sync-result)
           
           ;; 验证纲系
           (let ((parent-node (org-supertag-node-get parent-id))
                 (child-node (org-supertag-node-get child-id)))
             (message "Debug - Parent node: %S" parent-node)
             (message "Debug - Child node: %S" child-node)
             (should parent-node)
             (should child-node)
             ;; 验证层级关系
             (should (= (plist-get parent-node :level) 1))
             (should (= (plist-get child-node :level) 2))
             ;; 验证大纲路径
             (should (equal (plist-get child-node :olp)
                           (list (plist-get parent-node :title)))))))
     
     ;; 清理临时文件
     (when (file-exists-p buffer-file-name)
       (delete-file buffer-file-name))))))

;; 标签系统测试
(ert-deftest org-supertag-test-tag-system ()
  "测试标签系统核心功能."
  (org-supertag-with-test-env
   ;; 测试标签创建
   (should (org-supertag-tag-create "tag1" '(:name "Important")))
   (should (org-supertag-tag-create "tag2" '(:name "Urgent")))
   (should (org-supertag-tag-create "tag3" '(:name "Project")))
   
   ;; 测试继承关系
   (should (org-supertag-tag-extend "tag2" "tag1"))  ; tag2 继承 tag1
   (should (org-supertag-tag-inherits-p "tag2" "tag1"))
   (should-not (org-supertag-tag-inherits-p "tag1" "tag2"))
   
   ;; 试多级继承
   (should (org-supertag-tag-extend "tag3" "tag2"))  ; tag3 继承 tag2
   (should (org-supertag-tag-inherits-p "tag3" "tag1"))  ; 传递性检查
   
   ;; 测试祖先和后代查询
   (should (equal (org-supertag-tag-get-ancestors "tag3") '("tag2" "tag1")))
   (should (equal (org-supertag-tag-get-descendants "tag1") '("tag2" "tag3")))
   
   ;; 测试循环继承检测
   (should-not (org-supertag-tag-extend "tag1" "tag3"))  ; 这会造成循环
   (should (org-supertag-tag-detect-inheritance-cycle "tag1" "tag3"))
   
   ;; 测试组合关系
   (should (org-supertag-tag-contain "tag1" "tag2"))
   (should (equal (org-supertag-tag-get-container "tag2") "tag1"))
   (should (member "tag2" (org-supertag-tag-get-components "tag1")))
   
   ;; 测试引用关系
   (should (org-supertag-tag-reference "tag2" "tag3"))
   (should (member "tag3" (org-supertag-tag-get-references "tag2")))
   (should (member "tag2" (org-supertag-tag-get-referrers "tag3")))
   
   ;; 测试互斥关
   (should (org-supertag-tag-exclusive "tag1" "tag3"))
   (should (member "tag3" (org-supertag-tag-get-exclusives "tag1")))
   (should (member "tag1" (org-supertag-tag-get-exclusives "tag3")))))

;; 字段系统测试
(ert-deftest org-supertag-test-field-system ()
  "测试字段系统的核心功能."
  (org-supertag-with-test-env
   ;; 测试字段类型注册
   (should (ht-get org-supertag-field-type-registry :number))
   (should (ht-get org-supertag-field-type-registry :string))
   (should (ht-get org-supertag-field-type-registry :priority))
   (should (ht-get org-supertag-field-type-registry :todo))

   ;; 测试字段创建和验证
   (let ((field-def '(:entity-type :field
                     :type :number
                     :name "number"
                     :required t
                     :default 0
                     :validate (lambda (val) 
                               (and (numberp val) 
                                    (<= 0 val 5))))))
     ;; 创字段
     (let ((create-result (org-supertag-field-create "number" field-def)))
       (message "Debug - Field creation result: %S" create-result)
       (should create-result))
     
     ;; 获取并验证字段
     (let ((field (org-supertag-field-get "number")))
       (message "Debug - Retrieved field: %S" field)
       (should field)
       (should (car (org-supertag-field-validate field 3)))
       (should-not (car (org-supertag-field-validate field 6)))
       (should-not (car (org-supertag-field-validate field "invalid")))))

   ;; 测试枚举字段
   (let ((field-def '(:entity-type :field
                     :type :enum
                     :name "status"
                     :values (:active :pending :completed)
                     :default :active)))
     ;; 创建字段
     (should (org-supertag-field-create "status" field-def))
     
     ;; 获取并验证字段
     (let ((field (org-supertag-field-get "status")))
       (message "Debug - Status field: %S" field)
       (should field)
       (should (car (org-supertag-field-validate field :active)))
       (let ((invalid-result (org-supertag-field-validate field :invalid)))
         (message "Debug - Invalid validation result: %S" invalid-result)
         (should-not (car invalid-result)))))

   ;; 测试字段继承
   (let ((parent-def '(:type :number
                      :validate (lambda (val) 
                                (<= 0 val 10))))
         (child-def '(:type :number
                     :extends "number"
                     :validate (lambda (val) 
                               (<= 0 val 5)))))
     ;; 创建字段
     (should (org-supertag-field-create "parent-number" parent-def))
     ;; 创��子字段
     (should (org-supertag-field-create "child-number" child-def))
     
     ;; 获取并验证字段
     (let ((child-field (org-supertag-field-get "child-number")))
       (should child-field)
       (should (car (org-supertag-field-validate child-field 3)))
       (should-not (car (org-supertag-field-validate child-field 7)))))

   ;; 测试复合字段
   (let ((field-def '(:type :composite
                     :fields ("number" "status"))))
     ;; 创建复合字段
     (should (org-supertag-field-create "task" field-def))
     
     ;; 获取并验证字段
     (let ((field (org-supertag-field-get "task")))
       (should field)
       (should (plist-get field :fields))
       (should (member "number" (plist-get field :fields)))
       (should (member "status" (plist-get field :fields)))))

   ;; 测试 Org 集成功能
   (let ((prop-def '(:type :property))
         (drawer-def '(:type :drawer))
         (planning-def '(:type :planning)))
     ;; 创建字段
     (should (org-supertag-field-create "test-prop" prop-def))
     (should (org-supertag-field-create "test-drawer" drawer-def))
     (should (org-supertag-field-create "DEADLINE" planning-def))
     
     ;; 创建临时 org buffer 进行测试
     (with-temp-buffer
       (org-mode)
       ;; 首先插入标题
       (insert "* Test Headline\n")
       ;; 然后插入 planning 信息
       (insert "DEADLINE: <2024-01-01 一>\n")
       ;; 接着性抽屉
       (insert ":PROPERTIES:\n")
       (insert ":test-prop: test-value\n")
       (insert ":END:\n")
       ;; 最后是普屉
       (insert ":test-drawer:\n")
       (insert "drawer content\n")
       (insert ":END:\n")
       
       ;; 确保解析正确
       (org-element-cache-reset)
       (goto-char (point-min))
       (org-next-visible-heading 1)
       
       ;; 获取并验证字段
       (let ((prop-field (org-supertag-field-get "test-prop"))
             (drawer-field (org-supertag-field-get "test-drawer"))
             (planning-field (org-supertag-field-get "DEADLINE")))
         ;; 验证属性
         (should prop-field)
         (should (equal (org-supertag-field--get-property prop-field) "test-value"))
         
         ;; 验证抽屉
         (should drawer-field)
         (should (equal (org-supertag-field--get-drawer drawer-field) "drawer content\n"))
         
         ;; 验证规划信息
         (should planning-field)
         (let ((planning-value (org-supertag-field--get-planning planning-field)))
           (message "Debug - Planning value: %S" planning-value)
           (should planning-value)
           ;; 验证日期格式
           (should (string-match-p "<2024-01-01" planning-value))))))

   ;; 测试系统字段
   (should (seq-find (lambda (field)
                      (string= (plist-get field :name) "deadline"))
                    org-supertag-system-fields))

   ;; 测试字段值缓存
   (let ((field-def '(:entity-type :field
                     :type :property
                     :name "cached-field")))
     ;; 创建字段
     (should (org-supertag-field-create "cached-field" field-def))
     
     ;; 创建临时 org buffer 进行测试
     (with-temp-buffer
       (org-mode)
       (insert "* Test Headline\n")
       (insert ":PROPERTIES:\n")
       (insert ":cached-field: test-value\n")
       (insert ":END:\n")
       
       ;; 获取字段并验证值
       (let ((field (org-supertag-field-get "cached-field")))
         (should field)
         (goto-char (point-min))
         (org-next-visible-heading 1)
         (let ((value (org-supertag-field-get-value field)))
           (message "Debug - Retrieved value: %S" value)
           (should (equal value "test-value"))))))))

;; 用户交互命令测试

(ert-deftest org-supertag-test-user-commands ()
  "测试用户交互命令."
  (org-supertag-with-test-env
   ;; 准备测试数据
   (with-temp-buffer
     (setq buffer-file-name (make-temp-file "org-supertag-test" nil ".org"))
     ;; 确保 org-mode 完全初始化
     (let ((org-inhibit-startup t))
       (org-mode))
     (setq-local org-startup-folded nil)
     (insert "* Test Headline\n")
     (goto-char (point-min))
     (org-element-cache-reset)  ; 重置 org-element 缓存
     
     ;; 模拟 UI 函数和用户输入
     (cl-letf (((symbol-function 'org-supertag-ui-insert-tag)
                (lambda () (org-supertag-add-tag (org-id-get) "test-tag")))
               ((symbol-function 'org-supertag-ui-set-field-value)
                (lambda () (org-supertag-set-field (org-id-get) "test-field" "test-value")))
               ((symbol-function 'y-or-n-p)  ; 模拟用户确认
                (lambda (_) t)))
       
       ;; 1. 测试创建节点
       (should (org-at-heading-p))  ; 确认在标题位置
       (org-supertag-node-create-at-point)
       ;; ... 其他测试保持不变 ...
       ))))

(ert-deftest org-supertag-test-user-commands-errors ()
  "测试用户交互命令的错误处理."
  (org-supertag-with-test-env
   ;; 准备测试数据
   (with-temp-buffer
     (insert "Plain text")  ; 不是 headline
     (goto-char (point-min))
     
     ;; 1. 测试非 headline 位的错误
     (should-error (org-supertag-node-create-at-point)
                  :type 'user-error)
     
     ;; 2. 测试无 ID 的错误
     (insert "\n* Test Headline\n")
     (goto-char (point-min))
     (forward-line 1)
     (should-error (org-supertag-node-update-at-point)
                  :type 'user-error))))

(ert-deftest org-supertag-test-user-commands-ui ()
  "测试户交互命令的 UI 交互."
  (org-supertag-with-test-env
   ;; 准备测试数据
   (with-temp-buffer
     (setq buffer-file-name (make-temp-file "org-supertag-test" nil ".org"))
     (org-mode)
     (insert "* Test Headline\n")
     (goto-char (point-min))
     
     ;; 使用动态绑定来模拟用户输入
     (let ((y-or-n-p-function (lambda (_) t))  ; 总是回答 yes
           (completing-read-function  ; 模拟选择
            (lambda (_prompt collection &rest _)
              (if (listp collection)
                  (car collection)
                (car (all-completions "" collection))))))
       
       ;; 1. 测试创建节点时的交互
       (org-supertag-node-create-at-point)
       (let ((node-id (org-id-get)))
         (should node-id)
         
         ;; 2. 测试更新节点时的交互
         (org-supertag-node-update-at-point)
         
         ;; 3. 测试查找节点时的交互
         (org-supertag-find-node)
         (should (equal (org-id-get) node-id)))))))

(provide 'org-supertag-tests)
;;; org-supertag-tests.el ends here 

;;; org-supertag-db.el --- Database layer for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供最基础的数据存储和查询功能
;; 
;; 核心理念：
;; - 一切皆实体，一切皆关系
;; - 实体是关系中的节点，关系是节点之间的连接
;; - 通过关系连接实体 - 使用 type、from、to 来表达关系
;;
;; 主要功能：
;; - 实体管理：创建、更新、删除、查询实体
;; - 关系管理：创建、删除、查询关系
;; - 数据持久化：保存、加载、备份数据
;; - 缓存系统：提高查询性能
;;
;; 使用方式：
;; 1. 实体操作
;;    - 添加：(org-supertag-db-add id props)
;;    - 查询：(org-supertag-db-get id)
;;    - 删除：(org-supertag-db-remove-object id)
;;
;; 2. 关系操作
;;    - 添加：(org-supertag-db-link type from to)
;;    - 查询：(org-supertag-db-find-links type)
;;    - 删除：(org-supertag-db-remove-link type from to)
;; 3. 解析器
;;    - 解析：(org-supertag-db-parse-file file-path)
;; 4. 数据管理
;;    - 保存：(org-supertag-db-save)
;;    - 加载：(org-supertag-db-load)
;; 4. 事件系统
;;    - 事件监听：(org-supertag-db-add-listener event-type function)
;;    - 事件触发：(org-supertag-db-trigger-event event-type)
;; 5. 缓存系统  
;;    - 缓存设置：(org-supertag-db-cache-set key value)
;;    - 缓存获取：(org-supertag-db-cache-get key)
;;    - 缓存删除：(org-supertag-db-cache-remove key)
;; 7. 数据持久化
;;    - 保存：(org-supertag-db-save)
;;    - 加载：(org-supertag-db-load)
;; 8. 数据备份
;;    - 备份：(org-supertag-db-backup)
;;    - 恢复：(org-supertag-db-restore)


;; 本文件不支持【破坏性更新】，如固有函数不足以支持功能，则实现新函数

;;; Code:

(require 'ht)
(require 'cl-lib)
;;(require 'org-supertag-base)
(require 'org-element)


;;------------------------------------------------------------------------------
;; 类型系统
;;------------------------------------------------------------------------------    
;;  Field-Value 储存在  Node-Field-Value 关系表
;;  Node -----> Tag -----> Field 定义
;;    |          |          |
;;    |          |          |
;;    v          v          v
;;  Node-Tag 关系表
;;       |          
;;       v          
;;   Node-Field-Value 关系表

;; 实体类型定义
(defconst org-supertag-db-object-type
  '(:node    ; org 节点（带标签的 headline）
    :tag)    ; 标签（包含字段定义的 supertag）
  "系统支持的实体类型.")

;; 字段值类型定义
(defconst org-supertag-db-field-type
  '(:string    ; 描述为“Plain Text”
    :number    ; 描述为“Number”，可被计算和统计
    :date      ; 描述为“Date”，日期，使用 org-mode 的 date-stamp 格式
    :time      ; 描述为“Time”，具体时间，使用 org-mode 的 time-stamp 格式
    :list      ; 描述为“List”，列表
    :option    ; 描述为“Option”，提供预设列表选择
    :ref)      ; 描述为“Reference”，引用来自其它标签定义的 Field 以及它的值
  "支持的字段值类型.")

(defconst org-supertag-db-object-structure
  '((:type :node
     :required (;; 基础信息
                :id          ; 节点唯一标识
                :title       ; 节点标题
                :file-path   ; 文件路径
                ;; 位置信息
                :pos         ; 节点位置
                :olp         ; 祖先标题列表
                :level       ; 层级(0为文件级)
                )         
     :optional (;; 任务信息
                :scheduled   ; 计划时间
                :deadline    ; 截止时间
                :priority    ; 优先级
                :todo        ; 待办状态
                ;; 引用关系
                :ref-to     ; 引用的节点
                :ref-from   ; 被引用的节点
                :ref-count   ; 引用计数
                ;; 事件信息
                :created-at  ; 创建时间
                :modified-at)) ; 修改时间
    
    (:type :tag
     :required (;; 基础信息
                :id          ; 标签唯一标识
                :name        ; 标签名称
                ;; 字段定义
                :fields)     ; ((field-name . field-props) ...)
     :optional (;; 元信息
                :description ; 描述， 用于描述标签的用途
                :icon        ; 图标， 用于标识标签，应用时，将在标签名前显示图标
                :color       ; 颜色， 用于标识标签，应用时，将在标签名添加颜色（分为背景色和前景色）
                :command     ; ((cmd-name . cmd-props) ...)
                             ; 例如：
                             ; (("summarize" . (:type :ai
                             ;                  :backend "gptel"
                             ;                  :prompt "..."
                             ;                  :input (:content :fields ("status" "priority"))
                             ;                  :output (:field "summary")))
                             ;  ("archive" . (:type :elisp
                             ;               :function org-archive-subtree
                             ;               :confirm t)))
                ;; 事件信息
                :created-at  ; 创建时间
                :modified-at)) ; 修改时间
  "实体结构定义."))

;; 关系类型定义
(defconst org-supertag-db-link-type
  '(:node-tag     ; 节点-标签关系
    :node-field   ; 节点-字段关系
    :tag-ref)      ; 标签引用关系
  "系统支持的关系类型.")

(defconst org-supertag-db-link-structure
  '((:type :node-tag
     :required (:from :to)    ; node-id -> tag-id
     :optional (:created-at)) ; 创建时间
    
    (:type :node-field
     :required (:from         ; node-id
                :to           ; field-name
                :tag-id       ; 所属标签
                :value)       ; 字段值
     :optional (:created-at   
                :modified-at))
    
    (:type :tag-ref
     :required (:from :to)    ; tag-id -> tag-id
     :optional (:ref-type))   ; 引用类型
    )) 

;; 命令类型定义
;; 命令是标签的属性，标签通过命令可以执行对 Node 的操作
;; 命令类型定义示例:
;; 1. Elisp 命令
;; (("archive" . (:type :elisp
;;                :function org-archive-subtree
;;                :confirm t)))
;;
;; 2. Shell 命令
;; (("open-in-vscode" . (:type :shell
;;                      :command "code"
;;                      :args ("%f")
;;                      :confirm nil)))
;;
;; 3. AI 命令
;; (("summarize" . (:type :ai
;;                 :backend "gptel"
;;                 :prompt "请总结以下内容:"
;;                 :input (:content :fields ("status" "priority"))
;;                 :output (:field "summary")
;;                 :confirm nil)))
;;
;; 4. 工作流命令
;; (("publish" . (:type :workflow
;;               :steps (("format" . (:type :elisp
;;                                  :function org-indent-region))
;;                      ("export" . (:type :elisp
;;                                 :function org-export-dispatch))
;;                      ("git-push" . (:type :shell
;;                                   :command "git"
;;                                   :args ("add" "%f" "&&" "git" "commit" "-m" "update" "&&" "git" "push"))))
;;               :confirm t)))
(defconst org-supertag-db-command-type
  '(:elisp     ; Emacs Lisp 函数
    :shell     ; Shell 命令
    :ai        ; AI 命令
    :workflow) ; 工作流（组合命令）
  "支持的命令类型.")

;; 命令结构定义
(defconst org-supertag-db-command-structure
  '((:type :elisp
     :required (:function)        ; 要执行的函数
     :optional (:args             ; 函数参数
                :confirm))        ; 是否需要确认

    (:type :shell
     :required (:command)         ; shell 命令
     :optional (:args             ; 命令参数
                :confirm))        ; 是否需要确认

    (:type :ai
     :required (:backend          ; AI 后端类型
                :prompt)          ; 提示模板
     :optional (:backend-config   ; 后端特定配置
                :input            ; 输入配置
                :output           ; 输出配置
                :confirm))        ; 是否需要确认

    (:type :workflow
     :required (:steps)         ; 步骤列表
     :optional (:confirm)))     ; 是否需要确认
  "命令结构定义.")

;; AI 后端类型
;; 示例:
;; 使用 gptel 后端
;; (:type :ai
;;  :backend :gptel
;;  :prompt \"请总结这段文字:\"
;;  :input (:content \"这是一段需要总结的文字\")
;;  :output (:field \"summary\"))

;; ;; 使用 ellama 后端 
;; (:type :ai
;;  :backend :ellama
;;  :prompt \"Translate to English:\"
;;  :input (:content \"你好世界\")
;;  :output (:field \"translation\"))")
(defconst org-supertag-db-ai-backend
  '(:gptel    ; gptel.el
    :ellama)   ; ellama.el
  "支持的 AI 后端包.")

(defconst org-supertag-db-ai-output-structure
  '((:type :field
     :required (:name)          ; 字段名称（必需）
     :optional (:transform))    ; 转换函数（可选）
    
    (:type :property
     :required (:name)          ; 属性名称（必需）
     :optional (:transform))    ; 转换函数（可选）
    
    (:type :content
     :required (:mode)          ; 内容处理模式（必需）
     :optional (:transform      ; 转换函数（可选）
                :position))     ; 插入位置（可选）
    )
  "AI 命令输出配置结构定义.
每种输出类型都定义了必需和可选的属性:
- :field    将输出存储到指定字段
- :property 将输出存储到节点属性
- :content  直接修改节点内容")

;; 事件类型

(defconst org-supertag-db-events
  '(entity:changed    ; 实体更新事件
    link:created      ; 关系创建事件
    link:removed      ; 关系删除事件
    field:changed     ; 字段值变更事件
    ref:created       ; 引用创建事件
    ref:removed       ; 引用删除事件
    ref:updated)      ; 引用更新事件
  "系统支持的事件类型.")
;;------------------------------------------------------------------------------
;; 类型验证
;;------------------------------------------------------------------------------    

(defun org-supertag-db-valid-object-type-p (type)
  "检查是否为有效的实体类型.
TYPE 必须是 :node 或 :tag"
  (memq type org-supertag-db-object-type))

(defun org-supertag-db-valid-link-type-p (type)
  "检查是否为有效的关系类型."
  (memq type org-supertag-db-link-type))

(defun org-supertag-db-valid-object-p (type props)
  "验证实体数据是否有效.
TYPE: 实体类型
PROPS: 属性列表"
  (let* ((struct (cl-find type org-supertag-db-object-structure
                         :key (lambda (x) (plist-get x :type))))
         (required (plist-get struct :required)))
    ;; (message "\nValidating object:")
    ;; (message "Type: %S" type)
    ;; (message "Required properties: %S" required)
    (if required
        (let ((missing-props
               (cl-remove-if
                (lambda (key)
                  (let ((has-prop (plist-member props key)))
                    (message "Checking %S: %S" key has-prop)
                    has-prop))
                required)))
          (if missing-props
              (progn
                (message "Missing required properties: %S" missing-props)
                nil)
            (progn
              (message "All required properties present")
              t)))
      t)))

(defun org-supertag-db-valid-link-p (type from to props)
  "验证关系数据是否有效.
TYPE: 关系类型
FROM: 源实体ID
TO: 目标实体ID
PROPS: 关系属性"
  (let ((structure (cl-find type org-supertag-db-link-structure
                           :key (lambda (x) (plist-get x :type))))
    (and structure
         ;; 验证实体存在
         (org-supertag-db-get from)
         (org-supertag-db-get to)
         ;; 验证必需属性
         (let ((required (plist-get structure :required)))
           (cl-every (lambda (key)
                      (plist-member props key))
                    required))
         ;; 验证关系类型特定规则
         (pcase type
           ;; 节点标签关系
           (:node-tag
            (and (eq :node (org-supertag-db-get-type from))
                 (eq :tag (org-supertag-db-get-type to))))
           ;; 节点字段关系
           (:node-field
            (and (eq :node (org-supertag-db-get-type from))
                 (stringp (plist-get props :tag-id))
                 (plist-get props :value)))
           ;; 标签引用关系
           (:tag-ref
            (and (eq :tag (org-supertag-db-get-type from))
                 (eq :tag (org-supertag-db-get-type to)))))))))

(defun org-supertag-db--validate-props (type props)
  "验证实体属性是否符合结构定义.
TYPE 是实体类型，必须是 :node 或 :tag
PROPS 是实体属性列表"
  (let* ((struct (cl-find type org-supertag-db-object-structure
                         :key (lambda (x) (plist-get x :type))))
         (required (plist-get struct :required)))
    ;; 验证类型
    (unless struct
      (error "Invalid entity type: %s" type))
    ;; 验证必需属性
    (let ((missing-props
           (cl-remove-if
            (lambda (key) (plist-member props key))
            required)))
      (when missing-props
        (error "Missing required properties: %s" missing-props)))))
;;------------------------------------------------------------------------------
;; 实体构造函数
;;------------------------------------------------------------------------------    

(defun org-supertag-db-make-node (id title file &rest props)
  "创建节点实体.
ID: 节点唯一标识
TITLE: 节点标题
FILE: 所在文件
PROPS: 其他属性"
  (org-supertag-db-add id
    (append (list :type :node
                 :id id
                 :title title
                 :file-path file  ; file 参数直接用于 file-path
                 :pos nil 
                 :olp nil
                 :level nil
                 :end nil         ; 节点结束位置
                 :contents-begin nil ; 节点内容开始位置
                 :contents-end nil   ; 节点内容结束位置
                 :contents nil
                 :created-at (current-time))
            props)))

(defun org-supertag-db-make-tag (id name &rest props)
  "创建标签实体.
ID: 标签唯一标识
NAME: 标签名称
PROPS: 其他属性，可包含:
  :fields - ((field-name . field-props) ...)
  :description - 描述
  :icon - 图标
  :color - 颜色
  :command - 命令配置"
  (org-supertag-db-add id
    (append (list :type :tag
                 :id id
                 :name name
                 :fields nil  ; 默认空字段列表
                 :created-at (current-time))
            props)))

(defun org-supertag-db-make-field-props (name type &rest props)
  "创建字段属性列表.
NAME: 字段名称
TYPE: 字段类型 (:string/:number/:date/:time/:list/:option/:ref)
PROPS: 其他属性，可包含:
  :description - 描述
  :default - 默认值
  :options - 选项列表（用于 :option 类型）
  :ref-tag - 引用的标签（用于 :ref 类型）"
  (append (list :name name
                :type type)
          props))

;;------------------------------------------------------------------------------
;; 关系构造函数
;;------------------------------------------------------------------------------    

(defun org-supertag-db-make-node-tag (node-id tag-id)
  "创建节点-标签关系."
  (org-supertag-db-link :node-tag node-id tag-id
                        (list :created-at (current-time))))

(defun org-supertag-db-make-node-field (node-id field-name value tag-id)
  "创建节点-字段关系.
NODE-ID: 节点ID
FIELD-NAME: 字段名称
VALUE: 字段值
TAG-ID: 所属标签ID"
  (org-supertag-db-link :node-field node-id field-name
                        (list :tag-id tag-id
                              :value value
                              :created-at (current-time))))

(defun org-supertag-db-make-tag-ref (from-tag-id to-tag-id ref-type)
  "创建标签引用关系.
FROM-TAG-ID: 源标签ID
TO-TAG-ID: 目标标签ID
REF-TYPE: 引用类型"
  (org-supertag-db-link :tag-ref from-tag-id to-tag-id
                        (list :ref-type ref-type)))

(defun org-supertag-db-make-node-tag (node-id tag-id)
  "创建节点-标签关系."
  (org-supertag-db-link :node-tag node-id tag-id
                        (list :created-at (current-time))))

(defun org-supertag-db-make-node-field (node-id field-name value tag-id)
  "创建节点-字段关系.
NODE-ID: 节点ID
FIELD-NAME: 字段名称
VALUE: 字段值
TAG-ID: 所属标签ID"
  (org-supertag-db-link :node-field node-id field-name
                        (list :tag-id tag-id
                              :value value
                              :ref-pos nil
                              :ref-context nil
                              :created-at (current-time))))

(defun org-supertag-db-make-tag-ref (from-tag-id to-tag-id ref-type)
  "创建标签引用关系.
FROM-TAG-ID: 源标签ID
TO-TAG-ID: 目标标签ID
REF-TYPE: 引用类型"
  (org-supertag-db-link :tag-ref from-tag-id to-tag-id
                        (list :ref-type ref-type)))

;;------------------------------------------------------------------------------
;; 核心数据表
;;------------------------------------------------------------------------------    

(defvar org-supertag-db--object (ht-create)
  "实体存储 id -> plist.")

(defvar org-supertag-db--link (ht-create)
  "关系存储 rel-id -> (type from to props).")

;; 事件系统
(defvar org-supertag-db--events (ht-create)
  "事件系统 - 存储事件处理器.")

;; 缓存系统
(defvar org-supertag-db--cache (ht-create)
  "缓存系统 - 存储查询缓存.")

;;---------------------------------------------------------------------------------
;; Data Operation: Add
;;---------------------------------------------------------------------------------

(defun org-supertag-db-add (id props)
  "添加或更新实体.
ID 是实体唯一标识
PROPS 是实体属性"
  (let* ((type (plist-get props :type))
         (old-props (org-supertag-db-get id))
         (new-props (if old-props
                       ;; 更新时保留创建时间，更新修改时间
                       (append
                        (list :created-at (plist-get old-props :created-at)
                              :modified-at (current-time))
                        props)
                     ;; 新建时设置创建时间
                     (append
                      (list :created-at (current-time))
                      props))))
    ;; 验证属性
    (unless (org-supertag-db-valid-object-type-p type)
      (error "Invalid object type: %s" type))
    (unless (org-supertag-db-valid-object-p type new-props)
      (error "Invalid object properties"))
    ;; 存储实体
    (ht-set! org-supertag-db--object id new-props)
    ;; 清除缓存
    (org-supertag-db--cache-remove 'entity id)
    ;; 触发事件
    (if old-props
        (org-supertag-db-emit 'entity:changed type id new-props)
      (org-supertag-db-emit 'entity:added type id new-props))
    ;; 返回ID
    id))

(defun org-supertag-db-exists-p (id)
  "检查实体是否存在."
  (ht-contains-p org-supertag-db--object id))

;;---------------------------------------------------------------------------------
;; Data Operation: Link
;;---------------------------------------------------------------------------------

(defun org-supertag-db-link (type from to &optional props)
  "创建关系.
TYPE: 关系类型 (node-tag, node-field, tag-ref)
FROM: 源实体ID
TO: 目标实体ID
PROPS: 关系属性"
  (let* ((base-props (list :from from :to to))
         (full-props (if props 
                        (append base-props props)
                      base-props))
         (rel-id (format "%s:%s->%s" type from to)))
    ;; 验证关系类型
    (unless (org-supertag-db-valid-link-type-p type)
      (error "Invalid link type: %s" type))
    ;; 验证关系数据
    (unless (org-supertag-db-valid-link-p type from to full-props)
      (error "Invalid link data"))
    ;; 存储关系
    (ht-set! org-supertag-db--link rel-id full-props)
    ;; 清除缓存
    (org-supertag-db--cache-remove 'link rel-id)
    ;; 触发事件
    (org-supertag-db-emit 'link:created type from to props)
    ;; 返回关系ID
    rel-id))

(defun org-supertag-db-unlink (type from to &optional dry-run)
  "删除特定关系.
TYPE: 关系类型
FROM: 源实体ID
TO: 目标实体ID
DRY-RUN: 如果非nil，只检查是否存在而不删除

返回值：
- t 表示删除成功或关系存在（dry-run模式）
- nil 表示关系不存在"
  (let* ((rel-id (format "%s:%s->%s" type from to))
         (exists (ht-contains-p org-supertag-db--link rel-id)))
    (when (and exists (not dry-run))
      (ht-remove! org-supertag-db--link rel-id))
    exists))


(defun org-supertag-db-unlink-all (from &optional type)
  "删除实体的所有关系或特定类型的关系."
  (let ((count 0))
    (ht-map (lambda (k v)
              (when (and (equal from (nth 1 v))
                        (or (null type)
                            (equal type (car v))))
                (ht-remove! org-supertag-db--link k)
                (cl-incf count)))
            org-supertag-db--link)
    count))


;;---------------------------------------------------------------------------------
;; Data Operation: Get
;;---------------------------------------------------------------------------------
;; org-supertag-db-get (单实体查询)
;;     ├── org-supertag-db-get-prop (属性访问)
;;     │       └── org-supertag-db-get-type (类型访问便捷函数)

(defun org-supertag-db-get-node-tags (node-id)
  "获取节点的所有标签."
  (org-supertag-db-get-link :node-tag node-id))

(defun org-supertag-db-get-node-fields (node-id)
  "获取节点的所有字段值."
  (org-supertag-db-get-link :node-field node-id))

(defun org-supertag-db-get-tag-refs (tag-id)
  "获取标签的所有引用."
  (org-supertag-db-get-link :tag-ref tag-id))

(defun org-supertag-db-get (id &optional default)
  "获取实体的属性列表.
ID: 实体唯一标识
DEFAULT: 当实体不存在时返回的默认值
返回值：
- 如果实体存在，返回其属性列表
- 如果实体不存在，返回 DEFAULT（默认为nil）
- 属性列表格式: (:type type :prop1 value1 ...)

它会先试图从缓存读取数据，失败后再从实体表中读取数据"
  (or (org-supertag-db--cache-get 'entity id)
      (let ((value (or (ht-get org-supertag-db--object id)
                      default)))
        (when value
          (org-supertag-db--cache-set 'entity id value))
        value)))

(defun org-supertag-db-get-prop (id prop &optional default)
  "获取实体的特定属性值.
如果实体或属性不存在，返回 DEFAULT."
  (if-let ((props (org-supertag-db-get id)))
      (or (plist-get props prop) default)
    default))

(defun org-supertag-db-get-type (id)
  "获取实体的类型."
  (org-supertag-db-get-prop id :type))

;; Get Link
(defun org-supertag-db-get-link (type from)
  "获取关系.
TYPE: 关系类型
FROM: 源实体ID
返回 ((to props) ...) 形式的列表"
  (let (results)
    (ht-map (lambda (k v)
              (when (and (equal type (car v))
                        (equal from (cadr v)))
                (push (list (nth 2 v) (nth 3 v)) results)))
            org-supertag-db--link)
    (message "Debug - link: %S" (ht-items org-supertag-db--link))
    (nreverse results)))

(defun org-supertag-db-get-link-reverse (type to)
  "获取反向关系.
TYPE: 关系类型
TO: 目标实体ID
返回 ((from props) ...) 形式的列表"
  (let (results)
    (ht-map (lambda (k v)
              (when (and (equal type (car v))
                        (equal to (nth 2 v)))
                (push (list (cadr v) (nth 3 v)) results)))
            org-supertag-db--link)
    (nreverse results)))

    (defun org-supertag-db-get-all-link ()
  "获取所有关系.
返回 ((type from to props) ...) 形式的列表"
  (let (results)
    (ht-map (lambda (k v)
              (push v results))
            org-supertag-db--link)
    results))

;; Get Ref Link
(defun org-supertag-db-get-ref-context (ref-id)
  "获取引用的上下文信息.
REF-ID 是引用关系的ID"
  (when-let* ((props (org-supertag-db-get-link ref-id))
              (context (plist-get props :ref-context)))
    context))

(defun org-supertag-db-get-ref-pos (ref-id)
  "获取引用的位置信息.
REF-ID 是引用关系的ID"
  (when-let* ((props (org-supertag-db-get-link ref-id))
              (pos (plist-get props :ref-pos)))
    pos))
;;---------------------------------------------------------------------------------
;; Data Operation: Find
;;---------------------------------------------------------------------------------
;; org-supertag-db-find (base)
;;     ├── org-supertag-db-find-by-props (通用属性查询)
;;     │       └── org-supertag-db-find-by-type (类型查询便捷函数)



(defun org-supertag-db-find (pred)
  "查找满足条件的实体."
  (let ((cache-key (format "%s" (sxhash pred))))
    ;; 尝试从缓存获取
    (or (org-supertag-db--cache-get 'query cache-key)
        ;; 缓存未命中，执行查询
        (let (results)
          (ht-map (lambda (k v)
                    (when (funcall pred k v)
                      (push (cons k v) results)))
                  org-supertag-db--object)
          ;; 缓存并返回结果
          (let ((final-results (nreverse results)))
            (org-supertag-db--cache-set 'query cache-key final-results)
            final-results)))))

(defun org-supertag-db--check-match (props rules)
    "检查属性是否匹配条件."
    (cl-loop for (key value) on rules by #'cddr
            always (equal (plist-get props key) value)))

(defun org-supertag-db-find-by-props (props)
  "查找满足属性条件的实体."
  (org-supertag-db-find 
   (lambda (_k v) 
     (org-supertag-db--check-match v props))))

(defun org-supertag-db-find-by-type (type)
  "查找指定类型的实体."
  (mapcar #'car (org-supertag-db-find-by-props `(:type ,type))))

(defun org-supertag-db-find-nodes-by-tag (tag-id)
  "查找使用了指定标签的所有节点."
  (mapcar #'car 
          (org-supertag-db-find-links :type :node-tag :to tag-id)))

(defun org-supertag-db-find-nodes-by-field-value (field-name value &optional tag-id)
  "查找具有指定字段值的所有节点."
  (let ((links (org-supertag-db-find-links :type :node-field)))
    (cl-remove-if-not
     (lambda (link)
       (and (equal (plist-get (nth 3 link) :value) value)
            (or (null tag-id)
                (equal (plist-get (nth 3 link) :tag-id) tag-id))))
     links)))

(defun org-supertag-db-find-tags-by-field (field-name)
  "查找包含指定字段的所有标签."
  (org-supertag-db-find
   (lambda (_k v)
     (and (eq (plist-get v :type) :tag)
          (assoc field-name (plist-get v :fields))))))

;; Find Ref Link :TODO 需要更新，以符合org-supertag-db-object-structure 的定义，节点的引用关系是直接存储在节点属性中的id &optional direction)
;;   "查找节点的引用关系.
;; NODE-ID 是节点ID
;; DIRECTION 可以是:
;; - 'from: 返回该节点引用的其他节点
;; - 'to: 返回引用该节点的其他节点
;; - nil: 返回所有相关引用"
;;   (let ((refs nil))
;;     (maphash
;;      (lambda (rel-id props)
;;        (when (eq :node-ref (plist-get props :type))
;;          (let ((from (plist-get props :from))
;;                (to (plist-get props :to)))
;;            (when (or (and (eq direction 'from)
;;                          (equal from node-id))
;;                      (and (eq direction 'to)
;;                          (equal to node-id))
;;                      (and (null direction)
;;                           (or (equal from node-id)
;;                               (equal to node-id))))
;;              (push (cons rel-id props) refs)))))
;;      org-supertag-db--link)
;;     refs))       

;;---------------------------------------------------------------------------------
;; Data Operation: Remove
;;---------------------------------------------------------------------------------

(defun org-supertag-db-remove-object (id &optional dry-run)
  "删除实体及其相关的所有关系.
ID: 实体ID
DRY-RUN: 如果非nil，只返回将被删除的数据而不实际删除"
  (when-let ((entity (org-supertag-db-get id)))
    (let ((links (org-supertag-db-unlink-all id)))
      (unless dry-run
        ;; 删除实体
        (ht-remove! org-supertag-db--object id)
        ;; 清除缓存
        (org-supertag-db--cache-remove 'entity id)
        ;; 触发事件
        (org-supertag-db-emit 'entity:removed id entity))
      ;; 返回删除的数据
      (cons entity links))))

(defun org-supertag-db-remove-link (type from to &optional dry-run)
  "删除特定关系.
TYPE: 关系类型
FROM: 源实体ID
TO: 目标实体ID
DRY-RUN: 如果非nil，只返回将被删除的数据而不实际删除"
  (let* ((rel-id (format "%s:%s->%s" type from to))
         (link (ht-get org-supertag-db--link rel-id)))
    (when link
      (unless dry-run
        ;; 删除关系
        (ht-remove! org-supertag-db--link rel-id)
        ;; 清除缓存
        (org-supertag-db--cache-remove 'link rel-id)
        ;; 触发事件
        (org-supertag-db-emit 'link:removed type from to))
      ;; 返回删除的数据
      link)))

;;------------------------------------------------------------------------------
;; Org Element 解析
;;------------------------------------------------------------------------------    

(require 'org-element)

(defun org-supertag-db--parse-node-at-point ()
  "解析当前位置的节点数据."
  (save-excursion
    (let* ((element (org-element-at-point))
           (type (org-element-type element)))
      (if (eq type 'headline)
          ;; 解析 headline
          (let* ((title (org-element-property :raw-value element))
                 (level (org-element-property :level element))
                 (id (or (org-element-property :ID element)
                        (org-id-get-create)))
                 ;; 任务相关属性
                 (scheduled (org-element-property :scheduled element))
                 (deadline (org-element-property :deadline element))
                 (priority (org-element-property :priority element))
                 (todo (org-element-property :todo-keyword element))
                 ;; 获取大纲路径
                 (olp (org-get-outline-path t))
                 ;; 其他属性
                 (tags (org-element-property :tags element))
                 (properties (org-element-property :PROPERTIES element)))
      
            (list :type :node
                  :id id
                  :title title
                  :file-path (buffer-file-name)
                  :pos (org-element-property :begin element)
                  :olp olp
                  :level level
                  ;; 任务属性
                  :scheduled scheduled
                  :deadline deadline
                  :priority priority
                  :todo todo
                  ;; 其他属性
                  :properties properties
                  :tags tags
                  :created-at (current-time)))
        ;; 解析文件级节点
        (when (eq type 'org-data)
          (let ((title (or (cadr (assoc "TITLE" 
                                       (org-element-property :keywords element)))
                          (file-name-base (buffer-file-name)))))
            (list :type :node
                  :id (org-id-get-create)
                  :title title
                  :file-path (buffer-file-name)
                  :pos 1
                  :olp nil
                  :level 0
                  :created-at (current-time))))))))

(defun org-supertag-db-add-node-at-point ()
  "添加当前位置的节点到数据库."
  (when-let ((props (org-supertag-db--parse-node-at-point)))
    (let ((id (plist-get props :id)))
      (org-supertag-db-add id props)
      ;; 返回节点ID
      id)))

(defun org-supertag-db-update-node-at-point ()
  "更新当前位置节点的数据."
  (when-let* ((props (org-supertag-db--parse-node-at-point))
              (id (plist-get props :id))
              (old-props (org-supertag-db-get id)))
    ;; 保留创建时间
    (when-let ((created-at (plist-get old-props :created-at)))
      (setq props (plist-put props :created-at created-at)))
    ;; 更新修改时间
    (setq props (plist-put props :modified-at (current-time)))
    (org-supertag-db-add id props)
    id))

;;------------------------------------------------------------------------------
;; 批量解析
;;------------------------------------------------------------------------------    

(defun org-supertag-db--collect-nodes-in-buffer ()
  "收集当前缓冲区中的所有节点."
  (let ((nodes nil))
    ;; 确保在文件缓冲区中
    (when (buffer-file-name)
      ;; 首先添加文件级节点
      (save-excursion
        (goto-char (point-min))
        (push (org-supertag-db--parse-node-at-point) nodes))
      ;; 然后收集所有标题节点
      (org-map-entries
       (lambda ()
         (let ((node (org-supertag-db--parse-node-at-point)))
           (when node
             (push node nodes))))
       t nil))
    ;; 返回收集到的节点列表
    (nreverse nodes)))

(defun org-supertag-db-update-buffer ()
  "更新当前缓冲区中的所有节点数据.
返回更新的节点数量."
  (let ((count 0)
        (file-path (buffer-file-name)))
    (when file-path
      ;; 收集所有节点
      (dolist (node (org-supertag-db--collect-nodes-in-buffer))
        ;; 添加或更新节点
        (org-supertag-db-add (plist-get node :id) node)
        (cl-incf count)
      count))))

(defun org-supertag-db-update-file (file)
  "更新指定文件中的所有节点数据.
FILE 是文件路径
返回更新的节点数量."
  (when (and file (file-exists-p file))
    (with-current-buffer (find-file-noselect file)
      (org-mode)
      (org-supertag-db-update-buffer))))

(defun org-supertag-db-update-directory (dir)
  "更新目录中的所有 org 文件."
  (let ((files (directory-files-recursively dir "\\.org$"))
        (total 0))
    (dolist (file files)
      (cl-incf total (org-supertag-db-update-file file)))
    (message "Updated %d nodes in %d files" total (length files))
    total))

;;------------------------------------------------------------------------------
;; Node Reference Parsing
;;------------------------------------------------------------------------------    

(defun org-supertag-db--parse-node-all-ref ()
  "解析当前节点中的所有引用关系.
返回引用的节点ID列表."
  (let ((ref-to nil)
        (node-id (org-supertag-db--get-current-node-id)))
    (when node-id
      ;; 解析整个节点范围内的所有链接
      (let ((start (org-entry-beginning-position))
            (end (org-entry-end-position)))
        (save-excursion
          (goto-char start)
          (while (re-search-forward org-link-any-re end t)
            (let* ((link (org-element-context))
                   (type (org-element-property :type link))
                   (path (org-element-property :path link)))
              (when (and (equal type "id")
                       (org-uuidgen-p path))
                (push path ref-to)))))
        ;; 更新引用关系
        (let ((ref-to (delete-dups (nreverse ref-to))))
          (org-supertag-db--update-node-all-ref node-id ref-to)
          ref-to)))))


(defun org-supertag-db--update-node-all-ref (node-id ref-to)
  "更新节点的所有引用关系.
NODE-ID 是当前节点ID
ref-to 是引用的节点ID列表"
  (message "开始更新节点引用关系 - 节点ID: %s" node-id)
  (message "新引用列表: %S" ref-to)
  
  (let* ((node (org-supertag-db-get node-id))
         (old-refs (plist-get node :ref-to)))
    (message "原引用列表: %S" old-refs)
    
    ;; 更新当前节点的引用
    (org-supertag-db-add node-id
                         (plist-put node :ref-to ref-to))
    (message "已更新当前节点的引用")
    
    ;; 更新被引用节点的反向引用，并触发新增引用事件
    (dolist (ref-id ref-to)
      (message "处理引用节点: %s" ref-id)
      (unless (member ref-id old-refs)  ; 只对新增的引用触发事件
        (message "发现新增引用，触发事件")
        (org-supertag-db-emit 'ref:created node-id ref-id nil))
      (when-let ((ref-node (org-supertag-db-get ref-id)))
        (let* ((ref-from (plist-get ref-node :ref-from))
               (new-ref-from (cons node-id (delete node-id ref-from))))
          (message "更新引用节点的反向引用: %S" new-ref-from)
          (org-supertag-db-add ref-id
                              (plist-put ref-node 
                                        :ref-from 
                                        (delete-dups new-ref-from))))))
    ;; 清理旧引用，并触发删除引用事件
    (dolist (old-ref old-refs)
      (unless (member old-ref ref-to)
        (message "清理旧引用: %s" old-ref)
        (org-supertag-db-emit 'ref:removed node-id old-ref nil)
        (when-let ((ref-node (org-supertag-db-get old-ref)))
          (let ((ref-from (delete node-id (plist-get ref-node :ref-from))))
            (message "更新被引用节点的反向引用: %S" ref-from)
            (org-supertag-db-add old-ref
                                (plist-put ref-node :ref-from ref-from))))))
    (message "节点引用关系更新完成")))

(defun org-supertag-db--get-current-node-id ()
  "获取当前节点的ID."
  (save-excursion
    (org-back-to-heading t)
    (let ((id (or (org-id-get)
                  (org-id-get-create))))
      (message "获取当前节点ID: %s" id)
      id)))


;;------------------------------------------------------------------------------
;; 事件系统
;;------------------------------------------------------------------------------    

;; 事件系统示例:
;; 1. 事件类型
;;    - entity:changed  ; 实体更新事件,当实体属性变更时触发
;;    - link:created    ; 关系创建事件,当新建关系时触发
;;    - link:removed    ; 关系删除事件,当删除关系时触发
;;    - field:changed   ; 字段值变更事件,当字段值更新时触发
;; 2. 注册事件处理器
;; (org-supertag-db-on 'entity:changed handler)
;; handler 函数接收事件相关的参数:
;; - entity:changed: (id props)     ; 实体ID和新属���
;; - link:created:  (type from to)  ; 关系类型,起点,终点
;; - link:removed:  (type from to)  ; 关系类型,起点,终点
;; - field:changed: (field-id value); 字段ID和新值
;; 3. 示例:监听实体更新事件
;; (org-supertag-db-on 'entity:changed
;;                     (lambda (id props)
;;                       (message "实体 %s 已更新: %S" id props)))
;; 4. 事件触发
;; 当调用数据操作函数时会自动触发相应事件:
;; (org-supertag-db-add "node-1" '(:type :node :title "测试")) ; 触发 entity:changed



(defvar org-supertag-db--events (ht-create)
  "事件处理器映射表 event-name -> (handler1 handler2 ...).")

(defun org-supertag-db-on (event handler)
  "注册事件处理器.
EVENT: 事件名称，如 'entity:changedx
HANDLER: 处理函数，接收事件相关的参数"
  (let ((handlers (ht-get org-supertag-db--events event)))
    (ht-set! org-supertag-db--events event 
             (cons handler handlers))))

(defun org-supertag-db-off (event handler)
  "移除事件处理器."
  (let* ((handlers (ht-get org-supertag-db--events event))
         (new-handlers (remove handler handlers)))
    (ht-set! org-supertag-db--events event new-handlers)))

(defun org-supertag-db-emit (event &rest args)
  "触发事件.
EVENT: 事件名称
ARGS: 传递给处理器的参数"
  (dolist (handler (ht-get org-supertag-db--events event))
    (condition-case err
        (apply handler args)
      (error 
       (message "事件处理错误 [%s]: %S" event err)))))

;;------------------------------------------------------------------------------
;; Status
;;------------------------------------------------------------------------------    

(defun org-supertag-db-check-status ()
  "检查数据库状态."
  (interactive)
  (message "\n=== 数据库状态 ===")
  (message "数据库文件: %s" org-supertag-db-file)
  (message "文件存在: %s" (file-exists-p org-supertag-db-file))
  (message "当前实体数: %d" (ht-size org-supertag-db--object))
  (message "当前关系数: %d" (ht-size org-supertag-db--link)))

  (defun org-supertag-db-test ()
  "测试数据库操作."
  (interactive)
  (message "\n=== 测试数据库操作 ===")
  
  ;; 1. 添加一个测试实体
  (org-supertag-db-add "test-tag" '(:type :tag :name "test"))
  (message "添加实体后大小: %d" (ht-size org-supertag-db--object))
  
  ;; 2. 添加一个测试关系
  (org-supertag-db-link :node-tag "test-node" "test-tag")
  (message "添加关系后大小: %d" (ht-size org-supertag-db--link))
  
  ;; 3. 保存数据库
  (org-supertag-db-save)
  (message "数据已保存")
  
  ;; 4. 重新加载
  (org-supertag-db-load)
  (message "重新加载后:")
  (message "实体数: %d" (ht-size org-supertag-db--object))
  (message "关系数: %d" (ht-size org-supertag-db--link)))

;;-----------------------------------------------------------------------------
;; 缓存系统
;;-----------------------------------------------------------------------------

(defvar org-supertag-db--cache (ht-create)
  "简单的查询缓存.
key 格式: 'query:hash' -> 查询结果
key 格式: 'entity:id' -> 实体数据")

(defun org-supertag-db--cache-key (type key)
  "生成缓存键.
TYPE: 缓存类型 (query 或 entity)
KEY: 具体的键"
  (format "%s:%s" type key))

(defun org-supertag-db--cache-get (type key)
  "获取缓存值."
  (ht-get org-supertag-db--cache 
          (org-supertag-db--cache-key type key)))

(defun org-supertag-db--cache-set (type key value)
  "设置缓存值."
  (ht-set! org-supertag-db--cache 
           (org-supertag-db--cache-key type key)
           value))

(defun org-supertag-db--cache-remove (type key)
  "移除缓存."
  (ht-remove! org-supertag-db--cache 
              (org-supertag-db--cache-key type key)))

(defun org-supertag-db--cache-clear ()
  "清除所有缓存."
  (ht-clear! org-supertag-db--cache))

(defcustom org-supertag-db-auto-save-interval 300
  "自动保存数据库的时间间隔（秒）.
设置为 nil 禁用自动保存."
  :type '(choice (const :tag "禁用" nil)
                (integer :tag "间隔（秒）"))
  :group 'org-supertag)

(defvar org-supertag-db--auto-save-timer nil
  "自动保存定时器.")

(defun org-supertag-db--setup-auto-save ()
  "设置自动保存定时器."
  (when (and org-supertag-db-auto-save-interval
             (null org-supertag-db--auto-save-timer))
    (setq org-supertag-db--auto-save-timer
          (run-with-timer org-supertag-db-auto-save-interval
                         org-supertag-db-auto-save-interval
                         #'org-supertag-db-save))))

(defun org-supertag-db--cleanup-auto-save ()
  "清理自动保存定时器."
  (when org-supertag-db--auto-save-timer
    (cancel-timer org-supertag-db--auto-save-timer)
    (setq org-supertag-db--auto-save-timer nil)))

;;------------------------------------------------------------------------------
;; Database Custom
;;------------------------------------------------------------------------------  

(defcustom org-supertag-data-directory
  (expand-file-name "org-supertag" user-emacs-directory)
  "Org-supertag 数据存储目录.
默认在 Emacs 配置目录下的 org-supertag 子目录."
  :type 'directory
  :group 'org-supertag)

(defun org-supertag-db-ensure-data-directory ()
  "确保数据库目录和备份目录存在."
  (let ((db-dir (file-name-directory org-supertag-db-file)))
    ;; 1. 确保数据库目录存在
    (unless (file-exists-p db-dir)
      (make-directory db-dir t))
    ;; 2. 确保备份目录存在
    (unless (file-exists-p org-supertag-db-backup-dir)
      (make-directory org-supertag-db-backup-dir t))
    ;; 3. 验证目录创建结果
    (unless (and (file-exists-p db-dir)
                 (file-exists-p org-supertag-db-backup-dir))
      (error "无法创建必要的目录: %s 或 %s" 
             db-dir org-supertag-db-backup-dir))))

(defun org-supertag-data-file (filename)
  "获取数据文件的完整路径.
FILENAME 是相对于数据目录的文件名."
  (expand-file-name filename org-supertag-data-directory))

(defcustom org-supertag-db-file
  (org-supertag-data-file "supertag-db.el")
  "数据库文件路径."
  :type 'file
  :group 'org-supertag)

;;------------------------------------------------------------------------------
;; Data Backup
;;------------------------------------------------------------------------------      

(defcustom org-supertag-db-backup-directory
  (org-supertag-data-file "backups")
  "数据库备份目录."
  :type 'directory
  :group 'org-supertag)

(defcustom org-supertag-db-backup-max-days 3
  "保留备份的最大天数."
  :type 'integer
  :group 'org-supertag)

(defun org-supertag-db-cleanup-backups ()
  "清理旧的备份文件，只保留最近几天的备份."
  (let* ((now (float-time))
         (max-age (* org-supertag-db-backup-max-days 24 60 60))
         (cutoff (- now max-age)))
    (dolist (file (directory-files org-supertag-db-backup-directory t "db-.*\\.el$"))
      (let ((mtime (float-time (file-attribute-modification-time
                               (file-attributes file)))))
        (when (< mtime cutoff)
          (delete-file file))))))

(defun org-supertag-db-backup ()
  "创建数据库备份."
  (when (file-exists-p org-supertag-db-file)
    (let* ((backup-name (format "db-%s.el" 
                               (format-time-string "%Y%m%d-%H%M%S")))
           (backup-file (expand-file-name backup-name org-supertag-db-backup-directory)))
      (copy-file org-supertag-db-file backup-file t)
      (org-supertag-db-cleanup-backups))))

;;------------------------------------------------------------------------------
;; Data Save&Load
;;------------------------------------------------------------------------------

(defun org-supertag-db-save ()
  "安全地保存数据到文件.
使用临时文件和原子操作确保数据完整性."
  (run-hooks 'org-supertag-db-before-save-hook)
  (condition-case err
      (let ((temp-file (concat org-supertag-db-file ".tmp"))
            (backup-file (concat org-supertag-db-file ".bak")))
        ;; 确保目录存在
        (org-supertag-db-ensure-data-directory)
        
        ;; 1. 写入临时文件
        (with-temp-file temp-file
          (let ((print-level nil)
                (print-length nil))
            (message "正在保存数据库到临时文件: %s" temp-file)
            (insert ";; -*- lexical-binding: t -*-\n")
            (insert ";;; 自动生成的数据库文件，请勿手动编辑\n\n")
            ;; 保存数据库状态
            (insert (format ";; 保存时间: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
            (insert (format ";; 实体数量: %d\n" (ht-size org-supertag-db--object)))
            (insert (format ";; 关系数量: %d\n\n" (ht-size org-supertag-db--link)))
            ;; 重置数据库
            (insert "(setq org-supertag-db--object (ht-create))\n")
            (insert "(setq org-supertag-db--link (ht-create))\n\n")
            ;; 保存实体数据
            (ht-map (lambda (k v)
                     (insert (format "(ht-set! org-supertag-db--object %S '%S)\n"
                                   k v)))
                   org-supertag-db--object)
            ;; 保存链接数据
            (ht-map (lambda (k v)
                     (insert (format "(ht-set! org-supertag-db--link %S '%S)\n"
                                   k v)))
                   org-supertag-db--link)))
        
        ;; 2. 验证临时文件
        (unless (file-exists-p temp-file)
          (error "临时文件创建失败"))
        
        ;; 3. 备份当前文件（如果存在）
        (when (file-exists-p org-supertag-db-file)
          (copy-file org-supertag-db-file backup-file t))
        
        ;; 4. 原子替换
        (rename-file temp-file org-supertag-db-file t)
        
        ;; 5. 清理备份文件
        (when (and (file-exists-p backup-file)
                  (file-exists-p org-supertag-db-file))
          (delete-file backup-file))
        
        ;; 6. 运行保存后钩子
        (run-hooks 'org-supertag-db-after-save-hook)
        (message "数据库保存成功"))
    
    (error
     (message "数据库保存失败: %S" err)
     ;; 不要在错误时初始化数据库
     nil)))

(defun org-supertag-db-load ()
  "从文件安全地加载数据.
包含验证和错误恢复机制."
  (interactive)
  (run-hooks 'org-supertag-db-before-load-hook)
  (condition-case err
      (progn
        (org-supertag-db-ensure-data-directory)
        (if (file-exists-p org-supertag-db-file)
            (let ((backup-data nil))
              ;; 1. 保存当前数据作为备份
              (when (and (boundp 'org-supertag-db--object)
                        (boundp 'org-supertag-db--link))
                (setq backup-data
                      (list :object (ht-copy org-supertag-db--object)
                            :link (ht-copy org-supertag-db--link))))
              
              ;; 2. 尝试加载数据
              (with-temp-buffer
                (message "正在加载数据库: %s" org-supertag-db-file)
                (insert-file-contents org-supertag-db-file)
                ;; 验证文件基本格式
                (goto-char (point-min))
                (unless (looking-at ";; -*-.*-\\*-")
                  (error "数据库文件格式错误"))
                
                ;; 3. 创建新的空哈希表
                (setq org-supertag-db--object (ht-create)
                      org-supertag-db--link (ht-create))
                
                ;; 4. 读取并执行表达式
                (let ((success t))
                  (goto-char (point-min))
                  (while (and success (not (eobp)))
                    (condition-case load-err
                        (eval (read (current-buffer)))
                      (error
                       (setq success nil)
                       (message "加载数据时出错: %S" load-err))))
                  
                  (if success
                      (progn
                        ;; 5. 验证加载的数据
                        (unless (and (ht? org-supertag-db--object)
                                   (ht? org-supertag-db--link))
                          (error "加载的数据格式无效"))
                        
                        (run-hooks 'org-supertag-db-after-load-hook)
                        (message "数据库加载成功：%d 个实体, %d 个关系"
                                (ht-size org-supertag-db--object)
                                (ht-size org-supertag-db--link))
                        t)
                    ;; 加载失败，恢复备份数据
                    (when backup-data
                      (setq org-supertag-db--object (plist-get backup-data :object)
                            org-supertag-db--link (plist-get backup-data :link))
                      (message "恢复到备份数据"))
                    nil))))
          
          ;; 文件不存在时的处理
          (message "数据库文件不存在，创建新的空数据库")
          (setq org-supertag-db--object (ht-create)
                org-supertag-db--link (ht-create))
          nil))
    
    ;; 错误处理
    (error
     (message "数据库加载失败: %S" err)
     ;; 确保至少有空的数据结构
     (unless (and (boundp 'org-supertag-db--object)
                 (boundp 'org-supertag-db--link))
       (setq org-supertag-db--object (ht-create)
             org-supertag-db--link (ht-create)))
     nil)))

;;------------------------------------------------------------------------------
;; Data Save&Load Hooks
;;------------------------------------------------------------------------------    

(defvar org-supertag-db-before-save-hook nil
  "数据库保存前的钩子.
在数据实际写入文件前运行.")

(defvar org-supertag-db-after-save-hook nil
  "数据库保存后的钩子.
在数据成功写入文件后运行.")

(defvar org-supertag-db-before-load-hook nil
  "数据库加载前的钩子.
在开始从文件读取数据前运行.")

(defvar org-supertag-db-after-load-hook nil
  "数据库加载后的钩子.
在数据成功加载到内存后运行.")


;;------------------------------------------------------------------------------
;; 初始化
;;------------------------------------------------------------------------------  
(defcustom org-supertag-db-auto-save-interval 300
  "自动保存数据库的时间间隔（秒）.
设置为 nil 禁用自动保存."
  :type '(choice (const :tag "禁用" nil)
                (integer :tag "间隔（秒）"))
  :group 'org-supertag)

(defvar org-supertag-db--auto-save-timer nil
  "自动保存定时器.")

(defun org-supertag-db-init ()
  "初始化数据库.
说明：
1. 尝试加载现有数据库
2. 如果加载失败，创建空数据库
3. 设置自动保存"
  (message "初始化数据库...")
  ;; 尝试加载现有数据库
  (unless (org-supertag-db-load)
    ;; 加载失败时创建空数据库
    (setq org-supertag-db--object (ht-create)
          org-supertag-db--link (ht-create))  
    (message "创建空数据库"))
  
  ;; 设置自动保存
  (when (and org-supertag-db-auto-save-interval
             (null org-supertag-db--auto-save-timer))
    (setq org-supertag-db--auto-save-timer
          (run-with-timer org-supertag-db-auto-save-interval
                         org-supertag-db-auto-save-interval
                         #'org-supertag-db-save))))

;; 在 Emacs 退出时保存
(add-hook 'kill-emacs-hook #'org-supertag-db-save)

;; 在加载数据时清除缓存
(add-hook 'org-supertag-db-before-load-hook #'org-supertag-db--cache-clear)

(provide 'org-supertag-db)

;;; org-supertag-parser.el --- Parse and process org-mode supertags -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; 提供三种标签语法：
;;
;; 1. 基本标签：#tag
;;    纯粹的标签，用于分类和标记
;;    例如：#project #draft
;;
;; 2. 带字段标签：#tag.field=value
;;    为标签添加结构化信息
;;    例如：#task.status=pending #project.phase=planning
;;    会创建属性：:STATUS: pending :PHASE: planning
;;
;; 3. 字段即标签：#field=value
;;    将字段直接作为标签使用，简化常用属性的标记
;;    例如：#priority=high #status=done
;;    会创建属性：:PRIORITY: high :STATUS: done
;;
;; 同时支持引用语法：
;; @type.id 形式的引用会被转换为对应的 _REF 属性
;; 例如：@person.bob 会创建属性 :PERSON_REF: bob

;;; Code:

(require 'org)
(require 'org-element)

;;------------------------------------------------------------------------------ 
;; Parse tags
;;------------------------------------------------------------------------------ 

(defconst org-supertag-tag-regex
  (rx "#"                           ; 标签开始
      (group                        ; 捕获组1：标签名
       (or
        ;; 复合标签写法: aaa/bbb 或 aaa:bbb
        (seq (1+ (any "a-zA-Z")) 
             (zero-or-more (any "a-zA-Z0-9-_"))
             (any "/" ":")          ; 支持两种分隔符
             (1+ (any "a-zA-Z"))
             (zero-or-more (any "a-zA-Z0-9-_")))
        ;; 基本标签: tag
        (seq (1+ (any "a-zA-Z"))
             (zero-or-more (any "a-zA-Z0-9-_")))))
      (optional                     ; 字段部分（可选）
       "."
       (group                      ; 捕获组2：字段名
        (1+ (any "a-zA-Z0-9-_")))
       (optional                   ; 赋值部分（可选）
        "="
        (group                    ; 捕获组3：字段值
         (1+ (not (any "\n" " ")))))))
  "标签的正则表达式模式.
支持以下格式:
1. 基本标签: #tag
2. 复合标签: #aaa/bbb 或 #aaa:bbb
3. 带字段: #tag.field
4. 带字段和值: #tag.field=value

注意：复合标签写法（如 #aaa/bbb）只是种语法糖，
实际的标签关系应该通过字段系统来定义。")


;;------------------------------------------------------------------------------ 
;; Parse references 
;;------------------------------------------------------------------------------ 

(defconst org-supertag-reference-regex
  (rx "@"                           ; @ 开始
      (group                        ; 捕获组1：实体类型
       (1+ (any "a-zA-Z")))
      "."                          ; 点号分隔符
      (group                       ; 捕获组2：实体ID
       (1+ (any "a-zA-Z0-9_-"))))
  "匹配 @引用 的正则表达式.")

(defun org-supertag-parse-references (text)
  "解析文本中的 @ 引用.
返回引用列表，每个引用是一个 plist，格式为:
(:type 'reference :entity-type \"type\" :entity-id \"id\")"
  (let ((refs nil)
        (start 0))
    (while (string-match 
            (rx "@"                    ; @ 开始
                (group                 ; 捕获组1：实体类型
                 (1+ (any "a-zA-Z")))
                "."                    ; 点号分隔符
                (group                 ; 捕获组2：实体ID
                 (1+ (any "a-zA-Z0-9_"))))
            text start)
      (push (list :type 'reference
                 :entity-type (match-string-no-properties 1 text)
                 :entity-id (match-string-no-properties 2 text))
            refs)
      (setq start (match-end 0)))
    (nreverse refs)))

;;------------------------------------------------------------------------------ 
;; Parse tags
;;------------------------------------------------------------------------------  

(defun org-supertag-valid-context-p (pos)
  "检查位置 POS 是否在有效的标签上下文中.
返回 t 如果不在注释、引号、代码块等特殊上下文中."
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    (not (memq type '(comment
                      src-block
                      example-block
                      quote-block
                      verse-block
                      export-block)))))

(defun org-supertag-parse-headline-tag (headline-text)
  "解析标题文本中的标签.
返回解析结果 plist:
:valid-tags   - ((tag . (beg . end)) ...)
:invalid-tags - ((tag . reason) ...)
:duplicates   - ((tag . (pos1 pos2 ...)) ...)"
  (let ((valid-tags nil)
        (invalid-tags nil)
        (duplicates nil)
        (tag-positions (make-hash-table :test 'equal))
        (case-fold-search nil))
    
    ;; 收集所有标签及其位置
    (with-temp-buffer
      (insert headline-text)
      (goto-char (point-min))
      (while (re-search-forward org-supertag-tag-regex nil t)
        (let* ((tag (match-string-no-properties 1))
               (beg (match-beginning 0))
               (end (match-end 0))
               (pos-list (gethash tag tag-positions)))
          ;; 记录标签位置
          (puthash tag 
                   (cons (cons beg end) pos-list)
                   tag-positions))))
    
    ;; 处理收集到的标签
    (maphash
     (lambda (tag positions)
       (if (= (length positions) 1)
           ;; 单个标签，检查有效性
           (if (org-supertag-valid-context-p (caar positions))
               (push (cons tag (car positions)) valid-tags)
             (push (cons tag :invalid-context) invalid-tags))
         ;; 多个标签，记录为重复
         (push (cons tag (mapcar #'car positions)) duplicates)))
     tag-positions)
    
    ;; 返回结果
    (list :valid-tags (nreverse valid-tags)
          :invalid-tags (nreverse invalid-tags)
          :duplicates (nreverse duplicates))))

(defalias 'org-supertag-parse-headline-tags 'org-supertag-parse-headline-tag)

(defun org-supertag-parse-node-get-tags ()
  "解析当前节点的标签信息. 返回节点信息 plist."
  (let* ((element (org-element-at-point))
         (headline (org-element-property :raw-value element))
         (begin (org-element-property :begin element))
         (tags (org-supertag-parse-headline-tags headline)))
    
    ;; 调整标签位置为绝对位置
    (list :node-id (org-id-get-create)
          :valid-tags
          (mapcar (lambda (tag)
                   (cons (car tag)
                         (cons (+ begin (cadr tag))
                               (+ begin (cddr tag)))))
                 (plist-get tags :valid-tags))
          :invalid-tags (plist-get tags :invalid-tags)
          :duplicates (plist-get tags :duplicates))))

(defun org-supertag-split-compound-tag (tag-name)
  "分解复合标签名.
如果是复合标签（包含 / 或 :），返回 (prefix . suffix)
否则返回 nil.

例如：
  (org-supertag-split-compound-tag \"project/web\") => (\"project\" . \"web\")
  (org-supertag-split-compound-tag \"task:high\") => (\"task\" . \"high\")
  (org-supertag-split-compound-tag \"simple\") => nil"
  (when (string-match "\\`\\([[:alpha:]][[:alnum:]_-]*\\)\\([/:]\\)\\([[:alpha:]][[:alnum:]_-]*\\)\\'" tag-name)
    (cons (match-string 1 tag-name)
          (match-string 3 tag-name))))

(defun org-supertag-normalize-tag (tag-plist)
  "标准化标签数据.
将复合标签转换为标准形式，返回新的 plist.

例如：
  #project/web => (:type tag :name \"project\" :field \"type\" :value \"web\")
  #task:high => (:type tag :name \"task\" :field \"priority\" :value \"high\")"
  (let ((tag-name (plist-get tag-plist :name)))
    (if-let ((compound (org-supertag-split-compound-tag tag-name)))
        (list :type 'tag
              :name (car compound)
              :field (pcase (substring tag-name (length (car compound)) (1+ (length (car compound))))
                      ("/" "type")
                      (":" "category")
                      (_ "type"))  ; 默认使用 type
              :value (cdr compound))
      tag-plist)))

(defun org-supertag-parse-content-tag (text)
  "解析文本内容中的标签."
  (let ((tags nil)
        (start 0))
    (while (string-match org-supertag-tag-regex text start)
      (let* ((tag-name (match-string-no-properties 1 text))
             (field (match-string-no-properties 2 text))
             (value (match-string-no-properties 3 text))
             (tag-plist (list :type 'tag
                             :name tag-name
                             :field field
                             :value value)))
        (push (org-supertag-normalize-tag tag-plist) tags))
      (setq start (match-end 0)))
    (nreverse tags)))

;;------------------------------------------------------------------------------ 
;; Parse node
;;------------------------------------------------------------------------------ 


;; 获取节点的大纲路径
(defun org-supertag-parse-node-get-outline-path (element)
  "获取节点的大纲路径.
ELEMENT 是 org-element 对象"
  (let ((current element)
        path)
    (message "Debug - Starting outline path collection...")
    (message "Debug - Initial element title: %s"
             (org-element-property :raw-value element))
    ;; 先收集所有节点
    (while (and current 
                (eq (org-element-type current) 'headline))
      (let ((title (org-element-property :raw-value current)))
        (message "Debug - Found headline: %s" title)
        (push title path))
      (setq current (org-element-property :parent current))
      (when current
        (message "Debug - Parent type: %s" (org-element-type current))))
    (message "Debug - Final path before processing: %S" path)
    (let ((final-result (butlast path)))
      (message "Debug - Final result: %S" final-result)
      final-result)))

(defun org-supertag-parse-node-props (element)
  "从 org-element 中提取节点属性.
ELEMENT 是 org-element 对象"
  `(:type :node
    :title ,(org-element-property :raw-value element)
    :level ,(org-element-property :level element)
    :properties ,(org-entry-properties)
    :olp ,(org-supertag-parse-node-get-outline-path element)))

(defun org-supertag-parse-node ()
  "解析当前节点的完整信息，包括标题、内容和标签."
  (let* ((element (org-element-at-point))
         (begin (org-element-property :contents-begin element))
         (end (org-element-property :contents-end element))
         ;; 解析标题
         (headline-tags (org-supertag-parse-headline element))
         ;; 解析内容
         (content-tags (when (and begin end)
                        (org-supertag-parse-content begin end)))
         ;; 解析属性
         (props (org-supertag-parse-node-props element)))
    (append props 
            headline-tags
            content-tags)))

(defun org-supertag-parse-content (begin end)
  "解析指定范围内的内容，返回标签和引用列表."
  (let ((text (buffer-substring-no-properties begin end)))
    (append
     (org-supertag-parse-content-tag text)   ; 解析标签
     (org-supertag-parse-references text)))) ; 解析引用

(defun org-supertag-parse-element (element)
  "解析任意 org 元素中的标签和引用."
  (pcase (org-element-type element)
    ('paragraph (org-supertag-parse-paragraph element))
    ('table (org-supertag-parse-table element))
    ('src-block (org-supertag-parse-src-block element))
    (_ nil)))

;; 修改现有的 org-supertag-parse-node 函数
(defun org-supertag-parse-node ()
  "解析当前节点的完整信息，包括标题、内容和标签."
  (let* ((element (org-element-at-point))
         (begin (org-element-property :contents-begin element))
         (end (org-element-property :contents-end element))
         ;; 获取原有的标签和属性信息
         (headline-tags (org-supertag-parse-node-get-tags))
         (props (org-supertag-parse-node-props element))
         ;; 新增：解析内容
         (content-tags (when (and begin end)
                        (org-supertag-parse-content begin end))))
    ;; 合并所有信息
    (append props 
            headline-tags
            (list :content-tags content-tags))))

;;------------------------------------------------------------------------------ 
;; Parse content elements
;;------------------------------------------------------------------------------ 

(defun org-supertag-parse-paragraph (element)
  "解析段落中的标签和引用.
ELEMENT 是 org-element 对象，返回解析结果列表."
  (let ((text (org-element-property :raw-value element)))
    (when text
      (append
       (org-supertag-parse-references text)
       (org-supertag-parse-content-tag text)))))

(defun org-supertag-parse-table (element)
  "解析表格中的标签和引用.
ELEMENT 是 org-element 对象，返回解析结果列表."
  (let* ((table (org-element-property :value element))
         (result nil))
    (dolist (row table)
      (dolist (cell row)
        (when cell  ; 跳过空单元格
          (setq result (append result
                              (org-supertag-parse-references cell)
                              (org-supertag-parse-content-tag cell))))))
    result))

(defun org-supertag-parse-src-block (element)
  "解析源代码块中的标签和引用.
ELEMENT 是 org-element 对象，返回解析结果列表."
  (let ((text (org-element-property :value element)))
    (when text
      (append
       (org-supertag-parse-references text)
       (org-supertag-parse-content-tag text)))))

;;------------------------------------------------------------------------------ 
;; Detect changes
;;------------------------------------------------------------------------------ 
 
(defun org-supertag-detect-changes (old-tags new-tags)
  "检测标签的变化（添加/删除）"
  (list :added (cl-set-difference new-tags old-tags :test #'equal)
        :removed (cl-set-difference old-tags new-tags :test #'equal)))


;;------------------------------------------------------------------------------ 
;; Test
;;------------------------------------------------------------------------------ 


(defun org-supertag-parser-test-tags ()
  "测试标签解析功能."
  (interactive)
  (with-temp-buffer
    (let ((test-cases
           '(;; 基本标签
             ("#simple"
              ((:type tag :name "simple" :field nil :value nil)))
             
             ;; 带字段的标签
             ("#task.status"
              ((:type tag :name "task" :field "status" :value nil)))
             
             ;; 带字段和值的标签
             ("#task.priority=high"
              ((:type tag :name "task" :field "priority" :value "high")))
             
             ;; 复合标签
             ("#project/web"
              ((:type tag :name "project" :field "type" :value "web")))
             
             ("#priority:high"
              ((:type tag :name "priority" :field "category" :value "high")))
             
             ;; 简单组合
             ("#project/web #task.status=active"
              ((:type tag :name "project" :field "type" :value "web")
               (:type tag :name "task" :field "status" :value "active")))))
          (passed 0)
          (failed 0))
      
      (message "\n=== 标签解析测试 ===")
      (dolist (test test-cases)
        (let* ((input (car test))
               (expected (cadr test))
               (result (org-supertag-parse-content-tag input)))
          (if (equal result expected)
              (cl-incf passed)
            (cl-incf failed)
            (message "\n失败用例:")
            (message "输入: %S" input)
            (message "期望: %S" expected)
            (message "实际: %S" result))))
      
      (message "\n=== 测试结果 ===")
      (message "通过: %d" passed)
      (message "失败: %d" failed)
      (message "总计: %d\n" (+ passed failed)))))

(defun org-supertag-parser-run-tests ()
  "运行解析器的完整测试."
  (interactive)
  (with-temp-buffer
    (org-mode)
    ;; 插入测试数据
    (insert "* 测试节点 #project.type=test
:PROPERTIES:
:ID: test-node-001
:END:

这是一个测试任务，分配给 @person.john
优先级是 #task.priority=high

| 任务 | 负责人 | 状态 |
|------+--------+------|
| 设计 | @person.alice | #status.todo |

#+begin_src elisp
;; #code.type=elisp
(defun hello () ...)
#+end_src
")
    
    ;; 移动到节点开始处
    (goto-char (point-min))
    
    ;; 解析整个节点
    (let* ((node-info (org-supertag-parse-node))
           (headline-tags (plist-get node-info :valid-tags))
           (content-tags (plist-get node-info :content-tags)))
      
      ;; 打印测试结果
      (message "\n=== 测试结果 ===")
      
      ;; 测试标题标签
      (message "\n标题标签:")
      (dolist (tag headline-tags)
        (message "- %S" tag))
      
      ;; 测试内容标签和引用
      (message "\n内容中的标签和引用:")
      (dolist (item content-tags)
        (message "- %S" item))
      
      ;; 测试节点属性
      (message "\n节点属性:")
      (dolist (prop (plist-get node-info :properties))
        (message "- %S" prop))
      
      ;; 测试大纲路径
      (message "\n大纲路径:")
      (message "- %S" (plist-get node-info :olp))
      
      ;; 返回完整的解析结果
      node-info)))

;; 添加一些辅助测试函数
(defun org-supertag-test-parse-text (text)
  "测试文本解析功能."
  (message "\n=== 测试文本解析 ===")
  (message "输入: %S" text)
  (let ((tags (org-supertag-parse-tags text))
        (refs (org-supertag-parse-references text)))
    (message "找到的标签: %S" tags)
    (message "找到的引用: %S" refs)))

(defun org-supertag-test-parse-element (text element-type)
  "测试特定类型元素的解析.
TEXT 是要解析的文本
ELEMENT-TYPE 可以是 'paragraph, 'table, 或 'src-block"
  (message "\n=== 测试 %S 解析 ===" element-type)
  (with-temp-buffer
    (org-mode)
    (insert text)
    (let* ((element (org-element-at-point))
           (result (org-supertag-parse-element element)))
      (message "解析结果: %S" result))))

(defun org-supertag-parse-current-node ()
  "解析当前节点的标签和引用."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  
  (let* ((element (org-element-at-point))
         (begin (org-element-property :contents-begin element))
         (end (org-element-property :contents-end element))
         (title (org-element-property :raw-value element)))
    
    (message "解析标题: %s" title)
    (message "内容范围: %s-%s" begin end)
    
    (let ((title-tags (org-supertag-parse-content-tag title))
          (content-results (when (and begin end)
                           (org-supertag-parse-content begin end))))
      
      (message "标题标签: %S" title-tags)
      (message "内容结果: %S" content-results)
      
      ;; 简化输出，直接使用 message
      (message "\n=== 解析结果 ===")
      (message "标题: %s" title)
      (message "\n标题标签:")
      (dolist (tag title-tags)
        (message "- %S" tag))
      (message "\n内容标签和引用:")
      (dolist (item content-results)
        (message "- %S" item)))))

(defun org-supertag-parse-region (begin end)
  "解析选中区域的标签和引用."
  (interactive "r")
  (let ((results (org-supertag-parse-content begin end)))
    ;; 创建结果显示 buffer
    (with-current-buffer (get-buffer-create "*org-supertag-result*")
      (erase-buffer)
      (org-mode)
      (insert "* 区域解析结果\n\n")
      
      ;; 显示原文
      (insert "** 原文\n")
      (insert (buffer-substring-no-properties begin end) "\n\n")
      
      ;; 显示标签
      (insert "** 标签\n")
      (dolist (item results)
        (when (eq (plist-get item :type) 'tag)
          (insert (format "- 标签名: %s\n" (plist-get item :name)))
          (when (plist-get item :field)
            (insert (format "  字段: %s\n" (plist-get item :field)))
          (when (plist-get item :value)
            (insert (format "  值: %s\n" (plist-get item :value))))))
      
      ;; 显示引用
      (insert "\n** 引用\n")
      (dolist (item results)
        (when (eq (plist-get item :type) 'reference)
          (insert (format "- 类型: %s\n" (plist-get item :entity-type)))
          (insert (format "  ID: %s\n" (plist-get item :entity-id)))))
      
      ;; 显示 buffer
      (display-buffer (current-buffer))))))

;; 添加快捷键绑定建议
(defvar org-supertag-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t n") 'org-supertag-parse-current-node)
    (define-key map (kbd "C-c t r") 'org-supertag-parse-region)
    map)
  "Org SuperTag mode 的键盘映射.")

;;------------------------------------------------------------------------------ 
;; 特殊字段处理
;;------------------------------------------------------------------------------  

(defvar org-supertag-special-fields
  '(("priority" . (("high" . "A")
                  ("medium" . "B")
                  ("normal" . "C")
                  ("low" . "D")
                  ("trivial" . "E")))
  "特殊字段的值映射表."))

(defun org-supertag-convert-field-value (field value)
  "转换字段值，处理特殊情况."
  (let ((field-lower (downcase field)))
    (cond
     ;; 优先级特殊处理
     ((string= field-lower "priority")
      (or (cdr (assoc-string value (cdr (assoc "priority" org-supertag-special-fields)) t))
          (user-error "Priority '%s' 无效. 可用值: high, medium, normal, low, trivial" value)))
     ;; 其他字段直接返回原值
     (t value))))

(defvar-local org-supertag-cache nil
  "缓存最近解析结果.
格式: (buffer-tick node-id parse-results)")

(defun org-supertag-cache-valid-p ()
  "检查当前节点的缓存是否有效."
  (when org-supertag-cache
    (and (equal (nth 0 org-supertag-cache) (buffer-modified-tick))
         (equal (nth 1 org-supertag-cache) (org-id-get)))))

(defun org-supertag-quick-parse ()
  "快速解析当前节点的标签和引用."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((begin (point))
           (end (save-excursion
                  (or (org-get-next-sibling)
                      (org-end-of-subtree t t))))
           (content (buffer-substring-no-properties begin end))
           (tags nil)
           (fields nil)
           (refs nil))
      
      (message "\n=== 开始解析 ===")
      (message "节点内容:\n%s" content)
      
      ;; 解析标签和字段 - 修复优先级标记问题
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        ;; 跳过优先级标记
        (when (looking-at "\\* \\[#[A-Z]\\]")
          (goto-char (match-end 0)))
        
        ;; 解析实际标签
        (while (re-search-forward "#\\([^[:space:].=]+\\)\\(?:\\.\\([[:alnum:]_]+\\)=\\([^[:space:]]+\\)\\|=\\([^[:space:]]+\\)\\)?" nil t)
          (let* ((tag (match-string-no-properties 1))
                 (field (match-string-no-properties 2))
                 (field-value (match-string-no-properties 3))
                 (direct-value (match-string-no-properties 4))
                 (full-match (match-string-no-properties 0)))
            (message "找到标签: %s" full-match)
            (cond
             ;; 形式 2: #tag.field=value
             ((and field field-value)
              (let ((converted-value (org-supertag-convert-field-value field field-value)))
                (message "  → 标签.字段=值: %s.%s=%s (转换后: %s)" 
                        tag field field-value converted-value)
                (push (cons (upcase field) converted-value) fields)
                (push (list :name tag :field field 
                          :value field-value 
                          :converted-value converted-value
                          :type 'field-value) 
                      tags)))
             
             ;; 形式 3: #field=value
             (direct-value
              (let ((converted-value (org-supertag-convert-field-value tag direct-value)))
                (message "  → 字段=值: %s=%s (转换后: %s)" 
                        tag direct-value converted-value)
                (push (cons (upcase tag) converted-value) fields)
                (push (list :name tag 
                          :value direct-value
                          :converted-value converted-value
                          :type 'value) 
                      tags)))
             
             ;; 形式 1: #tag
             (t
              (message "  → 基本标签: %s" tag)
              (push (list :name tag :type 'simple) tags))))))
      
      ;; 解析引用
      (message "\n[解析引用]")
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "@\\([[:alpha:]]+\\)\\.\\([[:alnum:]_]+\\)" nil t)
          (let* ((type (match-string-no-properties 1))
                 (id (match-string-no-properties 2))
                 (full-ref (match-string-no-properties 0)))
            (message "找到引用: %s" full-ref)
            (message "  → 类型: %s, ID: %s" type id)
            (push (cons type id) refs)
            (push (cons (concat (upcase type) "_REF") id) fields))))
      
      ;; 设置所有属性
      (when fields
        (message "\n[设置属性]")
        (org-with-wide-buffer
         (let ((node-id (or (org-id-get) (org-id-get-create))))
           (message "节点 ID: %s" node-id)
           (dolist (field fields)
             (message "  → 设置 :%s: = %s" (car field) (cdr field))
             (org-entry-put nil (car field) (cdr field))))))
      
      ;; 显示最终结果
      (message "\n=== 解析结果 ===")
      (message "原始内容:\n%s" content)
      
      ;; 按类型分组显示标签
      (let ((simple-tags nil)
            (field-tags nil)
            (value-tags nil))
        (dolist (tag tags)
          (pcase (plist-get tag :type)
            ('simple (push tag simple-tags))
            ('field-value (push tag field-tags))
            ('value (push tag value-tags))))
        
        (message "\n解析出的标签:")
        (when simple-tags
          (message "  基本标签:")
          (dolist (tag (nreverse simple-tags))
            (message "    #%s" (plist-get tag :name))))
        (when field-tags
          (message "  带字段标签:")
          (dolist (tag (nreverse field-tags))
            (message "    #%s.%s=%s" 
                    (plist-get tag :name)
                    (plist-get tag :field)
                    (plist-get tag :value))))
        (when value-tags
          (message "  字段标签:")
          (dolist (tag (nreverse value-tags))
            (message "    #%s=%s" 
                    (plist-get tag :name)
                    (plist-get tag :value)))))
      
      ;; 按类型分组显示属性
      (when fields
        (let ((tag-props nil)
             (ref-props nil))
          (dolist (field fields)
            (if (string-match-p "_REF$" (car field))
                (push field ref-props)
              (push field tag-props)))
          
          (message "\n设置的属性:")
          (when tag-props
            (message "  标签属性:")
            (dolist (prop (nreverse tag-props))
              (message "    :%s: %s" (car prop) (cdr prop))))
          (when ref-props
            (message "  引用属性:")
            (dolist (prop (nreverse ref-props))
              (message "    :%s: %s" (car prop) (cdr prop))))))
      
      (when refs
        (message "\n解析出的引用:")
        (dolist (ref (nreverse refs))
          (message "  @%s.%s" (car ref) (cdr ref)))))))

;; 快捷键绑定
(define-key org-mode-map (kbd "C-c t l") 'org-supertag-quick-parse)

(provide 'org-supertag-parser)
;;; org-supertag-parser.el ends here 

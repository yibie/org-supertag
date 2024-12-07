;;; org-supertag-parser.el --- Parser for org-supertag -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)

;;------------------------------------------------------------------------------ 
;; Parse tags
;;------------------------------------------------------------------------------ 

(defconst org-supertag-tag-regex
  (rx "#"                                    ; 标签开始
      (group                                 ; 捕获组1：整个标签名
       (or
        ;; 命名空间标签: namespace/tag
        (seq (1+ (any "a-zA-Z")) 
             (zero-or-more (any "a-zA-Z0-9-_")) 
             "/"
             (1+ (any "a-zA-Z")) 
             (zero-or-more (any "a-zA-Z0-9-_")))
        ;; 组标签: group:tag
        (seq (1+ (any "a-zA-Z")) 
             (zero-or-more (any "a-zA-Z0-9-_")) 
             ":"
             (1+ (any "a-zA-Z")) 
             (zero-or-more (any "a-zA-Z0-9-_")))
        ;; 基本标签: tag
        (seq (1+ (any "a-zA-Z")) 
             (zero-or-more (any "a-zA-Z0-9-_"))))))
  "标签的正则表达式模式.")

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
  "解析当前节点的完整信息，包括 ID、标签和大纲路径"
  (let* ((element (org-element-at-point))
         (tags-info (org-supertag-parse-node-get-tags))
         (props (org-supertag-parse-node-props element)))
    (append props tags-info)))

;;------------------------------------------------------------------------------ 
;; Detect changes
;;------------------------------------------------------------------------------ 
 
(defun org-supertag-detect-changes (old-tags new-tags)
  "检测标签的变化（添加/删除）"
  (list :added (cl-set-difference new-tags old-tags :test #'equal)
        :removed (cl-set-difference old-tags new-tags :test #'equal)))

(provide 'org-supertag-parser)
;;; org-supertag-parser.el ends here 
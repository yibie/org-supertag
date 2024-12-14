;;; org-supertag-parser.el --- Parse and process org-mode supertags -*- lexical-binding: t; -*-

;;; Commentary:
;; 支持以下语法：
;;
;; 1. 标签引用：
;;    #tag                    ; 基本标签引用
;;    #tag.field             ; 引用标签的字段名
;;    #tag.field=value       ; 引用标签的特定字段值
;;
;; 2. 节点引用：
;;    @node                  ; 引用节点
;;    @node.field           ; 引用节点的字段名
;;    @node.field=value     ; 引用节点的特定字段值
;;
;; 标签和引用可以出现在：
;; - 标题中
;; - 正文中
;; - 属性抽屉中

;;; Code:

(require 'org)
(require 'org-element)

(defconst org-supertag-tag-regex
  (rx "#"                           ; 标签开始
      (group                        ; 捕获组1：标签名
       (1+ (any "a-zA-Z"))
       (zero-or-more (any "a-zA-Z0-9-_")))
      (optional                     ; 字段部分（可选）
       "."
       (group                      ; 捕获组2：字段名
        (1+ (any "a-zA-Z0-9-_")))
       (optional                   ; 字段值部分（可选）
        "="
        (group                    ; 捕获组3：字段值
         (1+ (not (any "\n" " ")))))))
  "标签引用的正则表达式模式.")

(defconst org-supertag-ref-regex
  (rx "@"                           ; 引用开始
      (group                        ; 捕获组1：节点名
       (1+ (any "a-zA-Z"))
       (zero-or-more (any "a-zA-Z0-9-_")))
      (optional                     ; 字段部分（可选）
       "."
       (group                      ; 捕获组2：字段名
        (1+ (any "a-zA-Z0-9-_")))
       (optional                   ; 字段值部分（可选）
        "="
        (group                    ; 捕获组3：字段值
         (1+ (not (any "\n" " ")))))))
  "节点引用的正则表达式模式.")

(defun org-supertag-parse-element-content (element)
  "解析元素的内容.
ELEMENT 是 org-element 对象
返回 plist:
:tags   - 标签引用列表，每个元素是 (:tag :field :value)
:refs   - 节点引用列表，每个元素是 (:node :field :value)"
  (let* ((content (org-element-property :raw-value element))
         (tags nil)
         (refs nil))
    (when content  ; 添加内容存在性检查
      (save-match-data
        ;; 解析标签引用
        (let ((start 0))
          (while (string-match org-supertag-tag-regex content start)
            (push (list :tag (match-string 1 content)
                       :field (match-string 2 content)
                       :value (match-string 3 content))
                  tags)
            (setq start (match-end 0))))
        ;; 解析节点引用
        (let ((start 0))
          (while (string-match org-supertag-ref-regex content start)
            (push (list :node (match-string 1 content)
                       :field (match-string 2 content)
                       :value (match-string 3 content))
                  refs)
            (setq start (match-end 0))))))
    
    ;; 即使没有内容也返回空列表
    (list :tags (nreverse tags)
          :refs (nreverse refs))))

(defun org-supertag-parse-node ()
  "解析当前节点的标签和引用.
返回一个 plist: (:node-id ID :tags TAGS :refs REFS)"
  (let* ((node-id (org-id-get-create))
         (tags nil)
         (refs nil))
    ;; 解析标题
    (let* ((heading (and t (org-element-at-point)))
           (parsed (and heading (org-supertag-parse-element-content heading))))
      (when parsed
        (setq tags (append tags (plist-get parsed :tags)))
        (setq refs (append refs (plist-get parsed :refs)))))
    
    ;; 解析内容
    (save-excursion
      ;; 获取内容范围
      (let* ((content-start (save-excursion 
                             (org-end-of-meta-data t)
                             (point)))  ; 使用 point 获取实际位置
             (content-end (org-entry-end-position))
             (content (when (< content-start content-end)
                       (buffer-substring-no-properties content-start content-end)))
             (parsed (when content
                      (with-temp-buffer
                        (insert content)
                        (org-mode)
                        (goto-char (point-min))
                        (org-supertag-parse-element-content (org-element-parse-buffer))))))
        (when parsed
          (setq tags (append tags (plist-get parsed :tags)))
          (setq refs (append refs (plist-get parsed :refs))))))
    
    ;; 返回结果
    (list :node-id node-id
          :tags (delete-dups tags))))  

(provide 'org-supertag-parser)
;;; org-supertag-parser.el ends here 

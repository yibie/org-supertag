;;; org-supertag-node.el --- Node management for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供节点的核心功能，包括：
;; - 节点结构定义
;; - 节点操作（创建、移动、复制、删除）
;; - 节点监控

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-id)
(require 'org-supertag-base)  ; 使用基础模块
(require 'org-supertag-api)
(require 'org-supertag-parser)
(require 'org-supertag-sync)

;;; 节点结构定义
(cl-defstruct (org-supertag-node (:constructor org-supertag-node--create)
                                (:copier nil)
                                (:conc-name org-supertag-node-))
  "Org-supertag 节点对象."
  
  ;; 基础标识
  (id          nil :type string  :documentation "节点ID")
  (title       nil :type string  :documentation
               "Return the node's heading, or #+title if it is not a subtree.")
  (file-title  nil :type string  :documentation
               "Return the #+title of the file where this node is. May be nil.")
  
  ;; 位置追踪
  (file-path   nil :type string  :documentation "文件路径")
  (olp         nil :type list    :documentation
               "Return list of ancestor headings to this node.")
  (pos         nil :type integer :documentation "节点位置")
  (level       nil :type integer :documentation
               "Return number of stars in the node heading. File-level node always 0.")

  ;; 任务信息
  (scheduled   nil :type string  :documentation
               "Return node's SCHEDULED state.")
  (priority    nil :type string  :documentation
               "Return priority such as [#A], as a string.")
  (todo        nil :type string  :documentation
               "Return node's TODO state.")
  
  ;; 核心数据
  (tags        nil :type list    :documentation "标签列表")
  (properties  nil :type list    :documentation "属性列表")
  
  ;; 引用关系 - 允许修改
  (refs-to     nil :type list    :documentation "引用的节点")
  (refs-from   nil :type list    :documentation "被引用的节点")
  (ref-count   0   :type integer :documentation "引用计数"))

;;; 节点创建和解析
(defun org-supertag-node-create (id props)
  "创建节点.
ID 是节点唯一标识
PROPS 是节点属性"
  ;; 确保基本属性存在
  (unless (plist-get props :title)
    (error "Node must have a :title property"))
  
  ;; 构建节点属性
  (let ((node-props (append (copy-sequence props)  ; 复制以避免修改原始属性
                           `(:id ,id
                             :type :node))))
    ;; 存储节点
    (org-supertag-db-put id node-props)
    ;; 返回创建的节点
    node-props))

(defun org-supertag-node-get (id)
  "获取节点.
ID 是节点唯一标识"
  (let ((node (org-supertag-db-get id)))
    (when (and node (eq (plist-get node :type) :node))
      node)))

(defun org-supertag-node-get-metadata (id)
  "获取节点元数据.
ID 是节点唯一标识"
  (let ((node (org-supertag-db-get id)))
    (when (and node (eq (plist-get node :type) :node))
      ;; 直接返回所有属性，除了 :type 和 :id
      (let ((metadata (copy-sequence node)))
        (cl-remf metadata :type)
        (cl-remf metadata :id)
        metadata))))

(defun org-supertag-node-update-metadata (id props)
  "更新节点元数据.
ID 是节点唯一标识
PROPS 是要更新的属性"
  (when-let ((node (org-supertag-db-get id)))
    (when (eq (plist-get node :type) :node)
      ;; 创建新的节点属性，合并原有属性和新属性
      (let ((new-node (copy-sequence node)))
        ;; 更新属
        (while props
          (let ((key (pop props))
                (value (pop props)))
            (setq new-node (plist-put new-node key value))))
        ;; 保存更新后的节点
        (org-supertag-db-put id new-node)
        t))))

(defun org-supertag-node-at-point ()
  "获取当前位置的节点信息.
返回一个新的 org-supertag-node 对象，如果当前位置不是有效的节点则返回 nil"
  (when-let* ((element (org-element-at-point))
              (node-id (org-id-get-create)))
    (org-supertag-node--parse-element element node-id)))

;;; 节点操作处理
(defun org-supertag-node--handle-delete (node)
  "处理节点的删除操作.
NODE 是要删除的节点."
  (when-let ((node-id (org-supertag-node-id node)))
    ;; 1. 获取节点的引用关系
    (let ((refs-to (org-supertag-node-refs-to node))
          (refs-from (org-supertag-node-refs-from node)))
      
      ;; 2. 清理引用关系
      (dolist (ref refs-to)
        (org-supertag-remove-reference node-id ref))
      (dolist (ref refs-from)
        (org-supertag-remove-reference ref node-id))
      
      ;; 3. 清理标签关系
      (dolist (tag (org-supertag-node-tags node))
        (org-supertag-remove-tag node-id tag))
      
      ;; 4. 删除节点记录
      (org-supertag-delete-node node-id))))

(defun org-supertag-node--detect-operation ()
  "检测当前操作类型（移动、复制或删除）.
返回值:
- 'move 表示移动操作
- 'copy 表示复制操作
- 'delete 表示删除操作
- nil 表示无法确定"
  (let ((this-command-keys (this-command-keys-vector)))
    (cond
     ;; org-refile 移动
     ((eq this-command 'org-refile) 'move)
     ;; org-copy 复制
     ((eq this-command 'org-copy) 'copy)
     ;; org-cut 剪切（移动）
     ((eq this-command 'org-cut) 'move)
     ;; org-paste 粘贴（要根据之的操作判断）
     ((eq this-command 'org-paste)
      (if (eq last-command 'org-cut)
          'move
        'copy))
     ;; 删除操作
     ((memq this-command '(org-cut-subtree org-delete-backward-char org-delete-char)) 'delete)
     ;; 其他情况返回 nil
     (t nil))))

(defun org-supertag-node--handle-operation (node operation)
  "处理节点的移动、复制或删除操作.
NODE 是要处理的节点
OPERATION 是操作类型，可以是 'move、'copy 或 'delete"
  (let ((node-id (org-supertag-node-id node)))
    (pcase operation
      ('move
       ;; 移动操作：保持原有 ID
       (org-supertag-sync-node-at-point))  ; 使用同步模块的函数
      ('copy
       ;; 复制操作：生成新 ID
       (org-id-get-create)
       (org-supertag-sync-node-at-point))  ; 使用同步模块的函数
      ('delete
       ;; 删除操作：清理节点及其关系
       (org-supertag-node--handle-delete node))
      (_ (error "Unknown operation: %s" operation)))))

;;; 节点同步功能

(defvar org-supertag-node--before-change-data nil
  "存储变更前的节点数据.")

(defun org-supertag-node-sync-at-point ()
  "同步当前位置的节点."
  (save-excursion
    (org-back-to-heading t)
    (let* ((node-id (org-id-get-create))
           (element (org-element-at-point))
           (props (org-supertag--extract-node-props element)))
      (when (org-supertag-sync-entity node-id props)
        node-id))))

(defun org-supertag-node-sync-tags (node-id tags)
  "同步节点的标签.
NODE-ID 是节点 ID
TAGS 是标签列表"
  (let ((existing-tags (org-supertag-node-get-tags node-id))
        (new-tags (mapcar #'org-supertag-node--normalize-tag-name tags)))
    ;; 移除不再存在的标签
    (dolist (old-tag existing-tags)
      (unless (member old-tag new-tags)
        (org-supertag-sync-relation :node-tag node-id old-tag)))
    ;; 添加新标签
    (dolist (new-tag new-tags)
      (unless (member new-tag existing-tags)
        (org-supertag-sync-relation :node-tag node-id new-tag)))))

(defun org-supertag-node--normalize-tag-name (tag)
  "标准标签名称.
TAG 是原始标签名"
  (concat "#" (downcase (if (string-prefix-p "#" tag)
                           (substring tag 1)
                         tag))))

;;; 引用序列化

(defun org-supertag-node-serialize-references (node)
  "序列化节点的引用关系.
NODE 是节点对象"
  (unless (org-supertag-node-p node)
    (error "Node must be an org-supertag-node object"))
  
  (let* ((refs-to (org-supertag-node-refs-to node))
         (refs-from (org-supertag-node-refs-from node))
         (ref-count (org-supertag-node-ref-count node)))
    ;; 将引用关系序列化为属性
    (org-entry-put (point) "REFS_TO" (mapconcat #'identity refs-to " "))
    (org-entry-put (point) "REFS_FROM" (mapconcat #'identity refs-from " "))
    (org-entry-put (point) "REF_COUNT" (number-to-string ref-count))))

(defun org-supertag-node-deserialize-references (node)
  "反序列化节点的引用关系.
NODE 是节点对象"
  (unless (org-supertag-node-p node)
    (error "Node must be an org-supertag-node object"))
  
  (let* ((refs-to-str (org-entry-get (point) "REFS_TO"))
         (refs-from-str (org-entry-get (point) "REFS_FROM"))
         (ref-count-str (org-entry-get (point) "REF_COUNT"))
         (refs-to (when refs-to-str
                   (split-string refs-to-str " " t)))
         (refs-from (when refs-from-str
                     (split-string refs-from-str " " t)))
         (ref-count (when ref-count-str
                     (string-to-number ref-count-str))))
    ;; 更新节点的引用关系
    (setf (org-supertag-node-refs-to node) refs-to)
    (setf (org-supertag-node-refs-from node) refs-from)
    (setf (org-supertag-node-ref-count node) (or ref-count 0))))

;;; 引用缓存

(defvar org-supertag-node--reference-cache (make-hash-table :test 'equal)
  "节点引用关系的缓存.
键是节点ID，值是一个 plist:
(:refs-to [引用的节点ID列表]
 :refs-from [被引用的节点ID列表]
 :ref-count [引用计数]
 :timestamp [最后更新时间戳])")

(defvar org-supertag-node-cache-ttl 300
  "引用缓存的有效期（秒）.")

(defun org-supertag-node--cache-get (node-id)
  "从缓存获取节点的引用关系.
NODE-ID 是节点ID"
  (let ((cache (gethash node-id org-supertag-node--reference-cache)))
    (when cache
      (let ((timestamp (plist-get cache :timestamp))
            (now (float-time)))
        (if (< (- now timestamp) org-supertag-node-cache-ttl)
            ;; 缓存有效
            cache
          ;; 缓存过期，移除
          (remhash node-id org-supertag-node--reference-cache)
          nil)))))

(defun org-supertag-node--cache-put (node-id refs-to refs-from ref-count)
  "更新节点引用关系的缓存.
NODE-ID 是节点ID
REFS-TO 是引用的节点ID列表
REFS-FROM 是被引用的节点ID列表
REF-COUNT 是引用计数"
  (puthash node-id
           (list :refs-to refs-to
                 :refs-from refs-from
                 :ref-count ref-count
                 :timestamp (float-time))
           org-supertag-node--reference-cache))

(defun org-supertag-node--cache-invalidate (node-id)
  "使节点的引用缓存失效.
NODE-ID 是节点ID"
  (remhash node-id org-supertag-node--reference-cache))

(defun org-supertag-node--cache-clear ()
  "清空引用缓存."
  (clrhash org-supertag-node--reference-cache))

;;; 引用操作改进

(defun org-supertag-node-get-references (node &optional reverse)
  "获取节点的引用关系.
NODE 是节点对象
当 REVERSE 非空时，获取反向引用"
  (unless (org-supertag-node-p node)
    (error "Node must be an org-supertag-node object"))
  
  (let* ((node-id (org-supertag-node-id node))
         (cache (org-supertag-node--cache-get node-id)))
    (if cache
        ;; 使用缓存
        (if reverse
            (plist-get cache :refs-from)
          (plist-get cache :refs-to))
      ;; 缓存未命中，重新加载
      (let ((refs-to (org-supertag-node-refs-to node))
            (refs-from (org-supertag-node-refs-from node))
            (ref-count (org-supertag-node-ref-count node)))
        ;; 更新缓存
        (org-supertag-node--cache-put node-id refs-to refs-from ref-count)
        (if reverse refs-from refs-to)))))

(defun org-supertag-node-create-reference (source target)
  "创建节点引用关系.
SOURCE 是引用源节点
TARGET 是被引用节点"
  (unless (and (org-supertag-node-p source)
               (org-supertag-node-p target))
    (error "Source and target must be org-supertag-node objects"))
  
  ;; 检查是否已存在引用
  (unless (member (org-supertag-node-id target)
                 (org-supertag-node-refs-to source))
    ;; 更新源节点的引用
    (setf (org-supertag-node-refs-to source)
          (cons (org-supertag-node-id target)
                (org-supertag-node-refs-to source)))
    ;; 更新目标节点的被引用
    (setf (org-supertag-node-refs-from target)
          (cons (org-supertag-node-id source)
                (org-supertag-node-refs-from target)))
    ;; 更新引用计数
    (setf (org-supertag-node-ref-count target)
          (1+ (org-supertag-node-ref-count target)))
    ;; 使缓存失效
    (org-supertag-node--cache-invalidate (org-supertag-node-id source))
    (org-supertag-node--cache-invalidate (org-supertag-node-id target))))

(defun org-supertag-node-remove-reference (source target)
  "移除节点引用关系.
SOURCE 是引用源节点
TARGET 是被引用节点"
  (unless (and (org-supertag-node-p source)
               (org-supertag-node-p target))
    (error "Source and target must be org-supertag-node objects"))
  
  ;; 检查是否存在引用
  (when (member (org-supertag-node-id target)
                (org-supertag-node-refs-to source))
    ;; 更新源节点的引用
    (setf (org-supertag-node-refs-to source)
          (delete (org-supertag-node-id target)
                 (org-supertag-node-refs-to source)))
    ;; 更新目标节点的被引用
    (setf (org-supertag-node-refs-from target)
          (delete (org-supertag-node-id source)
                 (org-supertag-node-refs-from target)))
    ;; 更新引用计数
    (setf (org-supertag-node-ref-count target)
          (1- (org-supertag-node-ref-count target)))
    ;; 使缓存失效
    (org-supertag-node--cache-invalidate (org-supertag-node-id source))
    (org-supertag-node--cache-invalidate (org-supertag-node-id target))))

;; 修改同步函数以使用缓存
(defun org-supertag-node-sync-references (node)
  "同步节点的引用关系.
NODE 是节点对象"
  (unless (org-supertag-node-p node)
    (error "Node must be an org-supertag-node object"))
  
  (let ((node-id (org-supertag-node-id node)))
    ;; 使当前节点的缓存失效
    (when (fboundp 'org-supertag-node--cache-invalidate)
      (org-supertag-node--cache-invalidate node-id))
    
    ;; 同步引用到的节点
    (dolist (ref-id (org-supertag-node-refs-to node))
      (when-let ((ref-node (org-supertag-node--create-from-id ref-id)))
        ;; 使引用节点的缓存失效
        (when (fboundp 'org-supertag-node--cache-invalidate)
          (org-supertag-node--cache-invalidate ref-id))
        ;; 确保反向引用存在
        (unless (member node-id (org-supertag-node-refs-from ref-node))
          (setf (org-supertag-node-refs-from ref-node)
                (cons node-id (org-supertag-node-refs-from ref-node)))
          ;; 更新引用计数
          (setf (org-supertag-node-ref-count ref-node)
                (1+ (org-supertag-node-ref-count ref-node)))
          ;; 序列化更新
          (org-supertag-node-serialize-references ref-node))))
    
    ;; 同步被引用的节点
    (dolist (ref-id (org-supertag-node-refs-from node))
      (when-let ((ref-node (org-supertag-node--create-from-id ref-id)))
        ;; 使引用节点的缓存失效
        (when (fboundp 'org-supertag-node--cache-invalidate)
          (org-supertag-node--cache-invalidate ref-id))
        ;; 确保正向引用存在
        (unless (member node-id (org-supertag-node-refs-to ref-node))
          (setf (org-supertag-node-refs-to ref-node)
                (cons node-id (org-supertag-node-refs-to ref-node)))
          ;; 序列化更新
          (org-supertag-node-serialize-references ref-node))))))

;; 在文件保存时清空缓存（如果缓存系统可用）
(add-hook 'after-save-hook
          (lambda ()
            (when (and (eq major-mode 'org-mode)
                      (fboundp 'org-supertag-node--cache-clear))
              (org-supertag-node--cache-clear))))


;;; 引用同步
(defun org-supertag-node-sync-references (node)
  "同步节点的引用关系.
NODE 是节点对象"
  (unless (org-supertag-node-p node)
    (error "Node must be an org-supertag-node object"))
  
  ;; 同步引用到的节点
  (dolist (ref-id (org-supertag-node-refs-to node))
    (when-let ((ref-node (org-supertag-node--create-from-id ref-id)))
      ;; 确保反向引用存在
      (unless (member (org-supertag-node-id node)
                     (org-supertag-node-refs-from ref-node))
        (setf (org-supertag-node-refs-from ref-node)
              (cons (org-supertag-node-id node)
                    (org-supertag-node-refs-from ref-node)))
        ;; 更新引用计数
        (setf (org-supertag-node-ref-count ref-node)
              (1+ (org-supertag-node-ref-count ref-node)))
        ;; 序列化更新
        (org-supertag-node-serialize-references ref-node))))
  
  ;; 同步被引用的节点
  (dolist (ref-id (org-supertag-node-refs-from node))
    (when-let ((ref-node (org-supertag-node--create-from-id ref-id)))
      ;; 确保正向引用存在
      (unless (member (org-supertag-node-id node)
                     (org-supertag-node-refs-to ref-node))
        (setf (org-supertag-node-refs-to ref-node)
              (cons (org-supertag-node-id node)
                    (org-supertag-node-refs-to ref-node)))
        ;; 序列化更新
        (org-supertag-node-serialize-references ref-node)))))

;; 引用显示样式
;; 定制引用显示样式
(defface org-supertag-reference-count
  '((t (:inherit org-priority :height 0.9)))
  "引用计数的显示样式.")

(defface org-supertag-reference-section
  '((t (:inherit org-drawer :height 1.1 :weight bold)))
  "引用区域标题的显示样式.")

(defun org-supertag-node-format-reference (node-id)
  "格式化节点引用显示.
NODE-ID 是节点ID"
  (when-let ((node (org-supertag-node--create-from-id node-id)))
    (let* ((title (org-supertag-node-title node))
           (tags (org-supertag-node-tags node))
           (ref-count (org-supertag-node-ref-count node))
           (tag-str (if tags
                       (format " %s"
                               (mapconcat (lambda (tag)
                                          (propertize tag
                                                    'face 'org-tag))
                                        tags " "))
                     ""))
           (ref-str (if (> ref-count 0)
                       (format " [%d refs]"
                               (propertize (number-to-string ref-count)
                                         'face 'org-priority))
                     "")))
      (format "- [[id:%s][%s]]%s%s"
              node-id
              (propertize title 'face 'org-link)
              tag-str
              ref-str))))

(defun org-supertag-node--update-reference-display-enhanced (node)
  "增强的引用显示更新函数.
NODE 是节点对象"
  (let ((refs-to (org-supertag-node-get-references node))
        (refs-from (org-supertag-node-get-references node t)))
    (when (or refs-to refs-from)
      (org-with-wide-buffer
       (org-back-to-heading t)
       (let ((end (save-excursion
                   (outline-next-heading)
                   (point))))
         ;; 删除现有的引用区域
         (save-excursion
           (while (re-search-forward "^[ \t]*:REFERENCES:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?" end t)
             (replace-match "")))
         ;; 创建新的引用区域
         (save-excursion
           (end-of-line)
           (insert "\n:REFERENCES:\n")
           ;; 引用到的节点
           (when refs-to
             (insert (propertize "引用到：\n" 'face 'org-supertag-reference-section))
             (dolist (ref-id refs-to)
               (insert (org-supertag-node-format-reference ref-id) "\n")))
           ;; 被引用的节点
           (when refs-from
             (when refs-to (insert "\n"))
             (insert (propertize "被引用：\n" 'face 'org-supertag-reference-section))
             (dolist (ref-id refs-from)
               (insert (org-supertag-node-format-reference ref-id) "\n")))
           (insert ":END:\n")))))))


;; 修改现有的更新显示函数以使用增强版本
(defun org-supertag-node-update-reference-display (node)
  "更新节点的引用显示.
NODE 是节点对象"
  (unless (org-supertag-node-p node)
    (error "Node must be an org-supertag-node object"))
  
  ;; 调用原始的基本显示更新
  (let ((refs-to (org-supertag-node-refs-to node)))
    (when refs-to
      (org-with-wide-buffer
       (org-back-to-heading t)
       (let ((end (save-excursion
                   (outline-next-heading)
                   (point))))
         (save-excursion
           (while (re-search-forward "^[ \t]*:REFERENCES:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?" end t)
             (replace-match "")))
         (save-excursion
           (end-of-line)
           (insert "\n:REFERENCES:\n")
           (dolist (ref-id refs-to)
             (when-let ((ref-node (org-supertag-node--create-from-id ref-id)))
               (insert (format "- [[id:%s][%s]]\n"
                             (org-supertag-node-id ref-node)
                             (org-supertag-node-title ref-node)))))
           (insert ":END:\n"))))))
  
  ;; 调用增强版本的显示更新
  (org-supertag-node--update-reference-display-enhanced node))

(defun org-supertag-node-sync-all-references ()
  "同步所有节点的引用关系."
  (interactive)
  (org-map-entries
   (lambda ()
     (when-let* ((id (org-id-get))
                 (node (org-supertag-node--create-from-id id)))
       (org-supertag-node-sync-references node)
       (org-supertag-node-update-reference-display node)))
   nil nil))

;; 添加到变更监听
(add-hook 'org-supertag-node-after-change-functions
          #'org-supertag-node-sync-references)
(add-hook 'org-supertag-node-after-change-functions
          #'org-supertag-node-update-reference-display)

;;; 节点引用功能

(defun org-supertag-node-create-reference (source target)
  "创建节点引用关系.
SOURCE 是引用源节点
TARGET 是被引用节点"
  (unless (and (org-supertag-node-p source)
               (org-supertag-node-p target))
    (error "Source and target must be org-supertag-node objects"))
  
  ;; 检查是否已存在引用
  (unless (member (org-supertag-node-id target)
                 (org-supertag-node-refs-to source))
    ;; 更新源节点的引用
    (setf (org-supertag-node-refs-to source)
          (cons (org-supertag-node-id target)
                (org-supertag-node-refs-to source)))
    ;; 更新目标节点的被引用
    (setf (org-supertag-node-refs-from target)
          (cons (org-supertag-node-id source)
                (org-supertag-node-refs-from target)))
    ;; 更新引用计数
    (setf (org-supertag-node-ref-count target)
          (1+ (org-supertag-node-ref-count target)))))

(defun org-supertag-node-remove-reference (source target)
  "移除节点引用关系.
SOURCE 是引用源节点
TARGET 是被引用节点"
  (unless (and (org-supertag-node-p source)
               (org-supertag-node-p target))
    (error "Source and target must be org-supertag-node objects"))
  
  ;; 检查是否存在引用
  (when (member (org-supertag-node-id target)
                (org-supertag-node-refs-to source))
    ;; 更新源节点的引用
    (setf (org-supertag-node-refs-to source)
          (delete (org-supertag-node-id target)
                 (org-supertag-node-refs-to source)))
    ;; 更新目标节点的被引用
    (setf (org-supertag-node-refs-from target)
          (delete (org-supertag-node-id source)
                 (org-supertag-node-refs-from target)))
    ;; 更新引用计数
    (setf (org-supertag-node-ref-count target)
          (1- (org-supertag-node-ref-count target)))))

(defun org-supertag-node-get-references (node &optional reverse)
  "获取节点的引用关系.
NODE 是节点对象
当 REVERSE 非空时，获取反向引用"
  (unless (org-supertag-node-p node)
    (error "Node must be an org-supertag-node object"))
  
  (if reverse
      (org-supertag-node-refs-from node)
    (org-supertag-node-refs-to node)))

(defun org-supertag-node-reference-at-point ()
  "在当前位置创建节点引用."
  (interactive)
  (when-let* ((source-id (org-id-get))
              (target-id (org-id-get-with-outline-path-completion))
              (source (org-supertag-node--create-from-id source-id))
              (target (org-supertag-node--create-from-id target-id)))
    (org-supertag-node-create-reference source target)))

(defun org-supertag-node--create-from-id (id)
  "从ID创建节点对象."
  (when-let* ((marker (org-id-find id t))
              (buffer (marker-buffer marker))
              (pos (marker-position marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (org-supertag-node--create-at-point)))))

(defun org-supertag-node--create-at-point ()
  "在当前位置创建节点对象."
  (unless (org-at-heading-p)
    (error "Must be at heading"))
  
  (let* ((id (org-id-get-create))
         (heading (org-get-heading t t t t))
         (olp (org-get-outline-path t t))
         (file (buffer-file-name))
         (level (org-current-level))
         (todo (org-get-todo-state))
         (priority (org-get-priority (org-get-heading t t)))
         (scheduled (org-get-scheduled-time (point)))
         (tags (org-get-tags))
         (properties (org-entry-properties)))
    
    (org-supertag-node--create
     :id id
     :title heading
     :file-title (org-get-title)
     :file-path file
     :olp olp
     :pos (point)
     :level level
     :todo todo
     :priority priority
     :scheduled scheduled
     :tags tags
     :properties properties
     :refs-to nil
     :refs-from nil
     :ref-count 0)))

;;; 变更监听

(defun org-supertag-node--before-change (beg end)
  "变更前的处理函数.
BEG 和 END 是变更范围"
  (when (org-at-heading-p)
    (setq org-supertag-node--before-change-data
          `(:id ,(org-id-get)
            :tags ,(org-get-tags nil t)
            :properties ,(org-entry-properties)))))

(defun org-supertag-node--after-change ()
  "变更后的处理函数."
  (when (and org-supertag-node--before-change-data
             (org-at-heading-p))
    (let ((before-id (plist-get org-supertag-node--before-change-data :id))
          (current-id (org-id-get)))
      (when (and before-id (equal before-id current-id))
        (org-supertag-node-sync-at-point)))
    (setq org-supertag-node--before-change-data nil)))

;;; 自动同步模式

(define-minor-mode org-supertag-node-sync-mode
  "自动同步 org 节点变化到 supertag 系统."
  :lighter " SuperTag[Node]"
  :global nil
  (if org-supertag-node-sync-mode
      (progn
        (org-supertag-sync-add-before-change-function #'org-supertag-node--before-change)
        (org-supertag-sync-add-after-change-function #'org-supertag-node--after-change)
        (org-supertag-sync-setup-hooks))
    (org-supertag-sync-remove-hooks)))

;;; 用户命令

(defun org-supertag-node-create-at-point ()
  "在当前位置创建一个新的 supertag 节点."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "光标必须在标题处"))
  (when (org-id-get)
    (user-error "该标题已经有一个节点"))
  (org-supertag-node-sync-at-point))

;;; 节点标签操作

(defun org-supertag-node--insert-tags (node-id)
  "为节点添加标签.
NODE-ID 是节点ID"
  (let* ((available-tags (mapcar #'car (org-supertag-find-entities :tag)))
         (selected-tags (completing-read-multiple
                        "Tags: "
                        available-tags)))
    (dolist (tag selected-tags)
      (org-supertag-add-tag node-id tag))))

(defun org-supertag-node--set-field (node-id)
  "为节点设置字段值.
NODE-ID 是节点ID"
  (let* ((available-fields (mapcar #'car (org-supertag-find-entities :field)))
         (field (completing-read
                "Field: "
                available-fields))
         (value (read-string "Value: ")))
    (org-supertag-set-field node-id field value)))

;;; 节点创建

(defun org-supertag-node-get-tags (node-id)
  "获取节点的标签列表.
NODE-ID 是节点ID"
  (org-supertag-get-node-tags node-id))

(defun org-supertag-node-update-at-point ()
  "更新当前位置的节点."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "光标必须在标题处"))
  (unless (org-id-get)
    (user-error "该标题没有关联的节点"))
  (org-supertag-node-sync-at-point))

(defun org-supertag-node-get-props ()
  "获取当前节点的属性."
  (let* ((element (org-element-at-point))
         (level (org-element-property :level element))
         (title (org-element-property :raw-value element))
         (olp (org-get-outline-path)))
    (message "Debug - Extracting props for level %d node: %s" level title)
    (let ((props (list :type :node
                      :title title
                      :level level
                      :olp olp)))
      (message "Debug - Node properties before return: %S" props)
      props)))

(defun org-supertag-update-to-point ()
  "更新当前位置的节点."
  (interactive)
  (message "Debug - Starting update-to-point")
  (save-excursion
    (message "Debug - Moving to heading")
    (org-back-to-heading t)
    (message "Debug - Getting node properties")
    (let* ((node-props (org-supertag-node-get-props))
           (_ (message "Debug - Got node props: %S" node-props)
           (node-id (org-id-get-create))
           (_ (message "Debug - Got node ID: %s" node-id)))
      (org-supertag-update-entity node-id node-props)))))

(provide 'org-supertag-node)
;;; org-supertag-node.el ends here

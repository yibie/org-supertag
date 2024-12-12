;;; org-supertag-db.el --- Database layer for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供最基础的数据存储和查询功能
;; 核心理念：一切皆实体，一切皆关系
;; 实体是关系中的节点，关系是节点之间的连接
;; 通过关系连接实体 - 使用 type、from、to 来表达关系
;; 本文件不支持【破坏性更新】，如固有函数不足以支持功能，则实现新函数

;;; Code:

(require 'ht)
(require 'org-supertag-base)


;;------------------------------------------------------------------------------
;; 核心数据表
;;------------------------------------------------------------------------------  
(defvar org-supertag-db--entities (ht-create)
  "实体存储 id -> plist.")

(defvar org-supertag-db--relations (ht-create)
  "关系存储 rel-id -> (type from to props).")

(defvar org-supertag-db--field-values (ht-create)
  "字段值存储 field-id:node-id -> value.")

;;------------------------------------------------------------------------------
;; 配置
;;------------------------------------------------------------------------------  
(defcustom org-supertag-db-file
  (org-supertag-data-file "db.el")
  "数据库文件路径."
  :type 'file
  :group 'org-supertag)

(defcustom org-supertag-db-backup-directory
  (org-supertag-data-file "backups")
  "数据库备份目录."
  :type 'directory
  :group 'org-supertag)

(defcustom org-supertag-db-backup-max-days 3
  "保留备份的最大天数."
  :type 'integer
  :group 'org-supertag)

(defun org-supertag-db-ensure-directories ()
  "确保数据库相关目录存在."
  (org-supertag-ensure-data-directory)
  (unless (file-exists-p org-supertag-db-backup-directory)
    (make-directory org-supertag-db-backup-directory t)))

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
;; 数据保存和加载
;;------------------------------------------------------------------------------  
(defun org-supertag-db-save ()
  "保存数据到文件."
  (interactive)  ; 添加交互式调用
  (org-supertag-db-ensure-directories)
  (org-supertag-db-backup)
  (let ((print-level nil)
        (print-length nil))
    (with-temp-file org-supertag-db-file  ; 使用 with-temp-file 而不是 write-region
      (message "保存数据库到: %s" org-supertag-db-file)
      (message "当前实体数: %d" (ht-size org-supertag-db--entities))
      ;; 打印数据结构
      (pp `(setq org-supertag-db--entities (ht<-alist ',(ht->alist org-supertag-db--entities))
                 org-supertag-db--relations (ht<-alist ',(ht->alist org-supertag-db--relations))
                 org-supertag-db--field-values (ht<-alist ',(ht->alist org-supertag-db--field-values)))
          (current-buffer)))
    (message "数据库保存完成")))

(defun org-supertag-db-load ()
  "从文件加载数据."
  (interactive)  ; 添加交互式调用
  (org-supertag-db-ensure-directories)
  (if (file-exists-p org-supertag-db-file)
      (condition-case err
          (with-temp-buffer
            (message "从文件加载数据库: %s" org-supertag-db-file)
            (insert-file-contents org-supertag-db-file)
            (eval (read (current-buffer)))
            (message "数据库加载成功：%d 个实体, %d 个关系, %d 个字段值"
                    (ht-size org-supertag-db--entities)
                    (ht-size org-supertag-db--relations)
                    (ht-size org-supertag-db--field-values)))
        (error
         (message "数据库加载失败: %S" err)
         (org-supertag-db-initialize)))
    (progn
      (message "数据库文件不存在，初始化空数据库")
      (org-supertag-db-initialize))))

(defun org-supertag-db-ready-p ()
  "检查数据库是否已正确初始化."
  (and (hash-table-p org-supertag-db--entities)
       (hash-table-p org-supertag-db--relations)
       (hash-table-p org-supertag-db--field-values)))

(defun org-supertag-db-ensure-ready ()
  "确保数据库已准备就绪."
  (unless (org-supertag-db-ready-p)
    (org-supertag-db-initialize)))   
;;------------------------------------------------------------------------------
;; 初始化
;;------------------------------------------------------------------------------  
(defun org-supertag-db-initialize ()
  "初始化空数据库."
  (message "初始化空数据库...")
  (setq org-supertag-db--entities (ht-create)
        org-supertag-db--relations (ht-create)
        org-supertag-db--field-values (ht-create)))



;;------------------------------------------------------------------------------ 
;; Core API
;;------------------------------------------------------------------------------ 

(defun org-supertag-db-put (id props)
  "存储实体.
ID: 实体唯一标识
PROPS: 属性列表，必须包含 :type 属性"
  (unless (plist-get props :type)
    (error "Entity must have a :type property"))
  (message "存储实体: %s -> %S" id props)  ; 添加调试信息
  (ht-set! org-supertag-db--entities id props)
  (message "当前实体数: %d" (ht-size org-supertag-db--entities))
  id)

(defun org-supertag-db-get (id)
  "获取实体.
返回实体的属性列表，如果不存在返回 nil"
  (ht-get org-supertag-db--entities id))


(defun org-supertag-db-link (type from to &optional props)
  "创建关系.
TYPE: 关系类型 (node-tag, node-field, tag-field 等)
FROM: 源实体ID
TO: 目标实体ID
PROPS: 关系属性"
  (let ((rel-id (format "%s:%s->%s" type from to)))
    (message "创建关系: %s" rel-id)  ; 添加调试信息
    (ht-set! org-supertag-db--relations rel-id 
             (list type from to props))
    (message "当前关系数: %d" (ht-size org-supertag-db--relations))
    t))
(defun org-supertag-db-get-links (type from)
  "获取关系.
TYPE: 关系类型
FROM: 源实体ID
返回 ((to props) ...) 形式的列表"
  (let (results)
    (ht-map (lambda (k v)
              (when (and (equal type (car v))
                        (equal from (cadr v)))
                (push (list (nth 2 v) (nth 3 v)) results)))
            org-supertag-db--relations)
    (message "Debug - Relations: %S" (ht-items org-supertag-db--relations))
    (nreverse results)))

(defun org-supertag-db-get-entities ()
  "获取所有实体的哈希表."
  org-supertag-db--entities)

(defun org-supertag-db-get-entities-alist ()
  "获取所有实体的关联列表 ((id . props) ...)."
  (ht-items org-supertag-db--entities))

(defun org-supertag-db-find-entities (type)
  "查找指定类型的所有实体.
TYPE 是实体类型"
  (let (results)
    (ht-map (lambda (id props)
              (when (eq (plist-get props :type) type)
                (push id results)))
            org-supertag-db--entities)
    results))

;; 获取所有关系
(defun org-supertag-db-get-all-relations ()
  "获取所有关系.
返回 ((type from to props) ...) 形式的列表"
  (let (results)
    (ht-map (lambda (k v)
              (push v results))
            org-supertag-db--relations)
    results))

 
;;; 辅助函数
(defun org-supertag-db-exists-p (id)
  "检查实体是否存在."
  (ht-contains-p org-supertag-db--entities id))

(defun org-supertag-db-remove (id)
  "删除实体及其相关的所有关系."
  (when (org-supertag-db-exists-p id)
    ;; 删除实体
    (ht-remove! org-supertag-db--entities id)
    ;; 删除相关的关系
    (let (to-remove)
      (ht-map (lambda (k v)
                (when (or (equal id (nth 1 v))
                         (equal id (nth 2 v)))
                  (push k to-remove)))
              org-supertag-db--relations)
      (dolist (k to-remove)
        (ht-remove! org-supertag-db--relations k)))
    ;; 删除相关的字段值
    (let ((prefix (concat id ":")))
      (ht-map (lambda (k _)
                (when (string-prefix-p prefix k)
                  (ht-remove! org-supertag-db--field-values k)))
              org-supertag-db--field-values))
    t))

(defun org-supertag-db-unlink (type from to)
  "删除特定关系."
  (let ((rel-id (format "%s:%s->%s" type from to)))
    (when (ht-contains-p org-supertag-db--relations rel-id)
      (ht-remove! org-supertag-db--relations rel-id)
      t)))

;;------------------------------------------------------------------------------
;; 字段值操作
;;------------------------------------------------------------------------------  

(defun org-supertag-db-set-field-value (field-id node-id value)
  "设置字段值."
  (let ((key (format "%s:%s" field-id node-id)))
    (ht-set! org-supertag-db--field-values key value)))

(defun org-supertag-db-get-field-value (field-id node-id)
  "获取字段值."
  (let ((key (format "%s:%s" field-id node-id)))
    (ht-get org-supertag-db--field-values key)))

(defun org-supertag-db-remove-field-value (field-id node-id)
  "删除字段值."
  (let ((key (format "%s:%s" field-id node-id)))
    (ht-remove! org-supertag-db--field-values key)))

(defun org-supertag-db-find-field-values (pred)
  "查找满足条件的字段值.
PRED 是一个接受 (field-id . value) 作为参数的函数"
  (let (results)
    (ht-map (lambda (k v)
              (when (funcall pred k v)
                (push (cons k v) results)))
            org-supertag-db--field-values)
    (nreverse results)))

;; 按节点查询
(defun org-supertag-db-find-field-values-by-node (node-id)
  "获取节点的所有字段值."
  (org-supertag-db-find-field-values 
   (lambda (k _v)
     (string-match-p (concat ":" (regexp-quote node-id) "$")
                     k))))

;; 按字段查询
(defun org-supertag-db-find-field-values-by-field (field-id)
  "获取指定字段的所有值."
  (org-supertag-db-find-field-values
   (lambda (k _v)
     (string-match-p (concat "^" (regexp-quote field-id) ":")
                     k))))

;;; 查询功能

(defun org-supertag-db-find (pred)
  "查找满足条件的实体.
PRED 是一个接受 (id . props) 作为参数的函数"
  (let (results)
    (ht-map (lambda (k v)
              (when (funcall pred k v)
                (push (cons k v) results)))
            org-supertag-db--entities)
    (nreverse results)))

(defun org-supertag-db-find-relations (pred)
  "查找满足条件的关系.
PRED 是一个接受 (rel-id . (type from to props)) 作为参数的函数"
  (let (results)
    (ht-map (lambda (k v)
              (when (funcall pred k v)
                (push (cons k v) results)))
            org-supertag-db--relations)
    (nreverse results)))

;;; 新增函数：按属性查询
(defun org-supertag-db-find-by-props (props)
  "查找满足属性条件的实体.
PROPS 是一个 plist，包含要匹配的属性."
  (let ((pred (lambda (_k v)
                (cl-loop for (key value) on props by #'cddr
                         always (equal (plist-get v key) value)))))
    (org-supertag-db-find pred)))

;;; 新增函数：按类型查询（常用场景的便捷函数）
(defun org-supertag-db-find-by-type (type)
  "查找指定类型的实体.
TYPE 是实体类型，如 :template, :tag 等."
  (org-supertag-db-find-by-props `(:type ,type)))

;;------------------------------------------------------------------------------
;; Status
;;------------------------------------------------------------------------------    

(defun org-supertag-db-check-status ()
  "检查数据库状态."
  (interactive)
  (message "\n=== 数据库状态 ===")
  (message "数据库文件: %s" org-supertag-db-file)
  (message "文件存在: %s" (file-exists-p org-supertag-db-file))
  (message "当前实体数: %d" (ht-size org-supertag-db--entities))
  (message "当前关系数: %d" (ht-size org-supertag-db--relations))
  (message "当前字段值数: %d" (ht-size org-supertag-db--field-values)))


(defun org-supertag-db-test ()
  "测试数据库操作."
  (interactive)
  (message "\n=== 测试数据库操作 ===")
  
  ;; 1. 添加一个测试实体
  (org-supertag-db-put "test-tag" '(:type :tag :name "test"))
  (message "添加实体后大小: %d" (ht-size org-supertag-db--entities))
  
  ;; 2. 添加一个测试关系
  (org-supertag-db-link :node-tag "test-node" "test-tag")
  (message "添加关系后大小: %d" (ht-size org-supertag-db--relations))
  
  ;; 3. 保存数据库
  (org-supertag-db-save)
  (message "数据已保存")
  
  ;; 4. 重新加载
  (org-supertag-db-load)
  (message "重新加载后:")
  (message "实体数: %d" (ht-size org-supertag-db--entities))
  (message "关系数: %d" (ht-size org-supertag-db--relations)))

(provide 'org-supertag-db)
;;; org-supertag-db.el ends here


;;; org-supertag-cache.el --- Cache management for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供缓存管理功能，包括节点缓存和查询缓存

;;; Code:

(require 'org-supertag-base)

(defcustom org-supertag-node-cache-file
  (org-supertag-data-file "cache/node-cache.el")
  "节点缓存文件路径."
  :type 'file
  :group 'org-supertag)

(defcustom org-supertag-query-cache-file
  (org-supertag-data-file "cache/query-cache.el")
  "查询缓存文件路径."
  :type 'file
  :group 'org-supertag)

(defcustom org-supertag-cache-enabled t
  "是否启用缓存."
  :type 'boolean
  :group 'org-supertag)

(defcustom org-supertag-cache-max-size 1000
  "缓存最大条目数."
  :type 'integer
  :group 'org-supertag)

(defvar org-supertag-node-cache (make-hash-table :test 'equal)
  "节点缓存.")

(defvar org-supertag-query-cache (make-hash-table :test 'equal)
  "查询缓存.")

(defun org-supertag-cache-ensure-directories ()
  "确保缓存目录存在."
  (org-supertag-ensure-data-directory)
  (let ((cache-dir (file-name-directory org-supertag-node-cache-file)))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir t))))

(defun org-supertag--hash-to-alist (hash-table)
  "Convert HASH-TABLE to an alist safely."
  (let (alist)
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hash-table)
    (nreverse alist)))

(defun org-supertag-cache-save ()
  "保存缓存到文件."
  (when org-supertag-cache-enabled
    (org-supertag-cache-ensure-directories)
    (with-temp-buffer
      (let ((print-level nil)
            (print-length nil))
        ;; 保存节点缓存
        (when (file-exists-p org-supertag-node-cache-file)
          (insert-file-contents org-supertag-node-cache-file)
          (goto-char (point-max)))
        (print `(setq org-supertag-node-cache
                      (ht-merge org-supertag-node-cache
                               ',(org-supertag--hash-to-alist org-supertag-node-cache)))
               (current-buffer))
        (write-region (point-min) (point-max) org-supertag-node-cache-file nil 'silent)
        ;; 保存查询缓存
        (erase-buffer)
        (when (file-exists-p org-supertag-query-cache-file)
          (insert-file-contents org-supertag-query-cache-file)
          (goto-char (point-max)))
        (print `(setq org-supertag-query-cache
                      (ht-merge org-supertag-query-cache
                               ',(org-supertag--hash-to-alist org-supertag-query-cache)))
               (current-buffer))
        (write-region (point-min) (point-max) org-supertag-query-cache-file nil 'silent)))))

(defun org-supertag-cache-load ()
  "从文件加载缓存."
  (when org-supertag-cache-enabled
    (org-supertag-cache-ensure-directories)
    (when (file-exists-p org-supertag-node-cache-file)
      (load org-supertag-node-cache-file))
    (when (file-exists-p org-supertag-query-cache-file)
      (load org-supertag-query-cache-file))))

(defun org-supertag-cache-initialize ()
  "初始化缓存系统.
在包加载时调用此函数."
  (org-supertag-cache-ensure-directories)
  (org-supertag-cache-load))

(defun org-supertag-cache-clear ()
  "清除所有缓存."
  (clrhash org-supertag-node-cache)
  (clrhash org-supertag-query-cache))

;; 在包加载时初始化缓存
(org-supertag-cache-initialize)

(provide 'org-supertag-cache)
;;; org-supertag-cache.el ends here 
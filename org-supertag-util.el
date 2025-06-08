;; -*- lexical-binding: t; -*-
;;; org-supertag-util.el --- Utility functions for org-supertag -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Your Name <your.email@example.com>
;; Keywords: org-mode, utilities

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains utility functions for org-supertag.

;;; Code:

(require 'cl-lib)
(require 'org)

(defgroup org-supertag-util nil
  "Utility functions for org-supertag."
  :group 'org-supertag)

(defun org-supertag-util-ensure-directory (dir)
  "确保目录 DIR 存在，如果不存在则创建它。"
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun org-supertag-util-get-data-dir ()
  "获取 org-supertag 的数据目录。"
  (let ((data-dir (expand-file-name "org-supertag" user-emacs-directory)))
    (org-supertag-util-ensure-directory data-dir)
    data-dir))

(defun org-supertag-util-get-hash-file ()
  "获取哈希文件的路径。"
  (expand-file-name "hashes.el" (org-supertag-util-get-data-dir)))

(defun org-supertag-util-save-hash (node-id hash content-hash)
  "保存节点 NODE-ID 的哈希值 HASH 和内容哈希值 CONTENT-HASH。"
  (let* ((hash-file (org-supertag-util-get-hash-file))
         (hash-data (if (file-exists-p hash-file)
                       (with-temp-buffer
                         (insert-file-contents hash-file)
                         (read (buffer-string)))
                     (make-hash-table :test 'equal)))
         (node-data (make-hash-table :test 'equal)))
    (puthash 'node-hash hash node-data)
    (puthash 'content-hash content-hash node-data)
    (puthash node-id node-data hash-data)
    (with-temp-file hash-file
      (prin1 hash-data (current-buffer)))))

(defun org-supertag-util-load-hash (node-id)
  "加载节点 NODE-ID 的哈希值。
返回一个包含 node-hash 和 content-hash 的哈希表，如果不存在则返回 nil。"
  (let* ((hash-file (org-supertag-util-get-hash-file)))
    (when (file-exists-p hash-file)
      (let* ((hash-data (with-temp-buffer
                         (insert-file-contents hash-file)
                         (read (buffer-string)))))
        (gethash node-id hash-data)))))

(defun org-supertag-util-clear-hash ()
  "清除所有哈希记录。"
  (let ((hash-file (org-supertag-util-get-hash-file)))
    (when (file-exists-p hash-file)
      (delete-file hash-file))))

(defun org-supertag-util-get-node-content-for-llm ()
  "获取当前节点的内容，格式化为适合 LLM 处理的格式。
返回一个包含节点标题和内容的字符串。"
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (let* ((title (org-get-heading t t t t))  ; 获取干净的标题（无 TODO/tags/priority）
             (content-start (save-excursion
                            (org-end-of-meta-data t)  ; 跳过属性抽屉和其他元数据
                            (point)))
             (content-end (point-max))
             (raw-content (buffer-substring-no-properties content-start content-end))
             ;; 清理内容：移除多余空行，修剪空白
             (cleaned-content (replace-regexp-in-string
                             "\\([\\n]\\)[\\n]+" "\\1"  ; 将多个空行替换为单个空行
                             (string-trim raw-content))))
        ;; 组合标题和内容
        (concat "# " title "\n\n" cleaned-content)))))

(provide 'org-supertag-util)

;;; org-supertag-util.el ends here 
;;; restore-heading-ids.el --- Restore IDs for org headings -*- lexical-binding: t; -*-

;;; Commentary:
;; 这个脚本提供了独立的 heading ID 恢复功能，不依赖于数据库。
;; 主要用于为有 #tag 的 heading 创建或恢复 ID。

;;; Code:

(require 'org)
(require 'org-id)

(defun restore-heading-ids--has-tag-p (title)
  "检查 TITLE 是否包含 #tag 格式的标签。"
  (and (stringp title)
       (string-match-p "#[a-zA-Z][a-zA-Z0-9_-]*" title)))

(defun restore-heading-ids-in-file (file)
  "为指定文件中有 #tag 但没有 ID 的 heading 创建 ID。
返回 (created . total) 表示创建的 ID 数量和总的 heading 数量。
如果文件不存在或处理过程中出错，返回 nil。"
  (if (not (and (stringp file) (file-exists-p file)))
      (progn
        (message "文件不存在或无效: %s" file)
        nil)
    (let* ((created-count 0)
           (total-count 0)
           (existing-buffer (find-buffer-visiting file))
           (buffer (condition-case err
                      (or existing-buffer (find-file-noselect file t))
                    (error
                     (message "打开文件失败 %s: %s" file (error-message-string err))
                     nil))))
      (unless buffer
        (message "无法打开文件: %s" file)
        (cl-return-from restore-heading-ids-in-file nil))
      (with-current-buffer buffer
        (org-mode)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward org-heading-regexp nil t)
           (cl-incf total-count)
           (when (org-at-heading-p)
             (let ((title (org-get-heading t t t t))
                   (id (org-entry-get nil "ID")))
               (when (and (not id) (restore-heading-ids--has-tag-p title))
                 (let ((new-id (org-id-new)))
                   (org-entry-put nil "ID" new-id)
                   (org-id-add-location new-id (buffer-file-name))
                   (cl-incf created-count)
                   (message "创建 ID %s: %s" new-id title))))))))
      
      ;; 保存修改
      (when (buffer-modified-p)
        (condition-case err
            (save-buffer)
          (error
           (message "保存文件失败 %s: %s" file (error-message-string err)))))
      
      ;; 如果是我们打开的缓冲区，就关闭它
      (unless existing-buffer
        (ignore-errors (kill-buffer buffer)))
      
      (cons created-count total-count))))   

(defun restore-heading-ids-in-directory (dir)
  "为目录中所有 org 文件中有 #tag 但没有 ID 的 heading 创建 ID。"
  (interactive "D选择目录: ")
  (if (not (and (stringp dir) (file-directory-p dir)))
      (user-error "无效的目录路径: %s" dir)
    (let* ((files (condition-case err
                     (directory-files-recursively dir "\\.org$")
                   (error
                    (user-error "搜索目录失败: %s" (error-message-string err)))))
           (total-files (length files))
           (total-created 0)
           (total-headings 0)
           (processed-files 0)
           (error-files 0))
      
      (message "开始处理目录: %s" dir)
      (message "找到 %d 个 org 文件" total-files)
      
      (dolist (file files)
        (condition-case err
            (progn
              (message "[%d/%d] 处理文件: %s..."
                      (1+ processed-files) total-files
                      (file-name-nondirectory file))
              (let ((result (restore-heading-ids-in-file file)))
                (if result
                    (progn
                      (cl-incf processed-files)
                      (cl-incf total-created (car result))
                      (cl-incf total-headings (cdr result))
                      (message "[%d/%d] 文件 %s: 创建了 %d 个 ID (共 %d 个 heading)"
                              processed-files total-files
                              (file-name-nondirectory file)
                              (car result)
                              (cdr result)))
                  (cl-incf error-files))))
          (error
           (cl-incf error-files)
           (message "处理文件 %s 时出错: %s"
                    (file-name-nondirectory file)
                    (error-message-string err))
           (sit-for 1)))) ; 显示错误信息后暂停一秒
      
      (message "")
      (message "=== 处理完成 ===")
      (message "成功处理文件数: %d/%d" processed-files total-files)
      (message "出错的文件数: %d" error-files)
      (message "处理的 heading 总数: %d" total-headings)
      (message "创建的 ID 总数: %d" total-created)
      (when (> error-files 0)
        (message "注意：有 %d 个文件处理失败，请检查上面的错误信息" error-files)))))

(defun restore-heading-ids-current-file ()
  "为当前文件中有 #tag 但没有 ID 的 heading 创建 ID。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "请在 Org 文件中运行此命令"))
  
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "当前缓冲区未关联文件"))
    
    (let ((result (restore-heading-ids-in-file file)))
      (if result
          (message "创建了 %d 个 ID (共 %d 个 heading)"
                   (car result)
                   (cdr result))
        (message "处理文件时出错")))))

(defun restore-heading-ids-at-point ()
  "为当前 heading 创建 ID（如果它有 #tag 但没有 ID）。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "请在 Org 文件中运行此命令"))
  
  (unless (org-at-heading-p)
    (user-error "请将光标移动到 heading 处"))
  
  (let ((title (org-get-heading t t t t))
        (id (org-entry-get nil "ID")))
    (cond
     (id
      (message "当前 heading 已有 ID: %s" id))
     ((not (restore-heading-ids--has-tag-p title))
      (message "当前 heading 没有 #tag 标记"))
     (t
      (let ((new-id (org-id-new)))
        (org-entry-put nil "ID" new-id)
        (org-id-add-location new-id (buffer-file-name))
        (message "创建了新 ID: %s" new-id))))))

(provide 'restore-heading-ids)
;;; restore-heading-ids.el ends here

;;; org-supertag-behavior-library.el --- Library functions for org-supertag behaviors -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; 提供各种功能库支持 org-supertag 的行为系统
;; 每个库专注于特定领域的功能，支持行为的实现和组合
;;
;; 目前包含的功能库：
;; 1. Node Operations Library - 节点操作相关的功能
;; 2. Progress Tracking Library - 进度追踪相关的功能
;; 3. Face Management Library - 标签样式管理功能

(require 'org)

;;------------------------------------------------------------------------------
;; Behavior Archive
;;------------------------------------------------------------------------------

(defgroup org-supertag-archive nil
  "Archive settings for org-supertag."
  :group 'org-supertag)

(defcustom org-supertag-archive-file
  (expand-file-name "archive.org" org-directory)
  "File for archived nodes."
  :type 'file
  :group 'org-supertag-archive)

(defun org-supertag-archive--ensure-year-heading (year)
  "Ensure year heading exists in archive file.
Returns position after year heading."
  (with-current-buffer 
      (or (find-buffer-visiting org-supertag-archive-file)
          (find-file-noselect org-supertag-archive-file))
    (goto-char (point-min))
    (let ((year-title (format "* %d" year)))
      (if (re-search-forward (format "^%s$" (regexp-quote year-title)) nil t)
          (progn
            (forward-line 1)
            (point))
        (goto-char (point-min))
        (insert year-title "\n")
        (point)))))

(defun org-supertag-behavior--do-archive (node-id)
  "Archive node with NODE-ID under current year heading."
  (message "Archive action started for node: %s" node-id)
  
  ;; 1. 获取节点位置
  (when-let ((pos (org-supertag-db-get-pos node-id)))
    (message "Found node position: %s" pos)
    
    ;; 2. 确保归档文件存在
    (unless (file-exists-p org-supertag-archive-file)
      (with-temp-file org-supertag-archive-file
        (insert "#+TITLE: Archive\n\n")))
    
    ;; 3. 准备归档位置
    (let* ((year (string-to-number (format-time-string "%Y")))
           (archive-buffer (find-file-noselect org-supertag-archive-file))
           (insert-pos nil))
      
      ;; 4. 在归档文件中查找或创建年份标题
      (with-current-buffer archive-buffer
        (org-with-wide-buffer
         (goto-char (point-min))
         (if (re-search-forward (format "^\\* %d$" year) nil t)
             (setq insert-pos (point))
           ;; 如果年份标题不存在，创建它
           (goto-char (point-max))
           (insert (format "\n* %d\n" year))
           (setq insert-pos (point)))))
      
      ;; 5. 移动到源节点
      (save-excursion
        (cond
         ((markerp pos) (set-buffer (marker-buffer pos)))
         ((numberp pos) (goto-char pos)))
        
        ;; 6. 执行归档
        (org-cut-subtree)
        
        ;; 7. 插入到归档文件
        (with-current-buffer archive-buffer
          (goto-char insert-pos)
          (org-paste-subtree 2)
          (save-buffer)))
      
      (message "Node archived successfully"))))

;;------------------------------------------------------------------------------
;; Node Operations Library
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--get-children (node-id)
  "Get direct children of node with NODE-ID.
Returns a list of (heading todo-state) for each child node.

Example:
  (org-supertag-behavior--get-children \"20240101T123456\")
  ;; => ((\"Task 1\" \"TODO\") (\"Task 2\" \"DONE\"))"
  (message "\n=== Getting Children for Node %s ===" node-id)
  (when-let ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (cond
       ((markerp pos) (set-buffer (marker-buffer pos)))
       ((numberp pos) (goto-char pos)))
      (org-back-to-heading t)
      
      (let ((parent-level (org-outline-level))
            children)
        (message "Parent level: %d at heading: %s" 
                parent-level 
                (org-get-heading t t t t))
        
        ;; 使用 org-map-entries 收集直接子节点
        (save-restriction
          (org-narrow-to-subtree)
          (let ((parent-pos (point)))  ;; 记住父节点位置
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (let* ((current-level (org-outline-level))
                     (heading (org-get-heading t t t t))
                     (todo (org-get-todo-state)))
                (message "Found entry - Level: %d, Heading: %s, TODO: %s" 
                        current-level heading todo)
                ;; 只收集直接子节点，不需要 ID
                (when (= current-level (1+ parent-level))
                  (message "Adding child: %s" heading)
                  (push (list heading todo) children))))))
        
        (message "Found children: %S" children)
        (nreverse children)))))

(defun org-supertag-behavior--find-parent-with-tag (tag-id &optional action-fn)
  "Find nearest parent node with TAG-ID and optionally apply ACTION-FN.
TAG-ID should be the tag identifier (e.g. \"task\").
ACTION-FN is called with parent node-id if found.

Example:
  (org-supertag-behavior--find-parent-with-tag 
    \"task\" 
    (lambda (parent-id) 
      (message \"Found parent: %s\" parent-id)))"
  (save-excursion
    (org-back-to-heading t)
    (let ((current-heading (org-get-heading t t t t)))
      (message "Current heading: %s" current-heading)
      (while (and (> (org-outline-level) 1)
                 (org-up-heading-safe))
        (let* ((tags (org-get-tags))
               (heading (org-get-heading t t t t)))
          (message "Checking parent: %s, Tags: %S" heading tags)
          (when (member (concat "#" tag-id) tags)
            (message "Found parent with tag %s: %s" tag-id heading)
            (when-let ((parent-id (org-id-get)))
              (when action-fn
                (funcall action-fn parent-id))
              (message "Processed parent: %s" parent-id)
              parent-id)))))))

;;------------------------------------------------------------------------------
;; Progress Tracking Library
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--calculate-progress ()
  "Calculate progress for current heading based on children's TODO states.
Returns (total done progress) where progress is a float 0-100."
  (save-excursion
    (org-back-to-heading t)
    (let ((current-level (org-outline-level))
          (total 0)
          (done 0)
          (done-states (or org-done-keywords '("DONE"))))
      
      (message "\n=== Calculating Progress ===")
      (message "Current heading: %s" (org-get-heading t t t t))
      (message "Current level: %d" current-level)
      
      ;; 保存当前位置
      (let ((start-pos (point)))
        ;; 移动到第一个子项
        (outline-next-heading)
        
        ;; 遍历所有子项
        (while (and (not (eobp))
                   (> (org-outline-level) current-level))
          (let ((todo-state (org-get-todo-state))
                (heading (org-get-heading t t t t)))
            (setq total (1+ total))
            (message "Found child: %s (TODO=%s)" heading todo-state)
            (when (member todo-state done-states)
              (setq done (1+ done))))
          (outline-next-heading))
        
        ;; 恢复位置
        (goto-char start-pos))
      
      (message "Final count: %d total, %d done" total done)
      (list total done 
            (if (> total 0)
                (* 100.0 (/ (float done) total))
              0.0)))))

(defun org-supertag-behavior--update-progress-display (title progress)
  "Update progress display in TITLE with PROGRESS percentage.
Returns the new title string with [XX%] format.

Example:
  (org-supertag-behavior--update-progress-display \"Task A\" 75.5)
  ;; => \"Task A [75.5%]\""
  (if (string-match "\\[\\([0-9.]+\\)%\\]" title)
      (replace-match (format "[%.1f%%]" progress) t nil title)
    (concat title (format " [%.1f%%]" progress))))

;;------------------------------------------------------------------------------
;; Face Management Library
;;------------------------------------------------------------------------------

(defgroup org-supertag-faces nil
  "Faces for org-supertag."
  :group 'org-faces)

(defcustom org-supertag-tag-faces nil
  "Alist of tag faces.
Each element is (TAG-ID . FACE-PLIST) where:
TAG-ID is the tag identifier (e.g. \"task\")
FACE-PLIST is a property list of face attributes."
  :type '(alist :key-type string
                :value-type (plist :key-type symbol
                                 :value-type sexp))
  :group 'org-supertag-faces)

(defun org-supertag-behavior--face-set (tag-id face-plist)
  "Set face for TAG-ID to FACE-PLIST."
  (customize-save-variable
   'org-supertag-tag-faces
   (cons (cons tag-id face-plist)
         (assoc-delete-all tag-id org-supertag-tag-faces))))

(defun org-supertag-behavior--face-get (tag-id)
  "Get face for TAG-ID."
  (cdr (assoc tag-id org-supertag-tag-faces)))

(defun org-supertag-behavior--apply-styles (node-id)
  "Apply styles for all tags on NODE-ID."
  (org-with-wide-buffer
   (when-let ((pos (org-supertag-db-get-pos node-id)))
     (save-excursion
       (goto-char pos)
       (org-back-to-heading t)
       (let* ((beg (line-beginning-position))
              (end (line-end-position)))
         (save-restriction
           (narrow-to-region beg end)
           ;; 清除现有样式
           (remove-overlays beg end 'org-supertag-face t)
           ;; 应用新样式
           (let ((tags (org-get-tags nil t)))
             (dolist (tag tags)
               (when (string-prefix-p "#" tag)
                 (let* ((tag-id (substring tag 1))
                        (behavior (org-supertag-behavior--get-behavior tag-id))
                        (style (plist-get behavior :style)))
                   ;; 确保 face 是有效的
                   (when-let ((face (plist-get style :face)))
                     ;; 构建有效的 face 属性
                     (let* ((face-attrs (if (facep face)
                                        (face-all-attributes face nil)
                                      face))
                            (bg (plist-get face-attrs :background))
                            (fg (plist-get face-attrs :foreground))
                            ;; 只包含非空的属性
                            (valid-attrs
                             (append
                              (when (and bg (color-defined-p bg))
                                (list :background bg))
                              (when (and fg (color-defined-p fg))
                                (list :foreground fg)))))
                       ;; 只在有有效属性时创建 overlay
                       (when valid-attrs
                         (let ((ov (make-overlay beg end)))
                           (overlay-put ov 'face valid-attrs)
                           (overlay-put ov 'org-supertag-face t)
                           (overlay-put ov 'node-id node-id)))))))))))
         ;; 更新前缀
         (org-supertag-behavior--update-prefix node-id)))))

(defun org-supertag-behavior--update-prefix (node-id)
  "Update prefix for NODE-ID based on its tags."
  (org-with-wide-buffer
   (when-let ((pos (org-supertag-db-get-pos node-id)))
     (save-excursion
       (goto-char pos)
       (org-back-to-heading t)
       (let ((has-prefix nil)
             (inhibit-read-only t)
             (inhibit-modification-hooks t))
         (save-match-data
           (let ((tags (org-get-tags nil t)))
             (dolist (tag tags)
               (when (and (not has-prefix)
                         (string-prefix-p "#" tag))
                 (let* ((tag-id (substring tag 1))
                        (behavior (org-supertag-behavior--get-behavior tag-id))
                        (style (plist-get behavior :style))
                        (prefix (plist-get style :prefix)))
                   (when prefix
                     (setq has-prefix t)
                     (when (looking-at org-complex-heading-regexp)
                       (let* ((stars-end (match-end 1))
                              (todo-end (or (match-end 2) stars-end))
                              (current-title (match-string 4))
                              (new-title (if (string-prefix-p prefix current-title)
                                           current-title
                                         (concat prefix " " current-title))))
                         ;; 使用 replace-match 而不是直接删除和插入
                         (replace-match new-title t t nil 4))))))))))))))


(provide 'org-supertag-behavior-library) 

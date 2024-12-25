;;; org-supertag-behavior.el --- Tag-driven behavior system -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供基于 tag 的节点行为系统
;; 1. 行为作为 tag 的属性存在
;; 2. 当 tag 被应用时触发行为
;; 3. 支持自动化执行

(require 'org-supertag-tag)

;;------------------------------------------------------------------------------
;; Behavior Registry
;;------------------------------------------------------------------------------

(defvar org-supertag-behavior--initialized nil
  "Flag indicating if behavior system is initialized.")

(defun org-supertag-behavior-register (tag-name &rest props)
  "Register behavior for TAG-NAME.
PROPS is a plist with:
:trigger  - When to execute (:on-add :on-remove :on-change :on-schedule)
:action   - Function to execute (node-id)
:style    - Visual properties to apply"
  (message "\n=== Registering Behavior for %s ===" tag-name)
  (message "Current DB state: %S" (ht->alist org-supertag-db--object))
  
  (let* ((tag-id (org-supertag-sanitize-tag-name tag-name))
         (behavior (list :trigger (plist-get props :trigger)
                        :action (plist-get props :action)
                        :style (plist-get props :style))))
    
    (message "Sanitized tag ID: %s" tag-id)
    (message "Behavior to register: %S" behavior)
    
    ;; 验证行为定义
    (org-supertag-behavior--validate behavior)
    
    ;; 检查标签是否已存在
    (let ((existing-tag (org-supertag-tag-get tag-id)))
      (message "Existing tag: %S" existing-tag))
    
    ;; 创建或更新标签
    (unless (org-supertag-tag-get tag-id)
      (message "Creating new tag: %s" tag-id)
      (org-supertag-tag-create tag-id))
    
    ;; 检查标签创建后的状态
    (message "Tag state after creation: %S" 
             (org-supertag-tag-get tag-id))
    
    ;; 添加行为字段
    (message "Adding behavior field to tag")
    (org-supertag-tag-add-field 
     tag-id
     (list :name "_behavior"
           :type 'behavior
           :value behavior))
    
    ;; 检查字段添加后的状态
    (message "Tag state after adding field: %S" 
             (org-supertag-tag-get tag-id))
    
    (message "DB state after registration: %S" 
             (ht->alist org-supertag-db--object))
    
    ;; 返回注册的行为
    behavior))

;;------------------------------------------------------------------------------
;; Behavior Execution
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--validate-node (node-id)
  "Validate if NODE-ID represents a valid org node.
Returns t if valid, nil otherwise."
  (when-let ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (cond
       ((markerp pos) (set-buffer (marker-buffer pos)))
       ((numberp pos) (goto-char pos)))
      (org-at-heading-p))))

(defun org-supertag-behavior--on-tag-change (node-id tag-id action)
  "Handle behavior when TAG-ID is applied to NODE-ID with ACTION."
  (message "Processing behavior: node=%s tag=%s action=%s" 
           node-id tag-id action)
  
  (when-let* ((behavior (org-supertag-behavior--get-behavior tag-id))
              (trigger (plist-get behavior :trigger))
              (action-fn (plist-get behavior :action)))
    (message "Found behavior: trigger=%s action=%S" trigger action-fn)
    
    ;; 修改触发条件判断
    (when (or (eq trigger :always)
              (eq trigger :on-change)  ; 对于 on-change，总是执行
              (and (eq trigger :on-add) (eq action :add))
              (and (eq trigger :on-remove) (eq action :remove)))
      
      (message "Executing action for node %s" node-id)
      (condition-case err
          (funcall action-fn node-id)
        (error 
         (message "Action failed: %S" err))))))



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
;; Style System
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--apply-styles (node-id)
  "Apply styles for all tags on NODE-ID."
  (message "Applying styles for node: %s" node-id)
  (when-let ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      ;; 移动到节点位置
      (cond
       ((markerp pos) (set-buffer (marker-buffer pos)))
       ((numberp pos) (goto-char pos)))
      
      ;; 获取节点的所有标签
      (let ((tags (org-get-tags)))
        (dolist (tag tags)
          ;; 只处理 supertag
          (when (string-prefix-p "#" tag)
            (let* ((tag-id (substring tag 1))
                   (behavior (org-supertag-behavior--get-behavior tag-id))
                   (style (plist-get behavior :style)))
              (when style
                (message "Applying style for tag %s: %S" tag style)
                ;; 应用到整个标题
                (save-excursion
                  (org-back-to-heading t)
                  (let* ((beg (line-beginning-position))
                         (end (line-end-position))
                         ;; 创建叠加层
                         (ov (make-overlay beg end)))
                    ;; 设置叠加层属性
                    (when-let ((face (plist-get style :face)))
                      (overlay-put ov 'face face))
                    ;; 存储叠加层以便后续管理
                    (overlay-put ov 'org-supertag-behavior t)
                    (overlay-put ov 'node-id node-id)
                    ;; 添加前缀到标题开头
                    (when-let ((prefix (plist-get style :prefix)))
                      (save-excursion
                        (goto-char beg)
                        (skip-chars-forward "* ")
                        (unless (looking-at-p (regexp-quote prefix))
                          (insert prefix " "))))))))))))))


;;------------------------------------------------------------------------------
;; Behavior System Hooks
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--init ()
  "Initialize behavior system."
  (message "\n=== Behavior System Init ===")
  (message "DB state before init: %S" (ht->alist org-supertag-db--object))
  (unless org-supertag-behavior--initialized
    ;; 样式相关钩子
    (add-hook 'org-supertag-after-tag-apply-hook
              #'org-supertag-behavior--apply-styles)
    
    ;; 行为触发相关钩子 - 使用 node-tag-added-hook 替代
    (add-hook 'org-supertag-node-tag-added-hook
              #'org-supertag-behavior--handle-tag-add)
    
    ;; 其他钩子保持不变
    (add-hook 'org-supertag-after-node-change-hook
              #'org-supertag-behavior--handle-node-change)
    (add-hook 'org-supertag-after-tag-remove-hook
              #'org-supertag-behavior--handle-tag-remove)
    
    ;; 定时任务相关
    (add-hook 'org-supertag-after-load-hook
              #'org-supertag-behavior--setup-scheduled-behaviors)
    
    ;; 确保在 Emacs 退出时清理
    (add-hook 'kill-emacs-hook
              #'org-supertag-behavior--cleanup)
    
    (setq org-supertag-behavior--initialized t)
    (message "Behavior system initialized")))

(defun org-supertag-behavior--handle-node-change (node-id)
  "Handle node change event for NODE-ID."
  (message "Node change handler: node=%s" node-id)
  (dolist (tag-id (org-supertag-node-get-tags node-id))
    (org-supertag-behavior--on-tag-change node-id tag-id :change)))

(defun org-supertag-behavior--handle-tag-add (node-id tag-id)
  "Handle tag addition for NODE-ID with TAG-ID."
  (message "Tag add handler: node=%s tag=%s" node-id tag-id)
  (org-supertag-behavior--on-tag-change node-id tag-id :add))

(defun org-supertag-behavior--handle-tag-remove (node-id tag-id)
  "Handle tag remove event for NODE-ID and TAG-ID."
  (message "Tag remove handler: node=%s tag=%s" node-id tag-id)
  (org-supertag-behavior--on-tag-change node-id tag-id :remove))

(defun org-supertag-behavior--setup-scheduled-behaviors ()
  "Setup scheduled behaviors."
  ;; TODO: 实现定时任务支持
  )

(defun org-supertag-behavior--cleanup ()
  "Cleanup behavior system."
  ;; 移除所有钩子
  (remove-hook 'org-supertag-after-tag-apply-hook
               #'org-supertag-behavior--apply-styles)
  (remove-hook 'org-supertag-after-node-change-hook
               #'org-supertag-behavior--handle-node-change)
  (remove-hook 'org-supertag-after-tag-add-hook
               #'org-supertag-behavior--handle-tag-add)
  (remove-hook 'org-supertag-after-tag-remove-hook
               #'org-supertag-behavior--handle-tag-remove)
  (remove-hook 'org-supertag-after-load-hook
               #'org-supertag-behavior--setup-scheduled-behaviors))

(defun org-supertag-behavior--get-behavior (tag-id)
  "Get behavior configuration for TAG-ID."
  (when-let* ((tag (org-supertag-tag-get tag-id)))
    (org-supertag-tag-get-field-value tag "_behavior")))


;;------------------------------------------------------------------------------
;; Integration with org-supertag-tag
;;------------------------------------------------------------------------------

;; 监听 tag 变化
(add-hook 'org-supertag-tag-after-add-hook
          (lambda (node-id tag-id)
            (org-supertag-behavior--on-tag-change node-id tag-id :add)))

(add-hook 'org-supertag-tag-after-remove-hook
          (lambda (node-id tag-id)
            (org-supertag-behavior--on-tag-change node-id tag-id :remove)))

;;------------------------------------------------------------------------------
;; Error Handling
;;------------------------------------------------------------------------------

(define-error 'org-supertag-behavior-error "Org Supertag Behavior Error")

(defun org-supertag-behavior--handle-error (err node-id tag-id action)
  "Handle behavior execution error.
ERR is the error object
NODE-ID is the affected node
TAG-ID is the tag
ACTION is the attempted action"
  (message "Behavior error for tag %s on node %s: %S" 
           tag-id node-id err)
  (when (eq :debug org-supertag-log-level)
    (message "Error details: %S" (cdr err))))

;;------------------------------------------------------------------------------
;; Behavior Validation
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--validate (behavior)
  "Validate behavior definition.
BEHAVIOR is the behavior plist to validate.
Returns t if valid, signals error if invalid."
  (let ((trigger (plist-get behavior :trigger))
        (action (plist-get behavior :action))
        (style (plist-get behavior :style)))
    
    ;; 验证触发器
    (unless (memq trigger '(:on-add :on-remove :on-change :on-schedule :always))
      (signal 'org-supertag-behavior-error 
              (list :invalid-trigger trigger)))
    
    ;; 验证动作(如果存在)
    (when action
      (unless (functionp action)
        (signal 'org-supertag-behavior-error 
                (list :invalid-action action))))
    
    ;; 验证样式(如果存在)
    (when style
      (unless (plist-member style :face)
        (signal 'org-supertag-behavior-error 
                (list :invalid-style style))))
    t))

;;------------------------------------------------------------------------------
;; Safe Execution
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--safe-execute (node-id tag-id action)
  "Safely execute behavior.
NODE-ID is target node
TAG-ID is behavior tag
ACTION is :add or :remove"
  (condition-case err
      (org-supertag-behavior--on-tag-change node-id tag-id action)
    (org-supertag-behavior-error
     (org-supertag-behavior--handle-error err node-id tag-id action))
    (error 
     (org-supertag-behavior--handle-error err node-id tag-id action))))

;;------------------------------------------------------------------------------
;; API Functions
;;------------------------------------------------------------------------------

(defun org-supertag-behavior-get (tag-id)
  "Get behavior definition for TAG-ID."
  (when-let ((tag (org-supertag-tag-get tag-id)))
    (org-supertag-tag-get-field-value tag "_behavior")))

(defun org-supertag-behavior-refresh-node (node-id)
  "Refresh behaviors for NODE-ID."
  (dolist (tag-id (org-supertag-db-get-tags node-id))
    (org-supertag-behavior--safe-execute node-id tag-id :add)))

;;------------------------------------------------------------------------------
;; Integration Hooks
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--setup ()
  "Setup behavior system hooks."
  ;; Tag 变化时触发行为
  (add-hook 'org-supertag-tag-after-add-hook
            (lambda (node-id tag-id)
              (org-supertag-behavior--safe-execute node-id tag-id :add)))
  
  (add-hook 'org-supertag-tag-after-remove-hook
            (lambda (node-id tag-id)
              (org-supertag-behavior--safe-execute node-id tag-id :remove)))
  
  ;; 节点创建时应用已有 tag 的行为
  (add-hook 'org-supertag-db-after-node-create-hook
            #'org-supertag-behavior-refresh-node))

(defun org-supertag-behavior--cleanup ()
  "Remove behavior system hooks."
  (remove-hook 'org-supertag-tag-after-add-hook
               #'org-supertag-behavior--safe-execute)
  (remove-hook 'org-supertag-tag-after-remove-hook
               #'org-supertag-behavior--safe-execute)
  (remove-hook 'org-supertag-db-after-node-create-hook
               #'org-supertag-behavior-refresh-node))

;;------------------------------------------------------------------------------
;; Minor Mode
;;------------------------------------------------------------------------------

(define-minor-mode org-supertag-behavior-mode
  "Toggle org-supertag behavior system."
  :global t
  :group 'org-supertag
  (if org-supertag-behavior-mode
      ;; Enable
      (org-supertag-behavior--init)
    ;; Disable
    (org-supertag-behavior--cleanup)))

;; 确保在包加载时启用
(defun org-supertag-behavior-setup ()
  "Setup org-supertag behavior system."
  (org-supertag-behavior-mode 1))

(add-hook 'org-supertag-after-load-hook
          #'org-supertag-behavior-setup)

;;------------------------------------------------------------------------------
;; Behavior Definition
;;------------------------------------------------------------------------------

(defun org-supertag-behavior-define (name &rest props)
  "Define a new behavior with NAME and properties PROPS.
PROPS should include:
:trigger - When to execute (:on-add :on-remove :on-change :always)
:action  - Function to execute (takes node-id as argument)
:style   - Optional visual properties to apply"
  (let* ((behavior (list :trigger (plist-get props :trigger)
                        :action (plist-get props :action)
                        :style (plist-get props :style))))
    ;; 验证行为定义
    (org-supertag-behavior--validate behavior)
    ;; 注册行为
    (org-supertag-behavior-register name behavior)))

(defun org-supertag-behavior-attach (tag-name behavior-name)
  "Attach BEHAVIOR-NAME to the tag specified by TAG-NAME."
  (interactive
   (list (completing-read "Tag: " (org-supertag-get-all-tags))
         (completing-read "Behavior: " 
                         (hash-table-keys org-supertag-behavior-registry))))
  
  (let* ((tag (org-supertag-tag-get tag-name))
         (behavior (gethash behavior-name org-supertag-behavior-registry)))
    
    (unless tag
      (error "Tag not found: %s" tag-name))
    
    (unless behavior
      (error "Behavior not found: %s" behavior-name))
    
    ;; 获取现有的字段列表
    (let* ((fields (or (plist-get tag :fields) '()))
           ;; 创建新的行为字段
           (behavior-field
            `(:name "_behavior"
              :type behavior
              :value ,behavior)))
      
      ;; 更新标签
      (org-supertag-tag-create 
       tag-name 
       :fields (cons behavior-field 
                    (cl-remove-if (lambda (f)
                                  (equal (plist-get f :name) "_behavior"))
                                fields)))
      
      (message "Behavior '%s' attached to tag '%s'" behavior-name tag-name))))


;;------------------------------------------------------------------------------
;; Behavior Registry
;;------------------------------------------------------------------------------

(defgroup org-supertag-behavior nil
  "Customization options for org-supertag behaviors."
  :group 'org-supertag)

(defcustom org-supertag-behavior-registry (make-hash-table :test 'equal)
  "Registry of defined behaviors."
  :type '(alist :key-type string :value-type sexp)
  :group 'org-supertag-behavior)

(defcustom org-supertag-behavior-presets
  '(("@archive" . (:trigger :on-add
                   :action org-supertag-behavior--do-archive
                   :style (:face (:foreground "gray50")
                           :prefix "📦")))
    ("@important" . (:trigger :always
                    :style (:face (:foreground "red" :weight bold)
                            :prefix "⚠")))
    ("@project" . (:trigger :on-change
                  :action org-supertag-project-update-progress
                  :style (:face (:foreground "blue")
                          :prefix "📋"))))
  "Preset behaviors that will be registered on startup."
  :type '(alist :key-type string :value-type sexp)
  :group 'org-supertag-behavior)

;;------------------------------------------------------------------------------
;; Example Usage
;;------------------------------------------------------------------------------
(defun org-supertag-behavior-ensure-defaults ()
  "Ensure default behaviors are registered."
  (message "\n=== Ensuring Default Behaviors ===")
  (message "DB state before defaults: %S" 
           (ht->alist org-supertag-db--object))
  
  (dolist (preset org-supertag-behavior-presets)
    (let ((tag-name (car preset))
          (props (cdr preset)))
      (unless (org-supertag-tag-get tag-name)
        (message "Registering preset behavior for %s" tag-name)
        (apply #'org-supertag-behavior-register tag-name
               (append props nil)))))
  
  (message "DB state after defaults: %S" 
           (ht->alist org-supertag-db--object)))
           
;; 只在初始化时执行一次
(add-hook 'org-supertag-after-load-hook
          #'org-supertag-behavior-ensure-defaults)


(defun org-supertag-project-update-progress (node-id)
  "更新项目节点的进度"
  (message "\n=== Updating Project Progress for %s ===" node-id)
  (when-let ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (cond
       ((markerp pos) (set-buffer (marker-buffer pos)))
       ((numberp pos) (goto-char pos)))
      (org-back-to-heading t)
      
      (let* ((children (org-supertag-behavior--get-children node-id))
             (total (length children))
             (done (cl-count-if 
                   (lambda (child)
                     (string= (nth 1 child) "DONE"))
                   children)))
        
        (message "Processing children - Total: %d, Done: %d" total done)
        (dolist (child children)
          (message "Child: Heading=%s, TODO=%s" 
                  (nth 0 child) 
                  (nth 1 child)))
        
        (let ((progress (if (> total 0)
                          (* 100.0 (/ (float done) total))
                        0.0)))
          (message "Final stats - Total: %d, Done: %d, Progress: %.1f%%" 
                  total done progress)
          
          ;; 更新进度属性
          (message "Updating Progress property...")
          (org-entry-put (point) "Progress" 
                        (format "%.1f" progress))
          
          ;; 更新标题显示
          (message "Updating heading...")
          (let ((title (org-get-heading t t t t)))
            (if (string-match "\\[\\([0-9.]+\\)%\\]" title)
                (setq title (replace-match 
                           (format "[%.1f%%]" progress)
                           t nil title))
              (setq title (concat title 
                                (format " [%.1f%%]" progress))))
            (org-edit-headline title)))))))

;; 监听子节点状态变化
(defun org-supertag-project-todo-state-change ()
  "当 TODO 状态改变时更新父项目进度"
  (message "\n=== TODO State Change Detected ===")
  (save-excursion
    (org-back-to-heading t)
    (let ((current-heading (org-get-heading t t t t)))
      (message "Current heading: %s" current-heading)
      ;; 向上查找带有 @project 标签的父节点
      (let ((current (point)))
        (while (and (> (org-outline-level) 1)
                   (org-up-heading-safe))
          (let* ((tags (org-get-tags))
                 (heading (org-get-heading t t t t)))
            (message "Checking parent: %s, Tags: %S" heading tags)
            (when (member "#@project" tags)  ;; 修正标签名
              (message "Found project parent: %s" heading)
              (when-let ((parent-id (org-id-get)))
                (message "Updating project with ID: %s" parent-id)
                (org-supertag-project-update-progress parent-id)))))))))

;; 添加到 org-after-todo-state-change-hook
(add-hook 'org-after-todo-state-change-hook
          #'org-supertag-project-todo-state-change)

(defun org-supertag-behavior--debug-node (node-id)
  "输出节点的详细调试信息"
  (message "\n=== Node Debug Info ===")
  (message "Node ID: %s" node-id)
  (when-let ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (cond
       ((markerp pos) 
        (message "Buffer: %s" (marker-buffer pos))
        (set-buffer (marker-buffer pos)))
       ((numberp pos)
        (message "Position: %d" pos)
        (goto-char pos)))
      (org-back-to-heading t)
      (message "Heading: %s" (org-get-heading t t t t))
      (message "TODO state: %s" (org-entry-get (point) "TODO"))
      (message "Properties: %S" (org-entry-properties))
      (let ((children (org-supertag-behavior--get-children node-id)))
        (message "Children: %d" (length children))
        (dolist (child children)
          (message "  Child: %s" child)
          (when-let ((child-pos (org-supertag-db-get-pos child)))
            (message "    TODO: %s" (org-entry-get child-pos "TODO"))))))))

(defun org-supertag-behavior--get-children (node-id)
  "获取节点的子节点，包含详细的调试信息"
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

(provide 'org-supertag-behavior)

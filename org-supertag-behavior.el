;;; org-supertag-behavior.el --- Tag-driven behavior system -*- lexical-binding: t; -*-

;;; Commentary
;;
;; 一切都是行为，行为相互调用
;;
;; 提供基于 tag 的节点行为系统
;; 1. 行为作为 tag 的属性存在
;; 2. 当 tag 被应用时触发行为
;; 3. 支持自动化执行
;; 4. 支持定时任务

(require 'org-supertag-tag)
(require 'org-supertag-behavior-library)

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

(defvar org-supertag-behavior--initialized nil
  "Flag indicating if behavior system is initialized.")

(defun org-supertag-behavior-register (tag-name &rest props)
  "Register behavior for TAG-NAME.
PROPS is a plist with:
:trigger  - When to execute (:on-add :on-remove :on-change :on-schedule)
:action   - Function, behavior name, or list of behavior names
:style    - Visual properties to apply
:hooks    - Optional list of (hook-name . hook-function) to setup"
  
  (let* ((tag-id (org-supertag-sanitize-tag-name tag-name))
         ;; 先创建基本行为定义
         (behavior (list :trigger (plist-get props :trigger)
                        :action (plist-get props :action)  ; 保存原始 action
                        :style (plist-get props :style)
                        :hooks (plist-get props :hooks))))
    
    ;; 先注册到行为注册表
    (puthash tag-name behavior org-supertag-behavior-registry)

    ;; 创建或更新标签
    (unless (org-supertag-tag-get tag-id)
      (message "Creating new tag: %s" tag-id)
      (org-supertag-tag-create tag-id))
    
    ;; 添加行为字段
    (let* ((tag (org-supertag-tag-get tag-id))
           (fields (or (plist-get tag :fields) '()))
           (behavior-field (list :name "_behavior"
                               :type 'behavior
                               :value behavior)))
      
      ;; 更新标签
      (org-supertag-tag-create 
       tag-id 
       :fields (cons behavior-field 
                     (cl-remove-if (lambda (f)
                                   (equal (plist-get f :name) "_behavior"))
                                 fields))
       :behaviors (list tag-name)))
    
    ;; 处理 hooks
    (when-let ((hooks (plist-get props :hooks)))
      (message "Setting up hooks: %S" hooks)
      (dolist (hook-spec hooks)
        (add-hook (car hook-spec) (cdr hook-spec))
        (message "Added hook %S -> %S" (car hook-spec) (cdr hook-spec))))
    
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
  (when-let* ((behavior (org-supertag-behavior--get-behavior tag-id))
              (trigger (plist-get behavior :trigger))
              (behavior-action (plist-get behavior :action)))
    
    (when (or (eq trigger :always)
              (eq trigger :on-change)
              (and (eq trigger :on-add) (eq action :add))
              (and (eq trigger :on-remove) (eq action :remove)))
      
      (condition-case err
          (org-supertag-behavior--execute-action node-id behavior-action)
        (error 
         (message "Action failed: %S" err))))))

(defun org-supertag-behavior--execute-action (action)
  "Execute ACTION at current heading.
ACTION can be:
- A string (behavior name to lookup)
- A list of behavior names
- A function to execute"
  (message "\n=== Executing Action ===")
  (save-excursion
    (org-back-to-heading t)  ; 确保在标题位置
    (let* ((props (org-supertag-db--parse-node-at-point))  ; 使用现成的解析函数
           (node-id (plist-get props :id)))
      (message "At node: %s" (plist-get props :title))
      
      (cond
       ;; 字符串：引用其他行为
       ((stringp action)
        (message "Looking up behavior: %s" action)
        (if-let ((behavior (gethash action org-supertag-behavior-registry)))
            (progn
              (message "Found behavior in registry: %S" behavior)
              (org-supertag-behavior--execute-action 
               (plist-get behavior :action)))
          (error "Behavior not found in registry: %s" action)))
       
       ;; 列表：多个行为
       ((and (listp action) (not (functionp action)))
        (message "Executing behavior list: %S" action)
        (dolist (behavior-name action)
          (message "Processing behavior: %s" behavior-name)
          (when-let ((behavior (gethash behavior-name org-supertag-behavior-registry)))
            (message "Found registered behavior: %S" behavior)
            (org-supertag-behavior--execute-action behavior-name))))
       
       ;; 函数：直接执行
       ((functionp action)
        (message "Executing function")
        (condition-case err
            (funcall action)
          (error 
           (message "Function execution failed: %S" err))))
       
       ;; 其他情况
       (t (error "Invalid action type: %S" action)))
      
      ;; 行为执行后，更新数据库
      (org-supertag-db-add node-id props))))

(defun org-supertag-behavior--trigger-for-node (node-id trigger)
  "Trigger behaviors for NODE-ID based on TRIGGER event."
  (message "\n=== Triggering behaviors for node %s ===" node-id)
  (let ((tag-relations (org-supertag-db-get-node-tags node-id)))
    (message "Node tag relations: %S" tag-relations)
    
    (dolist (relation tag-relations)
      (let* ((tag-id (plist-get relation :to))
             (behavior (gethash tag-id org-supertag-behavior-registry)))
        (message "Processing tag: %s" tag-id)
        (when behavior
          (message "Found behavior: %S" behavior)
          (let ((behavior-trigger (plist-get behavior :trigger))
                (action (plist-get behavior :action)))
            (when (or (eq behavior-trigger trigger)
                     (eq behavior-trigger :always))
              (message "Executing action")
              (condition-case err
                  (org-supertag-behavior--execute-action action)
                (error 
                 (message "Action failed: %S" err))))))))))


(defun org-supertag-tag-get-field-by-name (tag field-name)
  "Get field with FIELD-NAME from TAG.
Returns the field plist if found, nil otherwise."
  (when-let* ((fields (plist-get tag :fields)))
    (cl-find-if (lambda (field)
                  (equal (plist-get field :name) field-name))
                fields)))

;;------------------------------------------------------------------------------
;; Behavior System Hooks
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--add-overlays ()
  "Add overlays for all tagged nodes in current buffer."
  (org-supertag-behavior--face-refresh))

(defun org-supertag-behavior--remove-overlays ()
  "Remove all org-supertag overlays in current buffer."
  (remove-overlays (point-min) (point-max) 'org-supertag-face t)
  (remove-overlays (point-min) (point-max) 'org-supertag-prefix t))

(defun org-supertag-behavior--init ()
  "Initialize behavior system."
  (message "\n=== Behavior System Init ===")
  (unless org-supertag-behavior--initialized
    ;; Style Hooks
    (add-hook 'org-supertag-after-tag-apply-hook
              #'org-supertag-behavior--face-refresh)
    ;; Buffer Change Hooks
    (add-hook 'after-save-hook 
              #'org-supertag-behavior--face-refresh)
    (add-hook 'after-change-functions 
              #'org-supertag-behavior--face-refresh)
    ;; Behavior Trigger Hooks
    (add-hook 'org-supertag-node-tag-added-hook
              #'org-supertag-behavior--handle-tag-add)
    ;; Node Change Hooks
    (add-hook 'org-supertag-after-node-change-hook
              #'org-supertag-behavior--handle-node-change)
    (add-hook 'org-supertag-after-tag-remove-hook
              #'org-supertag-behavior--handle-tag-remove)
    ;; Scheduled Behaviors
    (add-hook 'org-supertag-after-load-hook
              #'org-supertag-behavior--setup-scheduled-behaviors)
    ;; Cleanup on Emacs Exit
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

(defun org-supertag-behavior--get-behavior (tag-name)
  "Get behavior definition for TAG-NAME from registry."
  (let ((behavior (gethash tag-name org-supertag-behavior-registry)))
    behavior))


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
;; Minor Mode
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--face-refresh (&optional beg end _len)
  "Refresh faces incrementally.
If BEG and END are provided, only refresh that region."
  (when (and org-supertag-behavior-mode
             (derived-mode-p 'org-mode))
    (save-excursion
      (save-restriction
        (widen)
        (if (and beg end)
            ;; 增量更新：只处理变化的标题
            (progn
              (goto-char beg)
              (when-let* ((element (org-element-context))
                         ((eq (org-element-type element) 'headline))
                         (node-id (org-id-get)))
                (org-supertag-behavior--apply-styles node-id)))
          ;; 全局刷新
          (goto-char (point-min))
          (while (re-search-forward "^\\*+ " nil t)
            (when-let* ((element (org-element-context))
                       ((eq (org-element-type element) 'headline))
                       (node-id (org-id-get)))
              (org-supertag-behavior--apply-styles node-id))))))))

(defun org-supertag-behavior--setup-buffer ()
  "Setup current buffer for org-supertag behaviors."
  (when (derived-mode-p 'org-mode)
    ;; 增量更新钩子
    (add-hook 'after-save-hook #'org-supertag-behavior--face-refresh nil t)
    (add-hook 'org-after-tags-change-hook #'org-supertag-behavior--face-refresh nil t)
    ;; 初始刷新
    (run-with-timer 0 nil #'org-supertag-behavior--face-refresh)))

(define-minor-mode org-supertag-behavior-mode
  "Toggle org-supertag behavior system."
  :global t
  :group 'org-supertag
  (if org-supertag-behavior-mode
      (progn
        ;; Enable
        (org-supertag-behavior--init)
        ;; 只设置当前 buffer
        (when (derived-mode-p 'org-mode)
          (org-supertag-behavior--setup-buffer))
        ;; 为新打开/切换的 org buffers 设置
        (add-hook 'org-mode-hook #'org-supertag-behavior--setup-buffer))
    ;; Disable
    (progn
      (org-supertag-behavior--cleanup)
      (remove-hook 'org-mode-hook #'org-supertag-behavior--setup-buffer)
      ;; 只清理当前 buffer
      (when (derived-mode-p 'org-mode)
        (remove-hook 'after-save-hook #'org-supertag-behavior--face-refresh t)
        (remove-hook 'org-after-tags-change-hook 
                    #'org-supertag-behavior--face-refresh t)))))

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
    (org-supertag-behavior--validate behavior)
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
    
    ;; Get existing fields and behaviors
    (let* ((fields (or (plist-get tag :fields) '()))
           (behaviors (or (plist-get tag :behaviors) '()))
           ;; Create new behavior field
           (behavior-field
            `(:name "_behavior"
              :type behavior
              :value ,behavior)))
      
      ;; Update tag with new field and behavior
      (org-supertag-tag-create 
       tag-name 
       :fields (cons behavior-field 
                    (cl-remove-if (lambda (f)
                                  (equal (plist-get f :name) "_behavior"))
                                fields))
       :behaviors (if (member behavior-name behaviors)
                     behaviors
                   (cons behavior-name behaviors)))
      
      (let ((nodes (org-supertag-db-get-tag-nodes tag-name)))
         (dolist (node-id nodes)
            ;; 重新触发行为
            (org-supertag-behavior--trigger-for-node node-id :on-add)
            ;; 刷新视觉效果
            (org-supertag-behavior--apply-styles node-id)))

      (message "Behavior '%s' attached to tag '%s'" behavior-name tag-name))))



(provide 'org-supertag-behavior)

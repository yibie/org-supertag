;;; org-supertag-behavior.el --- Tag-driven behavior system -*- lexical-binding: t; -*-

;;; Commentary
;;
;; Everything is behavior, behaviors call each other
;;
;; Provides a tag-based node behavior system
;; 1. Behaviors exist as tag properties
;; 2. Behaviors are triggered when tags are applied
;; 3. Supports automated execution
;; 4. Supports scheduled tasks

(require 'org-supertag-tag)
(require 'org-supertag-behavior-library)

;;------------------------------------------------------------------------------
;; Behavior Registry
;;------------------------------------------------------------------------------

(defgroup org-supertag-behavior nil
  "Customization options for org-supertag behaviors."
  :group 'org-supertag)

(defcustom org-supertag-behavior-registry (make-hash-table :test 'equal)
  "Registry of defined behaviors.
Key is the behavior name (string), value is a plist containing:
:trigger  - When to execute (:on-add :on-remove :on-change :on-schedule :always)
:action   - Function to execute or list of behavior names
:style    - Visual properties (optional)
:hooks    - List of (hook-name . hook-function) pairs (optional)
:params   - List of parameter definitions (optional)"
  :type 'hash-table
  :group 'org-supertag-behavior)

(defvar org-supertag-behavior--initialized nil
  "Flag indicating if behavior system is initialized.")

(defun org-supertag-behavior-register (behavior-name &rest props)
  "Register behavior with BEHAVIOR-NAME.
PROPS is a plist with:
:trigger  - When to execute
:action   - Function or behavior list
:style    - Visual properties
:hooks    - Optional hooks
:params   - Parameter names list
:list     - List of behaviors to execute"
  (let ((behavior (list :trigger (plist-get props :trigger)
                       :action (plist-get props :action)
                       :style (plist-get props :style)
                       :hooks (plist-get props :hooks)
                       :params (plist-get props :params)
                       :list (plist-get props :list))))
    
    ;; 注册到行为注册表（内存缓存）
    (puthash behavior-name behavior org-supertag-behavior-registry)
    
    ;; 处理 hooks
    (when-let ((hooks (plist-get props :hooks)))
      (dolist (hook-spec hooks)
        (add-hook (car hook-spec) (cdr hook-spec))))
    
    behavior))


(defun org-supertag-behavior--get-behavior (tag-name)
  "Get behavior definition for tag with TAG-NAME.
First try to get behavior directly from registry.
If not found, check if tag has associated behaviors."
  (or
   ;; 直接从注册表查找
   (gethash tag-name org-supertag-behavior-registry)
   ;; 从标签的关联行为中查找
   (when-let* ((tag (org-supertag-tag-get tag-name))
               (behaviors (plist-get tag :behaviors)))
     ;; 如果有多个行为，返回第一个
     (when (car behaviors)
       (gethash (car behaviors) org-supertag-behavior-registry)))))

(defun org-supertag-behavior--cleanup-duplicates ()
  "Clean up duplicate behaviors in all tags."
  (interactive)
  (maphash
   (lambda (key value)
     (when (plist-get value :behaviors)
       (let ((unique-behaviors (delete-dups (plist-get value :behaviors))))
         (org-supertag-tag-create
          key
          :type :tag
          :behaviors unique-behaviors))))
   org-supertag-db--object))

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
  (message "Debug on-tag-change - node=%s tag=%s action=%s" 
           node-id tag-id action)
  (when-let* ((behavior (org-supertag-behavior--get-behavior tag-id))
              (trigger (plist-get behavior :trigger)))
    (message "Debug on-tag-change - Found behavior=%S trigger=%S" 
             behavior trigger)
    (when (or (eq trigger :always)
              (eq trigger :on-change)
              (and (eq trigger :on-add) (eq action :add))
              (and (eq trigger :on-remove) (eq action :remove)))
      (org-supertag-behavior-execute node-id behavior))))

(defun org-supertag-behavior--plist-p (object)
  "Check if OBJECT is a property list."
  (and (listp object)
       (> (length object) 0)
       (keywordp (car object))))

(defun org-supertag-behavior-execute (node-id behavior-spec &rest params)
  "Execute behavior specified by BEHAVIOR-SPEC on NODE-ID with PARAMS."
  (pcase behavior-spec
    ((pred stringp)
     (when-let* ((behavior (gethash behavior-spec org-supertag-behavior-registry))
                 (param-names (plist-get behavior :params))
                 (param-values (when (and param-names params)
                               (org-supertag-behavior--parse-param-string 
                                (car params) param-names))))
       (if (plist-get behavior :list)
           ;; Execute behavior list
           (dolist (sub-behavior (plist-get behavior :list))
             (let* ((parts (split-string sub-behavior "="))
                    (name (car parts))
                    (args (cadr parts)))
               (org-supertag-behavior-execute node-id name args)))
         ;; Execute single behavior
         (org-supertag-behavior-execute node-id behavior param-values))))
    
    ;; plist: complete behavior definition
    ((pred org-supertag-behavior--plist-p)
     (when-let ((pos (org-supertag-db-get-pos node-id)))
       (save-excursion
         (org-with-point-at pos
           ;; Check for behavior list
           (if-let ((behavior-list (plist-get behavior-spec :list)))
               ;; Execute behavior list
               (dolist (sub-behavior behavior-list)
                 (let* ((parts (split-string sub-behavior "="))
                        (name (car parts))
                        (args (cadr parts)))
                   (org-supertag-behavior-execute node-id name args)))
             ;; Execute single behavior
             (when-let ((action (plist-get behavior-spec :action)))
               (if params
                   (let ((param-names (plist-get behavior-spec :params))
                         (param-values (car params)))
                     (save-excursion
                       (org-with-point-at pos
                         (funcall action node-id param-values))))
                 (save-excursion
                   (org-with-point-at pos
                     (funcall action node-id))))))))))
    
    ;; function: direct execution
    ((pred functionp)
     (when-let ((pos (org-supertag-db-get-pos node-id)))
       (save-excursion
         (org-with-point-at pos
           (if params
               (apply behavior-spec node-id params)
             (funcall behavior-spec node-id))))))
    
    (_ (error "Invalid behavior spec: %S" behavior-spec))))

(defun org-supertag-behavior--do-execute (action &rest params)
  "Execute ACTION at current point with optional PARAMS."
  (pcase action
    ((pred stringp)
     (if params
         (apply #'org-supertag-behavior-execute 
                (org-id-get) action params)
       (org-supertag-behavior-execute 
        (org-id-get) action)))
    
    ((pred listp)
     (dolist (act action)
       (if params
           (apply #'org-supertag-behavior--do-execute act params)
         (org-supertag-behavior--do-execute act))))
    
    ((pred functionp)
     (if params
         (apply action params)
       (funcall action)))
    
    (_ (error "Invalid action: %S" action))))


(defun org-supertag-behavior--parse-param-string (param-str param-names)
  "Parse parameter string into plist based on param names.
PARAM-STR is string like \"red,yellow,bold\"
PARAM-NAMES is list of parameter names like '(fg bg weight)

Returns plist like (:fg \"red\" :bg \"yellow\" :weight \"bold\")"
  (let* ((values (split-string param-str "," t "[ \t\n\r]+"))
         (result nil))
    (cl-loop for name in param-names
             for value in values
             do (setq result 
                      (plist-put result 
                                (intern (concat ":" (symbol-name name)))
                                value)))
    result))

;;------------------------------------------------------------------------------
;; Behavior attach
;;------------------------------------------------------------------------------

(defun org-supertag-behavior-attach (tag-name behavior-name)
  "Attach BEHAVIOR-NAME to the tag specified by TAG-NAME."
  (interactive
   (let* ((tag-name (completing-read "Tag name: " 
                                    (org-supertag-get-all-tags)))
          (behavior-name (completing-read "Behavior name: " 
                                        (ht-keys org-supertag-behavior-registry))))
     (list tag-name behavior-name)))
  
  (let* ((tag (org-supertag-tag-get tag-name))
         (behavior (gethash behavior-name org-supertag-behavior-registry)))
    (message "Attaching behavior: %s to tag: %s" behavior-name tag-name)
    (message "Behavior definition: %S" behavior)
    
    (unless tag
      (error "Tag not found: %s" tag-name))
    (unless behavior
      (error "Behavior not found: %s" behavior-name))
    
    ;; 更新标签的 behaviors 属性
    (let* ((behaviors (or (plist-get tag :behaviors) '())))
      (org-supertag-tag-create 
       tag-name 
       :type :tag
       :behaviors (cons behavior-name behaviors)))
    
    ;; 确保当前在有效的 org 节点上并执行行为
    (when (org-at-heading-p)
      (let ((node-id (org-id-get-create)))
        (message "Debug - Before execute behavior: node=%s tag=%s" 
                 node-id tag-name)
        ;; 对当前节点的所有相关标签执行行为
        (dolist (tag-id (org-supertag-node-get-tags node-id))
          (when (equal tag-id tag-name)
            (org-supertag-behavior--on-tag-change node-id tag-id :add)))
        
        ;; 直接对当前节点应用样式
        (save-excursion
          ;; 清除当前行的旧 overlay
          (remove-overlays (line-beginning-position) 
                          (line-end-position) 
                          'org-supertag-face t)
          ;; 应用新样式
          (org-supertag-behavior--apply-styles node-id))))
    
    (message "Behavior '%s' attached to tag '%s'" behavior-name tag-name)))

;;------------------------------------------------------------------------------
;; Behavior Execute at Point
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--execute-behavior (node-id behavior-name &optional param-str)
  "Execute a single behavior BEHAVIOR-NAME on NODE-ID with optional PARAM-STR.

This function serves as a middle layer between high-level commands and
the core behavior execution system. It handles:
1. Behavior resolution and validation
2. Parameter parsing and normalization
3. Position management
4. Error handling and logging

Arguments:
- NODE-ID: The ID of the node to execute behavior on
- BEHAVIOR-NAME: Name of the behavior to execute
- PARAM-STR: Optional parameter string (e.g. \"DONE\" or \"red,bold\")

The execution follows these steps:
1. Resolve and validate behavior
2. Parse parameters if provided
3. Execute behavior with proper error handling
4. Log execution results

Example:
  ;; Execute simple behavior
  (org-supertag-behavior--execute-behavior node-id \"@todo\")
  
  ;; Execute with parameters
  (org-supertag-behavior--execute-behavior node-id \"@todo\" \"DONE\")
  
  ;; Execute composite behavior
  (org-supertag-behavior--execute-behavior node-id \"@done+archive\")"
  (condition-case err
      (progn
        ;; 1. Resolve and validate behavior
        (let* ((behavior (or (gethash behavior-name org-supertag-behavior-registry)
                            (signal 'org-supertag-behavior-error
                                    (list :unknown-behavior behavior-name))))
               (action (plist-get behavior :action))
               (behavior-list (plist-get behavior :list)))
          
          (message "Debug execute-behavior - node=%s behavior=%s action=%S list=%S"
                  node-id behavior-name action behavior-list)
          ;; 2. Execute based on behavior type
          (when-let ((pos (org-supertag-db-get-pos node-id)))
            (save-excursion
              (org-with-point-at pos
                (cond
                 ;; Direct action
                 (action
                  (message "Debug execute-behavior - Executing direct action")
                  (if param-str
                      (org-supertag-behavior-execute 
                       node-id behavior-name param-str)
                    (org-supertag-behavior-execute 
                     node-id behavior-name)))
                 ;; Behavior list
                 (behavior-list
                  (message "Debug execute-behavior - Executing behavior list")
                  (dolist (spec behavior-list)
                    (let* ((parts (split-string spec "="))
                           (name (car parts))
                           (args (cadr parts)))
                      (message "Debug execute-behavior - Running: %s with args: %s"
                              name args)
                      (if args
                          (org-supertag-behavior-execute node-id name args)
                        (org-supertag-behavior-execute node-id name)))))
                 ;; Invalid behavior type
                 (t (signal 'org-supertag-behavior-error
                           (list :invalid-behavior-type behavior-name)))))
              ;; 3. Log success
              (message "Successfully executed behavior %s on node %s"
                      behavior-name node-id)))))
    ;; Error handling
    (error
     (org-supertag-behavior--handle-error 
      err node-id behavior-name 'execute-behavior)
     (signal (car err) (cdr err)))))


(defun org-supertag--validate-position ()
  "Ensure current position is valid for node operations."
  ;; 1. 首先尝试展开可见性
  (org-fold-show-all '(headings))
  
  ;; 2. 确保文件有标题
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward org-heading-regexp nil t)
      (user-error "No headlines found in buffer")))
  
  ;; 3. 如果不在标题上，安全地移动到标题
  (unless (org-at-heading-p)
    (condition-case nil
        (progn
          (outline-previous-heading)
          (unless (org-at-heading-p)
            (outline-next-heading)))
      (error
       (condition-case nil
           (outline-next-heading)
         (error
          (user-error "Cannot find a valid heading position"))))))
  
  ;; 4. 最后确认位置有效
  (unless (org-at-heading-p)
    (user-error "Please move cursor to a headline")))

(defun org-supertag-behavior-execute-at-point ()
  "Execute a behavior on the current node."
  (interactive)
  ;; 添加位置验证
  (org-supertag--validate-position)
  (when-let* ((node-id (org-id-get-create))
              (behavior-name (completing-read "Behavior: " 
                                           (ht-keys org-supertag-behavior-registry))))
    (message "Debug execute-at-point - node=%s behavior=%s" 
             node-id behavior-name)
    (org-supertag-behavior--execute-behavior node-id behavior-name)))

(defun org-supertag-behavior-execute-batch ()
  "Execute multiple behaviors on the current node in sequence.
Prompts for a list of behaviors to execute."
  (interactive)
  (when-let* ((node-id (org-id-get-create))
              (behaviors (completing-read-multiple "Behaviors: " 
                                                (ht-keys org-supertag-behavior-registry))))
    (message "Debug execute-batch - node=%s behaviors=%S" node-id behaviors)
    (dolist (behavior-name behaviors)
      (org-supertag-behavior--execute-behavior node-id behavior-name))))

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
  (unless org-supertag-behavior--initialized
    ;; 确保 org-id 系统正确初始化
    (require 'org-id)
    
    ;; 基本钩子
    (add-hook 'org-supertag-after-node-change-hook
              #'org-supertag-behavior--handle-node-change)
    (add-hook 'org-supertag-after-tag-add-hook
              #'org-supertag-behavior--handle-tag-add)
    (add-hook 'org-supertag-after-tag-remove-hook
              #'org-supertag-behavior--handle-tag-remove)
    (add-hook 'org-supertag-after-load-hook
              #'org-supertag-behavior--setup-scheduled-behaviors)
    
    ;; 添加 ID 位置保护
    (advice-add 'org-supertag-behavior--handle-node-change 
                :around #'org-supertag-behavior--protect-id-locations)
    
    (setq org-supertag-behavior--initialized t))
  (message "=== Behavior System Init Success ==="))

(defun org-supertag-behavior--protect-id-locations (orig-fun &rest args)
  "确保在行为执行前后 org-id-locations 保持正确状态."
  ;; 保存当前状态
  (let ((old-locations (when (boundp 'org-id-locations) 
                        org-id-locations)))
    (unwind-protect
        (progn
          ;; 确保是 hash table
          (unless (and (boundp 'org-id-locations)
                      (hash-table-p org-id-locations))
            (setq org-id-locations (make-hash-table :test 'equal)))
          ;; 执行原函数
          (apply orig-fun args))
      ;; 恢复状态
      (when old-locations
        (setq org-id-locations old-locations)))))

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
  ;; 移除钩子
  (remove-hook 'org-supertag-after-node-change-hook
               #'org-supertag-behavior--handle-node-change)
  (remove-hook 'org-supertag-after-tag-add-hook
               #'org-supertag-behavior--handle-tag-add)
  (remove-hook 'org-supertag-after-tag-remove-hook
               #'org-supertag-behavior--handle-tag-remove)
  (remove-hook 'org-supertag-after-load-hook
               #'org-supertag-behavior--setup-scheduled-behaviors)
  
  ;; 移除建议
  (advice-remove 'org-supertag-behavior--handle-node-change 
                #'org-supertag-behavior--protect-id-locations))

(defun org-supertag-behavior--handle-todo-change ()
  "Handle TODO state changes.
Called by `org-after-todo-state-change-hook'."
  (when-let* ((node-id (org-id-get))
              (new-state (org-get-todo-state)))
    ;; 获取节点上的所有行为
    (dolist (tag-id (org-supertag-node-get-tags node-id))
      (when-let* ((behavior (org-supertag-behavior--get-behavior tag-id))
                  (trigger (plist-get behavior :trigger)))
        ;; 检查是否需要触发
        (when (eq trigger :on-todo-change)
          (org-supertag-behavior-execute node-id behavior))))))

(defun org-supertag-behavior--handle-property-change (property value)
  "Handle property changes.
Called by `org-property-changed-functions'.
PROPERTY is the changed property name.
VALUE is the new value."
  (when-let* ((node-id (org-id-get)))
    (dolist (tag-id (org-supertag-node-get-tags node-id))
      (when-let* ((behavior (org-supertag-behavior--get-behavior tag-id))
                  (trigger (plist-get behavior :trigger)))
        (when (eq trigger :on-property-change)
          (org-supertag-behavior-execute node-id behavior))))))

(defun org-supertag-behavior--handle-tags-change ()
  "Handle tags changes.
Called by `org-after-tags-change-hook'."
  (when-let* ((node-id (org-id-get)))
    (dolist (tag-id (org-supertag-node-get-tags node-id))
      (when-let* ((behavior (org-supertag-behavior--get-behavior tag-id))
                  (trigger (plist-get behavior :trigger)))
        (when (eq trigger :on-tags-change)
          (org-supertag-behavior-execute node-id behavior))))))

(defun org-supertag-behavior--handle-timestamp-change ()
  "Handle timestamp changes.
Called by `org-timestamp-change-hook'."
  (when-let* ((node-id (org-id-get)))
    (dolist (tag-id (org-supertag-node-get-tags node-id))
      (when-let* ((behavior (org-supertag-behavior--get-behavior tag-id))
                  (trigger (plist-get behavior :trigger)))
        (when (eq trigger :on-timestamp-change)
          (org-supertag-behavior-execute node-id behavior))))))

(defun org-supertag-behavior--handle-cycle (&rest _args)
  "Handle outline state changes.
Called by `org-cycle-hook'.
_ARGS are ignored cycle hook arguments."
  (when-let* ((node-id (org-id-get)))
    (dolist (tag-id (org-supertag-node-get-tags node-id))
      (when-let* ((behavior (org-supertag-behavior--get-behavior tag-id))
                  (trigger (plist-get behavior :trigger)))
        (when (eq trigger :on-cycle)
          (org-supertag-behavior-execute node-id behavior))))))



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
        (condition-case err
            (if (and beg end)
                ;; 增量更新：只处理变化的区域
                (progn
                  (goto-char (max (point-min) beg))
                  (when (org-at-heading-p)
                    (when-let ((node-id (org-id-get)))
                      ;; 先清除旧的 overlay
                      (remove-overlays (line-beginning-position) 
                                     (line-end-position) 
                                     'org-supertag-face t)
                      ;; 应用新的样式
                      (org-supertag-behavior--apply-styles node-id))))
              ;; 全局刷新：使用数据库获取当前 buffer 的所有节点
              (let ((buffer-file (buffer-file-name)))
                ;; 清除所有旧的 overlay
                (remove-overlays (point-min) (point-max) 
                               'org-supertag-face t)
                ;; 遍历所有标题，找到有 ID 的节点
                (org-with-wide-buffer
                 (goto-char (point-min))
                 (while (< (point) (point-max))
                   (when (and (org-at-heading-p)
                             (org-id-get))
                     (let ((node-id (org-id-get)))
                       (when (org-supertag-node-get-tags node-id)
                         (org-supertag-behavior--apply-styles node-id))))
                   ;; 移动到下一个标题或文件末尾
                   (if (outline-next-heading)
                       (forward-line 0)
                     (goto-char (point-max)))))))
          (error
           (message "Error during face refresh: %S" err)))))))
           
(defun org-supertag-behavior--setup-buffer ()
  "Setup current buffer for org-supertag behaviors."
  (when (derived-mode-p 'org-mode)
    ;; 延迟初始化，等待 org-mode 完全加载
    (run-with-idle-timer 
     0.5 nil
     (lambda ()
       (when (buffer-live-p (current-buffer))
         ;; 增量更新钩子
         (add-hook 'after-save-hook 
                  #'org-supertag-behavior--face-refresh nil t)
         (add-hook 'org-after-tags-change-hook 
                  #'org-supertag-behavior--face-refresh nil t)
         ;; 初始刷新
         (org-supertag-behavior--face-refresh))))))

(define-minor-mode org-supertag-behavior-mode
  "Toggle org-supertag behavior system."
  :global t
  :group 'org-supertag
  (if org-supertag-behavior-mode
      (progn
        ;; Enable - 延迟初始化
        (run-with-idle-timer 
         0.1 nil
         (lambda ()
           (org-supertag-behavior--init)
           ;; 只设置当前 buffer
           (when (derived-mode-p 'org-mode)
             (org-supertag-behavior--setup-buffer))
           ;; 为新打开/切换的 org buffers 设置
           (add-hook 'org-mode-hook #'org-supertag-behavior--setup-buffer)))
        t)  ; 返回 t 表示成功启用
    ;; Disable
    (progn
      (org-supertag-behavior--cleanup)
      (remove-hook 'org-mode-hook #'org-supertag-behavior--setup-buffer)
      ;; 只清理当前 buffer
      (when (derived-mode-p 'org-mode)
        (remove-hook 'after-save-hook 
                    #'org-supertag-behavior--face-refresh t)
        (remove-hook 'org-after-tags-change-hook 
                    #'org-supertag-behavior--face-refresh t)))))

;; 确保在包加载时启用
(defun org-supertag-behavior-setup ()
  "Setup org-supertag behavior system."
  (org-supertag-behavior-mode 1))

(add-hook 'org-supertag-after-load-hook
          #'org-supertag-behavior-setup)


 

(provide 'org-supertag-behavior)


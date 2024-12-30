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

(defcustom org-supertag-log-level :info
  "Control the verbosity of logging.
Possible values:
- :error   Only log errors
- :warn    Log warnings and errors
- :info    Log general information (default)
- :debug   Log detailed debug information"
  :type '(choice
          (const :tag "Error" :error)
          (const :tag "Warning" :warn)
          (const :tag "Info" :info)
          (const :tag "Debug" :debug))
  :group 'org-supertag-behavior)

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

(defun org-supertag-behavior-register (tag-name &rest props)
  "Register a new behavior with TAG-NAME and properties PROPS.

This function is the core of the behavior registration system. It handles:
1. Behavior validation and normalization
2. Registry management
3. Tag association
4. Hook setup
5. Error handling

Arguments:
- TAG-NAME: Name of the behavior (e.g. \"@todo\")
- PROPS: Property list with keys:
  * :trigger - When to execute (:on-add :on-remove :on-change :on-schedule :always)
  * :action - Function to execute or behavior list
  * :style - Visual properties (optional)
  * :hooks - List of (hook-name . hook-function) pairs (optional)
  * :params - List of parameter names (optional)
  * :list - List of behaviors to execute (optional)

The registration process:
1. Sanitize tag name and validate properties
2. Create or update behavior in registry
3. Create or update associated tag
4. Setup any specified hooks
5. Apply any immediate effects

Example:
  ;; Register simple behavior
  (org-supertag-behavior-register \"@todo\"
    :trigger :on-add
    :action #'set-todo-state
    :params '(state)
    :style '(:face (:foreground \"blue\")))
  
  ;; Register composite behavior
  (org-supertag-behavior-register \"@done+archive\"
    :trigger :on-add
    :list '(\"@todo=DONE\"
           \"@property=ARCHIVE_TIME,now\"
           \"@archive\")
    :style '(:face (:foreground \"gray50\")))"
  (condition-case err
      (let* ((tag-id (org-supertag-sanitize-tag-name tag-name))
             (behavior (list :trigger (plist-get props :trigger)
                           :action (plist-get props :action)
                           :style (plist-get props :style)
                           :hooks (plist-get props :hooks)
                           :params (plist-get props :params)
                           :list (plist-get props :list))))
        
        ;; 1. Validate behavior definition
        (org-supertag-behavior--validate behavior)
        
        ;; 2. Register in behavior registry
        (message "Debug register - Registering behavior: %s" tag-name)
        (puthash tag-name behavior org-supertag-behavior-registry)
        
        ;; 3. Create or update associated tag
        (let* ((tag (or (org-supertag-tag-get tag-id)
                       (org-supertag-tag-create tag-id)))
               (behaviors (or (plist-get tag :behaviors) '())))
          (unless (member tag-name behaviors)
            (message "Debug register - Updating tag: %s with behavior: %s"
                    tag-id tag-name)
            (org-supertag-tag-create 
             tag-id 
             :type :tag
             :behaviors (cons tag-name behaviors))))
        
        ;; 4. Setup hooks
        (when-let ((hooks (plist-get props :hooks)))
          (message "Debug register - Setting up hooks: %S" hooks)
          (dolist (hook-spec hooks)
            (add-hook (car hook-spec) (cdr hook-spec))))
        
        ;; 5. Return registered behavior
        (message "Successfully registered behavior: %s" tag-name)
        behavior)
    
    ;; Error handling
    (error
     (message "Error registering behavior %s: %s" 
              tag-name (error-message-string err))
     (signal (car err) (cdr err)))))

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

(defun org-supertag-behavior--normalize-params (params param-names)
  "Normalize PARAMS according to PARAM-NAMES.
PARAMS can be:
- string: Parse as comma-separated values
- plist: Return as is if valid
- list: Convert to plist if matches param names
Returns a plist with normalized parameters."
  (cond
   ;; String: Parse as comma-separated
   ((stringp params)
    (org-supertag-behavior--parse-param-string params param-names))
   
   ;; Plist: Validate and return
   ((and (org-supertag-behavior--plist-p params)
         (cl-every (lambda (name)
                    (plist-member params
                                 (intern (concat ":" (symbol-name name)))))
                  param-names))
    params)
   
   ;; List: Convert to plist
   ((and (listp params)
         (= (length params) (length param-names)))
    (cl-loop for name in param-names
             for value in params
             append (list (intern (concat ":" (symbol-name name)))
                         value)))
   
   ;; Invalid params
   (t (signal 'org-supertag-behavior-error
              (list :invalid-params params param-names)))))

(defun org-supertag-behavior--execute-action (node-id action params)
  "Execute ACTION with PARAMS on NODE-ID.
ACTION can be a function or behavior plist.
Handles position management and error cases."
  (when-let ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (condition-case err
            (cond
             ;; Function: Direct call
             ((functionp action)
              (if params
                  (funcall action node-id params)
                (funcall action node-id)))
             
             ;; Behavior plist: Extract action
             ((and (org-supertag-behavior--plist-p action)
                   (plist-get action :action))
              (let ((fn (plist-get action :action)))
                (if params
                    (funcall fn node-id params)
                  (funcall fn node-id))))
             
             ;; Invalid action
             (t (signal 'org-supertag-behavior-error
                       (list :invalid-action action))))
          (error
           (org-supertag-behavior--handle-error
            err node-id action 'execute-action)
           (signal (car err) (cdr err))))))))

(defun org-supertag-behavior--execute-list (node-id behavior-list)
  "Execute BEHAVIOR-LIST on NODE-ID.
Each behavior in list can be string spec 'name=args' or behavior plist."
  (dolist (spec behavior-list)
    (condition-case err
        (if (stringp spec)
            ;; String spec: Parse and execute
            (let* ((parts (split-string spec "="))
                   (name (car parts))
                   (args (cadr parts)))
              (message "Debug execute-list - Running: name=%s args=%s" 
                      name args)
              (org-supertag-behavior-execute node-id name args))
          ;; Behavior plist: Execute directly
          (org-supertag-behavior-execute node-id spec))
      (error
       (org-supertag-behavior--handle-error
        err node-id spec 'execute-list)
       (message "Warning: Failed to execute behavior %S: %s"
                spec (error-message-string err))))))

(defun org-supertag-behavior--resolve-behavior (behavior-spec)
  "Resolve BEHAVIOR-SPEC into a complete behavior definition.
BEHAVIOR-SPEC can be:
- string: Name of registered behavior
- plist: Complete behavior definition
- function: Wrapped as behavior"
  (cond
   ;; String: Look up in registry
   ((stringp behavior-spec)
    (or (gethash behavior-spec org-supertag-behavior-registry)
        (signal 'org-supertag-behavior-error
                (list :unknown-behavior behavior-spec))))
   
   ;; Plist: Validate and return
   ((org-supertag-behavior--plist-p behavior-spec)
    (if (or (plist-get behavior-spec :action)
            (plist-get behavior-spec :list))
        behavior-spec
      (signal 'org-supertag-behavior-error
              (list :invalid-behavior behavior-spec))))
   
   ;; Function: Wrap as behavior
   ((functionp behavior-spec)
    `(:action ,behavior-spec))
   
   ;; Invalid spec
   (t (signal 'org-supertag-behavior-error
              (list :invalid-behavior-spec behavior-spec)))))

(defun org-supertag-behavior-execute (node-id behavior-spec &rest params)
  "Execute behavior specified by BEHAVIOR-SPEC on NODE-ID with PARAMS.

This function is the core of the behavior execution system. It handles:
1. Parameter validation and normalization
2. Behavior resolution and execution
3. Error handling and logging
4. Position management

Arguments:
- NODE-ID: The ID of the node to execute behavior on
- BEHAVIOR-SPEC: Can be one of:
  * string: Name of a registered behavior
  * plist: Complete behavior definition
  * function: Direct function to execute
- PARAMS: Optional parameters for the behavior

The execution follows these steps:
1. Validate node exists and is accessible
2. Resolve behavior definition if needed
3. Parse and validate parameters
4. Execute behavior with proper error handling
5. Log execution results

Example:
  ;; Execute registered behavior
  (org-supertag-behavior-execute node-id \"@todo\" \"DONE\")
  
  ;; Execute behavior definition
  (org-supertag-behavior-execute node-id 
    '(:action some-fn :params (state))
    '(:state \"DONE\"))
  
  ;; Execute function directly
  (org-supertag-behavior-execute node-id #'some-fn \"arg1\" \"arg2\")"
  (condition-case err
      (progn
        ;; 1. Validate node
        (unless (org-supertag-behavior--validate-node node-id)
          (signal 'org-supertag-behavior-error 
                  (list :invalid-node node-id)))
        
        ;; 2. Resolve behavior
        (let* ((behavior (org-supertag-behavior--resolve-behavior behavior-spec))
               (param-names (plist-get behavior :params))
               (normalized-params (when (and param-names params)
                                  (org-supertag-behavior--normalize-params
                                   (car params) param-names))))
          
          ;; 3. Execute behavior
          (if (plist-get behavior :list)
              ;; Execute behavior list
              (org-supertag-behavior--execute-list
               node-id (plist-get behavior :list))
            ;; Execute single action
            (org-supertag-behavior--execute-action
             node-id behavior normalized-params))
          
          ;; 4. Log success
          (message "Successfully executed behavior %S on node %s"
                  behavior-spec node-id)))
    
    ;; Error handling
    (error
     (org-supertag-behavior--handle-error 
      err node-id behavior-spec 'execute)
     (signal (car err) (cdr err)))))

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
PARAM-STR is string like \"red,yellow,bold\" or nil
PARAM-NAMES is list of parameter names like '(fg bg weight)

Returns plist like (:fg \"red\" :bg \"yellow\" :weight \"bold\")"
  (when (and param-str param-names)  ; 只在两个参数都非空时处理
    (let* ((values (split-string param-str "," t "[ \t\n\r]+"))
           (result nil))
      (cl-loop for name in param-names
               for value in values
               do (setq result 
                       (plist-put result 
                                 (intern (concat ":" (symbol-name name)))
                                 value)))
      result)))

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
    ;; 添加 TODO 状态变化监听
    (add-hook 'org-after-todo-state-change-hook
              #'org-supertag-behavior--handle-todo-change)
    ;; 添加标准 org hooks
    (add-hook 'org-after-todo-state-change-hook
              #'org-supertag-behavior--handle-todo-change)
    (add-hook 'org-property-changed-functions
              #'org-supertag-behavior--handle-property-change)
    (add-hook 'org-after-tags-change-hook
              #'org-supertag-behavior--handle-tags-change)
    (add-hook 'org-timestamp-change-hook
              #'org-supertag-behavior--handle-timestamp-change)
    (add-hook 'org-cycle-hook
              #'org-supertag-behavior--handle-cycle)
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
  (remove-hook 'org-supertag-after-tag-apply-hook
               #'org-supertag-behavior--apply-styles)
  (remove-hook 'org-supertag-after-node-change-hook
               #'org-supertag-behavior--handle-node-change)
  (remove-hook 'org-supertag-after-tag-add-hook
               #'org-supertag-behavior--handle-tag-add)
  (remove-hook 'org-supertag-after-tag-remove-hook
               #'org-supertag-behavior--handle-tag-remove)
  (remove-hook 'org-supertag-after-load-hook
               #'org-supertag-behavior--setup-scheduled-behaviors)
  (remove-hook 'org-after-todo-state-change-hook
               #'org-supertag-behavior--handle-todo-change)
  (remove-hook 'org-after-todo-state-change-hook
               #'org-supertag-behavior--handle-todo-change)
  (remove-hook 'org-property-changed-functions
               #'org-supertag-behavior--handle-property-change)
  (remove-hook 'org-after-tags-change-hook
               #'org-supertag-behavior--handle-tags-change)
  (remove-hook 'org-timestamp-change-hook
               #'org-supertag-behavior--handle-timestamp-change)
  (remove-hook 'org-cycle-hook
               #'org-supertag-behavior--handle-cycle))

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

(defun org-supertag-behavior-execute-at-point ()
  "Execute a behavior on the current node.
Prompts for behavior name and parameters if needed."
  (interactive)
  (when-let* ((node-id (org-id-get-create))
              (behavior-name (completing-read "Behavior: " 
                                           (ht-keys org-supertag-behavior-registry))))
    (message "Debug execute-at-point - node=%s behavior=%s" node-id behavior-name)
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

;; 监听归档事件
(defun org-supertag-behavior--handle-archive (old-file new-file old-pos new-pos)
  "Handle org archive events.
Update database when a node is archived.
OLD-FILE and NEW-FILE are the source and target files.
OLD-POS and NEW-POS are the positions in those files."
  (message "\n=== Handling Archive Event ===")
  (message "Source: %s:%s" old-file old-pos)
  (message "Target: %s:%s" new-file new-pos)
  
  ;; 1. 在新位置获取节点信息
  (with-current-buffer (find-file-noselect new-file)
    (save-excursion
      (goto-char new-pos)
      (when-let ((node-id (org-id-get)))
        (message "Found node ID: %s" node-id)
        ;; 2. 清理标签
        (org-supertag-behavior--cleanup-tags node-id)
        ;; 3. 更新节点信息
        (condition-case err
            (progn
              (org-supertag-node-update)
              (message "Successfully updated node: %s" node-id))
          (error
           (message "Error updating node: %S" err)
           (signal 'org-supertag-behavior-error
                   (list :archive-update-failed node-id err))))))))

(defun org-supertag-behavior--setup-archive-hooks ()
  "Setup hooks for handling org archive events."
  ;; 在归档完成后更新数据库
  (advice-add 'org-archive-subtree :after
              (lambda (&rest _)
                (when-let* ((old-file (buffer-file-name))
                           (old-pos (point))
                           ;; 获取归档位置
                           (loc (org-archive--compute-location org-archive-location))
                           (new-file (car loc))
                           (new-pos (with-current-buffer (find-file-noselect new-file)
                                    (goto-char (point-max))
                                    (point))))
                  (org-supertag-behavior--handle-archive 
                   old-file new-file old-pos new-pos))))
  
  ;; 在归档到同级节点后更新数据库
  (advice-add 'org-archive-to-archive-sibling :after
              (lambda (&rest _)
                (when-let* ((file (buffer-file-name))
                           (old-pos (point))
                           (new-pos (save-excursion
                                    (org-archive-goto-sibling)
                                    (point))))
                  (org-supertag-behavior--handle-archive 
                   file file old-pos new-pos)))))

;; 在行为系统设置时启用归档钩子
(add-hook 'org-supertag-behavior-mode-hook
          (lambda ()
            (if org-supertag-behavior-mode
                (org-supertag-behavior--setup-archive-hooks)
              ;; 移除建议
              (advice-remove 'org-archive-subtree
                           #'org-supertag-behavior--handle-archive)
              (advice-remove 'org-archive-to-archive-sibling
                           #'org-supertag-behavior--handle-archive))))

(provide 'org-supertag-behavior)

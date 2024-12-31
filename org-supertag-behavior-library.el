;;; org-supertag-behavior-library.el --- Library functions for org-supertag behaviors -*- lexical-binding: t; -*-

;;; Commentary:

;; Provide various library functions to support org-supertag's behavior system
;; Each library focuses on functionality in specific domains, supporting behavior implementation and composition

;; Behavior Library Development Guide
;; ================================
;;
;; 1. Function Naming and Documentation
;; ----------------------------------
;; - Use 'org-supertag-behavior--' prefix for all functions
;; - Function names should reflect specific actions
;; - Comprehensive docstrings must include:
;;   * Purpose and functionality
;;   * Parameter descriptions
;;   * Return value details
;;   * Usage examples
;;   * Side effects (if any)
;;
;; Example:
;; ```elisp
;; (defun org-supertag-behavior--set-todo (node-id params)
;;   "Set TODO state for NODE-ID based on PARAMS.
;; 
;; NODE-ID is the unique identifier of the org node.
;; PARAMS is a plist with following keys:
;;   :state    - The target TODO state (e.g., \"TODO\", \"DONE\")
;;   :log      - Whether to log the state change (optional)
;;   :note     - Note to add with state change (optional)
;;
;; Returns t on success, nil on failure.
;;
;; Side effects:
;; - Modifies the TODO state of the target node
;; - May create a log entry if logging is enabled
;;
;; Examples:
;;   ;; Simple state change
;;   (org-supertag-behavior--set-todo node-id '(:state \"DONE\"))
;;
;;   ;; State change with logging
;;   (org-supertag-behavior--set-todo 
;;     node-id 
;;     '(:state \"DONE\" :log t :note \"Completed on time\"))")
;;
;; 2. Parameter Handling
;; -------------------
;; - Required parameters:
;;   * node-id: Node identifier (string)
;;   * params: Parameter plist
;; - Use plist-get for parameter extraction
;; - Validate parameters with when-let*
;; - Provide meaningful defaults when appropriate
;;
;; Example:
;; ```elisp
;; (defun org-supertag-behavior--example (node-id params)
;;   (when-let* ((required-param (plist-get params :required))
;;               (optional-param (or (plist-get params :optional) "default"))
;;               (pos (org-supertag-db-get-pos node-id)))
;;     ;; Function body here
;;     ))
;;
;; 3. Position Management
;; --------------------
;; - Get node position: (org-supertag-db-get-pos node-id)
;; - Protect current position: (save-excursion ...)
;; - Ensure correct position: (org-with-point-at pos ...)
;; - Handle buffer switching safely
;;
;; Example:
;; ```elisp
;; (defun org-supertag-behavior--safe-edit (node-id)
;;   (when-let ((pos (org-supertag-db-get-pos node-id)))
;;     (save-excursion
;;       (org-with-point-at pos
;;         ;; Safe editing here
;;         ))))
;;
;; 4. Error Handling
;; ---------------
;; - Use when-let* for nil value handling
;; - Add debug messages: (message "Debug ...")
;; - Use condition-case for error catching
;; - Provide meaningful error messages
;;
;; Example:
;; ```elisp
;; (defun org-supertag-behavior--safe-operation (node-id)
;;   (condition-case err
;;       (when-let* ((pos (org-supertag-db-get-pos node-id)))
;;         (message "Debug: Processing node at %s" pos)
;;         ;; Operation here
;;         )
;;     (error
;;      (message "Error in safe-operation: %S" err)
;;      nil)))


;;; Code:

(require 'org)


;;------------------------------------------------------------------------------
;; State Change Management
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--on-state-change (node-id params)
  "Handle state change for NODE-ID based on PARAMS.
PARAMS is a plist with :from and :to keys for state transition.
Optionally :note key for state change note.

Example:
  (org-supertag-behavior--on-state-change node-id 
    '(:from \"TODO\" :to \"DONE\" :note \"Completed task\"))"
  (message "Debug state-change - node=%s params=%S" node-id params)
  (when-let* ((from (plist-get params :from))
              (to (plist-get params :to))
              (note (plist-get params :note))
              (pos (org-supertag-db-get-pos node-id)))
    (message "Debug state-change - Changing state from %s to %s" from to)
    (save-excursion
      (org-with-point-at pos
        ;; 记录状态变更
        (when note
          (org-add-log-note))
        ;; 触发 org-trigger-hook
        (run-hook-with-args 'org-trigger-hook
                           (list :type 'state-change
                                 :position pos
                                 :from from
                                 :to to))))))

(defun org-supertag-behavior--update-statistics (node-id _params)
  "Update TODO statistics cookies for NODE-ID.
This function doesn't require any parameters but follows the behavior function
signature for consistency.

Example:
  (org-supertag-behavior--update-statistics node-id nil)"
  (message "Debug update-stats - node=%s" node-id)
  (when-let ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (org-update-statistics-cookies nil)
(defun org-supertag-behavior--toggle-state (node-id params)
  "Toggle between two states for NODE-ID based on PARAMS.
PARAMS is a plist with :states (a list of two states) key.

Example:
  (org-supertag-behavior--toggle-state node-id 
    '(:states (\"TODO\" \"DONE\")))"
  (message "Debug toggle-state - node=%s params=%S" node-id params)
  (when-let* ((states (plist-get params :states))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let* ((current (org-get-todo-state))
               (next (if (equal current (car states))
                        (cadr states)
                      (car states))))
          (message "Debug toggle-state - Toggling from %s to %s" 
                   current next)
          (org-todo next))))))

(defun org-supertag-behavior--propagate-state (node-id params)
  "Propagate state change to children of NODE-ID based on PARAMS.
PARAMS is a plist with :state key and optional :recursive flag.

Example:
  ;; Propagate to immediate children
  (org-supertag-behavior--propagate-state node-id 
    '(:state \"DONE\"))
  ;; Propagate recursively
  (org-supertag-behavior--propagate-state node-id 
    '(:state \"DONE\" :recursive t))"
  (message "Debug propagate-state - node=%s params=%S" node-id params)
  (when-let* ((state (plist-get params :state))
              (recursive (plist-get params :recursive))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let ((level (org-outline-level)))
          (save-restriction
            (org-narrow-to-subtree)
            (while (outline-next-heading)
              (when (or (not recursive)
                       (= (org-outline-level) (1+ level)))
                (org-todo state)))))))))

;;------------------------------------------------------------------------------
;; Node Status
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--set-todo (node-id params)
  "Set TODO state for NODE-ID based on PARAMS.
PARAMS is a plist with :state key."
  (message "Debug set-todo - node=%s params=%S" node-id params)
  (when-let* ((state (plist-get params :state))
              (pos (org-supertag-db-get-pos node-id)))
    (message "Debug set-todo - Setting state to: %s" state)
    (save-excursion
      (org-with-point-at pos
        (org-todo state)))))


(defun org-supertag-behavior--set-priority (node-id params)
  "Set priority for NODE-ID based on PARAMS.
PARAMS is a plist with :priority key (A, B, or C).

Example:
  (org-supertag-behavior--set-priority node-id '(:priority \"A\"))"
  (message "Debug set-priority - node=%s params=%S" node-id params)
  (when-let* ((priority (plist-get params :priority))
              (pos (org-supertag-db-get-pos node-id)))
    (message "Debug set-priority - Setting priority to: %s" priority)
    (save-excursion
      (org-with-point-at pos
        (org-priority (string-to-char priority))))))

;;------------------------------------------------------------------------------
;; Node Property
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--set-property (node-id params)
  "Set property for NODE-ID based on PARAMS.
PARAMS is a plist with :name and :value keys.

Example:
  (org-supertag-behavior--set-property node-id '(:name \"CREATED\" :value \"2024-01-20\"))"
  (message "Debug set-property - node=%s params=%S" node-id params)
  (when-let* ((name (plist-get params :name))
              (value (plist-get params :value))
              (pos (org-supertag-db-get-pos node-id)))
    (message "Debug set-property - Setting %s to: %s" name value)
    (save-excursion
      (org-with-point-at pos
        (org-set-property name value)))))

(defun org-supertag-behavior--delete-property (node-id params)
  "Delete property for NODE-ID based on PARAMS.
PARAMS is a plist with :name key.

Example:
  (org-supertag-behavior--delete-property node-id '(:name \"CREATED\"))"
  (message "Debug delete-property - node=%s params=%S" node-id params)
  (when-let* ((name (plist-get params :name))
              (pos (org-supertag-db-get-pos node-id)))
    (message "Debug delete-property - Deleting property: %s" name)
    (save-excursion
      (org-with-point-at pos
        (org-delete-property name)))))

;;------------------------------------------------------------------------------
;; Node Heading
;;------------------------------------------------------------------------------  

(defun org-supertag-behavior--set-heading (node-id params)
  "Set heading for NODE-ID based on PARAMS.
PARAMS is a plist with :title key.

Example:
  (org-supertag-behavior--set-heading node-id '(:title \"New Title\"))"
  (message "Debug set-heading - node=%s params=%S" node-id params)
  (when-let* ((title (plist-get params :title))
              (pos (org-supertag-db-get-pos node-id)))
    (message "Debug set-heading - Setting title to: %s" title)
    (save-excursion
      (org-with-point-at pos
        (org-edit-headline title)))))


;;------------------------------------------------------------------------------
;; Property Management
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--get-property (node-id params)
  "Get property value for NODE-ID based on PARAMS.
PARAMS is a plist with :name key and optional :inherit flag.

Example:
  ;; Get local property
  (org-supertag-behavior--get-property node-id '(:name \"CATEGORY\"))
  ;; Get with inheritance
  (org-supertag-behavior--get-property node-id '(:name \"CATEGORY\" :inherit t))"
  (message "Debug get-property - node=%s params=%S" node-id params)
  (when-let* ((name (plist-get params :name))
              (inherit (plist-get params :inherit))
              (pos (org-supertag-db-get-pos node-id)))
    (message "Debug get-property - Getting %s (inherit=%s)" name inherit)
    (save-excursion
      (org-with-point-at pos
        (if inherit
            (org-entry-get nil name t)  ; t means inherit
          (org-entry-get nil name))))))

(defun org-supertag-behavior--get-properties (node-id params)
  "Get all properties for NODE-ID based on PARAMS.
PARAMS is a plist with optional :type key (:all, :inherited, or :local).
Default is :local.

Example:
  ;; Get local properties
  (org-supertag-behavior--get-properties node-id '(:type :local))
  ;; Get all properties including inherited
  (org-supertag-behavior--get-properties node-id '(:type :all))"
  (message "Debug get-properties - node=%s params=%S" node-id params)
  (when-let* ((type (or (plist-get params :type) :local))
              (pos (org-supertag-db-get-pos node-id)))
    (message "Debug get-properties - Getting properties type: %s" type)
    (save-excursion
      (org-with-point-at pos
        (pcase type
          (:all (org-entry-properties nil))
          (:inherited (org-entry-properties nil 'inherited))
          (:local (org-entry-properties nil 'standard))
          (_ (error "Invalid property type: %s" type)))))))

(defun org-supertag-behavior--copy-property (node-id params)
  "Copy property from one entry to another for NODE-ID based on PARAMS.
PARAMS is a plist with :name, :from-id, and optional :if-missing keys.

Example:
  ;; Copy property if it doesn't exist locally
  (org-supertag-behavior--copy-property node-id 
    '(:name \"CATEGORY\" :from-id \"parent-id\" :if-missing t))"
  (message "Debug copy-property - node=%s params=%S" node-id params)
  (when-let* ((name (plist-get params :name))
              (from-id (plist-get params :from-id))
              (if-missing (plist-get params :if-missing))
              (pos (org-supertag-db-get-pos node-id))
              (from-pos (org-supertag-db-get-pos from-id)))
    (message "Debug copy-property - Copying %s from %s" name from-id)
    (save-excursion
      (org-with-point-at from-pos
        (when-let ((value (org-entry-get nil name)))
          (org-with-point-at pos
            (when (or (not if-missing)
                     (not (org-entry-get nil name)))
              (org-entry-put nil name value))))))))

(defun org-supertag-behavior--track-ordered-property (node-id params)
  "Set ORDERED property and corresponding tag for NODE-ID based on PARAMS.
PARAMS is a plist with :value key (t or nil).

Example:
  (org-supertag-behavior--track-ordered-property node-id '(:value t))"
  (message "Debug track-ordered - node=%s params=%S" node-id params)
  (when-let* ((value (plist-get params :value))
              (pos (org-supertag-db-get-pos node-id)))
    (message "Debug track-ordered - Setting ORDERED to: %s" value)
    (save-excursion
      (org-with-point-at pos
        ;; 设置 ORDERED 属性
        (if value
            (org-entry-put nil "ORDERED" "t")
          (org-entry-delete nil "ORDERED"))
        ;; 如果启用了 org-track-ordered-property-with-tag
        (when org-track-ordered-property-with-tag
          (if value
              (org-toggle-tag "ORDERED" 'on)
            (org-toggle-tag "ORDERED" 'off)))))))

;;------------------------------------------------------------------------------
;; Drawer Management
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--insert-drawer (node-id params)
  "Insert drawer for NODE-ID based on PARAMS.
PARAMS is a plist with keys:
- :name : Drawer name
- :content : Optional initial content
- :region : Whether to wrap region (t or nil)

Example:
  ;; Insert empty drawer
  (org-supertag-behavior--insert-drawer node-id 
    '(:name \"NOTES\"))
  ;; Insert drawer with content
  (org-supertag-behavior--insert-drawer node-id 
    '(:name \"DETAILS\" 
      :content \"Initial content\"))
  ;; Insert drawer around region
  (org-supertag-behavior--insert-drawer node-id 
    '(:name \"EXAMPLE\" :region t))"
  (message "Debug insert-drawer - node=%s params=%S" node-id params)
  (when-let* ((name (plist-get params :name))
              (content (plist-get params :content))
              (region (plist-get params :region))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        ;; 插入抽屉
        (condition-case err
            (progn
              (if region
                  (org-insert-drawer)
                (insert ":" name ":\n:END:\n")
                (forward-line -2))
              ;; 如果有内容，插入内容
              (when content
                (forward-line 1)
                (insert content "\n")))
          (error
           (message "Error inserting drawer: %S" err)))))))

(defun org-supertag-behavior--log-into-drawer (node-id params)
  "Configure logging into drawer for NODE-ID based on PARAMS.
PARAMS is a plist with keys:
- :enabled : Whether to enable drawer logging
- :name : Optional custom drawer name
- :note : Optional note to add

Example:
  ;; Enable logging into default drawer
  (org-supertag-behavior--log-into-drawer node-id 
    '(:enabled t))
  ;; Enable logging into custom drawer
  (org-supertag-behavior--log-into-drawer node-id 
    '(:enabled t :name \"HISTORY\"))
  ;; Add note to drawer
  (org-supertag-behavior--log-into-drawer node-id 
    '(:enabled t :note \"Status update\"))"
  (message "Debug log-drawer - node=%s params=%S" node-id params)
  (when-let* ((enabled (plist-get params :enabled))
              (name (plist-get params :name))
              (note (plist-get params :note))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        ;; 设置抽屉日志
        (let ((org-log-into-drawer (if enabled
                                      (or name t)
                                    nil)))
          ;; 如果需要添加注释
          (when note
            (org-add-note note)))))))

(defun org-supertag-behavior--format-drawer (node-id params)
  "Format drawer for NODE-ID based on PARAMS.
PARAMS is a plist with keys:
- :name : Drawer name to format
- :format : Format type (:indent, :html, :latex, etc.)
- :style : Optional style properties

Example:
  ;; Indent drawer
  (org-supertag-behavior--format-drawer node-id 
    '(:name \"NOTES\" :format :indent))
  ;; Format for HTML
  (org-supertag-behavior--format-drawer node-id 
    '(:name \"DETAILS\" 
      :format :html 
      :style (:class \"custom-drawer\")))"
  (message "Debug format-drawer - node=%s params=%S" node-id params)
  (when-let* ((name (plist-get params :name))
              (format-type (plist-get params :format))
              (style (plist-get params :style))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (condition-case err
            (pcase format-type
              (:indent
               (org-indent-drawer))
              (:html
               (let ((org-html-format-drawer-function
                      (lambda (name contents info)
                        (format "<div class=\"%s\">%s</div>"
                                (plist-get style :class)
                                contents))))
                 (org-html-format-drawer name "" nil)))
              (:latex
               (let ((org-latex-format-drawer-function
                      (lambda (name contents info)
                        (format "\\begin{%s}\n%s\\end{%s}"
                                name contents name))))
                 (org-latex-format-drawer name "" nil)))
              (_ (error "Unsupported format type: %s" format-type)))
          (error
           (message "Error formatting drawer: %S" err)))))))

(defun org-supertag-behavior--export-drawer (node-id params)
  "Configure drawer export for NODE-ID based on PARAMS.
PARAMS is a plist with keys:
- :drawers : List of drawer names or t for all
- :exclude : Whether to exclude listed drawers
- :properties : Whether to include properties drawer

Example:
  ;; Export all drawers
  (org-supertag-behavior--export-drawer node-id 
    '(:drawers t))
  ;; Export specific drawers
  (org-supertag-behavior--export-drawer node-id 
    '(:drawers (\"NOTES\" \"DETAILS\")))
  ;; Exclude specific drawers
  (org-supertag-behavior--export-drawer node-id 
    '(:drawers (\"LOGBOOK\") :exclude t))"
  (message "Debug export-drawer - node=%s params=%S" node-id params)
  (when-let* ((drawers (plist-get params :drawers))
              (exclude (plist-get params :exclude))
              (properties (plist-get params :properties))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        ;; 设置导出选项
        (let ((org-export-with-drawers
               (if exclude
                   (cons 'not drawers)
                 drawers))
              (org-export-with-properties properties))
          ;; 可以在这里添加其他导出相关的操作
          )))))

;;------------------------------------------------------------------------------
;; TODO State Management
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--set-todo-with-log (node-id params)
  "Set TODO state for NODE-ID with logging based on PARAMS.
PARAMS is a plist with keys:
- :state : Target TODO state
- :log-type : Type of logging (:time, :note, or nil)
- :note : Note text when log-type is :note

Example:
  ;; Set state with timestamp
  (org-supertag-behavior--set-todo-with-log node-id 
    '(:state \"DONE\" :log-type :time))
  ;; Set state with note
  (org-supertag-behavior--set-todo-with-log node-id 
    '(:state \"DONE\" :log-type :note :note \"Completed early\"))"
  (message "Debug set-todo-log - node=%s params=%S" node-id params)
  (when-let* ((state (plist-get params :state))
              (log-type (plist-get params :log-type))
              (note (plist-get params :note))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        ;; 临时设置日志选项
        (let ((org-log-done 
               (pcase log-type
                 (:time 'time)
                 (:note 'note)
                 (_ nil))))
          ;; 设置状态
          (org-todo state)
          ;; 如果需要添加注释
          (when (and (eq log-type :note) note)
            (org-add-note note)))))))

(defun org-supertag-behavior--set-todo-with-faces (node-id params)
  "Set TODO state with custom faces for NODE-ID based on PARAMS.
PARAMS is a plist with keys:
- :state : Target TODO state
- :face : Face properties plist

Example:
  (org-supertag-behavior--set-todo-with-faces node-id 
    '(:state \"DONE\" 
      :face (:foreground \"green\" :weight bold)))"
  (message "Debug set-todo-faces - node=%s params=%S" node-id params)
  (when-let* ((state (plist-get params :state))
              (face (plist-get params :face))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        ;; 临时设置 face
        (let ((org-todo-keyword-faces
               (cons (cons state face)
                     org-todo-keyword-faces)))
          (org-todo state))))))

(defun org-supertag-behavior--set-todo-with-state-change (node-id params)
  "Set TODO state with state change hook for NODE-ID based on PARAMS.
PARAMS is a plist with keys:
- :state : Target TODO state
- :hook-fn : Function to run after state change
- :hook-args : Arguments for hook function

Example:
  (org-supertag-behavior--set-todo-with-state-change node-id 
    '(:state \"DONE\" 
      :hook-fn some-function
      :hook-args (arg1 arg2)))"
  (message "Debug set-todo-state-change - node=%s params=%S" node-id params)
  (when-let* ((state (plist-get params :state))
              (hook-fn (plist-get params :hook-fn))
              (hook-args (plist-get params :hook-args))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let ((old-state (org-get-todo-state)))
          ;; 添加临时钩子
          (add-hook 'org-todo-state-change-hook
                   (lambda ()
                     (apply hook-fn hook-args)))
          ;; 设置状态
          (org-todo state)
          ;; 移除临时钩子
          (remove-hook 'org-todo-state-change-hook
                      (lambda ()
                        (apply hook-fn hook-args))))))))   

;;------------------------------------------------------------------------------
;; Priority Management
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--set-priority-with-faces (node-id params)
  "Set priority with custom faces for NODE-ID based on PARAMS.
PARAMS is a plist with keys:
- :priority : Priority character (A, B, or C)
- :face : Face properties plist

Example:
  (org-supertag-behavior--set-priority-with-faces node-id 
    '(:priority \"A\" 
      :face (:foreground \"red\" :weight bold)))"
  (message "Debug priority-faces - node=%s params=%S" node-id params)
  (when-let* ((priority (plist-get params :priority))
              (face (plist-get params :face))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        ;; 临时设置 face
        (let ((org-priority-faces
               (cons (cons (string-to-char priority) face)
                     org-priority-faces)))
          (org-priority (string-to-char priority)))))))

(defun org-supertag-behavior--cycle-priority (node-id params)
  "Cycle priority for NODE-ID based on PARAMS.
PARAMS is a plist with optional keys:
- :direction : :up or :down for cycling direction
- :start-default : Whether to start from default priority

Example:
  ;; Cycle up from current priority
  (org-supertag-behavior--cycle-priority node-id '(:direction :up))
  ;; Cycle down starting from default
  (org-supertag-behavior--cycle-priority node-id 
    '(:direction :down :start-default t))"
  (message "Debug cycle-priority - node=%s params=%S" node-id params)
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let* ((direction (plist-get params :direction))
               (start-default (plist-get params :start-default))
               (org-priority-start-cycle-with-default start-default))
          (pcase direction
            (:up (org-priority-up))
            (:down (org-priority-down))
            (_ (org-priority))))))))

(defun org-supertag-behavior--set-priority-range (node-id params)
  "Set priority range for NODE-ID based on PARAMS.
PARAMS is a plist with keys:
- :highest : Highest priority character
- :lowest : Lowest priority character
- :default : Default priority character

Example:
  (org-supertag-behavior--set-priority-range node-id 
    '(:highest \"A\" :lowest \"E\" :default \"C\"))"
  (message "Debug priority-range - node=%s params=%S" node-id params)
  (when-let* ((highest (plist-get params :highest))
              (lowest (plist-get params :lowest))
              (default (plist-get params :default))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        ;; 临时设置优先级范围
        (let ((org-highest-priority (string-to-char highest))
              (org-lowest-priority (string-to-char lowest))
              (org-default-priority (string-to-char default)))
          ;; 设置当前优先级为默认值
          (org-priority (string-to-char default)))))))

(defun org-supertag-behavior--priority-with-hook (node-id params)
  "Set priority with hook for NODE-ID based on PARAMS.
PARAMS is a plist with keys:
- :priority : Priority character
- :hook-fn : Function to run after priority change
- :hook-args : Arguments for hook function

Example:
  (org-supertag-behavior--priority-with-hook node-id 
    '(:priority \"A\" 
      :hook-fn some-function
      :hook-args (arg1 arg2)))"
  (message "Debug priority-hook - node=%s params=%S" node-id params)
  (when-let* ((priority (plist-get params :priority))
              (hook-fn (plist-get params :hook-fn))
              (hook-args (plist-get params :hook-args))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        ;; 添加临时钩子
        (add-hook 'org-after-todo-state-change-hook
                 (lambda ()
                   (apply hook-fn hook-args)))
        ;; 设置优先级
        (org-priority (string-to-char priority))
        ;; 移除临时钩子
        (remove-hook 'org-after-todo-state-change-hook
                    (lambda ()
                      (apply hook-fn hook-args)))))))

;;------------------------------------------------------------------------------
;; Clock Management
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--clock-in (node-id params)
  "Start clock on NODE-ID based on PARAMS.
PARAMS is a plist with optional keys:
- :switch-state : State to switch to when starting clock
- :resume : Whether to resume last clock if exists
- :use-last-clock : Whether to use last clock time as start

Example:
  ;; Simple clock in
  (org-supertag-behavior--clock-in node-id nil)
  ;; Clock in with state switch
  (org-supertag-behavior--clock-in node-id '(:switch-state \"STARTED\"))
  ;; Resume last clock
  (org-supertag-behavior--clock-in node-id '(:resume t))"
  (message "Debug clock-in - node=%s params=%S" node-id params)
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let* ((switch-state (plist-get params :switch-state))
               (resume (plist-get params :resume))
               (use-last-clock (plist-get params :use-last-clock)))
          ;; 设置临时状态切换值
          (when switch-state
            (setq-local org-clock-in-switch-to-state switch-state))
          ;; 设置是否恢复
          (setq-local org-clock-in-resume resume)
          ;; 设置是否使用上次时间
          (setq-local org-clock-continuously use-last-clock)
          ;; 启动时钟
          (condition-case err
              (org-clock-in)
            (error
             (message "Error starting clock: %S" err))))))))

(defun org-supertag-behavior--clock-out (node-id params)
  "Stop clock on NODE-ID based on PARAMS.
PARAMS is a plist with optional keys:
- :switch-state : State to switch to after stopping clock
- :note : Note to add when stopping clock

Example:
  ;; Simple clock out
  (org-supertag-behavior--clock-out node-id nil)
  ;; Clock out with state change
  (org-supertag-behavior--clock-out node-id 
    '(:switch-state \"DONE\" :note \"Completed task\"))"
  (message "Debug clock-out - node=%s params=%S" node-id params)
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let* ((switch-state (plist-get params :switch-state))
               (note (plist-get params :note)))
          ;; 设置临时状态切换值
          (when switch-state
            (setq-local org-clock-out-switch-to-state switch-state))
          ;; 停止时钟
          (condition-case err
              (org-clock-out nil nil)
            (error
             (message "Error stopping clock: %S" err)))
          ;; 添加注释
          (when note
            (org-add-note note)))))))

(defun org-supertag-behavior--clock-cancel (node-id _params)
  "Cancel clock on NODE-ID.
This function doesn't require any parameters but follows the behavior function
signature for consistency.

Example:
  (org-supertag-behavior--clock-cancel node-id nil)"
  (message "Debug clock-cancel - node=%s" node-id)
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (condition-case err
            (org-clock-cancel)
          (error
           (message "Error canceling clock: %S" err)))))))

(defun org-supertag-behavior--clock-report (node-id params)
  "Generate clock report for NODE-ID based on PARAMS.
PARAMS is a plist with optional keys:
- :scope : Report scope (:subtree, :file, or :all)
- :range : Time range (:today, :week, :month, or days-number)

Example:
  ;; Report for today
  (org-supertag-behavior--clock-report node-id 
    '(:scope :subtree :range :today))
  ;; Report for last 7 days
  (org-supertag-behavior--clock-report node-id 
    '(:scope :file :range 7))"
  (message "Debug clock-report - node=%s params=%S" node-id params)
  (when-let* ((pos (org-supertag-db-get-pos node-id))
              (scope (or (plist-get params :scope) :subtree))
    (save-excursion
      (org-with-point-at pos
        (let ((rangeval (pcase (or (plist-get params :range) :today)
                         (:today '(:today))
                         (:week '(:week))
                         (:month '(:month))
                         ((pred numberp) `(:tstart 
                                         ,(- (org-time-today) 
                                             (* range 86400))))
                         (_ '(:today)))))
          ;; 插入或更新时钟报告
          (condition-case err
              (org-clock-report `(:scope ,scope ,@rangeval))
            (error
             (message "Error generating clock report: %S" err)))))))))

;;------------------------------------------------------------------------------
;; Timer Management
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--timer-start (node-id params)
  "Start a timer for NODE-ID based on PARAMS.
PARAMS is a plist with optional keys:
- :offset : Timer offset in minutes or 'HH:MM:SS' format
- :format : Custom format string for timer display

Example:
  ;; Start timer with default format
  (org-supertag-behavior--timer-start node-id nil)
  ;; Start with offset
  (org-supertag-behavior--timer-start node-id '(:offset \"1:30:00\"))
  ;; Start with custom format
  (org-supertag-behavior--timer-start node-id 
    '(:format \"Timer: %s\"))"
  (message "Debug timer-start - node=%s params=%S" node-id params)
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let* ((offset (plist-get params :offset))
               (format (or (plist-get params :format)
                          org-timer-format)))
          ;; 设置临时格式
          (setq-local org-timer-format format)
          ;; 启动计时器
          (condition-case err
              (if offset
                  (org-timer-start offset)
                (org-timer-start))
            (error
             (message "Error starting timer: %S" err))))))))

(defun org-supertag-behavior--timer-set (node-id params)
  "Set countdown timer for NODE-ID based on PARAMS.
PARAMS is a plist with keys:
- :duration : Timer duration in minutes or 'HH:MM:SS' format
- :default : Whether to use org-timer-default-timer
- :effort : Whether to use Effort property
- :hook-fn : Function to run when timer expires

Example:
  ;; Set timer for 30 minutes
  (org-supertag-behavior--timer-set node-id '(:duration \"30\"))
  ;; Use default timer
  (org-supertag-behavior--timer-set node-id '(:default t))
  ;; Use effort property
  (org-supertag-behavior--timer-set node-id '(:effort t))"
  (message "Debug timer-set - node=%s params=%S" node-id params)
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let* ((duration (plist-get params :duration))
               (use-default (plist-get params :default))
               (use-effort (plist-get params :effort))
               (hook-fn (plist-get params :hook-fn)))
          ;; 设置临时钩子
          (when hook-fn
            (add-hook 'org-timer-done-hook hook-fn))
          ;; 设置计时器
          (condition-case err
              (cond
               (duration
                (org-timer-set-timer duration))
               (use-default
                (let ((org-timer-default-timer org-timer-default-timer))
                  (org-timer-set-timer '(16))))
               (use-effort
                (org-timer-set-timer))
               (t
                (org-timer-set-timer)))
            (error
             (message "Error setting timer: %S" err)))
          ;; 移除临时钩子
          (when hook-fn
            (remove-hook 'org-timer-done-hook hook-fn)))))))

(defun org-supertag-behavior--timer-pause (node-id params)
  "Pause or continue timer for NODE-ID based on PARAMS.
PARAMS is a plist with optional :stop flag to completely stop timer.

Example:
  ;; Pause/continue timer
  (org-supertag-behavior--timer-pause node-id nil)
  ;; Stop timer
  (org-supertag-behavior--timer-pause node-id '(:stop t))"
  (message "Debug timer-pause - node=%s params=%S" node-id params)
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let ((stop (plist-get params :stop)))
          (condition-case err
              (org-timer-pause-or-continue stop)
            (error
             (message "Error pausing timer: %S" err))))))))

(defun org-supertag-behavior--timer-item (node-id params)
  "Insert timer item for NODE-ID based on PARAMS.
PARAMS is a plist with optional :prefix key for custom item prefix.

Example:
  ;; Insert timer item with default prefix
  (org-supertag-behavior--timer-item node-id nil)
  ;; Insert with custom prefix
  (org-supertag-behavior--timer-item node-id 
    '(:prefix \"=> \"))"
  (message "Debug timer-item - node=%s params=%S" node-id params)
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let ((prefix (plist-get params :prefix)))
          (when prefix
            (setq-local org-list-description-max-indent 0)
            (setq-local org-timer-item-format prefix))
          (condition-case err
              (org-timer-item)
            (error
             (message "Error inserting timer item: %S" err))))))))

(defun org-supertag-behavior--timer-show-remaining (node-id _params)
  "Show remaining time for timer on NODE-ID.
This function doesn't require any parameters but follows the behavior function
signature for consistency.

Example:
  (org-supertag-behavior--timer-show-remaining node-id nil)"
  (message "Debug timer-remaining - node=%s" node-id)
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (condition-case err
            (org-timer-show-remaining-time)
          (error
           (message "Error showing remaining time: %S" err)))))))

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
          (when (member (concat "#" tag-id) tags)
            (when-let ((parent-id (org-id-get)))
              (when action-fn
                (funcall action-fn parent-id))
              parent-id)))))))

;;------------------------------------------------------------------------------
;; Archive Management
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--cleanup-tags (node-id)
  "Clean up supertags for NODE-ID after archiving.
Removes all tags starting with # from both the heading text and internal org state."
  (message "\n=== Cleaning up tags for node %s ===" node-id)
  (when-let ((pos (org-supertag-db-get-pos node-id)))
    (message "Found position: %s" pos)
    (save-excursion
      (org-with-point-at pos
        (org-back-to-heading t)
        ;; 1. Get current tags and heading
        (let* ((current-tags (org-get-tags))
              (line (buffer-substring (line-beginning-position) (line-end-position)))
              (_ (message "Original heading: %s" line))
              (_ (message "Original tags: %S" current-tags))
              ;; 2. Filter out supertags (starting with #)
              (new-tags (seq-remove (lambda (tag)
                                    (string-prefix-p "#" tag))
                                  current-tags))
              ;; 3. Clean up heading text
              (new-line (replace-regexp-in-string ":[#@][^:]+:" "" line))
              (_ (message "New heading: %s" new-line))
              (_ (message "New tags: %S" new-tags)))
          ;; 4. Update heading text
          (let ((inhibit-read-only t))
            (delete-region (line-beginning-position) (line-end-position))
            (insert new-line))
          ;; 5. Update org tags
          (org-set-tags new-tags)
          ;; 6. Clean up database associations
          (dolist (tag current-tags)
            (when (string-prefix-p "#" tag)
              (let ((tag-id (substring tag 1)))
                (org-supertag-tag--remove tag-id node-id)))))))))

(defun org-supertag-behavior--archive-subtree (node-id &optional params)
  "Archive subtree at NODE-ID.
PARAMS is a plist with optional keys:
- :file : Archive file path (default: org-archive-location)
- :headline : Archive headline (default: \"* Archive\")
- :inherit-tags : Whether to inherit tags (default: nil)"
  (message "\n=== Archiving subtree for node %s ===" node-id)
  (when-let ((pos (org-supertag-db-get-pos node-id)))
    (message "Found node at position: %s" pos)
    (save-excursion
      (org-with-point-at pos
        (let* ((file (or (plist-get params :file)
                        (if (string-match "::\\(.*\\)\\'" org-archive-location)
                            (substring org-archive-location 0 (match-beginning 0))
                          "archive.org")))
               (headline (or (plist-get params :headline)
                           (if (string-match "::\\(.*\\)\\'" org-archive-location)
                               (match-string 1 org-archive-location)
                             "* Archive")))
               (inherit-tags (plist-get params :inherit-tags))
               (archive-location (concat file "::" headline)))
          
          (message "Archive location: %s" archive-location)
          (message "Current heading: %s" (org-get-heading t t t t))
          
          ;; 确保归档文件存在
          (unless (file-exists-p file)
            (message "Creating archive file: %s" file)
            (with-temp-buffer
              (insert "#+TITLE: Archive\n\n* Archive\n")
              (write-file file)))
          
          ;; 执行归档
          (let ((org-archive-location archive-location))
            ;; 设置继承标签选项（不使用 let 绑定）
            (setq-default org-archive-subtree-add-inherited-tags inherit-tags)
            (condition-case err
                (progn
                  (org-back-to-heading t)
                  ;; 使用基础的 org-archive-subtree 函数
                  (org-archive-subtree)
                  ;; 恢复默认值
                  (setq-default org-archive-subtree-add-inherited-tags nil)
                  
                  ;; 更新节点位置信息
                  (org-supertag-node-update)
                  (message "Successfully archived subtree"))
              (error
               ;; 确保恢复默认值
               (setq-default org-archive-subtree-add-inherited-tags nil)
               (message "Error archiving subtree: %S" err)
               (signal 'org-supertag-behavior-error
                       (list :archive-failed node-id err))))))))))

(defun org-supertag-behavior--archive-to-sibling (node-id params)
  "Archive to sibling for NODE-ID based on PARAMS.
PARAMS is a plist with keys:
- :sibling-heading : Custom archive sibling heading
- :add-tags : Additional tags to add
- :time-format : Format for archive time

Example:
  ;; Archive to default sibling
  (org-supertag-behavior--archive-to-sibling node-id nil)
  ;; Archive with custom heading
  (org-supertag-behavior--archive-to-sibling node-id 
    '(:sibling-heading \"Old Items\"
      :add-tags (\"archived\")))"
  (message "Debug archive-sibling - node=%s params=%S" node-id params)
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let* ((heading (plist-get params :sibling-heading))
               (tags (plist-get params :add-tags))
               (time-format (plist-get params :time-format))
               ;; 临时设置归档选项
               (org-archive-sibling-heading 
                (or heading org-archive-sibling-heading)))
          ;; 添加额外标签
          (when tags
            (dolist (tag tags)
              (org-toggle-tag tag 'on)))
          ;; 先清理标签
          (org-supertag-behavior--cleanup-tags node-id)
          ;; 执行归档
          (condition-case err
              (org-archive-to-archive-sibling)
            (error
             (message "Error archiving to sibling: %S" err))))))))

(defun org-supertag-behavior--toggle-archive-tag (node-id params)
  "Toggle archive tag for NODE-ID based on PARAMS.
PARAMS is a plist with keys:
- :check-children : Whether to check children before archiving
- :force : :on or :off to force tag state
- :recursive : Whether to apply recursively to subtree

Example:
  ;; Simple toggle
  (org-supertag-behavior--toggle-archive-tag node-id nil)
  ;; Check children and force on
  (org-supertag-behavior--toggle-archive-tag node-id 
    '(:check-children t :force :on))
  ;; Apply recursively
  (org-supertag-behavior--toggle-archive-tag node-id 
    '(:recursive t))"
  (message "\n=== Toggling archive tag for node %s ===" node-id)
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let* ((check-children (plist-get params :check-children))
               (force (plist-get params :force))
               (recursive (plist-get params :recursive))
               (current-tags (org-get-tags))
               (_ (message "Current tags: %S" current-tags)))
          
          ;; 1. 处理强制设置
          (when force
            (message "Force setting archive tag to: %s" force)
            (if (eq force :on)
                (unless (member org-archive-tag current-tags)
                  (org-toggle-tag org-archive-tag 'on))
              (when (member org-archive-tag current-tags)
                (org-toggle-tag org-archive-tag 'off))))
          
          ;; 2. 处理子节点检查
          (when check-children
            (message "Checking children before toggle")
            (save-excursion
              (org-back-to-heading t)
              (let* ((result (org-supertag-behavior--calculate-progress))
                     (total (nth 0 result))
                     (done (nth 1 result)))
                     (done-states (or org-done-keywords '("DONE"))))
                (message "Children status: %d/%d done" done total)
                (when (= done total)
                  (org-toggle-tag org-archive-tag 'on)))))
          
          ;; 3. 处理递归应用
          (when recursive
            (message "Applying archive tag recursively")
            (save-restriction
              (org-narrow-to-subtree)
              (let ((current-level (org-outline-level)))
                (save-excursion
                  (while (outline-next-heading)
                    (when (> (org-outline-level) current-level)
                      (message "Processing child at: %s" (org-get-heading t t t t))
                      (org-toggle-tag org-archive-tag
                                    (if (member org-archive-tag current-tags)
                                        'on 'off))))))))
          
          ;; 4. 默认切换行为
          (unless (or force check-children)
            (message "Performing default toggle")
            (org-toggle-tag org-archive-tag)
            (error
             (message "Error toggling archive tag: %S" err)))))))

(defun org-supertag-behavior--set-archive-location (node-id params)
  "Set archive location for NODE-ID based on PARAMS.
PARAMS is a plist with keys:
- :file : Archive file path
- :headline : Archive headline
- :scope : :buffer or :subtree for setting scope
- :inherit-tags : Whether to inherit tags

Example:
  ;; Set buffer archive location
  (org-supertag-behavior--set-archive-location node-id 
    '(:file \"archive.org\" 
      :headline \"* Archive\"
      :scope :buffer))
  ;; Set subtree archive location
  (org-supertag-behavior--set-archive-location node-id 
    '(:file \"~/org/archive.org\"
      :headline \"* Projects\"
      :scope :subtree
      :inherit-tags t))"
  (message "Debug set-archive-loc - node=%s params=%S" node-id params)
  (when-let* ((pos (org-supertag-db-get-pos node-id))
              (file (plist-get params :file))
              (headline (plist-get params :headline))
              (scope (plist-get params :scope))
              (inherit-tags (plist-get params :inherit-tags)))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let ((location (concat file "::" headline)))
          ;; 设置继承标签选项
          (setq-local org-archive-subtree-add-inherited-tags 
                      inherit-tags)
          ;; 设置归档位置
          (condition-case err
              (pcase scope
                (:buffer
                 (save-excursion
                   (goto-char (point-min))
                   (insert "#+ARCHIVE: " location "\n")))
                (:subtree
                 (org-entry-put nil "ARCHIVE" location))
                (_ (error "Invalid scope: %s" scope)))
            (error
             (message "Error setting archive location: %S" err)))))))
            

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

      ;; 保存当前位置
      (let ((start-pos (point)))
        ;; 移动到第一个子项
        (outline-next-heading)
        
        ;; 遍历所有子项
        (while (and (not (eobp))
                  (> (org-outline-level) current-level))
          (let ((todo-state (org-get-todo-state))
                (heading (org-get-heading t t t t)))
                (todo (org-get-todo-state)))
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
              0.0))))

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
  "Apply visual styles for NODE-ID based on its behaviors."
  (when-let* ((pos (org-supertag-db-get-pos node-id))
              (tags (org-supertag-node-get-tags node-id)))
    ;; 1. apply overlay style
    (dolist (tag-id tags)
      (when-let* ((behavior (org-supertag-behavior--get-behavior tag-id))
                  (style (plist-get behavior :style)))
        ;; apply overlay directly on the position
        (let ((ov (make-overlay pos (line-end-position))))
          (overlay-put ov 'org-supertag-face t)
          (overlay-put ov 'face (plist-get style :face)))))
    ;; 2. update prefix
    (org-supertag-behavior--update-prefix node-id)))

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

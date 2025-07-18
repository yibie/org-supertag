;;; org-supertag-behavior-library.el --- Library functions for org-supertag behaviors -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides various library functions to support org-supertag behavior system
;; Each library focuses on functionality in specific domains, supporting behavior implementation and composition
;;
;; Behavior Library Function Development Guide
;; =========================================
;;
;; 1. Function Naming and Documentation
;; ----------------------------------
;; - Use org-supertag-behavior-- prefix
;; - Function names should reflect specific actions
;; - Detailed docstrings including:
;;   * Functionality description
;;   * Parameter descriptions
;;   * Return value descriptions
;;   * Usage examples
;;
;; Example:
;; (defun org-supertag-behavior--set-todo (node-id params)
;;   "Set TODO state for NODE-ID based on PARAMS.
;; PARAMS is a plist with :state key.
;;
;; Example:
;;   (org-supertag-behavior--set-todo node-id '(:state \"DONE\"))")
;;
;; 2. Parameter Handling
;; -------------------
;; - Required parameters:
;;   * node-id: Node identifier
;;   * params: Parameter plist
;; - Use plist-get to extract parameters
;; - Use when-let* for parameter validation
;;
;; 3. Position Management
;; --------------------
;; - Get node position: (org-supertag-db-get-pos node-id)
;; - Protect current position: (save-excursion ...)
;; - Ensure correct position: (org-with-point-at pos ...)
;;
;; 4. Error Handling
;; ---------------
;; - Use when-let* to handle potential nil values
;; - Add debug info: (message "Debug ...")
;; - Use condition-case to catch errors when needed
;;
;; 5. Best Practices
;; ---------------
;; - Keep functions focused on single responsibility
;; - Prefer existing org-mode functions
;; - Implement complex functionality through composition
;; - Ensure position safety
;; - Add sufficient debug information
;;

;;; Code:

(require 'org)
(require 'org-supertag-db)
(require 'org-supertag-bridge) ;; Modern Bridge
(require 'org-supertag-api)   ;; Modern API


;;------------------------------------------------------------------------------
;; AI Interaction Library
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--call-ai (prompt &optional system-prompt)
  "Call the configured AI model with PROMPT and optional SYSTEM-PROMPT.
Uses the modern `org-supertag-api` via `org-supertag-bridge`.

PROMPT: The main user prompt string.
SYSTEM-PROMPT: An optional system prompt string to guide the AI's behavior.
This is currently handled by the Python LLMClient's configuration, but
the parameter is kept for potential future use.

Returns the AI's text response as a string on success.
Returns nil and logs an error on failure."
  (org-supertag-bridge--log "Calling AI with prompt (length %d)..." (length prompt))
  (condition-case err
      (let* ((full-prompt (if system-prompt (concat system-prompt "\n\n" prompt) prompt))
             (response (org-supertag-api-generate-text full-prompt)))

        (org-supertag-bridge--log "AI raw response: %S" response)

        ;; Check response status
        (if (and (eq (car response) 'success)
                 (plistp (cdr response))
                 (string= (plist-get (cdr response) :status) "success"))
            (let ((result (plist-get (cdr response) :result)))
              (org-supertag-bridge--log "AI call successful, response length: %d" (if result (length result) 0))
              result)
          ;; Handle AI-side error
          (let ((error-msg (or (plist-get (cdr response) :message) "Unknown AI error")))
            (org-supertag-bridge--log "AI call failed: %s" error-msg)
            (error "AI Error: %s" error-msg)
            nil)))
    ;; Handle Elisp-side errors (EPC connection, timeout, etc.)
    (error
     (org-supertag-bridge--log "Error calling AI: %s" (error-message-string err))
     (message "Error communicating with AI: %s" (error-message-string err))
     nil)))

;;------------------------------------------------------------------------------
;; Node Content Modification Library
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--insert-text (node-id params)
  "Insert text into the Org node specified by NODE-ID.
PARAMS is a plist containing:
- :text (required): The text string to insert.
- :location (required): Where to insert the text. Currently supported:
    - :body-end : Append text to the end of the node's body content.
    - '(:drawer \"DRAWER_NAME\" :append t) : Append text inside the drawer.
    - '(:drawer \"DRAWER_NAME\" :replace t) : Replace drawer content.

Logs an error and returns nil if insertion fails (e.g., invalid node-id,
unsupported location)."
  (let ((text (plist-get params :text))
        (location (plist-get params :location)))

    ;; Validate required parameters
    (unless text
      (message "Error in org-supertag-behavior--insert-text: :text parameter is required.")
      (error "Missing :text parameter"))
    (unless location
      (message "Error in org-supertag-behavior--insert-text: :location parameter is required.")
      (error "Missing :location parameter"))

    (message "Inserting text (length %d) into node %s at location %S"
             (length text) node-id location)

    (condition-case err
        (if-let ((pos (org-supertag-db-get-pos node-id)))
            (save-excursion
              (org-with-point-at pos
                (org-back-to-heading t) ;; Ensure we are at the heading start
                (pcase location
                  (:body-end
                   (let ((element (org-element-at-point)))
                     (if-let ((contents-end (org-element-property :contents-end element)))
                         (progn
                           (goto-char contents-end)
                           ;; Ensure a newline before inserting if needed
                           (unless (looking-back "\n" (max (point-min) (- (point) 1)))
                             (insert "\n"))
                           (insert text))
                       ;; Node might have no body, insert after heading
                       (end-of-line)
                       (insert "\n")
                       (insert text)))
                   t) ; Return t on success

                  (`(:drawer ,name :append ,_)
                   (let ((drawer-name (format ":%s:" name)))
                     (if (re-search-forward (concat "^[ \t]*" (regexp-quote drawer-name)) nil t)
                         (progn
                           (goto-char (match-end 0))
                           (forward-line 1)
                           (search-backward ":END:")
                           (goto-char (match-beginning 0))
                           (insert text "\n")
                           t)
                       ;; Drawer not found, create and insert
                       (progn
                         (org-supertag-behavior--insert-drawer node-id `(:name ,name :content ,text))
                         t))))

                  (`(:drawer ,name :replace ,_)
                    (let ((drawer-name (format ":%s:" name)))
                     (if (re-search-forward (concat "^[ \t]*" (regexp-quote drawer-name) "[ \t]*$") nil t)
                         (let ((start (match-end 0)))
                             (forward-line 1)
                             (if (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                                 (let ((end (match-beginning 0)))
                                   (delete-region start end)
                                   (insert "\n" text)
                                   t) ; Return t on success
                               (progn (message "Error: Could not find :END: for drawer %s" name) nil))) ; Error finding :END:
                       ;; Drawer not found, create and insert
                       (progn
                         (org-supertag-behavior--insert-drawer node-id `(:name ,name :content ,text))
                         t)))) ; Return t after creating

                  (_
                   (message "Error: Unsupported location specifier: %S" location)
                   nil)))) ; Return nil for unsupported location
          ;; Handle case where node-id is invalid
          (progn
            (message "Error: Could not find node with ID: %s" node-id)
            nil)) ; Return nil if node not found
      ;; Handle other errors during insertion
      (error
       (message "Error inserting text into node %s: %s" node-id (error-message-string err))
       nil)))) ; Return nil on error

;;------------------------------------------------------------------------------
;; Higher-Level Behavior Actions (Composing Basic Functions)
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--generate-topics-action (node-id params)
  "Action function for the @GenerateTopics behavior.
Calls AI to generate topic suggestions based on a predefined prompt
and inserts the result into the NODE-ID's body.
PARAMS are currently ignored."
  (message "Executing @GenerateTopics for node %s with params %S" node-id params)
  (let ((prompt my/ai-generate-topics-prompt)
        ;; 从 params 获取 location，如果未提供则默认为 :body-end
        (insert-location (plist-get params :location :body-end)))
    (message "正在调用 AI 生成选题 (将插入到 %S)..." insert-location)
    (when-let* ((ai-response (org-supertag-behavior--call-ai prompt)))
      (message "AI 已响应，正在插入内容...")
      (unless (org-supertag-behavior--insert-text node-id `(:text ,(concat "\n;; AI Generated Topics:\n" ai-response)
                                                          :location ,insert-location))
         (message "Error: Failed to insert AI response into node %s at %S" node-id insert-location))
      (message "选题已成功插入。")
    (unless ai-response
       (message "错误：未能从 AI 获取响应。请检查日志。")
       (message "Error: Failed to get response from AI for node %s" node-id)))))


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
        ;; record state change
        (when note
          (org-add-log-note))
        ;; trigger org-trigger-hook
        (run-hook-with-args 'org-trigger-hook
                           (list :type 'state-change
                                 :position pos
                                 :from from
                                 :to to))))))

(defun org-supertag-behavior--on-property-change (node-id params)
  "Record property value changes for NODE-ID.
PARAMS is a property list containing:
:name - Property name
:from - Original value
:to - New value
:note - Optional change note

Example:
  (org-supertag-behavior--on-property-change node-id 
    '(:name \"PRIORITY\" :from \"B\" :to \"A\" :note \"Increased priority\"))"
  (when-let* ((name (plist-get params :name))
              (from (plist-get params :from))
              (to (plist-get params :to))
              (note (plist-get params :note))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        ;; Record change
        (org-entry-put nil name to)
        ;; Add change log
        (when note
          (org-add-log-setup 'property name from to note))))))

(defun org-supertag-behavior--update-statistics (node-id _params)
  "Update TODO statistics cookies for NODE-ID.
This function doesn't require any parameters but follows the behavior function
signature for consistency.

Example:
  (org-supertag-behavior--update-statistics node-id nil)"
  (message "Debug update-stats - node=%s" node-id)
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (org-update-statistics-cookies nil)
        (run-hooks 'org-after-todo-statistics-hook)))))

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
  "Propagate state to direct children of NODE-ID.
PARAMS is a property list with :state parameter indicating the state to set.

Example:
  (org-supertag-behavior--propagate-state node-id 
    '(:state \"DONE\"))"
  (when-let* ((state (plist-get params :state))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (let ((level (org-outline-level)))
          (save-restriction
            (org-narrow-to-subtree)
            (while (outline-next-heading)
              (when (= (org-outline-level) (1+ level))
                (org-todo state)))))))))

;;------------------------------------------------------------------------------
;; Node Status
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--set-todo (node-id params)
  "Set TODO state for NODE-ID based on PARAMS.
PARAMS is a plist with :state key.
The state must be one of the valid states defined in `org-todo-keywords'."
  (message "Debug set-todo - node=%s params=%S" node-id params)
  (when-let* ((state (plist-get params :state))
              (pos (org-supertag-db-get-pos node-id)))
    ;; Get all valid todo states from org-todo-keywords
    (let* ((all-keywords (mapcar 
                         (lambda (seq)
                           (mapcar 
                            (lambda (kw)
                              (if (string-match "\\`\\([^(]*\\)\\(?:(\\w*)\\)?\\'" kw)
                                  (match-string 1 kw)
                                kw))
                            (remove "|" (cdr seq))))
                         org-todo-keywords))
           (valid-states (apply #'append all-keywords)))
      ;; Validate the state
      (if (member state valid-states)
          (progn
            (message "Debug set-todo - Setting state to: %s" state)
            (save-excursion
              (org-with-point-at pos
                (org-todo state))))
        (user-error "Invalid TODO state: %s. Valid states are: %s" 
                   state 
                   (mapconcat #'identity valid-states ", "))))))


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
PARAMS is a plist with :name key and optional :inherit flag."
  (message "Debug get-property - node=%s params=%S" node-id params)
  (when-let* ((name (plist-get params :name))
              (inherit (plist-get params :inherit))
              (pos (org-supertag-db-get-pos node-id)))
    (message "Debug get-property - Getting %s (inherit=%s)" name inherit)
    (save-excursion
      (org-with-point-at posp
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
        (when-let* ((value (org-entry-get nil name)))
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
        ;; Insert drawer
        (condition-case err
            (progn
              (if region
                  (org-insert-drawer)
                (insert ":" name ":\n:END:\n")
                (forward-line -2))
              ;; If there is content, insert it
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
        (let ((org-log-into-drawer (if enabled
                                      (or name t)
                                    nil)))
          (when note
            (org-add-note note)))))))

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
          (add-hook 'org-todo-state-change-hook
                   (lambda ()
                     (apply hook-fn hook-args)))
          (org-todo state)
          (remove-hook 'org-todo-state-change-hook
                      (lambda ()
                        (apply hook-fn hook-args))))))))   

;;------------------------------------------------------------------------------
;; Priority Management
;;------------------------------------------------------------------------------

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
        ;; temporarily set priority range
        (let ((org-highest-priority (string-to-char highest))
              (org-lowest-priority (string-to-char lowest))
              (org-default-priority (string-to-char default)))
          ;; set current priority to default
          (org-priority (string-to-char default)))))))

;;------------------------------------------------------------------------------
;; Clock Management
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--clock-in (node-id _params)
  "Start clock on node with NODE-ID."
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (condition-case err
            (org-clock-in)
          (error
           (message "Error starting clock: %S" err)))))))

(defun org-supertag-behavior--clock-out (node-id _params)
  "Stop clock on node with NODE-ID."
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (condition-case err
            (org-clock-out)
          (error
           (message "Error stopping clock: %S" err)))))))

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
          ;; set temporary format
          (setq-local org-timer-format format)
          ;; start timer
          (condition-case err
              (if offset
                  (org-timer-start offset)
                (org-timer-start))
            (error
             (message "Error starting timer: %S" err))))))))

;;------------------------------------------------------------------------------
;; Deadline Management Library
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--check-deadline (node-id params)
  "Check deadline status for NODE-ID based on PARAMS.
PARAMS is a plist with:
scope   - Where to check (subtree, file, or agenda)
days    - Days to look ahead (default 0 for only overdue)
action  - Function to call for each matching task
         Function receives (heading deadline) as arguments

Example:
  (org-supertag-behavior--check-deadline node-id 
    '(scope agenda 
      days 0
      action my-handler-fn))"
  (message "Debug check-deadline - node=%s params=%S" node-id params)
  (when-let* ((pos (org-supertag-db-get-pos node-id))
              (action-fn (plist-get params 'action))
              (scope (or (plist-get params 'scope) 'subtree))
              (days (or (plist-get params 'days) 0))
              (deadline-check
               (format "DEADLINE<=\"<%+%dd>\"" days)))
    (save-excursion
      (org-with-point-at pos
        (cl-case scope
          (subtree
           (org-map-entries
            (lambda ()
              (when-let* ((deadline (org-get-deadline-time nil)))
                (funcall action-fn 
                        (org-get-heading t t t t)
                        deadline)))
            deadline-check
            'tree))
          (file
           (org-map-entries
            (lambda ()
              (when-let* ((deadline (org-get-deadline-time nil)))
                (funcall action-fn 
                        (org-get-heading t t t t)
                        deadline)))
            deadline-check))
          (agenda
           (org-map-entries
            (lambda ()
              (when-let* ((deadline (org-get-deadline-time nil)))
                (funcall action-fn 
                        (org-get-heading t t t t)
                        deadline)))
            deadline-check
            'agenda)))))))

;;------------------------------------------------------------------------------
;; Node Operations Library
;;------------------------------------------------------------------------------
;;; Node Tree Navigation Library
;;; This library provides functions for navigating and manipulating the node tree
;;; structure in org-mode documents. It includes operations for:
;;; - Finding parent/child relationships
;;; - Traversing node hierarchies
;;; - Collecting node information at different tree levels

(defun org-supertag-behavior--get-children (node-id)
  "Get direct children of node with NODE-ID.
This function:
1. Finds the node position using NODE-ID
2. Gets all direct child nodes (one level below)
3. For each child, collects its heading and todo state
4. Returns list of (heading todo-state) pairs

Example:
  (org-supertag-behavior--get-children \"20240101T123456\")
  ;; => ((\"Task 1\" \"TODO\") (\"Task 2\" \"DONE\"))"
  (message "\n=== Getting Children for Node %s ===" node-id)
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
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
        ;; collect direct children using org-map-entries
        (save-restriction
          (org-narrow-to-subtree)
          (let ((parent-pos (point)))  ;; remember parent node position
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (let* ((current-level (org-outline-level))
                     (heading (org-get-heading t t t t))
                     (todo (org-get-todo-state)))
                (message "Found entry - Level: %d, Heading: %s, TODO: %s" 
                        current-level heading todo)
                ;; collect direct children, no need for ID
                (when (= current-level (1+ parent-level))
                  (message "Adding child: %s" heading)
                  (push (list heading todo) children))))))
        
        (message "Found children: %S" children)
        (nreverse children)))))

(defun org-supertag-behavior--find-parent-with-tag (tag-id &optional action-fn)
  "Find nearest parent node with TAG-ID and optionally apply ACTION-FN.
This function:
1. Traverses up the org tree from current position
2. Looks for a parent node that has the specified TAG-ID
3. If found, gets its node-id and optionally calls ACTION-FN
4. Returns the parent node-id if found

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
               (todo (org-get-todo-state)))
          (when (member (concat "#" tag-id) tags)
            (when-let* ((parent-id (org-id-get)))
              (when action-fn
                (funcall action-fn parent-id))
              parent-id))))))

;;; Node Operation - Move Node 
(defun org-supertag-behavior--move-node (node-id params)
  "Move node with NODE-ID based on PARAMS.
PARAMS is a plist with:
- target-file : Target file path"
  (when-let* ((target-file (plist-get params :target-file))
              (pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (condition-case err
            (org-supertag-node-move node-id target-file nil)
          (error
           (message "Error moving node: %S" err)))))))

;;------------------------------------------------------------------------------
;; Archive Management
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--archive-subtree (node-id _params)
  "Archive subtree at NODE-ID using org-archive-subtree.
The archive location is determined by `org-archive-location'."
  (when-let* ((pos (org-supertag-db-get-pos node-id)))
    (save-excursion
      (org-with-point-at pos
        (condition-case err
            (progn
              (org-archive-subtree)
              (org-supertag-db-update-buffer)
              (when-let* ((location-parts (split-string org-archive-location "::"))
                         (target-file (expand-file-name (car location-parts))))
                (with-current-buffer (find-file-noselect target-file)
                  (org-supertag-db-update-buffer))))
          (error
           (message "Error archiving subtree: %S" err)))))))


;;------------------------------------------------------------------------------
;; Script Execution Library
;;------------------------------------------------------------------------------

(defcustom org-supertag-script-executors
  '(("py" . (;; Python scripts - prefer venv if available
             :command "python3"
             :args nil
             :env (("PYTHONPATH" . "${script_dir}")
                   ("PYTHONIOENCODING" . "utf-8"))))
    ("rb" . (;; Ruby scripts
             :command "ruby"
             :args nil
             :env (("RUBYLIB" . "${script_dir}"))))
    ("js" . (;; Node.js scripts
             :command "node"
             :args nil
             :env (("NODE_PATH" . "${script_dir}"))))
    ("sh" . (;; Shell scripts
             :command "bash"
             :args nil
             :env (("SCRIPT_DIR" . "${script_dir}"))))
    ("pl" . (;; Perl scripts
             :command "perl"
             :args nil
             :env (("PERL5LIB" . "${script_dir}"))))
    ("R" . (;; R scripts
            :command "Rscript"
            :args ("--vanilla")
            :env (("R_LIBS_USER" . "${script_dir}/R/library"))))
    ("jl" . (;; Julia scripts
             :command "julia"
             :args nil
             :env (("JULIA_LOAD_PATH" . "${script_dir}"))))
    ("php" . (;; PHP scripts
              :command "php"
              :args nil
              :env (("PHP_INI_SCAN_DIR" . "${script_dir}")))))
  "Mapping of file extensions to script execution configurations.
Each configuration is a plist with:
:command - The command to execute
:args    - Optional list of command line arguments
:env     - Environment variables, can use ${script_dir} placeholder"
  :type '(alist :key-type string
                :value-type (plist :key-type symbol :value-type sexp))
  :group 'org-supertag-script)

(defcustom org-supertag-python-executable nil
  "Preferred Python executable for script execution.
If nil, will auto-detect the best available Python interpreter.
Can be set to:
- A specific path: \"/usr/local/bin/python3.11\"
- A command name: \"python3\"
- nil: Auto-detect (recommended)"
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Custom path/command"))
  :group 'org-supertag-script)

(defcustom org-supertag-python-venv-paths 
  '(".venv/bin/python"
    "venv/bin/python" 
    ".virtualenv/bin/python"
    "env/bin/python")
  "List of relative paths to search for virtual environment Python.
Paths are relative to script directory or project root."
  :type '(repeat string)
  :group 'org-supertag-script)

(defun org-supertag-behavior--find-python-executable (script-path)
  "Find the best Python executable for SCRIPT-PATH.
Search order:
1. Custom setting from `org-supertag-python-executable`
2. VIRTUAL_ENV environment variable
3. Virtual environment in script directory or parents
4. System python3"
  ;; 1. Use custom setting if specified
  (if org-supertag-python-executable
      org-supertag-python-executable
    ;; 2. Check VIRTUAL_ENV environment variable
    (if-let ((venv-path (getenv "VIRTUAL_ENV")))
        (let ((venv-python (expand-file-name "bin/python" venv-path)))
          (if (file-exists-p venv-python)
              venv-python
            "python3"))
      ;; 3. Search for virtual environment near script
      (let ((script-dir (file-name-directory (expand-file-name script-path)))
            (found-python nil))
        (catch 'found
          ;; Search in script directory and parent directories
          (let ((search-dir script-dir))
            (while (and search-dir 
                       (not (string= search-dir "/"))
                       (not (string= search-dir (expand-file-name "~"))))
              ;; Check each possible venv path
              (dolist (venv-path org-supertag-python-venv-paths)
                (let ((python-path (expand-file-name venv-path search-dir)))
                  (when (file-exists-p python-path)
                    (throw 'found python-path))))
              ;; Move to parent directory
              (setq search-dir (file-name-directory (directory-file-name search-dir))))))
        ;; 4. Fallback to system python3
        (or found-python "python3")))))

(defun org-supertag-behavior--get-script-executor (script-path)
  "Get executor configuration for script at SCRIPT-PATH.
Returns a plist containing :command, :args, and :env settings.
Raises error if no executor is configured for the script extension.

Example:
  (org-supertag-behavior--get-script-executor \"test.py\")
  ;; => (:command \"python3\" :args nil :env ((\"PYTHONPATH\" . \"...\")))"
  (let ((ext (file-name-extension script-path)))
    (unless ext
      (error "Script file has no extension: %s" script-path))
    (let ((executor (or (assoc-default ext org-supertag-script-executors)
                       (error "No executor configured for extension: %s" ext))))
      ;; For Python scripts, find the best Python executable
      (when (string= ext "py")
        (let ((python-cmd (org-supertag-behavior--find-python-executable script-path)))
          (setq executor (plist-put executor :command python-cmd))))
      executor)))

(defun org-supertag-behavior--setup-script-env (env script-dir)
  "Setup environment variables from ENV using SCRIPT-DIR.
Expands ${script_dir} placeholder in environment variable values.
Returns alist of (var-name . expanded-value) pairs.

Example:
  (org-supertag-behavior--setup-script-env
    '((\"PYTHONPATH\" . \"${script_dir}\"))
    \"/path/to/scripts\")"
  (mapcar (lambda (var)
            (let* ((name (car var))
                   (value (cdr var))
                   (expanded (replace-regexp-in-string
                            "${script_dir}"
                            script-dir
                            value)))
              (cons name expanded)))
          env))

(defun org-supertag-behavior--execute-script (node-id params)
  "Execute a script asynchronously and display its output.
Executes script specified in PARAMS and displays output in message buffer.

Required PARAMS:
- :script-path  : Path to script file

Optional PARAMS:
- :args         : List of additional command line arguments
- :async        : If t, run asynchronously (default: t)
- :callback     : Function to call when script completes (receives output string)

The function will:
1. Determine appropriate executor based on script extension
2. Setup execution environment
3. Run script asynchronously without blocking Emacs
4. Display results in message buffer when complete

Example:
  (org-supertag-behavior--execute-script node-id
    '(:script-path \"~/scripts/process.py\"
      :args (\"-v\" \"--mode=batch\")
      :async t
      :callback (lambda (output) (message \"Done: %s\" output))))"
  (let* ((script-path (expand-file-name (plist-get params :script-path)))
         (extra-args (plist-get params :args))
         (async (if (plist-member params :async) 
                   (plist-get params :async) 
                 t))  ; Default to async only if :async not specified
         (callback (plist-get params :callback))
         (script-dir (file-name-directory script-path)))
    
    (unless (file-exists-p script-path)
      (error "Script file not found: %s" script-path))
    
    (let* ((executor (org-supertag-behavior--get-script-executor script-path))
           (command (plist-get executor :command))
           (base-args (plist-get executor :args))
           (all-args (append (or base-args '())
                            (list script-path)
                            (or extra-args '())))
           (env (org-supertag-behavior--setup-script-env
                 (plist-get executor :env)
                 script-dir)))
      
      (if async
          ;; Async execution
          (progn
            (message "Starting script %s (async)..." (file-name-nondirectory script-path))
            (let* ((process-name (format "*script-%s*" (file-name-base script-path)))
                   (output-buffer (generate-new-buffer process-name))
                   (process-environment
                    (append
                     (mapcar (lambda (var)
                               (format "%s=%s" (car var) (cdr var)))
                             env)
                     process-environment))
                   (process (apply #'start-process
                                  process-name
                                  output-buffer
                                  command
                                  all-args)))
              
              ;; Set process completion handler
              (set-process-sentinel 
               process
               (lambda (proc event)
                 (when (string-match "finished\\|exited" event)
                   (let ((output (with-current-buffer (process-buffer proc)
                                   (buffer-string))))
                     (message "Script %s completed: %s" 
                             (file-name-nondirectory script-path)
                             (string-trim (substring event 0 -1)))
                     (message "Script output: %s" (string-trim output))
                     
                     ;; Call callback if provided
                     (when callback
                       (funcall callback output))
                     
                     ;; Cleanup
                     (kill-buffer (process-buffer proc))))))
              
              ;; Return process object
              process))
        
        ;; Sync execution (original behavior)
        (let ((output-buffer (generate-new-buffer "*script-output*")))
          (message "Executing script %s (sync)..." script-path)
          (let ((process-environment
                 (append
                  (mapcar (lambda (var)
                            (format "%s=%s" (car var) (cdr var)))
                          env)
                  process-environment)))
            (apply #'call-process
                   command    ; Program
                   nil       ; Input
                   output-buffer ; Output
                   nil       ; Display
                   all-args))
          
          ;; Get and display output
          (with-current-buffer output-buffer
            (let ((output (buffer-string)))
              (message "Script output: %s" (string-trim output))
              (when callback
                (funcall callback output))))
          
          ;; Cleanup
          (kill-buffer output-buffer))))))

;;------------------------------------------------------------------------------
;; Script Execution Callback Library
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--script-completion-callback (output)
  "General callback for script completion.
Shows a notification with script results."
  (let ((lines (split-string output "\n" t)))
    (message "🐍 Script completed! Last line: %s" 
             (or (car (last lines)) "No output"))))

(defun org-supertag-behavior--scheduled-script-callback (output)
  "Callback for scheduled script execution.
Logs completion time and basic stats."
  (let* ((lines (split-string output "\n" t))
         (line-count (length lines))
         (char-count (length output)))
    (message "⏰ Scheduled script completed at %s: %d lines, %d chars"
             (format-time-string "%H:%M")
             line-count char-count)))

(defun org-supertag-behavior--lex-scraper-callback (output)
  "Callback specifically for lex-scraper results.
Extracts video count and saved file info."
  (let ((video-count 0)
        (saved-file nil))
    ;; Extract video count
    (when (string-match "Successfully scraped \\([0-9]+\\) videos" output)
      (setq video-count (string-to-number (match-string 1 output))))
    ;; Extract saved file
    (when (string-match "Results saved to \\([^\\s]+\\.json\\)" output)
      (setq saved-file (match-string 1 output)))
    
    (message "📺 Lex-scraper completed! Videos: %d, Saved: %s" 
             video-count (or saved-file "unknown"))))

(defun org-supertag-behavior--dev-script-callback (output)
  "Callback for development scripts.
Shows detailed output for debugging."
  (let* ((lines (split-string output "\n" t))
         (error-lines (seq-filter (lambda (line) 
                                   (string-match-p "\\(error\\|Error\\|ERROR\\)" line)) 
                                 lines))
         (success (= 0 (length error-lines))))
    
    (if success
        (message "✅ Dev script completed successfully! Output: %d lines" (length lines))
      (message "❌ Dev script completed with %d errors. Check output." (length error-lines)))))

(defun org-supertag-behavior--data-processing-callback (output)
  "Callback for data processing scripts.
Extracts common data processing statistics."
  (let ((processed-count 0)
        (errors-count 0)
        (output-file nil))
    
    ;; Extract processed items count
    (when (string-match "processed \\([0-9]+\\)" output)
      (setq processed-count (string-to-number (match-string 1 output))))
    
    ;; Extract error count
    (when (string-match "\\([0-9]+\\) errors?" output)
      (setq errors-count (string-to-number (match-string 1 output))))
    
    ;; Extract output file
    (when (string-match "\\(?:saved to\\|output:\\|written to\\) \\([^\\s]+\\)" output)
      (setq output-file (match-string 1 output)))
    
    (message "📊 Data processing completed! Processed: %d, Errors: %d, Output: %s"
             processed-count errors-count (or output-file "console"))))

(defun org-supertag-behavior--backup-script-callback (output)
  "Callback for backup scripts.
Shows backup status and file counts."
  (let ((files-backed-up 0)
        (backup-size nil))
    
    ;; Extract file count
    (when (string-match "\\([0-9]+\\) files? backed up" output)
      (setq files-backed-up (string-to-number (match-string 1 output))))
    
    ;; Extract backup size
    (when (string-match "\\([0-9.]+\\s*[KMGT]B\\)" output)
      (setq backup-size (match-string 1 output)))
    
    (message "💾 Backup completed! Files: %d, Size: %s"
             files-backed-up (or backup-size "unknown"))))

;;------------------------------------------------------------------------------
;; Command Execution Library
;;------------------------------------------------------------------------------

(defun org-supertag-behavior--execute-command (node-id params)
  "Execute an Emacs command for NODE-ID based on PARAMS.
PARAMS is a plist with:
- :command : The command to execute (symbol or function)
- :args    : Optional list of arguments for the command

Example:
  (org-supertag-behavior--execute-command node-id
    '(:command org-agenda
      :args (\"a\")))"
  (let* ((command (plist-get params :command))
         (args (plist-get params :args)))
    
    (unless command
      (error "Missing required parameter: :command"))

    (condition-case err
        (if args
            (apply command args)
          (funcall command))
      (error
       (message "Error executing command: %S" err)))))

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
  (org-with-wide-buffer
   (when-let* ((pos (org-supertag-db-get-pos node-id)))
     (save-excursion
       (goto-char pos)
       (org-back-to-heading t)
       (let* ((beg (line-beginning-position))
              (end (line-end-position))
              (tags (org-get-tags nil t)))
         ;; 1. Clear existing styles
         (remove-overlays beg end 'org-supertag-face t)
         
         ;; 2. Apply styles for each tag
         (dolist (tag tags)
           (when (string-prefix-p "#" tag)
             (let* ((tag-id (substring tag 1))
                    (behavior (org-supertag-behavior--get-behavior tag-id))
                    (style (plist-get behavior :style)))
               ;; Apply face/color
               (when-let* ((face (plist-get style :face)))
                 (let* ((face-attrs (if (facep face)
                                      (face-all-attributes face nil)
                                    face))
                        (bg (plist-get face-attrs :background))
                        (fg (plist-get face-attrs :foreground))
                        (valid-attrs
                         (append
                          (when (and bg (color-defined-p bg))
                            (list :background bg))
                          (when (and fg (color-defined-p fg))
                            (list :foreground fg)))))
                   (when valid-attrs
                     (let ((ov (make-overlay beg end)))
                       (overlay-put ov 'face valid-attrs)
                       (overlay-put ov 'org-supertag-face t)
                       (overlay-put ov 'node-id node-id)))))
               
               ;; Apply prefix
               (when-let* ((prefix (plist-get style :prefix)))
                 (when (looking-at org-complex-heading-regexp)
                   (let* ((current-title (match-string 4))
                          (new-title (if (string-prefix-p prefix current-title)
                                       current-title
                                     (concat prefix " " current-title))))
                     (replace-match new-title t t nil 4))))))))))))

(defun org-supertag-behavior--update-prefix (node-id)
  "Update prefix for NODE-ID based on its tags."
  (org-with-wide-buffer
   (when-let* ((pos (org-supertag-db-get-pos node-id)))
     (save-excursion
       (goto-char pos)
       (org-back-to-heading t)
       (let ((tags (org-get-tags nil t)))
         (dolist (tag tags)
           (when (string-prefix-p "#" tag)
             (let* ((tag-id (substring tag 1))
                    (behavior (org-supertag-behavior--get-behavior tag-id))
                    (style (plist-get behavior :style))
                    (prefix (plist-get style :prefix)))
                     (when prefix
                       (when (looking-at org-complex-heading-regexp)
                         (let* ((current-title (match-string 4))
                                (new-title (if (string-prefix-p prefix current-title)
                                             current-title
                                           (concat prefix " " current-title))))
                         (replace-match new-title t t nil 4))))))))))))



(provide 'org-supertag-behavior-library) 


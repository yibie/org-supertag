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
:schedule - Cron format string (\"minute hour day month weekday\") for :schedule trigger
:action   - Function to execute or list of behavior names
:style    - Visual properties (optional)
:hooks    - List of (hook-name . hook-function) pairs (optional)
:params   - List of parameter definitions (optional)"
  :type 'hash-table
  :group 'org-supertag-behavior)

(defvar org-supertag-behavior--initialized nil
  "Flag to track if behavior system has been initialized.")

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
                        :schedule (plist-get props :schedule)
                        :action (plist-get props :action)
                        :style (plist-get props :style)
                        :hooks (plist-get props :hooks)
                        :params (plist-get props :params)
                        :list (plist-get props :list))))
    
    ;; Register to behavior registry (memory cache)
    (puthash behavior-name behavior org-supertag-behavior-registry)
    
    ;; Process hooks
    (when-let ((hooks (plist-get props :hooks)))
      (dolist (hook-spec hooks)
        (add-hook (car hook-spec) (cdr hook-spec))))
    
    behavior))


(defun org-supertag-behavior--get-behavior (tag-name)
  "Get behavior definition for tag with TAG-NAME.
First try to get behavior directly from registry.
If not found, check if tag has associated behaviors."
  (or
   ;; Look up directly from registry
   (gethash tag-name org-supertag-behavior-registry)
   ;; Look up from tag's associated behaviors
   (when-let* ((tag (org-supertag-tag-get tag-name))
               (behaviors (plist-get tag :behaviors)))
     ;; If multiple behaviors exist, return the first one
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
;; Secheduler System
;;------------------------------------------------------------------------------

(defvar org-supertag-scheduled-tasks (make-hash-table :test 'equal)
  "Store scheduled tasks. Key is task ID, value is task property list.")

(defvar org-supertag-scheduler-timer nil
  "Main timer for the scheduler.")

;; Cron format parsing and validation
(defun org-supertag-parse-cron (cron-string)
  "Parse cron expression.
CRON-STRING format: \"minute hour day month weekday\""
  (let ((fields (split-string cron-string " ")))
    (when (= (length fields) 5)
      (cl-destructuring-bind (minute hour day month weekday) fields
        (list :minute minute
              :hour hour
              :day day
              :month month
              :weekday weekday)))))

(defun org-supertag-validate-cron-field (value pattern)
  "Validate cron field.
VALUE is the field value
PATTERN is the validation pattern"
  (string-match-p pattern value))

(defun org-supertag-cron-valid-p (cron-string)
  "Check if cron expression is valid.
CRON-STRING is the complete cron expression"
  (when-let ((fields (org-supertag-parse-cron cron-string)))
    (and
     ;; Minutes (0-59)
     (org-supertag-validate-cron-field 
      (plist-get fields :minute)
      "^\\(?:\\*\\|[0-5]?[0-9]\\)$")
     ;; Hours (0-23)
     (org-supertag-validate-cron-field 
      (plist-get fields :hour)
      "^\\(?:\\*\\|\\(?:[0-1]?[0-9]\\|2[0-3]\\)\\)$")
     ;; Days (1-31)
     (org-supertag-validate-cron-field 
      (plist-get fields :day)
      "^\\(?:\\*\\|\\(?:[1-2]?[0-9]\\|3[0-1]\\)\\)$")
     ;; Months (1-12)
     (org-supertag-validate-cron-field 
      (plist-get fields :month)
      "^\\(?:\\*\\|\\(?:[1-9]\\|1[0-2]\\)\\)$")
     ;; Weekdays (0-6)
     (org-supertag-validate-cron-field 
      (plist-get fields :weekday)
      "^\\(?:\\*\\|[0-6]\\)$"))))

(defun org-supertag-cron-match-field (field-value current-value)
  "Check if cron field matches.
FIELD-VALUE is the cron field value
CURRENT-VALUE is the current time value"
  (or (string= field-value "*")
      (string= field-value (number-to-string current-value))))

(defun org-supertag-cron-matches-p (cron-expr time)
  "Check if given time matches cron expression.
CRON-EXPR is the cron expression
TIME is the time to check"
  (let* ((fields (org-supertag-parse-cron cron-expr))
         (time-fields (decode-time time))
         ;; Extract time fields
         (current-minute (nth 1 time-fields))
         (current-hour (nth 2 time-fields))
         (current-day (nth 3 time-fields))
         (current-month (nth 4 time-fields))
         (current-weekday (nth 6 time-fields)))
    ;; All fields must match
    (and (org-supertag-cron-match-field (plist-get fields :minute) current-minute)
         (org-supertag-cron-match-field (plist-get fields :hour) current-hour)
         (org-supertag-cron-match-field (plist-get fields :day) current-day)
         (org-supertag-cron-match-field (plist-get fields :month) current-month)
         (org-supertag-cron-match-field (plist-get fields :weekday) current-weekday))))

;; Task management
(defun org-supertag-schedule-add-task (behavior)
  "Add scheduled task.
BEHAVIOR is the behavior definition"
  (let* ((id (plist-get behavior :id))
         (schedule (plist-get behavior :schedule))
         (action (plist-get behavior :action)))
    
    ;; Validate cron format
    (unless (org-supertag-cron-valid-p schedule)
      (error "Invalid cron format: %s" schedule))
    
    ;; Add task
    (puthash id
             (list :schedule schedule
                   :action action
                   :last-run nil)
             org-supertag-scheduled-tasks)))

(defun org-supertag-scheduler-check-tasks ()
  "Check and execute tasks matching current time."
  (let ((now (current-time)))
    (maphash
     (lambda (id task)
       (when (org-supertag-cron-matches-p 
              (plist-get task :schedule) 
              now)
         ;; Avoid repeated execution in same minute
         (unless (equal (plist-get task :last-run)
                       (format-time-string "%Y-%m-%d %H:%M" now))
           ;; Execute task
           (condition-case err
               (progn
                 (message "Executing scheduled task %s" id)
                 (funcall (plist-get task :action))
                 ;; Update last execution time
                 (puthash id
                          (list :schedule (plist-get task :schedule)
                                :action (plist-get task :action)
                                :last-run (format-time-string 
                                         "%Y-%m-%d %H:%M" 
                                         now))
                          org-supertag-scheduled-tasks))
             (error
              (message "Error in scheduled task %s: %s" 
                       id (error-message-string err)))))))
     org-supertag-scheduled-tasks)))

;; Scheduler control functions remain unchanged
(defun org-supertag-scheduler-start ()
  "Start the scheduler."
  (unless org-supertag-scheduler-timer
    (setq org-supertag-scheduler-timer
          (run-at-time t 60 #'org-supertag-scheduler-check-tasks))
    (message "Scheduler started")))

(defun org-supertag-scheduler-stop ()
  "Stop the scheduler."
  (when org-supertag-scheduler-timer
    (cancel-timer org-supertag-scheduler-timer)
    (setq org-supertag-scheduler-timer nil)
    (message "Scheduler stopped")))

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
    
    ;; Update tag's behaviors property
    (let* ((behaviors (or (plist-get tag :behaviors) '())))
      (org-supertag-tag-create 
       tag-name 
       :type :tag
       :behaviors (cons behavior-name behaviors)))
    
    ;; Ensure we're at a valid org heading and execute behavior
    (when (org-at-heading-p)
      (let ((node-id (org-id-get-create)))
        (message "Debug - Before execute behavior: node=%s tag=%s" 
                 node-id tag-name)
        ;; Execute behavior for all relevant tags on current node
        (dolist (tag-id (org-supertag-node-get-tags node-id))
          (when (equal tag-id tag-name)
            (org-supertag-behavior--on-tag-change node-id tag-id :add)))
        
        ;; Apply styles directly to current node
        (save-excursion
          ;; Clear old overlays from current line
          (remove-overlays (line-beginning-position) 
                          (line-end-position) 
                          'org-supertag-face t)
          ;; Apply new styles
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
  ;; 1. First try to expand visibility
  (org-fold-show-all '(headings))
  
  ;; 2. Ensure file has headlines
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward org-heading-regexp nil t)
      (user-error "No headlines found in buffer")))
  
  ;; 3. If not at heading, safely move to one
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
  
  ;; 4. Final position validation
  (unless (org-at-heading-p)
    (user-error "Please move cursor to a headline")))

(defun org-supertag-behavior-execute-at-point ()
  "Execute a behavior on the current node."
  (interactive)
  ;; Add position validation
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
    ;; Ensure org-id system is properly initialized
    (require 'org-id)
    
    ;; Basic hooks
    (add-hook 'org-supertag-after-node-change-hook
              #'org-supertag-behavior--handle-node-change)
    (add-hook 'org-supertag-after-tag-add-hook
              #'org-supertag-behavior--handle-tag-add)
    (add-hook 'org-supertag-after-tag-remove-hook
              #'org-supertag-behavior--handle-tag-remove)
    
    ;; Add ID location protection
    (advice-add 'org-supertag-behavior--handle-node-change 
                :around #'org-supertag-behavior--protect-id-locations)

    ;; Setup scheduled behaviors directly
    (org-supertag-behavior--setup-scheduled-behaviors)

    (setq org-supertag-behavior--initialized t)
    (message "=== Behavior System Init Success ===")))

(defun org-supertag-behavior--protect-id-locations (orig-fun &rest args)
  "Ensure org-id-locations maintains correct state before and after behavior execution."
  ;; Save current state
  (let ((old-locations (when (boundp 'org-id-locations) 
                        org-id-locations)))
    (unwind-protect
        (progn
          ;; Ensure it's a hash table
          (unless (and (boundp 'org-id-locations)
                      (hash-table-p org-id-locations))
            (setq org-id-locations (make-hash-table :test 'equal)))
          ;; Execute original function
          (apply orig-fun args))
      ;; Restore state
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
  ;; Initialize scheduled tasks storage
  (setq org-supertag-scheduled-tasks (make-hash-table :test 'equal))
  ;; Iterate through all behaviors and register scheduled tasks
  (maphash
   (lambda (name behavior)
     (when (and (eq (plist-get behavior :trigger) :schedule)
                (plist-get behavior :schedule))
       ;; Build complete task definition
       (org-supertag-schedule-add-task
        (list :id name
              :schedule (plist-get behavior :schedule)
              :action (plist-get behavior :action)))))
   org-supertag-behavior-registry)
  
  ;; Start the scheduler
  (org-supertag-scheduler-start))

(defun org-supertag-behavior--cleanup ()
  "Cleanup behavior system."
  ;; Remove hooks
  (remove-hook 'org-supertag-after-node-change-hook
               #'org-supertag-behavior--handle-node-change)
  (remove-hook 'org-supertag-after-tag-add-hook
               #'org-supertag-behavior--handle-tag-add)
  (remove-hook 'org-supertag-after-tag-remove-hook
               #'org-supertag-behavior--handle-tag-remove)
  (remove-hook 'org-supertag-after-load-hook
               #'org-supertag-behavior--setup-scheduled-behaviors)
  
  ;; Stop scheduler
  (org-supertag-scheduler-stop)

  ;; Remove advice
  (advice-remove 'org-supertag-behavior--handle-node-change 
                #'org-supertag-behavior--protect-id-locations))

(defun org-supertag-behavior--handle-todo-change ()
  "Handle TODO state changes.
Called by `org-after-todo-state-change-hook'."
  (when-let* ((node-id (org-id-get))
              (new-state (org-get-todo-state)))

    (dolist (tag-id (org-supertag-node-get-tags node-id))
      (when-let* ((behavior (org-supertag-behavior--get-behavior tag-id))
                  (trigger (plist-get behavior :trigger)))

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
    
    ;; Validate trigger
    (unless (memq trigger '(:on-add :on-remove :on-change :on-schedule :always))
      (signal 'org-supertag-behavior-error 
              (list :invalid-trigger trigger)))
    
    ;; Validate action (if exists)
    (when action
      (unless (functionp action)
        (signal 'org-supertag-behavior-error 
                (list :invalid-action action))))
    
    ;; Validate style (if exists)
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

(defvar-local org-supertag-behavior--buffer-initialized nil
  "Flag to track if current buffer has been initialized.")

(defvar org-supertag-behavior--async-timer nil
  "Timer for async face refresh.")

(defvar org-supertag-behavior--async-queue nil
  "Queue of pending async refresh tasks.")

(defvar org-supertag-behavior-async-chunk-size 100
  "Number of headings to process in each async chunk.")

(defun org-supertag-behavior--setup-buffer ()
  "Setup current buffer for org-supertag behaviors with lazy loading."
  (when (and (derived-mode-p 'org-mode)
             (not org-supertag-behavior--buffer-initialized))
    ;; 只添加必要的 hooks
    (add-hook 'after-save-hook 
              #'org-supertag-behavior--face-refresh-current nil t)
    (add-hook 'org-after-tags-change-hook 
              #'org-supertag-behavior--face-refresh-current nil t)
    ;; 标记为已初始化
    (setq org-supertag-behavior--buffer-initialized t)
    ;; 延迟加载：只在有标签的节点才初始化样式
    (when (save-excursion
            (goto-char (point-min))
            (re-search-forward org-complex-heading-regexp nil t))
      ;; 使用异步刷新
      (org-supertag-behavior--schedule-async-refresh))))

(defun org-supertag-behavior--face-refresh-current ()
  "Refresh face for current heading only."
  (when (and org-supertag-behavior-mode
             (derived-mode-p 'org-mode)
             (org-at-heading-p))
    (save-excursion
      (when-let ((node-id (org-id-get)))
        ;; 清除当前行的 overlays
        (remove-overlays (line-beginning-position)
                        (line-end-position)
                        'org-supertag-face t)
        ;; 只应用当前节点的样式
        (when (org-supertag-node-get-tags node-id)
          (org-supertag-behavior--apply-styles node-id))))))

(defun org-supertag-behavior--face-refresh (&optional beg end _len)
  "Refresh faces incrementally with async support."
  (when (and org-supertag-behavior-mode
             (derived-mode-p 'org-mode))
    (save-excursion
      (save-restriction
        (widen)
        (condition-case err
            (if (and beg end)
                ;; 增量更新仍然同步处理
                (progn
                  (goto-char (max (point-min) beg))
                  (org-supertag-behavior--face-refresh-current))
              ;; 全局刷新使用异步处理
              (org-supertag-behavior--schedule-async-refresh))
          (error
           (message "Error during face refresh: %S" err)))))))

(defun org-supertag-behavior--schedule-async-refresh ()
  "Schedule async face refresh for current buffer."
  (let ((buffer (current-buffer))
        (cache-key (format "face-refresh:%s" (buffer-file-name))))
    ;; 如果已经在缓存中，跳过
    (unless (org-supertag-db--cache-get 'query cache-key)
      ;; 取消现有的定时器
      (when org-supertag-behavior--async-timer
        (cancel-timer org-supertag-behavior--async-timer))
      ;; 清除旧 overlays
      (remove-overlays (point-min) (point-max) 'org-supertag-face t)
      ;; 初始化队列
      (setq org-supertag-behavior--async-queue
            (list (cons buffer (point-min))))
      ;; 启动异步处理
      (setq org-supertag-behavior--async-timer
            (run-with-idle-timer 0.1 nil
                                #'org-supertag-behavior--process-async-chunk)))))

(defun org-supertag-behavior--process-async-chunk ()
  "Process a chunk of headings asynchronously."
  (when org-supertag-behavior--async-queue
    (let* ((task (car org-supertag-behavior--async-queue))
           (buffer (car task))
           (start-pos (cdr task))
           (processed 0))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (save-restriction
              (widen)
              (goto-char start-pos)
              ;; 处理一个chunk的标题
              (while (and (< processed org-supertag-behavior-async-chunk-size)
                         (< (point) (point-max)))
                (when (and (org-at-heading-p)
                          (org-id-get))
                  (org-supertag-behavior--face-refresh-current)
                  (cl-incf processed))
                (unless (outline-next-heading)
                  (goto-char (point-max))))
              ;; 更新队列
              (if (< (point) (point-max))
                  ;; 还有更多要处理
                  (setf (cdr task) (point))
                ;; 处理完成
                (progn
                  (setq org-supertag-behavior--async-queue
                        (cdr org-supertag-behavior--async-queue))
                  ;; 设置缓存
                  (org-supertag-db--cache-set 
                   'query 
                   (format "face-refresh:%s" (buffer-file-name))
                   t)))
              ;; 继续处理下一个chunk
              (when org-supertag-behavior--async-queue
                (setq org-supertag-behavior--async-timer
                      (run-with-idle-timer 0.1 nil
                                         #'org-supertag-behavior--process-async-chunk))))))))))

(define-minor-mode org-supertag-behavior-mode
  "Toggle org-supertag behavior system."
  :global t
  :group 'org-supertag
  (if org-supertag-behavior-mode
      (progn
        ;; Enable - delayed initialization
        (run-with-idle-timer 
         0.1 nil
         (lambda ()
           (org-supertag-behavior--init)
           ;; Setup only current buffer
           (when (derived-mode-p 'org-mode)
             (org-supertag-behavior--setup-buffer))
           ;; Setup for newly opened/switched org buffers
           (add-hook 'org-mode-hook #'org-supertag-behavior--setup-buffer)))
        t)  ; Return t to indicate successful enable
    ;; Disable
    (progn
      (org-supertag-behavior--cleanup)
      (remove-hook 'org-mode-hook #'org-supertag-behavior--setup-buffer)
      ;; Cleanup only current buffer
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


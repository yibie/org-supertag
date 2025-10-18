
;;; supertag-automation.el --- Unified Automation System for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file implements the complete Automation System 2.0 for the
;; Org-Supertag system. It unifies all automation-related functionality
;; including:
;;
;; 1. Rule indexing and CRUD operations
;; 2. Event-driven automation execution
;; 3. Real-time field synchronization between related entities
;; 4. Rollup calculations and formula fields
;; 5. Bidirectional relation updates
;; 6. Database-level automation rules
;;
;; Key design principles:
;; 1. Pure data-centric architecture with single source of truth
;; 2. Automatic rule indexing for O(1) performance
;; 3. No manual behavior attachment - rules are applied via index
;; 4. Modern automation with multi-action support
;; 5. Unified state management and execution engine

;;; Code:

(require 'cl-lib)
(require 'supertag-core-store)     ; For data storage operations
(require 'supertag-core-schema)    ; For validation functions
(require 'supertag-core-transform) ; For atomic data updates
(require 'supertag-core-notify)    ; For event notifications
(require 'supertag-core-state)     ; For state management

;; Declare function from supertag-automation-sync to avoid circular dependency
(declare-function supertag-automation-sync-handle-event "supertag-automation-sync"
                  (operation collection id payload previous &rest metadata))
(require 'supertag-ops-relation)   ; For relation operations
(require 'supertag-ops-tag)        ; For tag operations
(require 'supertag-ops-node)       ; For node operations
(require 'supertag-ops-field)      ; For field operations
(require 'supertag-service-org)
(require 'org-id)                  ; For ID generation
(require 'ht)

;;; --- Core State Management ---

(defun supertag-automation--ensure-plist (data)
  "Return a plist copy of DATA, converting hash tables when necessary.
This function preserves all data including tags and fields."
  (cond
   ((null data) nil)
   ((hash-table-p data)
    ;; Convert hash table to plist while preserving all data
    (let (plist)
      (maphash (lambda (k v)
                 ;; Ensure we don't lose data during conversion
                 (if (eq k :tags)
                     ;; Special handling for tags to ensure list format
                     (setq plist (plist-put plist :tags (if (listp v) v (list v))))
                   (setq plist (plist-put plist k v))))
               data)
      plist))
   ((listp data)
    (copy-tree data))
   (t
    (error "Unsupported automation entity format: %S" data))))

(defun supertag-automation--normalize-keyword (name)
  "Normalize NAME into a keyword symbol."
  (cond
   ((keywordp name) name)
   ((symbolp name) (intern (concat ":" (symbol-name name))))
   ((stringp name) (intern (concat ":" name)))
   (t (error "Unsupported property key: %S" name))))

(defvar supertag-automation--enabled t
  "Global flag to enable/disable automation execution.")

(defvar supertag-automation--executing nil
  "Flag to indicate if automation is currently executing actions.
This prevents recursive automation triggers during action execution.")

(defvar supertag-automation--event-queue nil
  "Queue of pending automation events to be processed asynchronously.
Each event is a list: (event-handler-fn args...)")

(defvar supertag-automation--processing-timer nil
  "Timer for processing queued automation events.")

(defvar supertag-automation--processing-queue nil
  "Queue for automation tasks to prevent infinite loops.")

(defvar supertag-automation--active-calculations (make-hash-table :test 'equal)
  "Hash table tracking active rollup calculations to prevent recursion.")

(defvar supertag-automation--sync-cache (make-hash-table :test 'equal)
  "Cache for field sync operations to improve performance.")

(defvar supertag-automation--current-event nil
  "Dynamically-bound event context during condition evaluation.
Plist keys:
  :path  - store event path
  :old   - old value at that path
  :new   - new value at that path
  :tag-event - when tag event happens, either :added or :removed
  :tag   - the tag name for a tag event")

;;; --- Automation Rule Indexing System ---

(defvar supertag--rule-index (make-hash-table :test 'equal)
  "The master index for automation rules.
This data structure allows for O(1) lookup of relevant rules
based on a trigger event, avoiding a full scan of all rules.

The structure is a hash table where:
 - KEY is the trigger source (e.g., a property keyword like :status,
  or a tag name like \"Project\").
 - VALUE is a list of rule IDs that are interested in this source.")

(defun supertag--extract-trigger-sources (condition)
  "Recursively parse a rule CONDITION and extract all trigger sources.
A trigger source is a specific property or tag that the rule depends on.
Returns a list of sources, e.g., '(:status :priority \"Project\")."
  (let (sources)
    ;; cl-labels allows defining a local recursive function walk.
    (cl-labels ((walk (form)
                   (when (listp form)
                     (pcase (car form)
                       ('has-tag (push (cadr form) sources))
                       ('field-equals
                        (let ((key (cadr form)))
                          ;; Index by keyword, to match the key generated by the event handler.
                          (when (stringp key)
                            (push (intern (concat ":" key)) sources))))
                       ('property-equals
                        (let ((key (cadr form)))
                          ;; Support both keyword (:status) and string ("status")
                          (cond
                           ((keywordp key) (push key sources))
                           ((stringp key) (push (intern (concat ":" key)) sources)))))
                       ('property-test
                        (let ((key (cadr form)))
                          (cond
                           ((keywordp key) (push key sources))
                           ((stringp key) (push (intern (concat ":" key)) sources)))))
                       ;; Handle quoted conditions e.g. '(and ...) by unwrapping
                       ('quote
                        (walk (cadr form)))
                       ;; For logical operators, walk their sub-expressions.
                       ((or 'and 'or 'not)
                        (dolist (subform (cdr form))
                          (walk subform)))
                       ;; Ignore any other operators/forms.
                       (_ nil)))))
      (walk condition))
    ;; Return the collected sources without duplicates.
    (cl-remove-duplicates sources :test 'equal)))

(defun supertag--add-rule-to-index (rule)
  "Parse a RULE and add its ID to the global rule index."
  (let* ((rule-id (plist-get rule :id))
         (sources (supertag--extract-trigger-sources (plist-get rule :condition))))
    ;; Also consider the trigger itself as a source if it's specific
    (pcase (plist-get rule :trigger)
      (`(:on-tag-added ,tag-name) (push tag-name sources))
      (`(:on-tag-removed ,tag-name) (push tag-name sources)))

    (dolist (source (cl-remove-duplicates sources :test 'equal))
      (let ((rules (gethash source supertag--rule-index '())))
        ;; Use cl-pushnew to add the rule-id to the list of rules, avoiding duplicates.
        (cl-pushnew rule-id rules :test 'equal)
        ;; Put the updated list back into the hash table.
        (puthash source rules supertag--rule-index)))))

(defun supertag--remove-rule-from-index (rule)
  "Parse a RULE and remove its ID from the global rule index."
  (let* ((rule-id (plist-get rule :id))
         (sources (supertag--extract-trigger-sources (plist-get rule :condition))))
    (pcase (plist-get rule :trigger)
      (`(:on-tag-added ,tag-name) (push tag-name sources))
      (`(:on-tag-removed ,tag-name) (push tag-name sources)))

    (dolist (source (cl-remove-duplicates sources))
      (when-let ((rules (gethash source supertag--rule-index)))
        (puthash source (remove rule-id rules) supertag--rule-index)))))

(defun supertag-rebuild-rule-index ()
  "Clear and rebuild the entire automation rule index.
This function should be called once on system startup to ensure
the index is synchronized with the stored rules."
  (interactive)
  (clrhash supertag--rule-index)
  (let ((all-rules (supertag-automation-list)))
    (dolist (rule all-rules)
      (supertag--add-rule-to-index rule))
    (message "Automation rule index rebuilt. %d rules indexed." (length all-rules))))

(defun supertag--get-rules-from-index (event-path)
  "Get a list of relevant rule IDs from the index based on an EVENT-PATH.
EVENT-PATH is the path from the :store-changed event, e.g.,
'(:nodes \"node-id\" :properties :status).
This function uses the pre-built supertag--rule-index for O(1) lookups."
  ;; Handle both flat (:nodes "ID") and nested ((:nodes "ID")) paths.
  (let* ((path (if (and (listp (car event-path)) (memq (caar event-path) '(:nodes :tags)))
                 (car event-path)
                 event-path))
         (entity-type (car path))
         (entity-id (cadr path))
         (node-data (cond
                     ((eq entity-type :nodes)
                      (supertag-node-get entity-id))
                     ((eq entity-type :fields)
                      (supertag-node-get entity-id))
                     (t nil)))
         (node-tags (when node-data (plist-get node-data :tags)))
         (changed-prop (let ((p path))
                         (cond
                          ;; Field change event, e.g., '(:fields "node-id" "tag-id" "status")
                          ((and (eq (car p) :fields) (>= (length p) 4) (stringp (cadddr p)))
                           (intern (concat ":" (cadddr p))))
                          ;; Node property change event, e.g., '(:nodes "id" :properties :status)
                          ((and (eq (car p) :nodes)
                                (>= (length p) 4)
                                (eq (nth 2 p) :properties)
                                (keywordp (nth 3 p)))
                           (nth 3 p))
                          (t (progn
                               (unless (and (eq (car p) :nodes) (= (length p) 2))
                                 ;; (message "DEBUG-3: Path did not match expected patterns. Path: %S" p)
                                 )
                               nil)))))
         (candidate-rules '()))

    ;; 1. Collect rules based on the specific property that changed
    (when changed-prop
      ;; (message "DEBUG-4: Rule lookup key is: %S" changed-prop)
      ;; (message "DEBUG-4.5: Current rule index keys: %S" (hash-table-keys supertag--rule-index))
      (when-let ((rules (gethash changed-prop supertag--rule-index)))
        ;; (message "DEBUG-5: Found rules for key: %S" rules)
        (setq candidate-rules (append rules candidate-rules)))
      (unless (gethash changed-prop supertag--rule-index)
        ;; (message "DEBUG-5.5: No rules found for key %S in index" changed-prop)
        ))

    ;; 2. Collect rules based on the tags of the affected node
    (when node-tags
      (dolist (tag-name node-tags)
        (when-let ((rules (gethash tag-name supertag--rule-index)))
          (setq candidate-rules (append rules candidate-rules)))))

    ;; 3. Return a unique list of rule IDs
    (cl-remove-duplicates candidate-rules :test 'equal)))

;;; --- Data Validation ---

(defun supertag-automation--normalize-trigger (trigger)
  "Normalize trigger to canonical format.
TRIGGER can be:
- A keyword like :on-field-change, :on-schedule
- A list like (:on-tag-added \"tagname\") or (:on-tag-removed \"tagname\")
Returns the normalized trigger."
  (cond
   ;; Already a list form - return as is
   ((and (listp trigger) (keywordp (car trigger))) trigger)
   ;; Simple keyword - return as is
   ((keywordp trigger) trigger)
   ;; String - try to intern it
   ((stringp trigger) (intern trigger))
   ;; Otherwise return as is
   (t trigger)))

(defun supertag--validate-automation-data (data)
  "Validate automation data structure."
  (unless (plist-get data :name)
    (error "Automation missing required :name field: %S" data))
  (unless (plist-get data :trigger)
    (error "Automation missing required :trigger field: %S" data))
  (unless (plist-get data :actions)
    (error "Automation missing required :actions field: %S" data)))





;;; --- Core CRUD Operations ---

(defun supertag-automation-create (automation-data)
  "Create a new automation rule.
AUTOMATION-DATA should contain:
- :name (required) - Automation name
- :description (optional) - Description
- :trigger (required) - Trigger condition
- :condition (optional) - When to execute (list of conditions)
- :actions (required) - List of actions to perform
- :enabled (optional) - Whether automation is enabled (default: t)

Returns the created automation data with assigned ID."
  (supertag-with-transaction
    (let* ((name (plist-get automation-data :name))
           (id (format "auto-%s" (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" name))))
      
      ;; Validate input data
      (supertag--validate-automation-data automation-data)
      
      ;; Check if automation already exists and delete it if it does
      (let ((existing (supertag-automation-get id)))
        (when existing
          (supertag-automation-delete id)))
      
      ;; Prepare automation data
      (let ((automation-plist (list :id id
                                    :name name
                                    :description (or (plist-get automation-data :description) "")
                                    :trigger (supertag-automation--normalize-trigger
                                              (plist-get automation-data :trigger))
                                    :condition (plist-get automation-data :condition)
                                    :actions (plist-get automation-data :actions)
                                    :enabled (if (plist-member automation-data :enabled)
                                               (plist-get automation-data :enabled)
                                             t)
                                    :created-at (current-time)
                                    :modified-at (current-time))))
        
        ;; Store automation
        (supertag-store-put-entity :automations id automation-plist t)

        ;; Add to the rule index
        (supertag--add-rule-to-index automation-plist)

        ;; Register to scheduler if needed
        (when (eq (plist-get automation-plist :trigger) :on-schedule)
          (supertag-automation--register-scheduled-rule automation-plist))
        
        ;; Notify system
        (when (fboundp 'supertag-notify)
          (supertag-notify :automation-created :automation-id id :data automation-plist))
        
        (message "Created automation: %s (ID: %s)" name id)
        automation-plist))))

(defun supertag-automation-get (id)
  "Get automation by ID."
  (supertag-store-get-entity :automations id))

(defun supertag-automation-update (id updater)
  "Update automation data.
ID is the automation identifier.
UPDATER is a function that receives current data and returns updated data."
  (let ((automation (supertag-automation-get id)))
    (when automation
      (let ((updated-automation (funcall updater automation)))
        (when updated-automation
          ;; Update scheduler bridge
          (when (eq (plist-get automation :trigger) :on-schedule)
            (supertag-automation--deregister-scheduled-rule automation))
          ;; Update indexes
          (supertag--remove-rule-from-index automation)
          ;; Normalize trigger before re-index
          (setq updated-automation
                (plist-put updated-automation :trigger
                           (supertag-automation--normalize-trigger
                            (plist-get updated-automation :trigger))))
          (supertag--add-rule-to-index updated-automation)
          (let ((final-automation (plist-put updated-automation :modified-at (current-time))))
            (supertag--validate-automation-data final-automation)
            (supertag-store-put-entity :automations id final-automation t)
            ;; Re-register if still scheduled
            (when (eq (plist-get final-automation :trigger) :on-schedule)
              (supertag-automation--register-scheduled-rule final-automation))
            (when (fboundp 'supertag-notify)
              (supertag-notify :automation-updated :automation-id id :data final-automation))
            final-automation))))))

(defun supertag-automation-delete (id)
  "Delete automation by ID.
Returns the deleted automation data or nil if not found."
  (supertag-with-transaction
    (let ((automation (supertag-automation-get id)))
      (when automation
        ;; Deregister from scheduler if needed
        (when (eq (plist-get automation :trigger) :on-schedule)
          (supertag-automation--deregister-scheduled-rule automation))
        ;; Remove from the rule index
        (supertag--remove-rule-from-index automation)
        ;; Delete from storage
        (supertag-store-remove-entity :automations id)
        ;; Notify system
        (when (fboundp 'supertag-notify)
          (supertag-notify :automation-deleted :automation-id id :data automation))
        (message "Deleted automation: %s" (plist-get automation :name))
        automation))))

(defun supertag-automation-list (&optional filter)
  "List all automations with optional FILTER.
FILTER can be a function that receives automation data and returns t/nil."
  (let ((result '()))
    (maphash
     (lambda (_id automation)
       (when (or (null filter) (funcall filter automation))
         (push automation result)))
     (supertag-store-get-collection :automations))
    (sort result (lambda (a b)
                   (string< (plist-get a :name) (plist-get b :name))))))

(defun supertag-automation-get-by-name (name)
  "Get automation by name.
Returns automation data or nil if not found."
  (let ((id (format "auto-%s" (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" name))))
    (supertag-automation-get id)))

;;; --- Query Functions ---

(defun supertag-automation-find-by-trigger (trigger-type)
  "Find all automations with specific trigger type."
  (let ((target (supertag-automation--normalize-trigger trigger-type)))
    (supertag-automation-list
     (lambda (automation)
       (equal (supertag-automation--normalize-trigger
               (plist-get automation :trigger))
              target)))))

(defun supertag-automation-find-enabled ()
  "Find all enabled automations."
  (supertag-automation-list (lambda (automation)
                              (plist-get automation :enabled))))

(defun supertag-automation-find-scheduled ()
  "Find all scheduled automations."
  (supertag-automation-list (lambda (automation)
                              (eq (plist-get automation :trigger) :on-schedule))))

;; === Scheduler Bridge ===
(defun supertag-automation--register-scheduled-rule (rule)
  "Register a :on-schedule RULE with the central scheduler, if available."
  (when (and (eq (plist-get rule :trigger) :on-schedule)
             (fboundp 'supertag-scheduler-register-task))
    (let* ((schedule (plist-get rule :schedule))
           (time (plist-get schedule :time))
           (days (plist-get schedule :days-of-week))
           (id-sym (intern (plist-get rule :id)))
           (runner
            (lambda ()
              (let ((rule-now (supertag-automation-get (plist-get rule :id))))
                (when (and rule-now (plist-get rule-now :enabled))
                  (dolist (action (plist-get rule-now :actions))
                    (when (eq (plist-get action :action) :call-function)
                      (supertag-automation-execute-action
                       :call-function nil (plist-get action :params)
                       (list :scheduled t :rule (plist-get rule-now :id))))))))))
      (condition-case err
          (progn
            (supertag-scheduler-register-task id-sym :daily runner
                                              :time time
                                              :days-of-week days)
            (message "[Automation Scheduler] Registered scheduled rule: %s at %s days=%S"
                     (plist-get rule :id) time days))
        (error (message "ERROR: Failed to register scheduled rule %s: %S"
                        (plist-get rule :id) err))))))

(defun supertag-automation--deregister-scheduled-rule (rule)
  "Deregister a scheduled RULE from the scheduler, if available."
  (when (and (fboundp 'supertag-scheduler-deregister-task)
             (plist-get rule :id))
    (let ((id-sym (intern (plist-get rule :id))))
      (condition-case err
          (progn
            (supertag-scheduler-deregister-task id-sym)
            (message "[Automation Scheduler] Deregistered scheduled rule: %s" (plist-get rule :id)))
        (error (message "ERROR: Failed to deregister scheduled rule %s: %S"
                        (plist-get rule :id) err))))))

(defun supertag-automation--register-all-scheduled ()
  "Register all :on-schedule rules with the scheduler."
  (when (fboundp 'supertag-scheduler-register-task)
    (dolist (rule (supertag-automation-find-scheduled))
      (supertag-automation--register-scheduled-rule rule))))

;;; --- Rule Execution Engine ---

(defun supertag-rule-get (id)
  "Get any rule by ID. Unified interface for rule retrieval."
  (supertag-automation-get id))

(defun supertag-rule-execute (rule node-id context)
  "Execute a rule with given context.
This is called by the automation engine when a rule matches an event."
  (when (and rule (plist-get rule :enabled))
    ;; Prevent recursive automation during action execution
    (if supertag-automation--executing
        (message "SKIP: Automation already executing, preventing recursion for rule %s" (plist-get rule :id))
      (let ((actions (plist-get rule :actions))
            (rule-id (plist-get rule :id))
            (supertag-automation--executing t))
        (message "Executing rule %s on node %s" rule-id node-id)
        (unwind-protect
            (progn
              (dolist (action-spec actions)
                (let ((action-type (plist-get action-spec :action))
                      (params (plist-get action-spec :params)))
                  (supertag-automation-execute-action action-type node-id params context)))
              (supertag-automation-log-execution rule-id node-id (plist-get (car actions) :action)))
          ;; Always clear the executing flag
          (setq supertag-automation--executing nil))))))

(defun supertag-automation-execute-action (action-type node-id params context)
  "Execute a specific action type.
ACTION-TYPE is the action to perform
NODE-ID is the target node
PARAMS are action parameters
CONTEXT provides execution context"
  (pcase action-type
    (:update-property
     (supertag-automation-action-update-property node-id params))

    (:update-todo-state
     (supertag-automation-action-update-todo-state node-id params))
    
    (:add-tag
     (supertag-automation-action-add-tag node-id params))
    
    (:remove-tag
     (supertag-automation-action-remove-tag node-id params))

    (:create-node
     (supertag-automation-action-create-node params))

    (:update-field
     (supertag-automation-action-update-field node-id params))
        
    (:call-function
     (supertag-automation-action-call-function node-id params context))
    
    (_
     (message "Unknown action type: %s" action-type))))

(defun supertag-automation-action-update-property (node-id params)
  "Update an Org-mode property on the node.
This updates native Org properties like :SCHEDULED:, :DEADLINE:, etc.
Preserves all existing node data including tags and other properties."
  (let ((property (plist-get params :property))
        (value (plist-get params :value)))
    (when (and node-id property)
      (supertag-node-update
       node-id
       (lambda (node)
         (when node
           (let* ((props (copy-tree (or (plist-get node :properties) '())))
                  (current (plist-get props property)))
             (if (equal current value)
                 (progn
                   (message "SKIP(update-property): %s unchanged on node %s" property node-id)
                   node)  ; Return unchanged node to preserve data
               ;; Create new node copy with updated property while preserving everything else
               (let ((new-props (plist-put props property value))
                     (updated-node (copy-tree node)))
                 (plist-put updated-node :properties new-props))))))))))

(defun supertag-automation-action-update-todo-state (node-id params)
  "Update the TODO state of a node.
PARAMS should contain :state with the new TODO keyword (e.g., \"DONE\")."
  (require 'supertag-service-org)
  (when-let ((state (plist-get params :state)))
    (supertag-service-org-set-todo-state node-id state)))

(defun supertag-automation-action-add-tag (node-id params)
  "Add a tag to the node.
Uses the same data-first approach as UI commands for consistency."
  (require 'supertag-ops-tag)
  (require 'supertag-view-helper)
  (when (and node-id)
    (when-let ((tag-name (plist-get params :tag)))
      (let* ((node (supertag-node-get node-id))
             (tags (plist-get node :tags)))
        (if (member tag-name tags)
            (message "SKIP(add-tag): tag '%s' already on node %s" tag-name node-id)
          ;; 1. Update database first (same as UI command)
          (supertag-ops-add-tag-to-node node-id tag-name :create-if-needed t)
          ;; 2. Update file text
          (supertag-service-org--with-node-buffer node-id
            (lambda ()
              ;; Check if tag already exists in the line to avoid duplicates
              (let ((line-content (buffer-substring (line-beginning-position) (line-end-position))))
                (unless (string-match (concat "#" (regexp-quote tag-name) "\\b") line-content)
                  (end-of-line)
                  (insert (concat " #" tag-name))))
              (save-buffer)
              ;; Mark as internal modification AFTER saving
              (when (buffer-file-name)
                (supertag--mark-internal-modification (buffer-file-name)))))
          (message "Automation: Added tag '%s' to node %s" tag-name node-id))))))

(defun supertag-automation-action-remove-tag (node-id params)
  "Remove a tag from the node.
Uses the same data-first approach as UI commands for consistency."
  (require 'supertag-ops-relation)
  (require 'supertag-ops-node)
  (when (and node-id)
    (when-let ((tag-name (plist-get params :tag)))
      (let* ((node (supertag-node-get node-id))
             (tags (plist-get node :tags)))
        (if (not (member tag-name tags))
            (message "SKIP(remove-tag): tag '%s' not present on node %s" tag-name node-id)
          ;; 1. Delete relationship from database (same as UI command)
          (let* ((relations (supertag-relation-find-between node-id tag-name :node-tag))
                 (relation-to-delete (car relations)))
            (when relation-to-delete
              (supertag-relation-delete (plist-get relation-to-delete :id))))
          ;; 2. Remove tag from node's tags list
          (supertag-node-remove-tag node-id tag-name)
          ;; 3. Update file text
          (supertag-service-org--with-node-buffer node-id
            (lambda ()
              (let ((tag-regexp (concat "\\s-?#" (regexp-quote tag-name) "\\b")))
                (when (re-search-forward tag-regexp (line-end-position) t)
                  (replace-match "")))
              (save-buffer)
              ;; Mark as internal modification AFTER saving
              (when (buffer-file-name)
                (supertag--mark-internal-modification (buffer-file-name)))))
          (message "Automation: Removed tag '%s' from node %s" tag-name node-id))))))

(defun supertag-automation-action-call-function (node-id params context)
  "Call a custom function with node context."
  (let ((function (plist-get params :function))
        (args (plist-get params :args)))
    (when (functionp function)
      (apply function node-id context args))))

(defun supertag-automation-action-create-node (params)
  "Create a new node. PARAMS supports: :title, :tags (list of strings)."
  (let ((title (plist-get params :title))
        (tags (plist-get params :tags)))
    (cond
     ((not (fboundp 'supertag-node-create))
      (message "ERROR(:create-node): supertag-node-create not available"))
     (t
      (let ((node-id (apply #'supertag-node-create
                            (append (when title (list :title title))
                                    (when tags (list :tags tags))))))
        (message "Created node %s (title=%s tags=%S)" node-id title tags)
        node-id)))))

(defun supertag-automation-action-update-field (node-id params)
  "Update a tag field for NODE-ID. PARAMS: :tag (id/name), :field (string), :value."
  (let ((tag-id (plist-get params :tag))
        (field (plist-get params :field))
        (value (plist-get params :value)))
    (cond
     ((not node-id)
      (message "ERROR(:update-field): node-id is nil"))
     ((not (and tag-id (stringp field)))
      (message "ERROR(:update-field): requires :tag and string :field")))
    (when (and node-id tag-id (stringp field))
      (let ((current (supertag-field-get node-id tag-id field)))
        (if (equal current value)
            (message "SKIP(update-field): %s/%s unchanged on node %s" tag-id field node-id)
          (supertag-field-set node-id tag-id field value))))))

;;; --- Field Synchronization Engine ---

(defun supertag-automation-sync-field (from-id to-id field-name value &optional relation-config)
  "Sync a field value between two entities.
FROM-ID is the source entity ID.
TO-ID is the target entity ID.
FIELD-NAME is the field to sync.
VALUE is the value to sync.
RELATION-CONFIG provides sync configuration from the relation."
  (when supertag-automation--enabled
    (let ((sync-key (format "%s->%s:%s" from-id to-id field-name)))
      ;; Check cache to avoid redundant syncs
      (unless (equal (gethash sync-key supertag-automation--sync-cache) value)
        ;; Update cache
        (puthash sync-key value supertag-automation--sync-cache)
        
        ;; Determine target entity type and update
        (let ((target-node (supertag-node-get to-id))
              (target-tag (supertag-tag-get to-id)))
          (cond
           ;; Target is a node
           (target-node
            (supertag-automation--sync-to-node to-id field-name value))
           
           ;; Target is a tag/database
           (target-tag
            (supertag-automation--sync-to-tag to-id field-name value))
           
           (t
            (message "Warning: Unknown target entity type for %s" to-id))))
        
        (message "Synced property %s=%s from %s to %s"
                 field-name value from-id to-id)))))

(defun supertag-automation--sync-to-node (node-id field-name value)
  "Sync field to a node entity via properties.
Preserves all existing node data including tags and other properties."
  (let ((field-key (supertag-automation--normalize-keyword field-name)))
    (supertag-node-update
     node-id
     (lambda (node)
       (when node
         (let* ((props (copy-tree (or (plist-get node :properties) '())))
                (current (plist-get props field-key)))
           (if (equal current value)
               node  ; Return unchanged node to preserve data
             ;; Create new props with updated field
             (let ((new-props (plist-put props field-key value)))
               ;; Update node while preserving all other fields
               (let ((updated-node (copy-tree node)))
                 (plist-put updated-node :properties new-props))))))))))

(defun supertag-automation--sync-to-tag (tag-id field-name value)
  "Sync field to a tag entity."
  (let ((field-key (supertag-automation--normalize-keyword field-name)))
    (when (fboundp 'supertag-tag-update)
      (supertag-tag-update
       tag-id
       (lambda (tag)
         (let ((current (plist-get tag field-key)))
           (if (equal current value)
               nil
             (plist-put tag field-key value))))))))

(defun supertag-automation-sync-all-relations (entity-id field-name value)
  "Sync a field to all related entities.
ENTITY-ID is the source entity.
FIELD-NAME is the field that changed.
VALUE is the new value."
  (when supertag-automation--enabled
    ;; Find all relations from this entity
    (let ((relations (when (fboundp 'supertag-relation-find-by-from)
                       (supertag-relation-find-by-from entity-id))))
      (dolist (relation relations)
        (let* ((sync-fields (plist-get relation :sync-fields))
               (sync-direction (plist-get relation :sync-direction))
               (to-id (plist-get relation :to)))
          
          ;; Check if this field should be synced
          (when (and sync-fields
                     (member field-name sync-fields)
                     (not (eq sync-direction :disabled)))
            (supertag-automation-sync-field entity-id to-id field-name value relation)))))))

;;; --- Rollup Calculation Engine ---

(defun supertag-automation-calculate-rollup (source-node-id relation-name &optional force)
  "Calculate rollup value for a 'one' side node based on a relation.
This function aligns with the Behavior System 2.0 guide.

SOURCE-NODE-ID is the ID of the 'one' side node (e.g., a project).
RELATION-NAME is the name of the relation template (e.g., 'tasks').
FORCE bypasses recursion protection when t."
  (let ((calculation-key (format "rollup-%s-%s" source-node-id relation-name)))
    (when (or force (not (gethash calculation-key supertag-automation--active-calculations)))
      (puthash calculation-key t supertag-automation--active-calculations)
      
      (unwind-protect
          (when-let* ((relation-template (when (fboundp 'supertag-relation-get-by-name)
                                           (supertag-relation-get-by-name relation-name)))
                      (rollup-config (plist-get relation-template :rollup)))

            (let* ((from-field (plist-get rollup-config :from-field))
                   (to-field (plist-get rollup-config :to-field))
                   (rollup-func (plist-get rollup-config :function))
                   (result (supertag-automation--execute-rollup-calculation
                            source-node-id relation-name from-field rollup-func)))

              (when result
                ;; Update the ':to-field' on the 'one' side node.
                (when (fboundp 'supertag-node-update-property)
                  (supertag-node-update-property source-node-id to-field result))
                (message "Calculated rollup for %s: %s = %s"
                         source-node-id to-field result)
                result)))
        ;; Always clean up active calculation flag
        (remhash calculation-key supertag-automation--active-calculations)))))

(defun supertag-automation--execute-rollup-calculation (source-node-id relation-name from-field rollup-function)
  "Execute the actual rollup calculation for a given source node."
  (let* ((related-nodes-info (when (fboundp 'supertag-relation-get-related-nodes)
                               (supertag-relation-get-related-nodes source-node-id relation-name)))
         (values (supertag-automation--collect-field-values
                   related-nodes-info from-field)))
    (when values
      (supertag-automation--apply-rollup-function rollup-function values))))

(defun supertag-automation--collect-field-values (entities-info field-name)
  "Collect values of FIELD-NAME from a list of entity info plists."
  (let ((values '()))
    (dolist (entity-info entities-info)
      (let* ((node-data (supertag-node-get (plist-get entity-info :id)))
             (props (plist-get node-data :properties))
             (value (plist-get props (intern (concat ":" field-name)))))
        (when value
          (push value values))))
    values))

(defun supertag-automation--apply-rollup-function (function-name values)
  "Apply FUNCTION-NAME to VALUES list.
Supports built-in functions and custom functions."
  (pcase function-name
    ('count (length values))
    ('sum (cl-reduce #'+ values :initial-value 0))
    ('average (if values (/ (cl-reduce #'+ values :initial-value 0.0) (length values)) 0))
    ('min (when values (apply #'min values)))
    ('max (when values (apply #'max values)))
    ('first (car values))
    ('last (car (last values)))
    ('unique-count (length (cl-remove-duplicates values :test #'equal)))
    ('concat (mapconcat #'identity values ", "))
    (_
     ;; Custom function
     (if (functionp function-name)
         (funcall function-name values)
       (message "Unknown rollup function: %s" function-name)))))

;;; --- Formula Field Engine ---

(defun supertag-automation-calculate-formula (entity-id formula-field)
  "Calculate formula field value for an entity.
ENTITY-ID is the target entity.
FORMULA-FIELD is the field configuration with formula."
  (let* ((formula (plist-get formula-field :formula))
         (field-name (plist-get formula-field :name)))
    
    (when formula
      (let ((result (supertag-automation--evaluate-formula formula entity-id)))
        (when result
          ;; Formula fields are for view-time rendering only in System 2.0
          result)))))

(defun supertag-automation--evaluate-formula (formula entity-id)
  "Evaluate FORMULA for ENTITY-ID.
Supports basic arithmetic and property references."
  (let* ((entity (or (supertag-node-get entity-id)
                     (supertag-tag-get entity-id)))
         (props (plist-get entity :properties)))
    
    ;; Simple formula evaluation - replace property references
    (let ((expanded-formula formula))
      ;; Replace {{property}} references with actual values
      (while (string-match "{{([^}]+)}}" expanded-formula)
        (let* ((prop-name (match-string 1 expanded-formula))
               (prop-value (plist-get props (intern prop-name)))
               (value-str (if prop-value (format "%s" prop-value) "0")))
          (setq expanded-formula
                (replace-match value-str nil nil expanded-formula))))
      
      ;; Evaluate the formula (basic arithmetic)
      (condition-case nil
          (eval (read expanded-formula))
        (error 0)))))

;;; --- Event Integration (Updated for Sync Processing) ---

(defun supertag-automation--queue-event (handler-fn &rest args)
  "Queue an automation event for asynchronous processing (legacy support).
HANDLER-FN is the function to call, ARGS are its arguments.
DEPRECATED: Use synchronous processing via supertag-automation-sync instead."
  (when supertag-automation--enabled
    (push (cons handler-fn args) supertag-automation--event-queue)
    ;; Schedule processing if not already scheduled
    (unless supertag-automation--processing-timer
      (setq supertag-automation--processing-timer
            (run-at-time 0.001 nil #'supertag-automation--process-event-queue)))))

(defun supertag-automation--process-event-queue ()
  "Process all queued automation events (legacy support)."
  (setq supertag-automation--processing-timer nil)
  (when supertag-automation--event-queue)
    (let ((events (nreverse supertag-automation--event-queue)))
      (setq supertag-automation--event-queue nil)
      (dolist (event events)
        (let ((handler (car event))
              (args (cdr event)))
          (apply handler args)))))

(defun supertag-automation--handle-entity-change (path old-value new-value)
  "Handle entity changes and trigger automation.
This integrates with the event notification system.
Events are now processed synchronously via the new commit system."
  ;; Ensure sync module is loaded and route through the new sync system
  (require 'supertag-automation-sync)
  (let ((operation (cond
                    ;; Determine operation type from context
                    ((null old-value) :create)
                    ((null new-value) :delete)
                    (t :update)))
        (collection (car path))
        (id (cadr path))
        (payload new-value)
        (previous old-value))
    (supertag-automation-sync-handle-event operation collection id payload previous)))

(defun supertag-automation--handle-entity-change-sync (path old-value new-value)
  "Synchronously handle entity changes (called from event queue).
This is the actual handler that was previously called directly."
  (when (listp path)
    (let* ((node-id (cadr path))
           (entity-type (car path))
           ;; Detect tag change under :nodes path like (:nodes ID :tags ...)
           (tags-change-under-node (and (eq entity-type :nodes) (member :tags path))))
      ;; Aggressive Re-entrancy Guard: Do not run any automation on a node
      ;; if it is already being processed.
      (when (and supertag-automation--enabled
                 (not (cl-member node-id supertag-automation--processing-queue :test 'equal)))
        (unwind-protect
            (progn
              ;; Add the node-id to the processing queue.
              (push node-id supertag-automation--processing-queue)
              (when (and (listp path) (>= (length path) 2))
                (cond
                 ;; Field or generic node changes (exclude explicit tag list change)
                 ((or (eq entity-type :fields)
                      (and (eq entity-type :nodes) (not tags-change-under-node)))
                  ;; (message "DEBUG-EVENT: node/field change path=%S" path)
                  (supertag-automation--handle-node-change path old-value new-value))
                 ;; Tag list changes (explicit :tags path or :nodes ... :tags ...)
                 ((or (eq entity-type :tags) tags-change-under-node)
                  (let* ((old-tags (if (listp old-value)
                                       old-value
                                     (let ((nd (supertag-node-get node-id)))
                                       (plist-get nd :tags))))
                         (new-tags (if (listp new-value)
                                       new-value
                                     (let ((nd (supertag-node-get node-id)))
                                       (plist-get nd :tags)))))
                    ;; (message "DEBUG-EVENT: tag change path=%S old=%S new=%S" path old-tags new-tags)
                    (supertag-automation--handle-tag-change node-id old-tags new-tags))))))
          ;; Always remove the node-id from the queue when done.
          (setq supertag-automation--processing-queue
                (cl-delete node-id supertag-automation--processing-queue :test 'equal)))))))

(defun supertag-automation--handle-node-change (path old-value new-value)
  "Handle node changes and trigger relevant automation."
  (let ((rule-ids (supertag--get-rules-from-index path)))
    ;; (message "DEBUG-6: Found %d candidate rules for path %S" (length rule-ids) path)
    (dolist (rule-id rule-ids)
      ;; (message "DEBUG-6.5: Checking rule %s" rule-id)
      (when-let ((rule (supertag-rule-get rule-id)))
        (let ((trig (plist-get rule :trigger)))
          ;; Runtime trigger gate: skip tag-only triggers here
          (if (and (consp trig) (memq (car trig) '(:on-tag-added :on-tag-removed)))
              ;; (message "DEBUG-TRIGGER: skip tag-triggered rule %s for non-tag event %S" rule-id path)
              nil
            (let ((supertag-automation--current-event (list :path path :old old-value :new new-value)))
              ;; (message "DEBUG-6.7: Evaluating condition for rule %s" rule-id)
              (if (supertag-automation--evaluate-condition (plist-get rule :condition) (cadr path))
                  (progn
                    (message "Condition passed for rule %s. Executing actions." rule-id)
                    (message "INDEX-MATCH: Event %S triggered rule %S" path rule-id)
                    (supertag-rule-execute rule (cadr path)
                                           (list :path path :old old-value :new new-value)))
                ;; (message "DEBUG-6.8: Condition failed for rule %s" rule-id)
                ))))))))

(defun supertag-automation--handle-tag-change (node-id old-tags new-tags)
  "Detect added/removed tags and execute runtime-gated tag rules."
  (let* ((resolve (lambda (value)
                    (cond
                     ((listp value) value)
                     (t (let ((nd (supertag-node-get node-id)))
                          (plist-get nd :tags))))))
         (old (cl-remove-duplicates (copy-sequence (or (funcall resolve old-tags) '()))
                                    :test 'equal))
         (new (cl-remove-duplicates (copy-sequence (or (funcall resolve new-tags) '()))
                                    :test 'equal))
         (added (cl-set-difference new old :test 'equal))
         (removed (cl-set-difference old new :test 'equal)))
    ;; (message "DEBUG-TAGS: node=%s added=%S removed=%S" node-id added removed)
    (dolist (tag added)
      (supertag-automation--execute-tag-trigger node-id tag :added))
    (dolist (tag removed)
      (supertag-automation--execute-tag-trigger node-id tag :removed))))

(defun supertag-automation--execute-tag-trigger (node-id tag-name op)
  "Execute rules gated by :trigger (:on-tag-added TAG) or (:on-tag-removed TAG)."
  (let ((candidate (gethash tag-name supertag--rule-index)))
    (when candidate
      (dolist (rule-id candidate)
        (when-let ((rule (supertag-rule-get rule-id)))
          (pcase (plist-get rule :trigger)
            (`(:on-tag-added ,tn)
             (when (and (eq op :added) (equal tn tag-name)
                        (let ((supertag-automation--current-event (list :tag-event op :tag tag-name)))
                          (supertag-automation--evaluate-condition (plist-get rule :condition) node-id)))
               (message "INDEX-MATCH: Tag added '%s' triggered rule %s" tag-name rule-id)
               (supertag-rule-execute rule node-id (list :tag-event :added :tag tag-name))))
            (`(:on-tag-removed ,tn)
             (when (and (eq op :removed) (equal tn tag-name)
                        (let ((supertag-automation--current-event (list :tag-event op :tag tag-name)))
                          (supertag-automation--evaluate-condition (plist-get rule :condition) node-id)))
               (message "INDEX-MATCH: Tag removed '%s' triggered rule %s" tag-name rule-id)
               (supertag-rule-execute rule node-id (list :tag-event :removed :tag tag-name))))
            (_ nil)))))))

;; Helper functions for condition evaluation
(defun supertag-automation--get-field-value (node-id tags field-name)
  "Get field value for FIELD-NAME from any tag in TAGS on NODE-ID."
  (when (and (stringp field-name) tags)
    (cl-some (lambda (tag-id)
               (supertag-field-get node-id tag-id field-name))
             tags)))

(defun supertag-automation--property-changed-p (name)
  "Check if property/field NAME changed in current event context."
  (let ((ev supertag-automation--current-event))
    (and ev
         (plist-get ev :path)
         (or
          ;; Field change: (:fields node-id tag-id field-name)
          (and (stringp name)
               (eq (car (plist-get ev :path)) :fields)
               (>= (length (plist-get ev :path)) 4)
               (string= (cadddr (plist-get ev :path)) name))
          ;; Node property change: (:nodes node-id :properties :prop-name)
          (and (keywordp name)
               (eq (car (plist-get ev :path)) :nodes)
               (>= (length (plist-get ev :path)) 4)
               (eq (nth 2 (plist-get ev :path)) :properties)
               (eq (nth 3 (plist-get ev :path)) name))))))

(defun supertag-automation--eval-single-condition (cond-form node-data)
  "Evaluate a single condition COND-FORM against NODE-DATA.
Returns t if condition passes, nil otherwise."
  (let* ((tags (plist-get node-data :tags))
         (props (plist-get node-data :properties))
         (node-id (plist-get node-data :id))
         (op (car cond-form))
         (args (cdr cond-form)))
    
    (pcase op
      ;; Logical operators
      ('and
       (cl-every (lambda (sub-cond)
                   (supertag-automation--eval-single-condition sub-cond node-data))
                 args))
      
      ('or
       (cl-some (lambda (sub-cond)
                  (supertag-automation--eval-single-condition sub-cond node-data))
                args))
      
      ('not
       (not (supertag-automation--eval-single-condition (car args) node-data)))
      
      ;; Unwrap quoted conditions
      ('quote
       (supertag-automation--eval-single-condition (cadr cond-form) node-data))
      
      ;; Tag conditions
      ('has-tag
       (and tags (member (car args) tags)))
      
      ;; Field conditions
      ('field-equals
       (let ((key (car args))
             (expected-val (cadr args)))
         (if (not (stringp key))
             (progn
               (message "Automation Error: field-equals expects string key, got: %S" key)
               nil)
           (let ((actual-val (supertag-automation--get-field-value node-id tags key)))
             (equal actual-val expected-val)))))
      
      ;; Property conditions
      ('property-equals
       (let ((key (car args))
             (expected-val (cadr args)))
         (cond
          ((keywordp key)
           (let ((actual-val (plist-get props key)))
             ;; (message "DEBUG-EVAL(property-equals): %s == %S ? actual=%S" key expected-val actual-val)
             (equal actual-val expected-val)))
          
          ((stringp key)
           (let ((actual-val (supertag-automation--get-field-value node-id tags key)))
             ;; (message "DEBUG-EVAL(property-equals str): %s == %S ? actual=%S" key expected-val actual-val)
             (equal actual-val expected-val))))))
      
      ('property-changed
       (let ((key (car args)))
         (if (or (keywordp key) (stringp key))
             (supertag-automation--property-changed-p key)
           nil)))
      
      ('property-test
       (let ((key (car args))
             (test-fn (cadr args))
             (fn-args (cddr args)))
         (let ((value (cond
                       ((keywordp key) (plist-get props key))
                       ((stringp key) (supertag-automation--get-field-value node-id tags key))
                       (t nil))))
           (if (functionp test-fn)
               (apply test-fn value fn-args)
             nil)))))))

(defun supertag-automation--evaluate-condition (condition node-id)
  "Evaluate a rule CONDITION for NODE-ID.
Returns t if condition passes, nil otherwise.
Empty/nil conditions always return t."
  (if (not condition)
      t
    (let ((node-data (supertag-node-get node-id)))
      (unless node-data
        (message "Automation Warning: Node %s not found" node-id)
        (setq node-data (list :id node-id :tags nil :properties nil)))
      
      ;; Handle both quoted and unquoted conditions
      (let ((cond-to-eval (if (and (consp condition)
                                   (eq (car condition) 'quote))
                              (cadr condition)
                            condition)))
        (if (not (consp cond-to-eval))
            (progn
              (message "Automation Warning: Condition is not a list: %S" cond-to-eval)
              nil)
          (supertag-automation--eval-single-condition cond-to-eval node-data))))))

(defun supertag-automation--handle-field-change (node-id old-props new-props)
  "Handle field changes and trigger sync/rollup."
  (when (and old-props new-props)
    ;; Find changed fields (stored as properties with : prefix)
    (let ((changed-fields '()))
      (cl-loop for (key value) on new-props by #'cddr do
               (let ((old-value (plist-get old-props key)))
                 (unless (equal old-value value)
                   ;; Only process field changes (properties with : prefix)
                   (when (and (keywordp key) (string-prefix-p ":" (symbol-name key)))
                     (let ((field-name (substring (symbol-name key) 1)))
                       (push (list field-name old-value value) changed-fields))))))
      
      ;; Process each changed field
      (dolist (change changed-fields)
        (let ((field-name (car change))
              (new-value (caddr change)))
          ;; Trigger field synchronization
          (supertag-automation-sync-all-relations node-id field-name new-value))))))

;;; --- Batch Operations ---

(defun supertag-automation-recalculate-all-rollups ()
  "Recalculate all rollup values in the system.
This is useful for data consistency maintenance."
  (interactive)
  (let ((relations (supertag-store-get-collection :relations))
        (count 0))
    
    (maphash (lambda (id relation)
               (when (plist-get relation :rollup-property)
                 (supertag-automation-calculate-rollup id nil t)
                 (cl-incf count)))
             relations)
    
    (message "Recalculated %d rollup values" count)
    count))

(defun supertag-automation-sync-all-fields ()
  "Sync all field synchronization relations.
This ensures all related entities have consistent field values."
  (interactive)
  (let ((relations (supertag-store-get-collection :relations))
        (count 0))
    
    (maphash (lambda (id relation)
               (when (plist-get relation :sync-fields)
                 (when (fboundp 'supertag-relation-sync-fields)
                   (supertag-relation-sync-fields id))
                 (cl-incf count)))
             relations)
    
    (message "Synced %d field synchronization relations" count)
    count))

;;; --- Utility Functions ---

(defun supertag-automation-log-execution (automation-id node-id action-type)
  "Log automation execution for debugging and monitoring."
  (message "Automation executed: %s on node %s (action: %s)"
           automation-id node-id action-type))

;;; --- System Integration ---

(defun supertag-automation-init ()
  "Initialize the unified automation system.
Sets up rule indexing and event handlers for the automation engine."
  (interactive)
  
  ;; Clear all state
  (clrhash supertag--rule-index)
  (clrhash supertag-automation--sync-cache)
  (clrhash supertag-automation--active-calculations)
  (setq supertag-automation--processing-queue nil)
  
  ;; Build the rule index for the first time
  (supertag-rebuild-rule-index)

  ;; Register all scheduled rules with scheduler (user may start scheduler separately)
  (supertag-automation--register-all-scheduled)
  
  ;; Subscribe to entity changes
  (when (fboundp 'supertag-subscribe)
    (supertag-subscribe :store-changed #'supertag-automation--handle-entity-change))
  
  (setq supertag-automation--enabled t)
  (message "Unified automation system initialized with %d rules indexed."
           (hash-table-count supertag--rule-index)))

(defun supertag-automation-cleanup ()
  "Cleanup the unified automation system."
  (interactive)
  (setq supertag-automation--enabled nil)
  (clrhash supertag--rule-index)
  (clrhash supertag-automation--sync-cache)
  (clrhash supertag-automation--active-calculations)
  (setq supertag-automation--processing-queue nil)
  (message "Unified automation system cleaned up"))

;;; --- Auto-initialization ---

;; Initialize the automation system when loaded
(supertag-automation-init)

(provide 'supertag-automation)

;;; supertag-automation.el ends here
    

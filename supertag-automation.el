
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
(require 'supertag-ops-relation)   ; For relation operations
(require 'supertag-ops-tag)        ; For tag operations
(require 'supertag-ops-node)       ; For node operations
(require 'org-id)                  ; For ID generation
(require 'ht)                      ; For hash table operations

;;; --- Core State Management ---

(defvar supertag-automation--enabled t
  "Global flag to enable/disable automation execution.")

(defvar supertag-automation--processing-queue nil
  "Queue for automation tasks to prevent infinite loops.")

(defvar supertag-automation--active-calculations (make-hash-table :test 'equal)
  "Hash table tracking active rollup calculations to prevent recursion.")

(defvar supertag-automation--sync-cache (make-hash-table :test 'equal)
  "Cache for field sync operations to improve performance.")

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
                       ('field-equals (push (cadr form) sources))
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
         (node-data (when (eq entity-type :nodes)
                     (supertag-get `(:nodes ,entity-id))))
         (node-tags (when node-data (plist-get node-data :tags)))
         (changed-prop (when (and (>= (length path) 4)
                                  (eq (caddr path) :properties))
                         (cadddr path)))
         (candidate-rules '()))

    ;; 1. Collect rules based on the specific property that changed
    (when changed-prop
      (when-let ((rules (gethash changed-prop supertag--rule-index)))
        (setq candidate-rules (append rules candidate-rules))))

    ;; 2. Collect rules based on the tags of the affected node
    (when node-tags
      (dolist (tag-name node-tags)
        (when-let ((rules (gethash tag-name supertag--rule-index)))
          (setq candidate-rules (append rules candidate-rules)))))

    ;; 3. Return a unique list of rule IDs
    (cl-remove-duplicates candidate-rules :test 'equal)))

;;; --- Data Validation ---

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
                                    :trigger (plist-get automation-data :trigger)
                                    :condition (plist-get automation-data :condition)
                                    :actions (plist-get automation-data :actions)
                                    :enabled (if (plist-member automation-data :enabled)
                                               (plist-get automation-data :enabled)
                                             t)
                                    :created-at (current-time)
                                    :modified-at (current-time))))
        
        ;; Store automation
        (supertag-store-direct-set :automations id automation-plist)

        ;; Add to the rule index
        (supertag--add-rule-to-index automation-plist)
        
        ;; Notify system
        (when (fboundp 'supertag-notify)
          (supertag-notify :automation-created :automation-id id :data automation-plist))
        
        (message "Created automation: %s (ID: %s)" name id)
        automation-plist))))

(defun supertag-automation-get (id)
  "Get automation by ID."
  (supertag-get (list :automations id)))

(defun supertag-automation-update (id updater)
  "Update automation data.
ID is the automation identifier.
UPDATER is a function that receives current data and returns updated data."
  (let ((automation (supertag-automation-get id)))
    (when automation
      (let ((updated-automation (funcall updater automation)))
        (when updated-automation
          ;; Update indexes
          (supertag--remove-rule-from-index automation)
          (supertag--add-rule-to-index updated-automation)
          (let ((final-automation (plist-put updated-automation :modified-at (current-time))))
            (supertag--validate-automation-data final-automation)
            (supertag-store-direct-set :automations id final-automation)
            (when (fboundp 'supertag-notify)
              (supertag-notify :automation-updated :automation-id id :data final-automation))
            final-automation))))))

(defun supertag-automation-delete (id)
  "Delete automation by ID.
Returns the deleted automation data or nil if not found."
  (supertag-with-transaction
    (let ((automation (supertag-automation-get id)))
      (when automation
        ;; Remove from the rule index
        (supertag--remove-rule-from-index automation)
        ;; Delete from storage
        (supertag-delete (list :automations id))
        ;; Notify system
        (when (fboundp 'supertag-notify)
          (supertag-notify :automation-deleted :automation-id id :data automation))
        (message "Deleted automation: %s" (plist-get automation :name))
        automation))))

(defun supertag-automation-list (&optional filter)
  "List all automations with optional FILTER.
FILTER can be a function that receives automation data and returns t/nil."
  (let ((automations (supertag-get (list :automations)))
        (result '()))
    (when automations
      (maphash
       (lambda (id automation)
         (when (or (null filter) (funcall filter automation))
           (push automation result)))
       automations))
    (sort result (lambda (a b) (string< (plist-get a :name) (plist-get b :name))))))

(defun supertag-automation-get-by-name (name)
  "Get automation by name.
Returns automation data or nil if not found."
  (let ((id (format "auto-%s" (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" name))))
    (supertag-automation-get id)))

;;; --- Query Functions ---

(defun supertag-automation-find-by-trigger (trigger-type)
  "Find all automations with specific trigger type."
  (supertag-automation-list (lambda (automation)
                              (equal (plist-get automation :trigger) trigger-type))))

(defun supertag-automation-find-enabled ()
  "Find all enabled automations."
  (supertag-automation-list (lambda (automation)
                              (plist-get automation :enabled))))

(defun supertag-automation-find-scheduled ()
  "Find all scheduled automations."
  (supertag-automation-list (lambda (automation)
                              (eq (plist-get automation :trigger) :on-schedule))))

;;; --- Rule Execution Engine ---

(defun supertag-rule-get (id)
  "Get any rule by ID. Unified interface for rule retrieval."
  (supertag-automation-get id))

(defun supertag-rule-execute (rule node-id context)
  "Execute a rule with given context.
This is called by the automation engine when a rule matches an event."
  (when (and rule (plist-get rule :enabled))
    (let ((actions (plist-get rule :actions))
          (rule-id (plist-get rule :id)))
      (message "Executing rule %s on node %s" rule-id node-id)
      (dolist (action-spec actions)
        (let ((action-type (plist-get action-spec :action))
              (params (plist-get action-spec :params)))
          (supertag-automation-execute-action action-type node-id params context)))
      (supertag-automation-log-execution rule-id node-id (plist-get (car actions) :action)))))

(defun supertag-automation-execute-action (action-type node-id params context)
  "Execute a specific action type.
ACTION-TYPE is the action to perform
NODE-ID is the target node
PARAMS are action parameters
CONTEXT provides execution context"
  (pcase action-type
    (:update-property
     (supertag-automation-action-update-property node-id params))
    
    (:call-function
     (supertag-automation-action-call-function node-id params context))
    
    (_
     (message "Unknown action type: %s" action-type))))

(defun supertag-automation-action-update-property (node-id params)
  "Update an Org-mode property on the node.
This updates native Org properties like :SCHEDULED:, :DEADLINE:, etc."
  (let ((property (plist-get params :property))
        (value (plist-get params :value)))
    (when (and property value)
      (supertag-update (list :nodes node-id)
                       (lambda (node)
                         (let ((props (or (plist-get node :properties) '())))
                           (plist-put node :properties
                                     (plist-put props property value))))))))

(defun supertag-automation-action-call-function (node-id params context)
  "Call a custom function with node context."
  (let ((function (plist-get params :function))
        (args (plist-get params :args)))
    (when (functionp function)
      (apply function node-id context args))))

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
        (cond
         ;; Target is a node
         ((supertag-get (list :nodes to-id))
          (supertag-automation--sync-to-node to-id field-name value))
         
         ;; Target is a tag/database
         ((supertag-get (list :tags to-id))
          (supertag-automation--sync-to-tag to-id field-name value))
         
         (t
          (message "Warning: Unknown target entity type for %s" to-id)))
        
        (message "Synced property %s=%s from %s to %s"
                 field-name value from-id to-id)))))

(defun supertag-automation--sync-to-node (node-id field-name value)
  "Sync field to a node entity via properties."
  (supertag-update (list :nodes node-id)
                   (lambda (node)
                     (let ((props (or (plist-get node :properties) '())))
                       (plist-put node :properties
                                 (plist-put props (intern (concat ":" field-name)) value))))))

(defun supertag-automation--sync-to-tag (tag-id field-name value)
  "Sync field to a tag entity."
  (when (fboundp 'supertag-tag-update)
    (supertag-tag-update tag-id
                         (lambda (tag)
                           (plist-put tag (intern (concat ":" field-name)) value)))))

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
      (let* ((node-data (supertag-get `(:nodes ,(plist-get entity-info :id))))
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
  (let* ((entity (or (supertag-get (list :nodes entity-id))
                    (supertag-get (list :tags entity-id))))
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

;;; --- Event Integration ---

(defun supertag-automation--handle-entity-change (path old-value new-value)
  "Handle entity changes and trigger automation.
This integrates with the event notification system."
  (when (and supertag-automation--enabled
             (listp path) (>= (length path) 2))
    (let ((entity-type (car path))
          (entity-id (cadr path)))
      
      (cond
       ;; Node changes
       ((eq entity-type :nodes)
        (supertag-automation--handle-node-change path old-value new-value))
       
       ;; Tag changes
       ((eq entity-type :tags)
        ;; Placeholder for future tag change handling
        nil)))))

(defun supertag-automation--handle-node-change (path old-value new-value)
  "Handle node changes and trigger relevant automation."
  (let ((rule-ids (supertag--get-rules-from-index path)))
    (dolist (rule-id rule-ids)
      (when-let ((rule (supertag-rule-get rule-id)))
        ;; Execute the rule after condition check
        (message "INDEX-MATCH: Event %S triggered rule %S" path rule-id)
        (supertag-rule-execute rule (cadr path) (list :path path :old old-value :new new-value))))))

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
  (let ((relations (supertag-get (list :relations)))
        (count 0))
    
    (when (hash-table-p relations)
      (maphash (lambda (id relation)
                 (when (plist-get relation :rollup-property)
                   (supertag-automation-calculate-rollup id nil t)
                   (cl-incf count)))
               relations))
    
    (message "Recalculated %d rollup values" count)
    count))

(defun supertag-automation-sync-all-fields ()
  "Sync all field synchronization relations.
This ensures all related entities have consistent field values."
  (interactive)
  (let ((relations (supertag-get (list :relations)))
        (count 0))
    
    (when (hash-table-p relations)
      (maphash (lambda (id relation)
                 (when (plist-get relation :sync-fields)
                   (when (fboundp 'supertag-relation-sync-fields)
                     (supertag-relation-sync-fields id))
                   (cl-incf count)))
               relations))
    
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
    
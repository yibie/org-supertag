;;; org-supertag/automation/sync.el --- Synchronous Event Processing for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file implements synchronous event processing for the automation system.
;; It replaces the asynchronous queue-based approach with direct synchronous execution.
;;
;; Key improvements:
;; 1. Eliminates queue/timer indirection
;; 2. Provides immediate automation execution
;; 3. Maintains performance through smart caching and recursion protection
;; 4. Integrates with the new supertag-ops-commit system

;;; Code:

(require 'cl-lib)
(require 'supertag-automation)     ; For existing automation functions
(require 'supertag-ops-node)
(require 'supertag-ops-tag)

(defvar supertag-debug-log-field-events nil)

;;; Customization

(defgroup supertag-automation-sync nil
  "Synchronous automation execution settings for Org-Supertag."
  :group 'supertag)

(defcustom supertag-automation-sync-use-commit-hooks nil
  "When non-nil, run automation via `supertag-after-operation-hook'.

By default this is nil, because the automation system already subscribes to
`:store-changed' events emitted by the unified commit pipeline, as well as
some legacy direct store updates (e.g. global field value updates) that do not
go through `supertag-ops-commit'."
  :type 'boolean
  :group 'supertag-automation-sync)

;;; --- Synchronous Event Processing State ---

(defvar supertag-automation-sync--processing-stack nil
  "Stack of currently processing node IDs to prevent infinite loops.")

(defvar supertag-automation-sync--enabled t
  "Enable/disable synchronous automation processing.")

(defvar supertag-automation-sync--async-enabled t
  "Enable async fallback for large installations.")

(defvar supertag-automation-sync--batch-size 50
  "Maximum number of operations to process before forcing async fallback.")

;;; --- Main Synchronous Event Handler ---

(defun supertag-automation-sync-handle-event (operation collection id payload previous &rest metadata)
  "Handle store events synchronously using the new commit system.
This function replaces the old queue-based event processing.

OPERATION is the operation type (:create, :update, :delete).
COLLECTION is the target collection (:nodes, :tags, :relations, :fields,
  :field-values, :field-definitions, :tag-field-associations).
ID is the entity identifier.
PAYLOAD is the operation data.
PREVIOUS is the previous entity value.
METADATA is additional operation context."

  (when supertag-automation-sync--enabled
    (pcase collection
      (:nodes
       (supertag-automation-sync--handle-node-event operation id payload previous metadata))
      (:fields
       (supertag-automation-sync--handle-field-event operation id payload previous metadata))
      ;; Global field value changes (new)
      (:field-values
       (supertag-automation-sync--handle-global-field-value-event operation id payload previous metadata))
      ;; Global field definition changes (new)
      (:field-definitions
       (supertag-automation-sync--handle-global-field-def-event operation id payload previous metadata))
      ;; Tag-field association changes (new)
      (:tag-field-associations
       (supertag-automation-sync--handle-association-event operation id payload previous metadata))
      (:tags
       (supertag-automation-sync--handle-tag-event operation id payload previous metadata))
      (:relations
       (supertag-automation-sync--handle-relation-event operation id payload previous metadata)))))

(defun supertag-automation-sync--handle-node-event (operation id payload previous metadata)
  "Handle node-related events synchronously."
  (when id
    (if (supertag-automation-sync--should-process-async-p)
        ;; Fallback to async processing for large operations
        (supertag-automation-sync--queue-async-handler
         (lambda () (supertag-automation-sync--handle-node-event operation id payload previous metadata)))

      ;; Process synchronously
      (supertag-automation-sync--with-protection id
        (lambda ()
          (pcase operation
            (:update
             (supertag-automation-sync--process-node-change id previous payload))
            (:create
             (supertag-automation-sync--process-node-creation id payload))
            (:delete
             (supertag-automation-sync--process-node-deletion id previous))
            (_ nil)))))))

(defun supertag-automation-sync--handle-field-event (operation id payload previous metadata)
  "Handle legacy field-related events synchronously.
This handles the old nested field storage format (:fields node-id tag-id field-name)."
  (when (and (plist-member payload :node-id) (plist-member payload :tag-id) (plist-member payload :field-name))
    (let ((node-id (plist-get payload :node-id))
          (tag-id (plist-get payload :tag-id))
          (field-name (plist-get payload :field-name))
          (new-value (plist-get payload :value))
          (old-value (when previous (plist-get previous :value))))

      (when (and node-id field-name)
        (supertag-automation-sync--with-protection node-id
          (lambda ()
            (when supertag-debug-log-field-events
              (message "supertag-automation-sync EVENT (legacy) %s node=%s tag=%s field=%s old=%S new=%S"
                       operation node-id tag-id field-name old-value new-value))
            ;; 1. Trigger field synchronization for relations
            (supertag-automation-sync-all-relations node-id field-name new-value)
            
            ;; 2. Trigger automation rules for field changes
            (supertag-automation-sync--process-field-change node-id tag-id field-name old-value new-value)))))))

(defun supertag-automation-sync--handle-global-field-value-event (operation id payload previous metadata)
  "Handle global field value change events synchronously.
ID is the node-id, PAYLOAD contains the new value, PREVIOUS contains the old value.
The field-id is extracted from the event path in metadata."
  (let* ((path (plist-get (car metadata) :path))
         (node-id (if (and path (>= (length path) 2)) (cadr path) id))
         (field-id (if (and path (>= (length path) 3)) (caddr path) nil))
         (new-value payload)
         (old-value previous))
    (when (and node-id field-id)
      (supertag-automation-sync--with-protection node-id
        (lambda ()
          (when supertag-debug-log-field-events
            (message "supertag-automation-sync EVENT (global) %s node=%s field=%s old=%S new=%S"
                     operation node-id field-id old-value new-value))
          ;; 1. Trigger field synchronization for relations
          (supertag-automation-sync-all-relations node-id field-id new-value)
          
          ;; 2. Trigger automation rules for global field changes
          (supertag-automation-sync--process-global-field-change node-id field-id old-value new-value))))))

(defun supertag-automation-sync--handle-global-field-def-event (operation id payload previous metadata)
  "Handle global field definition change events.
ID is the field-id. This is mainly for cache invalidation and schema updates."
  (when supertag-debug-log-field-events
    (message "supertag-automation-sync: field definition %s for %s" operation id))
  ;; Field definition changes don't typically trigger automation rules,
  ;; but we could add support for :on-field-defined triggers in the future
  nil)

(defun supertag-automation-sync--handle-association-event (operation id payload previous metadata)
  "Handle tag-field association change events.
ID is the tag-id. This is mainly for cache invalidation."
  (when supertag-debug-log-field-events
    (message "supertag-automation-sync: association %s for tag %s" operation id))
  ;; Association changes don't typically trigger automation rules,
  ;; but we could add support for :on-field-associated triggers in the future
  nil)

(defun supertag-automation-sync--handle-tag-event (operation id payload previous metadata)
  "Handle tag-related events synchronously."
  (when id
    (let ((node-ids (supertag-automation-sync--get-affected-node-ids id operation previous)))
      (dolist (node-id node-ids)
        (supertag-automation-sync--with-protection node-id
          (lambda ()
            (supertag-automation-sync--process-tag-change node-id operation id)))))))

(defun supertag-automation-sync--handle-relation-event (operation id payload previous metadata)
  "Handle relation-related events synchronously."
  (when id
    (let* ((relation (or payload previous))
           (sync-fields (plist-get relation :sync-fields)))
      (when sync-fields
        (supertag-automation-sync--process-relation-sync relation)))))

;;; --- Node Change Processing ---

(defun supertag-automation-sync--normalize-tag-list (value)
  "Normalize VALUE into a tag list."
  (cond
   ((null value) nil)
   ((listp value) value)
   (t (list value))))

(defun supertag-automation-sync--diff-tags (old-tags new-tags)
  "Return (ADDED . REMOVED) between OLD-TAGS and NEW-TAGS."
  (let* ((old (cl-remove-duplicates (copy-sequence (supertag-automation-sync--normalize-tag-list old-tags))
                                    :test 'equal))
         (new (cl-remove-duplicates (copy-sequence (supertag-automation-sync--normalize-tag-list new-tags))
                                    :test 'equal))
         (added (cl-set-difference new old :test 'equal))
         (removed (cl-set-difference old new :test 'equal)))
    (cons added removed)))

(defun supertag-automation-sync--condition-contains-op-p (condition ops)
  "Return non-nil when CONDITION contains any operator in OPS.

OPS is a list of symbols, e.g. '(property-changed field-changed)."
  (when condition
    (let ((targets (if (listp ops) ops (list ops)))
          (found nil))
      (cl-labels ((walk (form)
                        (when (and (not found) (consp form))
                          (pcase (car form)
                            ('quote (walk (cadr form)))
                            (_
                             (when (memq (car form) targets)
                               (setq found t))
                             (dolist (sub (cdr form))
                               (walk sub)))))))
        (walk condition))
      found)))

(defun supertag-automation-sync--execute-rule-for-event (rule node-id event)
  "Execute RULE for NODE-ID under EVENT when trigger/condition pass."
  (let* ((trigger (plist-get rule :trigger))
         (condition (plist-get rule :condition))
         (supertag-automation--current-event event))
    (when (and (supertag-automation--trigger-match-p trigger event)
               (supertag-automation--evaluate-condition condition node-id))
      (supertag-rule-execute rule node-id event))))

(defun supertag-automation-sync--execute-tag-trigger (node-id tag-name op)
  "Execute tag-trigger rules for NODE-ID when TAG-NAME changes.
OP must be :added or :removed."
  (when (and (boundp 'supertag--rule-index) tag-name)
    (when-let ((candidate (gethash tag-name supertag--rule-index)))
      (dolist (rule-id (cl-remove-duplicates candidate :test #'equal))
        (when-let ((rule (supertag-automation-get rule-id)))
          (pcase (plist-get rule :trigger)
            (`(:on-tag-added ,tn)
             (when (and (eq op :added) (equal tn tag-name))
               (supertag-automation-sync--execute-rule-for-event
                rule node-id (list :tag-event :added :tag tag-name))))
            (`(:on-tag-removed ,tn)
             (when (and (eq op :removed) (equal tn tag-name))
               (supertag-automation-sync--execute-rule-for-event
                rule node-id (list :tag-event :removed :tag tag-name))))
            (_ nil)))))))

(defun supertag-automation-sync--make-property-event (node-id prop old-val new-val)
  "Build a property-change event for NODE-ID/PROP."
  (list :path (list :nodes node-id :properties prop) :old old-val :new new-val))

(defun supertag-automation-sync--process-node-change (node-id old-node new-node)
  "Process a node change and trigger relevant automation rules."
  (let* ((old-tags (plist-get old-node :tags))
         (new-tags (plist-get new-node :tags))
         (tag-diff (supertag-automation-sync--diff-tags old-tags new-tags))
         (added-tags (car tag-diff))
         (removed-tags (cdr tag-diff))
         (changed-props (supertag-automation-sync--get-changed-properties old-node new-node))
         (rule-ids (supertag-automation-sync--get-relevant-rules node-id old-node new-node)))

    ;; 1) Tag triggers (runtime-gated): (:on-tag-added ...) / (:on-tag-removed ...)
    (dolist (tag added-tags)
      (supertag-automation-sync--execute-tag-trigger node-id tag :added))
    (dolist (tag removed-tags)
      (supertag-automation-sync--execute-tag-trigger node-id tag :removed))

    ;; 2) Property-change driven rules: provide precise :path so
    ;; (property-changed ...) can work deterministically.
    (when changed-props
      (let* ((old-props (plist-get old-node :properties))
             (new-props (plist-get new-node :properties))
             (representative (car changed-props))
             (representative-event
              (supertag-automation-sync--make-property-event
               node-id representative
               (plist-get old-props representative)
               (plist-get new-props representative))))
        (dolist (rule-id rule-ids)
          (when-let ((rule (supertag-automation-get rule-id)))
            ;; Skip tag-only triggers here; they are handled above.
            (let ((trigger (plist-get rule :trigger)))
              (unless (and (consp trigger) (memq (car trigger) '(:on-tag-added :on-tag-removed)))
                (if (supertag-automation-sync--condition-contains-op-p
                     (plist-get rule :condition)
                     '(property-changed field-changed global-field-changed))
                    ;; Change-sensitive rules: evaluate once per changed property.
                    (dolist (prop changed-props)
                      (supertag-automation-sync--execute-rule-for-event
                       rule node-id
                       (supertag-automation-sync--make-property-event
                        node-id prop
                        (plist-get old-props prop)
                        (plist-get new-props prop))))
                  ;; Generic rules: evaluate once per update.
                  (supertag-automation-sync--execute-rule-for-event
                   rule node-id representative-event))))))))

    ;; 3) Non-property changes (e.g., title/metadata) with no tag/property delta:
    ;; fall back to a node-change event.
    (when (and (null changed-props)
               (null added-tags)
               (null removed-tags)
               (not (equal old-node new-node)))
      (let ((node-event (list :path (list :nodes node-id) :old old-node :new new-node)))
        (dolist (rule-id rule-ids)
          (when-let ((rule (supertag-automation-get rule-id)))
            (let ((trigger (plist-get rule :trigger)))
              (unless (and (consp trigger) (memq (car trigger) '(:on-tag-added :on-tag-removed)))
                (supertag-automation-sync--execute-rule-for-event rule node-id node-event)))))))))

(defun supertag-automation-sync--process-node-creation (node-id payload)
  "Process a node creation and trigger relevant automation rules."
  (let* ((node-data (or payload (supertag-node-get node-id)))
         (rule-ids (supertag-automation-sync--get-relevant-rules node-id nil node-data)))
    ;; Treat initial tags as :added events (first time the node is tagged).
    (dolist (tag (supertag-automation-sync--normalize-tag-list (plist-get node-data :tags)))
      (supertag-automation-sync--execute-tag-trigger node-id tag :added))
    (dolist (rule-id rule-ids)
      (when-let ((rule (supertag-automation-get rule-id)))
        ;; Skip tag-only triggers here; they are handled above.
        (let ((trigger (plist-get rule :trigger)))
          (unless (and (consp trigger) (memq (car trigger) '(:on-tag-added :on-tag-removed)))
            (supertag-automation-sync--execute-rule-for-event
             rule node-id (list :path (list :nodes node-id) :old nil :new node-data))))))))

(defun supertag-automation-sync--process-node-deletion (node-id old-node)
  "Process a node deletion and trigger relevant automation rules."
  (let ((rule-ids (supertag-automation-sync--get-relevant-rules node-id old-node nil)))
    (dolist (rule-id rule-ids)
      (when-let ((rule (supertag-automation-get rule-id)))
        ;; Skip tag-only triggers; deletion is not a tag-change event.
        (let ((trigger (plist-get rule :trigger)))
          (unless (and (consp trigger) (memq (car trigger) '(:on-tag-added :on-tag-removed)))
            (supertag-automation-sync--execute-rule-for-event
             rule node-id (list :path (list :nodes node-id) :old old-node :new nil))))))))

(defun supertag-automation-sync--process-tag-change (node-id operation tag-id)
  "Process a tag change on a node and trigger relevant automation rules."
  (let ((op (pcase operation
              ((or :added :add-tag) :added)
              ((or :removed :remove-tag) :removed)
              (_ operation))))
    (when (memq op '(:added :removed))
      (supertag-automation-sync--execute-tag-trigger node-id tag-id op))))

(defun supertag-automation-sync--process-field-change (node-id tag-id field-name old-value new-value)
  "Process a legacy field change and trigger relevant automation rules."
  (let* ((path (list :fields node-id tag-id field-name))
         (rule-ids (supertag--get-rules-from-index path))
         (current-event (list :path path :old old-value :new new-value))
         (supertag-automation--current-event current-event))
    (dolist (rule-id rule-ids)
      (when-let ((rule (supertag-automation-get rule-id)))
        ;; Skip tag-only triggers
        (let ((trigger (plist-get rule :trigger)))
          (unless (and (consp trigger) (memq (car trigger) '(:on-tag-added :on-tag-removed)))
            (when (and (supertag-automation--trigger-match-p trigger current-event)
                       (supertag-automation--evaluate-condition (plist-get rule :condition) node-id))
              (supertag-automation--log "Field change triggered rule %s" rule-id)
              (supertag-rule-execute rule node-id current-event))))))))

(defun supertag-automation-sync--process-global-field-change (node-id field-id old-value new-value)
  "Process a global field change and trigger relevant automation rules."
  (let* ((path (list :field-values node-id field-id))
         (rule-ids (supertag--get-rules-from-index path))
         (current-event (list :path path :old old-value :new new-value))
         (supertag-automation--current-event current-event))
    (dolist (rule-id rule-ids)
      (when-let ((rule (supertag-automation-get rule-id)))
        ;; Skip tag-only triggers
        (let ((trigger (plist-get rule :trigger)))
          (unless (and (consp trigger) (memq (car trigger) '(:on-tag-added :on-tag-removed)))
            (when (and (supertag-automation--trigger-match-p trigger current-event)
                       (supertag-automation--evaluate-condition (plist-get rule :condition) node-id))
              (supertag-automation--log "Global field change triggered rule %s" rule-id)
              (supertag-rule-execute rule node-id current-event))))))))

;;; --- Rule Lookup and Matching ---

(defun supertag-automation-sync--get-relevant-rules (node-id old-node new-node)
  "Get automation rules relevant to a node change.
This is an optimized version of the original rule lookup."
  (let ((rules '())
        (node-data (or new-node old-node (supertag-node-get node-id))))
    (when node-data
      ;; Get rules based on node tags - safe lookup
      (let ((node-tags (plist-get node-data :tags)))
        (when (and node-tags (boundp 'supertag--rule-index))
          (dolist (tag node-tags)
            (when-let ((tag-rules (gethash tag supertag--rule-index)))
              (setq rules (append tag-rules rules))))))

      ;; Get rules based on changed properties - safe lookup
      (when (and old-node new-node (boundp 'supertag--rule-index))
        (let ((changed-props (supertag-automation-sync--get-changed-properties old-node new-node)))
          (dolist (prop changed-props)
            (when-let ((prop-rules (gethash prop supertag--rule-index)))
              (setq rules (append prop-rules rules)))))))

    ;; Remove duplicates and filter by trigger type
    (cl-remove-duplicates rules :test #'equal)))

(defun supertag-automation-sync--get-tag-trigger-rules (tag-id operation)
  "Get rules triggered by tag operations."
  (when (boundp 'supertag--rule-index)
    (let ((candidate-rules (gethash tag-id supertag--rule-index)))
      (when candidate-rules
        (cl-remove-if-not
         (lambda (rule-id)
           (when-let ((rule (supertag-automation-get rule-id)))
             (pcase (plist-get rule :trigger)
               (`(:on-tag-added ,tn) (and (memq operation '(:added :add-tag)) (equal tn tag-id)))
               (`(:on-tag-removed ,tn) (and (memq operation '(:removed :remove-tag)) (equal tn tag-id)))
               (_ nil))))
         candidate-rules)))))

(defun supertag-automation-sync--get-changed-properties (old-node new-node)
  "Get list of changed properties between OLD-NODE and NEW-NODE."
  (let ((changed '())
        (old-props (or (plist-get old-node :properties) '()))
        (new-props (or (plist-get new-node :properties) '())))
    ;; Compare properties
    (cl-loop for (key value) on new-props by #'cddr do
             (unless (equal (plist-get old-props key) value)
               (push key changed)))
    ;; Check for removed properties
    (cl-loop for (key _value) on old-props by #'cddr do
             (unless (plist-member new-props key)
               (push key changed)))
    (cl-remove-duplicates changed :test #'equal)))

(defun supertag-automation-sync--get-affected-node-ids (tag-id operation previous-tags)
  "Get list of node IDs affected by a tag operation."
  (let ((all-nodes '()))
    (maphash (lambda (_id node-data)
               (when (and (plist-get node-data :tags)
                          (member tag-id (plist-get node-data :tags)))
                 (push _id all-nodes)))
             (supertag-store-get-collection :nodes))
    all-nodes))

;;; --- Recursion Protection and Async Fallback ---

(defun supertag-automation-sync--with-protection (node-id thunk)
  "Execute THUNK with recursion protection for NODE-ID."
  (if (cl-member node-id supertag-automation-sync--processing-stack)
      (message "Automation: Skipping recursive processing for node %s" node-id)
    (unwind-protect
        (progn
          (push node-id supertag-automation-sync--processing-stack)
          (funcall thunk))
      (setq supertag-automation-sync--processing-stack
            (cl-delete node-id supertag-automation-sync--processing-stack :test #'equal)))))

(defun supertag-automation-sync--should-process-async-p ()
  "Determine if the current operation should be processed asynchronously."
  (and supertag-automation-sync--async-enabled
       (> (length supertag-automation-sync--processing-stack) supertag-automation-sync--batch-size)))

(defun supertag-automation-sync--queue-async-handler (handler)
  "Queue an async handler for later processing."
  (when supertag-automation-sync--async-enabled
    (push handler supertag-automation--event-queue)
    (unless supertag-automation--processing-timer
      (setq supertag-automation--processing-timer
            (run-at-time 0.001 nil #'supertag-automation-sync--process-async-queue)))))

(defun supertag-automation-sync--process-async-queue ()
  "Process all queued async handlers."
  (setq supertag-automation--processing-timer nil)
  (when supertag-automation--event-queue
    (let ((handlers (nreverse supertag-automation--event-queue)))
      (setq supertag-automation--event-queue nil)
      (dolist (handler handlers)
        (condition-case err
            (funcall handler)
          (error (message "Async automation handler failed: %S" err)))))))

;;; --- Field Synchronization ---

(defun supertag-automation-sync--process-relation-sync (relation)
  "Process field synchronization for a relation."
  (let ((sync-fields (plist-get relation :sync-fields))
        (from-id (plist-get relation :from))
        (to-id (plist-get relation :to)))
    (when (and sync-fields from-id to-id)
      (let ((from-entity (or (supertag-node-get from-id) (supertag-tag-get from-id))))
        (when from-entity
          (dolist (field-name sync-fields)
            (let ((value (supertag-automation-sync--get-field-value from-entity field-name)))
              (when value
                (supertag-automation-sync-field from-id to-id field-name value relation)))))))))

(defun supertag-automation-sync--get-field-value (entity field-name)
  "Get field value from ENTITY by FIELD-NAME.
Supports both global field storage and legacy nested storage."
  (if (plist-get entity :tags)  ; It's a node
      (let ((node-id (plist-get entity :id))
            (tags (plist-get entity :tags)))
        ;; Try global field storage first when enabled
        (if (and (boundp 'supertag-use-global-fields) supertag-use-global-fields)
            (let* ((field-id (if (fboundp 'supertag-sanitize-field-id)
                                 (supertag-sanitize-field-id field-name)
                               field-name))
                   (value (when (fboundp 'supertag-node-get-global-field)
                            (supertag-node-get-global-field node-id field-id))))
              (or value
                  ;; Fallback to legacy storage
                  (cl-some (lambda (tag-id)
                             (supertag-field-get node-id tag-id field-name))
                           tags)))
          ;; Legacy mode
          (cl-some (lambda (tag-id)
                     (supertag-field-get node-id tag-id field-name))
                   tags)))
    ;; It's a tag
    (plist-get entity (intern (concat ":" field-name)))))

(defun supertag-automation-sync-field (from-id to-id field-name value relation-config)
  "Sync a field value between two entities synchronously.
Preserves all existing target entity data."
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
          (supertag-automation-sync--update-node-field to-id field-name value))

         ;; Target is a tag/database
         (target-tag
          (supertag-automation-sync--update-tag-field to-id field-name value))

         (t
          (message "Warning: Unknown target entity type for %s" to-id)))))))

(defun supertag-automation-sync--update-node-field (node-id field-name value)
  "Update a field on a node entity synchronously.
Supports both global field storage and legacy property storage."
  ;; Use global field storage when enabled
  (if (and (boundp 'supertag-use-global-fields) supertag-use-global-fields)
      (let ((field-id (if (fboundp 'supertag-sanitize-field-id)
                          (supertag-sanitize-field-id field-name)
                        field-name)))
        (when (fboundp 'supertag-node-set-global-field)
          (supertag-node-set-global-field node-id field-id value)))
    ;; Legacy mode: update via properties
    (let ((field-key (intern (concat ":" field-name))))
      (supertag-node-update
       node-id
       (lambda (node)
         (when node
           (let* ((props (copy-tree (or (plist-get node :properties) '())))
                  (current (plist-get props field-key)))
             (if (equal current value)
                 node  ; Return unchanged node to preserve data
               ;; Create new node copy with updated field while preserving everything else
               (let ((updated-node (copy-tree node)))
                 (plist-put updated-node :properties (plist-put props field-key value)))))))))))

(defun supertag-automation-sync--update-tag-field (tag-id field-name value)
  "Update a field on a tag entity synchronously."
  (let ((field-key (intern (concat ":" field-name))))
    (supertag-tag-update
     tag-id
     (lambda (tag)
       (when tag
         (let ((current (plist-get tag field-key)))
           (if (equal current value)
               tag
             (plist-put tag field-key value))))))))

;;; --- Integration with Commit System ---

(defun supertag-automation-sync--register-commit-hooks ()
  "Register automation handlers with the commit system."
  (interactive)
  (add-hook 'supertag-after-operation-hook #'supertag-automation-sync--handle-commit-result))

(defun supertag-automation-sync--unregister-commit-hooks ()
  "Unregister automation handlers from the commit system."
  (interactive)
  (remove-hook 'supertag-after-operation-hook #'supertag-automation-sync--handle-commit-result)
  (message "Automation sync handlers unregistered from commit system"))

(defun supertag-automation-sync--handle-commit-result (event)
  "Handle the result of a commit operation.
EVENT is the plist provided by `supertag-ops-commit` after hooks."
  (when (plist-get event :changed)
    (let* ((operation (plist-get event :operation))
           (collection (plist-get event :collection))
           (id (plist-get event :id))
           (current (plist-get event :current))
           (previous (plist-get event :previous))
           (path (plist-get event :path))
           (context (plist-get event :context))
           (context-args (cond
                          ((null context) nil)
                          ((listp context) context)
                          (t (list context))))
           ;; Always pass the canonical path as metadata so handlers
           ;; (notably global field value handlers) can recover the
           ;; full location of the change.
           (meta-args (append (when path (list (list :path path)))
                              context-args)))
      (apply #'supertag-automation-sync-handle-event
             (append (list operation collection id current previous) meta-args)))))

;;; --- Configuration and Utilities ---

(defun supertag-automation-sync-enable ()
  "Enable synchronous automation processing."
  (interactive)
  (setq supertag-automation-sync--enabled t)
  (supertag-automation-sync--register-commit-hooks)
  (message "Synchronous automation processing enabled"))

(defun supertag-automation-sync-disable ()
  "Disable synchronous automation processing."
  (interactive)
  (setq supertag-automation-sync--enabled nil)
  (supertag-automation-sync--unregister-commit-hooks)
  (message "Synchronous automation processing disabled"))

(defun supertag-automation-sync-toggle-async ()
  "Toggle async fallback for large operations."
  (interactive)
  (setq supertag-automation-sync--async-enabled (not supertag-automation-sync--async-enabled))
  (message "Async fallback %s" (if supertag-automation-sync--async-enabled "enabled" "disabled")))

;;; --- Auto-initialization ---

;; Avoid double-triggering automation:
;; - `supertag-core-store.el` emits both :store-changed and `supertag-after-operation-hook`
;; - `supertag-automation.el` already subscribes to :store-changed
;;
;; Keep commit-hook integration opt-in for now.
(if supertag-automation-sync-use-commit-hooks
    (supertag-automation-sync--register-commit-hooks)
  (remove-hook 'supertag-after-operation-hook #'supertag-automation-sync--handle-commit-result))

(provide 'supertag-automation-sync)
;;; supertag-automation-sync.el ends here

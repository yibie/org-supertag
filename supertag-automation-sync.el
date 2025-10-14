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
COLLECTION is the target collection (:nodes, :tags, :relations, :fields).
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
  "Handle field-related events synchronously."
  (when (and (plist-member payload :node-id) (plist-member payload :tag-id) (plist-member payload :field-name))
    (let ((node-id (plist-get payload :node-id))
          (tag-id (plist-get payload :tag-id))
          (field-name (plist-get payload :field-name))
          (new-value (plist-get payload :value))
          (old-value (when previous (plist-get previous :value))))

      (when (and node-id field-name)
        (supertag-automation-sync--with-protection node-id
          (lambda ()
            ;; 1. Trigger field synchronization for relations
            (supertag-automation-sync-all-relations node-id field-name new-value)
            
            ;; 2. Trigger automation rules for field changes
            (supertag-automation-sync--process-field-change node-id tag-id field-name old-value new-value)))))))

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

(defun supertag-automation-sync--process-node-change (node-id old-node new-node)
  "Process a node change and trigger relevant automation rules."
  (let ((rule-ids (supertag-automation-sync--get-relevant-rules node-id old-node new-node)))
    (dolist (rule-id rule-ids)
      (when-let ((rule (supertag-automation-get rule-id)))
        (let* ((condition (plist-get rule :condition))
               (current-event (list :path (list :nodes node-id) :old old-node :new new-node))
               (supertag-automation--current-event current-event))
          (when (supertag-automation--evaluate-condition condition node-id)
            (supertag-rule-execute rule node-id current-event)))))))

(defun supertag-automation-sync--process-node-creation (node-id payload)
  "Process a node creation and trigger relevant automation rules."
  (let* ((node-data (or payload (supertag-node-get node-id)))
         (rule-ids (supertag-automation-sync--get-relevant-rules node-id nil node-data)))
    (dolist (rule-id rule-ids)
      (when-let ((rule (supertag-automation-get rule-id)))
        (let* ((condition (plist-get rule :condition))
               (current-event (list :path (list :nodes node-id) :old nil :new node-data))
               (supertag-automation--current-event current-event))
          (when (supertag-automation--evaluate-condition condition node-id)
            (supertag-rule-execute rule node-id current-event)))))))

(defun supertag-automation-sync--process-node-deletion (node-id old-node)
  "Process a node deletion and trigger relevant automation rules."
  (let ((rule-ids (supertag-automation-sync--get-relevant-rules node-id old-node nil)))
    (dolist (rule-id rule-ids)
      (when-let ((rule (supertag-automation-get rule-id)))
        (let* ((condition (plist-get rule :condition))
               (current-event (list :path (list :nodes node-id) :old old-node :new nil))
               (supertag-automation--current-event current-event))
          (when (supertag-automation--evaluate-condition condition node-id)
            (supertag-rule-execute rule node-id current-event)))))))

(defun supertag-automation-sync--process-tag-change (node-id operation tag-id)
  "Process a tag change on a node and trigger relevant automation rules."
  (let* ((current-event (list :tag-event operation :tag tag-id))
         (rule-ids (supertag-automation-sync--get-tag-trigger-rules tag-id operation))
         (supertag-automation--current-event current-event))
    (dolist (rule-id rule-ids)
      (when-let ((rule (supertag-automation-get rule-id)))
        (when (supertag-automation--evaluate-condition (plist-get rule :condition) node-id)
          (supertag-rule-execute rule node-id current-event))))))

(defun supertag-automation-sync--process-field-change (node-id tag-id field-name old-value new-value)
  "Process a field change and trigger relevant automation rules."
  (let* ((path (list :fields node-id tag-id field-name))
         (rule-ids (supertag--get-rules-from-index path))
         (current-event (list :path path :old old-value :new new-value))
         (supertag-automation--current-event current-event))
    (dolist (rule-id rule-ids)
      (when-let ((rule (supertag-automation-get rule-id)))
        ;; Skip tag-only triggers
        (let ((trigger (plist-get rule :trigger)))
          (unless (and (consp trigger) (memq (car trigger) '(:on-tag-added :on-tag-removed)))
            (when (supertag-automation--evaluate-condition (plist-get rule :condition) node-id)
              (message "Field change triggered rule %s" rule-id)
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
               (`(:on-tag-added ,tn) (and (eq operation :add-tag) (equal tn tag-id)))
               (`(:on-tag-removed ,tn) (and (eq operation :remove-tag) (equal tn tag-id)))
               (_ nil))))
         candidate-rules)))))

(defun supertag-automation-sync--get-changed-properties (old-node new-node)
  "Get list of changed properties between OLD-NODE and NEW-NODE."
  (let ((changed '())
        (old-props (plist-get old-node :properties))
        (new-props (plist-get new-node :properties)))
    (when (and old-props new-props)
      ;; Compare properties
      (cl-loop for (key value) on new-props by #'cddr do
               (unless (equal (plist-get old-props key) value)
                 (push key changed)))
      ;; Check for removed properties
      (cl-loop for (key value) on old-props by #'cddr do
               (unless (plist-member key new-props)
                 (push key changed))))
    changed))

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
  "Get field value from ENTITY by FIELD-NAME."
  (if (plist-get entity :tags)  ; It's a node
      (let ((node-id (plist-get entity :id))
            (tags (plist-get entity :tags)))
        (cl-some (lambda (tag-id)
                   (supertag-field-get node-id tag-id field-name))
                 tags))
    ;; It's a tag
    (plist-get entity (intern (concat ":" field-name)))))

(defun supertag-automation-sync-field (from-id to-id field-name value relation-config)
  "Sync a field value between two entities synchronously."
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
  "Update a field on a node entity synchronously."
  (let ((field-key (intern (concat ":" field-name))))
    (supertag-node-update
     node-id
     (lambda (node)
       (when node
         (let* ((props (copy-tree (or (plist-get node :properties) '())))
                (current (plist-get props field-key)))
           (if (equal current value)
               node
             (plist-put node :properties (plist-put props field-key value)))))))))

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
  (add-hook 'supertag-after-operation-hook #'supertag-automation-sync--handle-commit-result)
  (message "Automation sync handlers registered with commit system"))

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
           (context (plist-get event :context))
           (meta-args (cond
                       ((null context) nil)
                       ((listp context) context)
                       (t (list context)))))
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

;; Register with commit system when loaded
(supertag-automation-sync--register-commit-hooks)

(provide 'supertag-automation-sync)
;;; supertag-automation-sync.el ends here

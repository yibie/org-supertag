;;; org-supertag/ops/relation.el --- Relation operations for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides standardized operations for Relation entities in the
;; Org-Supertag data-centric architecture. All operations leverage
;; the core transform mechanism and adhere to the defined schema.

;;; Code:

(require 'cl-lib)
(require 'supertag-core-store)
(require 'supertag-core-schema)
(require 'supertag-core-transform)
(require 'sha1)

(declare-function supertag-node-update "supertag-ops-node" (id updater))
(declare-function supertag-tag-update "supertag-ops-tag" (id updater))

;;; --- Reference Field Ownership ---

(defconst supertag-reference-default-field-id "refs"
  "Default global field id used to store generic references.")

(defconst supertag-reference-default-field-name "Refs"
  "Default global field name used to store generic references.")

(defconst supertag-reference--missing (list :supertag-reference-missing)
  "Sentinel used to detect missing reference field values.")

(defvar supertag-reference--materializing nil
  "Non-nil while lazily materializing reference fields into relations.")

(defvar supertag-relation--last-error nil
  "Last error produced by `supertag-relation-add-reference`.
Value is a plist:
  :reason  symbol keyword for programmatic branching
  :message human-readable error message
  :detail  optional low-level detail.")

(defun supertag-relation-last-error ()
  "Return the last error payload from `supertag-relation-add-reference`."
  supertag-relation--last-error)

(defun supertag-relation--set-last-error (reason message &optional detail)
  "Store structured relation error with REASON, MESSAGE and DETAIL."
  (setq supertag-relation--last-error
        (list :reason reason :message message :detail detail))
  nil)

(defun supertag-reference--normalize-id-list (value)
  "Normalize VALUE into a list of non-empty string ids."
  (let* ((candidates (cond
                      ((null value) '())
                      ((and (listp value) (not (stringp value))) value)
                      ((stringp value) (list value))
                      (t (list (format "%s" value)))))
         (cleaned (cl-remove-if
                   (lambda (item)
                     (or (null item)
                         (and (stringp item) (= (length item) 0))))
                   (mapcar (lambda (item)
                             (cond
                              ((null item) nil)
                              ((stringp item) item)
                              (t (format "%s" item))))
                           candidates))))
    cleaned))

(defun supertag-reference--normalize-field-ids (fields)
  "Normalize FIELDS into a list of field-id strings."
  (supertag-reference--normalize-id-list fields))

(defun supertag-reference--pack-targets (targets)
  "Pack TARGETS list into stored field form."
  (pcase targets
    ('() nil)
    (`(,single) single)
    (_ targets)))

(defun supertag-reference--effective-fields (relation)
  "Return effective field ids for RELATION, defaulting to Refs."
  (let ((fields (supertag-reference--normalize-field-ids (plist-get relation :fields))))
    (if (null fields)
        (list supertag-reference-default-field-id)
      fields)))

(defun supertag-reference--relation-has-field-p (relation field-id)
  "Return non-nil when RELATION is associated with FIELD-ID."
  (member field-id (supertag-reference--effective-fields relation)))

(defun supertag-reference--collect-targets (from-id field-id)
  "Collect reference targets for FROM-ID filtered by FIELD-ID."
  (let ((relations (supertag-relation-find-by-from from-id :reference)))
    (cl-remove-duplicates
     (cl-loop for rel in relations
              when (supertag-reference--relation-has-field-p rel field-id)
              collect (plist-get rel :to))
     :test #'string=)))

(defun supertag-reference--merge-field-ids (current new-fields)
  "Merge NEW-FIELDS into CURRENT field list, preserving defaults."
  (let* ((base (if (null current)
                   (list supertag-reference-default-field-id)
                 (supertag-reference--normalize-field-ids current)))
         (extra (supertag-reference--normalize-field-ids new-fields)))
    (cl-remove-duplicates (append base extra) :test #'string=)))

(defun supertag-reference--update-field-cache (from-id field-id)
  "Sync FIELD-ID value for FROM-ID from reference relations."
  (when (and supertag-use-global-fields from-id field-id)
    (unless supertag-reference--materializing
      (let* ((raw (supertag-store-get-field-value from-id field-id supertag-reference--missing))
             (legacy (if (eq raw supertag-reference--missing)
                         '()
                       (supertag-reference--normalize-id-list raw)))
             (current (supertag-reference--collect-targets from-id field-id))
             (missing (cl-set-difference legacy current :test #'string=)))
        (when missing
          (let ((supertag-reference--materializing t))
            (dolist (target missing)
              (supertag-relation-create
               (list :type :reference
                     :from from-id
                     :to target
                     :fields (list field-id))))))))
    (let* ((targets (supertag-reference--collect-targets from-id field-id))
           (packed (supertag-reference--pack-targets targets)))
      (supertag-store-put-field-value from-id field-id packed t))))

(defun supertag-reference-get-targets (from-id field-id)
  "Return reference targets for FROM-ID/FIELD-ID, lazily materializing legacy values."
  (when (and from-id field-id)
    (supertag-reference--update-field-cache from-id field-id)
    (supertag-reference--collect-targets from-id field-id)))

(defun supertag-reference-set-targets (from-id field-id targets)
  "Set reference TARGETS for FROM-ID/FIELD-ID via relation updates."
  (let* ((desired (supertag-reference--normalize-id-list targets))
         (current (supertag-reference--collect-targets from-id field-id))
         (removed (cl-set-difference current desired :test #'string=))
         (added (cl-set-difference desired current :test #'string=)))
    ;; Remove field membership (or relation) for removed targets.
    (dolist (target removed)
      (dolist (rel (supertag-relation-find-between from-id target :reference))
        (let* ((rel-fields (supertag-reference--effective-fields rel))
               (new-fields (remove field-id rel-fields)))
          (if (null new-fields)
              (progn
                (supertag-relation-delete (plist-get rel :id))
                (when (fboundp 'supertag-ui--remove-link-under-node)
                  (supertag-ui--remove-link-under-node target from-id)))
            (supertag-relation-update
             (plist-get rel :id)
             (lambda (old) (plist-put (copy-sequence old) :fields new-fields)))))))
    ;; Add references for new targets.
    (dolist (target added)
      (supertag-relation-add-reference from-id target field-id))
    (supertag-reference--update-field-cache from-id field-id)
    desired))

;;; --- Internal Helper ---

;; Deterministic IDs are now the default for optimal data consistency
;; This prevents duplicate relations and ensures predictable behavior

(defun supertag--validate-relation-data (data)
  "Strict validation for relation data. Fails fast on any inconsistency.
Implements immediate error reporting as preferred by the user."
  (unless (plist-get data :type)
    (error "Relation missing required :type field: %S" data))
  (unless (plist-get data :from)
    (error "Relation missing required :from field: %S" data))
  (unless (plist-get data :to)
    (error "Relation missing required :to field: %S" data))
  ;; Validate that from and to are strings
  (unless (stringp (plist-get data :from))
    (error "Relation :from must be a string, got: %S" (plist-get data :from)))
  (unless (stringp (plist-get data :to))
    (error "Relation :to must be a string, got: %S" (plist-get data :to))))

(defun supertag-ops-relation--ensure-plist (data)
  "Return a plist copy of DATA, converting hash tables when necessary."
  (cond
   ((null data) nil)
   ((hash-table-p data)
    (let (plist)
      (maphash (lambda (k v)
                 (setq plist (plist-put plist k v)))
               data)
      plist))
   ((listp data)
    (copy-tree data))
   (t
    (error "Unsupported relation entity format: %S" data))))

(defun supertag-ops-relation--normalize-keyword (name)
  "Normalize NAME into a keyword symbol."
  (cond
   ((keywordp name) name)
   ((symbolp name) (intern (concat ":" (symbol-name name))))
   ((stringp name) (intern (concat ":" name)))
   (t (error "Unsupported property key: %S" name))))

(defun supertag-generate-relation-id (from-id to-id type)
  "Generate a deterministic relation ID based on FROM-ID, TO-ID, and TYPE.
Uses SHA1 hash for consistent ID generation, preventing duplicates."
  (format "rel-%s" (secure-hash 'sha1 (format "%s|%s|%s" from-id to-id type))))

(defun supertag--relation-update-node-references (from-id to-id operation)
  "Private helper to update reference properties on nodes.
OPERATION can be 'add or 'remove."
  (let ((from-node (supertag-store-get-entity :nodes from-id))
        (to-node (supertag-store-get-entity :nodes to-id)))
    (when (and from-node to-node)
      ;; Update the 'from' node's :ref-to list
      (let* ((ref-to-list (or (plist-get from-node :ref-to) '()))
             (new-ref-to (if (eq operation 'add)
                             (if (member to-id ref-to-list) ref-to-list (cons to-id ref-to-list))
                           (remove to-id ref-to-list))))
        (supertag-store-put-entity :nodes from-id
                                   (plist-put (copy-sequence from-node) :ref-to new-ref-to)))

      ;; Update the 'to' node's :ref-from and :ref-count
      (let* ((ref-from-list (or (plist-get to-node :ref-from) '()))
             (new-ref-from (if (eq operation 'add)
                               (if (member from-id ref-from-list) ref-from-list (cons from-id ref-from-list))
                             (remove from-id ref-from-list)))
             (new-ref-count (length new-ref-from)))
        (let* ((p-node (plist-put (copy-sequence to-node) :ref-from new-ref-from))
               (p-node (plist-put p-node :ref-count new-ref-count)))
          (supertag-store-put-entity :nodes to-id p-node))))))


;;; --- Relation Operations ---

;; 5.1 Basic Operations

(defun supertag-relation-create (relation-data)
  "Create a new relation using the unified commit system.
RELATION-DATA is a plist of relation properties.
Returns the created relation data."
  (let* ((data (supertag-ops-relation--ensure-plist relation-data))
         (type (plist-get data :type))
         (from (plist-get data :from))
         (to   (plist-get data :to))
         (rel-id (supertag-generate-relation-id from to type))
         (relation-plist (plist-put data :id rel-id)))

    ;; Ensure created-at exists but don't overwrite if caller provided it.
    (unless (plist-get relation-plist :created-at)
      (setq relation-plist (plist-put relation-plist :created-at (current-time))))

    ;; Strict validation
    (supertag--validate-relation-data relation-plist)

    ;; Check if relation already exists
    (let* ((existing-relations (supertag-relation-find-between from to type))
           ;; Be defensive against malformed/nil entries in relation buckets.
           (existing-relation (cl-find-if #'identity existing-relations)))
      (if existing-relation
          ;; Return the first valid existing relation.
          existing-relation
        ;; Create new relation if none exists
        ;; Use unified commit system
        (supertag-ops-commit
         :operation :create
         :collection :relations
         :id rel-id
         :new relation-plist
         :perform (lambda ()
                    (supertag-store-put-entity :relations rel-id relation-plist)
                    (when (eq type :reference)
                      (supertag--relation-update-node-references from to 'add))
                    relation-plist))))))

(defun supertag-relation-get (id)
  "Get relation data.
ID is the unique identifier of the relation.
Returns relation data, or nil if it does not exist."
  (supertag-store-get-entity :relations id))

(defun supertag-relation-update (id updater)
  "Update relation data using the unified commit system.
ID is the unique identifier of the relation.
UPDATER is a function that receives the current relation data and returns the updated data.
Returns the updated relation data."
  (let ((previous (supertag-relation-get id)))
    (when previous
      (supertag-ops-commit
       :operation :update
       :collection :relations
       :id id
       :previous previous
       :perform (lambda ()
                  (let ((updated-relation (funcall updater previous)))
                    (when updated-relation
                      (let ((final-relation (plist-put updated-relation :modified-at (current-time))))
                        (supertag--validate-relation-data final-relation)
                        (supertag-store-put-entity :relations id final-relation)
                        final-relation))))))))

(defun supertag-relation-delete (id)
  "Delete a relation by its ID and update node ref-counts if applicable.
ID is the unique identifier of the relation.
Returns the deleted relation data."
  (let ((previous (supertag-relation-get id)))
    (when previous
      (supertag-ops-commit
       :operation :delete
       :collection :relations
       :id id
       :previous previous
       :perform (lambda ()
                  (supertag-store-remove-entity :relations id)
                  (when (eq (plist-get previous :type) :reference)
                    (let ((from-id (plist-get previous :from))
                          (to-id (plist-get previous :to)))
                      (supertag--relation-update-node-references from-id to-id 'remove)))
                  nil)))))

;; 5.2 Reference Service

(defun supertag-relation-add-reference (from-id to-id)
  "Create a reference from FROM-ID to TO-ID at relation layer.

This involves:
1. Creating the relation in the database.
2. Inserting a reciprocal link in the TO-ID node's file.

Returns t on success, nil on failure."
  (require 'org)
  (require 'org-id)
  (setq supertag-relation--last-error nil)
  (cond
   ((or (not (stringp from-id)) (string-empty-p from-id))
    (supertag-relation--set-last-error :invalid-from
                                       "Failed to add reference: source node ID is missing or invalid."))
   ((or (not (stringp to-id)) (string-empty-p to-id))
    (supertag-relation--set-last-error :invalid-to
                                       "Failed to add reference: target node ID is missing or invalid."))
   ((null (supertag-node-get from-id))
    (supertag-relation--set-last-error :from-node-missing
                                       (format "Failed to add reference: source node %s does not exist in store." from-id)))
   ((null (supertag-node-get to-id))
    (supertag-relation--set-last-error :to-node-missing
                                       (format "Failed to add reference: target node %s does not exist in store." to-id)))
   (t
    (catch 'done
      (condition-case rel-err
          (let* ((existing-before (cl-find-if #'identity
                                              (supertag-relation-find-between from-id to-id :reference)))
                 ;; 1. Create relation in DB
                 (relation-result (supertag-relation-create `(:type :reference :from ,from-id :to ,to-id)))
                 (created-new (null existing-before)))
            (unless relation-result
              (throw 'done
                     (supertag-relation--set-last-error :db-create-failed
                                                        "Failed to add reference: relation creation returned nil.")))
            ;; 2. DB write succeeded, now insert reciprocal link.
            (let* ((from-node (supertag-node-get from-id))
                   (relation-id (plist-get relation-result :id))
                   ;; Prefer a cleaned title, fallback to raw/title/ID
                   (from-title (or (plist-get from-node :title)
                                   (plist-get from-node :raw-value)
                                   from-id))
                   (marker (org-id-find to-id 'marker)))
              (unless (and marker (marker-buffer marker))
                (when (and created-new relation-id)
                  (ignore-errors (supertag-relation-delete relation-id)))
                (throw 'done
                       (supertag-relation--set-last-error
                        :backlink-target-unresolved
                        (format "Failed to add reference: cannot locate target node %s in Org buffers." to-id))))
              (condition-case backlink-err
                  (with-current-buffer (marker-buffer marker)
                    (org-with-wide-buffer
                      (save-excursion
                        (goto-char marker)
                        ;; Ensure we are on the heading for TO-ID
                        (org-back-to-heading t)
                        (unless (org-at-heading-p)
                          (error "Target marker is not on an Org heading"))
                        ;; Skip metadata and go to end of own content (before children)
                        (org-end-of-meta-data t)
                        (let* ((content-start (point))
                               (content-end (save-excursion
                                              (if (re-search-forward org-outline-regexp nil t)
                                                  (match-beginning 0)
                                                (org-end-of-subtree t t)
                                                (point))))
                               (pattern (format "\\[\\[id:%s\\]" (regexp-quote from-id))))
                          ;; Avoid duplicate backlinks if one already exists
                          (goto-char content-start)
                          (unless (re-search-forward pattern content-end t)
                            (goto-char content-end)
                            (unless (bolp) (insert "\n"))
                            (insert (format "[[id:%s][%s]]\n" from-id from-title))))
                        ;; Mark as internal modification to prevent sync loop
                        (when (fboundp 'supertag--mark-internal-modification)
                          (supertag--mark-internal-modification (buffer-file-name)))
                        (save-buffer))))
                (error
                 (when (and created-new relation-id)
                   (ignore-errors (supertag-relation-delete relation-id)))
                 (throw 'done
                        (supertag-relation--set-last-error
                         :backlink-insert-failed
                         (format "Failed to add reference: backlink insert failed for %s -> %s." from-id to-id)
                         (error-message-string backlink-err)))))
              ;; 3. Return t for success
              (throw 'done t)))
        (error
         (throw 'done
                (supertag-relation--set-last-error :exception
                                                   (format "Failed to add reference: %s"
                                                           (error-message-string rel-err))
                                                   (error-message-string rel-err)))))))))
;; 5.3 Relation Query Operations

(defun supertag-relation-find-by-from (from-id &optional type)
  "Find all relations originating from a specific entity.
FROM-ID is the unique identifier of the source entity.
TYPE is an optional relation type filter.
Returns a list of relations."
  (let ((relations (supertag-store-get-collection :relations)))
    (when relations
      (let ((result '()))
        (maphash
         (lambda (id relation)
           (when (and relation
                      (equal (plist-get relation :from) from-id)
                      (or (null type) (eq (plist-get relation :type) type)))
             (push relation result)))
         relations)
        result))))

(defun supertag-relation-find-by-to (to-id &optional type)
  "Find all relations targeting a specific entity.
TO-ID is the unique identifier of the target entity.
TYPE is an optional relation type filter.
Returns a list of relations."
  (let ((relations (supertag-store-get-collection :relations)))
    (when relations
      (let ((result '()))
        (maphash
         (lambda (id relation)
           (when (and relation
                      (equal (plist-get relation :to) to-id)
                      (or (null type) (eq (plist-get relation :type) type)))
             (push relation result)))
         relations)
        result))))

(defun supertag-relation-find-between (from-id to-id &optional type)
  "Find all relations connecting two specific entities.
FROM-ID is the unique identifier of the source entity.
TO-ID is the unique identifier of the target entity.
TYPE is an optional relation type filter.
Returns a list of relations."
  (let ((relations (supertag-store-get-collection :relations)))
    (when relations
      (let ((result '()))
        (maphash
         (lambda (id relation)
           (when (and relation
                      (equal (plist-get relation :from) from-id)
                      (equal (plist-get relation :to) to-id)
                      (or (null type) (eq (plist-get relation :type) type)))
             (push relation result)))
         relations)
        result))))

;; 5.3 Relation Cleanup Operations

(defun supertag-relation-cleanup-duplicates ()
  "Clean up duplicate relations in the database.
Keeps the first relation for each unique (from, to, type) combination."
  (interactive)
  (let ((relations (supertag-store-get-collection :relations))
        (relation-groups (make-hash-table :test 'equal))
        (duplicates-found 0)
        (removed-count 0))
    
    ;; Group relations by (from, to, type)
    (when (hash-table-p relations)
      (maphash (lambda (id relation-data)
                 (let* ((from (plist-get relation-data :from))
                        (to (plist-get relation-data :to))
                        (type (plist-get relation-data :type))
                        (key (format "%s|%s|%s" from to type)))
                   (when (and from to type)
                     (let ((existing-group (gethash key relation-groups)))
                       (if existing-group
                           (progn
                             (push (cons id relation-data) existing-group)
                             (puthash key existing-group relation-groups)
                             (cl-incf duplicates-found))
                         (puthash key (list (cons id relation-data)) relation-groups))))))
               relations))
    
    ;; Process duplicate groups
    (maphash (lambda (key relation-list)
               (when (> (length relation-list) 1)
                 (message "Found %d duplicate relations for key '%s'" (length relation-list) key)
                 ;; Keep the first relation, delete the rest
                 (let ((keep-relation (car relation-list))
                       (delete-relations (cdr relation-list)))
                   (message "Keeping relation ID: %s" (car keep-relation))
                   (dolist (dup-relation delete-relations)
                     (message "Deleting duplicate relation ID: %s" (car dup-relation))
                     (supertag-store-remove-entity :relations (car dup-relation))
                     (cl-incf removed-count)))))
             relation-groups)
    
    (message "Duplicate relation cleanup complete. Found %d duplicates, removed %d relations."
             duplicates-found removed-count)
    removed-count))

(defun supertag-relation-delete-for-node (node-id)
  "Delete all relations associated with a specific node.
NODE-ID is the unique identifier of the node.
Returns the number of deleted relations."
  (let ((relations (supertag-store-get-collection :relations))
        (count 0))
    (when relations
      (maphash
       (lambda (id relation)
         (when (or (equal (plist-get relation :from) node-id)
                   (equal (plist-get relation :to) node-id))
           (supertag-relation-delete id) ; Use the enhanced delete function
           (setq count (1+ count))))
       relations))
    count))

(defun supertag-relation-delete-for-tag (tag-id)
  "Delete all relations associated with a specific tag.
TAG-ID is the unique identifier of the tag.
Returns the number of deleted relations."
  (let ((relations (supertag-store-get-collection :relations))
        (count 0))
    (when relations
      (maphash
       (lambda (id relation)
         (when (or (equal (plist-get relation :from) tag-id)
                   (equal (plist-get relation :to) tag-id)
                   ;; For node-field relations, check if tag-id is in props
                   (and (eq (plist-get relation :type) :node-field)
                        (equal (plist-get (plist-get relation :props) :tag-id) tag-id)))
           (supertag-relation-delete id) ; Use the enhanced delete function
           (setq count (1+ count))))
       relations))
    count))

;;; --- Notion-style Relation Operations ---

(defun supertag-relation-create-notion-style (relation-data)
  "Create a Notion-style relation with enhanced properties.
RELATION-DATA should contain:
- :type - Relation type (:one-to-one, :one-to-many, :many-to-many, etc.)
- :from - Source entity ID
- :to - Target entity ID  
- :sync-direction - :unidirectional or :bidirectional
- :sync-fields - List of fields to sync
- :rollup-field - Field name for rollup calculations
- :rollup-function - Function for rollup calculation

Returns the created relation data."
  (supertag-with-transaction
    (let* ((type (plist-get relation-data :type))
           (from (plist-get relation-data :from))
           (to (plist-get relation-data :to))
           (sync-direction (or (plist-get relation-data :sync-direction) :unidirectional))
           (sync-fields (plist-get relation-data :sync-fields))
           (rollup-field (plist-get relation-data :rollup-field))
           (rollup-function (plist-get relation-data :rollup-function)))
      
      ;; Validate Notion-style relation types
      (unless (memq type '(:one-to-one :one-to-many :many-to-many :rollup :formula :sync-field))
        (error "Invalid Notion-style relation type: %s" type))
      
      ;; Create enhanced relation data
      (let ((enhanced-relation
             (list :type type
                   :from from
                   :to to
                   :sync-direction sync-direction
                   :sync-fields sync-fields
                   :rollup-field rollup-field
                   :rollup-function rollup-function
                   :props (plist-get relation-data :props))))
        
        ;; Use existing creation function with enhanced data
        (let ((relation (supertag-relation-create enhanced-relation)))
          
          ;; If bidirectional sync is enabled, create reverse relation
          (when (eq sync-direction :bidirectional)
            (supertag-relation-create
             (list :type type
                   :from to
                   :to from
                   :sync-direction :unidirectional
                   :sync-fields sync-fields
                   :props (list :reverse-of (plist-get relation :id)))))
          
          ;; Trigger initial sync if fields are specified
          (when sync-fields
            (supertag-relation-sync-fields (plist-get relation :id)))
          
          ;; Calculate initial rollup if specified
          (when rollup-field
            (supertag-relation-calculate-rollup (plist-get relation :id)))
          
          relation)))))

(defun supertag-relation-sync-fields (relation-id)
  "Sync fields between related entities based on relation configuration.
RELATION-ID is the identifier of the relation defining the sync rules."
  (let ((relation (supertag-relation-get relation-id)))
    (when relation
      (let* ((from-id (plist-get relation :from))
             (to-id (plist-get relation :to))
             (sync-fields (plist-get relation :sync-fields))
             (from-entity (or (supertag-store-get-entity :nodes from-id)
                              (supertag-store-get-entity :tags from-id)))
             (from-plist (supertag-ops-relation--ensure-plist from-entity))
             (target-node (supertag-store-get-entity :nodes to-id))
             (target-tag (and (not target-node) (supertag-store-get-entity :tags to-id))))
        (when (and from-plist (or target-node target-tag) sync-fields)
          (dolist (prop-name sync-fields)
            (let* ((prop-key (supertag-ops-relation--normalize-keyword prop-name))
                   (prop-value (plist-get from-plist prop-key)))
              (when prop-value
                (if target-node
                    (when (fboundp 'supertag-node-update)
                      (supertag-node-update
                       to-id
                       (lambda (node)
                         (let* ((plist (supertag-ops-relation--ensure-plist node))
                                (current (plist-get plist prop-key)))
                           (if (equal current prop-value)
                               nil
                             (plist-put plist prop-key prop-value))))))
                  (when (and target-tag (fboundp 'supertag-tag-update))
                    (supertag-tag-update
                     to-id
                     (lambda (tag)
                       (let* ((plist (supertag-ops-relation--ensure-plist tag))
                              (current (plist-get plist prop-key)))
                         (if (equal current prop-value)
                             nil
                           (plist-put plist prop-key prop-value)))))))))))
        (message "Synced properties for relation %s: %s" relation-id sync-fields)))))

(defun supertag-relation-calculate-rollup (relation-id)
  "Calculate rollup value for a relation.
RELATION-ID is the identifier of the rollup relation."
  (let ((relation (supertag-relation-get relation-id)))
    (when relation
      (let* ((from-id (plist-get relation :from))
             (to-id (plist-get relation :to))
             (rollup-field (plist-get relation :rollup-field))
             (rollup-function (plist-get relation :rollup-function))
             (related-entities (supertag-relation-find-by-from from-id)))
        
        (when (and rollup-field rollup-function)
          ;; Collect values from related entities
          (let ((values '()))
            (dolist (rel related-entities)
              (let* ((entity-id (plist-get rel :to))
                     (entity (or (supertag-store-get-entity :nodes entity-id)
                                 (supertag-store-get-entity :tags entity-id)))
                     (entity-plist (supertag-ops-relation--ensure-plist entity))
                     (value-key (supertag-ops-relation--normalize-keyword rollup-field))
                     (value (when entity-plist
                              (plist-get entity-plist value-key))))
                (when value
                  (push value values))))
            
            ;; Calculate rollup result
              (let ((result (funcall rollup-function values)))
                ;; Update target entity with rollup result
                (let* ((field-name (cond
                                    ((keywordp rollup-field) (substring (symbol-name rollup-field) 1))
                                    ((symbolp rollup-field) (symbol-name rollup-field))
                                    ((stringp rollup-field) rollup-field)
                                    (t (format "%s" rollup-field))))
                       (rollup-key (intern (concat ":rollup-" field-name))))
                  (if (supertag-store-get-entity :nodes to-id)
                      (when (fboundp 'supertag-node-update)
                        (supertag-node-update
                         to-id
                         (lambda (node)
                           (let* ((plist (supertag-ops-relation--ensure-plist node))
                                  (current (plist-get plist rollup-key)))
                             (if (equal current result)
                                 nil
                               (plist-put plist rollup-key result))))))
                    (when (fboundp 'supertag-tag-update)
                      (supertag-tag-update
                       to-id
                       (lambda (tag)
                         (let* ((plist (supertag-ops-relation--ensure-plist tag))
                                (current (plist-get plist rollup-key)))
                           (if (equal current result)
                               nil
                             (plist-put plist rollup-key result))))))))

                  (message "Calculated rollup for %s: %s = %s" to-id rollup-field result)
                  result)))))))

(defun supertag-relation-define-database-relation (from-tag to-tag relation-config)
  "Define a Notion-style database relation between two tags.
FROM-TAG and TO-TAG are tag IDs representing virtual databases.
RELATION-CONFIG is a plist with:
- :type - Relation type (:one-to-many, :many-to-many, etc.)
- :from-property - Field name in from-tag
- :to-property - Field name in to-tag
- :sync-fields - List of fields to sync
- :rollup-config - Rollup configuration

Returns the created relation."
  (let* ((relation-type (plist-get relation-config :type))
         (from-prop (plist-get relation-config :from-property))
         (to-prop (plist-get relation-config :to-property))
         (sync-props (plist-get relation-config :sync-fields))
         (rollup-config (plist-get relation-config :rollup-config)))
    
    ;; Create the database relation
    (supertag-relation-create-notion-style
     (list :type relation-type
           :from from-tag
           :to to-tag
           :sync-direction :bidirectional
           :sync-fields sync-props
           :rollup-field (plist-get rollup-config :field)
           :rollup-function (plist-get rollup-config :function)
           :props (list :from-property from-prop
                       :to-property to-prop
                       :database-relation t)))))

(defun supertag-relation-get-database-relations (tag-id)
  "Get all database relations for a tag (virtual database).
TAG-ID is the tag identifier.
Returns list of database relations."
  (let ((relations (supertag-store-get-collection :relations))
        (result '()))
    (when relations
      (maphash
       (lambda (id relation)
         (when (and (or (equal (plist-get relation :from) tag-id)
                       (equal (plist-get relation :to) tag-id))
                   (plist-get (plist-get relation :props) :database-relation))
           (push relation result)))
       relations))
    result))

(defun supertag-relation-update-all-rollups ()
  "Update all rollup calculations in the system.
This function finds all rollup relations and recalculates their values."
  (interactive)
  (let ((relations (supertag-store-get-collection :relations))
        (count 0))
    (when relations
      (maphash
       (lambda (id relation)
         (when (eq (plist-get relation :type) :rollup)
           (supertag-relation-calculate-rollup id)
           (cl-incf count)))
       relations))
    (message "Updated %d rollup calculations" count)
    count))

(defun supertag-relation-sync-all-fields ()
  "Sync all field synchronization relations in the system."
  (interactive)
  (let ((relations (supertag-store-get-collection :relations))
        (count 0))
    (when relations
      (maphash
       (lambda (id relation)
         (when (plist-get relation :sync-fields)
           (supertag-relation-sync-fields id)
           (cl-incf count)))
       relations))
    (message "Synced %d field synchronization relations" count)
    count))

(provide 'supertag-ops-relation)

;;; supertag-migrate-tag-ids.el --- Migration script for UUID tag IDs to name-based IDs -*- lexical-binding: t; -*-

;;; Commentary:
;; This script migrates UUID-based tag IDs to name-based IDs in the supertag database.
;; It handles the complete migration process including:
;; 1. Tag entity ID updates
;; 2. Node tag list updates  
;; 3. Node-tag relation updates
;; 4. Data consistency validation

;;; Code:

(require 'supertag-core-store)
(require 'supertag-ops-tag)
(require 'supertag-ops-node)
(require 'supertag-ops-relation)

(defun supertag-migrate--is-uuid-tag-id-p (id)
  "Check if ID is a UUID-based tag ID.
Returns t if ID matches the UUID format used by old tag creation."
  (and (stringp id)
       (string-match-p "^id-[0-9]{8}-[0-9]{6}-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$" id)))

(defun supertag-migrate--get-tag-name-from-uuid-id (uuid-id)
  "Extract tag name from UUID-based tag ID.
For UUID IDs like 'id-20250827-215408-110aec2b-2b16-42b8-a53a-c804c671f038',
we need to get the actual tag name from the tag entity."
  (when-let* ((tag (supertag-tag-get uuid-id))
              (tag-name (plist-get tag :name)))
    tag-name))

(defun supertag-migrate--generate-new-tag-id (tag-name)
  "Generate new tag ID from tag name using the same sanitization logic."
  (supertag-sanitize-tag-name tag-name))

(defun supertag-migrate--find-all-uuid-tags ()
  "Find all UUID-based tags in the database.
Returns alist of (old-uuid-id . tag-data) pairs."
  (let ((uuid-tags '()))
    (maphash (lambda (id tag-data)
               (when (and (eq (plist-get tag-data :type) :tag)
                          (supertag-migrate--is-uuid-tag-id-p id))
                 (push (cons id tag-data) uuid-tags)))
             (supertag-get (list :tags)))
    uuid-tags))

(defun supertag-migrate--update-node-tags-list (node-id old-tag-id new-tag-id)
  "Update a node's tags list by replacing OLD-TAG-ID with NEW-TAG-ID."
  (supertag-node-update node-id
    (lambda (node)
      (let* ((tags (plist-get node :tags))
             (new-tags (mapcar (lambda (tag)
                                 (if (equal tag old-tag-id)
                                     new-tag-id
                                   tag))
                               tags)))
        (plist-put node :tags new-tags)))))

(defun supertag-migrate--update-all-node-tags (old-tag-id new-tag-id)
  "Update all nodes that reference OLD-TAG-ID to use NEW-TAG-ID."
  (let ((nodes (supertag-get (list :nodes))))
    (when (hash-table-p nodes)
      (maphash (lambda (node-id node-data)
                 (when (and (eq (plist-get node-data :type) :node)
                            (member old-tag-id (plist-get node-data :tags)))
                   (supertag-migrate--update-node-tags-list node-id old-tag-id new-tag-id)))
               nodes))))

(defun supertag-migrate--update-node-tag-relations (old-tag-id new-tag-id)
  "Update all node-tag relations from OLD-TAG-ID to NEW-TAG-ID."
  (let ((relations (supertag-relation-find-by-to old-tag-id :node-tag)))
    (dolist (relation relations)
      (let* ((relation-id (plist-get relation :id))
             (from-node-id (plist-get relation :from)))
        ;; Delete old relation
        (supertag-relation-delete relation-id)
        ;; Create new relation
        (supertag-relation-create
         (list :type :node-tag
               :from from-node-id
               :to new-tag-id
               :created-at (plist-get relation :created-at)))))))

(defun supertag-migrate--migrate-single-tag (old-tag-id tag-data)
  "Migrate a single UUID-based tag to name-based ID.
Returns new tag ID if successful, nil otherwise."
  (let* ((tag-name (plist-get tag-data :name))
         (new-tag-id (supertag-migrate--generate-new-tag-id tag-name)))
    
    ;; Check if new tag ID already exists (should not happen)
    (when (supertag-tag-get new-tag-id)
      (error "Cannot migrate tag %s: new ID %s already exists" old-tag-id new-tag-id))
    
    ;; Update tag entity ID
    (let ((updated-tag-data (plist-put tag-data :id new-tag-id)))
      (supertag-store-direct-set :tags new-tag-id updated-tag-data)
      (supertag-update (list :tags old-tag-id) nil))
    
    ;; Update all node tag lists
    (supertag-migrate--update-all-node-tags old-tag-id new-tag-id)
    
    ;; Update all node-tag relations
    (supertag-migrate--update-node-tag-relations old-tag-id new-tag-id)
    
    new-tag-id))

(defun supertag-migrate-tag-ids ()
  "Main function to migrate all UUID-based tag IDs to name-based IDs.
This function performs the complete migration in a transactional manner."
  (interactive)
  
  (message "Starting tag ID migration...")
  
  (let ((uuid-tags (supertag-migrate--find-all-uuid-tags))
        (migrated-count 0)
        (error-count 0))
    
    (if (null uuid-tags)
        (message "No UUID-based tags found to migrate.")
      
      (message "Found %d UUID-based tags to migrate." (length uuid-tags))
      
      ;; Perform migration within a transaction for atomicity
      (supertag-with-transaction
        (dolist (tag-pair uuid-tags)
          (let ((old-tag-id (car tag-pair))
                (tag-data (cdr tag-pair)))
            
            (let ((new-tag-id (supertag-migrate--migrate-single-tag old-tag-id tag-data)))
              (message "Migrated tag: %s -> %s" old-tag-id new-tag-id)
              (cl-incf migrated-count))))
      
      ;; Force save to ensure changes are persisted
      (supertag-save-store)
      
      (message "Migration completed: %d tags migrated, %d errors." migrated-count error-count)
      
      (when (> error-count 0)
        (message "Warning: Some tags failed to migrate. Check the database consistency.")))))

(defun supertag-migrate--validate-migration ()
  "Validate that the migration was successful.
Checks that no UUID-based tags remain and all references are updated."
  (let ((remaining-uuid-tags (supertag-migrate--find-all-uuid-tags))
        (validation-errors '()))
    
    ;; Check for remaining UUID tags
    (when remaining-uuid-tags
      (push (format "Found %d remaining UUID-based tags" (length remaining-uuid-tags)) 
            validation-errors))
    
    ;; Check for orphaned node tag references
    (let ((nodes (supertag-get (list :nodes))))
      (when (hash-table-p nodes)
        (maphash (lambda (node-id node-data)
                   (when (eq (plist-get node-data :type) :node)
                     (dolist (tag-id (plist-get node-data :tags))
                       (when (supertag-migrate--is-uuid-tag-id-p tag-id)
                         (push (format "Node %s has orphaned UUID tag reference: %s" node-id tag-id)
                               validation-errors)))))
                 nodes)))
    
    ;; Check for orphaned relations
    (let ((relations (supertag-get (list :relations))))
      (when (hash-table-p relations)
        (maphash (lambda (relation-id relation-data)
                   (when (and (eq (plist-get relation-data :type) :node-tag)
                              (supertag-migrate--is-uuid-tag-id-p (plist-get relation-data :to)))
                     (push (format "Orphaned node-tag relation: %s -> %s" 
                                   (plist-get relation-data :from) (plist-get relation-data :to))
                           validation-errors)))
                 relations)))
    
    (if validation-errors
        (progn
          (message "Validation failed with %d errors:" (length validation-errors))
          (dolist (error validation-errors)
            (message "  %s" error))
          nil)
      (message "Migration validation passed!")
      t)))

(defun supertag-migrate-tag-ids-with-validation ()
  "Perform migration followed by validation."
  (interactive)
  (supertag-migrate-tag-ids)
  (supertag-migrate--validate-migration))

(provide 'supertag-migrate-tag-ids)
;;; supertag-migrate-tag-ids.el ends here
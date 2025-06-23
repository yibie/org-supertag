;;; fix-node-type-corruption.el --- Fix corrupted node type data

(message "=== FIXING NODE TYPE CORRUPTION ===")

(defun org-supertag-fix-corrupted-node-types ()
  "Fix objects that should be nodes but are marked as tags.
Identifies objects with UUID-like IDs that are incorrectly typed as :tag
and converts them back to :node type."
  (interactive)
  (let ((fixed-count 0)
        (tag-count 0)
        (error-count 0)
        (preserved-tags '())
        (converted-nodes '())
        (failed-conversions '()))
    
    (message "\n1. Analyzing object types...")
    
    ;; First pass: identify and fix corrupted nodes
    (maphash (lambda (id props)
               (let ((type (plist-get props :type)))
                 (cond
                                     ;; Object marked as :tag but has UUID-like ID (should be node)
                   ((and (eq type :tag)
                         (string-match-p "^[0-9A-F]\\{8\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{12\\}$" id))
                    (message "Converting corrupted tag to node: %s" id)
                    
                    ;; Create clean node properties (only keep valid node attributes)
                    (let* ((title (or (plist-get props :name)
                                     (plist-get props :title)
                                     (plist-get props :raw-value)
                                     "Recovered Node"))
                           (raw-value (or (plist-get props :raw-value)
                                         (plist-get props :title)
                                         (plist-get props :name)
                                         title))
                           (created-at (or (plist-get props :created-at)
                                          (current-time)))
                           ;; Build clean node properties from scratch (ensure all required properties)
                           (fixed-props (list :type :node
                                            :id id
                                            :title title
                                            :raw-value raw-value
                                            ;; Required properties for :node type
                                            :file-path nil
                                            :pos 1
                                            :olp nil  ; Required: outline path
                                            :level 1
                                            ;; Optional but common properties
                                            :tags nil
                                            :todo-type nil
                                            :priority nil
                                            :properties nil
                                            :begin 1
                                            :contents-begin nil
                                            :contents-end nil
                                            :created-at created-at
                                            :modified-at (current-time))))
                      
                      ;; Debug: Verify properties before adding
                      (let ((required-props '(:id :title :file-path :pos :olp :level)))
                        (dolist (prop required-props)
                          (unless (plist-member fixed-props prop)
                            (message "Warning: Missing required property %s for node %s" prop id))))
                      
                      ;; Update in database
                      (condition-case err
                          (progn
                            (org-supertag-db-add id fixed-props)
                            (push (cons id fixed-props) converted-nodes)
                            (cl-incf fixed-count)
                            (message "Successfully converted: %s -> %s" id title))
                                                 (error
                          (message "Failed to convert %s: %s" id (error-message-string err))
                          (push (cons id (error-message-string err)) failed-conversions)
                          (cl-incf error-count)))))
                  
                  ;; Real tags (keep as-is)
                  ((eq type :tag)
                   (cl-incf tag-count)
                   (push (cons id props) preserved-tags))
                  
                  ;; Other types (keep as-is)
                  (t
                   nil))))
             org-supertag-db--object)
    
    (message "   Fixed %d corrupted nodes" fixed-count)
    (message "   Preserved %d real tags" tag-count)
    (message "   Failed conversions: %d" error-count)
    
    (when (> error-count 0)
      (message "   Failed conversion details:")
      (dolist (failure (seq-take failed-conversions 5))
        (message "     %s: %s" (car failure) (cdr failure))))
    
    (when converted-nodes
      (message "   Sample converted nodes:")
      (dolist (node (seq-take converted-nodes 5))
        (let ((id (car node))
              (props (cdr node)))
          (message "     %s: %s" id (plist-get props :title)))))
    
    ;; Force database save
    (when (> fixed-count 0)
      (message "\n2. Saving corrected database...")
      (org-supertag-db--mark-dirty)
      (org-supertag-db-save))
    
    (message "\n=== CORRUPTION FIX COMPLETED ===")
    (message "Total objects fixed: %d" fixed-count)
    (message "Failed conversions: %d" error-count)
    (if (= error-count 0)
        (message "✅ All corrupted nodes successfully converted!")
      (message "⚠️  Some conversions failed - check messages above"))
    (message "Database should now have correct node types!")
    
    fixed-count))

;; Execute the fix
(org-supertag-fix-corrupted-node-types)

(message "\n=== POST-FIX VERIFICATION ===")

;; Verify fix results
(let ((node-count 0)
      (tag-count 0)
      (other-count 0))
  (maphash (lambda (id props)
             (let ((type (plist-get props :type)))
               (cond
                ((eq type :node) (cl-incf node-count))
                ((eq type :tag) (cl-incf tag-count))
                (t (cl-incf other-count)))))
           org-supertag-db--object)
  
  (message "Final object count:")
  (message "  Nodes: %d" node-count)
  (message "  Tags: %d" tag-count)
  (message "  Other: %d" other-count)
  (message "  Total: %d" (+ node-count tag-count other-count)))

(message "\nRun org-supertag-background-sync again to test incremental sync!") 
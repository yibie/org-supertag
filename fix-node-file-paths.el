;;; fix-node-file-paths.el --- Fix file paths for recovered nodes

(message "=== ANALYZING NODE FILE PATHS ===")

(defun org-supertag-analyze-node-file-paths ()
  "Analyze file path issues in recovered nodes."
  (interactive)
  (let ((nodes-with-files 0)
        (nodes-without-files 0)
        (orphaned-nodes '())
        (sample-nodes '()))
    
    (message "\n1. Analyzing node file paths...")
    
    ;; Check all nodes
    (maphash (lambda (id props)
               (when (eq (plist-get props :type) :node)
                 (let ((file-path (plist-get props :file-path))
                       (title (plist-get props :title)))
                   (if (and file-path (not (string-empty-p file-path)))
                       (cl-incf nodes-with-files)
                     (cl-incf nodes-without-files)
                     (push (cons id props) orphaned-nodes))
                   
                   ;; Collect sample for analysis
                   (when (< (length sample-nodes) 10)
                     (push (list :id id 
                                :title title
                                :file-path file-path
                                :has-file (if (and file-path (not (string-empty-p file-path))) t nil))
                           sample-nodes)))))
             org-supertag-db--object)
    
    (message "   Nodes with files: %d" nodes-with-files)
    (message "   Nodes without files: %d" nodes-without-files)
    (message "   Sample node analysis:")
    (dolist (node sample-nodes)
      (message "     %s: %s [File: %s]" 
               (plist-get node :id)
               (or (plist-get node :title) "No Title")
               (if (plist-get node :has-file) "✓" "✗")))
    
    ;; Check if orphaned nodes might be recoverable
    (when orphaned-nodes
      (message "\n2. Analyzing orphaned nodes...")
      (message "   Found %d orphaned nodes (no file path)" (length orphaned-nodes))
      
      ;; Check if these might be actual file-based nodes
      (let ((possible-file-nodes 0)
            (probable-tag-nodes 0))
        (dolist (node-pair orphaned-nodes)
          (let* ((props (cdr node-pair))
                 (title (plist-get props :title))
                 (raw-value (plist-get props :raw-value)))
            ;; Heuristic: if title looks like a heading, might be file-based
            (if (and title 
                     (or (string-match-p "\\[.*\\]" title)  ; Date/time format
                         (> (length title) 20)              ; Long descriptive title
                         (string-match-p "[A-Z]" title)))   ; Mixed case
                (cl-incf possible-file-nodes)
              (cl-incf probable-tag-nodes))))
        
        (message "     Possibly from files: %d" possible-file-nodes)
        (message "     Probably tag-like: %d" probable-tag-nodes)))
    
    ;; Return analysis data
    (list :nodes-with-files nodes-with-files
          :nodes-without-files nodes-without-files
          :orphaned-nodes orphaned-nodes
          :sample-nodes sample-nodes)))

(defun org-supertag-fix-orphaned-nodes-strategy ()
  "Provide strategies to handle orphaned nodes."
  (interactive)
  (message "\n=== ORPHANED NODE STRATEGIES ===")
  
  (let ((analysis (org-supertag-analyze-node-file-paths)))
    (let ((orphaned-count (plist-get analysis :nodes-without-files)))
      
      (if (= orphaned-count 0)
          (message "✅ No orphaned nodes found!")
        
        (message "\n🔧 Available strategies:")
        (message "")
        (message "1. 🗑️  DELETE ORPHANED NODES")
        (message "   - Remove nodes without file paths")
        (message "   - Assume they're data artifacts")
        (message "   - Risk: Lose potentially valid data")
        (message "")
        (message "2. 🏷️  CONVERT TO METADATA OBJECTS")
        (message "   - Change type from :node to :tag or :metadata")
        (message "   - Keep data but exclude from sync")
        (message "   - Risk: Data type confusion")
        (message "")
        (message "3. 📁 ASSIGN PLACEHOLDER FILES")
        (message "   - Set file-path to a placeholder")
        (message "   - Include in sync but as special nodes")
        (message "   - Risk: Confuse file-based logic")
        (message "")
        (message "4. 🔍 MANUAL INVESTIGATION")
        (message "   - Check if these correspond to real org content")
        (message "   - Try to locate original source")
        (message "   - Risk: Time consuming")
        
        (message "\n💡 RECOMMENDATION:")
        (if (> orphaned-count 10)
            (message "   Many orphaned nodes (%d) - likely data corruption" orphaned-count)
          (message "   Few orphaned nodes (%d) - worth investigating" orphaned-count))
        
        (message "   Suggested approach: DELETE orphaned nodes")
        (message "   Reason: They lack file context and may be corrupted data")))))

;; Execute analysis
(org-supertag-analyze-node-file-paths)
(org-supertag-fix-orphaned-nodes-strategy)

(message "\nTo proceed with deletion, run:")
(message "(org-supertag-delete-orphaned-nodes)")

(defun org-supertag-delete-orphaned-nodes ()
  "Delete nodes without file paths to clean up corrupted data."
  (interactive)
  (when (yes-or-no-p "Delete all nodes without file paths? This cannot be undone! ")
    (let ((deleted-count 0)
          (nodes-to-delete '()))
      
      ;; Collect orphaned nodes
      (maphash (lambda (id props)
                 (when (and (eq (plist-get props :type) :node)
                           (or (null (plist-get props :file-path))
                               (string-empty-p (plist-get props :file-path))))
                   (push id nodes-to-delete)))
               org-supertag-db--object)
      
      ;; Delete them
      (dolist (id nodes-to-delete)
        (org-supertag-db-remove-object id)
        (cl-incf deleted-count))
      
      ;; Save database
      (when (> deleted-count 0)
        (org-supertag-db--mark-dirty)
        (org-supertag-db-save))
      
      (message "Deleted %d orphaned nodes" deleted-count)
      (message "Remaining nodes should have proper file paths")
      deleted-count)))

(message "\n=== ANALYSIS COMPLETED ===") 
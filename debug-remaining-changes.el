;;; Debug remaining changes issue

(require 'org-supertag-background-sync)
(require 'org-supertag-db)

(defun safe-substring (str start end)
  "Safely extract substring, handling cases where string is shorter than expected."
  (when str
    (let ((str-len (length str)))
      (substring str start (min end str-len)))))

(defun debug-remaining-changes ()
  "Analyze why we still have too many changes."
  (interactive)
  (message "\n=== DEBUGGING REMAINING CHANGES ===")
  
  ;; Helper function (same as in the fixed code)
  (cl-flet ((extract-id (props)
             (or (plist-get props :id)
                 (plist-get props :node-id)
                 (plist-get props :tag-id)
                 (plist-get props :link-id)
                 ;; For links, try source/target as fallback
                 (when (eq (plist-get props :type) :link)
                   (format "%s->%s" 
                           (or (plist-get props :source) "")
                           (or (plist-get props :target) ""))))))
    
    ;; 1. Check basic counts
    (message "\n1. Basic Counts:")
    (message "   Objects: %d" (hash-table-count org-supertag-db--object))
    (message "   Links: %d" (hash-table-count org-supertag-db--link))
    (message "   Hash records: %d" (hash-table-count org-supertag-background-sync--last-sync-hashes))
    
    ;; 2. Check ID extraction success rate
    (let ((successful-extractions 0)
          (failed-extractions 0)
          (sample-failed-ids '()))
      
      (maphash (lambda (db-id props)
                 (let ((extracted-id (extract-id props)))
                   (if extracted-id
                       (cl-incf successful-extractions)
                     (cl-incf failed-extractions)
                     (when (< (length sample-failed-ids) 3)
                       (push (safe-substring db-id 0 20) sample-failed-ids)))))
               org-supertag-db--object)
      
      (message "\n2. ID Extraction Analysis:")
      (message "   Successful extractions: %d" successful-extractions)
      (message "   Failed extractions: %d" failed-extractions)
      (when sample-failed-ids
        (message "   Sample failed IDs: %s" sample-failed-ids)))
    
    ;; 3. Hash lookup investigation
    (message "\n3. Hash Lookup Investigation:")
    (let ((test-id "29DB1144-A40B-4598-A6E9-1CE802751A72"))
      (message "   Testing specific ID: %s" test-id)
      (message "   Found in hash table: %s" 
               (if (gethash test-id org-supertag-background-sync--last-sync-hashes) "YES" "NO")))
    
    ;; Check a few sample hash keys
    (message "   Sample hash table keys (first 5):")
    (let ((count 0))
      (catch 'done
        (maphash (lambda (key value)
                   (when (>= count 5) (throw 'done nil))
                   (message "     Key: %s, Hash: %s" 
                           (safe-substring key 0 30)
                           (safe-substring value 0 8))
                   (cl-incf count))
                 org-supertag-background-sync--last-sync-hashes)))
    
    ;; 4. Detailed hash comparison for first few objects
    (message "\n4. Detailed Hash Comparison (first 3 objects):")
    (let ((count 0))
      (catch 'done
        (maphash (lambda (db-id props)
                   (when (>= count 3) (throw 'done nil))
                   (let* ((extracted-id (extract-id props))
                          (current-hash (org-supertag-background-sync--calculate-object-hash props))
                          (last-hash (gethash extracted-id org-supertag-background-sync--last-sync-hashes))
                          (db-id-short (safe-substring db-id 0 20))
                          (extracted-id-short (if extracted-id (safe-substring extracted-id 0 20) "nil")))
                     (message "   Object %d:" (1+ count))
                     (message "     DB ID: %s" db-id-short)
                     (message "     Extracted ID: %s" extracted-id-short)
                     (message "     Current hash: %s" (if current-hash (safe-substring current-hash 0 8) "nil"))
                     (message "     Last hash: %s" (if last-hash (safe-substring last-hash 0 8) "nil"))
                     (message "     Match: %s" (if (and current-hash last-hash extracted-id)
                                                   (string= current-hash last-hash)
                                                 "N/A"))
                     (message "     ID match: %s" (if extracted-id (string= db-id extracted-id) "N/A"))
                     ;; Additional debugging: manually check if this ID exists in hash table
                     (message "     Manual hash lookup: %s"
                             (if (gethash extracted-id org-supertag-background-sync--last-sync-hashes) "FOUND" "NOT FOUND"))
                     (cl-incf count)))
                 org-supertag-db--object)))
    
    ;; 5. Check hash table integrity
    (message "\n5. Hash Table Integrity Check:")
    (message "   Hash table test (manual lookup):")
    (let ((first-object-id nil))
      (catch 'found
        (maphash (lambda (db-id props)
                   (setq first-object-id (extract-id props))
                   (throw 'found t))
                 org-supertag-db--object))
      (when first-object-id
        (message "     First object ID: %s" first-object-id)
        (message "     Direct gethash result: %s" 
                (or (gethash first-object-id org-supertag-background-sync--last-sync-hashes) "nil"))))))

;; Run the analysis
(debug-remaining-changes) 

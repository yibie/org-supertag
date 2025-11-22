;;; check-reference-integrity.el --- Diagnose reference and backlink consistency issues -*- lexical-binding: t; -*-

;; This script checks for:
;; 1. Duplicate backlinks in target nodes
;; 2. Orphaned backlinks (in file but not in DB)
;; 3. Missing backlinks (in DB but not in file)

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'supertag-core-store)
(require 'supertag-ops-node)
(require 'supertag-ops-relation)

(defvar supertag--ref-check-results nil
  "Stores results of the reference integrity check.")

(defun supertag-check-reference-integrity ()
  "Check integrity of references and backlinks across all nodes.
Returns a report of issues found."
  (interactive)
  (unless (and (boundp 'supertag--store) 
               (hash-table-p supertag--store)
               (> (hash-table-count supertag--store) 0))
    (user-error "Store not loaded. Please ensure org-supertag is initialized with M-x supertag-init"))
  
  (let ((all-relations (supertag-store-get-collection :relations))
        (duplicate-backlinks '())
        (orphaned-backlinks '())
        (missing-backlinks '())
        (total-checked 0))
    
    (message "Starting reference integrity check...")
    
    ;; Check each :reference relation
    (when (hash-table-p all-relations)
      (maphash
       (lambda (rel-id relation)
         (when (eq (plist-get relation :type) :reference)
           (setq total-checked (1+ total-checked))
           (let* ((from-id (plist-get relation :from))
                  (to-id (plist-get relation :to))
                  (to-node (supertag-node-get to-id)))
             
             ;; Check if target node exists
             (if (not to-node)
                 (setq orphaned-backlinks
                       (cons (list :type 'missing-target-node
                                  :relation-id rel-id
                                  :from from-id
                                  :to to-id)
                             orphaned-backlinks))
               
               ;; Check backlinks in target node's file
               (let* ((target-file (plist-get to-node :file))
                      (target-pos (plist-get to-node :position))
                      (backlink-count 0))
                 
                 (when (and target-file target-pos (file-exists-p target-file))
                   (with-current-buffer (find-file-noselect target-file)
                     (org-with-wide-buffer
                      (save-excursion
                        (goto-char target-pos)
                        (condition-case err
                            (let ((content-start (progn
                                                   (org-back-to-heading t)
                                                   (org-end-of-meta-data t)
                                                   (point)))
                                  (content-end (save-excursion
                                                 (if (re-search-forward org-outline-regexp nil t)
                                                     (match-beginning 0)
                                                   (org-end-of-subtree t t)
                                                   (point))))
                                  (pattern (format "\\[\\[id:%s\\]" (regexp-quote from-id))))
                              
                              ;; Count occurrences of backlink
                              (goto-char content-start)
                              (while (re-search-forward pattern content-end t)
                                (setq backlink-count (1+ backlink-count)))
                              
                              ;; Report issues
                              (cond
                               ((= backlink-count 0)
                                (setq missing-backlinks
                                      (cons (list :type 'missing-backlink
                                                 :relation-id rel-id
                                                 :from from-id
                                                 :to to-id
                                                 :target-file target-file
                                                 :target-pos target-pos)
                                            missing-backlinks)))
                               ((> backlink-count 1)
                                (setq duplicate-backlinks
                                      (cons (list :type 'duplicate-backlink
                                                 :relation-id rel-id
                                                 :from from-id
                                                 :to to-id
                                                 :target-file target-file
                                                 :target-pos target-pos
                                                 :count backlink-count)
                                            duplicate-backlinks)))))
                          (error
                           (message "Error checking node %s: %s" to-id (error-message-string err)))))))))))))
       all-relations))
    
    ;; Generate report
    (let ((report (list :total-relations-checked total-checked
                       :duplicate-backlinks duplicate-backlinks
                       :orphaned-backlinks orphaned-backlinks
                       :missing-backlinks missing-backlinks
                       :duplicate-count (length duplicate-backlinks)
                       :orphaned-count (length orphaned-backlinks)
                       :missing-count (length missing-backlinks))))
      
      (setq supertag--ref-check-results report)
      
      ;; Display report
      (supertag--display-integrity-report report)
      
      report)))

(defun supertag--display-integrity-report (report)
  "Display the integrity check REPORT in a readable format."
  (let ((buf (get-buffer-create "*Supertag Reference Integrity*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== ORG-SUPERTAG REFERENCE INTEGRITY CHECK ===\n\n")
      (insert (format "Total relations checked: %d\n\n"
                     (plist-get report :total-relations-checked)))
      
      ;; Duplicate backlinks
      (let ((duplicates (plist-get report :duplicate-backlinks)))
        (insert (format "❌ DUPLICATE BACKLINKS: %d\n" (length duplicates)))
        (insert "These nodes have the same backlink inserted multiple times:\n\n")
        (dolist (issue duplicates)
          (insert (format "  • From: %s → To: %s\n" 
                         (plist-get issue :from)
                         (plist-get issue :to)))
          (insert (format "    File: %s\n"
                         (plist-get issue :target-file)))
          (insert (format "    Count: %d occurrences\n\n"
                         (plist-get issue :count)))))
      
      ;; Missing backlinks
      (let ((missing (plist-get report :missing-backlinks)))
        (insert (format "\n❌ MISSING BACKLINKS: %d\n" (length missing)))
        (insert "These relations exist in DB but backlink not found in file:\n\n")
        (dolist (issue missing)
          (insert (format "  • From: %s → To: %s\n"
                         (plist-get issue :from)
                         (plist-get issue :to)))
          (insert (format "    File: %s\n\n"
                         (plist-get issue :target-file)))))
      
      ;; Orphaned backlinks
      (let ((orphaned (plist-get report :orphaned-backlinks)))
        (insert (format "\n❌ ORPHANED RELATIONS: %d\n" (length orphaned)))
        (insert "These relations point to non-existent nodes:\n\n")
        (dolist (issue orphaned)
          (insert (format "  • From: %s → To: %s (node not found)\n"
                         (plist-get issue :from)
                         (plist-get issue :to)))
          (insert (format "    Relation ID: %s\n\n"
                         (plist-get issue :relation-id)))))
      
      ;; Summary
      (insert "\n=== SUMMARY ===\n")
      (if (and (= (plist-get report :duplicate-count) 0)
               (= (plist-get report :missing-count) 0)
               (= (plist-get report :orphaned-count) 0))
          (insert "✅ All reference and backlink data is consistent!\n")
        (insert "⚠️  Issues found. Please review the report above.\n"))
      
      (goto-char (point-min)))
    
    (pop-to-buffer buf)))

(defun supertag-fix-duplicate-backlinks ()
  "Attempt to fix duplicate backlinks by removing extras.
Only keeps the first occurrence of each backlink."
  (interactive)
  (unless supertag--ref-check-results
    (user-error "No check results available. Run `supertag-check-reference-integrity` first."))
  
  (let ((duplicates (plist-get supertag--ref-check-results :duplicate-backlinks))
        (fixed-count 0))
    
    (when (= (length duplicates) 0)
      (message "No duplicate backlinks to fix.")
      (cl-return-from supertag-fix-duplicate-backlinks))
    
    (unless (yes-or-no-p (format "Found %d nodes with duplicate backlinks. Fix them? " 
                                (length duplicates)))
      (user-error "Aborted."))
    
    (dolist (issue duplicates)
      (let* ((from-id (plist-get issue :from))
             (to-id (plist-get issue :to))
             (target-file (plist-get issue :target-file))
             (target-pos (plist-get issue :target-pos))
             (to-node (supertag-node-get to-id)))
        
        (when (and target-file target-pos (file-exists-p target-file))
          (with-current-buffer (find-file-noselect target-file)
            (org-with-wide-buffer
              (save-excursion
                (goto-char target-pos)
                (org-back-to-heading t)
                (org-end-of-meta-data t)
                (let* ((content-start (point))
                       (content-end (save-excursion
                                      (if (re-search-forward org-outline-regexp nil t)
                                          (match-beginning 0)
                                        (org-end-of-subtree t t)
                                        (point))))
                       (pattern (format "\\[\\[id:%s\\][^]]*\\]\\]" (regexp-quote from-id)))
                       (first-match-found nil))
                  
                  ;; Remove all matches except the first
                  (goto-char content-start)
                  (while (re-search-forward pattern content-end t)
                    (if (not first-match-found)
                        (setq first-match-found t)
                      ;; This is a duplicate, delete it
                      (let ((line-start (line-beginning-position))
                            (line-end (min (point-max) (1+ (line-end-position)))))
                        (delete-region line-start line-end)
                        (cl-incf fixed-count)
                        (setq content-end (- content-end (- line-end line-start))))))
                  
                  (save-buffer))))))))
    
    (message "Fixed %d duplicate backlinks." fixed-count)))

(defun supertag-fix-missing-backlinks ()
  "Fix missing backlinks by inserting them into target nodes.

Uses the last results stored in `supertag--ref-check-results`.
For each :reference relation that exists in DB but has no backlink
in the target node's file, this command inserts a canonical
backlink line under the target node's content region."
  (interactive)
  (unless supertag--ref-check-results
    (user-error "No check results available. Run `supertag-check-reference-integrity` first."))
  (let* ((missing (plist-get supertag--ref-check-results :missing-backlinks))
         (count (length missing))
         (fixed-count 0))
    (when (= count 0)
      (message "No missing backlinks to fix.")
      (cl-return-from supertag-fix-missing-backlinks))
    (unless (yes-or-no-p (format "Found %d missing backlinks. Insert them now? " count))
      (user-error "Aborted."))
    (dolist (issue missing)
      (let* ((from-id (plist-get issue :from))
             (to-id (plist-get issue :to))
             (target-file (plist-get issue :target-file))
             (target-pos (plist-get issue :target-pos))
             (to-node (supertag-node-get to-id))
             (from-node (supertag-node-get from-id)))
        (when (and target-file target-pos (file-exists-p target-file) to-node)
          (with-current-buffer (find-file-noselect target-file)
            (org-with-wide-buffer
              (save-excursion
                (goto-char target-pos)
                (condition-case err
                    (progn
                      (org-back-to-heading t)
                      (when (org-at-heading-p)
                        (org-end-of-meta-data t)
                        (let* ((content-start (point))
                               (content-end (save-excursion
                                              (if (re-search-forward org-outline-regexp nil t)
                                                  (match-beginning 0)
                                                (org-end-of-subtree t t)
                                                (point))))
                               (pattern (format "\\[\\[id:%s\\]" (regexp-quote from-id))))
                          ;; If somehow a backlink was added since the check, skip.
                          (goto-char content-start)
                          (unless (re-search-forward pattern content-end t)
                            (goto-char content-end)
                            (unless (bolp) (insert "\n"))
                            (let* ((from-title (or (plist-get from-node :title)
                                                   (plist-get from-node :raw-value)
                                                   from-id)))
                              (insert (format "[[id:%s][%s]]\n" from-id from-title))
                              (cl-incf fixed-count))))
                        (save-buffer)))
                  (error
                   (message "Error fixing backlink %s -> %s in %s: %s"
                            from-id to-id target-file (error-message-string err))))))))))
    (message "Inserted %d missing backlinks." fixed-count)))

(provide 'check-reference-integrity)
;;; check-reference-integrity.el ends here

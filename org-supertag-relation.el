;;; org-supertag-relation.el --- Tag relationship management for org-supertag -*- lexical-binding: t; -*-

;; This file provides functionality for managing relationships between tags
;; in org-supertag.  It supports recording, querying, and analyzing tag
;; co-occurrence relationships.

;;; Code:

(require 'org-supertag-db)
(require 'org-supertag-node)

;;; Customization

(defgroup org-supertag-relation nil
  "Customization for org-supertag tag relationships."
  :group 'org-supertag)

(defcustom org-supertag-relation-strength-decay-factor 0.9
  "Factor by which relationship strength decays over time.
Values closer to 1.0 mean slower decay, values closer to 0 mean faster decay."
  :type 'float
  :group 'org-supertag-relation)

(defcustom org-supertag-relation-min-strength 0.1
  "Minimum strength threshold for considering a relationship significant."
  :type 'float
  :group 'org-supertag-relation)

;;; Core Functions

(defun org-supertag-relation-add (from-tag to-tag type &optional strength)
  "Add or update a relation between FROM-TAG and TO-TAG.
TYPE is the relation type (:cooccurrence, :parent-child, :causal, etc.).
STRENGTH is an optional relation strength (0.0-1.0).

If the relationship already exists, its strength will be increased."
  (let* ((rel-id (format ":relation:%s:%s" from-tag to-tag))
         (existing (org-supertag-db-get rel-id))
         (current-strength (or (and existing (plist-get existing :strength)) 0.0))
         (new-strength (or strength (min 1.0 (+ current-strength 0.1))))
         (rel-data (list :type type
                         :from from-tag
                         :to to-tag
                         :strength new-strength
                         :updated-at (current-time))))
    
    ;; If relation already exists, preserve creation time
    (when existing
      (setq rel-data (plist-put rel-data :created-at 
                               (plist-get existing :created-at))))
    
    ;; Otherwise set creation time
    (unless (plist-get rel-data :created-at)
      (setq rel-data (plist-put rel-data :created-at (current-time))))
    
    ;; Save relation
    (org-supertag-db-add rel-id rel-data)))

(defun org-supertag-relation-get (from-tag to-tag)
  "Get relation data between FROM-TAG and TO-TAG.
Returns relation property list or nil if no relation exists."
  (org-supertag-db-get (format ":relation:%s:%s" from-tag to-tag)))

(defun org-supertag-relation-get-strength (from-tag to-tag)
  "Get strength of relation between FROM-TAG and TO-TAG.
Returns a float between 0.0 and 1.0, or nil if no relation exists."
  (when-let ((rel (org-supertag-relation-get from-tag to-tag)))
    (plist-get rel :strength)))

(defun org-supertag-relation-exists-p (from-tag to-tag)
  "Check if a relation exists between FROM-TAG and TO-TAG.
Returns t if relation exists, nil otherwise."
  (not (null (org-supertag-relation-get from-tag to-tag))))

(defun org-supertag-relation-get-all-from (tag-id)
  "Get all relations where TAG-ID is the source.
Returns a list of (to-tag . relation-data) pairs."
  (let (result)
    (maphash
     (lambda (id props)
       (when (and (string-prefix-p ":relation:" id)
                  (equal (plist-get props :from) tag-id))
         (push (cons (plist-get props :to) props) result)))
     org-supertag-db--object)
    (nreverse result)))

(defun org-supertag-relation-get-all-to (tag-id)
  "Get all relations where TAG-ID is the target.
Returns a list of (from-tag . relation-data) pairs."
  (let (result)
    (maphash
     (lambda (id props)
       (when (and (string-prefix-p ":relation:" id)
                  (equal (plist-get props :to) tag-id))
         (push (cons (plist-get props :from) props) result)))
     org-supertag-db--object)
    (nreverse result)))

(defun org-supertag-relation-get-all-related (tag-id &optional min-strength)
  "Get all tags related to TAG-ID with strength >= MIN-STRENGTH.
Returns a list of (related-tag . strength) pairs sorted by strength."
  (let ((min-strength (or min-strength org-supertag-relation-min-strength))
        related)
    
    ;; Get outgoing relations
    (dolist (rel (org-supertag-relation-get-all-from tag-id))
      (let ((to-tag (car rel))
            (strength (plist-get (cdr rel) :strength)))
        (when (>= strength min-strength)
          (push (cons to-tag strength) related))))
    
    ;; Get incoming relations
    (dolist (rel (org-supertag-relation-get-all-to tag-id))
      (let ((from-tag (car rel))
            (strength (plist-get (cdr rel) :strength)))
        (when (>= strength min-strength)
          (push (cons from-tag strength) related))))
    
    ;; Remove duplicates and sort by strength
    (setq related 
          (seq-sort-by #'cdr #'> 
                      (seq-uniq related 
                               (lambda (a b) 
                                 (equal (car a) (car b))))))
    
    related))

;;; Co-occurrence Recording

(defun org-supertag-relation-record-cooccurrence (node-id tag-id)
  "Record co-occurrence relationships for TAG-ID on NODE-ID.
This creates bidirectional relationships between TAG-ID and all
other tags already on the node."
  (let ((existing-tags (org-supertag-node-get-tags node-id)))
    (dolist (other-tag existing-tags)
      (unless (equal other-tag tag-id)
        ;; Create bidirectional relationship
        (org-supertag-relation-add tag-id other-tag :cooccurrence)
        (org-supertag-relation-add other-tag tag-id :cooccurrence)))))

;;; Relationship Analysis

(defun org-supertag-relation-find-indirect (from-tag to-tag &optional max-depth)
  "Find indirect relationship path from FROM-TAG to TO-TAG.
MAX-DEPTH limits the search depth (default: 3).
Returns a list of tags forming the shortest path, or nil if no path exists."
  (let ((max-depth (or max-depth 3))
        (visited (make-hash-table :test 'equal))
        (queue (list (list from-tag)))
        result)
    
    (while (and queue (not result))
      (let ((path (pop queue)))
        (let ((current (car path)))
          ;; Mark as visited
          (puthash current t visited)
          
          ;; Check if we reached the target
          (if (equal current to-tag)
              (setq result (reverse path))
            
            ;; Otherwise explore neighbors if not at max depth
            (when (< (length path) max-depth)
              (dolist (rel (org-supertag-relation-get-all-from current))
                (let ((next-tag (car rel)))
                  (unless (gethash next-tag visited)
                    (push (cons next-tag path) queue)))))))))
    
    result))

(defun org-supertag-relation-suggest-tags (tag-id &optional limit)
  "Suggest related tags for TAG-ID.
LIMIT is the maximum number of suggestions to return.
Returns a list of (tag-id . strength) pairs."
  (let ((related (org-supertag-relation-get-all-related tag-id))
        (limit (or limit 10)))
    (seq-take related limit)))

;;; Visualization

(defun org-supertag-relation-visualize-text (tag-id &optional depth)
  "Create a text visualization of TAG-ID relationships.
DEPTH controls how many levels of relationships to show (default: 1)."
  (interactive (list (completing-read "Tag: " (org-supertag-get-all-tags))))
  
  (let ((depth (or depth 1))
        (visited (make-hash-table :test 'equal)))
    
    (with-output-to-temp-buffer "*Tag Relationships*"
      (princ (format "Relationships for #%s:\n\n" tag-id))
      
      ;; Recursively display relationships
      (org-supertag-relation--visualize-text-recursive 
       tag-id 0 depth visited))))

(defun org-supertag-relation--visualize-text-recursive (tag-id level max-depth visited)
  "Recursive helper for text visualization.
TAG-ID is the current tag to visualize.
LEVEL is the current depth level.
MAX-DEPTH is the maximum depth to explore.
VISITED is a hash table of already visited tags."
  ;; Prevent cycles
  (puthash tag-id t visited)
  
  ;; Get relationships
  (let ((related (org-supertag-relation-get-all-related tag-id 0.1)))
    (dolist (rel related)
      (let ((other-tag (car rel))
            (strength (cdr rel)))
        ;; Only process unvisited tags
        (unless (gethash other-tag visited)
          ;; Display current relationship
          (princ (format "%s%s #%-20s (%.2f)\n"
                        (make-string (* level 2) ?\s)
                        (if (= level 0) "→" "└─")
                        other-tag
                        strength))
          
          ;; Recursively process next level (if not at max depth)
          (when (< (1+ level) max-depth)
            (org-supertag-relation--visualize-text-recursive 
             other-tag (1+ level) max-depth visited)))))))

;;; User Interactive Commands

(defun org-supertag-relation-show-related (tag-id)
  "Show tags related to TAG-ID."
  (interactive
   (list (completing-read "Tag: " 
                         (mapcar #'car 
                                (org-supertag-db-find-by-type :tag)))))
  
  (let ((related (org-supertag-relation-get-all-related tag-id)))
    (if related
        (with-output-to-temp-buffer "*Related Tags*"
          (princ (format "Tags related to #%s:\n\n" tag-id))
          (dolist (rel related)
            (princ (format "  #%-20s (strength: %.2f)\n" 
                          (car rel) (cdr rel)))))
      (message "No strong relationships found for #%s" tag-id))))

(defun org-supertag-relation-find-path (from-tag to-tag)
  "Find and display relationship path between FROM-TAG and TO-TAG."
  (interactive
   (let* ((from (completing-read "From tag: " 
                               (mapcar #'car 
                                      (org-supertag-db-find-by-type :tag))))
          (to (completing-read "To tag: " 
                             (mapcar #'car 
                                    (org-supertag-db-find-by-type :tag)))))
     (list from to)))
  
  (let ((path (org-supertag-relation-find-indirect from-tag to-tag)))
    (if path
        (with-output-to-temp-buffer "*Tag Path*"
          (princ (format "Path from #%s to #%s:\n\n" from-tag to-tag))
          (princ (mapconcat (lambda (tag) (concat "#" tag)) path " → ")))
      (message "No path found from #%s to #%s" from-tag to-tag))))

(defun org-supertag-relation-analyze-network ()
  "Analyze the tag relationship network and show statistics."
  (interactive)
  (let ((tags (mapcar #'car (org-supertag-db-find-by-type :tag)))
        (relations 0)
        (strongest-rel nil)
        (strongest-val 0)
        (isolated-tags 0))
    
    ;; Count relations and find strongest
    (maphash
     (lambda (id props)
       (when (string-prefix-p ":relation:" id)
         (cl-incf relations)
         (let ((strength (plist-get props :strength)))
           (when (> strength strongest-val)
             (setq strongest-val strength
                   strongest-rel (cons (plist-get props :from)
                                     (plist-get props :to)))))))
     org-supertag-db--object)
    
    ;; Count isolated tags
    (dolist (tag tags)
      (unless (or (org-supertag-relation-get-all-from tag)
                 (org-supertag-relation-get-all-to tag))
        (cl-incf isolated-tags)))
    
    ;; Display results
    (with-output-to-temp-buffer "*Tag Network Analysis*"
      (princ "Tag Relationship Network Analysis\n")
      (princ "================================\n\n")
      
      (princ (format "Total tags: %d\n" (length tags)))
      (princ (format "Total relationships: %d\n" relations))
      (princ (format "Isolated tags: %d (%.1f%%)\n" 
                    isolated-tags
                    (* 100.0 (/ (float isolated-tags) (length tags)))))
      
      (when strongest-rel
        (princ (format "\nStrongest relationship: #%s ↔ #%s (%.2f)\n"
                      (car strongest-rel)
                      (cdr strongest-rel)
                      strongest-val)))
      
      ;; Most connected tags
      (princ "\nMost connected tags:\n")
      (let* ((tag-counts
              (sort
               (mapcar
                (lambda (tag)
                  (let ((count (+ (length (org-supertag-relation-get-all-from tag))
                                 (length (org-supertag-relation-get-all-to tag)))))
                    (cons tag count)))
                tags)
               (lambda (a b) (> (cdr a) (cdr b)))))
             (top-tags (seq-take tag-counts (min 10 (length tag-counts)))))
        (dolist (item top-tags)
          (princ (format "  #%-20s %d connections\n" 
                        (car item) (cdr item))))))))

;;; Testing Functions

(defun org-supertag-relation-test ()
  "Run a simple test of the relation functionality."
  (interactive)
  (let ((test-tag1 "test-tag1")
        (test-tag2 "test-tag2")
        (test-tag3 "test-tag3"))
    
    ;; Create test tags if they don't exist
    (dolist (tag (list test-tag1 test-tag2 test-tag3))
      (unless (org-supertag-db-get tag)
        (org-supertag-db-add tag (list :type :tag
                                      :name tag
                                      :created-at (current-time)))))
    
    ;; Add relationships
    (org-supertag-relation-add test-tag1 test-tag2 :cooccurrence 0.5)
    (org-supertag-relation-add test-tag2 test-tag3 :cooccurrence 0.7)
    
    ;; Test relationship retrieval
    (let ((rel1-2 (org-supertag-relation-get test-tag1 test-tag2))
          (rel2-3 (org-supertag-relation-get test-tag2 test-tag3))
          (indirect (org-supertag-relation-find-indirect test-tag1 test-tag3)))
      
      (with-output-to-temp-buffer "*Relation Test Results*"
        (princ "Relation Test Results\n")
        (princ "====================\n\n")
        
        (princ (format "Relation %s -> %s: %s\n" 
                      test-tag1 test-tag2
                      (if rel1-2 
                          (format "Found (strength: %.2f)" 
                                 (plist-get rel1-2 :strength))
                        "Not found")))
        
        (princ (format "Relation %s -> %s: %s\n" 
                      test-tag2 test-tag3
                      (if rel2-3 
                          (format "Found (strength: %.2f)" 
                                 (plist-get rel2-3 :strength))
                        "Not found")))
        
        (princ (format "\nIndirect path from %s to %s: %s\n"
                      test-tag1 test-tag3
                      (if indirect
                          (mapconcat #'identity indirect " -> ")
                        "No path found")))
        
        (princ "\nAll related tags for test-tag1:\n")
        (let ((related (org-supertag-relation-get-all-related test-tag1)))
          (if related
              (dolist (rel related)
                (princ (format "  %s (strength: %.2f)\n" 
                             (car rel) (cdr rel))))
            (princ "  None found")))))))

(defun org-supertag-query-test-expression (query-string)
  "Test the query expression parser with QUERY-STRING."
  (interactive "sQuery: ")
  (let* ((expr (org-supertag-query-parse-expression query-string))
         (result (org-supertag-query-execute expr)))
    
    (with-output-to-temp-buffer "*Query Test Results*"
      (princ "Query Test Results\n")
      (princ "=================\n\n")
      
      (princ (format "Query: %s\n\n" query-string))
      
      (princ "Parsed Expression:\n")
      (pp expr (current-buffer))
      (princ "\n")
      
      (princ (format "Results (%d nodes):\n" (length result)))
      (if result
          (dolist (node-id result)
            (when-let ((node (org-supertag-db-get node-id)))
              (princ (format "  %s: %s\n" 
                           node-id 
                           (or (plist-get node :title) "[No title]")))))
        (princ "  No matching nodes found")))))

(provide 'org-supertag-relation)
;;; org-supertag-relation.el ends here 
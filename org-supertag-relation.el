;;; org-supertag-relation.el --- Tag relationship management for org-supertag -*- lexical-binding: t; -*-

;; This file provides functionality for managing relationships between tags
;; in org-supertag.  It supports recording, querying, and analyzing tag
;; co-occurrence relationships.

;;; Commentary:
;; This file provides functionality for managing relationships between tags
;; in org-supertag. It now stores co-occurrence relationships directly in the
;; main database as :tag-tag relations with :relation-type 'cooccurrence.
;; This simplifies the architecture by removing the need for a separate
;; co-occurrence file and allows for direct display in the relation-management interface.

;;; Current State:
;;; - Co-occurrence data stored directly in main database as :tag-tag relations
;;; - Co-occurrence relationships identified by :relation-type 'cooccurrence
;;; - Simplified architecture with no separate co-occurrence file
;;; - Direct display in relation-management interface

;;; Benefits:
;;; - Unified data management
;;; - Simplified architecture
;;; - Direct access in relation-management interface
;;; - Reduced complexity and potential data inconsistencies

;;; Code:

;;; Code:

(require 'ht)
(require 'cl-lib)
(require 'org-element)
(require 'org-supertag-db)
(require 'org-supertag-node nil t)  
(require 'org-supertag-tag nil t)   



(defgroup org-supertag-relation nil
  "Customization for org-supertag tag relationships."
  :group 'org-supertag)

(defcustom org-supertag-relation-types
  '((contrast . "A ⋮ B, A compare with B") ; contrast relationship
    (relate . "A ~ B, A relate to B")   ; general relation
    (influence . "A → B, A influence B") ; influence relationship
    (contain . "A ⊃ B, A contain B")  ; containment (parent)
    (belong . "A ⊂ B, A belong to B")   ; belonging (child)
    (parallel . "A ∥ B, A parallel with B") ; parallel relationship
    (dependency . "A ⇒ B, A depend on B") ; dependency relationship
    (prerequisite . "A ⊃ B, A prerequisite B") ; prerequisite relationship
    (cause . "A ⤳ B, A cause B")    ; causal relationship
    (effect . "A ⤝ B, A effect B")   ; effect relationship
    (cooccurrence . "A ⋈ B, A co-occur with B")) ; co-occurrence relationship
  "Predefined relation types.
Each relation type contains a symbol and a description text."
  :type '(alist :key-type symbol :value-type string)
  :group 'org-supertag-relation)

(defcustom org-supertag-relation-groups
  '((default . (cooccurrence contrast relate influence contain belong))
    (knowledge . (contrast relate influence contain belong))
    (cooccurrence . (cooccurrence)))
  "Predefined relation groups.
Each group contains a list of relation types."
  :type '(alist :key-type symbol :value-type (repeat symbol))
  :group 'org-supertag-relation)

(defcustom org-supertag-relation-types-file
  (org-supertag-data-file "relation-types.el")
  "File path for storing relation type definitions."
  :type 'file
  :group 'org-supertag-relation)

(defcustom org-supertag-relation-min-strength 0.2
  "Tag relationship strength threshold below which relationships are ignored.
This setting determines the minimum strength value for a relationship to be considered significant."
  :type 'float
  :group 'org-supertag-relation)

(defcustom org-supertag-relation-cooccurrence-normalization-factor 5
  "Co-occurrence relationship strength normalization factor.
This factor is used to smooth the count of co-occurrences.
Lower values make the strength of co-occurrences more pronounced,
while higher values make the changes more gradual."
  :type 'integer
  :group 'org-supertag-relation)

(defcustom org-supertag-relation-enable-incremental-updates t
  "Whether to enable incremental updates for co-occurrence relationships.
Enabling this option will update related co-occurrences immediately when tags are added or removed."
  :type 'boolean
  :group 'org-supertag-relation)

(defcustom org-supertag-relation-complementary-pairs
  '((contain . belong)
    (cause . effect)
    (dependency . prerequisite))
  "Define complementary relation pairs.
When adding a relation, the system will automatically add its complementary relation.
For example, when A contains B, B belongs to A."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'org-supertag-relation)

(defcustom org-supertag-relation-auto-complement t
  "Whether to automatically add complementary relations.
When set to t, adding a relation will automatically add its complementary relation."
  :type 'boolean
  :group 'org-supertag-relation)





(defun org-supertag-relation--clean-completion (value)
  "Safely extract a string from VALUE returned by `completing-read`."
  (if (listp value)
      (car value)
    value))






(defun org-supertag-relation-init ()
  "Initialize the relation module."
  ;; Ensure co-occurrence relations are always up-to-date in the main DB.
  (org-supertag-relation-analyze-cooccurrence-patterns))

;;----------------------------------------------------------------------
;; Co-occurrence relation management
;;----------------------------------------------------------------------

(defun org-supertag-relation--update-cooccurrence-strength (from-tag to-tag)
  "Update (or create) the co-occurrence relation in the main database.
This function calculates the strength based on raw frequency and stores it
as a :tag-tag relation with :relation-type 'cooccurrence."
  (let* ((freq (org-supertag-relation--get-cooccurrence-count from-tag to-tag))
         (norm-factor org-supertag-relation-cooccurrence-normalization-factor)
         (strength (if (> (+ freq norm-factor) 0)
                       (/ freq (+ freq norm-factor))
                     0)))
    (if (< strength org-supertag-relation-min-strength)
        (org-supertag-db-unlink :tag-tag from-tag to-tag)
      (org-supertag-db-link :tag-tag from-tag to-tag
                            `(:relation-type cooccurrence :strength ,strength)))))

(defun org-supertag-relation-record-cooccurrence (node-id tag-id)
  "Record the co-occurrence relationship of tags, unidirectionally propagated from parent node to child node.
NODE-ID: Node ID
TAG-ID: Tag ID

This function will:
1. Record the co-occurrence relationship between tags on the same node
2. Record the unidirectional co-occurrence relationship from parent node tags

Co-occurrence data is stored as :tag-tag relations in the main database."
  ;; --- FIX: Sanitize all incoming and retrieved tag IDs at the source ---
  ;; This prevents contaminated strings from being used to build DB keys.
  (let* ((clean-tag-id (substring-no-properties (format "%s" tag-id)))
         (existing-tags (org-supertag-node-get-tags node-id))
         (clean-existing-tags (mapcar (lambda (t) (substring-no-properties (format "%s" t))) existing-tags))
         (parent-tags (org-supertag-node-get-parent-tags node-id))
         (clean-parent-tags (mapcar (lambda (t) (substring-no-properties (format "%s" t))) parent-tags)))
    
    ;; Filter out nil and invalid tag IDs to prevent "emacs --[cooccurrence]--> nil" issues
    (setq clean-tag-id (when (and clean-tag-id (not (string-empty-p clean-tag-id))) clean-tag-id))
    (setq clean-existing-tags (cl-remove-if (lambda (t) (or (null t) (string-empty-p t))) clean-existing-tags))
    (setq clean-parent-tags (cl-remove-if (lambda (t) (or (null t) (string-empty-p t))) clean-parent-tags))
    
    ;; Skip processing if the main tag ID is invalid
    (unless clean-tag-id
      (message "WARNING: Invalid tag ID '%s' for node '%s', skipping cooccurrence recording" tag-id node-id)
      (cl-return-from org-supertag-relation-record-cooccurrence))

    ;; 1. Process co-occurrence between same-level tags
    (dolist (other-tag clean-existing-tags)
      (unless (equal other-tag clean-tag-id)
        (let* ((freq (org-supertag-relation--get-cooccurrence-count clean-tag-id other-tag))
               (new-freq (1+ freq)))
          (org-supertag-relation--set-cooccurrence-count clean-tag-id other-tag new-freq)
          ;; Update strengths in both directions
          (org-supertag-relation--update-cooccurrence-strength clean-tag-id other-tag)
          (org-supertag-relation--update-cooccurrence-strength other-tag clean-tag-id))))

    ;; 2. Process bidirectional co-occurrence from parent node tags
    (when clean-parent-tags
      (dolist (parent-tag clean-parent-tags)
        (unless (equal parent-tag clean-tag-id)
          (let* ((freq (org-supertag-relation--get-cooccurrence-count parent-tag clean-tag-id))
                 (new-freq (+ freq 0.5)))  ; The co-occurrence weight from parent to child is 0.5
            (org-supertag-relation--set-cooccurrence-count parent-tag clean-tag-id new-freq)
            ;; Update strengths in both directions
            (org-supertag-relation--update-cooccurrence-strength parent-tag clean-tag-id)
            (org-supertag-relation--update-cooccurrence-strength clean-tag-id parent-tag)))))))

(defun org-supertag-relation--update-cooccurrence-strength (from-tag to-tag)
  "Update (or create) the co-occurrence relation in the main database.
This function calculates the strength based on raw frequency and stores it
as a :tag-tag relation with :relation-type 'cooccurrence."
  (let* ((freq (org-supertag-relation--get-cooccurrence-count from-tag to-tag))
         (norm-factor org-supertag-relation-cooccurrence-normalization-factor)
         (strength (if (> (+ freq norm-factor) 0)
                       (/ freq (+ freq norm-factor))
                     0)))
    (if (< strength org-supertag-relation-min-strength)
        (org-supertag-db-unlink :tag-tag from-tag to-tag)
      (org-supertag-db-link :tag-tag from-tag to-tag
                            `(:relation-type cooccurrence :strength ,strength)))))

(defun org-supertag-relation-unrecord-cooccurrence (node-id removed-tag-id)
  "Decrement co-occurrence relationships when a tag is removed from a node.
This is the reverse operation of `org-supertag-relation-record-cooccurrence'.
It updates the co-occurrence strength in the main database."
  (let ((remaining-tags (org-supertag-node-get-tags node-id)) ; This is after the tag link is removed
        (parent-tags (org-supertag-node-get-parent-tags node-id)))
    ;; 1. Update relationships with same-level tags that are still on the node
    (dolist (other-tag remaining-tags)
      (let* ((current-freq (org-supertag-relation--get-cooccurrence-count removed-tag-id other-tag))
             (new-freq (max 0 (- current-freq 1))))
        (org-supertag-relation--set-cooccurrence-count removed-tag-id other-tag new-freq)
        ;; Update relation strength based on new frequency
        (org-supertag-relation--update-cooccurrence-strength removed-tag-id other-tag)
        (org-supertag-relation--update-cooccurrence-strength other-tag removed-tag-id)))
    ;; 2. Update relationships with parent tags
    (when parent-tags
      (dolist (parent-tag parent-tags)
        (let* ((current-freq (org-supertag-relation--get-cooccurrence-count parent-tag removed-tag-id))
               (new-freq (max 0 (- current-freq 0.5)))) ; The weight from parent to child is 0.5
          (org-supertag-relation--set-cooccurrence-count parent-tag removed-tag-id new-freq)
          ;; Update relation strength based on new frequency
          (org-supertag-relation--update-cooccurrence-strength parent-tag removed-tag-id)
          (org-supertag-relation--update-cooccurrence-strength removed-tag-id parent-tag))))))

(defun org-supertag-relation--get-cooccurrence-count (tag1 tag2)
  "Get the co-occurrence count (strength) between TAG1 and TAG2 from the main database.
If there is no co-occurrence record, return 0."
  (let* ((link (car (org-supertag-db-find-links :tag-tag tag1 tag2)))
         (cooccur-link (cl-find-if (lambda (l) (and (eq (plist-get l :relation-type) 'cooccurrence)
                                                  (equal (plist-get l :to) tag2))) link)))
    (if cooccur-link
        (plist-get cooccur-link :strength) ; Assuming strength is the count for now
      0)))

(defun org-supertag-relation--set-cooccurrence-count (tag1 tag2 count)
  "Set the co-occurrence count (strength) between TAG1 and TAG2 in the main database."
  (org-supertag-db-link :tag-tag tag1 tag2 `(:relation-type cooccurrence :strength ,(float count))))

(defun org-supertag-relation-analyze-cooccurrence-patterns ()
  "Analyze co-occurrence patterns across all tags and update relationship strength."
  (interactive)
  (message "Starting to analyze co-occurrence patterns...")
  (let* ((nodes (org-supertag-db-find-by-type :node))
         (tag-counts (make-hash-table :test 'equal))  ; Count of nodes per tag
         (cooccur-counts (make-hash-table :test 'equal))  ; Co-occurrence counts
         (total-nodes (length nodes)))
    
    ;; Calculate the number of times each tag appears and co-occurrence counts
    (dolist (node-id nodes)
      (let ((tags (org-supertag-node-get-tags node-id)))
        ;; Calculate the number of times a single tag appears
        (dolist (tag tags)
          (puthash tag (1+ (or (gethash tag tag-counts) 0)) tag-counts))
        
        ;; Calculate the co-occurrence count between tags
        (dolist (tag1 tags)
          (dolist (tag2 tags)
            (unless (equal tag1 tag2)
              (let ((key (format "%s:%s" tag1 tag2)))
                (puthash key (1+ (or (gethash key cooccur-counts) 0)) cooccur-counts)))))))
    
    ;; Calculate the point mutual information (PMI) for each co-occurrence
    (maphash
     (lambda (key count)
       (let* ((tags (split-string key ":"))
              (tag1 (car tags))
              (tag2 (cadr tags))
              (prob-t1 (if (> total-nodes 0) (/ (float (gethash tag1 tag-counts 0)) total-nodes) 0.0))
              (prob-t2 (if (> total-nodes 0) (/ (float (gethash tag2 tag-counts 0)) total-nodes) 0.0))
              (prob-t1t2 (if (> total-nodes 0) (/ (float count) total-nodes) 0.0))
              (pmi (if (and (> prob-t1 0) (> prob-t2 0) (> prob-t1t2 0))
                       (log (/ prob-t1t2 (* prob-t1 prob-t2)) 2)
                     0.0)))
         ;; Normalize PMI to 0-1 range, ensuring it's always positive
         (let* ((norm-pmi (if (>= pmi 0) (/ pmi (+ pmi 1)) (/ pmi (- pmi 1)))) ; Sigmoid-like normalization
                (strength (min 1.0 (max 0.0 norm-pmi)))) ; Ensure strength is between 0 and 1
           
           ;; If strength is meaningful, update the relation with the new strength
           (when (>= strength org-supertag-relation-min-strength)
             (org-supertag-db-link :tag-tag tag1 tag2 `(:relation-type cooccurrence :strength ,strength))))))
     cooccur-counts)
    
    (message "Co-occurrence analysis completed. Analyzed %d nodes and %d tag pairs." total-nodes (hash-table-count cooccur-counts))))

(defun org-supertag-relation-get-strength (tag1 tag2)
  "Get the strength of the relationship between TAG1 and TAG2.
If the relation does not exist, return nil."
  (let* ((link (car (org-supertag-db-find-links :tag-tag tag1 tag2)))
         (strength (and link (plist-get link :strength))))
    strength))

;; Add a command to display co-occurrence relations
(defun org-supertag-relation-find-cooccurrence-tags ()
  "Find tags co-occurring with the current tag."
  (interactive)
  (let ((tag-id org-supertag-relation--current-tag))
    (if (not tag-id)
        (user-error "Current tag not set. Please re-select tag")
      (let* ((relations (org-supertag-db-find-links :tag-tag tag-id nil))
             (cooccur-relations (cl-remove-if-not
                                  (lambda (rel) (eq (plist-get rel :relation-type) 'cooccurrence))
                                  relations))
             (related-tags '()))
        
        ;; Collect related tags
        (dolist (rel cooccur-relations)
          (let* ((other-tag-id (plist-get rel :to))
                 (other-tag-name (org-supertag-tag-get-name-by-id other-tag-id))
                 (strength (or (plist-get rel :strength) 0.0)))
            (when other-tag-name
              (push (cons other-tag-name (format "%.2f" strength)) related-tags))))
        
        ;; Display results
        (if (null related-tags)
            (message "No tags co-occurring with the current tag")
          (with-output-to-temp-buffer "*Org-Supertag Co-occurrence*"
            (princ "Tags co-occurring with the current tag (sorted by strength):\n\n")
            (dolist (tag (sort related-tags (lambda (a b) (> (string-to-number (cdr a)) (string-to-number (cdr b))))))
              (princ (format "- %s (Strength: %s)\n" (car tag) (cdr tag))))))))))

;;----------------------------------------------------------------------
;; Relation Type Management
;;----------------------------------------------------------------------

(defun org-supertag-relation--get-all-relation-types ()
  "Get all available relation types."
  org-supertag-relation-types)

(defun org-supertag-relation--get-relation-type-choices ()
  "Make the choice list highlight bidirectional relations.
Return the formatted option list."
  (let ((choices nil)
        (relation-types org-supertag-relation-types))
    (dolist (rel-pair relation-types)
      (let* ((rel-type (car rel-pair))
             (description (cdr rel-pair))
             (complement-type (org-supertag-relation-get-complement rel-type))
             (formatted-option
              (if complement-type
                  (let ((complement-desc
                         (cdr (assq complement-type relation-types))))
                    (format "%s - %s ⟷ [Automatically adds bidirectional relation: %s]"
                            rel-type description complement-type))
                (format "%s - %s" rel-type description))))
        (push formatted-option choices)))
    (nreverse choices)))

(defun org-supertag-relation--get-type-from-choice (choice)
  "Extract relation type from formatted choice."
  (let ((selected-type (car (split-string choice " - "))))
    (intern selected-type)))

(defun org-supertag-relation-add-relation (from-tag to-tag rel-type &optional strength)
  "Add a tag relation.
FROM-TAG: Source tag ID
TO-TAG: Target tag ID
REL-TYPE: Relation type
STRENGTH: Relation strength (optional, default is 1.0)"
  ;; Check if the tags are valid
  (cond
   ((not from-tag)
    (user-error "Source tag cannot be empty"))
   ((not to-tag)
    (user-error "Target tag cannot be empty"))
   ((equal from-tag to-tag)
    (user-error "Source tag and target tag cannot be the same")))

  (let ((rel-id (format ":tag-relation:%s->%s:%s" from-tag to-tag rel-type)))
    ;; Check if relation already exists
    (unless (gethash rel-id org-supertag-db--link)
      ;; Add the relation
      (let ((props (list :from from-tag
                        :to to-tag
                        :type rel-type
                        :strength (or strength 1.0)
                        :created-at (current-time))))
        (puthash rel-id props org-supertag-db--link)
        (org-supertag-db-emit 'link:created :tag-relation from-tag to-tag props)
        (org-supertag-db--mark-dirty)
        (org-supertag-db--schedule-save)
        (message "Added relation: %s -[%s]-> %s"
                 (org-supertag-tag-get-name-by-id from-tag)
                 rel-type
                 (org-supertag-tag-get-name-by-id to-tag))))))

(defun org-supertag-relation-add-relation-interactive ()
  "Interactively add a tag relation."
  (interactive)
  (let* ((all-tags (org-supertag-get-all-tags))
         (from-tag-name (org-supertag-relation--clean-completion
                         (completing-read "Source tag: " all-tags nil t)))
         (to-tag-name (org-supertag-relation--clean-completion
                       (completing-read "Target tag: " all-tags nil t)))
         (from-tag-id (org-supertag-tag-get-id-by-name from-tag-name))
         (to-tag-id (org-supertag-tag-get-id-by-name to-tag-name))
         (rel-choices (org-supertag-relation--get-relation-type-choices))
         (rel-type (org-supertag-relation--get-type-from-choice
                    (completing-read "Relation type: " rel-choices nil t))))
    (org-supertag-relation-add-relation from-tag-id to-tag-id rel-type 1.0)))

(defun org-supertag-relation-remove-relation (from-tag to-tag &optional rel-type)
  "Remove the relation between FROM-TAG and TO-TAG.
If REL-TYPE is provided, only remove that specific relation type,
otherwise remove all relations between the tags."
  (when (and from-tag to-tag)
    (let* ((from-name (org-supertag-tag-get-name-by-id from-tag))
           (to-name (org-supertag-tag-get-name-by-id to-tag)))

      ;; If rel-type is provided, only remove that specific relation
      (if rel-type
          (let ((rel-id (format ":tag-relation:%s->%s:%s" from-tag to-tag rel-type)))
            (when (gethash rel-id org-supertag-db--link)
              (remhash rel-id org-supertag-db--link)
              (org-supertag-db-emit 'link:removed :tag-relation from-tag to-tag)))
        ;; Otherwise, remove all relations between these tags
        (let ((removed-count 0))
          (maphash
           (lambda (key _)
             (when (and (string-prefix-p ":tag-relation:" key)
                        (string-match (format ":tag-relation:%s->%s:" from-tag to-tag) key))
               (remhash key org-supertag-db--link)
               (org-supertag-db-emit 'link:removed :tag-relation from-tag to-tag)
               (cl-incf removed-count)))
           org-supertag-db--link)
          (message "Removed relation: %s -> %s" from-name to-name)
          (org-supertag-db--mark-dirty)
          (org-supertag-db--schedule-save))))))

(defun org-supertag-relation-remove-relation-interactive ()
  "Interactively remove a tag relation."
  (interactive)
  (let* ((from-tag-name (org-supertag-relation--clean-completion
                         (completing-read "Source tag: " (org-supertag-get-all-tags) nil t)))
         (from-tag-id (org-supertag-tag-get-id-by-name from-tag-name))
         (to-choices (mapcar
                      (lambda (rel)
                        (format "%s (%s)" (org-supertag-tag-get-name-by-id (car rel)) (cdr rel)))
                      (org-supertag-relation-get-all-from from-tag-id)))
         (choice (completing-read "Relation to remove: " to-choices nil t))
         (cleaned-choice (org-supertag-relation--clean-completion choice))
         (to-tag-name (car (split-string cleaned-choice " (")))
         (to-tag-id (org-supertag-tag-get-id-by-name to-tag-name)))
    (org-supertag-relation-remove-relation from-tag-id to-tag-id)))

(defun org-supertag-relation-get (from-tag to-tag &optional rel-type)
  "Get the relation data between FROM-TAG and TO-TAG.
FROM-TAG and TO-TAG can be names or IDs.
If REL-TYPE is provided, get the specific relation type,
otherwise return all relations between the tags.
Returns a list of relation property lists."
  (when (and from-tag to-tag)
    (let ((result nil)
          (from-id (or (org-supertag-db-get from-tag) (org-supertag-tag-get-id-by-name from-tag)))
          (to-id (or (org-supertag-db-get to-tag) (org-supertag-tag-get-id-by-name to-tag))))
      (unless from-id (error "Source tag not found: %s" from-tag))
      (unless to-id (error "Target tag not found: %s" to-tag))
      (maphash
       (lambda (key props)
         (when (and (string-prefix-p ":tag-relation:" key)
                    (string-match (format ":tag-relation:%s->%s:\(.+\)" from-id to-id) key))
           (when (or (null rel-type)
                     (eq rel-type (plist-get props :type)))
             (push props result))))
       org-supertag-db--link)
      result)))

(defun org-supertag-relation-get-all-from (tag-id)
  "Get all relations from the TAG-ID.
Return the list of (other-tag-id . rel-type)."
  (if (null tag-id)
      nil
    (let (result)
      (maphash
       (lambda (rel-id props)
         (when (and (string-prefix-p ":tag-relation:" rel-id)
                    (string-match (format ":tag-relation:%s->\([^:]+\):\(.+\)" tag-id) rel-id))
           (let ((to-tag (match-string 1 rel-id))
                 (rel-type (plist-get props :type)))
             (push (cons to-tag rel-type) result))))
       org-supertag-db--link)
      result)))

(defun org-supertag-relation-get-all-to (tag-id)
  "Get all relations to the TAG-ID.
Return the list of (other-tag-id . rel-type)."
  (if (null tag-id)
      nil
    (let (result)
      (maphash
       (lambda (rel-id props)
         (when (and (string-prefix-p ":tag-relation:" rel-id)
                    (equal (plist-get props :to) tag-id))
           (let ((from-tag (plist-get props :from))
                 (rel-type (plist-get props :type)))
             (push (cons from-tag rel-type) result))))
       org-supertag-db--link)
      result)))

(defun org-supertag-relation-get-all (tag-id &optional relation-type-filter)
  "Get all relations involving the TAG-ID.
Return the list of relations, each relation is a plist with :from, :to and :type keys.
Only returns outgoing relations (where TAG-ID is the :from tag).
Optionally filter by RELATION-TYPE-FILTER."
  (if (null tag-id)
      nil
    (let ((all-links (org-supertag-db-find-links nil tag-id nil)))
      (cl-remove-if-not
       (lambda (link)
         (let ((link-type (plist-get link :type))
               (rel-type (plist-get link :relation-type)))
           (cond
            ((eq link-type :tag-tag)
             (if relation-type-filter
                 (eq rel-type relation-type-filter)
               t)) ; Include all tag-tag relations if no filter
            (t ; For other link types, just check if filter is nil or matches
             (if relation-type-filter
                 (eq link-type relation-type-filter)
               t))))) ; Include all other link types if no filter
       all-links))))

(defun org-supertag-relation-get-tag-frequency (tag-id)
  "Compute the frequency of the TAG-ID in the database.
Return the number of nodes associated with this tag."
  (if (null tag-id)
      0
    (let ((count 0))
      (maphash
       (lambda (link-id props)
         (when (and (string-prefix-p ":node-tag:" link-id)
                    (equal (plist-get props :to) tag-id))
           (cl-incf count)))
       org-supertag-db--link)
      count)))

;;----------------------------------------------------------------------
;; Tag Relation Interface
;;----------------------------------------------------------------------

(defvar org-supertag-relation--current-tag nil
  "The ID of the current tag being edited.")

(defvar org-supertag-relation-manage--buffer-name "*Org-Supertag Relation Management*"
  "The name of the buffer for the relation management interface.")

(defvar org-supertag-relation--current-section 'existing
  "Current section in the relation management buffer.
Can be 'existing, 'cooccurrence, or 'recommended.")

(defvar org-supertag-relation--current-item-index 0
  "Current item index in the current section.")

(defvar org-supertag-relation--selected-tags nil
  "List of selected tags for batch operations.")

(defun org-supertag-relation-manage ()
  "Manage tag relations interface entry point."
  (interactive)
  (let* ((tags (org-supertag-get-all-tags))
         (tag-name (completing-read "Select a tag to manage relations: " tags nil t)))
    (if (or (null tag-name) (string-empty-p tag-name))
        (user-error "No valid tag selected")
      (let ((tag-id (org-supertag-tag-get-id-by-name tag-name)))
        (if (null tag-id)
            (user-error "No tag ID found for: %s" tag-name)
          (org-supertag-relation--show-management-interface tag-id))))))

(defun org-supertag-relation--show-management-interface (tag-id)
  "Show the tag relations management interface.
TAG-ID: The ID of the current tag being edited."
  (when (get-buffer org-supertag-relation-manage--buffer-name)
    (kill-buffer org-supertag-relation-manage--buffer-name))
  
  (with-current-buffer (get-buffer-create org-supertag-relation-manage--buffer-name)
    (org-supertag-relation-mode)
    (setq-local org-supertag-relation--current-tag tag-id)
    (org-supertag-relation--refresh-display))
  
  (let ((display-buffer-alist
         '(( "\\*Org-Supertag Relation\\*"
            (display-buffer-in-side-window)
            (side . right)
            (window-width . 0.5)
            (slot . 0)))))
    (pop-to-buffer org-supertag-relation-manage--buffer-name)))

(defun org-supertag-relation--refresh-display ()
  "Refresh the relation management interface display."
  (when (get-buffer org-supertag-relation-manage--buffer-name)
    (with-current-buffer org-supertag-relation-manage--buffer-name
      (let ((inhibit-read-only t)
            (current-point (point)))
        (erase-buffer)
        (let ((tag-name (org-supertag-tag-get-name-by-id org-supertag-relation--current-tag)))
          (insert (propertize (format "Tag Relation Management - %s\n\n" tag-name) 'face '(:height 1.5 :weight bold))))

        ;; Existing relations
        (insert (propertize "Existing Relations (Manually set):\n" 'face 'org-level-2))
        (let* ((all-relations (org-supertag-relation-get-all org-supertag-relation--current-tag))
               (manual-relations (cl-remove-duplicates
                                (cl-remove-if
                                 (lambda (rel)
                                   (or (eq (plist-get rel :relation-type) 'cooccurrence)
                                       (equal (plist-get rel :to) org-supertag-relation--current-tag)))
                                 all-relations)
                                :test #'equal)))
          (if manual-relations
              (dolist (rel manual-relations)
                (let* ((other-tag-id (plist-get rel :to))
                       (other-tag-name (org-supertag-tag-get-name-by-id other-tag-id))
                       (rel-type (plist-get rel :type))
                       (is-complementary (org-supertag-relation-has-complement-p rel-type))
                       (relation-symbol (if is-complementary "◎" "→"))
                       (relation-face (if is-complementary '(:foreground "green") '(:foreground "blue")))
                       (remove-button-text (propertize "[-]" 'face '(:foreground "red"))))
                  (insert "")
                  (insert-text-button remove-button-text
                                    'action 'org-supertag-relation--remove-button-action
                                    'other-tag-id other-tag-id
                                    'follow-link t
                                    'help-echo "Click or press RET to remove this relation")
                  (insert " ")
                  (insert (propertize relation-symbol 'face relation-face))
                  (insert (format " %s " (propertize (format "%s" rel-type) 'face '(:foreground "white" :background "black"))))
                  (insert (format " %s" other-tag-name))
                  (when is-complementary
                    (let ((complement-type (org-supertag-relation-get-complement rel-type)))
                      (insert (format " (bidirectional: %s)" complement-type))))
                  (insert "\n")))
            (insert "  No existing relations\n")))

        ;; Co-occurrence relations
        (insert (propertize "\nCo-occurrence Relations (Auto-generated):\n" 'face 'org-level-2))
        (let* ((cooccur-relations (org-supertag-relation-get-all org-supertag-relation--current-tag 'cooccurrence)))
          (if cooccur-relations
              (dolist (rel cooccur-relations)
                (let* ((other-tag-id (plist-get rel :to))
                       (other-tag-name (org-supertag-tag-get-name-by-id other-tag-id))
                       ;; Get strength from the original link data
                       (strength (plist-get rel :strength))
                       (strength-text (format "%.2f" strength))
                       (strength-face (cond
                                      ((>= strength 0.7) 'shadow)
                                      ((>= strength 0.4) 'shadow)
                                      (t 'shadow))))
                  (insert "")
                  (insert-text-button (propertize "[+]" 'face '(:foreground "green"))
                                    'action 'org-supertag-relation--convert-cooccurrence-to-explicit
                                    'from-tag-id org-supertag-relation--current-tag
                                    'to-tag-id other-tag-id
                                    'follow-link t
                                    'help-echo "Click or press RET to convert this co-occurrence to an explicit relation")
                  (insert " ")
                  (insert (propertize "⋈" 'face '(:weight bold :foreground "green")))
                  (insert (format " %s " (propertize "cooccurrence" 'face '(:foreground "white" :background "black"))))
                  (insert (format " %s " other-tag-name))
                  (insert (propertize (format "(strength: %s)" strength-text) 'face strength-face))
                  (insert "\n")))
            (insert "  No co-occurrence relations found\n")))

        ;; Shortcuts help moved to bottom
        (insert (propertize "\nShortcuts:\n" 'face 'org-level-2))
        (insert "[a] - Batch add relations to other tags\n")
        (insert "[RET] on a line to Remove an existing relation\n")
        (insert "[q] - Quit    [r] - Refresh\n")

        ;; Relation Types help moved to bottom
        (insert (propertize "\nRelation Types:\n" 'face 'org-level-2))
        (insert " " (propertize "◎" 'face '(:foreground "green")) " - Bidirectional (adds reverse relation automatically)\n")
        (insert " " (propertize "→" 'face '(:foreground "blue")) " - Unidirectional\n\n")

        (goto-char current-point)))))

;;----------------------------------------------------------------------
;; Helper Functions
;;----------------------------------------------------------------------
(defun org-supertag-tag-get-name-by-id (tag-id)
  "Get the name of the tag with the given ID.
TAG-ID: The ID of the tag."
  (when tag-id
    (let ((entity (gethash tag-id org-supertag-db--object)))
      (when entity
        (plist-get entity :name)))))

(defun org-supertag-relation--get-types-in-group (group)
  "Get the types of relations in the specified group.
GROUP: The group symbol, such as 'default, 'knowledge, etc."
  (cdr (assoc group org-supertag-relation-groups)))

(defun org-supertag-relation--type-in-group-p (rel-type group)
  "Check if the relation type belongs to the specified group.
REL-TYPE: The relation type symbol, such as 'default, 'knowledge, etc.
GROUP: The group symbol, such as 'default, 'knowledge, etc."
  (memq rel-type (org-supertag-relation--get-types-in-group group)))

;;----------------------------------------------------------------------
;; Main Functions
;;----------------------------------------------------------------------  

(defvar org-supertag-relation-mode-map
  (let ((map (make-sparse-keymap)))
      (define-key map (kbd "a") 'org-supertag-relation-batch-add)
      (define-key map (kbd "d") 'org-supertag-relation-remove)
      (define-key map (kbd "r") 'org-supertag-relation-refresh-display)
      (define-key map (kbd "q") 'org-supertag-relation-quit)
      (define-key map (kbd "n") 'org-supertag-relation--next-line)
      (define-key map (kbd "p") 'org-supertag-relation--prev-line)
      (define-key map (kbd "+") 'org-supertag-relation--convert-cooccurrence-to-explicit)
      (define-key map (kbd "RET") 'org-supertag-relation--select-current-line)
  map))

(define-derived-mode org-supertag-relation-mode special-mode "Org-Supertag-Relation"
  :group 'org-supertag
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq header-line-format
        (propertize " Org-Supertag Relation Management" 'face '(:weight bold))))

(defun org-supertag-relation-batch-add ()
  "Batch add relations from the current tag to multiple other tags."
  (interactive)
  (let* ((tag-id org-supertag-relation--current-tag)
         (tag-name (org-supertag-tag-get-name-by-id tag-id)))
    (if (not tag-id)
        (user-error "Current tag not set. Please re-open the relation view.")
      (let* ((all-tags (remove tag-name (org-supertag-get-all-tags)))
             (target-names (completing-read-multiple (format "Batch relate '%s' to: " tag-name) all-tags nil t)))
        (if (not target-names)
            (message "No target tags selected.")
          (let* ((rel-choices (org-supertag-relation--get-relation-type-choices))
                 (choice (completing-read "Select relation type: " rel-choices nil t))
                 (rel-type (org-supertag-relation--get-type-from-choice choice))
                 (added-count 0))
            (dolist (target-name target-names)
              (let ((other-tag-id (org-supertag-tag-get-id-by-name target-name)))
                (when (and other-tag-id rel-type)
                  (if (org-supertag-relation-has-complement-p rel-type)
                      (org-supertag-relation-add-with-complement tag-id other-tag-id rel-type)
                    (org-supertag-relation-add-relation tag-id other-tag-id rel-type))
                  (cl-incf added-count))))
            (org-supertag-relation--refresh-display)
            (message "Batch added %d relations to '%s'." added-count tag-name)))))))

(defun org-supertag-relation-remove ()
  "Remove the relation between the current tag and the selected tag."
  (interactive)
  (org-supertag-relation-remove-relation-interactive))

(defun org-supertag-relation-refresh ()
  (interactive)
  (org-supertag-relation--refresh-display))

(defun org-supertag-relation-quit ()
  "Close the relation management buffer and its window."
  (interactive)
  (let ((buffer (get-buffer org-supertag-relation-manage--buffer-name)))
    (when buffer
      (kill-buffer buffer)
      (delete-window))))


(defun org-supertag-relation--convert-cooccurrence-to-explicit (button)
  "Convert a co-occurrence relation to an explicit relation."
  (interactive)
  (let* ((from-tag-id (button-get button 'from-tag-id))
         (to-tag-id (button-get button 'to-tag-id))
         (rel-choices (org-supertag-relation--get-relation-type-choices))
         (selected-rel-type-str (completing-read "Select explicit relation type: " rel-choices nil t))
         (selected-rel-type (org-supertag-relation--get-type-from-choice selected-rel-type-str)))

    (when (and from-tag-id to-tag-id selected-rel-type)
      ;; 1. Add the new explicit relation
      (org-supertag-relation-add-relation from-tag-id to-tag-id selected-rel-type)

      ;; 2. Remove the co-occurrence relation
      (org-supertag-db-unlink :tag-tag from-tag-id to-tag-id)

      ;; 3. Refresh the display
      (org-supertag-relation--refresh-display)
      (message "Co-occurrence relation from %s to %s converted to explicit %s relation." 
               (org-supertag-tag-get-name-by-id from-tag-id)
               (org-supertag-tag-get-name-by-id to-tag-id)
               selected-rel-type))))

(defun org-supertag-relation--remove-button-action (button)
  "Handle the action of clicking the remove relation button."
  (let* ((tag-id org-supertag-relation--current-tag)
         (other-tag-id (button-get button 'other-tag-id))
         ;; Get the relation type from the existing relation
         (rel (car (org-supertag-relation-get tag-id other-tag-id)))
         (rel-type (and rel (plist-get rel :type))))
    (when (and tag-id other-tag-id rel-type)
      (if (org-supertag-relation-has-complement-p rel-type)
          (org-supertag-relation-remove-with-complement tag-id other-tag-id rel-type)
        (org-supertag-relation-remove-relation tag-id other-tag-id rel-type))
      (org-supertag-relation--refresh-display)
      (message "Relation is removed: %s -[%s]-> %s" 
               (org-supertag-tag-get-name-by-id tag-id)
               rel-type
               (org-supertag-tag-get-name-by-id other-tag-id)))))

(defun org-supertag-relation--next-line ()
  "Move to the next line."
  (interactive)
  (with-current-buffer org-supertag-relation-manage--buffer-name
    (forward-line)
    (beginning-of-line)))

(defun org-supertag-relation--prev-line ()
  "Move to the previous line."
  (interactive)
  (with-current-buffer org-supertag-relation-manage--buffer-name
    (forward-line -1)
    (beginning-of-line)))

(defun org-supertag-relation--select-current-line ()
  "Select the button on the current line."
  (interactive)
  (with-current-buffer org-supertag-relation-manage--buffer-name
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward "\\(\[-\\]\\|\[Select\]\\)" (line-end-position) t)
        (let ((button (button-at (match-beginning 0))))
          (when button
            (push-button button)))))))

(defun org-supertag-relation-refresh-display ()
  "Interactive wrapper for org-supertag-relation--refresh-display."
  (interactive)
  (org-supertag-relation--refresh-display))
    
(defun org-supertag-relation-get-complement (relation-type)
  "Get the complementary relationship of a relation type.
RELATION-TYPE: The relation type.
Returns the complementary relationship type, or nil if none is found."
  (or (cdr (assq relation-type org-supertag-relation-complementary-pairs))
      (car (rassq relation-type org-supertag-relation-complementary-pairs))))

(defun org-supertag-relation-has-complement-p (relation-type)
  "Check if a relation type has a complementary relationship.
RELATION-TYPE: The relation type.
Returns t if a complementary relationship is found, otherwise nil."
  (not (null (org-supertag-relation-get-complement relation-type))))

(defun org-supertag-relation-add-with-complement (tag-id other-tag-id rel-type)
  "Add a relation of type REL-TYPE between TAG-ID and OTHER-TAG-ID.
If REL-TYPE has a complement in `org-supertag-relation-complementary-pairs',
also add the complementary relation from OTHER-TAG-ID to TAG-ID."
  (let ((complement (org-supertag-relation-get-complement rel-type)))
    (org-supertag-relation-add-relation tag-id other-tag-id rel-type)
    (when complement
      (org-supertag-relation-add-relation other-tag-id tag-id complement))))

(defun org-supertag-relation-remove-with-complement (tag-id other-tag-id rel-type)
  "Remove a relation of type REL-TYPE between TAG-ID and OTHER-TAG-ID.
If REL-TYPE has a complement in `org-supertag-relation-complementary-pairs',
also remove the complementary relation from OTHER-TAG-ID to TAG-ID."
  (let ((complement (org-supertag-relation-get-complement rel-type)))
    (org-supertag-relation-remove-relation tag-id other-tag-id rel-type)
    (when complement
      (org-supertag-relation-remove-relation other-tag-id tag-id complement))))

(defun org-supertag-relation-unselect-all-tags ()
  "Unselect all tags."
  (interactive)
  (setq org-supertag-relation--selected-tags nil)
  (org-supertag-relation--refresh-display)
  (message "Unselected all tags"))

;;----------------------------------------------------------------------
;; Contextual and Batch Relation Management (New Features)
;;----------------------------------------------------------------------

(defun org-supertag-relation-toggle-tag-selection-at-point ()
  "Toggle selection for the tag on the current line in the relation view."
  (interactive)
  (with-current-buffer org-supertag-relation-manage--buffer-name
    (let ((button (save-excursion
                    (beginning-of-line)
                    (when (re-search-forward "\\[Mark\\]" (line-end-position) t)
                      (button-at (match-beginning 0))))))
      (when button
        (org-supertag-relation-toggle-tag-selection button)))))


;;----------------------------------------------------------------------
;; Debug Functions
;;----------------------------------------------------------------------

(defun org-supertag-relation-debug-show-all-relations ()
  "Debug function to show all relations in the database."
  (interactive)
  (let ((tag-relations '())
        (tag-tag-relations '()))
    ;; Collect all relations
    (maphash
     (lambda (key value)
       (cond
        ((string-prefix-p ":tag-relation:" key)
         (push (cons key value) tag-relations))
        ((string-prefix-p ":tag-tag:" key)
         (push (cons key value) tag-tag-relations))))
     org-supertag-db--link)
    
    ;; Display results
    (with-output-to-temp-buffer "*Org-Supertag Relations Debug*"
      (princ "=== Tag Relations (:tag-relation: prefix) ===\n")
      (if tag-relations
          (dolist (rel tag-relations)
            (princ (format "Key: %s\n" (car rel)))
            (princ (format "Value: %S\n\n" (cdr rel))))
        (princ "No tag relations found\n"))
      
      (princ "\n=== Tag-Tag Relations (:tag-tag: prefix) ===\n")
      (if tag-tag-relations
          (dolist (rel tag-tag-relations)
            (princ (format "Key: %s\n" (car rel)))
            (princ (format "Value: %S\n\n" (cdr rel))))
        (princ "No tag-tag relations found\n")))))

(provide 'org-supertag-relation)

;;; org-supertag-relation.el ends here

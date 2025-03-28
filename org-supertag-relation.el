;;; org-supertag-relation.el --- Tag relationship management for org-supertag -*- lexical-binding: t; -*-

;; This file provides functionality for managing relationships between tags
;; in org-supertag.  It supports recording, querying, and analyzing tag
;; co-occurrence relationships.

;;; Code:

(require 'ht)
(require 'cl-lib)
(require 'org-element)
(require 'org-supertag-db)
(require 'org-supertag-sim-epc nil t)  ;; 如果可用则加载，否则静默跳过

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
  "从格式化的选项CHOICE中提取关系类型。"
  (let ((selected-type (car (split-string choice " - "))))
    (intern selected-type)))

(defun org-supertag-relation-add-relation (from-tag to-tag rel-type &optional strength)
  "Add a tag relation.
FROM-TAG: Source tag ID
TO-TAG: Target tag ID
REL-TYPE: Relation type
STRENGTH: Relation strength (optional, default is 1.0)"
  (interactive
   (let* ((from-tag (completing-read "Source tag: " (org-supertag-get-all-tags) nil t))
          (to-tag (completing-read "Target tag: " (org-supertag-get-all-tags) nil t))
          (rel-choices (org-supertag-relation--get-relation-type-choices))
          (rel-type (org-supertag-relation--get-type-from-choice 
                    (completing-read "Relation type: " rel-choices nil t))))
     (list 
      (org-supertag-tag-get-id-by-name from-tag)
      (org-supertag-tag-get-id-by-name to-tag)
      rel-type 
      1.0)))
  ;; Check if the tags are valid
  (cond
   ((not from-tag)
    (user-error "Source tag cannot be empty"))
   ((not to-tag)
    (user-error "Target tag cannot be empty"))
   ((equal from-tag to-tag)
    (user-error "Source tag and target tag cannot be the same")))
  
  (let* ((from-id (if (org-supertag-db-exists-p from-tag)
                      from-tag
                    (org-supertag-tag-get-id-by-name from-tag)))
         (to-id (if (org-supertag-db-exists-p to-tag)
                   to-tag
                 (org-supertag-tag-get-id-by-name to-tag)))
         ;; Include rel-type in the relation ID to support multiple relations
         (rel-id (format ":tag-relation:%s->%s:%s" from-id to-id rel-type)))
    
    ;; Check if relation already exists
    (when (gethash rel-id org-supertag-db--link)
      (user-error "Relation already exists: %s -[%s]-> %s"
                  (org-supertag-tag-get-name-by-id from-id)
                  rel-type
                  (org-supertag-tag-get-name-by-id to-id)))
    
    ;; Add the relation
    (let ((props (list :from from-id
                      :to to-id
                      :type rel-type
                      :strength (or strength 1.0)
                      :created-at (current-time))))
      (puthash rel-id props org-supertag-db--link)
         (org-supertag-db-emit 'link:created :tag-relation from-id to-id props)
      (org-supertag-db--mark-dirty)
      (org-supertag-db--schedule-save)
      (message "Added relation: %s -[%s]-> %s" 
               (org-supertag-tag-get-name-by-id from-id)
               rel-type 
               (org-supertag-tag-get-name-by-id to-id)))))

(defun org-supertag-relation-remove-relation (from-tag to-tag &optional rel-type)
  "Remove the relation between FROM-TAG and TO-TAG.
If REL-TYPE is provided, only remove that specific relation type,
otherwise remove all relations between the tags."
  (interactive
   (let* ((from-tag (completing-read "Source tag: " (org-supertag-get-all-tags) nil t))
          (to-choices (mapcar
                     (lambda (rel)
                       (format "%s (%s)" (car rel) (cdr rel)))
                     (org-supertag-relation-get-all-from from-tag)))
          (choice (completing-read "Relation to remove: " to-choices nil t))
          (to-tag (car (split-string choice " ("))))
     (list from-tag to-tag)))
  
  (when (and from-tag to-tag)
    (let* ((from-id (if (org-supertag-db-exists-p from-tag)
                       from-tag
                     (org-supertag-tag-get-id-by-name from-tag)))
           (to-id (if (org-supertag-db-exists-p to-tag)
                     to-tag
                   (org-supertag-tag-get-id-by-name to-tag)))
           (from-name (org-supertag-tag-get-name-by-id from-id))
           (to-name (org-supertag-tag-get-name-by-id to-id)))

      ;; If rel-type is provided, only remove that specific relation
      (if rel-type
          (let ((rel-id (format ":tag-relation:%s->%s:%s" from-id to-id rel-type)))
            (when (gethash rel-id org-supertag-db--link)
              (remhash rel-id org-supertag-db--link)
              ;; 触发 link:removed 事件
              (org-supertag-db-emit 'link:removed :tag-relation from-id to-id)))
        ;; Otherwise, remove all relations between these tags
        (let ((removed-count 0))
          (maphash
           (lambda (key _)
             (when (and (string-prefix-p ":tag-relation:" key)
                       (string-match (format ":tag-relation:%s->%s:" from-id to-id) key))
               (remhash key org-supertag-db--link)
               ;; 触发 link:removed 事件
               (org-supertag-db-emit 'link:removed :tag-relation from-id to-id)
               (cl-incf removed-count)))
           org-supertag-db--link)))
      
      ;; 标记数据库为脏
      (org-supertag-db--mark-dirty)
      ;; 调度保存
      (org-supertag-db--schedule-save)
      (message "Removed relation: %s -> %s" from-name to-name))))

(defun org-supertag-relation-get (from-tag to-tag &optional rel-type)
  "Get the relation data between FROM-TAG and TO-TAG.
If REL-TYPE is provided, get the specific relation type,
otherwise return all relations between the tags.
Returns a list of relation property lists."
  (when (and from-tag to-tag)
    (let ((result nil)
          (from-id (if (org-supertag-db-exists-p from-tag)
                      from-tag
                    (org-supertag-tag-get-id-by-name from-tag)))
          (to-id (if (org-supertag-db-exists-p to-tag)
                    to-tag
                  (org-supertag-tag-get-id-by-name to-tag))))
      (maphash
       (lambda (key props)
         (when (and (string-prefix-p ":tag-relation:" key)
                   (string-match (format ":tag-relation:%s->%s:\\(.+\\)" from-id to-id) key))
           (when (or (null rel-type)
                    (eq rel-type (plist-get props :type)))
             (push props result))))
       org-supertag-db--link)
      result)))

(defun org-supertag-relation-get-all-from (tag-id)
  "Get all relations from the TAG-ID.
Return the list of (other-tag . rel-type)."
  (if (null tag-id)
      nil
    (let (result)
      (maphash
       (lambda (rel-id props)
         (when (and (string-prefix-p ":tag-relation:" rel-id)
                   (string-match (format ":tag-relation:%s->\\([^:]+\\):\\(.+\\)" tag-id) rel-id))
           (let ((to-tag (match-string 1 rel-id))
                 (rel-type (plist-get props :type)))
             (push (cons to-tag rel-type) result))))
       org-supertag-db--link)
      result)))

(defun org-supertag-relation-get-all-to (tag-id)
  "Get all relations to the TAG-ID.
Return the list of (other-tag . rel-type)."
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

(defun org-supertag-relation-get-all (tag-id)
  "Get all relations involving the TAG-ID.
Return the list of relations, each relation is a plist with :from, :to and :type keys.
Only returns outgoing relations (where TAG-ID is the :from tag)."
  (if (null tag-id)
      nil
    (let (result)
      (maphash
       (lambda (rel-id props)
         (when (and (string-prefix-p ":tag-relation:" rel-id)
                   (equal (plist-get props :from) tag-id))  
           (push props result)))
       org-supertag-db--link)
      result)))

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

(defvar org-supertag-relation--recommendations-cache (make-hash-table :test 'equal)
  "Cache the recommendation results for each tag.
The key is the tag ID, and the value is the recommendation list.")

(defvar org-supertag-relation--updating-recommendations nil
  "Mark whether the recommendations are being updated to prevent duplicate calls.")

(defvar org-supertag-relation-manage--buffer-name "*Org-Supertag Relation*"
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
  (clrhash org-supertag-relation--recommendations-cache)
  (let* ((tags (org-supertag-get-all-tags))
         (tag-name (completing-read "Select a tag to manage relations: " tags nil t)))
    (if (or (null tag-name) (string-empty-p tag-name))
        (user-error "No valid tag selected")
      (let ((tag-id (org-supertag-tag-get-id-by-name tag-name)))
        (if (null tag-id)
            (user-error "No tag ID found for: %s" tag-name)
          (org-supertag-relation--show-management-interface tag-id))))))

(defun org-supertag-tag-get-id-by-name (tag-name)
  "Get the tag ID by TAG-NAME.
TAG-NAME: Tag name."
  (when (and tag-name (not (string-empty-p tag-name)))
    (let (result)
      (maphash
       (lambda (id entity)
         (when (and (eq (plist-get entity :type) :tag)
                   (string= (plist-get entity :name) tag-name))
           (setq result id)))
       org-supertag-db--object)
      result)))

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
         '(("\\*Org-Supertag Relation\\*"
            (display-buffer-in-side-window)
            (side . right)
            (window-width . 0.5)
            (slot . 0)))))
    (pop-to-buffer org-supertag-relation-manage--buffer-name)))

(defun org-supertag-relation--refresh-display ()
  "刷新关系管理界面显示。"
  (when (get-buffer org-supertag-relation-manage--buffer-name)
    (with-current-buffer org-supertag-relation-manage--buffer-name
      (let ((inhibit-read-only t)
            (current-point (point)))
        (erase-buffer)
        (let ((tag-name (org-supertag-tag-get-name-by-id org-supertag-relation--current-tag)))
          (insert (propertize (format "Tag Relation Management - %s\n\n" tag-name)
                             'face '(:height 1.5 :weight bold))))

        (insert (propertize "Shortcuts:\n" 'face '(:weight bold)))
        (insert " Navigation:\n")
        (insert "   [n] - Next    [p] - Previous\n")
        (insert " Operations:\n")
        (insert "   [RET] - Select/Remove Item\n")
        (insert "   [q] - Quit    [r] - Refresh\n")
        (insert "   [g] - Find by Group    [f] - Find by Relation\n")
        (insert "   [i] - Show Isolated Tags\n\n")
        
        ;; Display complementary relation information
        (insert (propertize "Relation Types:\n" 'face '(:weight bold)))
        (insert " " (propertize "◎" 'face '(:foreground "green")) " - bidirectional relation (Automatically adds reverse relation)\n")
        (insert " " (propertize "→" 'face '(:foreground "blue")) " - unidirectional relation\n\n")

        ;; 显示共现关系
        (insert (propertize "\n共现标签:\n" 'face '(:weight bold)))
        (let* ((all-relations (org-supertag-relation-get-all org-supertag-relation--current-tag))
               ;; 过滤共现关系
               (relations (cl-remove-duplicates
                         (cl-remove-if
                          (lambda (rel)
                            (or (not (eq (plist-get rel :type) 'cooccurrence))
                                (equal (plist-get rel :to) org-supertag-relation--current-tag)))
                          all-relations)
                         :test (lambda (a b)
                                (and (equal (plist-get a :to) (plist-get b :to))
                                     (eq (plist-get a :type) (plist-get b :type)))))))
          (if relations
              (dolist (rel relations)
                (let* ((other-tag-id (plist-get rel :to))
                       (other-tag-name (org-supertag-tag-get-name-by-id other-tag-id))
                       (strength (or (plist-get rel :strength) 0.0))
                       (strength-display (format " %.2f" strength))
                       (remove-button-text (propertize "[-]" 'face '(:foreground "red"))))
                  (insert " ")
                  (insert-text-button remove-button-text
                                    'action 'org-supertag-relation--remove-button-action
                                    'other-tag-id other-tag-id
                                    'follow-link t
                                    'help-echo "Click to remove this relation")
                  (insert (format " %s " 
                                (propertize "cooccurrence" 'face '(:foreground "white" :background "black"))))
                  (insert (format " %s%s\n" 
                                other-tag-name
                                (propertize strength-display 'face '(:foreground "gray50"))))))
            (insert "  (No cooccurrence relations)\n")))

        ;; 显示现有关系
        (insert (propertize "\n现有关系:\n" 'face '(:weight bold)))
        (let* ((all-relations (org-supertag-relation-get-all org-supertag-relation--current-tag))
               ;; 过滤非共现关系
               (relations (cl-remove-duplicates
                         (cl-remove-if
                          (lambda (rel)
                            (or (eq (plist-get rel :type) 'cooccurrence)
                                (equal (plist-get rel :to) org-supertag-relation--current-tag)))
                          all-relations)
                         :test (lambda (a b)
                                (and (equal (plist-get a :to) (plist-get b :to))
                                     (eq (plist-get a :type) (plist-get b :type)))))))
          (if relations
              (dolist (rel relations)
                (let* ((other-tag-id (plist-get rel :to))
                       (other-tag-name (org-supertag-tag-get-name-by-id other-tag-id))
                       (rel-type (plist-get rel :type))
                       (is-complementary (org-supertag-relation-has-complement-p rel-type))
                       (relation-symbol (if is-complementary "◎" "→"))
                       (relation-face (if is-complementary '(:foreground "green") '(:foreground "blue")))
                       (remove-button-text (propertize "[-]" 'face '(:foreground "red"))))
                  (insert " ")
                  (insert-text-button remove-button-text
                                    'action 'org-supertag-relation--remove-button-action
                                    'other-tag-id other-tag-id
                                    'follow-link t
                                    'help-echo "Click to remove this relation")
                  (insert " ")
                  (insert (propertize relation-symbol 'face relation-face))
                  (insert (format " %s " 
                                (propertize (format "%s" rel-type) 'face '(:foreground "white" :background "black"))))
                  (insert (format " %s" other-tag-name))
                  
                  ;; Display bidirectional relationship
                  (when is-complementary
                    (let ((complement-type (org-supertag-relation-get-complement rel-type)))
                      (insert (format " (bidirectional: %s)" complement-type))))
                  
                  (insert "\n")))
            (insert "  (No existing relations)\n")))
        
        ;; Update the recommended tag area
        (insert "\n")
        (insert (propertize "Recommended similar tags:\n" 'face '(:weight bold)))
        (if (gethash org-supertag-relation--current-tag org-supertag-relation--recommendations-cache)
            (progn
              (dolist (rec (gethash org-supertag-relation--current-tag org-supertag-relation--recommendations-cache))
                (let* ((tag-name (car rec))
                       (similarity (cdr rec))
                       (tag-id (org-supertag-tag-get-id-by-name tag-name))
                       (is-selected (member tag-id org-supertag-relation--selected-tags))
                       (selection-mark (if is-selected 
                                          (propertize "[✓]" 'face '(:foreground "green"))
                                        "[ ]")))
                  (insert " ")
                  (insert selection-mark)
                  (insert " ")
                  (insert-text-button (propertize "[Select]" 'face '(:foreground "green"))
                                    'action 'org-supertag-relation--select-tag-action
                                    'other-tag-id tag-id
                                    'follow-link t
                                    'help-echo "Click to select this tag to establish a relationship")
                  (insert " ")
                  (insert-text-button (propertize "[Mark]" 'face '(:foreground "blue"))
                                    'action 'org-supertag-relation-toggle-tag-selection
                                    'other-tag-id tag-id
                                    'follow-link t
                                    'help-echo "Mark/unmark this tag for batch operations")
                  (insert (format " %s (%.2f)\n" tag-name similarity)))))
          ;; No recommendations, trigger recommendation search
          (progn
            (insert "  (Getting similar tags...)\n")
            (let* ((all-tags (org-supertag-get-all-tags))
                   (other-tag-ids (mapcar #'org-supertag-tag-get-id-by-name 
                                        (remove (org-supertag-tag-get-name-by-id org-supertag-relation--current-tag) all-tags))))
              (run-with-timer 0 nil
                            (lambda ()
                              (org-supertag-relation--get-recommendations 
                               org-supertag-relation--current-tag other-tag-ids))))))
        
        (insert "\n")
        (insert (propertize "Explanation: " 'face '(:weight bold)))
        (insert "Click [Select] to add a recommended relationship, click [-] to remove an existing relationship\n")
        (insert "When selecting a relationship type with a [bidirectional] mark, the system will automatically add a complementary relationship\n")
        
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

(defun org-supertag-relation-find-tags-by-group (tag-id group &optional direction)
  "Find all tags related to the specified tag through a specific group relation.
TAG-ID: The ID of the tag.
GROUP: The group symbol, such as 'default, 'knowledge, etc.
DIRECTION: The direction, nil for both, 'from for from, 'to for to."
  (let* ((all-rels (cond
                   ((eq direction 'from) (org-supertag-relation-get-all-from tag-id))
                   ((eq direction 'to) (org-supertag-relation-get-all-to tag-id))
                   (t (org-supertag-relation-get-all tag-id))))
         (group-types (org-supertag-relation--get-types-in-group group))
         (filtered-rels (cl-remove-if-not
                        (lambda (rel)
                          (memq (plist-get rel :type) group-types))
                        all-rels))
         (result '()))
    (dolist (rel filtered-rels)
      (let ((other-id (if (eq direction 'from)
                          (plist-get rel :to)
                        (if (eq direction 'to)
                            (plist-get rel :from)
                          (if (string= (plist-get rel :from) tag-id)
                              (plist-get rel :to)
                            (plist-get rel :from))))))
        (when other-id
          (push other-id result))))
    (cl-remove-duplicates result :test 'string=)))

(defun org-supertag-relation-find-related-by-group (group)
  "Find related tags by group.
GROUP: The group symbol, such as 'default, 'knowledge, etc."
  (interactive
   (list (completing-read "Choose group: "
                         (mapcar (lambda (g) (symbol-name (car g)))
                                 org-supertag-relation-groups)
                         nil t)))
  (let* ((tag-id org-supertag-relation--current-tag)
         (group-sym (intern group)))
    (if (not tag-id)
        (user-error "Current tag not set. Please re-select tag")
      (let* ((related-ids (org-supertag-relation-find-tags-by-group tag-id group-sym))
             (related-names (mapcar 'org-supertag-tag-get-name-by-id related-ids)))
        (if (null related-names)
            (message "Can't find related tags through %s group relation" group)
          (message "Related tags: %s" (mapconcat 'identity related-names ", ")))))))

(defun org-supertag-relation-find-by-group ()
  "Find related tags by group."
  (interactive)
  (let ((groups (mapcar (lambda (g) (symbol-name (car g))) org-supertag-relation-groups)))
    (if (null groups)
        (message "No relation groups defined")
      (let* ((group (completing-read "Select relation group: " groups nil t)))
        (org-supertag-relation-find-related-by-group group)))))

(defun org-supertag-relation-find-related-tags ()
  "Find related tags to the current tag."
  (interactive)
  (let ((tag-id org-supertag-relation--current-tag))
    (if (not tag-id)
        (user-error "Current tag not set. Please re-select tag")
      (let ((relations (org-supertag-relation-get-all tag-id))
            (related-tags '()))
        (dolist (rel relations)
          (when rel ; make sure the relation is not nil
            (let* ((other-tag-id (plist-get rel :to))
                   (rel-type (plist-get rel :type)))
              (when other-tag-id ; make sure the other tag id is not nil
                (let ((other-tag-name (org-supertag-tag-get-name-by-id other-tag-id)))
                  (when other-tag-name ; make sure the other tag name is not nil
                    (push (cons other-tag-name rel-type) related-tags)))))))
        (if (null related-tags)
            (message "Can't find related tags")
          (with-output-to-temp-buffer "*Org-Supertag Relations*"
            (princ "Related tags:\n\n")
            (dolist (tag related-tags)
              (princ (format "- %s (%s)\n" (car tag) (cdr tag))))))))))

(defvar org-supertag-relation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'org-supertag-relation-add)
    (define-key map (kbd "d") 'org-supertag-relation-remove)
    (define-key map (kbd "r") 'org-supertag-relation-refresh)
    (define-key map (kbd "q") 'org-supertag-relation-quit)
    (define-key map (kbd "g") 'org-supertag-relation-find-by-group)
    (define-key map (kbd "f") 'org-supertag-relation-find-related-tags)
    (define-key map (kbd "i") 'org-supertag-relation-show-isolated-tags)
    ;; Navigation keys (Emacs style)
    (define-key map (kbd "n") 'org-supertag-relation--next-line)
    (define-key map (kbd "p") 'org-supertag-relation--prev-line)
    (define-key map (kbd "RET") 'org-supertag-relation--select-current-line)
    ;; 批量操作键绑定
    (define-key map (kbd "m") 'org-supertag-relation-toggle-tag-selection)
    (define-key map (kbd "M") 'org-supertag-relation-select-all-tags)
    (define-key map (kbd "u") 'org-supertag-relation-unselect-all-tags)
    (define-key map (kbd "b") 'org-supertag-relation-batch-add-selected)
    map))

(define-derived-mode org-supertag-relation-mode special-mode "Org-Supertag-Relation"
  :group 'org-supertag
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq header-line-format 
        (propertize " Org-Supertag Relation Management" 'face '(:weight bold))))

(defun org-supertag-relation-add (void-ok)
  "Add a relation to the current tag.
If VOID-OK is non-nil, allow missing tag IDs."
  (interactive "P")
  (let* ((tag-id org-supertag-relation--current-tag))
    (if (and (not tag-id) (not void-ok))
        (user-error "Current tag not set. Please re-select tag")
      (let* ((all-tags (org-supertag-get-all-tags))
             (other-tag-name (completing-read "Select related tag: " all-tags nil t))
             (other-tag-id (org-supertag-tag-get-id-by-name other-tag-name)))
        (if (not other-tag-id)
            (user-error "Can't find tag with ID: %s" other-tag-name)
          (unless tag-id
            (setq tag-id (org-supertag-tag-get-id-by-name
                         (completing-read "Select source tag: " all-tags nil t))))
        
          (let* ((rel-choices (org-supertag-relation--get-relation-type-choices))
                 (choice (completing-read "Select relation type: " rel-choices nil t))
                 (rel-type (org-supertag-relation--get-type-from-choice choice)))
            (when (and tag-id other-tag-id rel-type)
              (if (org-supertag-relation-has-complement-p rel-type)
                  (org-supertag-relation-add-with-complement tag-id other-tag-id rel-type)
                (org-supertag-relation-add-relation tag-id other-tag-id rel-type))
              (org-supertag-relation--refresh-display)
              (message "Added relation: %s %s %s" 
                       (org-supertag-tag-get-name-by-id tag-id)
                       rel-type 
                       (org-supertag-tag-get-name-by-id other-tag-id)))))))))

(defun org-supertag-relation-remove ()
  "Remove the relation between the current tag and the selected tag."
  (interactive)
  (let ((tag-id org-supertag-relation--current-tag))
    (if (not tag-id)
        (user-error "Current tag not set. Please re-select tag")
      (let* ((relations (org-supertag-relation-get-all tag-id))
             (rel-choices nil)
             (rel-to-id-map (make-hash-table :test 'equal)))        
        ;; Create choices and build mapping
        (dolist (rel relations)
          (let* ((other-tag-id (plist-get rel :to))
                 (other-tag-name (when other-tag-id
                                 (or (org-supertag-tag-get-name-by-id other-tag-id)
                                     "(Unnamed Tag)")))
                 (rel-type (plist-get rel :type))
                 (choice (when (and other-tag-name rel-type)
                          (format "%s (%s)" other-tag-name rel-type))))
            (when choice
              (push choice rel-choices)
              (puthash choice other-tag-id rel-to-id-map))))
        ;; Make sure the choices are in reverse order to make it easier to navigate
        (setq rel-choices (nreverse rel-choices))
        (if (null rel-choices)
            (message "There are no relations to remove")
          (let* ((choice (completing-read "Choose relation to remove:" rel-choices nil t))
                 (other-tag-id (gethash choice rel-to-id-map)))
            (if other-tag-id
                (progn
                  (org-supertag-relation-remove-relation tag-id other-tag-id)
                  (org-supertag-relation--refresh-display)
                  (message "Removed relation: %s <-> %s" 
                           (or (org-supertag-tag-get-name-by-id tag-id) tag-id)
                           (or (org-supertag-tag-get-name-by-id other-tag-id) other-tag-id)))
              (message "Can't find target tag ID. Please try again."))))))))

(defun org-supertag-relation-refresh ()
  (interactive)
  (org-supertag-relation--refresh-display))

(defun org-supertag-relation-quit ()
  "关闭关系管理缓冲区。"
  (interactive)
  (kill-buffer org-supertag-relation-manage--buffer-name))

;;------------------------------------------------------------------------
;; Tag recommendation algorithm
;;------------------------------------------------------------------------

(defun org-supertag-relation--get-recommendations (tag-id other-tags)
  "获取标签关系推荐.
TAG-ID: 当前标签 ID
OTHER-TAGS: 其他可能相关的标签列表"
  (if (or (null tag-id)
          (null other-tags)
          org-supertag-relation--updating-recommendations)  ; 防止重复调用
      (gethash tag-id org-supertag-relation--recommendations-cache)  ; 返回缓存的推荐列表
    (let ((tag-name (org-supertag-tag-get-name-by-id tag-id)))
      (when (and tag-name (fboundp 'org-supertag-sim-find-similar))
        (setq org-supertag-relation--updating-recommendations t)
        ;; 使用相似度搜索获取推荐
        (org-supertag-sim-find-similar
         tag-name
         10  ; 获取前10个相似标签
         (lambda (similar-tags)
           ;; 过滤掉已存在的关系
           (let* ((existing-relations (mapcar 
                                     (lambda (rel) (plist-get rel :to))
                                     (org-supertag-relation-get-all tag-id)))
                  (recommendations
                   (cl-remove-if
                    (lambda (tag)
                      (let ((other-id (org-supertag-tag-get-id-by-name (car tag))))
                        (or (null other-id)  ; 忽略不存在的标签
                            (equal other-id tag-id)  ; 忽略自身
                            (member other-id existing-relations))))  ; 忽略已有关系
                    similar-tags)))
             ;; 更新推荐缓存
             (puthash tag-id recommendations org-supertag-relation--recommendations-cache)
             (setq org-supertag-relation--updating-recommendations nil)
             ;; 使用 run-with-timer 延迟刷新显示，避免递归
             (run-with-timer 0.1 nil
                            (lambda ()
                              (when (get-buffer org-supertag-relation-manage--buffer-name)
                                (with-current-buffer org-supertag-relation-manage--buffer-name
                                  (let ((inhibit-read-only t))
                                    (org-supertag-relation--refresh-display))))))))))
      ;; 返回缓存的推荐列表
      (gethash tag-id org-supertag-relation--recommendations-cache))))

(defun org-supertag-relation--format-recommendations ()
  "Format the recommendation list for display.
Only show similar tags without suggesting relation types."
  (when (gethash org-supertag-relation--current-tag org-supertag-relation--recommendations-cache)
    (mapconcat
     (lambda (rec)
       (let* ((tag-name (car rec))
              (similarity (cdr rec)))
         (format "%-30s (%.2f)" tag-name similarity)))
     (gethash org-supertag-relation--current-tag org-supertag-relation--recommendations-cache)
     "\n")))

(defun org-supertag-relation--suggest-relation-type (tag1 tag2)
  "Recommend appropriate relation types for TAG1 and TAG2 based on tag characteristics."
  (when (and tag1 tag2)
    (let* ((cooccur-count (org-supertag-relation--get-cooccurrence-count tag1 tag2)))
      (if (> cooccur-count 0)
          'cooccurrence  ; If there is a cooccurrence record, prioritize recommending 'cooccurrence' relation
        (let* ((relation-types (mapcar #'car org-supertag-relation-types))
               (hash-val (abs (sxhash (concat (format "%s" tag1) (format "%s" tag2)))))
               (idx (mod hash-val (length relation-types))))
          (nth idx relation-types))))))

;; Co-occurrence relation management
(defun org-supertag-relation-record-cooccurrence (node-id tag-id)
  "Record the co-occurrence relationship of TAG-ID on NODE-ID.
This will create a bidirectional relationship between TAG-ID and all other tags on the node."
  (let ((existing-tags (org-supertag-node-get-tags node-id)))
    (dolist (other-tag existing-tags)
      (unless (equal other-tag tag-id)
        ;; Compute the co-occurrence relationship strength based on frequency
        (let* ((freq (org-supertag-relation--get-cooccurrence-count tag-id other-tag))
               (new-freq (1+ freq))
               ;; Apply a sigmoid-like normalization to get diminishing returns
               (norm-factor org-supertag-relation-cooccurrence-normalization-factor)
               (strength (/ new-freq (+ new-freq norm-factor))))
          ;; Store the updated frequency count for future calculations
          (org-supertag-relation--set-cooccurrence-count tag-id other-tag new-freq)
          ;; Create a bidirectional relationship with the calculated strength
          (org-supertag-relation-add-relation tag-id other-tag 'cooccurrence)
          (org-supertag-relation-add-relation other-tag tag-id 'cooccurrence))))))

(defun org-supertag-relation--get-cooccurrence-count (tag1 tag2)
  "Get the number of co-occurrences between TAG1 and TAG2.
If there is no co-occurrence record, return 0."
  (let* ((rel-id-key (format "tag-cooccur:%s:%s" tag1 tag2)))
    (or (org-supertag-db-get-metadata rel-id-key 0) 0)))

(defun org-supertag-relation--set-cooccurrence-count (tag1 tag2 count)
  "Set the co-occurrence count between TAG1 and TAG2 to COUNT."
  (let* ((tags (sort (list tag1 tag2) #'string<))
         (tag1-sorted (car tags))
         (tag2-sorted (cadr tags))
         (rel-id-key1 (format "tag-cooccur:%s:%s" tag1 tag2))
         (rel-id-key2 (format "tag-cooccur:%s:%s" tag2 tag1)))
    ;; Store the count in both directions for easy lookup
    (org-supertag-db-set-metadata rel-id-key1 count)
    (org-supertag-db-set-metadata rel-id-key2 count)))

;; Function to analyze co-occurrence patterns across the entire database
(defun org-supertag-relation-analyze-cooccurrence-patterns ()
  "Analyze co-occurrence patterns across all tags and update relationship strength."
  (interactive)
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
    
    ;; Save the global tag frequency data to metadata
    (org-supertag-db-set-metadata 'tag-frequency-data tag-counts)
    (org-supertag-db-set-metadata 'tag-total-nodes total-nodes)
    (org-supertag-db-set-metadata 'tag-analysis-timestamp (current-time))
    
    ;; Calculate the point mutual information (PMI) for each co-occurrence
    (maphash
     (lambda (key count)
       (let* ((tags (split-string key ":"))
              (tag1 (car tags))
              (tag2 (cadr tags))
              (prob-t1 (/ (float (gethash tag1 tag-counts)) total-nodes))
              (prob-t2 (/ (float (gethash tag2 tag-counts)) total-nodes))
              (prob-t1t2 (/ (float count) total-nodes))
              (pmi (log (/ prob-t1t2 (* prob-t1 prob-t2)) 2))
              (norm-pmi (/ (1+ pmi) 2))  ; Normalize to 0-1 range
              (strength (min 1.0 (max 0.1 norm-pmi))))  ; Limit to 0.1-1.0 range
         
         ;; Update co-occurrence count
         (org-supertag-relation--set-cooccurrence-count tag1 tag2 count)
         
         ;; Store PMI value in metadata for statistical access
         (org-supertag-db-set-metadata (format "tag-pmi:%s:%s" tag1 tag2) norm-pmi)
         
         ;; If strength is meaningful, update the relation with the new strength
         (when (>= strength org-supertag-relation-min-strength)
           (org-supertag-relation-add-relation tag1 tag2 'cooccurrence)
           (org-supertag-relation-add-relation tag2 tag1 'cooccurrence))))
     cooccur-counts)
    
    (message "Co-occurrence analysis completed. Analyzed %d nodes and %d tag pairs."
             total-nodes (hash-table-count cooccur-counts))))

;; Co-occurrence record incremental relation update
(defun org-supertag-relation-update-on-tag-add (node-id tag-id)
  "When NODE-ID adds TAG-ID as a tag, update the co-occurrence relationship.
This is an incremental update feature that only updates affected relations."
  (when org-supertag-relation-enable-incremental-updates
    ;; Get all existing tags on the node (excluding the newly added tag)
    (let ((existing-tags (cl-remove tag-id (org-supertag-node-get-tags node-id) :test #'equal)))
      (when existing-tags
        ;; Update each existing tag
        (dolist (existing-tag existing-tags)
          ;; Get the current co-occurrence count
          (let* ((current-count (org-supertag-relation--get-cooccurrence-count existing-tag tag-id))
                 (new-count (1+ current-count)))
            ;; Update co-occurrence count
            (org-supertag-relation--set-cooccurrence-count existing-tag tag-id new-count)
            
            ;; If co-occurrence is frequent enough, update or create the relation
            (when (>= new-count org-supertag-relation-cooccurrence-normalization-factor)
              (let* ((strength (/ new-count (+ new-count org-supertag-relation-cooccurrence-normalization-factor))))
                (when (>= strength org-supertag-relation-min-strength)
                  (org-supertag-relation-add-relation existing-tag tag-id 'cooccurrence)
                  (org-supertag-relation-add-relation tag-id existing-tag 'cooccurrence)))))))))
    
    ;; Record incremental update
    (org-supertag-db-set-metadata 'tag-incremental-updates-count 
                                 (1+ (org-supertag-db-get-metadata 'tag-incremental-updates-count 0))))

(defun org-supertag-relation-update-on-tag-remove (node-id tag-id)
  "When NODE-ID deletes TAG-ID as a tag, update the co-occurrence relationship.
This is an incremental update feature that only updates affected relations."
  (when org-supertag-relation-enable-incremental-updates
    ;; Get all existing tags on the node (excluding the newly deleted tag)
    (let ((remaining-tags (cl-remove tag-id (org-supertag-node-get-tags node-id) :test #'equal)))
      (when remaining-tags
        ;; If the deleted tag has related tags, update each existing tag
        (dolist (related-tag-id (mapcar #'car (org-supertag-relation-get-all tag-id)))
          (let* ((should-decrement (not (member related-tag-id remaining-tags)))
                 (current-count (org-supertag-relation--get-cooccurrence-count related-tag-id tag-id)))
            
            (when (and should-decrement (> current-count 0))
              (let ((new-count (1- current-count)))
                (org-supertag-relation--set-cooccurrence-count related-tag-id tag-id new-count)
                
                ;; If co-occurrence is not frequent enough, delete the relation
                (when (< new-count (/ org-supertag-relation-cooccurrence-normalization-factor 2))
                  ;; Use a more complex logic to decide whether to delete the relation
                  ;; For simplicity, we only delete the relation when the strength is below a threshold
                  (let ((cur-strength (org-supertag-relation-get-strength related-tag-id tag-id)))
                    (when (and cur-strength (< cur-strength org-supertag-relation-min-strength))  
                      ;; Delete the relation
                      (org-supertag-relation-remove-relation related-tag-id tag-id))))))))
        
        ;; Record incremental update
        (org-supertag-db-set-metadata 'tag-incremental-updates-count 
                                    (1+ (org-supertag-db-get-metadata 'tag-incremental-updates-count 0)))))))

;; Register hooks
(add-hook 'org-supertag-node-tag-added-hook #'org-supertag-relation-update-on-tag-add)
(add-hook 'org-supertag-node-tag-removed-hook #'org-supertag-relation-update-on-tag-remove)

(defun org-supertag-relation-get-strength (tag1 tag2)
  "Get the strength of the relationship between TAG1 and TAG2.
If the relation does not exist, return nil."
  (let ((rel (car (cl-remove-if
                  (lambda (r) (equal (plist-get r :to) tag2))
                  (org-supertag-relation-get-all tag1)))))
    (when rel
      (plist-get rel :strength))))

;; Add a command to display co-occurrence relations
(defun org-supertag-relation-find-cooccurrence-tags ()
  "Find tags co-occurring with the current tag."
  (interactive)
  (let ((tag-id org-supertag-relation--current-tag))
    (if (not tag-id)
        (user-error "Current tag not set. Please re-select tag")
      (let* ((relations (cl-remove-if-not
                        (lambda (rel) (eq (plist-get rel :type) 'cooccurrence))
                        (org-supertag-relation-get-all tag-id)))
             (related-tags '()))
        
        ;; Collect related tags
        (dolist (rel relations)
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

;; Button action handlers for the relation management interface
(defun org-supertag-relation--add-button-action (button)
  "Handle the action of clicking the add relation button."
  (let* ((tag-id org-supertag-relation--current-tag)
         (other-tag-id (button-get button 'other-tag-id))
         (rel-type (button-get button 'rel-type)))
    (when (and tag-id other-tag-id rel-type)
      (org-supertag-relation-add-with-complement tag-id other-tag-id rel-type)
      (org-supertag-relation--refresh-display)
      (message "Relation added: %s -[%s]-> %s" 
               (org-supertag-tag-get-name-by-id tag-id)
               rel-type
               (org-supertag-tag-get-name-by-id other-tag-id)))))

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
      (message "关系已移除: %s -[%s]-> %s" 
               (org-supertag-tag-get-name-by-id tag-id)
               rel-type
               (org-supertag-tag-get-name-by-id other-tag-id)))))

(defun org-supertag-relation--select-tag-action (button)
  "Handle the action of selecting a similar tag.
This will prompt user to choose a relation type for the selected tag."
  (let* ((tag-id org-supertag-relation--current-tag)
         (other-tag-id (button-get button 'other-tag-id))
         (rel-choices (org-supertag-relation--get-relation-type-choices))
         (choice (completing-read "选择关系类型: " rel-choices nil t))
         (rel-type (org-supertag-relation--get-type-from-choice choice)))
    (when (and tag-id other-tag-id rel-type)
      (if (org-supertag-relation-has-complement-p rel-type)
          (let ((complement (org-supertag-relation-get-complement rel-type)))
            (org-supertag-relation-add-with-complement tag-id other-tag-id rel-type)
            (org-supertag-relation--refresh-display)
            (message "Added bidirectional relation: %s -[%s]-> %s and %s -[%s]-> %s" 
                     (org-supertag-tag-get-name-by-id tag-id)
                     rel-type
                     (org-supertag-tag-get-name-by-id other-tag-id)
                     (org-supertag-tag-get-name-by-id other-tag-id)
                     complement
                     (org-supertag-tag-get-name-by-id tag-id)))
        (org-supertag-relation-add-relation tag-id other-tag-id rel-type)
        (org-supertag-relation--refresh-display)
        (message "Added relation: %s -[%s]-> %s" 
                 (org-supertag-tag-get-name-by-id tag-id)
                 rel-type
                 (org-supertag-tag-get-name-by-id other-tag-id))))))

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
      (when (re-search-forward "\\(\\[-\\]\\|\\[Select\\]\\)" (line-end-position) t)
        (let ((button (button-at (match-beginning 0))))
          (when button
            (push-button button)))))))

(defun org-supertag-relation--highlight-current-item ()
  "Highlight the current item in the buffer."
  (with-current-buffer org-supertag-relation-manage--buffer-name
    (let ((inhibit-read-only t))
      (remove-overlays (point-min) (point-max) 'org-supertag-relation-highlight t)
      (let* ((all-items (org-supertag-relation--get-all-items))
             (current-item (nth org-supertag-relation--current-item-index all-items)))
        (when current-item
          (let ((tag-name (cond
                          ((eq (car current-item) 'recommended)
                           (car (cdr current-item)))
                          (t
                           (org-supertag-tag-get-name-by-id 
                            (plist-get (cdr current-item) :to))))))
            (save-excursion
              (goto-char (point-min))
              (let ((found nil))
                (while (and (not found)
                           (search-forward tag-name nil t))
                  (save-excursion
                    (beginning-of-line)
                    (when (looking-at ".*\\(\\[-\\]\\|\\[Select\\]\\).*")
                      (setq found t)
                      (let* ((line-start (line-beginning-position))
                             (line-end (line-end-position))
                             (ov (make-overlay line-start line-end)))
                        (overlay-put ov 'face 'highlight)
                        (overlay-put ov 'org-supertag-relation-highlight t)))))))))))))  

(defun org-supertag-relation--get-all-items ()
  "Get all items from all sections in display order."
  (let ((items nil))
    ;; Co-occurrence section
    (dolist (rel (cl-remove-if-not
                 (lambda (rel) (eq (plist-get rel :type) 'cooccurrence))
                 (org-supertag-relation-get-all org-supertag-relation--current-tag)))
      (push (cons 'cooccurrence rel) items))
    
    ;; Existing relations section
    (dolist (rel (cl-remove-if
                 (lambda (rel) (eq (plist-get rel :type) 'cooccurrence))
                 (org-supertag-relation-get-all org-supertag-relation--current-tag)))
      (push (cons 'existing rel) items))
    
    ;; Recommended section
    (when-let ((recommendations (gethash org-supertag-relation--current-tag
                                       org-supertag-relation--recommendations-cache)))
      (dolist (rec recommendations)
        (push (cons 'recommended rec) items)))
    
    (nreverse items)))
    
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

;;----------------------------------------------------------------------
;; Isolated Tags Management
;;----------------------------------------------------------------------

(defun org-supertag-relation-find-isolated-tags ()
  "查找所有没有任何关系的孤立标签。
返回标签ID列表。"
  (let ((all-tags (org-supertag-db-find-by-type :tag))
        (isolated-tags nil))
    (dolist (tag-id all-tags)
      ;; 检查是否有从该标签出发的关系
      (let ((outgoing-relations (org-supertag-relation-get-all tag-id))
            (incoming-relations nil))
        
        ;; 检查是否有指向该标签的关系
        (maphash
         (lambda (rel-id props)
           (when (and (string-prefix-p ":tag-relation:" rel-id)
                      (equal (plist-get props :to) tag-id))
             (push props incoming-relations)))
         org-supertag-db--link)
        
        ;; 如果既没有出发的关系也没有指向的关系，则为孤立标签
        (when (and (null outgoing-relations) (null incoming-relations))
          (push tag-id isolated-tags))))
    
    isolated-tags))

(defun org-supertag-relation-show-isolated-tags ()
  "显示所有孤立标签的列表，并提供处理选项。"
  (interactive)
  (let* ((isolated-tags (org-supertag-relation-find-isolated-tags))
         (tags-with-names nil))
    
    ;; 收集标签名称
    (dolist (tag-id isolated-tags)
      (let ((tag-name (org-supertag-tag-get-name-by-id tag-id)))
        (when tag-name
          (push (cons tag-id tag-name) tags-with-names))))
    
    (if (null tags-with-names)
        (message "没有找到孤立标签")
      ;; 创建孤立标签浏览缓冲区
      (with-current-buffer (get-buffer-create "*Org-Supertag Isolated Tags*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-supertag-relation-mode) ;; 使用相同的模式
          
          ;; 标题
          (insert (propertize "孤立标签列表（没有任何关系的标签）\n\n" 
                             'face '(:height 1.5 :weight bold)))
          
          ;; 帮助信息
          (insert "操作说明:\n")
          (insert " [m] - 选择/取消选择标签\n")
          (insert " [M] - 选择所有标签    [u] - 取消选择所有标签\n")
          (insert " [RET] - 管理选中标签的关系\n")
          (insert " [b] - 对选中标签批量添加关系\n")
          (insert " [r] - 刷新列表\n")
          (insert " [q] - 退出\n\n")
          
          ;; 显示孤立标签列表
          (insert (propertize (format "找到 %d 个孤立标签:\n" (length tags-with-names))
                             'face '(:weight bold)))
          
          ;; 排序标签并插入
          (let ((sorted-tags (sort tags-with-names 
                                  (lambda (a b) 
                                    (string< (cdr a) (cdr b))))))
            (dolist (tag sorted-tags)
              (let* ((tag-id (car tag))
                     (tag-name (cdr tag))
                     (is-selected (member tag-id org-supertag-relation--selected-tags))
                     (selection-mark (if is-selected 
                                        (propertize "[✓]" 'face '(:foreground "green"))
                                      "[ ]")))
                (insert " ")
                (insert selection-mark)
                (insert " ")
                (insert-text-button (propertize "[管理]" 'face '(:foreground "green"))
                                   'action 'org-supertag-relation--manage-isolated-tag-action
                                   'tag-id tag-id
                                   'follow-link t
                                   'help-echo (format "管理 %s 标签的关系" tag-name))
                (insert " ")
                (insert-text-button (propertize "[标记]" 'face '(:foreground "blue"))
                                   'action 'org-supertag-relation-toggle-isolated-tag-selection
                                   'tag-id tag-id
                                   'follow-link t
                                   'help-echo "标记/取消标记此标签用于批量操作")
                (insert (format " %s\n" tag-name))))))
        
        ;; 绑定特殊按键
        (local-set-key (kbd "m") 'org-supertag-relation-toggle-isolated-tag-at-point)
        (local-set-key (kbd "M") 'org-supertag-relation-select-all-isolated-tags)
        (local-set-key (kbd "u") 'org-supertag-relation-unselect-all-tags)
        (local-set-key (kbd "RET") 'org-supertag-relation-manage-selected-isolated-tag)
        (local-set-key (kbd "r") 'org-supertag-relation-refresh-isolated-tags)
        (local-set-key (kbd "q") 'org-supertag-relation-quit-isolated-tags)
        (local-set-key (kbd "n") 'next-line)
        (local-set-key (kbd "p") 'previous-line)
        (local-set-key (kbd "j") 'next-line)
        (local-set-key (kbd "k") 'previous-line)
        (local-set-key (kbd "b") 'org-supertag-relation-batch-relate-isolated-tags)
        
        ;; 显示缓冲区
        (pop-to-buffer (current-buffer))))))

(defun org-supertag-relation--manage-isolated-tag-action (button)
  "响应点击孤立标签列表中的[管理]按钮。"
  (let ((tag-id (button-get button 'tag-id)))
    (when tag-id
      (org-supertag-relation--show-management-interface tag-id))))

(defun org-supertag-relation-toggle-isolated-tag-selection (button)
  "切换孤立标签的选中状态，用于批量操作。"
  (let ((tag-id (button-get button 'tag-id)))
    (when tag-id
      (if (member tag-id org-supertag-relation--selected-tags)
          ;; 取消选择
          (setq org-supertag-relation--selected-tags
                (delete tag-id org-supertag-relation--selected-tags))
        ;; 添加选择
        (push tag-id org-supertag-relation--selected-tags))
      
      ;; 刷新显示以更新选择状态
      (org-supertag-relation-refresh-isolated-tags)
      
      ;; 显示当前选中数量
      (message "已选择 %d 个标签" 
               (length org-supertag-relation--selected-tags)))))

(defun org-supertag-relation-toggle-isolated-tag-at-point ()
  "切换当前行孤立标签的选中状态。"
  (interactive)
  (let ((button (save-excursion 
                 (beginning-of-line)
                 (re-search-forward "\\[标记\\]" (line-end-position) t)
                 (button-at (match-beginning 0)))))
    (when button
      (org-supertag-relation-toggle-isolated-tag-selection button))))

(defun org-supertag-relation-select-all-isolated-tags ()
  "选择所有显示的孤立标签。"
  (interactive)
  (setq org-supertag-relation--selected-tags nil) ;先清空
  
  ;; 收集所有孤立标签
  (with-current-buffer "*Org-Supertag Isolated Tags*"
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[管理\\]" nil t)
        (let ((button (button-at (match-beginning 0))))
          (when button
            (push (button-get button 'tag-id) 
                  org-supertag-relation--selected-tags))))))
  
  ;; 刷新显示并提示
  (org-supertag-relation-refresh-isolated-tags)
  (message "已选择所有孤立标签 (%d 个)" 
           (length org-supertag-relation--selected-tags)))

(defun org-supertag-relation-manage-selected-isolated-tag ()
  "管理当前行的孤立标签关系。"
  (interactive)
  (let ((button (save-excursion 
                 (beginning-of-line)
                 (re-search-forward "\\[管理\\]" (line-end-position) t)
                 (button-at (match-beginning 0)))))
    (when button
      (org-supertag-relation--manage-isolated-tag-action button))))

(defun org-supertag-relation-refresh-isolated-tags ()
  "刷新孤立标签列表的显示。"
  (interactive)
  (when (get-buffer "*Org-Supertag Isolated Tags*")
    (let ((point (point)))
      (org-supertag-relation-show-isolated-tags)
      (goto-char (min point (point-max))))))

(defun org-supertag-relation-batch-relate-isolated-tags ()
  "批量为选中的孤立标签建立关系。"
  (interactive)
  (let ((selected-tags org-supertag-relation--selected-tags))
    (if (< (length selected-tags) 2)
        (message "需要选择至少两个标签才能建立批量关系")
      
      ;; 选择中心标签
      (let* ((tag-names (mapcar (lambda (id) 
                               (cons id (org-supertag-tag-get-name-by-id id)))
                             selected-tags))
             (choice (completing-read "选择中心标签（所有其他标签将与之建立关系）: " 
                                     (mapcar #'cdr tag-names) nil t))
             (center-tag-id (car (rassoc choice tag-names)))
             (other-tags (remove center-tag-id selected-tags)))
        
        ;; 选择关系类型
        (let* ((rel-choices (org-supertag-relation--get-relation-type-choices))
               (choice (completing-read "选择要应用的关系类型: " rel-choices nil t))
               (rel-type (org-supertag-relation--get-type-from-choice choice))
               (added-count 0))
          
          ;; 批量应用关系
          (dolist (other-tag-id other-tags)
            (if (org-supertag-relation-has-complement-p rel-type)
                (org-supertag-relation-add-with-complement center-tag-id other-tag-id rel-type)
              (org-supertag-relation-add-relation center-tag-id other-tag-id rel-type))
            (cl-incf added-count))
          
          ;; 清空选择并刷新显示
          (setq org-supertag-relation--selected-tags nil)
          (org-supertag-relation-refresh-isolated-tags)
          (message "已添加 %d 个关系（中心标签：%s）" 
                   added-count (org-supertag-tag-get-name-by-id center-tag-id)))))))

(defun org-supertag-relation-toggle-tag-selection (button)
  "切换标签的选中状态，用于批量操作。
这个函数用于在标准关系管理界面中标记/取消标记标签。"
  (interactive)
  (when (or button (get-text-property (point) 'button))
    (let* ((btn (or button (button-at (point))))
           (tag-id (when btn (button-get btn 'other-tag-id)))
           (current-tag org-supertag-relation--current-tag))
      
      (when (and tag-id (not (equal tag-id current-tag)))
        (if (member tag-id org-supertag-relation--selected-tags)
            ;; 取消选择
            (setq org-supertag-relation--selected-tags
                  (delete tag-id org-supertag-relation--selected-tags))
          ;; 添加选择
          (push tag-id org-supertag-relation--selected-tags))
        
        ;; 刷新显示以更新选择状态
        (org-supertag-relation--refresh-display)
        
        ;; 显示当前选中数量
        (message "已选择 %d 个标签" 
                 (length org-supertag-relation--selected-tags))))))

(defun org-supertag-relation-batch-add-selected ()
  "对所有选中的标签批量添加关系。
用于在标准关系管理界面中批量添加关系。"
  (interactive)
  (let* ((tag-id org-supertag-relation--current-tag)
         (selected-tags org-supertag-relation--selected-tags))
    
    (if (null selected-tags)
        (message "未选择任何标签")
      
      ;; 有选中的标签时，选择关系类型
      (let* ((rel-choices (org-supertag-relation--get-relation-type-choices))
             (choice (completing-read "选择要应用的关系类型: " rel-choices nil t))
             (rel-type (org-supertag-relation--get-type-from-choice choice))
             (added-count 0))
        
        ;; 批量应用关系
        (dolist (other-tag-id selected-tags)
          ;; 避免自己与自己建立关系
          (unless (equal tag-id other-tag-id)
            (if (org-supertag-relation-has-complement-p rel-type)
                (org-supertag-relation-add-with-complement tag-id other-tag-id rel-type)
              (org-supertag-relation-add-relation tag-id other-tag-id rel-type))
            (cl-incf added-count)))
        
        ;; 清空选择并刷新显示
        (setq org-supertag-relation--selected-tags nil)
        (org-supertag-relation--refresh-display)
        (message "已添加 %d 个关系" added-count)))))

(defun org-supertag-relation-select-all-tags ()
  "选择当前显示的所有推荐标签。"
  (interactive)
  (setq org-supertag-relation--selected-tags nil) ;先清空
  
  ;; 收集推荐区域的标签
  (when (gethash org-supertag-relation--current-tag org-supertag-relation--recommendations-cache)
    (dolist (rec (gethash org-supertag-relation--current-tag org-supertag-relation--recommendations-cache))
      (let* ((tag-name (car rec))
             (tag-id (org-supertag-tag-get-id-by-name tag-name)))
        (when tag-id
          (push tag-id org-supertag-relation--selected-tags)))))
  
  ;; 刷新显示并提示
  (org-supertag-relation--refresh-display)
  (message "已选择所有推荐标签 (%d 个)" 
           (length org-supertag-relation--selected-tags)))

(defun org-supertag-relation-unselect-all-tags ()
  "取消选择所有标签。"
  (interactive)
  (setq org-supertag-relation--selected-tags nil)
  (org-supertag-relation--refresh-display)
  (message "已取消选择所有标签"))

;;;###autoload
(defun org-supertag-relation-manage-isolated-tags ()
  "查找并管理所有没有关系的孤立标签。
此函数作为独立入口点，直接显示孤立标签管理界面。"
  (interactive)
  (org-supertag-relation-show-isolated-tags))

(defun org-supertag-relation-quit-isolated-tags ()
  "关闭孤立标签缓冲区。"
  (interactive)
  (kill-buffer "*Org-Supertag Isolated Tags*"))

(provide 'org-supertag-relation)
;;; org-supertag-relation.el ends here

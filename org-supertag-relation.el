;;; org-supertag-relation.el --- Tag relationship management for org-supertag -*- lexical-binding: t; -*-

;; This file provides functionality for managing relationships between tags
;; in org-supertag.  It supports recording, querying, and analyzing tag
;; co-occurrence relationships.

;;; Code:

(require 'ht)
(require 'cl-lib)
(require 'org-element)
(require 'org-supertag-db)

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

(defun org-supertag-relation--get-all-relation-types ()
  "Get all available relation types."
  org-supertag-relation-types)

(defun org-supertag-relation--get-relation-type-choices ()
  "Get relation type choices for user selection."
  (mapcar (lambda (type)
            (let ((symbol (car type))
                  (desc (cdr type)))
              (format "%s - %s" (symbol-name symbol) desc)))
          org-supertag-relation-types))

(defun org-supertag-relation--get-type-from-choice (choice)
  "从选项字符串获取关系类型符号。"
  (when choice
    (let ((parts (split-string choice " - ")))
      (when parts
        (intern (car parts))))))

(defun org-supertag-relation-add-relation (from-tag to-tag rel-type &optional strength)
  "Add a tag relation.
FROM-TAG: Source tag
TO-TAG: Target tag
REL-TYPE: Relation type
STRENGTH: Relation strength (optional, default is 1.0)"
  (interactive
   (let* ((from-tag (completing-read "Source tag: " (org-supertag-get-all-tags) nil t))
          (to-tag (completing-read "Target tag: " (org-supertag-get-all-tags) nil t))
          (rel-choices (org-supertag-relation--get-relation-type-choices))
          (rel-type (org-supertag-relation--get-type-from-choice 
                    (completing-read "Relation type: " rel-choices nil t))))
     (list from-tag to-tag rel-type 1.0)))
  ;; Check if the tags are valid
  (cond
   ((not from-tag)
    (user-error "Source tag cannot be empty"))
   ((not to-tag)
    (user-error "Target tag cannot be empty"))
   ((equal from-tag to-tag)
    (user-error "Source tag and target tag cannot be the same")))
  (let* ((from-name (if (org-supertag-db-exists-p from-tag)
                       ;; if the input is an ID, get its name
                       (or (org-supertag-tag-get-name-by-id from-tag) from-tag)
                     ;; otherwise, assume the input is a tag name
                     from-tag))
         (to-name (if (org-supertag-db-exists-p to-tag)
                     ;; if the input is an ID, get its name
                     (or (org-supertag-tag-get-name-by-id to-tag) to-tag)
                   ;; otherwise, assume the input is a tag name
                   to-tag))
         (rel-id (format ":tag-relation:%s->%s" from-tag to-tag))
         (props (list :from from-tag
                     :to to-tag
                     :type rel-type
                     :strength (or strength 1.0)
                     :created-at (current-time))))
    (puthash rel-id props org-supertag-db--link)
    (message "Add relation: %s -[%s]-> %s" from-name rel-type to-name)))

(defun org-supertag-relation-remove-relation (from-tag to-tag)
  "Remove the relation between FROM-TAG and TO-TAG."
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
    (let* ((from-name (if (org-supertag-db-exists-p from-tag)
                         (or (org-supertag-tag-get-name-by-id from-tag) from-tag)
                       from-tag))
           (to-name (if (org-supertag-db-exists-p to-tag)
                       (or (org-supertag-tag-get-name-by-id to-tag) to-tag)
                     to-tag))
           (rel-id (format ":tag-relation:%s->%s" from-tag to-tag)))
      (remhash rel-id org-supertag-db--link)
      (message "Remove relation: %s -> %s" from-name to-name))))

(defun org-supertag-relation-get (from-tag to-tag)
  "Get the relation data between FROM-TAG and TO-TAG.
If the relation exists, return the relation properties list, otherwise return nil."
  (when (and from-tag to-tag)
    (let ((rel-id (format ":tag-relation:%s->%s" from-tag to-tag)))
      (gethash rel-id org-supertag-db--link))))

(defun org-supertag-relation-get-all-from (tag-id)
  "Get all relations from the TAG-ID.
Return the list of (other-tag . rel-type)."
  (if (null tag-id)
      nil
    (let (result)
      (maphash
       (lambda (rel-id props)
         (when (and (string-prefix-p ":tag-relation:" rel-id)
                   (equal (plist-get props :from) tag-id))
           (let ((to-tag (plist-get props :to))
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
Return the list of relations, each relation is a plist with :from, :to and :type keys."
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
  (let ((buffer-name "*Org-Supertag Relation*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    (with-current-buffer (get-buffer-create buffer-name)
      (org-supertag-relation-mode)
      (setq-local org-supertag-relation--current-tag tag-id)
      (org-supertag-relation--refresh-display))
    
    (let ((display-buffer-alist
           '(("\\*Org-Supertag Relation\\*"
              (display-buffer-in-side-window)
              (side . right)
              (window-width . 0.5)
              (slot . 0)))))
      (pop-to-buffer buffer-name))))

(defun org-supertag-relation--refresh-display ()
  "Refresh the tag relations management interface display."
  (let ((inhibit-read-only t)
        (tag-id org-supertag-relation--current-tag))
    (unless tag-id
      (user-error "Current tag is not set, please select a tag"))
    
    (erase-buffer)
    (let ((tag-name (org-supertag-tag-get-name-by-id tag-id)))
      (insert (propertize (format "Tag Relation - %s\n\n" tag-name)
                         'face '(:height 1.5 :weight bold))))

    (insert (propertize "Operations:\n" 'face '(:weight bold)))
    (insert " [a] - Add     [d] - Remove    [r] - Refresh    [q] - Quit\n")
    (insert " [g] - Find By Relation Group    [f] - Find By Relation\n")

    (insert (propertize "\nCo-occurrence Tags:\n" 'face '(:weight bold)))
    (let* ((relations (cl-remove-if-not
                      (lambda (rel) (eq (plist-get rel :type) 'cooccurrence))
                      (org-supertag-relation-get-all tag-id))))
      (if relations
          (dolist (rel relations)
            (let* ((other-tag-id (plist-get rel :to))
                   (other-tag-name (org-supertag-tag-get-name-by-id other-tag-id))
                   (strength (or (plist-get rel :strength) 0.0))
                   (strength-display (format " %.2f" strength))
                   (remove-button-text (propertize "[-]" 'face '(:foreground "red"))))
              ;; create clickable remove button
              (insert " ")
              (insert-text-button remove-button-text
                                 'action 'org-supertag-relation--remove-button-action
                                 'other-tag-id other-tag-id
                                 'follow-link t
                                 'help-echo "click to remove this relation")
              (insert (format " %s " 
                             (propertize "cooccurrence" 'face '(:foreground "white" :background "black"))))
              (insert (format " %s%s\n" 
                             other-tag-name
                             (propertize strength-display 'face '(:foreground "gray50"))))))
        (insert "  (no cooccurrence relations)\n")))

    (insert (propertize "\nExisting Relations:\n" 'face '(:weight bold)))
    (let ((relations (cl-remove-if
                     (lambda (rel) (eq (plist-get rel :type) 'cooccurrence))
                     (org-supertag-relation-get-all tag-id))))
      (if relations
          (dolist (rel relations)
            (let* ((other-tag-id (plist-get rel :to))
                   (other-tag-name (org-supertag-tag-get-name-by-id other-tag-id))
                   (rel-type (plist-get rel :type))
                   (remove-button-text (propertize "[-]" 'face '(:foreground "red"))))
              (insert " ")
              (insert-text-button remove-button-text
                                 'action 'org-supertag-relation--remove-button-action
                                 'other-tag-id other-tag-id
                                 'follow-link t
                                 'help-echo "click to remove this relation")
              (insert (format " %s " 
                             (propertize (format "%s" rel-type) 'face '(:foreground "white" :background "black"))))
              (insert (format " %s\n" other-tag-name))))
        (insert "  (no existing relations)\n")))
    
    (insert "\n")
    (insert (propertize "Recommended Relations:\n" 'face '(:weight bold)))
    (let* ((all-tags (org-supertag-get-all-tags))
           (other-tag-ids (mapcar #'org-supertag-tag-get-id-by-name 
                                (remove (org-supertag-tag-get-name-by-id tag-id) all-tags)))
           (recommendations 
            (org-supertag-relation--get-recommendations tag-id other-tag-ids)))
      (if recommendations
          (progn
            (dolist (rec recommendations)
              (let* ((rec-tag-id (car rec))
                     (rec-tag-name (org-supertag-tag-get-name-by-id rec-tag-id))
                     (suggested-rel-type (org-supertag-relation--suggest-relation-type 
                                         tag-id rec-tag-id))
                     (add-button-text (propertize "[+]" 'face '(:foreground "green"))))

                (insert " ")
                (insert-text-button add-button-text
                                   'action 'org-supertag-relation--add-button-action
                                   'other-tag-id rec-tag-id
                                   'rel-type suggested-rel-type
                                   'follow-link t
                                   'help-echo "click to add this relation")
                (insert (format " %s " 
                               (propertize (format "%s" suggested-rel-type) 'face '(:foreground "white" :background "black"))))
                (insert (format " %s\n" rec-tag-name)))))
        (insert "  (no recommended relations)\n")))
    
    (insert "\n")
    (insert (propertize "Note: " 'face '(:weight bold)))
    (insert "Click [+] to add recommended relations, click [-] to remove existing relations\n")))

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
          (let* ((rel-choices (org-supertag-relation--get-relation-type-choices))
                 (choice (completing-read "Select relation type: " rel-choices nil t))
                 (rel-type (org-supertag-relation--get-type-from-choice choice)))
            (unless tag-id
              (setq tag-id (org-supertag-tag-get-id-by-name
                           (completing-read "Select source tag: " all-tags nil t))))
            (when (and tag-id other-tag-id rel-type)
              (org-supertag-relation-add-relation tag-id other-tag-id rel-type)
              (org-supertag-relation--refresh-display)
              (message "已添加关系: %s %s %s" 
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
          (let ((other-tag-id (plist-get rel :to)))
            (when other-tag-id
              (let ((other-tag-name (or (org-supertag-tag-get-name-by-id other-tag-id)
                                       "(Unnamed Tag)")))
                    (rel-type (plist-get rel :type)))
                (let ((choice (format "%s (%s)" other-tag-name rel-type)))
                  (push choice rel-choices)
                  (puthash choice other-tag-id rel-to-id-map))))))
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
              (message "Can't find target tag ID. Please try again.")))))))

(defun org-supertag-relation-refresh ()
  (interactive)
  (org-supertag-relation--refresh-display))

(defun org-supertag-relation-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

;;------------------------------------------------------------------------
;; Tag recommendation algorithm
;;------------------------------------------------------------------------

(defun org-supertag-relation--get-recommendations (tag-id other-tags)
  "Get tag relation recommendations.
TAG-ID: The current tag ID
OTHER-TAGS: The list of other possible related tags"
  (if (or (null tag-id) (null other-tags))
      nil
    (let (recommendations
          (existing-relations (mapcar 
                              (lambda (rel) (plist-get rel :to))
                              (org-supertag-relation-get-all tag-id))))
      (dolist (other-tag other-tags)
        (when (and other-tag 
                  (not (equal other-tag tag-id))
                  (not (member other-tag existing-relations)))
          (push (cons other-tag 0.5) recommendations)))
      
      (setq recommendations 
            (sort (seq-take recommendations 10) 
                  (lambda (a b) (string< (car a) (car b)))))
      recommendations)))

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
  (let ((rel (car (cl-remove-if-not
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
      (org-supertag-relation-add-relation tag-id other-tag-id rel-type)
      (org-supertag-relation--refresh-display)
      (message "Relation added: %s ->[%s]-> %s" 
               (org-supertag-tag-get-name-by-id tag-id)
               rel-type
               (org-supertag-tag-get-name-by-id other-tag-id)))))

(defun org-supertag-relation--remove-button-action (button)
  "Handle the action of clicking the remove relation button."
  (let* ((tag-id org-supertag-relation--current-tag)
         (other-tag-id (button-get button 'other-tag-id)))
    (when (and tag-id other-tag-id)
      (org-supertag-relation-remove-relation tag-id other-tag-id)
      (org-supertag-relation--refresh-display)
      (message "Relation removed: %s -> %s" 
               (org-supertag-tag-get-name-by-id tag-id)
               (org-supertag-tag-get-name-by-id other-tag-id)))))

(provide 'org-supertag-relation)
;;; org-supertag-relation.el ends here

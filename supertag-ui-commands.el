;;; org-supertag/ui/commands.el --- User command interface for Org-Supertag -*- lexical-binding: t; -*-

;;nn; Commentary:
;; This file provides the user-facing interactive commands for Org-Supertag.
;; These commands act as the entry points for user interaction, calling the
;; underlying operations and services.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'supertag-services-ui) ; For UI helper services
(require 'supertag-ops-node) ; For node operations
(require 'supertag-ops-tag)  ; For tag operations (e.g., tag completion)
(require 'supertag-ops-field) ; For field operations
(require 'supertag-ops-relation) ; For relation operations
(require 'supertag-services-query) ; For query operations
(require 'supertag-view-kanban)    ; For Kanban board view
(require 'supertag-services-capture) ; For capture services

;;; --- Internal Helper ---

(defun supertag-ui--extract-inline-tags-from-string (content-string)
  "Extract all #tags from a CONTENT-STRING using a regex."
  (let ((tags '()))
    (when content-string
      (with-temp-buffer
        (insert content-string)
        (goto-char (point-min))
        (while (re-search-forward "#\(\w[-_[:alnum:]]*\)" nil t)
          (push (match-string 1) tags))))
    (nreverse tags)))

(defun supertag--get-node-props-at-point ()
  "Extract node properties from the current Org heading at point.
Returns a plist of properties suitable for node creation/update."
  (when (org-at-heading-p)
    (let* ((element (org-element-at-point))
           (id (org-id-get)) ; Get existing ID if any
           (title (org-element-property :raw-value element))
           (level (org-element-property :level element))
           (file (buffer-file-name))
           (pos (point))
           (content (save-excursion
                       (org-end-of-meta-data t)
                       (buffer-substring-no-properties (point) (save-excursion (org-end-of-subtree t t) (point)))))
           (headline-tags (supertag-ui--extract-inline-tags-from-string title))
           (content-tags (supertag-ui--extract-inline-tags-from-string content))
           (all-tags (cl-union headline-tags content-tags :test 'equal)))
      (list :id id
            :title title
            :tags all-tags
            :level level
            :file file
            :position pos
            :content content))))

;;; --- User Commands ---

(defun supertag-ui--get-node-at-point ()
  "Check if point is at a heading and return the node ID.
Creates an ID if one does not exist. Errors out if not on a heading."
  (unless (org-at-heading-p)
    (user-error "Point must be at an Org heading."))
  (org-id-get-create))

(defun supertag-ui--get-containing-node-at-point ()
  "Get the node ID of the containing node, whether at heading or in content.
Works both when point is at a heading or within the content of a node.
Returns the node ID, creating one if it doesn't exist."
  (save-excursion
    ;; If we're already at a heading, use that
    (if (org-at-heading-p)
        (org-id-get-create)
      ;; Otherwise, find the containing heading
      (when (org-back-to-heading t)
        (org-id-get-create)))))

(defun supertag-view-kanban ()
  "Create an interactive Kanban board view based on a tag's field."
  (interactive)
  (let* ((available-tags (supertag-view-kanban--get-all-tags))
         (tag-name (completing-read "Select a tag to build Kanban from: " available-tags nil t))
         (tag-id (when tag-name (supertag-tag-get-id-by-name tag-name))))
    
    (if (not tag-id)
        (message "No valid tag selected.")
      (let* ((tag-data (supertag-tag-get tag-id))
             (tag-name-from-data (plist-get tag-data :name))  ; Get tag name from tag data
             (fields (plist-get tag-data :fields))
             (field-names (mapcar (lambda (f) (plist-get f :name)) fields)))
        
        (if (not field-names)
            (message "Tag '%s' has no fields to group by." tag-name-from-data)
          (let* ((field-name (completing-read "Group columns by which field: " field-names nil t))
                 (config (supertag-view-kanban-create-config tag-id field-name)))
            
            (when field-name
              (let* ((buf-name (format "*Supertag Kanban: %s by %s*" tag-name-from-data field-name))
                     (buf (get-buffer-create buf-name)))
                
                (with-current-buffer buf
                  (supertag-view-kanban-mode)
                  (supertag-view-kanban-render config)
                  (supertag-view-kanban--subscribe-updates))
                
                (switch-to-buffer buf)
                (message "Kanban board created for tag '%s' grouped by '%s'" tag-name-from-data field-name)))))))))


;;; --- Node Commands: Create, move, find, delete 
(defun supertag-create-node ()
  "Interactive command to create a new node.
If at an Org heading, it will create a node from that heading.
Otherwise, it will prompt for a title and create a new heading."
  (interactive)
  (let* ((props nil)
         (node-id nil))
    (if (org-at-heading-p)
        ;; Create node from existing heading
        (progn
          (setq props (supertag--get-node-props-at-point))
          (unless (plist-get props :id)
            (org-id-get-create) ; Ensure ID exists for the heading
            (setq props (plist-put props :id (org-id-get))))
          (setq node-id (plist-get props :id))
          (supertag-node-create props)
          (message "Node created from current heading: %s" (plist-get props :title)))
      ;; Create new heading and node
      (let* ((title (read-string "Node title: "))
             (level (if (org-at-heading-p) (org-outline-level) 1)))
        (save-excursion
          (beginning-of-line)
          (insert (make-string level ?*) " " title "\n")
          (forward-line -1) ; Move back to the new heading
          (org-id-get-create) ; Create ID for the new heading
          (setq props (supertag--get-node-props-at-point))
          (setq node-id (plist-get props :id))
          (supertag-node-create props)
          (message "New node '%s' created." title))))
    node-id))

(defun supertag-find-node ()
  "Find a node by its title/path and jump to it in the current window."
  (interactive)
  (let ((node-id (supertag-ui-select-node "Find node: " t))) ; Use cache for better performance
    (when node-id
      (supertag-goto-node node-id))))
 
(defun supertag-find-node-other-window ()
  "Find a node by its title/path and jump to it in another window."
  (interactive)
  (let ((node-id (supertag-ui-select-node "Find node (other window): " t))) ; Use cache for better performance
    (when node-id
      (supertag-goto-node node-id t))))

(defun supertag-delete-node ()
  "Delete the node at point, removing it from the database and the Org file."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point must be at a heading to delete a node."))
  (let ((node-id (org-id-get)))
    (unless node-id
      (user-error "Current heading does not have an ID, it is not a node."))
    (when (yes-or-no-p (format "Really delete node %s and its headline? " node-id))
      ;; 1. Call the core operation to delete from the database
      (supertag-node-delete node-id)
      ;; 2. Delete the headline from the Org buffer
      (org-back-to-heading t)
      (let* ((element (org-element-at-point))
             (begin (org-element-property :begin element))
             (end (org-element-property :end element)))
        (delete-region begin end)
        ;; Also delete the newline after the heading if it exists
        (when (looking-at "\n")
          (delete-char 1)))
      ;; 3. Save the buffer to persist the deletion from the file
      (save-buffer)
      (message "Node %s deleted." node-id))))

(defun supertag-update-node-at-point ()
  "Manually re-synchronize the node at the current headline with the database."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point must be at a heading to update a node."))
  ;; Ensure ID exists before syncing.
  (org-id-get-create)
  (if (supertag-node-sync-at-point)
      (message "Node at point re-synced successfully.")
    (user-error "Failed to re-sync node at point.")))

(defun supertag-back-to-heading ()
   "Remove the node at point from the supertag system.
 This removes the node and all its relations from the database,
 but leaves the Org heading and its content intact in the file,
 effectively converting it back to a regular heading."
   (interactive)
   (let ((node-id (org-id-get)))
     (unless node-id
       (user-error "Current heading does not have an ID, it is not a node."))
     (when (yes-or-no-p "Really remove this node from the d2atabase? (The heading will be preserved)")
       (supertag-node-delete node-id)
       (message "Node %s removed from database. It is now a regular Org heading." node-id))))

(defun supertag-move-node ()
  "Interactively move the node at point to another file.
The node's content (the entire subtree) will be cut from the
current file and inserted into the target file at a chosen position."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point must be at a heading to move a node."))
  (let ((node-id (org-id-get)))
    (unless node-id
      (user-error "Current heading does not have an ID, it is not a node."))

    ;; 1. Prompt for target file and position
    (let* ((target-file (read-file-name "Move node to file: "))
           (insert-info (supertag-ui-select-insert-position target-file))
           (target-pos (plist-get insert-info :position))
           (target-level (plist-get insert-info :level)))
      (unless (and target-file (file-exists-p target-file))
        (user-error "Target file does not exist: %s" target-file))
      (unless insert-info
        (user-error "No valid insert position selected."))

      (when (yes-or-no-p (format "Really move node %s to %s? " node-id (file-name-nondirectory target-file)))
        ;; 2. Get node content and original level
        (let* ((element (org-element-at-point))
               (begin (org-element-property :begin element))
               (end (org-element-property :end element))
               (original-level (org-element-property :level element))
               (content (buffer-substring-no-properties begin end)))

          ;; 3. Delete from source buffer
          (delete-region begin end)
          (save-buffer)
          (message "Cut node %s from source file." node-id)

          ;; 4. Adjust content level and insert into target file
          (let ((adjusted-content (supertag-ui--adjust-content-level content original-level target-level)))
            (with-current-buffer (find-file-noselect target-file)
              (goto-char target-pos)
              (unless (or (bobp) (looking-back "\n" 1)) (insert "\n"))
              (insert adjusted-content)
              (save-buffer))
            (message "Pasted node into %s." (file-name-nondirectory target-file)))

          ;; 5. Update the database with the new location
          (supertag-node-set-location node-id target-file target-pos)
          (message "Node %s successfully moved." node-id))))))

(defun supertag-move-node-and-link ()
    "Move the node at point to another file, leaving a link behind."
    (interactive)
    (unless (org-at-heading-p)
      (user-error "Point must be at a heading to move a node."))
    (let ((node-id (org-id-get)))
      (unless node-id
        (user-error "Current heading does not have an ID, it is not a node."))

      ;; 1. Get target file and position (reusing our UI service)
      (let* ((target-file (read-file-name "Move node to file: "))
             (insert-info (supertag-ui-select-insert-position target-file))
             (target-pos (plist-get insert-info :position))
             (target-level (plist-get insert-info :level)))
        (unless (and target-file (file-exists-p target-file))
          (user-error "Target file does not exist: %s" target-file))
        (unless insert-info
          (user-error "No valid insert position selected."))

        (when (yes-or-no-p (format "Really move node %s and leave a link? " node-id))
          ;; 2. Get node content and original properties
          (let* ((element (org-element-at-point))
                 (begin (org-element-property :begin element))
                 (end (org-element-property :end element))
                 (original-level (org-element-property :level element))
                 (title (org-element-property :raw-value element))
                 (content (buffer-substring-no-properties begin end)))

            ;; 3. Insert into target file (same as move-node)
            (let ((adjusted-content (supertag-ui--adjust-content-level content original-level target-level)))
              (with-current-buffer (find-file-noselect target-file)
                (goto-char target-pos)
                (unless (or (bobp) (looking-back "\n" 1)) (insert "\n"))
                (insert adjusted-content)
                (save-buffer))
              (message "Pasted node into %s." (file-name-nondirectory target-file)))

            ;; 4. Update the database with the new location (same as move-node)
            (supertag-node-set-location node-id target-file target-pos)

            ;; 5. KEY DIFFERENCE: Replace original content with a link
            (delete-region begin end)
            (insert (make-string original-level ?*) " "
                    (format "[[id:%s][%s]]\n" node-id title))
            (save-buffer)

            (message "Node %s moved and link created." node-id))))))


;; --- Node Commands: Add, Remove Reference
    
(defun supertag-add-reference ()
  "Add a reference from the current node to another selected node."
  (interactive)
  (let ((from-id (org-id-get)))
    (unless from-id
      (user-error "Current heading does not have an ID. Cannot add reference."))

    (let ((to-id (supertag-ui-select-node "Add reference to node: " t))) ; Use cache for better performance
      (when to-id
        ;; 1. Create the relationship in the database
        (supertag-relation-create
         `(:type :reference :from ,from-id :to ,to-id))

        ;; 2. Insert the org-link into the buffer
        (let* ((target-node-props (supertag-get (list :nodes to-id)))
               (target-title (plist-get target-node-props :raw-value))
               (link-text (format "[[id:%s][%s]]" to-id (or target-title "Untitled Node"))))
          (insert link-text))

        (message "Reference added to node %s" to-id)))))

(defun supertag-remove-reference ()
  "Interactively remove a reference from the current node."
  (interactive)
  (let ((from-id (org-id-get)))
    (unless from-id
      (user-error "Current heading does not have an ID. Cannot remove reference."))

    (let ((to-id (supertag-ui-select-reference-to-remove from-id)))
      (when to-id
        ;; 1. Find and delete the relationship in the database
        (let* ((relations (supertag-relation-find-between from-id to-id :reference))
               (relation-to-delete (car relations)))
          (when relation-to-delete
            (supertag-relation-delete (plist-get relation-to-delete :id))))

        ;; 2. Optional: Find and delete the org-link from the buffer
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward (format "\\[\\[id:%s\\]" (regexp-quote to-id)) nil t)
            (goto-char (match-beginning 0))
            (when-let* ((link (org-element-context)))
              (when (and (eq (org-element-type link) 'link)
                         (string= (org-element-property :path link) to-id))
                (delete-region (org-element-property :begin link)
                               (org-element-property :end link))))))

        (message "Reference to node %s removed." to-id)))))

;; --- Tag Commands: add, remove ----

(defun supertag-add-tag ()
  "Interactively add a tag to the current node.
This command handles tag creation, linking, and smart insertion
of the inline #tag text into the buffer. Can be used both at headings
and within node content areas."
  (interactive)
  (let* ((node-id (supertag-ui--get-containing-node-at-point))
         (all-tags (mapcar #'car (supertag-query :tags))) ; For completion candidates
         (raw-name (completing-read "Add tag: " all-tags nil nil))
         (tag-id (supertag-sanitize-tag-name raw-name)))
    (when (and tag-id (not (string-empty-p tag-id)))
      (let ((create-new (and (not (supertag-tag-get tag-id))
                             (yes-or-no-p (format "Tag '%s' does not exist. Create it? " tag-id)))))
        (if (supertag-ops-add-tag-to-node node-id tag-id :create-if-needed create-new)
            (progn
              ;; Insert #tag text with smart spacing at current point
              (require 'supertag-view-helper)
              (supertag-view-helper-insert-tag-text tag-id)
              
              (message "Tag '%s' added to node '%s'." tag-id node-id))
          (message "Tag add cancelled or failed."))))))

(defun supertag-remove-tag-from-node ()
  "Interactively remove a tag from the current node.
Can be used both at headings and within node content areas."
  (interactive)
  (let* ((node-id (supertag-ui--get-containing-node-at-point))
     (tag-id (supertag-ui-select-tag-on-node node-id)))
      (when tag-id
        ;; 1. Find and delete the relationship in the database
        (let* ((relations (supertag-relation-find-between node-id tag-id :node-tag))
               (relation-to-delete (car relations)))
          (when relation-to-delete
            (supertag-relation-delete (plist-get relation-to-delete :id))))

        ;; 2. Remove tag text using view-helper
        (require 'supertag-view-helper)
        (let ((removed-count (supertag-view-helper-remove-tag-text tag-id)))
          (message "Tag '%s' removed from node %s (%d instances removed)." tag-id node-id removed-count)))))

;;; --- Enhanced Tag Management Commands ---

(defun supertag-rename-tag ()
  "Interactively rename a tag across all files."
  (interactive)
  (let* ((all-tags (mapcar #'car (supertag-query :tags)))
         (old-name (completing-read "Tag to rename: " all-tags nil t))
         (new-name (read-string (format "New name for '%s': " old-name))))
    (when (and (not (string-empty-p old-name))
               (not (string-empty-p new-name)))
      (let ((sanitized-new-name (supertag-sanitize-tag-name new-name)))
        (when (yes-or-no-p (format "Rename tag '%s' to '%s'? This will affect all files." old-name sanitized-new-name))
          ;; 1. Check if new tag name already exists
          (when (supertag-tag-get sanitized-new-name)
            (user-error "Tag '%s' already exists. Cannot rename to existing tag." sanitized-new-name))
          
          ;; 2. Get nodes with this tag before renaming
          (let* ((nodes-with-tag (supertag-find-nodes-by-tag old-name))
                 (files (delete-dups (mapcar (lambda (node-pair)
                                               (let ((node (cdr node-pair)))
                                                 (plist-get node :file)))
                                             nodes-with-tag))))
            
            ;; 3. Create new tag with same properties but new name
            (let ((old-tag (supertag-tag-get old-name)))
              (when old-tag
                (let ((new-tag-props (plist-put (copy-sequence old-tag) :name sanitized-new-name)))
                  (supertag-tag-create (plist-put new-tag-props :id sanitized-new-name)))))
            
            ;; 4. Update all node-tag relationships and node tags lists
            (dolist (node-pair nodes-with-tag)
              (let ((node-id (car node-pair)))
                ;; Remove old relationship
                (let ((old-relations (supertag-relation-find-between node-id old-name :node-tag)))
                  (dolist (rel old-relations)
                    (supertag-relation-delete (plist-get rel :id))))
                ;; Remove old tag from node's tags list
                (supertag-node-remove-tag node-id old-name)
                ;; Create new relationship
                (supertag-relation-create `(:type :node-tag :from ,node-id :to ,sanitized-new-name))
                ;; Add new tag to node's tags list
                (supertag-node-add-tag node-id sanitized-new-name)))
            
            ;; 5. Delete old tag using proper API
            (supertag-tag-delete old-name)
            
            ;; 6. Update text in all relevant files using helper component
            (require 'supertag-view-helper)
            (let ((total-renamed (supertag-view-helper-rename-tag-text-in-files old-name sanitized-new-name files)))
              (message "Tag renamed from '%s' to '%s' (%d total instances)." 
                       old-name sanitized-new-name total-renamed)))))))

(defun supertag-delete-tag-everywhere ()
  "Interactively delete a tag definition and all its instances.
WARNING: This removes the tag from the database and from all org files."
  (interactive)
  (let* ((all-tags (mapcar #'car (supertag-query :tags)))
         (tag-name (completing-read "Delete tag permanently: " all-tags nil t)))
    (when (and (not (string-empty-p tag-name))
               (yes-or-no-p (format "DELETE tag '%s' and ALL its uses? This is irreversible." tag-name)))
      ;; 1. Find all nodes with this tag before deletion
      (let* ((nodes-with-tag (supertag-find-nodes-by-tag tag-name))
             (files (delete-dups (mapcar (lambda (node-pair)
                                           (let ((node (cdr node-pair)))
                                             (plist-get node :file)))
                                         nodes-with-tag))))
        
        ;; 2. Clean up all node-tag relationships first
        (dolist (node-pair nodes-with-tag)
          (let* ((node-id (car node-pair))
                 (relations (supertag-relation-find-between node-id tag-name :node-tag)))
            (dolist (rel relations)
              (supertag-relation-delete (plist-get rel :id)))))
        
        ;; 3. Remove tag from all nodes' tags lists
        (dolist (node-pair nodes-with-tag)
          (let ((node-id (car node-pair)))
            (supertag-node-remove-tag node-id tag-name)))
        
        ;; 4. Use supertag-tag-delete for proper database cleanup
        (supertag-tag-delete tag-name)
        
        ;; 5. Remove tag text from all files using helper component
        (require 'supertag-view-helper)
        (let ((total-deleted (supertag-view-helper-remove-tag-text-from-files tag-name files)))
          (message "Tag '%s' completely deleted from database and all files (%d total instances removed)." 
                   tag-name total-deleted)))))))

(defun supertag-change-tag-at-point ()
  "Interactively change a tag at the current point to a different tag.
Can be used both at headings and within node content areas."
  (interactive)
  (require 'supertag-view-helper)
  (let* ((current-tag (supertag-view-helper-get-tag-at-point))
         (node-id (supertag-ui--get-containing-node-at-point)))
    (unless current-tag
      (user-error "No tag found at point"))
    
    (let* ((all-tags (mapcar #'car (supertag-query :tags)))
           (new-tag-raw (completing-read (format "Change tag '%s' to: " current-tag) all-tags nil nil))
           (new-tag (supertag-sanitize-tag-name new-tag-raw)))
      (when (and new-tag (not (string-empty-p new-tag)))
        ;; 1. Create new tag if it doesn't exist
        (unless (supertag-tag-get new-tag)
          (when (yes-or-no-p (format "Tag '%s' does not exist. Create it? " new-tag))
            (supertag-tag-create `(:name ,new-tag :id ,new-tag))))
        
        (when (supertag-tag-get new-tag)
          ;; 2. Update database relationships
          (let* ((old-relations (supertag-relation-find-between node-id current-tag :node-tag))
                 (old-relation (car old-relations)))
            (when old-relation
              (supertag-relation-delete (plist-get old-relation :id)))
            (supertag-relation-create `(:type :node-tag :from ,node-id :to ,new-tag)))
          
          ;; 3. Replace tag text at point
          (save-excursion
            (let* ((tag-re (concat "#[" supertag-view-helper--valid-tag-chars "]+"))
                   (start (point)))
              (when (re-search-backward tag-re nil t)
                (when (and (<= (match-beginning 0) start)
                           (> (match-end 0) start))
                  (replace-match (concat "#" new-tag))))))
          
          (message "Tag changed from '%s' to '%s'." current-tag new-tag))))))

;;; --- Tag Inheritance Commands ---

(defun supertag-set-tag-extends ()
  "Interactively set or clear tag inheritance relationship.
Sets a parent tag for a child tag, or clears inheritance if no parent is selected."
  (interactive)
  (let* ((all-tags (mapcar #'car (supertag-query :tags)))
         (child-tag-id (completing-read "Select child tag: " all-tags nil t))
         (parent-tag-id (completing-read "Select parent tag (leave empty to clear): " 
                                        (cons "" (cl-remove child-tag-id all-tags :test 'equal)) 
                                        nil nil)))
    
    ;; Validate child tag exists
    (unless (supertag-tag-get child-tag-id)
      (error "Child tag '%s' not found" child-tag-id))
    
    ;; Check for self-inheritance
    (when (and (not (string-empty-p parent-tag-id))
               (equal child-tag-id parent-tag-id))
      (user-error "A tag cannot inherit from itself"))
    
    ;; If parent tag is empty, clear inheritance
    (if (string-empty-p parent-tag-id)
        (progn
          ;; Get current extends value to know what to remove
          (let ((current-extends (plist-get (supertag-tag-get child-tag-id) :extends)))
            (when current-extends
              (if (listp current-extends)
                  ;; If extends is a list, remove all parent relationships
                  (dolist (parent current-extends)
                    (supertag-tag-remove-extends child-tag-id parent))
                ;; If extends is a single value, remove that relationship
                (supertag-tag-remove-extends child-tag-id current-extends)))
            (message "Cleared inheritance for tag '%s'" child-tag-id)))
      ;; Validate parent tag exists
      (unless (supertag-tag-get parent-tag-id)
        (error "Parent tag '%s' not found" parent-tag-id))
      
      ;; Set inheritance relationship
      (supertag-tag-add-extends child-tag-id parent-tag-id)
      (message "Tag '%s' now inherits from '%s'" child-tag-id parent-tag-id))))

;;; --- Capture Commands ---

(defvar supertag-capture--last-node-id nil
  "Stores the ID of the last node created during capture for enrichment.")

(defun supertag-capture (&optional target-file headline)
  "Independent capture command for Org-Supertag.
Creates a new node with optional tags and field values.
TARGET-FILE is optional file path to capture to.
HEADLINE is optional headline text."
  (interactive)
  
  ;; Phase 1: Get capture details
  (let* ((capture-info (supertag-capture-interactive-headline))
         (full-title (plist-get capture-info :headline))
         (selected-tags (plist-get capture-info :tags))
         (target-file (or target-file (read-file-name "Capture to file: ")))
         (insert-info (supertag-ui-select-insert-position target-file))
         (insert-pos (plist-get insert-info :position))
         (insert-level (plist-get insert-info :level)))
    
    (unless insert-info
      (user-error "No valid insert position selected"))
    
    ;; Phase 2: Create the node in the file
    (with-current-buffer (find-file-noselect target-file)
      (save-excursion
        (goto-char insert-pos)
        (unless (or (bobp) (looking-back "\n" 1)) (insert "\n"))
        (insert (make-string insert-level ?*) " " full-title "\n")
        (insert ":PROPERTIES:\n:ID: " (org-id-new) "\n:END:\n")
        (save-buffer))
      
      ;; Phase 3: Sync and enrich
      (let ((node-id (org-id-get)))
        (when node-id
          (supertag-services-sync-file target-file)
          (message "Node %s created in %s" node-id (file-name-nondirectory target-file))
          
          ;; Phase 4: Auto field enrichment for tags with fields
          (when selected-tags
            (let ((fields (supertag-capture--get-fields-for-tags selected-tags)))
              (when fields
                (let ((field-values (supertag-capture--prompt-for-field-values fields)))
                  (dolist (fv field-values)
                    ;; Use field operations to set field values properly
                    (dolist (tag-id selected-tags)
                      (supertag-field-set node-id tag-id (car fv) (cdr fv))))))))
          
          ;; Phase 5: Optional manual field enrichment
          (when (y-or-n-p "Add additional properties to this node? ")
            (supertag-capture-enrich-node node-id))
          
          node-id)))))


(provide 'supertag-ui-commands)

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
(require 'supertag-services-sync) ; For sync services
(require 'supertag-services-capture) ; For capture services
(require 'supertag-core-store) ; For supertag--rebuild-all-indexes

;;; --- Customization ---

(defcustom supertag-batch-tag-insert-position 'end
  "Where to insert tags when adding tags in batch mode.
- 'end: Insert tags at the end of the heading (default)
- 'beginning: Insert tags at the beginning of the heading (after the stars and TODO keyword if any)"
  :type '(choice (const :tag "End of heading" end)
                 (const :tag "Beginning of heading" beginning))
  :group 'org-supertag)

(defcustom supertag-capture-tag-position 'end
  "Where to place tags when creating a headline via capture.
- 'end: Keep tags after the title (default, preserves current behavior).
- 'beginning: Insert tags immediately after the leading stars/TODO keyword."
  :type '(choice (const :tag "End of headline" end)
                 (const :tag "Beginning of headline" beginning))
  :group 'org-supertag)

(defun supertag-set-child (parent-tag child-tags)
  "Set CHILD-TAGS to extend a PARENT-TAG using interactive completion.
When invoked interactively, allows selecting multiple child tags." 
  (interactive
   (let* ((tags (mapcar #'car (supertag-query :tags))))
     (when (null tags)
       (user-error "No tags available"))
     (let* ((parent (completing-read "Parent tag: " tags nil t))
            (child-candidates (remove parent (copy-sequence tags)))
            (children (completing-read-multiple
                       (format "Child tags for '%s' (comma-separated): " parent)
                       child-candidates nil t)))
       (when (null children)
         (user-error "You must select at least one child tag"))
       (list parent children))))
  (let ((children (if (listp child-tags) child-tags (list child-tags))))
    (dolist (child children)
      (when (and child (not (string-empty-p child)))
        (supertag--set-parent child parent-tag)))
    (when (called-interactively-p 'interactive)
      (message "Tags %s now extend %s"
               (mapconcat #'identity children ", ") parent-tag))
    children))

(defun supertag-clear-parent (child-tags)
  "Clear parent relationships for one or more CHILD-TAGS."
  (interactive
   (let* ((tags (mapcar #'car (supertag-query :tags))))
     (when (null tags)
       (user-error "No tags available"))
     (let ((children (completing-read-multiple
                      "Clear parent for tag(s): " tags nil t)))
       (when (null children)
         (user-error "You must select at least one tag"))
       (list children))))
  (let ((children (if (listp child-tags) child-tags (list child-tags))))
    (dolist (child children)
      (when (and child (not (string-empty-p child)))
        (supertag--clear-parent child)))
    (when (called-interactively-p 'interactive)
      (message "Cleared parent for tag(s): %s"
               (mapconcat #'identity children ", ")))
    children))

(defun supertag-ui--get-nodes-in-region (beg end)
  "Extract all node IDs from Org headings within the region BEG to END.
Returns a list of node IDs. Creates IDs for headings that don't have one.
Only includes headings whose starting position is within [BEG, END)."
  (let ((node-ids '()))
    (save-excursion
      (goto-char beg)
      ;; Move to the beginning of the first heading in or after BEG
      (unless (org-at-heading-p)
        (org-next-visible-heading 1))
      
      ;; Collect all headings that start within the region
      (while (and (not (eobp))
                  (org-at-heading-p)
                  (< (point) end))  ; Heading must start before END
        (let ((heading-start (point))
              (node-id (org-id-get-create)))
          ;; Only include if heading starts within region
          (when (>= heading-start beg)
            (push node-id node-ids)))
        (org-next-visible-heading 1)))
    (nreverse node-ids)))

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
           (headline-tags (supertag-transform-extract-inline-tags title))
           (content-tags (supertag-transform-extract-inline-tags content))
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
  "Find a node by its title/path, with live preview in another window.
Jumps to the selected node in another window."
  (interactive)
  (let ((node-id (supertag-ui-select-node "Find node (other window): " t t))) ; Use cache & preview
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

(defun supertag-move-node (&optional beg end)
  "Interactively move node(s) to another file.
If region is active (BEG and END provided), move all nodes in the region.
Otherwise, move the node at point.
The node's content (the entire subtree) will be cut from the
current file and inserted into the target file at a chosen position."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))
  
  (let ((node-ids (if (and beg end)
                      ;; Batch mode: get all nodes in region
                      (supertag-ui--get-nodes-in-region beg end)
                    ;; Single mode: get node at point
                    (unless (org-at-heading-p)
                      (user-error "Point must be at a heading to move a node."))
                    (let ((node-id (org-id-get)))
                      (unless node-id
                        (user-error "Current heading does not have an ID, it is not a node."))
                      (list node-id)))))
    
    (unless node-ids
      (user-error "No nodes found to move."))
    
    ;; 1. Prompt for target file and position
    (let* ((target-file (read-file-name "Move node(s) to file: "))
           (insert-info (supertag-ui-select-insert-position target-file))
           (target-pos (plist-get insert-info :position))
           (target-level (plist-get insert-info :level)))
      (unless (and target-file (file-exists-p target-file))
        (user-error "Target file does not exist: %s" target-file))
      (unless insert-info
        (user-error "No valid insert position selected."))

      (when (yes-or-no-p (format "Really move %d node(s) to %s? "
                                 (length node-ids)
                                 (file-name-nondirectory target-file)))
        (require 'supertag-ops-batch)
        (supertag-with-transaction
          (let ((current-target-pos target-pos)
                (nodes-to-move '()))
            
            ;; 2. First pass: collect all node data before any modifications
            ;;    Read directly from current buffer to avoid database dependency
            (dolist (node-id node-ids)
              (let* ((marker (org-id-find node-id 'marker)))
                (when marker
                  (with-current-buffer (marker-buffer marker)
                    (goto-char (marker-position marker))
                    (when (org-at-heading-p)
                      (let* ((element (org-element-at-point))
                             (begin (org-element-property :begin element))
                             (end (org-element-property :end element))
                             (original-level (org-element-property :level element))
                             (content (buffer-substring-no-properties begin end))
                             (node-file (buffer-file-name)))
                        (push (list :id node-id
                                   :file node-file
                                   :begin begin
                                   :end end
                                   :level original-level
                                   :content content)
                              nodes-to-move)))))))
            
            (setq nodes-to-move (nreverse nodes-to-move))
            
            ;; 3. Second pass: group nodes by file and delete from each file
            ;;    (in reverse position order to preserve positions)
            (let ((nodes-by-file (make-hash-table :test 'equal)))
              ;; Group nodes by file
              (dolist (node-info nodes-to-move)
                (let ((file (plist-get node-info :file)))
                  (push node-info (gethash file nodes-by-file))))
              
              ;; Delete from each file (nodes in reverse position order)
              (maphash
               (lambda (file nodes-in-file)
                 (let ((sorted-nodes (sort nodes-in-file
                                          (lambda (a b)
                                            (> (plist-get a :begin)
                                               (plist-get b :begin))))))
                   (with-current-buffer (find-file-noselect file)
                     (dolist (node-info sorted-nodes)
                       (let ((begin (plist-get node-info :begin))
                             (end (plist-get node-info :end)))
                         (delete-region begin end)))
                     (save-buffer))))
               nodes-by-file))
            
            ;; 4. Third pass: insert into target file and update database
            (dolist (node-info nodes-to-move)
              (let* ((node-id (plist-get node-info :id))
                     (content (plist-get node-info :content))
                     (original-level (plist-get node-info :level))
                     (adjusted-content (supertag-ui--adjust-content-level content original-level target-level))
                     (node-start-pos nil))
                
                (with-current-buffer (find-file-noselect target-file)
                  (goto-char current-target-pos)
                  (unless (or (bobp) (looking-back "\n" 1)) (insert "\n"))
                  ;; Record the position where the node starts
                  (setq node-start-pos (point))
                  (insert adjusted-content)
                  ;; Update position for next node (after current insertion)
                  (setq current-target-pos (point))
                  (save-buffer))
                
                ;; Update the database with the new location (use node start position)
                (supertag-node-set-location node-id target-file node-start-pos)))
            
            (message "%d node(s) successfully moved to %s."
                     (length nodes-to-move)
                     (file-name-nondirectory target-file))))))))

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
        (let* ((target-node-props (supertag-store-get-entity :nodes to-id))
               (target-title (plist-get target-node-props :raw-value))
               (link-text (format "[[id:%s][%s]]" to-id (or target-title "Untitled Node"))))
          (insert link-text))

        (message "Reference added to node %s" to-id))))) 

(defun supertag-add-reference-and-create (beg end) 
  "Create a new node from the selected region and replace it with a link. 
  Interactively asks for a target location to save the new node." 
  (interactive "r") 
  (let* ((title (buffer-substring-no-properties beg end))) 
    (if (or (null title) (string-empty-p title)) 
        (user-error "Region is empty. Cannot create a node.") 
      ;; 1. Get target location from user 
      (let* ((target-file (read-file-name "Create node in file: " nil nil t)) 
             (insert-info (when (and target-file (file-exists-p target-file)) 
                            (supertag-ui-select-insert-position target-file))) 
             (insert-pos (plist-get insert-info :position)) 
             (insert-level (plist-get insert-info :level))) 

        (unless insert-info 
          (user-error "No valid insert position selected. Aborting.")) 

        (let ((new-node-id (org-id-new))) 
          ;; 2. Create the node in the target file (physical insertion) 
          (with-current-buffer (find-file-noselect target-file) 
            (goto-char insert-pos) 
            ;; Ensure we are on a new line before inserting 
            (unless (or (bobp) (looking-back "\n" 1)) (insert "\n")) 
            (insert (format "%s %s\n:PROPERTIES:\n:ID:       %s\n:END:\n" 
                            (make-string insert-level ?*) 
                            title 
                            new-node-id)) 
            (save-buffer)) 

          ;; 3. Create the node in the database (logical creation) 
          (supertag-node-create 
           `(:id ,new-node-id 
             :title ,title 
             :file ,target-file 
             :position ,insert-pos 
             :level ,insert-level)) 

          ;; 4. Create the relationship in the database
          (let ((from-id (org-id-get)))
            (when from-id
              (supertag-relation-create
               `(:type :reference :from ,from-id :to ,new-node-id))))

          ;; 5. Replace original text with a link 
          (delete-region beg end) 
          (insert (format "[[id:%s][%s]]" new-node-id title)) 

          (message "Node '%s' created and linked." title)))))) 

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

;; --- Embed Commands ---

(defun supertag-insert-embed ()
  "Insert an embed block at point by selecting a node.
This command provides a convenient way to embed node content directly
without first creating a link. It will prompt you to select a node
and then insert the embed block at the current position."
  (interactive)
  (require 'supertag-ui-embed)
  (supertag-ui-embed--insert-block))

(defun supertag-convert-link-to-embed ()
  "Convert the org id: link at point to an embed block.
This command provides a user-friendly interface to convert an existing
id: link into an embed block that displays the node's content inline."
  (interactive)
  (require 'supertag-ui-embed)
  (supertag-ui-embed--link-to-block))

;; --- Tag Commands: add, remove ----

(defun supertag-add-tag (&optional beg end)
  "Interactively add a tag to node(s).
If region is active (BEG and END provided), add tag to all nodes in the region.
Otherwise, add tag to the node at point.
This command handles tag creation, linking, and smart insertion
of the inline #tag text into the buffer. Can be used both at headings
and within node content area.

If you prefix your input with '=' (e.g. '=ref'), it will be treated as a literal
new tag name, bypassing fuzzy completion matching."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))
  
  (let* ((batch-mode (and beg end))
         (current-point (point))  ; Save current cursor position for single mode
         (node-ids (if batch-mode
                       ;; Batch mode: get all nodes in region
                       (supertag-ui--get-nodes-in-region beg end)
                     ;; Single mode: get node at point
                     (let ((node-id (supertag-ui--get-containing-node-at-point)))
                       ;; Ensure the node exists in the database before proceeding.
                       (unless (supertag-node-get node-id)
                         (supertag-node-sync-at-point))
                       (list node-id))))
         (all-tags (mapcar #'car (supertag-query :tags))) ; For completion candidates
         (raw-name (completing-read (format "Add tag to %d node(s) (use =tagname for exact match): "
                                            (length node-ids))
                                    all-tags nil nil))
         (literal-tag (and (> (length raw-name) 0) (eq (aref raw-name 0) ?=))))
    
    (unless node-ids
      (user-error "No nodes found to add tag to."))
    
    (when (and raw-name (not (string-empty-p raw-name)))
      (let* ((tag-name (if literal-tag
                          (substring raw-name 1) ; Remove the '=' prefix
                        raw-name))
             (tag-id (supertag-sanitize-tag-name tag-name)))
        (if literal-tag
            ;; Direct creation for literal tag names
            (when (yes-or-no-p (format "Create new tag '%s' and add to %d node(s)? "
                                       tag-id (length node-ids)))
              (require 'supertag-ops-batch)
              (require 'supertag-view-helper)
              (supertag-with-transaction
                (dolist (node-id node-ids)
                  ;; Ensure node exists
                  (unless (supertag-node-get node-id)
                    (let* ((marker (org-id-find node-id 'marker)))
                      (when marker
                        (with-current-buffer (marker-buffer marker)
                          (goto-char (marker-position marker))
                          (supertag-node-sync-at-point)))))
                  
                  (when (supertag-ops-add-tag-to-node node-id tag-id :create-if-needed t)
                    ;; Insert #tag text
                    (let* ((marker (org-id-find node-id 'marker)))
                      (when marker
                        (with-current-buffer (marker-buffer marker)
                          (if batch-mode
                              ;; Batch mode: insert based on user preference
                              (progn
                                (goto-char (marker-position marker))
                                (when (org-at-heading-p)
                                  (if (eq supertag-batch-tag-insert-position 'beginning)
                                      ;; Insert at beginning (after stars and TODO keyword)
                                      (progn
                                        (org-back-to-heading t)
                                        (forward-word)  ; Skip stars
                                        (when (org-get-todo-state)
                                          (forward-word))  ; Skip TODO keyword if present
                                        (supertag-view-helper-insert-tag-text tag-id))
                                    ;; Insert at end (default)
                                    (end-of-line)
                                    (supertag-view-helper-insert-tag-text tag-id))))
                            ;; Single mode: insert at current cursor position
                            (goto-char current-point)
                            (supertag-view-helper-insert-tag-text tag-id))
                          (save-buffer)))))))
              (message "Tag '%s' created and added to %d node(s)." tag-id (length node-ids)))
          ;; Original behavior for fuzzy matching
          (let ((tag-exists (supertag-tag-get tag-id)))
            (cond
             ;; If tag already exists, use it directly
             (tag-exists
              (require 'supertag-ops-batch)
              (require 'supertag-view-helper)
              (supertag-with-transaction
                (dolist (node-id node-ids)
                  ;; Ensure node exists
                  (unless (supertag-node-get node-id)
                    (let* ((marker (org-id-find node-id 'marker)))
                      (when marker
                        (with-current-buffer (marker-buffer marker)
                          (goto-char (marker-position marker))
                          (supertag-node-sync-at-point)))))
                  
                  (when (supertag-ops-add-tag-to-node node-id tag-id :create-if-needed nil)
                    ;; Insert #tag text
                    (let* ((marker (org-id-find node-id 'marker)))
                      (when marker
                        (with-current-buffer (marker-buffer marker)
                          (if batch-mode
                              ;; Batch mode: insert based on user preference
                              (progn
                                (goto-char (marker-position marker))
                                (when (org-at-heading-p)
                                  (if (eq supertag-batch-tag-insert-position 'beginning)
                                      ;; Insert at beginning (after stars and TODO keyword)
                                      (progn
                                        (org-back-to-heading t)
                                        (forward-word)  ; Skip stars
                                        (when (org-get-todo-state)
                                          (forward-word))  ; Skip TODO keyword if present
                                        (supertag-view-helper-insert-tag-text tag-id))
                                    ;; Insert at end (default)
                                    (end-of-line)
                                    (supertag-view-helper-insert-tag-text tag-id))))
                            ;; Single mode: insert at current cursor position
                            (goto-char current-point)
                            (supertag-view-helper-insert-tag-text tag-id))
                          (save-buffer)))))))
              (message "Tag '%s' added to %d node(s)." tag-id (length node-ids)))
             ;; If tag doesn't exist, create it
             (t
              (when (yes-or-no-p (format "Tag '%s' does not exist. Create it and add to %d node(s)? "
                                         tag-id (length node-ids)))
                (require 'supertag-ops-batch)
                (require 'supertag-view-helper)
                (supertag-with-transaction
                  (dolist (node-id node-ids)
                    ;; Ensure node exists
                    (unless (supertag-node-get node-id)
                      (let* ((marker (org-id-find node-id 'marker)))
                        (when marker
                          (with-current-buffer (marker-buffer marker)
                            (goto-char (marker-position marker))
                            (supertag-node-sync-at-point)))))
                    
                    (when (supertag-ops-add-tag-to-node node-id tag-id :create-if-needed t)
                      ;; Insert #tag text
                      (let* ((marker (org-id-find node-id 'marker)))
                        (when marker
                          (with-current-buffer (marker-buffer marker)
                            (if batch-mode
                                ;; Batch mode: insert based on user preference
                                (progn
                                  (goto-char (marker-position marker))
                                  (when (org-at-heading-p)
                                    (if (eq supertag-batch-tag-insert-position 'beginning)
                                        ;; Insert at beginning (after stars and TODO keyword)
                                        (progn
                                          (org-back-to-heading t)
                                          (forward-word)  ; Skip stars
                                          (when (org-get-todo-state)
                                            (forward-word))  ; Skip TODO keyword if present
                                          (supertag-view-helper-insert-tag-text tag-id))
                                      ;; Insert at end (default)
                                      (end-of-line)
                                      (supertag-view-helper-insert-tag-text tag-id))))
                              ;; Single mode: insert at current cursor position
                              (goto-char current-point)
                              (supertag-view-helper-insert-tag-text tag-id))
                            (save-buffer)))))))
                (message "Tag '%s' created and added to %d node(s)." tag-id (length node-ids)))))))))))

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
                       old-name sanitized-new-name total-renamed))))))))

(defun supertag-delete-tag-everywhere ()
  "Interactively delete a tag definition and all its instances.
WARNING: This removes the tag from the database and from all org files."
  (interactive)
  (let* ((all-tags (mapcar #'car (supertag-query :tags)))
         (tag-name (completing-read "Delete tag permanently: " all-tags nil t)))
    (when (and (not (string-empty-p tag-name))
               (yes-or-no-p (format "DELETE tag '%s' and ALL its uses? This is irreversible." tag-name)))
      ;; Call the centralized ops function to perform the deletion.
      (supertag-ops-delete-tag-everywhere tag-name))))

(defun supertag-ui-select-tag-on-node (node-id)
  "Interactively select a tag from the ones associated with NODE-ID.
Returns the selected tag ID (a string), or nil if canceled."
  (let* ((node (supertag-node-get node-id))
         (tags (and node (plist-get node :tags))))
    (unless tags
      (user-error "Node has no tags to select from."))
    (completing-read "Select tag: " tags nil t)))

(defun supertag-change-tag-at-point ()
  "Interactively change a tag on the current node to a different tag.
This command reads the authoritative list of tags from the database."
  (interactive)
  (require 'supertag-view-helper)
  (let* ((node-id (supertag-ui--get-containing-node-at-point))
         (current-tag (supertag-ui-select-tag-on-node node-id)))
    (unless current-tag
      (user-error "No tag selected."))
    
    (let* ((all-tags (mapcar #'car (supertag-query :tags)))
           (new-tag-raw (completing-read (format "Change tag '%s' to: " current-tag) all-tags nil nil))
           (new-tag (supertag-sanitize-tag-name new-tag-raw)))
      (when (and new-tag (not (string-empty-p new-tag)))
        ;; 1. Create new tag if it doesn't exist
        (unless (supertag-tag-get new-tag)
          (when (yes-or-no-p (format "Tag '%s' does not exist. Create it? " new-tag))
            (supertag-tag-create `(:name ,new-tag :id ,new-tag))))
        
        (when (supertag-tag-get new-tag)
          ;; 2. Update database relationships and node data
          (supertag-with-transaction
            ;; Remove old relation
            (let* ((old-relations (supertag-relation-find-between node-id current-tag :node-tag))
                   (old-relation (car old-relations)))
              (when old-relation
                (supertag-relation-delete (plist-get old-relation :id))))
            ;; Remove old tag from node's list
            (supertag-node-remove-tag node-id current-tag)
            
            ;; Add new relation
            (supertag-relation-create `(:type :node-tag :from ,node-id :to ,new-tag))
            ;; Add new tag to node's list
            (supertag-node-add-tag node-id new-tag))
          
          ;; 3. Replace all instances of the old tag text with the new one in the buffer
          (supertag-view-helper-rename-tag-text-in-node current-tag new-tag)
          
          (message "Tag changed from '%s' to '%s'." current-tag new-tag))))))

;;; --- Tag Inheritance Commands (REMOVED) ---
;; Note: Tag inheritance functionality has been removed for simplicity
;; and better data consistency. Tags no longer support extends relationships.

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
         ;; Optional body content below the headline
         (body (read-string "Body (optional, RET to skip): "))
         (insert-info (supertag-ui-select-insert-position target-file))
         (insert-pos (plist-get insert-info :position))
         (insert-level (plist-get insert-info :level)))
    
    (unless insert-info
      (user-error "No valid insert position selected"))
    
    ;; Phase 2: Create the node in the file
    (let ((new-node-id (org-id-new)))
      (supertag-capture--insert-node-into-buffer
       (find-file-noselect target-file)
       insert-pos insert-level full-title selected-tags body new-node-id
       supertag-capture-tag-position)

      ;; Phase 3: Sync and enrich
      (let ((node-id new-node-id))
        (when node-id
          (supertag-node-create (list :id node-id
                                      :title full-title
                                      :tags selected-tags
                                      :file target-file))
          (message "Node %s created in %s" node-id (file-name-nondirectory target-file))
          
          ;; Phase 4: Auto field enrichment for tags with fields
          (when selected-tags
            (let ((fields (supertag-capture--get-fields-for-tags selected-tags)))
              (when fields
                (let ((field-values (supertag-capture--prompt-for-field-values fields)))
                  (dolist (fv field-values)
                    ;; Use field operations to set field values properly
                    (dolist (tag-id selected-tags) (supertag-field-set node-id tag-id (car fv) (cdr fv))))))))
          
          ;; Phase 5: Optional manual field enrichment
          (when (y-or-n-p "Add additional properties to this node? ")
            (supertag-capture-enrich-node node-id))
          
          node-id)))))

(defun supertag-capture-with-template (&optional template-key)
  "Capture using a dynamic template.
If TEMPLATE-KEY is not provided, prompts for one."
  (interactive)
  (unless supertag-capture-templates
    (user-error "No templates defined. Please set `supertag-capture-templates'"))

  ;; 1. Select Template
  (let* ((template-alist supertag-capture-templates)
         ;; Create an alist of ("KEY - DESCRIPTION" . TEMPLATE-DATA) for completion.
         (completion-alist (mapcar (lambda (template)
                                     (cons (format "%s - %s" (car template) (cadr template))
                                           template))
                                   template-alist))
         (selected-display (completing-read "Template: " (mapcar #'car completion-alist) nil t))
         (selected-template (cdr (assoc selected-display completion-alist)))
         (key (car selected-template))
         ;; The rest of the template is the plist part.
         (template-data (cddr selected-template)))
    (unless template-data
      (user-error "Template doesn't exist: %s" key))
    
    (let* ((target-file (plist-get template-data :file))
           (raw-node-spec (plist-get template-data :node-spec))
           ;; Check if the node-spec is a :template string and parse it.
           (node-spec (if (and (consp raw-node-spec) (eq (car raw-node-spec) :template))
                          (supertag-capture--parse-template-string (cadr raw-node-spec))
                        raw-node-spec)))
      ;; If no target file specified, use move-node logic to select target
      (if target-file
          ;; Case 1: Template has :file - use it directly
          (progn
            ;; Expand the file path to handle ~ and relative paths
            (setq target-file (expand-file-name target-file))
            (unless (file-exists-p target-file)
              (user-error "Target file does not exist: %s" target-file))
            ;; Process spec into data and execute
            (let* ((processed-data (supertag-capture--process-spec node-spec))
                   (tag-position (or (plist-get processed-data :tag-position)
                                     supertag-capture-tag-position)))
              (supertag-capture--execute target-file processed-data tag-position)))
        ;; Case 2: No :file specified - use move-node style selection
        (let* ((processed-data (supertag-capture--process-spec node-spec))
               (title (plist-get processed-data :title))
               (tags (plist-get processed-data :tags))
               (body (plist-get processed-data :body))
               (field-settings (plist-get processed-data :fields))
               ;; Use move-node style file selection
               (selected-file (read-file-name "Capture node to file: " nil nil t))
               (insert-info (supertag-ui-select-insert-position selected-file))
               (insert-pos (plist-get insert-info :position))
               (insert-level (plist-get insert-info :level)))
          (unless insert-info
            (user-error "No valid insert position selected"))
          ;; Create node directly at selected location
          (let* ((tag-position (or (plist-get processed-data :tag-position)
                                   supertag-capture-tag-position))
                 (new-node-id (org-id-new)))
            (supertag-capture--insert-node-into-buffer
             (find-file-noselect selected-file)
             insert-pos insert-level title tags body new-node-id tag-position)
            ;; Create database record and set fields
            (supertag-node-create (list :id new-node-id
                                        :title title
                                        :tags tags
                                        :file selected-file))
            (message "Node %s created in %s" new-node-id (file-name-nondirectory selected-file))
            ;; Set field values from template
            (when field-settings
              (dolist (f-spec field-settings)
                (let* ((f-tag (plist-get f-spec :tag))
                       (f-field (plist-get f-spec :field))
                       (f-get (plist-get f-spec :get))
                       (f-value (if f-get
                                    (supertag-capture--get-content f-get)
                                  (plist-get f-spec :value))))
                  (when (and f-tag f-field f-value)
                    (supertag-field-set new-node-id f-tag f-field f-value)
                    (message "Set field: %s/%s -> %s" f-tag f-field f-value)))))))))))

;;; --- Sync Commands ---

;;;###autoload
(defun supertag-sync-full-initialize ()
 "Perform full initialization sync for new users.
This command will clear all sync state and reimport all files from
configured directories into the database. Intended for first-time
setup or when rebuilding the entire database."
 (interactive)
 (when (yes-or-no-p "This will clear all sync state and reimport all files. Continue? ")
   (message "Starting full initialization sync...")
   
   ;; Step 1: Clear sync state
   (message "Step 1: Clearing sync state...")
   (setq supertag-sync--state (make-hash-table :test 'equal))
   (supertag-sync-save-state)
   
   ;; Step 2: Get all files in sync directories
   (message "Step 2: Scanning all files in sync directories...")
   (let ((all-files (supertag-scan-sync-directories t)) ; Force scan all files
         (counters '(:nodes-created 0 :nodes-updated 0 :nodes-deleted 0
                     :references-created 0 :references-deleted 0))
         (total-files 0)
         (processed-files 0))
     
     (setq total-files (length all-files))
     (message "Found %d files to process" total-files)
     
     (if (= total-files 0)
         (message "No files found in sync directories: %s" org-supertag-sync-directories)
       (progn
         ;; Step 3: Process each file within transaction
         (message "Step 3: Processing files...")
         (supertag-with-transaction
           (dolist (file all-files)
             (setq processed-files (1+ processed-files))
             (message "Processing file %d/%d: %s" processed-files total-files
                      (file-name-nondirectory file))
             
             (condition-case err
                 (progn
                   ;; Force process the file (ignore existing state)
                   (supertag-sync--process-single-file file counters)
                   ;; Update sync state for the file
                   (supertag-sync-update-state file))
               (error
                (message "ERROR processing file %s: %s" file err)))))
         
         ;; Step 4: Save state and report results
         (supertag-sync-save-state)
         
         (let ((nodes-created (plist-get counters :nodes-created))
               (nodes-updated (plist-get counters :nodes-updated))
               (refs-created (or (plist-get counters :references-created) 0)))
           (message "Full initialization completed!")
           (message "Results: %d files processed, %d nodes created, %d nodes updated, %d references created"
                    processed-files nodes-created nodes-updated refs-created)
           
           ;; Show summary
           (when (> nodes-created 0)
             (message "Database successfully initialized with %d nodes from %d files."
                      nodes-created processed-files))
           
           (when (= nodes-created 0)
             (message "WARNING: No nodes were created. Please check:")
             (message "  - org-supertag-sync-directories: %s" org-supertag-sync-directories)
             (message "  - supertag-sync-file-pattern: %s" supertag-sync-file-pattern)
             (message "  - File contents have proper org headings with IDs"))))))))

;;;###autoload
(defun supertag-sync-force-resync-file (&optional file)
 "Force resync a specific file, ignoring existing sync state.
If FILE is not provided, prompt user to select a file."
 (interactive)
 (let ((target-file (or file
                        (read-file-name "Force resync file: " nil nil t))))
   (unless (file-exists-p target-file)
     (user-error "File does not exist: %s" target-file))
   
   (unless (supertag-sync--in-sync-scope-p target-file)
     (user-error "File is not in sync scope: %s" target-file))
   
   (when (yes-or-no-p (format "Force resync file %s? " (file-name-nondirectory target-file)))
     (message "Force resyncing file: %s" target-file)
     
     ;; Remove from sync state to force processing
     (let ((state-table (supertag-sync--get-state-table)))
       (remhash target-file state-table))
     
     ;; Process the file
     (let ((counters '(:nodes-created 0 :nodes-updated 0 :nodes-deleted 0
                       :references-created 0 :references-deleted 0)))
       (supertag-with-transaction
         (supertag-sync--process-single-file target-file counters))
       
       ;; Update state and report
       (supertag-sync-update-state target-file)
       (supertag-sync-save-state)
       
       (message "Force resync completed: %d created, %d updated, %d deleted"
                (plist-get counters :nodes-created)
                (plist-get counters :nodes-updated)
                (plist-get counters :nodes-deleted))))))

;;;###autoload
(defun supertag-sync-force-resync-current-file ()
 "Force resync the current file."
 (interactive)
 (unless (buffer-file-name)
   (user-error "Current buffer is not visiting a file"))
 (supertag-sync-force-resync-file (buffer-file-name)))

;;;###autoload
(defun supertag-sync-reset-state ()
 "Reset all sync state without touching the database.
This forces all files to be considered 'modified' on next sync."
 (interactive)
 (when (yes-or-no-p "This will reset all sync state. Continue? ")
   (setq supertag-sync--state (make-hash-table :test 'equal))
   (supertag-sync-save-state)
   (message "Sync state reset. All files will be reprocessed on next sync.")))

;;;###autoload
(defun supertag-start-auto-sync (&optional interval)
  "Start automatic synchronization.
If INTERVAL is provided, use it as the sync interval in seconds."
  (interactive "P")
  (let ((sync-interval (if interval
                          (prefix-numeric-value interval)
                        supertag-sync-auto-interval)))
    (supertag-sync-start-auto-sync sync-interval)
    (message "Auto-sync started with %d second interval." sync-interval)))

;;;###autoload
(defun supertag-stop-auto-sync ()
  "Stop automatic synchronization."
  (interactive)
  (supertag-sync-stop-auto-sync)
  (message "Auto-sync stopped."))

;;;###autoload
(defun supertag-sync-check-now ()
 "Immediately check and sync modified files."
 (interactive)
 (message "Starting manual sync check...")
 (supertag-sync--check-and-sync)
 (message "Manual sync check completed."))

;;;###autoload
(defun supertag-sync-status ()
 "Show current sync status and configuration."
 (interactive)
 (let* ((state-table (supertag-sync--get-state-table))
        (num-tracked-files (hash-table-count state-table))
        (modified-files (supertag-get-modified-files))
        (num-modified (length modified-files))
        (timer-active (and supertag-sync--timer (not (null supertag-sync--timer)))))
   
   (message "=== Supertag Sync Status ===")
   (message "Sync directories: %s" org-supertag-sync-directories)
   (message "Exclude directories: %s" supertag-sync-exclude-directories)
   (message "File pattern: %s" supertag-sync-file-pattern)
   (message "Auto-sync: %s" (if timer-active "ACTIVE" "INACTIVE"))
   (message "Tracked files: %d" num-tracked-files)
   (message "Modified files: %d" num-modified)
   
   (when (> num-modified 0)
     (message "Modified files:")
     (dolist (file modified-files)
       (message "  - %s" file)))))

;;;###autoload
(defun supertag-sync-cleanup-database ()
 "Perform database maintenance by validating nodes and cleaning up orphaned data."
 (interactive)
 (when (yes-or-no-p "This will validate all nodes and clean up orphaned data. Continue? ")
   (message "Starting database cleanup...")
   
   ;; Step 1: Validate all nodes and mark zombies as orphaned
   (let ((counters '(:nodes-deleted 0)))
     (supertag-sync-validate-nodes counters)
     (message "Node validation complete. %d nodes marked as orphaned."
              (plist-get counters :nodes-deleted))
     
     ;; Step 2: Garbage collect all orphaned nodes
     (let ((deleted-count (supertag-sync-garbage-collect-orphaned-nodes)))
       (message "Database cleanup complete. %d orphaned nodes deleted." deleted-count)))))

;;;###autoload


;;;###autoload
(defun supertag-cleanup-nil-tags ()
  "Find and remove any 'ghost' tags from the database.
A 'ghost' tag is a tag entry that has a nil value, which can
cause inconsistencies in the system. This command cleans them up."
  (interactive)
  (let ((tags-to-remove '())
        (tags-table (supertag-store-get-collection :tags)))
    (when tags-table
      (maphash (lambda (key value)
                 (when (null value)
                   (push key tags-to-remove)))
               tags-table))
    (if tags-to-remove
        (progn
          (message "Removing %d ghost tags: %s" (length tags-to-remove) tags-to-remove)
          (dolist (tag-id tags-to-remove)
            (supertag-store-remove-entity :tags tag-id))
          (message "Ghost tag cleanup complete."))
      (message "No ghost tags found."))))

(provide 'supertag-ui-commands)

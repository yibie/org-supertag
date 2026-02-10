;;; supertag-view-schema.el --- A UI for viewing the tag and field schema -*- lexical-binding: t; -*--

;;; Commentary:
;; This file provides a dedicated, interactive buffer for viewing the
;; entire schema of tags, fields, and their relationships.

;;; Code:

(require 'cl-lib)
(require 'supertag-services-query)
(require 'supertag-services-ui)
(require 'supertag-core-schema)
(require 'supertag-view-helper)
(require 'supertag-ops-tag)
(require 'supertag-ops-schema)
(require 'supertag-ops-global-field)
(require 'supertag-view-api)
(require 'supertag-virtual-column)
(require 'supertag-view-framework)

;;; --- Data Gathering and Structuring ---

(defun supertag-schema--ensure-plist (data)
  "Ensure DATA is a plist, converting from a hash-table if necessary."
  (if (hash-table-p data)
      (let (plist)
        (maphash (lambda (k v) (setq plist (plist-put plist k v))) data)
        plist)
    data))

(defun supertag-schema--get-all-tags-by-id ()
  "Query all tags and return a hash-table mapping tag IDs to their data.
This function also defensively ensures that the plist data for each
tag contains its own ID, ensuring consistency for later processing."
  (let ((tags-by-id (make-hash-table :test 'equal))
        (all-tags-alist (supertag-query :tags)))
    (dolist (pair all-tags-alist)
      (let* ((id (car pair))
             (data (cdr pair))
             ;; Defensively ensure the :id key exists in the data plist.
             (plist-data (plist-put (supertag-schema--ensure-plist data) :id id)))
        (when id
          (puthash id plist-data tags-by-id))))
    tags-by-id))

(defun supertag-schema--calculate-hierarchy (tags-by-id)
  "Calculate parent-child relationships from a map of tags.
Returns a list containing two items: the children-by-id map and the list of root IDs."
  (let ((children-by-id (make-hash-table :test 'equal))
        (root-ids '()))
    (maphash
     (lambda (id tag-plist)
       (let ((parent-id (plist-get tag-plist :extends)))
         (if (and parent-id (gethash parent-id tags-by-id))
             (push id (gethash parent-id children-by-id))
           (push id root-ids))))
     tags-by-id)
    (let ((unique-roots (cl-delete-duplicates root-ids :test #'equal)))
      (list children-by-id unique-roots))))

(defun supertag-schema--build-tree-from-maps (tags-by-id children-by-id root-ids)
  "Recursively build a tree structure from pre-calculated hierarchy maps."
  (cl-labels ((build-node (id)
                 (let* ((tag-plist (gethash id tags-by-id))
                        (child-ids (sort (gethash id children-by-id) #'string<))
                        (children (mapcar #'build-node child-ids)))
                   (plist-put (cl-copy-list tag-plist) :children children))))
    (let ((sorted-roots (sort root-ids #'string<)))
      (mapcar #'build-node sorted-roots))))

(defun supertag-schema--build-tree ()
  "Build a hierarchical tree of all tags by composing smaller helper functions."
  (let* ((tags-by-id (supertag-schema--get-all-tags-by-id))
         (hierarchy (supertag-schema--calculate-hierarchy tags-by-id))
         (children-by-id (car hierarchy))
         (root-ids (cadr hierarchy))
         (tree (supertag-schema--build-tree-from-maps tags-by-id children-by-id root-ids)))
    tree))


;;; --- Interactive Helpers ---

(defun supertag-schema--get-context-at-point ()
  "Get context directly from text properties. This is robust."
  (or (get-text-property (point) 'supertag-context)
      ;; Fallback for when cursor is at the very end of the line
      (get-text-property (1- (point)) 'supertag-context)))

(defun supertag-schema--rename-tag-at-point ()
  "Interactively rename the tag at the current line. Internal helper."
  (let* ((context (supertag-schema--get-context-at-point))
         (old-name (plist-get context :tag-id))
         (new-name (read-string (format "Rename tag '%s' to: " old-name) nil nil old-name)))
    (if (and new-name (not (string-empty-p new-name)) (not (string= old-name new-name)))
        (progn
          (supertag-tag-rename old-name new-name)
          (message "Tag '%s' renamed to '%s'. Refreshing view..." old-name new-name)
          (supertag-schema-refresh))
      (message "Tag rename cancelled."))))

(defun supertag-schema--rename-at-point ()
  "Rename the item (tag or field) at the current point."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (pcase (plist-get context :type)
      (:tag (supertag-schema--rename-tag-at-point))
      (:field (supertag-schema--rename-field-at-point))
      (_ (message "Not on a valid tag or field line.")))))

(defun supertag-schema--rename-field-at-point ()
  "Interactively rename the field at the current line."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (if (not (and context (eq (plist-get context :type) :field)))
        (message "Not on a valid field line.")
      (let* ((tag-id (plist-get context :tag-id))
             (field-name (plist-get context :field-name))
             (inherited-from (plist-get context :inherited-from)))
        (if inherited-from
            ;; Inherited field: cannot be renamed directly.
            (message "Cannot rename: Field '%s' is inherited from '%s'." field-name inherited-from)
          ;; Own field: proceed with rename.
          (let ((new-name (read-string (format "Rename field '%s' on tag '%s' to: " field-name tag-id))))
            (if (and new-name (not (string-empty-p new-name)))
                (progn
                  (supertag-tag-rename-field tag-id field-name new-name)
                  (message "Field '%s' renamed to '%s'. Refreshing view..." field-name new-name)
                  (supertag-schema-refresh))
              (message "Field rename cancelled."))))))))

(defun supertag-schema--edit-field-definition-at-point ()
  "Interactively edit the definition of the field at point with pre-filled values.
For global fields, uses `supertag-global-field-edit-interactive' for full editing.
For inherited fields, jumps to the parent tag definition."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (if (not (and context (eq (plist-get context :type) :field)))
        (message "Not on a valid field line.")
      (let* ((tag-id (plist-get context :tag-id))
             (field-name (plist-get context :field-name))
             (inherited-from (plist-get context :inherited-from))
             (field-def (supertag-tag-get-field tag-id field-name))
             (field-id (plist-get field-def :id)))
        
        (if inherited-from
            ;; Inherited Field: Jump to parent definition
            (progn
              (message "Field '%s' is inherited from '%s'. Jumping to definition..." field-name inherited-from)
              (supertag-schema--goto-tag inherited-from))
          
          ;; Own Field: Use new interactive editor with pre-filled values
          (if (and supertag-use-global-fields field-id)
              ;; Global field mode: full editing with pre-filled values
              (progn
                (supertag-global-field-edit-interactive field-id)
                (supertag-schema-refresh))
            ;; Legacy mode: simple type/options editing
            (let ((action (completing-read "Edit Field: " '("Name" "Type/Options") nil t nil nil "Name")))
              (cond
               ((string= action "Name")
                (supertag-schema--rename-field-at-point))
               ((string= action "Type/Options")
                (let* ((current-type (plist-get field-def :type))
                       (type-and-options (supertag-field-read-type-with-options current-type))
                       (new-type (car type-and-options))
                       (options (cdr type-and-options))
                       (new-field-def (plist-put (list :name field-name) :type new-type)))
                  (when (eq new-type :options)
                    (setq new-field-def (plist-put new-field-def :options options)))
                  (supertag-tag-add-field tag-id new-field-def)
                  (message "Field '%s' updated. Refreshing..." field-name)
                  (supertag-schema-refresh)))))))))))

;;; --- Rendering ---

(defun supertag-schema--render ()
  "Render the entire schema tree into the current buffer."
  (let ((tag-tree (supertag-schema--build-tree)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Supertag Schema\n")
      (insert "=================\n\n")
      (insert "Tags:\n")
      (dolist (root-tag tag-tree)
        (supertag-schema--render-tag-node root-tag))
      (supertag-view-helper-insert-simple-footer
       "Add:    [a f] Field | [a c] Child Tag (new/existing) | [a r] Root Tag"
       "Edit:   [e e] Edit Field | [e r] Rename | [e p] Parent | [e b] Bind Field"
       "Delete: [d d] Delete | [d m] Delete Marked"
       "Mark:   [m m] Mark | [m u] Unmark | [m U] Unmark All | [m e] Extend Marked"
       "Move:   [M-↑/↓] Move Field | [?] Full Help | [q] Quit")
      (goto-char (point-min)))))

(defun supertag-schema--get-own-fields (tag-id)
  "Get only the fields directly defined on TAG-ID, not inherited ones.
This function handles both legacy and global field modes."
  (if supertag-use-global-fields
      ;; Global field mode: get fields from tag-field-associations
      (let* ((assoc-table (supertag-view-api-get-collection :tag-field-associations))
             (entries (and (hash-table-p assoc-table) (gethash tag-id assoc-table)))
             (defs (supertag-view-api-get-collection :field-definitions))
             (result '()))
        (when (and entries (hash-table-p defs))
          (dolist (entry entries)
            (let* ((fid (if (plistp entry) (plist-get entry :field-id) entry))
                   (def (and fid (gethash fid defs))))
              (when def (push def result)))))
        (nreverse result))
    ;; Legacy mode: get fields directly from tag's :fields property
    (let* ((tag-data (supertag-tag-get tag-id))
           (plist-data (and tag-data (supertag-schema--ensure-plist tag-data))))
      (plist-get plist-data :fields))))

(defun supertag-schema--render-tag-node (tag-node &optional level)
  "Recursively render a tag node and its children into the buffer."
  (let* ((level (or level 0))
         (indent (make-string (* 2 level) ? ))
         (tag-id (plist-get tag-node :id))
         (parent-id (plist-get tag-node :extends))
         (children (plist-get tag-node :children)))
    ;; Render the tag itself
    (let ((start (point)))
      (insert (format "%s%s" indent tag-id))
      (when parent-id
        (insert (propertize (format " -> %s" parent-id) 'face 'font-lock-comment-face)))
      (insert "\n")
      (add-text-properties start (1- (point))
                           `(supertag-context (:type :tag :tag-id ,tag-id))))

    ;; Render fields, grouped by origin
    ;; Use supertag-schema--get-own-fields to get only directly defined fields
    (let* ((own-fields (supertag-schema--get-own-fields tag-id))
           (processed-fields (make-hash-table :test 'equal))
           (current-parent-id parent-id))

      ;; 1. Render own fields (directly defined on this tag)
      (when own-fields
        (dolist (field-def own-fields)
          (let ((start (point))
                (field-id (plist-get field-def :id))
                (field-name (or (plist-get field-def :name) field-id)))
            (when field-name
              (puthash field-name t processed-fields) ; Mark as processed (by display name)
              (insert (format "%s  %s\n"
                               indent (supertag-schema--format-field field-def)))
              (add-text-properties start (1- (point))
                                   `(supertag-context (:type :field :tag-id ,tag-id :field-name ,field-name :field-id ,field-id)))))))

      ;; 2. Traverse parents and render their fields as inherited
      (while current-parent-id
        (let* ((parent-own-fields (supertag-schema--get-own-fields current-parent-id))
               (fields-to-render '()))
          ;; Collect only new, un-overridden fields from this parent
          (dolist (field parent-own-fields)
            (let ((field-name (or (plist-get field :name)
                                  (plist-get field :id))))
              (when (and field-name (not (gethash field-name processed-fields)))
                (puthash field-name t processed-fields)
                (push field fields-to-render))))

          (when fields-to-render
            (insert (format "%s  %s\n"
                             indent (propertize (format "// Inherited from %s" current-parent-id) 'face 'font-lock-comment-face)))
            (dolist (field-def (nreverse fields-to-render))
              (let ((start (point))
                    (field-id (plist-get field-def :id))
                    (field-name (or (plist-get field-def :name) field-id)))
                (insert (format "%s  %s\n"
                                 indent (supertag-schema--format-field field-def)))
                (add-text-properties start (1- (point))
                                     `(supertag-context (:type :field :tag-id ,tag-id :field-name ,field-name :field-id ,field-id :inherited-from ,current-parent-id)))))))

        ;; Move to next parent
        (setq current-parent-id (plist-get (supertag-schema--ensure-plist (supertag-tag-get current-parent-id)) :extends))))

    ;; Render children recursively
    (dolist (child children)
      (supertag-schema--render-tag-node child (1+ level)))))

(defun supertag-schema--format-field (field-def)
  "Format a single field definition into a display string."
  (let* ((name (or (plist-get field-def :name)
                   (plist-get field-def :id)
                   "unnamed"))
         (id (plist-get field-def :id))
         (type (plist-get field-def :type))
         (options (plist-get field-def :options))
         (type-str (if type (format "(type: %s)" (substring (symbol-name type) 1)) "(type: string)")))
    (let ((label (if (and supertag-use-global-fields id)
                     (format "%s [%s]" name id)
                   name)))
      (if (and (eq type :options) options)
          (format "- %s %s %s" label type-str options)
        (format "- %s %s" label type-str)))))

;;; --- Major Mode and User Command ---

(define-derived-mode supertag-schema-view-mode special-mode "Schema"
  "A major mode for viewing the Org-Supertag schema."
  (setq-local buffer-read-only t)
  (let ((map (make-sparse-keymap)))
    ;; ========== Add Commands (a prefix) ==========
    (let ((add-map (make-sparse-keymap "Add...")))
      (define-key add-map "f" #'supertag-schema--add-field-at-point)      ; a f: Add Field
      (define-key add-map "c" #'supertag-schema--add-child-tag-at-point)  ; a c: Add Child Tag
      (define-key add-map "r" #'supertag-schema--add-new-tag)             ; a r: Add Root Tag
      (define-key map "a" add-map))
    
    ;; ========== Edit Commands (e prefix) ==========
    (let ((edit-map (make-sparse-keymap "Edit...")))
      (define-key edit-map "e" #'supertag-schema--edit-field-definition-at-point)  ; e e: Edit Field
      (define-key edit-map "r" #'supertag-schema--rename-at-point)                  ; e r: Rename
      (define-key edit-map "p" #'supertag-view-schema-set-extends)                  ; e p: Edit Parent (extends)
      (define-key edit-map "b" #'supertag-schema--bind-existing-field-at-point)     ; e b: Bind Field
      (define-key map "e" edit-map))
    
    ;; ========== Delete Commands (d prefix) ==========
    (let ((delete-map (make-sparse-keymap "Delete...")))
      (define-key delete-map "d" #'supertag-schema--delete-at-point)      ; d d: Delete at point
      (define-key delete-map "m" #'supertag-schema--batch-delete-marked-items) ; d m: Delete Marked
      (define-key map "d" delete-map))
    
    ;; ========== Mark Commands (m prefix) ==========
    (let ((mark-map (make-sparse-keymap "Mark...")))
      (define-key mark-map "m" #'supertag-schema--mark-item)              ; m m: Mark
      (define-key mark-map "u" #'supertag-schema--unmark-item)            ; m u: Unmark
      (define-key mark-map "U" #'supertag-schema--unmark-all)             ; m U: Unmark All
      (define-key mark-map "e" #'supertag-schema--batch-extends-marked-tags) ; m e: Extend Marked
      (define-key map "m" mark-map))
    
    ;; ========== Virtual Column Commands (v prefix) ==========
    (let ((vc-map (make-sparse-keymap "Virtual Column...")))
      (define-key vc-map "c" #'supertag-virtual-column-create-interactive)   ; v c: Create
      (define-key vc-map "e" #'supertag-virtual-column-edit-interactive)     ; v e: Edit
      (define-key vc-map "d" #'supertag-virtual-column-delete-interactive)   ; v d: Delete
      (define-key vc-map "l" #'supertag-virtual-column-list-interactive)     ; v l: List
      (define-key vc-map "v" #'supertag-view-select-from-schema)             ; v v: Select View
      (define-key map "v" vc-map))
    
    ;; ========== Move Commands ==========
    (define-key map (kbd "M-<up>") #'supertag-schema--move-field-up)      ; M-up: Move Field Up
    (define-key map (kbd "M-<down>") #'supertag-schema--move-field-down)  ; M-down: Move Field Down
    
    ;; ========== Misc ==========
    (define-key map "g" #'supertag-schema-refresh)                        ; g: Refresh
    (define-key map "q" #'quit-window)                                    ; q: Quit
    (define-key map "?" #'supertag-schema--show-help)                     ; ?: Help
    
    ;; ========== Navigation (vim + emacs style) ==========
    (define-key map "n" #'next-line)                                      ; n: Next line
    (define-key map "p" #'previous-line)                                  ; p: Previous line
    (define-key map "j" #'next-line)                                      ; j: Next line (vim)
    (define-key map "k" #'previous-line)                                  ; k: Previous line (vim)
    
    ;; ========== Legacy shortcuts (for backward compatibility) ==========
    (define-key map "r" #'supertag-schema--rename-at-point)               ; r: Rename (legacy)
    (define-key map "D" #'supertag-schema--batch-delete-marked-items)     ; D: Delete Marked (legacy)
    
    (use-local-map map))
  (setq-local revert-buffer-function #'(lambda (&rest _) (supertag-schema-refresh))))

(defface supertag-schema-marked-face
  '((t :background "blue" :foreground "white"))
  "Face for marked items in the schema view.")
(defvar-local supertag-schema--marked-items nil
  "A list of context plists for marked items in the schema view.")

;;;###autoload
(defun supertag-view-schema ()
  "Create and display a buffer showing the entire tag and field schema."
  (interactive)
  (let ((buffer (get-buffer-create "*Supertag Schema*")))
    (with-current-buffer buffer
      ;; Render the content FIRST, while the buffer is still writable.
      (supertag-schema--render)
      ;; Set the major mode AFTER rendering is complete.
      (supertag-schema-view-mode))
    (pop-to-buffer buffer)))

(defun supertag-schema--add-new-tag ()
  "Interactively create a new top-level tag."
  (interactive)
  (let ((new-name (read-string "New top-level tag name: ")))
    (if (and new-name (not (string-empty-p new-name)))
        (progn
          ;; The create function handles sanitization and ID creation.
          (supertag-tag-create `(:name ,new-name))
          (message "Tag '%s' created. Refreshing view..." new-name)
          (supertag-schema-refresh))
      (message "Tag creation cancelled."))))

(defun supertag-view-schema-set-extends ()
  "Interactively set or clear the inheritance for the tag at point."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (if (not (and context (eq (plist-get context :type) :tag)))
        (message "Not on a valid tag line.")
      (let* ((child-id (plist-get context :tag-id))
             (all-tags (mapcar #'car (supertag-query :tags)))
             (parent-candidates (cl-remove child-id all-tags :test #'equal))
             (parent-id (completing-read (format "Set parent for '%s' (empty to clear): " child-id)
                                         (cons "" parent-candidates)
                                         nil t)))
        (cond
         ;; Case 1: User entered empty string to clear inheritance
         ((string-empty-p parent-id)
          (when (yes-or-no-p (format "Clear parent for '%s'?" child-id))
            (supertag--clear-parent child-id)
            (message "Cleared parent for '%s'. Refreshing..." child-id) (supertag-schema-refresh)))
         ;; Case 2: User selected a parent to add
         (t
          (supertag--set-tag-parent child-id parent-id)
          (message "Set '%s' to extend '%s'. Refreshing..." child-id parent-id)
          (supertag-schema-refresh)))))))

(defun supertag-schema--add-child-tag-at-point ()
  "Interactively add a child tag to the tag at point.
Offers choice between creating a new tag or selecting an existing tag."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (if (not (and context (eq (plist-get context :type) :tag)))
        (message "Not on a valid tag line to add a child to.")
      (let* ((parent-id (plist-get context :tag-id))
             (action (completing-read "Add child: "
                                     '("Create new tag" "Select existing tag")
                                     nil t)))
        (cond
         ;; Option 1: Create new tag
         ((string= action "Create new tag")
          (let ((child-name (read-string (format "New child tag name for '%s': " parent-id))))
            (if (and child-name (not (string-empty-p child-name)))
                (progn
                  (supertag-tag-create `(:name ,child-name :extends ,parent-id))
                  (message "Child tag '%s' created under '%s'. Refreshing..." child-name parent-id)
                  (supertag-schema-refresh))
              (message "Tag creation cancelled."))))
         
         ;; Option 2: Select existing tag
         ((string= action "Select existing tag")
          (let* ((all-tags (let (tags)
                             (maphash (lambda (id _) (push id tags))
                                     (supertag-store-get-collection :tags))
                             tags))
                 ;; Exclude current tag and its existing children
                 (existing-children (let (children)
                                     (maphash (lambda (id tag)
                                               (when (string= (plist-get tag :extends) parent-id)
                                                 (push id children)))
                                             (supertag-store-get-collection :tags))
                                     children))
                 (available-tags (cl-remove-if (lambda (tag)
                                                (or (string= tag parent-id)
                                                    (member tag existing-children)))
                                              all-tags)))
            (if (null available-tags)
                (message "No available tags to add as child (all tags are already children or is the parent).")
              (let ((child-id (completing-read (format "Select tag to add as child of '%s': " parent-id)
                                              available-tags nil t)))
                (if (and child-id (not (string-empty-p child-id)))
                    (progn
                      (supertag--set-tag-parent child-id parent-id)
                      (message "Tag '%s' is now a child of '%s'. Refreshing..." child-id parent-id)
                      (supertag-schema-refresh))
                  (message "No tag selected."))))))
         
         (t (message "Action cancelled.")))))))

(defun supertag-schema--add-field-at-point ()
  "Interactively add a new field to the tag at the current line."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (if (and context (eq (plist-get context :type) :tag))
        (let* ((tag-id (plist-get context :tag-id))
               (field-def (supertag-ui-create-field-definition)))
          (if (not field-def)
              (message "Field creation cancelled.")
            (if (and supertag-use-global-fields
                     (let* ((fid (or (plist-get field-def :id)
                                     (supertag-sanitize-field-id (plist-get field-def :name)))))
                       (and fid (supertag-global-field-get fid))))
                ;; Conflict: existing global field with same slug
                (let* ((fid (or (plist-get field-def :id)
                                (supertag-sanitize-field-id (plist-get field-def :name))))
                       (choice (completing-read
                                (format "Field '%s' exists. Action: " fid)
                                '("Reuse existing (bind only)"
                                  "Overwrite existing definition"
                                  "Cancel")
                                nil t nil nil "Reuse existing (bind only)")))
                  (pcase choice
                    ("Reuse existing (bind only)"
                     (supertag-tag-associate-field tag-id fid)
                     (message "Bound existing field '%s' to tag '%s'." fid tag-id)
                     (supertag-schema-refresh))
                    ("Overwrite existing definition"
                     (supertag-global-field-update fid (lambda (_old) field-def))
                     (supertag-tag-associate-field tag-id fid)
                     (message "Overwrote field '%s' and bound to tag '%s'." fid tag-id)
                     (supertag-schema-refresh))
                    (_ (message "Field creation cancelled."))))
              ;; No conflict
              (progn
                (supertag-tag-add-field tag-id field-def)
                (message "Field '%s' added to tag '%s'. Refreshing view..."
                         (plist-get field-def :name) tag-id)
                (supertag-schema-refresh)))))
      (message "Not on a valid tag line."))))

(defun supertag-schema--bind-existing-field-at-point ()
  "Bind an existing global field to the tag at point (append order)."
  (interactive)
  (unless supertag-use-global-fields
    (user-error "Global fields are disabled; set `supertag-use-global-fields` to t"))
  (let ((context (supertag-schema--get-context-at-point)))
    (if (and context (eq (plist-get context :type) :tag))
        (let* ((tag-id (plist-get context :tag-id))
               (defs (supertag-view-api-get-collection :field-definitions))
               (current (mapcar (lambda (f)
                                  (supertag-sanitize-field-id
                                   (or (plist-get f :id) (plist-get f :name))))
                                (or (supertag-tag-get-all-fields tag-id) '())))
               (current-set (let ((ht (make-hash-table :test 'equal)))
                              (dolist (fid current) (when fid (puthash fid t ht))) ht))
               (candidates '()))
          (when (hash-table-p defs)
            (maphash
             (lambda (fid def)
               (unless (gethash fid current-set)
                 (let* ((name (or (plist-get def :name) fid))
                        (type (plist-get def :type))
                        (label (format "%s (%s%s%s)"
                                       fid
                                       (or type "unknown")
                                       (if name " · " "")
                                       (or name ""))))
                   (push (cons label fid) candidates))))
             defs))
          (if (null candidates)
              (message "No unbound global fields available.")
            (let* ((choice (completing-read "Bind existing field: "
                                            (mapcar #'car candidates)
                                            nil t))
                   (fid (cdr (assoc choice candidates))))
              (when fid
                (supertag-tag-associate-field tag-id fid)
                (supertag-schema-refresh)
                (message "Bound field %s to tag %s" fid tag-id))))))
      (message "Not on a valid tag line.")))

(defun supertag-schema--delete-at-point ()
  "Interactively delete the tag or field at the current line.
Dispatches to the correct deletion logic based on context."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (pcase (plist-get context :type)
      (:field
       (let* ((tag-id (plist-get context :tag-id))
             (field-name (plist-get context :field-name))
             (field-id (plist-get context :field-id))
             (inherited-from (plist-get context :inherited-from)))
         (if inherited-from
             (message "Cannot delete: Field '%s' is inherited from '%s'. Delete it from the parent tag." field-name inherited-from)
           (when (yes-or-no-p (format "Really delete field '%s' from tag '%s'?" field-name tag-id))
             (if (and supertag-use-global-fields field-id (stringp field-id) (not (string-empty-p field-id)))
                 (supertag-tag-disassociate-field tag-id field-id)
               (supertag-tag-remove-field tag-id field-name))
             (message "Field '%s' deleted. Refreshing view..." field-name)
             (supertag-schema-refresh)))))
      (:tag
       (let ((tag-id (plist-get context :tag-id)))
         (when (yes-or-no-p (format "DELETE tag '%s' and ALL its uses? This is irreversible." tag-id))
           (supertag-ops-delete-tag-everywhere tag-id)
           (supertag-schema-refresh))))
      (_
       (message "Not on a valid tag or field line.")))))

(defun supertag-schema--move-field-up ()
  "Move the field at the current line up in the tag's field list."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (if (and context (eq (plist-get context :type) :field))
        (let ((tag-id (plist-get context :tag-id))
              (field-name (plist-get context :field-name)))
          (when (supertag-tag-move-field-up tag-id field-name)
            (supertag-schema-refresh)
            (when (supertag-schema--goto-context context)
              (message "Field '%s' moved up." field-name))))
      (message "Not on a valid field line."))))

(defun supertag-schema--move-field-down ()
  "Move the field at the current line down in the tag's field list."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (if (and context (eq (plist-get context :type) :field))
        (let ((tag-id (plist-get context :tag-id))
              (field-name (plist-get context :field-name)))
          (when (supertag-tag-move-field-down tag-id field-name)
            (supertag-schema-refresh)
            (when (supertag-schema--goto-context context)
              (message "Field '%s' moved down." field-name))))
      (message "Not on a valid field line."))))

(defun supertag-schema-refresh ()
  "Refresh the schema view while attempting to preserve point."
  (interactive)
  (let ((context-before (supertag-schema--get-context-at-point)))
    (let ((inhibit-read-only t))
      (supertag-schema--render))
    (when context-before
      (supertag-schema--goto-context context-before))))

(defun supertag-schema--goto-context (context)
  "Search for CONTEXT from top of buffer and move point there."
  (let ((foundp nil))
    (goto-char (point-min))
    (let ((tag-id (plist-get context :tag-id))
          (field-name (plist-get context :field-name)))
      (when (and tag-id (re-search-forward (concat "^\\s-*" (regexp-quote tag-id)) nil t))
        ;; We found the tag line.
        (if (eq (plist-get context :type) :tag)
            ;; If we are looking for the tag, we're done.
            (progn
              (goto-char (line-beginning-position))
              (setq foundp t))
          ;; Otherwise, we are looking for a field under this tag.
          (when (eq (plist-get context :type) :field)
            (let ((eob (save-excursion (end-of-buffer) (point)))
                  (search-active t))
              (while (and search-active (re-search-forward (concat "^\\s-*- " (regexp-quote field-name)) eob t))
                (let ((found-context (supertag-schema--get-context-at-point)))
                  (when (equal (plist-get found-context :tag-id) tag-id)
                    (goto-char (line-beginning-position))
                    (setq foundp t)
                    (setq search-active nil)))))))))
    foundp))

(defun supertag-schema--goto-tag (tag-id)
  "Jump to the definition of TAG-ID in the schema view."
  (interactive "sTag ID to jump to: ") ; Make it interactive for testing, but will be called non-interactively
  (let ((original-context (supertag-schema--get-context-at-point)))
    (goto-char (point-min))
    (when (re-search-forward (concat "^\\s-*" (regexp-quote tag-id)) nil t)
      (goto-char (line-beginning-position))
      (message "Jumped to tag '%s'." tag-id))
    (unless (equal (supertag-schema--get-context-at-point) original-context)
      (supertag-schema-refresh))))

(defun supertag-schema--mark-item ()
  "Mark the item at point and move to the next line."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (when context
      (let ((inhibit-read-only t))
        (unless (member context supertag-schema--marked-items)
          (push context supertag-schema--marked-items)
          (add-text-properties (line-beginning-position) (line-end-position) '(face supertag-schema-marked-face))))
      (next-line 1))))

(defun supertag-schema--unmark-item ()
  "Unmark the item at point and move to the next line."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (when context
      (let ((inhibit-read-only t))
        (setq supertag-schema--marked-items (cl-remove context supertag-schema--marked-items :test #'equal))
        (remove-text-properties (line-beginning-position) (line-end-position) '(face supertag-schema-marked-face)))
      (next-line 1))))

(defun supertag-schema--unmark-all ()
  "Unmark all marked items in the buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (setq supertag-schema--marked-items nil)
    (remove-text-properties (point-min) (point-max) '(face supertag-schema-marked-face)))
  (message "All marks removed."))

(defun supertag-schema--batch-delete-marked-items ()
  "Delete all marked items."
  (interactive)
  (if (not supertag-schema--marked-items)
      (message "No items marked.")
    (when (yes-or-no-p (format "Really delete %d marked items?" (length supertag-schema--marked-items)))
      (dolist (context supertag-schema--marked-items)
        (pcase (plist-get context :type)
          (:field
           (supertag-tag-remove-field (plist-get context :tag-id) (plist-get context :field-name)))
          (:tag
           (supertag-ops-delete-tag-everywhere (plist-get context :tag-id)))))
      (setq supertag-schema--marked-items nil)
      (supertag-schema-refresh)
      (message "Batch delete complete."))))

(defun supertag-schema--batch-extends-marked-tags ()
  "Set a common parent for all marked tags."
  (interactive)
  (let ((marked-tags (cl-remove-if-not (lambda (ctx) (eq (plist-get ctx :type) :tag))
                                       supertag-schema--marked-items)))
    (if (not marked-tags)
        (message "No tags marked.")
      (let* ((all-tags (mapcar #'car (supertag-query :tags)))
             (marked-tag-ids (mapcar #'(lambda (ctx) (plist-get ctx :tag-id)) marked-tags))
             (parent-candidates (cl-set-difference all-tags marked-tag-ids :test #'equal))
             (parent-id (completing-read (format "Set parent for %d marked tags: " (length marked-tags))
                                         parent-candidates nil t)))
        (when (and parent-id (not (string-empty-p parent-id)))
          (when (yes-or-no-p (format "Set %d tags to extend '%s'?" (length marked-tags) parent-id))
            (dolist (tag-context marked-tags)
              (supertag--set-tag-parent (plist-get tag-context :tag-id) parent-id))
            (setq supertag-schema--marked-items nil)
            (supertag-schema-refresh)
            (message "Batch extends complete.")))))))

(defun supertag-schema--cleanup-inherited-field-associations (tag-id)
  "Remove field associations from TAG-ID that are inherited from parent tags.
This cleans up redundant associations where a field is defined on both
a parent tag and a child tag."
  (interactive "sTag ID: ")
  (unless supertag-use-global-fields
    (user-error "This function only works in global field mode"))
  (let* ((tag-data (supertag-tag-get tag-id))
         (plist-data (and tag-data (supertag-schema--ensure-plist tag-data)))
         (parent-id (plist-get plist-data :extends)))
    (unless parent-id
      (message "Tag '%s' has no parent, nothing to clean up." tag-id)
      (cl-return-from supertag-schema--cleanup-inherited-field-associations nil))
    ;; Collect all field IDs from parent chain
    (let ((parent-field-ids (make-hash-table :test 'equal))
          (current-parent parent-id))
      (while current-parent
        (let ((parent-assocs (supertag-store-get-tag-field-associations current-parent)))
          (dolist (assoc parent-assocs)
            (let ((fid (if (plistp assoc) (plist-get assoc :field-id) assoc)))
              (when fid (puthash fid t parent-field-ids)))))
        (let* ((parent-data (supertag-tag-get current-parent))
               (parent-plist (and parent-data (supertag-schema--ensure-plist parent-data))))
          (setq current-parent (plist-get parent-plist :extends))))
      ;; Filter out inherited fields from current tag's associations
      (let* ((current-assocs (supertag-store-get-tag-field-associations tag-id))
             (filtered-assocs '())
             (removed-count 0))
        (dolist (assoc current-assocs)
          (let ((fid (if (plistp assoc) (plist-get assoc :field-id) assoc)))
            (if (gethash fid parent-field-ids)
                (progn
                  (cl-incf removed-count)
                  (message "Removing inherited field '%s' from tag '%s'" fid tag-id))
              (push assoc filtered-assocs))))
        (when (> removed-count 0)
          (supertag-store-put-tag-field-associations tag-id (nreverse filtered-assocs) t)
          (message "Removed %d inherited field associations from tag '%s'" removed-count tag-id))
        removed-count))))

(defun supertag-schema--cleanup-all-inherited-associations ()
  "Clean up inherited field associations from all child tags."
  (interactive)
  (unless supertag-use-global-fields
    (user-error "This function only works in global field mode"))
  (let ((all-tags (supertag-query :tags))
        (total-removed 0))
    (dolist (tag-pair all-tags)
      (let* ((tag-id (car tag-pair))
             (tag-data (cdr tag-pair))
             (plist-data (supertag-schema--ensure-plist tag-data))
             (parent-id (plist-get plist-data :extends)))
        (when parent-id
          (let ((removed (supertag-schema--cleanup-inherited-field-associations tag-id)))
            (when removed
              (setq total-removed (+ total-removed removed)))))))
    (message "Total: removed %d inherited field associations." total-removed)
    (when (> total-removed 0)
      (supertag-schema-refresh))
    total-removed))

(defun supertag-schema--debug-tag-data (tag-id)
  "Debug function to inspect the raw data for TAG-ID."
  (interactive "sTag ID: ")
  (let* ((tag-data (supertag-tag-get tag-id))
         (plist-data (and tag-data (supertag-schema--ensure-plist tag-data)))
         (own-fields-legacy (plist-get plist-data :fields))
         (assoc-table (supertag-view-api-get-collection :tag-field-associations))
         (own-fields-global (and (hash-table-p assoc-table) (gethash tag-id assoc-table)))
         (resolved (ignore-errors (supertag-ops-schema-get-resolved-tag tag-id))))
    (with-current-buffer (get-buffer-create "*Supertag Debug*")
      (erase-buffer)
      (insert (format "=== Debug Info for Tag: %s ===\n\n" tag-id))
      (insert (format "supertag-use-global-fields: %s\n\n" supertag-use-global-fields))
      (insert "--- Raw Tag Data ---\n")
      (insert (format "%S\n\n" plist-data))
      (insert "--- Legacy :fields property ---\n")
      (insert (format "%S\n\n" own-fields-legacy))
      (insert "--- Global field associations (from :tag-field-associations) ---\n")
      (insert (format "%S\n\n" own-fields-global))
      (insert "--- Resolved schema (from supertag-ops-schema-get-resolved-tag) ---\n")
      (insert (format "%S\n\n" resolved))
      (insert "--- supertag-schema--get-own-fields result ---\n")
      (insert (format "%S\n" (supertag-schema--get-own-fields tag-id)))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;; --- Help System ---

(defun supertag-schema--show-help ()
  "Display full keyboard help for Schema View."
  (interactive)
  (with-help-window "*Supertag Schema Help*"
    (princ "Supertag Schema View - Full Keyboard Reference
================================================\n\n")
    
    (princ "Navigation:\n")
    (princ "  n, j    Next line\n")
    (princ "  p, k    Previous line\n")
    (princ "  M-<up>  Move field up (reorder)\n")
    (princ "  M-<down> Move field down (reorder)\n\n")
    
    (princ "Add Commands (prefix: a):\n")
    (princ "  a f     Add Field to current tag\n")
    (princ "  a t     Add Child Tag (create new OR select existing)\n")
    (princ "  a r     Add Root Tag (no parent)\n\n")
    
    (princ "Edit Commands (prefix: e):\n")
    (princ "  e e     Edit Field definition (with pre-filled values)\n")
    (princ "  e r     Rename tag or field\n")
    (princ "  e p     Edit Parent (set extends)\n")
    (princ "  e b     Bind existing global field\n")
    (princ "  r       Rename (legacy shortcut)\n\n")
    
    (princ "Delete Commands (prefix: d):\n")
    (princ "  d d     Delete item at point\n")
    (princ "  d m     Delete all marked items\n")
    (princ "  D       Delete marked (legacy shortcut)\n\n")
    
    (princ "Mark Commands (prefix: m):\n")
    (princ "  m m     Mark item at point\n")
    (princ "  m u     Unmark item at point\n")
    (princ "  m U     Unmark all items\n")
    (princ "  m e     Extend all marked tags\n")
    (princ "  m       Mark (legacy shortcut)\n")
    (princ "  u       Unmark (legacy shortcut)\n")
    (princ "  U       Unmark all (legacy shortcut)\n")
    (princ "  E       Extend marked (legacy shortcut)\n\n")
    
    (princ "View Commands (prefix: v):\n")
    (princ "  v c     Create virtual column\n")
    (princ "  v e     Edit virtual column\n")
    (princ "  v d     Delete virtual column\n")
    (princ "  v l     List virtual columns\n")
    (princ "  v v     Select view\n\n")
    
    (princ "Global Commands:\n")
    (princ "  g       Refresh view\n")
    (princ "  q       Quit window\n")
    (princ "  ?       Show this help\n\n")
    
    (princ "Notes:\n")
    (princ "  - Field editing now uses pre-filled values from existing definition\n")
    (princ "  - Inherited fields cannot be edited directly; jump to parent instead\n")
    (princ "  - Batch operations work on marked items across the entire schema\n")))

(provide 'supertag-view-schema)

;;; supertag-view-schema.el ends here

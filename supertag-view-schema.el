;;; supertag-view-schema.el --- A UI for viewing the tag and field schema -*- lexical-binding: t; -*--

;;; Commentary:
;; This file provides a dedicated, interactive buffer for viewing the
;; entire schema of tags, fields, and their relationships.

;;; Code:

(require 'cl-lib)
(require 'supertag-services-query)
(require 'supertag-services-ui)
(require 'supertag-core-schema)
(require 'supertag-core-tag-path)
(require 'supertag-view-helper)
(require 'supertag-ops-tag)
(require 'supertag-ops-tag-merge)
(require 'supertag-ops-schema)
(require 'supertag-ops-global-field)
(require 'supertag-view-api)
(require 'supertag-virtual-column)
(require 'supertag-view-framework)

(declare-function supertag-view-table "supertag-view-table"
                  (data-source &optional columns view-config named-views))

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

(defun supertag-schema--build-tree ()
  "Build a namespace tree from complete slash-delimited tag IDs.
Missing namespace ancestors are virtual nodes.  Explicit `:extends'
remains tag metadata and never controls indentation."
  (let ((tags-by-id (supertag-schema--get-all-tags-by-id))
        (nodes-by-id (make-hash-table :test 'equal))
        (children-by-id (make-hash-table :test 'equal))
        roots)
    (cl-labels
        ((ensure-node
          (path)
          (unless (gethash path nodes-by-id)
            (puthash path
                     (list :id path
                           :label (supertag-tag-path-leaf path)
                           :virtual t)
                     nodes-by-id)
            (when-let* ((parent (supertag-tag-path-parent path)))
              (ensure-node parent)))))
      (maphash
       (lambda (id tag)
         (ensure-node id)
         (let ((actual (copy-sequence tag)))
           (setq actual (plist-put actual :id id))
           (setq actual (plist-put actual :label
                                   (supertag-tag-path-leaf id)))
           (setq actual (plist-put actual :virtual nil))
           (puthash id actual nodes-by-id)))
       tags-by-id))
    (maphash
     (lambda (id _node)
       (let ((parent (supertag-tag-path-parent id)))
         (if (and parent (gethash parent nodes-by-id))
             (push id (gethash parent children-by-id))
           (push id roots))))
     nodes-by-id)
    (cl-labels
        ((build-node
          (id)
          (let ((node (copy-sequence (gethash id nodes-by-id))))
            (plist-put
             node :children
             (mapcar #'build-node
                     (sort (copy-sequence (gethash id children-by-id))
                           #'string<))))))
      (mapcar #'build-node (sort roots #'string<)))))


;;; --- Interactive Helpers ---

(defun supertag-schema--get-context-at-point ()
  "Get context directly from text properties. This is robust."
  (or (get-text-property (point) 'supertag-context)
      ;; Fallback for when cursor is at the very end of the line
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'supertag-context))))

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
       "Add:    [a f] Field | [a n] Nested Tag | [a c] Inheritance Child | [a r] Root Tag"
       "Edit:   [e e] Edit Field | [e r] Rename | [e p] Parent | [e b] Bind Field"
       "Delete: [d d] Delete | [d m] Delete Marked"
       "Mark:   [m m] Mark | [m u] Unmark | [m U] Unmark All | [m e] Extend Marked"
       "View:   [v v] Custom View | [v t] Table | [?] Full Help | [q] Quit")
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
         (label (or (plist-get tag-node :label) tag-id))
         (virtual (plist-get tag-node :virtual))
         (parent-id (plist-get tag-node :extends))
         (children (plist-get tag-node :children))
         (branch (and children t)))
    ;; Render the tag itself
    (let* ((start (point))
           (context (if virtual
                        (list :type :namespace :path tag-id
                              :has-descendants t)
                      (list :type :tag :tag-id tag-id
                            :has-descendants branch))))
      (insert (format "%s%s%s" indent label (if branch "/" "")))
      (when parent-id
        (insert (propertize (format " -> %s" parent-id) 'face 'font-lock-comment-face)))
      (insert "\n")
      (add-text-properties start (1- (point))
                           `(supertag-context ,context)))

    ;; Render fields, grouped by origin
    ;; Use supertag-schema--get-own-fields to get only directly defined fields
    (unless virtual
      (let* ((own-fields (supertag-schema--get-own-fields tag-id))
           (processed-fields (make-hash-table :test 'equal))
           (visited-parents (make-hash-table :test 'equal))
           (current-parent-id parent-id))

      ;; 1. Render own fields (directly defined on this tag)
      (when own-fields
        (dolist (field-def own-fields)
          (let* ((start (point))
                 (field-id (plist-get field-def :id))
                 (field-name (or (plist-get field-def :name) field-id)))
            (when field-name
              (puthash field-name t processed-fields) ; Mark as processed (by display name)
              (insert (format "%s  %s\n"
                               indent (supertag-schema--format-field field-def)))
              (add-text-properties start (1- (point))
                                   `(supertag-context (:type :field :tag-id ,tag-id :field-name ,field-name :field-id ,field-id)))))))

      ;; 2. Traverse parents and render their fields as inherited
      (while (and current-parent-id
                  (not (gethash current-parent-id visited-parents)))
        (puthash current-parent-id t visited-parents)
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
              (let* ((start (point))
                     (field-id (plist-get field-def :id))
                     (field-name (or (plist-get field-def :name) field-id)))
                (insert (format "%s  %s\n"
                                 indent (supertag-schema--format-field field-def)))
                (add-text-properties start (1- (point))
                                     `(supertag-context (:type :field :tag-id ,tag-id :field-name ,field-name :field-id ,field-id :inherited-from ,current-parent-id)))))))

        ;; Move to next parent
        (setq current-parent-id
              (plist-get
               (supertag-schema--ensure-plist
                (supertag-tag-get current-parent-id))
               :extends)))))

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

(defvar supertag-schema-view-mode-map
  (let ((map (make-sparse-keymap)))
    ;; ========== Add Commands (a prefix) ==========
    (let ((add-map (make-sparse-keymap "Add...")))
      (define-key add-map "f" #'supertag-schema--add-field-at-point)      ; a f: Add Field
      (define-key add-map "n" #'supertag-schema--add-nested-tag-at-point) ; a n: Add Nested Tag
      (define-key add-map "c" #'supertag-schema--add-child-tag-at-point)  ; a c: Add Inheritance Child
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
      (define-key mark-map "M" #'supertag-schema-merge-marked-tags)       ; m M: Merge Marked
      (define-key map "m" mark-map))

    ;; ========== Virtual Column Commands (v prefix) ==========
    (let ((vc-map (make-sparse-keymap "Virtual Column...")))
      (define-key vc-map "c" #'supertag-virtual-column-create-interactive)   ; v c: Create
      (define-key vc-map "e" #'supertag-virtual-column-edit-interactive)     ; v e: Edit
      (define-key vc-map "d" #'supertag-virtual-column-delete-interactive)   ; v d: Delete
      (define-key vc-map "l" #'supertag-virtual-column-list-interactive)     ; v l: List
      (define-key vc-map "v" #'supertag-view-select-from-schema)             ; v v: Select View
      (define-key vc-map "t" #'supertag-schema-view-table-at-point)          ; v t: Table
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

    map)
  "Keymap for `supertag-schema-view-mode'.
Users can rebind keys in this map to avoid conflicts with modal editing.")

(define-derived-mode supertag-schema-view-mode special-mode "Schema"
  "A major mode for viewing the Org-Supertag schema.

\\{supertag-schema-view-mode-map}"
  :keymap supertag-schema-view-mode-map
  (setq-local buffer-read-only t)
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

(defun supertag-schema--create-nested-tag (parent-path child-path)
  "Create CHILD-PATH below PARENT-PATH without adding inheritance."
  (let* ((child (supertag-sanitize-tag-name child-path))
         (full-path (concat parent-path "/" child)))
    (unless (and (supertag-tag-path-valid-p parent-path)
                 (supertag-tag-path-valid-p child)
                 (supertag-tag-path-valid-p full-path))
      (user-error "Nested tag paths cannot contain empty segments"))
    (when (supertag-tag-get full-path)
      (user-error "Tag '%s' already exists" full-path))
    (supertag-tag-create (list :id full-path :name full-path))))

(defun supertag-schema--add-nested-tag-at-point ()
  "Create a slash-delimited child below the namespace at point."
  (interactive)
  (let* ((context (supertag-schema--get-context-at-point))
         (parent (pcase (plist-get context :type)
                   (:namespace (plist-get context :path))
                   (:tag (plist-get context :tag-id)))))
    (if (not parent)
        (message "Not on a tag or namespace line.")
      (let ((child (read-string (format "Nested tag below '%s': " parent))))
        (unless (string-empty-p child)
          (let ((created (supertag-schema--create-nested-tag parent child)))
            (supertag-schema-refresh)
            (supertag-schema--goto-context
             (list :type :tag :tag-id (plist-get created :id)))
            (message "Nested tag '%s' created." (plist-get created :id))))))))

(defun supertag-schema-view-table-at-point ()
  "Open an exact or descendant table for the Schema item at point."
  (interactive)
  (let ((query (supertag-view--get-tag-at-point)))
    (unless query
      (user-error "Not on a tag or namespace line"))
    (require 'supertag-view-table)
    (supertag-view-table query)))

(defun supertag-view-schema-set-extends ()
  "Interactively set or clear the inheritance for the tag at point."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (if (not (and context (eq (plist-get context :type) :tag)))
        (message "Not on a valid tag line.")
      (let* ((child-id (plist-get context :tag-id))
             (all-tags (mapcar #'car (supertag-query :tags)))
             (parent-candidates (cl-remove child-id all-tags :test #'equal))
             (parent-id
              (supertag-ui-read-tag
               (format "Set parent for '%s' (empty to clear): " child-id)
               parent-candidates nil t)))
        (cond
         ;; Case 1: User entered empty string to clear inheritance
         ((null parent-id)
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
              (let ((child-id
                     (supertag-ui-read-tag
                      (format "Select tag to add as child of '%s': " parent-id)
                      available-tags nil nil)))
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
  "Refresh the schema view while preserving point as best we can.
Tries three strategies in order:
  1. restore the exact context (tag + field) that was at point;
  2. fall back to just the tag line if the field is gone;
  3. fall back to the same line number if both are gone."
  (interactive)
  (let* ((context-before (supertag-schema--get-context-at-point))
         (line-before (line-number-at-pos)))
    (let ((inhibit-read-only t))
      (supertag-schema--render))
    (or (and context-before
             (supertag-schema--goto-context context-before))
        ;; field gone but its tag may still exist — jump to the tag line
        (and context-before
             (eq (plist-get context-before :type) :field)
             (supertag-schema--goto-context
              (list :type :tag :tag-id (plist-get context-before :tag-id))))
        ;; nothing left of the context — keep the same line number
        (progn
          (goto-char (point-min))
          (forward-line (1- line-before))
          (goto-char (line-beginning-position))))))

(defun supertag-schema--goto-context (context)
  "Search for CONTEXT from top of buffer and move point there."
  (let ((foundp nil)
        (wanted-type (plist-get context :type))
        (wanted-tag (plist-get context :tag-id))
        (wanted-path (plist-get context :path))
        (wanted-field (plist-get context :field-name))
        (wanted-origin (plist-get context :inherited-from)))
    (goto-char (point-min))
    (while (and (not foundp) (not (eobp)))
      (let ((candidate
             (get-text-property (line-beginning-position) 'supertag-context)))
        (when (and candidate
                   (eq wanted-type (plist-get candidate :type))
                   (equal wanted-tag (plist-get candidate :tag-id))
                   (equal wanted-path (plist-get candidate :path))
                   (equal wanted-field (plist-get candidate :field-name))
                   (equal wanted-origin
                          (plist-get candidate :inherited-from)))
          (setq foundp t))
        (unless foundp (forward-line 1))))
    foundp))

(defun supertag-schema--goto-tag (tag-id)
  "Jump to the definition of TAG-ID in the schema view."
  (interactive "sTag ID to jump to: ") ; Make it interactive for testing, but will be called non-interactively
  (when (supertag-schema--goto-context (list :type :tag :tag-id tag-id))
    (message "Jumped to tag '%s'." tag-id)))

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
             (parent-id
              (supertag-ui-read-tag
               (format "Set parent for %d marked tags: " (length marked-tags))
               parent-candidates nil nil)))
        (when (and parent-id (not (string-empty-p parent-id)))
          (when (yes-or-no-p (format "Set %d tags to extend '%s'?" (length marked-tags) parent-id))
            (dolist (tag-context marked-tags)
              (supertag--set-tag-parent (plist-get tag-context :tag-id) parent-id))
            (setq supertag-schema--marked-items nil)
            (supertag-schema-refresh)
            (message "Batch extends complete.")))))))

(defun supertag-schema--merge-read-target ()
  "Read a merge destination."
  (supertag-ui-read-tag
   "Merge destination (RET on a typed path creates it): "
   (mapcar #'car (supertag-query :tags)) t nil))

(defun supertag-schema--merge-read-fields (source-ids)
  "Read source field keys to retain from SOURCE-IDS.
Submitting an empty selection retains every available source field."
  (let* ((groups (supertag-tag-merge--field-groups source-ids))
         (choices
          (mapcar
           (lambda (group)
             (let* ((key (car group))
                    (entries (cdr group))
                    (sources (mapcar (lambda (entry) (plist-get entry :tag-id)) entries))
                    (types (supertag-tag-merge--unique
                            (mapcar (lambda (entry)
                                      (plist-get (plist-get entry :definition) :type))
                                    entries)))
                    (label (format "%s  [%s; %s]"
                                   key
                                   (mapconcat #'identity sources ", ")
                                   (mapconcat (lambda (type) (format "%s" type)) types ", "))))
               (cons label key)))
           groups))
         (selected-labels
          (if choices
              (completing-read-multiple
               "Source fields to retain (empty = all): "
               (mapcar #'car choices) nil t)
            nil)))
    (if selected-labels
        (mapcar (lambda (label) (cdr (assoc label choices))) selected-labels)
      :all)))

(defun supertag-schema--merge-resolve-definitions (conflicts)
  "Read source choices for field-definition CONFLICTS."
  (let (choices)
    (dolist (conflict conflicts (nreverse choices))
      (when (eq (plist-get conflict :kind) :field-definition)
        (let* ((field (plist-get conflict :field))
               (candidates (plist-get conflict :candidates))
               (labels
                (mapcar
                 (lambda (entry)
                   (let ((definition (plist-get entry :definition)))
                     (cons (format "%s — %s %S"
                                   (plist-get entry :tag-id)
                                   (plist-get definition :type)
                                   (plist-get definition :options))
                           (plist-get entry :tag-id))))
                 candidates))
               (selected (completing-read
                          (format "Definition to keep for '%s': " field)
                          (mapcar #'car labels) nil t)))
          (push (cons field (cdr (assoc selected labels))) choices))))))

(defun supertag-schema--merge-value-label (candidate)
  "Return a completion label for value CANDIDATE."
  (format "%s — %S" (plist-get candidate :tag-id) (plist-get candidate :value)))

(defun supertag-schema--merge-multi-values (candidates)
  "Read one or more atomic values from CANDIDATES."
  (let ((values
         (supertag-tag-merge--unique
          (apply #'append
                 (mapcar (lambda (candidate)
                           (let ((value (plist-get candidate :value)))
                             (if (listp value) (copy-sequence value) (list value))))
                         candidates)))))
    (let* ((labels (mapcar (lambda (value) (cons (format "%S" value) value)) values))
           (selected (completing-read-multiple
                      "Values to keep (one or more): " (mapcar #'car labels) nil t)))
      (unless selected
        (user-error "Select at least one value"))
      (list :merge-values
            (mapcar (lambda (label) (cdr (assoc label labels))) selected)))))

(defun supertag-schema--merge-resolve-values (conflicts)
  "Read per-node resolutions for field-value CONFLICTS."
  (let (resolutions)
    (dolist (conflict conflicts (nreverse resolutions))
      (when (eq (plist-get conflict :kind) :field-value)
        (let* ((node-id (plist-get conflict :node-id))
               (field (plist-get conflict :field))
               (definition (plist-get conflict :definition))
               (candidates (plist-get conflict :candidates))
               (multi-p (memq (plist-get definition :type)
                              supertag-tag-merge--multi-value-types))
               (value
                (if multi-p
                    (supertag-schema--merge-multi-values candidates)
                  (let* ((labels (mapcar (lambda (candidate)
                                           (cons (supertag-schema--merge-value-label candidate)
                                                 (plist-get candidate :value)))
                                         candidates))
                         (selected (completing-read
                                    (format "Value for %s/%s: " node-id field)
                                    (mapcar #'car labels) nil t)))
                    (cdr (assoc selected labels))))))
          (push (cons (list node-id field) value) resolutions))))))

(defun supertag-schema--merge-show-preview (plan)
  "Display a human-readable merge PLAN."
  (with-help-window "*Supertag Tag Merge Preview*"
    (princ "Supertag Tag Merge Preview\n===========================\n\n")
    (princ (format "Source tags: %s\n"
                   (mapconcat #'identity (plist-get plan :source-ids) ", ")))
    (princ (format "Destination: %s%s\n"
                   (plist-get plan :target-id)
                   (if (plist-get plan :target-exists-p) " (existing)" " (new)")))
    (princ (format "Fields imported: %s\n"
                   (if (plist-get plan :selected-fields)
                       (mapconcat #'identity (plist-get plan :selected-fields) ", ")
                     "none")))
    (princ (format "Nodes affected: %d\n" (length (plist-get plan :nodes))))
    (princ (format "Files affected: %d\n" (length (plist-get plan :files))))
    (princ (format "Field values written: %d\n"
                   (length (plist-get plan :value-writes))))
    (when-let* ((warnings (plist-get plan :warnings)))
      (princ (format "\nWarnings (%d):\n" (length warnings)))
      (dolist (warning warnings) (princ (format "  - %S\n" warning))))
    (when-let* ((conflicts (plist-get plan :conflicts)))
      (princ (format "\nBlocking conflicts (%d):\n" (length conflicts)))
      (dolist (conflict conflicts) (princ (format "  - %S\n" conflict))))))

(defun supertag-schema-merge-marked-tags ()
  "Merge marked Schema View tags into one new or existing tag."
  (interactive)
  (let* ((participants
          (supertag-tag-merge--unique
           (mapcar (lambda (context) (plist-get context :tag-id))
                   (cl-remove-if-not
                    (lambda (context) (eq (plist-get context :type) :tag))
                    supertag-schema--marked-items)))))
    (unless (>= (length participants) 2)
      (user-error "Mark at least two tags before merging"))
    (let* ((target (supertag-schema--merge-read-target))
           (sources (if (member target participants)
                        (remove target participants)
                      participants))
           (selected-fields (supertag-schema--merge-read-fields sources))
           (plan (supertag-tag-merge-plan participants target
                                           :selected-fields selected-fields))
           (field-sources
            (supertag-schema--merge-resolve-definitions (plist-get plan :conflicts))))
      (when field-sources
        (setq plan (supertag-tag-merge-plan participants target
                                             :selected-fields selected-fields
                                             :field-sources field-sources)))
      (let ((resolutions
             (supertag-schema--merge-resolve-values (plist-get plan :conflicts))))
        (when resolutions
          (setq plan (supertag-tag-merge-plan participants target
                                               :selected-fields selected-fields
                                               :field-sources field-sources
                                               :resolutions resolutions))))
      (supertag-schema--merge-show-preview plan)
      (when (plist-get plan :conflicts)
        (user-error "Merge blocked by %d unresolved conflict(s); see preview"
                    (length (plist-get plan :conflicts))))
      (when (yes-or-no-p (format "Permanently merge %d tag(s) into '%s'? "
                                 (length (plist-get plan :source-ids)) target))
        (let ((result (supertag-tag-merge-execute plan)))
          (setq supertag-schema--marked-items nil)
          (supertag-schema-refresh)
          (message "Merged %d tag(s) into '%s'; %d node(s), %d file edit(s)."
                   (length (plist-get result :source-ids))
                   (plist-get result :target-id)
                   (plist-get result :node-count)
                   (plist-get result :file-change-count)))))))

(cl-defun supertag-schema--cleanup-inherited-field-associations (tag-id)
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
    (princ "  a n     Add nested path tag below current namespace\n")
    (princ "  a c     Add inheritance child (create new OR select existing)\n")
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
    (princ "  m M     Merge marked tags\n")
    (princ "  m       Mark (legacy shortcut)\n")
    (princ "  u       Unmark (legacy shortcut)\n")
    (princ "  U       Unmark all (legacy shortcut)\n")
    (princ "  E       Extend marked (legacy shortcut)\n\n")

    (princ "View Commands (prefix: v):\n")
    (princ "  v t     Open exact/descendant table at point\n")
    (princ "  v v     Select a custom view at point\n")
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

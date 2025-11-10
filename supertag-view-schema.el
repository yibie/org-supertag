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
  "Interactively edit the definition (name, type, options) of the field at point."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (if (not (and context (eq (plist-get context :type) :field)))
        (message "Not on a valid field line.")
      (let* ((tag-id (plist-get context :tag-id))
             (field-name (plist-get context :field-name))
             (inherited-from (plist-get context :inherited-from)))
        
        (if inherited-from
            ;; Inherited Field: Only option is to jump to definition.
            (progn
              (message "Field '%s' is inherited from '%s'. Jumping to definition..." field-name inherited-from)
              (supertag-schema--goto-tag inherited-from))
          
          ;; Own Field: Original workflow for editing name or type/options.
          (let* ((field-def (supertag-tag-get-field tag-id field-name))
                 (action (completing-read "Edit Field: " '("Name" "Type/Options") nil t nil nil "Name")))
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
                (supertag-schema-refresh))))))))))

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
       "Add:   [a f] Field | [a t] Child Tag | [a r] Root Tag"
       "Item:  [r] Rename | [d] Delete | [e] Set Parent | [C-e] Edit Field | [M-↑/↓] Move Field"
       "Batch: [m] Mark | [u] Unmark | [U] Clear Marks | [D] Delete Marked | [E] Extend Marked"
       "Global: [g] Refresh | [q] Quit")
      (goto-char (point-min)))))

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
    (let* ((tag-data (supertag-tag-get tag-id))
           (own-fields (plist-get (supertag-schema--ensure-plist tag-data) :fields))
           (processed-fields (make-hash-table :test 'equal))
           (current-parent-id parent-id))

      ;; 1. Render own fields
      (when own-fields
        (dolist (field-def own-fields)
          (let ((start (point))
                (field-name (plist-get field-def :name)))
            (puthash field-name t processed-fields) ; Mark as processed
            (insert (format "%s  %s\n"
                             indent (supertag-schema--format-field field-def)))
            (add-text-properties start (1- (point))
                                 `(supertag-context (:type :field :tag-id ,tag-id :field-name ,field-name))))))

      ;; 2. Traverse parents and render their fields
      (while current-parent-id
        (let* ((parent-data (supertag-tag-get current-parent-id))
               (parent-fields (plist-get (supertag-schema--ensure-plist parent-data) :fields))
               (fields-to-render '()))
          ;; Collect only new, un-overridden fields
          (dolist (field parent-fields)
            (let ((field-name (plist-get field :name)))
              (unless (gethash field-name processed-fields)
                (puthash field-name t processed-fields)
                (push field fields-to-render))))

          (when fields-to-render
            (insert (format "%s  %s\n"
                             indent (propertize (format "// Inherited from %s" current-parent-id) 'face 'font-lock-comment-face)))
            (dolist (field-def (nreverse fields-to-render))
              (let ((start (point))
                    (field-name (plist-get field-def :name)))
                (insert (format "%s  %s\n"
                                 indent (supertag-schema--format-field field-def)))
                (add-text-properties start (1- (point))
                                     `(supertag-context (:type :field :tag-id ,tag-id :field-name ,field-name :inherited-from ,current-parent-id)))))))

        ;; Move to next parent
        (setq current-parent-id (plist-get (supertag-schema--ensure-plist (supertag-tag-get current-parent-id)) :extends))))

    ;; Render children recursively
    (dolist (child children)
      (supertag-schema--render-tag-node child (1+ level)))))

(defun supertag-schema--format-field (field-def)
  "Format a single field definition into a display string."
  (let* ((name (plist-get field-def :name))
         (type (plist-get field-def :type))
         (options (plist-get field-def :options))
         (type-str (if type (format "(type: %s)" (substring (symbol-name type) 1)) "(type: string)")))
    (if (and (eq type :options) options)
        (format "- %s %s %s" name type-str options)
      (format "- %s %s" name type-str))))

;;; --- Major Mode and User Command ---

(define-derived-mode supertag-schema-view-mode special-mode "Schema"
  "A major mode for viewing the Org-Supertag schema."
  (setq-local buffer-read-only t)
  (let ((map (make-sparse-keymap)))
    ;; Create a prefix map for 'add' commands
    (let ((add-map (make-sparse-keymap "Add...")))
      (define-key add-map "f" #'supertag-schema--add-field-at-point)
      (define-key add-map "t" #'supertag-schema--add-child-tag-at-point)
      (define-key add-map "r" #'supertag-schema--add-new-tag)
      (define-key map "a" add-map))
    ;; Marking
    (define-key map "m" #'supertag-schema--mark-item)
    (define-key map "u" #'supertag-schema--unmark-item)
    (define-key map "U" #'supertag-schema--unmark-all)
    ;; Batch Actions
    (define-key map "D" #'supertag-schema--batch-delete-marked-items)
    (define-key map "E" #'supertag-schema--batch-extends-marked-tags)
    ;; Single-item Actions
    (define-key map "r" #'supertag-schema--rename-at-point)
    (define-key map "d" #'supertag-schema--delete-at-point)
    (define-key map "e" #'supertag-view-schema-set-extends)
    (define-key map (kbd "C-e") #'supertag-schema--edit-field-definition-at-point) ; C-e to edit field
    (define-key map (kbd "M-<up>") #'supertag-schema--move-field-up)
    (define-key map (kbd "M-<down>") #'supertag-schema--move-field-down)
    (define-key map "g" #'supertag-schema-refresh)
    (define-key map "q" #'quit-window)
    ;; Navigation
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "j" #'next-line)
    (define-key map "k" #'previous-line)
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
  "Interactively create a new tag that extends the tag at point."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (if (not (and context (eq (plist-get context :type) :tag)))
        (message "Not on a valid tag line to add a child to.")
      (let* ((parent-id (plist-get context :tag-id))
             (child-name (read-string (format "New child tag name for '%s': " parent-id))))
        (if (and child-name (not (string-empty-p child-name)))
            (progn
              (supertag-tag-create `(:name ,child-name :extends ,parent-id))
              (message "Child tag '%s' created under '%s'. Refreshing..." child-name parent-id)
              (supertag-schema-refresh))
          (message "Tag creation cancelled."))))))

(defun supertag-schema--add-field-at-point ()
  "Interactively add a new field to the tag at the current line."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (if (and context (eq (plist-get context :type) :tag))
        (let* ((tag-id (plist-get context :tag-id))
               (field-def (supertag-ui-create-field-definition)))
          (if field-def
              (progn
                (supertag-tag-add-field tag-id field-def)
                (message "Field '%s' added to tag '%s'. Refreshing view..."
                         (plist-get field-def :name) tag-id)
                (supertag-schema-refresh))
            (message "Field creation cancelled.")))
      (message "Not on a valid tag line."))))

(defun supertag-schema--delete-at-point ()
  "Interactively delete the tag or field at the current line.
Dispatches to the correct deletion logic based on context."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (pcase (plist-get context :type)
      (:field
       (let* ((tag-id (plist-get context :tag-id))
              (field-name (plist-get context :field-name))
              (inherited-from (plist-get context :inherited-from)))
         (if inherited-from
             (message "Cannot delete: Field '%s' is inherited from '%s'. Delete it from the parent tag." field-name inherited-from)
           (when (yes-or-no-p (format "Really delete field '%s' from tag '%s'?" field-name tag-id))
             (supertag-tag-remove-field tag-id field-name)
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

(provide 'supertag-view-schema)

;;; supertag-view-schema.el ends here

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
    (message "SCHEMA-DEBUG (1/4): Found %d total tag entries from query." (length all-tags-alist))
    (dolist (pair all-tags-alist)
      (let* ((id (car pair))
             (data (cdr pair))
             ;; Defensively ensure the :id key exists in the data plist.
             (plist-data (plist-put (supertag-schema--ensure-plist data) :id id)))
        (when id
          (puthash id plist-data tags-by-id)
          (message "SCHEMA-DEBUG (1/4): Ingested tag '%s'." id))))
    (message "SCHEMA-DEBUG (1/4): Finished. Prepared map with %d tags." (hash-table-count tags-by-id))
    tags-by-id))

(defun supertag-schema--calculate-hierarchy (tags-by-id)
  "Calculate parent-child relationships from a map of tags.
Returns a list containing two items: the children-by-id map and the list of root IDs."
  (let ((children-by-id (make-hash-table :test 'equal))
        (root-ids '()))
    (message "SCHEMA-DEBUG (2/4): Calculating hierarchy for %d tags..." (hash-table-count tags-by-id))
    (maphash
     (lambda (id tag-plist)
       (let ((parent-id (plist-get tag-plist :extends)))
         (if (and parent-id (gethash parent-id tags-by-id))
             (progn
               (message "SCHEMA-DEBUG (2/4): Tag '%s' extends %s." id parent-id)
               (push id (gethash parent-id children-by-id)))
           (progn
             (message "SCHEMA-DEBUG (2/4): Tag '%s' is a root." id)
             (push id root-ids)))))
     tags-by-id)
    (let ((unique-roots (cl-delete-duplicates root-ids :test #'equal)))
      (message "SCHEMA-DEBUG (2/4): Finished. Found %d root tags." (length unique-roots))
      (list children-by-id unique-roots))))

(defun supertag-schema--build-tree-from-maps (tags-by-id children-by-id root-ids)
  "Recursively build a tree structure from pre-calculated hierarchy maps."
  (cl-labels ((build-node (id)
                 (let* ((tag-plist (gethash id tags-by-id))
                        (child-ids (sort (gethash id children-by-id) #'string<))
                        (children (mapcar #'build-node child-ids)))
                   (message "SCHEMA-DEBUG (3/4): Building node for '%s', found %d children." id (length children))
                   (plist-put (cl-copy-list tag-plist) :children children))))
    (let ((sorted-roots (sort root-ids #'string<)))
      (message "SCHEMA-DEBUG (3/4): Building tree from %d sorted root nodes..." (length sorted-roots))
      (mapcar #'build-node sorted-roots))))

(defun supertag-schema--build-tree ()
  "Build a hierarchical tree of all tags by composing smaller helper functions."
  (message "SCHEMA-DEBUG: Starting to build schema tree...")
  (let* ((tags-by-id (supertag-schema--get-all-tags-by-id))
         (hierarchy (supertag-schema--calculate-hierarchy tags-by-id))
         (children-by-id (car hierarchy))
         (root-ids (cadr hierarchy))
         (tree (supertag-schema--build-tree-from-maps tags-by-id children-by-id root-ids)))
    (message "SCHEMA-DEBUG: Finished building schema tree. Result has %d root nodes." (length tree))
    tree))


;;; --- Interactive Helpers ---

(defun supertag-schema--get-context-at-point ()
  "Parse the buffer to find the tag or field at point from the simplified view."
  (save-excursion
    (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (cond
       ;; Check for field: "  - field-name ..."
       ((string-match "^\\s-*- \\([[:graph:]]+\\)" line)
        (let ((field-name (match-string 1 line))
              (tag-id nil)
              (original-indent (current-indentation)))
          ;; Search backwards for the parent tag line, which must have less indentation
          (save-excursion
            (while (and (not tag-id) (> (line-beginning-position) (point-min)))
              (previous-line 1)
              (when (< (current-indentation) original-indent)
                (let ((parent-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                  (when (string-match "^\\s-*\\([[:graph:]]+\\)" parent-line)
                    (setq tag-id (match-string 1 parent-line)))))))
          (when tag-id
            `(:type :field :field-name ,field-name :tag-id ,tag-id))))
       ;; Check for tag: "tag-name ..."
       ((string-match "^\\s-*\\([[:graph:]]+\\)" line)
        (let ((tag-id (match-string 1 line)))
          `(:type :tag :tag-id ,tag-id)))
       (t nil)))))

(defun supertag-schema--rename-field-at-point ()
  "Interactively rename the field at the current line."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (if (and context (eq (plist-get context :type) :field))
        (let* ((tag-id (plist-get context :tag-id))
               (old-name (plist-get context :field-name))
               (new-name (read-string (format "Rename field '%s' on tag '%s' to: " old-name tag-id))))
          (if (and new-name (not (string-empty-p new-name)))
              (progn
                (supertag-tag-rename-field tag-id old-name new-name)
                (message "Field '%s' renamed to '%s'. Refreshing view..." old-name new-name)
                (supertag-schema-refresh))
            (message "Field rename cancelled.")))
      (message "Not on a valid field line."))))

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
       "Tag: [a] Add Field | [e] Set Parent | [d] Delete"
       "Field: [r] Rename | [d] Delete | [M-↑/↓] Move"
       "Global: [A] Add Tag | [g] Refresh | [q] Quit")
      (goto-char (point-min)))))

(defun supertag-schema--render-tag-node (tag-node &optional level)
  "Recursively render a tag node and its children into the buffer."
  (let* ((level (or level 0))
         (indent (make-string (* 2 level) ? ))
         (tag-id (plist-get tag-node :id))
         ;; Use the resolved fields from the cache
         (fields (supertag-tag-get-all-fields tag-id))
         (parent-id (plist-get tag-node :extends))
         (children (plist-get tag-node :children)))
    ;; Render the tag itself
    (insert (format "%s%s" indent tag-id))
    (when parent-id
      (insert (propertize (format " -> %s" parent-id) 'face 'font-lock-comment-face)))
    (insert "\n")

    ;; Render fields if any
    (when fields
      (dolist (field fields)
        (insert (format "%s  %s\n" indent (supertag-schema--format-field field)))))

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
    ;; Marking
    (define-key map "m" #'supertag-schema--mark-item)
    (define-key map "u" #'supertag-schema--unmark-item)
    (define-key map "U" #'supertag-schema--unmark-all)
    ;; Batch Actions
    (define-key map "D" #'supertag-schema--batch-delete-marked-items)
    (define-key map "E" #'supertag-schema--batch-extends-marked-tags)
    ;; Single-item Actions
    (define-key map "r" #'supertag-schema--rename-field-at-point)
    (define-key map "d" #'supertag-schema--delete-at-point)
    (define-key map "a" #'supertag-schema--add-field-at-point) ; 'a' for add field
    (define-key map "A" #'supertag-schema--add-new-tag)      ; 'A' for add tag
    (define-key map "e" #'supertag-view-schema-set-extends)
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
(defun supertag-schema-view ()
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
          (supertag--set-parent child-id parent-id)
          (message "Set '%s' to extend '%s'. Refreshing..." child-id parent-id)
          (supertag-schema-refresh)))))))

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
              (field-name (plist-get context :field-name)))
         (when (yes-or-no-p (format "Really delete field '%s' from tag '%s'?" field-name tag-id))
           (supertag-tag-remove-field tag-id field-name)
           (message "Field '%s' deleted. Refreshing view..." field-name)
           (supertag-schema-refresh))))
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
          (supertag-tag-move-field-up tag-id field-name)
          (message "Moved field '%s' up. Refreshing..." field-name)
          (supertag-schema-refresh))
      (message "Not on a valid field line."))))

(defun supertag-schema--move-field-down ()
  "Move the field at the current line down in the tag's field list."
  (interactive)
  (let ((context (supertag-schema--get-context-at-point)))
    (if (and context (eq (plist-get context :type) :field))
        (let ((tag-id (plist-get context :tag-id))
              (field-name (plist-get context :field-name)))
          (supertag-tag-move-field-down tag-id field-name)
          (message "Moved field '%s' down. Refreshing..." field-name)
          (supertag-schema-refresh))
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
              (supertag--set-parent (plist-get tag-context :tag-id) parent-id))
            (setq supertag-schema--marked-items nil)
            (supertag-schema-refresh)
            (message "Batch extends complete.")))))))

(provide 'supertag-view-schema)

;;; supertag-view-schema.el ends here

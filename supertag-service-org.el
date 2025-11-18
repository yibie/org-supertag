  `emacs-lisp
  ;;; supertag-service-org.el --- Org Buffer Interaction Service -- lexical-binding: t; --

  ;;; Commentary:
  ;; This module provides high-level functions that correctly synchronize
  ;; changes by using robust, ID-based node location instead of stale
  ;; character positions.

  (require 'cl-lib)
  (require 'org)
  (require 'org-id) ;; Required for org-id-goto
  (require 'subr-x)
  (require 'supertag-core-store)
  (require 'supertag-ops-node)
  (require 'supertag-services-sync)
  (require 'supertag-ops-tag)
  (require 'supertag-ops-relation)
  (require 'supertag-ops-field)
  (require 'supertag-view-helper)

  (defun supertag-service-org--with-node-buffer (node-id func)
    "Find the buffer and position for NODE-ID using robust org-id-goto
  and execute FUNC there.
  Uses save-window-excursion to avoid disrupting user's view."
    (let* ((node-info (supertag-node-get node-id))
           (file-path (plist-get node-info :file)))
      (when (and file-path (file-exists-p file-path))
        (save-window-excursion
          (let ((buffer (find-file-noselect file-path)))
            (with-current-buffer buffer
              (save-excursion
                ;; DO NOT trust the :position property. It can be stale.
                ;; Instead, use the canonical way to find a node by its ID.
                (org-id-goto node-id)
                (when (org-at-heading-p)
                  (funcall func)))))))))

  (defun supertag-service-org--update-buffer-and-resync (node-id buffer-update-func)
    "Generic function to run a buffer-updating function and then trigger a resync."
    (supertag-service-org--with-node-buffer node-id
      (lambda ()
        (funcall buffer-update-func)
        ;; Sync to update memory from the modified buffer
        (when (fboundp 'supertag-node-sync-at-point)
          (supertag-node-sync-at-point))
        (save-buffer)
        ;; Mark as internal modification AFTER saving
        (when (buffer-file-name)
          (supertag--mark-internal-modification (buffer-file-name))))))

  (defun supertag-service-org-set-todo-state (node-id state)
    "Sets the TODO state for NODE-ID in the buffer and triggers a resync."
    (supertag-service-org--update-buffer-and-resync node-id
      (lambda () (org-todo state))))

  (defun supertag-service-org-add-tag (node-id tag-name)
    "Adds #TAG-NAME text to the headline for NODE-ID and triggers a resync."
    (supertag-service-org--update-buffer-and-resync node-id
      (lambda ()
        (end-of-line)
        (insert (concat " #" tag-name)))))

  (defun supertag-service-org-remove-tag (node-id tag-name)
    "Removes #TAG-NAME text from the headline for NODE-ID and triggers a resync."
    (supertag-service-org--update-buffer-and-resync node-id
      (lambda ()
        (let ((tag-regexp (concat "\\s-?#" (regexp-quote tag-name) "\\b")))
          (when (re-search-forward tag-regexp (line-end-position) t)
            (replace-match ""))))))

  ;; ---------------------------------------------------------------------------
  ;; Field export helpers (DB -> Org properties)
  ;; ---------------------------------------------------------------------------

  (defcustom supertag-export-field-property-prefix "ST_"
    "Legacy prefix used for Org properties created when exporting fields.
New exports no longer use this prefix in property names, but the prefix
is still used to recognize and clean up older exported properties."
    :type 'string
    :group 'org-supertag)

  (defcustom supertag-debug-export-fields nil
    "When non-nil, log detailed information during field export."
    :type 'boolean
    :group 'org-supertag)

  (defun supertag-export--sanitize-symbol-component (name)
    "Return NAME uppercased and sanitized for use in property names."
    (let* ((s (upcase (format "%s" (or name ""))))
           (s (replace-regexp-in-string "[^A-Z0-9_]" "_" s)))
      (if (string-empty-p s) "FIELD" s)))

  (defun supertag-export--field-property-name (_tag-id field-name)
    "Build human-friendly Org property name for FIELD-NAME.
TAG-ID is currently ignored; property names are derived only from the
field name, uppercased and sanitized. For example, \"rate\" becomes
\"RATE\" and \"who\" becomes \"WHO\"."
    (supertag-export--sanitize-symbol-component field-name))

  (defun supertag-export--format-date (value)
    "Best-effort formatting of VALUE as a date string."
    (cond
     ((null value) "")
     ((stringp value) value)
     ;; Emacs time list (high low micro pico)
     ((and (listp value) (= (length value) 4))
      (format-time-string "%Y-%m-%d" value))
     ;; Fallback
     (t (format "%s" value))))

  (defun supertag-export--format-timestamp (value)
    "Best-effort formatting of VALUE as a timestamp string."
    (cond
     ((null value) "")
     ((stringp value) value)
     ;; Emacs time list (high low micro pico)
     ((and (listp value) (= (length value) 4))
      (format-time-string "%Y-%m-%d %H:%M" value))
     (t (format "%s" value))))

  (defun supertag-export--field-value-to-string (tag-id field-name value)
    "Convert field VALUE for TAG-ID/FIELD-NAME into a string for properties."
    (let* ((field-def (ignore-errors (supertag-tag-get-field tag-id field-name)))
           (type (plist-get field-def :type)))
      (pcase type
        (:boolean (if value "true" "false"))
        (:date (supertag-export--format-date value))
        (:timestamp (supertag-export--format-timestamp value))
        ;; For list-like types, serialize as readable Lisp.
        (:options (prin1-to-string value))
        (:tag (prin1-to-string value))
        (:node-reference
         ;; Resolve node IDs to their titles for human-friendly export.
         (let* ((ids (cond
                      ((null value) nil)
                      ((and (listp value) (not (stringp value))) value)
                      (t (list value))))
                (titles
                 (delq nil
                       (mapcar
                        (lambda (id)
                          (let* ((id-str (format "%s" id))
                                 (node (supertag-node-get id-str))
                                 (title (plist-get node :title)))
                            (or title id-str)))
                        ids))))
           (mapconcat #'identity titles ", ")))
        ;; Default: stringify.
        (_ (format "%s" (or value ""))))))

  (defun supertag-export--collect-node-field-properties (node-id)
    "Collect all field values for NODE-ID as an alist of (PROP . STRING)."
    (let* ((fields-root (supertag-store-get-collection :fields))
           (node-table (and (hash-table-p fields-root)
                            (gethash node-id fields-root))))
      (when (hash-table-p node-table)
        (let (result)
          (maphash
           (lambda (tag-id tag-table)
             (when (hash-table-p tag-table)
               (maphash
                (lambda (field-name value)
                  (let* ((prop-name (supertag-export--field-property-name tag-id field-name))
                         (str (supertag-export--field-value-to-string tag-id field-name value)))
                    (push (cons prop-name str) result)))
                tag-table)))
           node-table)
          result))))

  (defun supertag-export--apply-properties-at-point (props-alist)
    "Apply PROPS-ALIST as ST_* properties at current heading.
Returns non-nil when any property was changed."
    (let* ((existing (org-entry-properties nil 'standard))
           (prefix supertag-export-field-property-prefix)
           (wanted-keys (mapcar #'car props-alist))
           (changed nil))
      ;; Set or update properties
      (dolist (pair props-alist)
        (let* ((key (car pair))
               (new (or (cdr pair) ""))
               (old (cdr (assoc key existing))))
          (unless (string= (or old "") new)
            (org-entry-put (point) key new)
            (setq changed t))))
      ;; Delete obsolete ST_* properties
      (dolist (pair existing)
        (let ((key (car pair)))
          (when (and (string-prefix-p prefix key)
                     (not (member key wanted-keys)))
            (org-entry-delete (point) key)
            (setq changed t))))
      changed))

  (defun supertag-export-all-fields-to-properties (&optional save-buffers)
    "Export all field values from the database into Org properties.
This scans the :fields collection, and for each node with field values,
adds or updates Org properties on its heading using the prefix
`supertag-export-field-property-prefix'. When SAVE-BUFFERS is non-nil
(or when called interactively with a prefix argument), modified buffers
are saved automatically."
    (interactive "P")
    (let* ((fields-root (supertag-store-get-collection :fields))
           (node-ids (when (hash-table-p fields-root)
                       (let (ids)
                         (maphash (lambda (id _value)
                                    (push id ids))
                                  fields-root)
                         (nreverse ids)))))
      (cond
       ((not (hash-table-p fields-root))
        (message "Supertag export: :fields collection is not initialized."))
       ((null node-ids)
        (message "Supertag export: no nodes with field values found."))
       (t
        (let* ((total (length node-ids))
               (reporter (make-progress-reporter
                          "Supertag: exporting fields to Org properties..."
                          0 total))
               (modified-files (make-hash-table :test 'equal))
               (index 0))
          (dolist (node-id node-ids)
            (cl-incf index)
            (let ((props (supertag-export--collect-node-field-properties node-id)))
              (when props
                (supertag-service-org--with-node-buffer
                 node-id
                 (lambda ()
                   (when (org-at-heading-p)
                     (let ((changed (supertag-export--apply-properties-at-point props)))
                       (when changed
                         (let ((file (buffer-file-name)))
                           (when file
                             (puthash file t modified-files)
                             (when save-buffers
                               (save-buffer)
                               (supertag--mark-internal-modification file)))))))))))
            (progress-reporter-update reporter index))
          (progress-reporter-done reporter)
          (let ((file-count (hash-table-count modified-files)))
            (message "Supertag export: processed %d nodes, modified %d files%s."
                     total
                     file-count
                     (if save-buffers ", saved buffers" ""))))))))

  (provide 'supertag-service-org)
  ;;; supertag-service-org.el ends here

;;; supertag-service-org.el --- Org Buffer Interaction Service -*- lexical-binding: t; -*-

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
(require 'supertag-ops-global-field)
(require 'supertag-view-helper)
(require 'supertag-core-scan)

(defgroup supertag-org-link nil
  "Org link integration for Org-Supertag."
  :group 'org-supertag)

(defcustom supertag-org-id-open-link-auto-enable t
  "When non-nil, let `org-id-open-link` resolve IDs via Org-Supertag first.

This avoids depending on `org-id-locations` when the target node exists in the
Supertag store. The fallback remains the original Org behavior when the node is
unknown to Supertag or the recorded file is missing."
  :type 'boolean
  :group 'supertag-org-link)

(defun supertag-service-org--goto-id-in-current-buffer (node-id)
  "Jump to NODE-ID by searching `:ID:` in the current buffer.

This intentionally does not consult `org-id-locations`."
  (when (and (stringp node-id) (not (string-empty-p node-id)))
    (org-with-wide-buffer
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^[ \t]*:ID:[ \t]*" (regexp-quote node-id) "[ \t]*$")
             nil t)
        (org-back-to-heading t)
        t))))

(defun supertag-service-org-follow-id (node-id)
  "Open NODE-ID using Org-Supertag's node location and a robust `:ID:` search.

Returns non-nil when NODE-ID was handled, nil otherwise."
  (let* ((node (and (stringp node-id) (supertag-node-get node-id)))
         (file (and (listp node) (plist-get node :file))))
    (when (and (stringp file)
               (file-exists-p file)
               ;; Only apply to files in sync scope.
               (or (not (fboundp 'supertag-sync--in-sync-scope-p))
                   (supertag-sync--in-sync-scope-p file)))
      (let ((buffer (find-file-noselect file)))
        (pop-to-buffer buffer)
        (when (supertag-service-org--goto-id-in-current-buffer node-id)
          (org-show-context)
          (recenter)
          t)))))

(defun supertag-service-org--org-id-open-link-advice (orig-fn &rest args)
  "Advice for `org-id-open-link` that prefers Org-Supertag lookup when available."
  (let ((node-id (car args)))
    (if (and supertag-org-id-open-link-auto-enable
             (bound-and-true-p supertag--initialized)
             (stringp node-id)
             (supertag-service-org-follow-id node-id))
        t
      (apply orig-fn args))))

(defun supertag-enable-org-id-open-link-integration ()
  "Enable Org-Supertag integration for `org-id-open-link`."
  (interactive)
  (setq supertag-org-id-open-link-auto-enable t)
  (when (fboundp 'org-id-open-link)
    (advice-add 'org-id-open-link :around #'supertag-service-org--org-id-open-link-advice))
  (message "[supertag] org-id-open-link integration enabled"))

(defun supertag-disable-org-id-open-link-integration ()
  "Disable Org-Supertag integration for `org-id-open-link`."
  (interactive)
  (setq supertag-org-id-open-link-auto-enable nil)
  (when (fboundp 'org-id-open-link)
    (advice-remove 'org-id-open-link #'supertag-service-org--org-id-open-link-advice))
  (message "[supertag] org-id-open-link integration disabled"))

(when supertag-org-id-open-link-auto-enable
  (supertag-enable-org-id-open-link-integration))

(defun supertag-service-org--normalize-plist (data)
  "Return DATA as a plist. Convert hash tables into plists."
  (if (hash-table-p data)
      (let (plist)
        (maphash (lambda (k v)
                   (setq plist (plist-put plist k v)))
                 data)
        plist)
    data))

(defun supertag-service-org--node-tags (node-id)
  "Return the :tags list for NODE-ID, normalized from stored data."
  (let* ((node (supertag-service-org--normalize-plist (supertag-node-get node-id)))
         (tags (plist-get node :tags)))
    (when (listp tags) tags)))

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

(defun supertag-service-org--parent-title (node-id)
  "Return the direct parent's title for NODE-ID, or nil.
If NODE-ID is already a top-level heading, return nil."
  (let* ((node-info (supertag-node-get node-id))
         (file-path (plist-get node-info :file))
         (result nil))
    (when (and file-path (file-exists-p file-path))
      (save-window-excursion
        (let ((buffer (find-file-noselect file-path)))
          (with-current-buffer buffer
            (save-excursion
              (org-id-goto node-id)
              (when (org-at-heading-p)
                (when (org-up-heading-safe)
                  (setq result (org-get-heading t t t t)))))))))
    result))

(defun supertag-service-org--update-buffer-and-resync (node-id buffer-update-func)
  "Generic function to run a buffer-updating function and then trigger a resync."
  (supertag-service-org--with-node-buffer
   node-id
   (lambda ()
     (let ((before-tick (buffer-chars-modified-tick)))
       (funcall buffer-update-func)
       ;; Only sync/save when buffer actually changed, to avoid noisy no-op runs.
       (unless (eq before-tick (buffer-chars-modified-tick))
         ;; Sync to update memory from the modified buffer.
         (when (fboundp 'supertag-node-sync-at-point)
           (supertag-node-sync-at-point))
         ;; Mark internal modification BEFORE save so after-save hook can skip.
         (when (buffer-file-name)
           (supertag--mark-internal-modification (buffer-file-name)))
         (let ((inhibit-message t))
           (save-buffer)))))))

(defun supertag-service-org-set-todo-state (node-id state)
  "Set the TODO STATE for NODE-ID in the buffer and trigger a resync."
  (supertag-service-org--update-buffer-and-resync
   node-id
   (lambda ()
     (let ((inhibit-message t)
           (current (when (fboundp 'org-get-todo-state)
                      (org-get-todo-state))))
       (unless (equal current state)
         (org-todo state))))))

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

(defconst supertag-export--missing (make-symbol "supertag-export-missing")
  "Sentinel used to detect missing field values during export.")

(defun supertag-export--sanitize-symbol-component (name)
  "Return NAME uppercased and sanitized for use in property names."
  (let* ((s (upcase (format "%s" (or name ""))))
         (s (replace-regexp-in-string "[^A-Z0-9_]" "_" s)))
    (if (string-empty-p s) "FIELD" s)))

(defun supertag-export--field-property-name (tag-id field-name)
  "Build human-friendly Org property name for TAG-ID/FIELD-NAME.
When `supertag-use-global-fields' is enabled, prefer FIELD-ID (slug) only
to avoid duplicates across tags that share a field."
  (let* ((field-part (supertag-export--sanitize-symbol-component field-name)))
    (if supertag-use-global-fields
        field-part
      (let ((tag-part (supertag-export--sanitize-symbol-component tag-id)))
        (format "%s_%s" tag-part field-part)))))

(defun supertag-export--legacy-field-property-names (_tag-id field-name)
  "Return legacy property names for FIELD-NAME without the tag prefix.
Includes both plain and `supertag-export-field-property-prefix' variants
to support imports from older exports."
  (let* ((field-part (supertag-export--sanitize-symbol-component field-name))
         (prefixed (concat (upcase supertag-export-field-property-prefix) field-part)))
    (list field-part prefixed)))

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
  (let* ((field-def (if supertag-use-global-fields
                        (supertag-global-field-get (supertag-sanitize-field-id field-name))
                      (ignore-errors (supertag-tag-get-field tag-id field-name))))
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

(defun supertag-export--global-field-order (node-id)
  "Return ordered, deduped field ids for NODE-ID using tag associations.
Falls back to any values already stored on the node to avoid data loss."
  (let ((tags (supertag-service-org--node-tags node-id))
        (seen (make-hash-table :test 'equal))
        (ordered '()))
    (dolist (tag-id tags)
      (dolist (field (supertag-tag-get-all-fields tag-id))
        (let* ((fid (or (plist-get field :id) (plist-get field :name)))
               (slug (and fid (supertag-sanitize-field-id fid))))
          (when (and slug (not (gethash slug seen)))
            (puthash slug t seen)
            (push slug ordered)))))
    ;; Append any field ids that already have values on the node (defensive).
    (let* ((vals (supertag-store-get-collection :field-values))
           (node-table (and (hash-table-p vals) (gethash node-id vals))))
      (when (hash-table-p node-table)
        (maphash
         (lambda (fid _)
           (unless (gethash fid seen)
             (puthash fid t seen)
             (push fid ordered)))
         node-table)))
    (nreverse ordered)))

(defun supertag-export--collect-node-field-properties (node-id)
  "Collect all field values for NODE-ID as an alist of (PROP . STRING)."
  (if supertag-use-global-fields
      (let* ((vals (supertag-store-get-collection :field-values))
             (node-table (and (hash-table-p vals) (gethash node-id vals)))
             (field-ids (supertag-export--global-field-order node-id)))
        (when (hash-table-p node-table)
          (let (result)
            (dolist (fid field-ids)
              (let ((value (gethash fid node-table supertag-export--missing)))
                (unless (eq value supertag-export--missing)
                  (let ((prop (supertag-export--field-property-name nil fid)))
                    (push (cons prop (supertag-export--field-value-to-string nil fid value))
                          result)))))
            (nreverse result))))
    ;; legacy path with tag-scoped fields
    (let* ((fields-root (supertag-store-get-collection :fields))
           (node-table (and (hash-table-p fields-root)
                            (gethash node-id fields-root)))
           (tag-order (or (supertag-service-org--node-tags node-id)
                          (when (hash-table-p node-table)
                            (let (ids)
                              (maphash (lambda (tag-id _value) (push tag-id ids)) node-table)
                              (sort ids #'string<))))))
      (when (hash-table-p node-table)
        (let ((result '()))
          (dolist (tag-id tag-order)
            (let ((tag-table (and (hash-table-p node-table) (gethash tag-id node-table))))
              (when (hash-table-p tag-table)
                (let* ((field-defs (or (supertag-tag-get-all-fields tag-id) '()))
                       (seen '())
                       (missing (make-symbol "supertag-export-missing")))
                  ;; Export fields in the order defined on the tag.
                  (dolist (field-def field-defs)
                    (let ((fname (plist-get field-def :name)))
                      (when fname
                        (let ((value (gethash fname tag-table missing)))
                          (unless (eq value missing)
                            (push fname seen)
                            (push (cons (supertag-export--field-property-name tag-id fname)
                                        (supertag-export--field-value-to-string tag-id fname value))
                                  result))))))
                  ;; Include any leftover fields to avoid data loss.
                  (maphash
                   (lambda (field-name value)
                     (unless (member field-name seen)
                       (push (cons (supertag-export--field-property-name tag-id field-name)
                                   (supertag-export--field-value-to-string tag-id field-name value))
                             result)))
                   tag-table)))))
          (nreverse result))))))

(defun supertag-export--apply-properties-at-point (props-alist)
  "Apply PROPS-ALIST as ST_* properties at current heading.
Returns non-nil when any property was changed."
  (let* ((existing (org-entry-properties nil 'standard))
         (prefix (if supertag-use-global-fields "" supertag-export-field-property-prefix))
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
    ;; Delete obsolete prefixed properties (legacy cleanup). No prefix in global mode.
    (when (and (not (string-empty-p prefix)) (not supertag-use-global-fields))
      (dolist (pair existing)
        (let ((key (car pair)))
          (when (and (string-prefix-p prefix key)
                     (not (member key wanted-keys)))
            (org-entry-delete (point) key)
            (setq changed t)))))
    changed))

(defun supertag-export-all-fields-to-properties (&optional save-buffers)
  "Export all field values from the database into Org properties.
This scans the :fields collection, and for each node with field values,
adds or updates Org properties on its heading using the prefix
`supertag-export-field-property-prefix'. When SAVE-BUFFERS is non-nil
(or when called interactively with a prefix argument), modified buffers
are saved automatically."
  (interactive "P")
  (let* ((fields-root (if supertag-use-global-fields
                          (supertag-store-get-collection :field-values)
                        (supertag-store-get-collection :fields)))
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

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

;; ---------------------------------------------------------------------------
;; Field import helpers (Org properties -> DB)
;; ---------------------------------------------------------------------------

(defconst supertag-import--missing (make-symbol "supertag-import-missing")
  "Sentinel used to detect missing properties during import.")

(defun supertag-import--tags-at-point ()
  "Return sanitized tags for the current heading, preserving buffer order."
  (let ((raw-tags (ignore-errors (org-get-tags nil t))))
    (when raw-tags
      (mapcar #'supertag-sanitize-tag-name raw-tags))))

(defun supertag-import--properties-hash ()
  "Return current heading properties as a hash keyed by uppercase name."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (pair (org-entry-properties nil 'standard))
      (when (car pair)
        (puthash (upcase (car pair)) (cdr pair) table)))
    table))

(defun supertag-import--legacy-name-map (tag-ids)
  "Build a map of legacy property keys to tag/field pairs for TAG-IDS.
Used to disambiguate older exports that did not include the tag prefix."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (tag-id tag-ids)
      (dolist (field (supertag-tag-get-all-fields tag-id))
        (when-let* ((fname (plist-get field :name)))
          (dolist (legacy (supertag-export--legacy-field-property-names tag-id fname))
            (puthash legacy
                     (cons (list :tag tag-id :field fname)
                           (gethash legacy table))
                     table)))))
    table))

(defun supertag-import--select-property-value (tag-id field-name props legacy-map)
  "Return RAW property value for TAG-ID/FIELD-NAME or nil if missing.
Prefers the new TAG_FIELD format. Falls back to legacy field-only names
when they map to a single tag on the node."
  (let* ((primary (supertag-export--field-property-name tag-id field-name))
         (value (gethash primary props supertag-import--missing)))
    (cond
     ((not (eq value supertag-import--missing))
      (cons primary value))
     (t
      (catch 'found
        (dolist (legacy (supertag-export--legacy-field-property-names tag-id field-name))
          (let ((owners (gethash legacy legacy-map)))
            (when (and owners (= (length owners) 1))
              (let ((legacy-val (gethash legacy props supertag-import--missing)))
                (unless (eq legacy-val supertag-import--missing)
                  (throw 'found (cons legacy legacy-val))))))))
      nil))))

(defun supertag-import--normalize-string-list (raw)
  "Return RAW as a list of strings, splitting on commas when needed."
  (cond
   ((null raw) '())
   ((and (listp raw) (not (stringp raw)))
    (mapcar (lambda (item) (if (stringp item) item (format "%s" item))) raw))
   ((stringp raw)
    (let* ((trimmed (string-trim raw))
           (maybe-read (ignore-errors (read trimmed))))
      (cond
       ((and maybe-read (listp maybe-read) (not (stringp maybe-read)))
        (mapcar (lambda (item) (if (stringp item) item (format "%s" item))) maybe-read))
       ((stringp maybe-read) (list maybe-read))
       ((symbolp maybe-read) (list (symbol-name maybe-read)))
       ((numberp maybe-read) (list (format "%s" maybe-read)))
       (t (mapcar #'string-trim (split-string raw "," t "[ \t\n\r]+"))))))
   (t (list (format "%s" raw)))))

(defun supertag-import--find-node-id-by-title (title)
  "Return node ID whose title exactly matches TITLE, or nil if ambiguous."
  (let* ((pattern (format "\\`%s\\'" (regexp-quote title)))
         (matches (supertag-find-nodes-by-title pattern)))
    (when (= (length matches) 1)
      (caar matches))))

(defun supertag-import--resolve-node-references (raw)
  "Resolve RAW node reference text to IDs. Returns (IDS . UNRESOLVED)."
  (let ((items (supertag-import--normalize-string-list raw))
        (resolved '())
        (unresolved '()))
    (dolist (item items)
      (let* ((candidate (and (stringp item) (string-trim item)))
             (id (and candidate
                      (or (and (supertag-node-get candidate) candidate)
                          (supertag-import--find-node-id-by-title candidate)))))
        (if id
            (push id resolved)
          (when candidate
            (push candidate unresolved)))))
    (cons (nreverse resolved) (nreverse unresolved))))

(defun supertag-import--coerce-field-value (field-def raw-value)
  "Convert RAW-VALUE string into a value suitable for FIELD-DEF."
  (let* ((type (plist-get field-def :type))
         (text (or raw-value "")))
    (pcase type
      (:boolean
       (let ((truthy '("t" "true" "yes" "y" "1" "on")))
         (list :value (and (not (string-empty-p text))
                           (member (downcase text) truthy))
               :unresolved nil)))
      (:integer
       (let ((parsed (and (string-match-p "\\`[+-]?[0-9]+\\'" text)
                          (string-to-number text))))
         (list :value (or parsed (unless (string-empty-p text) text))
               :unresolved nil)))
      (:number
       (let ((parsed (and (string-match-p "\\`[+-]?[0-9]*\\.?[0-9]+\\'" text)
                          (string-to-number text))))
         (list :value (or parsed (unless (string-empty-p text) text))
               :unresolved nil)))
      (:date
       (let ((parsed (and (not (string-empty-p text))
                          (ignore-errors (org-read-date nil t text)))))
         (list :value (or parsed (unless (string-empty-p text) text))
               :unresolved nil)))
      (:timestamp
       (let ((parsed (and (not (string-empty-p text))
                          (ignore-errors (org-read-date t t text)))))
         (list :value (or parsed (unless (string-empty-p text) text))
               :unresolved nil)))
      (:options
       (let ((parsed (ignore-errors (read text))))
         (list :value (or parsed (unless (string-empty-p text) text))
               :unresolved nil)))
      (:tag
       (let ((parsed (ignore-errors (read text))))
         (list :value (cond
                       ((and (listp parsed) (not (stringp parsed))) parsed)
                       ((stringp parsed) parsed)
                       ((symbolp parsed) (symbol-name parsed))
                       ((string-empty-p text) nil)
                       (t text))
               :unresolved nil)))
      (:node-reference
       (let* ((parsed (supertag-import--resolve-node-references text))
              (ids (car parsed))
              (unresolved (cdr parsed))
              (packed (supertag-field-pack-node-reference-value ids)))
         (list :value packed :unresolved unresolved)))
      (_ (list :value (if (string-empty-p text) nil text)
               :unresolved nil)))))

(defun supertag-import--global-field-order (node-id tag-ids)
  "Return ordered, deduped field ids for NODE-ID using TAG-IDS."
  (let ((seen (make-hash-table :test 'equal))
        (ordered '()))
    (dolist (tag-id tag-ids)
      (dolist (field (supertag-tag-get-all-fields tag-id))
        (let* ((fid (or (plist-get field :id) (plist-get field :name)))
               (slug (and fid (supertag-sanitize-field-id fid))))
          (when (and slug (not (gethash slug seen)))
            (puthash slug t seen)
            (push slug ordered)))))
    ;; include any existing values to avoid dropping data
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

(defun supertag-import--find-global-property-value (field-id tag-ids props-table)
  "Return (KEY . VALUE) for FIELD-ID using PROPS-TABLE, considering TAG-IDS.
Tries the global property name, prefixed legacy name, and any tag-prefixed
legacy names before giving up."
  (let* ((primary (supertag-export--field-property-name nil field-id))
         (candidates (list primary
                           (concat (upcase supertag-export-field-property-prefix) primary))))
    (dolist (tag-id tag-ids)
      (let ((name (supertag-export--field-property-name tag-id field-id)))
        (push name candidates)
        (push (concat (upcase supertag-export-field-property-prefix) name) candidates)))
    (catch 'found
      (dolist (key (delete-dups (nreverse candidates)))
        (let ((val (gethash key props-table supertag-import--missing)))
          (unless (eq val supertag-import--missing)
            (throw 'found (cons key val)))))
      nil)))

(defun supertag-import--apply-properties-for-node (node-id props-table)
  "Import fields for NODE-ID using PROPS-TABLE (hash of property values).
Returns a plist with :updated count and :unresolved list for references."
  (let* ((fields-root (supertag-store-get-collection :fields))
         (node-table (and (hash-table-p fields-root)
                          (gethash node-id fields-root)))
         (tag-order-raw (or (supertag-import--tags-at-point)
                            (supertag-service-org--node-tags node-id)
                            (when (hash-table-p node-table)
                              (let (ids)
                                (maphash (lambda (tag-id _value) (push tag-id ids)) node-table)
                                (sort ids #'string<)))))
         (tag-order (cl-remove-if-not
                     (lambda (tid)
                       (when (not (supertag-tag-get tid))
                         (message "Supertag import: tag %S not found in store, skipping." tid))
                       (supertag-tag-get tid))
                     tag-order-raw))
         (legacy-map (supertag-import--legacy-name-map tag-order))
         (updates '())
         (unresolved '()))
    (if supertag-use-global-fields
        ;; Global field path: map properties to deduped field ids
        (let ((field-ids (supertag-import--global-field-order node-id tag-order)))
          (dolist (fid field-ids)
            (let* ((field-def (supertag-global-field-get fid))
                   (prop-pair (supertag-import--find-global-property-value fid tag-order props-table)))
              (when field-def
                (when prop-pair
                  (let* ((raw (cdr prop-pair))
                         (parsed (supertag-import--coerce-field-value field-def raw))
                         (value (plist-get parsed :value))
                         (unresolved-tokens (plist-get parsed :unresolved)))
                    (push (list :field fid :value value) updates)
                    (when unresolved-tokens
                      (push (list :node node-id :field fid :tokens unresolved-tokens)
                            unresolved))))))))
      (dolist (u (nreverse updates))
        (let ((fid (plist-get u :field)))
          (when fid
            (supertag-node-set-global-field node-id fid (plist-get u :value))))))
    ;; Legacy tag-scoped path
    (dolist (tag-id tag-order)
      (let ((field-defs
             (let ((tag-def (supertag-tag-get tag-id)))
               (when tag-def
                 (ignore-errors (supertag-tag-get-all-fields tag-id))))))
        (when (and (null field-defs)
                   (not (supertag-tag-get tag-id)))
          (message "Supertag import: tag %S not found, skipping." tag-id))
        (dolist (field-def field-defs)
          (when-let* ((field-name (plist-get field-def :name))
                      (prop-pair (supertag-import--select-property-value tag-id field-name props-table legacy-map)))
            (let* ((raw (cdr prop-pair))
                   (parsed (supertag-import--coerce-field-value field-def raw))
                   (value (plist-get parsed :value))
                   (unresolved-tokens (plist-get parsed :unresolved)))
              (push (list :tag tag-id :field field-name :value value) updates)
              (when unresolved-tokens
                (push (list :node node-id :tag tag-id :field field-name :tokens unresolved-tokens)
                      unresolved)))))))
    (when updates
      (supertag-field-set-many node-id (nreverse updates))))
  (list :updated (length updates)
        :unresolved (nreverse unresolved)))

(defun supertag-import-fields-from-properties ()
  "Import field values from Org property drawers into the database.
Scans all nodes currently in the database, reads properties that match
the exported TAG_FIELD naming convention (with legacy fallbacks), and
writes them into the :fields collection or global field-values when enabled."
  (interactive)
  (let* ((nodes-root (supertag-store-get-collection :nodes))
         (node-ids (when (hash-table-p nodes-root)
                     (let (ids)
                       (maphash (lambda (id _value) (push id ids)) nodes-root)
                       (sort ids #'string<)))))
    (cond
     ((not (hash-table-p nodes-root))
      (message "Supertag import: :nodes collection is not initialized."))
     ((null node-ids)
      (message "Supertag import: no nodes found in database."))
     (t
      (let* ((total (length node-ids))
             (reporter (make-progress-reporter
                        "Supertag: importing fields from Org properties..."
                        0 total))
             (index 0)
             (updated-nodes 0)
             (updated-fields 0)
             (unresolved '()))
        (dolist (node-id node-ids)
          (cl-incf index)
          (supertag-service-org--with-node-buffer
           node-id
           (lambda ()
             (when (org-at-heading-p)
               (let* ((props-table (supertag-import--properties-hash))
                      (result (supertag-import--apply-properties-for-node node-id props-table)))
                 (when (> (plist-get result :updated) 0)
                   (cl-incf updated-nodes)
                   (cl-incf updated-fields (plist-get result :updated)))
                 (when-let* ((pending (plist-get result :unresolved)))
                   (setq unresolved (nconc unresolved pending)))))))
          (progress-reporter-update reporter index))
        (progress-reporter-done reporter)
        (message "Supertag import: processed %d nodes, applied %d field values%s."
                 total
                 updated-fields
                 (if unresolved
                     (format ", %d unresolved node-reference entries" (length unresolved))
                   ""))
        (when unresolved
          (message "Supertag import: unresolved node references %S" unresolved)))))))

(provide 'supertag-service-org)
;;; supertag-service-org.el ends here

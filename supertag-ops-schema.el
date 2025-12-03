;;; org-supertag/ops/schema.el --- Tag schema materialization for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralizes tag schema resolution, including inheritance handling
;; and caching. The cache stores materialized field definitions for
;; fast runtime access while preserving validation safeguards.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'supertag-core-store)

(defvar supertag-ops-schema--resolved-cache (make-hash-table :test 'equal)
  "Materialized tag schema cache keyed by tag id.")

(defun supertag-ops-schema--ensure-plist (data)
  "Ensure DATA is represented as a plist."
  (cond
   ((null data) nil)
   ((hash-table-p data)
    (let (plist)
      (maphash (lambda (k v)
                 (setq plist (plist-put plist k v)))
               data)
      plist))
   ((listp data) data)
   (t (error "Unsupported tag data format: %S" data))))

(defun supertag-ops-schema--get-all-raw-tags ()
  "Return a hash table of tag-id -> raw tag plist."
  (let ((raw (supertag-store-get-collection :tags))
        (result (make-hash-table :test 'equal))
        (count 0))
    (when (hash-table-p raw)
      (maphash (lambda (tag-id data)
                 ;; (message "[SCHEMA-DEBUG] Raw Tag: %s -> %S" tag-id data)
                 (setq count (1+ count))
                 (puthash tag-id (supertag-ops-schema--ensure-plist data) result))
               raw))
    ;; (message "[SCHEMA-DEBUG] get-all-raw-tags: Found %d tags." count)
    result))

(defun supertag-ops-schema--detect-cycles (parent-map)
  "Raise an error when PARENT-MAP contains inheritance cycles."
  (let ((visited (make-hash-table :test 'equal))
        (stack (make-hash-table :test 'equal)))
    (cl-labels ((dfs (node path)
                  (puthash node t visited)
                  (puthash node t stack)
                  (let ((parent (gethash node parent-map)))
                    (when parent
                      (cond
                       ((gethash parent stack)
                        (error "Tag inheritance cycle detected: %s"
                               (mapconcat #'identity (append path (list parent)) " -> ")))
                       ((not (gethash parent visited))
                        (dfs parent (append path (list parent)))))))
                  (remhash node stack)))
      (maphash (lambda (tag-id _parent)
                 (unless (gethash tag-id visited)
                   (dfs tag-id (list tag-id))))
               parent-map))))

(defun supertag-ops-schema--build-parent-map (raw-tags)
  "Construct a child->parent map from RAW-TAGS and validate it."
  (let ((parent-map (make-hash-table :test 'equal)))
    (maphash
     (lambda (tag-id tag-data)
       (let ((parent (plist-get (supertag-ops-schema--ensure-plist tag-data) :extends)))
         (when (and parent (stringp parent) (not (string-empty-p parent)))
           (if (gethash parent raw-tags)
               (progn
                 (puthash tag-id parent parent-map)
                 ;; (message "[SCHEMA-DEBUG] build-parent-map: Found relation: %s -> %s" tag-id parent)
                 )
             (message "[supertag] Warning: parent tag '%s' referenced by '%s' not found. Ignoring extends."
                      parent tag-id)))))
    raw-tags)
   ;; (message "[SCHEMA-DEBUG] build-parent-map: Final map has %d entries." (hash-table-count parent-map))
    (supertag-ops-schema--detect-cycles parent-map)
    parent-map))

(defun supertag-ops-schema--merge-field-definitions (base override)
  "Merge OVERRIDE plist onto BASE plist and return a fresh copy."
  (let ((result (copy-tree base)))
    (cl-loop for (key val) on override by #'cddr
             do (setq result (plist-put result key val)))
    result))

(defun supertag-ops-schema--get-tag-fields (tag-id raw-tags)
  "Get fields for TAG-ID, supporting both legacy and global field modes.
Returns a list of field definition plists."
  (if (and (boundp 'supertag-use-global-fields) supertag-use-global-fields)
      ;; Global field mode: read from :tag-field-associations and :field-definitions
      (let* ((assoc-table (supertag-store-get-collection :tag-field-associations))
             (defs (supertag-store-get-collection :field-definitions))
             (raw-entries (and (hash-table-p assoc-table) (gethash tag-id assoc-table)))
             (order (cond
                     ;; preferred: list of plists with :field-id
                     ((and (listp raw-entries) (plistp (car raw-entries)))
                      (mapcar (lambda (entry) (plist-get entry :field-id)) raw-entries))
                     ;; fallback: list of field ids
                     ((listp raw-entries) raw-entries)
                     (t nil)))
             (result '()))
        (dolist (fid order (nreverse result))
          (let ((def (and fid (hash-table-p defs) (gethash fid defs))))
            (when def (push (copy-tree def) result)))))
    ;; Legacy mode: read from tag's :fields property
    (let ((tag-data (gethash tag-id raw-tags)))
      (or (plist-get tag-data :fields) '()))))

(defun supertag-ops-schema--resolve-fields-for-tag (tag-id parent-map raw-tags)
  "Resolve final fields for TAG-ID using PARENT-MAP and RAW-TAGS."
  (let ((chain '())
        (current tag-id))
    (while current
      (push current chain)
      (setq current (gethash current parent-map)))
    ;; (message "[SCHEMA-DEBUG] resolve-fields: Inheritance chain for %s: %S" tag-id chain)
    (let ((ordered-fields '()))
      (dolist (tid (nreverse chain))
        (let ((fields (supertag-ops-schema--get-tag-fields tid raw-tags)))
          (dolist (field fields)
            (let ((name (or (plist-get field :name)
                            (plist-get field :id))))
              (when name
                (let ((existing (cl-assoc name ordered-fields :test #'equal)))
                  (if existing
                      ;; Merge: child properties override parent properties while preserving position.
                      (setcdr existing (supertag-ops-schema--merge-field-definitions (cdr existing) field))
                    ;; Append new field, preserving declared order.
                    (setq ordered-fields
                          (append ordered-fields (list (cons name (copy-tree field))))))))))))
      (mapcar #'cdr ordered-fields))))

(defun supertag-ops-schema--materialize-all ()
  "Populate `supertag-ops-schema--resolved-cache' with materialized schemas."
  (let* ((raw-tags (supertag-ops-schema--get-all-raw-tags))
         (parent-map (supertag-ops-schema--build-parent-map raw-tags)))
    (clrhash supertag-ops-schema--resolved-cache)
    (maphash
     (lambda (tag-id tag-data)
       (let* ((final-fields (supertag-ops-schema--resolve-fields-for-tag tag-id parent-map raw-tags))
              (materialized (plist-put (copy-sequence (supertag-ops-schema--ensure-plist tag-data))
                                       :fields final-fields)))
         ;; (message "[SCHEMA-DEBUG] materialize-all: Caching for %s: %S" tag-id materialized)
         (puthash tag-id materialized supertag-ops-schema--resolved-cache)))
     raw-tags)))

;;;###autoload
(defun supertag-ops-schema-rebuild-cache ()
  "Rebuild the tag schema materialization cache."
  (interactive)
  (condition-case err
      (progn
        (supertag-ops-schema--materialize-all)
        (when (called-interactively-p 'interactive)
          (message "Supertag schema cache rebuilt (%d tags)."
                   (hash-table-count supertag-ops-schema--resolved-cache))))
    (error
     (clrhash supertag-ops-schema--resolved-cache)
     (signal (car err) (cdr err)))))

(defun supertag-ops-schema-get-resolved-tag (tag-id)
  "Return the materialized tag plist for TAG-ID, or nil if not cached."
  (gethash tag-id supertag-ops-schema--resolved-cache))

(provide 'supertag-ops-schema)

;;; org-supertag/ops/schema.el ends here

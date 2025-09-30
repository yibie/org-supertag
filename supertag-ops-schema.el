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
  (let ((raw (supertag-get '(:tags)))
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

(defun supertag-ops-schema--resolve-fields-for-tag (tag-id parent-map raw-tags)
  "Resolve final fields for TAG-ID using PARENT-MAP and RAW-TAGS."
  (let ((chain '())
        (current tag-id))
    (while current
      (push current chain)
      (setq current (gethash current parent-map)))
    ;; (message "[SCHEMA-DEBUG] resolve-fields: Inheritance chain for %s: %S" tag-id chain)
    (let ((final-fields (make-hash-table :test 'equal)))
      (dolist (tid chain)
        (let* ((tag-data (gethash tid raw-tags))
               (fields (copy-tree (plist-get tag-data :fields))))
          ;; (message "[SCHEMA-DEBUG] resolve-fields: Processing %s in chain. Own fields: %S" tid fields)
          (dolist (field (or fields '()))
            (let ((name (plist-get field :name)))
              (when name
                (let ((existing-def (gethash name final-fields)))
                  (if existing-def
                      ;; Merge: child properties override parent properties
                      (let ((merged-def (copy-tree existing-def)))
                        (cl-loop for (key val) on field by #'cddr
                                 do (setq merged-def (plist-put merged-def key val)))
                        (puthash name merged-def final-fields))
                    ;; No existing field, just add it
                    (puthash name field final-fields))))))))
      (let (result)
        (maphash (lambda (_k v) (push v result)) final-fields)
        ;; (message "[SCHEMA-DEBUG] resolve-fields: Final merged fields for %s: %S" tag-id result)
        result))))

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

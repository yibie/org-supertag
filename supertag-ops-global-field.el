;;; supertag-ops-global-field.el --- Global field operations for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal ops layer for the global field model:
;; - Global field CRUD stored in :field-definitions
;; - Tag/field association list stored in :tag-field-associations
;; - Node/field values stored in :field-values
;; Intended to coexist with legacy tag-scoped model during transition.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'supertag-core-store)
(require 'supertag-core-schema)
(declare-function supertag-tag--normalize-field-def "supertag-ops-tag" (field-def))

(defun supertag--assoc-entry-field-id (entry)
  "Return field-id string from association ENTRY.
ENTRY may be a plist like (:field-id \"refs\" ...) or a bare string \"refs\"."
  (cond
   ((and (listp entry) (plist-member entry :field-id))
    (plist-get entry :field-id))
   ((stringp entry) entry)
   (t nil)))

(defun supertag--normalize-tag-field-associations (entries)
  "Normalize association ENTRIES into a list of plists.
Accepts both legacy string lists and plist entries."
  (let ((result '())
        (idx 0))
    (dolist (entry (or entries '()) (nreverse result))
      (let ((fid (supertag--assoc-entry-field-id entry)))
        (when (and fid (stringp fid) (not (string-empty-p fid)))
          (push (if (and (listp entry) (plist-member entry :field-id))
                    entry
                  (list :field-id fid :order idx))
                result)
          (setq idx (1+ idx)))))))

(defun supertag--dedupe-tag-field-associations (entries)
  "Deduplicate association ENTRIES by :field-id, preserving first occurrence."
  (let ((seen (make-hash-table :test 'equal))
        (result '()))
    (dolist (entry (supertag--normalize-tag-field-associations entries) (nreverse result))
      (let ((fid (plist-get entry :field-id)))
        (unless (gethash fid seen)
          (puthash fid t seen)
          (push entry result))))))

(defun supertag-global-field--normalize (props)
  "Normalize PROPS for global field creation/update."
  (unless (fboundp 'supertag-tag--normalize-field-def)
    (ignore-errors (require 'supertag-ops-tag)))
  (let* ((name (plist-get props :name))
         (id (or (plist-get props :id)
                 (and name (supertag-sanitize-field-id name)))))
    (unless id
      (error "Global field must have :id or :name"))
    (let ((normalized (plist-put props :id id)))
      (supertag-tag--normalize-field-def normalized))))

(defun supertag-global-field-create (props)
  "Create a global field definition using PROPS plist."
  (let* ((field (supertag-global-field--normalize props))
         (id (plist-get field :id)))
    (when (supertag-store-get-field-definition id)
      (error "Global field '%s' already exists" id))
    (supertag-store-put-field-definition id field t)
    (supertag-schema-rebuild-global-field-caches)
    field))

(defun supertag-global-field-get (field-id)
  "Get global field definition by FIELD-ID."
  (supertag-store-get-field-definition field-id))

(defun supertag-global-field-update (field-id updater)
  "Update global field FIELD-ID using UPDATER fn returning new props."
  (let ((previous (supertag-store-get-field-definition field-id)))
    (unless previous
      (error "Global field '%s' not found" field-id))
    (let* ((updated (funcall updater (copy-tree previous)))
           (normalized (supertag-global-field--normalize updated)))
      (supertag-store-put-field-definition field-id normalized t)
      (supertag-schema-rebuild-global-field-caches)
      normalized)))

(defun supertag-global-field-delete (field-id &optional prune-values)
  "Delete global field FIELD-ID. When PRUNE-VALUES, drop node values and associations."
  (let ((previous (supertag-store-get-field-definition field-id)))
    (unless previous
      (error "Global field '%s' not found" field-id))
    (supertag-store-remove-field-definition field-id)
    (when prune-values
      ;; remove from tag associations
      (let ((assoc-table (supertag-store-get-collection :tag-field-associations)))
        (when (hash-table-p assoc-table)
          (maphash
           (lambda (tag-id entries)
             (when (listp entries)
               (let* ((filtered (cl-remove field-id entries
                                           :key (lambda (e) (plist-get e :field-id))
                                           :test #'equal)))
                 (unless (equal filtered entries)
                   (supertag-store-put-tag-field-associations tag-id filtered t)))))
           assoc-table)))
      ;; remove node values
      (let ((vals (supertag-store-get-collection :field-values)))
        (when (hash-table-p vals)
          (maphash
           (lambda (node-id table)
             (when (hash-table-p table)
               (when (ht-contains? table field-id)
                 (remhash field-id table)
                 (supertag-emit-event :store-changed (list :field-values node-id field-id) previous nil))))
           vals))))
    (supertag-schema-rebuild-global-field-caches)
    previous))

(defun supertag-tag-associate-field (tag-id field-id &optional order)
  "Associate FIELD-ID with TAG-ID. ORDER defaults to append."
  (let* ((assoc-table (supertag-store-get-collection :tag-field-associations))
         (raw (gethash tag-id assoc-table))
         (entries (supertag--dedupe-tag-field-associations raw))
         (existing (cl-find field-id entries :key (lambda (e) (plist-get e :field-id)) :test #'equal))
         (next-order (length entries))
         (new-order (or order next-order))
         (entry (list :field-id field-id :order new-order)))
    (cond
     (existing
      ;; Update order if provided
      (when order
        (setf entries (mapcar (lambda (e)
                                (if (equal (plist-get e :field-id) field-id)
                                    (plist-put (copy-sequence e) :order new-order)
                                  e))
                              entries))
        (supertag-store-put-tag-field-associations tag-id entries t)))
     (t
      (supertag-store-put-tag-field-associations tag-id (append entries (list entry)) t)))
    (supertag-schema-rebuild-global-field-caches)
    (gethash tag-id (supertag-store-get-collection :tag-field-associations))))

(defun supertag-tag-disassociate-field (tag-id field-id)
  "Remove association of FIELD-ID from TAG-ID."
  (let* ((raw (supertag-store-get-tag-field-associations tag-id))
         (entries (supertag--dedupe-tag-field-associations raw))
         (filtered (cl-remove field-id entries :key (lambda (e) (plist-get e :field-id)) :test #'equal)))
    (when (listp raw)
      ;; Important: persist even when FILTERED is empty, otherwise last-entry removal is impossible.
      (supertag-store-put-tag-field-associations tag-id filtered t)
      (supertag-schema-rebuild-global-field-caches))
    filtered))

(defun supertag-node-set-global-field (node-id field-id value)
  "Set VALUE for FIELD-ID on NODE-ID using global field storage."
  (supertag-store-put-field-value node-id field-id value t))

(defun supertag-node-get-global-field (node-id field-id &optional default)
  "Get VALUE for FIELD-ID on NODE-ID, or DEFAULT."
  (supertag-store-get-field-value node-id field-id default))

(provide 'supertag-ops-global-field)

;;; supertag-ops-global-field.el ends here

;;; supertag-core-index.el --- In-memory relation indexes -*- lexical-binding: t; -*-

;;; Commentary:
;; Maintains secondary indexes over the :relations collection to
;; accelerate `supertag-relation-find-by-from', `supertag-relation-find-by-to',
;; and `supertag-relation-find-between' from O(N) full-table scans to O(1)
;; hash-table lookups.
;;
;; Two indexes are kept:
;;   supertag--index-relations-by-from : from-id -> hash-set of relation-ids
;;   supertag--index-relations-by-to   : to-id   -> hash-set of relation-ids
;;
;; Indexes are rebuilt from scratch on store load and maintained
;; incrementally by `supertag-index--on-relation-added' and
;; `supertag-index--on-relation-removed'.

;;; Code:

(require 'cl-lib)

;;; --- Index Variables ---

(defvar supertag--index-relations-by-from (make-hash-table :test 'equal)
  "Index: from-id -> hash-table of relation-id -> t.")

(defvar supertag--index-relations-by-to (make-hash-table :test 'equal)
  "Index: to-id -> hash-table of relation-id -> t.")

;;; --- Incremental Maintenance ---

(defun supertag-index--on-relation-added (relation-id from-id to-id)
  "Record RELATION-ID in from/to indexes for FROM-ID and TO-ID."
  (let ((from-set (gethash from-id supertag--index-relations-by-from)))
    (unless from-set
      (setq from-set (make-hash-table :test 'equal))
      (puthash from-id from-set supertag--index-relations-by-from))
    (puthash relation-id t from-set))
  (let ((to-set (gethash to-id supertag--index-relations-by-to)))
    (unless to-set
      (setq to-set (make-hash-table :test 'equal))
      (puthash to-id to-set supertag--index-relations-by-to))
    (puthash relation-id t to-set)))

(defun supertag-index--on-relation-removed (relation-id from-id to-id)
  "Remove RELATION-ID from from/to indexes for FROM-ID and TO-ID."
  (let ((from-set (gethash from-id supertag--index-relations-by-from)))
    (when from-set
      (remhash relation-id from-set)
      (when (= 0 (hash-table-count from-set))
        (remhash from-id supertag--index-relations-by-from))))
  (let ((to-set (gethash to-id supertag--index-relations-by-to)))
    (when to-set
      (remhash relation-id to-set)
      (when (= 0 (hash-table-count to-set))
        (remhash to-id supertag--index-relations-by-to)))))

;;; --- Full Rebuild ---

(defun supertag-index-rebuild-relations ()
  "Rebuild relation indexes from the :relations collection.
Call this after loading the store from disk."
  (setq supertag--index-relations-by-from (make-hash-table :test 'equal))
  (setq supertag--index-relations-by-to   (make-hash-table :test 'equal))
  (when (and (boundp 'supertag--store)
             (hash-table-p supertag--store))
    (let ((relations (gethash :relations supertag--store)))
      (when (hash-table-p relations)
        (maphash
         (lambda (rel-id relation)
           (when relation
             (let ((from-id (plist-get relation :from))
                   (to-id   (plist-get relation :to)))
               (when (and from-id to-id)
                 (supertag-index--on-relation-added rel-id from-id to-id)))))
         relations)))))

;;; --- Index-Accelerated Queries ---

(defun supertag-index--collect-relations (entity-id index-table &optional type)
  "Collect relation plists for ENTITY-ID from INDEX-TABLE, optionally filtered by TYPE."
  (let ((id-set (gethash entity-id index-table))
        (result '()))
    (when id-set
      (let ((relations-ht (and (boundp 'supertag--store)
                               (hash-table-p supertag--store)
                               (gethash :relations supertag--store))))
        (when (hash-table-p relations-ht)
          (maphash
           (lambda (rel-id _v)
             (let ((relation (gethash rel-id relations-ht)))
               (when (and relation
                          (or (null type)
                              (eq (plist-get relation :type) type)))
                 (push relation result))))
           id-set))))
    result))

(defun supertag-index-find-by-from (from-id &optional type)
  "Find relations originating from FROM-ID.  O(k) where k = matching relations.
Optional TYPE filters by relation type."
  (supertag-index--collect-relations from-id supertag--index-relations-by-from type))

(defun supertag-index-find-by-to (to-id &optional type)
  "Find relations targeting TO-ID.  O(k) where k = matching relations.
Optional TYPE filters by relation type."
  (supertag-index--collect-relations to-id supertag--index-relations-by-to type))

(defun supertag-index-find-between (from-id to-id &optional type)
  "Find relations from FROM-ID to TO-ID.  O(k) where k = from-id's relations.
Optional TYPE filters by relation type."
  (let ((id-set (gethash from-id supertag--index-relations-by-from))
        (result '()))
    (when id-set
      (let ((relations-ht (and (boundp 'supertag--store)
                               (hash-table-p supertag--store)
                               (gethash :relations supertag--store))))
        (when (hash-table-p relations-ht)
          (maphash
           (lambda (rel-id _v)
             (let ((relation (gethash rel-id relations-ht)))
               (when (and relation
                          (equal (plist-get relation :to) to-id)
                          (or (null type)
                              (eq (plist-get relation :type) type)))
                 (push relation result))))
           id-set))))
    result))

(provide 'supertag-core-index)

;;; supertag-core-index.el ends here

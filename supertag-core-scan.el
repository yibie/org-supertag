;;; supertag-core-scan.el --- Scan-based query functions for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides simple, internal API query functions that operate by
;; scanning the core data store. It is the dedicated layer for all
;; scan-based queries, sitting on top of the pure storage layer
;; (`supertag-core-store.el`) and below high-level services.

;;; Code:

(require 'supertag-core-store)


;;; --- Scan-based Query Functions ---

(defun supertag-index-get-nodes-by-tag (tag-name)
  "Find all nodes with TAG-NAME by scanning the store.
This is an O(N) operation."
  (let ((nodes-ht (supertag-get '(:nodes)))
        (results '()))
    (when (hash-table-p nodes-ht)
      (maphash (lambda (node-id node-data)
                 (when (member tag-name (plist-get node-data :tags))
                   (push node-id results)))
               nodes-ht))
    (nreverse results)))

(defun supertag-index-get-nodes-by-word (word)
  "Find all nodes containing WORD by scanning the store.
This is an O(N) operation and performs a simple substring search."
  (let ((nodes-ht (supertag-get '(:nodes)))
        (results '())
        (search-word (downcase word)))
    (when (hash-table-p nodes-ht)
      (maphash (lambda (node-id node-data)
                 (let ((title (plist-get node-data :title))
                       (content (plist-get node-data :content)))
                   (when (or (and title (string-match-p (regexp-quote search-word) (downcase title)))
                             (and content (string-match-p (regexp-quote search-word) (downcase content))))
                     (push node-id results))))
               nodes-ht))
    (nreverse (delete-dups results))))

(defun supertag-index-get-nodes-by-date-range (start-time end-time &optional date-field)
  "Find all nodes created/modified within a date range by scanning.
This is an O(N) operation.
DATE-FIELD can be :created-at or :modified-at (default :created-at)."
  (let* ((field (or date-field :created-at))
         (nodes-ht (supertag-get '(:nodes)))
         (matching-nodes '()))
    (when (hash-table-p nodes-ht)
      (maphash (lambda (node-id node-data)
                 (let ((node-time (plist-get node-data field)))
                   (when node-time
                     (let ((start-check (or (null start-time) (time-less-p start-time node-time)))
                           (end-check (or (null end-time) (time-less-p node-time end-time))))
                       (when (and start-check end-check)
                         (push node-id matching-nodes))))))
               nodes-ht))
    (nreverse matching-nodes)))

(defun supertag-find-nodes-by-tag (tag-name)
  "Find all nodes with TAG-NAME by scanning the store.
This is an O(N) operation.
TAG-NAME is the name of the tag to search for.
Returns a list of (node-id . node-data) pairs."
  (let ((nodes-ht (supertag-get '(:nodes)))
        (results '()))
    (when (hash-table-p nodes-ht)
      (maphash (lambda (node-id node-data)
                 (when (and node-data (member tag-name (plist-get node-data :tags)))
                   (push (cons node-id node-data) results)))
               nodes-ht))
    (nreverse results)))

(defun supertag-find-nodes-by-file (file-path)
  "Find all nodes located in FILE-PATH.
Returns a list of (node-id . node-data) pairs."
  (let ((nodes-collection (supertag-get '(:nodes)))
        (found-nodes '()))
    (when (hash-table-p nodes-collection)
      (maphash
       (lambda (id node-data)
         ;; Safely extract :file and ensure it's a string
         (when-let* ((node-file (and node-data (plist-get node-data :file)))
                     ((stringp node-file)))
           ;; Direct string comparison without path normalization
           (when (equal node-file file-path)
             (push (cons id node-data) found-nodes))))
       nodes-collection))
    (nreverse found-nodes)))

(defun supertag-find-nodes-by-title (title-pattern)
  "Find all nodes whose title matches TITLE-PATTERN by scanning the store.
TITLE-PATTERN is a regular expression string.
Returns a list of (node-id . node-data) pairs."
  (let ((nodes-ht (supertag-get '(:nodes)))
        (results '()))
    (when (hash-table-p nodes-ht)
      (maphash (lambda (node-id node-data)
                 (when (and node-data
                            (plist-get node-data :title)
                            (string-match-p title-pattern (plist-get node-data :title)))
                   (push (cons node-id node-data) results)))
               nodes-ht))
    (nreverse results)))

(defun supertag-find-nodes (predicates)
  "Find nodes satisfying PREDICATES by scanning the store.
PREDICATES is a list of predicate functions. Each predicate function receives (id . data) and returns t or nil.
Returns a list of (node-id . node-data) pairs that satisfy all predicates."
  (let ((nodes-ht (supertag-get '(:nodes)))
        (results '()))
    (when (hash-table-p nodes-ht)
      (maphash (lambda (node-id node-data)
                 (when (and node-data
                            (cl-every (lambda (pred) (funcall pred node-id node-data)) predicates))
                   (push (cons node-id node-data) results)))
               nodes-ht))
    (nreverse results)))

(defun supertag-index-node-has-tag-p (node-id tag-name)
  "Check if a node has a specific tag by direct lookup.
This is an O(1) operation on the node data."
  (when-let ((node-data (supertag-get (list :nodes node-id))))
    (member tag-name (plist-get node-data :tags))))

(provide 'supertag-core-scan)


;;; supertag-core-scan.el ends here

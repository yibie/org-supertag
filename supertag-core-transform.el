;;; org-supertag/transform.el --- Core data transformation mechanism for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file implements the central data transformation mechanism for the
;; Org-Supertag data-centric architecture. It provides a functional,
;; atomic, and consistent way to modify the application state.

;;; Code:

(require 'cl-lib) ; For cl-loop, cl-find, etc.
(require 'ht) ; Ensures `ht` API availability
(require 'org-element)
(require 'supertag-core-store) ; Depends on supertag-get and supertag-update
(require 'supertag-core-state) ; For shared state variables
(require 'supertag-core-notify) ; For supertag--notify-change

;;; --- Core Transform Function ---

(defun supertag-transform (path fn &rest args)
  "Transform data at PATH using function FN.
PATH is a list of keys (e.g., '(:nodes \"123\" :tags)).
FN is a function that receives the current value as its first argument,
and ARGS as subsequent arguments. Returns the transformed value.

Canonical store mode restricts PATH to collection or collection-entity locations."
  (unless (and (listp path) path)
    (error "PATH must be a non-empty list, got: %S" path))
  (when (> (length path) 2)
    (error "Canonical transform only supports collection/entity paths, got: %S" path))
  (let* ((current-value (supertag-get path))
         (new-value (apply fn current-value args)))
    ;; Update store with new value directly.
    ;; Validation is handled by specific ops functions in hybrid architecture.
    ;; Rollback recording (when a transaction is active) happens at the
    ;; store-level seam inside `supertag-update' itself (which ultimately
    ;; calls `supertag-store-put-entity' / puthash on the collection), so
    ;; there is nothing to log here — see `supertag--transaction-record-old-value'.
    (supertag-update path new-value)

    ;; Return new value
    new-value))

;;; --- Batch Transform ---

(defun supertag-batch-transform (transforms)
  "Execute multiple transformations in a batch, ensuring atomicity.
TRANSFORMS is a list of (path fn args...) tuples.
Notifications are suppressed until all transformations are complete."
  (let ((results '()))
    (supertag-core-state-with-suppressed-notifications
     (dolist (transform transforms)
       (let ((path (nth 0 transform))
             (fn (nth 1 transform))
             (args (nthcdr 2 transform)))
         (let ((new-value (apply #'supertag-transform path fn args)))
           (push (cons path new-value) results))))
     (supertag--notify-batch-changes)) ; Call batch notification after all transforms
    (nreverse results)))

;;; --- Transaction Support ---

(defun supertag--transaction-restore-entry (entry)
  "Undo one recorded ENTRY of the form (PATH EXISTED-P OLD-VALUE).
Dispatches on the shape of PATH:
- (:fields NODE-ID TAG-ID FIELD-NAME) — a single legacy field value.
- (:fields NODE-ID TAG-ID) — a per-tag hash table inside the legacy
  `:fields' collection (only ever recorded via
  `supertag-store-put-legacy-field''s node/tag creation markers).
- (:fields NODE-ID) — a per-node hash table inside the legacy `:fields'
  collection. These three `:fields' arms restore by direct
  `puthash'/`remhash' on the live nested hash tables — never through
  `supertag-store-put-entity'/`supertag--normalize-entity', which would
  flatten a hash-table OLD-VALUE into a plist.
- (:field-values NODE-ID FIELD-ID) — a single field value.
- (COLLECTION ID) — a canonical entity (also covers the :field-values
  \"this node's bucket didn't exist yet\" marker, which always has
  EXISTED-P nil and therefore only ever takes the remove branch, so it
  never risks flattening a per-node field hash table into a plist via
  `supertag--normalize-entity').
- (COLLECTION) — a whole-collection replace/clear."
  (let ((path (nth 0 entry))
        (existed-p (nth 1 entry))
        (old-value (nth 2 entry)))
    (cond
     ((and (eq (nth 0 path) :fields) (= (length path) 4))
      (let ((node-id (nth 1 path))
            (tag-id (nth 2 path))
            (field-name (nth 3 path)))
        (if existed-p
            (supertag-store-put-legacy-field node-id tag-id field-name old-value)
          (supertag-store-remove-legacy-field node-id tag-id field-name))))
     ((and (eq (nth 0 path) :fields) (= (length path) 3))
      (let* ((node-id (nth 1 path))
             (tag-id (nth 2 path))
             (fields-root (supertag-store-get-collection :fields))
             (node-table (gethash node-id fields-root)))
        (when (hash-table-p node-table)
          (if existed-p
              (puthash tag-id old-value node-table)
            (remhash tag-id node-table)))))
     ((and (eq (nth 0 path) :fields) (= (length path) 2))
      (let ((node-id (nth 1 path))
            (fields-root (supertag-store-get-collection :fields)))
        (if existed-p
            (puthash node-id old-value fields-root)
          (remhash node-id fields-root))))
     ((= (length path) 3)
      (let ((node-id (nth 1 path))
            (field-id (nth 2 path)))
        (if existed-p
            (supertag-store-put-field-value node-id field-id old-value)
          (supertag-store-remove-field-value node-id field-id))))
     ((= (length path) 2)
      (let ((collection (nth 0 path))
            (id (nth 1 path)))
        (if existed-p
            (supertag-store-put-entity collection id old-value)
          (supertag-store-remove-entity collection id))))
     ((= (length path) 1)
      (if existed-p
          (supertag-update path old-value)
        (supertag-delete path)))
     (t
      (error "supertag--transaction-restore-entry: unsupported path shape %S" path)))))

(defun supertag--transaction-rollback (log)
  "Undo every change recorded in LOG, most-recently-touched path first.
LOG is a list of (PATH EXISTED-P OLD-VALUE) entries as produced by
`supertag--transaction-record-old-value' — since entries are pushed as they
are first recorded, LOG is already in the correct (reverse chronological)
order for `dolist' to walk directly. Restoration itself must not be treated
as new transactional writes, so the active-transaction flag is bound to nil
for the duration."
  (let ((supertag--transaction-active nil)
        (supertag--transaction-seen nil))
    (dolist (entry log)
      (supertag--transaction-restore-entry entry))))

(defvar supertag-after-transaction-rollback-hook nil
  "Hook run after an outer transaction restores its Store state.
Functions on this hook must not mutate the Store.")

(defun supertag--run-transaction-rollback-hooks ()
  "Run every rollback invariant and return the first error, if any."
  (let (first-error)
    (run-hook-wrapped
     'supertag-after-transaction-rollback-hook
     (lambda (function)
       (condition-case err
           (funcall function)
         (error
          (unless first-error
            (setq first-error err))))
       nil))
    first-error))

(defmacro supertag-with-transaction (&rest body)
  "Execute BODY within a transaction.
If an error occurs during BODY execution, every path touched during the
transaction — directly, or transitively via automation actions triggered
synchronously by those writes — is restored to its exact pre-transaction
value: entities created during the transaction are removed again, and
entities deleted during the transaction are resurrected with their original
value. This works because every low-level store mutation primitive
(`supertag-store-put-entity', `supertag-store-remove-entity',
`supertag-store-put-field-value', `supertag-store-remove-field-value', and
the whole-collection replace/clear paths in `supertag-update'/`supertag-delete')
calls `supertag--transaction-record-old-value' before mutating, which is a
no-op unless a transaction is active.

Nesting: invoking `supertag-with-transaction' while one is already active
simply runs BODY inline so its changes join the *enclosing* transaction's
log — there is no separate commit, rollback, or notification flush for the
inner call; only the outermost transaction commits or rolls back.

Notifications are suppressed until the (outermost) transaction commits, at
which point exactly one batch notification flush happens."
  (declare (indent 0))
  `(if supertag--transaction-active
       ;; Already inside a transaction: just run BODY so it joins the
       ;; enclosing transaction's log instead of starting/ending its own.
       (progn ,@body)
     (let ((supertag--transaction-active t) ; Flag for transaction
           (supertag--transaction-log '()) ; Log for rollback
           (supertag--transaction-seen nil) ; Dedup set: first-touch only
           (supertag--tx-success nil)
           (supertag--rollback-error nil)
           result) ; Variable to capture the result
       (unwind-protect
           (progn
             (setq result (supertag-core-state-with-suppressed-notifications
                           (progn ,@body)))
             (setq supertag--tx-success t)
             ;; Commit transaction: notify all pending changes
             (when (fboundp 'supertag--notify-batch-changes)
               (supertag--notify-batch-changes))
             result) ; Return the result
         ;; Cleanup: roll back on error, then always reset transaction state.
         (unless supertag--tx-success
           (supertag--transaction-rollback supertag--transaction-log)
           (setq supertag--rollback-error
                 (supertag--run-transaction-rollback-hooks)))
         (setq supertag--transaction-active nil)
         (setq supertag--transaction-log nil)
         (setq supertag--transaction-seen nil)
         (when supertag--rollback-error
           (signal (car supertag--rollback-error)
                   (cdr supertag--rollback-error)))))))

;;; --- Path Pattern Matching ---

(defun supertag-transform-pattern (pattern fn &rest args)
  "Apply transformation function FN to all paths matching PATTERN.
PATTERN can contain wildcards (e.g., '(:nodes * :tags)).
Returns a list of (path . new-value) pairs for each transformation."
  (let ((matching-paths (supertag--find-matching-paths pattern)) ; Helper to be implemented in store.el or query.el
        (transforms '()))
    (dolist (path matching-paths)
      (push (list path fn args) transforms))
    (supertag-batch-transform transforms)))

;;; --- Internal Helper for Path Matching ---

(defun supertag--find-matching-paths (pattern)
  "Find all data paths matching PATTERN in the supertag--store.
PATTERN can contain wildcards (e.g., '(:nodes * :tags)).
Returns a list of matching paths."
  (require 'supertag-core-store) ; Ensure supertag--store is available
  (let ((matches '()))
    (supertag--traverse-store-matches supertag--store pattern '() matches)
    matches))

(defun supertag--traverse-store-matches (store pattern current-path matches)
  "Recursively traverse STORE to find paths matching PATTERN.
STORE is the current hash table being traversed.
PATTERN is the remaining pattern to match.
CURRENT-PATH is the path accumulated so far.
MATCHES is the list to collect matching paths."
  (if (null pattern)
      ;; Pattern exhausted, current-path is a match
      (push (nreverse current-path) matches)
    (let ((key (car pattern))
          (rest-pattern (cdr pattern)))
      (cond
       ;; Wildcard match: match all keys at this level
       ((eq key '*)
        (maphash
         (lambda (k v)
           (when (hash-table-p v)
             (supertag--traverse-store-matches
              v rest-pattern (cons k current-path) matches)))
         store))

       ;; Exact match: continue with specific key
       (t
        (let ((value (gethash key store)))
          (when (and value (hash-table-p value))
             (supertag--traverse-store-matches
              value rest-pattern (cons key current-path) matches))))))))

(defconst supertag-inline-tag-regexp
  "\\(?:\\`\\|\\([[:space:]]\\)\\)#\\([^[:space:]#]+\\)"
  "Regexp for an inline tag at string start or after whitespace.
Group 1 is the optional whitespace boundary; group 2 is the tag name.")

(defun supertag-transform-inline-tag-name-p (name)
  "Return non-nil when NAME can be an inline tag.
An apostrophe immediately after # is Emacs Lisp function-quote syntax,
not a tag."
  (and (stringp name)
       (not (string-empty-p name))
       (not (eq (aref name 0) ?'))))

(defun supertag-transform--inline-tag-object-ranges
    (begin end &optional element restriction)
  "Return Org object ranges between BEGIN and END.
When ELEMENT is nil, parse the region as secondary Org text using
RESTRICTION.  Each range is (BEGIN END TRANSPARENT); sub/superscript
objects are transparent so underscores and carets remain valid Tag text."
  (let* ((parsed (or element
                     (org-element-parse-secondary-string
                      (buffer-substring-no-properties begin end)
                      (org-element-restriction (or restriction 'paragraph)))))
         (parts (cond
                 ((null element) parsed)
                 ((eq (org-element-type element) 'headline)
                  (org-element-property :title element))
                 (t (org-element-contents element))))
         (offset (if element 0 (1- begin)))
         ranges)
    (cl-labels
        ((collect (part)
           (unless (stringp part)
             (let ((object-begin (org-element-property :begin part))
                   (object-end (org-element-property :end part)))
               (when (and object-begin object-end)
                 (setq object-begin (+ offset object-begin)
                       object-end (+ offset object-end))
                 (when (and (< object-begin end) (> object-end begin))
                   (push (list object-begin object-end
                               (memq (org-element-type part)
                                     '(subscript superscript)))
                         ranges))))
             (dolist (child (org-element-contents part))
               (collect child)))))
      (dolist (part (if (listp parts) parts (list parts)))
        (collect part)))
    (sort ranges (lambda (a b) (< (car a) (car b))))))

(defun supertag-transform--inline-tag-prose-end (begin end object-ranges)
  "Return Tag prose end between BEGIN and END using OBJECT-RANGES.
Return nil when BEGIN is inside an Org object.  Transparent ranges may
occur later inside a Tag; other objects terminate it at their opening."
  (unless (cl-some (lambda (range)
                     (and (<= (nth 0 range) begin)
                          (< begin (nth 1 range))))
                   object-ranges)
    (catch 'boundary
      (dolist (range object-ranges)
        (let ((object-begin (nth 0 range))
              (transparent (nth 2 range)))
          (when (and (not transparent)
                     (> object-begin begin)
                     (< object-begin end))
            (throw 'boundary object-begin))))
      end)))

(defun supertag-transform-inline-tag-matches-in-region
    (begin end &optional element restriction)
  "Return range-aware prose Tag matches between BEGIN and END.
Each result is (BEGIN END NAME), with absolute buffer positions.  ELEMENT
may be the parsed headline or paragraph owning the region.  Otherwise the
region is parsed as secondary Org text using RESTRICTION."
  (save-excursion
    (goto-char
     (if (and (> begin (point-min))
              (eq (char-syntax (char-before begin)) ?\s))
         (1- begin)
       begin))
    (let ((object-ranges
           (supertag-transform--inline-tag-object-ranges
            begin end element restriction))
          matches)
      (while (re-search-forward supertag-inline-tag-regexp end t)
        (let* ((name-begin (match-beginning 2))
               (raw-end (match-end 2))
               (tag-begin (1- name-begin))
               (tag-end
                (and (>= tag-begin begin)
                     (save-match-data
                       (supertag-transform--inline-tag-prose-end
                        tag-begin raw-end object-ranges))))
               (name (and tag-end
                          (> tag-end name-begin)
                          (buffer-substring-no-properties name-begin tag-end))))
          (when (and name
                     (supertag-transform-inline-tag-name-p name))
            (push (list tag-begin tag-end name) matches))))
      (nreverse matches))))

(defun supertag-transform-extract-inline-tags (content-string)
  "Extract whitespace-delimited #tags from CONTENT-STRING."
  (let ((tags '()))
    (when content-string
      (with-temp-buffer
        (insert content-string)
        (goto-char (point-min))
        (while (re-search-forward supertag-inline-tag-regexp nil t)
          (let ((tag (match-string 2)))
            (when (supertag-transform-inline-tag-name-p tag)
              (push tag tags))))))
    (nreverse tags)))

(provide 'supertag-core-transform)

;;; org-supertag/transform.el ends here

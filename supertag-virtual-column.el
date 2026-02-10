;;; supertag-virtual-column.el --- Virtual column system -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Org-Supertag Team
;; Keywords: org-mode, tags, fields
;; Version: 1.0

;;; Commentary:

;; Virtual column system for Org-Supertag.
;; Provides computed fields: rollup, formula, aggregate, reference.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'supertag-core-store)
(require 'supertag-core-schema)

(defgroup supertag-virtual-column nil
  "Virtual column settings."
  :group 'supertag)

(defcustom supertag-virtual-column-cache-size 1000
  "Maximum cached values per node."
  :type 'integer
  :group 'supertag-virtual-column)

(defcustom supertag-virtual-column-compute-timeout 5
  "Max seconds for computation."
  :type 'number
  :group 'supertag-virtual-column)

(cl-defstruct (supertag-virtual-column-cache
               (:constructor supertag-virtual-column-cache--make)
               (:copier nil))
  value computed-at dependencies dirty-flag)

(defvar supertag--virtual-column-definitions (make-hash-table :test 'equal))
(defvar supertag--virtual-column-cache (make-hash-table :test 'equal))
(defvar supertag--virtual-column-dependency-graph (make-hash-table :test 'equal))
(defvar supertag--virtual-column-compute-stack nil)
(defvar supertag--virtual-column-current-dependencies nil)

(defconst supertag-virtual-column-types
  '(:rollup :formula :aggregate :reference))

(defconst supertag-virtual-column-rollup-functions
  '(:sum :count :avg :max :min :first :last))

(defun supertag-virtual-column-create (props)
  "Create virtual column from PROPS plist with :id :name :type :params."
  (let* ((id (or (plist-get props :id)
                 (error "Virtual column must have :id")))
         (name (or (plist-get props :name) id))
         (type (plist-get props :type))
         (params (plist-get props :params)))
    (unless (memq type supertag-virtual-column-types)
      (error "Invalid type: %s" type))
    (when (gethash id supertag--virtual-column-definitions)
      (error "Column '%s' already exists" id))
    (let ((def (list :id id :name name :type type :params params
                     :created-at (current-time))))
      (puthash id def supertag--virtual-column-definitions)
      (message "Virtual column '%s' created" id)
      def)))

(defun supertag-virtual-column-get-definition (column-id)
  "Get definition by COLUMN-ID."
  (gethash column-id supertag--virtual-column-definitions))

(defun supertag-virtual-column-update (column-id updater)
  "Update COLUMN-ID using UPDATER function."
  (let ((current (supertag-virtual-column-get-definition column-id)))
    (unless current
      (error "Column '%s' not found" column-id))
    (let* ((updated (funcall updater (copy-tree current)))
           (new-def (list :id column-id
                          :name (or (plist-get updated :name)
                                    (plist-get current :name))
                          :type (or (plist-get updated :type)
                                    (plist-get current :type))
                          :params (or (plist-get updated :params)
                                      (plist-get current :params))
                          :updated-at (current-time)
                          :created-at (plist-get current :created-at))))
      (puthash column-id new-def supertag--virtual-column-definitions)
      (supertag-virtual-column--invalidate-all column-id)
      (message "Virtual column '%s' updated" column-id)
      new-def)))

(defun supertag-virtual-column-delete (column-id)
  "Delete COLUMN-ID."
  (let ((def (supertag-virtual-column-get-definition column-id)))
    (when def
      (remhash column-id supertag--virtual-column-definitions)
      (supertag-virtual-column--invalidate-all column-id)
      (message "Virtual column '%s' deleted" column-id)
      def)))

(defun supertag-virtual-column-list ()
  "List all columns."
  (let (result)
    (maphash (lambda (_id def) (push def result))
             supertag--virtual-column-definitions)
    (sort result (lambda (a b)
                   (string< (plist-get a :id) (plist-get b :id))))))

(defun supertag-virtual-column--cache-get (node-id column-id)
  "Get cache entry."
  (let ((node-cache (gethash node-id supertag--virtual-column-cache)))
    (when node-cache (gethash column-id node-cache))))

(defun supertag-virtual-column--cache-put (node-id column-id value deps)
  "Store VALUE with dependencies DEPS."
  (let ((node-cache (or (gethash node-id supertag--virtual-column-cache)
                        (puthash node-id (make-hash-table :test 'equal)
                                supertag--virtual-column-cache))))
    (puthash column-id
             (supertag-virtual-column-cache--make
              :value value :computed-at (current-time)
              :dependencies deps :dirty-flag nil)
             node-cache)
    value))

(defun supertag-virtual-column--cache-invalidate (node-id column-id)
  "Mark as dirty."
  (let ((entry (supertag-virtual-column--cache-get node-id column-id)))
    (when entry
      (setf (supertag-virtual-column-cache-dirty-flag entry) t))))

(defun supertag-virtual-column--invalidate-all (column-id)
  "Invalidate all entries for COLUMN-ID."
  (maphash (lambda (node-id node-cache)
             (when (gethash column-id node-cache)
               (supertag-virtual-column--cache-invalidate node-id column-id)))
           supertag--virtual-column-cache))

(defun supertag-virtual-column-get (node-id column-id &optional default)
  "Get value for NODE-ID/COLUMN-ID."
  (condition-case err
      (let* ((def (supertag-virtual-column-get-definition column-id))
             (cache (supertag-virtual-column--cache-get node-id column-id)))
        (cond
         ((not def) default)
         ((and cache (not (supertag-virtual-column-cache-dirty-flag cache)))
          (supertag-virtual-column-cache-value cache))
         (t
          (setq supertag--virtual-column-current-dependencies nil)
          (let ((value (supertag-virtual-column--compute node-id def)))
            (supertag-virtual-column--cache-put
             node-id column-id value
             supertag--virtual-column-current-dependencies)
            (or value default)))))
    (error default)))

(defun supertag-virtual-column--compute (node-id def)
  "Compute value for NODE-ID using DEF."
  (let* ((type (plist-get def :type))
         (params (plist-get def :params))
         (stack-item (cons node-id (plist-get def :id))))
    (when (member stack-item supertag--virtual-column-compute-stack)
      (error "Circular dependency"))
    (let ((supertag--virtual-column-compute-stack
           (cons stack-item supertag--virtual-column-compute-stack)))
      (pcase type
        (:rollup (supertag-virtual-column--compute-rollup node-id params))
        (:formula (supertag-virtual-column--compute-formula node-id params))
        (:aggregate (supertag-virtual-column--compute-aggregate node-id params))
        (:reference (supertag-virtual-column--compute-reference node-id params))
        (_ (error "Unknown type: %s" type))))))

(declare-function supertag-relation-find-by-from "supertag-ops-relation" (from-id &optional type))
(declare-function supertag-node-get-global-field "supertag-ops-global-field" (node-id field-id &optional default))
(declare-function supertag-find-nodes-by-tag "supertag-core-scan" (tag-name))

(defun supertag-virtual-column--compute-rollup (node-id params)
  "Compute rollup."
  (let* ((rel (plist-get params :relation))
         (field (plist-get params :field))
         (func (plist-get params :function))
         (relations (when (fboundp 'supertag-relation-find-by-from)
                      (supertag-relation-find-by-from
                       node-id (if (keywordp rel) rel nil))))
         (values
          (when relations
            (cl-loop for r in relations
                     for tid = (plist-get r :to)
                     when tid
                     collect (if (fboundp 'supertag-node-get-global-field)
                                 (supertag-node-get-global-field tid field 0)
                               0)
                     into vals
                     finally return vals))))
    (pcase func
      (:sum (apply #'+ (or values '(0))))
      (:count (length values))
      (:avg (if values (/ (apply #'+ values) (length values)) 0))
      (:max (if values (apply #'max values) nil))
      (:min (if values (apply #'min values) nil))
      (:first (car values))
      (:last (car (last values))))))

(defun supertag-virtual-column--compute-aggregate (_node-id params)
  "Compute aggregate across all nodes with specified tag."
  (let* ((tag (plist-get params :tag))
         (field (plist-get params :field))
         (func (plist-get params :function))
         (nodes (when (fboundp 'supertag-find-nodes-by-tag)
                  (supertag-find-nodes-by-tag tag)))
         (values
          (when nodes
            (cl-loop for (node-id . _node-data) in nodes
                     when node-id
                     collect (if (fboundp 'supertag-node-get-global-field)
                                 (supertag-node-get-global-field node-id field 0)
                               0)
                     into vals
                     finally return vals))))
    (pcase func
      (:sum (apply #'+ (or values '(0))))
      (:count (length values))
      (:avg (if values (/ (apply #'+ values) (length values)) 0))
      (:max (if values (apply #'max values) nil))
      (:min (if values (apply #'min values) nil))
      (:first (car values))
      (:last (car (last values))))))

(defun supertag-virtual-column--compute-reference (node-id params)
  "Compute reference to another node's field."
  (let* ((relation-type (plist-get params :relation))
         (field-name (plist-get params :field))
         (index (or (plist-get params :index) 0))
         (relations (when (fboundp 'supertag-relation-find-by-from)
                      (supertag-relation-find-by-from
                       node-id (if (keywordp relation-type) relation-type nil))))
         (target-node-id
          (when relations
            (let* ((rel (if (and index (> index 0))
                           (nth index relations)
                         (car relations)))
                   (target (when rel (plist-get rel :to))))
              target))))
    (when target-node-id
      (if (fboundp 'supertag-node-get-global-field)
          (supertag-node-get-global-field target-node-id field-name nil)
        nil))))

(defun supertag-virtual-column-refresh (node-id column-id)
  "Force refresh."
  (interactive (list (read-string "Node: ") (read-string "Column: ")))
  (supertag-virtual-column--cache-invalidate node-id column-id)
  (supertag-virtual-column-get node-id column-id))

(defun supertag-virtual-column-clear-cache (&optional node-id)
  "Clear cache."
  (interactive)
  (if node-id
      (remhash node-id supertag--virtual-column-cache)
    (clrhash supertag--virtual-column-cache))
  (message "Cache cleared"))

;; Formula Parser Implementation

(defun supertag-formula-tokenize (input)
  "Tokenize INPUT string into list of tokens."
  (let ((tokens nil)
        (i 0)
        (len (length input)))
    (while (< i len)
      (let ((ch (aref input i)))
        (cond
         ((member ch '(?\s ?\t ?\n ?\r))
          (setq i (1+ i)))
         ((= ch ?+)
          (push '+ tokens)
          (setq i (1+ i)))
         ((= ch ?-)
          (push '- tokens)
          (setq i (1+ i)))
         ((= ch ?*)
          (push '* tokens)
          (setq i (1+ i)))
         ((= ch ?/)
          (push '/ tokens)
          (setq i (1+ i)))
         ((= ch ?()
          (push '*lparen* tokens)
          (setq i (1+ i)))
         ((= ch ?))
          (push '*rparen* tokens)
          (setq i (1+ i)))
         ((and (>= ch ?0) (<= ch ?9))
          (let ((start i))
            (while (and (< i len)
                        (let ((c (aref input i)))
                          (or (and (>= c ?0) (<= c ?9))
                              (= c ?.))))
              (setq i (1+ i)))
            (push (string-to-number (substring input start i)) tokens)))
         ((or (and (>= ch ?a) (<= ch ?z))
              (and (>= ch ?A) (<= ch ?Z))
              (= ch ?_))
          (let ((start i))
            (while (and (< i len)
                        (let ((c (aref input i)))
                          (or (and (>= c ?a) (<= c ?z))
                              (and (>= c ?A) (<= c ?Z))
                              (and (>= c ?0) (<= c ?9))
                              (= c ?_))))
              (setq i (1+ i)))
            (push (substring input start i) tokens)))
         (t
          (error "Invalid character at position %d: %c" i ch)))))
    (nreverse tokens)))

(defun supertag-formula-parse (tokens)
  "Parse TOKENS into AST using recursive descent."
  (let ((pos 0)
        (len (length tokens)))
    (cl-labels
        ((peek ()
           (when (< pos len) (nth pos tokens)))
         (consume ()
           (prog1 (peek) (setq pos (1+ pos))))
         (expect (token)
           (if (eq (peek) token)
               (consume)
             (error "Expected %s but got %s at position %d" token (peek) pos)))
         (parse-expr ()
           (parse-additive))
         (parse-additive ()
           (let ((left (parse-multiplicative)))
             (while (member (peek) '(+ -))
               (let ((op (consume))
                     (right (parse-multiplicative)))
                 (setq left (list op left right))))
             left))
         (parse-multiplicative ()
           (let ((left (parse-primary)))
             (while (member (peek) '(* /))
               (let ((op (consume))
                     (right (parse-primary)))
                 (setq left (list op left right))))
             left))
         (parse-primary ()
           (let ((token (peek)))
             (cond
              ((null token)
               (error "Unexpected end of input"))
              ((eq token '*lparen*)
               (consume)
               (let ((expr (parse-expr)))
                 (expect '*rparen*)
                 expr))
              ((numberp token)
               (consume)
               (list '*number* token))
              ((stringp token)
               (consume)
               (list '*var* token))
              ((eq token '-)
               (consume)
               (list '*neg* (parse-primary)))
              (t
               (error "Unexpected token: %s" token))))))
      (let ((result (parse-expr)))
        (when (< pos len)
          (error "Unexpected token at end: %s" (peek)))
        result))))

(defun supertag-formula-eval (ast node-id)
  "Evaluate AST for NODE-ID, resolving variables as field values."
  (pcase ast
    ((pred numberp) ast)
    (`(*number* ,n) n)
    (`(*var* ,field-name)
     (if (fboundp 'supertag-node-get-global-field)
         (supertag-node-get-global-field node-id field-name 0)
       0))
    (`(+ ,a ,b) (+ (supertag-formula-eval a node-id)
                   (supertag-formula-eval b node-id)))
    (`(- ,a ,b) (- (supertag-formula-eval a node-id)
                   (supertag-formula-eval b node-id)))
    (`(* ,a ,b) (* (supertag-formula-eval a node-id)
                   (supertag-formula-eval b node-id)))
    (`(/ ,a ,b)
     (let ((denom (supertag-formula-eval b node-id)))
       (if (zerop denom) 0
         (/ (supertag-formula-eval a node-id) denom))))
    (`(*neg* ,a) (- (supertag-formula-eval a node-id)))
    (_ (error "Unknown AST node: %s" ast))))

(defun supertag-formula-parse-string (input)
  "Parse INPUT string to AST."
  (let ((tokens (supertag-formula-tokenize input)))
    (supertag-formula-parse tokens)))

(defun supertag-formula-eval-string (input node-id)
  "Parse and evaluate INPUT string for NODE-ID."
  (let ((ast (supertag-formula-parse-string input)))
    (supertag-formula-eval ast node-id)))

(defun supertag-virtual-column--compute-formula (node-id params)
  "Compute formula virtual column."
  (let ((formula (plist-get params :formula)))
    (unless formula
      (error "Formula column requires :formula parameter"))
    (supertag-formula-eval-string formula node-id)))

;; UI Commands for Virtual Columns

(defun supertag-virtual-column-create-interactive ()
  "Interactively create a new virtual column."
  (interactive)
  (let* ((id (read-string "Virtual column ID: "))
         (name (read-string "Display name: " id))
         (type (intern (completing-read "Type: " '("rollup" "formula" "aggregate" "reference") nil t))))
    (when (gethash id supertag--virtual-column-definitions)
      (error "Column '%s' already exists" id))
    (let ((params
           (pcase type
             (:rollup (supertag-virtual-column--read-rollup-params))
             (:formula (supertag-virtual-column--read-formula-params))
             (:aggregate (supertag-virtual-column--read-aggregate-params))
             (:reference (supertag-virtual-column--read-reference-params))
             (_ (error "Unknown type: %s" type)))))
      (supertag-virtual-column-create
       (list :id id :name name :type type :params params))
      (message "Created virtual column '%s'" id))))

(defun supertag-virtual-column--read-rollup-params ()
  "Read params for rollup type."
  (let ((relation (read-string "Relation type (e.g., children): "))
        (field (read-string "Field to aggregate: "))
        (function (intern (completing-read "Function: "
                                          '("sum" "count" "avg" "max" "min" "first" "last")
                                          nil t))))
    (list :relation relation :field field :function function)))

(defun supertag-virtual-column--read-formula-params ()
  "Read params for formula type."
  (let ((formula (read-string "Formula (e.g., (done / total) * 100): ")))
    (list :formula formula)))

(defun supertag-virtual-column--read-aggregate-params ()
  "Read params for aggregate type."
  (let ((tag (read-string "Tag name (e.g., project): "))
        (field (read-string "Field to aggregate: "))
        (function (intern (completing-read "Function: "
                                          '("sum" "count" "avg" "max" "min" "first" "last")
                                          nil t))))
    (list :tag tag :field field :function function)))

(defun supertag-virtual-column--read-reference-params ()
  "Read params for reference type."
  (let ((relation (read-string "Relation type (e.g., parent): "))
        (field (read-string "Field to reference: "))
        (index (read-number "Index (0=first, default 0): " 0)))
    (list :relation relation :field field :index index)))

(defun supertag-virtual-column-edit-interactive ()
  "Interactively edit a virtual column."
  (interactive)
  (let* ((columns (supertag-virtual-column-list))
         (ids (mapcar (lambda (c) (plist-get c :id)) columns))
         (id (completing-read "Edit virtual column: " ids nil t))
         (current (supertag-virtual-column-get-definition id)))
    (unless current
      (error "Column '%s' not found" id))
    (let* ((current-name (plist-get current :name))
           (new-name (read-string "New name (empty to keep): " current-name))
           (updater (lambda (_)
                      (when (and new-name (not (string-empty-p new-name)))
                        (list :name new-name)))))
      (supertag-virtual-column-update id updater)
      (message "Updated virtual column '%s'" id))))

(defun supertag-virtual-column-delete-interactive ()
  "Interactively delete a virtual column."
  (interactive)
  (let* ((columns (supertag-virtual-column-list))
         (ids (mapcar (lambda (c) (plist-get c :id)) columns))
         (id (completing-read "Delete virtual column: " ids nil t)))
    (when (yes-or-no-p (format "Delete virtual column '%s'? " id))
      (supertag-virtual-column-delete id)
      (message "Deleted virtual column '%s'" id))))

(defun supertag-virtual-column-list-interactive ()
  "Display list of virtual columns."
  (interactive)
  (with-output-to-temp-buffer "*Virtual Columns*"
    (princ "Virtual Columns\n")
    (princ "===============\n\n")
    (let ((columns (supertag-virtual-column-list)))
      (if (null columns)
          (princ "No virtual columns defined.\n")
        (dolist (col columns)
          (princ (format "ID: %s\n" (plist-get col :id)))
          (princ (format "  Name: %s\n" (plist-get col :name)))
          (princ (format "  Type: %s\n" (plist-get col :type)))
          (princ (format "  Params: %s\n\n" (plist-get col :params)))))))
  (pop-to-buffer "*Virtual Columns*"))

(defun supertag-virtual-column-init ()
  "Initialize."
  (interactive)
  (clrhash supertag--virtual-column-cache)
  (clrhash supertag--virtual-column-dependency-graph)
  (message "Virtual column system initialized"))

(provide 'supertag-virtual-column)

;;; supertag-virtual-column.el ends here

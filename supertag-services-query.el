;;; org-supertag/services/query.el --- Query system for Org-Supertag -*- lexical-binding: t; -*-

;;; ⚠️  ARCHITECTURE WARNING ⚠️
;; This file should ONLY contain S-expression query engine functionality.
;; For internal API queries, use the index functions in supertag-store.el:
;; - supertag-index-get-nodes-by-tag      (for tag-based queries)
;; - supertag-index-get-nodes-by-word     (for full-text search)  
;; - supertag-index-get-nodes-by-date-range (for time-based queries)
;; - supertag-index-node-has-tag-p        (for boolean checks)
;; - supertag-find-nodes-by-tag           (for complete node data)
;; - supertag-find-nodes-by-file          (for file-based queries)
;; - supertag-find-nodes-by-title         (for title pattern matching)
;; - supertag-find-nodes                  (for complex predicate filtering)
;;
;; DO NOT add any simple query functions to this file!
;;
;;; Commentary:
;; This file provides S-expression query engine for the Org-Supertag
;; data-centric architecture. It allows users to express complex queries
;; using a Lisp-like syntax, but should NOT be used for internal module queries.

;;; Code:

(require 'cl-lib)
(require 'supertag-core-store) ; For supertag-get

;;; --- Query System ---

(defun supertag-query (collection &optional filter)
  "Query data from a COLLECTION in the central store.
COLLECTION is the path to the collection (e.g., :nodes, :tags, :relations).
FILTER is an optional function that receives (id . data) pairs and returns t if the item should be included.
Returns a list of (id . data) pairs for matching items."
  (let ((data (supertag-get (list collection)))) ; collection should be a list of keys
    (if (not (hash-table-p data))
        '() ; Return empty list if collection is not a hash table
      (let ((results '()))
        (maphash
         (lambda (id value)
           (when (or (null filter) (funcall filter id value))
             (push (cons id value) results)))
         data)
        (nreverse results)))))

;;; --- S-expression Query Engine (User-facing) ---
;; All simple API queries have been moved to supertag-store.el index functions

(defun supertag-query-nodes (&optional filter)
  "Query all nodes in the store with an optional filter.
FILTER is an optional function that receives (id node-data) and returns t if the node should be included.
Returns a list of (id . node-data) pairs."
  (supertag-query '(:nodes) filter))



;;; --- S-expression Query Engine ---
;; This is the new high-performance query engine that replaces the old org-supertag-query.el
;; It uses indexes for O(1) lookups instead of O(n) table scans

(require 'cl-lib)

(defun supertag-query-sexp (query-sexp)
  "Execute an S-expression query and return matching node IDs.
QUERY-SEXP is an S-expression like (and (tag \"foo\") (term \"bar\")).
Returns a list of node IDs matching the query."
  (let* ((ast (supertag-query--parse-sexp query-sexp))
         (node-ids (supertag-query--execute-ast ast)))
    (message "Supertag Query Debug: Query %s returned node IDs: %s" query-sexp node-ids)
    node-ids))

(defun supertag-query--parse-sexp (query-sexp)
  "Parse a query S-expression into an AST.
This function is compatible with the old query syntax."
  (let ((op (car query-sexp))
        (args (cdr query-sexp)))
    (cond
     ((eq op 'and) `(:type and :children ,(mapcar #'supertag-query--parse-sexp args)))
     ((eq op 'or) `(:type or :children ,(mapcar #'supertag-query--parse-sexp args)))
     ((eq op 'not)
      (unless (= (length args) 1) 
        (error "'not' operator expects exactly one argument, but got %S" args))
      `(:type not :child ,(supertag-query--parse-sexp (car args))))
     ((eq op 'tag)
      (unless (= (length args) 1) 
        (error "'tag' operator expects exactly one argument, but got %S" args))
      `(:type tag :value ,(if (stringp (car args)) (car args) (symbol-name (car args)))))
     ((eq op 'field)
      (unless (= (length args) 2) 
        (error "'field' operator expects exactly two arguments, but got %S" args))
      `(:type field :key ,(if (stringp (car args)) (car args) (symbol-name (car args)))
              :value ,(if (stringp (cadr args)) (cadr args) (symbol-name (cadr args)))))
     ((eq op 'after)
      (unless (= (length args) 1) 
        (error "'after' operator expects one date string argument, but got %S" args))
      `(:type after :date ,(car args)))
     ((eq op 'before)
      (unless (= (length args) 1) 
        (error "'before' operator expects one date string argument, but got %S" args))
      `(:type before :date ,(car args)))
     ((eq op 'between)
      (unless (= (length args) 2) 
        (error "'between' operator expects two date string arguments, but got %S" args))
      `(:type between :start-date ,(car args) :end-date ,(cadr args)))
     ((eq op 'term)
      (unless (= (length args) 1) 
        (error "'term' operator expects exactly one argument, but got %S" args))
      `(:type term :value ,(if (stringp (car args)) (car args) (symbol-name (car args)))))
     (t (error "Invalid query operator: %S" op)))))

(defun supertag-query--execute-ast (ast)
  "Execute a query AST and return a list of matching node IDs.
This uses indexes for O(1) lookups instead of O(n) table scans."
  (let ((ast-type (plist-get ast :type)))
    (cond
     ((eq ast-type 'and)
      (let ((child-results (mapcar #'supertag-query--execute-ast (plist-get ast :children)))
            (all-node-ids (supertag-query--get-all-node-ids)))
        (cl-reduce (lambda (result next-list)
                     (cl-intersection result next-list :test #'equal))
                   child-results
                   :initial-value all-node-ids)))
     
     ((eq ast-type 'or)
      (let ((child-results (mapcar #'supertag-query--execute-ast (plist-get ast :children))))
        (cl-reduce #'cl-union child-results :initial-value '())))
     
     ((eq ast-type 'not)
      (let ((all-node-ids (supertag-query--get-all-node-ids))
            (nodes-to-exclude (supertag-query--execute-ast (plist-get ast :child))))
        (cl-set-difference (or all-node-ids '()) nodes-to-exclude :test #'equal)))
     
     ;; Fast index-based lookups below
     ((eq ast-type 'tag)
      (let ((tag-value (plist-get ast :value)))
        (message "Supertag Query Debug: Looking up nodes with tag '%s'" tag-value)
        (let ((result (supertag-index-get-nodes-by-tag tag-value)))
          (message "Supertag Query Debug: Tag lookup returned %d nodes: %s" (length result) result)
          result)))
     
     ((eq ast-type 'field)
      (supertag-query--find-nodes-by-field-indexed (plist-get ast :key) (plist-get ast :value)))
     
     ((eq ast-type 'after)
      (let ((query-time (supertag-query--resolve-date-string (plist-get ast :date))))
        (unless query-time (error "Invalid date format for 'after': %s" (plist-get ast :date)))
        (message "Supertag Query Debug: After query - resolved time: %s, current time: %s" query-time (current-time))
        (supertag-index-get-nodes-by-date-range query-time nil)))

     ((eq ast-type 'before)
      (let ((query-time (supertag-query--resolve-date-string (plist-get ast :date))))
        (unless query-time (error "Invalid date format for 'before': %s" (plist-get ast :date)))
        (supertag-index-get-nodes-by-date-range nil query-time)))
     
     ((eq ast-type 'between)
      (let ((start-time (supertag-query--resolve-date-string (plist-get ast :start-date)))
            (end-time (supertag-query--resolve-date-string (plist-get ast :end-date))))
        (unless start-time (error "Invalid start date for 'between': %s" (plist-get ast :start-date)))
        (unless end-time (error "Invalid end date for 'between': %s" (plist-get ast :end-date)))
        (supertag-index-get-nodes-by-date-range start-time end-time)))
     
     ((eq ast-type 'term)
      (supertag-index-get-nodes-by-word (plist-get ast :value)))
     
     (t '()))))

(defun supertag-query--get-all-node-ids ()
  "Get all node IDs in the system."
  (let ((nodes-collection (supertag-get '(:nodes)))
        (all-ids '()))
    (when (hash-table-p nodes-collection)
      (maphash (lambda (id _data) (push id all-ids)) nodes-collection))
    all-ids))

(defun supertag-query--find-nodes-by-field-indexed (field-name value)
  "Find nodes by field using indexed lookup.
This is much faster than the old approach that scanned the entire link table."
  ;; For now, fall back to the existing field query implementation
  ;; TODO: Implement field indexing for even better performance
  (let ((matching-nodes '()))
    ;; Query using new store format
    (let ((nodes-collection (supertag-get '(:nodes))))
      (when (hash-table-p nodes-collection)
        (maphash
         (lambda (node-id node-data)
           (let ((properties (plist-get node-data :properties)))
             (when (and properties 
                        (equal (plist-get properties (intern (concat ":" field-name))) value))
               (push node-id matching-nodes))))
         nodes-collection)))
    matching-nodes))

(defun supertag-query--resolve-date-string (date-str)
  "Resolve a date string into an absolute time value.
Handles absolute dates ('YYYY-MM-DD'), 'now', and relative dates ('-7d', '+1m', etc.).
Compatible with the old query engine date format."
  (let ((now (current-time)))
    (cond
     ;; Case 1: "now"
     ((string= date-str "now") now)

     ;; Case 2: Relative date like "-7d", "+2w", "-1y"
     ((string-match "^\\([+-]\\)?\\([0-9]+\\)\\([dwmy]\\)$" date-str)
      (let* ((sign (if (match-string 1 date-str) (match-string 1 date-str) "+"))
             (num (string-to-number (match-string 2 date-str)))
             (unit (match-string 3 date-str))
             (seconds-per-day 86400)
             (delta-seconds
              (* num
                 (pcase unit
                   ("d" seconds-per-day)
                   ("w" (* 7 seconds-per-day))
                   ;; Approximation: 30 days for a month
                   ("m" (* 30 seconds-per-day))
                   ;; Approximation: 365.25 days for a year
                   ("y" (* 365.25 seconds-per-day))))))
        (if (string= sign "-")
            (time-subtract now (seconds-to-time delta-seconds))
          (time-add now (seconds-to-time delta-seconds)))))

     ;; Case 3: Absolute date "YYYY-MM-DD"
     ((string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" date-str)
      (parse-time-string date-str))

     ;; Default: Invalid format
     (t nil))))

(defun supertag-query--get-fields-from-ast (ast)
  "Extract field keys from the query AST.
Used for generating table headers in Org Babel output."
  (let ((fields '()))
    (cl-labels ((walk (sub-ast)
                  (let ((type (plist-get sub-ast :type)))
                    (cond
                     ((member type '(and or))
                      (dolist (child (plist-get sub-ast :children)) (walk child)))
                     ((eq type 'not)
                      (walk (plist-get sub-ast :child)))
                     ((eq type 'field)
                      (push (plist-get sub-ast :key) fields))))))
      (walk ast))
    (cl-delete-duplicates fields :test #'string=)))

(defun supertag-query--get-node-field-value (node-id field-name)
  "Get the value of FIELD-NAME for NODE-ID.
This uses the new data format instead of the old link table."
  (let ((node-data (supertag-get (list :nodes node-id))))
    (when node-data
      (let ((properties (plist-get node-data :properties)))
        (when properties
          (plist-get properties (intern (concat ":" field-name))))))))

;;; --- Extended Query System for Relations and Databases ---

(defun supertag-query-related (entity-id &optional relation-type direction)
  "Query entities related to ENTITY-ID.
RELATION-TYPE optionally filters by relation type.
DIRECTION can be :from, :to, or nil (both directions).
Returns list of (related-id . related-data) pairs."
  (let ((relations-from (when (or (null direction) (eq direction :from))
                         (require 'supertag-ops-relation)
                         (supertag-relation-find-by-from entity-id relation-type)))
        (relations-to (when (or (null direction) (eq direction :to))
                       (require 'supertag-ops-relation)
                       (supertag-relation-find-by-to entity-id relation-type)))
        (results '()))
    
    ;; Collect related entities from outgoing relations
    (dolist (relation relations-from)
      (let* ((related-id (plist-get relation :to))
             (related-data (or (supertag-get (list :nodes related-id))
                              (supertag-get (list :tags related-id)))))
        (when related-data
          (push (cons related-id related-data) results))))
    
    ;; Collect related entities from incoming relations
    (dolist (relation relations-to)
      (let* ((related-id (plist-get relation :from))
             (related-data (or (supertag-get (list :nodes related-id))
                              (supertag-get (list :tags related-id)))))
        (when related-data
          (push (cons related-id related-data) results))))
    
    (cl-remove-duplicates results :test (lambda (a b) (equal (car a) (car b))))))

(defun supertag-query-database-records (database-id &optional view-config)
  "Query all records (nodes) belonging to a database.
DATABASE-ID is the database tag identifier.
VIEW-CONFIG is optional view configuration for filtering/sorting.
Returns list of (node-id . node-data) pairs."
  (require 'supertag-ops-tag)
  
  (let* ((all-nodes (supertag-query '(:nodes)))
         (database-records '()))
    
    ;; Find all nodes that belong to this database
    (dolist (node-pair all-nodes)
      (let* ((node-id (car node-pair))
             (node-data (cdr node-pair))
             (tags (plist-get node-data :tags)))
        (when (member database-id tags)
          (push node-pair database-records))))
    
    ;; Apply view configuration if provided
    (if view-config
        (supertag-query--apply-view-config database-records view-config)
      database-records)))

(defun supertag-query--apply-view-config (records view-config)
  "Apply view configuration to filter and sort RECORDS.
VIEW-CONFIG contains filter, sort, and grouping options."
  (let* ((filter-config (plist-get view-config :filter))
         (sort-config (plist-get view-config :sort))
         (group-config (plist-get view-config :group-by))
         (limit (plist-get view-config :limit))
         (filtered-records records))
    
    ;; Apply filters
    (when filter-config
      (setq filtered-records 
            (supertag-query--apply-filters filtered-records filter-config)))
    
    ;; Apply sorting
    (when sort-config
      (setq filtered-records 
            (supertag-query--apply-sorting filtered-records sort-config)))
    
    ;; Apply limit
    (when limit
      (setq filtered-records (cl-subseq filtered-records 0 (min limit (length filtered-records)))))
    
    ;; Apply grouping (returns grouped structure)
    (if group-config
        (supertag-query--apply-grouping filtered-records group-config)
      filtered-records)))

(defun supertag-query--apply-filters (records filter-config)
  "Apply filters to RECORDS based on FILTER-CONFIG."
  (cl-remove-if-not
   (lambda (record)
     (let ((node-data (cdr record)))
       (supertag-query--evaluate-filter-condition node-data filter-config)))
   records))

(defun supertag-query--evaluate-filter-condition (node-data condition)
  "Evaluate a filter CONDITION against NODE-DATA."
  (pcase (car condition)
    ('and
     (cl-every (lambda (sub-condition)
                 (supertag-query--evaluate-filter-condition node-data sub-condition))
               (cdr condition)))
    
    ('or
     (cl-some (lambda (sub-condition)
                (supertag-query--evaluate-filter-condition node-data sub-condition))
              (cdr condition)))
    
    ('not
     (not (supertag-query--evaluate-filter-condition node-data (cadr condition))))
    
    ('org-property
     (let* ((prop-name (cadr condition))
            (operator (caddr condition))
            (expected-value (cadddr condition))
            (props (plist-get node-data :properties))
            (actual-value (plist-get props (intern prop-name))))
       (supertag-query--compare-values actual-value operator expected-value)))
    
    ('title
     (let* ((operator (cadr condition))
            (expected-value (caddr condition))
            (actual-value (plist-get node-data :title)))
       (supertag-query--compare-values actual-value operator expected-value)))
    
    ('tag
     (let* ((operator (cadr condition))
            (expected-tag (caddr condition))
            (tags (plist-get node-data :tags)))
       (pcase operator
         ('has (member expected-tag tags))
         ('not-has (not (member expected-tag tags)))
         (_ nil))))
    
    (_ nil)))

(defun supertag-query--compare-values (actual operator expected)
  "Compare ACTUAL value with EXPECTED using OPERATOR."
  (pcase operator
    ('= (equal actual expected))
    ('!= (not (equal actual expected)))
    ('> (and (numberp actual) (numberp expected) (> actual expected)))
    ('< (and (numberp actual) (numberp expected) (< actual expected)))
    ('>= (and (numberp actual) (numberp expected) (>= actual expected)))
    ('<= (and (numberp actual) (numberp expected) (<= actual expected)))
    ('contains (and (stringp actual) (stringp expected) (string-match-p expected actual)))
    ('starts-with (and (stringp actual) (stringp expected) (string-prefix-p expected actual)))
    ('ends-with (and (stringp actual) (stringp expected) (string-suffix-p expected actual)))
    ('empty (or (null actual) (and (stringp actual) (string-empty-p actual))))
    ('not-empty (not (or (null actual) (and (stringp actual) (string-empty-p actual)))))
    (_ nil)))

(defun supertag-query--apply-sorting (records sort-config)
  "Apply sorting to RECORDS based on SORT-CONFIG."
  (let ((sort-field (plist-get sort-config :field))
        (sort-order (or (plist-get sort-config :order) :asc)))
    
    (sort records
          (lambda (a b)
            (let* ((node-a (cdr a))
                   (node-b (cdr b))
                   (value-a (supertag-query--get-sort-value node-a sort-field))
                   (value-b (supertag-query--get-sort-value node-b sort-field)))
              (if (eq sort-order :desc)
                  (supertag-query--compare-sort-values value-b value-a)
                (supertag-query--compare-sort-values value-a value-b)))))))

(defun supertag-query--get-sort-value (node-data field)
  "Get sort value from NODE-DATA for FIELD."
  (pcase field
    ('title (plist-get node-data :title))
    ('created-at (plist-get node-data :created-at))
    ('modified-at (plist-get node-data :modified-at))
    ('todo (plist-get node-data :todo))
    ('priority (plist-get node-data :priority))
    (_
     ;; Check fields
     (let ((props (plist-get node-data :properties)))
       (plist-get props (intern (format ":%s" field)))))))

(defun supertag-query--compare-sort-values (a b)
  "Compare two sort values A and B."
  (cond
   ((and (null a) (null b)) nil)
   ((null a) t)
   ((null b) nil)
   ((and (numberp a) (numberp b)) (< a b))
   ((and (stringp a) (stringp b)) (string< a b))
   ((and (listp a) (listp b)) ; timestamps
    (time-less-p a b))
   (t (string< (format "%s" a) (format "%s" b)))))

(defun supertag-query--apply-grouping (records group-config)
  "Apply grouping to RECORDS based on GROUP-CONFIG.
Returns alist of (group-value . records-list)."
  (let ((group-field (plist-get group-config :field))
        (groups (make-hash-table :test 'equal)))
    
    ;; Group records by field value
    (dolist (record records)
      (let* ((node-data (cdr record))
             (group-value (supertag-query--get-sort-value node-data group-field))
             (group-key (or group-value "__ungrouped__")))
        (let ((existing-group (gethash group-key groups)))
          (puthash group-key (cons record existing-group) groups))))
    
    ;; Convert to alist and sort groups
    (let ((result '()))
      (maphash (lambda (key value)
                 (push (cons key (nreverse value)) result))
               groups)
      (sort result (lambda (a b) (string< (format "%s" (car a)) (format "%s" (car b))))))))

;;; --- Advanced Query Functions ---

(defun supertag-query-join-relations (base-query relation-configs)
  "Join related data to base query results.
BASE-QUERY is the initial query results.
RELATION-CONFIGS specifies which relations to join.
Returns enriched results with related data."
  (mapcar (lambda (record)
            (let* ((entity-id (car record))
                   (entity-data (cdr record))
                   (enriched-data (cl-copy-list entity-data)))
              
              ;; Add related data for each relation config
              (dolist (relation-config relation-configs)
                (let* ((relation-name (plist-get relation-config :name))
                       (relation-type (plist-get relation-config :type))
                       (direction (plist-get relation-config :direction))
                       (related-entities (supertag-query-related entity-id relation-type direction)))
                  
                  ;; Add related entities to the record
                  (setq enriched-data 
                        (plist-put enriched-data 
                                  (intern (format ":related-%s" relation-name))
                                  related-entities))))
              
              (cons entity-id enriched-data)))
          base-query))

(defun supertag-query-aggregate (records aggregate-config)
  "Perform aggregation on RECORDS based on AGGREGATE-CONFIG.
Returns aggregated results."
  (let* ((aggregate-field (plist-get aggregate-config :field))
         (aggregate-function (plist-get aggregate-config :function))
         (group-by (plist-get aggregate-config :group-by))
         (values '()))
    
    ;; Collect values
    (dolist (record records)
      (let* ((node-data (cdr record))
             (value (supertag-query--get-sort-value node-data aggregate-field)))
        (when value
          (push value values))))
    
    ;; Apply aggregation function
    (pcase aggregate-function
      ('count (length values))
      ('sum (when (cl-every #'numberp values) (cl-reduce #'+ values :initial-value 0)))
      ('avg (when (and values (cl-every #'numberp values)) 
              (/ (cl-reduce #'+ values :initial-value 0.0) (length values))))
      ('min (when values (apply #'min values)))
      ('max (when values (apply #'max values)))
      ('first (car values))
      ('last (car (last values)))
      (_ nil))))

;;; --- Query Builder Interface ---

(defun supertag-query-builder ()
  "Create a new query builder instance.
Returns a query builder object that can be chained."
  (list :type :builder
        :collection nil
        :filters '()
        :sorts '()
        :joins '()
        :limit nil
        :group-by nil))

(defun supertag-query-from (builder collection)
  "Set the collection for the query BUILDER."
  (plist-put builder :collection collection))

(defun supertag-query-where (builder field operator value)
  "Add a WHERE condition to the query BUILDER."
  (let ((filters (plist-get builder :filters))
        (condition (list 'org-property field operator value)))
    (plist-put builder :filters (cons condition filters))))

(defun supertag-query-order-by (builder field &optional order)
  "Add an ORDER BY clause to the query BUILDER."
  (let ((sorts (plist-get builder :sorts))
        (sort-config (list :field field :order (or order :asc))))
    (plist-put builder :sorts (cons sort-config sorts))))

(defun supertag-query-limit (builder count)
  "Add a LIMIT clause to the query BUILDER."
  (plist-put builder :limit count))

(defun supertag-query-execute (builder)
  "Execute the query BUILDER and return results."
  (let* ((collection (plist-get builder :collection))
         (filters (plist-get builder :filters))
         (sorts (plist-get builder :sorts))
         (limit (plist-get builder :limit))
         (results (supertag-query (list collection))))
    
    ;; Apply filters
    (when filters
      (setq results 
            (cl-remove-if-not
             (lambda (record)
               (cl-every (lambda (filter)
                          (supertag-query--evaluate-filter-condition (cdr record) filter))
                        filters))
             results)))
    
    ;; Apply sorting
    (when sorts
      (dolist (sort-config (reverse sorts))
        (setq results (supertag-query--apply-sorting results sort-config))))
    
    ;; Apply limit
    (when limit
      (setq results (cl-subseq results 0 (min limit (length results)))))
    
    results))

(defun supertag-query-get-all-data ()
  "Return all nodes and links from the store as a single list of plists.
This is a low-level function for services like knowledge sync."
  (let ((all-items '()))
    (let ((nodes-table (supertag-get '(:nodes)))
          (relations-table (supertag-get '(:relations))))
      ;; Add all nodes
      (when nodes-table
        (maphash (lambda (_id props)
                   (push props all-items))
                 nodes-table))
      ;; Add all relations
      (when relations-table
        (maphash (lambda (_id props)
                   (push props all-items))
                 relations-table)))
    (nreverse all-items)))

(provide 'supertag-services-query)

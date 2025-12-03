;;; supertag-view-table.el --- Table view for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides a generic, responsive grid UI component that can display
;; any query results with real-time updates from the automation system.
;;
;; Key features:
;; - Data source separation: UI is stateless, data provided by query objects
;; - Reactive updates: Automatically updates when underlying data changes
;; - Generic component: Can display any query results, not just specific tags
;; - Named views: Support for predefined view configurations

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'supertag-core-store)
(require 'supertag-core-notify)
(require 'supertag-ops-node)
(require 'supertag-ops-tag)
(require 'supertag-ops-field)
(require 'supertag-ops-relation)
(require 'supertag-ui-commands) ; For backlink helpers
(require 'supertag-services-formula)
(require 'supertag-services-ui)
(require 'supertag-view-helper)
(require 'org)

;;; Faces

(defface supertag-view-table-parent-title-face
  '((t :inherit shadow :background "#303030"))
  "Face used for the parent-title line in the Title column."
  :group 'supertag-view)

;;; --- Core State Management ---

(defvar supertag-view-table--active-views (make-hash-table :test 'equal)
  "Hash table tracking active grid views and their subscriptions.")

(defvar-local supertag-view-table--query-objs nil
  "List of query objects that provide data for this grid view.")

(defvar-local supertag-view-table--entity-ids nil
  "List of entity IDs currently displayed in the grid.")

(defvar-local supertag-view-table--columns nil
  "Column configuration for the grid.")

(defvar-local supertag-view-table--current-table-index 0
  "Index of currently active table in multi-table view.")

(defvar-local supertag-view-table--view-config nil
  "View configuration for filtering, sorting, and column selection.")

(defvar-local supertag-view-table--named-views nil
  "Alist of named views for the current table. ((\"View Name\" . <view-config-plist>) ...)")

(defvar-local supertag-view-table--current-view-name nil
  "Name of the currently active view.")

;;; --- Layout Registry and Render Dispatcher ---

(defvar supertag-view-table--layout-registry (make-hash-table :test 'equal)
  "Registry of layout renderers for view-table. Keys are symbols, values are functions.")

(defun supertag-view-table-register-layout (layout fn)
  "Register a renderer FN for LAYOUT symbol."
  (puthash layout fn supertag-view-table--layout-registry))

(defcustom supertag-view-table-default-layout 'table
  "Default layout used by view-table renderer dispatcher."
  :type '(choice (const table) (symbol))
  :group 'supertag-view)

(defun supertag-view-table-render (layout state)
  "Render STATE using LAYOUT. STATE is a plist produced by `supertag-view-table--build-state'."
  (let* ((lo (or layout supertag-view-table-default-layout 'table))
         (fn (gethash lo supertag-view-table--layout-registry)))
    (unless (functionp fn)
      (error "No renderer registered for layout: %s" lo))
    (funcall fn state)))

;;; --- State Builder (data layer) ---

(defun supertag-view-table--build-state ()
  "Build a render-agnostic state plist for the current table buffer.
Returns a plist with keys:
  :columns  - column definitions (list of plists)
  :rows     - rows as list of plists (:id and :values alist)
  :meta     - additional info (view config, options)"
  (let* ((columns supertag-view-table--columns)
         (entity-ids supertag-view-table--entity-ids)
         (rows (cl-loop for eid in entity-ids
                        for data = (supertag-view-table--get-entity-data eid)
                        when data
                        collect (list :id eid
                                      :values (cl-loop for col in columns
                                                       for key = (plist-get col :key)
                                                       collect (cons key (supertag-view-table--get-cell-value data key col))))))
         (meta (list :view-config supertag-view-table--view-config
                     :current-view-name supertag-view-table--current-view-name
                     :query-obj (supertag-view-table--get-current-query-obj))))
    (list :columns columns :rows rows :meta meta)))

;;; --- Expanded Rows (Inline Details) ---

(defvar-local supertag-view-table--expanded-rows (make-hash-table :test 'equal)
  "Set of expanded row entity-ids for inline details.")

(defun supertag-view-table--row-expanded-p (entity-id)
  (and entity-id (gethash entity-id supertag-view-table--expanded-rows)))

(defun supertag-view-table--expand-row (entity-id)
  (when entity-id (puthash entity-id t supertag-view-table--expanded-rows)))

(defun supertag-view-table--collapse-row (entity-id)
  (when entity-id (remhash entity-id supertag-view-table--expanded-rows)))

(defun supertag-view-table-toggle-row-details ()
  "Toggle inline details under the current row."
  (interactive)
  (let* ((coords (supertag-view-table--get-cell-coords))
         (entity-id (and coords (plist-get coords :entity-id))))
    (if (not entity-id)
        (message "No row selected")
      (if (supertag-view-table--row-expanded-p entity-id)
          (supertag-view-table--collapse-row entity-id)
        (supertag-view-table--expand-row entity-id))
      (supertag-view-table-refresh))))

(defun supertag-view-table-expand-all-rows ()
  "Expand inline details for all rows."
  (interactive)
  (clrhash supertag-view-table--expanded-rows)
  (dolist (eid supertag-view-table--entity-ids)
    (puthash eid t supertag-view-table--expanded-rows))
  (supertag-view-table-refresh))

(defun supertag-view-table-collapse-all-rows ()
  "Collapse inline details for all rows."
  (interactive)
  (clrhash supertag-view-table--expanded-rows)
  (supertag-view-table-refresh))

(defcustom supertag-view-table-image-target-char-height 5
  "Image target display height in table cells (in character lines)."
  :type 'integer
  :group 'supertag-view)

(defcustom supertag-view-table-image-max-width-ratio 0.8
  "Maximum ratio of image width to cell content space."
  :type 'float
  :group 'supertag-view)

(defcustom supertag-view-table-image-column-width 14
  "Default display width of image column (in characters)."
  :type 'integer
  :group 'supertag-view)

(defcustom supertag-view-table-per-tag-image-widths nil
  "Image column width settings for each tag, format: ((tag1 . width1) (tag2 . width2) ...)"
  :type '(alist :key-type string :value-type integer)
  :group 'supertag-view)

;;; --- TODO State Handling ---

(defcustom supertag-view-table-strip-todo-keywords t
  "Whether to strip TODO keywords from node titles in table view.
If non-nil, TODO keywords will be removed from titles.
If nil, titles will be displayed as-is with TODO keywords."
  :type 'boolean
  :group 'supertag-view)

(defcustom supertag-view-table-todo-keywords
  '("TODO" "DONE" "NEXT" "WAITING" "HOLD" "CANCELLED" "CANCELED"
    "STARTED" "DELEGATED" "DEFERRED" "SOMEDAY")
  "List of TODO keywords to strip from node titles in table view.
Only used when `supertag-view-table-strip-todo-keywords' is non-nil.
You can customize this list to match your org-mode TODO keywords."
  :type '(repeat string)
  :group 'supertag-view)

(defun supertag-view-table--strip-todo-keyword (title)
  "Remove TODO keywords from TITLE if configured to do so.
Removes org-mode TODO keywords based on `supertag-view-table-todo-keywords'.
Only strips keywords if `supertag-view-table-strip-todo-keywords' is non-nil."
  (if (not supertag-view-table-strip-todo-keywords)
      title
    (let ((keywords-regexp (concat "^\\("
                                   (mapconcat #'regexp-quote
                                             supertag-view-table-todo-keywords
                                             "\\|")
                                   "\\)\\s-+")))
      (if (string-match keywords-regexp title)
          (string-trim (substring title (match-end 0)))
        title))))

;;; --- Grid Rendering Engine ---

(defun supertag-view-table (data-source &optional columns view-config named-views)
  "Interactive table view for various data sources.
DATA-SOURCE can be:
- A tag name (string) for nodes with that tag
- A plist with :type and :value for specific data types
- A list of query objects for multi-table view
- A function that returns a list of entity IDs

COLUMNS is an optional list of column configurations.
VIEW-CONFIG is an optional view configuration plist with:
- :fields - List of visible field names
- :sort - Sort configuration (:field and :order)
- :filter - Filter conditions
- :group-by - Grouping field
NAMED-VIEWS is an alist of pre-defined views.

If called interactively without DATA-SOURCE, prompts for data source selection."
  (interactive
   (let* ((tag (completing-read "View table for tag: " (supertag-view-table--get-available-tags) nil t)))
     (list (list :type :tag :value tag))))
  
  (let* ((query-objs (if (listp data-source) 
                        (if (plistp (car data-source)) data-source (list data-source))
                      (list data-source)))
         (entity-ids (supertag-view-table--get-entities (car query-objs)))
         (columns (or columns (supertag-view-table--get-columns (car query-objs))))
         (buf-name (if (> (length query-objs) 1)
                      (format "*Supertag Multi-Table*")
                    (format "*Supertag Table: %s*" (plist-get (car query-objs) :value))))
         (buf (get-buffer-create buf-name)))
    
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (supertag-view-table-mode)
        ;; Forcefully set truncate-lines, as it might be overridden by other hooks.
        ;; This ensures that long lines are truncated rather than wrapped.
        (setq-local truncate-lines t)

        ;; Store view state
        (setq-local supertag-view-table--query-objs query-objs)
        (setq-local supertag-view-table--entity-ids entity-ids)
        (setq-local supertag-view-table--columns columns)
        (setq-local supertag-view-table--current-table-index 0)
        (setq-local supertag-view-table--view-config view-config)
        (setq-local supertag-view-table--named-views named-views)
        ;; Set the current view to the first named view if available, otherwise nil.
        (setq-local supertag-view-table--current-view-name (if (and named-views (stringp (caar named-views)))
                                                              (caar named-views)
                                                            nil))

        ;; Apply view configuration if provided
        (let ((active-config (or view-config
                                 (and supertag-view-table--current-view-name
                                      (cdr (assoc supertag-view-table--current-view-name supertag-view-table--named-views))))))
          (when active-config
            (supertag-view-table--apply-view-config active-config)))

        ;; Subscribe to updates
        (supertag-view-table--subscribe-updates)
        
        ;; Render initial grid via new dispatcher
        (let ((state (supertag-view-table--build-state)))
          (supertag-view-table-render supertag-view-table-default-layout state))

        ;; Move cursor to the first cell for better UX
        (supertag-view-table--goto-first-cell)
        
        (message "Rendered table with %d entities" (length entity-ids))))
    
    (let* ((current-window (selected-window))
           (target-window (if (window-parameter current-window 'window-side)
                               (or (when (fboundp 'window-main-window)
                                     (window-main-window (selected-frame)))
                                   (frame-root-window))
                             current-window)))
      (when (and target-window (window-live-p target-window))
        (select-window target-window))
      (switch-to-buffer buf)
      (set-window-scroll-bars (selected-window) nil t nil t))
    buf))

(defun supertag-view-table--apply-view-config (view-config)
  "Apply VIEW-CONFIG to current table state."
  (when view-config
    (let ((visible-fields (plist-get view-config :visible-fields))
          (sort-config (plist-get view-config :sort))
          (filter-config (plist-get view-config :filter)))
      
      ;; Filter columns based on visible fields
      (when visible-fields
        (setq-local supertag-view-table--columns
                  (cl-remove-if-not
                   (lambda (col)
                     (member (symbol-name (plist-get col :key)) visible-fields))
                   supertag-view-table--columns)))
      
      ;; Apply sorting
      (when sort-config
        (setq-local supertag-view-table--entity-ids
                  (supertag-view-table--apply-sorting supertag-view-table--entity-ids sort-config)))
      
      ;; Apply filtering
      (when filter-config
        (setq-local supertag-view-table--entity-ids
                  (supertag-view-table--apply-filtering supertag-view-table--entity-ids filter-config))))))

(defun supertag-view-table--apply-sorting (entity-ids sort-config)
  "Apply sorting to ENTITY-IDS based on SORT-CONFIG."
  (let ((sort-field (plist-get sort-config :field))
        (sort-order (or (plist-get sort-config :order) :asc)))
    
    (sort entity-ids
          (lambda (a b)
            (let* ((entity-a (supertag-view-table--get-entity-data a))
                   (entity-b (supertag-view-table--get-entity-data b))
                   (value-a (supertag-view-table--get-sort-value entity-a sort-field))
                   (value-b (supertag-view-table--get-sort-value entity-b sort-field)))
              (if (eq sort-order :desc)
                  (supertag-view-table--compare-sort-values value-b value-a)
                (supertag-view-table--compare-sort-values value-a value-b)))))))

(defun supertag-view-table--get-sort-value (entity-data field)
  "Get sort value from ENTITY-DATA for FIELD."
  (pcase field
    ('title (plist-get entity-data :title))
    ('created-at (plist-get entity-data :created-at))
    ('modified-at (plist-get entity-data :modified-at))
    ('todo (plist-get entity-data :todo))
    ('priority (plist-get entity-data :priority))
    (_
     ;; Check properties
     (let ((props (plist-get entity-data :properties)))
       (plist-get props (intern (format ":%s" field)))))))

(defun supertag-view-table--compare-sort-values (a b)
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

(defun supertag-view-table--apply-filtering (entity-ids filter-config)
  "Apply filtering to ENTITY-IDS based on FILTER-CONFIG."
  (cl-remove-if-not
   (lambda (entity-id)
     (let ((entity-data (supertag-view-table--get-entity-data entity-id)))
       (supertag-view-table--evaluate-filter-condition entity-data filter-config)))
   entity-ids))

(defun supertag-view-table--evaluate-filter-condition (entity-data condition)
  "Evaluate a filter CONDITION against ENTITY-DATA."
  (pcase (car condition)
    ('and
     (cl-every (lambda (sub-condition)
                 (supertag-view-table--evaluate-filter-condition entity-data sub-condition))
               (cdr condition)))
    
    ('or
     (cl-some (lambda (sub-condition)
                (supertag-view-table--evaluate-filter-condition entity-data sub-condition))
              (cdr condition)))
    
    ('not
     (not (supertag-view-table--evaluate-filter-condition entity-data (cadr condition))))
    
    ('field
     (let* ((field-name (cadr condition))
            (operator (caddr condition))
            (expected-value (cadddr condition))
            (query-obj (supertag-view-table--get-current-query-obj))
            (actual-value
             (pcase (plist-get query-obj :type)
                (:tag
                 ;; For tag queries, get field value using default-aware accessor
                 (let* ((node-id (plist-get entity-data :id))
                        (tag-id (supertag-view-table--get-current-tag-id))
                        ;; Remove leading colon from field-name if present
                        (clean-field-name (if (string-prefix-p ":" field-name)
                                             (substring field-name 1)
                                           field-name)))
                   (when (and node-id tag-id)
                     (supertag-field-get-with-default node-id tag-id clean-field-name))))
                (_
                 ;; For other query types, try properties first
                 (let ((props (plist-get entity-data :properties)))
                   (plist-get props (intern field-name)))))))
       (supertag-view-table--compare-values actual-value operator expected-value)))
    
    ('title
     (let* ((operator (cadr condition))
            (expected-value (caddr condition))
            (actual-value (plist-get entity-data :title)))
       (supertag-view-table--compare-values actual-value operator expected-value)))
    
    ('tag
     (let* ((operator (cadr condition))
            (expected-tag (caddr condition))
            (tags (plist-get entity-data :tags)))
       (pcase operator
         ('has (member expected-tag tags))
         ('not-has (not (member expected-tag tags)))
         (_ nil))))
    
    (_ nil)))

(defun supertag-view-table--compare-values (actual operator expected)
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

(defun supertag-view-table--select-multiple-tags ()
  "Select multiple tags for multi-table view."
  (let* ((available-tags (supertag-view-table--get-available-tags))
         (selected-tags '()))
    (while (let ((tag (completing-read "Select tag (RET to finish): " available-tags nil t)))
             (when (and tag (not (string-empty-p tag)))
               (push (list :type :tag :value tag) selected-tags)
               t)))
    (nreverse selected-tags)))

(defun supertag-view-table--get-entities (query-obj)
  "Get entity IDs based on QUERY-OBJ configuration."
  (pcase (plist-get query-obj :type)
    (:tag
     (supertag-index-get-nodes-by-tag (plist-get query-obj :value)))
    (:behavior
     (mapcar (lambda (b) (plist-get b :id)) (supertag-database-list-behaviors)))
    (:automation
     (mapcar (lambda (a) (plist-get a :id)) (supertag-database-list-automations)))
    (_
     (error "Unknown query type: %s" (plist-get query-obj :type)))))

(defun supertag-view-table--get-columns (query-obj)
  "Get column configuration based on QUERY-OBJ."
  (pcase (plist-get query-obj :type)
    (:tag
     (supertag-view-table--get-columns-for-tag (plist-get query-obj :value)))
    (:behavior
     '((:name "Name" :key :name :width 30)
       (:name "Trigger" :key :trigger :width 15)
       (:name "Action" :key :action :width 15)
       (:name "Enabled" :key :enabled :width 10 :type :boolean)))
    (:automation
     '((:name "Name" :key :name :width 30)
       (:name "Description" :key :description :width 40)
       (:name "Trigger" :key :trigger :width 15)
       (:name "Enabled" :key :enabled :width 10 :type :boolean)))
    (_
     (supertag-view-table--default-columns))))

(defun supertag-view-table-select-tag ()
  "Interactive table view with tag selection from list.
Prompts user to select a tag from available tags."
  (interactive)
  (let* ((available-tags (supertag-view-table--get-available-tags))
         (selected-tag (completing-read "Select tag: " available-tags nil t)))
    (when selected-tag
      (supertag-view-table selected-tag))))

(defun supertag-view-table--get-available-tags ()
  "Return list of available tag names."
  (let ((tags (supertag-store-get-collection :tags))
        (tag-names '()))
    (when (hash-table-p tags)
      (maphash (lambda (id tag-data)
                 (when (plist-get tag-data :name)
                   (push (plist-get tag-data :name) tag-names)))
               tags))
    (sort tag-names #'string<)))

(defun supertag-tag-get-id-by-name (tag-name)
  "Get tag ID by TAG-NAME."
  (let ((tags (supertag-store-get-collection :tags)))
    (when (hash-table-p tags)
      (catch 'found
        (maphash (lambda (id tag-data)
                   (when (equal (plist-get tag-data :name) tag-name)
                     (throw 'found id)))
                 tags)
        nil))))

(defun supertag-view-table--get-columns-for-tag (tag-name)
  "Get column configuration for TAG-NAME, including custom fields with type information.
Automatically detects virtual databases and uses their database fields."
  (let ((tag-id (supertag-tag-get-id-by-name tag-name)))
    (if (not tag-id)
        (supertag-view-table--default-columns)
      (let* ((fields (supertag-tag-get-all-fields tag-id)) ; Use recursive getter for inherited fields
             (base-columns '((:name "Title" :key :title :width 40)))
             (seen (make-hash-table :test 'equal))
             (field-columns
              (cl-loop for field-def in fields
                       for raw-name = (plist-get field-def :name)
                       for fid = (or (plist-get field-def :id) raw-name)
                       for slug = (and fid (supertag-sanitize-field-id fid))
                       for dedupe-key = (if supertag-use-global-fields slug raw-name)
                       unless (or (null dedupe-key) (gethash dedupe-key seen))
                       do (puthash dedupe-key t seen)
                       collect (let ((col `(:name ,raw-name
                                          :key ,(intern (or slug raw-name))
                                          :field-id ,slug
                                          :width 20)))
                                 ;; Keep the full field definition (type/options etc.)
                                 (append col field-def)))))
        (append base-columns field-columns)))))

(defun supertag-view-table--default-columns ()
  "Return default column configuration."
  '((:name "Title" :key :title :width 40)))

(defun supertag-view-table--render-table (state)
  "Renderer for layout 'table'. Consumes STATE built by `supertag-view-table--build-state'."
  (let* ((inhibit-read-only t)
         (columns (plist-get state :columns))
         (rows (plist-get state :rows))
         (query-obj (supertag-view-table--get-current-query-obj)))
    (erase-buffer)
    ;; Calculate dynamic column widths from data
    (let* ((headers (mapcar (lambda (col) (plist-get col :name)) columns))
           (data-rows (mapcar (lambda (row)
                                (mapcar (lambda (col)
                                          (let* ((key (plist-get col :key))
                                                 (pair (assoc key (plist-get row :values))))
                                            (cdr pair)))
                                        columns))
                              rows))
           (calculated-widths (supertag-view-table--calculate-column-widths headers data-rows))
           (table-info (when query-obj
                         (format "Table: %s (%d/%d)"
                                 (plist-get query-obj :value)
                                 (1+ supertag-view-table--current-table-index)
                                 (length supertag-view-table--query-objs))))
           (view-info (if supertag-view-table--current-view-name
                          (format "View: %s" supertag-view-table--current-view-name)
                        "View: Default"))
           (full-header (string-join (list table-info view-info) " | "))
           ;; Update live columns with widths
           (updated-columns (cl-loop for col in columns
                                     for width in calculated-widths
                                     collect (plist-put (copy-sequence col) :width width))))
      (setq supertag-view-table--columns updated-columns)
      ;; Table info above the table
      (when full-header
        (insert (propertize full-header 'face '(:weight bold :foreground "blue")) "\n\n"))
      ;; Top border
      (insert (supertag-view-table--draw-separator "┌" "┬" "┐") "\n")
      ;; Header row
      (let ((header-parts (cl-loop for col in updated-columns
                                   for width in (mapcar (lambda (c) (plist-get c :width)) updated-columns)
                                   collect (propertize (format " %s " (supertag-view-table--pad-string (plist-get col :name) width))
                                                       'face 'bold))))
        (insert (format "│%s│" (string-join header-parts "│")) "\n"))
      ;; Header separator
      (insert (supertag-view-table--draw-separator "├" "┼" "┤") "\n")
      ;; Body rows
      (let ((processed-rows
             (mapcar (lambda (row)
                       (let* ((eid (plist-get row :id))
                              (widths (mapcar (lambda (c) (plist-get c :width)) updated-columns)))
                         (cl-loop for col in updated-columns
                                  for width in widths
                                  for col-idx from 0
                                  for key = (plist-get col :key)
                                  for val = (cdr (assoc key (plist-get row :values)))
                                  collect (let ((lines (supertag-view-table--format-cell val width)))
                                            (list :entity-id eid :col-index col-idx :col-key key :lines lines)))))
                     rows)))
        (dolist (prow processed-rows)
          (when prow
            (let* ((row-height (apply #'max (cons 1 (mapcar (lambda (cell) (length (plist-get cell :lines))) prow))))
                   (output-lines '()))
              (dotimes (line-idx row-height)
                (let ((line-parts
                       (cl-loop for p-cell in prow
                                collect (let* ((lines (plist-get p-cell :lines))
                                               (content-part (or (nth line-idx lines) ""))
                                               (width (plist-get (nth (plist-get p-cell :col-index) updated-columns) :width))
                                               (padded (supertag-view-table--pad-string content-part width)))
                                          (propertize (format " %s " padded)
                                                      'entity-id (plist-get p-cell :entity-id)
                                                      'col-index (plist-get p-cell :col-index)
                                                      'col-key (plist-get p-cell :col-key))))))
                  (push (format "│%s│" (string-join line-parts "│")) output-lines)))
              (insert (string-join (nreverse output-lines) "\n"))
              ;; Expanded inline details (read-only, no cell props)
              (let ((eid (plist-get (car prow) :entity-id)))
                (when (supertag-view-table--row-expanded-p eid)
                  (insert "\n")
                  (supertag-view-table--insert-expanded-details eid)))
              (unless (eq prow (car (last processed-rows)))
                (insert "\n" (supertag-view-table--draw-separator "├" "┼" "┤") "\n"))))))
      ;; Bottom border
      (insert "\n" (supertag-view-table--draw-separator "└" "┴" "┘"))
      ;; Footer
      (insert "\n\n")
      (supertag-view-helper-insert-simple-footer
       "⌨️ Edit: [RET] Edit Cell | [C-c C-i] Insert Image | [w] Adjust Image Width"
       "📊 Columns: [C-c C-a] Add | [C-c C-d] Delete | [C-c C-r] Rename | [C-c C-t] Set Type"
       "🔍 Filter: [/] Apply Filter | [C-c /] Clear Filter | [C-c v s] Switch View"
       "📍 Navigation: [n/p] Line | [f/b] Cell | [TAB] Next Cell | [t] Switch Table | [o] Goto Node"
       "🔧 Actions: [g] Refresh | [?] Help | [q] Quit")
      ;; Move to first cell
      (supertag-view-table--goto-first-cell)
      (setq buffer-read-only t))))

(defun supertag-view-table--pad-string (text width)
  "Pad TEXT with spaces to fit WIDTH while respecting maximum cell width."
  (let* ((text-str (truncate-string-to-width (format "%s" text) width 0 nil))
         (padding (- width (string-width text-str))))
    (if (> padding 0)
        (concat text-str (make-string padding ?\s))
      text-str)))

;;; --- Additional Layouts: Node Detail ---

(defun supertag-view-table--build-node-detail-state (node-id)
  "Build a minimal state plist for rendering NODE-ID via the node-detail layout.
This uses `supertag-view-build-node-state' to obtain the data-only node view."
  (let ((node-state (supertag-view-build-node-state node-id)))
    (when node-state
      (list :layout 'node-detail
            :node-state node-state))))

(defun supertag-view-table--render-node-detail (state)
  "Render a single node detail card from STATE in the current buffer.
STATE is expected to contain a :node-state plist built by
`supertag-view-build-node-state'."
  (let* ((node-state (plist-get state :node-state))
         (node (plist-get node-state :node))
         (node-id (plist-get node-state :id)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cond
       ;; 优先复用现有的 Node View 渲染函数（如果已加载）。
       ((and node-state (fboundp 'supertag-view-node--render-from-state))
        (supertag-view-node--render-from-state node-state))
       ;; 简单回退：只显示标题和 ID。
       (node
        (let* ((title (or (plist-get node :title) "Untitled Node")))
          (insert (format "📄 %s\n\n" title))
          (insert (format "ID: %s\n" node-id))))
       (t
        (insert "Node not found."))))))

;; Register default table layout renderer + node-detail layout at load time.
(ignore-errors
  (supertag-view-table-register-layout 'table #'supertag-view-table--render-table)
  (supertag-view-table-register-layout 'node-detail #'supertag-view-table--render-node-detail))

(defun supertag-view-table--calculate-column-widths (headers data-rows)
  "Calculate the optimal column widths based on headers and data.
HEADERS is a list of strings for the column titles.
DATA-ROWS is a list of lists, where each inner list represents a row.
Returns a list of integers representing the calculated width for each column."
  (when (or headers data-rows)
    (let* ((num-columns (length headers))
           (min-width 8) ; Minimum width for any column
           (max-width 50) ; Maximum width for any column
           (widths (make-list num-columns 0))
           (current-tag (supertag-view-table--get-current-tag-id))
           (image-width (if current-tag
                            (supertag-view-table--get-image-column-width-for-tag current-tag)
                          supertag-view-table-image-column-width)))

      ;; 1. Calculate initial widths from headers
      (dotimes (i num-columns)
        (setf (nth i widths) (max min-width (string-width (nth i headers)))))

      ;; 2. Expand widths based on data rows
      (dolist (row data-rows)
        (dotimes (i num-columns)
          (when (< i (length row))
            (let* ((cell-content (format "%s" (or (nth i row) "")))
                   (content-width
                    (if (supertag-view-table--is-image-path-p cell-content)
                        image-width
                      (string-width cell-content))))
              (setf (nth i widths) (max (nth i widths) content-width))))))

      ;; 3. Apply max width constraint
      (dotimes (i num-columns)
        (setf (nth i widths) (min max-width (nth i widths))))

      ;; 4. (Future) Adjust total width to fit window size.
      ;; For now, we just return the calculated widths.
      widths)))

(defun supertag-view-table--render-header ()
  "Render the grid header with column names only.
Table information is now displayed separately above the table."
  (let ((header-parts (cl-loop for col in supertag-view-table--columns
                             for width in (mapcar (lambda (c) (plist-get c :width)) supertag-view-table--columns)
                             collect (propertize 
                                      (format " %s " (supertag-view-table--pad-string (plist-get col :name) width))
                                      'face 'bold))))
    (insert (format "│%s│" (string-join header-parts "│")))))

(defun supertag-view-table--draw-separator (&optional left mid right)
  "Draw a horizontal separator line with configurable characters.
LEFT, MID, RIGHT are the characters for left-end, middle, and right-end.
Uses improved styling from old version."
  (let* ((left (or left "├"))
         (mid (or mid "┼"))
         (right (or right "┤"))
         (widths (mapcar (lambda (c) (plist-get c :width)) supertag-view-table--columns))
         (segments (mapcar (lambda (w) (make-string (+ w 2) ?─)) widths)))
    (format "%s%s%s" left (string-join segments mid) right)))

(defun supertag-view-table--get-entity-data (entity-id)
  "Get entity data based on current query type."
  (let ((query-obj (supertag-view-table--get-current-query-obj)))
    (pcase (plist-get query-obj :type)
      (:tag
       (supertag-node-get entity-id))
      (:behavior
       (supertag-database-get-behavior entity-id))
      (:automation
       (supertag-database-get-automation entity-id))
      (:database
       (supertag-database-get-database entity-id))
      (_
       (supertag-store-get-entity (plist-get query-obj :type) entity-id)))))

(defun supertag-view-table--get-cell-value (entity-data key column)
  "Get cell value from ENTITY-DATA for KEY, formatted according to COLUMN type."
  (let ((query-obj (supertag-view-table--get-current-query-obj)))
    (let* ((raw-value
            (pcase (plist-get query-obj :type)
              (:tag
               (pcase key
                 (:title
                  (let* ((raw-title (or (plist-get entity-data :title) "No Title"))
                         (base-title (supertag-view-table--strip-todo-keyword raw-title))
                         ;; Render org-style links inside the title so [[id:...][desc]]
                         ;; shows as clickable `desc` within the cell.
                         (rendered-title (supertag-view-helper-render-org-links base-title))
                         (olp (plist-get entity-data :olp))
                         (parent-title (and (listp olp)
                                            (> (length olp) 1)
                                            ;; OLP is [root ... parent current]
                                            (nth (- (length olp) 2) olp))))
                    (if (and parent-title (not (string-empty-p parent-title)))
                        ;; First line: current title (with org links rendered);
                        ;; second line: parent title (grey background).
                        (concat rendered-title "\n"
                                (propertize parent-title 'face 'supertag-view-table-parent-title-face))
                      rendered-title)))
                 (:file (or (plist-get entity-data :file) "No File"))
                 (:tags (string-join (plist-get entity-data :tags) ", "))
                 (_ (let* ((node-id (plist-get entity-data :id))
                           (tag-id (supertag-view-table--get-current-tag-id))
                           (field-name (symbol-name key)))
                      (when (and node-id tag-id)
                        (supertag-field-get-with-default node-id tag-id field-name))))))
              (_
               (plist-get entity-data key)))))
      ;; For title cells we preserve existing faces and explicit newlines;
      ;; for all other cells we still run the usual formatting and org link rendering.
      (if (and (eq (plist-get query-obj :type) :tag)
               (eq key :title))
          raw-value
        (let ((formatted (supertag-view-table--format-cell-value
                          raw-value (plist-get column :type))))
          (supertag-view-helper-render-org-links formatted))))))

(defun supertag-view-table--format-cell-value (value type)
  "Format VALUE according to TYPE for display in table cells."
  (cond
   ((null value) "")
   ((eq type :date) (supertag-view-table--format-date value))
   ((eq type :timestamp) (supertag-view-table--format-timestamp value))
   ((eq type :options) (supertag-view-table--format-options value))
   ((eq type :boolean) (supertag-view-table--format-boolean value))
   ((eq type :node-reference)
    (if (string-empty-p value)
        ""
      (let* ((node (supertag-node-get value))
             (title (plist-get node :title)))
        (format "[[id:%s][%s]]" value (or title "No Title")))))
   (t (format "%s" value))))

(defun supertag-view-table--format-date (value)
  "Format date VALUE for display."
  (cond
   ((null value) "")
   ((listp value)
    ;; Handle Emacs time format (high low micro pico) or (high low)
    (condition-case nil
        (format-time-string "%Y-%m-%d" value)
      (error (format "%s" value))))
   (t (format "%s" value))))

(defun supertag-view-table--format-timestamp (value)
  "Format timestamp VALUE for display."
  (cond
   ((null value) "")
   ((listp value)
    ;; Handle Emacs time format (high low micro pico) or (high low)
    (condition-case nil
        (format-time-string "%Y-%m-%d %H:%M" value)
      (error (format "%s" value))))
   (t (format "%s" value))))

(defun supertag-view-table--format-options (value)
  "Format options VALUE for display."
  (if (listp value)
      (string-join value " / ")
    (format "%s" value)))

(defun supertag-view-table--format-boolean (value)
  "Format boolean VALUE for display."
  (cond
   ((eq value t) "Yes")
   ((eq value nil) "No")
   ((equal value "true") "Yes")
   ((equal value "false") "No")
   (t (format "%s" value))))

(defun supertag-view-table--wrap-text (text width)
  "Wrap TEXT (possibly propertized) into segments within WIDTH columns."
  (let* ((string (cond
                  ((null text) "")
                  ((stringp text) text)
                  (t (format "%s" text))))
         (len (length string))
         (pos 0)
         (lines '()))
    (while (< pos len)
      (if (eq (aref string pos) ?\n)
          (progn
            (push "" lines)
            (setq pos (1+ pos)))
        (let* ((newline-pos (cl-position ?\n string :start pos))
               (break-pos (if (and newline-pos (<= (- newline-pos pos) width))
                              newline-pos
                            (supertag-view-table--wrap-find-break
                             string pos width (or newline-pos len)))))
          (push (substring string pos (or break-pos len)) lines)
          (setq pos (or break-pos len))
          (when (and (< pos len) (eq (aref string pos) ?\n))
            (setq pos (1+ pos))))))
    (setq lines (nreverse lines))
    (when (null lines)
      (setq lines (list "")))
    lines))

(defun supertag-view-table--wrap-find-break (string start width limit)
  "Find a break position within STRING starting at START.
WIDTH is the desired column width. LIMIT bounds the search (e.g., newline)."
  (let ((pos start)
        (col 0)
        (last-break nil))
    (catch 'wrap-break
      (while (< pos limit)
        (let* ((char (aref string pos))
               (char-width (or (char-width char) 1)))
          (when (> (+ col char-width) width)
            (throw 'wrap-break (or last-break (if (> pos start)
                                                 pos
                                               (min limit (1+ pos))))))
          (setq col (+ col char-width))
          (setq pos (1+ pos))
          (when (or (eq char ?\s) (eq char ?-))
            (setq last-break pos))))
      pos)))

(defun supertag-view-table--format-cell (value width)
  "Format VALUE for a cell with character-WIDTH.
If VALUE is an image path, it's sliced into multiple strings.
If VALUE is text, wrap it into multiple lines within WIDTH and apply org-mode markup rendering."
  (if (supertag-view-table--is-image-path-p value)
      (with-temp-buffer
        (let ((lines
               (if-let* ((source-info (supertag-view-table--get-loadable-image-source value))
                         (target-px-h (* supertag-view-table-image-target-char-height (supertag-view-table--get-exact-line-height)))
                         (image (create-image (car source-info) (cadr source-info) nil
                                              :width (* width (frame-char-width)) :height target-px-h
                                              :ascent 'center)))
                   (progn
                     (insert-sliced-image image " " nil supertag-view-table-image-target-char-height width)
                     (split-string (buffer-string) "\n" t))
                 ;; Fallback if image can't be loaded
                 (list (format "[Image: %s]" (file-name-nondirectory value))))))
          ;; Enforce exact line height on every slice for perfect alignment
          (mapcar (lambda (line) (propertize line 'line-height (supertag-view-table--get-exact-line-height))) lines)))
    ;; For text, wrap into multiple lines (preserving any existing text properties).
    (let* ((text (cond
                  ((stringp value) value)
                  ((null value) "")
                  (t (format "%s" value))))
           ;; Split logical lines on explicit newlines so callers can request
           ;; multi-line cells (e.g., title + parent title).
           (logical-lines (split-string text "\n" nil))
           (wrapped-lines
            (cl-mapcan
             (lambda (line)
               ;; We assume markup (and faces) have already been applied in
               ;; `supertag-view-table--get-cell-value` where needed; here we
               ;; only wrap and pad without touching faces.
               (supertag-view-table--wrap-text line width))
             logical-lines)))
      (mapcar
       (lambda (line)
         (let* ((padded (supertag-view-table--pad-string line width))
                (copy (copy-sequence padded)))
           (add-text-properties 0 (length copy)
                                `(line-height ,(supertag-view-table--get-exact-line-height))
                                copy)
           copy))
       wrapped-lines))))

(defun supertag-view-table--get-exact-line-height ()
  "Get exact line height in pixels, including frame-level line-spacing."
  (let* ((char-height (frame-char-height))
         (buffer-line-spacing (or line-spacing 0))
         (frame-line-spacing (or (frame-parameter nil 'line-spacing) 0))
         (effective-line-spacing (max buffer-line-spacing frame-line-spacing))
         (total-line-height (+ char-height effective-line-spacing)))
    total-line-height))

(defun supertag-view-table--propertize-org-markup (text)
  "Parse a string containing org-mode markup and return a propertized string."
  (if (or (not (stringp text)) (string-empty-p text))
      ""
    (with-temp-buffer
      (org-mode)
      ;; Ensure tab-width is 8 as required by org-current-text-column
      (setq-local tab-width 8)
      ;; Insert text and ensure it's treated as a paragraph
      (insert text "\n\n")
      (goto-char (point-min))
      (font-lock-fontify-region (point-min) (point-max))
      ;; Extract the propertized string, removing the extra newlines
      (buffer-substring (point-min) (- (point-max) 2)))))

(defun supertag-view-table--is-image-path-p (text)
  "Return t if TEXT is a string that points to an existing image file."
  (and (stringp text)
       (file-exists-p text)
       (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|svg\\|webp\\|bmp\\)$"
                       (downcase text))))


(defun supertag-view-table--get-loadable-image-source (file)
  "Find a loadable image source.
Enhanced with old version's robust image processing including format conversion and error handling.
Returns a list '(FILE-PATH TYPE)' on success, nil on failure."
  ;; First check file existence and readability (from old version)
  (if (not (and (stringp file) (file-exists-p file)))
      (progn
        (message "!!! ERROR: File does not exist or is not accessible: %s" file)
        nil)
    ;; Process file path encoding issues (particularly Chinese filenames)
    (let* ((normalized-file (expand-file-name file))
           (original-type (image-type-from-file-name normalized-file)))
      
      ;; Debug information from old version
      (message "DEBUG: Processing image file: %s" normalized-file)
      (message "DEBUG: Detected image type: %s" original-type)
      
      (cond
       ;; Strategy 1: Check if Emacs natively supports this image type
       ((and original-type (image-type-available-p original-type))
        (message "DEBUG: Using native image support for type: %s" original-type)
        (list normalized-file original-type))
       
       ;; Strategy 2: If type detection fails, try extension-based inference
       ((null original-type)
        (message "DEBUG: Image type detection failed, trying extension-based detection")
        (let ((extension (downcase (file-name-extension normalized-file))))
          (cond
           ((member extension '("jpg" "jpeg"))
            (if (image-type-available-p 'jpeg)
                (progn
                  (message "DEBUG: Using JPEG based on file extension")
                  (list normalized-file 'jpeg))
              (message "DEBUG: JPEG not available, will try conversion")
              nil))
           ((member extension '("png"))
            (if (image-type-available-p 'png)
                (progn
                  (message "DEBUG: Using PNG based on file extension")
                  (list normalized-file 'png))
              (message "DEBUG: PNG not available, will try conversion")
              nil))
           ((member extension '("gif"))
            (if (image-type-available-p 'gif)
                (progn
                  (message "DEBUG: Using GIF based on file extension")
                  (list normalized-file 'gif))
              (message "DEBUG: GIF not available, will try conversion")
              nil))
           (t
            (message "DEBUG: Unknown file extension: %s" extension)
            nil))))
       
       ;; Strategy 3: If not supported and on macOS, call sips for conversion
       ((eq system-type 'darwin)
        (message "DEBUG: Image type '%s' not directly supported, trying conversion with 'sips'..." original-type)
        (let* ((temp-file (make-temp-file "supertag-img-" nil ".png"))
               (exit-code (call-process "sips" nil nil nil "-s" "format" "png" normalized-file "--out" temp-file)))
          (if (and (zerop exit-code) (file-exists-p temp-file))
              (progn
                (message "DEBUG: External conversion to PNG successful: %s" temp-file)
                (list temp-file 'png))
            (progn
              (message "!!! ERROR: External conversion (sips) failed for %s (exit code: %s)" normalized-file exit-code)
              nil))))
       
       ;; Strategy 4: On other systems, if not supported then fail directly
       (t
        (message "!!! ERROR: Image type '%s' not supported and no conversion available on this system" original-type)
        nil)))))

(defun supertag-view-table--get-image-column-width-for-tag (tag)
  "Get image column width settings for a specific TAG."
  (or (cdr (assoc tag supertag-view-table-per-tag-image-widths))
      supertag-view-table-image-column-width))

(defun supertag-view-table--set-image-column-width-for-tag (tag width)
  "Set image column width for a specific TAG."
  (let ((existing (assoc tag supertag-view-table-per-tag-image-widths)))
    (if existing
        (setcdr existing width)
      (push (cons tag width) supertag-view-table-per-tag-image-widths))
    (customize-save-variable 'supertag-view-table-per-tag-image-widths 
                            supertag-view-table-per-tag-image-widths)))

(defun supertag-view-table--adjust-image-column-width ()
  "Interactively adjust the image column width for the current tag and save the setting."
  (interactive)
  (let* ((query-obj (supertag-view-table--get-current-query-obj))
         (current-tag (plist-get query-obj :value))
         (current-width (supertag-view-table--get-image-column-width-for-tag current-tag))
         (new-width (read-number (format "Image column width (tag: %s, current: %d characters): " 
                                        current-tag current-width) 
                                current-width)))
    (supertag-view-table--set-image-column-width-for-tag current-tag new-width)
    (supertag-view-table-refresh)
    (message "Image column width for tag '%s' set to %d characters and saved" current-tag new-width)))

(defun supertag-view-table--insert-image-path ()
  "Insert an image path into the current cell."
  (interactive)
  (let ((coords (supertag-view-table--get-cell-coords)))
    (when coords
      (let* ((entity-id (plist-get coords :entity-id))
             (col-key (plist-get coords :col-key))
             (query-obj (supertag-view-table--get-current-query-obj))
             (image-file (read-file-name "Select image: " nil nil t nil
                                        (lambda (f) 
                                          (and (file-exists-p f)
                                               (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|svg\\|webp\\|bmp\\)$" f))))))
        (when image-file
          (pcase (plist-get query-obj :type)
            (:tag
             (supertag-field-set entity-id 
                                (supertag-view-table--get-current-tag-id) 
                                (symbol-name col-key) 
                                image-file))
            (_
             (let* ((entity-data (supertag-view-table--get-entity-data entity-id))
                    (current-value (plist-get entity-data col-key)))
               (supertag-database-update (plist-get query-obj :type) entity-id
                                        (lambda (data) (plist-put data col-key image-file))))))
          (message "Image path inserted: %s" (file-name-nondirectory image-file)))))))

;;; --- Reactive Update System ---

(defun supertag-view-table--subscribe-updates ()
  "Subscribe to entity update events for reactive updates."
  (let ((view-id (format "%s" (current-buffer))))
    ;; Store view reference
    (puthash view-id (current-buffer) supertag-view-table--active-views)
    
    ;; Subscribe to entity updates
    (supertag-subscribe :node-updated
                       (lambda (path old-value new-value)
                         (supertag-view-table--handle-entity-update path old-value new-value view-id)))
    (supertag-subscribe :database-updated
                       (lambda (path old-value new-value)
                         (supertag-view-table--handle-entity-update path old-value new-value view-id)))))

(defun supertag-view-table--handle-entity-update (path old-value new-value view-id)
  "Handle entity update event for reactive grid updates."
  (when (and (listp path) (memq (car path) '(:nodes :databases)))
    (let ((entity-id (cadr path))
          (buf (gethash view-id supertag-view-table--active-views)))
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (when (member entity-id supertag-view-table--entity-ids)
            (supertag-view-table--update-cell entity-id)))))))

(defun supertag-view-table--update-cell (entity-id)
  "Update the display of a single cell for ENTITY-ID."
  (let ((inhibit-read-only t)
        (line-number (supertag-view-table--find-row-line entity-id)))
    (when line-number
      (save-excursion
        (goto-line line-number)
        (beginning-of-line)
        (let ((start (point))
              (end (line-end-position)))
          (delete-region start end)
          ;; Re-render the entire grid via new dispatcher
          (let ((state (supertag-view-table--build-state)))
            (supertag-view-table-render supertag-view-table-default-layout state)
            ))
          
          ;; Visual feedback for automated updates
          (supertag-view-table--flash-cell line-number)))))

(defun supertag-view-table--find-row-line (entity-id)
  "Find the line number for the row containing ENTITY-ID."
  (save-excursion
    (goto-char (point-min))
    (let ((current-line 1))
      (while (not (eobp))
        (when (get-text-property (point) 'entity-id entity-id)
          (return current-line))
        (forward-line)
        (cl-incf current-line))
      nil)))

(defun supertag-view-table--flash-cell (line-number)
  "Provide visual feedback for updated cells."
  (save-excursion
    (goto-line line-number)
    (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
      (overlay-put ov 'face 'highlight)
      (run-with-timer 0.5 nil (lambda () (delete-overlay ov))))))

(defun supertag-view-table--get-cell-coords-robust ()
  "Get the logical coordinates of the cell at the current point.
Robustly searches the current line if point is not directly on cell text.
Based on old version's superior navigation system."
  (let ((props-at-point (get-text-property (point) 'entity-id)))
    (if props-at-point
        ;; Fast path: Point is directly on cell text
        (let ((col-index (get-text-property (point) 'col-index))
              (col-key (get-text-property (point) 'col-key)))
          (when (and col-index col-key)
            (list :entity-id props-at-point
                  :col-index col-index
                  :col-key col-key)))
      ;; Slow path: Point is on a border or padding. Search the line.
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((next-cell-pos (next-single-property-change (point) 'entity-id nil (line-end-position))))
          (when next-cell-pos
            (goto-char next-cell-pos)
            (let ((col-index (get-text-property (point) 'col-index))
                  (col-key (get-text-property (point) 'col-key)))
              (when (and col-index col-key)
                (list :entity-id (get-text-property (point) 'entity-id)
                      :col-index col-index
                      :col-key col-key)))))))))

(defun supertag-view-table-next-cell ()
  "Move to next cell in the table."
  (interactive)
  (let ((next-pos (next-single-property-change (point) 'entity-id)))
    (when next-pos
      (goto-char next-pos)
      (unless (get-text-property (point) 'entity-id)
        (supertag-view-table-next-cell))
      )))

(defun supertag-view-table-previous-cell ()
  "Move to previous cell in the table."
  (interactive)
  (let ((prev-pos (previous-single-property-change (point) 'entity-id)))
    (when prev-pos
      (goto-char prev-pos)
      (unless (get-text-property (point) 'entity-id)
        (supertag-view-table-previous-cell))
      )))

(defun supertag-view-table--goto-row-relative (delta)
  "Move vertically by DELTA rows while staying in the same column."
  (let ((coords (supertag-view-table--get-cell-coords)))
    (if (null coords)
        (message "No cell at point; move onto a table cell first.")
      (let* ((entity-id (plist-get coords :entity-id))
             (col-index (plist-get coords :col-index))
             (target-index (when entity-id
                             (cl-position entity-id supertag-view-table--entity-ids :test #'equal))))
        (if (null target-index)
            (message "Unable to determine current row.")
          (let ((new-index (+ target-index delta)))
            (if (or (< new-index 0)
                    (>= new-index (length supertag-view-table--entity-ids)))
                (message "No more rows in that direction.")
              (let ((target-entity (nth new-index supertag-view-table--entity-ids)))
                (supertag-view-table--goto-cell
                 (list :entity-id target-entity :col-index col-index))
                ))))))))

(defun supertag-view-table-next-line ()
  "Move to the cell in the next data row, attempting to preserve column."
  (interactive)
  (supertag-view-table--goto-row-relative 1))

(defun supertag-view-table-previous-line ()
  "Move to the cell in the previous data row, attempting to preserve column."
  (interactive)
  (supertag-view-table--goto-row-relative -1))

;;; --- User Interaction ---

(defun supertag-view-table-add-column ()
  "Add a new column (field) to the current tag's schema."
  (interactive)
  (let ((tag-id (supertag-view-table--get-current-tag-id)))
    (when tag-id
      (when-let* ((field-def (supertag-ui-create-field-definition))) ; Call with no arguments
        (supertag-tag-add-field tag-id field-def)
        (supertag-view-table-refresh)
        (message "Column '%s' added to tag '%s'." (plist-get field-def :name) tag-id)))))

(defun supertag-view-table-delete-column ()
  "Delete the column at point from the current tag's schema."
  (interactive)
  (let* ((coords (supertag-view-table--get-cell-coords))
         (col-key (plist-get coords :col-key))
         (col-name (symbol-name col-key))
         (tag-id (supertag-view-table--get-current-tag-id)))
    (when (and tag-id col-key (not (member col-key '(:title)))) ; Protect primary column
      (when (yes-or-no-p (format "Really delete column '%s' and all its data? " col-name))
        (supertag-tag-remove-field tag-id col-name)
        (supertag-view-table-refresh)
        (message "Column '%s' deleted." col-name)))))

(defun supertag-view-table-rename-column ()
  "Rename the column at point."
  (interactive)
  (let* ((coords (supertag-view-table--get-cell-coords))
         (col-key (plist-get coords :col-key))
         (old-name (symbol-name col-key))
         (tag-id (supertag-view-table--get-current-tag-id)))
    (when (and tag-id col-key (not (member col-key '(:title))))
      (let ((new-name (read-string (format "New name for '%s': " old-name) old-name)))
        (when (and (not (string-empty-p new-name)) (not (equal old-name new-name)))
          (supertag-tag-rename-field tag-id old-name new-name)
          (supertag-view-table-refresh)
          (message "Column '%s' renamed to '%s'." old-name new-name))))))

(defun supertag-view-table-set-column-type ()
  "Set the type of the column at point."
  (interactive)
  (let* ((coords (supertag-view-table--get-cell-coords))
         (col-key (plist-get coords :col-key))
         (col-name (symbol-name col-key))
         (tag-id (supertag-view-table--get-current-tag-id)))
    (when (and tag-id col-key (not (member col-key '(:title))))
      (let* ((field-def (supertag-tag-get-field tag-id col-name))
             (type-str (completing-read (format "New type for '%s': " col-name)
                                        (mapcar (lambda (sym) (substring (symbol-name sym) 1))
                                                supertag-field-types)
                                        nil t))
             (new-type (when (and type-str (not (string-empty-p type-str)))
                         (let ((clean (string-trim type-str)))
                           (when (string-prefix-p ":" clean)
                             (setq clean (substring clean 1)))
                           (when (not (string-empty-p clean))
                             (intern (concat ":" clean)))))))
        (when (and new-type field-def)
          (let ((new-field-def (plist-put field-def :type new-type)))
            ;; Handle options type specifically
            (when (eq new-type :options)
              (let* ((options-str (read-string "Enter options (comma-separated): "))
                     (options (split-string options-str "," t " ")))
                (setq new-field-def (plist-put new-field-def :options options))))
            ;; For non-options types, we don't need to remove :options
            ;; The system handles nil options correctly
            (supertag-tag-add-field tag-id new-field-def)
            (supertag-view-table-refresh)
            (message "Column '%s' type set to '%s'." col-name new-type)))))))

(defun supertag-view-table--goto-first-cell ()
  "Move point to the first data cell in the table.
If no data cells exist, moves to the beginning of the buffer."
  (let ((target-pos
         (save-excursion
           (goto-char (point-min))
           (let (pos)
             (while (and (not pos) (not (eobp)))
               (when (get-text-property (point) 'entity-id)
                 (setq pos (point)))
               (unless pos (forward-char 1)))
             pos))))
    (if target-pos
        (goto-char target-pos)
      (goto-char (point-min)))))

(defun supertag-view-table--goto-cell (coords)
  "Move point to the cell specified by COORDS.
COORDS is a plist with :entity-id and :col-index."
  (when coords
    (let ((entity-id (plist-get coords :entity-id))
          (col-index (plist-get coords :col-index))
          (target-pos nil))
      (save-excursion
        (goto-char (point-min))
        (while (and (not target-pos) (not (eobp)))
          (when (and (equal (get-text-property (point) 'entity-id) entity-id)
                     (eq (get-text-property (point) 'col-index) col-index))
            (setq target-pos (point)))
          (unless target-pos (forward-char 1))))
      (when target-pos
        (goto-char target-pos)))))

(defun supertag-view-table-edit-cell ()
  "Edit the current cell's value with type-specific input."
  (interactive)
  (let ((coords (supertag-view-table--get-cell-coords)))
    (if (null coords)
        (message "No cell found at point. Please click on a table cell.")
      (when-let* ((entity-id   (plist-get coords :entity-id))
                  (col-key     (plist-get coords :col-key))
                  (col-def     (plist-get coords :col-def))
                  (query-obj   (supertag-view-table--get-current-query-obj)))
        (pcase (plist-get query-obj :type)
          (:tag
           (if (eq col-key :title)
               (message "Read-Only")
             (let* ((tag-id (supertag-view-table--get-current-tag-id))
                    (current-value (supertag-field-get-with-default entity-id
                                                                    tag-id
                                                                    (symbol-name col-key)))
                    (new-value (supertag-ui-read-field-value col-def current-value))
                    (field-type (plist-get col-def :type)))
               (when new-value
                 ;; For :node-reference fields, keep :reference relations and backlinks in sync.
                 (when (eq field-type :node-reference)
                   (let* ((current-targets (supertag-field-normalize-node-reference-list current-value))
                          (new-targets (supertag-field-normalize-node-reference-list new-value))
                          (removed (cl-set-difference current-targets new-targets :test #'string=))
                          (added (cl-set-difference new-targets current-targets :test #'string=)))
                     ;; Remove stale references and backlinks.
                     (dolist (target removed)
                       (dolist (rel (supertag-relation-find-between entity-id target :reference))
                         (supertag-relation-delete (plist-get rel :id)))
                       (supertag-ui--remove-link-under-node target entity-id))
                     ;; Add new references via unified service.
                     (dolist (target added)
                       (supertag-relation-add-reference entity-id target))))
                 ;; Persist field value and refresh view.
                 (supertag-field-set entity-id tag-id (symbol-name col-key) new-value)
                 (supertag-view-table-refresh)
                 (supertag-view-table--goto-cell coords)))))
          (_
           (let* ((entity-data (supertag-view-table--get-entity-data entity-id))
                  (current-value (plist-get entity-data col-key))
                  (new-value (supertag-ui-read-field-value col-def current-value)))
             (when new-value
               (supertag-database-update (plist-get query-obj :type) entity-id
                                         (lambda (data) (plist-put data col-key new-value)))
               (supertag-view-table-refresh)
               (supertag-view-table--goto-cell coords)))))))))

(defun supertag-view-table-goto-node ()
  "Jump to the Org node corresponding to the current row.
Opens the file in a new window on the right and moves cursor to the node."
  (interactive)
  (let ((coords (supertag-view-table--get-cell-coords)))
    (if (null coords)
        (message "No cell found at point. Please move to a table cell first.")
      (let ((entity-id (plist-get coords :entity-id)))
        (if (null entity-id)
            (message "No entity ID found in cell coordinates.")
          (when-let* ((node (supertag-node-get entity-id))
                      (file (plist-get node :file)))
            (if (not (file-exists-p file))
                (message "Error: File for node %s does not exist." entity-id)
              ;; Split window to the right if not already split
              (unless (window-in-direction 'right)
                (split-window-right))
              ;; Select the right window
              (let ((target-window (window-in-direction 'right)))
                (select-window target-window)
                ;; Open the file in the selected window
                (find-file file)
                ;; Find and jump to the node
                (goto-char (point-min))
                (if (re-search-forward (format ":ID:[ \t]+%s" (regexp-quote entity-id)) nil t)
                    (progn
                      (org-back-to-heading t)
                      (org-show-context)
                      (recenter)
                      (message "Jumped to node: %s" (or (plist-get node :title) entity-id)))
                  (message "Error: Could not find ID %s in file %s" entity-id file))))))))))

(defun supertag-view-table--get-current-query-obj ()
  "Get current query object."
  (when supertag-view-table--query-objs
    (nth supertag-view-table--current-table-index supertag-view-table--query-objs)))

(defun supertag-view-table--get-current-tag-id ()
  "Get current tag ID from query object."
  (when-let* ((query-obj (supertag-view-table--get-current-query-obj)))
    (plist-get query-obj :value)))

(defun supertag-view-table--get-cell-coords ()
  "Get coordinates of the cell at point using text properties.
Uses robust coordinate detection from old version."
  (let ((coords (supertag-view-table--get-cell-coords-robust)))
    (when coords
      (let* ((col-index (plist-get coords :col-index))
             (col-key (plist-get coords :col-key))
             (valid-index (and col-index (numberp col-index) 
                              (< col-index (length supertag-view-table--columns))))
             (col-def (when valid-index
                       (nth col-index supertag-view-table--columns))))
        ;; Debug: Check if we have valid coordinates
        (unless (and col-index col-key)
          (message "Debug: Missing col-index (%s) or col-key (%s)" col-index col-key))
        (when (and col-index col-key)
          (list :entity-id (plist-get coords :entity-id)
                :col-key col-key
                :col-type (when col-def (plist-get col-def :type))
                :col-index col-index
                :col-def col-def))))))

(defun supertag-view-table-switch-table (&optional index)
  "Switch to another table in multi-table view.
With prefix argument INDEX, switch to specific table number."
  (interactive "P")
  (when supertag-view-table--query-objs
    (let* ((num-tables (length supertag-view-table--query-objs))
           (new-index (if index
                         (min (max (1- (prefix-numeric-value index)) 0) (1- num-tables))
                       (mod (1+ supertag-view-table--current-table-index) num-tables))))
      
      (setq-local supertag-view-table--current-table-index new-index)
      (setq-local supertag-view-table--entity-ids 
                 (supertag-view-table--get-entities (supertag-view-table--get-current-query-obj)))
      (setq-local supertag-view-table--columns 
                 (supertag-view-table--get-columns (supertag-view-table--get-current-query-obj)))
      
      (let ((state (supertag-view-table--build-state)))
        (supertag-view-table-render supertag-view-table-default-layout state)
        ))
      
      (message "Switched to table: %s" 
               (plist-get (supertag-view-table--get-current-query-obj) :value))))

(defun supertag-view-table-show-tag ()
  "Switch the current view to display a tag."
  (interactive)
  (let ((tag-name (completing-read "View table for tag: " (supertag-view-table--get-available-tags) nil t)))
    (when (and tag-name (not (string-empty-p tag-name)))
      (let ((query-obj (list :type :tag :value tag-name)))
        (setq-local supertag-view-table--query-objs (list query-obj))
        (setq-local supertag-view-table--current-table-index 0)
        (rename-buffer (format "*Supertag Table: %s*" tag-name))
        (supertag-view-table-refresh)
        (message "Switched view to tag: %s" tag-name)))))

(defun supertag-view-table-switch-view ()
  "Switch to a named view."
  (interactive)
  (when (or supertag-view-table--named-views
            supertag-view-table--current-view-name)
    (let* ((view-names (mapcar #'car supertag-view-table--named-views))
           (all-options (cons "Default" view-names))
           (chosen-view (completing-read "Switch to view: " all-options nil t)))
      (when (and chosen-view (not (string-empty-p chosen-view)))
        (setq-local supertag-view-table--current-view-name (if (equal chosen-view "Default")
                                                               nil
                                                             chosen-view))
        ;; When switching, the ad-hoc view-config should be cleared
        ;; and replaced by the named view's config.
        (setq-local supertag-view-table--view-config
                    (if supertag-view-table--current-view-name
                        (cdr (assoc supertag-view-table--current-view-name supertag-view-table--named-views))
                      nil))
        (supertag-view-table-refresh)
        (message "Switched to view: %s" chosen-view)))))

(defun supertag-view-table-save-current-view-as-named ()
  "Save the current view's configuration (filters, sort) as a new named view."
  (interactive)
  (unless supertag-view-table--view-config
    (user-error "No active view configuration to save. Apply a filter or sort first."))
  (let ((new-name (read-string "Save current view as: ")))
    (when (and new-name (not (string-empty-p new-name)))
      (if (assoc new-name supertag-view-table--named-views)
          (when (yes-or-no-p (format "View '%s' already exists. Overwrite?" new-name))
            (setf (cdr (assoc new-name supertag-view-table--named-views)) supertag-view-table--view-config))
        (push (cons new-name supertag-view-table--view-config) supertag-view-table--named-views))
      (setq-local supertag-view-table--current-view-name new-name)
      (message "View '%s' saved." new-name)
      (let ((state (supertag-view-table--build-state)))
        (supertag-view-table-render supertag-view-table-default-layout state)
        )))) 

(defun supertag-view-table-delete-named-view ()
  "Delete a named view."
  (interactive)
  (if (null supertag-view-table--named-views)
      (message "No named views to delete.")
    (let* ((view-names (mapcar #'car supertag-view-table--named-views))
           (chosen-view (completing-read "Delete view: " view-names nil t)))
      (when (and chosen-view (assoc chosen-view supertag-view-table--named-views))
        (when (yes-or-no-p (format "Really delete view '%s'?" chosen-view))
          (setq-local supertag-view-table--named-views (delq (assoc chosen-view supertag-view-table--named-views)
                                                             supertag-view-table--named-views))
          (when (equal supertag-view-table--current-view-name chosen-view)
            (setq-local supertag-view-table--current-view-name nil)
            (setq-local supertag-view-table--view-config nil))
          (supertag-view-table-refresh)
          (message "View '%s' deleted." chosen-view))))))

(defun supertag-view-table-filter ()
  "Interactively build and apply a filter to the current view."
  (interactive)
  (let ((conditions '())
        (done nil))
    (while (not done)
      (let* ((field (supertag-view-table--read-filter-field))
             (operator (when field (supertag-view-table--read-filter-operator field)))
             (value (when operator (supertag-view-table--read-filter-value field operator))))
        (if (and field operator)
            (progn
              (push (supertag-view-table--build-condition field operator value) conditions)
              (unless (yes-or-no-p "Add another filter condition?")
                (setq done t)))
          (setq done t))))

    (when conditions
      (let* ((filter-expression (if (> (length conditions) 1)
                                   (cons 'and (nreverse conditions))
                                 (car conditions))))
        ;; Merge with existing sort/group config if any
        (setq-local supertag-view-table--view-config
                    (copy-sequence (plist-put (or supertag-view-table--view-config '()) :filter filter-expression)))
        ;; Clear the current named view since we are now using an ad-hoc filter
        (setq-local supertag-view-table--current-view-name "Unsaved Filter")
        (supertag-view-table-refresh)
        (message "Filter applied.")))))

(defun supertag-view-table--read-filter-field ()
  "Interactively read a field to filter on."
  (let* ((column-keys (mapcar (lambda (c) (symbol-name (plist-get c :key))) supertag-view-table--columns))
         (special-keys '("tag"))
         (all-fields (sort (append special-keys column-keys) #'string<)))
    (completing-read "Filter by field: " all-fields nil t)))

(defun supertag-view-table--read-filter-operator (field)
  "Interactively read a filter operator for a given FIELD."
  (let* ((field-key (intern (format ":%s" field)))
         (col-def (cl-find field-key supertag-view-table--columns :key (lambda (c) (plist-get c :key))))
         (col-type (plist-get col-def :type))
         (operators (pcase col-type
                      ((or :number :date :timestamp) '("=" "!=" ">" "<" ">=" "<="))
                      (_ '("=" "!=" "contains" "starts-with" "ends-with")))))
    (when (equal field "tag")
      (setq operators '("has" "not-has")))
    (completing-read "Operator: " (append operators '("empty" "not-empty")) nil t)))

(defun supertag-view-table--read-filter-value (field operator)
  "Interactively read a filter value."
  (if (memq (intern operator) '(empty not-empty))
      nil
    (read-string (format "Value for %s %s: " field operator))))

(defun supertag-view-table--build-condition (field operator value)
  "Build a single filter condition expression."
  (let ((op-sym (intern operator)))
    (pcase field
      ("tag" `(tag ,op-sym ,value))
      ("title" `(title ,op-sym ,value))
      (_ `(field ,(format ":%s" field) ,op-sym ,value)))))

(defun supertag-view-table-clear-filter ()
  "Clear the currently applied filter."
  (interactive)
  (setq-local supertag-view-table--view-config (plist-put supertag-view-table--view-config :filter nil))
  (setq-local supertag-view-table--current-view-name nil)
  (supertag-view-table-refresh)
  (message "Filter cleared."))

(defun supertag-view-table-show-behaviors ()
  "Switch the current view to display all behaviors."
  (interactive)
  (let ((query-obj (list :type :behavior :value "*all-behaviors*")))
    (setq-local supertag-view-table--query-objs (list query-obj))
    (setq-local supertag-view-table--current-table-index 0)
    (rename-buffer (format "*Supertag Table: %s*" "Behaviors"))
    (supertag-view-table-refresh)
    (message "Switched view to Behaviors.")))

(defun supertag-view-table-show-automations ()
  "Switch the current view to display all automations."
  (interactive)
  (let ((query-obj (list :type :automation :value "*all-automations*")))
    (setq-local supertag-view-table--query-objs (list query-obj))
    (setq-local supertag-view-table--current-table-index 0)
    (rename-buffer (format "*Supertag Table: %s*" "Automations"))
    (supertag-view-table-refresh)
    (message "Switched view to Automations.")))

(defun supertag-view-table-add-table ()
  "Add a new tag's table to the current multi-table view."
  (interactive)
  (when (not supertag-view-table--query-objs)
    (user-error "This command is only for an active table view."))

  ;; 1. Get available tags and filter out existing ones.
  (let* ((all-tags (supertag-view-table--get-available-tags))
         (existing-tags (mapcar (lambda (obj) (plist-get obj :value)) supertag-view-table--query-objs))
         (available-to-add (cl-remove-if (lambda (tag) (member tag existing-tags)) all-tags))
         (new-tag-name (completing-read "Add table for tag: " available-to-add nil t)))

    (when (and new-tag-name (not (string-empty-p new-tag-name)))
      ;; 2. Create new query object and append it.
      (let ((new-query-obj (list :type :tag :value new-tag-name)))
        (setq-local supertag-view-table--query-objs (append supertag-view-table--query-objs (list new-query-obj))))

      ;; 3. Refresh and switch to the new table.
      (let ((new-table-index (1- (length supertag-view-table--query-objs))))
        (supertag-view-table-switch-table (1+ new-table-index)))

      (message "Table for tag '%s' added." new-tag-name))))

;;; --- Mode Definition ---

(defvar supertag-view-table-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supertag-view-table-edit-cell)
    ;; Toggle inline details for current row
    (define-key map (kbd "z") #'supertag-view-table-toggle-row-details)
    (define-key map (kbd "C-c v e") #'supertag-view-table-expand-all-rows)
    (define-key map (kbd "C-c v c") #'supertag-view-table-collapse-all-rows)
    
    
    (define-key map (kbd "g") #'supertag-view-table-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "n") #'supertag-view-table-next-line)
    (define-key map (kbd "p") #'supertag-view-table-previous-line)
    (define-key map (kbd "f") #'supertag-view-table-next-cell)
    (define-key map (kbd "b") #'supertag-view-table-previous-cell)
    (define-key map (kbd "<") #'beginning-of-buffer)
    (define-key map (kbd ">") #'end-of-buffer)
    (define-key map (kbd "TAB") #'supertag-view-table-next-cell)
    (define-key map (kbd "<backtab>") #'supertag-view-table-previous-cell)
    (define-key map (kbd "t") #'supertag-view-table-switch-table)
    (define-key map (kbd "A") #'supertag-view-table-add-table)
    ;; View switching commands
    (define-key map (kbd "C-c v t") #'supertag-view-table-show-tag)
    ;; Named View Management
    (define-key map (kbd "C-c v s") #'supertag-view-table-switch-view)
    (define-key map (kbd "C-c v S") #'supertag-view-table-save-current-view-as-named)
    (define-key map (kbd "C-c v d") #'supertag-view-table-delete-named-view)
    ;; Filtering
    (define-key map (kbd "/") #'supertag-view-table-filter)
    (define-key map (kbd "C-c /") #'supertag-view-table-clear-filter)
    (define-key map (kbd "C-c v b") #'supertag-view-table-show-behaviors)
    (define-key map (kbd "C-c v a") #'supertag-view-table-show-automations)
    ;; Image-related commands
    (define-key map (kbd "w") #'supertag-view-table--adjust-image-column-width)
    (define-key map (kbd "C-c C-i") #'supertag-view-table--insert-image-path)
    ;; Schema management commands
    (define-key map (kbd "C-c C-a") #'supertag-view-table-add-column)
    (define-key map (kbd "C-c C-d") #'supertag-view-table-delete-column)
    (define-key map (kbd "C-c C-r") #'supertag-view-table-rename-column)
    (define-key map (kbd "C-c C-t") #'supertag-view-table-set-column-type)
    (define-key map (kbd "o") #'supertag-view-table-goto-node)
    ;; Help
    (define-key map (kbd "?") #'supertag-view-table-help)
    map)
  "Keymap for supertag-view-table-mode.")

(define-derived-mode supertag-view-table-mode special-mode "Supertag-Grid"
  "Major mode for Supertag grid views."
  (setq buffer-read-only t)
  ;; Critical: Set line-spacing to 0 to ensure image slices align perfectly,
  ;; recreating the "perfect display environment" from the old version.
  (setq-local line-spacing 0)
  ;; Prevent line wrapping to keep table structure intact
  (setq-local truncate-lines t)
  ;; Even in narrow split windows we want truncation instead of wrapping
  (setq-local truncate-partial-width-windows nil)
  ;; Enable horizontal scrolling
  (setq-local auto-hscroll-mode t)
  ;; Disable word wrap
  (setq-local word-wrap nil)
  ;; Ensure consistent line height
  (setq-local line-height-factor 1.0)
  ;; Disable visual line mode if active
  (when (bound-and-true-p visual-line-mode)
    (visual-line-mode -1)))

(defun supertag-view-table-refresh ()
  "Refresh the grid view with current data."
  (interactive)
  (when supertag-view-table--query-objs
    (let* ((current-query (supertag-view-table--get-current-query-obj))
           ;; 1. Re-fetch base entities and columns
           (_ (setq-local supertag-view-table--entity-ids (supertag-view-table--get-entities current-query)))
           (_ (setq-local supertag-view-table--columns (supertag-view-table--get-columns current-query)))
           ;; 2. Get active view config
           ;; If current-view-name is "Unsaved Filter", use view-config directly
           ;; Otherwise, try to find named view, falling back to view-config
           (active-config (if (and supertag-view-table--current-view-name
                                   (not (equal supertag-view-table--current-view-name "Unsaved Filter")))
                              (or (cdr (assoc supertag-view-table--current-view-name supertag-view-table--named-views))
                                  supertag-view-table--view-config)
                            supertag-view-table--view-config)))

      ;; 3. Apply view config to the freshly fetched entities
      (when active-config
        (supertag-view-table--apply-view-config active-config))

      ;; 4. Re-render with the (potentially filtered) entities using dispatcher
      (let ((state (supertag-view-table--build-state)))
        (supertag-view-table-render supertag-view-table-default-layout state)
        ))
      (message "Table refreshed.")))

(defun supertag-view-table-help ()
  "Show help information about table view commands and image support."
  (interactive)
  (with-output-to-temp-buffer "*Supertag Table Help*"
    (princ "=== Supertag Table View Commands ===\n\n")
    (princ "Navigation:\n")
    (princ "  n, p        - Next/previous line\n")
    (princ "  f, b        - Next/previous cell\n")
    (princ "  TAB         - Next cell\n")
    (princ "  <backtab>   - Previous cell\n")
    (princ "  <, >        - Beginning/end of buffer\n\n")
    (princ "Editing:\n")
    (princ "  RET         - Edit current cell\n")
    (princ "  C-c C-i     - Insert image into current cell\n\n")
    (princ "View Management:\n")
    (princ "  g           - Refresh view\n")
    (princ "  t           - Switch between tables (multi-table view)\n")
    (princ "  w           - Adjust image column width\n")
    (princ "  q           - Quit window\n\n")
    (princ "Filtering:\n")
    (princ "  /           - Interactively build and apply a filter\n")
    (princ "  C-c /       - Clear the current filter\n\n")
    (princ "Named Views:\n")
    (princ "  C-c v s     - Switch to a named view\n")
    (princ "  C-c v S     - Save current view as named view\n")
    (princ "  C-c v d     - Delete a named view\n\n")
    (princ "Field Management:\n")
    (princ "  C-c C-a     - Add new field/column\n")
    (princ "  C-c C-d     - Delete field/column\n")
    (princ "  C-c C-r     - Rename field/column\n")
    (princ "  C-c C-t     - Set field/column type\n\n")
    (princ "=== Image Support ===\n\n")
    (princ "The table view supports image rendering:\n")
    (princ "- Simply enter the full path to an image file in any cell\n")
    (princ "- Supported formats: PNG, JPEG, GIF, SVG, WebP, BMP\n")
    (princ "- Images are automatically scaled to fit the column width\n")
    (princ "- Use 'w' to adjust image column width for the current tag\n")
    (princ "- Use C-c C-i to insert an image path interactively\n\n")
    (princ "Example image paths:\n")
    (princ "/path/to/image.png\n")
    (princ "~/Pictures/photo.jpg\n")))

;;; --- Convenience Commands ---

(defun supertag-view-table-behaviors ()
  "Display all behaviors in table view."
  (interactive)
  (supertag-view-table (list :type :behavior :value "*all-behaviors*")))

(defun supertag-view-table-automations ()
  "Display all automations in table view."
  (interactive)
  (supertag-view-table (list :type :automation :value "*all-automations*")))

(defun supertag-view-table-project-task-correlation ()
  "Display project and task tables together for correlation analysis."
  (interactive)
  (let* ((available-tags (supertag-view-table--get-available-tags))
         (project-tag (completing-read "Select project tag: " available-tags nil t))

         (task-tag (completing-read "Select task tag: " available-tags nil t)))
    (when (and project-tag task-tag)
      (supertag-view-table 
       (list (list :type :tag :value project-tag)
             (list :type :tag :value task-tag))))))

;;; --- Cleanup ---

(defun supertag-view-table-cleanup ()
  "Clean up grid view subscriptions."
  (interactive)
  (let ((view-id (format "%s" (current-buffer))))
    (remhash view-id supertag-view-table--active-views)
    
    ))

(add-hook 'kill-buffer-hook #'supertag-view-table-cleanup)

(provide 'supertag-view-table)
;;; supertag-view-table.el ends here

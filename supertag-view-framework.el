;;; supertag-view-framework.el --- Framework for creating custom views -*- lexical-binding: t; -*-

;;; Commentary:

;; This module provides a framework for developers to create custom views
;; of org-supertag data. It is NOT an end-user configuration tool - it is
;; a toolbox for Elisp developers.
;;
;; Quick start - define a view:
;;
;;   (define-supertag-view progress-dashboard "Project Progress"
;;     (tag nodes)
;;     (supertag-view--with-buffer "Progress" tag
;;       (supertag-view--header "Progress Dashboard")
;;       (dolist (node nodes)
;;         (insert (format "%s\n" (plist-get node :title))))))
;;
;; Or using the low-level API:
;;
;;   (supertag-view-register
;;    :id 'my-view
;;    :name "My View"
;;    :render-fn #'my-render-function)

;;; Code:

(require 'cl-lib)

;; ============================================================================
;; Core Registry
;; ============================================================================

(defvar supertag--view-registry (make-hash-table :test 'eq)
  "Registry of all views.
Key is view ID (symbol), value is view definition plist.

View definition plist structure:
  :id           - Symbol identifier
  :name         - Display name (string)
  :description  - Optional description (string)
  :category     - Optional category (symbol)
  :render-fn    - Function to render the view (required)
  :valid-for    - List of tag names this view applies to, or nil for all")

(defvar supertag-view--rendering-view-id nil
  "Dynamic var: current view id while rendering.")

(defvar supertag-view--rendering-context nil
  "Dynamic var: current view context while rendering.")

(defvar supertag-view--rendering-context-builder nil
  "Dynamic var: current context builder while rendering.")

(defvar-local supertag-view--buffer-view-id nil
  "Buffer-local current view id for refresh.")

(defvar-local supertag-view--buffer-tag nil
  "Buffer-local tag name for refresh.")

(defvar-local supertag-view--buffer-context-builder nil
  "Buffer-local context builder for refresh.")

;; ============================================================================
;; Core API
;; ============================================================================

(defun supertag-view--context-builder-from-context (context)
  "Build a context builder function from CONTEXT."
  (let ((builder (plist-get context :context-builder))
        (tag (plist-get context :tag)))
    (cond
     ((functionp builder) builder)
     ((and (stringp tag) (> (length tag) 0))
      (lambda () (supertag-view--build-context tag)))
     (t nil))))

(defun supertag-view-register (&rest props)
  "Register a new view with properties PROPS.

Required properties:
  :id        - Symbol identifier (e.g., 'progress-dashboard)
  :name      - Display name string
  :render-fn - Function to render the view

Optional properties:
  :description - Description string
  :category    - Category symbol (e.g., :project-management)
  :valid-for   - List of tag names, or nil for all tags

Example:
  (supertag-view-register
   :id 'progress-dashboard
   :name \"Progress Dashboard\"
   :description \"Show project progress overview\"
   :category :project-management
   :render-fn #'supertag-view--render-progress
   :valid-for '(\"project\"))

Returns the view definition plist."
  (let* ((id (plist-get props :id))
         (name (plist-get props :name))
         (render-fn (plist-get props :render-fn)))
    ;; Validate required fields
    (unless id
      (error "View must have an :id"))
    (unless (symbolp id)
      (error "View :id must be a symbol, got: %s" (type-of id)))
    (unless name
      (error "View must have a :name"))
    (unless (stringp name)
      (error "View :name must be a string, got: %s" (type-of name)))
    (unless render-fn
      (error "View must have a :render-fn"))
    (unless (functionp render-fn)
      (error "View :render-fn must be a function, got: %s" (type-of render-fn)))
    ;; Store in registry
    (puthash id props supertag--view-registry)
    (message "Registered view '%s' (%s)" name id)
    props))

(defun supertag-view-unregister (id)
  "Unregister view with ID.
Returns the removed view definition, or nil if not found."
  (let ((view (gethash id supertag--view-registry)))
    (when view
      (remhash id supertag--view-registry)
      (message "Unregistered view '%s'" id)
      view)))

(defun supertag-view-get (id)
  "Get view definition by ID.
Returns the view plist, or nil if not found."
  (gethash id supertag--view-registry))

(defun supertag-view-list ()
  "List all registered views.
Returns a list of view definition plists sorted by name."
  (let (result)
    (maphash (lambda (_id view) (push view result))
             supertag--view-registry)
    (sort result (lambda (a b)
                   (string< (plist-get a :name)
                            (plist-get b :name))))))

(defun supertag-view-list-for-tag (tag-name)
  "List views applicable to TAG-NAME.
Returns a list of view definition plists.
If a view has :valid-for nil, it applies to all tags."
  (cl-remove-if-not
   (lambda (view)
     (let ((valid-for (plist-get view :valid-for)))
       (or (null valid-for)
           (member tag-name valid-for))))
   (supertag-view-list)))

(defun supertag-view-render (id context)
  "Render view ID with CONTEXT.

CONTEXT is a plist containing:
  :tag          - Tag name being viewed
  :nodes        - List of node data
  :virtual-columns - Available virtual column definitions

Example:
  (supertag-view-render 'progress-dashboard
                        (list :tag \"project\"
                              :nodes node-list))"
  (let ((view (supertag-view-get id)))
    (unless view
      (error "Unknown view: %s" id))
    (let ((render-fn (plist-get view :render-fn)))
      (let ((supertag-view--rendering-view-id id)
            (supertag-view--rendering-context context)
            (supertag-view--rendering-context-builder
             (supertag-view--context-builder-from-context context)))
        (funcall render-fn context)))))

;; ============================================================================
;; Developer Macro
;; ============================================================================

(defmacro define-supertag-view (id name arglist &rest body)
  "Define a new view with ID, NAME, and ARGLIST.

ARGLIST should be (TAG NODES) - these are extracted from context.
BODY is the render code.

Example:
  (define-supertag-view my-progress \"My Progress\"
    (tag nodes)
    (supertag-view--with-buffer \"Progress\" tag
      (insert \"Content here...\")))"
  (declare (indent defun))
  (let ((render-fn-name (intern (format "supertag-view--render-%s" id))))
    `(progn
       ;; Define the render function
       (defun ,render-fn-name (context)
         (let ((,(car arglist) (plist-get context :tag))
               (,(cadr arglist) (plist-get context :nodes)))
           ,@body))
       ;; Register the view
       (supertag-view-register
        :id ',id
        :name ,name
        :render-fn #',render-fn-name))))

;; ============================================================================
;; Rendering Utilities (Developer Toolbox)
;; ============================================================================

(defmacro supertag-view--with-buffer (base-name tag &rest body)
  "Execute BODY in a buffer named '*View: BASE-NAME - TAG*'.
Creates or reuses the buffer, makes it read-only at the end."
  (declare (indent 2))
  (let ((buf (gensym "buf"))
        (buf-name (gensym "buf-name")))
    `(let* ((,buf-name (format "*View: %s - %s*" ,base-name ,tag))
            (,buf (get-buffer-create ,buf-name)))
       (with-current-buffer ,buf
         (when supertag-view--rendering-view-id
           (setq-local supertag-view--buffer-view-id supertag-view--rendering-view-id)
           (setq-local supertag-view--buffer-tag
                       (plist-get supertag-view--rendering-context :tag))
           (setq-local supertag-view--buffer-context-builder
                       supertag-view--rendering-context-builder))
         (let ((inhibit-read-only t))
           (erase-buffer)
           ,@body)
         (special-mode)
         (setq buffer-read-only t)
         (goto-char (point-min)))
       (pop-to-buffer ,buf))))

(defun supertag-view--header (title)
  "Insert a header with TITLE."
  (insert (format "%s\n" title))
  (insert (make-string (length title) ?=))
  (insert "\n\n"))

(defun supertag-view--subheader (title)
  "Insert a subheader with TITLE."
  (insert (format "%s\n" title))
  (insert (make-string (length title) ?-))
  (insert "\n\n"))

(defun supertag-view--progress-bar (percentage &optional width)
  "Insert a text progress bar for PERCENTAGE (0-100).
WIDTH is the bar width in characters (default 20)."
  (let* ((w (or width 20))
         (filled (round (* w (/ percentage 100.0))))
         (empty (- w filled)))
    (insert "[")
    (insert (make-string filled ?█))
    (insert (make-string empty ?░))
    (insert (format "] %d%%\n" percentage))))

(defun supertag-view--stat-row (stats)
  "Insert a row of statistics.
STATS is a list of (label . value) pairs."
  (dolist (stat stats)
    (insert (format "  %s: %s\n" (car stat) (cdr stat))))
  (insert "\n"))

(defun supertag-view--separator (&optional char)
  "Insert a separator line using CHAR (default ?-)."
  (let ((c (or char ?-)))
    (insert (make-string (window-width) c))
    (insert "\n\n")))

;; ============================================================================
;; Data Access Utilities
;; ============================================================================

(defun supertag-view--get-vc (node-id column-id &optional default)
  "Get virtual column value for NODE-ID and COLUMN-ID.
Returns DEFAULT if not found or error."
  (if (fboundp 'supertag-virtual-column-get)
      (supertag-virtual-column-get node-id column-id default)
    default))

(defun supertag-view--get-global-field (node-id field-id &optional default)
  "Get global field value for NODE-ID and FIELD-ID."
  (if (fboundp 'supertag-node-get-global-field)
      (supertag-node-get-global-field node-id field-id default)
    default))

;; ============================================================================
;; Interactive Commands
;; ============================================================================

(declare-function supertag-view-table--get-current-tag-id "supertag-view-table" ())

(defun supertag-view-select-and-render (tag-name)
  "Interactively select a view for TAG-NAME and render it."
  (interactive (list (supertag-view--read-tag)))
  (let* ((views (supertag-view-list-for-tag tag-name))
         (view-names (mapcar (lambda (v) (plist-get v :name)) views)))
    (if (null views)
        (message "No views available for tag '%s'" tag-name)
      (let* ((selected-name (completing-read
                            (format "Select view for #%s: " tag-name)
                            view-names
                            nil t))
             (selected (cl-find selected-name views
                               :key (lambda (v) (plist-get v :name))
                               :test #'string=)))
        (when selected
          (let ((id (plist-get selected :id)))
            (supertag-view-render id
                                 (supertag-view--build-context tag-name))))))))

(defun supertag-view-select-from-schema ()
  "Select and render a view from Schema View."
  (interactive)
  (let ((tag-name (or (supertag-view--get-tag-at-point)
                      (supertag-view--read-tag))))
    (supertag-view-select-and-render tag-name)))

(defun supertag-view--read-tag ()
  "Read a tag name interactively."
  (read-string "Tag name: "))

(defun supertag-view--get-tag-at-point ()
  "Try to get the tag name at point in schema view."
  nil)

(defun supertag-view--build-context (tag-name)
  "Build render context for TAG-NAME."
  (let* ((node-ids (when (and (stringp tag-name)
                              (> (length tag-name) 0)
                              (fboundp 'supertag-view-api-nodes-by-tag))
                     (supertag-view-api-nodes-by-tag tag-name)))
         (nodes (when (and node-ids
                           (fboundp 'supertag-view-api-get-entities))
                  (supertag-view-api-get-entities :nodes node-ids))))
    (list :tag tag-name
          :nodes nodes
          :virtual-columns nil
          :get-vc #'supertag-view--get-vc
          :get-global-field #'supertag-view--get-global-field)))

(defun supertag-view-list-interactive ()
  "Display list of all views in a buffer."
  (interactive)
  (with-output-to-temp-buffer "*Supertag Views*"
    (princ "Registered Views\n")
    (princ "=================\n\n")
    (let ((views (supertag-view-list)))
      (if (null views)
          (princ "No views registered.\n")
        (dolist (view views)
          (princ (format "ID: %s\n" (plist-get view :id)))
          (princ (format "  Name: %s\n" (plist-get view :name)))
          (when (plist-get view :description)
            (princ (format "  Description: %s\n" (plist-get view :description))))
          (when (plist-get view :category)
            (princ (format "  Category: %s\n" (plist-get view :category))))
          (let ((valid-for (plist-get view :valid-for)))
            (if valid-for
                (princ (format "  Valid for: %s\n" valid-for))
              (princ "  Valid for: (all tags)\n")))
          (princ "\n")))))
  (pop-to-buffer "*Supertag Views*"))

(defun supertag-view-refresh ()
  "Refresh current view buffer."
  (interactive)
  (unless supertag-view--buffer-view-id
    (user-error "Not in a view buffer"))
  (let* ((builder (or supertag-view--buffer-context-builder
                      (when (and (stringp supertag-view--buffer-tag)
                                 (> (length supertag-view--buffer-tag) 0))
                        (let ((tag supertag-view--buffer-tag))
                          (lambda () (supertag-view--build-context tag))))))
         (context (and builder (funcall builder))))
    (unless builder
      (user-error "No context builder available for refresh"))
    (unless (listp context)
      (user-error "Context builder did not return a plist"))
    (condition-case err
        (supertag-view-render supertag-view--buffer-view-id context)
      (error
       (message "View refresh failed: %s" (error-message-string err))))))

;; ============================================================================
;; Configuration Persistence
;; ============================================================================

(defvar supertag--view-configs (make-hash-table :test 'eq)
  "Hash table storing view configurations (not the render functions).
Key is view ID, value is configuration plist without :render-fn.
This is used for saving/loading view definitions.")

(defun supertag-view-config-register (config)
  "Register a view CONFIG (plist) for persistence.
This stores the configuration without the render function.
The render function should be provided by the view implementation."
  (let* ((id (plist-get config :id))
         (config-without-fn (cl-remove-if (lambda (x) (eq x :render-fn)) config
                                         :key #'identity
                                         :test-not (lambda (a b) (eq a b)))))
    (puthash id config supertag--view-configs)
    config))

(defun supertag-view-config-get (id)
  "Get stored configuration for view ID."
  (gethash id supertag--view-configs))

(defun supertag-view-config-list ()
  "List all stored view configurations."
  (let (result)
    (maphash (lambda (_id config) (push config result))
             supertag--view-configs)
    (sort result (lambda (a b)
                   (string< (plist-get a :name)
                            (plist-get b :name))))))

(defun supertag-view-config-export-elisp (id)
  "Export view ID configuration as Elisp code.
Returns a string that can be saved to a file and loaded later.
The exported code will recreate the view registration."
  (let ((config (supertag-view-config-get id)))
    (unless config
      (error "No configuration found for view: %s" id))
    (format ";; View configuration for %s\n(supertag-view-register\n %s)"
            id
            (string-join
             (cl-loop for (key value) on config by #'cddr
                     unless (eq key :render-fn)
                     collect (format "%S %S" key value))
             "\n "))))

(defun supertag-view-config-export-all-elisp ()
  "Export all view configurations as Elisp code."
  (let ((configs (supertag-view-config-list)))
    (with-output-to-temp-buffer "*View Configs Export*"
      (princ ";; Supertag View Configurations\n")
      (princ ";; Generated: ")
      (princ (format-time-string "%Y-%m-%d %H:%M:%S"))
      (princ "\n\n")
      (princ "(require 'supertag-view-framework)\n\n")
      (dolist (config configs)
        (let ((id (plist-get config :id)))
          (princ (supertag-view-config-export-elisp id))
          (princ "\n\n"))))
    (pop-to-buffer "*View Configs Export*")))

(defun supertag-view-config-save-to-file (filename)
  "Save all view configurations to FILENAME as Elisp code."
  (interactive "FSave view configs to file: ")
  (with-temp-file filename
    (insert ";; Supertag View Configurations\n")
    (insert ";; Generated: ")
    (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
    (insert "\n\n")
    (insert "(require 'supertag-view-framework)\n\n")
    (dolist (config (supertag-view-config-list))
      (let ((id (plist-get config :id)))
        (insert (supertag-view-config-export-elisp id))
        (insert "\n\n"))))
  (message "View configs saved to %s" filename))

(defun supertag-view-config-load-from-file (filename)
  "Load view configurations from FILENAME.
Note: This loads the Elisp code which should register the views."
  (interactive "fLoad view configs from file: ")
  (load filename nil nil t)
  (message "View configs loaded from %s" filename))

;; ============================================================================
;; Widget Rendering Helpers (DSL v2)
;; ============================================================================

(defun supertag-view--resolve-prop (value context)
  "Resolve VALUE in CONTEXT.
If VALUE is a function, call it with CONTEXT."
  (if (functionp value)
      (condition-case err
          (funcall value context)
        (error
         (message "View DSL: prop binding failed: %s"
                  (error-message-string err))
         nil))
    value))

(defun supertag-view--resolve-props (widget context)
  "Resolve WIDGET properties using CONTEXT."
  (let (props)
    (cl-loop for (key value) on widget by #'cddr
             unless (eq key :type)
             do (setq props (plist-put props key
                                       (supertag-view--resolve-prop value context))))
    props))

(defun supertag-view--render-widget (widget context)
  "Render a single WIDGET definition with CONTEXT."
  (unless (listp widget)
    (error "Widget must be a plist, got: %S" widget))
  (let ((type (plist-get widget :type)))
    (unless type
      (error "Widget missing :type: %S" widget))
    (supertag-widget-render type
                            (supertag-view--resolve-props widget context)
                            context)))

(defun supertag-view--render-widgets (widgets context)
  "Render WIDGETS list with CONTEXT."
  (when widgets
    (unless (listp widgets)
      (error "Widgets must be a list, got: %S" widgets))
    (dolist (widget widgets)
      (supertag-view--render-widget widget context))))

(defun supertag-view--render-widgets-to-lines (widgets context)
  "Render WIDGETS into a list of lines using CONTEXT."
  (with-temp-buffer
    (supertag-view--render-widgets widgets context)
    (split-string (buffer-string) "\n" nil)))

(defun supertag-view--pad-line (line width)
  "Pad or truncate LINE to WIDTH."
  (let ((cell (truncate-string-to-width (or line "") width 0 nil t)))
    (if (< (string-width cell) width)
        (concat cell (make-string (- width (string-width cell)) ?\s))
      cell)))

;; ============================================================================
;; Widget System
;; ============================================================================

(defvar supertag--widget-registry (make-hash-table :test 'eq)
  "Registry of widget types.
Key is widget type symbol, value is render function.
Widgets are reusable UI components for building views.")

(defun supertag-widget--normalize-type (type)
  "Normalize widget TYPE to a registry key symbol."
  (if (keywordp type)
      (intern (substring (symbol-name type) 1))
    type))

(defun supertag-widget--accepts-context-p (render-fn)
  "Return non-nil if RENDER-FN accepts a CONTEXT argument."
  (let* ((arity (ignore-errors (func-arity render-fn)))
         (min-args (car arity))
         (max-args (cdr arity)))
    (or (and (integerp min-args) (>= min-args 2))
        (eq max-args 'many)
        (and (integerp max-args) (>= max-args 2)))))

(defface supertag-view-widget-badge-face
  '((t :weight bold))
  "Face for badge widget content.")

(defface supertag-view-widget-toolbar-label-face
  '((t :weight bold))
  "Face for toolbar label text.")

(defun supertag-widget-register (type render-fn)
  "Register a widget TYPE with RENDER-FN.
TYPE is a symbol like 'header, 'progress-bar, etc.
RENDER-FN is a function that takes a plist of properties and renders the widget."
  (let ((key (supertag-widget--normalize-type type)))
    (puthash key render-fn supertag--widget-registry)
    key))

(defun supertag-widget-render (type props &optional context)
  "Render widget TYPE with PROPS.
TYPE is the widget type symbol.
PROPS is a plist of properties for the widget.
Optional CONTEXT is passed to renderers that accept it.
Example: (supertag-widget-render 'header '(:text \"Title\"))"
  (let* ((key (supertag-widget--normalize-type type))
         (render-fn (gethash key supertag--widget-registry)))
    (unless render-fn
      (error "Unknown widget type: %s" type))
    (if (and context (supertag-widget--accepts-context-p render-fn))
        (funcall render-fn props context)
      (funcall render-fn props))))

;; Built-in widgets

(supertag-widget-register 'header
  (lambda (props)
    (let ((text (plist-get props :text)))
      (insert (format "%s\n" text))
      (insert (make-string (length text) ?=))
      (insert "\n\n"))))

(supertag-widget-register 'subheader
  (lambda (props)
    (let ((text (plist-get props :text)))
      (insert (format "%s\n" text))
      (insert (make-string (length text) ?-))
      (insert "\n\n"))))

(supertag-widget-register 'text
  (lambda (props)
    (let ((content (plist-get props :content)))
      (insert (format "%s\n" content)))))

(supertag-widget-register 'progress-bar
  (lambda (props)
    (let* ((value (plist-get props :value))
           (max (or (plist-get props :max) 100))
           (width (or (plist-get props :width) 20))
           (percentage (* 100.0 (/ value max)))
           (filled (round (* width (/ percentage 100.0))))
           (empty (- width filled)))
      (insert "[")
      (insert (make-string filled ?█))
      (insert (make-string empty ?░))
      (insert (format "] %d%%\n" (round percentage))))))

(supertag-widget-register 'stats-row
  (lambda (props)
    (let ((stats (plist-get props :stats)))
      (dolist (stat stats)
        (insert (format "  %s: %s\n" (car stat) (cdr stat))))
      (insert "\n"))))

(supertag-widget-register 'separator
  (lambda (props)
    (let ((char (or (plist-get props :char) ?-)))
      (insert (make-string (window-width) char))
      (insert "\n\n"))))

(supertag-widget-register 'list
  (lambda (props)
    (let ((items (plist-get props :items)))
      (dotimes (i (length items))
        (let ((item (nth i items)))
          (insert (format "%d. %s\n" (1+ i) item))))
      (insert "\n"))))

(supertag-widget-register 'table
  (lambda (props)
    (let* ((headers (plist-get props :headers))
           (rows (plist-get props :rows))
           (widths (or (plist-get props :widths)
                      (make-list (length headers) 15))))
      ;; Header row
      (dotimes (i (length headers))
        (insert (format "%-*s " (nth i widths) (nth i headers))))
      (insert "\n")
      ;; Separator
      (dotimes (i (length headers))
        (insert (make-string (nth i widths) ?-)))
      (insert "\n")
      ;; Data rows
      (dolist (row rows)
        (dotimes (i (length row))
          (insert (format "%-*s " (nth i widths) (nth i row))))
        (insert "\n"))
      (insert "\n"))))

;; Container widgets (DSL v2)

(supertag-widget-register 'section
  (lambda (props &optional context)
    (let ((title (plist-get props :title))
          (children (plist-get props :children)))
      (when title
        (supertag-view--subheader title))
      (when children
        (unless (listp children)
          (error "Widget :children must be a list, got: %S" children))
        (supertag-view--render-widgets children context)))))

(supertag-widget-register 'stack
  (lambda (props &optional context)
    (let* ((children (plist-get props :children))
           (spacing (or (plist-get props :spacing) 1))
           (count 0)
           (index 0))
      (unless (listp children)
        (error "Widget :children must be a list, got: %S" children))
      (setq count (length children))
      (dolist (child children)
        (setq index (1+ index))
        (supertag-view--render-widget child context)
        (when (< index count)
          (dotimes (_ spacing)
            (insert "\n")))))))

(supertag-widget-register 'columns
  (lambda (props &optional context)
    (let ((columns (plist-get props :columns)))
      (unless (listp columns)
        (error "Widget :columns must be a list, got: %S" columns))
      (let* ((column-data
              (mapcar
               (lambda (column)
                 (let* ((width (supertag-view--resolve-prop
                                (plist-get column :width) context))
                        (width (if (and (integerp width) (> width 0)) width 30))
                        (children (plist-get column :children)))
                   (unless (listp children)
                     (error "Column :children must be a list, got: %S" children))
                   (list (supertag-view--render-widgets-to-lines children context)
                         width)))
               columns))
             (lines-per-col (mapcar #'car column-data))
             (widths (mapcar #'cadr column-data))
             (max-lines (if lines-per-col
                            (apply #'max (mapcar #'length lines-per-col))
                          0))
             (col-count (length columns)))
        (dotimes (line-idx max-lines)
          (dotimes (col-idx col-count)
            (let* ((col-lines (nth col-idx lines-per-col))
                   (width (nth col-idx widths))
                   (line (or (nth line-idx col-lines) "")))
              (insert (supertag-view--pad-line line width))
              (when (< col-idx (1- col-count))
                (insert " "))))
          (insert "\n"))))))

;; Layout and info widgets (DSL v2)

(defun supertag-widget--render-card (props context)
  "Render a simple card container with box-drawing style."
  (let* ((title (plist-get props :title))
         (children (plist-get props :children))
         (width (plist-get props :width))
         (child-lines
          (when children
            (unless (listp children)
              (error "Widget :children must be a list, got: %S" children))
            (supertag-view--render-widgets-to-lines children context)))
         (lines (append (when title (list (format "%s" title))) child-lines))
         (content-width (if lines
                            (apply #'max (mapcar #'string-width lines))
                          0))
         (max-width (max 1 (- (window-width) 4)))
         (inner-width (cond
                       ((and (integerp width) (> width 0)) width)
                       ((> content-width 0) content-width)
                       (t 1))))
    (setq inner-width (min inner-width max-width))
    (when (null lines)
      (setq lines (list "")))
    (insert (format "┌%s┐\n" (make-string (+ inner-width 2) ?─)))
    (let ((is-title t))
      (dolist (line lines)
        (let ((padded (supertag-view--pad-line line inner-width)))
          (when (and is-title title)
            (setq padded (propertize padded 'face 'bold)))
          (insert (format "│ %s │\n" padded)))
        (setq is-title nil)))
    (insert (format "└%s┘\n" (make-string (+ inner-width 2) ?─)))
    (insert "\n")))

(supertag-widget-register 'card #'supertag-widget--render-card)
(supertag-widget-register 'panel #'supertag-widget--render-card)

(defun supertag-widget--render-field-table (props)
  "Render field/value pairs in a table style aligned with `supertag-view-table`."
  (let* ((items (or (plist-get props :items) '()))
         (pairs
          (mapcar
           (lambda (item)
             (cond
              ((consp item) (cons (car item) (cdr item)))
              ((and (listp item) (= (length item) 2))
               (cons (nth 0 item) (nth 1 item)))
              (t (cons (format "%s" item) ""))))
           items))
         (label-texts (mapcar (lambda (pair) (format "%s" (car pair))) pairs))
         (value-texts (mapcar (lambda (pair) (format "%s" (cdr pair))) pairs))
         (label-width (apply #'max 5 (mapcar #'string-width (cons "Field" label-texts))))
         (value-width (apply #'max 5 (mapcar #'string-width (cons "Value" value-texts))))
         (max-width (max 10 (- (window-width) 7))))
    (when (> (+ label-width value-width) max-width)
      (let* ((spill (- (+ label-width value-width) max-width))
             (trim (min spill (max 0 (- value-width 5)))))
        (setq value-width (max 5 (- value-width trim)))))
    (let* ((label-seg (make-string (+ label-width 2) ?─))
           (value-seg (make-string (+ value-width 2) ?─))
           (top (format "┌%s┬%s┐" label-seg value-seg))
           (mid (format "├%s┼%s┤" label-seg value-seg))
           (bottom (format "└%s┴%s┘" label-seg value-seg)))
      (insert top "\n")
      (cl-loop for label in label-texts
               for value in value-texts
               for idx from 0
               do (progn
                    (when (> idx 0)
                      (insert mid "\n"))
                    (insert (format "│ %s │ %s │\n"
                                    (supertag-view--pad-line label label-width)
                                    (supertag-view--pad-line value value-width)))))
      (insert bottom "\n\n"))))

(supertag-widget-register 'field #'supertag-widget--render-field-table)
(supertag-widget-register 'kv #'supertag-widget--render-field-table)

(supertag-widget-register 'badge
  (lambda (props)
    (let* ((text (plist-get props :text))
           (items (or (plist-get props :items)
                      (when text (list text)))))
      (when items
        (insert (mapconcat
                 (lambda (item)
                   (propertize (format "[%s]" item)
                               'face 'supertag-view-widget-badge-face))
                 items
                 " ")))
      (insert "\n"))))

(supertag-widget-register 'empty
  (lambda (props)
    (let ((title (or (plist-get props :title) "No data"))
          (message (plist-get props :message)))
      (insert (format "%s\n" title))
      (when message
        (insert (format "%s\n" message)))
      (insert "\n"))))

(supertag-widget-register 'toolbar
  (lambda (props)
    (let* ((items (or (plist-get props :items) '()))
           (label (or (plist-get props :label) "Operations"))
           (formatted
            (mapcar
             (lambda (item)
               (cond
                ((consp item) (format "%s (%s)" (car item) (cdr item)))
                ((stringp item) item)
                (t (format "%s" item))))
             items)))
      (insert (propertize (format "%s:" label)
                          'face 'supertag-view-widget-toolbar-label-face))
      (insert (format " %s\n"
                      (mapconcat #'identity formatted " | ")))
      (insert "\n"))))

;; ============================================================================
;; DSL - Declarative View Definition
;; ============================================================================

(defun supertag-view-define-from-config (config)
  "Define a view from a declarative CONFIG.
CONFIG is a plist with:
  :id       - View identifier (symbol)
  :name     - Display name
  :tag      - Target tag (optional)
  :widgets  - List of widget definitions

Widget definition:
  :type can be a symbol (header) or keyword (:header).
  Keywords are normalized to symbols at render time."
  (let* ((id (plist-get config :id))
         (name (plist-get config :name))
         (tag (plist-get config :tag))
         (widgets (plist-get config :widgets)))

    ;; Create render function from widgets
    (let ((render-fn (lambda (context)
                       (let ((tag (plist-get context :tag)))
                         (supertag-view--with-buffer name tag
                           (supertag-view--render-widgets widgets context))))))

      ;; Register the view
      (supertag-view-register
       :id id
       :name name
       :render-fn render-fn
       :valid-for (when tag (list tag)))

      ;; Also store config for persistence
      (supertag-view-config-register config)

      (message "View '%s' defined from config" name)
      id)))

(defun supertag-view-dsl-example ()
  "Example of using the DSL to define a view."
  (interactive)
  (supertag-view-define-from-config
   (list :id 'dsl-example
         :name "DSL Example"
         :tag "demo"
         :widgets
         (list
          (list :type :section :title "Overview"
                :children
                (list
                 (list :type :text :content "This view was created using the DSL!")
                 (list :type :stats-row
                       :stats (lambda (ctx)
                                (list (cons "Total" (length (plist-get ctx :nodes))))))))
          (list :type :stack
                :children
                (list
                 (list :type :progress-bar
                       :value (lambda (ctx)
                                (or (plist-get (car (plist-get ctx :nodes)) :progress) 0)))
                 (list :type :list :items ("Task A" "Task B" "Task C"))))))))

;; ============================================================================
;; Initialization
;; ============================================================================

(defun supertag-view-framework-init ()
  "Initialize the view framework.
Clears all registered views and stored configurations."
  (interactive)
  (clrhash supertag--view-registry)
  (clrhash supertag--view-configs)
  (clrhash supertag--widget-registry)
  (message "View framework initialized"))

(provide 'supertag-view-framework)

;;; supertag-view-framework.el ends here

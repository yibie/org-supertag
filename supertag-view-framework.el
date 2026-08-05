;;; supertag-view-framework.el --- Framework for creating custom views -*- lexical-binding: t; -*-

;;; Commentary:

;; This module provides a framework for developers to create custom views
;; of org-supertag data.  It is NOT an end-user configuration tool - it is
;; a toolbox for Elisp developers.
;;
;; Quick start - register a view:
;;
;;   (supertag-view-register
;;    :id 'my-view
;;    :name "My View"
;;    :state-fn #'identity
;;    :render-fn #'my-render-function)

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'subr-x)
(require 'widget)
(require 'wid-edit)
(require 'supertag-core-tag-path)
(require 'supertag-services-ui)
(require 'supertag-view-api)

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
  :valid-for    - List of tag names this view applies to, or nil for all
  :selectable   - Nil hides an internal Adapter from the custom-view picker
  :buffer-name / :buffer-name-fn - Runtime buffer naming
  :mode-fn      - Runtime major-mode installer
  :state-fn     - Build refreshable state from the original input
  :display-action - Native `display-buffer' action
  :subscribe-fn - Return cleanup callbacks for Runtime-owned resources
  :capture-selection-fn / :restore-selection-fn - Refresh position hooks")

(defvar-local supertag-view--instance nil
  "Buffer-local View Runtime instance plist.")

;; ============================================================================
;; Core API
;; ============================================================================

(defun supertag-view--context-builder-from-context (context)
  "Build a context builder function from CONTEXT."
  (let ((builder (plist-get context :context-builder))
        (query (or (plist-get context :query)
                   (plist-get context :tag))))
    (cond
     ((functionp builder) builder)
     (query
      (lambda () (supertag-view--build-context query)))
     (t nil))))

(defun supertag-view--rebuild-context (context)
  "Rebuild CONTEXT from its builder or query when available."
  (if-let* ((builder (supertag-view--context-builder-from-context context)))
      (funcall builder)
    context))

(defun supertag-view-register (&rest props)
  "Register a new view with properties PROPS.

Required properties:
  :id        - Symbol identifier (for example, `progress-dashboard')
  :name      - Display name string
  :render-fn - Function to render the view

Optional properties:
  :description - Description string
  :category    - Category symbol (e.g., :project-management)
  :valid-for   - List of tag names, or nil for all tags
  :selectable  - Nil to hide an internal Adapter from the view picker
  :buffer-name or :buffer-name-fn - Runtime buffer naming
  :mode-fn, :state-fn, :display-action, :subscribe-fn
  :capture-selection-fn, :restore-selection-fn

Example:
  (supertag-view-register
   :id (quote progress-dashboard)
   :name \"Progress Dashboard\"
   :description \"Show project progress overview\"
   :category :project-management
   :render-fn (function supertag-view--render-progress)
   :valid-for (list \"project\"))

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

(defun supertag-view--cleanup-instance ()
  "Clean the current buffer's View Runtime instance."
  (when supertag-view--instance
    (let ((cleanup-fns (plist-get supertag-view--instance :cleanup-fns))
          first-error)
      (setq supertag-view--instance nil)
      (dolist (cleanup cleanup-fns)
        (condition-case err
            (funcall cleanup)
          (error
           (unless first-error
             (setq first-error err)))))
      (when first-error
        (message "View cleanup failed: %s"
                 (error-message-string first-error))))))

(defun supertag-view-open (id input &optional display-action)
  "Open registered view ID with INPUT through the View Runtime.
DISPLAY-ACTION overrides the view's registered display action."
  (let ((view (supertag-view-get id)))
    (unless view
      (user-error "Unknown view: %s" id))
    (let* ((state-fn (plist-get view :state-fn))
           (state (if state-fn (funcall state-fn input) input))
           (buffer-name-fn (plist-get view :buffer-name-fn))
           (buffer-name
            (cond
             (buffer-name-fn (funcall buffer-name-fn input))
             ((plist-get view :buffer-name))
             (t (format "*View: %s*" (plist-get view :name)))))
           (buffer (get-buffer-create buffer-name))
           (mode-fn (or (plist-get view :mode-fn) #'special-mode))
           (subscribe-fn (plist-get view :subscribe-fn)))
      (condition-case err
          (progn
            (with-current-buffer buffer
              (supertag-view--cleanup-instance)
              (funcall mode-fn)
              (add-hook 'kill-buffer-hook #'supertag-view--cleanup-instance nil t)
              (let ((inhibit-read-only t))
                (funcall (plist-get view :render-fn) state))
              (setq-local supertag-view--instance
                          (list :view-id id :input input :state state
                                :cleanup-fns nil))
              (when subscribe-fn
                (let ((cleanup
                       (funcall subscribe-fn input state
                                (lambda (&rest _event)
                                  (when (buffer-live-p buffer)
                                    (with-current-buffer buffer
                                      (supertag-view-refresh)))))))
                  (setf (plist-get supertag-view--instance :cleanup-fns)
                        (if (functionp cleanup) (list cleanup) cleanup)))))
            (display-buffer buffer (or display-action
                                       (plist-get view :display-action)))
            buffer)
        (error
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (supertag-view--cleanup-instance)))
         (signal (car err) (cdr err)))))))

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
       (and (not (and (plist-member view :selectable)
                      (null (plist-get view :selectable))))
            (or (null valid-for)
                (member tag-name valid-for)))))
   (supertag-view-list)))

;; ============================================================================
;; Rendering Utilities (Developer Toolbox)
;; ============================================================================

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
  "Get global field value for NODE-ID and FIELD-ID, or DEFAULT."
  (if (fboundp 'supertag-node-get-global-field)
      (supertag-node-get-global-field node-id field-id default)
    default))

;; ============================================================================
;; Interactive Commands
;; ============================================================================

(declare-function supertag-view-table--get-current-tag-id "supertag-view-table" ())

(defun supertag-view--normalize-tag-query (tag-or-query)
  "Return a canonical tag query plist for TAG-OR-QUERY."
  (cond
   ((and (stringp tag-or-query) (not (string-empty-p tag-or-query)))
    (list :type :tag :value tag-or-query))
   ((and (listp tag-or-query)
         (eq (plist-get tag-or-query :type) :tag)
         (stringp (plist-get tag-or-query :value))
         (not (string-empty-p (plist-get tag-or-query :value))))
    (copy-sequence tag-or-query))
   (t
    (user-error "Expected a tag name or tag query, got %S" tag-or-query))))

(defun supertag-view-select-and-render (tag-or-query)
  "Interactively select a view for TAG-OR-QUERY and render it."
  (interactive (list (supertag-view--read-tag)))
  (let* ((query (supertag-view--normalize-tag-query tag-or-query))
         (tag-name (plist-get query :value))
         (views (supertag-view-list-for-tag tag-name))
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
          (supertag-view-open (plist-get selected :id)
                              (supertag-view--build-context query)))))))

(defun supertag-view-select-from-schema ()
  "Select and render a view from Schema View."
  (interactive)
  (let ((query (or (supertag-view--get-tag-at-point)
                   (supertag-view--read-tag))))
    (supertag-view-select-and-render query)))

(defun supertag-view--read-tag ()
  "Read a tag query, including derived namespace choices."
  (let* ((tag-ids (supertag-view-api-list-tag-ids))
         (tag (supertag-ui-read-tag
               "Tag or namespace: " tag-ids nil nil t)))
    (append (list :type :tag :value tag)
            (when (supertag-tag-path-has-descendants-p tag tag-ids)
              '(:include-descendants t)))))

(defun supertag-view--get-tag-at-point ()
  "Return a tag query derived from Schema View context at point."
  (let* ((fallback (max (point-min) (1- (point))))
         (context (or (get-text-property (point) 'supertag-context)
                      (get-text-property fallback 'supertag-context)))
         (type (plist-get context :type)))
    (pcase type
      (:namespace
       (list :type :tag :value (plist-get context :path)
             :include-descendants t))
      ((or :tag :field)
       (append (list :type :tag :value (plist-get context :tag-id))
               (when (plist-get context :has-descendants)
                 '(:include-descendants t)))))))

(defun supertag-view--build-context (tag-or-query)
  "Build render context for TAG-OR-QUERY."
  (let* ((query (supertag-view--normalize-tag-query tag-or-query))
         (tag-name (plist-get query :value))
         (include-descendants (plist-get query :include-descendants))
         (node-ids (supertag-view-api-nodes-by-tag
                    tag-name include-descendants))
         (nodes (when (and node-ids
                           (fboundp 'supertag-view-api-get-entities))
                  (supertag-view-api-get-entities :nodes node-ids))))
    (list :tag tag-name
          :query query
          :include-descendants include-descendants
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

(defun supertag-view--refresh-instance ()
  "Refresh the current buffer's View Runtime instance."
  (let* ((view-id (plist-get supertag-view--instance :view-id))
         (view (supertag-view-get view-id)))
    (unless view
      (user-error "Unknown view: %s" view-id))
    (let* ((input (plist-get supertag-view--instance :input))
           (state-fn (plist-get view :state-fn))
           (capture-fn (plist-get view :capture-selection-fn))
           (restore-fn (plist-get view :restore-selection-fn))
           (selection (when capture-fn (funcall capture-fn)))
           (state (if state-fn (funcall state-fn input) input)))
      (let ((inhibit-read-only t))
        (funcall (plist-get view :render-fn) state))
      (setf (plist-get supertag-view--instance :state) state)
      (when restore-fn
        (funcall restore-fn selection)))))

(defun supertag-view-refresh (&optional buffer)
  "Refresh BUFFER or the current view buffer."
  (interactive)
  (let ((target (or buffer (current-buffer))))
    (unless (buffer-live-p target)
      (user-error "View buffer is not live"))
    (with-current-buffer target
      (unless supertag-view--instance
        (user-error "Not in a view buffer"))
      (supertag-view--refresh-instance))))

;; ============================================================================
;; Configuration Persistence
;; ============================================================================

(defvar supertag--view-configs (make-hash-table :test 'eq)
  "Hash table storing view configurations (not the render functions).
Key is view ID, value is configuration plist without :render-fn.
This is used for saving/loading view definitions.")

(defun supertag-view-config-register (config)
  "Register a view CONFIG (plist) for persistence.
The render function should be provided by the view implementation."
  (let ((id (plist-get config :id)))
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

(defconst supertag-view--literal-props '(:key :action :on-change)
  "Widget properties whose values are literals, not context bindings.")

(defvar supertag-view-widget-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map
                       (make-composed-keymap widget-keymap special-mode-map))
    (define-key map (kbd "TAB") #'supertag-view-widget-forward)
    (define-key map (kbd "<backtab>") #'supertag-view-widget-backward)
    map)
  "Keymap for `supertag-view-widget-mode'.")

(defvar supertag-view-widget-field-map
  (let ((map (copy-keymap widget-field-keymap)))
    (define-key map (kbd "TAB") #'supertag-view-widget-forward)
    (define-key map (kbd "<backtab>") #'supertag-view-widget-backward)
    map)
  "Keymap used by editable fields in Widget DSL views.")

(define-derived-mode supertag-view-widget-mode special-mode "Supertag-View"
  "Major mode for declarative Supertag views."
  (setq buffer-read-only nil))

(defun supertag-view-widget--interactive-positions ()
  "Return sorted positions of text buttons and editable fields."
  (let ((position (point-min))
        button
        positions)
    (when (setq button (button-at position))
      (push (button-start button) positions)
      (setq position (button-start button)))
    (while (setq button (next-button position))
      (push (button-start button) positions)
      (setq position (button-start button)))
    (dolist (field widget-field-list)
      (push (widget-field-start field) positions))
    (sort (delete-dups positions) #'<)))

(defun supertag-view-widget-forward (count)
  "Move forward COUNT interactive controls, wrapping at buffer ends."
  (interactive "p")
  (let ((positions (supertag-view-widget--interactive-positions)))
    (unless positions
      (user-error "No interactive controls in this view"))
    (dotimes (_ (abs count))
      (goto-char
       (if (> count 0)
           (or (cl-find-if (lambda (position) (> position (point))) positions)
               (car positions))
         (or (cl-find-if (lambda (position) (< position (point)))
                         positions :from-end t)
             (car (last positions))))))))

(defun supertag-view-widget-backward (count)
  "Move backward COUNT interactive controls, wrapping at buffer ends."
  (interactive "p")
  (supertag-view-widget-forward (- count)))

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
                                       (if (memq key supertag-view--literal-props)
                                           value
                                         (supertag-view--resolve-prop
                                          value context)))))
    props))

(defun supertag-view--add-widget-key (from to key)
  "Add KEY between FROM and TO without replacing nested widget keys."
  (let ((position from))
    (while (< position to)
      (let ((end (or (next-single-property-change
                      position 'supertag-widget-key nil to)
                     to)))
        (unless (get-text-property position 'supertag-widget-key)
          (put-text-property position end 'supertag-widget-key key))
        (setq position end)))))

(defun supertag-view--render-widget (widget context)
  "Render a single WIDGET definition with CONTEXT."
  (unless (listp widget)
    (error "Widget must be a plist, got: %S" widget))
  (let* ((type (plist-get widget :type))
         (props (supertag-view--resolve-props widget context))
         (key (plist-get props :key))
         (start (point)))
    (unless type
      (error "Widget missing :type: %S" widget))
    (supertag-widget-render type props context)
    (when key
      (supertag-view--add-widget-key start (point) key))))

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

(defun supertag-view-widget--clear ()
  "Clear rendered text and stale editable-field bookkeeping."
  (dolist (field (delete-dups (append widget-field-new widget-field-list)))
    (widget-leave-text field))
  (setq widget-field-new nil
        widget-field-list nil)
  (let ((inhibit-modification-hooks t))
    (erase-buffer)))

(defun supertag-view-widget--capture-selection ()
  "Capture point as a stable Widget DSL key and offset."
  (let* ((position (if (and (eobp) (> (point) (point-min)))
                       (1- (point))
                     (point)))
         (key (get-text-property position 'supertag-widget-key)))
    (when key
      (let ((start position))
        (while (and (> start (point-min))
                    (equal (get-text-property
                            (1- start) 'supertag-widget-key)
                           key))
          (setq start (1- start)))
        (list :key key :offset (- position start))))))

(defun supertag-view-widget--restore-selection (selection)
  "Restore keyed SELECTION, falling back to `point-min'."
  (goto-char (point-min))
  (when-let* ((key (plist-get selection :key)))
    (let ((position (point-min))
          found)
      (while (and (< position (point-max)) (not found))
        (if (equal (get-text-property position 'supertag-widget-key) key)
            (setq found position)
          (setq position
                (or (next-single-property-change
                     position 'supertag-widget-key nil (point-max))
                    (point-max)))))
      (when found
        (let ((end (or (next-single-property-change
                        found 'supertag-widget-key nil (point-max))
                       (point-max))))
          (goto-char (min (1- end)
                          (+ found (or (plist-get selection :offset) 0)))))))))

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
  "Face for badge widget content."
  :group 'org-supertag)

(defface supertag-view-widget-toolbar-label-face
  '((t :weight bold))
  "Face for toolbar label text."
  :group 'org-supertag)

(defun supertag-widget-register (type render-fn)
  "Register a widget TYPE with RENDER-FN.
TYPE is a symbol such as `header' or `progress-bar'.
RENDER-FN is a function that takes a plist of properties and renders the widget."
  (let ((key (supertag-widget--normalize-type type)))
    (puthash key render-fn supertag--widget-registry)
    key))

(defun supertag-widget-render (type props &optional context)
  "Render widget TYPE with PROPS.
TYPE is the widget type symbol.
PROPS is a plist of properties for the widget.
Optional CONTEXT is passed to renderers that accept it.
Example: (supertag-widget-render (quote header) (list :text \"Title\"))"
  (let* ((key (supertag-widget--normalize-type type))
         (render-fn (gethash key supertag--widget-registry)))
    (unless render-fn
      (error "Unknown widget type: %s" type))
    (if (and context (supertag-widget--accepts-context-p render-fn))
        (funcall render-fn props context)
      (funcall render-fn props))))

(defun supertag-view-widget--insert-placeholder (descriptor text)
  "Insert TEXT carrying interactive leaf DESCRIPTOR."
  (let ((start (point)))
    (insert text)
    (put-text-property start (point)
                       'supertag-widget-placeholder descriptor)))

(defun supertag-widget--render-action (props face)
  "Render PROPS as a deferred text button using FACE."
  (let ((label (plist-get props :label))
        (action (plist-get props :action)))
    (unless (stringp label)
      (error "Widget action :label must be a string, got: %S" label))
    (unless (functionp action)
      (error "Widget action :action must be a function, got: %S" action))
    (supertag-view-widget--insert-placeholder
     (list :kind 'button :action action :face face
           :help-echo (plist-get props :help-echo))
     label)
    (unless (and (plist-member props :newline)
                 (null (plist-get props :newline)))
      (insert "\n"))))

(defun supertag-widget--render-editable-field (props)
  "Render PROPS as a deferred built-in editable field."
  (let ((value (plist-get props :value))
        (width (plist-get props :width))
        (on-change (plist-get props :on-change)))
    (unless (stringp value)
      (error "Editable field :value must be a string, got: %S" value))
    (unless (and (integerp width) (> width 0))
      (error "Editable field :width must be positive, got: %S" width))
    (when (> (string-width value) width)
      (error "Editable field value is wider than :width %d: %S"
             width value))
    (unless (or (null on-change) (functionp on-change))
      (error "Editable field :on-change must be a function, got: %S"
             on-change))
    (supertag-view-widget--insert-placeholder
     (list :kind 'editable-field :value value :on-change on-change)
     (concat value (make-string (- width (string-width value)) ?\s)))
    (unless (and (plist-member props :newline)
                 (null (plist-get props :newline)))
      (insert "\n"))))

(defun supertag-view-widget--placeholder-ranges ()
  "Return deferred interactive ranges in reverse buffer order."
  (let ((position (point-min))
        ranges)
    (while (< position (point-max))
      (let* ((descriptor
              (get-text-property position 'supertag-widget-placeholder))
             (end (or (next-single-property-change
                       position 'supertag-widget-placeholder nil (point-max))
                      (point-max))))
        (when descriptor
          (push (list position end descriptor
                      (get-text-property position 'supertag-widget-key))
                ranges))
        (setq position end)))
    ranges))

(defun supertag-view-widget--materialize ()
  "Materialize deferred buttons and fields in the final buffer."
  (dolist (range (supertag-view-widget--placeholder-ranges))
    (pcase-let ((`(,from ,to ,descriptor ,key) range))
      (remove-text-properties
       from to '(supertag-widget-placeholder nil))
      (pcase (plist-get descriptor :kind)
        ('button
         (let ((action (plist-get descriptor :action)))
           (make-text-button
            from to
            'action (lambda (_button) (funcall action))
            'face (plist-get descriptor :face)
            'mouse-face 'highlight
            'follow-link t
            'help-echo (plist-get descriptor :help-echo))))
        ('editable-field
         (let ((on-change (plist-get descriptor :on-change))
               (value (plist-get descriptor :value))
               (width (string-width
                       (buffer-substring-no-properties from to))))
           (delete-region from to)
           (goto-char from)
           (let ((start (point)))
             (widget-create
              'editable-field
              :format "%v"
              :size (+ (length value) (- width (string-width value)))
              :keymap supertag-view-widget-field-map
              :value value
              :notify (lambda (widget &rest _ignore)
                        (let ((new-value (widget-value widget)))
                          (when (> (string-width new-value) width)
                            (user-error
                             "Editable field value exceeds width %d"
                             width))
                          (when on-change
                            (funcall on-change new-value)))))
             (remove-text-properties
              start (point) '(supertag-widget-placeholder nil))
             (when key
               (put-text-property start (point)
                                  'supertag-widget-key key)))))
        (_
         (error "Unknown Widget placeholder kind: %S"
                (plist-get descriptor :kind)))))))

(defun supertag-view-widget--render-tree (widgets context)
  "Render WIDGETS for CONTEXT and initialize native controls."
  (supertag-view-widget--clear)
  (supertag-view--render-widgets
   (if (functionp widgets) (funcall widgets context) widgets)
   context)
  (supertag-view-widget--materialize)
  (widget-setup)
  (goto-char (point-min)))

;; Built-in widgets

(supertag-widget-register 'button
  (lambda (props)
    (supertag-widget--render-action props 'button)))

(supertag-widget-register 'link
  (lambda (props)
    (supertag-widget--render-action props 'link)))

(supertag-widget-register 'editable-field
  #'supertag-widget--render-editable-field)

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
    (let ((content (plist-get props :content))
          (face (plist-get props :face))
          (start (point)))
      (insert (format "%s\n" content))
      (when face
        (add-text-properties start (point) (list 'face face))))))

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
        (insert (supertag-view--pad-line
                 (format "%s" (nth i headers)) (nth i widths))
                " "))
      (insert "\n")
      ;; Separator
      (dotimes (i (length headers))
        (insert (make-string (nth i widths) ?-)))
      (insert "\n")
      ;; Data rows
      (dolist (row rows)
        (dotimes (i (length row))
          (insert (supertag-view--pad-line
                   (format "%s" (nth i row)) (nth i widths))
                  " "))
        (insert "\n"))
      (insert "\n"))))

;; Container widgets (DSL v2)

(supertag-widget-register 'section
  (lambda (props &optional context)
    (let ((title (plist-get props :title))
          (face (plist-get props :face))
          (children (plist-get props :children)))
      (when title
        (let ((start (point)))
          (supertag-view--subheader title)
          (when face
            (add-text-properties start (point) (list 'face face)))))
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
  "Render PROPS as a simple card using CONTEXT for child widgets."
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
          (insert "│ " padded " │\n"))
        (setq is-title nil)))
    (insert (format "└%s┘\n" (make-string (+ inner-width 2) ?─)))
    (insert "\n")))

(supertag-widget-register 'card #'supertag-widget--render-card)
(supertag-widget-register 'panel #'supertag-widget--render-card)

(defun supertag-widget--render-field-table (props)
  "Render field/value pairs from PROPS in a table style."
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
  :widgets  - List of widget definitions or a function of context
  :persist  - Nil skips developer-config persistence

Widget definition:
  :type can be a symbol (header) or keyword (:header).
  Keywords are normalized to symbols at render time."
  (let* ((id (plist-get config :id))
         (name (plist-get config :name))
         (tag (plist-get config :tag))
         (widgets (plist-get config :widgets)))

    ;; Create a Runtime renderer from widgets.
    (let ((render-fn
           (lambda (context)
             (supertag-view-widget--render-tree widgets context)))
          (state-fn #'supertag-view--rebuild-context))

      ;; Register the view
      (supertag-view-register
       :id id
       :name name
       :buffer-name-fn
       (lambda (context)
         (format "*View: %s - %s*" name (plist-get context :tag)))
       :mode-fn #'supertag-view-widget-mode
       :state-fn state-fn
       :render-fn render-fn
       :capture-selection-fn #'supertag-view-widget--capture-selection
       :restore-selection-fn #'supertag-view-widget--restore-selection
       :display-action '(display-buffer-pop-up-window)
       :valid-for (when tag (list tag)))

      ;; Also store config for persistence
      (unless (and (plist-member config :persist)
                   (null (plist-get config :persist)))
        (supertag-view-config-register config))

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
                 (list :type :list :items (list "Task A" "Task B" "Task C"))))))))

;; ============================================================================
;; Initialization
;; ============================================================================

(defun supertag-view-framework-init ()
  "Initialize the view framework.
Clears registered views and stored configurations."
  (interactive)
  (clrhash supertag--view-registry)
  (clrhash supertag--view-configs)
  (message "View framework initialized"))

(provide 'supertag-view-framework)

;;; supertag-view-framework.el ends here

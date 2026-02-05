;;; supertag-view-demo-dashboard.el --- Demo dashboard view (DSL v2) -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is a working example of a DSL v2 view:
;; - Define a view using `supertag-view-define-from-config`.
;; - Use nested widgets and function bindings.
;; - Use view context helpers: :get-vc and :get-global-field.
;; - Use sample data (no user data access).
;; - Refresh manually via `M-x supertag-view-refresh`.
;;
;; Usage:
;;   (add-to-list 'load-path "/path/to/org-supertag/doc/examples/")
;;   (require 'supertag-view-demo-dashboard)
;;   M-x supertag-view-demo-dashboard-open

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'supertag-view-framework)

(defconst supertag-view-demo-dashboard--view-id 'demo-dashboard)

(defconst supertag-view-demo-dashboard--list-limit 10
  "Max number of nodes shown in the list.")

(defconst supertag-view-demo-dashboard--progress-vc-id "progress-percent"
  "Virtual column id for progress percentage.")

(defconst supertag-view-demo-dashboard--priority-field-id "priority"
  "Global field id for priority.")

(defconst supertag-view-demo-dashboard--sample-nodes
  (list
   (list :id "node-1" :title "Project Alpha" :file "/tmp/alpha.org")
   (list :id "node-2" :title "Project Beta" :file "/tmp/beta.org")
   (list :id "node-3" :title "Project Gamma")
   (list :id "node-4" :title "Project Delta" :file "/tmp/delta.org"))
  "Sample nodes used in the demo (no user data).")

(defconst supertag-view-demo-dashboard--sample-progress
  '(("node-1" . 60)
    ("node-2" . 20)
    ("node-3" . 75)
    ("node-4" . 10))
  "Sample virtual column values for progress.")

(defconst supertag-view-demo-dashboard--sample-priority
  '(("node-1" . "High")
    ("node-2" . "Low")
    ("node-4" . "Medium"))
  "Sample global field values for priority.")

(defun supertag-view-demo-dashboard--sample-get (alist key &optional default)
  "Return value for KEY in ALIST or DEFAULT if not found."
  (let ((pair (assoc key alist)))
    (if pair (cdr pair) default)))

(defun supertag-view-demo-dashboard--node-title (node)
  "Return a display title for NODE plist."
  (or (plist-get node :raw-value)
      (plist-get node :title)
      (plist-get node :id)
      "<untitled>"))

(defun supertag-view-demo-dashboard--get-vc (node-id column-id &optional default)
  "Return virtual column value for NODE-ID/COLUMN-ID from sample data."
  (if (and (stringp column-id)
           (string= column-id supertag-view-demo-dashboard--progress-vc-id))
      (supertag-view-demo-dashboard--sample-get
       supertag-view-demo-dashboard--sample-progress node-id default)
    default))

(defun supertag-view-demo-dashboard--get-global-field (node-id field-id &optional default)
  "Return global field value for NODE-ID/FIELD-ID from sample data."
  (if (and (stringp field-id)
           (string= field-id supertag-view-demo-dashboard--priority-field-id))
      (supertag-view-demo-dashboard--sample-get
       supertag-view-demo-dashboard--sample-priority node-id default)
    default))

(defun supertag-view-demo-dashboard--node-priority (context node)
  "Return priority value for NODE using CONTEXT helpers."
  (let* ((get-global-field (plist-get context :get-global-field))
         (node-id (plist-get node :id)))
    (if (and (functionp get-global-field) node-id)
        (let ((value (funcall get-global-field node-id
                              supertag-view-demo-dashboard--priority-field-id
                              nil)))
          (cond
           ((null value) "unset")
           ((and (stringp value) (string-empty-p value)) "unset")
           (t (format "%s" value))))
      "unset")))

(defun supertag-view-demo-dashboard--node-progress (context node)
  "Return progress value for NODE using CONTEXT helpers.

Falls back to NODE plist :progress if virtual column is unavailable."
  (let* ((get-vc (plist-get context :get-vc))
         (node-id (plist-get node :id))
         (vc-value (when (and (functionp get-vc) node-id)
                     (funcall get-vc node-id
                              supertag-view-demo-dashboard--progress-vc-id
                              nil))))
    (cond
     ((numberp vc-value) vc-value)
     ((numberp (plist-get node :progress)) (plist-get node :progress))
     (t nil))))

(defun supertag-view-demo-dashboard--avg-progress (context)
  "Return average progress (0-100) across nodes in CONTEXT."
  (let* ((nodes (or (plist-get context :nodes) '()))
         (values (cl-remove-if-not
                  #'numberp
                  (mapcar (lambda (node)
                            (supertag-view-demo-dashboard--node-progress context node))
                          nodes)))
         (count (length values)))
    (if (> count 0)
        (round (/ (apply #'+ values) (float count)))
      0)))

(defun supertag-view-demo-dashboard--build-stats (context)
  "Return stats list for CONTEXT.
Used by :stats binding in DSL widgets."
  (let* ((nodes (or (plist-get context :nodes) '()))
         (total (length nodes))
         (with-files
          (cl-count-if
           (lambda (node)
             (let ((file (plist-get node :file)))
               (and (stringp file) (not (string-empty-p file)))))
           nodes))
         (with-priority
          (cl-count-if
           (lambda (node)
             (let ((value (supertag-view-demo-dashboard--node-priority context node)))
               (and value (not (string= value "unset")))))
           nodes))
         (avg-progress (supertag-view-demo-dashboard--avg-progress context)))
    (list (cons "Nodes" total)
          (cons "With file" with-files)
          (cons "With priority" with-priority)
          (cons "Avg progress" (format "%d%%" avg-progress)))))

(defun supertag-view-demo-dashboard--node-widgets (context &optional limit)
  "Return a list of widget plists for nodes in CONTEXT.

LIMIT controls the number of entries; defaults to
`supertag-view-demo-dashboard--list-limit'."
  (let* ((nodes (or (plist-get context :nodes) '()))
         (limit (or limit supertag-view-demo-dashboard--list-limit))
         (sorted
          (sort (copy-sequence nodes)
                (lambda (a b)
                  (string<
                   (downcase (supertag-view-demo-dashboard--node-title a))
                   (downcase (supertag-view-demo-dashboard--node-title b))))))
         (visible (if (> (length sorted) limit)
                      (cl-subseq sorted 0 limit)
                    sorted)))
    (if (null visible)
        (list (list :type :text :content "No nodes found for this tag."))
      (mapcar
       (lambda (node)
         (let* ((title (supertag-view-demo-dashboard--node-title node))
                (file (plist-get node :file))
                (priority (supertag-view-demo-dashboard--node-priority context node))
                (progress (supertag-view-demo-dashboard--node-progress context node))
                (progress (if (numberp progress) progress 0))
                (file-label (if (and (stringp file) (not (string-empty-p file)))
                                (file-name-nondirectory file)
                              "-")))
           (list :type :stack
                 :spacing 0
                 :children
                 (list
                  (list :type :text
                        :content (format "%s [P:%s]" title priority))
                  (list :type :progress-bar
                        :value progress
                        :max 100
                        :width 22)
                  (list :type :text
                        :content (format "File: %s" file-label))))))
       visible))))

(defun supertag-view-demo-dashboard--build-context (tag)
  "Build demo context for TAG using sample data."
  (let ((tag (or tag "demo")))
    (list :tag tag
          :nodes (copy-sequence supertag-view-demo-dashboard--sample-nodes)
          :get-vc #'supertag-view-demo-dashboard--get-vc
          :get-global-field #'supertag-view-demo-dashboard--get-global-field
          :context-builder (lambda () (supertag-view-demo-dashboard--build-context tag)))))

(defconst supertag-view-demo-dashboard--config
  (list
   :id supertag-view-demo-dashboard--view-id
   :name "Demo Dashboard"
   :widgets
   (list
    (list :type :header
          :text (lambda (ctx)
                  (format "Demo Dashboard: #%s" (plist-get ctx :tag))))
    (list :type :subheader
          :text "DSL v2 demo (nested + binding + refresh)")
    (list :type :toolbar
          :items (list "g refresh"
                       "q quit"
                       "M-x supertag-view-refresh"))
    (list :type :columns
          :columns
          (list
           (list :width 30
                 :children
                 (list
                  (list :type :section
                        :title "Overview")
                  (list :type :card
                        :title "Summary"
                        :children
                        (list
                         (list :type :stats-row
                               :stats #'supertag-view-demo-dashboard--build-stats)
                         (list :type :progress-bar
                               :value #'supertag-view-demo-dashboard--avg-progress)))
                  (list :type :panel
                        :title "Key Facts"
                        :children
                        (list
                         (list :type :field
                               :items (lambda (ctx)
                                        (list
                                         (cons "Tag" (plist-get ctx :tag))
                                         (cons "Nodes" (length (plist-get ctx :nodes)))
                                         (cons "Avg progress"
                                               (format "%d%%"
                                                       (supertag-view-demo-dashboard--avg-progress ctx))))))))))
           (list :width 60
                 :children
                 (list
                  (list :type :card
                        :title "Nodes"
                        :children
                        (list
                         (list :type :stack
                               :spacing 1
                               :children (lambda (ctx)
                                           (supertag-view-demo-dashboard--node-widgets ctx))))))
                  (list :type :card
                        :title "Components"
                        :children
                        (list
                         (list :type :text
                               :content "List + Table + Badge + Empty")
                         (list :type :list
                               :items (list "List item A" "List item B" "List item C"))
                         (list :type :table
                               :headers (list "Name" "Value")
                               :rows (list (list "Alpha" "1")
                                           (list "Beta" "2")
                                           (list "Gamma" "3"))
                               :widths (list 12 10))
                         (list :type :badge
                               :items (list "High" "Medium" "Low"))
                         (list :type :empty
                               :title "Empty state"
                               :message "No data for this section."))))))
    (list :type :separator)
    (list :type :badge :text "Demo data only")
    (list :type :text :content "Hint: M-x supertag-view-refresh")))
  "View config for the DSL v2 demo dashboard.")


(defun supertag-view-demo-dashboard-open (&optional tag)
  "Open a demo dashboard for TAG.

When TAG is nil, prompt for a label (display-only)."
  (interactive)
  (let* ((tag (or tag (read-string "Demo tag: " "demo")))
         (context (supertag-view-demo-dashboard--build-context tag)))
    (supertag-view-define-from-config supertag-view-demo-dashboard--config)
    (supertag-view-render supertag-view-demo-dashboard--view-id context)))

(provide 'supertag-view-demo-dashboard)

;;; supertag-view-demo-dashboard.el ends here

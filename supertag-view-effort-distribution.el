;;; supertag-view-effort-distribution.el --- Effort distribution analysis view -*- lexical-binding: t; -*-

;;; Commentary:

;; Analyzes and visualizes effort distribution across projects/tasks.
;;
;; Grouping options:
;; - By status (done, in-progress, todo)
;; - By tag
;; - By assignee (if field exists)
;;
;; Usage:
;;   M-x supertag-view-schema
;;   Navigate to a tag
;;   Press v v
;;   Select "Effort Distribution"

;;; Code:

(require 'supertag-view-framework)
(require 'supertag-core-scan)

;; ============================================================================
;; Data Collection
;; ============================================================================

(defun supertag-view-effort--collect-by-status (tag-name)
  "Collect effort data grouped by status for TAG-NAME.
Returns an alist of (status . effort)."
  (let ((nodes (supertag-find-nodes-by-tag tag-name))
        (status-groups (list (cons "done" 0)
                            (cons "in-progress" 0)
                            (cons "todo" 0)
                            (cons "other" 0))))
    (dolist (node-pair nodes)
      (let* ((node-id (car node-pair))
             (node-data (cdr node-pair))
             (status (or (plist-get node-data :status) "other"))
             (effort (or (supertag-view--get-global-field node-id "effort" 0)
                        (supertag-view--get-global-field node-id "effort_hours" 0)
                        0)))
        ;; Normalize status
        (setq status (cond
                      ((member status '("done" "DONE" "completed" "COMPLETED")) "done")
                      ((member status '("in-progress" "IN_PROGRESS" "doing" "DOING")) "in-progress")
                      ((member status '("todo" "TODO" "pending" "PENDING")) "todo")
                      (t "other")))
        ;; Accumulate
        (let ((entry (assoc status status-groups)))
          (if entry
              (setcdr entry (+ (cdr entry) effort))
            (push (cons status effort) status-groups)))))
    status-groups))

(defun supertag-view-effort--collect-by-tag (tag-name)
  "Collect effort data for TAG-NAME and related tags.
Returns an alist of (related-tag . effort)."
  (let ((nodes (supertag-find-nodes-by-tag tag-name))
        (tag-groups nil))
    (dolist (node-pair nodes)
      (let* ((node-id (car node-pair))
             (node-data (cdr node-pair))
             (node-tags (plist-get node-data :tags))
             (effort (or (supertag-view--get-global-field node-id "effort" 0)
                        (supertag-view--get-global-field node-id "effort_hours" 0)
                        0)))
        ;; Accumulate by each tag (except the main one)
        (dolist (node-tag node-tags)
          (when (and (stringp node-tag) (not (equal node-tag tag-name)))
            (let ((entry (assoc node-tag tag-groups)))
              (if entry
                  (setcdr entry (+ (cdr entry) effort))
                (push (cons node-tag effort) tag-groups)))))))
    ;; Sort by effort descending
    (sort tag-groups (lambda (a b) (> (cdr a) (cdr b))))))

;; ============================================================================
;; Visualization
;; ============================================================================

(defun supertag-view-effort--bar-chart (label value total &optional max-width)
  "Return one text bar chart row.
LABEL is the label, VALUE is the numeric value, TOTAL is for percentage.
MAX-WIDTH is the bar width (default 30)."
  (let* ((w (or max-width 30))
         (percentage (if (> total 0) (/ (* value 100.0) total) 0))
         (filled (round (* w (/ percentage 100.0))))
         (empty (- w filled)))
    (format "%-15s [%s%s] %6.1f%% (%d)"
            label
            (make-string filled ?█)
            (make-string empty ?░)
            percentage value)))

(defun supertag-view-effort--pie-chart-text (data)
  "Return a text-based pie chart representation.
DATA is an alist of (label . value)."
  (let* ((total (cl-reduce #'+ data :key #'cdr :initial-value 0))
         (sorted (sort (copy-sequence data) (lambda (a b) (> (cdr a) (cdr b))))))
    (mapconcat (lambda (item)
                 (supertag-view-effort--bar-chart
                  (car item) (cdr item) total))
               sorted "\n")))

;; ============================================================================
;; Main View Definition
;; ============================================================================

(defun supertag-view-effort--widgets (context)
  "Return effort distribution widgets for CONTEXT."
  (let* ((tag (plist-get context :tag))
         (nodes (plist-get context :nodes))
         (by-status (supertag-view-effort--collect-by-status tag))
         (total-effort (cl-reduce #'+ by-status :key #'cdr :initial-value 0))
         (by-related-tags (supertag-view-effort--collect-by-tag tag))
         (top-tags (cl-subseq by-related-tags
                              0 (min 5 (length by-related-tags))))
         (done-effort (cdr (assoc "done" by-status)))
         (in-progress-effort (cdr (assoc "in-progress" by-status)))
         (insights
          (if (= total-effort 0)
              "Insights:\n  No effort data available."
            (concat
             "Insights:\n"
             (format "  • Completion rate: %.1f%%\n"
                     (/ (* done-effort 100.0) total-effort))
             (format "  • In progress: %.1f%%"
                     (/ (* in-progress-effort 100.0) total-effort))
             (when (> done-effort 0)
               (format "\n  • Delivered value: %d hours" done-effort))))))
    (append
     (list
      (list :type :header :text (format "Effort Distribution - #%s" tag))
      (list :type :section :title "Overview"
            :children
            (list
             (list :type :stats-row
                   :stats `(("Total Effort" . ,(format "%d hours"
                                                       total-effort))
                            ("Nodes Analyzed" . ,(length nodes))
                            ("Related Tags" . ,(length by-related-tags))))))
      (list :type :section :title "By Status"
            :children
            (list
             (if (= total-effort 0)
                 (list :type :empty :title "No effort data found.")
               (list :type :text
                     :content (supertag-view-effort--pie-chart-text
                               by-status))))))
     (when by-related-tags
       (list
        (list :type :section :title "By Related Tags (Top 5)"
              :children
              (list
               (list :type :text
                     :content (supertag-view-effort--pie-chart-text
                               top-tags))))))
     (list
      (list :type :separator)
      (list :type :text :content insights)
      (list :type :text
            :content "Tip: Ensure nodes have 'effort' or 'effort_hours' field.")))))

(supertag-view-define-from-config
 (list :id 'effort-distribution
       :name "Effort Distribution"
       :persist nil
       :widgets #'supertag-view-effort--widgets))

;; ============================================================================
;; Demo
;; ============================================================================

(defun supertag-view-effort-distribution-demo ()
  "Demonstrate the effort distribution view with mock data."
  (interactive)
  (cl-letf (((symbol-function 'supertag-find-nodes-by-tag)
             (lambda (_tag)
               (list
                (cons "task-1" (list :title "Task 1" :status "done" :tags '("project" "frontend") :effort 8))
                (cons "task-2" (list :title "Task 2" :status "done" :tags '("project" "backend") :effort 12))
                (cons "task-3" (list :title "Task 3" :status "in-progress" :tags '("project" "frontend") :effort 6))
                (cons "task-4" (list :title "Task 4" :status "todo" :tags '("project" "backend") :effort 10))
                (cons "task-5" (list :title "Task 5" :status "in-progress" :tags '("project" "backend") :effort 8))))))

    (supertag-view-open 'effort-distribution
                        (list :tag "project"
                              :nodes nil))))  ; nodes are fetched by the mock

(provide 'supertag-view-effort-distribution)

;;; supertag-view-effort-distribution.el ends here

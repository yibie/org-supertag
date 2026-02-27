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
        (dolist (t node-tags)
          (when (and (stringp t) (not (equal t tag-name)))
            (let ((entry (assoc t tag-groups)))
              (if entry
                  (setcdr entry (+ (cdr entry) effort))
                (push (cons t effort) tag-groups)))))))
    ;; Sort by effort descending
    (sort tag-groups (lambda (a b) (> (cdr a) (cdr b))))))

;; ============================================================================
;; Visualization
;; ============================================================================

(defun supertag-view-effort--bar-chart (label value total &optional max-width)
  "Draw a text bar chart.
LABEL is the label, VALUE is the numeric value, TOTAL is for percentage.
MAX-WIDTH is the bar width (default 30)."
  (let* ((w (or max-width 30))
         (percentage (if (> total 0) (/ (* value 100.0) total) 0))
         (filled (round (* w (/ percentage 100.0))))
         (empty (- w filled)))
    (insert (format "%-15s " label))
    (insert "[")
    (insert (make-string filled ?█))
    (insert (make-string empty ?░))
    (insert (format "] %6.1f%% (%d)\n" percentage value))))

(defun supertag-view-effort--pie-chart-text (data)
  "Draw a text-based pie chart representation.
DATA is an alist of (label . value)."
  (let* ((total (cl-reduce #'+ data :key #'cdr :initial-value 0))
         (sorted (sort (copy-sequence data) (lambda (a b) (> (cdr a) (cdr b))))))
    (dolist (item sorted)
      (supertag-view-effort--bar-chart (car item) (cdr item) total))))

;; ============================================================================
;; Main View Definition
;; ============================================================================

(define-supertag-view effort-distribution "Effort Distribution"
  ;; Analyzes effort distribution by status and related tags.
  (tag nodes)

  (let* ((by-status (supertag-view-effort--collect-by-status tag))
         (total-effort (cl-reduce #'+ by-status :key #'cdr :initial-value 0))
         (by-related-tags (supertag-view-effort--collect-by-tag tag))
         (top-tags (cl-subseq by-related-tags 0 (min 5 (length by-related-tags)))))

    (supertag-view--with-buffer "Effort Distribution" tag
      ;; Header
      (supertag-view--header (format "Effort Distribution - #%s" tag))

      ;; Overall stats
      (supertag-view--subheader "Overview")
      (supertag-view--stat-row
       `(("Total Effort" . ,(format "%d hours" total-effort))
         ("Nodes Analyzed" . ,(length nodes))
         ("Related Tags" . ,(length by-related-tags))))

      ;; By status
      (supertag-view--subheader "By Status")
      (if (= total-effort 0)
          (insert "No effort data found.\n")
        (supertag-view-effort--pie-chart-text by-status))

      ;; By related tags (if any)
      (when (> (length by-related-tags) 0)
        (supertag-view--subheader "By Related Tags (Top 5)")
        (supertag-view-effort--pie-chart-text top-tags))

      ;; Summary insights
      (supertag-view--separator)
      (insert "Insights:\n")
      (let ((done-effort (cdr (assoc "done" by-status)))
            (in-progress-effort (cdr (assoc "in-progress" by-status))))
        (if (= total-effort 0)
            (insert "  No effort data available.\n")
          (insert (format "  • Completion rate: %.1f%%\n"
                         (/ (* done-effort 100.0) total-effort)))
          (insert (format "  • In progress: %.1f%%\n"
                         (/ (* in-progress-effort 100.0) total-effort)))
          (when (> done-effort 0)
            (insert (format "  • Delivered value: %d hours\n" done-effort))))))

      ;; Help
      (insert "\n")
      (insert "Tip: Ensure nodes have 'effort' or 'effort_hours' field.\n")))

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
                (cons "task-5" (list :title "Task 5" :status "in-progress" :tags '("project" "backend") :effort 8)))))

    (supertag-view-render 'effort-distribution
                         (list :tag "project"
                               :nodes nil)))))  ; nodes are fetched by the mock

(provide 'supertag-view-effort-distribution)

;;; supertag-view-effort-distribution.el ends here

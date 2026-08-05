;;; supertag-view-progress-dashboard.el --- Progress dashboard view -*- lexical-binding: t; -*-

;;; Commentary:

;; A production-ready progress dashboard view for org-supertag.
;;
;; This view displays projects with their progress bars, task counts,
;; and effort totals.  It integrates with virtual columns for dynamic data.
;;
;; Requirements:
;; - Virtual columns: "progress", "total-tasks", "done-tasks", "total-effort"
;;   (if these don't exist, the view will show N/A)
;;
;; Usage:
;;   M-x supertag-view-schema
;;   Navigate to #project
;;   Press v v
;;   Select "Progress Dashboard"

;;; Code:

(require 'supertag-view-framework)
(require 'supertag-core-scan)

;; ============================================================================
;; Data Collection
;; ============================================================================

(defun supertag-view-progress--collect-data (tag-name)
  "Collect project data for TAG-NAME.
Returns a list of project data plists."
  (let ((nodes (supertag-find-nodes-by-tag tag-name)))
    (mapcar
     (lambda (node-pair)
       (let* ((node-id (car node-pair))
              (node-data (cdr node-pair))
              (title (or (plist-get node-data :title) "Untitled"))
              ;; Get virtual column values
              (progress (supertag-view--get-vc node-id "progress" 0))
              (total-tasks (supertag-view--get-vc node-id "total-tasks" 0))
              (done-tasks (supertag-view--get-vc node-id "done-tasks" 0))
              (total-effort (supertag-view--get-vc node-id "total-effort" 0)))

         (list :id node-id
               :title title
               :progress progress
               :total-tasks total-tasks
               :done-tasks done-tasks
               :total-effort total-effort
               :status (cond
                       ((= progress 100) 'completed)
                       ((> progress 75) 'on-track)
                       ((> progress 50) 'in-progress)
                       ((> progress 0) 'started)
                       (t 'not-started)))))
     nodes)))

(defun supertag-view-progress--status-indicator (status)
  "Get visual indicator for STATUS."
  (pcase status
    ('completed "✓")
    ('on-track "▶")
    ('in-progress "○")
    ('started "◐")
    ('not-started "·")
    (_ "?")))

;; ============================================================================
;; Main View Definition
;; ============================================================================

(defun supertag-view-progress--widgets (context)
  "Return progress dashboard widgets for CONTEXT."
  (let* ((tag (plist-get context :tag))
         (projects (supertag-view-progress--collect-data tag))
         (total-projects (length projects))
         (completed-count
          (cl-count-if (lambda (project)
                         (eq (plist-get project :status) 'completed))
                       projects))
         (in-progress-count
          (cl-count-if (lambda (project)
                         (memq (plist-get project :status)
                               '(on-track in-progress)))
                       projects))
         (total-effort-all
          (cl-reduce #'+ projects
                     :key (lambda (project)
                            (plist-get project :total-effort))
                     :initial-value 0))
         (rows
          (mapcar
           (lambda (project)
             (let* ((progress (plist-get project :progress))
                    (filled (round (* 10 (/ progress 100.0))))
                    (total-tasks (plist-get project :total-tasks))
                    (effort (plist-get project :total-effort)))
               (list
                (supertag-view-progress--status-indicator
                 (plist-get project :status))
                (plist-get project :title)
                (format "[%s%s] %d%%"
                        (make-string filled ?█)
                        (make-string (- 10 filled) ?░)
                        progress)
                (if (> total-tasks 0)
                    (format "%d/%d" (plist-get project :done-tasks)
                            total-tasks)
                  "N/A")
                (if (> effort 0) (format "%d h" effort) "N/A"))))
           projects)))
    (list
     (list :type :header :text (format "Progress Dashboard - #%s" tag))
     (list :type :section :title "Summary"
           :children
           (list
            (list :type :stats-row
                  :stats `(("Total Projects" . ,total-projects)
                           ("Completed" . ,completed-count)
                           ("In Progress" . ,in-progress-count)
                           ("Total Effort" . ,(format "%d hours"
                                                      total-effort-all))))))
     (list :type :section :title "Projects"
           :children
           (list
            (if projects
                (list :type :table
                      :headers '("Status" "Project" "Progress" "Tasks" "Effort")
                      :widths '(8 25 18 10 8)
                      :rows rows)
              (list :type :empty :title "No projects found."))))
     (list :type :separator)
     (list :type :text
           :content
           (concat
            "Legend: ✓ Completed  ▶ On Track  ○ In Progress  ◐ Started  · Not Started\n\n"
            "Tip: Set up virtual columns to see live data:\n"
            "  - 'progress' or 'progress-percent': completion percentage\n"
            "  - 'total-tasks', 'done-tasks': task counts\n"
            "  - 'total-effort': effort in hours")))))

(supertag-view-define-from-config
 (list :id 'progress-dashboard
       :name "Progress Dashboard"
       :persist nil
       :widgets #'supertag-view-progress--widgets))

;; ============================================================================
;; Demo
;; ============================================================================

(defun supertag-view-progress-dashboard-demo ()
  "Demonstrate the progress dashboard with mock data."
  (interactive)
  ;; Create mock virtual column functions if not available
  (cl-letf (((symbol-function 'supertag-view--get-vc)
             (lambda (node-id column-id &optional default)
               ;; Mock data based on node-id
               (pcase node-id
                 ("proj-1" (pcase column-id
                            ("progress" 100)
                            ("total-tasks" 10)
                            ("done-tasks" 10)
                            ("total-effort" 80)
                            (_ default)))
                 ("proj-2" (pcase column-id
                            ("progress" 65)
                            ("total-tasks" 20)
                            ("done-tasks" 13)
                            ("total-effort" 120)
                            (_ default)))
                 ("proj-3" (pcase column-id
                            ("progress" 30)
                            ("total-tasks" 15)
                            ("done-tasks" 5)
                            ("total-effort" 60)
                            (_ default)))
                 (_ default)))))

    (supertag-view-open 'progress-dashboard
                        (list :tag "project"
                              :nodes (list
                                      (list :id "proj-1" :title "Website Redesign")
                                      (list :id "proj-2" :title "Mobile App Development")
                                      (list :id "proj-3" :title "Database Migration"))))))

(provide 'supertag-view-progress-dashboard)

;;; supertag-view-progress-dashboard.el ends here

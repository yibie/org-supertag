;;; supertag-view-progress-dashboard.el --- Progress dashboard view -*- lexical-binding: t; -*-

;;; Commentary:

;; A production-ready progress dashboard view for org-supertag.
;;
;; This view displays projects with their progress bars, task counts,
;; and effort totals. It integrates with virtual columns for dynamic data.
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

(defun supertag-view-progress--status-face (status)
  "Get face for STATUS (for color)."
  (pcase status
    ('completed 'success)
    ('on-track '(:foreground "green"))
    ('in-progress '(:foreground "blue"))
    ('started '(:foreground "orange"))
    ('not-started '(:foreground "gray"))
    (_ 'default)))

;; ============================================================================
;; Main View Definition
;; ============================================================================

(define-supertag-view progress-dashboard "Progress Dashboard"
  ;; A dashboard showing project progress with task counts and effort totals.
  ;; Requires virtual columns: progress, total-tasks, done-tasks, total-effort
  (tag nodes)

  ;; Collect real data from the database
  (let* ((projects (supertag-view-progress--collect-data tag))
         (total-projects (length projects))
         (completed-count (cl-count-if (lambda (p) (eq (plist-get p :status) 'completed)) projects))
         (in-progress-count (cl-count-if (lambda (p) (memq (plist-get p :status) '(on-track in-progress))) projects))
         (total-effort-all (cl-reduce #'+ projects :key (lambda (p) (plist-get p :total-effort)) :initial-value 0)))

    (supertag-view--with-buffer "Progress Dashboard" tag
      ;; Header
      (supertag-view--header (format "Progress Dashboard - #%s" tag))

      ;; Summary section
      (supertag-view--subheader "Summary")
      (supertag-view--stat-row
       `(("Total Projects" . ,total-projects)
         ("Completed" . ,completed-count)
         ("In Progress" . ,in-progress-count)
         ("Total Effort" . ,(format "%d hours" total-effort-all))))

      ;; Projects list
      (supertag-view--subheader "Projects")

      (if (null projects)
          (insert "No projects found.\n")

        ;; Table header
        (insert "Status  Project                    Progress    Tasks      Effort\n")
        (insert "──────  ─────────────────────────  ──────────  ─────────  ───────\n")

        ;; Each project
        (dolist (project projects)
          (let* ((status (plist-get project :status))
                 (indicator (supertag-view-progress--status-indicator status))
                 (title (plist-get project :title))
                 (progress (plist-get project :progress))
                 (total-tasks (plist-get project :total-tasks))
                 (done-tasks (plist-get project :done-tasks))
                 (effort (plist-get project :total-effort))
                 ;; Format fields
                 (display-title (if (> (length title) 24)
                                   (concat (substring title 0 21) "...")
                                 (format "%-24s" title)))
                 (task-str (if (> total-tasks 0)
                              (format "%d/%d" done-tasks total-tasks)
                            "N/A"))
                 (effort-str (if (> effort 0)
                                (format "%d h" effort)
                              "N/A")))

            ;; Status indicator
            (insert (format "  %s    " indicator))

            ;; Title
            (insert display-title)
            (insert "  ")

            ;; Progress bar (short)
            (let ((bar-width 10))
              (insert "[")
              (let* ((filled (round (* bar-width (/ progress 100.0))))
                     (empty (- bar-width filled)))
                (insert (make-string filled ?█))
                (insert (make-string empty ?░)))
              (insert (format "] %3d%%  " progress)))

            ;; Tasks
            (insert (format "%-8s  " task-str))

            ;; Effort
            (insert effort-str)

            (insert "\n"))))

      ;; Help text
      (supertag-view--separator)
      (insert "Legend: ✓ Completed  ▶ On Track  ○ In Progress  ◐ Started  · Not Started\n")
      (insert "\n")
      (insert "Tip: Set up virtual columns to see live data:\n")
      (insert "  - 'progress' or 'progress-percent': completion percentage\n")
      (insert "  - 'total-tasks', 'done-tasks': task counts\n")
      (insert "  - 'total-effort': effort in hours\n"))))

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

    (supertag-view-render 'progress-dashboard
                         (list :tag "project"
                               :nodes (list
                                      (list :id "proj-1" :title "Website Redesign")
                                      (list :id "proj-2" :title "Mobile App Development")
                                      (list :id "proj-3" :title "Database Migration"))))))

(provide 'supertag-view-progress-dashboard)

;;; supertag-view-progress-dashboard.el ends here

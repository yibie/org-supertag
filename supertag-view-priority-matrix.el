;;; supertag-view-priority-matrix.el --- Priority matrix view -*- lexical-binding: t; -*-

;;; Commentary:

;; Displays tasks in a 2x2 priority matrix (Eisenhower Matrix):
;;   - X-axis: Urgency (Low -> High)
;;   - Y-axis: Importance (Low -> High)
;;
;; Quadrants:
;;   1. Important & Urgent (DO)       - Crisis, deadlines
;;   2. Important & Not Urgent (PLAN) - Strategy, planning
;;   3. Not Important & Urgent (DELEGATE) - Interruptions
;;   4. Not Important & Not Urgent (DELETE) - Time wasters
;;
;; Usage:
;;   M-x supertag-view-schema
;;   Navigate to #task
;;   Press v v
;;   Select "Priority Matrix"
;;
;; Requirements:
;;   - 'priority' or 'urgency' and 'importance' fields
;;   - Or uses todo state and custom fields

;;; Code:

(require 'supertag-view-framework)
(require 'supertag-core-scan)

;; ============================================================================
;; Priority Calculation
;; ============================================================================

(defun supertag-view-priority--get-urgency (node-data)
  "Extract urgency from NODE-DATA (0-10 scale)."
  (or (plist-get node-data :urgency)
      (plist-get node-data :Urgency)
      ;; Infer from deadline
      (let ((deadline (plist-get node-data :deadline)))
        (if deadline
            (let* ((deadline-time (org-time-string-to-time deadline))
                   (days-diff (/ (float-time (time-subtract deadline-time (current-time))) 86400)))
              (cond
               ((< days-diff 2) 9)   ; Very urgent
               ((< days-diff 7) 7)   ; Urgent
               ((< days-diff 14) 5)  ; Moderate
               (t 3)))               ; Not urgent
          5))                       ; Default
      5))                           ; Fallback

(defun supertag-view-priority--get-importance (node-data)
  "Extract importance from NODE-DATA (0-10 scale)."
  (or (plist-get node-data :importance)
      (plist-get node-data :Importance)
      ;; Infer from priority tag or field
      (let ((priority (plist-get node-data :priority)))
        (pcase priority
          ('"A" 9)
          ('"B" 6)
          ('"C" 3)
          (_ 5)))
      5))

(defun supertag-view-priority--classify (node-data)
  "Classify NODE-DATA into quadrant.
Returns one of: do, plan, delegate, delete"
  (let ((urgency (supertag-view-priority--get-urgency node-data))
        (importance (supertag-view-priority--get-importance node-data)))
    (cond
     ((and (>= importance 6) (>= urgency 6)) 'do)
     ((and (>= importance 6) (< urgency 6)) 'plan)
     ((and (< importance 6) (>= urgency 6)) 'delegate)
     (t 'delete))))

;; ============================================================================
;; Matrix Building
;; ============================================================================

(defun supertag-view-priority--build-matrix (tag-name)
  "Build priority matrix for TAG-NAME.
Returns an alist of (quadrant . tasks)."
  (let ((nodes (supertag-find-nodes-by-tag tag-name))
        (matrix (list (cons 'do nil)
                     (cons 'plan nil)
                     (cons 'delegate nil)
                     (cons 'delete nil))))
    (dolist (node-pair nodes)
      (let* ((node-id (car node-pair))
             (node-data (cdr node-pair))
             (quadrant (supertag-view-priority--classify node-data))
             (task-data (list :id node-id
                             :title (or (plist-get node-data :title) "Untitled")
                             :urgency (supertag-view-priority--get-urgency node-data)
                             :importance (supertag-view-priority--get-importance node-data))))
        (push task-data (cdr (assoc quadrant matrix)))))
    ;; Sort each quadrant by urgency * importance score
    (dolist (entry matrix)
      (setcdr entry (sort (cdr entry)
                         (lambda (a b)
                           (> (+ (* (plist-get a :urgency) (plist-get a :importance)))
                              (+ (* (plist-get b :urgency) (plist-get b :importance))))))))
    matrix))

;; ============================================================================
;; Display
;; ============================================================================

(defun supertag-view-priority--quadrant-title (quadrant)
  "Get title for QUADRANT."
  (pcase quadrant
    ('do "DO (Do First)")
    ('plan "PLAN (Schedule)")
    ('delegate "DELEGATE (Assign)")
    ('delete "DELETE (Eliminate)")
    (_ "Unknown")))

(defun supertag-view-priority--quadrant-color (quadrant)
  "Get color for QUADRANT."
  (pcase quadrant
    ('do '(:foreground "red" :weight bold))
    ('plan '(:foreground "blue"))
    ('delegate '(:foreground "orange"))
    ('delete '(:foreground "gray"))
    (_ 'default)))

(defun supertag-view-priority--format-task (task max-title-len)
  "Format TASK for display.
MAX-TITLE-LEN is the maximum title length."
  (let ((title (plist-get task :title))
        (urgency (plist-get task :urgency))
        (importance (plist-get task :importance)))
    ;; Truncate title if needed
    (when (> (length title) max-title-len)
      (setq title (concat (substring title 0 (- max-title-len 3)) "...")))
    (format "  %-30s  U:%d I:%d" title urgency importance)))

;; ============================================================================
;; Main View Definition
;; ============================================================================

(define-supertag-view priority-matrix "Priority Matrix"
  ;; Eisenhower Matrix: prioritize tasks by urgency and importance.
  (tag nodes)
  
  (let* ((matrix (supertag-view-priority--build-matrix tag))
         (do-tasks (cdr (assoc 'do matrix)))
         (plan-tasks (cdr (assoc 'plan matrix)))
         (delegate-tasks (cdr (assoc 'delegate matrix)))
         (delete-tasks (cdr (assoc 'delete matrix)))
         (total-tasks (+ (length do-tasks) (length plan-tasks)
                        (length delegate-tasks) (length delete-tasks))))
    
    (supertag-view--with-buffer "Priority Matrix" tag
      ;; Header
      (supertag-view--header (format "Priority Matrix - #%s" tag))
      (insert "Eisenhower Matrix: Urgency × Importance\n\n")
      
      ;; Summary
      (supertag-view--stat-row
       `(("Total Tasks" . ,total-tasks)
         ("DO (Urgent+Important)" . ,(length do-tasks))
         ("PLAN (Important)" . ,(length plan-tasks))
         ("DELEGATE (Urgent)" . ,(length delegate-tasks))
         ("DELETE (Neither)" . ,(length delete-tasks))))
      
      ;; The Matrix (2x2 grid)
      (supertag-view--subheader "The Matrix")
      
      ;; Show each quadrant
      (dolist (quadrant '(do plan delegate delete))
        (let ((tasks (cdr (assoc quadrant matrix)))
              (title (supertag-view-priority--quadrant-title quadrant)))
          
          ;; Quadrant header with color
          (insert (propertize (format "\n%s\n" title)
                             'face (supertag-view-priority--quadrant-color quadrant)))
          (insert (make-string (+ 2 (length title)) ?─))
          (insert "\n")
          
          ;; Tasks in this quadrant
          (if (null tasks)
              (insert "  (no tasks)\n")
            (dolist (task (cl-subseq tasks 0 (min 10 (length tasks))))
              (insert (supertag-view-priority--format-task task 25))
              (insert "\n"))
            (when (> (length tasks) 10)
              (insert (format "  ... and %d more\n" (- (length tasks) 10))))))
      
      ;; Legend and help
      (supertag-view--separator)
      (insert "How to use:\n")
      (insert "  1. DO: Do these tasks immediately\n")
      (insert "  2. PLAN: Schedule time for these\n")
      (insert "  3. DELEGATE: Assign to someone else\n")
      (insert "  4. DELETE: Consider eliminating these\n")
      (insert "\n")
      (insert "Set 'urgency' and 'importance' fields (0-10) on nodes for accurate classification.\n")))))

;; ============================================================================
;; Demo
;; ============================================================================

(defun supertag-view-priority-matrix-demo ()
  "Demonstrate the priority matrix with mock data."
  (interactive)
  (cl-letf (((symbol-function 'supertag-find-nodes-by-tag)
             (lambda (_tag)
               (list
                ;; DO: High urgency, high importance
                (cons "task-1" (list :title "Fix critical bug" :urgency 9 :importance 10))
                (cons "task-2" (list :title "Client deadline today" :urgency 10 :importance 8))
                ;; PLAN: Low urgency, high importance
                (cons "task-3" (list :title "Q4 planning" :urgency 3 :importance 9))
                (cons "task-4" (list :title "Refactor codebase" :urgency 2 :importance 8))
                ;; DELEGATE: High urgency, low importance
                (cons "task-5" (list :title "Answer email" :urgency 7 :importance 2))
                (cons "task-6" (list :title "Schedule meeting" :urgency 6 :importance 3))
                ;; DELETE: Low urgency, low importance
                (cons "task-7" (list :title "Check social media" :urgency 1 :importance 1))
                (cons "task-8" (list :title "Organize desktop" :urgency 2 :importance 2)))))
    
    (supertag-view-render 'priority-matrix
                         (list :tag "task"
                               :nodes nil)))))

(provide 'supertag-view-priority-matrix)

;;; supertag-view-priority-matrix.el ends here

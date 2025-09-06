;;; supertag-services-scheduler.el --- Unified task scheduler for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module provides a unified, robust scheduler for managing all background
;; and periodic tasks within Org-Supertag. It replaces scattered timer
;; implementations with a centralized, efficient system.
;;
;; Features:
;; - Centralized task registry with single master timer
;; - Support for :interval (periodic) and :daily (time-based) tasks
;; - Automatic handling of missed daily task executions
;; - State persistence across Emacs sessions

;;; Code:

(require 'cl-lib)
(require 'supertag-core-persistence) ; For supertag-data-directory

;;; === Configuration ===

(defcustom supertag-scheduler-check-interval 300
  "Interval in seconds for master timer to check for pending tasks.
300 seconds (5 minutes) provides good balance between accuracy and resource usage."
  :type 'integer
  :group 'supertag-services)

;;; === Core State Management ===

(defvar supertag-scheduler--tasks (make-hash-table :test 'equal)
  "Central registry for all scheduled tasks.
Key: Unique task ID (symbol)
Value: Task plist with :type, :function, and scheduling parameters")

(defvar supertag-scheduler--master-timer nil
  "Master timer that periodically calls `supertag-scheduler--check-tasks`.")

(defvar supertag-scheduler--state-file
  (supertag-data-file "scheduler-state.json")
  "File to persist state of daily tasks (last run times).")

;;; === State Persistence ===

(defun supertag-scheduler--save-state ()
  "Save task states (last-run times) to persistent storage."
  (require 'json)
  (let ((state (make-hash-table :test 'equal)))
    (maphash (lambda (id task)
               (when-let ((last-run (plist-get task :last-run)))
                 (puthash (symbol-name id) last-run state)))
             supertag-scheduler--tasks)
    (with-temp-buffer
      (insert (json-encode state))
      (write-file supertag-scheduler--state-file nil))))

(defun supertag-scheduler--load-state ()
  "Load task states from persistent storage."
  (when (file-exists-p supertag-scheduler--state-file)
    (require 'json)
    (let ((json-content (with-temp-buffer
                          (insert-file-contents supertag-scheduler--state-file)
                          (buffer-string))))
      (when (> (length (string-trim json-content)) 0)
        (let ((state (json-read-from-string json-content)))
          (maphash (lambda (id task)
                     (let ((last-run (gethash (symbol-name id) state)))
                       (when last-run
                         (plist-put task :last-run last-run)
                         (puthash id task supertag-scheduler--tasks))))
                   supertag-scheduler--tasks))))))

;;; === Public API ===

(defun supertag-scheduler-register-task (id type function &rest args)
  "Register a task with the central scheduler.
ID: Unique symbol identifying the task
TYPE: :interval or :daily
FUNCTION: Function to call when task runs
ARGS: Plist with scheduling parameters:
  - :interval seconds for interval tasks
  - :time HH:MM string for daily tasks"
  (let ((task (list :type type :function function)))
    (pcase type
      (:interval
       (let ((interval (plist-get args :interval)))
         (unless (and (integerp interval) (> interval 0))
           (error "Invalid interval for task %s" id))
         (setq task (plist-put task :interval interval))))
      (:daily
       (let ((time (plist-get args :time))
             (days (plist-get args :days-of-week)))
         (unless (and (stringp time) (string-match "^[0-2][0-9]:[0-5][0-9]$" time))
           (error "Invalid time format for task %s. Must be HH:MM" id))
         (setq task (plist-put task :time time))
         (when days
           (setq task (plist-put task :days-of-week days))))))
    (puthash id task supertag-scheduler--tasks)
    (message "[Supertag Scheduler] Task '%s' registered." id)))

(defun supertag-scheduler-deregister-task (id)
  "Remove a task from the scheduler.
ID: Unique symbol of task to remove."
  (remhash id supertag-scheduler--tasks)
  (message "[Supertag Scheduler] Task '%s' deregistered." id))

(defun supertag-scheduler-start ()
  "Start the master scheduler timer.
Should be called once during Supertag initialization."
  (interactive)
  (unless (timerp supertag-scheduler--master-timer)
    (supertag-scheduler--load-state)
    (setq supertag-scheduler--master-timer
          (run-with-timer 0
                          supertag-scheduler-check-interval
                          #'supertag-scheduler--check-tasks))
    (message "Supertag Scheduler started.")))

(defun supertag-scheduler-stop ()
  "Stop the master scheduler timer."
  (interactive)
  (when (timerp supertag-scheduler--master-timer)
    (cancel-timer supertag-scheduler--master-timer)
    (setq supertag-scheduler--master-timer nil)
    (supertag-scheduler--save-state)
    (message "Supertag Scheduler stopped.")))

(defun supertag-scheduler-list-tasks ()
  "Display all registered tasks and their status."
  (interactive)
  (let ((tasks '()))
    (maphash (lambda (id task)
               (push (format "- %s: %s" id task) tasks))
             supertag-scheduler--tasks)
    (message "[Supertag Scheduler] Registered tasks:\n%s" 
             (mapconcat #'identity (nreverse tasks) "\n"))))

;;; === Core Scheduling Logic ===

(defun supertag-scheduler--check-tasks ()
  "Master timer function - check and execute pending tasks.
Prevents thundering herd by running only one daily task per cycle."
  (let ((now (current-time))
        (daily-task-ran nil))
    (maphash
     (lambda (id task)
       (pcase (plist-get task :type)
         (:interval
          (let ((interval (plist-get task :interval))
                (last-run (plist-get task :last-run)))
            (when (or (not last-run)
                      (>= (time-to-seconds (time-subtract now last-run)) interval))
              (supertag-scheduler--run-task id task now))))
         (:daily
          (unless daily-task-ran
            (let* ((today (format-time-string "%Y-%m-%d" now))
                   (current-time (format-time-string "%H:%M" now))
                   (scheduled-time (plist-get task :time))
                   (last-run (plist-get task :last-run)))
              (when (and (string-greaterp current-time scheduled-time)
                         (not (equal last-run today)))
                (supertag-scheduler--run-task id task now)
                (setq daily-task-ran t)))))))
     supertag-scheduler--tasks)))

(defun supertag-scheduler--run-task (id task now)
  "Execute a task and update its state."
  (let ((func (plist-get task :function)))
    (message "[Supertag Scheduler] Running task '%s'..." id)
    (funcall func)
    ;; Update last-run time
    (pcase (plist-get task :type)
      (:interval (plist-put task :last-run now))
      (:daily (plist-put task :last-run (format-time-string "%Y-%m-%d" now))))
    (puthash id task supertag-scheduler--tasks)
    ;; Persist state after daily tasks
    (when (eq (plist-get task :type) :daily)
      (supertag-scheduler--save-state))))

(provide 'supertag-services-scheduler)

;;; supertag-services-scheduler.el ends here
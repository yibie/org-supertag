;;; org-supertag-scheduler.el --- A unified task scheduler for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module provides a unified, robust scheduler for managing all background
;; and periodic tasks within org-supertag. It replaces the scattered and
;; inconsistent timer implementations in various modules.
;;
;; Features:
;; - Centralized task registry.
;; - A single, master timer to reduce overhead.
;; - Support for two distinct task types:
;;   - :interval: For tasks that run every N seconds (relative time).
;;   - :daily: For tasks that run once per day at a specific time (absolute time).
;; - Handles missed executions for daily tasks (e.g., if Emacs is not running).
;; - Persists state for daily tasks across Emacs sessions.

;;; Code:

(require 'cl-lib)

;;; === Customization ===

(defcustom org-supertag-scheduler-check-interval 300
  "The interval in seconds for the master timer to check for pending tasks.
A shorter interval provides more accuracy for scheduled tasks but uses
more resources. 300 seconds (5 minutes) is a reasonable default."
  :type 'integer
  :group 'org-supertag)

;;; === Core Variables ===

(defvar org-supertag-scheduler--tasks (make-hash-table :test 'equal)
  "The central registry for all scheduled tasks.
Key: A unique task ID (symbol).
Value: A plist defining the task, e.g.:
'(:type :daily
  :time \"03:00\"
  :funtion #'my-task-function
  :last-run \"2023-10-28\")")

(defvar org-supertag-scheduler--master-timer nil
  "The single master timer that periodically calls `org-supertag-scheduler--check-tasks`.")

(defvar org-supertag-scheduler--state-file
  (expand-file-name "scheduler-state.json" org-supertag-data-directory)
  "File to persist the state of daily tasks (e.g., last run times).")


;;; === State Management ===

(defun org-supertag-scheduler--save-state ()
  "Save the state of all tasks (specifically last-run times) to a file."
  (require 'json)
  (let ((state-to-save (make-hash-table :test 'equal)))
    (maphash (lambda (id task-plist)
               (when-let ((last-run (plist-get task-plist :last-run)))
                 (puthash (symbol-name id) last-run state-to-save)))
             org-supertag-scheduler--tasks)
    (with-temp-buffer
      (json-encode state-to-save)
      (write-file org-supertag-scheduler--state-file nil))))

(defun org-supertag-scheduler--load-state ()
  "Load task states from the state file and update the in-memory registry."
  (when (file-exists-p org-supertag-scheduler--state-file)
    (require 'json)
    (let ((json-string (with-temp-buffer
                         (insert-file-contents org-supertag-scheduler--state-file)
                         (buffer-string))))
      (when (and json-string (> (length (string-trim json-string)) 0))
        (let ((state (json-read-from-string json-string)))
          (maphash (lambda (id task-plist)
                     (let* ((id-str (symbol-name id))
                            (last-run (cdr (assoc-string id-str state))))
                       (when last-run
                         (plist-put task-plist :last-run last-run)
                         (puthash id task-plist org-supertag-scheduler--tasks))))
                   org-supertag-scheduler--tasks))))))


;;; === Public API ===

(defun org-supertag-scheduler-register-task (id type function &rest args)
  "Register a task with the central scheduler.
ID: A unique symbol to identify the task (e.g., 'auto-tag-scan).
TYPE: The type of task, either :interval or :daily.
FUNCTION: The function to call when the task runs.
ARGS: A plist of options depending on TYPE.
  - For :interval: :interval (seconds)
  - For :daily: :time (string in \"HH:MM\" format)

If a task with the same ID already exists, it will be updated."
  (let ((task-plist (list :type type :function function)))
    (pcase type
      (:interval
       (let ((interval (plist-get args :interval)))
         (unless (and (integerp interval) (> interval 0))
           (error "Invalid interval for task %s" id))
         (setq task-plist (plist-put task-plist :interval interval))))
      (:daily
       (let ((time (plist-get args :time)))
         (unless (and (stringp time) (string-match "^[0-2][0-9]:[0-5][0-9]$" time))
           (error "Invalid time format for task %s. Must be HH:MM" id))
         (setq task-plist (plist-put task-plist :time time)))))
    (puthash id task-plist org-supertag-scheduler--tasks)
    (message "[Org-supertag-scheduler] Task '%s' registered." id)))

(defun org-supertag-scheduler-deregister-task (id)
  "Remove a task from the scheduler.
ID: The unique symbol of the task to remove."
  (remhash id org-supertag-scheduler--tasks)
  (message "[Org-supertag-scheduler] Task '%s' deregistered." id))

(defun org-supertag-scheduler-start ()
  "Start the master scheduler timer.
This function should be called once when org-supertag initializes."
  (interactive)
  (unless (timerp org-supertag-scheduler--master-timer)
    (org-supertag-scheduler--load-state)
    (setq org-supertag-scheduler--master-timer
          (run-with-timer 0
                          org-supertag-scheduler-check-interval
                          #'org-supertag-scheduler--check-tasks))
    (message "Org SuperTag Scheduler started.")))

(defun org-supertag-scheduler-stop ()
  "Stop the master scheduler timer."
  (interactive)
  (when (timerp org-supertag-scheduler--master-timer)
    (cancel-timer org-supertag-scheduler--master-timer)
    (setq org-supertag-scheduler--master-timer nil)
    (org-supertag-scheduler--save-state)
    (message "Org SuperTag Scheduler stopped.")))

(defun org-supertag-scheduler-list-tasks ()
  "Display a list of all registered tasks and their status."
  (interactive)
  (let ((tasks-info '()))
    (maphash (lambda (id task)
               (push (format "- %s: %s" id task) tasks-info))
             org-supertag-scheduler--tasks)
    (message "[Org-supertag-scheduler] Registered tasks:\n%s" (mapconcat #'identity (nreverse tasks-info) "\n"))))


;;; === Internal Functions ===

(defun org-supertag-scheduler--check-tasks ()
  "The core function called by the master timer.
Iterates through all registered tasks and runs them if their conditions are met.
To prevent a 'thundering herd' on startup, it will only run ONE missed
daily task per cycle, ensuring a smoother startup experience."
  ;;(message "[Scheduler-Debug] Master timer running check-tasks at %s" (format-time-string "%H:%M:%S"))
  (let ((now (current-time))
        (daily-task-ran-p nil)) ; Flag to ensure only one daily task runs per cycle
    (maphash
     (lambda (id task)
       ;; Only check remaining tasks if a daily task hasn't already run in this cycle.
       ;; Interval tasks are lightweight and can always run.
       (pcase (plist-get task :type)
         (:interval
          (let ((interval (plist-get task :interval))
                (last-run (plist-get task :last-run)))
            (when (or (not last-run)
                      (>= (time-to-seconds (time-subtract now last-run)) interval))
              (org-supertag-scheduler--run-task id task now))))
         (:daily
          (unless daily-task-ran-p
            (let* ((today-str (format-time-string "%Y-%m-%d" now))
                   (current-time-str (format-time-string "%H:%M" now))
                   (scheduled-time-str (plist-get task :time))
                   (last-run-date (plist-get task :last-run)))
              ;;(message "[Scheduler-Debug] Checking daily task '%s': Now=%s, Scheduled=%s, LastRun=%s, Today=%s"
              ;;         id current-time-str scheduled-time-str (or last-run-date "never") today-str)
              (when (and (string-greaterp current-time-str scheduled-time-str)
                         (not (equal last-run-date today-str)))
                (message "[Scheduler-Debug] -> Conditions MET for '%s'. Running..." id)
                (org-supertag-scheduler--run-task id task now)
                (setq daily-task-ran-p t))))))) ; Set flag to stop further daily tasks this cycle
     org-supertag-scheduler--tasks)))

(defun org-supertag-scheduler--run-task (id task now)
  "Execute a task and update its state."
  (let ((func (plist-get task :function)))
    (message "[Org-supertag-scheduler] Running task '%s'..." id)
    (if (org-supertag-bridge-ready-p)
        (progn
          (condition-case err
              (funcall func)
            (error
             (message "[Org-supertag-scheduler] Error running task '%s': %s" id (error-message-string err))))
          ;; Update last-run time
          (pcase (plist-get task :type)
            (:interval
             (plist-put task :last-run now))
            (:daily
             (plist-put task :last-run (format-time-string "%Y-%m-%d" now))))
          (puthash id task org-supertag-scheduler--tasks)
          ;; Persist state immediately after a daily task runs
          (when (eq (plist-get task :type) :daily)
            (org-supertag-scheduler--save-state)))
      (message "[Org-supertag-scheduler] Skipped task '%s' because Python server is not running." id))))

(provide 'org-supertag-scheduler)

;;; org-supertag-scheduler.el ends here
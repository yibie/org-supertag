;;; test-automation-scheduled.el --- One-shot scheduled-rule self-check  -*- lexical-binding: t; -*-
;; ponytail: standalone batch sanity check, no ert framework.
;; Run with:
;;   emacs --batch -Q -L . -l test-automation-scheduled.el

(require 'cl-lib)
(setq supertag-data-directory (make-temp-file "supertag-test-" t))
(require 'supertag-automation)
(require 'supertag-services-scheduler)

(defvar test-automation-fired 0)

(defun test-automation-bump (_node-id _context &rest _args)
  (cl-incf test-automation-fired))

;; --- 1. Create an :on-schedule rule that calls test-automation-bump ---
;;     automation-create derives the id from :name; capture it.
(let* ((created (supertag-automation-create
                 (list :name "schedfire"
                       :enabled t
                       :trigger :on-schedule
                       :schedule (list :time "00:00")
                       :actions (list (list :action :call-function
                                            :params (list :function 'test-automation-bump))))))
       (rule-id (plist-get created :id))
       (task-sym (intern rule-id)))

;; --- 2. Force-register with the scheduler ---
(supertag-automation--register-all-scheduled)

;; --- 3. Pretend yesterday so today's :time has "passed" ---
(let ((task (gethash task-sym supertag-scheduler--tasks)))
  (cl-assert task nil "scheduled task was not registered (id=%S)" task-sym)
  (plist-put task :last-run "1970-01-01")
  (puthash task-sym task supertag-scheduler--tasks))

;; --- 4. Run the scheduler tick directly ---
(supertag-scheduler--check-tasks)

;; --- 5. Assert ---
(cl-assert (= test-automation-fired 1)
           nil "scheduled rule did not fire (count=%d)" test-automation-fired)

;; --- 6. :days-of-week filter respected ---
(setq test-automation-fired 0)
(supertag-automation-update
 rule-id
 (lambda (rule)
   (plist-put rule :schedule
              (list :time "00:00"
                    ;; pick a day that is NOT today (1..7, %u is 1..7)
                    :days-of-week
                    (list (1+ (mod (string-to-number
                                    (format-time-string "%u"))
                                   7)))))))
(supertag-automation--register-all-scheduled)
(let ((task (gethash task-sym supertag-scheduler--tasks)))
  (plist-put task :last-run "1970-01-01")
  (puthash task-sym task supertag-scheduler--tasks))
(supertag-scheduler--check-tasks)
(cl-assert (= test-automation-fired 0)
           nil ":days-of-week filter was ignored (count=%d)" test-automation-fired)

(message "OK: scheduled rule fires; days-of-week filter works.")
(kill-emacs 0))

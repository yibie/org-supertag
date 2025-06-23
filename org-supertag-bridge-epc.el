;;; epcs.el --- EPC Server              -*- lexical-binding: t -*-

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; deferred
(cl-defmacro org-supertag-bridge-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar org-supertag-bridge-deferred-debug nil
  "Debug output switch.")

(defvar org-supertag-bridge-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun org-supertag-bridge-deferred-log (&rest args)
  "[internal] Debug log function."
  (when org-supertag-bridge-deferred-debug
    (with-current-buffer (get-buffer-create "*org-supertag-bridge-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" org-supertag-bridge-deferred-debug-count (apply #'format args)))))
    (cl-incf org-supertag-bridge-deferred-debug-count)))

(defvar org-supertag-bridge-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(cl-defmacro org-supertag-bridge-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`org-supertag-bridge-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal org-supertag-bridge-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar org-supertag-bridge-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar org-supertag-bridge-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `org-supertag-bridge-deferred-post-task' and `org-supertag-bridge-deferred-worker'.")

(defun org-supertag-bridge-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`org-supertag-bridge-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack org-supertag-bridge-deferred-queue)
    (org-supertag-bridge-deferred-log "QUEUE-POST [%s]: %s" (length org-supertag-bridge-deferred-queue) pack)
    (run-at-time org-supertag-bridge-deferred-tick-time nil 'org-supertag-bridge-deferred-worker)
    d))

(defun org-supertag-bridge-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when org-supertag-bridge-deferred-queue
    (let* ((pack (car (last org-supertag-bridge-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq org-supertag-bridge-deferred-queue (nbutlast org-supertag-bridge-deferred-queue))
      (condition-case err
          (setq value (org-supertag-bridge-deferred-exec-task d which arg))
        (error
         (org-supertag-bridge-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: org-supertag-bridge-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `org-supertag-bridge-deferred-resignal')
;; cancel      : a canceling function (default `org-supertag-bridge-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct org-supertag-bridge-deferred-object
  (callback 'identity)
  (errorback 'org-supertag-bridge-deferred-resignal)
  (cancel 'org-supertag-bridge-deferred-default-cancel)
  next status value)

(defun org-supertag-bridge-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun org-supertag-bridge-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (org-supertag-bridge-deferred-log "CANCEL : %s" d)
  (setf (org-supertag-bridge-deferred-object-callback d) 'identity)
  (setf (org-supertag-bridge-deferred-object-errorback d) 'org-supertag-bridge-deferred-resignal)
  (setf (org-supertag-bridge-deferred-object-next d) nil)
  d)

(defun org-supertag-bridge-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (org-supertag-bridge-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "org-supertag-bridge-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (org-supertag-bridge-deferred-object-callback d)
                    (org-supertag-bridge-deferred-object-errorback d)))
        (next-deferred (org-supertag-bridge-deferred-object-next d)))
    (cond
     (callback
      (org-supertag-bridge-deferred-condition-case err
                                         (let ((value (funcall callback arg)))
                                           (cond
                                            ((org-supertag-bridge-deferred-object-p value)
                                             (org-supertag-bridge-deferred-log "WAIT NEST : %s" value)
                                             (if next-deferred
                                                 (org-supertag-bridge-deferred-set-next value next-deferred)
                                               value))
                                            (t
                                             (if next-deferred
                                                 (org-supertag-bridge-deferred-post-task next-deferred 'ok value)
                                               (setf (org-supertag-bridge-deferred-object-status d) 'ok)
                                               (setf (org-supertag-bridge-deferred-object-value d) value)
                                               value))))
                                         (error
                                          (cond
                                           (next-deferred
                                            (org-supertag-bridge-deferred-post-task next-deferred 'ng err))
                                           (t
                                            (org-supertag-bridge-deferred-log "ERROR : %S" err)
                                            (message "deferred error : %S" err)
                                            (setf (org-supertag-bridge-deferred-object-status d) 'ng)
                                            (setf (org-supertag-bridge-deferred-object-value d) err)
                                            err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (org-supertag-bridge-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (org-supertag-bridge-deferred-resignal arg)))))))

(defun org-supertag-bridge-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (org-supertag-bridge-deferred-object-next prev) next)
  (cond
   ((eq 'ok (org-supertag-bridge-deferred-object-status prev))
    (setf (org-supertag-bridge-deferred-object-status prev) nil)
    (let ((ret (org-supertag-bridge-deferred-exec-task
                next 'ok (org-supertag-bridge-deferred-object-value prev))))
      (if (org-supertag-bridge-deferred-object-p ret) ret
        next)))
   ((eq 'ng (org-supertag-bridge-deferred-object-status prev))
    (setf (org-supertag-bridge-deferred-object-status prev) nil)
    (let ((ret (org-supertag-bridge-deferred-exec-task next 'ng (org-supertag-bridge-deferred-object-value prev))))
      (if (org-supertag-bridge-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun org-supertag-bridge-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-org-supertag-bridge-deferred-object :callback callback)
    (make-org-supertag-bridge-deferred-object)))

(defun org-supertag-bridge-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (org-supertag-bridge-deferred-exec-task d 'ok arg))

(defun org-supertag-bridge-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (org-supertag-bridge-deferred-exec-task d 'ng arg))

(defun org-supertag-bridge-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (org-supertag-bridge-deferred-post-task d 'ok arg))

(defun org-supertag-bridge-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (org-supertag-bridge-deferred-callback-post (org-supertag-bridge-deferred-new callback))."
  (let ((d (if callback
               (make-org-supertag-bridge-deferred-object :callback callback)
             (make-org-supertag-bridge-deferred-object))))
    (org-supertag-bridge-deferred-callback-post d arg)
    d))

(defun org-supertag-bridge-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-org-supertag-bridge-deferred-object :callback callback)))
    (org-supertag-bridge-deferred-set-next d nd)))

(defun org-supertag-bridge-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-org-supertag-bridge-deferred-object :errorback callback)))
    (org-supertag-bridge-deferred-set-next d nd)))

(defvar org-supertag-bridge-epc-debug nil)

(defun org-supertag-bridge-epc-log (&rest args)
  (when org-supertag-bridge-epc-debug
    (with-current-buffer (get-buffer-create "*org-supertag-bridge-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun org-supertag-bridge-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar org-supertag-bridge-epc-uid 1)

(defun org-supertag-bridge-epc-uid ()
  (cl-incf org-supertag-bridge-epc-uid))

(defvar org-supertag-bridge-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct org-supertag-bridge-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun org-supertag-bridge-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return org-supertag-bridge-epc-connection object."
  (org-supertag-bridge-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (org-supertag-bridge-epc-uid))
         (connection-name (format "org-supertag-bridge-epc con %s" connection-id))
         (connection-buf (org-supertag-bridge-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-org-supertag-bridge-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (org-supertag-bridge-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (org-supertag-bridge-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (org-supertag-bridge-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun org-supertag-bridge-epc-process-sentinel (connection process msg)
  (org-supertag-bridge-epc-log "!! Process Sentinel [%s] : %S : %S"
                     (org-supertag-bridge-epc-connection-name connection) process msg)
  (org-supertag-bridge-epc-disconnect connection))

(defun org-supertag-bridge-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (org-supertag-bridge-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (org-supertag-bridge-epc-connection-process connection)))
    (org-supertag-bridge-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun org-supertag-bridge-epc-disconnect (connection)
  (let ((process (org-supertag-bridge-epc-connection-process connection))
        (buf (org-supertag-bridge-epc-connection-buffer connection))
        (name (org-supertag-bridge-epc-connection-name connection)))
    (org-supertag-bridge-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (org-supertag-bridge-epc-log "!! Disconnected finished [%s]" name)))

(defun org-supertag-bridge-epc-process-filter (connection process message)
  (org-supertag-bridge-epc-log "INCOMING: [%s] [%S]" (org-supertag-bridge-epc-connection-name connection) message)
  (with-current-buffer (org-supertag-bridge-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (org-supertag-bridge-epc-process-available-input connection process)))

(defun org-supertag-bridge-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (org-supertag-bridge-deferred-new callback)
             (org-supertag-bridge-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun org-supertag-bridge-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (org-supertag-bridge-deferred-callback-post d event))))

(defun org-supertag-bridge-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (org-supertag-bridge-epc-net-have-input-p)
      (let ((event (org-supertag-bridge-epc-net-read-or-lose process))
            (ok nil))
        (org-supertag-bridge-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'org-supertag-bridge-epc-signal-send
                         (cons (org-supertag-bridge-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (org-supertag-bridge-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (org-supertag-bridge-epc-process-available-input connection process)))))))

(defun org-supertag-bridge-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (org-supertag-bridge-epc-net-decode-length))))

(defun org-supertag-bridge-epc-net-read-or-lose (_process)
  (condition-case error
      (org-supertag-bridge-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun org-supertag-bridge-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (org-supertag-bridge-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun org-supertag-bridge-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun org-supertag-bridge-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct org-supertag-bridge-epc-manager
  "Root object that holds all information related to an EPC activity.

`org-supertag-bridge-epc-start-epc' returns this object.

title          : instance name for displaying on the `org-supertag-bridge-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : org-supertag-bridge-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct org-supertag-bridge-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar org-supertag-bridge-epc-live-connections nil
  "[internal] A list of `org-supertag-bridge-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun org-supertag-bridge-epc-server-process-name (uid)
  (format "org-supertag-bridge-epc-server:%s" uid))

(defun org-supertag-bridge-epc-server-buffer-name (uid)
  (format " *%s*" (org-supertag-bridge-epc-server-process-name uid)))

(defun org-supertag-bridge-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (org-supertag-bridge-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (org-supertag-bridge-epc-disconnect (org-supertag-bridge-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 org-supertag-bridge-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq org-supertag-bridge-epc-live-connections (delete mngr org-supertag-bridge-epc-live-connections))
    ))

(defun org-supertag-bridge-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun org-supertag-bridge-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an org-supertag-bridge-epc-connection instance."
  (let* ((mngr mngr)
         (conn (org-supertag-bridge-epc-manager-connection mngr))
         (channel (org-supertag-bridge-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (org-supertag-bridge-epc-log "SIG CALL: %S" args)
                    (apply 'org-supertag-bridge-epc-handler-called-method ,mngr (org-supertag-bridge-epc-args args))))
               (return
                . (lambda (args)
                    (org-supertag-bridge-epc-log "SIG RET: %S" args)
                    (apply 'org-supertag-bridge-epc-handler-return ,mngr (org-supertag-bridge-epc-args args))))
               (return-error
                . (lambda (args)
                    (org-supertag-bridge-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'org-supertag-bridge-epc-handler-return-error ,mngr (org-supertag-bridge-epc-args args))))
               (epc-error
                . (lambda (args)
                    (org-supertag-bridge-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'org-supertag-bridge-epc-handler-epc-error ,mngr (org-supertag-bridge-epc-args args))))
               (methods
                . (lambda (args)
                    (org-supertag-bridge-epc-log "SIG METHODS: %S" args)
                    (org-supertag-bridge-epc-handler-methods ,mngr (caadr args))))
               ) do
             (org-supertag-bridge-epc-signal-connect channel method body))
    (push mngr org-supertag-bridge-epc-live-connections)
    mngr))

(defun org-supertag-bridge-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (org-supertag-bridge-epc-manager-connection mngr)))
    (org-supertag-bridge-epc-net-send conn (cons method messages))))

(defun org-supertag-bridge-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (org-supertag-bridge-epc-manager-methods mngr)
           if (eq method-name (org-supertag-bridge-epc-method-name i))
           do (cl-return i)))

(defun org-supertag-bridge-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (org-supertag-bridge-epc-manager-methods mngr)
                  collect
                  (list
                   (org-supertag-bridge-epc-method-name i)
                   (or (org-supertag-bridge-epc-method-arg-specs i) "")
                   (or (org-supertag-bridge-epc-method-docstring i) "")))))
    (org-supertag-bridge-epc-manager-send mngr 'return uid info)))

(defun org-supertag-bridge-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (org-supertag-bridge-epc-manager-methods mngr))
           (method (org-supertag-bridge-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (org-supertag-bridge-epc-log "ERR: No such method : %s" name)
        (org-supertag-bridge-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (org-supertag-bridge-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((org-supertag-bridge-deferred-object-p ret)
                (org-supertag-bridge-deferred-nextc ret
                                          (lambda (xx) (org-supertag-bridge-epc-manager-send mngr 'return uid xx))))
               (t (org-supertag-bridge-epc-manager-send mngr 'return uid ret))))
          (error
           (org-supertag-bridge-epc-log "ERROR : %S" err)
           (org-supertag-bridge-epc-manager-send mngr 'return-error uid err))))))))

(defun org-supertag-bridge-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (org-supertag-bridge-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (org-supertag-bridge-epc-manager-sessions mngr) ret)))

(defun org-supertag-bridge-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (org-supertag-bridge-epc-manager-sessions mngr))))
    (cond
     (pair
      (org-supertag-bridge-epc-log "RET: id:%s [%S]" uid args)
      (org-supertag-bridge-epc-manager-remove-session mngr uid)
      (org-supertag-bridge-deferred-callback (cdr pair) args)
      )
     (t                                 ; error
      (org-supertag-bridge-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun org-supertag-bridge-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (org-supertag-bridge-epc-manager-sessions mngr))))
    (cond
     (pair
      (org-supertag-bridge-epc-log "RET-ERR: id:%s [%S]" uid args)
      (org-supertag-bridge-epc-manager-remove-session mngr uid)
      (org-supertag-bridge-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (org-supertag-bridge-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun org-supertag-bridge-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (org-supertag-bridge-epc-manager-sessions mngr))))
    (cond
     (pair
      (org-supertag-bridge-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (org-supertag-bridge-epc-manager-remove-session mngr uid)
      (org-supertag-bridge-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (org-supertag-bridge-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun org-supertag-bridge-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (org-supertag-bridge-epc-uid))
        (sessions (org-supertag-bridge-epc-manager-sessions mngr))
        (d (org-supertag-bridge-deferred-new)))
    (push (cons uid d) sessions)
    (setf (org-supertag-bridge-epc-manager-sessions mngr) sessions)
    (org-supertag-bridge-epc-manager-send mngr 'call uid method-name args)
    d))

(defun org-supertag-bridge-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-org-supertag-bridge-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (org-supertag-bridge-epc-manager-methods mngr))))
    (setf (org-supertag-bridge-epc-manager-methods mngr) methods)
    method))

(defun org-supertag-bridge-epc-sync (mngr d timeout-seconds)
  "Wrap deferred methods with synchronous waiting for TIMEOUT-SECONDS, and return the result.
If an exception is occurred or timeout, this function throws the error."
  (let ((result 'org-supertag-bridge-epc-nothing) ; Placeholder for result. User should ensure this symbol is unique or use a gensym if necessary.
        (start-time (current-time)))
    (org-supertag-bridge-deferred-chain
     d
     (org-supertag-bridge-deferred-nextc it
                               (lambda (x) (setq result x)))
     (org-supertag-bridge-deferred-error it
                               (lambda (er) (setq result (cons 'error er)))))

    (while (and (eq result 'org-supertag-bridge-epc-nothing)
                (< (float-time (time-since start-time)) timeout-seconds))
      ;; accept-process-output with a short internal timeout for responsiveness of the loop
      (accept-process-output
       (org-supertag-bridge-epc-connection-process (org-supertag-bridge-epc-manager-connection mngr))
       0.02 ; Short timeout (20ms) for this call to accept-process-output
       nil  ; NOMSG (don't display "Process ... not running")
       t    ; JUST-THIS (only process output from this specific process)
       ))

    ;; After the loop, check if it was a timeout
    (when (eq result 'org-supertag-bridge-epc-nothing) 
      (setq result (cons 'error (format "EPC call timed out after %s seconds" timeout-seconds))))
      
    ;; Handle the final result
    (if (and (consp result) (eq 'error (car result)))
        (progn
          (org-supertag-bridge-epc-log "ERROR in epc-sync or callback: %S" (cdr result))
          (signal (car result) (cdr result))) ; Re-signal the error for condition-case
        result)))

(defun org-supertag-bridge-epc-call-sync (mngr method-name args &optional timeout-seconds)
  "Call peer's method with ARGS synchronously and return the result.
If an exception is occurred, or if it times out after TIMEOUT-SECONDS (default 60s),
this function throws an error."
  (let ((effective-timeout (or timeout-seconds 60))) ; Default timeout if not provided
    (org-supertag-bridge-epc-sync mngr 
                                 (org-supertag-bridge-epc-call-deferred mngr method-name args)
                                 effective-timeout)))


(defun org-supertag-bridge-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (org-supertag-bridge-epc-connection-process (org-supertag-bridge-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))


(defvar org-supertag-bridge-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`org-supertag-bridge-epc-manager' instance]).
When the server process accepts the client connection, the
`org-supertag-bridge-epc-manager' instance is created and stored in this variable
`org-supertag-bridge-epc-server-client-processes'. This variable is used for the management
purpose.")

;; org-supertag-bridge-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `org-supertag-bridge-epc-manager' instances
(cl-defstruct org-supertag-bridge-epc-server name process port connect-function)

(defvar org-supertag-bridge-epc-server-processes nil
  "[internal] A list of ([process object] . [`org-supertag-bridge-epc-server' instance]).
This variable is used for the management purpose.")

(defun org-supertag-bridge-epc-server-get-manager-by-process (proc)
  "[internal] Return the org-supertag-bridge-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in org-supertag-bridge-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun org-supertag-bridge-epc-server-accept (process)
  "[internal] Initialize the process and return org-supertag-bridge-epc-manager object."
  (org-supertag-bridge-epc-log "PYTHONBRIDGE-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (org-supertag-bridge-epc-uid))
         (connection-name (format "org-supertag-bridge-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-org-supertag-bridge-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (org-supertag-bridge-epc-log "PYTHONBRIDGE-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (org-supertag-bridge-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (org-supertag-bridge-epc-process-sentinel connection p e)))
    (make-org-supertag-bridge-epc-manager :server-process process :port t
                                :connection connection)))

(defun org-supertag-bridge-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (org-supertag-bridge-epc-log "PYTHONBRIDGE-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (org-supertag-bridge-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (org-supertag-bridge-epc-server-accept process)))
            (push (cons process mngr) org-supertag-bridge-epc-server-client-processes)
            (org-supertag-bridge-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (org-supertag-bridge-epc-log "PYTHONBRIDGE-EPC-SERVER- Protocol error: %S" err)
         (org-supertag-bridge-epc-log "PYTHONBRIDGE-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process org-supertag-bridge-epc-server-client-processes)) _d)
        (when pair
          (org-supertag-bridge-epc-log "PYTHONBRIDGE-EPC-SERVER- DISCONNECT %S" process)
          (org-supertag-bridge-epc-stop-epc (cdr pair))
          (setq org-supertag-bridge-epc-server-client-processes
                (assq-delete-all process org-supertag-bridge-epc-server-client-processes))
          ))
      nil))))

(defun org-supertag-bridge-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "PYTHON-BRIDGE EPC Server %s" (org-supertag-bridge-epc-uid)))
       (buf (org-supertag-bridge-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :noquery t
         :sentinel
         (lambda (process message)
           (org-supertag-bridge-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-org-supertag-bridge-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          org-supertag-bridge-epc-server-processes)
    main-process))

(provide 'org-supertag-bridge-epc)
;;; org-supertag-bridge-epc.el ends here

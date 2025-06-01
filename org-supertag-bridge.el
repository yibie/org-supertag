;;; org-supertag-bridge.el --- EPC Bridge for Org SuperTag -*- lexical-binding: t; -*-

;; Filename: org-supertag-bridge.el
;; Description: EPC Bridge for Org SuperTag, adapted from python-bridge.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com> (Original python-bridge.el)
;; Maintainer: Your Name <your.email@example.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved. (Original python-bridge.el)
;; Copyright (C) 2024, Your Name (Adaptations for Org SuperTag)
;; Created: 2018-06-15 (Original python-bridge.el)
;; Version: 0.1.0 (Org SuperTag Bridge)
;; Last-Updated: 
;;           By: 
;; URL: 
;; Keywords: org, tags, epc
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (org-supertag-bridge-epc "0.1"))

;;; This file is NOT part of GNU Emacs

;;; License
;; (Retain original license or adapt as needed, e.g. GPLv3)
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;; This file provides the Emacs Lisp side of an EPC bridge for org-supertag,
;; interfacing with simtag_bridge.py. It is adapted from python-bridge.el.
;; It manages the Python server process and communication channels.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
;; 'json, 'map, 'seq might not be directly needed, review later.
(require 'json nil t) 
(require 'map nil t)
(require 'seq nil t)
(require 'org-supertag-bridge-epc) ; Renamed from python-bridge-epc

;; =============================================================================
;; Defgroup & Customization Variables
;; =============================================================================
(defgroup org-supertag-bridge nil
  "Org SuperTag EPC Bridge configuration."
  :group 'org-supertag ; Assuming 'org-supertag group exists or is desired
  :prefix "org-supertag-bridge-")

(defcustom org-supertag-bridge-python-command "python3"
  "The Python interpreter used to run `simtag_bridge.py`."
  :type 'string
  :group 'org-supertag-bridge)

(defcustom org-supertag-bridge-python-script
  (expand-file-name "simtag_bridge.py" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the `simtag_bridge.py` script.
Assumed to be in the same directory as this Elisp file."
  :type 'file
  :group 'org-supertag-bridge)

(defcustom org-supertag-bridge-data-directory
  (expand-file-name ".org-supertag" user-emacs-directory) ; Changed default
  "Data directory for org-supertag, passed to `simtag_bridge.py`."
  :type 'directory
  :group 'org-supertag-bridge)

(defcustom org-supertag-bridge-process-buffer-name "*simtag-bridge-py-output*"
  "Name of the buffer for `simtag_bridge.py`'s output."
  :type 'string
  :group 'org-supertag-bridge)

(defcustom org-supertag-bridge-log-buffer-name "*org-supertag-bridge-log*"
  "Buffer for Org SuperTag bridge logs."
  :type 'string
  :group 'org-supertag-bridge)

(defcustom org-supertag-bridge-enable-log t
  "Enable logging for the Org SuperTag bridge operations."
  :type 'boolean
  :group 'org-supertag-bridge)

(defcustom org-supertag-bridge-enable-profile nil ; Kept from original, might be useful
  "Enable Python profiling for `simtag_bridge.py`. Profile data saved in data directory."
  :type 'boolean
  :group 'org-supertag-bridge)

;; =============================================================================
;; Internal State Variables
;; =============================================================================

;; Emacs-side EPC server (Python connects to this)
(defvar org-supertag-bridge--emacs-epc-server nil
  "The Emacs-side EPC server process that simtag_bridge.py connects to.")
(defvar org-supertag-bridge--emacs-epc-server-port nil
  "The port number for the Emacs-side EPC server.")

;; Python process details
(defvar org-supertag-bridge--python-process nil
  "Process object for the running `simtag_bridge.py` script.")
(defvar org-supertag-bridge--python-program-to-run nil
  "Full command for the Python interpreter.")
(defvar org-supertag-bridge--python-program-args nil
  "Arguments for the Python script.")

;; EPC Manager for connection FROM Emacs TO Python server
(defvar org-supertag-bridge--python-epc-manager nil
  "EPC Manager for the connection from Emacs to the SimTagBridge Python server.")

(defvar org-supertag-bridge--ready-p nil
  "Non-nil if the bridge to Python is fully initialized and ready.")

;; =============================================================================
;; Logging
;; =============================================================================
(defun org-supertag-bridge--log (format-string &rest args)
  "Log MESSAGE if `org-supertag-bridge-enable-log` is non-nil."
  (when org-supertag-bridge-enable-log
    (let ((log-buffer (get-buffer-create org-supertag-bridge-log-buffer-name)))
      (with-current-buffer log-buffer
        (goto-char (point-max))
        (insert (format-time-string "[%Y-%m-%d %H:%M:%S.%3N] "))
        (insert (apply #'format format-string args))
        (insert "\n")))))

;; =============================================================================
;; Emacs-side EPC Server (for Python to connect to)
;; =============================================================================
(defun org-supertag-bridge--start-emacs-epc-server ()
  "Start the Emacs-side EPC server for `simtag_bridge.py` to connect to.
This server allows Python to call methods defined in Emacs."
  (org-supertag-bridge--log "Attempting to start Emacs-side EPC server (current server: %S, live: %S)..."
                            org-supertag-bridge--emacs-epc-server
                            (if (processp org-supertag-bridge--emacs-epc-server)
                                (process-live-p org-supertag-bridge--emacs-epc-server)
                              "not a process"))
  (unless (and org-supertag-bridge--emacs-epc-server (process-live-p org-supertag-bridge--emacs-epc-server))
    (org-supertag-bridge--log "No live Emacs EPC server found, attempting to create one.")
    (let ((callback (lambda (manager) ; Callback when Python connects
                      (org-supertag-bridge--log "Python process connected to Emacs EPC server. Manager: %S" manager)
                      (org-supertag-bridge-epc-define-method manager
                                                             'simtag-bridge/report-ready ; Python calls this
                                                             #'org-supertag-bridge--handle-python-server-ready-signal) ; Our handler
                      (org-supertag-bridge-epc-define-method manager
                                                             'eval-in-emacs ; Python calls this
                                                             #'org-supertag-bridge--eval-in-emacs-func) ; Our handler
                      (org-supertag-bridge--log "Defined EPC methods for Python on Emacs server."))))
      (org-supertag-bridge--log "Calling org-supertag-bridge-epc-server-start now with callback...")
      ;; Ensure `org-supertag-bridge-epc.el` is loaded for `org-supertag-bridge-epc-server-start`
      (require 'org-supertag-bridge-epc)
      (setq org-supertag-bridge--emacs-epc-server (org-supertag-bridge-epc-server-start callback))
      (org-supertag-bridge--log "org-supertag-bridge-epc-server-start returned: %S" org-supertag-bridge--emacs-epc-server)))

  (if org-supertag-bridge--emacs-epc-server
      (if (process-live-p org-supertag-bridge--emacs-epc-server) ; Extra check for liveness
          (progn
            (setq org-supertag-bridge--emacs-epc-server-port (process-contact org-supertag-bridge--emacs-epc-server :service))
            (org-supertag-bridge--log "Emacs-side EPC server started successfully. Process: %S. Listening on port: %d"
                                      org-supertag-bridge--emacs-epc-server
                                      org-supertag-bridge--emacs-epc-server-port)
            t) ; Success
        (progn
          (org-supertag-bridge--log "ERROR: Emacs EPC server process %S is not live after creation." org-supertag-bridge--emacs-epc-server)
          (message "[OrgSuperTagBridge] Error: Emacs-side EPC server process not live after creation.")
          (setq org-supertag-bridge--emacs-epc-server nil) ; Clear it if not live
          nil)) ; Failure
    (progn
      (org-supertag-bridge--log "ERROR: Failed to start Emacs-side EPC server (server object is nil after attempt).")
      (message "[OrgSuperTagBridge] Error: Emacs-side EPC server failed to start (nil server object).")
      nil))) ; Failure

;; Handler for 'eval-in-emacs' called by Python
(defun org-supertag-bridge--eval-in-emacs-func (sexp-string)
  "Evaluate SEXP-STRING received from Python. Called by `_eval_in_emacs` in Python."
  (org-supertag-bridge--log "Emacs EPC: eval-in-emacs called with: %s" sexp-string)
  (condition-case err
      (let ((result (eval (read sexp-string))))
        ;; Optionally log or handle result, but typically Python doesn't expect a return value from this.
        (org-supertag-bridge--log "eval-in-emacs result: %S" result)
        nil) ; Return nil to Python to avoid "too many arguments" EPC error
    (error
     (org-supertag-bridge--log "ERROR in eval-in-emacs: %S. Sexp was: %s" err sexp-string)
     ;; Decide if Python should be notified of the error. For now, just log.
     nil)))

;; Handler for Python's readiness signal
(defun org-supertag-bridge--handle-python-server-ready-signal (python-server-port)
  "Handles `simtag-bridge/report-ready` call from Python.
PYTHON-SERVER-PORT is the port the Python EPC server is listening on.
Establishes the main EPC connection from Emacs TO the Python server."
  (org-supertag-bridge--log "Python server reported ready. Listening on its port: %s" python-server-port)
  (condition-case-unless-debug err
      (progn
        (setq org-supertag-bridge--python-epc-manager
              (make-org-supertag-bridge-epc-manager ; From -epc.el
               :server-process org-supertag-bridge--python-process ; Associate with the running script
               :commands (cons org-supertag-bridge--python-program-to-run org-supertag-bridge--python-program-args)
               :title (format "OrgSuperTagBridge-Client-to-Python:%s" python-server-port)
               :port python-server-port ; Port of the Python server
               :connection (org-supertag-bridge-epc-connect "127.0.0.1" python-server-port)))
        
        (if (and org-supertag-bridge--python-epc-manager (org-supertag-bridge-epc-live-p org-supertag-bridge--python-epc-manager))
            (progn
              (org-supertag-bridge-epc-init-epc-layer org-supertag-bridge--python-epc-manager) ; Initialize protocol layer
              (setq org-supertag-bridge--ready-p t)
              (message "[OrgSuperTagBridge] Successfully connected to SimTagBridge Python server on port %s." python-server-port)
              (org-supertag-bridge--log "✅ Connection FROM Emacs TO SimTagBridge Python server established."))
          (progn
            (setq org-supertag-bridge--ready-p nil)
            (error (format "[OrgSuperTagBridge] Failed to establish live connection to Python server on port %s" python-server-port)))))
    (error
     (setq org-supertag-bridge--ready-p nil)
     (org-supertag-bridge--log "ERROR establishing connection TO Python server: %S" err)
     (message "[OrgSuperTagBridge] Error connecting to Python server: %s" (error-message-string err))
     (org-supertag-bridge-kill-process) ; Clean up if connection failed
     nil)))

;; =============================================================================
;; Python Process Management
;; =============================================================================
(cl-defun org-supertag-bridge-start-process ()
  "Start the `simtag_bridge.py` process if it isn't already running and connected."
  (interactive)
  (if (and org-supertag-bridge--python-epc-manager (org-supertag-bridge-epc-live-p org-supertag-bridge--python-epc-manager) org-supertag-bridge--ready-p)
      (progn
        (org-supertag-bridge--log "SimTagBridge Python connection already live and ready.")
        (message "[OrgSuperTagBridge] Already connected."))
    (org-supertag-bridge--log "Attempting to start SimTagBridge process pipeline...")
    
    ;; 1. Ensure Emacs-side EPC server is running for Python to connect back
    (unless (org-supertag-bridge--start-emacs-epc-server)
      (org-supertag-bridge--log "Aborting Python process start: Emacs-side EPC server failed.")
      (cl-return-from org-supertag-bridge-start-process nil))
      
    (unless org-supertag-bridge--emacs-epc-server-port
      (org-supertag-bridge--log "Aborting Python process start: Emacs-side EPC server port not available.")
      (error "[OrgSuperTagBridge] Emacs-side EPC server port not set. Cannot start Python process.")
      (cl-return-from org-supertag-bridge-start-process nil))

    ;; 2. Prepare and launch the simtag_bridge.py script
    (setq org-supertag-bridge--ready-p nil) ; Reset readiness flag
    (let* ((python-script-path org-supertag-bridge-python-script)
           (emacs-port-str (number-to-string org-supertag-bridge--emacs-epc-server-port))
           (data-dir org-supertag-bridge-data-directory)
           (script-working-dir (file-name-directory python-script-path)) ; Run Python from its own dir
           (process-environment process-environment) ; Inherit current environment
           (profile-arg (when org-supertag-bridge-enable-profile (list "--profile"))))

      (unless (file-exists-p python-script-path)
        (error "[OrgSuperTagBridge] Python script not found: %s" python-script-path))
      (unless (file-directory-p data-dir)
        (make-directory data-dir t)
        (org-supertag-bridge--log "Created data directory: %s" data-dir))
      
      (setq org-supertag-bridge--python-program-to-run org-supertag-bridge-python-command)
      (setq org-supertag-bridge--python-program-args 
            (append (list python-script-path emacs-port-str data-dir) profile-arg))

      (org-supertag-bridge--log "Launching SimTagBridge: %s %s" 
                                org-supertag-bridge--python-program-to-run 
                                (mapconcat #'identity org-supertag-bridge--python-program-args " "))
      (org-supertag-bridge--log "  Working directory for script: %s" script-working-dir)
      
      (let ((current-process-environment process-environment) ; Save current
            (process-connection-type nil)) ; For stdio pipes
        (setq process-environment current-process-environment) ; Restore for other Emacs processes
        (setq org-supertag-bridge--python-process
              (apply #'start-file-process ; Using start-file-process to better control working dir
                     "simtag-bridge-py" ; Process name
                     org-supertag-bridge-process-buffer-name ; Output buffer
                     org-supertag-bridge--python-program-to-run
                     org-supertag-bridge--python-program-args
                     ;; Keyword arguments for start-file-process if needed, e.g. :default-directory
                     ))
        (when org-supertag-bridge--python-process
           (set-process-query-on-exit-flag org-supertag-bridge--python-process nil)
           ;; Set process working directory if `start-file-process` doesn't have a direct arg for it
           ;; (process-put org-supertag-bridge--python-process :cwd script-working-dir) ; Example if needed
           (org-supertag-bridge--log "SimTagBridge Python process started. Waiting for it to connect back and report ready..."))
        org-supertag-bridge--python-process)))) ; Return the process object

(defun org-supertag-bridge-kill-process ()
  "Stop the SimTagBridge Python process and clean up related EPC resources."
  (interactive)
  (org-supertag-bridge--log "Attempting to kill SimTagBridge Python process and EPC connections...")
  
  ;; 1. Politely ask Python server to cleanup (if connected)
  (when (and org-supertag-bridge--python-epc-manager (org-supertag-bridge-epc-live-p org-supertag-bridge--python-epc-manager) org-supertag-bridge--ready-p)
    (org-supertag-bridge--log "Sending 'cleanup' request to SimTagBridge Python server...")
    (condition-case err
        (org-supertag-bridge-call-sync "cleanup" 5) ; Add a timeout, e.g. 5 seconds
      (error 
       (org-supertag-bridge--log "Error/timeout during Python 'cleanup' call (or connection already down): %S" err))))

  ;; 2. Close EPC manager connection TO Python
  (when org-supertag-bridge--python-epc-manager
    (org-supertag-bridge--log "Stopping EPC manager to Python...")
    (condition-case err
        (org-supertag-bridge-epc-stop-epc org-supertag-bridge--python-epc-manager)
      (error (org-supertag-bridge--log "Error stopping Python EPC manager: %S" err)))
    (setq org-supertag-bridge--python-epc-manager nil))

  ;; 3. Kill the Python process itself
  (when (and org-supertag-bridge--python-process (process-live-p org-supertag-bridge--python-process))
    (org-supertag-bridge--log "Killing SimTagBridge Python process...")
    (ignore-errors (delete-process org-supertag-bridge--python-process))
    (message "[OrgSuperTagBridge] Python process killed."))
  (setq org-supertag-bridge--python-process nil)

  ;; 4. Stop the Emacs-side EPC server that Python connects to
  (when (and org-supertag-bridge--emacs-epc-server (process-live-p org-supertag-bridge--emacs-epc-server))
    (org-supertag-bridge--log "Stopping Emacs-side EPC server...")
    (condition-case err 
        (ignore-errors (delete-process org-supertag-bridge--emacs-epc-server))
      (error (org-supertag-bridge--log "Error stopping Emacs-side EPC server process: %S" err)))
    (message "[OrgSuperTagBridge] Emacs-side EPC server stopped."))
  (setq org-supertag-bridge--emacs-epc-server nil
        org-supertag-bridge--emacs-epc-server-port nil)
  
  (setq org-supertag-bridge--ready-p nil) ; Reset readiness flag
  (org-supertag-bridge--log "SimTagBridge connections and process should be terminated."))

(defun org-supertag-bridge-restart-process ()
  "Stop and restart the SimTagBridge Python process."
  (interactive)
  (org-supertag-bridge--log "Restarting SimTagBridge Python process...")
  (org-supertag-bridge-kill-process)
  ;; Short delay to ensure ports are released, if necessary
  ;; (sit-for 0.5) 
  (if (org-supertag-bridge-start-process)
      (message "[OrgSuperTagBridge] Python process restart initiated.")
    (message "[OrgSuperTagBridge] Python process restart failed.")))

(add-hook 'kill-emacs-hook #'org-supertag-bridge-kill-process)

;; =============================================================================
;; Public API: Calling Python Server Methods
;; =============================================================================
(defun org-supertag-bridge-call-sync (method-name timeout &rest args)
  "Call METHOD-NAME on SimTagBridge Python server synchronously with ARGS.
Wait for TIMEOUT seconds.
Returns the result from Python or signals an error."
  (unless (and org-supertag-bridge--python-epc-manager (org-supertag-bridge-epc-live-p org-supertag-bridge--python-epc-manager) org-supertag-bridge--ready-p)
    (org-supertag-bridge--log "Bridge to Python not live/ready for sync call: %s" method-name)
    (error (format "SimTagBridge to Python not live/ready for sync call: %s" method-name)))

  (org-supertag-bridge--log "Calling sync Python method '%s' with timeout %ds, args: %S" method-name timeout args)
  (condition-case-unless-debug err
      (let ((result (org-supertag-bridge-epc-call-sync
                       org-supertag-bridge--python-epc-manager
                       (intern method-name) ; Method name as symbol
                       args                 ; List of arguments
                       timeout)))            ; Timeout in seconds
        (org-supertag-bridge--log "Sync Python method '%s' result: %S" method-name result)
        result)
    (error
     (org-supertag-bridge--log "ERROR calling sync Python method '%s': %S" method-name err)
     (signal (car err) (cdr err))))) ; Re-signal the error

(cl-defun org-supertag-bridge-call-async (method-name callback &rest args)
  "Call METHOD-NAME on SimTagBridge Python server asynchronously with ARGS.
CALLBACK is a function of one arg (the result or error structure)."
  (unless (and org-supertag-bridge--python-epc-manager (org-supertag-bridge-epc-live-p org-supertag-bridge--python-epc-manager) org-supertag-bridge--ready-p)
    (org-supertag-bridge--log "Bridge to Python not live/ready for async call: %s" method-name)
    (when callback (funcall callback (list :status :error :message "SimTagBridge to Python not live/ready")))
    (cl-return-from org-supertag-bridge-call-async nil))
  
  (org-supertag-bridge--log "Calling async Python method '%s' with args: %S" method-name args)
  (let ((deferred (apply #'org-supertag-bridge-epc-call-deferred org-supertag-bridge--python-epc-manager (intern method-name) args)))
    (org-supertag-bridge-deferred-nextc deferred ; from -epc.el (deferred.el)
      (lambda (response)
        (org-supertag-bridge--log "Async Python method '%s' success: %S" method-name response)
        (when callback (funcall callback response))))
    (org-supertag-bridge-deferred-error deferred ; from -epc.el (deferred.el)
      (lambda (err-deferred)
        (org-supertag-bridge--log "Async Python method '%s' error: %S" method-name err-deferred)
        (when callback (funcall callback (list :status :error :message (format "EPC async call '%s' to Python failed: %S" method-name err-deferred))))))
    deferred))

;; =============================================================================
;; Convenience and Auto-start
;; =============================================================================
(defun org-supertag-bridge-ensure-ready (&optional timeout)
  "Ensure the SimTagBridge is started and ready.
Returns t if ready, nil otherwise. Waits up to TIMEOUT seconds (default 10)."
  (interactive)
  (let ((wait-timeout (or timeout 10))) ; Default timeout 10 seconds
    (if (and org-supertag-bridge--python-epc-manager (org-supertag-bridge-epc-live-p org-supertag-bridge--python-epc-manager) org-supertag-bridge--ready-p)
        t ; Already ready
      (progn
        (org-supertag-bridge-start-process) ; Attempt to start if not already
        ;; Wait for readiness
        (let ((attempts 0)
              (max-attempts (* wait-timeout 2))) ; Check every 0.5 seconds
          (while (and (not org-supertag-bridge--ready-p) (< attempts max-attempts))
            (sit-for 0.5)
            (setq attempts (1+ attempts))
            (when (= 0 (% attempts 4)) ; Log every 2 seconds
                (org-supertag-bridge--log "Waiting for bridge readiness... (attempt %d/%d)" attempts max-attempts)))
          (if org-supertag-bridge--ready-p
              (progn (org-supertag-bridge--log "Bridge became ready after %d attempts." attempts) t)
            (progn (org-supertag-bridge--log "Bridge NOT ready after %d attempts (timeout %ds)." attempts wait-timeout) nil)))))))

;; Example auto-start (can be removed or made optional)
(defun org-supertag-bridge-enable-auto-start ()
  "Enable auto-starting of the bridge on command execution."
  (interactive)
  (add-hook 'post-command-hook #'org-supertag-bridge-ensure-ready nil t) ; Add locally to hook
  (message "[OrgSuperTagBridge] Auto-start enabled (will try to ensure ready on post-command)."))

(defun org-supertag-bridge-disable-auto-start ()
  "Disable auto-starting of the bridge."
  (interactive)
  (remove-hook 'post-command-hook #'org-supertag-bridge-ensure-ready t) ; Remove local hook
  (message "[OrgSuperTagBridge] Auto-start disabled."))

(provide 'org-supertag-bridge)

;;; org-supertag-bridge.el ends here

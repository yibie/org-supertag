;;; org-supertag-bridge.el --- EPC Bridge for Org SuperTag -*- lexical-binding: t; -*-


;;; This file is NOT part of GNU Emacs

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
(require 'org-supertag-db) ;; <=== NEW: access tag database


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
  (expand-file-name "simtag/simtag_bridge.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the `simtag_bridge.py` script.
Assumed that `org-supertag-bridge.el` is in the project root, and the script is in a 'simtag' subdirectory."
  :type 'file
  :group 'org-supertag-bridge)

(defcustom org-supertag-bridge-data-directory
  (expand-file-name "org-supertag" user-emacs-directory)
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

(defcustom org-supertag-bridge-enable-verbose-async-debug nil
  "Enable verbose debug logging for asynchronous call callbacks."
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
    (let* ((python-cmd org-supertag-bridge-python-command)
           (emacs-port-str (number-to-string org-supertag-bridge--emacs-epc-server-port))
           (data-dir org-supertag-bridge-data-directory)
           (profile-arg (when org-supertag-bridge-enable-profile (list "--profile")))
           (default-directory (expand-file-name ".." (file-name-directory org-supertag-bridge-python-script))))

      (unless (file-exists-p python-cmd)
        (error "[OrgSuperTagBridge] Python script not found: %s" python-cmd))
      (unless (file-directory-p data-dir)
        (make-directory data-dir t)
        (org-supertag-bridge--log "Created data directory: %s" data-dir))
      
      (setq org-supertag-bridge--python-program-to-run python-cmd)
      (setq org-supertag-bridge--python-program-args 
            (append (list "-m" "simtag.simtag_bridge" emacs-port-str data-dir) profile-arg))

      (org-supertag-bridge--log "DEBUG: Using Python command from org-supertag-bridge-python-command: '%s'" python-cmd)
      (org-supertag-bridge--log "Launching SimTagBridge: %s %s" 
                                org-supertag-bridge--python-program-to-run 
                                (mapconcat #'identity org-supertag-bridge--python-program-args " "))
      (org-supertag-bridge--log "  Working directory for script: %s" default-directory)
      
      (let ((current-process-environment process-environment) ; Save current
            (process-connection-type nil)) ; For stdio pipes
        (setq process-environment current-process-environment) ; Restore for other Emacs processes
        (setq org-supertag-bridge--python-process
              (apply #'start-file-process
                     "simtag-bridge-py"
                     org-supertag-bridge-process-buffer-name
                     python-cmd
                     "-m" "simtag.simtag_bridge" emacs-port-str data-dir
                     profile-arg)))
        (when org-supertag-bridge--python-process
           (set-process-query-on-exit-flag org-supertag-bridge--python-process nil)
           ;; Set process working directory if `start-file-process` doesn't have a direct arg for it
           ;; (process-put org-supertag-bridge--python-process :cwd script-working-dir) ; Example if needed
           (org-supertag-bridge--log "SimTagBridge Python process started. Waiting for it to connect back and report ready..."))
        org-supertag-bridge--python-process)))

(defun org-supertag-bridge-kill-process ()
  "Stop the SimTagBridge Python process and clean up related EPC resources."
  (interactive)
  (org-supertag-bridge--log "Attempting to kill SimTagBridge Python process and EPC connections...")
  
  ;; 1. Politely ask Python server to cleanup (if connected)
  (when (and org-supertag-bridge--python-epc-manager (org-supertag-bridge-epc-live-p org-supertag-bridge--python-epc-manager) org-supertag-bridge--ready-p)
    (org-supertag-bridge--log "Sending 'cleanup' request to SimTagBridge Python server...")
    (let ((debug-on-error nil)) ; Temporarily disable debugger for this specific call
      (condition-case err
          (org-supertag-bridge-call-sync "cleanup" 5) ; Add a timeout, e.g. 5 seconds
        (error 
         (org-supertag-bridge--log "Error/timeout during Python 'cleanup' call (or connection already down): %S" err)))))

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
(defun org-supertag-bridge--normalize-parameters (params)
  "Ensure parameters are in a format serializable by EPC.
This version simply returns the parameters as-is, to avoid unwanted
conversions from alist to plist, which was causing issues with Python dict expectations."
  (identity params))

(defun org-supertag-bridge-call-sync (method-name &optional timeout &rest args)
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
                       (org-supertag-bridge--normalize-parameters args) ; Normalized arguments
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
  (let ((captured-calling-buffer (current-buffer))
        (deferred (org-supertag-bridge-epc-call-deferred org-supertag-bridge--python-epc-manager 
                                                     (intern method-name)
                                                     ;; Apply the same normalization as sync calls
                                                     (org-supertag-bridge--normalize-parameters args))))
    ;; First form in let body: Attach the callback
    (org-supertag-bridge-deferred-nextc deferred
      (lambda (response) 
        (when org-supertag-bridge-enable-verbose-async-debug
          (org-supertag-bridge--log "[BRIDGE_ASYNC_CB] Outer callback in bridge.el received: %S" response))
        (if (buffer-live-p captured-calling-buffer)
            (with-current-buffer captured-calling-buffer
              (if callback 
                  (progn
                    (when org-supertag-bridge-enable-verbose-async-debug
                      (org-supertag-bridge--log "[BRIDGE_ASYNC_CB] Calling inner callback for method %s" method-name))
                    (funcall callback response))
                (message "Async call to %s returned (no inner callback): %S" method-name response)))
          (org-supertag-bridge--log "[BRIDGE_ASYNC_CB] Calling buffer for method %s (%S) no longer live." 
                                    method-name captured-calling-buffer))))
    ;; Second form in let body: Return the deferred object itself
    deferred))

;; =============================================================================
;; Convenience and Auto-start
;; =============================================================================
(defun org-supertag-bridge-ensure-ready (&optional timeout)
  "Ensure the SimTagBridge is started and ready.
Returns t if ready, nil otherwise. Waits up to TIMEOUT seconds (default 10)."
  (interactive)
  (let ((wait-timeout (or timeout 10))) ; Default timeout 10 seconds
    (if (and org-supertag-bridge--python-epc-manager 
             (org-supertag-bridge-epc-live-p org-supertag-bridge--python-epc-manager) 
             org-supertag-bridge--ready-p)
        t ; Already ready
      (progn
        (org-supertag-bridge-start-process) ; Attempt to start if not already
        ;; Wait for readiness
        (let ((attempts 0)
              (max-attempts (* wait-timeout 2))) ; Check every 0.5 seconds
          (while (and (not org-supertag-bridge--ready-p) (< attempts max-attempts))
            (sit-for 0.1)
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

;; =============================================================================
;; New: lightweight pull-based catalog for Python
;; =============================================================================

(defun org-supertag-bridge--format-ts (time-val)
  "返回 ISO 字符串, 若 `org-supertag-mirror--format-timestamp-for-payload' 可用则复用.
若不可用, 则尝试将 Emacs 时间列表转换为 YYYY-MM-DD HH:MM:SS 格式."
  (cond
   ((fboundp 'org-supertag-mirror--format-timestamp-for-payload)
    (org-supertag-mirror--format-timestamp-for-payload time-val))
   ((stringp time-val) ; Already a string, assume it's correct
    time-val)
   ((and (listp time-val) (time-less-p nil time-val)) ; Check if it's a valid Emacs time list
    (format-time-string "%Y-%m-%d %H:%M:%S" time-val))
   (t ; Fallback if not a string or valid time list
    (org-supertag-bridge--log "Warning: Could not format timestamp: %S" time-val)
    nil)))

(defun org-supertag-bridge--get-node-snapshot (&optional since-time)
  "Get all nodes modified since SINCE-TIME.
If SINCE-TIME is nil, get all nodes.
This prepares node data for Python, excluding bulky fields."
  (let ((nodes '()))
    (maphash
     (lambda (id node-data)
       (when (eq (plist-get node-data :type) :node)
         (let ((updated-at (plist-get node-data :updated-at)))
           (when (or (not since-time)
                     (and updated-at (string< since-time updated-at)))
             (push (org-supertag-bridge--prepare-object-for-python node-data) nodes)))))
     org-supertag-db--object)
    (nreverse nodes)))

(defun org-supertag-bridge--get-tag-snapshot (&optional since-time)
  "Get all tags modified since SINCE-TIME.
If SINCE-TIME is nil, get all tags.
This prepares tag data for Python, excluding bulky fields."
  (let ((tags '()))
    (maphash
     (lambda (id tag-data)
       (when (eq (plist-get tag-data :type) :tag)
         (let ((updated-at (plist-get tag-data :updated-at)))
           (when (or (not since-time)
                     (and updated-at (string< since-time updated-at)))
             (push (org-supertag-bridge--prepare-object-for-python tag-data) tags)))))
     org-supertag-db--object)
    (nreverse tags)))

(defun org-supertag-bridge--get-link-catalog (&optional since-time)
  "返回边目录 ((LINK-ID TYPE FROM-ID TO-ID PROPERTIES MODIFIED))."
  (when (and (hash-table-p org-supertag-db--link) 
             (= 0 (hash-table-count org-supertag-db--link)) 
             (fboundp 'org-supertag-db-load))
    (org-supertag-bridge--log "Get-link-catalog: org-supertag-db--link is empty, attempting to load DB...")
    (condition-case err
        (org-supertag-db-load)
      (error (org-supertag-bridge--log "Get-link-catalog: Error during org-supertag-db-load: %S" err))))
  
  (org-supertag-bridge--log "Get-link-catalog called. since-time: %S. DB-link size: %S"
                           since-time
                           (if (hash-table-p org-supertag-db--link)
                               (hash-table-count org-supertag-db--link)
                             "org-supertag-db--link is not a hash-table or is nil"))
  
  (let (result)
    (if (not (hash-table-p org-supertag-db--link))
        (progn
          (org-supertag-bridge--log "Get-link-catalog: Error - org-supertag-db--link is not a hash-table or is nil!")
          nil)
      (maphash
       (lambda (lid plist)
         (let* ((mtime (org-supertag-bridge--format-ts (plist-get plist :modified-at)))
                (include-p (or (null since-time) 
                             (and mtime (string> mtime since-time)))))
           (when include-p
             (push (list (format "%s" lid)
                        (format "%s" (plist-get plist :type))
                        (format "%s" (plist-get plist :from))
                        (format "%s" (plist-get plist :to))
                        (or (plist-get plist :properties) '())
                        mtime)
                   result))))
       org-supertag-db--link))
    (org-supertag-bridge--log "Get-link-catalog: Returning %d links." (length result))
    (nreverse result)))

(defun org-supertag-bridge--ping ()
  "Internal ping function for Python bridge to check Emacs readiness.
Returns 'pong' to indicate Emacs is ready."
  (message "[org-supertag-bridge--ping] Received ping from Python bridge.")
  "pong")

(defun org-supertag-bridge--should-sync-node-p (props)
  "Return non-nil if the node with PROPS should be synced."
  ;; For now, we sync all nodes. This can be extended with filters.
  (eq (plist-get props :type) :node))

(defun org-supertag-bridge--sync-single-node-to-python (id props)
  "Sync a single node's PROPS to the Python bridge using the new simplified API.
The PROPS plist is sent directly, and Python handles the data transformation."
  (org-supertag-bridge--log (format "Syncing node to Python: %s" id))
  ;; The props plist is already in a format that the EPC bridge can serialize
  ;; correctly into a Python dictionary.
  (org-supertag-bridge--call-async 'sync_node_from_elisp props))

;;;###autoload
(defun org-supertag-bridge-init ()
  "Initialize the bridge and its connections to the database."
  (interactive)
  (org-supertag-bridge--log "Initializing bridge and DB listeners...")
  ;; Add a listener to sync node changes to Python
  (org-supertag-db-add-listener
   'entity:changed
   (lambda (id props)
     (when (org-supertag-bridge--should-sync-node-p props)
       (org-supertag-bridge--sync-single-node-to-python id props)))))

(defun org-supertag-bridge--call-python-epc-sync (method-name &rest args)
  (apply #'epc:call-sync org-supertag-bridge--epc-connection method-name args))

;; --- Incremental Sync Helpers ---

(defun org-supertag-bridge--get-node-snapshot (&optional since-time)
  "Get all nodes modified since SINCE-TIME.
If SINCE-TIME is nil, get all nodes.
This prepares node data for Python, excluding bulky fields."
  (let ((nodes '()))
    (maphash
     (lambda (id node-data)
       (when (eq (plist-get node-data :type) :node)
         (let ((updated-at (plist-get node-data :updated-at)))
           (when (or (not since-time)
                     (and updated-at (string< since-time updated-at)))
             (push (org-supertag-bridge--prepare-object-for-python node-data) nodes)))))
     org-supertag-db--object)
    (nreverse nodes)))

(defun org-supertag-bridge--get-tag-snapshot (&optional since-time)
  "Get all tags modified since SINCE-TIME.
If SINCE-TIME is nil, get all tags.
This prepares tag data for Python, excluding bulky fields."
  (let ((tags '()))
    (maphash
     (lambda (id tag-data)
       (when (eq (plist-get tag-data :type) :tag)
         (let ((updated-at (plist-get tag-data :updated-at)))
           (when (or (not since-time)
                     (and updated-at (string< since-time updated-at)))
             (push (org-supertag-bridge--prepare-object-for-python tag-data) tags)))))
     org-supertag-db--object)
    (nreverse tags)))

(defun org-supertag-bridge--get-link-catalog (&optional since-time)
  "Get all links modified since SINCE-TIME.
If SINCE-TIME is nil, get all links."
  (let ((links '()))
    (maphash
     (lambda (id link-data)
       (let ((updated-at (plist-get link-data :updated-at)))
         (when (or (not since-time)
                   (and updated-at (string< since-time updated-at)))
           (push (org-supertag-bridge--prepare-object-for-python link-data) links))))
     org-supertag-db--link)
    (nreverse links)))

(defun org-supertag-bridge--prepare-object-for-python (props)
  "Prepare a plist PROPS for sending to Python.
Removes Elisp-specific properties like buffer objects."
  (let ((clean-props (copy-sequence props)))
    ;; Remove any properties that are not serializable or useful for Python
    (dolist (key '(:buffer))
      (setq clean-props (org-supertag-bridge--plist-remove key clean-props)))
    
    ;; 确保有哈希值
    (unless (plist-get clean-props :hash)
      (setq clean-props 
            (plist-put clean-props :hash 
                      (org-supertag-node-hash clean-props))))
    
    ;; 如果启用了内容检查，确保有内容哈希
    (when org-supertag-sync-check-contents
      (unless (plist-get clean-props :content-hash)
        (setq clean-props
              (plist-put clean-props :content-hash
                        (org-supertag-node-content-hash clean-props)))))
    
    clean-props))

(defun org-supertag-bridge--plist-remove (key plist)
  "Remove KEY from PLIST and return the modified plist."
  (let ((result '())
        (tail plist))
    (while tail
      (let ((current-key (car tail))
            (current-value (cadr tail)))
        (unless (eq current-key key)
          (setq result (append result (list current-key current-value))))
        (setq tail (cddr tail))))
    result))

(defun org-supertag-bridge-sync-incremental (changed-nodes changed-tags changed-links)
  "发送增量变化到Python后端进行同步。
CHANGED-NODES: 变化的节点列表，格式为 ((id . props) ...)
CHANGED-TAGS: 变化的标签列表，格式为 ((id . props) ...)  
CHANGED-LINKS: 变化的链接列表，格式为 ((id . props) ...)

返回同步结果plist，包含 :success 和 :error 字段。"
  (condition-case err
      (let* ((nodes-for-python (mapcar 
                               (lambda (pair)
                                 (let* ((id (car pair))
                                        (props (cdr pair))
                                        (title (or (plist-get props :title) ""))
                                        (content (or (plist-get props :content) ""))
                                        (tags (or (plist-get props :tags) '()))
                                        (file-path (or (plist-get props :file-path) ""))
                                        (modified-at (or (plist-get props :modified-at) "")))
                                   ;; 使用与 bulk_process_snapshot 兼容的格式
                                   (list id title content tags file-path modified-at)))
                               changed-nodes))
             (links-for-python (mapcar
                               (lambda (pair)
                                 (let* ((id (car pair))
                                        (props (cdr pair)))
                                   ;; 转换链接格式
                                   (list id
                                         (plist-get props :type)
                                         (plist-get props :from)
                                         (plist-get props :to)
                                         (plist-get props :properties)
                                         (plist-get props :modified-at))))
                               changed-links))
             (sync-data `((:nodes . ,nodes-for-python)
                         (:links . ,links-for-python)
                         (:sync_timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))))
             ;; 使用现有的 bulk_process_snapshot 方法
             (result (org-supertag-bridge-call-sync "bulk_process_snapshot" 120 sync-data)))
        
        (if (and result (plist-get result :status))
            (cond 
             ((string= (plist-get result :status) "success")
              `(:success t :message ,(or (plist-get result :message) "Incremental sync completed")))
             (t 
              `(:success nil :error ,(plist-get result :message))))
          `(:success nil :error "Invalid response from Python backend")))
    
    (error
     `(:success nil :error ,(error-message-string err)))))

(defun org-supertag-bridge-sync-incremental-async (changed-nodes changed-tags changed-links callback)
  "异步发送增量变化到Python后端进行同步。
CHANGED-NODES: 变化的节点列表，格式为 ((id . props) ...)
CHANGED-TAGS: 变化的标签列表，格式为 ((id . props) ...)  
CHANGED-LINKS: 变化的链接列表，格式为 ((id . props) ...)
CALLBACK: 回调函数，接收同步结果plist，包含 :success 和 :error 字段。"
  (condition-case err
      (let* ((nodes-for-python (mapcar 
                               (lambda (pair)
                                 (let* ((id (car pair))
                                        (props (cdr pair))
                                        (title (or (plist-get props :title) ""))
                                        (content (or (plist-get props :content) ""))
                                        (tags (or (plist-get props :tags) '()))
                                        (file-path (or (plist-get props :file-path) ""))
                                        (modified-at (or (plist-get props :modified-at) "")))
                                   ;; 使用与 bulk_process_snapshot 兼容的格式
                                   (list id title content tags file-path modified-at)))
                               changed-nodes))
             (links-for-python (mapcar
                               (lambda (pair)
                                 (let* ((id (car pair))
                                        (props (cdr pair)))
                                   ;; 转换链接格式
                                   (list id
                                         (plist-get props :type)
                                         (plist-get props :from)
                                         (plist-get props :to)
                                         (plist-get props :properties)
                                         (plist-get props :modified-at))))
                               changed-links))
             (sync-data `((:nodes . ,nodes-for-python)
                         (:links . ,links-for-python)
                         (:sync_timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))))
        
        ;; 使用异步调用
        (org-supertag-bridge-call-async 
         "bulk_process_snapshot"
         (lambda (result)
           (let ((processed-result
                  (if (and result (plist-get result :status))
                      (cond 
                       ((string= (plist-get result :status) "success")
                        `(:success t :message ,(or (plist-get result :message) "Incremental sync completed")))
                       (t 
                        `(:success nil :error ,(plist-get result :message))))
                    `(:success nil :error "Invalid response from Python backend"))))
             (when callback
               (funcall callback processed-result))))
         sync-data))
    
    (error
     (when callback
       (funcall callback `(:success nil :error ,(error-message-string err)))))))

(provide 'org-supertag-bridge)

;;; org-supertag-bridge.el ends here

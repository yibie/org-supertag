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

(defcustom org-supertag-bridge-python-command (expand-file-name ".venv/bin/python" org-supertag-project-root)
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

(defvar org-supertag-bridge-ready-hook nil
  "Hook run after the Python bridge is successfully initialized.
Functions added to this hook will be run without arguments.")

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
  "This function is now DEPRECATED.
The new connection logic no longer requires a persistent Emacs-side server
for the initial handshake. Python server is now polled directly."
  (message "org-supertag-bridge--start-emacs-epc-server is deprecated and should not be called.")
  nil)

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

;; NEW: Handler for 'simtag-bridge/log' called by Python
(defun org-supertag-bridge-epc-log-message (message-string)
  "Receives a MESSAGE-STRING from Python and logs it with a [Python] prefix."
  (org-supertag-bridge--log "[Python] %s" message-string))

;; Handler for Python's readiness signal
(defun org-supertag-bridge--handle-python-server-ready-signal (args)
  "This function is now DEPRECATED.
The new connection logic polls the Python server directly."
  (message "org-supertag-bridge--handle-python-server-ready-signal is deprecated.")
  nil)

;; =============================================================================
;; Python Process Management
;; =============================================================================
(cl-defun org-supertag-bridge-start-process ()
  "Start the `simtag_bridge.py` process and poll it until it's ready."
  (interactive)
  (if (and (processp org-supertag-bridge--python-process)
           (process-live-p org-supertag-bridge--python-process))
      (progn
        (org-supertag-bridge--log "SimTagBridge Python process is already live. Skipping new process start.")
        (message "[OrgSuperTagBridge] Process already running."))
    (org-supertag-bridge--log "Attempting to start SimTagBridge process pipeline...")
    
    (when (and (processp org-supertag-bridge--python-process)
               (not (process-live-p org-supertag-bridge--python-process)))
      (org-supertag-bridge--log "Stale process object found. Cleaning up before starting new process.")
      (org-supertag-bridge-kill-process))

    ;; 1. Prepare and launch the simtag_bridge.py script
    (setq org-supertag-bridge--ready-p nil)
    (let* ((python-cmd org-supertag-bridge-python-command)
           ;; CRITICAL: Use the centralized project root variable.
           (project-root org-supertag-project-root)
           (port-file (expand-file-name "simtag_bridge.port" org-supertag-data-directory))
           (args (list "-m" "simtag.simtag_bridge" "--port-file" port-file "--data-directory" org-supertag-data-directory)))

      (when (file-exists-p port-file)
        (delete-file port-file)) ; Clean up old port file

      (setq org-supertag-bridge--python-program-to-run python-cmd)
      (setq org-supertag-bridge--python-program-args (append args (when org-supertag-bridge-enable-profile (list "--profile"))))

      (org-supertag-bridge--log "Starting Python process in directory: %s" project-root)
      (org-supertag-bridge--log "Command: %s %s" python-cmd (string-join org-supertag-bridge--python-program-args " "))

      ;; CRITICAL FIX: Use `let` to dynamically bind `default-directory` for the child process.
      ;; This ensures the process starts in the correct project root, regardless of the current buffer's directory.
      (let ((default-directory project-root))
        (setq org-supertag-bridge--python-process (apply #'start-process "SimTagBridge-Python" (get-buffer-create org-supertag-bridge-process-buffer-name) python-cmd org-supertag-bridge--python-program-args)))
      
      (when (processp org-supertag-bridge--python-process)
        (set-process-sentinel org-supertag-bridge--python-process #'org-supertag-bridge-process-sentinel)
        (org-supertag-bridge--log "Python process started. Polling for port file and connection...")
        (org-supertag-bridge--poll-for-connection port-file 10)))))

(defun org-supertag-bridge--poll-for-connection (port-file max-wait-seconds)
  "Poll for PORT-FILE to appear and contain the port number."
  (let ((start-time (current-time))
        (port nil))
    (while (and (not port) (< (time-to-seconds (time-subtract (current-time) start-time)) max-wait-seconds))
      (when (file-exists-p port-file)
        (with-temp-buffer
          (insert-file-contents port-file)
          (setq port (string-to-number (buffer-string)))))
      (unless port
        (sleep-for 0.2)))

    (if (not port)
        (progn
          (org-supertag-bridge--log "ERROR: Python server did not write port file in time.")
          (message "[OrgSuperTagBridge] Error: Connection timed out.")
          (org-supertag-bridge-kill-process))
      (org-supertag-bridge--log "Got port %d from file. Attempting to connect..." port)
      (condition-case-unless-debug err
          (let ((manager (make-org-supertag-bridge-epc-manager
                          :server-process org-supertag-bridge--python-process
                          :commands (cons org-supertag-bridge--python-program-to-run org-supertag-bridge--python-program-args)
                          :title (format "OrgSuperTagBridge-Client-to-Python:%s" port)
                          :port port
                          :connection (org-supertag-bridge-epc-connect "127.0.0.1" port))))
            (setq org-supertag-bridge--python-epc-manager manager)
            (org-supertag-bridge-epc-init-epc-layer manager)
            ;; Final check using a simple ping
            (if (equal "pong" (org-supertag-bridge-call-sync "ping" nil 5))
                (progn
                  (setq org-supertag-bridge--ready-p t)
                  (message "[OrgSuperTagBridge] Successfully connected to SimTagBridge Python server on port %d." port)
                  (org-supertag-bridge--log "✅ Connection established. Running ready hook...")
                  (run-hooks 'org-supertag-bridge-ready-hook))
              (error "Ping to Python server failed.")))
        (error
         (org-supertag-bridge--log "ERROR connecting to Python server: %S" err)
         (message "[OrgSuperTagBridge] Error connecting to Python server: %s" (error-message-string err))
         (org-supertag-bridge-kill-process))))))

(defun org-supertag-bridge-process-sentinel (process event)
  "Sentinel function for the Python process.
Handles process termination and reports errors."
  (org-supertag-bridge--log "Process sentinel triggered for %s with event: %s" process event)
  (let ((exit-status (process-status process)))
    (unless (memq exit-status '(run signal)) ; Ignore normal running or signal-based termination for now
      (setq org-supertag-bridge--ready-p nil)
      (setq org-supertag-bridge--python-process nil)
      (org-supertag-bridge--log "Python process terminated. Status: %S" exit-status)
      (message "[OrgSuperTagBridge] Python process terminated: %s" event)
      (let ((output-buffer (process-buffer process)))
        (when (buffer-live-p output-buffer)
          (with-current-buffer output-buffer
            (let ((output (buffer-string)))
              (when (string-match-p "\\S-" output) ; If there's non-whitespace output
                (org-supertag-bridge--log "--- Python Process Output ---")
                (org-supertag-bridge--log output)
                (org-supertag-bridge--log "---------------------------")
                (display-buffer output-buffer) ; Show the user what went wrong
                (message "[OrgSuperTagBridge] Python process exited with an error. See %s for details."
                         (buffer-name output-buffer))))))))))

(defun org-supertag-bridge-kill-process ()
  "Kill the `simtag_bridge.py` process and associated EPC connections."
  (interactive)
  (org-supertag-bridge--log "Attempting to kill SimTagBridge Python process and EPC connections...")
  
  ;; 1. Politely ask Python server to cleanup (if connected)
  (when (and org-supertag-bridge--python-epc-manager (org-supertag-bridge-epc-live-p org-supertag-bridge--python-epc-manager) org-supertag-bridge--ready-p)
    (org-supertag-bridge--log "Sending 'cleanup' request to SimTagBridge Python server...")
    (let ((debug-on-error nil)) ; Temporarily disable debugger for this specific call
      (condition-case err
          (org-supertag-bridge-call-sync "cleanup" nil 5) ; Add a timeout, e.g. 5 seconds
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
(defun org-supertag-bridge-ready-p ()
  "Return non-nil if the bridge to Python is fully initialized and ready."
  org-supertag-bridge--ready-p)

(defun org-supertag-bridge-call-async (method-name params callback)
  "Asynchronously call a Python METHOD-NAME via EPC.

The caller is responsible for ensuring that PARAMS adheres to the
established data contract: for complex data, it must be an
association list (alist) wrapped in a single-element list, e.g.,
`(list payload-alist)`.

See `simtag/utils/unified_tag_processor.py` for the definitive data contract.

METHOD-NAME: A symbol or string for the Python method name.
PARAMS: A list containing all positional arguments for the Python method.
CALLBACK: A function to handle the async result. It will be called
          with one argument: either the result from Python, or a
          list like '(:error \"description\")' on failure."

  ;; 确保 EPC 连接已就绪
  (unless (org-supertag-bridge--ensure-server-running)
    (when callback
      (funcall callback (list :error "Python server is not running.")))
    (error "Python server is not running."))

  ;; 进行异步调用
  (let ((d (org-supertag-bridge-epc-call-deferred
            org-supertag-bridge--python-epc-manager
            (intern method-name)
            (or params nil))))
    ;; 只有在提供回调时才注册统一的成功/失败处理
    (when callback
      (org-supertag-bridge-deferred-nextc d
        (lambda (result)
          (when org-supertag-bridge-enable-verbose-async-debug
            (org-supertag-bridge--log "Async SUCCESS for '%s': %S" method-name result))
          (funcall callback result))))
    d))

(defun org-supertag-bridge--ensure-server-running ()
  "Check if the bridge is ready, and try to start it if not.
Returns t if ready, nil otherwise."
  (or org-supertag-bridge--ready-p
      (progn
        (org-supertag-bridge--log "Server not ready. Attempting to start...")
        (org-supertag-bridge-start-process)
        ;; After attempting to start, we check readiness again.
        ;; There might be a delay, so a better implementation might involve
        ;; waiting or checking status differently, but this is a start.
        org-supertag-bridge--ready-p)))

(defun org-supertag-bridge-call-sync (method-name params &optional timeout)
  "Synchronously call a Python METHOD-NAME via EPC.

The caller is responsible for ensuring that PARAMS adheres to the
established data contract: for complex data, it must be an
association list (alist) wrapped in a single-element list, e.g.,
`(list payload-alist)`.

See `simtag/utils/unified_tag_processor.py` for the definitive data contract.

METHOD-NAME: A symbol or string for the Python method name.
PARAMS: A list containing all positional arguments for the Python method.
TIMEOUT: Timeout in seconds (default 60)."

  (org-supertag-bridge--log "Sync Call to '%s' with params: %S (timeout %s)" method-name params timeout)
  (unless (org-supertag-bridge-ready-p)
    (org-supertag-bridge--log "Bridge not ready for sync call. Starting process.")
    (org-supertag-bridge-start-process))

  (org-supertag-bridge-epc-call-sync
   org-supertag-bridge--python-epc-manager
   (intern method-name)
   (or params nil)
   (or timeout 60)))

;; =============================================================================
;; Helper functions to call into EPC layer
;; =============================================================================
;; NOTE: These helper functions were incorrect and have been removed.
;; The main public API functions now call the EPC layer directly.


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
              (max-attempts (* wait-timeout 10))) ; Check every 0.1 seconds
          (while (and (not org-supertag-bridge--ready-p) (< attempts max-attempts))
            (sit-for 0.1)
            (setq attempts (1+ attempts))
            (when (= 0 (% attempts 20)) ; Log every 2 seconds
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
;; AI/LLM Convenience Functions
;; =============================================================================

(defun org-supertag-bridge-fetch-available-models ()
  "Fetch the list of available AI models via the Python bridge.
Returns a list of model name strings on success, or signals an error."
  (interactive)
  (org-supertag-bridge--log "Fetching available AI models via bridge...")
  (unless (org-supertag-bridge-ensure-ready)
    (error "Bridge is not ready. Cannot fetch models."))
  (let ((response (org-supertag-bridge-call-sync "get_available_models" nil)))
    (org-supertag-bridge--log "Raw response for models: %S" response)
    (if (and (listp response) (plistp response) (string= (plist-get response :status) "success"))
        (let ((result (plist-get response :result)))
          (if (listp result)
              (progn
                (org-supertag-bridge--log "Successfully fetched %d models." (length result))
                result)
            (error "Invalid result format for models: Expected list, got %S" result)))
      (error "Error fetching models from backend: %S" response))))

(defun org-supertag-bridge-select-model ()
  "Interactively select an available AI model.
Fetches the list of models from the backend and presents them for
selection in the minibuffer. Inserts the selected model name into
the current buffer at point."
  (interactive)
  (condition-case err
      (let* ((available-models (org-supertag-bridge-fetch-available-models))
             (selected-model nil))
        (if available-models
            (setq selected-model
                  (completing-read "Select Model: "
                                   available-models
                                   nil ; predicate
                                   t   ; require-match
                                   nil ; initial-input
                                   nil ; history var
                                   (car available-models))) ; default
          (message "No available models found."))
        (if (and selected-model (not (string-empty-p selected-model)))
            (progn
              (insert selected-model)
              (message "Inserted model: %s" selected-model))
          (message "No model selected.")))
    (error
     (message "Error selecting model: %s" (error-message-string err)))))

(defun org-supertag-bridge-llm-invoke (prompt &key system-prompt model temperature max-tokens)
  "Synchronously invoke an LLM via the Python bridge.
This is a high-level wrapper that sends a request to the `reasoning_invoke`
method in the Python backend.

PROMPT: The main user prompt string.
:system-prompt: Optional system prompt string.
:model: Optional model name string to override the default.
:temperature: Optional temperature override.
:max-tokens: Optional max-tokens override.

Returns the AI response string on success, or signals an error."
  (org-supertag-bridge--log "Invoking LLM with prompt: %s..." (truncate-string-to-width prompt 80))
  (unless (org-supertag-bridge-ensure-ready)
    (error "Bridge is not ready. Cannot invoke LLM."))

  (let* ((payload-alist `((prompt . ,prompt)
                         ,@(when system-prompt `((system_prompt . ,system-prompt)))
                         ,@(when model `((model . ,model)))
                         ,@(when temperature `((temperature . ,temperature)))
                         ,@(when max-tokens `((max_tokens . ,max-tokens)))))
         (response (org-supertag-bridge-call-sync "reasoning_invoke" (list payload-alist))))

    (org-supertag-bridge--log "Raw LLM response: %S" response)
    (if (and (listp response) (plistp response) (string= (plist-get response :status) "success"))
        (let ((result (plist-get response :result)))
          (org-supertag-bridge--log "LLM call successful.")
          result)
      (error "LLM call failed: %S" response))))

(provide 'org-supertag-bridge)

;;; org-supertag-bridge.el ends here

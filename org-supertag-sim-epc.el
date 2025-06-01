;;; org-supertag-sim-epc.el --- tag similarity service based on EPC -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Your Name
;; Keywords: convenience, tools
;; Version: 1.0.0

;;; Commentary:

;; This package provides tag similarity service through Python EPC bridge,
;; designed with architecture patterns inspired by blink-search.

;;; Code:

(require 'epc)
(require 'cl-lib)
(require 'deferred)

;; Define deferred chain macro (from blink-search)
(cl-defmacro org-supertag-sim-epc-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; =============================================================================
;; Variables & Constants
;; =============================================================================

(defgroup org-supertag-sim-epc nil
  "Tag similarity service via EPC."
  :group 'convenience
  :prefix "org-supertag-sim-epc-")

(defcustom org-supertag-sim-epc-python-command "python"
  "Python command for starting EPC server."
  :type 'string
  :group 'org-supertag-sim-epc)

(defcustom org-supertag-sim-epc-enable-log nil
  "Enable logging for debugging."
  :type 'boolean
  :group 'org-supertag-sim-epc)

;; Internal variables
(defvar org-supertag-sim-epc-server-process nil
  "EPC server process.")

(defvar org-supertag-sim-epc-manager nil
  "EPC manager instance.")

(defvar org-supertag-sim-epc-server-port nil
  "EPC server port.")

(defvar org-supertag-sim-epc-root-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of org-supertag-sim-epc.")

(defvar org-supertag-sim-epc-venv-dir
  (expand-file-name ".venv" org-supertag-sim-epc-root-directory)
  "Virtual environment directory.")

(defvar org-supertag-sim-epc-message-queue nil)
(defvar org-supertag-sim-epc-message-thread nil)

(defvar org-supertag-sim-epc-server-ready-p nil
  "Whether EPC server is ready.")

(defvar org-supertag-sim-epc-connection-established-hook nil
  "Hook run when EPC connection is successfully established.
Functions in this hook can safely use EPC methods.")

(defun org-supertag-sim-epc-queue-create ()
  "Create a thread-safe queue."
  (cons nil nil))

(defun org-supertag-sim-epc-queue-enqueue (item queue)
  "Add ITEM to the end of QUEUE."
  (let ((new-cell (list item)))
    (if (car queue)
        (setcdr (cdr queue) new-cell)
      (setcar queue new-cell))
    (setcdr queue new-cell)))

(defun org-supertag-sim-epc-queue-dequeue (queue)
  "Remove and return the first item from QUEUE."
  (when (car queue)
    (let ((item (caar queue)))
      (setcar queue (cdar queue))
      (unless (car queue)
        (setcdr queue nil))
      item)))
(defvar org-supertag-sim-epc-state-table (make-hash-table :test 'equal))

;; =============================================================================
;; Core Utility Functions
;; =============================================================================

(defun org-supertag-sim-epc-log (format-string &rest args)
  "Log message with FORMAT-STRING and ARGS if logging is enabled."
  (when org-supertag-sim-epc-enable-log
    (with-current-buffer (get-buffer-create "*org-supertag-sim-epc-log*")
      (goto-char (point-max))
      (insert (format "[%s] %s\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S")
                     (apply #'format format-string args))))))

(defun org-supertag-sim-epc-get-python-command ()
  "Get Python command for virtual environment or system."
  (let ((venv-python (expand-file-name "bin/python" org-supertag-sim-epc-venv-dir)))
    (if (file-executable-p venv-python)
        venv-python
      org-supertag-sim-epc-python-command)))

(defun org-supertag-sim-epc-get-server-script-path ()
  "Get path to EPC server script."
  (expand-file-name "simtag/epc_server.py" org-supertag-sim-epc-root-directory))

;; =============================================================================
;; EPC Server Management
;; =============================================================================

(defun org-supertag-sim-epc-server-alive-p ()
  "Check if EPC server process is alive."
  (and org-supertag-sim-epc-server-process
       (process-live-p org-supertag-sim-epc-server-process)))

(defun org-supertag-sim-epc-manager-live-p ()
  "Check if EPC manager connection is live."
  (and org-supertag-sim-epc-manager
       (epc:live-p org-supertag-sim-epc-manager)))

(defun org-supertag-sim-epc-kill-server ()
  "Kill EPC server process."
  (interactive)
  (org-supertag-sim-epc-log "Killing EPC server...")
  
  ;; Close EPC manager connection
  (when org-supertag-sim-epc-manager
    (condition-case err
        (epc:stop-epc org-supertag-sim-epc-manager)
      (error (org-supertag-sim-epc-log "Error stopping EPC manager: %s" err)))
    (setq org-supertag-sim-epc-manager nil))
  
  ;; Kill server process
  (when (org-supertag-sim-epc-server-alive-p)
    (condition-case err
        (progn
          (delete-process org-supertag-sim-epc-server-process)
          (setq org-supertag-sim-epc-server-process nil))
      (error (org-supertag-sim-epc-log "Error deleting process: %s" err))))
  
  ;; Reset state
  (setq org-supertag-sim-epc-server-port nil
        org-supertag-sim-epc-server-ready-p nil)
  
  (org-supertag-sim-epc-log "EPC server killed"))

(defun org-supertag-sim-epc-message-dispatcher ()
  "Dispatch messages from Python server."
  (while (org-supertag-sim-epc-manager-live-p)
        (let ((msg (org-supertag-sim-epc-queue-dequeue org-supertag-sim-epc-message-queue)))
      (condition-case err
          (apply #'org-supertag-sim-epc-handler (car msg) (cdr msg))
        (error (org-supertag-sim-epc-log "Message handler error: %s" err))))))

(defun org-supertag-sim-epc-handler (method &rest args)
  "Handle incoming EPC messages."
  (pcase method
    ('update_state (apply #'org-supertag-sim-epc-update-state args))
    ('log_error (org-supertag-sim-epc-log "Server error: %s" (car args)))
    (_ (org-supertag-sim-epc-log "Unknown method: %s" method))))

(defun org-supertag-sim-epc-update-state (state-name value)
  "Update state table and trigger handlers."
  (puthash state-name value org-supertag-sim-epc-state-table)
  (run-hook-with-args 'org-supertag-sim-epc-state-update-hook state-name value))

(defun org-supertag-sim-epc-start-server ()
  "Start EPC server process with simplified logic - no threads."
  (interactive)
  (org-supertag-sim-epc-log "Starting EPC server (simplified)...")
  
  ;; Kill existing server first without complex cleanup
  (when org-supertag-sim-epc-manager
    (setq org-supertag-sim-epc-manager nil))
  (when org-supertag-sim-epc-server-process
    (ignore-errors (delete-process org-supertag-sim-epc-server-process))
    (setq org-supertag-sim-epc-server-process nil))
  
  ;; Reset state
  (setq org-supertag-sim-epc-server-port nil
        org-supertag-sim-epc-server-ready-p nil
        org-supertag-sim-epc-message-queue nil
        org-supertag-sim-epc-message-thread nil)
  
  (condition-case err
      (let* ((python-command (org-supertag-sim-epc-get-python-command))
             (server-script (org-supertag-sim-epc-get-server-script-path))
             (default-directory org-supertag-sim-epc-root-directory)
             (data-directory (expand-file-name "~/.emacs.d/org-supertag"))
             ;; 设置必要的环境变量 - 确保正确传递
             (process-environment
              (append
               (list
                (format "PYTHONPATH=%s" org-supertag-sim-epc-root-directory)
                (format "ORG_SUPERTAG_DATA_DIRECTORY=%s" data-directory)
                (format "VIRTUAL_ENV=%s" org-supertag-sim-epc-venv-dir))
               process-environment)))
        
        ;; 确保数据目录存在
        (unless (file-directory-p data-directory)
          (make-directory data-directory t))
        
        ;; Verify that the server script exists
        (unless (file-exists-p server-script)
          (error "EPC server script not found: %s" server-script))
        
        (org-supertag-sim-epc-log "Python command: %s" python-command)
        (org-supertag-sim-epc-log "Server script: %s" server-script)
        (org-supertag-sim-epc-log "PYTHONPATH: %s" org-supertag-sim-epc-root-directory)
        (org-supertag-sim-epc-log "ORG_SUPERTAG_DATA_DIRECTORY: %s" data-directory)
        
        ;; 创建进程，使用简化的过滤器
        (setq org-supertag-sim-epc-server-process
              (make-process
               :name "org-supertag-sim-epc-server"
               :buffer (get-buffer-create "*org-supertag-sim-epc-server*")
               :command (list python-command server-script)
               :filter #'org-supertag-sim-epc-server-filter
               :sentinel #'org-supertag-sim-epc-server-sentinel
               :noquery t))
        
        ;; 检查进程是否成功启动
        (if (and org-supertag-sim-epc-server-process
                 (process-live-p org-supertag-sim-epc-server-process))
            (org-supertag-sim-epc-log "Server process started successfully")
          (error "Failed to start Python EPC server process")))
    
    (error 
     (org-supertag-sim-epc-log "❌ Error starting server: %s" (error-message-string err))
     (message "Error starting SimTag EPC server: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

(defun org-supertag-sim-epc-server-filter (process output)
  "Simplified filter function for EPC server PROCESS with OUTPUT."
  (let ((buffer (get-buffer-create "*org-supertag-sim-epc-server*")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert output))
    (org-supertag-sim-epc-log "Server output: %s" (string-trim output))
    
    ;; 搜索端口信息，找到后立即连接
    (when (and (null org-supertag-sim-epc-server-port)
               (string-match "EPC_PORT:\\([0-9]+\\)" output))
      (let ((port (string-to-number (match-string 1 output))))
        (setq org-supertag-sim-epc-server-port port)
        (org-supertag-sim-epc-log "Found EPC server port: %d" port)
        
        ;; 使用定时器延迟连接，避免阻塞
        (run-with-timer 2 nil
                       (lambda ()
                         (org-supertag-sim-epc-connect port)))))))

(defun org-supertag-sim-epc-connect (port)
  "Simple connection to EPC server on PORT without retries."
  (org-supertag-sim-epc-log "Connecting to EPC server on port %d..." port)
  
  (condition-case err
      (progn
        ;; 使用正确的EPC连接方式 - epc:connect创建连接，然后手动创建manager
        (let ((connection (epc:connect "127.0.0.1" port)))
          (setq org-supertag-sim-epc-manager 
                (make-epc:manager :server-process nil
                                 :commands nil
                                 :title (format "SimTag-EPC:%d" port)
                                 :port port
                                 :connection connection
                                 :methods nil
                                 :sessions nil
                                 :exit-hooks nil))
          
          (if (and org-supertag-sim-epc-manager
                   (epc:live-p org-supertag-sim-epc-manager))
              (progn
                (setq org-supertag-sim-epc-server-ready-p t)
                (org-supertag-sim-epc-log "✅ EPC connection established successfully")
                (message "SimTag EPC server connected on port %d" port)
                ;; 连接建立后，通知其他模块可以开始使用EPC
                (run-hooks 'org-supertag-sim-epc-connection-established-hook))
            (progn
              (org-supertag-sim-epc-log "❌ EPC connection failed - manager not live")
              (setq org-supertag-sim-epc-manager nil)))))
    
    (error
     (org-supertag-sim-epc-log "❌ EPC connection error: %s" (error-message-string err))
     (setq org-supertag-sim-epc-manager nil))))

(defun org-supertag-sim-epc-restart ()
  "Simple restart using the safe start function."
  (interactive)
  (org-supertag-sim-epc-log "Simple restart initiated...")
  (org-supertag-sim-epc-safe-reset)
  (org-supertag-sim-epc-start-server)
  (message "EPC server simple restart completed"))

(defun org-supertag-sim-epc-check-connection ()
  "定期检查EPC连接状态 - 简化版本"
  (when (and org-supertag-sim-epc-server-port
             (null org-supertag-sim-epc-server-ready-p))
    (condition-case err
        (org-supertag-sim-epc-connect org-supertag-sim-epc-server-port)
      (error 
       (org-supertag-sim-epc-log "连接检查失败: %s" (error-message-string err))))))

(defun org-supertag-sim-epc-server-sentinel (process event)
  "Sentinel function for EPC server PROCESS with EVENT."
  (org-supertag-sim-epc-log "EPC server sentinel: %s - %s" process (string-trim event))
  (unless (process-live-p process)
    (setq org-supertag-sim-epc-server-process nil
          org-supertag-sim-epc-server-ready-p nil)))

(defun org-supertag-sim-epc-connect-to-server-with-retry (port retry-times delay)
  "Connect to EPC server on PORT with RETRY-TIMES retries and DELAY seconds between attempts."
  (org-supertag-sim-epc-log "Connecting to EPC server on port %d (retries: %d, delay: %ds)..." port retry-times delay)
  
  (let ((retry-count 0)
        (connected nil))
    
    (while (and (< retry-count retry-times)
                (not connected))
      (setq retry-count (1+ retry-count))
      (org-supertag-sim-epc-log "Connection attempt %d/%d..." retry-count retry-times)
      
      (condition-case err
          (progn
            ;; 使用 Blink-Search 风格的连接方式
            (let ((connection (epc:connect "127.0.0.1" port)))
              ;; 创建 EPC 管理器
              (setq org-supertag-sim-epc-manager 
                    (make-epc:manager :server-process nil
                                     :commands nil
                                     :title (format "SimTag-EPC:%d" port)
                                     :port port
                                     :connection connection
                                     :methods nil
                                     :sessions nil
                                     :exit-hooks nil))
              
              (org-supertag-sim-epc-log "EPC manager created with connection: %S" connection)
              
              ;; 检查连接是否成功建立
              (if (epc:live-p org-supertag-sim-epc-manager)
                  (progn
                    (setq org-supertag-sim-epc-server-ready-p t
                          connected t)
                    (org-supertag-sim-epc-log "✅ EPC connection established successfully on attempt %d" retry-count)
                    (message "SimTag EPC server connected on port %d" port))
                (progn
                  (org-supertag-sim-epc-log "❌ EPC connection failed on attempt %d - manager not live" retry-count)
                  (setq org-supertag-sim-epc-manager nil)))))
        
        (error
         (org-supertag-sim-epc-log "❌ EPC connection failed on attempt %d: %s" 
                                  retry-count (error-message-string err))
         (setq org-supertag-sim-epc-manager nil)))
      
      ;; 如果连接失败且还有重试次数，则等待指定的延迟时间
      (when (and (not connected) (< retry-count retry-times))
        (org-supertag-sim-epc-log "Waiting %d seconds before next connection attempt..." delay)
        (sleep-for delay)))
    
    ;; 如果所有重试都失败，则报错
    (unless connected
      (org-supertag-sim-epc-log "❌ Failed to connect to EPC server after %d attempts" retry-times)
      (error "Failed to connect to EPC server after %d attempts (each delayed by %ds)" retry-times delay))))

(defun org-supertag-sim-epc-connect-to-server (port)
  "Connect to EPC server on PORT (legacy function for compatibility)."
  (org-supertag-sim-epc-connect-to-server-with-retry port 1 1))

;; =============================================================================
;; EPC Method Calls (Interface for org-supertag-sim.el)
;; =============================================================================

(defun org-supertag-sim-epc-call-method (method &rest args)
  "Call EPC METHOD with ARGS safely with timeout - SIMPLE VERSION."
  (unless (org-supertag-sim-epc-manager-live-p)
    (error "EPC manager not live"))
  
  (org-supertag-sim-epc-log "Calling EPC method: %s with args: %S" method args)
  
  (condition-case err
      (let ((start-time (current-time))
            (timeout 10) ;; Configurable timeout
            (result 'timeout)
            (deferred-obj (epc:call-deferred org-supertag-sim-epc-manager method args))
            (done nil))
        
        (deferred:nextc deferred-obj
          (lambda (response)
            (org-supertag-sim-epc-log "Deferred:nextc - success callback triggered. Response: %S" response)
            (setq result response done t)))
        (deferred:error deferred-obj
          (lambda (err-from-deferred)
            (org-supertag-sim-epc-log "Deferred:error - error callback triggered. Error: %S" err-from-deferred)
            (setq result err-from-deferred done t)))
        
        (org-supertag-sim-epc-log "Initial deferred object: %S" deferred-obj)

        (while (and (not done)
                    (< (float-time (time-subtract (current-time) start-time)) timeout))
          (accept-process-output nil 0.1) ;; Yield and process I/O
          (when (and done (not (eq result 'timeout)))
            (org-supertag-sim-epc-log "Loop: 'done' is t, result is: %S. Exiting loop." result)))
        
        (if done
            (progn
              (org-supertag-sim-epc-log "EPC call final result: %S" result)
              result)
          (progn
            (org-supertag-sim-epc-log "EPC call timeout in wait loop. Deferred object: %S" deferred-obj)
            (error "EPC call timeout"))))
    (error
     (org-supertag-sim-epc-log "EPC call error (outer condition-case): %S" err)
     (signal (car err) (cdr err)))))

;; Add a purely async version for testing
(defun org-supertag-sim-epc-call-method-async (method args callback)
  "Call EPC METHOD with ARGS asynchronously and call CALLBACK with result."
  (unless (org-supertag-sim-epc-manager-live-p)
    (error "EPC manager not live"))
  
  (org-supertag-sim-epc-log "Calling EPC method async: %s with args: %S" method args)
  
  (deferred:$
    (epc:call-deferred org-supertag-sim-epc-manager method args)
    (deferred:nextc it
      (lambda (response)
        (org-supertag-sim-epc-log "Async EPC method %s succeeded: %S" method response)
        (when callback (funcall callback response))))
    (deferred:error it
      (lambda (err)
        (org-supertag-sim-epc-log "Async EPC method %s failed: %S" method err)
        (when callback (funcall callback nil))))))

;; =============================================================================
;; Public API Functions (Compatible with org-supertag-sim.el)
;; =============================================================================

(defvar org-supertag-sim-epc-state-update-hook nil
  "Hook run when server state updates. Functions take two arguments: STATE-NAME and VALUE.")

(defun org-supertag-sim-epc-call-find-similar (tags &optional limit threshold)
  "Find similar tags to TAGS with optional LIMIT and THRESHOLD."
  (org-supertag-sim-epc-call-method 'find_similar tags (or limit 10) (or threshold 0.5)))

(defun org-supertag-sim-epc-call-suggest-tags (content &optional limit)
  "Suggest tags for CONTENT with optional LIMIT."
  (org-supertag-sim-epc-call-method 'suggest_tags content (or limit 5)))

(defun org-supertag-sim-epc-call-extract-entities (text)
  "Extract entities from TEXT."
  (org-supertag-sim-epc-call-method 'extract_entities text))

(defun org-supertag-sim-epc-call-analyze-relations (tags)
  "Analyze relations between TAGS."
  (org-supertag-sim-epc-call-method 'analyze_tag_relations tags))

(defun org-supertag-sim-epc-call-get-status ()
  "Get server status."
  (org-supertag-sim-epc-call-method 'get_status))

(defun org-supertag-sim-epc-call-get-config ()
  "Get server configuration."
  (org-supertag-sim-epc-call-method 'get_config))

;; =============================================================================
;; Diagnostic & Management Functions
;; =============================================================================

(defun org-supertag-sim-epc-safe-reset ()
  "Safely reset EPC connection with minimal blocking operations."
  (interactive)
  (org-supertag-sim-epc-log "Starting safe EPC reset...")
  
  ;; Simply set all variables to nil - let Emacs GC handle cleanup
  (setq org-supertag-sim-epc-manager nil
        org-supertag-sim-epc-server-process nil
        org-supertag-sim-epc-server-port nil
        org-supertag-sim-epc-server-ready-p nil
        org-supertag-sim-epc-message-queue nil
        org-supertag-sim-epc-message-thread nil)
  
  ;; Clear state table if it exists
  (when (hash-table-p org-supertag-sim-epc-state-table)
    (clrhash org-supertag-sim-epc-state-table))
  
  (org-supertag-sim-epc-log "✅ Safe reset completed.")
  (message "EPC connection safely reset"))

(defun org-supertag-sim-epc-restart ()
  "Clean restart of EPC server with full state reset."
  (interactive)
  (org-supertag-sim-epc-log "Starting clean restart of EPC server...")
  
  ;; Step 1: Reset all state variables
  (org-supertag-sim-epc-safe-reset)
  
  ;; Step 2: Verify Python script exists
  (let ((server-script (org-supertag-sim-epc-get-server-script-path)))
    (unless (file-exists-p server-script)
      (error "EPC server script not found: %s" server-script)))
  
  ;; Step 3: Start the server using simplified method
  (org-supertag-sim-epc-log "Starting new EPC server...")
  (org-supertag-sim-epc-start-server)
  
  (message "EPC server clean restart initiated"))

(defun org-supertag-sim-epc-show-log ()
  "Show EPC log buffer."
  (interactive)
  (let ((buffer (get-buffer "*org-supertag-sim-epc-log*")))
    (if buffer
        (switch-to-buffer buffer)
      (message "No log buffer found"))))

(defun org-supertag-sim-epc-show-server-output ()
  "Show EPC server output buffer."
  (interactive)
  (let ((buffer (get-buffer "*org-supertag-sim-epc-server*")))
    (if buffer
        (switch-to-buffer buffer)
      (message "No server output buffer found"))))

(defun org-supertag-sim-epc-status ()
  "Display EPC server status information."
  (interactive)
  (let* ((process-alive (and org-supertag-sim-epc-server-process
                            (process-live-p org-supertag-sim-epc-server-process)))
         (manager-exists (not (null org-supertag-sim-epc-manager)))
         (manager-live (and org-supertag-sim-epc-manager
                           (epc:live-p org-supertag-sim-epc-manager)))
         (info (list
                (cons "Process alive" process-alive)
                (cons "Process object" (if org-supertag-sim-epc-server-process 
                                          (format "<%s>" (process-name org-supertag-sim-epc-server-process))
                                        "nil"))
                (cons "Manager exists" manager-exists)
                (cons "Manager live" manager-live) 
                (cons "Manager object" (if org-supertag-sim-epc-manager
                                         (format "<%s>" (type-of org-supertag-sim-epc-manager))
                                       "nil"))
                (cons "Server ready" org-supertag-sim-epc-server-ready-p)
                (cons "Server port" org-supertag-sim-epc-server-port)
                (cons "Python command" (org-supertag-sim-epc-get-python-command))
                (cons "Server script" (org-supertag-sim-epc-get-server-script-path))
                (cons "Virtual env" org-supertag-sim-epc-venv-dir))))
    (message "EPC server status: %S" info)
    info))

;; =============================================================================
;; Setup & Installation Functions
;; =============================================================================

(defun org-supertag-sim-epc-auto-setup ()
  "Automatically setup virtual environment and dependencies."
  (interactive)
  (let ((setup-script (expand-file-name "simtag/setup.sh" org-supertag-sim-epc-root-directory)))
    (if (file-exists-p setup-script)
        (progn
          (message "Starting automatic setup...")
          (async-shell-command (format "cd %s && bash simtag/setup.sh" org-supertag-sim-epc-root-directory)))
      (error "Setup script not found: %s" setup-script))))

;; =============================================================================
;; Compatibility Layer (Preserve existing function names)
;; =============================================================================

;; Keep existing function names for backward compatibility
(defalias 'org-supertag-sim-epc-emergency-restart 'org-supertag-sim-epc-restart)
(defalias 'org-supertag-sim-epc-restart-server 'org-supertag-sim-epc-restart)

;; Main interface function used by org-supertag-sim.el
(defun org-supertag-sim-epc-find-similar-tags (tags &optional limit threshold)
  "Find similar tags (main interface function)."
  (org-supertag-sim-epc-call-find-similar tags limit threshold))

;; =============================================================================
;; Initialization & Cleanup
;; =============================================================================

(defun org-supertag-sim-epc-cleanup ()
  "Cleanup function for mode disable or package unload."
  (org-supertag-sim-epc-kill-server))

;; Cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'org-supertag-sim-epc-cleanup)

(defun org-supertag-sim-epc-reconnect ()
  "Reconnect to existing EPC server without killing the process."
  (interactive)
  (org-supertag-sim-epc-log "Attempting to reconnect to existing EPC server...")
  
  (let ((port org-supertag-sim-epc-server-port))
    (if (not port)
        (progn
          (org-supertag-sim-epc-log "No server port found, starting new server...")
          (org-supertag-sim-epc-start-server))
      
      ;; Clear only the manager, keep the process
      (setq org-supertag-sim-epc-manager nil
            org-supertag-sim-epc-server-ready-p nil)
      
      (org-supertag-sim-epc-log "Reconnecting to port %d..." port)
      (condition-case err
          (org-supertag-sim-epc-connect-to-server-with-retry port 3 1)
        (error
         (org-supertag-sim-epc-log "Reconnect failed: %s" (error-message-string err))
         (message "Reconnect failed: %s" (error-message-string err)))))))

(defun org-supertag-sim-epc-test-startup ()
  "Test EPC server startup and check if environment variables are properly set."
  (interactive)
  (org-supertag-sim-epc-log "Testing EPC server startup...")
  
  ;; 首先清理状态
  (org-supertag-sim-epc-safe-reset)
  
  ;; 启动服务器
  (org-supertag-sim-epc-start-server)
  
  ;; 等待服务器启动并连接
  (message "EPC server startup test initiated. Check *org-supertag-sim-epc-log* buffer for details."))

(provide 'org-supertag-sim-epc)

;;; org-supertag-sim-epc.el ends here

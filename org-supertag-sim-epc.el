;;; org-supertag-sim-epc.el --- tag similarity service based on EPC -*- lexical-binding: t; -*-

;; 依赖
(require 'json)
(require 'org-supertag-db)
(require 'cl-lib)
(require 'epc)  

(defgroup org-supertag-sim-epc nil
  "Tag similarity service based on EPC."
  :group 'org-supertag)

(defcustom org-supertag-sim-epc-python-path 
  (or (executable-find "python3")
      (executable-find "python"))
  "Python interpreter path."
  :type 'string
  :group 'org-supertag-sim-epc)

(defcustom org-supertag-sim-epc-script-path
  (expand-file-name "simtag_epc.py"
                   (file-name-directory
                    (or load-file-name buffer-file-name)))
  "simtag_epc.py script path."
  :type 'string
  :group 'org-supertag-sim-epc)

(defcustom org-supertag-sim-epc-vector-file
  (expand-file-name "tag_vectors.json"
                   (file-name-directory
                    (if (boundp 'org-supertag-db-file)
                        org-supertag-db-file
                      (expand-file-name "supertag-db.el" org-supertag-data-directory))))
  "Tag vector file path."
  :type 'string
  :group 'org-supertag-sim-epc)

(defcustom org-supertag-sim-epc-request-timeout 30
  "Request timeout (seconds)."
  :type 'integer
  :group 'org-supertag-sim-epc)

(defvar org-supertag-sim-epc-manager nil
  "EPC manager object.")

(defvar org-supertag-sim-epc-initialized nil
  "Whether the tag system has been initialized.")

(defvar org-supertag-sim-epc--startup-timer nil
  "Server startup timer.")

(defcustom org-supertag-sim-epc-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "SimTag EPC server directory path."
  :type 'directory
  :group 'org-supertag-sim-epc)

(defcustom org-supertag-sim-epc-data-dir
  (expand-file-name "data" org-supertag-sim-epc-dir)
  "SimTag data directory path."
  :type 'directory
  :group 'org-supertag-sim-epc)

(defcustom org-supertag-sim-epc-venv-dir
  (expand-file-name ".venv" org-supertag-sim-epc-dir)
  "Python virtual environment directory."
  :type 'directory
  :group 'org-supertag-sim-epc)

;; Ensure the data directory exists
(unless (file-exists-p org-supertag-sim-epc-data-dir)
  (make-directory org-supertag-sim-epc-data-dir t))

;; Log function
(defun org-supertag-sim-epc-log (format-string &rest args)
  "Log information.
FORMAT-STRING is the format string
ARGS is the format parameters"
  (let ((msg (apply #'format format-string args)))
    (with-current-buffer (get-buffer-create "*simtag-epc-log*")
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
      (insert msg)
      (insert "\n"))
    (message "SimTag EPC: %s" msg)))

(defun org-supertag-sim-epc--ensure-db-file ()
  "Ensure the database file exists and returns its path and tag data."
  (unless (boundp 'org-supertag-db-file)
    (error "Database file path is not defined"))
  (let* ((db-file org-supertag-db-file)
         (tags (org-supertag-db-find-by-type :tag)))
    ;; Ensure the database file exists
    (unless (file-exists-p db-file)
      (org-supertag-db-save))
    
    ;; Convert tag data to list format
    (let ((tag-list
           (cl-loop for tag in tags
                    for props = (org-supertag-db-get tag)
                    for name = (and props (plist-get props :name))
                    when name
                    collect (list :id tag :name name))))
      (list db-file tag-list))))

(defun org-supertag-sim-epc-debug-env ()
  "Debug Python environment settings."
  (let* ((python-exe org-supertag-sim-epc-python-path)
         (script-dir (file-name-directory org-supertag-sim-epc-script-path))
         (debug-script "
import sys
import os

print('=== Python Environment Debug ===')
print(f'Python version: {sys.version}')
print(f'Current working directory: {os.getcwd()}')
print(f'PYTHONPATH: {os.environ.get(\"PYTHONPATH\", \"Not set\")}')
print(f'Sys.path: {sys.path}')
print('=== End Debug ===')
"))
    (with-temp-buffer
      (call-process python-exe nil t nil "-c" debug-script)
      (message "Python环境信息:\n%s" (buffer-string)))))

(defun org-supertag-sim-epc-start-server ()
  "Start SimTag EPC server."
  (interactive)
  ;; Cancel existing startup timer
  (when org-supertag-sim-epc--startup-timer
    (cancel-timer org-supertag-sim-epc--startup-timer))
  
  ;; Set a new startup timer to execute after 1 second of idle time in Emacs
  (setq org-supertag-sim-epc--startup-timer
        (run-with-idle-timer 
         1 nil  ; Execute after 1 second
         (lambda ()
           (condition-case err
               (org-supertag-sim-epc--start-server-internal)
             (error
              (org-supertag-sim-epc-log "服务器启动失败: %s" err)
              (message "SimTag EPC服务器启动失败，将在下次空闲时重试")))))))

(defun org-supertag-sim-epc--start-server-internal ()
  "Internal function: Start SimTag EPC server."
  (let* ((python-exe org-supertag-sim-epc-python-path)
         (python-file org-supertag-sim-epc-script-path)
         (vector-file org-supertag-sim-epc-vector-file)  ; Use the correct custom variable
         (db-file org-supertag-db-file)                  ; Use the correct custom variable
         (base-dir org-supertag-sim-epc-dir)
         (process-environment 
          (cons (format "PYTHONPATH=%s" base-dir)
                process-environment))
         (default-directory base-dir)
         (process-buffer (get-buffer-create "*simtag-epc-process*")))
    
    ;; Ensure the environment settings are correct
    (org-supertag-sim-epc-setup-venv)
    (org-supertag-sim-epc-check-module-structure)
    
    ;; Record startup information
    (org-supertag-sim-epc-log "Starting server...")
    (org-supertag-sim-epc-log "Working directory: %s" default-directory)
    (org-supertag-sim-epc-log "PYTHONPATH: %s" (getenv "PYTHONPATH"))
    
    (org-supertag-sim-epc-log "Starting EPC server...")
    (org-supertag-sim-epc-log "Python path: %s" python-exe)
    (org-supertag-sim-epc-log "Script path: %s" python-file)
    (org-supertag-sim-epc-log "Startup parameters: %S" (list python-file "--vector-file" vector-file "--db-file" db-file "--debug"))
    
    ;; Clear the process buffer
    (with-current-buffer process-buffer
      (erase-buffer))
    
    ;; Create a process
    (make-process
     :name "simtag-epc"
     :buffer process-buffer
     :command (cons python-exe (list python-file "--vector-file" vector-file "--db-file" db-file "--debug"))
     :filter (lambda (proc output)
               (org-supertag-sim-epc-log "进程输出: %s" output)
               (with-current-buffer (process-buffer proc)
                 (goto-char (point-max))
                 (insert output))
               ;; Check the port number
               (when (string-match "\\([0-9]+\\)[\n\r]" output)
                 (let ((port (string-to-number (match-string 1 output))))
                   (org-supertag-sim-epc-log "Found port number: %d" port)
                   ;; Create EPC connection
                   (condition-case err
                       (progn
                         (setq org-supertag-sim-epc-manager 
                               (epc:start-epc python-exe (list python-file "--vector-file" vector-file "--db-file" db-file "--debug")))
                         (org-supertag-sim-epc-log "EPC connection created"))
                     (error
                      (org-supertag-sim-epc-log "EPC connection failed: %s" 
                                               (error-message-string err))))))))))

(defun org-supertag-sim-epc-stop-server ()
  "Stop SimTag EPC server."
  (when (and org-supertag-sim-epc-manager
             (epc:live-p org-supertag-sim-epc-manager))
    (org-supertag-sim-epc-log "Stopping EPC server...")
    (epc:stop-epc org-supertag-sim-epc-manager)
    (org-supertag-sim-epc-log "EPC server stopped"))
  
  (setq org-supertag-sim-epc-manager nil
        org-supertag-sim-epc-initialized nil))

(defun org-supertag-sim-epc-server-running-p ()
  "Check if the EPC server is running."
  (and org-supertag-sim-epc-manager
       (epc:live-p org-supertag-sim-epc-manager)))

(defun org-supertag-sim-epc-ensure-server ()
  "Ensure the EPC server is running, start it if not running."
  (unless (org-supertag-sim-epc-server-running-p)
    (org-supertag-sim-epc-start-server)))

(defun org-supertag-sim-epc-check-ollama-installed ()
  "Check if Ollama is installed, similar to the check_ollama_installed function in ollama_bridge.py."
  (org-supertag-sim-epc-log "Checking if Ollama is installed...")
  (let ((result 
         (cond
          ;; Windows system
          ((string-match-p "windows" (symbol-name system-type))
           (let ((possible-paths '("C:/Program Files/Ollama/ollama.exe"
                                   "C:/Program Files (x86)/Ollama/ollama.exe"
                                   "~/AppData/Local/Programs/Ollama/ollama.exe"
                                   "~/scoop/apps/ollama/current/ollama.exe"))
                 (found nil))
             (dolist (path possible-paths)
               (when (file-exists-p (expand-file-name path))
                 (setq found t)))
             found))
          ;; Unix system (Linux/macOS)
          (t
           (= 0 (call-process "which" nil nil nil "ollama"))))))
    
    (if result
        (progn
          (org-supertag-sim-epc-log "Ollama is installed")
          t)
      (org-supertag-sim-epc-log "Ollama is not installed")
      nil)))

(defun org-supertag-sim-epc-get-ollama-install-instruction ()
  "Get the installation command for Ollama, similar to get_install_command in ollama_bridge.py."
  (let ((system-type (symbol-name system-type)))
    (cond
     ((string-match-p "darwin" system-type)  ;; macOS
      "curl -fsSL https://ollama.com/install.sh | sh")
     ((string-match-p "gnu/linux" system-type)  ;; Linux
      "curl -fsSL https://ollama.com/install.sh | sh")
     ((string-match-p "windows" system-type)  ;; Windows
      "Windows安装选项:
1. 使用winget(推荐):
   winget install Ollama.Ollama

2. 使用Scoop:
   scoop bucket add main
   scoop install ollama

3. 直接下载安装包:
   访问 https://ollama.com/download")
     (t
      "请访问 https://ollama.com 获取安装指南"))))

(defun org-supertag-sim-epc-check-ollama ()
  "Check if the Ollama service is running.
Check方式类似于ollama_bridge.py中的is_service_running方法."
  (org-supertag-sim-epc-log "Checking the Ollama service...")
  (let ((result (condition-case nil
                    (with-timeout (3)  ;; Set 3 second timeout
                      (let ((output (shell-command-to-string "curl -s --connect-timeout 2 http://localhost:11434/api/tags")))
                        (with-temp-buffer
                          (insert output)
                          (goto-char (point-min))
                          ;; Verify if a valid JSON response is returned
                          (and (> (length output) 2)
                               (or (looking-at "\\[")    ;; Should start with a JSON array
                                   (looking-at "{"))))))  ;; Or start with a JSON object
                  (error nil))))
    (if result
        (progn
          (org-supertag-sim-epc-log "Ollama service is running")
          t)
      (org-supertag-sim-epc-log "Failed to connect to the Ollama service! Please ensure Ollama is started")
      (message "Warning: Failed to connect to the Ollama service! Please ensure Ollama is started")
      nil)))

(defun org-supertag-sim-epc-start-ollama ()
  "Start the Ollama service, select the appropriate startup method based on the current system type."
  (org-supertag-sim-epc-log "Attempting to start the Ollama service...")
  (let ((system-type (symbol-name system-type)))
    (cond
     ((or (string-match-p "darwin" system-type)
          (string-match-p "gnu/linux" system-type))
      (start-process "ollama-start" nil "ollama" "serve"))
     ((string-match-p "windows" system-type)
      (start-process "ollama-start" nil "ollama.exe" "serve"))
     (t (message "Unsupported system type: %s" system-type)
        nil))))

(defun org-supertag-sim-epc-ensure-ollama-running ()
  "Ensure the Ollama service is running, similar to the _ensure_service process in ollama_bridge.py.
1. Check if Ollama is installed
2. If not installed, provide installation instructions
3. Check if the service is running
4. If not running, start the service and wait for it to be ready"
  ;; 1. Check if Ollama is installed
  (unless (org-supertag-sim-epc-check-ollama-installed)
    (let ((install-instruction (org-supertag-sim-epc-get-ollama-install-instruction)))
      (org-supertag-sim-epc-log "Ollama is not installed, please install it first")
      (message "Ollama is not installed, please run the following command to install:\n%s" install-instruction)
      (error "Ollama is not installed, cannot continue")))
  
  ;; 2. Check if the service is running
  (unless (org-supertag-sim-epc-check-ollama)
    (message "Ollama service is not running, starting...")
    
    ;; 3. Try to start the service
    (let ((system-type (symbol-name system-type)))
      (cond
       ((or (string-match-p "darwin" system-type)
            (string-match-p "gnu/linux" system-type))
        (start-process "ollama-start" nil "ollama" "serve"))
       ((string-match-p "windows" system-type)
        (start-process "ollama-start" nil "ollama.exe" "serve"))
       (t (message "Unsupported system type: %s" system-type))))
    
    ;; 4. Wait for the service to start
    (let ((max-attempts 5)
          (attempt 0)
          (success nil))
      (while (and (< attempt max-attempts) (not success))
        (setq attempt (1+ attempt))
        (message "Waiting for Ollama service to start... Attempt %d/%d" attempt max-attempts)
        (sleep-for 2)  ;; Wait 2 seconds each time
        (setq success (org-supertag-sim-epc-check-ollama))
        (when success
          (message "Ollama service has been successfully started"))
        (when (and (not success) (= attempt max-attempts))
          (message "Warning: Ollama service failed to start, please start it manually")
          (error "Ollama service failed to start, please start it manually"))))))

(defun org-supertag-sim-epc-verify-ollama-model (model-name)
  "Verify if the Ollama model exists, similar to ensure_model_exists in ollama_bridge.py.
MODEL-NAME is the name of the model to verify." 
  (org-supertag-sim-epc-log "Verifying if model %s exists..." model-name)
  (let ((result (condition-case err
                    (with-timeout (3)
                      (let ((output (shell-command-to-string 
                                     "curl -s --connect-timeout 2 http://127.0.0.1:11434/api/tags")))
                        (org-supertag-sim-epc-log "API response: %s" (truncate-string-to-width output 100))
                        (with-temp-buffer
                          (insert output)
                          (goto-char (point-min))
                          ;; Check if the returned JSON contains the specified model
                          (and (looking-at "{")  ;; Should be a JSON object
                               (condition-case err
                                   (let* ((json-object-type 'hash-table)
                                          (json-array-type 'list)
                                          (json-key-type 'string)
                                          (json-data (json-read))
                                          (models (gethash "models" json-data))
                                          (found nil))
                                     ;; Iterate through the models array, check if it contains the specified model
                                     (when models
                                       (dolist (model models)
                                         (let ((name (gethash "name" model)))
                                           (org-supertag-sim-epc-log "Checking model: %s" name)
                                           (when (and name (string= name model-name))
                                             (setq found t)))))
                                     found)
                                 (error
                                  (org-supertag-sim-epc-log "JSON parsing error: %s" (error-message-string err))
                                  nil))))))
                  (error
                   (org-supertag-sim-epc-log "Error verifying model: %s" (error-message-string err))
                   nil))))
    (if result
        (progn
          (org-supertag-sim-epc-log "Model %s exists" model-name)
          t)
      (org-supertag-sim-epc-log "Model %s does not exist" model-name)
      nil)))

(defun org-supertag-sim-epc-pull-ollama-model (model-name)
  "Pull the Ollama model, similar to pull_model in ollama_bridge.py.
MODEL-NAME is the name of the model to pull."
  (org-supertag-sim-epc-log "Pulling model %s..." model-name)
  (message "Pulling Ollama model %s, this may take some time..." model-name)
  (let ((process (start-process "ollama-pull" nil "ollama" "pull" model-name)))
    (set-process-sentinel 
     process
     (lambda (proc event)
       (if (string-match-p "finished" event)
           (progn
             (org-supertag-sim-epc-log "Model %s pulled successfully" model-name)
             (message "Ollama model %s pulled successfully" model-name))
         (org-supertag-sim-epc-log "Model %s pull failed: %s" model-name event)
         (message "Warning: Ollama model %s pull failed: %s" model-name event))))
    ;; Return the process so it can be tracked
    process))

(defun org-supertag-sim-epc-ensure-ollama-model (model-name)
  "Ensure the Ollama model exists and is available, pull it if it doesn't exist.
MODEL-NAME is the name of the model to ensure, default is gemma-3-4b."
  (let ((model (or model-name "hf.co/unsloth/gemma-3-4b-it-GGUF:latest")))
    ;; First ensure the service is running
    (org-supertag-sim-epc-ensure-ollama-running)
    
    ;; Check if the model exists
    (unless (org-supertag-sim-epc-verify-ollama-model model)
      (message "Model %s does not exist, pulling..." model)
      (org-supertag-sim-epc-pull-ollama-model model)
      ;; Don't wait for the pull to complete, as it may take a long time
      ;; In actual application, some callback mechanism may be needed
      )))

;; The complete Ollama check and initialization process
(defun org-supertag-sim-epc-setup-ollama ()
  "Complete setup of the Ollama environment, including checking installation, starting the service, and preparing the default model."
  (interactive)
  (message "Setting up the Ollama environment...")
  (condition-case err
      (progn
        ;; 1. Ensure Ollama is installed
        (unless (org-supertag-sim-epc-check-ollama-installed)
          (let ((install-instruction (org-supertag-sim-epc-get-ollama-install-instruction)))
            (message "Ollama is not installed, please run the following command to install:\n%s" install-instruction)
            (error "Ollama is not installed")))
        
        ;; 2. Ensure the service is running
        (org-supertag-sim-epc-ensure-ollama-running)
        
        ;; 3. Prepare the default model (optional)
        ;; Here you can choose whether to check and prepare the default model
        ;; Since model download may take a long time, the default is to check without downloading
        (when (org-supertag-sim-epc-verify-ollama-model "hf.co/unsloth/gemma-3-4b-it-GGUF:latest")
          (org-supertag-sim-epc-log "Default model is ready"))
        
        (message "Ollama environment setup completed"))
    (error
     (message "Ollama environment setup failed: %s" (error-message-string err))
     nil)))

;; Function interface
(defun org-supertag-sim-epc-init ()
  "Initialize the EPC service and Ollama service for tag similarity engine.
This is an internal function called by org-supertag-sim-init.
For normal users, use org-supertag-sim-init as the main entry point."
  (org-supertag-sim-epc-log "Starting initialization...")
  (org-supertag-sim-epc-ensure-server)
  
  ;; Ensure the Ollama service is set up and running (no error capture)
  (org-supertag-sim-epc-log "Ensuring Ollama service is running...")
  (org-supertag-sim-epc-setup-ollama)
  
  (let* ((db-info (org-supertag-sim-epc--ensure-db-file))
         (db-file (car db-info))
         (tag-list (cadr db-info)))
    
    (org-supertag-sim-epc-log "Database file: %s" db-file)
    (org-supertag-sim-epc-log "Vector file: %s" org-supertag-sim-epc-vector-file)
    
    (condition-case err
        (progn
          (org-supertag-sim-epc-log "Calling the initialize method...")
          ;; Pass the file path string directly
          (let ((response (epc:call-sync org-supertag-sim-epc-manager 
                                        'initialize 
                                        (list org-supertag-sim-epc-vector-file 
                                              db-file))))
            (org-supertag-sim-epc-log "Initialization return result: %S" response)
            (if (string= (plist-get response :status) "success")
                (progn
                  (setq org-supertag-sim-epc-initialized t)
                  (org-supertag-sim-epc-log "Initialization successful")
                  (plist-get response :result))
              (error "Initialization failed: %S" response))))
      (error
       (org-supertag-sim-epc-log "Initialization process error: %s" (error-message-string err))
       (error "Initialization process error: %s" (error-message-string err)))))) ;; Modify here, throw an error directly


(defun org-supertag-sim-epc-restart-server ()
  "Restart the SimTag EPC server."
  (interactive)
  (org-supertag-sim-epc-stop-server)
  (sleep-for 1)  ; Wait for the process to fully terminate
  (org-supertag-sim-epc-start-server)
  (when (org-supertag-sim-epc-server-running-p)
    (message "SimTag EPC server has been restarted")))

(defun org-supertag-sim-epc-show-vector-file-info ()
  "Show the information of the vector file."
  (interactive)
  (message "Vector file path: %s" org-supertag-sim-epc-vector-file)
  (when (boundp 'org-supertag-db-file)
    (message "Database file path: %s" org-supertag-db-file))
  (if (file-exists-p org-supertag-sim-epc-vector-file)
      (let ((size (nth 7 (file-attributes org-supertag-sim-epc-vector-file)))
            (mod-time (format-time-string "%Y-%m-%d %H:%M:%S"
                                          (nth 5 (file-attributes org-supertag-sim-epc-vector-file)))))
        (message "Vector file exists (size: %d bytes, update time: %s)" size mod-time))
    (message "Vector file does not exist, will be created during initialization")))

(defun org-supertag-sim-epc-clean-python-cache ()
  "Clean the Python cache file."
  (interactive)
  (let ((script-dir (file-name-directory org-supertag-sim-epc-script-path)))
    (message "Cleaning Python cache file...")
    (shell-command (format "find %s -name '__pycache__' -type d -exec rm -rf {} +; find %s -name '*.pyc' -delete" 
                           script-dir script-dir))
    (message "Python cache file has been cleaned")))

(defun org-supertag-sim-epc-force-kill ()
  "Force terminate all SimTag EPC related processes."
  (interactive)
  (message "Force terminating SimTag EPC processes...")
  
  ;; Terminate known processes
  (when (org-supertag-sim-epc-server-running-p)
    (org-supertag-sim-epc-stop-server))
  
  ;; Clean Python processes
  (message "Terminating related Python processes...")
  (shell-command "pkill -f 'python.*simtag_epc\\.py' || true")
  
  ;; Reset status
  (setq org-supertag-sim-epc-manager nil)
  (setq org-supertag-sim-epc-initialized nil)
  
  (message "SimTag EPC processes have been terminated"))

(defun org-supertag-sim-epc-emergency-restart ()
  "Emergency restart the EPC server."
  (interactive)
  (message "Performing emergency restart...")
  (org-supertag-sim-epc-force-kill)
  (sit-for 1)
  (org-supertag-sim-epc-start-server)
  (sit-for 2)
  (if (org-supertag-sim-epc-server-running-p)
      (message "EPC server has been restarted")
    (message "EPC server restart failed")))

(defun org-supertag-sim-epc-echo-test ()
  "Test the EPC connection."
  (interactive)
  (org-supertag-sim-epc-ensure-server)
  (condition-case err
      (let ((result (epc:call-sync org-supertag-sim-epc-manager 'echo '("测试消息"))))
        (message "Echo test successful: %S" result)
        t)
    (error
     (message "Echo test failed: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim-epc-show-log ()
  "Show the SimTag EPC log buffer."
  (interactive)
  (let ((log-buffer (get-buffer-create "*simtag-epc-log*")))
    (with-current-buffer log-buffer
      (special-mode)  ; Make the buffer read-only
      (goto-char (point-max)))
    (display-buffer log-buffer)))

(defun org-supertag-sim-epc-setup-venv ()
  "Setup the Python virtual environment."
  (interactive)
  (let ((venv-dir org-supertag-sim-epc-venv-dir))
    (unless (file-exists-p venv-dir)
      (org-supertag-sim-epc-log "Creating virtual environment...")
      (make-directory venv-dir t)
      (shell-command-to-string 
       (format "python3 -m venv %s" venv-dir)))
    
    ;; Install dependencies
    (let ((pip (expand-file-name "bin/pip" venv-dir)))
      (org-supertag-sim-epc-log "Installing dependencies...")
      (shell-command-to-string 
       (format "%s install epc sentence-transformers torch numpy requests" pip)))
    
    ;; Update the Python interpreter path
    (let ((python-path (expand-file-name "bin/python" venv-dir)))
      (when (file-exists-p python-path)
        (setq org-supertag-sim-epc-python-path python-path)
        (org-supertag-sim-epc-log "Python path updated: %s" python-path)))))

(defun org-supertag-sim-epc-check-module-structure ()
  "Check and create the necessary module structure."
  (interactive)
  (let* ((base-dir org-supertag-sim-epc-dir)
         (simtag-dir (expand-file-name "simtag" base-dir))
         (init-file (expand-file-name "__init__.py" simtag-dir)))
    
    ;; Create simtag directory
    (unless (file-exists-p simtag-dir)
      (make-directory simtag-dir t))
    
    ;; Create __init__.py
    (unless (file-exists-p init-file)
      (with-temp-file init-file
        (insert "# SimTag package\n")))
    
    ;; Check necessary Python files
    (dolist (file '("config.py" "epc_server.py"))
      (let ((file-path (expand-file-name file simtag-dir)))
        (unless (file-exists-p file-path)
          (org-supertag-sim-epc-log "Missing necessary file: %s" file-path))))
    
    (org-supertag-sim-epc-log "Module structure check completed")))

(defun org-supertag-sim-epc-test-server ()
  "Test the server functionality comprehensively."
  (interactive)
  (org-supertag-sim-epc-log "Starting server test...")
  
  ;; 1. 测试基本连接
  (condition-case err
      (let ((result (epc:call-sync org-supertag-sim-epc-manager 'echo '("test"))))
        (org-supertag-sim-epc-log "Echo test successful: %S" result))
    (error
     (org-supertag-sim-epc-log "Echo test failed: %s" (error-message-string err))
     (error "Echo test failed")))
  
  ;; 2. 测试模块导入
  (condition-case err
      (let ((result (epc:call-sync org-supertag-sim-epc-manager 'check_imports '())))
        (org-supertag-sim-epc-log "Module import test successful: %S" result))
    (error
     (org-supertag-sim-epc-log "Module import test failed: %s" (error-message-string err))
     (error "Module import test failed")))
  
  ;; 3. 测试配置
  (condition-case err
      (let ((result (epc:call-sync org-supertag-sim-epc-manager 'get_config '())))
        (org-supertag-sim-epc-log "Configuration test successful: %S" result))
    (error
     (org-supertag-sim-epc-log "Configuration test failed: %s" (error-message-string err))
     (error "Configuration test failed")))
  
  (org-supertag-sim-epc-log "Server test completed"))

(defun org-supertag-sim-epc-extract-entities-async (text callback)
  "Asynchronously extract entities from TEXT and pass the result to the CALLBACK function.
TEXT is the text to analyze
CALLBACK is the callback function that receives the entity list"
  (org-supertag-sim-epc-log "Asynchronous entity extraction, text length: %d" (length text))
  (deferred:$
    (deferred:try
      (deferred:$
        (org-supertag-sim-epc-ensure-server)
        (deferred:nextc it
          (lambda (_)
            (epc:call-deferred org-supertag-sim-epc-manager
                              'extract_entities
                              (list text))))
        (deferred:nextc it
          (lambda (response)
            (let ((status (plist-get response :status))
                  (result (plist-get response :result)))
              (if (string= status "success")
                  (progn
                    (org-supertag-sim-epc-log "Entity extraction successful, found %d entities" 
                                              (length result))
                    (funcall callback result))
                (error "Entity extraction failed: %s" 
                       (or (plist-get response :message) "Unknown error")))))))
      :catch
      (lambda (err)
        (org-supertag-sim-epc-log "Entity extraction error: %s" (error-message-string err))
        (message "Entity extraction error: %s" (error-message-string err))
        (funcall callback nil)))))

(defun org-supertag-sim-epc-get-tag-suggestions-async (text limit callback)
  "Asynchronously get tag suggestions for TEXT.
TEXT is the text to analyze
LIMIT is the maximum number of suggestions to return
CALLBACK is the callback function that receives the suggestions"
  (org-supertag-sim-epc-log "Asynchronous tag suggestion, text length: %d, limit: %d" 
                           (length text) limit)
  (deferred:$
    (deferred:try
      (deferred:$
        (org-supertag-sim-epc-ensure-server)
        (deferred:nextc it
          (lambda (_)
            (epc:call-deferred org-supertag-sim-epc-manager
                              'suggest_tags
                              (list text limit))))
        (deferred:nextc it
          (lambda (response)
            (let ((status (plist-get response :status))
                  (result (plist-get response :result)))
              (if (string= status "success")
                  (progn
                    (org-supertag-sim-epc-log "Tag suggestion successful, found %d suggestions" 
                                              (length result))
                    (funcall callback result))
                (error "Tag suggestion failed: %s" 
                       (or (plist-get response :message) "Unknown error")))))))
      :catch
      (lambda (err)
        (org-supertag-sim-epc-log "Tag suggestion error: %s" (error-message-string err))
        (message "Tag suggestion error: %s" (error-message-string err))
        (funcall callback nil)))))

(defun org-supertag-sim-epc-find-similar-async (tag-name limit callback)
  "Asynchronously find similar tags to TAG-NAME.
LIMIT is the maximum number of similar tags to return
CALLBACK is the callback function that receives the similar tags"
  (org-supertag-sim-epc-log "Asynchronous similar tag search: %s, limit: %d" tag-name limit)
  (deferred:$
    (deferred:try
      (deferred:$
        (org-supertag-sim-epc-ensure-server)
        (deferred:nextc it
          (lambda (_)
            (epc:call-deferred org-supertag-sim-epc-manager
                              'find_similar
                              (list tag-name limit))))
        (deferred:nextc it
          (lambda (response)
            (let ((status (plist-get response :status))
                  (result (plist-get response :result)))
              (if (string= status "success")
                  (progn
                    (org-supertag-sim-epc-log "Similar tag search successful, found %d similar tags" 
                                              (length result))
                    (funcall callback result))
                (error "Similar tag search failed: %s" 
                       (or (plist-get response :message) "Unknown error")))))))
      :catch
      (lambda (err)
        (org-supertag-sim-epc-log "Similar tag search error: %s" (error-message-string err))
        (message "Similar tag search error: %s" (error-message-string err))
        (funcall callback nil)))))

(defun org-supertag-sim-epc-interactive-ollama ()
  "Create a buffer where users can interact with Ollama."
  (interactive)
  (require 'org-supertag-sim)
  (unless (and org-supertag-sim--initialized org-supertag-sim-epc-initialized)
    (org-supertag-sim-init))
  (let ((buffer (get-buffer-create "*org-supertag-ollama-chat*")))
    (switch-to-buffer buffer)
    (when (= (buffer-size) 0)
      (insert "===== Ollama 交互式对话 =====\n\n")
      (insert "在此输入消息，按 C-c C-c 发送\n\n")
      (insert "系统提示 (可选):\n")
      (insert "--------------------------\n")
      (insert "你是一个专注于Emacs和org-mode的AI助手，专为org-supertag项目提供帮助。\n")
      (insert "--------------------------\n\n")
      (insert "用户消息:\n")
      (insert "请输入您的问题...\n")
      (local-set-key (kbd "C-c C-c") 'org-supertag-sim-epc--send-message)
      (message "输入您的消息，然后按 C-c C-c 发送给Ollama"))))

(defun org-supertag-sim-epc--send-message ()
  "Send a message from the interactive Ollama buffer."
  (interactive)
  (with-current-buffer "*org-supertag-ollama-chat*"
    ;; Extract user message and system prompt
    (let ((system-prompt nil)
          (user-message nil))
      
      ;; Get system prompt
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "--------------------------\n" nil t)
          (let ((start (point)))
            (when (search-forward "--------------------------\n" nil t)
              (setq system-prompt (buffer-substring-no-properties 
                                  start
                                  (- (point) 
                                     (length "--------------------------\n"))))))))
      
      ;; Get user message
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "用户消息:\n" nil t)
          (setq user-message (buffer-substring-no-properties 
                             (point)
                             (point-max)))))
      
      ;; Clear user message area and prepare to receive reply
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (when (search-forward "用户消息:\n" nil t)
            (delete-region (point) (point-max))
            (insert user-message)
            (insert "\n\n等待Ollama响应...\n"))))
      
      ;; Send message to Ollama
      (when user-message
        (deferred:$
          (deferred:next
            (lambda ()
              (let ((args (if system-prompt
                              (list user-message system-prompt)
                            (list user-message))))
                (epc:call-deferred org-supertag-sim-epc-manager
                                  'run_ollama
                                  args))))
          
          (deferred:nextc it
            (lambda (response)
              (with-current-buffer "*org-supertag-ollama-chat*"
                (save-excursion
                  (goto-char (point-max))
                  (let ((status (plist-get response :status))
                        (result (plist-get response :result))
                        (message (plist-get response :message)))
                    (delete-region (- (point-max) 
                                     (length "\n\nWaiting for Ollama response...\n")) 
                                  (point-max))
                    (if (string= status "success")
                        (progn
                          (insert "\n\nOllama reply:\n")
                          (insert result)
                          (insert "\n\n------\n\nUser message:\n"))
                      (insert "\n\nError: " (or message "Unknown error") "\n\nUser message:\n"))))
                (goto-char (point-max)))))
          
          (deferred:error it
            (lambda (err)
              (with-current-buffer "*org-supertag-ollama-chat*"
                (save-excursion
                  (goto-char (point-max))
                  (delete-region (- (point-max) 
                                   (length "\n\nWaiting for Ollama response...\n")) 
                                (point-max))
                  (insert "\n\nError: " (error-message-string err) "\n\nUser message:\n"))
                (goto-char (point-max))))))))))

(provide 'org-supertag-sim-epc)
;;; org-supertag-sim-epc.el ends here

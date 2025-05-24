;; Test behavior to verify the execute-script function signature fix  -*- lexical-binding: t; -*-

(defun test-script-executor ()
  "Test the script executor configuration and Python detection."
  (interactive)
  (let ((test-file "~/Documents/prj/lex-scraper/lex-scraper.py"))
    (if (file-exists-p (expand-file-name test-file))
        (progn
          (message "Script found at: %s" test-file)
          ;; Test Python detection
          (let ((python-exec (org-supertag-behavior--find-python-executable test-file)))
            (message "Detected Python executable: %s" python-exec))
          ;; Test executor config
          (let ((executor (org-supertag-behavior--get-script-executor test-file)))
            (message "Python executor config: %S" executor)))
      (message "Script not found at: %s" test-file))))

(defun test-python-detection ()
  "Test Python detection for different scenarios."
  (interactive)
  (let ((test-cases '(("Script in project root" . "~/Documents/prj/lex-scraper/lex-scraper.py")
                     ("Script in subdirectory" . "~/Documents/prj/lex-scraper/sub/test.py") 
                     ("Generic script" . "/tmp/test.py"))))
    
    (message "=== Python Detection Tests ===")
    
    ;; Test current VIRTUAL_ENV
    (if-let ((venv (getenv "VIRTUAL_ENV")))
        (message "Current VIRTUAL_ENV: %s" venv)
      (message "No VIRTUAL_ENV set"))
    
    ;; Test each case
    (dolist (test-case test-cases)
      (let* ((description (car test-case))
             (script-path (cdr test-case))
             (detected-python (org-supertag-behavior--find-python-executable script-path)))
        (message "%s: %s -> %s" description script-path detected-python)))
    
    ;; Test custom configuration
    (message "\n=== Testing Custom Configuration ===")
    (let ((org-supertag-python-executable "/usr/bin/python3"))
      (message "With custom setting '/usr/bin/python3': %s"
               (org-supertag-behavior--find-python-executable "test.py")))
    
    (message "=== End Tests ===")))

(defun test-simple-script-execution ()
  "Test basic script execution without database dependencies."
  (interactive)
  (let* ((output-buffer (generate-new-buffer "*test-script-output*"))
         (command "python3")
         (args '("--version")))
    
    (message "Testing script execution with command: %s %s" command (mapconcat 'identity args " "))
    
    (condition-case err
        (progn
          (apply #'call-process
                 command    ; Program
                 nil       ; Input
                 output-buffer ; Output
                 nil       ; Display
                 args)     ; Arguments
          
          (with-current-buffer output-buffer
            (let ((output (buffer-string)))
              (message "Script output: %s" output)))
          
          (kill-buffer output-buffer)
          (message "Test completed successfully"))
      (error
       (progn
         (kill-buffer output-buffer)
         (message "Error: %s" (error-message-string err)))))))

(defun test-execute-script-with-mock-node ()
  "Test execute-script with a mock node that exists in database."
  (interactive)
  (save-excursion
    ;; Create a temporary org buffer with a real node
    (with-temp-buffer
      (org-mode)
      (insert "* Test Script Execution\n")
      (insert ":PROPERTIES:\n")
      (insert ":SCRIPT_PATH: ~/Documents/prj/lex-scraper/lex-scraper.py\n")
      (insert ":END:\n\n")
      (goto-char (point-min))
      (org-next-visible-heading 1)
      
      ;; Create ID for this node
      (let* ((node-id (org-id-get-create))
             (params '(:script-path "python3"
                      :args ("--version")
                      :log-drawer "TEST_LOG")))
        
        (message "Testing with real node-id: %s and params: %S" node-id params)
        (condition-case err
            (progn
              ;; Add node to database temporarily
              (org-supertag-db-add-node node-id (point-marker))
              (org-supertag-behavior--execute-script node-id params)
              (message "Test completed successfully!"))
          (error
           (message "Error: %s" (error-message-string err))))))))

(defun test-execute-script-simple ()
  "Test the execute-script function with fake node-id (output only)."
  (interactive)
  (let ((node-id "test-node")
        (params '(:script-path "python3"
                  :args ("--version")              ; Simple version check
                  :log-drawer "TEST_LOG")))
    (message "Testing with fake node-id: %s and params: %S" node-id params)
    (condition-case err
        (org-supertag-behavior--execute-script node-id params)
      (error
       (message "Error: %s" (error-message-string err))))))

(defun test-execute-script ()
  "Test the execute-script function with your actual script."
  (interactive)
  (let ((node-id "test-node")
        (params '(:script-path "~/Documents/prj/lex-scraper/lex-scraper.py"  ; Use your actual script path
                  :log-drawer "TEST_LOG")))
    (message "Testing with node-id: %s and params: %S" node-id params)
    (condition-case err
        (org-supertag-behavior--execute-script node-id params)
      (error
       (message "Error: %s" (error-message-string err))))))

;; Test call without arguments (this should fail before our fix)
(defun test-execute-script-no-args ()
  "Test calling execute-script without arguments (should fail)."
  (interactive)
  (condition-case err
      (org-supertag-behavior--execute-script)
    (error
     (message "Expected error: %s" (error-message-string err)))))

;; Test the simplified execute-script function
(defun test-execute-script-simplified ()
  "Test the simplified execute-script function (no logging)."
  (interactive)
  (message "=== Testing Simplified Execute Script ===")
  
  ;; Test 1: Simple Python version check - create a temp script
  (message "Test 1: Python version check")
  (let ((temp-script "/tmp/test_version.py"))
    (with-temp-file temp-script
      (insert "import sys; print(f'Python {sys.version}')\n"))
    (condition-case err
        (org-supertag-behavior--execute-script "any-node-id" 
          `(:script-path ,temp-script))
      (error
       (message "Test 1 Error: %s" (error-message-string err))))
    (delete-file temp-script))
  
  ;; Test 2: Your actual script
  (message "\nTest 2: Your lex-scraper script")
  (condition-case err
      (org-supertag-behavior--execute-script "any-node-id"
        '(:script-path "~/Documents/prj/lex-scraper/lex-scraper.py"))
    (error
     (message "Test 2 Error: %s" (error-message-string err))))
  
  ;; Test 3: Non-existent script (should fail gracefully)
  (message "\nTest 3: Non-existent script")
  (condition-case err
      (org-supertag-behavior--execute-script "any-node-id"
        '(:script-path "~/Documents/prj/lex-scraper/lex-scraper.py"))
    (error
     (message "Test 3 Expected Error: %s" (error-message-string err))))
  
  (message "\n=== Testing Complete ==="))

;; Test with a simple script that doesn't require external dependencies
(defun test-execute-script-no-deps ()
  "Test script execution with a script that has no external dependencies."
  (interactive)
  (let ((simple-script "/tmp/simple_test.py"))
    ;; Create a simple script
    (with-temp-file simple-script
      (insert "#!/usr/bin/env python3\n")
      (insert "import os\n")
      (insert "import sys\n")
      (insert "print(f'Hello from Python {sys.version_info.major}.{sys.version_info.minor}!')\n")
      (insert "print(f'Current directory: {os.getcwd()}')\n")
      (insert "print(f'Script path: {sys.argv[0]}')\n")
      (insert "print('All environment variables with PYTHON:')\n")
      (insert "for key, value in os.environ.items():\n")
      (insert "    if 'PYTHON' in key:\n")
      (insert "        print(f'  {key} = {value}')\n"))
    
    (message "=== Testing Simple Script (No Dependencies) ===")
    (condition-case err
        (org-supertag-behavior--execute-script "test-node"
          `(:script-path ,simple-script))
      (error
       (message "Error: %s" (error-message-string err))))
    
    (delete-file simple-script)
    (message "=== Simple Test Complete ===")))

;; Test async script execution 
(defun test-execute-script-async ()
  "Test asynchronous script execution."
  (interactive)
  (message "=== Testing Async Script Execution ===")
  
  ;; Test 1: Quick async script
  (message "Test 1: Quick async Python script")
  (let ((temp-script "/tmp/quick_test.py"))
    (with-temp-file temp-script
      (insert "import time\n")
      (insert "print('Starting...')\n")
      (insert "time.sleep(2)\n")  ; 2 second delay
      (insert "print('Script completed after 2 seconds')\n"))
    
    (org-supertag-behavior--execute-script "test-node"
      `(:script-path ,temp-script
        :async t
        :callback (lambda (output)
                   (message "Callback received: %s" (string-trim output))
                   (delete-file ,temp-script))))
    
    (message "Script started in background, Emacs is NOT blocked!"))
  
  ;; Test 2: Your lex-scraper (async)
  (message "\nTest 2: Lex-scraper script (async)")
  (org-supertag-behavior--execute-script "test-node"
    '(:script-path "~/Documents/prj/lex-scraper/lex-scraper.py"
      :async t
      :callback (lambda (output)
                 (message "Lex-scraper completed! Output length: %d characters" 
                         (length output)))))
  
  ;; Test 3: Sync execution for comparison
  (message "\nTest 3: Quick sync script for comparison")
  (let ((temp-script "/tmp/sync_test.py"))
    (with-temp-file temp-script
      (insert "print('This is a synchronous script')\n"))
    
    (org-supertag-behavior--execute-script "test-node"
      `(:script-path ,temp-script
        :async nil  ; Force sync
        :callback (lambda (output)
                   (message "Sync callback: %s" (string-trim output))
                   (delete-file ,temp-script))))
    
    (message "Sync script completed"))
  
  (message "\n=== You can continue using Emacs while scripts run! ===")) 
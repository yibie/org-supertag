;;; org-supertag-sim-epc-test.el --- Test for SimTag EPC connection

;; Copyright (C) 2023 

;; Author: 
;; Keywords: org-mode, nlp, epc, test

;;; Commentary:
;; 
;; This file provides the functionality to test the Org-SuperTag SimTag EPC connection.
;; It can help diagnose connection issues with the EPC server.
;;

;;; Code:

(require 'cl-lib)
(require 'epc)
(require 'json)

;; Global variable to save test results
(defvar org-supertag-sim-epc-test-results nil
  "Save test results.")

(defun org-supertag-sim-epc-test-display-log ()
  "Display test log."
  (interactive)
  (with-current-buffer (get-buffer-create "*simtag-test-log*")
    (display-buffer (current-buffer))))

(defun org-supertag-sim-epc-test-log (format-string &rest args)
  "Record test log information.
FORMAT-STRING is the format string
ARGS are the format arguments"
  (let ((msg (apply #'format format-string args)))
    (with-current-buffer (get-buffer-create "*simtag-test-log*")
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
      (insert msg)
      (insert "\n"))
    (message "SimTag Test: %s" msg)))
(defun org-supertag-sim-epc-test-connect-port (port)
  "Test connection to EPC server at specified port.
PORT is the port number to connect to"
  (interactive "nPort: ")
  (org-supertag-sim-epc-test-log "Testing connection to port %d" port)
  (condition-case err
      (let ((epc-process nil))
        (org-supertag-sim-epc-test-log "Attempting to create EPC connection...")
        (setq epc-process (epc:start-epc "127.0.0.1" port))
        (if (epc:live-p epc-process)
            (progn 
              (org-supertag-sim-epc-test-log "EPC connection successful")
              (let ((result (org-supertag-sim-epc-test-echo epc-process "Test message")))
                (org-supertag-sim-epc-test-log "Echo test result: %S" result))
              (epc:stop-epc epc-process)
              t)
          (org-supertag-sim-epc-test-log "EPC connection failed")
          nil))
    (error
     (org-supertag-sim-epc-test-log "Connection error: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim-epc-test-echo (epc-process msg)
  "Test EPC server echo functionality.
EPC-PROCESS is the EPC connection
MSG is the message to send"
  (org-supertag-sim-epc-test-log "Testing Echo functionality, sending message: %s" msg)
  (condition-case err
      (let ((result (epc:call-sync epc-process 'echo (list msg) 3)))
        (org-supertag-sim-epc-test-log "Echo response: %S" result)
        result)
    (error
     (org-supertag-sim-epc-test-log "Echo test error: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim-epc-test-status (epc-process)
  "Get EPC server status.
EPC-PROCESS is the EPC connection"
  (org-supertag-sim-epc-test-log "Getting server status...")
  (condition-case err
      (let ((result (epc:call-sync epc-process 'get_server_status nil 3)))
        (org-supertag-sim-epc-test-log "Server status: %S" result)
        result)
    (error
     (org-supertag-sim-epc-test-log "Get status error: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim-epc-test-extract-entities (epc-process text)
  "Test entity extraction functionality.
EPC-PROCESS is the EPC connection
TEXT is the text to extract entities from"
  (org-supertag-sim-epc-test-log "Testing entity extraction, text: %s" text)
  (condition-case err
      (let ((result (epc:call-sync epc-process 'extract_entities (list text) 5)))
        (org-supertag-sim-epc-test-log "Extraction result: %S" result)
        result)
    (error
     (org-supertag-sim-epc-test-log "Entity extraction error: %s" (error-message-string err))
     nil)))

;;;###autoload
(defun org-supertag-sim-epc-test-check-python-script ()
  "Check if Python script exists."
  (interactive)
  (let ((script-path (expand-file-name "simtag_epc.py"
                                      (file-name-directory (locate-library "org-supertag-sim-epc")))))
    (if (file-exists-p script-path)
        (progn
          (org-supertag-sim-epc-test-log "Python script exists: %s" script-path)
          (let ((size (nth 7 (file-attributes script-path)))
                (mod-time (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (nth 5 (file-attributes script-path)))))
            (org-supertag-sim-epc-test-log "Script size: %d bytes, modification time: %s" size mod-time)))
      (org-supertag-sim-epc-test-log "Python script does not exist: %s" script-path))))

;;;###autoload
(defun org-supertag-sim-epc-test-check-env-script ()
  "Check if environment script exists."
  (interactive)
  (let ((script-path (expand-file-name "run_simtag_epc_venv.sh"
                                       (file-name-directory (locate-library "org-supertag-sim-epc")))))
    (if (file-exists-p script-path)
        (progn
          (org-supertag-sim-epc-test-log "Environment script exists: %s" script-path)
          (let ((size (nth 7 (file-attributes script-path)))
                (mod-time (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (nth 5 (file-attributes script-path))))
                (executable (file-executable-p script-path)))
            (org-supertag-sim-epc-test-log "Script size: %d bytes, modification time: %s, executable: %s" 
                                          size mod-time executable)
            (unless executable
              (org-supertag-sim-epc-test-log "Warning: Script is not executable"))))
      (org-supertag-sim-epc-test-log "Environment script does not exist: %s" script-path))))

;;;###autoload
(defun org-supertag-sim-epc-test-python-version ()
  "Check Python version."
  (interactive)
  (org-supertag-sim-epc-test-log "Checking Python version...")
  (let ((output (shell-command-to-string "python3 --version 2>&1")))
    (org-supertag-sim-epc-test-log "Python version: %s" output)
    ;; Check if Python is available
    (if (string-match "Python 3\\.[0-9]+" output)
        (org-supertag-sim-epc-test-log "Python 3 is available")
      (org-supertag-sim-epc-test-log "Warning: Python 3 not detected"))))

;;;###autoload
(defun org-supertag-sim-epc-test-port (port)
  "Check if port is in use.
PORT is the port number to check"
  (interactive "nPort: ")
  (org-supertag-sim-epc-test-log "Checking if port %d is in use..." port)
  (let ((output (shell-command-to-string (format "lsof -i :%d" port))))
    (if (string-empty-p output)
        (org-supertag-sim-epc-test-log "Port %d is not in use" port)
      (org-supertag-sim-epc-test-log "Port %d is in use:\n%s" port output))))

;;;###autoload
(defun org-supertag-sim-epc-run-tests ()
  "Run all EPC server tests."
  (interactive)
  ;; Create or clear log buffer
  (with-current-buffer (get-buffer-create "*simtag-test-log*")
    (erase-buffer)
    (display-buffer (current-buffer)))
  
  (setq org-supertag-sim-epc-test-results nil)
  
  (org-supertag-sim-epc-test-log "Starting SimTag EPC tests...")
  
  ;; Check script files
  (org-supertag-sim-epc-test-check-python-script)
  (org-supertag-sim-epc-test-check-env-script)
  
  ;; Check Python version
  (org-supertag-sim-epc-test-python-version)
  
  ;; Check port
  (org-supertag-sim-epc-test-port 21278)
  
  ;; Test EPC connection
  (let ((port 21278))
    (org-supertag-sim-epc-test-log "===============================================")
    (org-supertag-sim-epc-test-log "Testing EPC connection to port %d" port)
    (org-supertag-sim-epc-test-log "===============================================")
    
    (condition-case err
        (let ((epc-process nil))
          (org-supertag-sim-epc-test-log "Attempting to create EPC connection...")
          (setq epc-process (epc:start-epc "127.0.0.1" port))
          (if (epc:live-p epc-process)
              (progn 
                (org-supertag-sim-epc-test-log "EPC connection successful")
                
                ;; Test echo functionality
                (let ((echo-result (org-supertag-sim-epc-test-echo epc-process "Test message")))
                  (push `(:test "echo" :result ,echo-result) org-supertag-sim-epc-test-results))
                
                ;; Test get status
                (let ((status-result (org-supertag-sim-epc-test-status epc-process)))
                  (push `(:test "status" :result ,status-result) org-supertag-sim-epc-test-results))
                
                ;; Test entity extraction
                (let ((extract-result (org-supertag-sim-epc-test-extract-entities epc-process "This is a test text for testing entity extraction functionality.")))
                  (push `(:test "extract" :result ,extract-result) org-supertag-sim-epc-test-results))
                
                ;; Stop EPC process
                (epc:stop-epc epc-process)
                (org-supertag-sim-epc-test-log "EPC connection closed"))
            (org-supertag-sim-epc-test-log "EPC connection failed")))
      (error
       (org-supertag-sim-epc-test-log "Connection error: %s" (error-message-string err)))))
  
  ;; Display test results summary
  (org-supertag-sim-epc-test-log "===============================================")
  (org-supertag-sim-epc-test-log "Test results summary")
  (org-supertag-sim-epc-test-log "===============================================")
  
  (let ((all-passed t))
    (dolist (result org-supertag-sim-epc-test-results)
      (let ((test-name (plist-get result :test))
            (test-result (plist-get result :result)))
        (if test-result
            (org-supertag-sim-epc-test-log "Test '%s': Passed" test-name)
          (org-supertag-sim-epc-test-log "Test '%s': Failed" test-name)
          (setq all-passed nil))))
    
    (org-supertag-sim-epc-test-log "===============================================")
    (if all-passed
        (org-supertag-sim-epc-test-log "All tests passed!")
      (org-supertag-sim-epc-test-log "Some tests failed, please check the log for details.")))
  
  (org-supertag-sim-epc-test-log "Tests completed")
)
(provide 'org-supertag-sim-epc-test)
;;; org-supertag-sim-epc-test.el ends here

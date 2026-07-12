;;; diagnose.el --- Diagnose virtual column loading issue -*- lexical-binding: t; -*-

(message "=== Diagnostic Info ===")
(message "")

;; Check 1: Which file is loaded?
(message "1. Checking loaded file...")
(let ((lib (locate-library "supertag-virtual-column")))
  (if lib
      (progn
        (message "   File: %s" lib)
        (message "   Exists: %s" (if (file-exists-p lib) "YES" "NO"))
        (when (file-exists-p lib)
          (message "   Size: %d bytes" (nth 7 (file-attributes lib)))
          (message "   Modified: %s" (format-time-string "%Y-%m-%d %H:%M:%S" 
                                                        (nth 5 (file-attributes lib))))))
    (message "   File: NOT FOUND in load-path")))

(message "")

;; Check 2: Module loaded?
(message "2. Module status: %s" 
         (if (featurep 'supertag-virtual-column) 
             "LOADED" 
           "NOT LOADED"))

(message "")

;; Check 3: Variable state
(message "3. Variable state:")
(message "   Definitions table: %s" 
         (if (boundp 'supertag--virtual-column-definitions)
             (format "bound (%d entries)" 
                     (hash-table-count supertag--virtual-column-definitions))
           "NOT BOUND"))

(message "")

;; Check 4: Try to find the problematic string in loaded file
(message "4. Checking for problematic patterns...")
(when (featurep 'supertag-virtual-column)
  (let* ((lib (locate-library "supertag-virtual-column"))
         (content (when lib (with-temp-buffer
                             (insert-file-contents lib)
                             (buffer-string)))))
    (if content
        (let ((has-old-pattern (string-match-p "'(:id \"total-effort\"" content)))
          (message "   Old pattern found: %s" (if has-old-pattern "YES (PROBLEM!)" "NO (OK)"))
          (when has-old-pattern
            (message "   File contains old quoted plist pattern!")
            (message "   Solution: Delete this file and let me recreate it:")
            (message "   %s" lib)))
      (message "   Cannot read file content"))))

(message "")
(message "=== End Diagnostic ===")

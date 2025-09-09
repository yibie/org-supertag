;;; test-capture-fix.el --- Test script for capture fix

;; This script demonstrates the fix for the nil file parameter bug

(defun test-nil-file-handling ()
  "Test that nil file parameters are properly handled"
  (condition-case err
      (progn
        ;; Test supertag-ui-select-insert-position with nil
        (supertag-ui-select-insert-position nil)
        (message "ERROR: Should have failed with nil file"))
    (error 
     (if (string-match "FILE parameter cannot be nil" (error-message-string err))
         (message "✓ PASS: supertag-ui-select-insert-position correctly rejects nil file")
       (message "✗ FAIL: Wrong error message: %s" (error-message-string err)))))
  
  (condition-case err
      (progn
        ;; Test supertag-capture--execute with nil target-file
        (supertag-capture--execute nil '(:title "test" :tags nil :body "" :fields nil))
        (message "ERROR: Should have failed with nil target-file"))
    (error 
     (if (string-match "TARGET-FILE cannot be nil" (error-message-string err))
         (message "✓ PASS: supertag-capture--execute correctly rejects nil target-file")
       (message "✗ FAIL: Wrong error message: %s" (error-message-string err)))))
  
  (condition-case err
      (progn
        ;; Test capture template with missing file property
        (let ((supertag-capture-templates '(("t" "Test template" (:file nil)))))
          (supertag-capture-with-template "t"))
        (message "ERROR: Should have failed with nil file in template"))
    (error 
     (if (string-match "has no target file specified" (error-message-string err))
         (message "✓ PASS: Template correctly rejects nil file property")
       (message "✗ FAIL: Wrong error message: %s" (error-message-string err))))))

(test-nil-file-handling)

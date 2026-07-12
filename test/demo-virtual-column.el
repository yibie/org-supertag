;;; demo-virtual-column.el --- Quick demo of virtual column system -*- lexical-binding: t; -*-

;;; Commentary:
;; Quick demonstration of virtual column functionality.
;; 
;; Usage:
;;   1. First load the main module:
;;      (add-to-list 'load-path "/Users/chenyibin/Documents/emacs/package/org-supertag")
;;      (require 'supertag-virtual-column)
;;
;;   2. Then load this file and run:
;;      (load-file "/Users/chenyibin/Documents/emacs/package/org-supertag/test/demo-virtual-column.el")
;;      M-x supertag-demo-virtual-column

;;; Code:

(defun supertag-demo-virtual-column ()
  "Run a quick demonstration of virtual columns."
  (interactive)
  
  ;; Check if module is loaded
  (unless (featurep 'supertag-virtual-column)
    (error "Please load supertag-virtual-column first: (require 'supertag-virtual-column)"))
  
  ;; Clear any previous demo data
  (clrhash supertag--virtual-column-definitions)
  (clrhash supertag--virtual-column-cache)
  
  (with-output-to-temp-buffer "*Virtual Column Demo*"
    (princ "Virtual Column System Demo\n")
    (princ "==========================\n\n")
    
    ;; Demo 1: Create virtual columns
    (princ "1. Creating Virtual Columns\n")
    (princ "   Creating rollup column...\n")
    
    (condition-case err
        (let ((column-def (supertag-virtual-column-create
                           (list :id "test-roll"
                                 :name "Test Rollup"
                                 :type :rollup
                                 :params (list :relation "children"
                                               :field "effort"
                                               :function :sum)))))
          (princ (format "   Created: %s (%s)\n" 
                        (plist-get column-def :name)
                        (plist-get column-def :id))))
      (error (princ (format "   Error: %s\n" (error-message-string err)))))
    
    (princ "\n")
    
    ;; Demo 2: List columns
    (princ "2. Listing Virtual Columns\n")
    (let ((cols (supertag-virtual-column-list)))
      (princ (format "   Found %d column(s):\n" (length cols)))
      (dolist (col cols)
        (princ (format "   - %s (%s): %s\n"
                      (plist-get col :name)
                      (plist-get col :id)
                      (plist-get col :type)))))
    
    (princ "\n")
    
    ;; Demo 3: Cache operations
    (princ "3. Cache Operations\n")
    (princ "   Storing value in cache...\n")
    (supertag-virtual-column--cache-put 
     "demo-node" "demo-vc" 42 
     (list (cons "task-1" "effort")))
    (princ "   Value cached\n")
    
    (let ((entry (supertag-virtual-column--cache-get "demo-node" "demo-vc")))
      (if entry
          (princ (format "   Retrieved: %s\n"
                        (supertag-virtual-column-cache-value entry)))
        (princ "   Cache miss\n")))
    
    (princ "\n")
    
    ;; Demo 4: Update
    (princ "4. Update Virtual Column\n")
    (condition-case err
        (progn
          (supertag-virtual-column-update
           "test-roll"
           (lambda (old)
             (plist-put old :name "Updated Name")))
          (let ((updated (supertag-virtual-column-get-definition "test-roll")))
            (princ (format "   Updated name: %s\n" (plist-get updated :name)))))
      (error (princ (format "   Error: %s\n" (error-message-string err)))))
    
    (princ "\n")
    
    ;; Demo 5: Cleanup
    (princ "5. Cleanup\n")
    (supertag-virtual-column-delete "test-roll")
    (if (supertag-virtual-column-get-definition "test-roll")
        (princ "   Column still exists\n")
      (princ "   Column deleted\n"))
    
    (princ "\n")
    (princ "Demo complete!\n")))

(provide 'demo-virtual-column)

;;; demo-virtual-column.el ends here

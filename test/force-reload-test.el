;;; force-reload-test.el --- Force reload and test virtual column -*- lexical-binding: t; -*-

;; This script ensures a clean reload of the virtual column module

;; Step 1: Unload if already loaded
(when (featurep 'supertag-virtual-column)
  (unload-feature 'supertag-virtual-column t)
  (message "Unloaded old virtual-column module"))

;; Step 2: Clear all related variables
(setq supertag--virtual-column-definitions nil)
(setq supertag--virtual-column-cache nil)
(setq supertag--virtual-column-dependency-graph nil)
(setq supertag--virtual-column-compute-stack nil)

;; Step 3: Set load path
(let ((project-dir "/Users/chenyibin/Documents/emacs/package/org-supertag"))
  (unless (member project-dir load-path)
    (add-to-list 'load-path project-dir))
  
  ;; Step 4: Load fresh copy
  (load-file (expand-file-name "supertag-virtual-column.el" project-dir))
  
  ;; Step 5: Verify it's loaded
  (if (featurep 'supertag-virtual-column)
      (message "✓ Virtual column module loaded fresh")
    (error "Failed to load module"))
  
  ;; Step 6: Run minimal test
  (message "Running minimal test...")
  (condition-case err
      (progn
        ;; Create a test column
        (let ((def (supertag-virtual-column-create
                    (list :id "force-test"
                          :name "Force Test"
                          :type :rollup
                          :params (list :relation "children"
                                        :field "effort"
                                        :function :sum)))))
          (message "✓ Created: %s" (plist-get def :id))
          
          ;; Verify it exists
          (if (supertag-virtual-column-get-definition "force-test")
              (message "✓ Definition found")
            (message "✗ Definition not found"))
          
          ;; Clean up
          (supertag-virtual-column-delete "force-test")
          (message "✓ Deleted")))
    
    (error 
     (message "✗ ERROR: %s" (error-message-string err))
     (message "")
     (message "Debug info:")
     (message "  Error type: %s" (car err))
     (message "  Error data: %s" (cdr err))))
  
  (message "")
  (message "Test complete."))

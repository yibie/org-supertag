;;; test-automation-complete.el --- Complete automation test -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete end-to-end test for the automation system

;;; Code:

(require 'supertag-automation)
(require 'supertag-ops-field)
(require 'supertag-ops-node)
(require 'supertag-ops-tag)

(defun test-automation-complete-setup ()
  "Set up the automation rule for testing."
  (interactive)
  
  ;; Delete old rule if exists
  (supertag-automation-delete "test-archive-rule")
  
  ;; Create the automation rule
  (supertag-automation-create
   '(:name "test-archive-rule"
     :trigger :on-field-change
     :condition '(and (has-tag "task") 
                     (field-equals "status" "done"))
     :actions ((:action :update-todo-state 
                :params (:state "DONE"))
               (:action :remove-tag 
                :params (:tag "task"))
               (:action :add-tag 
                :params (:tag "archive")))))
  
  ;; Rebuild rule index
  (supertag-rebuild-rule-index)
  
  (message "✓ Automation rule 'test-archive-rule' created and indexed"))

(defun test-automation-complete-create-test-node ()
  "Create a test node with task tag and status field."
  (interactive)
  
  ;; Create a test node
  (let* ((test-id (org-id-new))
         (test-title (format "Test Node %s" (format-time-string "%H:%M:%S")))
         (node-data (list :id test-id
                         :title test-title
                         :tags '("task")
                         :file (buffer-file-name)
                         :level 1
                         :type :node)))
    
    ;; Create the node in database
    (supertag-node-create node-data)
    
    ;; Insert into current org buffer
    (save-excursion
      (goto-char (point-max))
      (insert "\n")
      (insert (format "* %s #task\n" test-title))
      (insert ":PROPERTIES:\n")
      (insert (format ":ID:       %s\n" test-id))
      (insert ":END:\n\n"))
    
    (save-buffer)
    
    ;; Add task tag with status field
    (supertag-ops-add-tag-to-node test-id "task" :create-if-needed t)
    
    ;; Ensure task tag has status field definition
    (unless (supertag-tag-get-field "task" "status")
      (supertag-tag-add-field "task"
                             '(:name "status"
                               :type :select
                               :options ("todo" "in-progress" "done")
                               :default "todo")))
    
    ;; Set initial status
    (supertag-field-set test-id "task" "status" "todo")
    
    (message "✓ Created test node: %s (ID: %s)" test-title test-id)
    (message "  Initial state: #task, status=todo")
    (message "  Next: Change status to 'done' to trigger automation")
    
    test-id))

(defun test-automation-complete-trigger (node-id)
  "Trigger the automation by changing status to 'done'."
  (interactive 
   (list (read-string "Node ID: " 
                     (or (org-id-get) 
                         (when (boundp 'supertag-view-node--current-node-id)
                           supertag-view-node--current-node-id)))))
  
  (message "")
  (message "=== Triggering Automation ===")
  (message "Changing status from 'todo' to 'done'...")
  
  ;; Change status to done (this should trigger the automation)
  (supertag-field-set node-id "task" "status" "done")
  
  ;; Wait a moment for async event queue to process
  (sit-for 0.1)
  
  (message "")
  (message "=== Checking Results ===")
  
  ;; Check results
  (let ((node (supertag-get `(:nodes ,node-id)))
        (relations (supertag-relation-find-by-from node-id :node-tag)))
    
    (message "Node title: %s" (plist-get node :title))
    (message "Node tags: %S" (plist-get node :tags))
    (message "Relations: %S" (mapcar (lambda (r) (plist-get r :to)) relations))
    
    ;; Verify automation worked
    (let ((has-task (member "task" (plist-get node :tags)))
          (has-archive (member "archive" (plist-get node :tags)))
          (task-relation (cl-find "task" relations :key (lambda (r) (plist-get r :to)) :test #'equal))
          (archive-relation (cl-find "archive" relations :key (lambda (r) (plist-get r :to)) :test #'equal)))
      
      (message "")
      (message "=== Verification Results ===")
      (message "✓ Task tag removed from :tags? %s" (if has-task "❌ NO" "✅ YES"))
      (message "✓ Archive tag added to :tags? %s" (if has-archive "✅ YES" "❌ NO"))
      (message "✓ Task relation deleted? %s" (if task-relation "❌ NO" "✅ YES"))
      (message "✓ Archive relation created? %s" (if archive-relation "✅ YES" "❌ NO"))
      
      (if (and (not has-task) has-archive (not task-relation) archive-relation)
          (progn
            (message "")
            (message "🎉 Automation test PASSED!"))
        (progn
          (message "")
          (message "❌ Automation test FAILED!")
          (message "   Check the debug messages above for details"))))))

(defun test-automation-complete-full-test ()
  "Run complete automation test: setup -> create node -> trigger -> verify."
  (interactive)
  
  (message "")
  (message "╔════════════════════════════════════════╗")
  (message "║   Complete Automation System Test     ║")
  (message "╚════════════════════════════════════════╝")
  (message "")
  
  ;; Step 1: Setup
  (message "Step 1: Setting up automation rule...")
  (test-automation-complete-setup)
  (sit-for 0.2)
  
  ;; Step 2: Create test node
  (message "")
  (message "Step 2: Creating test node...")
  (let ((test-id (test-automation-complete-create-test-node)))
    (sit-for 0.2)
    
    ;; Step 3: Trigger automation
    (message "")
    (message "Step 3: Triggering automation...")
    (test-automation-complete-trigger test-id))
  
  (message "")
  (message "Test completed!"))

(defun test-automation-complete-cleanup ()
  "Clean up test rule."
  (interactive)
  (supertag-automation-delete "test-archive-rule")
  (message "✓ Test automation rule deleted"))

(provide 'test-automation-complete)
;;; test-automation-complete.el ends here

;;; org-supertag-command-tests.el --- Tests for org-supertag-command -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-supertag-command)

;;; Basic Behavior Tests

(ert-deftest org-supertag-test-behavior-define ()
  "Test basic behavior definition."
  (let ((behavior (org-supertag-behavior-define 'test-behavior
                   '(:when (:timing :immediate)
                     :what (:operation :transform
                           :transform :property
                           :property "status"
                           :value "done")
                     :how (:method :sync)))))
    (message "Defined behavior: %S" behavior)
    ;; Check structure
    (message "Checking behavior structure...")
    (should (plist-get behavior :name))
    (should (plist-get behavior :when))
    (should (plist-get behavior :what))
    (should (plist-get behavior :how))
    ;; Check values
    (message "Checking behavior values...")
    (should (eq (plist-get behavior :name) 'test-behavior))
    (should (eq (plist-get (plist-get behavior :when) :timing) :immediate))
    (should (eq (plist-get (plist-get behavior :how) :method) :sync))
    (message "All behavior checks passed")))

(ert-deftest org-supertag-test-behavior-execute ()
  "Test behavior execution."
  (let* ((test-node-id "test-node")
         (node-props '(:type :node
                      :id "test-node"
                      :title "Test Node"
                      :file-path "/test/file.org"
                      :pos 1
                      :olp ("Root")
                      :level 1))
         ;; Create test node
         (node (progn
                (message "Creating test node with props: %S" node-props)
                (org-supertag-db-add test-node-id node-props)))
         ;; Define behavior
         (behavior (progn
                    (message "Defining test behavior")
                    (org-supertag-behavior-define 'test-behavior
                      '(:when (:timing :immediate
                              :condition (:type :node))
                        :what (:operation :transform
                              :transform :property
                              :property "status"
                              :value "done")
                        :how (:method :sync)))))
         ;; Execute behavior
         (result (progn
                  (message "Executing behavior with node ID: %s" test-node-id)
                  (org-supertag-behavior-execute behavior `(:node-id ,test-node-id)))))
    (message "Behavior execution result: %S" result)
    ;; Verify property was set correctly
    (should (org-supertag-behavior--verify-property test-node-id "status" "done"))))

;;; Component System Tests

(ert-deftest org-supertag-test-component-define ()
  "Test component definition and retrieval."
  ;; Define test component
  (org-supertag-component-define 'test-component
    '(:when (:timing :immediate)
      :what (:operation :notify
            :message "Test notification")
      :how (:method :sync)))
  ;; Try to retrieve it
  (let ((component (org-supertag-component-get 'test-component)))
    (should component)
    (should (plist-get component :when))
    (should (plist-get component :what))
    (should (plist-get component :how))))

;;; State Management Tests

(ert-deftest org-supertag-test-state-management ()
  "Test state management functions."
  ;; Test state set/get
  (org-supertag-behavior--state-set "test-key" "test-value")
  (should (equal (org-supertag-behavior--state-get "test-key")
                "test-value"))
  
  ;; Test state update
  (org-supertag-behavior--state-update "test-key"
                                      (lambda (old-value)
                                        (concat old-value "-updated")))
  (should (equal (org-supertag-behavior--state-get "test-key")
                "test-value-updated"))
  
  ;; Test state remove
  (org-supertag-behavior--state-remove "test-key")
  (should (null (org-supertag-behavior--state-get "test-key"))))

;;; Execution Engine Tests

(ert-deftest org-supertag-test-task-scheduler ()
  "Test task scheduling and execution."
  (let* ((test-executed nil)
         (test-behavior (org-supertag-behavior-define 'test-behavior
                         '(:when (:timing :immediate)
                           :what (:operation :notify
                                 :message "Test executed")
                           :how (:method :sync))))
         ;; Schedule task
         (_ (org-supertag-behavior--schedule-task test-behavior nil))
         ;; Process queue
         (_ (org-supertag-behavior--process-task-queue)))
    ;; Check task queue is empty after processing
    (should (null org-supertag-behavior--task-queue))))

(ert-deftest org-supertag-test-batch-execution ()
  "Test batch operation execution."
  ;; Start new batch
  (org-supertag-behavior--batch-start)
  
  ;; Add operations
  (org-supertag-behavior--batch-add
   '(:what (:operation :notify :message "Test 1")
     :how (:method :sync)))
  (org-supertag-behavior--batch-add
   '(:what (:operation :notify :message "Test 2")
     :how (:method :sync)))
  
  ;; Execute batch
  (let ((results (org-supertag-behavior--batch-execute)))
    ;; Check results
    (should (= (length results) 2))
    ;; Check queue is empty
    (should (null org-supertag-behavior--batch-queue))))

;;; Component Composition Tests

(ert-deftest org-supertag-test-component-composition ()
  "Test component composition functionality."
  (message "\n=== Starting Component Composition Test ===")
  
  ;; First register components
  (message "Registering check-todo component...")
  (let ((check-todo-spec '(:name check-todo
                          :when (:timing :immediate
                                :condition (:type :node
                                         :todo "DONE"))
                          :what (:operation :function
                                :function org-supertag-behavior--check-todo
                                :args (:value "DONE"))
                          :how (:method :sync))))
    (message "Check-todo spec: %S" check-todo-spec)
    (org-supertag-component-define 'check-todo check-todo-spec))
  
  (message "Registering notify-done component...")
  (let ((notify-done-spec '(:name notify-done
                           :when (:timing :immediate
                                 :condition (:type :custom
                                          :function always))
                           :what (:operation :notify
                                 :message "Task {title} is done!")
                           :how (:method :sync))))
    (message "Notify-done spec: %S" notify-done-spec)
    (org-supertag-component-define 'notify-done notify-done-spec))
  
  ;; Then test composition
  (let* ((test-node-id "test-node")
         ;; Setup test node
         (node-props '(:type :node
                      :id "test-node"
                      :title "Test Task"
                      :file-path "/test/file.org"
                      :pos 1
                      :olp ("Root")
                      :level 1
                      :todo "DONE")))
    
    (message "Setting up test node with props: %S" node-props)
    ;; 检查类型验证
    (message "Type validation: %S" 
            org-supertag-db-object-type)
    ;; 检查属性验证
    (message "Props validation: %S"
            (org-supertag-db-valid-object-p 
             (plist-get node-props :type)
             node-props))
    ;; 尝试创建节点
    (condition-case err
        (org-supertag-db-add test-node-id node-props)
      (error
       (message "Error creating node: %S" err)
       (signal (car err) (cdr err))))
    
    (let* ((components '(check-todo notify-done))
           (context `(:node-id ,test-node-id :title "Test Task")))
      (message "Components to compose: %S" components)
      (message "Context for composition: %S" context)
      
      ;; 尝试组合组件
      (message "Attempting to compose components...")
      (condition-case err
          (progn
            (message "Getting component definitions...")
            (let* ((check-todo-def (org-supertag-component-get 'check-todo))
                   (_ (message "check-todo definition: %S" check-todo-def))
                   (notify-done-def (org-supertag-component-get 'notify-done))
                   (_ (message "notify-done definition: %S" notify-done-def))
                   (result (org-supertag-behavior--compose-components 
                          components context)))
              (message "Composition result: %S" result)
              ;; Verify results
              (should (= (length result) 2))
              (should (car result))
              (should (cadr result))))
        (error
         (message "Error during composition: %S" err)
         (signal (car err) (cdr err))))))
  
  (message "=== Component Composition Test Complete ===\n"))

(ert-deftest org-supertag-test-args-processing ()
  "Test argument processing with context."
  (let* ((context '(:node-id "123" 
                   :title "Test Task" 
                   :deadline "2024-01-01"))
         (args '(:message "Task {title} due on {deadline}"
                :value "{title}"))
         (processed (org-supertag-behavior--process-args args context)))
    ;; Check processed values
    (should (equal (plist-get processed :message)
                  "Task Test Task due on 2024-01-01"))
    (should (equal (plist-get processed :value)
                  "Test Task"))))

(ert-deftest org-supertag-test-condition-checking ()
  "Test condition checking system."
  ;; Clear test database
  (clrhash org-supertag--test-db)
  
  (let* ((test-node-id "test-node")
         ;; Convert deadline string to time
         (deadline (encode-time 
                   (parse-time-string "2024-01-01 00:00:00")))
         ;; Setup test node with all required properties
         (_ (org-supertag-db-add test-node-id
                                `(:type :node
                                  :id "test-node"
                                  :title "Test Node"
                                  :file-path "/test/file.org"
                                  :pos 1
                                  :olp ("Root")
                                  :level 1
                                  :todo "DONE"
                                  :deadline ,deadline)))
         ;; Test conditions
         (todo-condition '(:type :todo :value "DONE"))
         (deadline-condition '(:type :deadline :days 7 :direction before))
         (property-condition '(:type :property :name "priority" :value "A")))
    
    ;; Verify node was added
    (should (gethash test-node-id org-supertag--test-db))
    
    ;; Check TODO condition
    (should (org-supertag-behavior--check-condition todo-condition 
                                                   `(:node-id ,test-node-id)))
    
    ;; Check deadline condition
    (should (org-supertag-behavior--check-condition deadline-condition 
                                                   `(:node-id ,test-node-id)))
    
    ;; Check property condition (should fail)
    (should-not (org-supertag-behavior--check-condition property-condition 
                                                       `(:node-id ,test-node-id)))))

(ert-deftest org-supertag-test-event-integration ()
  "Test behavior event integration."
  (let* ((test-node-id "test-node")
         (events-received '())
         ;; Setup event listener
         (_ (org-supertag-db-on 'behavior:before-execute
              (lambda (behavior context)
                (push `(:before ,behavior ,context) events-received))))
         (_ (org-supertag-db-on 'behavior:after-execute
              (lambda (behavior context result)
                (push `(:after ,behavior ,context ,result) 
                      events-received))))
         ;; Define and execute test behavior
         (behavior (org-supertag-behavior-define 'test-behavior
                    '(:when (:timing :immediate)
                      :what (:operation :notify
                            :message "Test")
                      :how (:method :sync))))
         (_ (org-supertag-behavior--wrap-execution 
             behavior 
             `(:node-id ,test-node-id))))
    ;; Verify events
    (should (= (length events-received) 2))
    (should (eq (caar events-received) :after))
    (should (eq (caar (cdr events-received)) :before))))

(provide 'org-supertag-command-tests) 
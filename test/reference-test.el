;;; reference-test.el --- Reference virtual column tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the reference virtual column type

;;; Code:

(require 'ert)

;; Load the module under test
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(require 'supertag-virtual-column)

(ert-deftest test-reference-column-create ()
  "Test creating reference virtual column."
  (supertag-virtual-column-init)
  (let ((col (supertag-virtual-column-create
              (list :id "parent-deadline"
                    :name "Parent Deadline"
                    :type :reference
                    :params (list :relation "parent"
                                  :field "deadline")))))
    (should col)
    (should (equal (plist-get col :id) "parent-deadline"))
    (should (eq (plist-get col :type) :reference))
    (let ((params (plist-get col :params)))
      (should (equal (plist-get params :relation) "parent"))
      (should (equal (plist-get params :field) "deadline")))))

(ert-deftest test-reference-column-with-index ()
  "Test reference with index parameter."
  (supertag-virtual-column-init)
  (let ((col (supertag-virtual-column-create
              (list :id "second-parent-status"
                    :name "Second Parent Status"
                    :type :reference
                    :params (list :relation "parent"
                                  :field "status"
                                  :index 1)))))
    (should col)
    (should (eq (plist-get (plist-get col :params) :index) 1))))

(ert-deftest test-reference-returns-nil-when-no-relation ()
  "Test reference returns nil when no relation found."
  (supertag-virtual-column-init)
  (supertag-virtual-column-create
   (list :id "test-ref"
         :name "Test Reference"
         :type :reference
         :params (list :relation "nonexistent-relation"
                       :field "field-name")))
  ;; Should return nil when no relations
  (let ((value (supertag-virtual-column-get "any-node" "test-ref")))
    (should (null value)))
  (supertag-virtual-column-delete "test-ref"))

;; Manual verification helper
(defun test-reference-manual ()
  "Run manual reference tests."
  (interactive)
  (message "=== Reference Virtual Column Tests ===")
  
  (supertag-virtual-column-init)
  
  ;; Create test columns
  (message "\nCreating test reference columns...")
  
  (supertag-virtual-column-create
   (list :id "parent-deadline"
         :name "Parent Deadline"
         :type :reference
         :params (list :relation "parent"
                       :field "deadline")))
  (message "Created: parent-deadline (reference parent node's deadline)")
  
  (supertag-virtual-column-create
   (list :id "parent-status"
         :name "Parent Status"
         :type :reference
         :params (list :relation "parent"
                       :field "status")))
  (message "Created: parent-status (reference parent node's status)")
  
  (supertag-virtual-column-create
   (list :id "project-owner-name"
         :name "Project Owner Name"
         :type :reference
         :params (list :relation "owned-by"
                       :field "name")))
  (message "Created: project-owner-name (reference owner's name)")
  
  ;; List all columns
  (message "\nAll virtual columns:")
  (dolist (col (supertag-virtual-column-list))
    (message "  - %s (%s): %s"
             (plist-get col :id)
             (plist-get col :type)
             (plist-get col :name)))
  
  ;; Try to get values (will be nil if no test data)
  (message "\nReference values (will be nil if no relations):")
  (message "  parent-deadline on 'child-node': %s"
           (supertag-virtual-column-get "child-node" "parent-deadline"))
  (message "  parent-status on 'child-node': %s"
           (supertag-virtual-column-get "child-node" "parent-status"))
  
  (message "\n=== Tests Complete ==="))

(provide 'reference-test)

;;; reference-test.el ends here

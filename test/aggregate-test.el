;;; aggregate-test.el --- Aggregate virtual column tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the aggregate virtual column type

;;; Code:

(require 'ert)

;; Load the module under test
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(require 'supertag-virtual-column)

(ert-deftest test-aggregate-column-create ()
  "Test creating aggregate virtual column."
  (supertag-virtual-column-init)
  (let ((col (supertag-virtual-column-create
              (list :id "total-project-effort"
                    :name "Total Project Effort"
                    :type :aggregate
                    :params (list :tag "project"
                                  :field "effort"
                                  :function :sum)))))
    (should col)
    (should (equal (plist-get col :id) "total-project-effort"))
    (should (eq (plist-get col :type) :aggregate))
    (let ((params (plist-get col :params)))
      (should (equal (plist-get params :tag) "project"))
      (should (equal (plist-get params :field) "effort"))
      (should (eq (plist-get params :function) :sum)))))

(ert-deftest test-aggregate-column-all-functions ()
  "Test that all aggregation functions are accepted."
  (supertag-virtual-column-init)
  (dolist (func '(:sum :count :avg :max :min :first :last))
    (let ((col (supertag-virtual-column-create
                (list :id (format "test-%s" func)
                      :name (format "Test %s" func)
                      :type :aggregate
                      :params (list :tag "test"
                                    :field "value"
                                    :function func)))))
      (should col)
      (should (eq (plist-get (plist-get col :params) :function) func))))
  ;; Clean up
  (dolist (func '(:sum :count :avg :max :min :first :last))
    (supertag-virtual-column-delete (format "test-%s" func))))

(ert-deftest test-aggregate-returns-zero-when-no-nodes ()
  "Test aggregate returns 0/count when no nodes found."
  (supertag-virtual-column-init)
  (supertag-virtual-column-create
   (list :id "test-agg"
         :name "Test Aggregate"
         :type :aggregate
         :params (list :tag "nonexistent-tag-xyz"
                       :field "effort"
                       :function :sum)))
  ;; Should return 0 for sum with no nodes
  (let ((value (supertag-virtual-column-get "any-node" "test-agg")))
    (should (or (null value) (eq value 0))))
  (supertag-virtual-column-delete "test-agg"))

;; Manual verification helper
(defun test-aggregate-manual ()
  "Run manual aggregate tests."
  (interactive)
  (message "=== Aggregate Virtual Column Tests ===")
  
  (supertag-virtual-column-init)
  
  ;; Create test columns
  (message "\nCreating test aggregate columns...")
  
  (supertag-virtual-column-create
   (list :id "total-project-effort"
         :name "Total Project Effort"
         :type :aggregate
         :params (list :tag "project"
                       :field "effort"
                       :function :sum)))
  (message "Created: total-project-effort (sum of effort for all #project)")
  
  (supertag-virtual-column-create
   (list :id "project-count"
         :name "Project Count"
         :type :aggregate
         :params (list :tag "project"
                       :field "effort"
                       :function :count)))
  (message "Created: project-count (count of #project nodes)")
  
  (supertag-virtual-column-create
   (list :id "avg-project-effort"
         :name "Average Project Effort"
         :type :aggregate
         :params (list :tag "project"
                       :field "effort"
                       :function :avg)))
  (message "Created: avg-project-effort (average effort for #project)")
  
  ;; List all columns
  (message "\nAll virtual columns:")
  (dolist (col (supertag-virtual-column-list))
    (message "  - %s (%s): %s"
             (plist-get col :id)
             (plist-get col :type)
             (plist-get col :name)))
  
  ;; Try to get values (will be 0 if no test data)
  (message "\nAggregate values (will be 0 if no test data):")
  (message "  total-project-effort on 'test-node': %s"
           (supertag-virtual-column-get "test-node" "total-project-effort"))
  (message "  project-count on 'test-node': %s"
           (supertag-virtual-column-get "test-node" "project-count"))
  (message "  avg-project-effort on 'test-node': %s"
           (supertag-virtual-column-get "test-node" "avg-project-effort"))
  
  (message "\n=== Tests Complete ==="))

(provide 'aggregate-test)

;;; aggregate-test.el ends here

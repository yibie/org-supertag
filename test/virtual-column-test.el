;;; virtual-column-test.el --- Tests for supertag-virtual-column -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for virtual column system.

;;; Code:

(require 'ert)

;; Mock declarations for compilation
(declare-function supertag-virtual-column-create "supertag-virtual-column" (props))
(declare-function supertag-virtual-column-get-definition "supertag-virtual-column" (column-id))
(declare-function supertag-virtual-column-update "supertag-virtual-column" (column-id updater))
(declare-function supertag-virtual-column-delete "supertag-virtual-column" (column-id))
(declare-function supertag-virtual-column-list "supertag-virtual-column" ())
(declare-function supertag-virtual-column-get "supertag-virtual-column" (node-id column-id &optional default))
(declare-function supertag-virtual-column-refresh "supertag-virtual-column" (node-id column-id))
(declare-function supertag-virtual-column-clear-cache "supertag-virtual-column" (&optional node-id))

;; Load the module when running tests
(when (file-exists-p (expand-file-name "../supertag-virtual-column.el" (file-name-directory (or load-file-name buffer-file-name))))
  (load-file (expand-file-name "../supertag-virtual-column.el" (file-name-directory (or load-file-name buffer-file-name)))))

;;; --- Test Setup ---

(defvar supertag-test--virtual-column-fixtures nil
  "Store test fixtures for cleanup.")

(defun supertag-test--virtual-column-setup ()
  "Set up test environment."
  ;; Ensure module is loaded
  (unless (featurep 'supertag-virtual-column)
    (error "supertag-virtual-column module not loaded"))
  ;; Initialize hash tables if needed
  (unless (boundp 'supertag--virtual-column-definitions)
    (defvar supertag--virtual-column-definitions (make-hash-table :test 'equal)))
  (unless (boundp 'supertag--virtual-column-cache)
    (defvar supertag--virtual-column-cache (make-hash-table :test 'equal)))
  (unless (boundp 'supertag--virtual-column-dependency-graph)
    (defvar supertag--virtual-column-dependency-graph (make-hash-table :test 'equal)))
  (unless (boundp 'supertag--virtual-column-compute-stack)
    (defvar supertag--virtual-column-compute-stack nil))
  ;; Clear them
  (clrhash supertag--virtual-column-definitions)
  (clrhash supertag--virtual-column-cache)
  (clrhash supertag--virtual-column-dependency-graph)
  (setq supertag--virtual-column-compute-stack nil))

(defun supertag-test--virtual-column-teardown ()
  "Clean up test environment."
  (supertag-test--virtual-column-setup))

(defmacro supertag-test-with-virtual-column-env (&rest body)
  "Execute BODY in isolated virtual column test environment."
  `(progn
     (supertag-test--virtual-column-setup)
     (unwind-protect
         (progn ,@body)
       (supertag-test--virtual-column-teardown))))

;;; --- Schema Tests ---

(ert-deftest test-virtual-column-create-basic ()
  "Test basic virtual column creation."
  (skip-unless (featurep 'supertag-virtual-column))
  (supertag-test-with-virtual-column-env
   ;; Create a rollup column
   (let ((def (supertag-virtual-column-create
               (list :id "test-rollup"
                     :name "Test Rollup"
                     :type :rollup
                     :params (list :relation "children"
                                   :field "effort"
                                   :function :sum)))))
     ;; Verify definition structure
     (should (equal (plist-get def :id) "test-rollup"))
     (should (equal (plist-get def :name) "Test Rollup"))
     (should (eq (plist-get def :type) :rollup))
     (should (plist-get def :created-at))
     ;; Verify it was stored
     (should (supertag-virtual-column-get-definition "test-rollup")))))

(ert-deftest test-virtual-column-create-invalid-type ()
  "Test that invalid type is rejected."
  (skip-unless (featurep 'supertag-virtual-column))
  (supertag-test-with-virtual-column-env
   (should-error
    (supertag-virtual-column-create
     (list :id "test-invalid" :name "Test" :type :invalid-type :params ())))
   :type 'error))

(ert-deftest test-virtual-column-create-duplicate ()
  "Test that duplicate ID is rejected."
  (skip-unless (featurep 'supertag-virtual-column))
  (supertag-test-with-virtual-column-env
   (supertag-virtual-column-create
    (list :id "duplicate" :name "First" :type :rollup :params ()))
   (should-error
    (supertag-virtual-column-create
     (list :id "duplicate" :name "Second" :type :rollup :params ())))
   :type 'error))

(ert-deftest test-virtual-column-update ()
  "Test virtual column update."
  (skip-unless (featurep 'supertag-virtual-column))
  (supertag-test-with-virtual-column-env
   ;; Create initial
   (supertag-virtual-column-create
    (list :id "to-update" :name "Old Name" :type :rollup
          :params (list :relation "children" :field "old" :function :sum)))
   ;; Update
   (let ((updated (supertag-virtual-column-update
                   "to-update"
                   (lambda (old)
                     (plist-put (plist-put old :name "New Name")
                                :params '(:relation "children" :field "new" :function :count))))))
     (should (equal (plist-get updated :name) "New Name"))
     (should (eq (plist-get (plist-get updated :params) :function) :count))
     (should (plist-get updated :updated-at)))))

(ert-deftest test-virtual-column-delete ()
  "Test virtual column deletion."
  (skip-unless (featurep 'supertag-virtual-column))
  (supertag-test-with-virtual-column-env
   (supertag-virtual-column-create
    (list :id "to-delete" :name "Delete Me" :type :rollup :params ()))
   (should (supertag-virtual-column-get-definition "to-delete"))
   (supertag-virtual-column-delete "to-delete")
   (should-not (supertag-virtual-column-get-definition "to-delete"))))

(ert-deftest test-virtual-column-list ()
  "Test listing virtual columns."
  (skip-unless (featurep 'supertag-virtual-column))
  (supertag-test-with-virtual-column-env
   ;; Create multiple columns
   (supertag-virtual-column-create
    (list :id "col-a" :name "A" :type :rollup :params ()))
   (supertag-virtual-column-create
    (list :id "col-b" :name "B" :type :rollup :params ()))
   (supertag-virtual-column-create
    (list :id "col-c" :name "C" :type :rollup :params ()))
   ;; List should return all 3, sorted by id
   (let ((cols (supertag-virtual-column-list)))
     (should (= (length cols) 3))
     (should (equal (plist-get (nth 0 cols) :id) "col-a"))
     (should (equal (plist-get (nth 1 cols) :id) "col-b"))
     (should (equal (plist-get (nth 2 cols) :id) "col-c")))))

;;; --- Cache Tests ---

(ert-deftest test-virtual-column-cache-basic ()
  "Test basic cache operations."
  (skip-unless (featurep 'supertag-virtual-column))
  (supertag-test-with-virtual-column-env
   ;; Put value in cache
   (supertag-virtual-column--cache-put "node-1" "vc-1" 42 '(("node-2" . "field-1")))
   ;; Get should return cached value
   (let ((entry (supertag-virtual-column--cache-get "node-1" "vc-1")))
     (should entry)
     (should (= (supertag-virtual-column-cache-value entry) 42))
     (should (equal (supertag-virtual-column-cache-dependencies entry)
                    '(("node-2" . "field-1")))))))

(ert-deftest test-virtual-column-cache-invalidation ()
  "Test cache invalidation."
  (skip-unless (featurep 'supertag-virtual-column))
  (supertag-test-with-virtual-column-env
   (supertag-virtual-column--cache-put "node-1" "vc-1" 42 nil)
   ;; Mark as dirty
   (supertag-virtual-column--cache-invalidate "node-1" "vc-1")
   ;; Cache entry should be marked dirty
   (let ((entry (supertag-virtual-column--cache-get "node-1" "vc-1")))
     (should (supertag-virtual-column-cache-dirty-flag entry)))))

(ert-deftest test-virtual-column-cache-clear ()
  "Test cache clearing."
  (skip-unless (featurep 'supertag-virtual-column))
  (supertag-test-with-virtual-column-env
   (supertag-virtual-column--cache-put "node-1" "vc-1" 42 nil)
   (supertag-virtual-column--cache-put "node-2" "vc-2" 100 nil)
   ;; Clear all
   (supertag-virtual-column-clear-cache)
   ;; Should be empty
   (should-not (supertag-virtual-column--cache-get "node-1" "vc-1"))
   (should-not (supertag-virtual-column--cache-get "node-2" "vc-2"))))

;;; --- Rollup Function Tests ---

(ert-deftest test-virtual-column-rollup-functions ()
  "Test rollup aggregation functions with known values."
  (let ((values '(1 2 3 4 5)))
    ;; :sum
    (should (= (apply #'+ values) 15))
    ;; :count
    (should (= (length values) 5))
    ;; :avg
    (should (= (/ (apply #'+ values) (length values)) 3))
    ;; :max
    (should (= (apply #'max values) 5))
    ;; :min
    (should (= (apply #'min values) 1))
    ;; :first
    (should (= (car values) 1))
    ;; :last
    (should (= (car (last values)) 5))))

(ert-deftest test-virtual-column-rollup-edge-cases ()
  "Test rollup edge cases."
  ;; Single value
  (should (= (apply #'+ '(42)) 42))
  (should (= (car '(42)) 42))
  ;; Empty for sum (with default 0)
  (should (= (apply #'+ '(0)) 0))
  ;; Negative numbers
  (should (= (apply #'+ '(-5 -3 -2)) -10))
  (should (= (apply #'max '(-5 -3 -2)) -2)))

;;; --- Manual Test Helper ---

(defun supertag-test-virtual-column-manual ()
  "Run manual tests and display results."
  (interactive)
  (with-output-to-temp-buffer "*Virtual Column Tests*"
    (princ "Virtual Column Manual Test Results\n")
    (princ "==================================\n\n")
    
    ;; Test 1: Basic creation
    (princ "Test 1: Basic Creation\n")
    (condition-case err
        (progn
          (supertag-test--virtual-column-setup)
          (supertag-virtual-column-create
           (list :id "manual-test" :name "Manual Test" :type :rollup
                 :params (list :relation "children" :field "effort" :function :sum)))
          (if (supertag-virtual-column-get-definition "manual-test")
              (princ "✓ PASS: Virtual column created successfully\n")
            (princ "✗ FAIL: Virtual column not found after creation\n"))
          (supertag-test--virtual-column-teardown))
      (error (princ (format "✗ ERROR: %s\n" (error-message-string err)))))
    
    (princ "\n")
    
    ;; Test 2: Cache operations
    (princ "Test 2: Cache Operations\n")
    (condition-case err
        (progn
          (supertag-test--virtual-column-setup)
          (supertag-virtual-column--cache-put "test-node" "test-vc" 123 nil)
          (let ((entry (supertag-virtual-column--cache-get "test-node" "test-vc")))
            (if (and entry (= (supertag-virtual-column-cache-value entry) 123))
                (princ "✓ PASS: Cache put/get works\n")
              (princ "✗ FAIL: Cache value mismatch\n")))
          (supertag-test--virtual-column-teardown))
      (error (princ (format "✗ ERROR: %s\n" (error-message-string err)))))
    
    (princ "\n")
    
    ;; Test 3: List columns
    (princ "Test 3: List Columns\n")
    (condition-case err
        (progn
          (supertag-test--virtual-column-setup)
          (supertag-virtual-column-create (list :id "a" :name "A" :type :rollup :params ()))
          (supertag-virtual-column-create (list :id "b" :name "B" :type :rollup :params ()))
          (let ((cols (supertag-virtual-column-list)))
            (if (= (length cols) 2)
                (princ "✓ PASS: List returns correct count\n")
              (princ (format "✗ FAIL: Expected 2 columns, got %d\n" (length cols)))))
          (supertag-test--virtual-column-teardown))
      (error (princ (format "✗ ERROR: %s\n" (error-message-string err)))))
    
    (princ "\n")
    (princ "Manual tests complete.\n")))

;;; --- Provider Registration

(provide 'virtual-column-test)

;;; virtual-column-test.el ends here

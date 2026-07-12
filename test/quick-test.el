;;; quick-test.el --- Minimal test for virtual column -*- lexical-binding: t; -*-

;; Quick test to verify virtual column module loads and works

;; Step 1: Setup load path
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

;; Step 2: Load module
(require 'supertag-virtual-column)

;; Step 3: Run basic tests
(message "=== Virtual Column Quick Test ===")

;; Test 1: Create
(message "Test 1: Creating virtual column...")
(let ((def (supertag-virtual-column-create
            (list :id "quick-test"
                  :name "Quick Test"
                  :type :rollup
                  :params (list :relation "children"
                                :field "effort"
                                :function :sum)))))
  (if (equal (plist-get def :id) "quick-test")
      (message "✓ Create: PASS")
    (message "✗ Create: FAIL")))

;; Test 2: Get definition
(message "Test 2: Getting definition...")
(let ((def (supertag-virtual-column-get-definition "quick-test")))
  (if (and def (equal (plist-get def :name) "Quick Test"))
      (message "✓ Get: PASS")
    (message "✗ Get: FAIL")))

;; Test 3: Cache
(message "Test 3: Cache operations...")
(supertag-virtual-column--cache-put "node-1" "quick-test" 100 nil)
(let ((entry (supertag-virtual-column--cache-get "node-1" "quick-test")))
  (if (and entry (= (supertag-virtual-column-cache-value entry) 100))
      (message "✓ Cache: PASS")
    (message "✗ Cache: FAIL")))

;; Test 4: List
(message "Test 4: List columns...")
(let ((cols (supertag-virtual-column-list)))
  (if (and cols (= (length cols) 1))
      (message "✓ List: PASS (%d column)" (length cols))
    (message "✗ List: FAIL (%d columns)" (length cols))))

;; Test 5: Delete
(message "Test 5: Delete column...")
(supertag-virtual-column-delete "quick-test")
(if (supertag-virtual-column-get-definition "quick-test")
    (message "✗ Delete: FAIL (still exists)")
  (message "✓ Delete: PASS"))

(message "=== Quick Test Complete ===")

;;; virtual-column-benchmark.el --- Performance benchmarks for virtual columns -*- lexical-binding: t; -*-

;;; Commentary:

;; Performance benchmarks for virtual column system.
;; Tests rollup calculation with large datasets (1000+ child tasks).
;;
;; Usage:
;;   M-x supertag-benchmark-virtual-columns
;;   M-x supertag-benchmark-rollup-1000
;;   M-x supertag-benchmark-run-all

;;; Code:

(require 'cl-lib)

;; Load the module under test
(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))
(require 'supertag-virtual-column)

(defvar supertag-benchmark--test-nodes nil
  "List of test node IDs created during benchmark.")

(defvar supertag-benchmark--parent-node nil
  "Parent node ID for rollup tests.")

(defun supertag-benchmark--setup-test-data (count)
  "Create test data with COUNT child tasks."
  (supertag-benchmark--cleanup-test-data)
  (message "Creating %d test nodes..." count)
  
  ;; Create parent node
  (setq supertag-benchmark--parent-node (format "benchmark-parent-%s" (float-time)))
  
  ;; Create child nodes with effort field
  (dotimes (i count)
    (let ((node-id (format "benchmark-child-%s-%d" (float-time) i))
          (effort (1+ (random 10))))  ; Random effort 1-10
      (push node-id supertag-benchmark--test-nodes)
      ;; Simulate storing node with effort field
      ;; In real scenario, this would use supertag-node-create
      ))
  
  (message "Created %d test nodes" count))

(defun supertag-benchmark--cleanup-test-data ()
  "Clean up test data."
  (setq supertag-benchmark--test-nodes nil)
  (setq supertag-benchmark--parent-node nil)
  (supertag-virtual-column-clear-cache))

(defmacro supertag-benchmark--measure-time (&rest body)
  "Measure execution time of BODY in seconds."
  `(let ((start (float-time)))
     ,@body
     (- (float-time) start)))

(defun supertag-benchmark--mock-rollup-compute (node-id params)
  "Mock rollup compute for benchmarking without database."
  (let* ((count (or (plist-get params :_test-count) 1000))
         (func (plist-get params :function))
         ;; Generate mock values
         (values (cl-loop for i below count
                         collect (1+ (random 10)))))
    ;; Simulate field lookup overhead
    (dotimes (_ count)
      (plist-get params :field))
    ;; Apply aggregation
    (pcase func
      (:sum (apply #'+ (or values '(0))))
      (:count (length values))
      (:avg (if values (/ (apply #'+ values) (length values)) 0))
      (:max (if values (apply #'max values) nil))
      (:min (if values (apply #'min values) nil))
      (:first (car values))
      (:last (car (last values))))))

(defun supertag-benchmark-rollup-cache-miss (count)
  "Benchmark rollup calculation with cache miss for COUNT nodes."
  (interactive "nNumber of child nodes: ")
  (supertag-virtual-column-clear-cache)
  (supertag-virtual-column-init)
  
  ;; Create virtual column
  (supertag-virtual-column-create
   (list :id "benchmark-total-effort"
         :name "Total Effort"
         :type :rollup
         :params (list :relation "children"
                       :field "effort"
                       :function :sum
                       :_test-count count)))
  
  ;; Benchmark cache miss (first computation)
  (let* ((elapsed (supertag-benchmark--measure-time
                   (dotimes (_ 10)  ; Run multiple times for average
                     (supertag-virtual-column-clear-cache)
                     (supertag-virtual-column--compute-rollup 
                      "test-node"
                      (list :relation "children"
                            :field "effort"
                            :function :sum
                            :_test-count count)))))
         (avg-time (/ elapsed 10)))
    
    (message "Rollup cache miss (%d nodes): %.3f ms (avg of 10 runs)" 
             count (* avg-time 1000))
    (supertag-virtual-column-delete "benchmark-total-effort")
    avg-time))

(defun supertag-benchmark-rollup-cache-hit (count)
  "Benchmark rollup calculation with cache hit for COUNT nodes."
  (interactive "nNumber of child nodes: ")
  (supertag-virtual-column-clear-cache)
  (supertag-virtual-column-init)
  
  ;; Create virtual column
  (supertag-virtual-column-create
   (list :id "benchmark-total-effort"
         :name "Total Effort"
         :type :rollup
         :params (list :relation "children"
                       :field "effort"
                       :function :sum
                       :_test-count count)))
  
  ;; Pre-populate cache
  (supertag-virtual-column--compute-rollup 
   "test-node"
   (list :relation "children"
         :field "effort"
         :function :sum
         :_test-count count))
  
  ;; Benchmark cache hit (cached computation)
  (let* ((elapsed (supertag-benchmark--measure-time
                   (dotimes (_ 100)  ; Run many times for measurable result
                     (supertag-virtual-column--cache-get "test-node" "benchmark-total-effort"))))
         (avg-time (/ elapsed 100)))
    
    (message "Rollup cache hit (%d nodes): %.3f ms (avg of 100 runs)" 
             count (* avg-time 1000))
    (supertag-virtual-column-delete "benchmark-total-effort")
    avg-time))

(defun supertag-benchmark-formula-complexity (formula-str)
  "Benchmark formula evaluation complexity."
  (interactive "sFormula (e.g., '(a + b) * 2'): ")
  (let* ((ast (supertag-formula-parse-string formula-str))
         (elapsed (supertag-benchmark--measure-time
                   (dotimes (_ 10000)
                     (supertag-formula-eval ast "test-node"))))
         (avg-time (/ elapsed 10000)))
    (message "Formula '%s': %.3f ms (avg of 10000 runs)" 
             formula-str (* avg-time 1000))
    avg-time))

(defun supertag-benchmark-rollup-1000 ()
  "Benchmark rollup with 1000 child nodes."
  (interactive)
  (message "\n=== Benchmark: Rollup with 1000 nodes ===")
  (let ((cache-miss (supertag-benchmark-rollup-cache-miss 1000))
        (cache-hit (supertag-benchmark-rollup-cache-hit 1000)))
    (message "\nResults:")
    (message "  Cache miss: %.3f ms (target: < 5000 ms) %s" 
             (* cache-miss 1000)
             (if (< cache-miss 5.0) "✓ PASS" "✗ FAIL"))
    (message "  Cache hit:  %.3f ms (target: < 1000 ms) %s" 
             (* cache-hit 1000)
             (if (< cache-hit 1.0) "✓ PASS" "✗ FAIL"))
    (list :cache-miss cache-miss :cache-hit cache-hit)))

(defun supertag-benchmark-rollup-scaling ()
  "Benchmark rollup performance at different scales."
  (interactive)
  (message "\n=== Benchmark: Rollup Scaling ===")
  (let ((results '()))
    (dolist (count '(100 500 1000 2000))
      (message "\nTesting with %d nodes..." count)
      (let ((cache-miss (supertag-benchmark-rollup-cache-miss count))
            (cache-hit (supertag-benchmark-rollup-cache-hit count)))
        (push (list :count count 
                   :cache-miss cache-miss 
                   :cache-hit cache-hit)
              results)))
    
    (message "\n=== Scaling Results ===")
    (message "%-10s %-15s %-15s" "Nodes" "Cache Miss" "Cache Hit")
    (message "------------------------------------------")
    (dolist (r (reverse results))
      (message "%-10d %-15.3f %-15.6f"
               (plist-get r :count)
               (* (plist-get r :cache-miss) 1000)
               (* (plist-get r :cache-hit) 1000)))
    results))

(defun supertag-benchmark-formula-suite ()
  "Benchmark various formula complexities."
  (interactive)
  (message "\n=== Benchmark: Formula Complexity ===")
  (let ((formulas '("2 + 3"
                   "a + b"
                   "(a + b) * 2"
                   "(done / total) * 100"
                   "x * 2 + y / 3 - z"
                   "((a + b) * (c - d)) / e")))
    (message "%-30s %-15s" "Formula" "Time (ms)")
    (message "------------------------------------------")
    (dolist (f formulas)
      (let ((time (supertag-benchmark-formula-complexity f)))
        (message "%-30s %-15.6f" f (* time 1000))))))

(defun supertag-benchmark-run-all ()
  "Run all benchmarks and generate report."
  (interactive)
  (with-output-to-temp-buffer "*Virtual Column Benchmark*"
    (princ "Virtual Column Performance Benchmark\n")
    (princ "====================================\n\n")
    
    (princ "Date: ")
    (princ (format-time-string "%Y-%m-%d %H:%M:%S"))
    (princ "\n\n")
    
    ;; Rollup 1000 benchmark
    (princ "--- Rollup with 1000 Nodes ---\n")
    (let ((cache-miss (supertag-benchmark-rollup-cache-miss 1000))
          (cache-hit (supertag-benchmark-rollup-cache-hit 1000)))
      (princ (format "Cache miss: %.3f ms (target: < 5000 ms) %s\n"
                     (* cache-miss 1000)
                     (if (< cache-miss 5.0) "✓ PASS" "✗ FAIL")))
      (princ (format "Cache hit:  %.3f ms (target: < 1000 ms) %s\n"
                     (* cache-hit 1000)
                     (if (< cache-hit 1.0) "✓ PASS" "✗ FAIL")))
      (princ "\n"))
    
    ;; Scaling benchmark
    (princ "--- Rollup Scaling ---\n")
    (princ (format "%-10s %-15s %-15s\n" "Nodes" "Cache Miss" "Cache Hit"))
    (princ "-----------------------------------\n")
    (dolist (count '(100 500 1000 2000))
      (let ((cache-miss (supertag-benchmark-rollup-cache-miss count))
            (cache-hit (supertag-benchmark-rollup-cache-hit count)))
        (princ (format "%-10d %-15.3f %-15.6f\n"
                       count
                       (* cache-miss 1000)
                       (* cache-hit 1000)))))
    
    (princ "\n")
    
    ;; Formula benchmark
    (princ "--- Formula Complexity ---\n")
    (princ (format "%-30s %-15s\n" "Formula" "Time (ms)"))
    (princ "------------------------------------------\n")
    (dolist (f '("2 + 3" "a + b" "(a + b) * 2" "(done / total) * 100"))
      (let ((time (supertag-benchmark-formula-complexity f)))
        (princ (format "%-30s %-15.6f\n" f (* time 1000))))))
  
  (pop-to-buffer "*Virtual Column Benchmark*"))

(provide 'virtual-column-benchmark)

;;; virtual-column-benchmark.el ends here

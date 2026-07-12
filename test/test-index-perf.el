;;; test-index-perf.el --- Benchmark relation index performance -*- lexical-binding: t; -*-

;;; Commentary:
;; Compare O(N) maphash scan vs O(k) index lookup for relation queries.
;; Run: emacs --batch -L . -l test/test-index-perf.el

;;; Code:

(require 'cl-lib)
(require 'supertag-core-store)
(require 'supertag-core-index)

;;; --- Old O(N) scan implementations (for comparison) ---

(defun perf-test--scan-find-by-from (from-id &optional type)
  "O(N) full scan — the old implementation."
  (let ((relations (supertag-store-get-collection :relations))
        (result '()))
    (when relations
      (maphash
       (lambda (_id relation)
         (when (and relation
                    (equal (plist-get relation :from) from-id)
                    (or (null type) (eq (plist-get relation :type) type)))
           (push relation result)))
       relations))
    result))

(defun perf-test--scan-find-by-to (to-id &optional type)
  "O(N) full scan — the old implementation."
  (let ((relations (supertag-store-get-collection :relations))
        (result '()))
    (when relations
      (maphash
       (lambda (_id relation)
         (when (and relation
                    (equal (plist-get relation :to) to-id)
                    (or (null type) (eq (plist-get relation :type) type)))
           (push relation result)))
       relations))
    result))

(defun perf-test--scan-find-between (from-id to-id &optional type)
  "O(N) full scan — the old implementation."
  (let ((relations (supertag-store-get-collection :relations))
        (result '()))
    (when relations
      (maphash
       (lambda (_id relation)
         (when (and relation
                    (equal (plist-get relation :from) from-id)
                    (equal (plist-get relation :to) to-id)
                    (or (null type) (eq (plist-get relation :type) type)))
           (push relation result)))
       relations))
    result))

;;; --- Test Harness ---

(defun perf-test--time-call (fn &rest args)
  "Run FN with ARGS N times, return average microseconds."
  (let* ((iterations 1000)
         (start (float-time))
         (_result nil))
    (dotimes (_ iterations)
      (setq _result (apply fn args)))
    (let ((elapsed (- (float-time) start)))
      (/ (* elapsed 1e6) iterations))))

(defun perf-test--generate-data (n-nodes n-relations-per-node)
  "Generate N-NODES nodes with N-RELATIONS-PER-NODE relations each."
  (supertag-store-clear)
  (supertag--ensure-store)
  (let ((node-ids '())
        (types '(:reference :node-tag :node-node :parent-child)))
    ;; Create node ids
    (dotimes (i n-nodes)
      (push (format "node-%05d" i) node-ids))
    (setq node-ids (nreverse node-ids))
    ;; Create relations
    (let ((rel-count 0))
      (dolist (from-id node-ids)
        (dotimes (j n-relations-per-node)
          (let* ((to-idx (mod (+ (string-to-number (substring from-id 5)) j 1) n-nodes))
                 (to-id (format "node-%05d" to-idx))
                 (type (nth (mod j 4) types))
                 (rel-id (format "rel-%05d" rel-count)))
            (supertag-store-put-entity :relations rel-id
              (list :id rel-id :type type :from from-id :to to-id))
            (cl-incf rel-count))))
      ;; Rebuild index
      (supertag-index-rebuild-relations)
      (message "Generated %d nodes, %d relations" n-nodes rel-count)
      (list :nodes n-nodes :relations rel-count))))

(defun perf-test--run-benchmark (label n-nodes rels-per-node)
  "Run a benchmark with LABEL, N-NODES nodes, RELS-PER-NODE relations each."
  (message "\n══════════════════════════════════════════════════")
  (message " %s: %d nodes, %d rels/node" label n-nodes rels-per-node)
  (message "══════════════════════════════════════════════════")
  (let* ((stats (perf-test--generate-data n-nodes rels-per-node))
         (total-rels (plist-get stats :relations))
         ;; Pick a node in the middle for queries
         (test-node (format "node-%05d" (/ n-nodes 2)))
         (test-to   (format "node-%05d" (+ (/ n-nodes 2) 1))))

    (message "\nTotal relations in store: %d" total-rels)
    (message "Query target: %s -> %s\n" test-node test-to)

    ;; Verify correctness first
    (let ((scan-from  (perf-test--scan-find-by-from test-node))
          (index-from (supertag-index-find-by-from test-node)))
      (unless (= (length scan-from) (length index-from))
        (error "MISMATCH find-by-from: scan=%d index=%d"
               (length scan-from) (length index-from)))
      (message "Correctness check: find-by-from returns %d results ✓" (length scan-from)))

    (let ((scan-to  (perf-test--scan-find-by-to test-node))
          (index-to (supertag-index-find-by-to test-node)))
      (unless (= (length scan-to) (length index-to))
        (error "MISMATCH find-by-to: scan=%d index=%d"
               (length scan-to) (length index-to)))
      (message "Correctness check: find-by-to returns %d results ✓" (length scan-to)))

    (let ((scan-btw  (perf-test--scan-find-between test-node test-to))
          (index-btw (supertag-index-find-between test-node test-to)))
      (unless (= (length scan-btw) (length index-btw))
        (error "MISMATCH find-between: scan=%d index=%d"
               (length scan-btw) (length index-btw)))
      (message "Correctness check: find-between returns %d results ✓" (length scan-btw)))

    ;; Benchmark
    (message "\n%-25s %12s %12s %10s" "Operation" "Scan (μs)" "Index (μs)" "Speedup")
    (message "%-25s %12s %12s %10s" "─────────────────────────" "────────────" "────────────" "──────────")

    (let* ((scan-us  (perf-test--time-call #'perf-test--scan-find-by-from test-node))
           (index-us (perf-test--time-call #'supertag-index-find-by-from test-node))
           (speedup  (if (> index-us 0) (/ scan-us index-us) 999.0)))
      (message "%-25s %12.1f %12.1f %9.1fx" "find-by-from" scan-us index-us speedup))

    (let* ((scan-us  (perf-test--time-call #'perf-test--scan-find-by-to test-node))
           (index-us (perf-test--time-call #'supertag-index-find-by-to test-node))
           (speedup  (if (> index-us 0) (/ scan-us index-us) 999.0)))
      (message "%-25s %12.1f %12.1f %9.1fx" "find-by-to" scan-us index-us speedup))

    (let* ((scan-us  (perf-test--time-call #'perf-test--scan-find-between test-node test-to))
           (index-us (perf-test--time-call #'supertag-index-find-between test-node test-to))
           (speedup  (if (> index-us 0) (/ scan-us index-us) 999.0)))
      (message "%-25s %12.1f %12.1f %9.1fx" "find-between" scan-us index-us speedup))

    (let* ((scan-us  (perf-test--time-call #'perf-test--scan-find-by-from test-node :reference))
           (index-us (perf-test--time-call #'supertag-index-find-by-from test-node :reference))
           (speedup  (if (> index-us 0) (/ scan-us index-us) 999.0)))
      (message "%-25s %12.1f %12.1f %9.1fx" "find-by-from (typed)" scan-us index-us speedup))

    ;; Benchmark index rebuild time
    (message "\n--- Index Rebuild ---")
    (let* ((start (float-time))
           (_ (dotimes (_ 10) (supertag-index-rebuild-relations)))
           (elapsed (/ (- (float-time) start) 10.0)))
      (message "Rebuild %d relations: %.1f ms" total-rels (* elapsed 1000)))))

;;; --- Main ---

(perf-test--run-benchmark "Small"   100  5)     ;; 500 relations
(perf-test--run-benchmark "Medium"  500  10)    ;; 5,000 relations
(perf-test--run-benchmark "Large"   2000 10)    ;; 20,000 relations
(perf-test--run-benchmark "XLarge"  5000 10)    ;; 50,000 relations

(message "\n\nDone.")

;;; test-index-perf.el ends here

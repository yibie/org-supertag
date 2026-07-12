;;; perf-benchmark.el --- Performance baseline for org-supertag hot paths -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Establishes a reproducible performance baseline for the known O(N) hot
;; paths in org-supertag, using a synthetic, isolated in-memory dataset.
;; This file never touches the user's real `~/.emacs.d' data.
;;
;; This is a standalone, batch-runnable benchmark. It is NOT wired into
;; test/run-tests.sh (which only runs deterministic pass/fail ERT suites);
;; run it directly with:
;;
;;   emacs -batch -L . --eval "(package-initialize)" \
;;     -l test/perf-benchmark.el -f supertag-perf-benchmark-run
;;
;; What it measures (see `supertag-perf-benchmark-run'):
;;   1. `supertag-index-get-nodes-by-tag'  - O(N) full :nodes scan, hot + rare tag
;;   2. `supertag-index-get-nodes-by-word' - O(N) full :nodes scan, substring search
;;   3. `supertag-save-store' / `supertag-load-store' - full DB serialize/read,
;;      including the atomic-save + verify-after-save path
;;   4. `supertag-index-rebuild-relations' - full :relations scan, index rebuild
;;   5. Table view refresh data path (`supertag-view-table--get-entities',
;;      `supertag-view-table--get-columns', `supertag-view-table--build-state')
;;      plus, since it turned out to need no live window, the full
;;      `supertag-view-table--render-table' pass too (see the feasibility
;;      note above `supertag-perf-benchmark--view-refresh-data').
;;
;; The dataset (10,000 nodes / 50 tags / ~2,000 relations / fields on ~30%
;; of nodes) is generated purely arithmetically (no `random' calls), so
;; repeated runs produce the exact same dataset shape and hit rates.
;;
;; This is measurement only. No optimization work is done here.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'benchmark)
(require 'ht)

(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-core-scan)
(require 'supertag-core-index)
(require 'supertag-core-persistence)
(require 'supertag-view-api)
(require 'supertag-view-table)

;;; --- Dataset configuration (deterministic; do not use `random') ---

(defconst supertag-perf-benchmark-node-count 10000
  "Total number of synthetic nodes.")

(defconst supertag-perf-benchmark-normal-tag-count 49
  "Number of \"normal\" tags. Plus one dedicated rare tag, that's 50 total.")

(defconst supertag-perf-benchmark-rare-tag-node-count 20
  "Number of nodes assigned to the dedicated rare tag.")

(defconst supertag-perf-benchmark-vocab-size 200
  "Size of the shared title vocabulary.")

(defconst supertag-perf-benchmark-relation-count 2000
  "Number of synthetic :reference relations.")

(defconst supertag-perf-benchmark-hot-tag "tag00"
  "Tag id expected to receive the largest node count (~1/50 of nodes).")

(defconst supertag-perf-benchmark-rare-tag "tag-rare"
  "Tag id deliberately assigned very few nodes.")

(defconst supertag-perf-benchmark-common-word "word005"
  "A vocabulary word that appears in a realistic fraction of titles.")

(defconst supertag-perf-benchmark-rare-word "raretoken"
  "A token injected into only a handful of node titles.")

(defconst supertag-perf-benchmark-rare-word-indices '(0 5000 9999)
  "Node indices that get `supertag-perf-benchmark-rare-word' in their title.")

(defconst supertag-perf-benchmark-report-file
  (expand-file-name "dev-note/perf-baseline-2026-07-12.md"
                     (file-name-as-directory
                      (expand-file-name ".."
                                         (file-name-directory
                                          (or load-file-name buffer-file-name)))))
  "Where the baseline report is written.")

;;; --- Fixture construction (isolated in-memory dataset) ---

(defmacro supertag-perf-benchmark--with-temp-env (&rest body)
  "Run BODY with persistence state redirected into an isolated temp dir.
Mirrors the fixture pattern in test/persistence-hardening-test.el: rebinds
`supertag-data-directory', `supertag-db-file', `supertag-db-backup-directory',
and the in-memory store, so this benchmark NEVER touches the user's real
`~/.emacs.d'. The temp directory is removed afterwards."
  (declare (indent 0))
  `(let* ((tmp (file-name-as-directory (make-temp-file "supertag-perf-bench" t)))
          (supertag-data-directory tmp)
          (supertag-db-file (expand-file-name "supertag-db.el" tmp))
          (supertag-db-backup-directory (expand-file-name "backups" tmp))
          (supertag-db-verify-after-save t)
          (supertag-db-lock nil)
          (supertag-db-auto-migrate nil)
          (supertag--store nil)
          (supertag--store-origin nil)
          (supertag--db-lock-conflict nil)
          (supertag--db-locked-file nil))
     (unwind-protect
         (progn ,@body)
       (supertag--db-release-lock)
       (ignore-errors (delete-directory tmp t)))))

(defun supertag-perf-benchmark--tag-id-for-node (i)
  "Return the tag id assigned to node index I (0-based).
The first `supertag-perf-benchmark-rare-tag-node-count' nodes go to the
dedicated rare tag; the rest are round-robin distributed across
`supertag-perf-benchmark-normal-tag-count' normal tags, so tag00 (the
first bucket) ends up the largest -- the designated \"hot\" tag."
  (if (< i supertag-perf-benchmark-rare-tag-node-count)
      supertag-perf-benchmark-rare-tag
    (format "tag%02d"
            (mod (- i supertag-perf-benchmark-rare-tag-node-count)
                 supertag-perf-benchmark-normal-tag-count))))

(defun supertag-perf-benchmark--node-title (i)
  "Build a deterministic title for node index I from the shared vocabulary."
  (let* ((words (cl-loop for k from 0 below 5
                         collect (format "word%03d"
                                         (mod (+ (* i 31) (* k 17) 7)
                                              supertag-perf-benchmark-vocab-size))))
         (base (format "Node %05d %s" i (mapconcat #'identity words " "))))
    (if (memq i supertag-perf-benchmark-rare-word-indices)
        (concat base " " supertag-perf-benchmark-rare-word)
      base)))

(defun supertag-perf-benchmark--build-fixture ()
  "Populate `supertag--store' with the synthetic benchmark dataset.
Returns a plist describing the dataset shape. Uses direct store writes
(`supertag-store-put-entity' and raw hash-table puts), bypassing the ops/
commit layer entirely -- this is fixture generation, not an ops-layer
correctness test, and it keeps generation fast and free of automation/
notification side effects."
  (supertag--ensure-store)
  (let* ((fixed-time (encode-time 0 0 0 1 1 2025))
         (tag-ids (append (cl-loop for j from 0 below supertag-perf-benchmark-normal-tag-count
                                   collect (format "tag%02d" j))
                          (list supertag-perf-benchmark-rare-tag)))
         (field-defs (list (list :name "Priority" :type :string)
                           (list :name "Effort" :type :number)
                           (list :name "Refs" :type :node-reference)))
         (fields-root (supertag-store-get-collection :fields))
         (fields-with-values 0))
    ;; --- Tags ---
    (dolist (tag-id tag-ids)
      (supertag-store-put-entity
       :tags tag-id
       (list :id tag-id :name tag-id :type :tag :extends nil
             :fields field-defs
             :created-at fixed-time :modified-at fixed-time)))
    ;; --- Nodes (+ ~30% get field values) ---
    (dotimes (i supertag-perf-benchmark-node-count)
      (let* ((id (format "node%05d" i))
             (tag-id (supertag-perf-benchmark--tag-id-for-node i))
             (title (supertag-perf-benchmark--node-title i)))
        (supertag-store-put-entity
         :nodes id
         (list :id id :type :node :title title :content nil
               :tags (list tag-id) :file "/tmp/supertag-perf-bench.org"
               :level 1 :olp nil
               :created-at fixed-time :modified-at fixed-time))
        (when (< (mod i 10) 3)         ; ~30% of nodes get field values
          (cl-incf fields-with-values)
          (let* ((node-table (or (gethash id fields-root)
                                 (let ((ht (ht-create)))
                                   (puthash id ht fields-root)
                                   ht)))
                 (tag-table (or (gethash tag-id node-table)
                                (let ((ht (ht-create)))
                                  (puthash tag-id ht node-table)
                                  ht))))
            (puthash "Priority" (nth (mod i 3) '("Low" "Medium" "High")) tag-table)
            (puthash "Effort" (1+ (mod i 10)) tag-table)))))
    ;; --- Relations ---
    (dotimes (r supertag-perf-benchmark-relation-count)
      (let* ((from-idx (mod (* r 7) supertag-perf-benchmark-node-count))
             (to-idx (mod (+ (* r 7) 1 (* r 3)) supertag-perf-benchmark-node-count))
             (from-id (format "node%05d" from-idx))
             (to-id (format "node%05d" to-idx))
             (rel-id (format "rel%05d" r)))
        (supertag-store-put-entity
         :relations rel-id
         (list :id rel-id :type :reference :from from-id :to to-id))))
    (supertag-index-rebuild-relations)
    (list :node-count supertag-perf-benchmark-node-count
          :tag-count (length tag-ids)
          :relation-count supertag-perf-benchmark-relation-count
          :fields-with-values fields-with-values
          :hot-tag-count (length (supertag-index-get-nodes-by-tag
                                   supertag-perf-benchmark-hot-tag))
          :rare-tag-count (length (supertag-index-get-nodes-by-tag
                                    supertag-perf-benchmark-rare-tag))
          :common-word-hits (length (supertag-index-get-nodes-by-word
                                      supertag-perf-benchmark-common-word))
          :rare-word-hits (length (supertag-index-get-nodes-by-word
                                    supertag-perf-benchmark-rare-word)))))

;;; --- Measurement harness ---

(defmacro supertag-perf-benchmark--measure (n &rest body)
  "Run BODY N times, return a list of per-run elapsed wall-clock seconds
\(one float per run, NOT an average).

BODY is byte-compiled exactly ONCE (via `byte-compile', into a zero-arg
closure), so every run times genuinely compiled code, then each of the N
runs is individually timed via `benchmark-run' wrapping a plain `funcall'
of that closure. This two-step approach -- compile once, time N times --
turned out to matter a lot in practice, for two compounding reasons
discovered while developing this file:

1. `benchmark-run-compiled' recompiles its FORMS argument on every macro
   expansion. Naively writing `(benchmark-run-compiled 1 BODY)' inside a
   `(dotimes (_ N) ...)' loop -- i.e. expanding it N times with the real
   BODY inlined -- recompiles the whole benchmarked expression on every
   single run. For a body that scans a 10k-entry hash table this alone
   made even 30 iterations take tens of seconds.
2. Worse, on a native-comp Emacs `benchmark-run-compiled' expands to
   `(benchmark-call (native-compile (quote (lambda () FORMS))) N)' --
   a full native (libgccjit) compilation, not a byte-compile. Invoking
   that on every one of N iterations turned a 300-iteration measurement
   into a multi-minute hang. `native-compile' also compiles from a
   *quoted* lambda, so it cannot see any surrounding lexical `let'
   binding either.

`benchmark-run' (no \"-compiled\") avoids both problems: its macroexpansion
is just `(benchmark-call (lambda () FORMS) N)' -- a plain, real (lexically
closing) closure with no compilation step at all -- so timing N runs of an
already-byte-compiled thunk via `benchmark-run' is cheap and accurate,
while the actual workload under measurement still runs as compiled code."
  (declare (indent 1))
  `(let ((supertag-perf-benchmark--fn (byte-compile (lambda () ,@body)))
         times)
     (dotimes (_ ,n)
       (push (car (benchmark-run 1 (funcall supertag-perf-benchmark--fn))) times))
     (nreverse times)))

(defun supertag-perf-benchmark--min (times) (apply #'min times))
(defun supertag-perf-benchmark--mean (times)
  (/ (apply #'+ times) (float (length times))))

(defvar supertag-perf-benchmark--results nil
  "List of (LABEL ITERATIONS MIN-SECONDS MEAN-SECONDS) accumulated by the
current run of `supertag-perf-benchmark-run'.")

(defun supertag-perf-benchmark--record (label iterations times)
  "Record a benchmark result for LABEL run for ITERATIONS with TIMES."
  (push (list label iterations
              (supertag-perf-benchmark--min times)
              (supertag-perf-benchmark--mean times))
        supertag-perf-benchmark--results))

;;; --- Table view render (data-collection stage; see commentary below) ---

(defun supertag-perf-benchmark--view-refresh-data (tag-name)
  "Run the non-display data-collection stage of a table view refresh for
TAG-NAME in the current buffer, and return the built state plist.
This mirrors what `supertag-view-table-refresh' does before rendering:
re-fetch entity ids, re-fetch columns, then build render state."
  (let ((query-obj (list :type :tag :value tag-name)))
    (setq supertag-view-table--query-objs (list query-obj))
    (setq supertag-view-table--current-table-index 0)
    (setq supertag-view-table--current-view-name nil)
    (setq supertag-view-table--view-config nil)
    (setq supertag-view-table--entity-ids (supertag-view-table--get-entities query-obj))
    (setq supertag-view-table--columns (supertag-view-table--get-columns query-obj))
    (supertag-view-table--build-state)))

;; Feasibility note (see dev-note/perf-baseline-2026-07-12.md "reading the
;; numbers" section for the full writeup): `supertag-view-table--render-table'
;; turned out to need no live window at all -- it only calls `insert',
;; `erase-buffer', and text-property/point operations on the *current
;; buffer*, so it runs cleanly inside `with-temp-buffer' in `-batch' mode.
;; We therefore measure BOTH the data-collection stage alone and the full
;; render (data-collection + `supertag-view-table--render-table') below.

;;; --- Report generation ---

(defun supertag-perf-benchmark--fmt-ms (seconds)
  (format "%.3f" (* seconds 1000.0)))

(defun supertag-perf-benchmark--extrapolate-n-for-100ms (n-now seconds-at-n-now)
  "Linearly extrapolate the dataset size at which SECONDS-AT-N-NOW would
reach 100ms, assuming O(N) scaling from N-NOW. Returns nil when the
operation is already at/above 100ms at N-NOW, or when timing is ~0."
  (when (and (> seconds-at-n-now 0) (< seconds-at-n-now 0.1))
    (round (* n-now (/ 0.1 seconds-at-n-now)))))

(defun supertag-perf-benchmark--build-report (dataset)
  "Build the full report string from DATASET stats and accumulated results."
  (let* ((results (reverse supertag-perf-benchmark--results))
         (node-count (plist-get dataset :node-count)))
    (with-temp-buffer
      (insert "# Org-Supertag Performance Baseline -- 2026-07-12\n\n")
      (insert "Generated by `test/perf-benchmark.el' (`supertag-perf-benchmark-run').\n")
      (insert "Run standalone with:\n\n")
      (insert "```\nemacs -batch -L . --eval \"(package-initialize)\" \\\n")
      (insert "  -l test/perf-benchmark.el -f supertag-perf-benchmark-run\n```\n\n")
      (insert "This is a measurement-only baseline -- no optimization work was done.\n\n")

      (insert "## Machine context\n\n")
      (insert (format "- Emacs version: %s\n" emacs-version))
      (insert (format "- `system-type`: %s\n" system-type))
      (insert (format "- Generated at: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S %Z")))

      (insert "## Dataset shape\n\n")
      (insert (format "- Nodes: %d\n" (plist-get dataset :node-count)))
      (insert (format "- Tags: %d (49 \"normal\" round-robin tags + 1 dedicated rare tag)\n"
                       (plist-get dataset :tag-count)))
      (insert (format "- Hot tag `%s`: %d nodes (~1/50 of dataset)\n"
                       supertag-perf-benchmark-hot-tag (plist-get dataset :hot-tag-count)))
      (insert (format "- Rare tag `%s`: %d nodes\n"
                       supertag-perf-benchmark-rare-tag (plist-get dataset :rare-tag-count)))
      (insert (format "- Common word `%s`: %d node title matches\n"
                       supertag-perf-benchmark-common-word (plist-get dataset :common-word-hits)))
      (insert (format "- Rare word `%s`: %d node title matches\n"
                       supertag-perf-benchmark-rare-word (plist-get dataset :rare-word-hits)))
      (insert (format "- Relations: %d (:reference type)\n" (plist-get dataset :relation-count)))
      (insert (format "- Nodes with field values: %d (~30%%)\n\n"
                       (plist-get dataset :fields-with-values)))

      (insert "## Results\n\n")
      (insert "| Operation | Iterations | Min (ms) | Mean (ms) |\n")
      (insert "|---|---:|---:|---:|\n")
      (dolist (row results)
        (cl-destructuring-bind (label iterations min-s mean-s) row
          (insert (format "| %s | %d | %s | %s |\n"
                          label iterations
                          (supertag-perf-benchmark--fmt-ms min-s)
                          (supertag-perf-benchmark--fmt-ms mean-s)))))
      (insert "\n")

      (insert "## Reading the numbers\n\n")
      (insert (format "Dataset size for this baseline: N = %d nodes.\n\n" node-count))
      (insert "- `supertag-index-get-nodes-by-tag` and `supertag-index-get-nodes-by-word`\n")
      (insert "  (supertag-core-scan.el:17 and :29) are full `maphash` walks over the\n")
      (insert "  entire `:nodes` table on every call -- their cost is O(N) in the total\n")
      (insert "  node count, essentially independent of the tag/word's own hit rate\n")
      (insert "  (hot vs. rare tag, common vs. rare word should read as roughly the same\n")
      (insert "  cost above, since every node is still visited).\n")
      (insert "- Every table view refresh (supertag-view-table.el, `-refresh` /\n")
      (insert "  `--build-state`) re-runs one of these O(N) scans via\n")
      (insert "  `supertag-view-api-list-entity-ids` and then does O(rows * columns)\n")
      (insert "  work rebuilding the whole grid -- there is no incremental/partial\n")
      (insert "  re-render path.\n")
      (insert "- `supertag-index-rebuild-relations` (supertag-core-index.el) is O(R) in\n")
      (insert "  the relation count and runs on every store load.\n\n")
      (insert "### Linear extrapolation to the ~100ms interactive threshold\n\n")
      (insert "The table below linearly extrapolates from this run's O(N)-labeled\n")
      (insert "operations to estimate the dataset size at which each would cross\n")
      (insert "~100ms (a common threshold for \"feels interactive\"). **This is a\n")
      (insert "naive linear projection from a single data point** -- it ignores GC\n")
      (insert "pressure, cache effects, and non-linear terms, and should be read as a\n")
      (insert "rough order-of-magnitude signal, not a prediction.\n\n")
      (insert "| Operation | Time at N | Extrapolated N for ~100ms |\n")
      (insert "|---|---:|---:|\n")
      (dolist (row results)
        (cl-destructuring-bind (label iterations min-s mean-s) row
          (ignore iterations min-s)
          (when (string-match-p "get-nodes-by-\\|rebuild-relations" label)
            (let ((n100 (supertag-perf-benchmark--extrapolate-n-for-100ms node-count mean-s)))
              (insert (format "| %s | %s ms @ N=%d | %s |\n"
                              label (supertag-perf-benchmark--fmt-ms mean-s) node-count
                              (if n100 (format "~%d" n100) "already >=100ms at this N")))))))
      (insert "\n")
      (insert "### Table view render feasibility\n\n")
      (insert "`supertag-view-table--render-table` does not require a live window: it\n")
      (insert "only calls `insert`/`erase-buffer`/text-property operations on the\n")
      (insert "*current buffer*, so both the render-state data-collection stage\n")
      (insert "(`supertag-view-table--build-state`) AND the full render pass were\n")
      (insert "measured directly in `-batch` mode inside a `with-temp-buffer`, scoped\n")
      (insert (format "to the hot tag's %d rows.\n\n" (plist-get dataset :hot-tag-count)))
      (buffer-string))))

;;; --- Main entry point ---

;;;###autoload
(defun supertag-perf-benchmark-run ()
  "Run the full org-supertag performance baseline and print + save a report."
  (interactive)
  (setq supertag-perf-benchmark--results nil)
  (supertag-perf-benchmark--with-temp-env
    (message "Building synthetic dataset (%d nodes, %d relations)..."
             supertag-perf-benchmark-node-count supertag-perf-benchmark-relation-count)
    (let ((dataset (supertag-perf-benchmark--build-fixture)))
      (message "Dataset ready: %S" dataset)

      ;; 1. supertag-index-get-nodes-by-tag: hot tag + rare tag
      (supertag-perf-benchmark--record
       "get-nodes-by-tag (hot)" 300
       (supertag-perf-benchmark--measure 300
         (supertag-index-get-nodes-by-tag supertag-perf-benchmark-hot-tag)))
      (supertag-perf-benchmark--record
       "get-nodes-by-tag (rare)" 300
       (supertag-perf-benchmark--measure 300
         (supertag-index-get-nodes-by-tag supertag-perf-benchmark-rare-tag)))

      ;; 2. supertag-index-get-nodes-by-word: common word + rare word
      (supertag-perf-benchmark--record
       "get-nodes-by-word (common)" 300
       (supertag-perf-benchmark--measure 300
         (supertag-index-get-nodes-by-word supertag-perf-benchmark-common-word)))
      (supertag-perf-benchmark--record
       "get-nodes-by-word (rare)" 300
       (supertag-perf-benchmark--measure 300
         (supertag-index-get-nodes-by-word supertag-perf-benchmark-rare-word)))

      ;; 3. Store save/load, including atomic-save + verify-after-save path.
      (supertag-persistence-ensure-data-directory)
      (cl-letf (((symbol-function 'supertag--persistence--expected-sync-state-file)
                 (lambda () nil)))
        ;; Establish a valid store origin so the save guard does not refuse
        ;; to write (mirrors test/persistence-hardening-test.el's approach
        ;; of neutralizing the unrelated sync-state guard for a store that
        ;; was built directly rather than via `supertag-load-store').
        (supertag--record-store-origin :ok)
        (supertag-mark-dirty)
        (supertag-save-store)           ; writes the fixture to disk once
        (unless (file-exists-p supertag-db-file)
          (error "Fixture save failed; %s does not exist" supertag-db-file))
        (message "Fixture written to %s (%d bytes)"
                 supertag-db-file (file-attribute-size (file-attributes supertag-db-file)))

        (supertag-perf-benchmark--record
         "supertag-save-store (10k nodes)" 5
         (supertag-perf-benchmark--measure 5
           (progn (supertag-mark-dirty) (supertag-save-store))))

        (supertag-perf-benchmark--record
         "supertag-load-store (10k nodes)" 5
         (supertag-perf-benchmark--measure 5
           (supertag-load-store))))

      ;; 4. Relation index rebuild
      (supertag-perf-benchmark--record
       "supertag-index-rebuild-relations" 300
       (supertag-perf-benchmark--measure 300
         (supertag-index-rebuild-relations)))

      ;; 5. Table view refresh data path (+ full render; see feasibility note)
      (with-temp-buffer
        (let ((supertag-view-table--query-objs nil)
              (supertag-view-table--entity-ids nil)
              (supertag-view-table--columns nil)
              (supertag-view-table--current-table-index 0)
              (supertag-view-table--view-config nil)
              (supertag-view-table--named-views nil)
              (supertag-view-table--current-view-name nil)
              (supertag-view-table--expanded-rows (make-hash-table :test 'equal)))
          (supertag-perf-benchmark--record
           "table view: build-state (data only, hot tag)" 50
           (supertag-perf-benchmark--measure 50
             (supertag-perf-benchmark--view-refresh-data supertag-perf-benchmark-hot-tag)))
          (supertag-perf-benchmark--record
           "table view: full render (data + render-table, hot tag)" 50
           (supertag-perf-benchmark--measure 50
             (let ((state (supertag-perf-benchmark--view-refresh-data
                           supertag-perf-benchmark-hot-tag)))
               (supertag-view-table-render 'table state))))))

      ;; --- Report ---
      (let ((report (supertag-perf-benchmark--build-report dataset)))
        (princ report)
        (make-directory (file-name-directory supertag-perf-benchmark-report-file) t)
        (with-temp-file supertag-perf-benchmark-report-file
          (insert report))
        (message "Report written to %s" supertag-perf-benchmark-report-file))))
  (message "supertag-perf-benchmark-run: done."))

(provide 'perf-benchmark)

;;; perf-benchmark.el ends here

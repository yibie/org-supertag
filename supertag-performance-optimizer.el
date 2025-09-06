;;; supertag-performance-optimizer.el --- Performance optimization for Behavior system -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides performance optimization utilities for the modernized
;; Behavior system. It includes:
;;
;; 1. Intelligent caching strategies for behavior execution
;; 2. Batch processing optimizations for bulk operations
;; 3. Memory management for large datasets
;; 4. Query optimization for database operations
;; 5. Event throttling and debouncing for high-frequency updates
;; 6. Performance monitoring and profiling tools
;;
;; The optimizer ensures the new behavior system maintains high performance
;; even with complex automation rules and large datasets.

;;; Code:

(require 'cl-lib)
(require 'supertag-core-store)              ; For data access
(require 'supertag-automation)  ; For automation system
(require 'supertag-services-query)     ; For query system

;;; --- Performance Configuration ---

(defcustom supertag-perf-cache-size 1000
  "Maximum number of items to keep in behavior execution cache."
  :type 'integer
  :group 'supertag-performance)

(defcustom supertag-perf-batch-size 50
  "Default batch size for bulk operations."
  :type 'integer
  :group 'supertag-performance)

(defcustom supertag-perf-throttle-delay 0.1
  "Delay in seconds for event throttling."
  :type 'float
  :group 'supertag-performance)

(defcustom supertag-perf-enable-profiling nil
  "Enable performance profiling and monitoring."
  :type 'boolean
  :group 'supertag-performance)

;;; --- Cache Management ---

(defvar supertag-perf--execution-cache (make-hash-table :test 'equal)
  "Cache for behavior execution results.")

(defvar supertag-perf--query-cache (make-hash-table :test 'equal)
  "Cache for frequently used queries.") 

(defvar supertag-perf--relation-cache (make-hash-table :test 'equal)
  "Cache for relation lookups.")

(defvar supertag-perf--cache-stats (list :hits 0 :misses 0 :evictions 0)
  "Cache performance statistics.")

(defun supertag-perf--cache-key (prefix &rest elements)
  "Generate cache key from PREFIX and ELEMENTS."
  (format "%s:%s" prefix (mapconcat (lambda (e) (format "%s" e)) elements "|")))

(defun supertag-perf--cache-get (cache key)
  "Get value from CACHE using KEY with statistics tracking."
  (if-let ((cached-value (gethash key cache)))
      (progn
        (cl-incf (plist-get supertag-perf--cache-stats :hits))
        cached-value)
    (cl-incf (plist-get supertag-perf--cache-stats :misses))
    nil))

(defun supertag-perf--cache-put (cache key value &optional ttl)
  "Put VALUE in CACHE with KEY and optional TTL."
  ;; Check cache size and evict if necessary
  (when (>= (hash-table-count cache) supertag-perf-cache-size)
    (supertag-perf--cache-evict-lru cache))
  
  ;; Add timestamp for TTL support
  (let ((cache-entry (if ttl
                         (list :value value :timestamp (current-time) :ttl ttl)
                       (list :value value :timestamp (current-time)))))
    (puthash key cache-entry cache)))

(defun supertag-perf--cache-evict-lru (cache)
  "Evict least recently used items from CACHE."
  (let* ((entries '())
         (evict-count (max 1 (/ (hash-table-count cache) 4)))) ; Evict 25%
    
    ;; Collect all entries with timestamps
    (maphash (lambda (key entry)
               (push (cons key (plist-get entry :timestamp)) entries))
             cache)
    
    ;; Sort by timestamp (oldest first)
    (setq entries (sort entries (lambda (a b) (time-less-p (cdr a) (cdr b)))))
    
    ;; Evict oldest entries
    (dotimes (i evict-count)
      (when entries
        (remhash (car (pop entries)) cache)
        (cl-incf (plist-get supertag-perf--cache-stats :evictions))))))

(defun supertag-perf--cache-cleanup-expired ()
  "Remove expired entries from all caches."
  (let ((current-time (current-time)))
    (dolist (cache (list supertag-perf--execution-cache 
                        supertag-perf--query-cache 
                        supertag-perf--relation-cache))
      (let ((expired-keys '()))
        (maphash (lambda (key entry)
                   (when-let ((ttl (plist-get entry :ttl))
                             (timestamp (plist-get entry :timestamp)))
                     (when (time-less-p (time-add timestamp (seconds-to-time ttl)) current-time)
                       (push key expired-keys))))
                 cache)
        (dolist (key expired-keys)
          (remhash key cache))))))

;;; --- Behavior Execution Optimization ---

(defun supertag-perf-optimize-behavior-execution (original-function)
  "Wrap behavior execution with caching and optimization."
  (lambda (behavior node-id context)
    (let* ((behavior-id (plist-get behavior :id))
           (cache-key (supertag-perf--cache-key "behavior" behavior-id node-id))
           (cached-result (supertag-perf--cache-get supertag-perf--execution-cache cache-key)))
      
      (if cached-result
          (plist-get cached-result :value)
        (let ((result (funcall original-function behavior node-id context)))
          ;; Cache result for 30 seconds
          (supertag-perf--cache-put supertag-perf--execution-cache cache-key result 30)
          result)))))

(defun supertag-perf-batch-behavior-execution (behaviors node-ids)
  "Execute BEHAVIORS on multiple NODE-IDS in optimized batches."
  (let ((results '())
        (batch-size supertag-perf-batch-size))
    
    ;; Process nodes in batches
    (cl-loop for batch in (supertag-perf--split-into-batches node-ids batch-size)
             do (let ((batch-results (supertag-perf--execute-behavior-batch behaviors batch)))
                  (setq results (append results batch-results))))
    
    results))

;;; --- Query Optimization ---

(defun supertag-perf-optimize-query (query-function)
  "Wrap query function with caching and optimization."
  (lambda (&rest args)
    (let* ((cache-key (supertag-perf--cache-key "query" args))
           (cached-result (supertag-perf--cache-get supertag-perf--query-cache cache-key)))
      
      (if cached-result
          (plist-get cached-result :value)
        (let ((result (apply query-function args)))
          ;; Cache query results for 60 seconds
          (supertag-perf--cache-put supertag-perf--query-cache cache-key result 60)
          result)))))

(defun supertag-perf-optimize-database-queries ()
  "Apply optimizations to database query functions."
  ;; Wrap frequently used query functions
  (advice-add 'supertag-query-database-records :around 
              (lambda (orig-fun &rest args)
                (apply (supertag-perf-optimize-query orig-fun) args)))
  
  (advice-add 'supertag-query-related :around
              (lambda (orig-fun &rest args)
                (apply (supertag-perf-optimize-query orig-fun) args)))
  
  (message "Database query optimizations applied"))

;;; --- Relation Optimization ---

(defun supertag-perf-optimize-relation-lookups ()
  "Optimize relation lookup operations."
  (advice-add 'supertag-relation-find-by-from :around
              (lambda (orig-fun from-id &optional type)
                (let* ((cache-key (supertag-perf--cache-key "relation-from" from-id type))
                       (cached-result (supertag-perf--cache-get supertag-perf--relation-cache cache-key)))
                  
                  (if cached-result
                      (plist-get cached-result :value)
                    (let ((result (funcall orig-fun from-id type)))
                      (supertag-perf--cache-put supertag-perf--relation-cache cache-key result 120)
                      result)))))
  
  (advice-add 'supertag-relation-find-by-to :around
              (lambda (orig-fun to-id &optional type)
                (let* ((cache-key (supertag-perf--cache-key "relation-to" to-id type))
                       (cached-result (supertag-perf--cache-get supertag-perf--relation-cache cache-key)))
                  
                  (if cached-result
                      (plist-get cached-result :value)
                    (let ((result (funcall orig-fun to-id type)))
                      (supertag-perf--cache-put supertag-perf--relation-cache cache-key result 120)
                      result)))))
  
  (message "Relation lookup optimizations applied"))

;;; --- Event Throttling ---

(defvar supertag-perf--throttled-events (make-hash-table :test 'equal)
  "Hash table tracking throttled events.")

(defun supertag-perf-throttle-event (event-key function &rest args)
  "Throttle EVENT-KEY to execute FUNCTION with ARGS at most once per throttle delay."
  (let ((last-execution (gethash event-key supertag-perf--throttled-events)))
    (when (or (null last-execution)
              (> (float-time (time-subtract (current-time) last-execution))
                 supertag-perf-throttle-delay))
      (puthash event-key (current-time) supertag-perf--throttled-events)
      (apply function args))))

(defun supertag-perf-debounce-event (event-key function delay &rest args)
  "Debounce EVENT-KEY to execute FUNCTION after DELAY seconds of inactivity."
  (let ((timer-key (format "debounce-%s" event-key)))
    ;; Cancel existing timer
    (when-let ((existing-timer (gethash timer-key supertag-perf--throttled-events)))
      (cancel-timer existing-timer))
    
    ;; Set new timer
    (let ((timer (run-with-timer delay nil 
                                (lambda () 
                                  (remhash timer-key supertag-perf--throttled-events)
                                  (apply function args)))))
      (puthash timer-key timer supertag-perf--throttled-events))))

;;; --- Memory Management ---

(defun supertag-perf-cleanup-memory ()
  "Clean up memory used by the behavior system."
  (interactive)
  (let ((freed-items 0))
    
    ;; Clean up expired cache entries
    (supertag-perf--cache-cleanup-expired)
    
    ;; Clear old throttled events
    (let ((current-time (current-time))
          (expired-keys '()))
      (maphash (lambda (key last-time)
                 (when (and (not (timerp last-time)) ; Skip active timers
                           (> (float-time (time-subtract current-time last-time)) 300)) ; 5 minutes
                   (push key expired-keys)))
               supertag-perf--throttled-events)
      (dolist (key expired-keys)
        (remhash key supertag-perf--throttled-events)
        (cl-incf freed-items)))
    
    ;; Run garbage collection
    (garbage-collect)
    
    (message "Memory cleanup completed: %d items freed" freed-items)))

;;; --- Performance Monitoring ---

(defvar supertag-perf--profile-data (make-hash-table :test 'equal)
  "Performance profiling data.")

(defun supertag-perf-profile-function (function-name original-function)
  "Profile FUNCTION-NAME by wrapping ORIGINAL-FUNCTION."
  (lambda (&rest args)
    (if supertag-perf-enable-profiling
        (let ((start-time (current-time)))
          (let ((result (apply original-function args)))
            (let* ((execution-time (float-time (time-subtract (current-time) start-time)))
                   (profile-entry (gethash function-name supertag-perf--profile-data))
                   (call-count (if profile-entry (plist-get profile-entry :calls) 0))
                   (total-time (if profile-entry (plist-get profile-entry :total-time) 0.0))
                   (max-time (if profile-entry (plist-get profile-entry :max-time) 0.0))
                   (min-time (if profile-entry (plist-get profile-entry :min-time) most-positive-fixnum)))
              
              (puthash function-name
                       (list :calls (1+ call-count)
                             :total-time (+ total-time execution-time)
                             :max-time (max max-time execution-time)
                             :min-time (min min-time execution-time)
                             :avg-time (/ (+ total-time execution-time) (1+ call-count)))
                       supertag-perf--profile-data)
              result))
          )
      (apply original-function args))))

(defun supertag-perf-show-profile-report ()
  "Show performance profiling report."
  (interactive)
  (if (zerop (hash-table-count supertag-perf--profile-data))
      (message "No profiling data available. Enable profiling first.")
    (let ((report-buffer (get-buffer-create "*Supertag Performance Report*")))
      (with-current-buffer report-buffer
        (erase-buffer)
        (insert "# Supertag Performance Profile Reportnn")
        (insert (format "Generated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
        
        ;; Cache statistics
        (insert "## Cache Statistics\n\n")
        (insert (format "Cache Hits: %d\n" (plist-get supertag-perf--cache-stats :hits)))
        (insert (format "Cache Misses: %d\n" (plist-get supertag-perf--cache-stats :misses)))
        (insert (format "Cache Evictions: %d\n" (plist-get supertag-perf--cache-stats :evictions)))
        (let ((hit-rate (if (> (+ (plist-get supertag-perf--cache-stats :hits)
                                 (plist-get supertag-perf--cache-stats :misses)) 0)
                            (* 100.0 (/ (float (plist-get supertag-perf--cache-stats :hits))
                                       (+ (plist-get supertag-perf--cache-stats :hits)
                                          (plist-get supertag-perf--cache-stats :misses))))
                          0)))
          (insert (format "Hit Rate: %.1f%%\n\n" hit-rate)))
        
        ;; Function performance
        (insert "## Function Performance\n\n")
        (insert "| Function | Calls | Total Time | Avg Time | Max Time | Min Time |\n")
        (insert "|----------|-------|------------|----------|----------|----------|\n")
        
        (let ((sorted-functions '()))
          (maphash (lambda (func-name data)
                     (push (cons func-name data) sorted-functions))
                   supertag-perf--profile-data)
          (setq sorted-functions 
                (sort sorted-functions 
                      (lambda (a b) (> (plist-get (cdr a) :total-time)
                                      (plist-get (cdr b) :total-time)))))
          
          (dolist (entry sorted-functions)
            (let* ((func-name (car entry))
                   (data (cdr entry))
                   (calls (plist-get data :calls))
                   (total-time (plist-get data :total-time))
                   (avg-time (plist-get data :avg-time))
                   (max-time (plist-get data :max-time))
                   (min-time (plist-get data :min-time)))
              (insert (format "| %s | %d | %.3fs | %.3fs | %.3fs | %.3fs |\n"
                             func-name calls total-time avg-time max-time min-time)))))
        
        (goto-char (point-min))
        (display-buffer report-buffer)))))

(defun supertag-perf-reset-profile-data ()
  "Reset all profiling data."
  (interactive)
  (clrhash supertag-perf--profile-data)
  (setq supertag-perf--cache-stats (list :hits 0 :misses 0 :evictions 0))
  (message "Performance profiling data reset"))

;;; --- System Integration ---

(defun supertag-perf-apply-all-optimizations ()
  "Apply all available performance optimizations."
  (interactive)
  (message "Applying performance optimizations...")
  
  ;; Apply query optimizations
  (supertag-perf-optimize-database-queries)
  
  ;; Apply relation optimizations
  (supertag-perf-optimize-relation-lookups)
  
  ;; Apply behavior execution profiling
  (when supertag-perf-enable-profiling
    (advice-add 'supertag-behavior--execute-single-behavior :around
                (lambda (orig-fun &rest args)
                  (apply (supertag-perf-profile-function "behavior-execution" orig-fun) args))))
  
  ;; Schedule periodic cleanup
  (run-with-timer 300 300 #'supertag-perf-cleanup-memory) ; Every 5 minutes
  
  (message "Performance optimizations applied successfully"))

(defun supertag-perf-remove-all-optimizations ()
  "Remove all performance optimizations."
  (interactive)
  (message "Removing performance optimizations...")
  
  ;; Remove advice
  (advice-remove 'supertag-query-database-records)
  (advice-remove 'supertag-query-related)
  (advice-remove 'supertag-relation-find-by-from)
  (advice-remove 'supertag-relation-find-by-to)
  (advice-remove 'supertag-behavior--execute-single-behavior)
  
  ;; Clear caches
  (clrhash supertag-perf--execution-cache)
  (clrhash supertag-perf--query-cache)
  (clrhash supertag-perf--relation-cache)
  (clrhash supertag-perf--throttled-events)
  
  (message "Performance optimizations removed"))

;;; --- Configuration Interface ---

(defun supertag-perf-configure-optimizations ()
  "Interactive configuration of performance optimizations."
  (interactive)
  (let ((cache-size (read-number "Cache size: " supertag-perf-cache-size))
        (batch-size (read-number "Batch size: " supertag-perf-batch-size))
        (throttle-delay (read-number "Throttle delay (seconds): " supertag-perf-throttle-delay))
        (enable-profiling (y-or-n-p "Enable profiling? ")))
    
    (setq supertag-perf-cache-size cache-size)
    (setq supertag-perf-batch-size batch-size)
    (setq supertag-perf-throttle-delay throttle-delay)
    (setq supertag-perf-enable-profiling enable-profiling)
    
    (message "Performance configuration updated")
    
    ;; Reapply optimizations with new settings
    (supertag-perf-remove-all-optimizations)
    (supertag-perf-apply-all-optimizations)))

;; Auto-apply optimizations when loaded
(supertag-perf-apply-all-optimizations)

(provide 'supertag-performance-optimizer)

;;; supertag-performance-optimizer.el ends here

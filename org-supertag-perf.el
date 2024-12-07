;;; org-supertag-perf.el --- Performance monitoring for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供性能监控和统计功能

;;; Code:

(require 'org-supertag-base)

(defcustom org-supertag-perf-log-file
  (org-supertag-data-file "logs/perf.log")
  "性能日志文件路径."
  :type 'file
  :group 'org-supertag)

(defcustom org-supertag-perf-enabled nil
  "是否启用性能监控."
  :type 'boolean
  :group 'org-supertag)

(defcustom org-supertag-perf-log-threshold 0.1
  "性能日志记录阈值（秒）.
只记录执行时间超过此阈值的操作."
  :type 'float
  :group 'org-supertag)

(defvar org-supertag-perf--stats (make-hash-table :test 'equal)
  "性能统计数据.")

(defun org-supertag-perf-ensure-log-directory ()
  "确保日志目录存在."
  (let ((log-dir (file-name-directory org-supertag-perf-log-file)))
    (unless (file-exists-p log-dir)
      (make-directory log-dir t))))

(defmacro org-supertag-perf-measure (name &rest body)
  "测量代码块的执行时间.
NAME 是操作名称
BODY 是要执行的代码"
  (declare (indent 1))
  `(if org-supertag-perf-enabled
       (let ((start-time (float-time))
             (result (progn ,@body))
             (end-time))
         (setq end-time (float-time))
         (let ((duration (- end-time start-time)))
           (when (>= duration org-supertag-perf-log-threshold)
             (org-supertag-perf--log ,name duration))
           (org-supertag-perf--update-stats ,name duration))
         result)
     (progn ,@body)))

(defun org-supertag-perf--log (name duration)
  "记录性能数据到日志文件.
NAME 是操作名称
DURATION 是执行时间"
  (org-supertag-perf-ensure-log-directory)
  (with-temp-buffer
    (insert (format-time-string "[%Y-%m-%d %H:%M:%S] ")
            (format "%s: %.3fs\n" name duration))
    (append-to-file (point-min) (point-max) org-supertag-perf-log-file)))

(defun org-supertag-perf--update-stats (name duration)
  "更新性能统计数据.
NAME 是操作名称
DURATION 是执行时间"
  (let* ((stats (or (gethash name org-supertag-perf--stats)
                    (puthash name (list :count 0 :total 0.0 :min most-positive-fixnum :max 0.0)
                            org-supertag-perf--stats)))
         (count (plist-get stats :count))
         (total (plist-get stats :total))
         (min (min duration (plist-get stats :min)))
         (max (max duration (plist-get stats :max))))
    (puthash name
             (list :count (1+ count)
                   :total (+ total duration)
                   :min min
                   :max max)
             org-supertag-perf--stats)))

(defun org-supertag-perf-report ()
  "生成性能报告."
  (let ((report-lines '("Performance Report:"))
        (stats-list nil))
    ;; 收集并排序统计数据
    (maphash (lambda (name stats)
               (push (cons name stats) stats-list))
             org-supertag-perf--stats)
    (setq stats-list (sort stats-list
                          (lambda (a b)
                            (> (plist-get (cdr a) :total)
                               (plist-get (cdr b) :total)))))
    ;; 生成报告
    (dolist (stat stats-list)
      (let* ((name (car stat))
             (stats (cdr stat))
             (count (plist-get stats :count))
             (total (plist-get stats :total))
             (min (plist-get stats :min))
             (max (plist-get stats :max))
             (avg (/ total count)))
        (push (format "%-30s count: %5d  total: %7.3fs  avg: %7.3fs  min: %7.3fs  max: %7.3fs"
                     name count total avg min max)
              report-lines)))
    (with-current-buffer (get-buffer-create "*Org-Supertag Performance*")
      (erase-buffer)
      (insert (mapconcat 'identity (nreverse report-lines) "\n"))
      (pop-to-buffer (current-buffer)))))

(defun org-supertag-perf-reset ()
  "重置性能统计数据."
  (clrhash org-supertag-perf--stats))

(provide 'org-supertag-perf)
;;; org-supertag-perf.el ends here 
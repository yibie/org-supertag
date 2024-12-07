;;; org-supertag-test-runner.el --- Command line test runner -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供命令行测试运行支持

;;; Code:

(require 'ert)
(require 'org-supertag-tests)

(defun org-supertag-run-tests-batch ()
  "运行所有测试并输出详细结果."
  (ert-run-tests-batch t))

;; 当作为脚本运行时执行测试
(when noninteractive
  (org-supertag-run-tests-batch))

(provide 'org-supertag-test-runner)
;;; org-supertag-test-runner.el ends here 
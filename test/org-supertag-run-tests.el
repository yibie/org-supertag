;;; org-supertag-run-tests.el --- Test runner for org-supertag -*- lexical-binding: t; -*-

(require 'ert)

;; 设置加载路径
(when load-file-name
  (setq test-path (file-name-directory load-file-name))
  (setq project-path (expand-file-name ".." test-path))
  ;; 添加项目路径到 load-path
  (add-to-list 'load-path project-path)
  (add-to-list 'load-path test-path))

;; 加载被测试的模块（按依赖顺序）
(require 'org-supertag-field)  ;; 基础字段功能
(require 'org-supertag-db)     ;; 数据库功能
(require 'org-supertag-tag)    ;; 标签功能
(require 'org-supertag-node)   ;; 节点功能
(require 'org-supertag-ui)     ;; UI 功能

;;----------------------------------------------------------------------
;; 测试辅助函数
;;----------------------------------------------------------------------

(defun org-supertag-test-db-clear ()
  "仅用于测试：清空数据库中的所有数据。
警告：此函数仅用于测试目的，不应在生产环境中使用。"
  (message "\n=== [TEST] 清空测试数据库 ===")
  ;; 清空所有存储
  (when org-supertag-db--stores
    (maphash (lambda (store-name store)
               (message "清空测试存储: %s" store-name)
               (clrhash store))
             org-supertag-db--stores))
  ;; 重置存储表
  (setq org-supertag-db--stores (make-hash-table :test 'eq))
  ;; 重新初始化数据库
  (org-supertag-db-init)
  (message "测试数据库已清空"))

;; 加载测试辅助工具
(require 'test-helper)

;; 加载测试文件
 ; (require 'org-supertag-tag-test)
(require 'org-supertag-field-test)
;; (require 'org-supertag-integration-test)
(require 'org-supertag-node-test)
(require 'org-supertag-db-test)
;;(require 'org-supertag-ui-test)

;; 运行测试
(defun org-supertag-run-tests ()
  "运行所有 org-supertag 测试。"
  (unwind-protect
      (progn
        ;; 在测试开始前删除可能存在的锁定文件
        (when (file-exists-p "~/.emacs.d/.#org-supertag-templates.org")
          (delete-file "~/.emacs.d/.#org-supertag-templates.org"))
        ;; 添加调试信息
        (message "\n=== 开始运行测试 ===")
        (message "测试路径: %s" test-path)
        (message "项目路径: %s" project-path)
        (message "Load path: %S" load-path)
        ;; 运行测试
        (ert-run-tests-batch-and-exit "^test-org-supertag-"))
    ;; 确保在测试结束后也清理锁定文件
    (when (file-exists-p "~/.emacs.d/.#org-supertag-templates.org")
      (delete-file "~/.emacs.d/.#org-supertag-templates.org"))))

;; 如果是直接运行此文件，则执行测试
(when (and noninteractive
           (string-equal (file-name-nondirectory load-file-name)
                        "org-supertag-run-tests.el"))
  (org-supertag-run-tests))



(provide 'org-supertag-run-tests)
;;; org-supertag-run-tests.el ends here

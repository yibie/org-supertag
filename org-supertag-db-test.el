;;; org-supertag-db-test.el --- Tests for org-supertag-db -*- lexical-binding: t; -*-

;;; Commentary:
;; 测试 org-supertag-db.el 的功能

;;; Code:

(require 'ert)
(require 'org-supertag-db)



;;------------------------------------------------------------------------------
;; 测试辅助函数
;;------------------------------------------------------------------------------

(defmacro org-supertag-db-test-with-temp-buffer (&rest body)
  "在测试文件中执行测试代码."
  (declare (indent 0))
  `(let ((test-file (expand-file-name "test.org")))
     ;; 确保测试文件存在
     (unless (file-exists-p test-file)
       (with-temp-file test-file
         (insert "")))
     
     (unwind-protect
         (with-current-buffer (find-file-noselect test-file)
           (erase-buffer)
           (org-mode)
           (setq-local org-id-track-globally nil)
           (setq-local org-id-locations-file nil)
           ,@body)
       ;; 清理缓冲区内容，但保留文件
       (with-current-buffer (find-file-noselect test-file)
         (erase-buffer)
         (save-buffer)))))

(defmacro org-supertag-db-test-with-temp-db (&rest body)
  "在临时数据库环境中执行测试代码."
  (declare (indent 0))
  `(let ((org-supertag-db--object (ht-create))
         (org-supertag-db--link (ht-create))
         (org-supertag-db--cache (ht-create)))
     (message "初始化空数据库...")
     ,@body))

(defmacro org-supertag-db-test-with-temp-buffer (&rest body)
  "在临时 org buffer 中执行测试代码."
  (declare (indent 0))
  `(with-temp-buffer
     (org-mode)
     ,@body))

(message "\nRequired properties in structure:")
(dolist (struct org-supertag-db-object-structure)
  (when (eq :node (plist-get struct :type))
    (message "Node type required props: %S" 
            (plist-get struct :required))))
            
(ert-deftest org-supertag-db-basic-operations-test ()
  "测试数据库的基本操作：保存、加载、备份."
  (let* ((test-dir (make-temp-file "org-supertag-test-" t))
         (org-supertag-db-file (expand-file-name "test-db.el" test-dir))
         (org-supertag-db-backup-directory (expand-file-name "backup" test-dir))
         (test-data '((:id "test-1" :title "Test Node 1"))
                     (:id "test-2" :title "Test Node 2"))
         (backup-files nil))
    
    (unwind-protect
        (progn
          ;; 1. 初始化测试环境
          (make-directory org-supertag-db-backup-directory t)  ; 先创建备份目录
          (org-supertag-db-ensure-data-directory)
          (should (file-exists-p org-supertag-db-backup-directory))
          
          ;; 2. 添加测试数据
          (setq org-supertag-db--object (ht-create))
          (dolist (node test-data)
            (org-supertag-db-add (plist-get node :id) node))
          (should (= (ht-size org-supertag-db--object) 2))
          
          ;; 3. 保存数据
          (should (org-supertag-db-save))
          (should (file-exists-p org-supertag-db-file))
          
          ;; 4. 验证备份创建
          (should (setq backup-files 
                       (directory-files org-supertag-db-backup-directory
                                      t "\\.el$")))
          (should (= (length backup-files) 1))
          
          ;; 5. 清空内存数据
          (setq org-supertag-db--object (ht-create))
          (should (= (ht-size org-supertag-db--object) 0))
          
          ;; 6. 加载数据
          (should (org-supertag-db-load))
          (should (= (ht-size org-supertag-db--object) 2))
          
          ;; 7. 验证数据完整性
          (dolist (node test-data)
            (let* ((id (plist-get node :id))
                   (loaded-node (org-supertag-db-get id)))
              (should loaded-node)
              (should (equal (plist-get loaded-node :title)
                           (plist-get node :title)))))
          
          ;; 8. 测试备份间隔
          (sleep-for 1)  ; 确保时间戳不同
          (should (org-supertag-db-save))
          (should (= (length (directory-files org-supertag-db-backup-directory 
                                            t "\\.el$"))
                    2))
          
          ;; 9. 测试从备份恢复
          (delete-file org-supertag-db-file)
          (should-not (file-exists-p org-supertag-db-file))
          (should (org-supertag-db-load))
          (should (= (ht-size org-supertag-db--object) 2)))
      
      ;; 清理测试文件
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest org-supertag-db-auto-save-test ()
  "测试数据库自动保存功能."
  (let* ((test-dir (make-temp-file "org-supertag-test-" t))
         (org-supertag-db-file (expand-file-name "test-db.el" test-dir))
         (org-supertag-db-auto-save-interval 1))
    
    (unwind-protect
        (progn
          ;; 1. 初始化
          (org-supertag-db-init)
          (should org-supertag-db--auto-save-timer)
          
          ;; 2. 添加测试数据
          (org-supertag-db-add "test-1" '(:id "test-1" :title "Test"))
          
          ;; 3. 等待自动保存
          (sleep-for 2)
          (should (file-exists-p org-supertag-db-file))
          
          ;; 4. 验证保存的数据
          (let ((saved-data (org-supertag-db-get "test-1")))
            (should saved-data)
            (should (equal (plist-get saved-data :title) "Test"))))
      
      ;; 清理
      (when org-supertag-db--auto-save-timer
        (cancel-timer org-supertag-db--auto-save-timer))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest org-supertag-db-auto-save-test ()
  "测试数据库自动保存功能."
  (let* ((test-dir (make-temp-file "org-supertag-test-" t))
         (org-supertag-db-file (expand-file-name "test-db.el" test-dir))
         (org-supertag-db-auto-save-interval 1))
    
    (unwind-protect
        (progn
          ;; 1. 初始化
          (org-supertag-db-init)
          (should org-supertag-db--auto-save-timer)
          
          ;; 2. 添加测试数据
          (org-supertag-db-add "test-1" '(:id "test-1" :title "Test"))
          
          ;; 3. 等待自动保存
          (sleep-for 2)
          (should (file-exists-p org-supertag-db-file))
          
          ;; 4. 验证保存的数据
          (let ((saved-data (org-supertag-db-get "test-1")))
            (should saved-data)
            (should (equal (plist-get saved-data :title) "Test"))))
      
      ;; 清理
      (when org-supertag-db--auto-save-timer
        (cancel-timer org-supertag-db--auto-save-timer))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(provide 'org-supertag-db-test)
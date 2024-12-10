;;; org-supertag-tests.el --- Tests for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供完整的测试套件

;;; Code:

(require 'ert)
(require 'org-supertag)
(require 'org-supertag-db)
(require 'org-supertag-api)

;; 测试辅助函数
(defmacro org-supertag-with-test-env (&rest body)
  "在干净的测试环境中执行测试代码."
  `(let ((org-supertag-db--entities (ht-create))
         (org-supertag-db--relations (ht-create))
         (org-supertag-db--field-values (ht-create))
         (temp-file (make-temp-file "org-supertag-test" nil ".org")))
     (unwind-protect
         (with-current-buffer (find-file-noselect temp-file)
           (org-mode)
           (insert "* Test Node\n")
           (goto-char (point-min))
           ,@body)
       (when (file-exists-p temp-file)
         (delete-file temp-file)))))

;; 基础 CRUD 测试
(ert-deftest org-supertag-test-basic-crud ()
  "测试基本的 CRUD 操作."
  (org-supertag-with-test-env
   ;; 测试创建
   (should (org-supertag-create-node "node1" '(:title "Test Node")))
   (should (org-supertag-create-tag "tag1" '(:name "Important")))
   (should (org-supertag-create-field "priority" '(:type :number)))
   
   ;; 测试读取
   (should (org-supertag-db-exists-p "node1"))
   (should (equal (plist-get (org-supertag-db-get "node1") :title) "Test Node"))
   
   ;; 测试更新
   (org-supertag-db-put "node1" '(:type :node :title "Updated Node"))
   (should (equal (plist-get (org-supertag-db-get "node1") :title) "Updated Node"))
   
   ;; 测试删除
   (should (org-supertag-db-remove "node1"))
   (should-not (org-supertag-db-exists-p "node1"))))

;; 标签操作测试
(ert-deftest org-supertag-test-tag-operations ()
  "测试标签的增加、删除、修改操作."
  (org-supertag-with-test-env
   (let ((node-id (org-id-get-create)))
     ;; 1. 测试标签创建和字段定义
     (let* ((tag-name "test-tag")
            (fields '((:name "priority" :type :enum :values ("A" "B" "C"))
                     (:name "deadline" :type :date)
                     (:name "notes" :type :string)
                     (:name "progress" :type :number))))
       (should (org-supertag-create-tag tag-name `(:fields ,fields)))
       (should (equal (org-supertag-get-tag-fields tag-name) fields)))
     
     ;; 2. 测试标签应用
     (should (org-supertag-add-tag node-id "test-tag"))
     (let ((node-tags (org-supertag-get-node-tags node-id)))
       (should (member "test-tag" node-tags)))
     
     ;; 3. 测试标签字段修改
     (let* ((new-fields '((:name "priority" :type :enum :values ("1" "2" "3"))
                         (:name "notes" :type :string)
                         (:name "count" :type :number))))
       ;; 更新标签字段
       (should (org-supertag-update-tag "test-tag" `(:fields ,new-fields)))
       ;; 验证字段更新
       (should (equal (org-supertag-get-tag-fields "test-tag") new-fields)))
     
     ;; 4. 测试标签删除
     (should (org-supertag-remove-tag node-id "test-tag"))
     (let ((node-tags (org-supertag-get-node-tags node-id)))
       (should-not (member "test-tag" node-tags))))))

;; 字段值操作测试
(ert-deftest org-supertag-test-field-operations ()
  "测试字段值的修改和验证."
  (org-supertag-with-test-env
   (let ((node-id (org-id-get-create)))
     ;; 创建节点实体
     (should (org-supertag-create-node node-id '(:type :node)))
     
     ;; 1. 测试字符串字段
     (let ((field-def '(:name "notes" :type :string)))
       ;; 创建字段
       (should (org-supertag-create-field "notes" field-def))
       ;; 验证字段创建
       (should (org-supertag-db-exists-p "notes"))
       ;; 有效值测试
       (message "Debug - Setting field notes for node %s" node-id)
       (should (org-supertag-set-field node-id "notes" "test notes"))
       (should (equal (org-supertag-db-get-field-value "notes" node-id) "test notes")))
     
     ;; 2. 测试数字字段
     (let ((field-def '(:name "count" :type :number)))
       ;; 创建字段
       (should (org-supertag-create-field "count" field-def))
       ;; 验证字段创建
       (should (org-supertag-db-exists-p "count"))
       ;; 有效值测试
       (message "Debug - Setting field count for node %s" node-id)
       (should (org-supertag-set-field node-id "count" "50"))
       (should (equal (org-supertag-db-get-field-value "count" node-id) "50"))
       ;; 类型错误测试
       (should-error (org-supertag-set-field node-id "count" "not-a-number")))
     
     ;; 3. 测试枚举字段
     (let ((field-def '(:name "status" :type :enum :values ("TODO" "DONE"))))
       ;; 创建字段
       (should (org-supertag-create-field "status" field-def))
       ;; 验证字段创建
       (should (org-supertag-db-exists-p "status"))
       ;; 有效值测试
       (message "Debug - Setting field status for node %s" node-id)
       (should (org-supertag-set-field node-id "status" "TODO"))
       (should (equal (org-supertag-db-get-field-value "status" node-id) "TODO"))
       ;; 无效值测试
       (should-error (org-supertag-set-field node-id "status" "INVALID"))))))

(provide 'org-supertag-tests)
;;; org-supertag-tests.el ends here
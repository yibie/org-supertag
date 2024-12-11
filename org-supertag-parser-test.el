;;; org-supertag-parser-test.el --- Tests for org-supertag-parser -*- lexical-binding: t; -*-

;; 设置加载路径
(when load-file-name
  (add-to-list 'load-path (file-name-directory load-file-name)))

;; 加载依赖
(require 'ert)
(require 'org)
(require 'org-element)

;; 直接加载被测试文件的内容，而不是require
(load (expand-file-name "org-supertag-parser.el"
                       (file-name-directory (or load-file-name buffer-file-name))))

;;; 测试辅助函数

(defun org-supertag-test-setup ()
  "设置测试环境"
  (let ((org-buffer (get-buffer-create "*org-supertag-test*")))
    (with-current-buffer org-buffer
      (erase-buffer)
      (org-mode)
      (org-element-cache-reset t)
      ;; 创建一个临时文件并关联到缓冲区
      (let ((temp-file (make-temp-file "org-supertag-test-" nil ".org")))
        (write-file temp-file))
      ;; 设置必要的 org 变量
      (setq-local org-id-locations-file 
                  (expand-file-name ".org-id-locations-test" temporary-file-directory))
      (setq-local org-id-track-globally nil)
      (setq-local org-id-locations nil))
    org-buffer))

(defun org-supertag-test-teardown (buffer)
  "清理测试环境"
  (when buffer
    (with-current-buffer buffer
      (org-element-cache-reset t)
      ;; 删除临时文件
      (when buffer-file-name
        (delete-file buffer-file-name))
      ;; 删除 org-id-locations 文件
      (when (and org-id-locations-file
                 (file-exists-p org-id-locations-file))
        (delete-file org-id-locations-file)))
    (kill-buffer buffer)))

;;; 测试用例

(ert-deftest org-supertag-test-tag-regex ()
  "测试标签正则表达式匹配"
  (should (string-match org-supertag-tag-regex "#basic-tag"))
  (should (string-match org-supertag-tag-regex "#namespace/tag"))
  (should (string-match org-supertag-tag-regex "#group:tag"))
  
  ;; 无效标签
  (should-not (string-match org-supertag-tag-regex "#/invalid"))
  (should-not (string-match org-supertag-tag-regex "#:invalid"))
  (should-not (string-match org-supertag-tag-regex "#123invalid")))

(ert-deftest org-supertag-test-parse-headline-tag ()
  "测试标题标签解析"
  (let* ((headline "测试标题 #tag1 #namespace/tag2 #group:tag3")
         (result (org-supertag-parse-headline-tag headline)))
    
    ;; 检查有效标签
    (let ((valid-tags (plist-get result :valid-tags)))
      (should (= (length valid-tags) 3))
      (should (assoc "tag1" valid-tags))
      (should (assoc "namespace/tag2" valid-tags))
      (should (assoc "group:tag3" valid-tags)))
    
    ;; 检查无效标签（这里应该为空）
    (should-not (plist-get result :invalid-tags))
    
    ;; 检查重复标签（这里应该为空）
    (should-not (plist-get result :duplicates))))

(ert-deftest org-supertag-test-parse-headline-tag-duplicates ()
  "测试重复标签的解析"
  (let* ((headline "测试标题 #tag1 #tag1 #namespace/tag2")
         (result (org-supertag-parse-headline-tag headline)))
    
    ;; 检查有效标签
    (let ((valid-tags (plist-get result :valid-tags)))
      (should (= (length valid-tags) 1))
      (should (assoc "namespace/tag2" valid-tags)))
    
    ;; 检查重复标签
    (let ((duplicates (plist-get result :duplicates)))
      (should (= (length duplicates) 1))
      (should (assoc "tag1" duplicates))
      (should (= (length (cdr (assoc "tag1" duplicates))) 2)))))

(ert-deftest org-supertag-test-parse-node-get-tags ()
  "测试节点标签解析"
  (let ((test-buffer (org-supertag-test-setup)))
    (unwind-protect
        (with-current-buffer test-buffer
          ;; 插入测试数据
          (insert "* 测试标题 #tag1 #namespace/tag2\n")
          (goto-char (point-min))
          (save-buffer)  ; 确保文件被保存
          
          ;; 解析节点
          (let ((result (org-supertag-parse-node-get-tags)))
            ;; 检查节点 ID 是否存在
            (should (plist-get result :node-id))
            
            ;; 检查有效标签
            (let ((valid-tags (plist-get result :valid-tags)))
              (should (= (length valid-tags) 2))
              (should (assoc "tag1" valid-tags))
              (should (assoc "namespace/tag2" valid-tags)))
            
            ;; 检查标签位置是否正确（绝对位置）
            (let* ((tag1-pos (cdr (assoc "tag1" (plist-get result :valid-tags))))
                  (tag2-pos (cdr (assoc "namespace/tag2" (plist-get result :valid-tags)))))
              (should tag1-pos)
              (should tag2-pos))))
      
      ;; 清理测试环境
      (org-supertag-test-teardown test-buffer))))

(ert-deftest org-supertag-test-valid-context ()
  "测试标签上下文有效性检查"
  (let ((test-buffer (org-supertag-test-setup)))
    (unwind-protect
        (with-current-buffer test-buffer
          ;; 测试普通文本中的标签
          (insert "* 标题 #valid-tag\n")
          (goto-char (point-min))
          (search-forward "#")
          (should (org-supertag-valid-context-p (point)))
          
          ;; 测试代码块中的标签
          (erase-buffer)
          (insert "* 标题\n#+BEGIN_SRC elisp\n#invalid-tag\n#+END_SRC\n")
          (goto-char (point-min))
          (search-forward "#")
          (should-not (org-supertag-valid-context-p (point))))
      
      ;; 清理测试环境
      (org-supertag-test-teardown test-buffer))))

(ert-deftest org-supertag-test-parse-references ()
  "测试引用解析"
  (interactive)
  (let* ((text "这个任务分配给 @person.john，优先级是 #task.priority=high")
         (result (org-supertag-parse-content 
                 (with-temp-buffer
                   (insert text)
                   (point-min))
                 (with-temp-buffer
                   (insert text)
                   (point-max)))))
    
    ;; 检查引用
    (let ((refs (cl-remove-if-not 
                (lambda (item) (eq (plist-get item :type) 'reference))
                result)))
      (should (= (length refs) 1))
      (let ((ref (car refs)))
        (should (equal (plist-get ref :entity-type) "person"))
        (should (equal (plist-get ref :entity-id) "john"))))
    
    ;; 检查标签
    (let ((tags (cl-remove-if-not 
                (lambda (item) (eq (plist-get item :type) 'tag))
                result)))
      (should (= (length tags) 1))
      (let ((tag (car tags)))
        (should (equal (plist-get tag :name) "task"))
        (should (equal (plist-get tag :field) "priority"))
        (should (equal (plist-get tag :value) "high"))))))

;;; 运行测试的便捷函数
(defun org-supertag-run-parser-tests ()
  "运行所有 org-supertag-parser 测试"
  (interactive)
  (ert-run-tests-interactively "^org-supertag-test-"))

(provide 'org-supertag-parser-test)

;;; org-supertag-parser-test.el ends here
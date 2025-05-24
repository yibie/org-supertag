;;; recover-tag-relations-fixed.el --- 修复版标签关系恢复 -*- lexical-binding: t; -*-

;; 添加当前目录到load-path
(add-to-list 'load-path ".")
(add-to-list 'load-path "./lisp")

;; 加载必要的库
(require 'org-supertag-db)
(require 'org-supertag-relation)

(defun org-supertag-extract-cooccurrence-from-metadata ()
  "从metadata中提取已有的共现关系数据。"
  (interactive)
  (message "=== 从metadata提取共现关系数据 ===")
  
  (let ((metadata (org-supertag-db-get "metadata"))
        (cooccurrence-pairs '())
        (metadata-hash nil))
    
    (if (not metadata)
        (progn
          (message "未找到metadata记录")
          nil)
      
      ;; 获取metadata中的数据哈希表
      (setq metadata-hash (plist-get metadata :data))
      
      (cond
       ((not metadata-hash)
        (message "metadata中没有数据哈希表")
        nil)
       
       ((not (hash-table-p metadata-hash))
        (message "metadata :data 不是哈希表类型: %S" (type-of metadata-hash))
        nil)
       
       (t
        (let ((found-pairs 0))
          
          (message "metadata哈希表大小: %d" (hash-table-count metadata-hash))
          (message "")
          
          ;; 使用简单的错误处理
          (ignore-errors
            (maphash (lambda (key value)
                      (when (and (symbolp key) (stringp (symbol-name key)))
                        (let ((key-str (symbol-name key)))
                          ;; 寻找 tag-cooccur: 开头的键
                          (when (string-prefix-p "tag-cooccur:" key-str)
                            (let* ((pair-part (substring key-str (length "tag-cooccur:")))
                                   (tags (split-string pair-part ":"))
                                   (tag1 (car tags))
                                   (tag2 (cadr tags))
                                   (count value))
                              
                              (when (and tag1 tag2 (numberp count) (> count 0))
                                ;; 获取对应的PMI值
                                (let* ((pmi-key (intern (format "tag-pmi:%s:%s" tag1 tag2)))
                                       (pmi-value (gethash pmi-key metadata-hash 0))
                                       ;; 计算强度
                                       (strength (if (> pmi-value 0)
                                                    (max 0.1 (min 1.0 (/ pmi-value 6.0)))
                                                  (max 0.1 (min 1.0 (/ count 10.0))))))
                                  
                                  ;; 只保留tag1 < tag2的对，避免重复
                                  (when (string< tag1 tag2)
                                    (push (list :tag1 tag1 
                                              :tag2 tag2 
                                              :count count 
                                              :pmi pmi-value
                                              :strength strength
                                              :source "metadata")
                                          cooccurrence-pairs)
                                    (setq found-pairs (1+ found-pairs))))))))))
                    metadata-hash))
          
          (message "从metadata中提取到 %d 个共现关系对" found-pairs)
          
          ;; 按强度排序并显示
          (when cooccurrence-pairs
            (setq cooccurrence-pairs (sort cooccurrence-pairs 
                                          (lambda (a b) (> (plist-get a :strength) (plist-get b :strength)))))
            
            ;; 显示前10个高强度关系
            (message "")
            (message "=== 前10个高强度共现关系 ===")
            (dolist (pair (seq-take cooccurrence-pairs 10))
              (message "  %s <-> %s: 次数%d, PMI%.3f, 强度%.3f (%s)"
                      (plist-get pair :tag1)
                      (plist-get pair :tag2)  
                      (plist-get pair :count)
                      (plist-get pair :pmi)
                      (plist-get pair :strength)
                      (plist-get pair :source))))
          
          cooccurrence-pairs))))))

(defun org-supertag-recover-cooccurrence-relations (cooccurrence-pairs &optional min-strength)
  "基于共现分析恢复cooccurrence关系。"
  (let ((min-strength (or min-strength 0.3))
        (recovered-count 0))
    
    (message "=== 恢复共现关系 (最小强度: %.2f) ===" min-strength)
    
    (dolist (pair cooccurrence-pairs)
      (let ((tag1 (plist-get pair :tag1))
            (tag2 (plist-get pair :tag2))
            (strength (plist-get pair :strength)))
        
        (when (>= strength min-strength)
          ;; 获取标签ID
          (let ((tag1-id (org-supertag-tag-get-id-by-name tag1))
                (tag2-id (org-supertag-tag-get-id-by-name tag2)))
            
            (when (and tag1-id tag2-id)
              ;; 检查关系是否已存在
              (unless (org-supertag-relation-get tag1-id tag2-id 'cooccurrence)
                ;; 添加双向共现关系
                (org-supertag-relation-add-relation tag1-id tag2-id 'cooccurrence strength)
                (org-supertag-relation-add-relation tag2-id tag1-id 'cooccurrence strength)
                (setq recovered-count (+ recovered-count 2))
                (message "  添加共现关系: %s <-> %s (强度: %.3f)" tag1 tag2 strength)))))))
    
    (when (> recovered-count 0)
      (message "")
      (message "恢复了 %d 个共现关系" recovered-count))
    
    recovered-count))

(defun org-supertag-relation-recovery-preview ()
  "预览可恢复的标签关系，不做实际更改。"
  (interactive)
  (message "=== 标签关系恢复预览 ===")
  (message "")
  
  ;; 从metadata提取共现数据
  (let ((cooccurrence-pairs (org-supertag-extract-cooccurrence-from-metadata)))
    
    ;; 统计摘要
    (message "")
    (message "=== 恢复统计摘要 ===")
    (if cooccurrence-pairs
        (progn
          (let ((high-strength-pairs (cl-remove-if (lambda (p) (< (plist-get p :strength) 0.5)) cooccurrence-pairs))
                (medium-strength-pairs (cl-remove-if (lambda (p) (or (< (plist-get p :strength) 0.3) 
                                                                     (>= (plist-get p :strength) 0.5))) cooccurrence-pairs)))
            
            (message "总共现关系: %d个" (length cooccurrence-pairs))
            (message "高强度关系 (>0.5): %d个" (length high-strength-pairs))
            (message "中等强度关系 (0.3-0.5): %d个" (length medium-strength-pairs))
            
            ;; 显示高质量关系预览
            (when high-strength-pairs
              (message "")
              (message "=== 高强度共现关系预览 ===")
              (dolist (pair (seq-take high-strength-pairs 5))
                (message "  %s <-> %s (强度: %.3f, 次数: %d)"
                        (plist-get pair :tag1)
                        (plist-get pair :tag2)
                        (plist-get pair :strength)
                        (plist-get pair :count))))))
      
      (message "未发现可恢复的共现关系"))))

(defun org-supertag-relation-recovery-interactive ()
  "交互式标签关系恢复流程。"
  (interactive)
  (message "=== 标签关系恢复工具 ===")
  (message "")
  
  ;; 从metadata提取共现数据
  (let ((cooccurrence-pairs (org-supertag-extract-cooccurrence-from-metadata)))
    
    (if cooccurrence-pairs
        (progn
          (message "")
          ;; 询问是否恢复关系
          (when (yes-or-no-p "是否恢复共现关系？")
            (let ((min-strength (read-number "最小强度 (0.1-1.0): " 0.3)))
              (org-supertag-recover-cooccurrence-relations cooccurrence-pairs min-strength)
              (message "")))
          
          ;; 保存数据库
          (when (yes-or-no-p "是否保存数据库？")
            (org-supertag-db-save)
            (message "数据库已保存"))
          
          (message "")
          (message "=== 标签关系恢复完成 ==="))
      
      (message "未找到可恢复的关系数据"))))

;; 提供选择菜单
(defun org-supertag-relation-recovery-menu ()
  "标签关系恢复菜单。"
  (interactive)
  (let ((choice (read-char-choice 
                 "标签关系恢复选项:
1. 预览可恢复的关系
2. 执行关系恢复  
3. 取消
请选择 (1-3): " 
                 '(?1 ?2 ?3))))
    (cond
     ((eq choice ?1) (org-supertag-relation-recovery-preview))
     ((eq choice ?2) (org-supertag-relation-recovery-interactive))
     ((eq choice ?3) (message "已取消"))
     (t (message "无效选择")))))

;; 提供使用说明
(message "已加载修复版标签关系恢复脚本")
(message "使用方法: M-x org-supertag-relation-recovery-menu")

(provide 'recover-tag-relations-fixed) 

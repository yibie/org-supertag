;;; org-supertag-sim-advanced.el --- Advanced EPC operations for org-supertag

;; Copyright (C) 2023 

;; Author: 
;; Keywords: org-mode, nlp, epc

;; This file is not part of GNU Emacs.

;;; Commentary:
;; 
;; This module provides advanced operations for org-supertag using the EPC
;; communication mechanism defined in org-supertag-sim-epc.el.
;;
;; The main functions are:
;; - Interactive entity extraction from text
;; - Batch processing of entities from multiple files
;; - Interactive tag suggestion helpers
;; - Similarity-based tag navigation
;;

;;; Code:

(require 'org-supertag-sim-epc)
(require 'org-supertag-db)
(require 'org)
(require 'cl-lib)

(defgroup org-supertag-sim-advanced nil
  "Advanced NLP features for Org Supertag."
  :group 'org-supertag)

(defcustom org-supertag-sim-suggest-threshold 0.6
  "Threshold for tag similarity suggestions (0.0-1.0).
Higher values require greater similarity."
  :type 'float
  :group 'org-supertag-sim-advanced)

(defcustom org-supertag-sim-max-suggestions 5
  "Maximum number of tag suggestions to display."
  :type 'integer
  :group 'org-supertag-sim-advanced)

(defcustom org-supertag-sim-batch-size 10
  "Number of files to process in each batch for batch operations."
  :type 'integer
  :group 'org-supertag-sim-advanced)

(defcustom org-supertag-sim-disable-live-suggestions nil
  "When non-nil, disable real-time tag suggestions."
  :type 'boolean
  :group 'org-supertag-sim-advanced)

;;;###autoload
(defun org-supertag-sim-extract-from-region (begin end)
  "从选定区域提取实体并提供标签建议.
BEGIN 和 END 是区域的起始和结束位置."
  (interactive "r")
  (unless (region-active-p)
    (user-error "未选择区域"))
  
  (let ((text (buffer-substring-no-properties begin end)))
    (message "正在分析选定区域...")
    
    ;; 使用EPC异步处理
    (org-supertag-sim-epc-extract-entities-async 
     text
     (lambda (entities)
       ;; 实体提取后的回调
       (when entities
         (let ((buf (get-buffer-create "*org-supertag-entities*"))
               (entity-count (length entities)))
           (with-current-buffer buf
             (erase-buffer)
             (insert (format "从选定文本中提取了 %d 个实体:\n\n" entity-count))
             
             ;; 显示提取到的实体
             (dolist (entity entities)
               (let ((entity-text (cdr (assoc "entity" entity)))
                     (entity-type (cdr (assoc "type" entity))))
                 (insert (format "- %s (%s)\n" entity-text entity-type))))
             
             ;; 如果有实体，提供标签建议
             (when entities
               (insert "\n正在基于提取到的实体生成标签建议...\n")
               
               ;; 提取实体文本
               (let ((entity-texts (mapcar (lambda (e) (cdr (assoc "entity" e))) entities)))
                 ;; 连接所有实体文本
                 (let ((combined-text (mapconcat 'identity entity-texts " ")))
                   ;; 异步获取标签建议
                   (org-supertag-sim-epc-get-tag-suggestions-async 
                    combined-text
                    org-supertag-sim-max-suggestions
                    (lambda (suggestions)
                      ;; 标签建议完成后的回调
                      (with-current-buffer buf
                        (goto-char (point-max))
                        (insert "\n标签建议:\n\n")
                        
                        ;; 显示建议的标签
                        (if suggestions
                            (dolist (suggestion suggestions)
                              (let ((tag-name (cdr (assoc "tag" suggestion)))
                                    (score (cdr (assoc "score" suggestion))))
                                (insert (format "- %s (%.2f)\n" tag-name score))))
                          (insert "没有找到相关标签建议\n"))
                        
                        ;; 添加应用建议的按钮
                        (insert "\n")
                        (dolist (suggestion suggestions)
                          (let ((tag-name (cdr (assoc "tag" suggestion))))
                            (insert-button (format "[应用标签: %s] " tag-name)
                                          'action `(lambda (_)
                                                    (org-supertag-sim-advanced--apply-tag ,tag-name))
                                          'follow-link t
                                          'help-echo (format "将标签 '%s' 应用到当前条目" tag-name))
                          (insert " ")))
                        
                        ;; 添加创建所有标签的按钮
                        (when suggestions
                          (insert "\n\n")
                          (insert-button "[应用所有建议标签]"
                                        'action `(lambda (_)
                                                  (org-supertag-sim-advanced--apply-all-tags ',suggestions))
                                        'follow-link t
                                        'help-echo "将所有建议的标签应用到当前条目"))))))))))
           
           ;; 显示结果缓冲区
           (switch-to-buffer-other-window buf)))))))

(defun org-supertag-sim-advanced--apply-tag (tag-name)
  "将标签应用到当前Org条目.
TAG-NAME 是要应用的标签名称."
  (when (and tag-name (org-supertag-db-find-by-name tag-name :tag))
    ;; 如果标签存在，直接应用
    (message "应用标签：%s" tag-name)
    (org-back-to-heading t)
    (let ((tag-id (org-supertag-db-find-by-name tag-name :tag)))
      (when tag-id
        (org-supertag-add `(:tag ,tag-id)))))
  
  ;; 如果标签不存在，提示是否创建
  (when (and tag-name (not (org-supertag-db-find-by-name tag-name :tag)))
    (when (yes-or-no-p (format "标签 '%s' 不存在，是否创建? " tag-name))
      (let ((tag-id (org-supertag-upsert `(:name ,tag-name :type :tag))))
        (when tag-id
          (message "创建并应用标签：%s" tag-name)
          (org-back-to-heading t)
          (org-supertag-add `(:tag ,tag-id)))))))

(defun org-supertag-sim-advanced--apply-all-tags (suggestions)
  "将所有建议的标签应用到当前Org条目.
SUGGESTIONS 是标签建议列表."
  (org-back-to-heading t)
  (let ((applied-count 0))
    (dolist (suggestion suggestions)
      (let* ((tag-name (cdr (assoc "tag" suggestion)))
             (tag-id (org-supertag-db-find-by-name tag-name :tag)))
        
        ;; 如果标签不存在，创建它
        (unless tag-id
          (setq tag-id (org-supertag-upsert `(:name ,tag-name :type :tag))))
        
        ;; 应用标签
        (when tag-id
          (org-supertag-add `(:tag ,tag-id))
          (cl-incf applied-count))))
    
    (message "已应用 %d 个标签" applied-count)))

;;;###autoload
(defun org-supertag-sim-suggest-for-buffer ()
  "为当前缓冲区内容生成标签建议."
  (interactive)
  (save-excursion
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (message "正在分析缓冲区内容...")
      
      ;; 异步获取标签建议
      (org-supertag-sim-epc-get-tag-suggestions-async 
       text
       org-supertag-sim-max-suggestions
       (lambda (suggestions)
         ;; 创建交互式选择界面
         (if suggestions
             (let* ((choices (mapcar (lambda (s) 
                                      (cons (format "%s (%.2f)" 
                                                    (cdr (assoc "tag" s)) 
                                                    (cdr (assoc "score" s)))
                                            s))
                                    suggestions))
                    (selection (completing-read "选择要应用的标签: " choices nil t)))
               (when selection
                 (let* ((selected-suggestion (cdr (assoc selection choices)))
                        (tag-name (cdr (assoc "tag" selected-suggestion))))
                   (org-supertag-sim-advanced--apply-tag tag-name))))
           (message "没有找到相关标签建议")))))))

;;;###autoload
(defun org-supertag-sim-extract-from-current-heading ()
  "从当前Org标题提取实体并提供标签建议."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((heading-element (org-element-at-point))
           (heading-text (org-element-property :title heading-element))
           (section-end (or (org-element-property :contents-end heading-element)
                            (save-excursion 
                              (org-end-of-subtree t) 
                              (point))))
           (content-start (org-element-property :contents-begin heading-element))
           (content (when content-start
                      (buffer-substring-no-properties content-start section-end)))
           (full-text (concat heading-text " " (or content ""))))
      
      (message "正在分析当前标题内容...")
      
      ;; 使用EPC异步处理
      (org-supertag-sim-epc-extract-entities-async 
       full-text
       (lambda (entities)
         ;; 实体提取后的回调
         (when entities
           (let ((entity-texts (mapcar (lambda (e) (cdr (assoc "entity" e))) entities)))
             ;; 连接所有实体文本
             (let ((combined-text (mapconcat 'identity entity-texts " ")))
               ;; 异步获取标签建议
               (org-supertag-sim-epc-get-tag-suggestions-async 
                combined-text
                org-supertag-sim-max-suggestions
                (lambda (suggestions)
                  ;; 创建交互式选择界面
                  (if suggestions
                      (let* ((choices (mapcar (lambda (s) 
                                               (cons (format "%s (%.2f)" 
                                                             (cdr (assoc "tag" s)) 
                                                             (cdr (assoc "score" s)))
                                                     s))
                                             suggestions))
                             (selection (completing-read "选择要应用的标签: " choices nil t)))
                        (when selection
                          (let* ((selected-suggestion (cdr (assoc selection choices)))
                                 (tag-name (cdr (assoc "tag" selected-suggestion))))
                            (org-supertag-sim-advanced--apply-tag tag-name))))
                    (message "没有找到相关标签建议"))))))))))))

;;;###autoload
(defun org-supertag-sim-find-similar-to-tag (tag-id)
  "查找与指定标签相似的标签并提供导航选项.
TAG-ID 是要查找相似标签的标签ID."
  (interactive (list (org-supertag-sim-advanced--select-tag)))
  
  (when tag-id
    (let* ((tag-props (org-supertag-db-get tag-id))
           (tag-name (plist-get tag-props :name)))
      (message "查找与 '%s' 相似的标签..." tag-name)
      
      ;; 异步查找相似标签
      (org-supertag-sim-epc-find-similar-async
       tag-name
       org-supertag-sim-max-suggestions
       (lambda (similar-tags)
         (if similar-tags
             (let* ((choices (mapcar (lambda (s)
                                      (cons (format "%s (%.2f)" 
                                                    (cdr (assoc "tag" s)) 
                                                    (cdr (assoc "score" s)))
                                            s))
                                    similar-tags))
                    (selection (completing-read "选择要跳转的相似标签: " choices nil t)))
               (when selection
                 (let* ((selected-tag (cdr (assoc selection choices)))
                        (tag-name (cdr (assoc "tag" selected-tag)))
                        (tag-id (org-supertag-db-find-by-name tag-name :tag)))
                   (when tag-id
                     (org-supertag-find tag-id)))))
           (message "没有找到与 '%s' 相似的标签" tag-name)))))))

(defun org-supertag-sim-advanced--select-tag ()
  "选择一个标签并返回其ID."
  (let* ((tags (org-supertag-db-find-by-type :tag))
         (tag-names (mapcar (lambda (id)
                             (let ((props (org-supertag-db-get id)))
                               (cons (plist-get props :name) id)))
                           tags))
         (selection (completing-read "选择标签: " tag-names nil t)))
    (cdr (assoc selection tag-names))))

;;;###autoload
(defun org-supertag-sim-batch-process-org-files (org-files)
  "批量处理多个Org文件，提取实体并为每个条目生成标签建议.
ORG-FILES 是Org文件路径列表."
  (interactive (list (directory-files-recursively 
                      (read-directory-name "选择Org文件目录: ") 
                      "\\.org$")))
  
  (when org-files
    (let ((total-files (length org-files))
          (processed-files 0)
          (report-buffer (get-buffer-create "*SimTag批量处理*"))
          (batch-timer nil))
      
      ;; 初始化报告缓冲区
      (with-current-buffer report-buffer
        (erase-buffer)
        (insert "SimTag批量处理报告\n")
        (insert "===================\n\n")
        (insert (format "总文件数: %d\n\n" total-files))
        (insert "处理中...\n\n"))
      
      ;; 显示报告缓冲区
      (display-buffer report-buffer)
      
      ;; 定义批量处理函数
      (cl-labels 
          ((process-batch 
            (remaining-files)
            (if (null remaining-files)
                ;; 所有文件处理完成
                (with-current-buffer report-buffer
                  (goto-char (point-max))
                  (insert "\n处理完成!\n")
                  (insert (format "处理了 %d 个文件\n" processed-files)))
              
              ;; 取当前批次的文件
              (let* ((current-batch (cl-subseq remaining-files 0 
                                               (min org-supertag-sim-batch-size 
                                                    (length remaining-files))))
                     (next-batch (cl-subseq remaining-files 
                                            (min org-supertag-sim-batch-size 
                                                 (length remaining-files)))))
                
                ;; 处理当前批次的文件
                (process-file (car current-batch) 
                              (cdr current-batch) 
                              next-batch)))))
        
        ;; 定义单个文件处理函数
        (process-file 
         (file remaining-files-in-batch next-batch)
         (if (null file)
             ;; 当前批次处理完成，继续下一批次
             (setq batch-timer 
                   (run-with-timer 0.5 nil #'process-batch next-batch))
           
           ;; 处理单个文件
           (with-current-buffer report-buffer
             (goto-char (point-max))
             (insert (format "正在处理文件: %s\n" file)))
           
           (with-temp-buffer
             (insert-file-contents file)
             (org-mode)
             (setq processed-files (1+ processed-files))
             
             ;; 处理文件中的每个标题
             (org-map-entries
              (lambda ()
                (let* ((heading-element (org-element-at-point))
                       (heading-text (org-element-property :title heading-element))
                       (section-end (or (org-element-property :contents-end heading-element)
                                        (save-excursion 
                                          (org-end-of-subtree t) 
                                          (point))))
                       (content-start (org-element-property :contents-begin heading-element))
                       (content (when content-start
                                  (buffer-substring-no-properties content-start section-end)))
                       (full-text (concat heading-text " " (or content ""))))
                  
                  ;; 记录处理的标题
                  (with-current-buffer report-buffer
                    (goto-char (point-max))
                    (insert (format "  - 标题: %s\n" heading-text)))
                  
                  ;; 异步提取实体
                  (org-supertag-sim-epc-extract-entities-async 
                   full-text
                   (lambda (entities)
                     (when entities
                       ;; 记录提取到的实体
                       (with-current-buffer report-buffer
                         (goto-char (point-max))
                         (insert (format "    - 提取到 %d 个实体\n" (length entities))))
                       
                       ;; 组合实体文本并获取标签建议
                       (let ((entity-texts (mapcar (lambda (e) 
                                                    (cdr (assoc "entity" e))) 
                                                  entities)))
                         (when entity-texts
                           (let ((combined-text (mapconcat 'identity entity-texts " ")))
                             ;; 异步获取标签建议
                             (org-supertag-sim-epc-get-tag-suggestions-async 
                              combined-text
                              org-supertag-sim-max-suggestions
                              (lambda (suggestions)
                                (when suggestions
                                  ;; 记录标签建议
                                  (with-current-buffer report-buffer
                                    (goto-char (point-max))
                                    (insert (format "    - 生成了 %d 个标签建议\n" 
                                                    (length suggestions)))
                                    (dolist (suggestion suggestions)
                                      (insert (format "      * %s (%.2f)\n" 
                                                      (cdr (assoc "tag" suggestion))
                                                      (cdr (assoc "score" suggestion)))))))))))))))))
             
             ;; 处理完当前文件，继续处理批次中的下一个文件
             (process-file (car remaining-files-in-batch)
                           (cdr remaining-files-in-batch)
                           next-batch))))
        
        ;; 开始处理第一批文件
        (process-batch org-files)))))

;; 提供自动标签建议的钩子函数
(defvar org-supertag-sim-suggestion-timer nil
  "用于延迟标签建议的计时器.")

(defvar org-supertag-sim-last-content ""
  "上次分析的内容，用于避免重复分析.")

(defun org-supertag-sim-advanced--suggest-on-idle ()
  "在空闲时为当前标题提供标签建议."
  (unless org-supertag-sim-disable-live-suggestions
    (when (and (eq major-mode 'org-mode)
               (not (minibufferp)))
      ;; 取消之前的计时器
      (when org-supertag-sim-suggestion-timer
        (cancel-timer org-supertag-sim-suggestion-timer))
      
      ;; 设置新的计时器
      (setq org-supertag-sim-suggestion-timer
            (run-with-idle-timer 
             2 nil
             (lambda ()
               (when (and (eq major-mode 'org-mode)
                          (not (minibufferp)))
                 (save-excursion
                   (when (org-at-heading-p)
                     (let* ((heading-element (org-element-at-point))
                            (heading-text (org-element-property :title heading-element))
                            (full-text heading-text))
                       
                       ;; 只在内容变化时触发分析
                       (unless (string= full-text org-supertag-sim-last-content)
                         (setq org-supertag-sim-last-content full-text)
                         
                         ;; 异步获取标签建议
                         (org-supertag-sim-epc-get-tag-suggestions-async 
                          full-text
                          3  ;; 减少实时建议的数量
                          (lambda (suggestions)
                            (when (and suggestions 
                                       (> (length suggestions) 0))
                              (message "标签建议: %s" 
                                       (mapconcat 
                                        (lambda (s) 
                                          (format "%s" (cdr (assoc "tag" s))))
                                        suggestions ", ")))))))))))))))

;; 添加到光标移动钩子
(add-hook 'post-command-hook 'org-supertag-sim-advanced--suggest-on-idle)

;;;###autoload
(defun org-supertag-sim-toggle-live-suggestions ()
  "切换实时标签建议功能."
  (interactive)
  (setq org-supertag-sim-disable-live-suggestions 
        (not org-supertag-sim-disable-live-suggestions))
  (message "实时标签建议已%s" 
           (if org-supertag-sim-disable-live-suggestions "禁用" "启用")))

(provide 'org-supertag-sim-advanced)
;;; org-supertag-sim-advanced.el ends here 
;;; org-supertag-intelligent-graphrag-ui.el --- UI components for intelligent GraphRAG -*- lexical-binding: t; -*-

;;; Commentary:
;; 智能GraphRAG系统的用户界面组件
;; 包含：智能建议面板、资源展示、交互式操作等

;;; Code:

(require 'org-supertag-intelligent-graphrag)

;;; =================================================================
;;; 智能建议面板
;;; =================================================================

(defvar org-supertag-igr-suggestions-buffer "*智能GraphRAG建议*"
  "智能建议面板缓冲区名称")

(defun org-supertag-igr-display-smart-suggestions-panel (node-id similar-nodes)
  "显示智能建议面板"
  (let ((buffer (get-buffer-create org-supertag-igr-suggestions-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        
        ;; 标题
        (let* ((node (org-supertag-db-get node-id))
               (title (plist-get node :title)))
          (insert (propertize "🧠 智能GraphRAG建议\n" 
                             'face '(:height 1.4 :weight bold)))
          (insert (propertize (format "当前节点: %s\n\n" title)
                             'face '(:height 1.1 :foreground "blue"))))
        
        ;; 相似节点部分
        (when similar-nodes
          (org-supertag-igr-insert-similar-nodes-section similar-nodes))
        
        ;; 操作说明
        (insert "\n")
        (insert (propertize "操作说明:\n" 'face '(:weight bold)))
        (insert "  [RET] - 打开节点    [TAB] - 循环选择    [q] - 关闭面板\n")
        (insert "  [r] - 刷新建议    [c] - 清空缓存    [s] - 保存建议\n")
        
        (goto-char (point-min))
        (setq buffer-read-only t)))
    
    ;; 显示面板
    (let ((window (display-buffer-in-side-window 
                   buffer '((side . right) (window-width . 0.4)))))
      (select-window window))))

(defun org-supertag-igr-insert-similar-nodes-section (similar-nodes)
  "插入相似节点部分"
  (insert (propertize "🔗 相似节点 " 'face '(:weight bold :height 1.2)))
  (insert (propertize (format "(%d个)\n" (length similar-nodes))
                     'face '(:foreground "gray60")))
  (insert "────────────────────────────────────────\n")
  
  (dolist (item similar-nodes)
    (let* ((other-node-id (car item))
           (score (cdr item))
           (node (org-supertag-db-get other-node-id))
           (title (plist-get node :title))
           (file-path (plist-get node :file-path))
           (file-name (file-name-nondirectory file-path)))
      
      ;; 节点链接
      (insert "  ")
      (insert-text-button 
       (format "📄 %s" title)
       'action `(lambda (_) (org-supertag-igr-open-node ,other-node-id))
       'follow-link t
       'help-echo (format "打开节点: %s" title))
      
      ;; 相似度分数
      (insert (propertize (format " (%.2f)" score)
                         'face '(:foreground "green")))
      
      ;; 文件信息
      (insert (propertize (format " - %s" file-name)
                         'face '(:foreground "gray50")))
      
      ;; 标签信息
      (let ((tags (mapcar #'org-supertag-tag-get-name-by-id 
                         (org-supertag-node-get-tags other-node-id))))
        (when tags
          (insert (propertize (format " [%s]" (string-join tags ", "))
                             'face '(:foreground "blue")))))
      
      (insert "\n")))
  
  (insert "\n"))

;;; =================================================================
;;; 交互操作
;;; =================================================================

(defun org-supertag-igr-open-node (node-id)
  "打开指定节点"
  (when-let* ((node (org-supertag-db-get node-id))
              (file-path (plist-get node :file-path))
              (pos (plist-get node :pos)))
    (find-file file-path)
    (goto-char pos)
    (org-show-context)))

;;; =================================================================
;;; 键盘绑定和模式
;;; =================================================================

(defvar org-supertag-igr-suggestions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-supertag-igr-open-at-point)
    (define-key map (kbd "TAB") #'org-supertag-igr-next-suggestion)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "r") #'org-supertag-igr-refresh-suggestions)
    (define-key map (kbd "c") #'org-supertag-igr-clear-caches)
    (define-key map (kbd "s") #'org-supertag-igr-save-suggestions)
    map)
  "智能建议面板的键盘映射")

(define-derived-mode org-supertag-igr-suggestions-mode special-mode 
  "IGR-Suggestions"
  "智能GraphRAG建议面板模式"
  :group 'org-supertag-intelligent-graphrag)

(defun org-supertag-igr-open-at-point ()
  "打开光标处的节点"
  (interactive)
  (let ((button (button-at (point))))
    (when button
      (button-activate button))))

(defun org-supertag-igr-next-suggestion ()
  "跳转到下一个建议项"
  (interactive)
  (forward-button 1))

(defun org-supertag-igr-refresh-suggestions ()
  "刷新建议内容"
  (interactive)
  (message "刷新智能建议..."))

(defun org-supertag-igr-save-suggestions ()
  "保存当前建议到文件"
  (interactive)
  (message "保存建议功能待实现"))

(provide 'org-supertag-intelligent-graphrag-ui)

;;; org-supertag-intelligent-graphrag-ui.el ends here

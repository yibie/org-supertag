;;; org-supertag-auto-tag.el --- Automatic tag detection and suggestion for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides automatic tag detection and suggestion functionality,
;; inspired by inline tag patterns. It can detect potential tags in text content
;; and suggest appropriate tags based on context.

;;; Code:

(require 'org-supertag-bridge)
(require 'org-supertag-db)

(defgroup org-supertag-auto-tag nil
  "Settings for automatic tag detection and suggestion."
  :group 'org-supertag)

(defcustom org-supertag-auto-tag-enabled t
  "Enable automatic tag detection and suggestion."
  :type 'boolean
  :group 'org-supertag-auto-tag)

(defcustom org-supertag-auto-tag-inline-patterns
  '(
    ;; #标签模式（主要使用的模式）
    ("#\\([a-zA-Z0-9_@\u4e00-\u9fff]+\\)" . 1)
    ;; *加粗*模式（单个*号）
    ("\\*\\([^*\n]+\\)\\*" . 1)
    )
  "Patterns for detecting inline tag markers in text.
Each pattern is a cons cell (REGEX . GROUP) where:
- REGEX is the regular expression to match
- GROUP is the capture group number containing the tag name"
  :type '(repeat (cons string integer))
  :group 'org-supertag-auto-tag)

(defcustom org-supertag-auto-tag-suggestion-threshold 0.7
  "Minimum confidence threshold for tag suggestions."
  :type 'float
  :group 'org-supertag-auto-tag)

;;; Variables

(defvar org-supertag-auto-tag--suggestion-cache (make-hash-table :test 'equal)
  "Cache for tag suggestions to avoid repeated API calls.")

(defvar org-supertag-auto-tag--last-analysis-content ""
  "Content of last automatic analysis to avoid duplicate processing.")

;;; Core Functions

(defun org-supertag-auto-tag-extract-inline-tags (content)
  "Extract potential tags from CONTENT using inline patterns.
Returns a list of tag names found in the content."
  (let ((tags '()))
    (dolist (pattern org-supertag-auto-tag-inline-patterns)
      (let ((regex (car pattern))
            (group (cdr pattern)))
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          (while (re-search-forward regex nil t)
            (when-let* ((tag-text (match-string group)))
              (let ((clean-tag (org-supertag-auto-tag--normalize-tag-name tag-text)))
                (when (and clean-tag (not (member clean-tag tags)))
                  (push clean-tag tags))))))))
    (nreverse tags)))

(defun org-supertag-auto-tag--normalize-tag-name (tag-text)
  "Normalize TAG-TEXT to a valid tag name.
Removes special characters and converts to appropriate format."
  (when (and tag-text (not (string-empty-p tag-text)))
    (let ((normalized (string-trim tag-text)))
      ;; 移除特殊字符，保留字母、数字、中文、下划线和@
      (when (string-match "^[a-zA-Z0-9\u4e00-\u9fff_@]+$" normalized)
        normalized))))

(defun org-supertag-auto-tag-suggest-for-content (content &optional callback)
  "Suggest tags for CONTENT using AI analysis.
If CALLBACK is provided, make an asynchronous call."
  (unless (string= content org-supertag-auto-tag--last-analysis-content)
    (setq org-supertag-auto-tag--last-analysis-content content)
    
    (let ((cache-key (secure-hash 'sha256 content)))
      (if-let* ((cached-result (gethash cache-key org-supertag-auto-tag--suggestion-cache)))
          (progn
            (message "Using cached tag suggestions")
            (if callback
                (funcall callback cached-result)
              cached-result))
        ;; 调用AI进行标签建议
        (org-supertag-api-get-tag-suggestions-for-note
         content
         '()
         (if callback
             (lambda (result)
               (when result
                 (puthash cache-key result org-supertag-auto-tag--suggestion-cache)
                 (funcall callback result)))
           nil))))))

;;;###autoload
(defun org-supertag-auto-tag-analyze-current-node ()
  "Analyze current node and suggest tags. 只在无标签时自动建议。"
  (interactive)
  (when (and org-supertag-auto-tag-enabled
             (org-at-heading-p))
    (let* ((node-id (org-id-get-create))
           (content (org-supertag-auto-tag--get-node-content))
           (existing-tags (org-get-tags)))
      (when (and content (> (length content) 10))
        (unless (and existing-tags (> (length existing-tags) 0))
          (org-supertag-auto-tag--run-suggestion content existing-tags))))))

(defun org-supertag-auto-tag--run-suggestion (content existing-tags)
  "内部函数：根据内容和已有标签异步生成标签建议。"
  ;; 1. 提取内联标签
  (let ((inline-tags (org-supertag-auto-tag-extract-inline-tags content)))
    (when inline-tags
      (message "Found inline tags: %s" (string-join inline-tags ", "))
      (org-supertag-auto-tag--show-tag-suggestions 
       (mapcar (lambda (tag) `(:tag ,tag :confidence 0.9 :source "inline")) inline-tags)
       existing-tags)))
  ;; 2. AI标签建议（异步）
  (org-supertag-api-get-tag-suggestions-for-note
   content
   (or existing-tags '())
   (lambda (ai-result)
     (when ai-result
       (let ((suggestions (plist-get ai-result :suggestions)))
         (when suggestions
           (message "Got AI tag suggestions: %d items" (length suggestions))
           (org-supertag-auto-tag--show-tag-suggestions suggestions existing-tags)))))))

;;;###autoload
(defun org-supertag-auto-tag-force-suggest-current-node ()
  "强制为当前节点生成标签建议，无论是否已有标签。"
  (interactive)
  (when (org-at-heading-p)
    (let* ((content (org-supertag-auto-tag--get-node-content))
           (existing-tags (org-get-tags)))
      (when (and content (> (length content) 10))
        (org-supertag-auto-tag--run-suggestion content existing-tags)))))

(defun org-supertag-auto-tag--get-node-content ()
  "Get content of current node including title and body."
  (when (org-at-heading-p)
    (let* ((element (org-element-at-point))
           (title (org-element-property :raw-value element))
           (contents-begin (org-element-property :contents-begin element))
           (contents-end (org-element-property :contents-end element))
           (body (if (and contents-begin contents-end)
                     (buffer-substring-no-properties contents-begin contents-end)
                   "")))
      (concat title "\n" body))))

(defun org-supertag-auto-tag--show-tag-suggestions (suggestions existing-tags)
  "Show tag SUGGESTIONS to user, filtering out EXISTING-TAGS."
  (let ((filtered-suggestions
         (cl-remove-if 
          (lambda (sugg)
            (member (plist-get sugg :tag) existing-tags))
          suggestions)))
    
    (when filtered-suggestions
      (let ((high-confidence-tags
             (cl-remove-if
              (lambda (sugg)
                (< (or (plist-get sugg :confidence) 0)
                   org-supertag-auto-tag-suggestion-threshold))
              filtered-suggestions)))
        
        (if high-confidence-tags
            (org-supertag-auto-tag--prompt-for-tags high-confidence-tags)
          (message "No high-confidence tag suggestions found"))))))

(defun org-supertag-auto-tag--prompt-for-tags (suggestions)
  "Prompt user to select from tag SUGGESTIONS."
  (let ((choices '())
        (max-suggestions 5))
    
    ;; 构建选择列表（最多显示5个）
    (dolist (sugg (seq-take suggestions max-suggestions))
      (let* ((tag (plist-get sugg :tag))
             (confidence (plist-get sugg :confidence))
             (source (plist-get sugg :source))
             (display (format "%s (%.1f%% %s)" 
                             tag 
                             (* (or confidence 0.5) 100)
                             (or source "ai"))))
        (push (cons display tag) choices)))
    
    (when choices
      (let ((selected-display (completing-read 
                              "Select tags to add (space to select multiple): "
                              choices nil t)))
        (when selected-display
          (if-let* ((selected-tag (cdr (assoc selected-display choices))))
              (progn
                (org-supertag-tag-apply selected-tag)
                (message "Added tag: %s" selected-tag))
            (message "No tag selected")))))))

;;;###autoload
(defun org-supertag-auto-tag-process-buffer ()
  "Process entire buffer and suggest tags for all nodes."
  (interactive)
  (when org-supertag-auto-tag-enabled
    (save-excursion
      (let ((processed 0)
            (total 0))
        (org-map-entries
         (lambda ()
           (setq total (1+ total))
           (when (and (>= (org-current-level) 1)
                     (org-id-get))
             (org-supertag-auto-tag-analyze-current-node)
             (setq processed (1+ processed))))
         t 'file)
        (message "Auto-tag analysis complete: processed %d/%d nodes" processed total)))))

;;;###autoload
(defun org-supertag-auto-tag-clear-cache ()
  "Clear the tag suggestion cache."
  (interactive)
  (clrhash org-supertag-auto-tag--suggestion-cache)
  (setq org-supertag-auto-tag--last-analysis-content "")
  (message "Auto-tag cache cleared"))

;;; Minor Mode

(define-minor-mode org-supertag-auto-tag-mode
  "Minor mode for automatic tag detection and suggestion."
  :lighter " AutoTag"
  :group 'org-supertag-auto-tag
  (if org-supertag-auto-tag-mode
      (progn
        ;; 添加 hook
        (add-hook 'org-after-refile-insert-hook 
                  #'org-supertag-auto-tag-analyze-current-node nil t)
        (add-hook 'org-insert-heading-hook 
                  #'org-supertag-auto-tag-analyze-current-node nil t)
        (message "Org SuperTag auto-tag mode enabled"))
    (progn
      ;; 移除 hook
      (remove-hook 'org-after-refile-insert-hook 
                   #'org-supertag-auto-tag-analyze-current-node t)
      (remove-hook 'org-insert-heading-hook 
                   #'org-supertag-auto-tag-analyze-current-node t)
      (message "Org SuperTag auto-tag mode disabled"))))



(provide 'org-supertag-auto-tag)

;;; org-supertag-auto-tag.el ends here 
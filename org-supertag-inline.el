;;; org-supertag-inline.el --- Support for inline tags in org-mode content -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024

;; Author: User <user@example.com>
;; Keywords: org-mode, tags, inline

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This module adds support for inline tags within org-mode content.
;; Inline tags are prefixed with a '#' symbol (like #hashtags) and are
;; distinct from headline tags.
;;
;; Example:
;; "This is a paragraph with an #inline-tag that can be tracked and queried."

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-supertag-db)
(require 'org-supertag-node)
(require 'org-supertag-tag)

(defgroup org-supertag-inline nil
  "Customization options for org-supertag inline tags."
  :group 'org-supertag)

(defcustom org-supertag-inline-tag-regexp "#\\([[:alnum:]-_]+\\)"
  "Regular expression pattern to match inline tags.
The first capture group should match the tag name without the '#' prefix."
  :type 'regexp
  :group 'org-supertag-inline)

(defcustom org-supertag-inline-excluded-contexts
  '(src-block verbatim code comment-block comment headline)
  "List of org element contexts where inline tags should not be recognized.
This prevents interpreting '#' symbols in code blocks, comments, etc. as tags."
  :type '(repeat symbol)
  :group 'org-supertag-inline)

(defcustom org-supertag-inline-enable-fontification t
  "Whether to enable special fontification for inline tags."
  :type 'boolean
  :group 'org-supertag-inline)

(defcustom org-supertag-inline-auto-sync-enabled nil
  "Whether to automatically sync inline tags when saving.
When set to nil, inline tags must be manually synced."
  :type 'boolean
  :group 'org-supertag-inline)

(defface org-supertag-inline-tag-face
  '((t (:inherit org-tag :weight bold :foreground "black" :background "light yellow")))
  "Face used for highlighting inline tags in org-mode buffers."
  :group 'org-supertag-inline)

;; Internal variables

(defvar org-supertag-inline--node-tags-cache (make-hash-table :test 'equal)
  "Cache mapping node IDs to lists of inline tags.
This is used to avoid repeatedly parsing the same content.")

(defun org-supertag-inline--cache-clear ()
  "Clear the inline tag cache."
  (clrhash org-supertag-inline--node-tags-cache))

(defun org-supertag-inline--valid-context-p ()
  "Check if the current point is in a valid context for inline tags.
Returns nil if the point is in one of the contexts listed in
`org-supertag-inline-excluded-contexts'."
  (let ((context (org-element-context)))
    (not (memq (org-element-type context) org-supertag-inline-excluded-contexts))))

;;;###autoload
(defun org-supertag-inline-parse-buffer ()
  "Parse the current buffer for inline tags.
Returns a hash table mapping node IDs to lists of tag names."
  (let ((result (make-hash-table :test 'equal))
        (org-trust-scanner-tags t)) ;; Allow org-get-tags-at to work
    (org-map-entries
     (lambda ()
       (let ((node-id (org-id-get-create))
             (tags (org-supertag-inline-extract-tags-at-point)))
         (when tags
           (puthash node-id tags result))))
     nil 'file)
    result))

(defun org-supertag-inline--extract-tags-region (begin end)
  "Extract inline tags from the region between BEGIN and END.
Returns a list of unique tag names (without the '#' prefix)."
  (let ((tags '()))
    (save-excursion
      (goto-char begin)
      (while (re-search-forward org-supertag-inline-tag-regexp end t)
        (when (org-supertag-inline--valid-context-p)
          (let ((tag (match-string-no-properties 1)))
            (push tag tags)))))
    (delete-dups tags)))

;;;###autoload
(defun org-supertag-inline-extract-tags-at-point ()
  "Extract inline tags from the current node.
Returns a list of unique tag names (without the '#' prefix)."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (let* ((element (org-element-at-point))
             (begin (org-element-property :begin element))
             (end (org-element-property :end element)))
        (org-supertag-inline--extract-tags-region begin end)))))

;;;###autoload
(defun org-supertag-inline-sync-node-at-point ()
  "Synchronize inline tags for the current node with the database."
  (interactive)
  (when-let* ((node-id (org-id-get-create))
              (tags (org-supertag-inline-extract-tags-at-point)))
    ;; First, remove all existing inline tag relationships for this node
    (org-supertag-node-db-remove-inline-tags node-id)
    
    ;; Then add new relationships for each tag
    (dolist (tag-name tags)
      (let ((tag-id (org-supertag-tag-ensure tag-name)))
        (when (and (org-supertag-node-db-exists-p node-id)
                 (org-supertag-db-exists-p tag-id))
          (org-supertag-db-link :node-inline-tag node-id tag-id))))
    
    ;; Update cache
    (puthash node-id tags org-supertag-inline--node-tags-cache)
    
    ;; Return the list of synced tags
    tags))

(defun org-supertag-node-add-inline-tag (node-id tag-id)
  "Add an inline tag relationship between NODE-ID and TAG-ID."
  (when (and (org-supertag-node-db-exists-p node-id)
             (org-supertag-db-exists-p tag-id))
    ;; Add relationship
    (org-supertag-db-link :node-inline-tag 
                         node-id 
                         tag-id 
                         ;; Add relationship properties
                         `(:created-at ,(current-time)))))

(defun org-supertag-node-db-remove-inline-tags (node-id)
  "Remove all inline tag relationships for NODE-ID."
  (org-supertag-db-remove-links-by-from
   :node-inline-tag node-id))

(defun org-supertag-node-get-inline-tags (node-id)
  "Get all inline tags for NODE-ID.
Returns a list of tag IDs."
  (mapcar 
   #'cdr
   (org-supertag-db-get-links-by-from-and-type
    node-id :node-inline-tag)))

(defun org-supertag-tag-get-inline-nodes (tag-id)
  "Get all nodes that have TAG-ID as an inline tag.
Returns a list of node IDs."
  (mapcar 
   #'car
   (org-supertag-db-get-links-by-to-and-type
    tag-id :node-inline-tag)))

;;;###autoload
(defun org-supertag-inline-sync-buffer ()
  "Synchronize all inline tags in the current buffer."
  (interactive)
  (if (and (fboundp 'org-element-cache-active-p)
           (org-element-cache-active-p))
      ;; 使用缓存的方法
      (condition-case err
          (org-map-entries
           (lambda () (org-supertag-inline-sync-node-at-point))
           nil 'file)
        (error
         (message "Error during inline tag sync with cache: %s" (error-message-string err))
         ;; 回退到非缓存方法
         (org-supertag-inline--sync-buffer-without-cache)))
    ;; 直接使用非缓存方法
    (org-supertag-inline--sync-buffer-without-cache)))

(defun org-supertag-inline--sync-buffer-without-cache ()
  "Synchronize inline tags without relying on the org element cache."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^\\*+ " nil t)
        (beginning-of-line)
        (when (org-at-heading-p)
          (save-excursion
            (condition-case nil
                (progn
                  (org-supertag-inline-sync-node-at-point)
                  (cl-incf count))
              (error nil))))
        (end-of-line))
      (message "Synced inline tags for %d headlines without using cache" count))))

;;;###autoload
(defun org-supertag-inline-sync-force-all ()
  "Force synchronization of inline tags for all open org-mode buffers."
  (interactive)
  (let ((count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'org-mode)
                   (buffer-file-name))
          (condition-case err
              (progn
                (org-supertag-inline-sync-buffer)
                (cl-incf count))
            (error
             (message "Error syncing inline tags in buffer %s: %s" 
                      (buffer-name) (error-message-string err)))))))
    (message "Inline tag synchronization complete for %d org-mode buffer(s)" count)))

;; New function for inserting inline tags

;;;###autoload
(defun org-supertag-inline-insert-tag (tag-name)
  "Insert an inline tag at point and establish proper relationships.
With prefix arg, also add it as a normal tag to the current headline.
TAG-NAME is the name of the tag to insert."
  (interactive
   (list (let* ((all-tags (org-supertag-get-all-tags))
                (preset-names (mapcar #'car org-supertag-preset-tags))
                ;; 移除已经存在的预设标签，避免重复
                (user-tags (cl-remove-if (lambda (tag) (member tag preset-names)) all-tags))
                (candidates (delete-dups
                            (append 
                             ;; 使用 [P] 前缀标记预设标签
                             (mapcar (lambda (name) 
                                     (format "[P] %s" name))
                                   preset-names)
                             ;; 常规标签原样保留
                             user-tags)))
                (input (completing-read
                       "Inline tag: "
                       candidates nil nil)))
           ;; 处理输入，去除 [P] 前缀
           (if (string-prefix-p "[P] " input)
               (substring input 4)
             input))))
  
  (when tag-name
    (let* (;; 检查输入是否以 # 结尾，如果是则直接创建新标签
           (direct-create (string-suffix-p "#" tag-name))
           (tag-name-clean (if direct-create
                             (substring tag-name 0 -1)
                           tag-name))
           (sanitized-name (org-supertag-sanitize-tag-name tag-name-clean))
           ;; 确保当前节点存在
           (node-id (org-id-get-create)))
      
      ;; 获取或创建标签
      (let ((tag-id
             (cond
              ;; 如果标签已存在，直接使用
              ((org-supertag-tag-exists-p sanitized-name)
               sanitized-name)
              ;; 否则，创建新标签
              (t
               (if (or direct-create
                       (y-or-n-p (format "Create new tag '%s'? " sanitized-name)))
                   (org-supertag-tag-create sanitized-name)
                 (user-error "Tag creation cancelled"))))))
        
        ;; 在光标位置插入内联标签
        (insert (concat "#" tag-id))
        
        ;; 确保节点已在数据库中创建
        (unless (org-supertag-db-exists-p node-id)
          (org-supertag--create-node node-id))
        
        ;; 建立节点-标签关系
        (org-supertag-db-link :node-tag 
                             node-id 
                             tag-id 
                             `(:inline t :created-at ,(current-time)))
        
        ;; 如果标签有字段定义，应用这些字段
        (when-let* ((tag (org-supertag-tag-get tag-id))
                   (fields (plist-get tag :fields)))
          (dolist (field fields)
            (let* ((field-name (plist-get field :name))
                   (field-type (plist-get field :type))
                   (type-def (org-supertag-get-field-type field-type))
                   (initial-value (org-supertag-field-get-initial-value field)))
              (when-let* ((formatter (plist-get type-def :formatter))
                         (formatted-value (if formatter
                                            (funcall formatter initial-value field)
                                          (format "%s" initial-value))))
                (org-set-property field-name formatted-value)
                (org-supertag-tag--set-field-value tag-id node-id field-name initial-value)))))
        
        ;; 触发标签应用后的行为
        (run-hook-with-args 'org-supertag-after-tag-apply-hook node-id)
        (when (fboundp 'org-supertag-behavior--on-tag-change)
          (org-supertag-behavior--on-tag-change node-id tag-id :add))
        (when (fboundp 'org-supertag-behavior--apply-styles)
          (org-supertag-behavior--apply-styles node-id))
        
        ;; 如果提供了前缀参数，也将其添加为普通标签
        (when current-prefix-arg
          (save-excursion
            (org-back-to-heading t)
            (org-supertag-tag-apply tag-id)))
        
        (message "Inserted inline tag #%s" tag-id)))))

;; Extend the query system to include inline tags

(defun org-supertag-query-nodes-by-inline-tag (tag-name)
  "Query nodes that have TAG-NAME as an inline tag.
Returns a list of node IDs."
  (when-let* ((tag-id (org-supertag-tag-get-id tag-name)))
    (org-supertag-tag-get-inline-nodes tag-id)))

;; Completion support for inline tags

;;;###autoload
(defun org-supertag-tag-ensure (tag-name)
  "Ensure a tag exists, creating it if necessary.
TAG-NAME is the name of the tag to ensure exists.
Returns the tag ID."
  (let ((sanitized-name (org-supertag-sanitize-tag-name tag-name)))
    (if (org-supertag-tag-exists-p sanitized-name)
        sanitized-name
      (org-supertag-tag-create sanitized-name))))

;; New completion backend specifically for inline tags

(defun org-supertag-inline-completion-at-point ()
  "Function for `completion-at-point-functions' for inline tags."
  (when (and (derived-mode-p 'org-mode)
             (org-supertag-inline--valid-context-p))
    (let ((bounds (save-excursion
                    (when (looking-back "#\\([[:alnum:]-_]*\\)" (line-beginning-position))
                      (cons (match-beginning 0) (match-end 0))))))
      (when bounds
        (let* ((prefix (buffer-substring-no-properties (car bounds) (cdr bounds)))
               (prefix-no-hash (if (> (length prefix) 1)
                                  (substring prefix 1)
                                ""))
               (all-tags (org-supertag-get-all-tags))
               (matching-tags 
                (mapcar (lambda (tag) (concat "#" tag))
                        (cl-remove-if-not 
                         (lambda (tag) 
                           (string-prefix-p prefix-no-hash tag t))
                         all-tags))))
          (list (car bounds) (cdr bounds) matching-tags
                :exclusive 'no
                :annotation-function
                (lambda (tag)
                  (when-let* ((tag-name (substring tag 1))
                              (tag-entity (org-supertag-tag-get tag-name))
                              (fields (plist-get tag-entity :fields)))
                    (format " [%d fields]" (length fields))))
                :exit-function
                (lambda (tag _status)
                  (let ((tag-name (substring tag 1)))
                    ;; Ensure tag exists
                    (org-supertag-tag-ensure tag-name)
                    ;; When a node exists, sync the tag
                    (when-let ((node-id (org-id-get)))
                      (when (and (org-supertag-node-db-exists-p node-id)
                               (org-supertag-db-exists-p tag-name))
                        (org-supertag-db-link :node-inline-tag node-id tag-name)))))))))))

;; Company backend for inline tags

(defun org-supertag-inline-company-backend (command &optional arg &rest ignored)
  "Company backend for inline tag completion.
COMMAND, ARG and IGNORED are standard company backend arguments."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'org-supertag-inline-company-backend))
    (prefix (when (and (derived-mode-p 'org-mode)
                      (org-supertag-inline--valid-context-p)
                      (looking-back "#\\([[:alnum:]-_]*\\)" (line-beginning-position)))
              (match-string-no-properties 0)))
    (candidates 
     (let* ((prefix-no-hash (if (> (length arg) 1)
                               (substring arg 1)
                             ""))
            (all-tags (org-supertag-get-all-tags)))
       (mapcar (lambda (tag) (concat "#" tag))
               (cl-remove-if-not 
                (lambda (tag) 
                  (string-prefix-p prefix-no-hash tag t))
                all-tags))))
    (post-completion 
     (let ((tag-name (substring arg 1))) ; Remove # prefix
       ;; Ensure tag exists
       (org-supertag-tag-ensure tag-name)
       ;; When a node exists, sync the tag
       (when-let ((node-id (org-id-get)))
         (when (and (org-supertag-node-db-exists-p node-id)
                  (org-supertag-db-exists-p tag-name))
           (org-supertag-db-link :node-inline-tag node-id tag-name)))))
    (annotation 
     (when-let* ((tag-name (substring arg 1))
                (tag (org-supertag-tag-get tag-name))
                (fields (plist-get tag :fields)))
        (format " [%d fields]" (length fields))))
    (t nil)))

;; Fontification

(defun org-supertag-inline-fontify (limit)
  "Apply face to inline tags in the buffer up to LIMIT.
This function is designed to be used with `font-lock-add-keywords'."
  (when (and org-supertag-inline-enable-fontification
             (re-search-forward org-supertag-inline-tag-regexp limit t))
    (when (org-supertag-inline--valid-context-p)
      ;; Apply face to the entire match (including the # symbol)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'org-supertag-inline-tag-face)
      t)))

(defun org-supertag-inline-refresh-fontification ()
  "Refresh fontification for inline tags in the current buffer."
  (interactive)
  (when (and org-supertag-inline-mode 
             org-supertag-inline-enable-fontification)
    (font-lock-flush)
    (message "Refreshed inline tag fontification")))

;; Minor mode for inline tags

;;;###autoload
(define-minor-mode org-supertag-inline-mode
  "Toggle org-supertag inline tag mode.
With prefix argument ARG, turn the mode on if ARG is positive, otherwise turn it off.
When enabled, inline tags (prefixed with #) are recognized and can be queried."
  :lighter " OrgST-Inline"
  :group 'org-supertag-inline
  (if org-supertag-inline-mode
      (progn
        ;; Set up fontification if enabled
        (when org-supertag-inline-enable-fontification
          (font-lock-add-keywords nil '((org-supertag-inline-fontify)) t)
          ;; Ensure higher priority for our keywords
          (setq-local font-lock-keywords-case-fold-search t))
        
        ;; Set up completion
        (add-hook 'completion-at-point-functions 
                  #'org-supertag-inline-completion-at-point nil t)
        
        ;; Add hook to sync tags when saving (if auto-sync is enabled)
        (when org-supertag-inline-auto-sync-enabled
          (add-hook 'after-save-hook 'org-supertag-inline-sync-buffer nil t))
        
        ;; Add hook to refresh fontification after insert/edit
        (add-hook 'after-change-functions 
                  (lambda (&rest _) 
                    (when (and org-supertag-inline-enable-fontification
                               (save-excursion
                                 (goto-char (point-at-bol))
                                 (re-search-forward org-supertag-inline-tag-regexp (point-at-eol) t)))
                      (font-lock-flush (point-at-bol) (point-at-eol))))
                  nil t)
        
        ;; Add keybindings
        (local-set-key (kbd "C-c C-x #") 'org-supertag-inline-insert-tag))
    
    ;; Cleanup when disabling the mode
    (when org-supertag-inline-enable-fontification
      (font-lock-remove-keywords nil '((org-supertag-inline-fontify))))
    
    (remove-hook 'completion-at-point-functions 
                 #'org-supertag-inline-completion-at-point t)
    
    (remove-hook 'after-change-functions 
                 (lambda (&rest _) 
                   (when (and org-supertag-inline-enable-fontification
                              (save-excursion
                                (goto-char (point-at-bol))
                                (re-search-forward org-supertag-inline-tag-regexp (point-at-eol) t)))
                     (font-lock-flush (point-at-bol) (point-at-eol))))
                 t)
    
    (when org-supertag-inline-auto-sync-enabled
      (remove-hook 'after-save-hook 'org-supertag-inline-sync-buffer t))
    
    ;; Unset keybindings
    (local-unset-key (kbd "C-c C-x #"))
    (local-unset-key (kbd "C-c C-x #a"))
    (local-unset-key (kbd "C-c C-x #r"))
    
    ;; Clear fontification
    (font-lock-flush))
  
  ;; Force fontification update
  (when font-lock-mode
    (if org-supertag-inline-mode
        (font-lock-flush)
      (font-lock-ensure))))

;;;###autoload
(defun org-supertag-inline-setup ()
  "Set up the inline tag system.
This initializes the necessary components and integrates with
the org-supertag synchronization system."
  (interactive)
  ;; Initialize components
  (org-supertag-inline--cache-clear)
  
  ;; Register with the org-supertag system
  (add-hook 'org-supertag-mode-hook
            (lambda ()
              (org-supertag-inline-mode (if org-supertag-mode 1 -1))))
  
  ;; Setup Company completion if available
  (when (featurep 'company)
    (add-to-list 'company-backends 'org-supertag-inline-company-backend))
  
  ;; Inform user about the setup mode
  (if org-supertag-inline-auto-sync-enabled
      (message "Inline tag support enabled with automatic synchronization.")
    (message "Inline tag support enabled with manual insertion. Use C-c C-x # to insert tags.")))




(provide 'org-supertag-inline)
;;; org-supertag-inline.el ends here 

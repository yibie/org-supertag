;; org-supertag.el --- Org-supertag 插件 -*- lexical-binding: t; -*-

(require 'org-supertag-db)
(require 'org-supertag-node)
(require 'org-supertag-field)
(require 'org-supertag-tag)
(require 'org-supertag-query)

(defgroup org-supertag nil
  "Org-supertag 配置选项."
  :group 'org
  :prefix "org-supertag-")

;;;###autoload
(define-minor-mode org-supertag-mode
  "Toggle org-supertag mode."
  :init-value nil
  :lighter " SuperTag"
  :group 'org-supertag
  (if org-supertag-mode
      (org-supertag--enable)
    (org-supertag--disable)))

(defun org-supertag--enable ()
  "启用 org-supertag."
  ;; 1. 确保数据库目录存在
  (org-supertag-db-ensure-data-directory)
  ;; 2. 初始化数据库
  (org-supertag-db-init)
  ;; 3. 设置自动保存
  (org-supertag-db--setup-auto-save)
  ;; 4. 设置必要的钩子
  (add-hook 'kill-emacs-hook #'org-supertag-db-save))

(defun org-supertag--disable ()
  "禁用 org-supertag."
  ;; 1. 保存数据库
  (org-supertag-db-save)
  ;; 2. 清理自动保存定时器
  (org-supertag-db--cleanup-auto-save)
  ;; 3. 清理缓存
  (org-supertag-db--cache-clear)
  ;; 4. 移除钩子
  (remove-hook 'kill-emacs-hook #'org-supertag-db-save))

(defun org-supertag-cleanup ()
  "清理 org-supertag 资源.
用于手动清理或重置系统状态."
  (interactive)
  ;; 1. 保存数据库
  (org-supertag-db-save)
  ;; 2. 清理自动保存定时器
  (org-supertag-db--cleanup-auto-save)
  ;; 3. 清理缓存
  (org-supertag-db--cache-clear)
  ;; 4. 重置脏数据标记
  (org-supertag-db--clear-dirty))

;;;###autoload
(defun org-supertag-setup ()
  "设置 org-supertag.
在 org-mode 缓冲区中自动启用 org-supertag-mode."
  (interactive)
  (org-supertag-db-init)
  ;; 为所有 org-mode 缓冲区启用 org-supertag-mode
  (add-hook 'org-mode-hook #'org-supertag-mode))

(provide 'org-supertag)
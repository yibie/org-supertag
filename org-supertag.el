;; org-supertag.el --- Org-supertag 插件 -*- lexical-binding: t; -*-

(require 'org-supertag-db)
(require 'org-supertag-core)
(require 'org-supertag-node)

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
  ;; 3. 设置必要的钩子
  (add-hook 'kill-emacs-hook #'org-supertag-db-save))

(defun org-supertag--disable ()
  "禁用 org-supertag."
  ;; 1. 保存数据库
  (org-supertag-db-save)
  ;; 2. 移除钩子
  (remove-hook 'kill-emacs-hook #'org-supertag-db-save))

;;;###autoload
(defun org-supertag-setup ()
  "设置 org-supertag.
在 org-mode 缓冲区中自动启用 org-supertag-mode."
  (interactive)
  ;; 1. 确保数据库初始化
  (org-supertag-db-init)
  ;; 2. 为所有 org-mode 缓冲区启用 org-supertag-mode
  (add-hook 'org-mode-hook #'org-supertag-mode))



(provide 'org-supertag)
;;; supertag-compat.el --- Compatibility layer for supertag -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides backward compatibility for org-supertag commands and keybindings.
;; All commands use the `org-supertag-` prefix and are bound to `C-c s` by default.
;;
;; ## Customizing Keybindings
;;
;; To disable all compatibility keybindings:
;;
;;     (org-supertag-compat-mode 0)
;;
;; To unbind a specific key:
;;
;;     (with-eval-after-load 'supertag-compat
;;       (define-key org-supertag-compat-mode-map (kbd "C-c s b") nil))
;;
;; To rebind a command to a different key:
;;
;;     (with-eval-after-load 'supertag-compat
;;       (define-key org-supertag-compat-mode-map (kbd "C-c s b") nil)
;;       (define-key org-supertag-compat-mode-map (kbd "C-c o b") 'org-supertag-insert-embed))
;;

;;; Code:

;; Backward compatibility aliases
(defalias 'org-supertag-inline-add 'supertag-add-tag)
(defalias 'org-supertag-inline-remove 'supertag-remove-tag-from-node)
(defalias 'org-supertag-inline-rename 'supertag-rename-tag)
(defalias 'org-supertag-inline-delete-all 'supertag-delete-tag-everywhere)
(defalias 'org-supertag-inline-change-tag 'supertag-change-tag-at-point)
(defalias 'org-supertag-set-child 'supertag-set-child)
(defalias 'org-supertag-clear-parent 'supertag-clear-parent)
(defalias 'org-supertag-capture-direct 'supertag-capture)
(defalias 'org-supertag-capture-template 'supertag-capture-with-template)
(defalias 'org-supertag-insert-query-block 'supertag-insert-query-block)
(defalias 'org-supertag-move-node-and-link 'supertag-move-node-and-link)
(defalias 'org-supertag-node-add-reference 'supertag-add-reference)
(defalias 'org-supertag-node-remove-reference 'supertag-remove-reference)
(defalias 'org-supertag-node-back-to-heading 'supertag-back-to-heading)
(defalias 'org-supertag-node-create 'supertag-create-node)
(defalias 'org-supertag-node-delete 'supertag-delete-node)
(defalias 'org-supertag-node-find 'supertag-find-node)
(defalias 'org-supertag-node-find-other-window 'supertag-find-node-other-window)
(defalias 'org-supertag-node-move 'supertag-move-node)
(defalias 'org-supertag-node-update 'supertag-update-node-at-point)
(defalias 'org-supertag-query 'supertag-search)
(defalias 'org-supertag-query-export-results-to-file 'supertag-search-export-results-to-file)
(defalias 'org-supertag-query-export-results-to-new-file 'supertag-search-export-results-to-new-file)
;;(defalias 'org-supertag-query-export-results-here ')
(defalias 'org-supertag-query-insert-at-point 'supertag-search-insert-at-point)
(defalias 'org-supertag-view-chat-open 'supertag-chat)
(defalias 'org-supertag-view-node 'supertag-view-node)
(defalias 'org-supertag-view-table 'supertag-view-table)
(defalias 'org-supertag-view-kanban 'supertag-view-kanban)
(defalias 'org-supertag-clean-database 'supertag-sync-cleanup-database)
;; Embed operations
(defalias 'org-supertag-insert-embed 'supertag-insert-embed)
(defalias 'org-supertag-convert-link-to-embed 'supertag-convert-link-to-embed)
(defalias 'org-supertag-embed-refresh-all 'supertag-services-embed-refresh-all)
(defalias 'org-supertag-reference-and-create 'supertag-add-reference-and-create)

;; Keymap definitions for backward compatibility
(defvar org-supertag-compat-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Capture operations
    (define-key map (kbd "C-c s C") 'org-supertag-capture-direct)
    (define-key map (kbd "C-c s t") 'org-supertag-capture-template)
    
    ;; Tag operations
    (define-key map (kbd "C-c s a") 'org-supertag-inline-add)
    (define-key map (kbd "C-c s r") 'org-supertag-inline-remove)
    (define-key map (kbd "C-c s n") 'org-supertag-inline-rename)
    (define-key map (kbd "C-c s d") 'org-supertag-inline-delete-all)
    (define-key map (kbd "C-c s c") 'org-supertag-inline-change-tag)
    (define-key map (kbd "C-c s x") 'org-supertag-set-child)
    (define-key map (kbd "C-c s X") 'org-supertag-clear-parent)
    
    ;; Node operations
    (define-key map (kbd "C-c s i") 'org-supertag-insert-query-block)
    (define-key map (kbd "C-c s m") 'org-supertag-move-node-and-link)
    (define-key map (kbd "C-c s l") 'org-supertag-node-add-reference)
    (define-key map (kbd "C-c s L") 'org-supertag-reference-and-create)
    (define-key map (kbd "C-c s R") 'org-supertag-node-remove-reference)
    (define-key map (kbd "C-c s h") 'org-supertag-node-back-to-heading)
    (define-key map (kbd "C-c s N") 'org-supertag-node-create)
    (define-key map (kbd "C-c s D") 'org-supertag-node-delete)
    (define-key map (kbd "C-c s f") 'org-supertag-node-find)
    (define-key map (kbd "C-c s o") 'org-supertag-node-find-other-window)
    (define-key map (kbd "C-c s M") 'org-supertag-node-move)
    (define-key map (kbd "C-c s u") 'org-supertag-node-update)
    
    ;; Query operations
    (define-key map (kbd "C-c s s") 'org-supertag-query)
    (define-key map (kbd "C-c s e") 'org-supertag-query-export-results-to-file)
    (define-key map (kbd "C-c s E") 'org-supertag-query-export-results-to-new-file)
    (define-key map (kbd "C-c s I") 'org-supertag-query-insert-at-point)
    
    ;; View operations
    (define-key map (kbd "C-c s g") 'org-supertag-view-chat-open)
    (define-key map (kbd "C-c s v") 'org-supertag-view-node)
    (define-key map (kbd "C-c s T") 'org-supertag-view-table)
    (define-key map (kbd "C-c s k") 'org-supertag-view-kanban)
    
    ;; Embed operations
    (define-key map (kbd "C-c s b") 'org-supertag-insert-embed)
    (define-key map (kbd "C-c s B") 'org-supertag-convert-link-to-embed)
    (define-key map (kbd "C-c s C-r") 'org-supertag-embed-refresh-all)
    
    ;; Database operations
    (define-key map (kbd "C-c s C-c") 'org-supertag-clean-database)
    
    map)
  "Keymap for org-supertag compatibility mode.")

;; Define the minor mode for compatibility
(define-minor-mode org-supertag-compat-mode
  "Minor mode for org-supertag backward compatibility keybindings."
  :lighter " supertag-compat"
  :keymap org-supertag-compat-mode-map
  :global t
  :group 'supertag)

;; Enable the mode by default
(org-supertag-compat-mode 1)

(provide 'supertag-compat)

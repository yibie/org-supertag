;;; supertag-compat.el --- Compatibility layer for supertag -*- lexical-binding: t; -*-
;;; Code:

;; Backward compatibility aliases
(defalias 'org-supertag-inline-add 'supertag-add-tag)
(defalias 'org-supertag-inline-remove 'supertag-remove-tag-from-node)
(defalias 'org-supertag-inline-rename 'supertag-rename-tag)
(defalias 'org-supertag-inline-delete-all 'supertag-delete-tag-everywhere)
(defalias 'org-supertag-inline-change-tag 'supertag-change-tag-at-point)
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
(defalias 'org-supertag-tag-set-extends 'supertag-set-tag-extends)
(defalias 'org-supertag-view-chat-open 'supertag-chat)
(defalias 'org-supertag-view-node 'supertag-view-node)
(defalias 'org-supertag-view-table 'supertag-view-table)
(defalias 'org-supertag-view-kanban 'supertag-view-kanban)
(defalias 'org-supertag-clean-database 'supertag-sync-cleanup-database)

;; Define the prefix key first
(defvar org-supertag-prefix-map
  (let ((map (make-sparse-keymap)))
    ;; Capture operations
    (define-key map (kbd "C") 'org-supertag-capture-direct)
    (define-key map (kbd "t") 'org-supertag-capture-template)
    
    ;; Tag operations
    (define-key map (kbd "a") 'org-supertag-inline-add)
    (define-key map (kbd "r") 'org-supertag-inline-remove)
    (define-key map (kbd "n") 'org-supertag-inline-rename)
    (define-key map (kbd "d") 'org-supertag-inline-delete-all)
    (define-key map (kbd "c") 'org-supertag-inline-change-tag)
    
    ;; Node operations
    (define-key map (kbd "i") 'org-supertag-insert-query-block)
    (define-key map (kbd "m") 'org-supertag-move-node-and-link)
    (define-key map (kbd "A") 'org-supertag-node-add-reference)
    (define-key map (kbd "R") 'org-supertag-node-remove-reference)
    (define-key map (kbd "h") 'org-supertag-node-back-to-heading)
    (define-key map (kbd "N") 'org-supertag-node-create)
    (define-key map (kbd "D") 'org-supertag-node-delete)
    (define-key map (kbd "f") 'org-supertag-node-find)
    (define-key map (kbd "o") 'org-supertag-node-find-other-window)
    (define-key map (kbd "M") 'org-supertag-node-move)
    (define-key map (kbd "u") 'org-supertag-node-update)
    
    ;; Query operations
    (define-key map (kbd "s") 'org-supertag-query)
    (define-key map (kbd "e") 'org-supertag-query-export-results-to-file)
    (define-key map (kbd "E") 'org-supertag-query-export-results-to-new-file)
    (define-key map (kbd "I") 'org-supertag-query-insert-at-point)
    
    ;; Tag operations
    (define-key map (kbd "x") 'org-supertag-tag-set-extends)
    
    ;; View operations
    (define-key map (kbd "g") 'org-supertag-view-chat-open)
    (define-key map (kbd "v") 'org-supertag-view-node)
    (define-key map (kbd "T") 'org-supertag-view-table)
    (define-key map (kbd "k") 'org-supertag-view-kanban)
    
    ;; Database operations
    (define-key map (kbd "C-c") 'org-supertag-clean-database)
      
    map)
  "Keymap for org-supertag prefix commands.")

;; Keymap definitions for backward compatibility with C-c s prefix
(defvar org-supertag-compat-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Set up the prefix key C-c s
    (define-key map (kbd "C-c s") org-supertag-prefix-map)
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

;; Also bind the prefix key globally to ensure it works
(global-set-key (kbd "C-c s") org-supertag-prefix-map)

(provide 'supertag-compat)
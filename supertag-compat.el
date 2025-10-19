;;; supertag-compat.el --- Compatibility layer for supertag -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file keeps a stub compatibility mode so configurations that still
;; toggle `org-supertag-compat-mode` do not break. Legacy keybindings and
;; `org-supertag-` command aliases have been retired—see the README for the
;; current `supertag-` commands and sample keybinding snippet.
;;

;;; Code:

;; Compatibility mode is now a no-op; customization moved to documentation.
(defvar org-supertag-compat-mode-map nil
  "Deprecated compatibility keymap. Intentionally left empty.")

(define-minor-mode org-supertag-compat-mode
  "Deprecated compatibility mode retained for backward compatibility.
This mode no longer installs key bindings or legacy command aliases.
See the README for guidance on configuring custom shortcuts."
  :lighter " supertag-compat"
  :keymap org-supertag-compat-mode-map
  :global t
  :group 'supertag)

(make-obsolete 'org-supertag-compat-mode
               "Configure key bindings manually; see README for examples."
               "0.9.0")

(provide 'supertag-compat)

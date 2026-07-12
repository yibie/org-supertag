;;; supertag-setup.el --- First-run setup wizard for Org-Supertag -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Yibie

;; Author: Yibie
;; Keywords: org-mode, tags, metadata, workflow, automation

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; `M-x supertag-setup' is a first-run configuration wizard for
;; Org-Supertag.  It is meant to save new users from having to hand-edit
;; their init file before anything works: it walks through picking sync
;; directories, a file-ID source, how to persist those choices, and
;; (optionally) running the initial database scan.
;;
;; Every step is skippable and the whole wizard is safe to re-run.  Nothing
;; is applied to a live variable until the "persist" step; hitting `C-g'
;; at any point before then leaves your configuration untouched.  The one
;; exception is the initial-scan step, which is an explicit, clearly
;; labeled action.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-supertag)

;;; --- Status helpers --------------------------------------------------

(defun supertag-setup--current-directories ()
  "Return the currently configured sync directories, or nil."
  (and (boundp 'org-supertag-sync-directories)
       org-supertag-sync-directories))

(defun supertag-setup--current-file-id-source ()
  "Return the currently configured file-ID source symbol."
  (if (boundp 'org-supertag-file-id-source)
      org-supertag-file-id-source
    'org-roam))

(defun supertag-setup--db-file ()
  "Return the configured database file path, or nil if unknown."
  (and (boundp 'supertag-db-file) supertag-db-file))

(defun supertag-setup--node-count ()
  "Return the number of nodes currently loaded in memory, or nil if unknown."
  (ignore-errors
    (when (and (boundp 'supertag--store) (hash-table-p supertag--store))
      (let ((nodes (gethash :nodes supertag--store)))
        (when (hash-table-p nodes) (hash-table-count nodes))))))

(defun supertag-setup--status-report ()
  "Collect a plist describing the current Org-Supertag configuration."
  (let ((db-file (supertag-setup--db-file)))
    (list :directories (supertag-setup--current-directories)
          :file-id-source (supertag-setup--current-file-id-source)
          :db-file db-file
          :db-exists (and db-file (file-exists-p db-file))
          :node-count (supertag-setup--node-count))))

(defun supertag-setup--report-status (status)
  "Print a human-readable summary of STATUS via `message'."
  (let ((dirs (plist-get status :directories))
        (file-id (plist-get status :file-id-source))
        (db-file (plist-get status :db-file))
        (db-exists (plist-get status :db-exists))
        (node-count (plist-get status :node-count)))
    (message "Org-Supertag setup -- current status:\n  Sync directories : %s\n  File-ID source   : %s\n  Database file    : %s (%s)\n  Nodes loaded     : %s"
             (if dirs (mapconcat #'abbreviate-file-name dirs ", ") "(not configured)")
             file-id
             (if db-file (abbreviate-file-name db-file) "(unknown)")
             (if db-exists "exists" "not created yet")
             (or node-count "none loaded"))))

;;; --- Step: sync directories --------------------------------------------

(defun supertag-setup--read-directories (current)
  "Interactively build a list of sync directories.
CURRENT pre-fills the default for the first prompt.  Each candidate is
validated with `file-directory-p'; invalid entries are rejected and the
user is asked whether to try again.  Returns the resulting list, or nil
if the user adds nothing."
  (let ((dirs nil)
        (default (or (car current) default-directory))
        (keep-going t))
    (while keep-going
      (let* ((prompt (if dirs
                          "Add another sync directory (C-g to stop adding): "
                        "Sync directory to watch: "))
             (input (expand-file-name (read-directory-name prompt default default))))
        (cond
         ((not (file-directory-p input))
          (message "Not a directory, skipping: %s" (abbreviate-file-name input))
          (setq keep-going (y-or-n-p "Try adding another directory? ")))
         ((member input dirs)
          (message "Already added: %s" (abbreviate-file-name input))
          (setq keep-going (y-or-n-p "Add another directory? ")))
         (t
          (push input dirs)
          (setq default input)
          (setq keep-going (y-or-n-p "Directory added. Add another? "))))))
    (nreverse dirs)))

;;; --- Step: file-ID source -----------------------------------------------

(defun supertag-setup--parse-const-branch (branch)
  "Return (VALUE . TAG) for a `(const :tag TAG VALUE)' custom-type BRANCH."
  (when (and (consp branch) (eq (car branch) 'const))
    (let ((plist (cdr branch))
          (tag nil))
      (while (keywordp (car plist))
        (when (eq (car plist) :tag)
          (setq tag (cadr plist)))
        (setq plist (cddr plist)))
      (cons (car plist) (or tag (format "%s" (car plist)))))))

(defun supertag-setup--file-id-source-choices ()
  "Return an alist of (VALUE . TAG) for `org-supertag-file-id-source'.
The list is read from the defcustom's own `:type' so it always reflects
the real set of valid options; if that shape ever changes underneath us,
fall back to the documented set."
  (let* ((type (get 'org-supertag-file-id-source 'custom-type))
         (choices (when (and (consp type) (eq (car type) 'choice))
                    (delq nil (mapcar #'supertag-setup--parse-const-branch
                                       (cdr type))))))
    (or choices
        '((org-roam . "org-roam (:PROPERTIES: :ID:)")
          (denote . "denote (#+IDENTIFIER:)")
          (auto . "auto-detect either identity")
          (disabled . "disable file nodes")))))

(defun supertag-setup--read-file-id-source (current)
  "Prompt for a file-ID source, defaulting to CURRENT."
  (let* ((choices (supertag-setup--file-id-source-choices))
         (labels (mapcar (lambda (c) (format "%s -- %s" (car c) (cdr c))) choices))
         (default-label (or (cl-find-if
                             (lambda (l) (string-prefix-p (format "%s " current) l))
                             labels)
                            (car labels)))
         (chosen (completing-read
                  (format "File-ID source [default %s]: " current)
                  labels nil t nil nil default-label))
         (value (car (nth (or (cl-position chosen labels :test #'equal) 0) choices))))
    (or value current)))

;;; --- Step: persistence ---------------------------------------------------

(defun supertag-setup--customize-save-possible-p ()
  "Return non-nil if `customize-save-variable' can write to disk.
Mirrors the precondition Emacs's own `custom-file' function applies:
customizations cannot be saved when Emacs was started without loading an
init file (e.g. `emacs -q'), even if `custom-file' happens to be set, so
we also require `user-init-file' to be non-nil."
  (and user-init-file
       (let ((file (or custom-file user-init-file)))
         (and (stringp file)
              (not (string-empty-p file))
              (if (file-exists-p file)
                  (file-writable-p file)
                (let ((dir (file-name-directory file)))
                  (and dir (file-writable-p dir))))))))

(defun supertag-setup--snippet-text (dirs file-id-source)
  "Return a copy-pastable Elisp snippet for DIRS and FILE-ID-SOURCE."
  (concat
   ";; Org-Supertag configuration snippet\n"
   ";; Paste into your init file, or adapt for `use-package''s :custom.\n\n"
   (if dirs
       (format "(setq org-supertag-sync-directories\n      '(%s))\n\n"
               (mapconcat (lambda (d) (format "%S" d)) dirs "\n        "))
     ";; No sync directories were configured.\n\n")
   (format "(setq org-supertag-file-id-source '%s)\n" file-id-source)))

(defun supertag-setup--show-snippet (text)
  "Display TEXT in the *supertag-setup* buffer."
  (let ((buf (get-buffer-create "*supertag-setup*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert text)
      (when (fboundp 'emacs-lisp-mode)
        (emacs-lisp-mode))
      (goto-char (point-min)))
    (unless noninteractive
      (display-buffer buf))
    buf))

(defun supertag-setup--persist (dirs file-id-source)
  "Apply DIRS and FILE-ID-SOURCE for this session, then persist per user choice.
Returns the chosen method: `save', `session', or `snippet'."
  (let* ((can-save (supertag-setup--customize-save-possible-p))
         (options (if can-save
                      '(("Save permanently (customize-save-variable)" . save)
                        ("Set for this Emacs session only" . session)
                        ("Just show me an Elisp snippet to paste myself" . snippet))
                    '(("Set for this Emacs session only" . session)
                      ("Just show me an Elisp snippet to paste myself" . snippet))))
         (prompt (if can-save
                     "How should these settings be persisted? "
                   "No writable custom-file/init file was found, so permanent save is unavailable. Apply how? "))
         (default-label (caar options))
         (choice-label (completing-read prompt (mapcar #'car options) nil t nil nil default-label))
         (choice (cdr (assoc choice-label options))))
    ;; Always apply for the current session so a following scan step (and
    ;; the rest of this Emacs session) sees the new configuration.
    (setq org-supertag-sync-directories dirs)
    (setq org-supertag-file-id-source file-id-source)
    (cond
     ((eq choice 'save)
      (customize-save-variable 'org-supertag-sync-directories dirs)
      (customize-save-variable 'org-supertag-file-id-source file-id-source)
      (message "Saved permanently to %s" (abbreviate-file-name (or custom-file user-init-file))))
     ((eq choice 'session)
      (message "Applied for this Emacs session only (not written to disk)."))
     ((eq choice 'snippet)
      (supertag-setup--show-snippet (supertag-setup--snippet-text dirs file-id-source))
      (message "Applied for this session, and wrote a copy-pastable snippet to *supertag-setup*.")))
    choice))

;;; --- Step: initial scan ----------------------------------------------------

(defun supertag-setup--run-scan (scan-fn label)
  "Run SCAN-FN (a symbol) if bound, reporting node counts before/after.
LABEL names the operation for the confirmation prompt."
  (if (not (fboundp scan-fn))
      (message "%s is not available in this build; run it manually once Org-Supertag is fully loaded." scan-fn)
    (when (y-or-n-p (format "Run %s now? This may take a while on large vaults. " label))
      (let ((before (or (supertag-setup--node-count) 0)))
        (when (fboundp 'supertag-persistence-ensure-data-directory)
          (supertag-persistence-ensure-data-directory))
        (funcall scan-fn)
        (let ((after (or (supertag-setup--node-count) 0)))
          (message "%s complete. Nodes before: %d, after: %d" label before after))))))

;;; --- Step: finish ----------------------------------------------------------

(defun supertag-setup--doc-path ()
  "Return the path to the \"A Day with Org-SuperTag\" walkthrough, if found."
  (let ((lib (locate-library "org-supertag")))
    (when lib
      (let ((path (expand-file-name "doc/A-DAY-WITH-ORG-SUPERTAG.org" (file-name-directory lib))))
        (and (file-exists-p path) path)))))

(defun supertag-setup--finish ()
  "Print a closing message pointing at further reading and next steps."
  (let ((doc (supertag-setup--doc-path))
        (entry-point (if (fboundp 'supertag-menu) "M-x supertag-menu" "M-x supertag-view-table")))
    (message "Org-Supertag setup finished. Explore your data with %s.%s"
             entry-point
             (if doc
                 (format "  For a guided tour, read \"A Day with Org-SuperTag\": %s" (abbreviate-file-name doc))
               ""))))

;;; --- Already-configured branch ------------------------------------------

(defun supertag-setup--offer-reconfigure-or-rescan ()
  "Ask an already-configured user what to do next.
Returns `reconfigure', `rescan', or `skip'."
  (let* ((options '("Just rescan with the current configuration"
                    "Reconfigure (directories / file-ID source / persistence)"
                    "Nothing -- I only wanted to see the status"))
         (choice (completing-read
                  "Org-Supertag is already configured. What would you like to do? "
                  options nil t nil nil (car options))))
    (cond
     ((string-prefix-p "Just rescan" choice) 'rescan)
     ((string-prefix-p "Reconfigure" choice) 'reconfigure)
     (t 'skip))))

;;; --- Entry point -----------------------------------------------------------

(defun supertag-setup--run-configure (status)
  "Run the configure-then-scan portion of the wizard for STATUS.
Prompts for sync directories and file-ID source, persists the choice,
optionally runs the initial scan, and finishes."
  (let* ((current-dirs (plist-get status :directories))
         (current-file-id (plist-get status :file-id-source))
         (new-dirs (supertag-setup--read-directories current-dirs))
         (new-file-id (supertag-setup--read-file-id-source current-file-id)))
    (supertag-setup--persist new-dirs new-file-id)
    (supertag-setup--run-scan 'supertag-sync-full-initialize "supertag-sync-full-initialize")
    (supertag-setup--finish)))

(defun supertag-setup--run ()
  "Run the wizard body.  See `supertag-setup'."
  (let* ((status (supertag-setup--status-report))
         (configured-p (and (plist-get status :directories) t)))
    (supertag-setup--report-status status)
    (if (not configured-p)
        (supertag-setup--run-configure status)
      (let ((action (supertag-setup--offer-reconfigure-or-rescan)))
        (cond
         ((eq action 'rescan)
          (supertag-setup--run-scan 'supertag-sync-full-rescan "supertag-sync-full-rescan")
          (supertag-setup--finish))
         ((eq action 'reconfigure)
          (supertag-setup--run-configure status))
         (t
          (supertag-setup--finish)))))))

;;;###autoload
(defun supertag-setup ()
  "Interactive first-run configuration wizard for Org-Supertag.

Walks through:
  1. Showing the current status (sync directories, file-ID source,
     database file, node count).  If already configured, offers to just
     rescan instead of reconfiguring from scratch.
  2. Choosing sync directories (`org-supertag-sync-directories').
  3. Choosing a file-ID source (`org-supertag-file-id-source').
  4. Persisting those choices: permanently via `customize-save-variable',
     for this session only, or as a copy-pastable Elisp snippet.
  5. Optionally running the initial database scan.
  6. Pointing at `\\[supertag-menu]' (or `\\[supertag-view-table]') and the
     \"A Day with Org-SuperTag\" walkthrough.

Every step is skippable, and nothing is written to a live variable until
step 4; hitting \\`C-g\\' before then leaves your configuration untouched.
The wizard is safe to run more than once."
  (interactive)
  (condition-case nil
      (supertag-setup--run)
    (quit (message "Org-Supertag setup cancelled -- no changes were made.") nil)))

(provide 'supertag-setup)

;;; supertag-setup.el ends here

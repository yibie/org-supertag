;; -*- lexical-binding: t; -*-
;;; org-supertag-util.el --- Utility functions for org-supertag -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Your Name <your.email@example.com>
;; Keywords: org-mode, utilities

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains utility functions for org-supertag.

;;; Code:

(require 'cl-lib)
(require 'org)

(defgroup org-supertag-util nil
  "Utility functions for org-supertag."
  :group 'org-supertag)

(defun org-supertag-util-ensure-directory (dir)
  "Make sure directory DIR exists, create it if it doesn't."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun org-supertag-util-get-data-dir ()
  "Get org-supertag data directory."
  (let ((data-dir (expand-file-name "org-supertag" user-emacs-directory)))
    (org-supertag-util-ensure-directory data-dir)
    data-dir))

(defun org-supertag-util-get-hash-file ()
  "Get hash file path."
  (expand-file-name "hashes.el" (org-supertag-util-get-data-dir)))

(defun org-supertag-util-save-hash (node-id hash content-hash)
  "Save node NODE-ID hash value HASH and content hash value CONTENT-HASH."
  (let* ((hash-file (org-supertag-util-get-hash-file))
         (hash-data (if (file-exists-p hash-file)
                       (with-temp-buffer
                         (insert-file-contents hash-file)
                         (read (buffer-string)))
                     (make-hash-table :test 'equal)))
         (node-data (make-hash-table :test 'equal)))
    (puthash 'node-hash hash node-data)
    (puthash 'content-hash content-hash node-data)
    (puthash node-id node-data hash-data)
    (with-temp-file hash-file
      (prin1 hash-data (current-buffer)))))

(defun org-supertag-util-load-hash (node-id)
  "Load node NODE-ID hash value.
Return a hash table containing node-hash and content-hash, or nil if not found."
  (let* ((hash-file (org-supertag-util-get-hash-file)))
    (when (file-exists-p hash-file)
      (let* ((hash-data (with-temp-buffer
                         (insert-file-contents hash-file)
                         (read (buffer-string)))))
        (gethash node-id hash-data)))))

(defun org-supertag-util-clear-hash ()
  "Clear all hash records."
  (let ((hash-file (org-supertag-util-get-hash-file)))
    (when (file-exists-p hash-file)
      (delete-file hash-file))))

(defun org-supertag-util-get-node-content ()
  "Get current node's title and content.
Skip property drawer and leading metadata, start from the main content.
Return a string containing node title and content."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (let* ((title (org-get-heading t t t t))  ; Get clean title (no TODO/tags/priority)
             (content-start (save-excursion
                            (org-end-of-meta-data t)  ; Skip property drawer and other metadata
                            (point)))
             (content-end (point-max))
             (raw-content (buffer-substring-no-properties content-start content-end))
             (no-drawer-content (if (fboundp 'org-supertag-db--remove-drawers-from-content)
                                   (org-supertag-db--remove-drawers-from-content raw-content)
                                 raw-content))
             ;; Clean content: remove extra empty lines, trim whitespace
             (cleaned-content (replace-regexp-in-string
                             "\\([\\n]\\)[\\n]+" "\\1"  ; Replace multiple empty lines with single empty line
                             (string-trim no-drawer-content))))
         ;; Combine title and content
        (concat "# " title "\n\n" cleaned-content))))

(defun org-plist-delete (plist prop)
  "Delete property PROP from PLIST.
Returns a new plist with the property removed."
  (let (result)
    (while plist
      (unless (eq (car plist) prop)
        (setq result (cons (cadr plist) (cons (car plist) result))))
      (setq plist (cddr plist)))
    (nreverse result))))

(provide 'org-supertag-util)

;;; org-supertag-util.el ends here 
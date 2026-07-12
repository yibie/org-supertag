;;; add-reference-test.el --- ERT tests for supertag-add-reference -*- lexical-binding: t -*-

;;; Commentary:

;; Tests that `supertag-add-reference' inserts the forward link at the
;; original cursor position even when the reciprocal backlink is inserted
;; earlier in the same file (which shifts text positions).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-id)

(when load-file-name
  ;; This file lives in test/; add the project root (its parent) to
  ;; load-path so the `require' calls below can find sibling modules
  ;; even when invoked without an explicit `-L .' flag.
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-ops-node)
(require 'supertag-ops-relation)
(require 'supertag-ui-commands)

;; File node support reads this variable from org-supertag.el, which pulls
;; in UI dependencies we don't need for tests.
(defvar org-supertag-file-id-source 'org-roam)

(defmacro add-reference-test--with-clean-env (&rest body)
  "Run BODY with a clean isolated store and temp directory."
  (declare (indent 0))
  `(let* ((tmp (make-temp-file "supertag-add-reference-test" t))
          (supertag-data-directory tmp)
          (supertag-db-file (expand-file-name "supertag-db.el" tmp))
          (supertag-db-backup-directory (expand-file-name "backups" tmp))
          (supertag--store nil)
          (supertag--store-origin nil)
          (org-id-locations nil)
          (org-id-files nil))
     (unwind-protect
         (progn
           (supertag--ensure-store)
           ,@body)
       (ignore-errors
         (delete-directory tmp t)))))

(ert-deftest add-reference-inserts-at-cursor-after-backlink-shift ()
  "Forward link is inserted at cursor even if backlink shifts text."
  (add-reference-test--with-clean-env
    (let* ((test-file (expand-file-name "test.org" supertag-data-directory))
           (source-title "Source Node")
           (target-title "Target Node"))
      ;; Create a file where the target node appears BEFORE the source node.
      (with-temp-file test-file
        (org-mode)
        (insert (format "* %s\n:PROPERTIES:\n:ID:       target-id\n:END:\n\nTarget content.\n\n* %s\n:PROPERTIES:\n:ID:       source-id\n:END:\n\nSource content before cursor. Cursor here. After cursor.\n"
                        target-title source-title)))
      ;; Visit the file and register IDs with org-id.
      (with-current-buffer (find-file-noselect test-file)
        (org-mode)
        (org-id-update-id-locations nil t)
        ;; Sync both nodes into the store.
        (goto-char (point-min))
        (org-back-to-heading t)
        (supertag-node-sync-at-point)
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (supertag-node-sync-at-point)
        ;; Ensure source node exists in store.
        (should (supertag-node-get "source-id"))
        (should (supertag-node-get "target-id"))

        ;; Place cursor in source content at a known position.
        (goto-char (point-min))
        (re-search-forward "Cursor here" nil t)
        ;; Mock node selection to return the target.
        (cl-letf (((symbol-function 'supertag-ui-select-node)
                   (lambda (&rest _) "target-id")))
          (condition-case err
              (supertag-add-reference)
            (error (signal (car err) (cdr err)))))
        ;; Verify forward link was inserted exactly at the original cursor.
        (goto-char (point-min))
        (re-search-forward "Cursor here" nil t)
        (should (looking-at-p "\\[\\[id:target-id\\]\\["))
        ;; Verify reciprocal backlink was inserted in target content.
        (goto-char (point-min))
        (should (re-search-forward "\\[\\[id:source-id\\]\\[" nil t))))))

(ert-deftest add-reference-backlink-includes-timestamp-when-enabled ()
  "When `supertag-reference-backlink-include-timestamp' is t, backlink carries a timestamp."
  (add-reference-test--with-clean-env
    (let* ((test-file (expand-file-name "test.org" supertag-data-directory))
           (supertag-reference-backlink-include-timestamp t)
           (source-title "Source Node")
           (target-title "Target Node"))
      ;; Create a file where the target node appears BEFORE the source node.
      (with-temp-file test-file
        (org-mode)
        (insert (format "* %s\n:PROPERTIES:\n:ID:       target-id\n:END:\n\nTarget content.\n\n* %s\n:PROPERTIES:\n:ID:       source-id\n:END:\n\nSource content before cursor. Cursor here. After cursor.\n"
                        target-title source-title)))
      ;; Visit the file and register IDs with org-id.
      (with-current-buffer (find-file-noselect test-file)
        (org-mode)
        (org-id-update-id-locations nil t)
        ;; Sync both nodes into the store.
        (goto-char (point-min))
        (org-back-to-heading t)
        (supertag-node-sync-at-point)
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (supertag-node-sync-at-point)
        ;; Place cursor in source content at a known position.
        (goto-char (point-min))
        (re-search-forward "Cursor here" nil t)
        ;; Mock node selection to return the target.
        (cl-letf (((symbol-function 'supertag-ui-select-node)
                   (lambda (&rest _) "target-id")))
          (condition-case err
              (supertag-add-reference)
            (error (signal (car err) (cdr err)))))
        ;; Verify reciprocal backlink includes a timestamp after the link.
        (goto-char (point-min))
        (should (re-search-forward
                 "\\[\\[id:source-id\\]\\[Source Node\\]\\] \\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\]"
                 nil t))))))

(ert-deftest add-reference-file-node-as-source ()
  "A file node can be the source of a reference to a heading."
  (add-reference-test--with-clean-env
    (let* ((test-file (expand-file-name "test.org" supertag-data-directory)))
      (with-temp-file test-file
        (org-mode)
        (insert ":PROPERTIES:\n:ID:       file-id\n:END:\n#+TITLE: Test File\n\n* Target Heading\n:PROPERTIES:\n:ID:       target-id\n:END:\n\nTarget content.\n"))
      (with-current-buffer (find-file-noselect test-file)
        (org-mode)
        (org-id-update-id-locations nil t)
        ;; Sync file node manually (test file is outside normal sync scope).
        (goto-char (point-min))
        (let ((file-header (supertag-sync--parse-file-header))
              (counters '(:nodes-created 0 :nodes-updated 0 :nodes-deleted 0
                          :references-created 0 :references-deleted 0)))
          (supertag-sync--upsert-file-node test-file file-header counters))
        (outline-next-visible-heading 1)
        (supertag-node-sync-at-point)
        ;; Place cursor at file level, before any heading.
        (goto-char (point-min))
        (re-search-forward "Test File" nil t)
        (end-of-line)
        ;; Mock node selection to return the heading.
        (cl-letf (((symbol-function 'supertag-ui-select-node)
                   (lambda (&rest _) "target-id")))
          (condition-case err
              (supertag-add-reference)
            (error (signal (car err) (cdr err)))))
        ;; Forward link should be inserted at file level.
        (goto-char (point-min))
        (should (re-search-forward "\\[\\[id:target-id\\]\\[" nil t))
        ;; Reciprocal backlink should be in heading content.
        (goto-char (point-min))
        (outline-next-visible-heading 1)
        (should (re-search-forward "\\[\\[id:file-id\\]\\[" nil t))))))

(ert-deftest add-reference-file-node-as-target ()
  "A heading can reference a file node, and the backlink goes to file metadata area."
  (add-reference-test--with-clean-env
    (let* ((test-file (expand-file-name "test.org" supertag-data-directory)))
      (with-temp-file test-file
        (org-mode)
        (insert ":PROPERTIES:\n:ID:       file-id\n:END:\n#+TITLE: Test File\n\n* Source Heading\n:PROPERTIES:\n:ID:       source-id\n:END:\n\nSource content.\n"))
      (with-current-buffer (find-file-noselect test-file)
        (org-mode)
        (org-id-update-id-locations nil t)
        ;; Sync file node manually (test file is outside normal sync scope).
        (goto-char (point-min))
        (let ((file-header (supertag-sync--parse-file-header))
              (counters '(:nodes-created 0 :nodes-updated 0 :nodes-deleted 0
                          :references-created 0 :references-deleted 0)))
          (supertag-sync--upsert-file-node test-file file-header counters))
        (outline-next-visible-heading 1)
        (supertag-node-sync-at-point)
        ;; Place cursor in heading content.
        (goto-char (point-min))
        (re-search-forward "Source content" nil t)
        ;; Mock node selection to return the file node.
        (cl-letf (((symbol-function 'supertag-ui-select-node)
                   (lambda (&rest _) "file-id")))
          (condition-case err
              (supertag-add-reference)
            (error (signal (car err) (cdr err)))))
        ;; Forward link should be in heading content.
        (goto-char (point-min))
        (outline-next-visible-heading 1)
        (should (re-search-forward "\\[\\[id:file-id\\]\\[" nil t))
        ;; Reciprocal backlink should be after file metadata and before heading.
        (goto-char (point-min))
        (re-search-forward "#\\+IDENTIFIER: file-id" nil t)
        (end-of-line)
        (should (re-search-forward "\\[\\[id:source-id\\]\\["
                                  (save-excursion
                                    (goto-char (point-min))
                                    (re-search-forward "\\* Source Heading" nil t))
                                  t))))))

(provide 'add-reference-test)
;;; add-reference-test.el ends here

;;; org-supertag-parser-test.el --- Tests for org-supertag-parser -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for org-supertag-parser.el

;;; Code:

(require 'ert)
(require 'org)
(require 'org-supertag-parser)

(defvar org-supertag-parser-test-directory
  (expand-file-name "test-data"
                   (file-name-directory
                    (or load-file-name buffer-file-name)))
  "Directory for test files.")

(defvar org-supertag-parser-test-file
  (expand-file-name "sample.org" org-supertag-parser-test-directory)
  "Test org file path.")

(defun org-supertag-parser--delete-directory-recursive (dir)
  "Delete directory DIR and its contents recursively."
  (when (file-exists-p dir)
    (dolist (file (directory-files dir t))
      (unless (member (file-name-nondirectory file) '("." ".."))
        (if (file-directory-p file)
            (org-supertag-parser--delete-directory-recursive file)
          (delete-file file))))
    (delete-directory dir)))

(defun org-supertag-parser-test-setup ()
  "Setup test environment."
  ;; Clean up any existing test directory
  (when (file-exists-p org-supertag-parser-test-directory)
    (org-supertag-parser--delete-directory-recursive org-supertag-parser-test-directory))
  ;; Create fresh test directory
  (make-directory org-supertag-parser-test-directory t)
  ;; Create test file
  (with-temp-file org-supertag-parser-test-file
    (insert "* First Heading
:PROPERTIES:
:ID: test-id-1
:CUSTOM_ID: first
:END:
Some content here.

* TODO Second Heading [#A]                                              :tag1:tag2:
:PROPERTIES:
:ID: test-id-2
:CUSTOM_ID: second
:END:
Content with a [[id:test-id-1][link]] to first heading.
And another [[id:test-id-3][link]] to sub-heading.

** DONE Sub-heading
:PROPERTIES:
:ID: test-id-3
:END:
More content here with a [[id:test-id-1][back link]] to first heading.

* Third Heading with Multiple References
:PROPERTIES:
:ID: test-id-4
:END:
This heading has multiple references:
- [[id:test-id-1][First]]
- [[id:test-id-2][Second]]
- [[id:test-id-3][Third]]
And some duplicate references:
- [[id:test-id-1][First Again]]
- [[id:test-id-2][Second Again]]")))

(defun org-supertag-parser-test-cleanup ()
  "Clean up test environment."
  ;; Kill test buffer if it exists
  (when-let ((buf (find-buffer-visiting org-supertag-parser-test-file)))
    (kill-buffer buf))
  ;; Remove test directory and contents
  (when (file-exists-p org-supertag-parser-test-directory)
    (org-supertag-parser--delete-directory-recursive org-supertag-parser-test-directory)))

(ert-deftest org-supertag-parser-test-parse-node ()
  "Test parsing a single node."
  (org-supertag-parser-test-setup)
  (unwind-protect
      (with-current-buffer (find-file-noselect org-supertag-parser-test-file)
        ;; Test first heading
        (goto-char (point-min))
        (let ((node (org-supertag-parser-parse-node)))
          (should (equal (plist-get node :title) "First Heading"))
          (should (equal (plist-get node :level) 1))
          (should (equal (plist-get node :id) "test-id-1"))
          (should (equal (plist-get node :todo) nil))
          (should (equal (plist-get node :tags) nil))
          (should (string-match "Some content here" (plist-get node :content)))
          ;; No references in first heading
          (should (equal (plist-get node :ref-to) nil))
          (should (equal (plist-get node :ref-count) 0)))

        ;; Test second heading
        (search-forward "* TODO")
        (beginning-of-line)
        (let ((node (org-supertag-parser-parse-node)))
          (should (equal (plist-get node :title) "Second Heading"))
          (should (equal (plist-get node :level) 1))
          (should (equal (plist-get node :id) "test-id-2"))
          (should (equal (plist-get node :todo) "TODO"))
          (should (equal (plist-get node :priority) "[#A]"))
          (should (equal (plist-get node :tags) '("tag1" "tag2")))
          ;; Test references
          (should (equal (plist-get node :ref-to) '("test-id-1" "test-id-3")))
          (should (equal (plist-get node :ref-count) 2))))
    (org-supertag-parser-test-cleanup)))

(ert-deftest org-supertag-parser-test-parse-references ()
  "Test parsing node references."
  (org-supertag-parser-test-setup)
  (unwind-protect
      (with-current-buffer (find-file-noselect org-supertag-parser-test-file)
        ;; Test multiple references
        (search-forward "* Third Heading")
        (beginning-of-line)
        (let* ((node (org-supertag-parser-parse-node))
               (refs (plist-get node :ref-to)))
          ;; Should have unique references
          (should (equal (length refs) 3))
          ;; Should contain all referenced IDs
          (should (member "test-id-1" refs))
          (should (member "test-id-2" refs))
          (should (member "test-id-3" refs))
          ;; Should have correct ref count
          (should (equal (plist-get node :ref-count) 3))))
    (org-supertag-parser-test-cleanup)))

(ert-deftest org-supertag-parser-test-parse-file ()
  "Test parsing an entire file."
  (org-supertag-parser-test-setup)
  (unwind-protect
      (let* ((result (org-supertag-parser-parse-file org-supertag-parser-test-file))
             (nodes (car result))
             (links (cdr result)))
        ;; Check nodes
        (should (= (length nodes) 4))
        ;; Check first node
        (let ((node1 (nth 0 nodes)))
          (should (equal (plist-get node1 :title) "First Heading"))
          (should (equal (plist-get node1 :level) 1)))
        ;; Check second node
        (let ((node2 (nth 1 nodes)))
          (should (equal (plist-get node2 :title) "Second Heading"))
          (should (equal (plist-get node2 :todo) "TODO")))
        ;; Check third node
        (let ((node3 (nth 2 nodes)))
          (should (equal (plist-get node3 :title) "Sub-heading"))
          (should (equal (plist-get node3 :level) 2))
          (should (equal (plist-get node3 :todo) "DONE")))
        ;; Check fourth node (multiple references)
        (let ((node4 (nth 3 nodes)))
          (should (equal (plist-get node4 :title) "Third Heading with Multiple References"))
          (should (equal (length (plist-get node4 :ref-to)) 3)))
        ;; Check links
        (should links)
        (should (cl-some (lambda (link)
                          (and (eq (plist-get link :type) :link)
                               (equal (plist-get link :link-type) "id")))
                        links)))
    (org-supertag-parser-test-cleanup)))

(provide 'org-supertag-parser-test)
;;; org-supertag-parser-test.el ends here 
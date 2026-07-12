;;; file-node-display-test.el --- ERT tests for file node display formatting -*- lexical-binding: t -*-

;;; Commentary:

;; Regression tests for `supertag-ui--format-node-display' ensuring that
;; file nodes (level 0) are visually distinguishable and fall back to the
;; file name when they have no title.

;;; Code:

(require 'ert)

(when load-file-name
  ;; This file lives in test/; add the project root (its parent) to
  ;; load-path so the `require' calls below can find sibling modules
  ;; even when invoked without an explicit `-L .' flag.
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-services-ui)

(ert-deftest format-file-node-with-title ()
  "File node with #+TITLE shows emoji prefix and title."
  (let ((display (supertag-ui--format-node-display
                  '(:id "file-id"
                    :level 0
                    :title "My Notes"
                    :file "/home/user/notes.org"))))
    (should (string-prefix-p "📄 " display))
    (should (string-match-p "My Notes" display))
    (should (string-suffix-p "  (in notes.org)" display))))

(ert-deftest format-file-node-without-title-uses-filename ()
  "File node without title falls back to file name."
  (let ((display (supertag-ui--format-node-display
                  '(:id "file-id"
                    :level 0
                    :file "/home/user/diary.org"))))
    (should (string-prefix-p "📄 " display))
    (should (string-match-p "diary.org" display))
    (should (string-suffix-p "  (in diary.org)" display))))

(ert-deftest format-file-node-with-raw-value-prefers-raw-value ()
  "File node prefers :raw-value over :title when present."
  (let ((display (supertag-ui--format-node-display
                  '(:id "file-id"
                    :level 0
                    :title "Title"
                    :raw-value "Raw Value"
                    :file "/home/user/notes.org"))))
    (should (string-match-p "Raw Value" display))
    (should (not (string-match-p "Title" display)))))

(ert-deftest format-heading-node-has-no-prefix ()
  "Heading nodes do not get the file-node prefix."
  (let ((display (supertag-ui--format-node-display
                  '(:id "heading-id"
                    :level 1
                    :title "My Heading"
                    :file "/home/user/notes.org"))))
    (should (not (string-prefix-p "📄 " display)))
    (should (string-prefix-p "My Heading" display))
    (should (string-suffix-p "  (in notes.org)" display))))

(ert-deftest format-heading-node-with-olp ()
  "Heading nodes with outline path display parent / child."
  (let ((display (supertag-ui--format-node-display
                  '(:id "heading-id"
                    :level 2
                    :title "Child"
                    :olp ("Parent" "Child")
                    :file "/home/user/notes.org"))))
    (should (string-equal display "Parent / Child  (in notes.org)"))))

(ert-deftest format-orphaned-heading ()
  "Heading node without :file shows orphaned marker."
  (let ((display (supertag-ui--format-node-display
                  '(:id "orphan-id"
                    :level 1
                    :title "Orphaned Heading"))))
    (should (string-suffix-p "  [orphaned]" display))
    (should (not (string-match-p "📄" display)))))

(ert-deftest format-orphaned-file-node ()
  "File node without :file and without title shows Untitled with prefix."
  (let ((display (supertag-ui--format-node-display
                  '(:id "orphan-file-id"
                    :level 0))))
    (should (string-prefix-p "📄 " display))
    (should (string-match-p "Untitled" display))
    (should (string-suffix-p "  [orphaned]" display))))

(ert-deftest build-node-candidates-includes-file-node ()
  "`supertag-ui--build-node-candidates' includes file nodes with emoji prefix."
  (let* ((supertag--store nil)
         (supertag-db-file nil)
         (node (supertag-node-create
                '(:id "file-id"
                  :level 0
                  :title "My File"
                  :file "/tmp/test.org")))
         (candidates (supertag-ui--build-node-candidates))
         (display (car (assoc "📄 My File  (in test.org)" candidates))))
    (should (= (length candidates) 1))
    (should display)
    (should (string-equal (cdr (assoc display candidates)) "file-id"))))

(provide 'file-node-display-test)
;;; file-node-display-test.el ends here

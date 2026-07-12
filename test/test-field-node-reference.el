;;; test-field-node-reference.el --- ERT tests for :node-reference field side effects -*- lexical-binding: t -*-

;;; Commentary:

;; Regression tests ensuring that `supertag-field-set' automatically
;; maintains :reference relations and reciprocal backlinks for fields of
;; type :node-reference.  Callers should not need to know the field type.

;;; Code:

(require 'ert)
(require 'cl-lib)

(when load-file-name
  ;; This file lives in test/; add the project root (its parent) to
  ;; load-path so the `require' calls below can find sibling modules
  ;; even when invoked without an explicit `-L .' flag.
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-ops-node)
(require 'supertag-ops-tag)
(require 'supertag-ops-relation)
(require 'supertag-ops-field)
(require 'supertag-ui-commands)

(defmacro field-node-reference-test--with-env (&rest body)
  "Run BODY with a clean store and two synced nodes in a temp Org file."
  (declare (indent 0))
  `(let* ((tmp (make-temp-file "supertag-field-ref-test" t))
          (supertag-data-directory tmp)
          (supertag-db-file (expand-file-name "supertag-db.el" tmp))
          (supertag-db-backup-directory (expand-file-name "backups" tmp))
          (supertag--store nil)
          (supertag--store-origin nil)
          (org-id-locations nil)
          (org-id-files nil)
          (test-file (expand-file-name "test.org" tmp)))
     (unwind-protect
         (progn
           (supertag--ensure-store)
           (with-temp-file test-file
             (org-mode)
             (insert "* Source\n:PROPERTIES:\n:ID: source-id\n:END:\n\nSource content.\n\n* Target\n:PROPERTIES:\n:ID: target-id\n:END:\n\nTarget content.\n"))
           (with-current-buffer (find-file-noselect test-file)
             (org-mode)
             (org-id-update-id-locations nil t)
             (goto-char (point-min))
             (org-back-to-heading t)
             (supertag-node-sync-at-point)
             (org-next-visible-heading 1)
             (supertag-node-sync-at-point)
             (set-buffer-modified-p nil))
           (let ((source-id "source-id")
                 (target-id "target-id"))
             ,@body))
       (ignore-errors
         (delete-directory tmp t)))))

(defun field-node-reference-test--tag-with-ref-field ()
  "Create and return a tag with a :node-reference field."
  (supertag-tag-create
   (list :id "ref-tag"
         :name "Reference Tag"
         :fields (list (list :name "ref"
                             :type :node-reference
                             :required nil)))))

(ert-deftest field-set-node-reference-creates-relation-and-backlink ()
  "Setting a :node-reference field creates a relation and reciprocal backlink."
  (field-node-reference-test--with-env
    (field-node-reference-test--tag-with-ref-field)
    (supertag-node-add-tag source-id "ref-tag")
    (supertag-field-set source-id "ref-tag" "ref" target-id)
    ;; Relation exists.
    (let ((rels (supertag-relation-find-between source-id target-id :reference)))
      (should (= 1 (length rels))))
    ;; Backlink exists in target's content.
    (with-current-buffer (find-file-noselect test-file)
      (org-with-wide-buffer
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-end-of-meta-data t)
        (should (re-search-forward (format "\\[\\[id:%s\\]\\[Source\\]\\]" source-id) nil t))))))

(ert-deftest field-set-node-reference-removes-relation-and-backlink ()
  "Clearing a :node-reference field removes the relation and backlink."
  (field-node-reference-test--with-env
    (field-node-reference-test--tag-with-ref-field)
    (supertag-node-add-tag source-id "ref-tag")
    ;; Set, then clear.
    (supertag-field-set source-id "ref-tag" "ref" target-id)
    (supertag-field-set source-id "ref-tag" "ref" nil)
    ;; Relation removed.
    (should (null (supertag-relation-find-between source-id target-id :reference)))
    ;; Backlink removed from target's content.
    (with-current-buffer (find-file-noselect test-file)
      (org-with-wide-buffer
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-end-of-meta-data t)
        (should-not (re-search-forward (format "\\[\\[id:%s\\]" source-id) nil t))))))

(ert-deftest field-set-node-reference-swap-target ()
  "Changing the target of a :node-reference field updates relation/backlink."
  (field-node-reference-test--with-env
    (field-node-reference-test--tag-with-ref-field)
    ;; Create a second target in the same file.
    (with-current-buffer (find-file-noselect test-file)
      (goto-char (point-max))
      (insert "\n* Target2\n:PROPERTIES:\n:ID: target2-id\n:END:\n\nTarget2 content.\n")
      (save-buffer)
      (org-id-update-id-locations nil t)
      (goto-char (point-min))
      (re-search-forward "target2-id" nil t)
      (org-back-to-heading t)
      (supertag-node-sync-at-point))
    (supertag-node-add-tag source-id "ref-tag")
    ;; First target.
    (supertag-field-set source-id "ref-tag" "ref" target-id)
    ;; Swap to second target.
    (supertag-field-set source-id "ref-tag" "ref" "target2-id")
    ;; Old relation gone, new relation exists.
    (should (null (supertag-relation-find-between source-id target-id :reference)))
    (should (= 1 (length (supertag-relation-find-between source-id "target2-id" :reference))))
    ;; Old backlink gone, new backlink exists.
    (with-current-buffer (find-file-noselect test-file)
      (org-with-wide-buffer
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-end-of-meta-data t)
        (should-not (re-search-forward (format "\\[\\[id:%s\\]" source-id) (save-excursion (org-end-of-subtree t t)) t))))
    (with-current-buffer (find-file-noselect test-file)
      (org-with-wide-buffer
        (goto-char (point-min))
        (re-search-forward "Target2" nil t)
        (org-end-of-meta-data t)
        (should (re-search-forward (format "\\[\\[id:%s\\]\\[Source\\]\\]" source-id) nil t))))))

(ert-deftest field-set-string-does-not-touch-relations ()
  "Setting a non-:node-reference field does not create or delete relations."
  (field-node-reference-test--with-env
    (supertag-tag-create
     (list :id "string-tag"
           :name "String Tag"
           :fields (list (list :name "note" :type :string :required nil))))
    (supertag-node-add-tag source-id "string-tag")
    (supertag-field-set source-id "string-tag" "note" "hello")
    (should (null (supertag-relation-find-between source-id target-id :reference)))))

(ert-deftest field-set-node-reference-equal-value-skips-side-effects ()
  "Setting the same :node-reference value twice does not duplicate relations."
  (field-node-reference-test--with-env
    (field-node-reference-test--tag-with-ref-field)
    (supertag-node-add-tag source-id "ref-tag")
    (supertag-field-set source-id "ref-tag" "ref" target-id)
    (supertag-field-set source-id "ref-tag" "ref" target-id)
    (let ((rels (supertag-relation-find-between source-id target-id :reference)))
      (should (= 1 (length rels))))))

(ert-deftest field-set-node-reference-global-field ()
  "Global :node-reference fields also sync relations/backlinks."
  (field-node-reference-test--with-env
    (require 'supertag-ops-global-field)
    (let ((supertag-use-global-fields t))
      (supertag-global-field-create
       (list :id "ref" :name "Reference" :type :node-reference :required nil))
      (supertag-field-set source-id "any-tag" "ref" target-id)
      (let ((rels (supertag-relation-find-between source-id target-id :reference)))
        (should (= 1 (length rels))))
      (supertag-field-set source-id "any-tag" "ref" nil)
      (should (null (supertag-relation-find-between source-id target-id :reference))))))

(provide 'test-field-node-reference)
;;; test-field-node-reference.el ends here

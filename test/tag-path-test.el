;;; tag-path-test.el --- ERT tests for nested tag paths -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-core-scan)
(require 'supertag-core-transform)
(require 'supertag-view-api)

(defmacro tag-path-test--with-clean-store (&rest body)
  "Run BODY with a clean in-memory store."
  (declare (indent 0))
  `(let ((supertag--store nil)
         (supertag--store-origin nil))
     (supertag--ensure-store)
     ,@body))

(defun tag-path-test--put-tag (id)
  "Put a minimal tag entity with ID into the test store."
  (supertag-store-put-entity :tags id (list :id id :name id :type :tag)))

(defun tag-path-test--put-node (id tag)
  "Put a minimal node ID carrying TAG into the test store."
  (supertag-store-put-entity
   :nodes id (list :id id :title id :type :node :tags (list tag))))

(ert-deftest tag-path-extraction-preserves-the-complete-id ()
  (should
   (equal '("emacs/package" "emacs/package/elpa")
          (supertag-transform-extract-inline-tags
           "#emacs/package #emacs/package/elpa"))))

(ert-deftest tag-path-query-keeps-exact-matching-as-the-default ()
  (tag-path-test--with-clean-store
    (dolist (pair '(("exact" . "emacs")
                    ("child" . "emacs/package")
                    ("deep" . "emacs/package/elpa")
                    ("lookalike" . "emacs2/package")
                    ("other" . "linux/package")))
      (tag-path-test--put-node (car pair) (cdr pair)))
    (should (equal '("exact")
                   (supertag-index-get-nodes-by-tag "emacs")))
    (should
     (equal '("exact")
            (mapcar #'car (supertag-find-nodes-by-tag "emacs"))))))

(ert-deftest tag-path-query-includes-only-segment-boundary-descendants ()
  (tag-path-test--with-clean-store
    (dolist (pair '(("exact" . "emacs")
                    ("child" . "emacs/package")
                    ("deep" . "emacs/package/elpa")
                    ("lookalike" . "emacs2/package")
                    ("other" . "linux/package")))
      (tag-path-test--put-node (car pair) (cdr pair)))
    (let ((expected '("child" "deep" "exact")))
      (should
       (equal expected
              (sort (supertag-index-get-nodes-by-tag "emacs" t) #'string<)))
      (should
       (equal expected
              (sort (mapcar #'car (supertag-find-nodes-by-tag "emacs" t))
                    #'string<)))
      (should
       (equal expected
              (sort (supertag-view-api-nodes-by-tag "emacs" t) #'string<)))
      (should
       (equal expected
              (sort
               (supertag-view-api-list-entity-ids
                '(:type :tag :value "emacs" :include-descendants t))
               #'string<))))))

(ert-deftest tag-path-descendants-return-only-real-complete-tag-ids ()
  (tag-path-test--with-clean-store
    (dolist (tag '("emacs/package"
                   "emacs/package/elpa"
                   "emacs/"
                   "emacs2/package"
                   "linux/package"))
      (tag-path-test--put-tag tag))
    (should
     (equal '("emacs/package" "emacs/package/elpa")
            (sort (supertag-find-tag-descendants "emacs") #'string<)))))

(provide 'tag-path-test)
;;; tag-path-test.el ends here

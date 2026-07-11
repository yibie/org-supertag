;;; test-view-table.el --- Tests for table views -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'supertag-view-table)

(ert-deftest supertag-view-table-refs-field-is-not-duplicated ()
  "The reserved Refs column must replace a same-slug schema field."
  (let ((supertag-use-global-fields nil))
    (cl-letf (((symbol-function 'supertag-tag-get-id-by-name)
               (lambda (_) "task"))
              ((symbol-function 'supertag-view-table--ensure-refs-field)
               #'ignore)
              ((symbol-function 'supertag-tag-get-all-fields)
               (lambda (_) '((:name "Refs" :type :node-reference))))
              ((symbol-function 'supertag-view-table--get-virtual-columns)
               #'ignore))
      (let ((keys
             (mapcar (lambda (column) (plist-get column :key))
                     (supertag-view-table--get-columns-for-tag "task"))))
        (should (= 1 (cl-count :refs keys :test #'eq)))
        (should-not (memq 'refs keys))))))

(provide 'test-view-table)

;;; test-view-table.el ends here

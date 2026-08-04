;;; test-view-table.el --- Tests for table views -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'supertag-view-table)

(ert-deftest supertag-view-table-refs-field-is-not-duplicated ()
  "The reserved Refs column must replace a same-slug schema field."
  (let ((supertag-use-global-fields nil))
    (cl-letf (((symbol-function 'supertag-tag-get-id-by-name)
               (lambda (_) "task"))
              ((symbol-function 'supertag-tag-get-all-fields)
               (lambda (_) '((:name "Refs" :type :node-reference))))
              ((symbol-function 'supertag-view-table--get-virtual-columns)
               #'ignore))
      (let ((keys
             (mapcar (lambda (column) (plist-get column :key))
                     (supertag-view-table--get-columns-for-tag "task"))))
        (should (= 1 (cl-count :refs keys :test #'eq)))
        (should-not (memq 'refs keys))))))

(ert-deftest supertag-view-table-column-read-does-not-create-refs-schema ()
  "Reading Table columns must not mutate legacy or global schemas."
  (dolist (use-global '(nil t))
    (let ((supertag--store (make-hash-table :test 'equal))
          (supertag--schema-cache (make-hash-table :test 'eq))
          (supertag-ops-schema--resolved-cache (make-hash-table :test 'equal))
          (supertag-use-global-fields use-global)
          (event-count 0))
      (supertag--ensure-store)
      (supertag-store-put-entity
       :tags "task" '(:id "task" :name "task" :fields nil))
      ;; Pre-create the read collections so the assertion only observes
      ;; semantic writes, not lazy empty-bucket allocation.
      (supertag-store-get-collection :field-definitions)
      (supertag-store-get-collection :tag-field-associations)
      (cl-letf (((symbol-function 'supertag-emit-event)
                 (lambda (&rest _args) (cl-incf event-count))))
        (should (memq :refs
                      (mapcar (lambda (column) (plist-get column :key))
                              (supertag-view-table--get-columns-for-tag
                               "task")))))
      (should (zerop event-count))
      (should-not (supertag-store-get-field-definition "refs"))
      (should-not (supertag-store-get-tag-field-associations "task"))
      (should-not (plist-get (supertag-store-get-entity :tags "task")
                             :fields)))))

(ert-deftest supertag-view-table-preserves-smart-key-cell-properties ()
  "Rendered cells must retain the existing Smart Key properties."
  (let ((supertag--store (make-hash-table :test 'equal))
        (buffer-name "*Supertag Table: all*"))
    (unwind-protect
        (progn
          (supertag--ensure-store)
          (supertag-store-put-entity
           :nodes "table-1" '(:id "table-1" :title "Alpha"))
          (let ((buffer
                 (save-window-excursion
                   (supertag-view-table
                    '(:type :nodes :value "all")
                    '((:name "Title" :key :title :width 20))))))
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "Alpha")
              (let ((position (1- (point))))
                (should (equal (get-text-property position 'entity-id)
                               "table-1"))
                (should (eq (get-text-property position 'col-key) :title))
                (should (= (get-text-property position 'col-index) 0))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest supertag-view-table-runtime-refresh-restores-selected-row ()
  "Table refresh must rebuild data and restore the selected entity."
  (let ((supertag--store (make-hash-table :test 'equal))
        (buffer-name "*Supertag Table: all*"))
    (unwind-protect
        (progn
          (supertag--ensure-store)
          (supertag-store-put-entity
           :nodes "table-1" '(:id "table-1" :title "Alpha"))
          (supertag-store-put-entity
           :nodes "table-2" '(:id "table-2" :title "Beta"))
          (let ((buffer
                 (save-window-excursion
                   (supertag-view-table
                    '(:type :nodes :value "all")
                    '((:name "Title" :key :title :width 20))))))
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "Beta"))
            (supertag-store-put-entity
             :nodes "table-2" '(:id "table-2" :title "Beta updated"))
            (with-current-buffer buffer
              (supertag-view-table-refresh)
              (should (equal (get-text-property (point) 'entity-id)
                             "table-2"))
              (should (equal (get-text-property (point) 'supertag-entity-id)
                             "table-2"))
              (should (string-match-p "Beta updated" (buffer-string))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest supertag-view-table-runtime-owns-store-subscription ()
  "Table must refresh on real Store events and unsubscribe on kill."
  (let ((supertag--store (make-hash-table :test 'equal))
        (supertag--subscribers (make-hash-table :test 'equal))
        (buffer-name "*Supertag Table: all*"))
    (unwind-protect
        (progn
          (supertag--ensure-store)
          (supertag-store-put-entity
           :nodes "table-1" '(:id "table-1" :title "Before"))
          (let ((buffer
                 (save-window-excursion
                   (supertag-view-table
                    '(:type :nodes :value "all")
                    '((:name "Title" :key :title :width 20))))))
            (should (= (length (gethash :store-changed supertag--subscribers))
                       1))
            (supertag-store-put-entity
             :nodes "table-1" '(:id "table-1" :title "After") t)
            (with-current-buffer buffer
              (should (string-match-p "After" (buffer-string))))
            (kill-buffer buffer)
            (should-not (gethash :store-changed supertag--subscribers))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(provide 'test-view-table)

;;; test-view-table.el ends here

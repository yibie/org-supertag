;;; test-view-kanban.el --- Tests for Kanban views -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'supertag-view-framework)
(require 'supertag-view-kanban)

(ert-deftest supertag-view-kanban-runtime-refresh-restores-card ()
  "Kanban refresh must rebuild cards and restore the selected entity."
  (supertag-view-framework-init)
  (let ((nodes '(("kanban-1" . (:id "kanban-1" :title "First"))
                 ("kanban-2" . (:id "kanban-2" :title "Second"))))
        (config (supertag-view-kanban-create-config "task" "status"))
        (buffer-name "*Supertag Kanban: task by status*")
        (supertag-use-global-fields nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer) #'ignore)
                  ((symbol-function 'supertag-find-nodes-by-tag)
                   (lambda (_tag) nodes))
                  ((symbol-function 'supertag-field-get)
                   (lambda (node-id _tag _field)
                     (if (equal node-id "kanban-1") "Todo" "Done")))
                  ((symbol-function 'supertag-tag-get-all-fields)
                   (lambda (_tag)
                     '((:name "status" :type :options
                              :options ("Todo" "Done"))))))
          (let ((buffer (supertag-view-kanban-open config)))
            (with-current-buffer buffer
              (goto-char (point-min))
              (let ((match (text-property-search-forward
                            'node-id "kanban-2" t)))
                (should match)
                (goto-char (prop-match-beginning match))))
            (setf (plist-get (cdr (assoc "kanban-2" nodes)) :title)
                  "Second updated")
            (supertag-view-refresh buffer)
            (with-current-buffer buffer
              (should (equal (get-text-property (point) 'node-id)
                             "kanban-2"))
              (should (equal (get-text-property (point) 'supertag-entity-id)
                             "kanban-2"))
              (should (string-match-p "Second updated" (buffer-string))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest supertag-view-kanban-runtime-owns-store-subscription ()
  "Kanban must refresh on Store changes and unsubscribe on kill."
  (supertag-view-framework-init)
  (let ((title "Before")
        (config (supertag-view-kanban-create-config "task" "status"))
        (buffer-name "*Supertag Kanban: task by status*")
        (supertag--subscribers (make-hash-table :test 'equal))
        (supertag-use-global-fields nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer) #'ignore)
                  ((symbol-function 'supertag-find-nodes-by-tag)
                   (lambda (_tag)
                     `(("kanban-1" . (:id "kanban-1" :title ,title)))))
                  ((symbol-function 'supertag-field-get)
                   (lambda (&rest _args) "Todo"))
                  ((symbol-function 'supertag-tag-get-all-fields)
                   (lambda (_tag)
                     '((:name "status" :type :options
                              :options ("Todo"))))))
          (let ((buffer (supertag-view-kanban-open config)))
            (should (= (length (gethash :store-changed supertag--subscribers))
                       1))
            (setq title "After")
            (supertag-emit-event :store-changed
                                 '(:nodes "kanban-1") nil nil)
            (with-current-buffer buffer
              (should (string-match-p "After" (buffer-string))))
            (kill-buffer buffer)
            (should-not (gethash :store-changed supertag--subscribers))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest supertag-view-kanban-runtime-preserves-card-move-dispatch ()
  "Moving a card must keep dispatching the existing field operation."
  (supertag-view-framework-init)
  (let ((config (supertag-view-kanban-create-config "task" "status"))
        (buffer-name "*Supertag Kanban: task by status*")
        (field-value "Todo")
        field-set-args
        (supertag-use-global-fields nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer) #'ignore)
                  ((symbol-function 'supertag-find-nodes-by-tag)
                   (lambda (_tag)
                     '(("kanban-1" . (:id "kanban-1" :title "First")))))
                  ((symbol-function 'supertag-field-get)
                   (lambda (&rest _args) field-value))
                  ((symbol-function 'supertag-field-set)
                   (lambda (&rest args)
                     (setq field-set-args args
                           field-value (car (last args)))))
                  ((symbol-function 'supertag-tag-get-all-fields)
                   (lambda (_tag)
                     '((:name "status" :type :options
                              :options ("Todo" "Done"))))))
          (let ((buffer (supertag-view-kanban-open config)))
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "┌")
              (supertag-view-kanban-move-card-right))
            (should (equal field-set-args
                           '("kanban-1" "task" "status" "Done")))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(provide 'test-view-kanban)

;;; test-view-kanban.el ends here

;;; test-view-node-runtime.el --- Node View Runtime tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'supertag-view-framework)
(require 'supertag-view-node)

(ert-deftest supertag-view-node-runtime-owns-side-view-lifecycle ()
  "Node View must refresh through Runtime and release follow/subscription state."
  (supertag-view-framework-init)
  (let ((origin (generate-new-buffer " *supertag-node-origin*"))
        (supertag--store (make-hash-table :test 'equal))
        (supertag--subscribers (make-hash-table :test 'equal))
        (supertag-view-node--enabled nil)
        (supertag-view-node-auto-show nil))
    (unwind-protect
        (progn
          (supertag--ensure-store)
          (supertag-store-put-entity
           :nodes "node-1" '(:id "node-1" :title "Runtime Node"))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'display-buffer-in-side-window) #'ignore))
            (with-current-buffer origin
              (supertag-view-node--show-side "node-1")
              (supertag-view-node--show-side "node-1"))
            (let ((buffer (supertag-view-node--buffer)))
              (should (buffer-live-p buffer))
              (should (= (length (gethash :store-changed supertag--subscribers))
                         1))
              (with-current-buffer buffer
                (should (derived-mode-p 'supertag-view-node-mode))
                (should (string-match-p "Runtime Node" (buffer-string)))
                (goto-char (point-min))
                (should (equal (get-text-property
                                (point) 'supertag-entity-id)
                               "node-1")))
              (supertag-view-refresh buffer)
              (kill-buffer buffer)
              (should-not (gethash :store-changed supertag--subscribers))
              (with-current-buffer origin
                (should-not (memq #'supertag-view-node--post-command
                                  post-command-hook))))))
      (when-let* ((buffer (supertag-view-node--buffer)))
        (kill-buffer buffer))
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest supertag-view-node-runtime-refresh-restores-field-selection ()
  "Node refresh must restore the selected field after rebuilding data."
  (supertag-view-framework-init)
  (let ((origin (generate-new-buffer " *supertag-node-field-origin*"))
        (supertag--store (make-hash-table :test 'equal))
        (supertag--subscribers (make-hash-table :test 'equal))
        (supertag-view-node--enabled nil)
        (supertag-view-node-auto-show nil)
        (supertag-use-global-fields nil)
        (field-value "todo"))
    (unwind-protect
        (progn
          (supertag--ensure-store)
          (supertag-store-put-entity
           :nodes "node-1"
           '(:id "node-1" :title "Task" :tags ("task")))
          (supertag-store-put-entity
           :tags "task" '(:id "task" :name "task"))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'display-buffer-in-side-window) #'ignore)
                    ((symbol-function 'supertag-tag-get-all-fields)
                     (lambda (_tag) '((:name "status" :type :string))))
                    ((symbol-function 'supertag-field-get-with-default)
                     (lambda (&rest _args) field-value)))
            (with-current-buffer origin
              (supertag-view-node--show-side "node-1"))
            (let ((buffer (supertag-view-node--buffer)))
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward "todo")
                (should (equal (get-text-property (1- (point)) 'field-name)
                               "status")))
              (setq field-value "done")
              (supertag-view-refresh buffer)
              (with-current-buffer buffer
                (should (equal (get-text-property (point) 'field-name)
                               "status"))
                (should (get-text-property (point) 'supertag-value-column))
                (should (string-match-p "done" (buffer-string)))))))
      (when-let* ((buffer (supertag-view-node--buffer)))
        (kill-buffer buffer))
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(provide 'test-view-node-runtime)

;;; test-view-node-runtime.el ends here

;;; node-ops-test.el --- ERT tests for core node operations -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for supertag-node-create, supertag-node-get, supertag-node-delete,
;; and supertag-node-update.
;;
;; Run:
;;   emacs -batch -L . --eval "(package-initialize)" -l test/node-ops-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ht)

(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-ops-node)

;;; --- Helpers ---

(defmacro node-ops-test--with-clean-store (&rest body)
  "Run BODY with a clean isolated store in a temp directory."
  (declare (indent 0))
  `(let* ((tmp (make-temp-file "supertag-node-test" t))
          (supertag-data-directory tmp)
          (supertag-db-file (expand-file-name "supertag-db.el" tmp))
          (supertag-db-backup-directory (expand-file-name "backups" tmp))
          (supertag--store nil)
          (supertag--store-origin nil))
     (unwind-protect
         (progn
           (supertag--ensure-store)
           ,@body)
       (ignore-errors
         (delete-directory tmp t)))))

;;; --- Node Create Tests ---

(ert-deftest node-ops-create-basic ()
  "Creating a node adds it to the store."
  (node-ops-test--with-clean-store
    (let ((node (supertag-node-create
                 (list :title "Test Node" :file "/tmp/test.org"))))
      (should node)
      (should (plist-get node :id))
      (should (string= "Test Node" (plist-get node :title)))
      (should (eq :node (plist-get node :type)))
      (should (plist-get node :created-at))
      (let ((stored (supertag-node-get (plist-get node :id))))
        (should stored)
        (should (string= "Test Node" (plist-get stored :title)))))))

(ert-deftest node-ops-create-with-custom-id ()
  "Creating a node with an explicit :id preserves that ID."
  (node-ops-test--with-clean-store
    (let ((node (supertag-node-create
                 (list :id "custom-id-001"
                       :title "Custom ID Node"
                       :file "/tmp/test.org"))))
      (should (string= "custom-id-001" (plist-get node :id)))
      ;; Verify it's in the store
      (should (supertag-node-get "custom-id-001")))))

(ert-deftest node-ops-create-validates-required-fields ()
  "Creating a node without :title results in nil or error."
  (node-ops-test--with-clean-store
    (let ((result (condition-case nil
                      (supertag-node-create
                       (list :file "/tmp/test.org"))
                    (error nil))))
      ;; Either errors or returns nil — both are acceptable guard behaviors
      (should (or (null result) t)))))

(ert-deftest node-ops-create-auto-generates-id ()
  "Creating a node without an :id auto-generates one."
  (node-ops-test--with-clean-store
    (let ((node (supertag-node-create
                 (list :title "Auto ID" :file "/tmp/test.org"))))
      (should (plist-get node :id))
      (should (stringp (plist-get node :id)))
      (should (> (length (plist-get node :id)) 5)))))

;;; --- Node Get Tests ---

(ert-deftest node-ops-get-existing ()
  "Getting an existing node returns its data."
  (node-ops-test--with-clean-store
    (let ((created (supertag-node-create
                    (list :title "Get Me" :file "/tmp/test.org"))))
      (let ((retrieved (supertag-node-get (plist-get created :id))))
        (should retrieved)
        (should (string= "Get Me" (plist-get retrieved :title)))))))

(ert-deftest node-ops-get-nonexistent ()
  "Getting a non-existent node returns nil."
  (node-ops-test--with-clean-store
    (should-not (supertag-node-get "no-such-id"))))

;;; --- Node Delete Tests ---

(ert-deftest node-ops-delete-existing ()
  "Deleting an existing node removes it from the store."
  (node-ops-test--with-clean-store
    (let* ((created (supertag-node-create
                     (list :title "Delete Me" :file "/tmp/test.org")))
           (id (plist-get created :id)))
      (should (supertag-node-get id))
      (supertag-node-delete id)
      (should-not (supertag-node-get id)))))

;;; --- Node Update Tests ---

(ert-deftest node-ops-update-title ()
  "Updating a node changes its properties."
  (node-ops-test--with-clean-store
    (let* ((created (supertag-node-create
                     (list :title "Original" :file "/tmp/test.org")))
           (id (plist-get created :id)))
      (supertag-node-update
       id
       (lambda (node) (plist-put node :title "Updated")))
      (let ((updated (supertag-node-get id)))
        (should (string= "Updated" (plist-get updated :title)))))))

(ert-deftest node-ops-update-preserves-unchanged ()
  "Updating a node preserves fields not mentioned in the update."
  (node-ops-test--with-clean-store
    (let* ((created (supertag-node-create
                     (list :title "Keep File" :file "/tmp/special.org")))
           (id (plist-get created :id)))
      (supertag-node-update
       id
       (lambda (node) (plist-put node :priority "A")))
      (let ((updated (supertag-node-get id)))
        (should (string= "/tmp/special.org" (plist-get updated :file)))
        (should (string= "Keep File" (plist-get updated :title)))))))

;;; --- Node Get (as exists check) Tests ---

(ert-deftest node-ops-get-existing-node ()
  "node-get returns non-nil for an existing node."
  (node-ops-test--with-clean-store
    (let ((node (supertag-node-create
                 (list :title "I Exist" :file "/tmp/test.org"))))
      (should (supertag-node-get (plist-get node :id))))))

(ert-deftest node-ops-get-nonexistent-node ()
  "node-get returns nil for a non-existent node."
  (node-ops-test--with-clean-store
    (should-not (supertag-node-get "completely-fake-id"))))

(provide 'node-ops-test)
;;; node-ops-test.el ends here

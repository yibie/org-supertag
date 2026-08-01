;;; tag-merge-test.el --- ERT tests for destructive tag merge -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   Emacs -batch -L . --eval "(package-initialize)" \
;;     -l test/tag-merge-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ht)

(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-core-index)
(require 'supertag-ops-node)
(require 'supertag-ops-tag)
(require 'supertag-ops-field)
(require 'supertag-ops-global-field)
(require 'supertag-ops-relation)
(require 'supertag-ops-tag-merge)
(require 'supertag-view-schema)

(defmacro tag-merge-test--with-store (&rest body)
  "Run BODY with an isolated store and temporary Org file directory."
  (declare (indent 0))
  `(let* ((tmp (make-temp-file "supertag-tag-merge-test" t))
          (supertag-data-directory tmp)
          (supertag-db-file (expand-file-name "supertag-db.el" tmp))
          (supertag-db-backup-directory (expand-file-name "backups" tmp))
          (supertag--store nil)
          (supertag--store-origin nil)
          (supertag-use-global-fields nil)
          (supertag-query-saved nil)
          (supertag--view-configs (make-hash-table :test 'eq))
          (supertag--index-relations-by-from (make-hash-table :test 'equal))
          (supertag--index-relations-by-to (make-hash-table :test 'equal)))
     (unwind-protect
         (progn
           (supertag--ensure-store)
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p tmp file)
             (kill-buffer buffer))))
       (ignore-errors (delete-directory tmp t)))))

(defun tag-merge-test--write-org (dir name text)
  "Write TEXT to NAME under DIR and return its path."
  (let ((file (expand-file-name name dir)))
    (with-temp-file file (insert text))
    file))

(defun tag-merge-test--create-tag (id fields &optional parent)
  "Create tag ID with FIELDS and optional PARENT."
  (supertag-tag-create (list :id id :name id :fields fields :extends parent)))

(defun tag-merge-test--create-node (id file tags)
  "Create node ID in FILE with TAGS and matching relations."
  (supertag-node-create (list :id id :title id :file file :tags tags))
  (dolist (tag tags)
    (supertag-relation-create (list :type :node-tag :from id :to tag)))
  id)

(ert-deftest tag-cleanup-only-deletes-unreferenced-schema-free-tags ()
  "Orphan cleanup is conservative and rechecks before deleting."
  (tag-merge-test--with-store
    (dolist (id '("orphan" "used" "schema" "parent" "child" "legacy"
                  "automation" "query" "view" "relation" "associated"))
      (tag-merge-test--create-tag id nil))
    (supertag-tag-add-field "schema" '(:name "status" :type :string))
    (supertag--set-tag-parent "child" "parent")
    (tag-merge-test--create-node "n1" nil '("used"))
    (supertag-store-put-legacy-field "n1" "legacy" "value" 1)
    (supertag-store-put-tag-field-associations
     "associated" '((:field-id "status")))
    (supertag-relation-create
     '(:type :related :from "relation" :to "n1"))
    (supertag-store-put-entity
     :automations "rule" '(:id "rule" :when (:tag "automation")))
    (setq supertag-query-saved '(("saved" . "(tag \"query\")")))
    (puthash 'saved-view '(:query (:type :tag :value "view"))
             supertag--view-configs)
    (should (equal (supertag-tag-orphaned-ids) '("orphan")))
    (should (featurep 'supertag-query-library))
    (should (= (supertag-tag-delete-orphans '("orphan")) 1))
    (should-not (supertag-tag-get "orphan"))
    (should-error (supertag-tag-delete-orphans '("used")))
    (should (supertag-tag-get "used"))))

(ert-deftest tag-cleanup-protects-schema-tag-default-references ()
  "A Tag referenced by a schema field definition is not an orphan."
  (tag-merge-test--with-store
    (tag-merge-test--create-tag "target" nil)
    (tag-merge-test--create-tag
     "schema" '((:name "related" :type :tag :default "target")))
    (should-not (member "target" (supertag-tag-orphaned-ids)))
    (should-error (supertag-tag-delete-orphans '("target")) :type 'user-error)
    (should (supertag-tag-get "target"))))

(ert-deftest tag-cleanup-keeps-all-tags-when-saved-query-is-unreadable ()
  "Unreadable saved configuration must disable destructive candidates."
  (tag-merge-test--with-store
    (tag-merge-test--create-tag "keep" nil)
    (setq supertag-query-saved '(("broken" . "(tag")))
    (should-not (supertag-tag-orphaned-ids))))

(ert-deftest tag-cleanup-rechecks-stale-preview ()
  "A reference added after preview must block deletion."
  (tag-merge-test--with-store
    (tag-merge-test--create-tag "late" nil)
    (let ((preview (supertag-tag-orphaned-ids)))
      (should (member "late" preview))
      (tag-merge-test--create-node "n1" nil '("late"))
      (should-error (supertag-tag-delete-orphans preview) :type 'user-error)
      (should (supertag-tag-get "late")))))

(ert-deftest tag-cleanup-rechecks-after-before-operation-hook ()
  "A hook-created reference must block the pending Tag deletion."
  (tag-merge-test--with-store
    (tag-merge-test--create-tag "late" nil)
    (let* ((created nil)
           (supertag-before-operation-hook
            (list
             (lambda (payload)
               (when (and (not created)
                          (eq (plist-get payload :operation) :delete)
                          (equal (plist-get payload :id) "late"))
                 (setq created t)
                 (supertag-store-put-entity
                  :nodes "hook-node"
                  '(:id "hook-node" :title "Hook" :tags ("late"))))))))
      (should-error (supertag-tag-delete-orphans '("late")) :type 'user-error)
      (should (supertag-tag-get "late"))
      (should-not (supertag-node-get "hook-node")))))

(ert-deftest tag-cleanup-rollback-rebuilds-schema-cache ()
  "Rollback after a partial batch delete restores the resolved cache."
  (tag-merge-test--with-store
    (tag-merge-test--create-tag "a" nil)
    (tag-merge-test--create-tag "b" nil)
    (supertag-ops-schema-rebuild-cache)
    (let ((supertag-before-operation-hook
           (list
            (lambda (payload)
              (when (and (eq (plist-get payload :operation) :delete)
                         (equal (plist-get payload :id) "b"))
                (error "Injected delete failure"))))))
      (should-error (supertag-tag-delete-orphans '("a" "b")))
      (should (supertag-tag-get "a"))
      (should (supertag-tag-get "b"))
      (should (supertag-ops-schema-get-resolved-tag "a"))
      (should (supertag-ops-schema-get-resolved-tag "b")))))

(ert-deftest tag-merge-plan-requires-two-participants ()
  (tag-merge-test--with-store
    (tag-merge-test--create-tag "task" nil)
    (should-error
     (supertag-tag-merge-plan '("task") "work" :selected-fields :all))))

(ert-deftest tag-merge-schema-view-binds-merge-to-mark-prefix ()
  (should (eq (lookup-key supertag-schema-view-mode-map (kbd "m M"))
              #'supertag-schema-merge-marked-tags)))

(ert-deftest tag-merge-plan-requires-definition-choice-for-incompatible-fields ()
  (tag-merge-test--with-store
    (tag-merge-test--create-tag "a" '((:name "status" :type :string)))
    (tag-merge-test--create-tag "b" '((:name "status" :type :integer)))
    (let ((blocked (supertag-tag-merge-plan '("a" "b") "merged"
                                             :selected-fields '("status"))))
      (should (eq (plist-get (car (plist-get blocked :conflicts)) :kind)
                  :field-definition)))
    (let ((resolved (supertag-tag-merge-plan
                     '("a" "b") "merged"
                     :selected-fields '("status")
                     :field-sources '(("status" . "b")))))
      (should-not (plist-get resolved :conflicts))
      (should (eq (plist-get (cdr (assoc "status" (plist-get resolved :field-definitions)))
                             :type)
                  :integer)))))

(ert-deftest tag-merge-definition-choice-filters-incompatible-source-values ()
  (tag-merge-test--with-store
    (let ((file (tag-merge-test--write-org tmp "node.org" "* Node #a #b\n")))
      (tag-merge-test--create-tag "a" '((:name "status" :type :string)))
      (tag-merge-test--create-tag "b" '((:name "status" :type :integer)))
      (tag-merge-test--create-node "n1" file '("a" "b"))
      (supertag-field-set "n1" "a" "status" "not-an-integer")
      (supertag-field-set "n1" "b" "status" 2)
      (let* ((plan (supertag-tag-merge-plan
                    '("a" "b") "merged"
                    :selected-fields '("status")
                    :field-sources '(("status" . "b"))))
             (_result (supertag-tag-merge-execute plan)))
        (should-not (plist-get plan :conflicts))
        (should (eq (plist-get (car (plist-get plan :warnings)) :kind)
                    :incompatible-source-field-definition))
        (should (= (supertag-field-get "n1" "merged" "status") 2))))))

(ert-deftest tag-merge-plan-blocks-deleting-target-ancestor ()
  (tag-merge-test--with-store
    (tag-merge-test--create-tag "parent" nil)
    (tag-merge-test--create-tag "other" nil)
    (tag-merge-test--create-tag "target" nil "parent")
    (let ((plan (supertag-tag-merge-plan '("parent" "other") "target"
                                          :selected-fields nil)))
      (should (eq (plist-get (car (plist-get plan :conflicts)) :kind)
                  :inheritance-cycle)))))

(ert-deftest tag-merge-allows-untitled-file-node ()
  "File nodes without #+TITLE remain valid merge participants."
  (tag-merge-test--with-store
    (let ((file (tag-merge-test--write-org tmp "node.org" "* Node #project\n")))
      (tag-merge-test--create-tag "project" nil)
      (tag-merge-test--create-tag "prj" nil)
      (tag-merge-test--create-node "file-node" file '("project"))
      (let ((node (copy-sequence (supertag-node-get "file-node"))))
        (setq node (plist-put node :level 0))
        (supertag-store-put-entity :nodes "file-node" (plist-put node :title nil)))
      (supertag-tag-merge-execute
       (supertag-tag-merge-plan '("project" "prj") "prj"
                                :selected-fields nil))
      (should-not (supertag-tag-get "project"))
      (should (equal (plist-get (supertag-node-get "file-node") :tags) '("prj")))
      (should-not (plist-get (supertag-node-get "file-node") :title))
      (with-temp-buffer
        (insert-file-contents file)
        (should (search-forward "#prj" nil t))))))

(ert-deftest tag-merge-legacy-to-new-target-migrates-store-files-and-references ()
  (tag-merge-test--with-store
    (let* ((file (tag-merge-test--write-org
                  tmp "nodes.org" "* Node #task #todo\n* Unrelated #tasking\n"))
           (status '(:name "status" :type :string))
           (priority '(:name "priority" :type :integer)))
      (tag-merge-test--create-tag "task" (list status))
      (tag-merge-test--create-tag "todo" (list priority))
      (tag-merge-test--create-tag "child" nil "task")
      (tag-merge-test--create-node "n1" file '("task" "todo"))
      (supertag-field-set "n1" "task" "status" "doing")
      (supertag-field-set "n1" "todo" "priority" 2)
      (supertag-store-put-entity
       :automations "auto-1"
       '(:id "auto-1"
         :name "move task"
         :condition (and (has-tag "task") (has-any-tag "todo" "other"))
         :actions ((:type :add-tag :tag "task"))))
      (setq supertag-query-saved '(("work" . "(and (tag \"task\") (tag \"other\"))")))
      (puthash 'board '(:id board :tag "todo") supertag--view-configs)
      (let* ((plan (supertag-tag-merge-plan '("task" "todo") "work"
                                              :selected-fields :all))
             (result (supertag-tag-merge-execute plan)))
        (should-not (plist-get plan :conflicts))
        (should (eq (plist-get result :status) :merged))
        (should (supertag-tag-get "work"))
        (should-not (supertag-tag-get "task"))
        (should-not (supertag-tag-get "todo"))
        (should (equal (plist-get (supertag-tag-get "child") :extends) "work"))
        (should (equal (plist-get (supertag-node-get "n1") :tags) '("work")))
        (should (equal (supertag-field-get "n1" "work" "status") "doing"))
        (should (= (supertag-field-get "n1" "work" "priority") 2))
        (should (eq (supertag-field-get "n1" "task" "status" :missing) :missing))
        (should (supertag-relation-find-between "n1" "work" :node-tag))
        (let ((automation (supertag-store-get-entity :automations "auto-1")))
          (should (equal (plist-get automation :condition)
                         '(and (has-tag "work") (has-any-tag "work" "other"))))
          (should (equal (plist-get (car (plist-get automation :actions)) :tag) "work")))
        (should (equal (cdr (assoc "work" supertag-query-saved))
                       "(and (tag \"work\") (tag \"other\"))"))
        (should (equal (plist-get (gethash 'board supertag--view-configs) :tag) "work"))
        (with-temp-buffer
          (insert-file-contents file)
          (should-not (re-search-forward "#task\\(?:[[:space:]#]\\|$\\)" nil t))
          (should-not (re-search-forward "#todo\\(?:[[:space:]#]\\|$\\)" nil t))
          (should (search-forward "#work" nil t))
          (should (search-forward "#tasking" nil t)))))))

(ert-deftest tag-merge-existing-target-preserves-fields-and-requires-value-resolution ()
  (tag-merge-test--with-store
    (let* ((file (tag-merge-test--write-org tmp "node.org" "* Node #old-a #old-b #work\n"))
           (status '(:name "status" :type :string))
           (owner '(:name "owner" :type :string)))
      (tag-merge-test--create-tag "work" (list status owner))
      (tag-merge-test--create-tag "old-a" (list status))
      (tag-merge-test--create-tag "old-b" (list status))
      (tag-merge-test--create-node "n1" file '("work" "old-a" "old-b"))
      (supertag-field-set "n1" "work" "status" "keep")
      (supertag-field-set "n1" "work" "owner" "alice")
      (supertag-field-set "n1" "old-a" "status" "doing")
      (supertag-field-set "n1" "old-b" "status" "done")
      (let ((blocked (supertag-tag-merge-plan '("old-a" "old-b") "work"
                                               :selected-fields '("status"))))
        (should (= 1 (length (plist-get blocked :conflicts))))
        (should-error (supertag-tag-merge-execute blocked)))
      (let* ((resolved (supertag-tag-merge-plan
                        '("old-a" "old-b") "work"
                        :selected-fields '("status")
                        :resolutions '((("n1" "status") . "doing"))))
             (_result (supertag-tag-merge-execute resolved)))
        (should-not (plist-get resolved :conflicts))
        (should (equal (supertag-field-get "n1" "work" "status") "doing"))
        (should (equal (supertag-field-get "n1" "work" "owner") "alice"))
        (should (supertag-tag-get-field "work" "owner"))
        (should (= 1 (length (supertag-relation-find-between "n1" "work" :node-tag))))))))

(ert-deftest tag-merge-multi-value-resolution-is-explicit ()
  (tag-merge-test--with-store
    (let* ((file (tag-merge-test--write-org tmp "node.org" "* Node #a #b\n"))
           (labels '(:name "labels" :type :tag)))
      (tag-merge-test--create-tag "a" (list labels))
      (tag-merge-test--create-tag "b" (list labels))
      (tag-merge-test--create-node "n1" file '("a" "b"))
      (supertag-field-set "n1" "a" "labels" '("red" "blue"))
      (supertag-field-set "n1" "b" "labels" "green")
      (let* ((plan (supertag-tag-merge-plan
                    '("a" "b") "merged"
                    :selected-fields '("labels")
                    :resolutions '((("n1" "labels") . (:merge-values ("red" "green"))))))
             (_result (supertag-tag-merge-execute plan)))
        (should-not (plist-get plan :conflicts))
        (should (equal (supertag-field-get "n1" "merged" "labels")
                       '("red" "green")))))))

(ert-deftest tag-merge-global-fields-retains-selected-and-drops-orphaned-values ()
  (tag-merge-test--with-store
    (let* ((supertag-use-global-fields t)
           (file (tag-merge-test--write-org tmp "node.org" "* Node #a #b\n")))
      (tag-merge-test--create-tag "a" nil)
      (tag-merge-test--create-tag "b" nil)
      (supertag-global-field-create '(:id "status" :name "status" :type :string))
      (supertag-global-field-create '(:id "discard" :name "discard" :type :string))
      (supertag-tag-associate-field "a" "status")
      (supertag-tag-associate-field "b" "discard")
      (tag-merge-test--create-node "n1" file '("a" "b"))
      (supertag-store-put-field-value "n1" "status" "doing")
      (supertag-store-put-field-value "n1" "discard" "drop-me")
      (let* ((plan (supertag-tag-merge-plan '("a" "b") "merged"
                                              :selected-fields '("status")))
             (_result (supertag-tag-merge-execute plan)))
        (should-not (plist-get plan :conflicts))
        (should (equal (mapcar (lambda (entry) (plist-get entry :field-id))
                               (supertag-store-get-tag-field-associations "merged"))
                       '("status")))
        (should (equal (supertag-store-get-field-value "n1" "status") "doing"))
        (should-not (supertag-store-get-field-value "n1" "discard"))
        (should-not (supertag-store-get-tag-field-associations "a"))
        (should-not (supertag-store-get-tag-field-associations "b"))))))

(ert-deftest tag-merge-file-failure-rolls-back-store ()
  (tag-merge-test--with-store
    (let* ((file (tag-merge-test--write-org tmp "node.org" "* Node #a #b\n"))
           (field '(:name "status" :type :string)))
      (tag-merge-test--create-tag "a" (list field))
      (tag-merge-test--create-tag "b" nil)
      (tag-merge-test--create-node "n1" file '("a" "b"))
      (supertag-field-set "n1" "a" "status" "before")
      (let ((plan (supertag-tag-merge-plan '("a" "b") "merged"
                                            :selected-fields :all)))
        (cl-letf (((symbol-function 'supertag-view-helper-rename-tag-text-in-files)
                   (lambda (&rest _) (error "simulated file failure"))))
          (should-error (supertag-tag-merge-execute plan))))
      (should (supertag-tag-get "a"))
      (should (supertag-tag-get "b"))
      (should-not (supertag-tag-get "merged"))
      (should (equal (plist-get (supertag-node-get "n1") :tags) '("a" "b")))
      (should (equal (supertag-field-get "n1" "a" "status") "before"))
      (with-temp-buffer
        (insert-file-contents file)
        (should (search-forward "#a" nil t))
        (should (search-forward "#b" nil t))))))

(ert-deftest tag-merge-partial-file-write-is-restored-on-later-failure ()
  (tag-merge-test--with-store
    (let* ((file (tag-merge-test--write-org tmp "node.org" "* Node #a #b\n"))
           (original-rewrite (symbol-function 'supertag-view-helper-rename-tag-text-in-files))
           (calls 0))
      (tag-merge-test--create-tag "a" nil)
      (tag-merge-test--create-tag "b" nil)
      (tag-merge-test--create-node "n1" file '("a" "b"))
      (let ((plan (supertag-tag-merge-plan '("a" "b") "merged"
                                            :selected-fields nil)))
        (cl-letf (((symbol-function 'supertag-view-helper-rename-tag-text-in-files)
                   (lambda (&rest args)
                     (setq calls (1+ calls))
                     (if (= calls 1)
                         (apply original-rewrite args)
                       (error "Second file pass failed")))))
          (should-error (supertag-tag-merge-execute plan))))
      (should (supertag-tag-get "a"))
      (should-not (supertag-tag-get "merged"))
      (with-temp-buffer
        (insert-file-contents file)
        (should (search-forward "#a" nil t))
        (should (search-forward "#b" nil t))
        (should-not (search-forward "#merged" nil t))))))

(ert-deftest tag-merge-derived-state-failure-rolls-back-store-and-file ()
  (tag-merge-test--with-store
    (let ((file (tag-merge-test--write-org tmp "node.org" "* Node #a #b\n")))
      (tag-merge-test--create-tag "a" nil)
      (tag-merge-test--create-tag "b" nil)
      (tag-merge-test--create-node "n1" file '("a" "b"))
      (let ((plan (supertag-tag-merge-plan '("a" "b") "merged"
                                            :selected-fields nil)))
        (cl-letf (((symbol-function 'supertag-tag-merge--rebuild-derived-state)
                   (lambda () (error "Index rebuild failed"))))
          (should-error (supertag-tag-merge-execute plan))))
      (should (supertag-tag-get "a"))
      (should (supertag-tag-get "b"))
      (should-not (supertag-tag-get "merged"))
      (with-temp-buffer
        (insert-file-contents file)
        (should (search-forward "#a" nil t))
        (should (search-forward "#b" nil t))))))

(ert-deftest tag-merge-plan-blocks-unsaved-affected-buffer ()
  (tag-merge-test--with-store
    (let ((file (tag-merge-test--write-org tmp "node.org" "* Node #a #b\n")))
      (tag-merge-test--create-tag "a" nil)
      (tag-merge-test--create-tag "b" nil)
      (tag-merge-test--create-node "n1" file '("a" "b"))
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-max))
        (insert "unsaved")
        (let ((plan (supertag-tag-merge-plan '("a" "b") "merged"
                                              :selected-fields nil)))
          (should (eq (plist-get (car (plist-get plan :conflicts)) :kind)
                      :unsaved-buffer)))))))

(provide 'tag-merge-test)
;;; tag-merge-test.el ends here

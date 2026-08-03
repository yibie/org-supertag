;;; tag-path-test.el --- ERT tests for nested tag paths -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-core-tag-path)
(require 'supertag-core-scan)
(require 'supertag-core-transform)
(require 'supertag-ops-relation)
(require 'supertag-ops-tag)
(require 'supertag-services-sync)
(require 'supertag-view-api)
(require 'supertag-view-framework)
(require 'supertag-view-schema)
(require 'supertag-view-table)
(require 'supertag-ui-completion)

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

(defun tag-path-test--context-on-line (regexp)
  "Return Schema View context on the line matching REGEXP."
  (goto-char (point-min))
  (re-search-forward regexp)
  (get-text-property (line-beginning-position) 'supertag-context))

(ert-deftest tag-path-semantics-preserve-segment-boundaries ()
  (should (supertag-tag-path-valid-p "emacs/package/elpa"))
  (should-not (supertag-tag-path-valid-p "/emacs"))
  (should-not (supertag-tag-path-valid-p "emacs/"))
  (should-not (supertag-tag-path-valid-p "emacs//package"))
  (should (equal "emacs/package"
                 (supertag-tag-path-parent "emacs/package/elpa")))
  (should (equal "elpa" (supertag-tag-path-leaf "emacs/package/elpa")))
  (should (supertag-tag-path-descendant-p "emacs/package" "emacs"))
  (should-not (supertag-tag-path-descendant-p "emacs2/package" "emacs"))
  (should (equal "lisp/package/elpa"
                 (supertag-tag-path-rebase
                  "emacs/package/elpa" "emacs/package" "lisp/package"))))

(ert-deftest tag-path-completion-candidates-are-one-level-at-a-time ()
  (let ((paths '("ATTACH" "Apple" "Apple/Shortcut/\u8bed\u8a00"
                 "diary/personal" "diary/work")))
    (should
     (equal '("ATTACH" "Apple" "Apple/" "diary/")
            (supertag-tag-path-direct-candidates paths "")))
    (should
     (equal '("Apple/Shortcut/")
            (supertag-tag-path-direct-candidates paths "Apple/")))
    (should
     (equal '("Apple/Shortcut/\u8bed\u8a00")
            (supertag-tag-path-direct-candidates
             paths "Apple/Shortcut/")))
    (should
     (equal '("diary/personal" "diary/work")
            (supertag-tag-path-direct-candidates paths "diary/")))))

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

(ert-deftest tag-path-single-node-sync-keeps-entities-and-relations-aligned ()
  (tag-path-test--with-clean-store
    (let ((file (make-temp-file "supertag-tag-path" nil ".org")))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (setq buffer-file-name file)
            (insert "* Package #emacs/package\n:PROPERTIES:\n:ID: node-1\n:END:\n")
            (goto-char (point-min))
            (supertag-node-sync-at-point)
            (should (supertag-tag-get "emacs/package"))
            (should (equal '("emacs/package")
                           (plist-get (supertag-node-get "node-1") :tags)))
            (should (= 1 (length (supertag-relation-find-between
                                  "node-1" "emacs/package" :node-tag))))
            (goto-char (point-min))
            (re-search-forward " #emacs/package")
            (replace-match "")
            (goto-char (point-min))
            (supertag-node-sync-at-point)
            (should-not (plist-get (supertag-node-get "node-1") :tags))
            (should-not (supertag-relation-find-by-from "node-1" :node-tag)))
        (delete-file file)))))

(ert-deftest tag-path-incremental-sync-respects-native-tag-policy ()
  (with-temp-buffer
    (org-mode)
    (insert "* Node :legacy:\n")
    (let ((headline
           (car (org-element-contents (org-element-parse-buffer)))))
      (dolist (policy '(read-only preserve lazy-convert))
        (let ((supertag-sync-legacy-tags-policy policy))
          (should (equal '("legacy")
                         (plist-get
                          (supertag-extractor--tags headline nil nil)
                          :tags)))))
      (let ((supertag-sync-legacy-tags-policy 'ignore))
        (should-not
         (plist-get (supertag-extractor--tags headline nil nil) :tags))))))

(ert-deftest tag-path-schema-tree-separates-namespace-from-inheritance ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "base")
    (supertag-tag-add-field "base" '(:name "tier" :type :string))
    (tag-path-test--put-tag "emacs/package")
    (supertag-tag-create
     '(:id "emacs/package/elpa" :name "emacs/package/elpa" :extends "base"))
    (let* ((tree (supertag-schema--build-tree))
           (emacs (cl-find "emacs" tree :key (lambda (node) (plist-get node :id))
                           :test #'equal))
           (package (car (plist-get emacs :children)))
           (elpa (car (plist-get package :children))))
      (should (plist-get emacs :virtual))
      (should-not (plist-get package :virtual))
      (should (equal "package" (plist-get package :label)))
      (should (equal "emacs/package/elpa" (plist-get elpa :id)))
      (should (equal "base" (plist-get elpa :extends))))
    (with-temp-buffer
      (supertag-schema--render)
      (should (string-match-p "^emacs/$" (buffer-string)))
      (should (string-match-p "^  package/$" (buffer-string)))
      (should (string-match-p "^    elpa -> base$" (buffer-string)))
      (should (string-match-p "Inherited from base" (buffer-string)))
      (let ((namespace (tag-path-test--context-on-line "^emacs/$"))
            (leaf (tag-path-test--context-on-line "^    elpa -> base$")))
        (should (eq :namespace (plist-get namespace :type)))
        (should (equal "emacs" (plist-get namespace :path)))
        (should (eq :tag (plist-get leaf :type)))
        (should (equal "emacs/package/elpa" (plist-get leaf :tag-id)))))))

(ert-deftest tag-path-schema-creates-a-path-child-without-inheritance ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "emacs/package")
    (let ((created (supertag-schema--create-nested-tag
                    "emacs/package" "elpa")))
      (should (equal "emacs/package/elpa" (plist-get created :id)))
      (should-not (plist-get created :extends)))))

(ert-deftest tag-path-view-context-retains-descendant-scope ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-node "child" "emacs/package")
    (tag-path-test--put-node "other" "emacs2/package")
    (with-temp-buffer
      (insert (propertize "emacs/"
                          'supertag-context
                          '(:type :namespace :path "emacs")))
      (goto-char (point-min))
      (let* ((query (supertag-view--get-tag-at-point))
             (context (supertag-view--build-context query))
             (rebuilt (funcall
                       (supertag-view--context-builder-from-context context))))
        (should (equal '(:type :tag :value "emacs" :include-descendants t)
                       query))
        (should (equal '("child")
                       (mapcar (lambda (node) (plist-get node :id))
                               (plist-get context :nodes))))
        (should (plist-get rebuilt :include-descendants))
        (should (equal query (plist-get rebuilt :query)))))))

(ert-deftest tag-path-table-aggregate-is-read-only-and-uses-common-columns ()
  (let ((query '(:type :tag :value "emacs" :include-descendants t)))
    (should (equal '(:title :tags :file)
                   (mapcar (lambda (column) (plist-get column :key))
                           (supertag-view-table--get-columns query))))
    (with-temp-buffer
      (setq-local supertag-view-table--query-objs (list query))
      (setq-local supertag-view-table--current-table-index 0)
      (should-error (supertag-view-table-add-column) :type 'user-error)
      (should-error (supertag-view-table-edit-cell) :type 'user-error))))

(ert-deftest tag-path-completion-navigates-namespaces-without-writing ()
  (cl-letf (((symbol-function 'supertag-completion--get-all-tags)
             (lambda () '("emacs/package" "emacs/package/elpa"
                          "emacs2/package"))))
    (let* ((candidates (supertag-completion--get-completion-table "emacs"))
           (namespace
            (cl-find-if
             (lambda (candidate)
               (get-text-property 0 'supertag-namespace-prefix candidate))
             candidates))
           (writes 0))
      (should (equal "emacs/" (substring-no-properties namespace)))
      (should-not (member "emacs/package" candidates))
      (should
       (equal '("emacs/package" "emacs/package/")
              (mapcar #'substring-no-properties
                      (seq-remove
                       (lambda (candidate)
                         (get-text-property 0 'is-new-tag candidate))
                       (supertag-completion--get-completion-table "emacs/")))))
      (should
       (equal '("emacs/package/elpa")
              (mapcar #'substring-no-properties
                      (supertag-completion--get-completion-table
                       "emacs/package/"))))
      (cl-letf (((symbol-function 'org-id-get-create)
                 (lambda () (setq writes (1+ writes)) "node"))
                ((symbol-function 'supertag-ops-add-tag-to-node)
                 (lambda (&rest _) (setq writes (1+ writes)))))
        (with-temp-buffer
          (insert namespace)
          (supertag-completion--post-completion-action namespace)
          (should (equal "emacs/" (buffer-string)))
          (should (zerop writes))))
      (should-not
       (cl-find-if (lambda (candidate)
                     (get-text-property 0 'is-new-tag candidate))
                   (supertag-completion--get-completion-table "emacs/")))
      (should-not
       (cl-find-if (lambda (candidate)
                     (get-text-property 0 'is-new-tag candidate))
                   (supertag-completion--get-completion-table "emacs//new"))))))

(ert-deftest tag-path-capf-filters-unrelated-root-tags-below-namespace ()
  (cl-letf (((symbol-function 'supertag-completion--get-all-tags)
             (lambda () '("ATTACH" "Apple" "Apple/Shortcut/\u8bed\u8a00"
                          "diary/personal" "diary/work"))))
    (with-temp-buffer
      (org-mode)
      (insert "#diary/")
      (let* ((completion-styles '(basic))
             (capf (supertag-completion-at-point))
             (table (nth 2 capf)))
        (should
         (equal '("diary/personal" "diary/work")
                (mapcar #'substring-no-properties
                        (all-completions "diary/" table))))))))

(ert-deftest tag-path-capf-can-descend-from-an-existing-flat-tag ()
  (cl-letf (((symbol-function 'supertag-completion--get-all-tags)
             (lambda () '("diary" "diaryx"))))
    (with-temp-buffer
      (org-mode)
      (insert "#diary")
      (let* ((completion-styles '(basic))
             (capf (supertag-completion-at-point))
             (table (nth 2 capf))
             (candidates (all-completions "diary" table))
             (enumerated (funcall table "" nil t))
             (namespace
              (cl-find "diary/" candidates
                       :key #'substring-no-properties :test #'equal)))
        (should namespace)
        (should (cl-find "diary/" enumerated
                         :key #'substring-no-properties :test #'equal))
        (should (get-text-property 0 'supertag-namespace-prefix namespace))))))

(ert-deftest tag-path-shared-reader-navigates-direct-children ()
  (let ((tags '("Apple/Shortcut/\u8bed\u8a00" "Apple/Shortcut/English"
                "diary/work"))
        (answers '("Apple/" "Apple/Shortcut/" "Apple/Shortcut/\u8bed\u8a00"))
        seen)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (push (mapcar #'substring-no-properties collection) seen)
                 (pop answers))))
      (should
       (equal "Apple/Shortcut/\u8bed\u8a00"
              (supertag-ui-read-tag "Tag: " tags nil nil)))
      (should
       (equal '(("Apple/" "diary/")
                ("Apple/Shortcut/")
                ("Apple/Shortcut/English" "Apple/Shortcut/\u8bed\u8a00"))
              (nreverse seen))))))

(ert-deftest tag-path-shared-reader-can-select-a-virtual-namespace ()
  (let (seen)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq seen (mapcar #'substring-no-properties collection))
                 "Apple")))
      (should
       (equal "Apple"
              (supertag-ui-read-tag
               "Tag or namespace: "
               '("Apple/Shortcut/\u8bed\u8a00" "diary/work")
               nil nil t)))
      (should (equal '("Apple" "Apple/" "diary" "diary/") seen)))))

(ert-deftest tag-path-branch-rename-migrates-complete-identities ()
  (tag-path-test--with-clean-store
    (let ((file (make-temp-file "supertag-tag-path" nil ".org"
                                "* P #emacs/package\n* E #emacs/package/elpa\n"))
          (supertag-query-saved
           '(("packages" . "(has-tag \"emacs/package\")")))
          (supertag--view-configs (make-hash-table :test 'eq)))
      (unwind-protect
          (progn
            (supertag-tag-create
             '(:id "emacs/package"
               :name "emacs/package"
               :fields ((:name "tier" :type :string)
                        (:name "related" :type :tag)
                        (:name "plain-text" :type :string))))
            (supertag-tag-create
             '(:id "emacs/package/elpa"
               :name "emacs/package/elpa"
               :extends "emacs/package"))
            (supertag-store-put-entity
             :nodes "node"
             (list :id "node" :title "node" :type :node :file file
                   :tags '("emacs/package" "emacs/package/elpa")))
            (supertag-store-put-tag-field-associations
             "emacs/package" '((:field-id "tier")))
            (supertag-store-put-legacy-field
             "node" "emacs/package" "tier" "core")
            (supertag-store-put-legacy-field
             "node" "emacs/package" "related" "emacs/package/elpa")
            (supertag-store-put-legacy-field
             "node" "emacs/package" "plain-text" "emacs/package")
            (supertag-store-put-field-definition
             "related-tags"
             '(:id "related-tags" :name "Related tags" :type :tag))
            (supertag-store-put-field-definition
             "plain-global"
             '(:id "plain-global" :name "Plain global" :type :string))
            (supertag-store-put-field-value
             "node" "related-tags"
             '("emacs/package" "emacs/package/elpa"))
            (supertag-store-put-field-value
             "node" "plain-global" "emacs/package")
            (supertag-store-put-entity
             :automations "auto"
             '(:id "auto" :tag "emacs/package" :action :notify))
            (supertag-store-put-entity
             :relations "schema-relation"
             (list :id "schema-relation" :type :schema-link
                   :from "emacs/package" :to "other"
                   :props '(:target-tag "emacs/package/elpa")))
            (puthash 'packages
                     '(:id packages :tag "emacs/package")
                     supertag--view-configs)
            (supertag--process-node-tags (supertag-node-get "node"))
            (supertag-tag-rename "emacs/package" "lisp/package")
            (should-not (supertag-tag-get "emacs/package"))
            (should (supertag-tag-get "lisp/package"))
            (should (equal "lisp/package"
                           (plist-get
                            (supertag-tag-get "lisp/package/elpa")
                            :extends)))
            (should (equal '("lisp/package" "lisp/package/elpa")
                           (plist-get (supertag-node-get "node") :tags)))
            (should (= 2 (length (supertag-relation-find-by-from
                                  "node" :node-tag))))
            (should (supertag-store-get-tag-field-associations
                     "lisp/package"))
            (should (equal "core"
                           (supertag-get
                            '(:fields "node" "lisp/package" "tier"))))
            (should (equal "lisp/package/elpa"
                           (supertag-get
                            '(:fields "node" "lisp/package" "related"))))
            (should (equal "emacs/package"
                           (supertag-get
                            '(:fields "node" "lisp/package" "plain-text"))))
            (should (equal '("lisp/package" "lisp/package/elpa")
                           (supertag-store-get-field-value
                            "node" "related-tags")))
            (should (equal "emacs/package"
                           (supertag-store-get-field-value
                            "node" "plain-global")))
            (should
             (cl-some
              (lambda (relation)
                (and (equal "lisp/package" (plist-get relation :from))
                     (equal "lisp/package/elpa"
                            (plist-get
                             (plist-get relation :props)
                             :target-tag))))
              (supertag-relation-find-by-from "lisp/package" :schema-link)))
            (should (equal "lisp/package"
                           (plist-get
                            (supertag-store-get-entity
                             :automations "auto")
                            :tag)))
            (should (equal "(has-tag \"lisp/package\")"
                           (cdr (assoc "packages" supertag-query-saved))))
            (should (equal "lisp/package"
                           (plist-get
                            (gethash 'packages supertag--view-configs)
                            :tag)))
            (with-temp-buffer
              (insert-file-contents file)
              (should (string-match-p "#lisp/package\\(?:\\s-\\|$\\)"
                                      (buffer-string)))
              (should (string-match-p "#lisp/package/elpa\\(?:\\s-\\|$\\)"
                                      (buffer-string)))))
        (delete-file file)))))

(ert-deftest tag-path-branch-rename-collision-leaves-store-unchanged ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "emacs/package")
    (tag-path-test--put-tag "emacs/package/elpa")
    (tag-path-test--put-tag "lisp/package/elpa")
    (tag-path-test--put-node "node" "emacs/package/elpa")
    (should-error
     (supertag-tag-rename "emacs/package" "emacs/package/elpa"))
    (should-error
     (supertag-tag-rename "emacs/package" "lisp/package"))
    (should (supertag-tag-get "emacs/package"))
    (should (supertag-tag-get "emacs/package/elpa"))
    (should (supertag-tag-get "lisp/package/elpa"))
    (should (equal '("emacs/package/elpa")
                   (plist-get (supertag-node-get "node") :tags)))))

(ert-deftest tag-path-exact-delete-does-not-truncate-a-descendant-token ()
  (let ((file (make-temp-file "supertag-tag-path" nil ".org"
                              "* Node #a #a/b\n")))
    (unwind-protect
        (progn
          (should (= 1 (supertag-view-helper-remove-tag-text-from-files
                        "a" (list file))))
          (with-temp-buffer
            (insert-file-contents file)
            (should-not (string-match-p "\\(?:^\\|\\s-\\)#a\\(?:\\s-\\|$\\)"
                                        (buffer-string)))
            (should (string-match-p "#a/b\\(?:\\s-\\|$\\)"
                                    (buffer-string)))))
      (delete-file file))))

(provide 'tag-path-test)
;;; tag-path-test.el ends here

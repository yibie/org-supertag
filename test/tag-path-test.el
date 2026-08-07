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

(defun tag-path-test--put-tag (id &optional parent)
  "Put a minimal tag entity with ID and optional PARENT into the test store."
  (supertag-store-put-entity
   :tags id (list :id id :name id :type :tag :extends parent :fields nil)))

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

(ert-deftest nested-tag-query-follows-explicit-extends-only ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "emacs")
    (tag-path-test--put-tag "package" "emacs")
    (tag-path-test--put-tag "elpa" "package")
    (tag-path-test--put-tag "emacs2")
    (tag-path-test--put-tag "emacs/legacy")
    (dolist (pair '(("exact" . "emacs")
                    ("child" . "package")
                    ("deep" . "elpa")
                    ("lookalike" . "emacs2")
                    ("flat-slash" . "emacs/legacy")))
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

(ert-deftest nested-tag-descendants-return-transitive-extends-ids ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "emacs")
    (tag-path-test--put-tag "package" "emacs")
    (tag-path-test--put-tag "elpa" "package")
    (tag-path-test--put-tag "emacs/legacy")
    (tag-path-test--put-tag "emacs2")
    (should
     (equal '("elpa" "package")
            (sort (supertag-find-tag-descendants "emacs") #'string<)))))

(ert-deftest nested-tag-sync-resolves-display-path-to-real-id ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "emacs")
    (tag-path-test--put-tag "package" "emacs")
    (let ((file (make-temp-file "supertag-tag-path" nil ".org")))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (setq buffer-file-name file)
            (insert "* Package #emacs/package\n:PROPERTIES:\n:ID: node-1\n:END:\n")
            (goto-char (point-min))
            (supertag-node-sync-at-point)
            (should-not (supertag-tag-get "emacs/package"))
            (should (equal '("package")
                           (plist-get (supertag-node-get "node-1") :tags)))
            (should (= 1 (length (supertag-relation-find-between
                                  "node-1" "package" :node-tag))))
            (goto-char (point-min))
            (re-search-forward " #emacs/package")
            (replace-match "")
            (goto-char (point-min))
            (supertag-node-sync-at-point)
            (should-not (plist-get (supertag-node-get "node-1") :tags))
            (should-not (supertag-relation-find-by-from "node-1" :node-tag)))
        (delete-file file)))))

(ert-deftest nested-tag-bulk-import-stores-real-id ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "diary")
    (tag-path-test--put-tag "happy" "diary")
    (cl-letf (((symbol-function 'supertag--parse-org-nodes)
               (lambda (&rest _)
                 '((:id "node-1" :title "Node" :type :node
                    :tags ("diary/happy"))))))
      (let ((result (supertag-migrate-org-files-to-database "/tmp/source.org")))
        (should (= 1 (plist-get result :nodes-created)))
        (should (= 0 (plist-get result :errors)))))
    (should (equal '("happy")
                   (plist-get (supertag-node-get "node-1") :tags)))
    (should (= 1 (length (supertag-relation-find-between
                          "node-1" "happy" :node-tag))))))

(ert-deftest nested-tag-create-rejects-persistent-slash-id ()
  (tag-path-test--with-clean-store
    (should-error
     (supertag-tag-create '(:id "emacs/package" :name "emacs/package"))
     :type 'user-error)
    (should-error
     (supertag-tag-rename "emacs" "emacs/package")
     :type 'user-error)))

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

(ert-deftest nested-tag-schema-tree-uses-explicit-parents-only ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "diary")
    (supertag-tag-add-field "diary" '(:name "mood" :type :string))
    (tag-path-test--put-tag "happy" "diary")
    (tag-path-test--put-tag "coding/日志")
    (let* ((tree (supertag-schema--build-tree))
           (diary (cl-find "diary" tree
                           :key (lambda (node) (plist-get node :id))
                           :test #'equal))
           (coding (cl-find "coding/日志" tree
                            :key (lambda (node) (plist-get node :id))
                            :test #'equal)))
      (should (equal '("happy")
                     (mapcar (lambda (node) (plist-get node :id))
                             (plist-get diary :children))))
      (should coding)
      (should-not (plist-get coding :children)))
    (with-temp-buffer
      (supertag-schema--render)
      (should (string-match-p "^diary$" (buffer-string)))
      (should (string-match-p "^  happy$" (buffer-string)))
      (should-not (string-match-p "happy -> diary" (buffer-string)))
      (should (string-match-p "Inherited from diary" (buffer-string)))
      (should (string-match-p "^coding/日志$" (buffer-string)))
      (let ((happy (tag-path-test--context-on-line "^  happy$")))
        (should (eq :tag (plist-get happy :type)))
        (should (equal "happy" (plist-get happy :tag-id)))))))

(ert-deftest tag-path-schema-uses-one-child-command ()
  (should (eq #'supertag-schema--add-child-tag-at-point
              (lookup-key supertag-schema-view-mode-map (kbd "a n"))))
  (should (eq #'supertag-schema--add-child-tag-at-point
              (lookup-key supertag-schema-view-mode-map (kbd "a c")))))

(ert-deftest nested-tag-schema-tree-follows-explicit-parent ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "a" "a/b")
    (tag-path-test--put-tag "a/b")
    (let ((tree (supertag-schema--build-tree)))
      (should (equal '("a/b")
                     (mapcar (lambda (node) (plist-get node :id)) tree)))
      (should (equal '("a")
                     (mapcar (lambda (node) (plist-get node :id))
                             (plist-get (car tree) :children)))))))

(ert-deftest tag-path-view-context-retains-descendant-scope ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "emacs")
    (tag-path-test--put-tag "package" "emacs")
    (tag-path-test--put-tag "emacs2")
    (tag-path-test--put-node "child" "package")
    (tag-path-test--put-node "other" "emacs2")
    (with-temp-buffer
      (insert (propertize "emacs"
                          'supertag-context
                          '(:type :tag :tag-id "emacs" :has-descendants t)))
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

(ert-deftest tag-path-completion-searches-leaf-and-displays-parent-path ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "diary")
    (tag-path-test--put-tag "happy" "diary")
    (with-temp-buffer
      (org-mode)
      (insert "#hap")
      (let* ((completion-styles '(basic))
             (capf (supertag-completion-at-point))
             (table (nth 2 capf))
             (metadata (funcall table "hap" nil 'metadata))
             (affix (cdr (assq 'affixation-function (cdr metadata))))
             (candidates (all-completions "hap" table))
             (happy (cl-find "happy" candidates
                             :key #'substring-no-properties :test #'equal))
             (display (car (funcall affix (list happy)))))
        (should happy)
        (should (equal "happy"
                       (get-text-property 0 'supertag-tag-id happy)))
        (should (cl-every #'stringp display))
        (should (equal "diary/" (nth 1 display)))
        (should (equal "happy" (substring-no-properties (car display))))))))

(ert-deftest tag-path-completion-progresses-from-parent-display-path ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "diary")
    (tag-path-test--put-tag "happy" "diary")
    (with-temp-buffer
      (org-mode)
      (insert "#diary")
      (let* ((completion-styles '(basic))
             (capf (supertag-completion-at-point))
             (table (nth 2 capf))
             (candidates (all-completions "diary" table))
             (child (cl-find "diary/happy" candidates
                             :key #'substring-no-properties :test #'equal))
             recorded)
        (should child)
        (should (equal "happy"
                       (get-text-property 0 'supertag-tag-id child)))
        (delete-region (1+ (point-min)) (point-max))
        (insert child)
        (cl-letf (((symbol-function 'org-id-get-create) (lambda () "node"))
                  ((symbol-function 'supertag-node-get) (lambda (_) '(:id "node")))
                  ((symbol-function 'supertag-ops-add-tag-to-node)
                   (lambda (_node tag &rest _) (setq recorded tag) t)))
          (supertag-completion--post-completion-action child))
        (should (equal "happy" recorded))
        (should (equal "#happy " (buffer-string)))))))

(ert-deftest tag-path-completion-does-not-shadow-a-real-full-path ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "diary")
    (tag-path-test--put-tag "happy" "diary")
    (tag-path-test--put-tag "diary/happy")
    (with-temp-buffer
      (org-mode)
      (insert "#diary/")
      (let* ((completion-styles '(basic))
             (table (nth 2 (supertag-completion-at-point)))
             (candidate (cl-find "diary/happy"
                                 (all-completions "diary/" table)
                                 :key #'substring-no-properties :test #'equal)))
        (should candidate)
        (should (equal "diary/happy"
                       (get-text-property 0 'supertag-tag-id candidate)))))))

(ert-deftest nested-tag-completion-does-not-offer-new-slash-id ()
  (tag-path-test--with-clean-store
    (with-temp-buffer
      (org-mode)
      (insert "#unknown/child")
      (let* ((completion-styles '(basic))
             (table (nth 2 (supertag-completion-at-point))))
        (should-not (all-completions "unknown/child" table))))))

(ert-deftest tag-path-completion-does-not-commit-a-partial-prefix ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "diary")
    (tag-path-test--put-tag "happy" "diary")
    (with-temp-buffer
      (org-mode)
      (insert "#dia")
      (let* ((completion-styles '(basic))
             (capf (supertag-completion-at-point))
             (table (nth 2 capf))
             (metadata (funcall table "dia" nil 'metadata))
             (sorter (cdr (assq 'display-sort-function (cdr metadata))))
             (affix (cdr (assq 'affixation-function (cdr metadata))))
             (exit (plist-get (nthcdr 3 capf) :exit-function))
             (candidates (all-completions "dia" table))
             (sorted (funcall sorter candidates))
             (visible (mapcar
                       (lambda (candidate)
                         (or (get-text-property 0 'new-tag-name candidate)
                             (substring-no-properties candidate)))
                       sorted))
             (new (cadr sorted))
             committed
             create-if-needed)
        (should (equal "diary" (funcall table "dia" nil nil)))
        (should-not (funcall table "dia" nil 'lambda))
        (should (equal '("diary" "dia" "diary/happy") visible))
        (should (get-text-property 0 'is-new-tag new))
        (should-not (equal "dia" (substring-no-properties new)))
        (should (equal "dia"
                       (substring-no-properties
                        (car (car (funcall affix (list new)))))))
        (cl-letf (((symbol-function
                    'supertag-completion--post-completion-action)
                   (lambda (_) (setq committed t))))
          (funcall exit new nil))
        (should-not committed)
        (insert " ")
        (cl-letf (((symbol-function 'supertag-ops-add-tag-to-node)
                   (lambda (&rest _) (setq committed t))))
          (supertag-completion--auto-record-on-boundary))
        (should-not committed)
        (delete-char -1)
        (cl-letf (((symbol-function 'org-id-get-create) (lambda () "node"))
                  ((symbol-function 'supertag-node-get)
                   (lambda (_) '(:id "node")))
                  ((symbol-function 'supertag-ops-add-tag-to-node)
                   (lambda (_node _tag &rest args)
                     (setq create-if-needed
                           (plist-get args :create-if-needed))
                     t)))
          (supertag-completion--post-completion-action new)
          (should create-if-needed)
          (setq create-if-needed 'unset)
          (supertag-completion--post-completion-action (car sorted)))
        (should-not create-if-needed)))))

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

(ert-deftest tag-path-shared-reader-searches-leaf-and-displays-parent-path ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "diary")
    (tag-path-test--put-tag "happy" "diary")
    (let (seen display)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq seen (mapcar #'substring-no-properties collection)
                       display
                       (car (funcall
                             (plist-get completion-extra-properties
                                        :affixation-function)
                             '("happy"))))
                 "happy")))
      (should
       (equal "happy" (supertag-ui-read-tag "Tag: ")))
      (should (equal '("diary" "happy") seen))
      (should (equal "diary/" (nth 1 display)))))))

(ert-deftest nested-tag-shared-reader-rejects-virtual-namespace ()
  (let (seen)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq seen (mapcar #'substring-no-properties collection))
                 "Apple")))
      (should-error
       (supertag-ui-read-tag
        "Tag: " '("Apple/Shortcut/\u8bed\u8a00" "diary/work") nil nil t)
       :type 'user-error)
      (should
       (equal '("Apple/Shortcut/\u8bed\u8a00" "diary/work")
              seen)))))

(ert-deftest nested-tag-legacy-slash-rename-migrates-complete-identity ()
  (tag-path-test--with-clean-store
    (let ((file (make-temp-file "supertag-tag-path" nil ".org"
                                "* P #emacs/package\n"))
          (supertag-query-saved
           '(("packages" . "(has-tag \"emacs/package\")")))
          (supertag--view-configs (make-hash-table :test 'eq)))
      (unwind-protect
          (progn
            (supertag-store-put-entity
             :tags "emacs/package"
             '(:id "emacs/package" :name "emacs/package" :type :tag
               :fields ((:name "tier" :type :string)
                        (:name "related" :type :tag)
                        (:name "plain-text" :type :string))))
            (supertag-store-put-entity
             :nodes "node"
             (list :id "node" :title "node" :type :node :file file
                   :tags '("emacs/package")))
            (supertag-store-put-tag-field-associations
             "emacs/package" '((:field-id "tier")))
            (supertag-store-put-legacy-field
             "node" "emacs/package" "tier" "core")
            (supertag-store-put-legacy-field
             "node" "emacs/package" "related" "emacs/package")
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
             '("emacs/package"))
            (supertag-store-put-field-value
             "node" "plain-global" "emacs/package")
            (supertag-store-put-entity
             :automations "auto"
             '(:id "auto" :tag "emacs/package" :action :notify))
            (supertag-store-put-entity
             :relations "schema-relation"
             (list :id "schema-relation" :type :schema-link
                   :from "emacs/package" :to "other"
                   :props '(:target-tag "emacs/package")))
            (puthash 'packages
                     '(:id packages :tag "emacs/package")
                     supertag--view-configs)
            (supertag--process-node-tags (supertag-node-get "node"))
            (supertag-tag-rename "emacs/package" "package")
            (should-not (supertag-tag-get "emacs/package"))
            (should (supertag-tag-get "package"))
            (should (equal '("package")
                           (plist-get (supertag-node-get "node") :tags)))
            (should (= 1 (length (supertag-relation-find-by-from
                                  "node" :node-tag))))
            (should (supertag-store-get-tag-field-associations
                     "package"))
            (should (equal "core"
                           (supertag-get
                            '(:fields "node" "package" "tier"))))
            (should (equal "package"
                           (supertag-get
                            '(:fields "node" "package" "related"))))
            (should (equal "emacs/package"
                           (supertag-get
                            '(:fields "node" "package" "plain-text"))))
            (should (equal '("package")
                           (supertag-store-get-field-value
                            "node" "related-tags")))
            (should (equal "emacs/package"
                           (supertag-store-get-field-value
                            "node" "plain-global")))
            (should
             (cl-some
              (lambda (relation)
                (and (equal "package" (plist-get relation :from))
                     (equal "package"
                            (plist-get
                             (plist-get relation :props)
                             :target-tag))))
              (supertag-relation-find-by-from "package" :schema-link)))
            (should (equal "package"
                           (plist-get
                            (supertag-store-get-entity
                             :automations "auto")
                            :tag)))
            (should (equal "(has-tag \"package\")"
                           (cdr (assoc "packages" supertag-query-saved))))
            (should (equal "package"
                           (plist-get
                            (gethash 'packages supertag--view-configs)
                            :tag)))
            (with-temp-buffer
              (insert-file-contents file)
              (should (string-match-p "#package\\(?:\\s-\\|$\\)"
                                      (buffer-string)))))
        (delete-file file)))))

(ert-deftest tag-rename-collision-leaves-store-unchanged ()
  (tag-path-test--with-clean-store
    (tag-path-test--put-tag "package")
    (tag-path-test--put-tag "lisp")
    (tag-path-test--put-node "node" "package")
    (should-error (supertag-tag-rename "package" "lisp"))
    (should (supertag-tag-get "package"))
    (should (supertag-tag-get "lisp"))
    (should (equal '("package")
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

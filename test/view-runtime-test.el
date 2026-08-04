;;; view-runtime-test.el --- View Runtime contract tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(when load-file-name
  (add-to-list 'load-path
               (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-view-framework)
(require 'supertag-ui-search)

(ert-deftest test-view-runtime-open-rejects-unknown-view ()
  "Opening an unknown view must fail at the public seam."
  (supertag-view-framework-init)
  (should-error (supertag-view-open 'missing-view nil)
                :type 'user-error))

(ert-deftest test-view-runtime-open-builds-and-renders-state ()
  "Opening a view must build state, render it, and display its buffer."
  (supertag-view-framework-init)
  (let ((buffer-name " *supertag-runtime-open*")
        displayed-buffer
        displayed-action)
    (unwind-protect
        (progn
          (supertag-view-register
           :id 'runtime-open
           :name "Runtime Open"
           :buffer-name-fn (lambda (_input) buffer-name)
           :mode-fn #'fundamental-mode
           :state-fn (lambda (input)
                       (list :text (plist-get input :text)))
           :render-fn (lambda (state)
                        (erase-buffer)
                        (insert (plist-get state :text))))
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (buffer action &rest _args)
                       (setq displayed-buffer buffer
                             displayed-action action)
                       nil)))
            (let* ((action 'test-display-action)
                   (buffer (supertag-view-open
                            'runtime-open '(:text "hello") action)))
              (should (buffer-live-p buffer))
              (should (eq buffer displayed-buffer))
              (should (eq action displayed-action))
              (with-current-buffer buffer
                (should (eq major-mode 'fundamental-mode))
                (should (equal (buffer-string) "hello"))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-runtime-reopen-reuses-buffer-and-cleans-old-instance ()
  "Reopening a view must replace, not stack, its live subscription."
  (supertag-view-framework-init)
  (let ((buffer-name " *supertag-runtime-reopen*")
        (subscribe-count 0)
        (cleanup-count 0))
    (unwind-protect
        (progn
          (supertag-view-register
           :id 'runtime-reopen
           :name "Runtime Reopen"
           :buffer-name buffer-name
           :mode-fn #'fundamental-mode
           :state-fn #'identity
           :render-fn (lambda (state)
                        (erase-buffer)
                        (insert (plist-get state :text)))
           :subscribe-fn
           (lambda (_input _state _refresh)
             (cl-incf subscribe-count)
             (lambda () (cl-incf cleanup-count))))
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((first (supertag-view-open
                          'runtime-reopen '(:text "one")))
                  second)
              (setq second (supertag-view-open
                            'runtime-reopen '(:text "two")))
              (should (eq first second))
              (should (= subscribe-count 2))
              (should (= cleanup-count 1))
              (with-current-buffer second
                (should (equal (buffer-string) "two"))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-runtime-refresh-rebuilds-state-and-restores-selection ()
  "Refreshing must rebuild from the original input and restore opaque selection."
  (supertag-view-framework-init)
  (let ((buffer-name " *supertag-runtime-refresh*")
        (source "one")
        restored)
    (unwind-protect
        (progn
          (supertag-view-register
           :id 'runtime-refresh
           :name "Runtime Refresh"
           :buffer-name buffer-name
           :mode-fn #'fundamental-mode
           :state-fn (lambda (_input) source)
           :render-fn (lambda (state)
                        (erase-buffer)
                        (insert state))
           :capture-selection-fn (lambda () 'selected-entity)
           :restore-selection-fn (lambda (selection)
                                   (setq restored selection)))
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((buffer (supertag-view-open 'runtime-refresh nil)))
              (setq source "two")
              (supertag-view-refresh buffer)
              (with-current-buffer buffer
                (should (equal (buffer-string) "two")))
              (should (eq restored 'selected-entity)))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-runtime-kill-runs-all-cleanup-callbacks ()
  "Killing a view must run every cleanup even when one callback fails."
  (supertag-view-framework-init)
  (let ((buffer-name " *supertag-runtime-kill*")
        calls)
    (supertag-view-register
     :id 'runtime-kill
     :name "Runtime Kill"
     :buffer-name buffer-name
     :mode-fn #'fundamental-mode
     :render-fn #'ignore
     :subscribe-fn
     (lambda (_input _state _refresh)
       (list (lambda ()
               (push 'first calls)
               (error "cleanup failed"))
             (lambda () (push 'second calls)))))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (let ((buffer (supertag-view-open 'runtime-kill nil)))
        (kill-buffer buffer)
        (should-not (buffer-live-p buffer))
        (should (equal (sort calls
                             (lambda (a b)
                               (string< (symbol-name a) (symbol-name b))))
                       '(first second)))))))

(ert-deftest test-view-runtime-open-error-does-not-publish-instance ()
  "A failed open must not leave a refreshable half-instance behind."
  (supertag-view-framework-init)
  (let ((buffer-name " *supertag-runtime-error*"))
    (unwind-protect
        (progn
          (supertag-view-register
           :id 'runtime-error
           :name "Runtime Error"
           :buffer-name buffer-name
           :mode-fn #'fundamental-mode
           :render-fn #'ignore
           :subscribe-fn (lambda (&rest _args)
                           (error "subscribe failed")))
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (should-error (supertag-view-open 'runtime-error nil))
            (should-error (supertag-view-refresh (get-buffer buffer-name))
                          :type 'user-error)))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-runtime-display-error-cleans-subscription ()
  "A failed display action must roll back the new Runtime instance."
  (supertag-view-framework-init)
  (let ((buffer-name " *supertag-runtime-display-error*")
        (cleanup-count 0))
    (unwind-protect
        (progn
          (supertag-view-register
           :id 'runtime-display-error
           :name "Runtime Display Error"
           :buffer-name buffer-name
           :mode-fn #'fundamental-mode
           :render-fn #'ignore
           :subscribe-fn
           (lambda (&rest _args)
             (lambda () (cl-incf cleanup-count))))
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (&rest _args) (error "display failed"))))
            (should-error (supertag-view-open 'runtime-display-error nil)))
          (should (= cleanup-count 1))
          (with-current-buffer (get-buffer buffer-name)
            (should-not supertag-view--instance)))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-runtime-subscription-callback-ignores-dead-buffer ()
  "A late event must not refresh a killed view buffer."
  (supertag-view-framework-init)
  (let ((buffer-name " *supertag-runtime-dead*")
        refresh-callback)
    (supertag-view-register
     :id 'runtime-dead
     :name "Runtime Dead"
     :buffer-name buffer-name
     :mode-fn #'fundamental-mode
     :render-fn #'ignore
     :subscribe-fn (lambda (_input _state refresh)
                     (setq refresh-callback refresh)
                     #'ignore))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (let ((buffer (supertag-view-open 'runtime-dead nil)))
        (kill-buffer buffer)
        (should-not (buffer-live-p buffer))
        (should (functionp refresh-callback))
        (should-not (funcall refresh-callback :store-changed))))))

(ert-deftest test-view-runtime-refresh-state-error-keeps-instance-usable ()
  "A failed state rebuild must leave the previous view available for retry."
  (supertag-view-framework-init)
  (let ((buffer-name " *supertag-runtime-retry*")
        (source "stable")
        fail)
    (unwind-protect
        (progn
          (supertag-view-register
           :id 'runtime-retry
           :name "Runtime Retry"
           :buffer-name buffer-name
           :mode-fn #'fundamental-mode
           :state-fn (lambda (_input)
                       (if fail (error "state failed") source))
           :render-fn (lambda (state)
                        (erase-buffer)
                        (insert state)))
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((buffer (supertag-view-open 'runtime-retry nil)))
              (setq fail t)
              (should-error (supertag-view-refresh buffer))
              (with-current-buffer buffer
                (should (equal (buffer-string) "stable")))
              (setq fail nil
                    source "recovered")
              (supertag-view-refresh buffer)
              (with-current-buffer buffer
                (should (equal (buffer-string) "recovered"))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-runtime-search-show-results-opens-refreshable-buffer ()
  "The existing Search entry must open a Runtime-managed results buffer."
  (supertag-view-framework-init)
  (let* ((buffer-name "*Org SuperTag Search*")
         (node '(:id "search-1" :title "First result"))
         (results (list (cons node nil))))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer) #'ignore)
                  ((symbol-function 'supertag-search--get-node-tags) #'ignore))
          (let ((buffer (supertag-search-show-results '("first") results)))
            (should (buffer-live-p buffer))
            (should (equal (buffer-name buffer) buffer-name))
            (with-current-buffer buffer
              (should org-supertag-search-mode)
              (should (string-match-p "First result" (buffer-string))))
            (supertag-view-refresh buffer)
            (with-current-buffer buffer
              (should (string-match-p "First result" (buffer-string))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-runtime-search-refresh-preserves-selected-entity ()
  "Search refresh must preserve the selected result by common entity ID."
  (supertag-view-framework-init)
  (let* ((buffer-name "*Org SuperTag Search*")
         (results (list (cons '(:id "search-1" :title "First") nil)
                        (cons '(:id "search-2" :title "Second") nil))))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer) #'ignore)
                  ((symbol-function 'supertag-search--get-node-tags) #'ignore))
          (let ((buffer (supertag-search-show-results '("result") results)))
            (with-current-buffer buffer
              (goto-char (point-min))
              (let ((match (text-property-search-forward
                            'node-id "search-2" t)))
                (should match)
                (goto-char (prop-match-beginning match))))
            (supertag-view-refresh buffer)
            (with-current-buffer buffer
              (should (equal (get-text-property (point) 'node-id)
                             "search-2"))
              (should (equal (get-text-property (point) 'supertag-entity-id)
                             "search-2")))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-runtime-search-command-refreshes-from-store ()
  "The Search command must rebuild results from Store on manual refresh."
  (supertag-view-framework-init)
  (let ((buffer-name "*Org SuperTag Search*")
        (origin (generate-new-buffer " *supertag-search-origin*"))
        (supertag--store (make-hash-table :test 'equal))
        (supertag-search-history-file
         (make-temp-name (expand-file-name "supertag-search-history-"
                                           temporary-file-directory))))
    (unwind-protect
        (progn
          (supertag--ensure-store)
          (supertag-store-put-entity
           :nodes "search-old"
           '(:id "search-old" :title "first old" :content ""))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'supertag-search--get-keywords)
                     (lambda () '("first"))))
            (let ((buffer (with-current-buffer origin
                            (supertag-search))))
              (with-current-buffer buffer
                (should (string-match-p "first old" (buffer-string))))
              (remhash "search-old" (supertag-store-get-collection :nodes))
              (supertag-store-put-entity
               :nodes "search-new"
               '(:id "search-new" :title "first new" :content ""))
              (supertag-view-refresh buffer)
              (with-current-buffer buffer
                (should-not (string-match-p "first old" (buffer-string)))
                (should (string-match-p "first new" (buffer-string)))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer))
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest test-view-runtime-search-quit-restores-origin ()
  "Search quit must kill results and restore the saved origin point."
  (supertag-view-framework-init)
  (let ((buffer-name "*Org SuperTag Search*")
        (origin (generate-new-buffer " *supertag-search-quit-origin*")))
    (unwind-protect
        (progn
          (with-current-buffer origin
            (insert "origin")
            (goto-char 4))
          (setq supertag-search--original-buffer origin
                supertag-search--original-point 4)
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'supertag-search--get-node-tags) #'ignore))
            (let ((buffer (supertag-search-show-results '("none") nil)))
              (with-current-buffer buffer
                (supertag-search-quit))
              (should-not (buffer-live-p buffer))
              (with-current-buffer origin
                (should (= (point) 4))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer))
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest test-view-runtime-dsl-view-opens-and-refreshes-in-place ()
  "A declarative Widget DSL view must use the Runtime buffer lifecycle."
  (supertag-view-framework-init)
  (let ((source "one")
        (buffer-name "*View: Runtime DSL - demo*"))
    (unwind-protect
        (progn
          (supertag-view-define-from-config
           (list :id 'runtime-dsl
                 :name "Runtime DSL"
                 :tag "demo"
                 :widgets
                 (list (list :type :text
                             :content (lambda (_context) source)))))
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((buffer
                   (supertag-view-open
                    'runtime-dsl '(:tag "demo" :nodes nil))))
              (with-current-buffer buffer
                (should (string-match-p "one" (buffer-string))))
              (setq source "two")
              (supertag-view-refresh buffer)
              (with-current-buffer buffer
                (should (string-match-p "two" (buffer-string)))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-runtime-dsl-selection-uses-runtime-open ()
  "Selecting a declarative view must create its Runtime buffer."
  (supertag-view-framework-init)
  (let ((buffer-name "*View: Selected DSL - demo*"))
    (unwind-protect
        (progn
          (supertag-view-define-from-config
           '(:id selected-dsl
             :name "Selected DSL"
             :tag "demo"
             :widgets ((:type :text :content "selected"))))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _args) "Selected DSL"))
                    ((symbol-function 'display-buffer) #'ignore))
            (supertag-view-select-and-render "demo")
            (let ((buffer (get-buffer buffer-name)))
              (should (buffer-live-p buffer))
              (with-current-buffer buffer
                (should (string-match-p "selected" (buffer-string)))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-runtime-stream-shaped-adapter-needs-no-special-case ()
  "A Stream-shaped adapter must work using only the public Runtime contract."
  (supertag-view-framework-init)
  (let ((buffer-name " *supertag-stream-fixture*")
        (bodies '("First body" "Second body")))
    (unwind-protect
        (progn
          (supertag-view-register
           :id 'stream-fixture
           :name "Stream Fixture"
           :runtime t
           :selectable nil
           :buffer-name buffer-name
           :mode-fn #'special-mode
           :state-fn (lambda (_input) bodies)
           :render-fn (lambda (state)
                        (erase-buffer)
                        (dolist (body state)
                          (insert body "\n\n"))))
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((buffer (supertag-view-open 'stream-fixture '(:tag "demo"))))
              (with-current-buffer buffer
                (should (equal (buffer-string)
                               "First body\n\nSecond body\n\n")))
              (setq bodies '("Updated body"))
              (supertag-view-refresh buffer)
              (with-current-buffer buffer
                (should (equal (buffer-string) "Updated body\n\n"))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(provide 'view-runtime-test)

;;; view-runtime-test.el ends here

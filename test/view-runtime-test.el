;;; view-runtime-test.el --- View Runtime contract tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(when load-file-name
  (add-to-list 'load-path
               (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-view-framework)
(require 'supertag-ui-search)

(ert-deftest test-view-runtime-picker-always-uses-public-open ()
  "The custom-view picker must not route definitions around the Runtime."
  (supertag-view-framework-init)
  (let (opened)
    (supertag-view-register
     :id 'picker-runtime
     :name "Picker Runtime"
     :render-fn #'ignore)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Picker Runtime"))
              ((symbol-function 'supertag-view--build-context)
               (lambda (_query) '(:tag "demo" :nodes nil)))
              ((symbol-function 'supertag-view-open)
               (lambda (id input &optional _display-action)
                 (setq opened (list id input)))))
      (supertag-view-select-and-render "demo"))
    (should (equal opened
                   '(picker-runtime (:tag "demo" :nodes nil))))))

(ert-deftest test-view-runtime-built-in-dashboards-open-and-refresh ()
  "Built-in dashboards must use the single Runtime buffer lifecycle."
  (supertag-view-framework-init)
  (dolist (file '("supertag-view-progress-dashboard"
                  "supertag-view-effort-distribution"
                  "supertag-view-priority-matrix"))
    (load file nil t))
  (dolist (case '((progress-dashboard
                   "Progress Dashboard"
                   "No projects found."
                   supertag-view-progress--render)
                  (effort-distribution
                   "Effort Distribution"
                   "No effort data found."
                   supertag-view-effort--render)
                  (priority-matrix
                   "Priority Matrix"
                   "Eisenhower Matrix"
                   supertag-view-priority--render)))
    (pcase-let ((`(,id ,name ,body-text ,old-render) case))
      (let* ((tag "demo")
             (buffer-name (format "*View: %s - demo*" name))
             (input (list :tag tag :nodes nil
                          :context-builder
                          (lambda () (list :tag tag :nodes nil)))))
        (unwind-protect
            (cl-letf (((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'supertag-find-nodes-by-tag)
                       (lambda (_tag) nil)))
              (let ((view (supertag-view-get id))
                    (buffer (supertag-view-open id input)))
                (should (eq (plist-get view :mode-fn)
                            #'supertag-view-widget-mode))
                (should-not (fboundp old-render))
                (should (equal (buffer-name buffer) buffer-name))
                (with-current-buffer buffer
                  (should (eq (plist-get supertag-view--instance :view-id) id))
                  (should (string-match-p body-text (buffer-string))))
                (setq tag "updated")
                (supertag-view-refresh buffer)
                (with-current-buffer buffer
                  (should (string-match-p
                           (format "%s - #updated" name)
                           (buffer-string))))))
          (when-let* ((buffer (get-buffer buffer-name)))
            (kill-buffer buffer)))))))

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

(ert-deftest test-view-runtime-dsl-restores-keyed-selection-after-refresh ()
  "DSL refresh must restore the same keyed region and fall back safely."
  (supertag-view-framework-init)
  (let ((prefix "Short")
        (show-target t)
        (buffer-name "*View: Keyed DSL - demo*"))
    (unwind-protect
        (progn
          (supertag-view-define-from-config
           (list :id 'keyed-dsl
                 :name "Keyed DSL"
                 :tag "demo"
                 :persist nil
                 :widgets
                 (lambda (_context)
                   (append
                    (list (list :type :text :content prefix))
                    (when show-target
                      (list (list :type :text
                                  :key 'delete
                                  :content "Target")))))))
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((buffer (supertag-view-open
                           'keyed-dsl '(:tag "demo" :nodes nil))))
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward "Target")
                (goto-char (- (point) 4)))
              (setq prefix "A much longer prefix")
              (supertag-view-refresh buffer)
              (with-current-buffer buffer
                (should (eq (get-text-property
                             (point) 'supertag-widget-key)
                            'delete))
                (should (eq (char-after) ?r)))
              (setq show-target nil)
              (supertag-view-refresh buffer)
              (with-current-buffer buffer
                (should (= (point) (point-min)))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-runtime-dsl-uses-native-buttons-and-editable-field ()
  "DSL actions and fields must use real built-in Emacs primitives."
  (supertag-view-framework-init)
  (let ((activated nil)
        (changed nil)
        (buffer-name "*View: Native DSL - demo*"))
    (unwind-protect
        (progn
          (supertag-view-define-from-config
           (list :id 'native-dsl
                 :name "Native DSL"
                 :tag "demo"
                 :persist nil
                 :widgets
                 (list
                  (list :type :button :key 'run :label "Run"
                        :action (lambda () (setq activated 'button)))
                  (list :type :link :key 'open :label "Open"
                        :action (lambda () (setq activated 'link)))
                  (list :type :editable-field :key 'title
                        :value "old" :width 10
                        :on-change (lambda (value) (setq changed value))))))
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((buffer (supertag-view-open
                           'native-dsl '(:tag "demo" :nodes nil))))
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward "Run")
                (button-activate (button-at (1- (point))))
                (should (eq activated 'button))
                (search-forward "Open")
                (button-activate (button-at (1- (point))))
                (should (eq activated 'link))
                (search-forward "old")
                (let ((field (widget-at (- (point) 2))))
                  (should field)
                  (widget-value-set field "new")
                  (widget-apply field :notify field nil)
                  (should (equal changed "new")))
                (goto-char (point-min))
                (should (eq (key-binding "x") 'undefined))
                (supertag-view-widget-forward 1)
                (should (equal (button-label (button-at (point))) "Open"))
                (supertag-view-widget-forward 1)
                (should (widget-at (point)))
                (supertag-view-widget-backward 1)
                (should (equal (button-label (button-at (point)))
                               "Open"))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-runtime-dsl-materializes-interaction-after-layout ()
  "Interactive leaves must remain live after columns/cards and refresh."
  (supertag-view-framework-init)
  (let ((activated 0)
        (buffer-name "*View: Layout DSL - demo*"))
    (unwind-protect
        (progn
          (supertag-view-define-from-config
           (list :id 'layout-dsl
                 :name "Layout DSL"
                 :tag "demo"
                 :persist nil
                 :widgets
                 (list
                  (list :type :columns
                        :columns
                        (list
                         (list :width 12
                               :children
                               (list
                                (list :type :button :key 'run
                                      :label "Run"
                                      :action
                                      (lambda () (setq activated
                                                         (1+ activated))))))
                         (list :width 12
                               :children
                               (list (list :type :text
                                           :content "Status")))))
                  (list :type :card :title "Editor" :width 14
                        :children
                        (list
                         (list :type :editable-field :key 'title
                               :value "旧值" :width 8
                               :on-change #'ignore))))))
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((buffer (supertag-view-open
                           'layout-dsl '(:tag "demo" :nodes nil))))
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward "Run")
                (button-activate (button-at (1- (point))))
                (should (= activated 1))
                (search-forward "旧值")
                (should (widget-at (- (point) 2)))
                (let ((inhibit-field-text-motion t))
                  (should (= (string-width
                              (buffer-substring
                               (line-beginning-position)
                               (line-end-position)))
                             18)))
                (should (= (length widget-field-list) 1))
                (let ((overlay-count (length (overlays-in
                                              (point-min) (point-max)))))
                  (supertag-view-refresh buffer)
                  (should (= (length widget-field-list) 1))
                  (should (= (length (overlays-in
                                      (point-min) (point-max)))
                             overlay-count)))
                (goto-char (point-min))
                (search-forward "Run")
                (button-activate (button-at (1- (point))))
                (should (= activated 2))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-demo-dashboard-showcases-all-widgets-interactively ()
  "The in-memory DSL demo must cover every widget and preserve interactions."
  (let* ((example-dir (expand-file-name "doc/examples" default-directory))
         (load-path (cons example-dir load-path))
         (buffer-name "*View: Demo Dashboard - demo*"))
    (require 'supertag-view-demo-dashboard)
    (unwind-protect
        (progn
          (supertag-view-framework-init)
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((buffer (supertag-view-demo-dashboard-open "demo")))
              (with-current-buffer buffer
                (cl-labels
                    ((types
                      (widgets)
                      (mapcan
                       (lambda (widget)
                         (append
                          (list (supertag-widget--normalize-type
                                 (plist-get widget :type)))
                          (types (plist-get widget :children))
                          (let ((columns (plist-get widget :columns)))
                            (when (functionp columns)
                              (setq columns (funcall columns nil)))
                            (mapcan (lambda (column)
                                      (types (plist-get column :children)))
                                    columns))))
                       widgets)))
                  (let ((showcased
                         (types (plist-get
                                 supertag-view-demo-dashboard--config
                                 :widgets))))
                    (maphash (lambda (type _renderer)
                               (should (memq type showcased)))
                             supertag--widget-registry)))
                (should (string-match-p "60%" (buffer-string)))
                (should-not (string-match-p "…" (buffer-string)))
                (should-not (string-match-p "[—→]" (buffer-string)))
                (goto-char (point-min))
                (search-forward "Increment")
                (button-activate (button-at (1- (point))))
                (should (string-match-p "Button clicks: 1" (buffer-string)))
                (goto-char (point-min))
                (search-forward "Sample link")
                (button-activate (button-at (1- (point))))
                (should (string-match-p "Link activations: 1" (buffer-string)))
                (goto-char (point-min))
                (search-forward "editable-field")
                (search-forward "Project Alpha")
                (let ((field (widget-at (1- (point)))))
                  (widget-value-set field "Renamed")
                  (widget-apply field :notify field nil))
                (supertag-view-refresh)
                (should (equal
                         (plist-get
                          supertag-view-demo-dashboard--interaction-state
                          :edited-value)
                         "Renamed"))
                (should (string-match-p "Edited value: Renamed"
                                        (buffer-string)))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest test-view-demo-dashboard-collapses-below-minimum-width ()
  "The demo must switch between two columns and one at its breakpoint."
  (let* ((example-dir (expand-file-name "doc/examples" default-directory))
         (load-path (cons example-dir load-path))
         (buffer-name "*View: Demo Dashboard - demo*")
         (frame (selected-frame))
         (original-width (window-total-width)))
    (require 'supertag-view-demo-dashboard)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-frame-width frame 120)
          (supertag-view-framework-init)
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let* ((buffer (supertag-view-demo-dashboard-open "demo"))
                   (wide-window (selected-window)))
              (set-window-buffer wide-window buffer)
              (with-current-buffer buffer
                (supertag-view-demo-dashboard--window-size-changed wide-window)
                (goto-char (point-min))
                (let ((nodes (search-forward "Nodes")))
                  (goto-char (point-min))
                  (should (< nodes (search-forward "Summary")))))
              (let ((narrow-window (split-window-right)))
                (set-window-buffer wide-window buffer)
                (with-current-buffer buffer
                  (supertag-view-demo-dashboard--window-size-changed wide-window)
                  (goto-char (point-min))
                  (let ((summary (search-forward "Summary")))
                    (goto-char (point-min))
                    (should (< summary (search-forward "Nodes")))))
                (delete-window narrow-window))
              (with-current-buffer buffer
                (supertag-view-demo-dashboard--window-size-changed wide-window)
                (goto-char (point-min))
                (let ((nodes (search-forward "Nodes")))
                  (goto-char (point-min))
                  (should (< nodes (search-forward "Summary"))))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer))
      (set-frame-width frame original-width))))

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
  "A keyed interactive Stream must need no Runtime special case."
  (supertag-view-framework-init)
  (let ((buffer-name "*View: Stream Fixture - demo*")
        (nodes '(("node-1" . "First body")
                 ("node-2" . "Second body")))
        opened
        edited)
    (unwind-protect
        (progn
          (supertag-view-define-from-config
           (list
            :id 'stream-fixture
            :name "Stream Fixture"
            :tag "demo"
            :persist nil
            :widgets
            (lambda (_context)
              (mapcar
               (lambda (node)
                 (let ((id (car node))
                       (body (cdr node)))
                   (list
                    :type :card
                    :key (list 'node id)
                    :width 32
                    :children
                    (list
                     (list :type :text :key (list 'body id)
                           :content body)
                     (list :type :link :key (list 'tag id)
                           :label "#demo"
                           :action (lambda () (setq opened id)))
                     (list :type :button :key (list 'edit id)
                           :label "Edit"
                           :action (lambda () (setq edited id)))))))
               nodes))))
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((buffer (supertag-view-open 'stream-fixture '(:tag "demo"))))
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward "First body")
                (goto-char (- (point) 4)))
              (setq nodes (cons '("node-0" . "New body") nodes))
              (supertag-view-refresh buffer)
              (with-current-buffer buffer
                (should (equal (get-text-property
                                (point) 'supertag-widget-key)
                               '(body "node-1")))
                (goto-char (point-min))
                (search-forward "#demo")
                (button-activate (button-at (1- (point))))
                (should (equal opened "node-0"))
                (search-forward "Edit")
                (button-activate (button-at (1- (point))))
                (should (equal edited "node-0"))))))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer)))))

(provide 'view-runtime-test)

;;; view-runtime-test.el ends here

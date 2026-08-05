;;; view-framework-test.el --- Tests for supertag-view-framework -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the view framework registration and rendering system.

;;; Code:

(require 'ert)

;; Load the module under test
(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))
(require 'supertag-view-framework)
(require 'supertag-view-helper)
(require 'supertag-view-svg-tag)

;; Setup and teardown
(defun view-framework-test--setup ()
  "Initialize clean view framework for testing."
  (supertag-view-framework-init))

;; Tests for registration
(ert-deftest test-view-register-basic ()
  "Test basic view registration."
  (view-framework-test--setup)
  (let ((view (supertag-view-register
               :id 'test-view
               :name "Test View"
               :render-fn #'ignore)))
    (should view)
    (should (eq (plist-get view :id) 'test-view))
    (should (string= (plist-get view :name) "Test View"))
    (should (functionp (plist-get view :render-fn)))))

(ert-deftest test-view-register-with-optional-props ()
  "Test registration with optional properties."
  (view-framework-test--setup)
  (let ((view (supertag-view-register
               :id 'full-view
               :name "Full View"
               :description "A test view with all properties"
               :category :test
               :render-fn #'ignore
               :valid-for '("project" "task"))))
    (should (string= (plist-get view :description) "A test view with all properties"))
    (should (eq (plist-get view :category) :test))
    (should (equal (plist-get view :valid-for) '("project" "task")))))

(ert-deftest test-view-register-error-no-id ()
  "Test that registration fails without :id."
  (view-framework-test--setup)
  (should-error (supertag-view-register
                 :name "No ID View"
                 :render-fn #'ignore)))

(ert-deftest test-view-register-error-no-name ()
  "Test that registration fails without :name."
  (view-framework-test--setup)
  (should-error (supertag-view-register
                 :id 'no-name-view
                 :render-fn #'ignore)))

(ert-deftest test-view-register-error-no-render-fn ()
  "Test that registration fails without :render-fn."
  (view-framework-test--setup)
  (should-error (supertag-view-register
                 :id 'no-render-view
                 :name "No Render View")))

;; Tests for unregistration
(ert-deftest test-view-unregister ()
  "Test view unregistration."
  (view-framework-test--setup)
  (supertag-view-register
   :id 'to-remove
   :name "To Remove"
   :render-fn #'ignore)
  (should (supertag-view-get 'to-remove))
  (let ((removed (supertag-view-unregister 'to-remove)))
    (should removed)
    (should (eq (plist-get removed :id) 'to-remove))
    (should-not (supertag-view-get 'to-remove))))

;; Tests for listing
(ert-deftest test-view-list-empty ()
  "Test listing when no views registered."
  (view-framework-test--setup)
  (should (null (supertag-view-list))))

(ert-deftest test-view-list-multiple ()
  "Test listing multiple views."
  (view-framework-test--setup)
  (supertag-view-register :id 'view-a :name "View A" :render-fn #'ignore)
  (supertag-view-register :id 'view-b :name "View B" :render-fn #'ignore)
  (supertag-view-register :id 'view-c :name "View C" :render-fn #'ignore)
  (let ((list (supertag-view-list)))
    (should (= (length list) 3))
    ;; Should be sorted by name
    (should (string= (plist-get (nth 0 list) :name) "View A"))
    (should (string= (plist-get (nth 1 list) :name) "View B"))
    (should (string= (plist-get (nth 2 list) :name) "View C"))))

(ert-deftest test-view-list-for-tag-hides-internal-adapters ()
  "Internal adapters must not appear in the custom view picker."
  (view-framework-test--setup)
  (supertag-view-register
   :id 'visible-view :name "Visible" :render-fn #'ignore)
  (supertag-view-register
   :id 'hidden-adapter :name "Hidden" :render-fn #'ignore
   :selectable nil)
  (should (equal (mapcar (lambda (view) (plist-get view :id))
                         (supertag-view-list-for-tag "demo"))
                 '(visible-view))))

;; Tests for rendering utilities
(ert-deftest test-view-header ()
  "Test header insertion."
  (with-temp-buffer
    (supertag-view--header "Test Header")
    (should (string-match-p "Test Header" (buffer-string)))
    (should (string-match-p "===========" (buffer-string)))))

(ert-deftest test-view-progress-bar ()
  "Test progress bar insertion."
  (with-temp-buffer
    (supertag-view--progress-bar 50 10)
    (let ((content (buffer-string)))
      (should (string-match-p "\\[" content))
      (should (string-match-p "\\]" content))
      (should (string-match-p "50%" content)))))

(ert-deftest test-view-stat-row ()
  "Test stat row insertion."
  (with-temp-buffer
    (supertag-view--stat-row '(("Total" . 100) ("Done" . 80)))
    (let ((content (buffer-string)))
      (should (string-match-p "Total: 100" content))
      (should (string-match-p "Done: 80" content)))))

(ert-deftest test-view-style-enables-existing-org-buffers ()
  "Late loading must enable styling in Org buffers that already exist."
  (let ((org-buffer (generate-new-buffer " *supertag-existing-org*"))
        (text-buffer (generate-new-buffer " *supertag-existing-text*"))
        (supertag-view-style-auto-enable t))
    (unwind-protect
        (progn
          (with-current-buffer org-buffer
            (org-mode)
            (supertag-view-style-mode -1))
          (with-current-buffer text-buffer
            (text-mode))
          (let ((supertag-view-style-auto-enable nil))
            (supertag-view-helper--enable-existing-org-buffers))
          (with-current-buffer org-buffer
            (should-not supertag-view-style-mode))
          (supertag-view-helper--enable-existing-org-buffers)
          (with-current-buffer org-buffer
            (should supertag-view-style-mode)
            (insert "plain #tag")
            (cl-letf (((symbol-function 'display-graphic-p)
                       (lambda (&optional _frame) t))
                      ((symbol-function 'supertag-svg-tag--get-cached)
                       (lambda (_tag) '(image :type svg :data "dummy"))))
              (supertag-svg-tag--refresh-all-buffers)
              (font-lock-ensure))
            (goto-char (point-min))
            (search-forward "#")
            (should (get-text-property (1- (point)) 'display)))
          (with-current-buffer text-buffer
            (should-not supertag-view-style-mode)))
      (kill-buffer org-buffer)
      (kill-buffer text-buffer))))

(ert-deftest test-view-style-face-stops-before-adjacent-org-link ()
  "Face font-lock must style only the range-aware Tag token."
  (let ((supertag-view-style-auto-enable nil)
        (supertag-svg-tag-enable nil))
    (with-temp-buffer
      (org-mode)
      (insert "* T #outer[[id:n][label]]\n")
      (supertag-view-style-mode 1)
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "#outer")
      (let ((tag-start (- (point) (length "#outer")))
            (link-start (point))
            (label-start (progn (search-forward "label")
                                (- (point) (length "label")))))
        (should (eq (get-text-property tag-start 'face)
                    'supertag-inline-face))
        (should-not (eq (get-text-property link-start 'face)
                        'supertag-inline-face))
        (should-not (eq (get-text-property label-start 'face)
                        'supertag-inline-face))))))

(ert-deftest test-view-style-svg-stops-before-adjacent-org-link ()
  "SVG font-lock must not replace the Org link following a Tag token."
  (let ((supertag-view-style-auto-enable nil)
        (supertag-svg-tag-enable t))
    (with-temp-buffer
      (org-mode)
      (insert "* T #outer[[id:n][label]]\n")
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional _frame) t))
                ((symbol-function 'supertag-svg-tag--get-cached)
                 (lambda (_tag) '(image :type svg :data "dummy"))))
        (supertag-view-style-mode 1)
        (font-lock-ensure))
      (goto-char (point-min))
      (search-forward "#outer")
      (let ((tag-start (- (point) (length "#outer")))
            (link-start (point))
            (label-start (progn (search-forward "label")
                                (- (point) (length "label")))))
        (should (equal (get-text-property tag-start 'display)
                       '(image :type svg :data "dummy")))
        (should-not (get-text-property link-start 'display))
        (should-not (get-text-property label-start 'display))))))

(provide 'view-framework-test)

;;; view-framework-test.el ends here

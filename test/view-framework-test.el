;;; view-framework-test.el --- Tests for supertag-view-framework -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the view framework registration and rendering system.

;;; Code:

(require 'ert)

;; Load the module under test
(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))
(require 'supertag-view-framework)

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

;; Tests for the macro
(ert-deftest test-define-supertag-view-macro ()
  "Test the define-supertag-view macro."
  (view-framework-test--setup)
  (define-supertag-view macro-test "Macro Test"
    (tag nodes)
    (insert (format "Tag: %s, Nodes: %d" tag (length nodes))))
  
  ;; Check that view was registered
  (let ((view (supertag-view-get 'macro-test)))
    (should view)
    (should (string= (plist-get view :name) "Macro Test"))
    (should (functionp (plist-get view :render-fn)))))

(ert-deftest test-define-supertag-view-rendering ()
  "Test that macro-defined view renders correctly."
  (view-framework-test--setup)
  (define-supertag-view render-test "Render Test"
    (tag nodes)
    (insert (format "%s:%d" tag (length nodes))))
  
  (with-temp-buffer
    (supertag-view-render 'render-test
                         (list :tag "project" :nodes '(1 2 3)))
    ;; The render function should insert into a buffer
    ;; We can't easily test the output buffer, but at least it shouldn't error
    ))

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

;; Manual verification helper
(defun test-view-framework-manual ()
  "Run manual view framework tests."
  (interactive)
  (message "=== View Framework Tests ===")
  
  (supertag-view-framework-init)
  
  ;; Register test views using macro
  (message "\nRegistering test views with macro...")
  (define-supertag-view progress-dashboard "Progress Dashboard"
    (tag nodes)
    (supertag-view--with-buffer "Progress" tag
      (supertag-view--header "Progress Dashboard")
      (insert (format "Tag: %s\n" tag))
      (insert (format "Nodes: %d\n\n" (length nodes)))
      (dolist (node nodes)
        (insert (format "- %s\n" (plist-get node :title))))))
  
  (define-supertag-view stats-view "Statistics View"
    (tag nodes)
    (supertag-view--with-buffer "Stats" tag
      (supertag-view--header "Statistics")
      (supertag-view--stat-row
       `(("Total" . ,(length nodes))
         ("Tag" . ,tag)))))
  
  ;; List all
  (message "\nAll views:")
  (dolist (v (supertag-view-list))
    (message "  - %s (%s)" (plist-get v :name) (plist-get v :id)))
  
  ;; Render demo
  (message "\nRendering 'progress-dashboard:")
  (supertag-view-render 'progress-dashboard
                       (list :tag "project"
                             :nodes (list
                                    (list :title "Project A")
                                    (list :title "Project B"))))
  
  (message "\n=== Tests Complete ==="))

(provide 'view-framework-test)

;;; view-framework-test.el ends here

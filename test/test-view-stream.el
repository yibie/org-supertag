;;; test-view-stream.el --- Stream View workflow tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(when load-file-name
  (add-to-list 'load-path
               (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-view-framework)
(require 'supertag-view-stream)

(defmacro supertag-view-stream-test--with-store (&rest body)
  "Run BODY with an isolated Store and subscriber table."
  (declare (indent 0))
  `(let ((supertag--store nil)
         (supertag--subscribers (make-hash-table :test 'equal)))
     (supertag--ensure-store)
     (supertag-view-stream--register-view)
     ,@body))

(defun supertag-view-stream-test--put-node
    (id title tags content created-at &optional file level)
  "Put a Stream fixture node with ID, TITLE, TAGS and CONTENT."
  (supertag-store-put-entity
   :nodes id
   (list :id id :type :node :title title :tags tags :content content
         :created-at created-at :file file :level (or level 1))))

(defun supertag-view-stream-test--put-tag (id &optional parent)
  "Put a Stream fixture Tag ID with optional PARENT."
  (supertag-store-put-entity
   :tags id (list :id id :name id :type :tag :extends parent)))

(defun supertag-view-stream-test--kill-buffers ()
  "Kill Stream test buffers without prompting."
  (dolist (buffer (buffer-list))
    (when (and (buffer-name buffer)
               (string-match-p
                "\\`\\*Supertag \\(Stream\\|Stream Index\\|Edit\\)"
                (buffer-name buffer)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(ert-deftest supertag-view-stream-state-includes-real-descendants-and-sorts ()
  "Stream state must include transitive :extends descendants only."
  (supertag-view-stream-test--with-store
    (supertag-view-stream-test--put-tag "diary")
    (supertag-view-stream-test--put-tag "happy" "diary")
    (supertag-view-stream-test--put-tag "private" "diary")
    (supertag-view-stream-test--put-tag "day" "private")
    (supertag-view-stream-test--put-tag "diaryx")
    (supertag-view-stream-test--put-tag "diary/legacy")
    (supertag-view-stream-test--put-node
     "late" "Late" '("diary") "late" '(0 30 0 0))
    (supertag-view-stream-test--put-node
     "early" "Early" '("happy") "early" '(0 10 0 0))
    (supertag-view-stream-test--put-node
     "lookalike" "Wrong" '("diaryx") "wrong" '(0 1 0 0))
    (supertag-view-stream-test--put-node
     "flat-slash" "Wrong" '("diary/legacy") "wrong" '(0 2 0 0))
    (supertag-view-stream-test--put-node
     "untimed-b" "B" '("day") "b" nil)
    (supertag-view-stream-test--put-node
     "untimed-a" "A" '("private") "a" nil)
    (let ((state (supertag-view-stream--build-state
                  '(:tag "diary" :layout split))))
      (should (eq (plist-get state :layout) 'split))
      (should
       (equal (mapcar (lambda (node) (plist-get node :id))
                      (plist-get state :nodes))
              '("early" "late" "untimed-a" "untimed-b"))))))

(ert-deftest supertag-view-stream-runtime-renders-full-lightweight-node-blocks ()
  "The real Runtime path must render tags, title and full Org body."
  (supertag-view-stream-test--with-store
    (unwind-protect
        (progn
          (supertag-view-stream-test--put-node
           "node-1" "Package archives"
           '("emacs/package" "emacs/package/elpa")
           "A paragraph.\n\n| Name | URL |\n| GNU | elpa.gnu.org |\n\n#+begin_quote\nKeep it small.\n#+end_quote"
           '(0 10 0 0) "/tmp/private-note.org")
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((buffer
                   (supertag-view-open
                    'stream '(:tag "emacs/package" :layout plain))))
              (with-current-buffer buffer
                (font-lock-ensure)
                (should (derived-mode-p 'supertag-view-stream-mode))
                (should (equal (plist-get supertag-view--instance :view-id)
                               'stream))
                (should (string-match-p "#emacs/package/elpa"
                                        (buffer-string)))
                (should (string-match-p "Package archives" (buffer-string)))
                (should (string-match-p "| GNU | elpa.gnu.org |"
                                        (buffer-string)))
                (should (string-match-p "Keep it small" (buffer-string)))
                (should-not (string-match-p "/tmp/private-note.org"
                                            (buffer-string)))
                (goto-char (point-min))
                (search-forward "Package archives")
                (let ((position (1- (point))))
                  (should (equal (get-text-property
                                  position 'supertag-entity-id)
                                 "node-1"))
                  (should (eq (get-text-property position 'font-lock-face)
                              'supertag-view-stream-title-face)))))))
      (supertag-view-stream-test--kill-buffers))))

(ert-deftest supertag-view-stream-refresh-restores-node-id-and-falls-back ()
  "Refresh must restore the same node, then fall back when it disappears."
  (supertag-view-stream-test--with-store
    (unwind-protect
        (progn
          (supertag-view-stream-test--put-node
           "node-1" "First" '("diary") "First body" '(0 10 0 0))
          (supertag-view-stream-test--put-node
           "node-2" "Second" '("diary") "Second body" '(0 20 0 0))
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((buffer
                   (supertag-view-open
                    'stream '(:tag "diary" :layout plain))))
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward "Second body")
                (should (equal (supertag-view-stream--current-node-id)
                               "node-2")))
              (supertag-view-stream-test--put-node
               "node-0" "Earlier" '("diary") "Earlier body" '(0 1 0 0))
              (supertag-view-refresh buffer)
              (with-current-buffer buffer
                (should (equal (supertag-view-stream--current-node-id)
                               "node-2")))
              (remhash "node-2" (supertag-store-get-collection :nodes))
              (supertag-view-refresh buffer)
              (with-current-buffer buffer
                (should (equal (supertag-view-stream--current-node-id)
                               "node-0"))))))
      (supertag-view-stream-test--kill-buffers))))

(ert-deftest supertag-view-stream-public-command-toggles-split-and-cleans-up ()
  "The public command must own one Runtime plus a removable index companion."
  (supertag-view-stream-test--with-store
    (unwind-protect
        (save-window-excursion
          (supertag-view-stream-test--put-tag "diary")
          (supertag-view-stream-test--put-tag "happy" "diary")
          (supertag-view-stream-test--put-node
           "node-1" "First title" '("diary")
           (mapconcat (lambda (number) (format "First body %d" number))
                      (number-sequence 1 80) "\n")
           '(0 10 0 0))
          (supertag-view-stream-test--put-node
           "node-2" "Second title" '("happy") "Second body" '(0 20 0 0))
          (let ((main (supertag-view-stream "diary")))
            (should (buffer-live-p main))
            (should (buffer-live-p (get-buffer "*Supertag Stream Index: diary*")))
            (let ((main-window (get-buffer-window main))
                  (index-window
                   (get-buffer-window "*Supertag Stream Index: diary*")))
              (should (> (window-total-width main-window)
                         (window-total-width index-window)))
              (should (<= (window-total-width index-window)
                          supertag-view-stream-index-width))
              (select-window index-window)
              (with-current-buffer "*Supertag Stream Index: diary*"
                (should (string-match-p "First title" (buffer-string)))
                (should (string-match-p "Second title" (buffer-string)))
                (should-not (string-match-p "/" (buffer-string)))
                (forward-line 1)
                (push-button))
              (let ((position
                     (with-current-buffer main
                       (supertag-view-stream--find-entity "node-2"))))
                (should (eq (selected-window) main-window))
                (should (= (window-point main-window) position))
                (should (= (window-start main-window) position))))
            (with-current-buffer main
              (should (equal (supertag-view-stream--current-node-id)
                             "node-2")))
            (with-current-buffer main
              (supertag-view-stream-toggle-layout)
              (should (eq (plist-get
                           (plist-get supertag-view--instance :input) :layout)
                          'plain)))
            (should-not (get-buffer "*Supertag Stream Index: diary*"))
            (with-current-buffer main
              (supertag-view-stream-toggle-layout)
              (should (eq (plist-get
                           (plist-get supertag-view--instance :input) :layout)
                          'split)))
            (should (get-buffer "*Supertag Stream Index: diary*"))
            (with-current-buffer main
              (supertag-view-stream-quit))
            (should-not (buffer-live-p main))
            (should-not (get-buffer "*Supertag Stream Index: diary*"))))
      (supertag-view-stream-test--kill-buffers))))

(ert-deftest supertag-view-stream-public-command-keeps-one-buffer-per-tag ()
  "Different tags get different buffers; reopening one tag reuses its buffer."
  (supertag-view-stream-test--with-store
    (unwind-protect
        (save-window-excursion
          (supertag-view-stream-test--put-tag "diary")
          (supertag-view-stream-test--put-tag "work")
          (supertag-view-stream-test--put-node
           "diary-node" "Diary title" '("diary") "Diary body" '(0 10 0 0))
          (supertag-view-stream-test--put-node
           "work-node" "Work title" '("work") "Work body" '(0 20 0 0))
          (let* ((diary (supertag-view-stream "diary"))
                 (work (supertag-view-stream "work")))
            (should-not (eq diary work))
            (should (equal (buffer-name diary) "*Supertag Stream: diary*"))
            (should (equal (buffer-name work) "*Supertag Stream: work*"))
            (should-not
             (get-buffer-window "*Supertag Stream Index: diary*"))
            (should (get-buffer-window "*Supertag Stream Index: work*"))
            (let ((diary-again (supertag-view-stream "diary")))
              (should (eq diary diary-again))
              (should (buffer-live-p work))
              (should-not
               (get-buffer-window "*Supertag Stream Index: work*"))
              (should (get-buffer-window "*Supertag Stream Index: diary*"))
              (with-current-buffer diary
                (should (equal (plist-get
                                (plist-get supertag-view--instance :input) :tag)
                               "diary"))
                (should (string-match-p "Diary body" (buffer-string)))
                (should-not (string-match-p "Work body" (buffer-string))))
              (with-current-buffer work
                (should (equal (plist-get
                                (plist-get supertag-view--instance :input) :tag)
                               "work"))
                (should (string-match-p "Work body" (buffer-string)))
                (should-not (string-match-p "Diary body" (buffer-string)))))))
      (supertag-view-stream-test--kill-buffers))))

(ert-deftest supertag-view-stream-navigation-and-node-view-use-stable-id ()
  "Navigation and field dispatch must use the node ID at point."
  (supertag-view-stream-test--with-store
    (unwind-protect
        (progn
          (supertag-view-stream-test--put-node
           "node-1" "First" '("diary") "First body" '(0 10 0 0))
          (supertag-view-stream-test--put-node
           "node-2" "Second" '("diary") "Second body" '(0 20 0 0))
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((buffer
                   (supertag-view-open
                    'stream '(:tag "diary" :layout plain)))
                  opened)
              (with-current-buffer buffer
                (goto-char (point-min))
                (should (equal (supertag-view-stream--current-node-id)
                               "node-1"))
                (supertag-view-stream-next-node)
                (should (equal (supertag-view-stream--current-node-id)
                               "node-2"))
                (should
                 (equal (get-text-property
                         (overlay-start supertag-view-stream--selection-overlay)
                         'supertag-entity-id)
                        "node-2"))
                (cl-letf (((symbol-function 'supertag-view-node-open)
                           (lambda (node-id) (setq opened node-id))))
                  (supertag-view-stream-open-node-view))
                (should (equal opened "node-2"))))))
      (supertag-view-stream-test--kill-buffers))))

(ert-deftest supertag-view-stream-edit-narrows-source-without-autosave ()
  "Stream editing must share source text, exclude children and not auto-save."
  (supertag-view-stream-test--with-store
    (let ((file (make-temp-file
                 "supertag-stream-edit-" nil ".org"
                 "* Parent #diary\n:PROPERTIES:\n:ID: edit-node\n:END:\nOriginal body\n** Child\nChild body\n"))
          base
          edit)
      (unwind-protect
          (progn
            (supertag-view-stream-test--put-node
             "edit-node" "Parent" '("diary") "Original body"
             '(0 10 0 0) file 1)
            (cl-letf (((symbol-function 'display-buffer) #'ignore))
              (let ((main
                     (supertag-view-open
                      'stream '(:tag "diary" :layout plain))))
                (with-current-buffer main
                  (setq edit (supertag-view-stream-edit)))
                (setq base (buffer-base-buffer edit))
                (with-current-buffer edit
                  (should (buffer-narrowed-p))
                  (should (string-match-p "Original body" (buffer-string)))
                  (should-not (string-match-p "Child body" (buffer-string)))
                  (goto-char (point-max))
                  (insert "Changed in Stream\n")
                  (supertag-view-stream-edit-finish))
                (should-not (buffer-live-p edit))
                (with-current-buffer base
                  (should (string-match-p "Changed in Stream"
                                          (buffer-string))))
                (should (equal (plist-get
                                (supertag-store-get-entity :nodes "edit-node")
                                :created-at)
                               '(0 10 0 0)))
                (with-temp-buffer
                  (insert-file-contents file)
                  (should-not (string-match-p "Changed in Stream"
                                              (buffer-string)))))))
        (when (buffer-live-p edit)
          (with-current-buffer edit (set-buffer-modified-p nil))
          (kill-buffer edit))
        (when (buffer-live-p base)
          (with-current-buffer base (set-buffer-modified-p nil))
          (kill-buffer base))
        (supertag-view-stream-test--kill-buffers)
        (delete-file file)))))

(ert-deftest supertag-view-stream-runtime-owns-store-subscription ()
  "Killing the main Stream buffer must remove its only Store subscription."
  (supertag-view-stream-test--with-store
    (unwind-protect
        (progn
          (supertag-view-stream-test--put-node
           "node-1" "First" '("diary") "Body" '(0 10 0 0))
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (let ((buffer
                   (supertag-view-open
                    'stream '(:tag "diary" :layout plain))))
              (should (= 1 (length
                            (gethash :store-changed supertag--subscribers))))
              (kill-buffer buffer)
              (should-not (gethash :store-changed supertag--subscribers)))))
      (supertag-view-stream-test--kill-buffers))))

(provide 'test-view-stream)

;;; test-view-stream.el ends here

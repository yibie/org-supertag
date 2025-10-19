;;; test-capture-improvements.el --- Test capture improvements -*- lexical-binding: t; -*-

;;; Commentary:
;; This file tests the capture system improvements including:
;; 1. Input order prioritization (title before body)
;; 2. Dynamic tag positioning based on template structure

;;; Code:

(require 'org-supertag)

(defun test-capture-improvements ()
  "Test the capture system improvements."
  (interactive)
  (let ((test-templates
         '(
           ;; Test 1: Tags after title (traditional)
           ("t1" "Tags after title"
            :template "* %^{Task Title} #task #urgent
- status: todo
- priority: high

%{Task Details}")

           ;; Test 2: Tags before title (new feature)
           ("t2" "Tags before title"
            :template "* #project #active %^{Project Name}
- status: active
- priority: medium

%{Project Description}")

           ;; Test 3: Mixed with fields
           ("t3" "Meeting with tags before"
            :template "* #meeting %^{Meeting Topic}
- type: %^{Meeting Type}
- status: completed
- location: %^{Location}

Time: %date
Participants:

Agenda:

Discussion Points:

Action Items:

%?")))

    ;; Set test templates
    (setq supertag-capture-templates test-templates)

    ;; Test each template
    (dolist (template test-templates)
      (let ((key (car template))
            (description (cadr template)))
        (message "Testing template: %s (%s)" key description)

        ;; Simulate template parsing
        (let* ((template-string (plist-get (caddr template) :template))
               (parsed-spec (supertag-capture--parse-template-string template-string)))
          (message "Parsed spec: %S" parsed-spec)

          ;; Test processing (will prompt for input)
          ;; (let ((processed (supertag-capture--process-spec parsed-spec)))
          ;;   (message "Processed data: %S" processed))
          )))

    (message "Test completed! Check the output above for parsing results.")))

(defun test-tag-position-detection ()
  "Test tag position detection logic."
  (interactive)
  (let ((test-cases
        '(
          ;; Tags after title
          ("* My Task #task #urgent" :after-title "My Task" ("task" "urgent"))
          ;; Tags before title
          ("* #project #active My Project" :before-title "My Project" ("project" "active"))
          ;; Mixed with spaces
          ("*   #meeting   #weekly   Team Standup" :after-title "Team Standup" ("meeting" "weekly"))
          ;; No tags
          ("* Just a title" nil "Just a title" nil)
          ;; Only tags
          ("* #todo #important" :after-title "" ("todo" "important")))))

    (dolist (test-case test-cases)
      (let* ((input (car test-case))
             (expected-position (cadr test-case))
             (expected-title (caddr test-case))
             (expected-tags (cadddr test-case)))

        ;; Simulate parsing
        (when (string-match "^\\*+ \\(.*\\)" input)
          (let* ((headline-content (string-trim (match-string 1 input)))
                 (tags (supertag-transform-extract-inline-tags headline-content))
                 (detected-position nil))

            (when tags
              (let ((cleaned-headline (replace-regexp-in-string "\\s-+#\\w[-_[:alnum:]]*" "" headline-content)))
                (setq detected-position (if (string-match-p "^\\s-+#[^[:space:]]+" cleaned-headline)
                                          :after-title
                                        :before-title))))

            (let ((actual-title (string-trim (replace-regexp-in-string "\\s-+#\\w[-_[:alnum:]]*" "" headline-content))))

              (message "Input: %S" input)
              (message "  Expected: pos=%S, title=%S, tags=%S" expected-position expected-title expected-tags)
              (message "  Actual:   pos=%S, title=%S, tags=%S" detected-position actual-title tags)
              (message "  Result: %s"
                      (if (and (eq detected-position expected-position)
                               (equal actual-title expected-title)
                               (equal tags expected-tags))
                          "✓ PASS" "✗ FAIL"))
              (message "")))))))

(provide 'test-capture-improvements)
;;; test-capture-improvements.el ends here
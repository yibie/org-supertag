;;; test-inline-tag-filter.el --- self-check for inline tag filtering -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q --eval '(package-initialize)' -L . -l test/test-inline-tag-filter.el

(require 'cl-lib)
(require 'org)
(require 'supertag-view-helper)

(defun test-inline-tag-filter--matches (text)
  "Return renderable inline tag matches in TEXT."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (goto-char (point-min))
    (let ((regex (concat "#[" supertag-view-helper--valid-tag-chars "]+"))
          matches)
      (while (re-search-forward regex nil t)
        (when (supertag-view-helper--valid-inline-tag-match-p)
          (push (match-string 0) matches)))
      (nreverse matches))))

(cl-assert (equal (test-inline-tag-filter--matches "plain #sss tag")
                  '("#sss")))
(cl-assert (equal (test-inline-tag-filter--matches "[#A] priority #real")
                  '("#real")))
(cl-assert (equal (test-inline-tag-filter--matches "https://x.com/sss#xxxxx #ok")
                  '("#ok")))

(message "OK inline tag filter skips Org priorities and URL fragments.")
(kill-emacs 0)

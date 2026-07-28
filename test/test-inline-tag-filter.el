;;; test-inline-tag-filter.el --- self-check for inline tag filtering -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q --eval '(package-initialize)' -L . -l test/test-inline-tag-filter.el

(require 'cl-lib)
(require 'org)
(require 'supertag-core-transform)
(require 'supertag-ui-completion)
(require 'supertag-view-helper)
(require 'supertag-view-svg-tag)

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

(dolist
    (case
     '(("plain #paper #coding/语言 #🧠 #C++ #task.　#全角空格"
        ("#paper" "#coding/语言" "#🧠" "#C++" "#task." "#全角空格"))
       ("#root\n* Heading #heading\n- item #list\n[fn:1] note #footnote"
        ("#root" "#heading" "#list" "#footnote"))
       ("#+begin_quote\nquoted #quote\n#+end_quote" ("#quote"))
       ("[#A] priority #real" ("#real"))
       ("https://x.com/sss#fragment #ok" ("#ok"))
       ("[[file:Copyright.xhtml#Copyright.xhtml][→ Copyright.xhtml]] #ok"
        ("#ok"))
       ("[[id:node][#linked]] #outside" ("#outside"))
       ("word#tag 中文#标签 copyright &#169; &#xA9; \\#escaped (#wrapped)" nil)
       ("~#code~ =#verbatim= {{{hash(#macro)}}} <<#target>> <<<#radio>>>" nil)
       ("#+TITLE: #title\n# comment #comment\n:PROPERTIES:\n:URL: file#property\n:END:\n:DRAWER:\nvalue #drawer\n:END:\n| #table |\n: fixed #fixed"
        nil)
       ("* COMMENT Hidden #heading\nbody #body" nil)
       ("#+begin_src text\n#source\n#+end_src\n#+begin_example\n#example\n#+end_example\n#+begin_verse\n#verse\n#+end_verse"
        nil)
       ("Use #'zettel-follow, #'zettel-export and #'zettel-preview." nil)))
  (pcase-let ((`(,text ,expected) case))
    (let ((actual (test-inline-tag-filter--matches text)))
      (unless (equal actual expected)
        (error "Inline tag mismatch for %S: expected %S, got %S"
               text expected actual)))))

(cl-assert
 (null
  (supertag-transform-extract-inline-tags
   "Use #'zettel-follow, #'zettel-export and #'zettel-preview."))
 nil "Function quotes leaked into extracted tags")

(let ((supertag--store (ht-create))
      (tags (ht-create)))
  (puthash "tools" '(:id "tools" :name "tools" :type :tag) tags)
  (puthash "'zettel-export"
           '(:id "'zettel-export" :name "'zettel-export" :type :tag)
           tags)
  (puthash :tags tags supertag--store)
  (cl-assert (equal (supertag-completion--get-all-tags) '("tools"))
             nil "Completion exposed a function-quote artifact"))

(cl-letf (((symbol-function 'supertag-svg-tag--char-height) (lambda () 20))
          ((symbol-function 'supertag-svg-tag--char-width) (lambda () 10)))
  (let* ((image (supertag-svg-tag--make-svg "#tools" "tools"))
         (xml (plist-get (cdr image) :data)))
    (cl-assert (string-match-p "font-size=\"14\"" xml)
               nil "SVG tag font is not the expected 14px: %s" xml)))

(message "OK inline tag filter accepts prose tokens and rejects Org objects.")
(kill-emacs 0)

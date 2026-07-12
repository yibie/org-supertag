;;; demo-formula.el --- Formula parser demo -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive demo of the formula parser
;; Usage: M-x supertag-demo-formula

;;; Code:

(require 'supertag-virtual-column)

(defun supertag-demo-formula ()
  "Interactive demo of formula parser."
  (interactive)
  (switch-to-buffer "*Formula Demo*")
  (erase-buffer)
  (org-mode)
  
  (insert "# Formula Parser Demo\n\n")
  
  ;; Section 1: Tokenizer
  (insert "## 1. Tokenizer Examples\n\n")
  (insert "Tokenizer converts input string to tokens.\n\n")
  
  (let ((examples '("42" 
                    "effort"
                    "a + b"
                    "(done / total) * 100"
                    "x * 2 + y / 3")))
    (dolist (ex examples)
      (insert (format "- Input: =%s=\n" ex))
      (insert (format "  Tokens: ~%s~\n\n" 
                      (supertag-formula-tokenize ex)))))
  
  ;; Section 2: Parser
  (insert "## 2. Parser Examples\n\n")
  (insert "Parser converts tokens to Abstract Syntax Tree (AST).\n\n")
  
  (let ((examples '("2 + 3 * 4"
                    "(2 + 3) * 4"
                    "(done / total) * 100"
                    "a * 2 - b / 3")))
    (dolist (ex examples)
      (insert (format "- Formula: =%s=\n" ex))
      (insert (format "  AST: ~%s~\n\n"
                      (supertag-formula-parse-string ex)))))
  
  ;; Section 3: Evaluator
  (insert "## 3. Evaluator Examples\n\n")
  (insert "Evaluator computes AST result.\n\n")
  (insert "*Note:* Variables default to 0 in this demo.\n\n")
  
  (let ((examples '("2 + 3"
                    "10 / 2"
                    "2 + 3 * 4"
                    "(2 + 3) * 4"
                    "100 / (5 * 2)"
                    "-5 + 10")))
    (dolist (ex examples)
      (let ((result (supertag-formula-eval-string ex "demo-node")))
        (insert (format "- =%s= => *%s*\n" ex result)))))
  
  (insert "\n## 4. Virtual Column Integration\n\n")
  
  ;; Create a formula virtual column
  (supertag-virtual-column-init)
  
  (let ((col (supertag-virtual-column-create
              (list :id "progress-percent"
                    :name "Progress %"
                    :type :formula
                    :params (list :formula "(done / total) * 100")))))
    (insert (format "Created formula column: ~%s~\n\n" col)))
  
  ;; Show all columns
  (insert "All virtual columns:\n")
  (dolist (col (supertag-virtual-column-list))
    (insert (format "- ~%s~: ~%s~ (~%s~)\n"
                    (plist-get col :id)
                    (plist-get col :name)
                    (plist-get col :type))))
  
  (insert "\n## 5. Try It Yourself\n\n")
  (insert "Evaluate the following:\n\n")
  (insert "#+begin_src elisp\n")
  (insert ";; Parse a formula\n")
  (insert "(supertag-formula-parse-string \"(a + b) * 2\")\n\n")
  (insert ";; Evaluate a formula\n")
  (insert "(supertag-formula-eval-string \"10 * 5 + 3\" \"node-id\")\n\n")
  (insert ";; Create a formula virtual column\n")
  (insert "(supertag-virtual-column-create\n")
  (insert " (list :id \"my-formula\"\n")
  (insert "       :name \"My Formula\"\n")
  (insert "       :type :formula\n")
  (insert "       :params (list :formula \"x * 2 + y\")))\n")
  (insert "#+end_src\n")
  
  (goto-char (point-min)))

(provide 'demo-formula)

;;; demo-formula.el ends here

;;; formula-test.el --- Formula parser tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the formula parser in supertag-virtual-column.el

;;; Code:

(require 'ert)

;; Load the module under test
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(require 'supertag-virtual-column)

;; Test tokenizer
(ert-deftest test-formula-tokenize-number ()
  "Test tokenizing simple numbers."
  (should (equal (supertag-formula-tokenize "42") '(42)))
  (should (equal (supertag-formula-tokenize "3.14") '(3.14)))
  (should (equal (supertag-formula-tokenize "0") '(0))))

(ert-deftest test-formula-tokenize-operators ()
  "Test tokenizing operators."
  (should (equal (supertag-formula-tokenize "+") '(+)))
  (should (equal (supertag-formula-tokenize "-") '(-)))
  (should (equal (supertag-formula-tokenize "*") '(*)))
  (should (equal (supertag-formula-tokenize "/") '(/))))

(ert-deftest test-formula-tokenize-parentheses ()
  "Test tokenizing parentheses."
  (should (equal (supertag-formula-tokenize "()") '(*lparen* *rparen*)))
  (should (equal (supertag-formula-tokenize "(a)") '(*lparen* "a" *rparen*))))

(ert-deftest test-formula-tokenize-variables ()
  "Test tokenizing variable names."
  (should (equal (supertag-formula-tokenize "effort") '("effort")))
  (should (equal (supertag-formula-tokenize "total_count") '("total_count")))
  (should (equal (supertag-formula-tokenize "a1b2") '("a1b2"))))

(ert-deftest test-formula-tokenize-complex ()
  "Test tokenizing complex expressions."
  (should (equal (supertag-formula-tokenize "a + b")
                 '("a" + "b")))
  (should (equal (supertag-formula-tokenize "(done / total) * 100")
                 '(*lparen* "done" / "total" *rparen* * 100))))

(ert-deftest test-formula-tokenize-whitespace ()
  "Test tokenizing with whitespace."
  (should (equal (supertag-formula-tokenize "  a  +  b  ")
                 '("a" + "b")))
  (should (equal (supertag-formula-tokenize "a\n+\tb")
                 '("a" + "b"))))

;; Test parser
(ert-deftest test-formula-parse-number ()
  "Test parsing numbers."
  (let ((tokens '(42)))
    (should (equal (supertag-formula-parse tokens)
                   '(*number* 42)))))

(ert-deftest test-formula-parse-variable ()
  "Test parsing variables."
  (let ((tokens '("effort")))
    (should (equal (supertag-formula-parse tokens)
                   '(*var* "effort")))))

(ert-deftest test-formula-parse-simple-addition ()
  "Test parsing simple addition."
  (let ((tokens '("a" + "b")))
    (should (equal (supertag-formula-parse tokens)
                   '(+ (*var* "a") (*var* "b"))))))

(ert-deftest test-formula-parse-precedence ()
  "Test operator precedence."
  ;; Multiplication has higher precedence than addition
  (let ((tokens '("a" + "b" * "c")))
    (should (equal (supertag-formula-parse tokens)
                   '(+ (*var* "a")
                       (* (*var* "b") (*var* "c")))))))

(ert-deftest test-formula-parse-parentheses ()
  "Test parentheses grouping."
  (let ((tokens '(*lparen* "a" + "b" *rparen* * "c")))
    (should (equal (supertag-formula-parse tokens)
                   '(* (+ (*var* "a") (*var* "b"))
                       (*var* "c"))))))

(ert-deftest test-formula-parse-complex ()
  "Test parsing complex expression."
  (let ((tokens '(*lparen* "done" / "total" *rparen* * 100)))
    (should (equal (supertag-formula-parse tokens)
                   '(* (/ (*var* "done") (*var* "total"))
                       (*number* 100))))))

(ert-deftest test-formula-parse-negation ()
  "Test unary minus."
  (let ((tokens '(- 42)))
    (should (equal (supertag-formula-parse tokens)
                   '(*neg* (*number* 42))))))

;; Test evaluator
(ert-deftest test-formula-eval-number ()
  "Test evaluating numbers."
  (should (= (supertag-formula-eval '(*number* 42) "node1") 42)))

(ert-deftest test-formula-eval-arithmetic ()
  "Test evaluating arithmetic."
  (should (= (supertag-formula-eval '(+ (*number* 2) (*number* 3)) "node1") 5))
  (should (= (supertag-formula-eval '(- (*number* 5) (*number* 3)) "node1") 2))
  (should (= (supertag-formula-eval '(* (*number* 3) (*number* 4)) "node1") 12))
  (should (= (supertag-formula-eval '(/ (*number* 10) (*number* 2)) "node1") 5)))

(ert-deftest test-formula-eval-precedence ()
  "Test evaluating with precedence."
  ;; 2 + 3 * 4 = 14 (not 20)
  (let ((ast '(+ (*number* 2)
                 (* (*number* 3) (*number* 4)))))
    (should (= (supertag-formula-eval ast "node1") 14))))

(ert-deftest test-formula-eval-division-by-zero ()
  "Test division by zero handling."
  (let ((ast '(/ (*number* 10) (*number* 0))))
    (should (= (supertag-formula-eval ast "node1") 0))))

(ert-deftest test-formula-eval-negation ()
  "Test negation."
  (should (= (supertag-formula-eval '(*neg* (*number* 5)) "node1") -5)))

;; Test end-to-end
(ert-deftest test-formula-end-to-end-simple ()
  "Test complete parse and eval."
  (let ((ast (supertag-formula-parse-string "2 + 3 * 4")))
    (should (= (supertag-formula-eval ast "node1") 14))))

(ert-deftest test-formula-end-to-end-percentage ()
  "Test percentage formula."
  (let ((ast (supertag-formula-parse-string "(50 / 100) * 100")))
    (should (= (supertag-formula-eval ast "node1") 50))))

(ert-deftest test-formula-end-to-end-complex ()
  "Test complex formula."
  (let ((ast (supertag-formula-parse-string "(a + b) * 2 - c / 2")))
    ;; Variables will be 0 (default), so: (0 + 0) * 2 - 0 / 2 = 0
    (should (= (supertag-formula-eval ast "node1") 0))))

;; Test integration with virtual column system
(ert-deftest test-virtual-column-formula-create ()
  "Test creating formula virtual column."
  (supertag-virtual-column-init)
  (let ((col (supertag-virtual-column-create
              (list :id "double-effort"
                    :name "Double Effort"
                    :type :formula
                    :params (list :formula "effort * 2")))))
    (should col)
    (should (equal (plist-get col :id) "double-effort"))
    (should (eq (plist-get col :type) :formula))))

;; Manual verification helper
(defun test-formula-manual ()
  "Run manual formula tests."
  (interactive)
  (message "=== Formula Parser Tests ===")
  
  ;; Tokenizer tests
  (message "\nTokenizer:")
  (message "  '42' => %s" (supertag-formula-tokenize "42"))
  (message "  'a + b' => %s" (supertag-formula-tokenize "a + b"))
  (message "  '(done / total) * 100' => %s"
           (supertag-formula-tokenize "(done / total) * 100"))
  
  ;; Parser tests
  (message "\nParser:")
  (message "  '2 + 3 * 4' => %s"
           (supertag-formula-parse-string "2 + 3 * 4"))
  (message "  '(done / total) * 100' => %s"
           (supertag-formula-parse-string "(done / total) * 100"))
  
  ;; Evaluator tests (variables default to 0)
  (message "\nEvaluator (variables = 0):")
  (message "  '2 + 3' => %s" (supertag-formula-eval-string "2 + 3" "test"))
  (message "  '10 / 2' => %s" (supertag-formula-eval-string "10 / 2" "test"))
  (message "  '(10 + 5) * 2' => %s" (supertag-formula-eval-string "(10 + 5) * 2" "test"))
  
  (message "\n=== Tests Complete ==="))

(provide 'formula-test)

;;; formula-test.el ends here

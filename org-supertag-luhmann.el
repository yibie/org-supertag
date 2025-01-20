;;; org-supertag-luhmann.el --- Luhmann numbering system for org-supertag -*- lexical-binding: t -*-

;; Copyright (C) 2024 Qiantan Hong

;; Author: Qiantan Hong <qhong@mit.edu>
;; Maintainer: Qiantan Hong <qhong@mit.edu>
;; URL: https://github.com/QianTan/org-supertag
;; Version: 2.1.0
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;; Keywords: org-mode, outlines, note-taking, zettelkasten

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module implements Luhmann's numbering system for org-supertag.
;; It provides functions to:
;; - Parse Luhmann numbers from node titles
;; - Generate next available numbers in main sequence
;; - Generate branch numbers from existing nodes
;; - Generate insertion letters between nodes
;; - Validate number format and relationships

;;; Code:

(require 'org-supertag-node)


;;------------------------------------------------------------------------------
;; Luhmann Numbering System
;;------------------------------------------------------------------------------

(defun org-supertag-node--parse-number-part (part)
  "Parse a single part of a Luhmann number.
PART is a string like \"1\", \"1a\", etc.
Returns plist with :number and optional :letter properties."
  (if (string-match "^\\([0-9]+\\)\\([a-z]\\)?$" part)
      (let ((num (string-to-number (match-string 1 part)))
            (letter (match-string 2 part)))
        (if letter
            (list :number num :letter letter)
          (list :number num)))
    (list :number (string-to-number part))))

(defun org-supertag-node--parse-luhmann-number (title)
  "Parse Luhmann number from node title.
TITLE is the node title string.
Returns a list of plists, each representing one level.

Examples:
'1'       -> ((:number 1))
'1.2'     -> ((:number 1) (:number 2))
'1a'      -> ((:number 1 :letter \"a\"))
'1.2a'    -> ((:number 1) (:number 2 :letter \"a\"))
'1a.2b'   -> ((:number 1 :letter \"a\") (:number 2 :letter \"b\"))
'1.1f.1'  -> ((:number 1) (:number 1 :letter \"f\") (:number 1))"
  (when (string-match "^\\([0-9]+\\(?:[a-z]+\\)?\\(?:\\.[0-9]+\\(?:[a-z]+\\)?\\)*\\)[[:space:]]" title)
    (let* ((number-str (match-string 1 title))
           (parts (split-string number-str "\\.")))
      (mapcar #'org-supertag-node--parse-number-part parts))))

(defun org-supertag-node--number-to-string (number-parts)
  "Convert parsed number parts back to string.
NUMBER-PARTS is a list of plists as returned by `org-supertag-node--parse-luhmann-number'.

Examples:
((:number 1))                          -> \"1\"
((:number 1) (:number 2))              -> \"1.2\"
((:number 1 :letter \"a\"))            -> \"1a\"
((:number 1) (:number 2 :letter \"a\")) -> \"1.2a\"
((:number 1) (:number 1 :letter \"f\") (:number 1)) -> \"1.1f.1\""
  (string-join
   (mapcar
    (lambda (part)
      (concat
       (number-to-string (plist-get part :number))
       (or (plist-get part :letter) "")))
    number-parts)
   "."))

(defun org-supertag-node--increment-letter (letter)
  "Increment letter sequence.
LETTER is the current letter sequence (or nil).
Returns next letter in sequence.

Examples:
nil -> \"a\"
\"a\" -> \"b\"
\"z\" -> \"aa\"
\"aa\" -> \"ab\"
\"az\" -> \"ba\"
\"zz\" -> \"aaa\""
  (cond
   ((null letter) "a")
   ((string= letter "z") "aa")
   ((string-match "z+$" letter)
    (let* ((len (length letter))
           (non-z (substring letter 0 (- len (match-length 0)))))
      (if (string= non-z "")
          (make-string (1+ len) ?a)
        (concat (substring non-z 0 -1)
                (char-to-string (1+ (aref non-z (1- (length non-z)))))
                (make-string (match-length 0) ?a)))))
   (t (let ((last-char (aref letter (1- (length letter)))))
        (concat (substring letter 0 -1)
                (char-to-string (1+ last-char)))))))

(defun org-supertag-node--validate-number-level (number level)
  "Validate if Luhmann NUMBER is appropriate for heading LEVEL.
Returns t if valid, nil otherwise.

A number's level is determined by dots only:
- 1     -> level 1
- 1a    -> level 2 (same as 1.1)
- 1.1   -> level 2
- 1.1a  -> level 2
- 1.1.1 -> level 3

Examples for each level:
Level 1: 1, 2, 3
Level 2: 1.1, 1a, 1b, 1.2
Level 3: 1.1.1, 1.1a.1
Level 4: 1.1.1.1, 1.1a.1a"
  (let* ((parts (org-supertag-node--parse-luhmann-number number))
         (total-depth (length parts)))
    (<= total-depth level)))

(defun org-supertag-node--get-next-main-number ()
  "Get next available main number in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((max-num 0))
      (while (re-search-forward "^\\([0-9]+\\)\\(?:[a-z]\\|[.]\\|[[:space:]]\\)" nil t)
        (let ((num (string-to-number (match-string 1))))
          (setq max-num (max max-num num))))
      (number-to-string (1+ max-num)))))

(defun org-supertag-node--get-siblings (base-num)
  "Get all sibling numbers for BASE-NUM.
Returns a list of parsed number parts at the same level as BASE-NUM.
Example: For base number \"1.1\", returns all \"1.1\", \"1.1a\", \"1.1b\" etc."
  (let ((siblings nil)
        (base-pattern (concat "^"
                             (regexp-quote base-num)
                             "\\([a-z]+\\)?[[:space:]]")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward base-pattern nil t)
        (let ((letter (match-string 1)))
          (when letter
            (push (list :number base-num :letter letter) siblings)))))
    siblings))

(defun org-supertag-node--get-next-branch-number (base-num)
  "Get next available branch number for BASE-NUM.
BASE-NUM is the parent number to branch from.

Examples:
- If BASE-NUM is 1, looks for 1.1, 1.2, etc.
- If BASE-NUM is 1a, looks for 1a.1, 1a.2, etc."
  (let ((max-branch 0)
        (branch-pattern (format "^%s\\.\\([0-9]+\\)\\(?:[a-z]\\)?[[:space:]]+" 
                              (regexp-quote base-num))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward branch-pattern nil t)
        (let ((num (string-to-number (match-string 1))))
          (when (> num max-branch)
            (setq max-branch num)))))
    (1+ max-branch)))

(defun org-supertag-node--get-next-sibling-letter (number-str)
  "Get next letter in sequence for NUMBER-STR.
For example:
- \"1\" -> \"a\"
- \"1a\" -> \"b\"
- \"1z\" -> \"aa\"
- \"1aa\" -> \"ab\"
- \"1az\" -> \"ba\"
- \"1zz\" -> \"aaa\""
  (if (string-match "\\([0-9.]+\\)\\([a-z]+\\)?$" number-str)
      (let ((current-letters (match-string 2 number-str)))
        (if current-letters
            ;; 如果已有字母后缀，计算下一个字母序列
            (let ((len (length current-letters))
                  (last-char (aref current-letters (1- (length current-letters)))))
              (if (char-equal last-char ?z)
                  ;; 如果最后一个字母是 'z'，需要进位
                  (if (= len 1)
                      "aa"  ; z -> aa
                    (let* ((prefix (substring current-letters 0 (1- len)))
                          (last-prefix-char (aref prefix (1- (length prefix)))))
                      (if (char-equal last-prefix-char ?z)
                          (make-string (1+ len) ?a)  ; zz -> aaa
                        (concat (org-supertag-node--increment-letter-sequence prefix) "a"))))  ; az -> ba
                ;; 如果最后一个字母不是 'z'，直接递增最后一个字母
                (concat (substring current-letters 0 (1- len))
                       (char-to-string (1+ last-char)))))
          ;; 如果没有字母后缀，返回 'a'
          "a"))
    "a"))

(defun org-supertag-node--increment-letter-sequence (letters)
  "Increment a sequence of letters.
For example: \"a\" -> \"b\", \"z\" -> \"aa\", \"az\" -> \"ba\"."
  (let* ((len (length letters))
         (last-char (aref letters (1- len))))
    (if (char-equal last-char ?z)
        (if (= len 1)
            "aa"
          (concat (org-supertag-node--increment-letter-sequence 
                  (substring letters 0 (1- len))) "a"))
      (concat (substring letters 0 (1- len))
              (char-to-string (1+ last-char))))))

(defun org-supertag-node--get-current-context ()
  "Analyze current context for Luhmann numbering."
  (save-excursion
    (let* ((at-heading (org-at-heading-p))
           ;; If not at heading, go back to parent heading
           (_ (unless at-heading
                (org-back-to-heading t)))
           (current-level (org-outline-level))
           (current-title (org-get-heading t t t t))
           (current-num (when current-title
                         (when (string-match "^\\([0-9]+\\(?:[a-z]*\\)?\\(?:\\.[0-9]+\\(?:[a-z]*\\)?\\)*\\)" current-title)
                           (match-string 1 current-title))))
           prev-sibling-str
           parent-str
           base-level)
      
      ;; Get previous sibling info - 改进这部分
      (save-excursion
        (org-back-to-heading t)
        ;; 尝试多次获取前一个兄弟节点，直到找到有编号的
        (let ((found nil))
          (while (and (not found)
                     (org-get-previous-sibling))
            (let* ((prev-title (org-get-heading t t t t)))
              (when (string-match "^\\([0-9]+\\(?:[a-z]*\\)?\\(?:\\.[0-9]+\\(?:[a-z]*\\)?\\)*\\)" prev-title)
                (setq prev-sibling-str (match-string 1 prev-title)
                      found t))))))
      
      ;; Get parent info
      (save-excursion
        (when (> current-level 1)
          (org-up-heading-safe)
          (let ((parent-title (org-get-heading t t t t)))
            (when (string-match "^\\([0-9]+\\(?:[a-z]*\\)?\\(?:\\.[0-9]+\\(?:[a-z]*\\)?\\)*\\)" parent-title)
              (setq parent-str (match-string 1 parent-title))))))
      
      ;; Find base level for the number sequence
      (save-excursion
        (goto-char (point-min))
        (when (and current-num
                  (string-match "^\\([0-9.]+\\(?:[a-z]+\\)?\\)" current-num))
          (let ((base-pattern (concat "^"
                                    (regexp-quote (match-string 1 current-num))
                                    "[[:space:]]")))
            (while (re-search-forward base-pattern nil t)
              (setq base-level (org-outline-level))))))
      
      ;; If no base level found, use current level
      (unless base-level
        (setq base-level current-level))
      
      ;; Debug output
      (message "Context debug: current='%s' prev-sibling='%s' parent='%s' base-level=%s"
               current-num prev-sibling-str parent-str base-level)
      
      (list :level base-level
            :current-level current-level
            :current current-num
            :prev-sibling prev-sibling-str
            :parent-str parent-str))))

(defun org-supertag-node--generate-number (option context)
  "Generate new number based on OPTION and CONTEXT.
OPTION is a cons cell (description . (type base-number))
CONTEXT is the current node context plist.

Types can be:
- main: Generate new main number
- branch-current: Branch from current number
- branch-parent: Branch from parent number
- letter-sequence: Continue letter sequence"
  (let* ((type (car (cdr option)))
         (base (cadr (cdr option))))
    (pcase type
      ('main
       (org-supertag-node--get-next-main-number))
      
      ('branch-current
       (when base
         (concat base ".1")))
      
      ('branch-parent
       (when base
         (let* ((parent-base (if (string-match "\\(.*\\)[.][0-9]+[a-z]*$" base)
                                (match-string 1 base)
                              base))
                (next-num (if (string-match ".*[.]\\([0-9]+\\)[a-z]*$" base)
                             (1+ (string-to-number (match-string 1 base)))
                           1)))
           (format "%s.%d" parent-base next-num))))
      
      ('letter-sequence
       (when base
         (let ((current (plist-get context :current)))
           (if (string-match "\\(.*?\\)\\([a-z]+\\)?$" current)
               ;; 如果当前编号已经有字母后缀，生成下一个字母
               (concat (match-string 1 current)
                      (org-supertag-node--get-next-sibling-letter current))
             ;; 如果当前编号没有字母后缀，添加 'a'
             (concat base "a"))))))))

(defun org-supertag-node--get-number-options (context &optional command)
  "Get available numbering options based on CONTEXT.
COMMAND can be 'add-node or nil (for update-current)."
  (let* ((level (plist-get context :level))
         (prev-sibling (plist-get context :prev-sibling))
         (current (plist-get context :current))
         (parent-str (plist-get context :parent-str))
         options)
    
    (message "Options debug - level=%s prev='%s' current='%s' parent='%s' command='%s'"
             level prev-sibling current parent-str command)
    
    ;; 总是添加新主编号选项
    (push '("New main number" . (main nil)) options)
    
    ;; 如果当前节点有编号，添加分支选项
    (when current
      (push `(,(format "Branch from current (%s.1)" current)
              . (branch-current ,current))
              options)
      
      ;; 处理字母序列选项
      (when (string-match "\\(.*?\\)\\([a-z]+\\)?$" current)
        (let* ((base-num (match-string 1 current))
               (current-letter (match-string 2 current))
               (next-letter (org-supertag-node--get-next-sibling-letter current)))
          (push `(,(format "Continue sequence (%s%s)" 
                          base-num
                          next-letter)
                  . (letter-sequence ,current))
                options))))
    
    ;; 如果在有编号的父节点下，添加从父节点分支的选项
    (when (and (> level 1) parent-str)
      (push `(,(format "Branch from parent (%s.1)" parent-str)
              . (branch-parent ,parent-str))
            options))
    
    (nreverse options)))

(defun org-supertag-node-add-number-node ()
  "Add or update Luhmann number for the current node."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Must be on a heading"))
  
  (let* ((context (org-supertag-node--get-current-context))
         (title (org-get-heading t t t t))
         (has-number (org-supertag-node--parse-luhmann-number title))
         (current (plist-get context :current))
         (options (org-supertag-node--get-number-options context 'add-node))
         (choice (completing-read "Select numbering option: "
                                (mapcar #'car options) nil t))
         (option (assoc choice options))
         (number (org-supertag-node--generate-number option context))
         ;; 添加新标题输入
         (new-heading-text (read-string "Enter heading text: " "New heading")))
    
    (message "Debug: Starting with title='%s', number='%s', choice='%s'" 
             (substring-no-properties title)
             (substring-no-properties number)
             (substring-no-properties choice))
    
    ;; Apply the new number
    (when number
      (let* ((option-type (if (stringp (nth 1 (cdr option)))
                             'letter-sequence  ; 如果是字符串，默认为字母序列
                           (nth 1 (cdr option))))
             (current-level (org-outline-level))
             ;; 使用用户输入的标题文本
             (new-title (concat number " " new-heading-text))
             ;; 计算新编号的层级深度
             (number-depth (length (split-string number "\\.")))
             ;; 根据编号深度决定新标题的级别
             (new-level (pcase option-type
                         ('letter-sequence current-level)      ; 同级
                         ('branch-current number-depth)        ; 使用编号深度
                         ('branch-parent number-depth)         ; 使用编号深度
                         ('main number-depth)                  ; 使用编号深度
                         (_ number-depth))))                   ; 默认使用编号深度
        
        (message "Debug: Preparing new title='%s', current-level=%s, new-level=%s, number-depth=%s, option-type=%S"
                (substring-no-properties new-title)
                current-level
                new-level
                number-depth
                option-type)
        
        ;; 先确保缓冲区可写
        (let ((inhibit-read-only t))
          ;; 根据选项类型决定插入位置和级别
          (save-excursion
            ;; 保存当前标题的位置
            (org-back-to-heading t)
            
            ;; 根据选项类型确定插入位置
            (pcase option-type
              ('letter-sequence
               (message "Debug: Inserting as letter sequence")
               (org-end-of-subtree t)  ; t 表示不包含子树
               (insert "\n" (make-string new-level ?*) " " new-title "\n"))
              
              ('branch-current
               (message "Debug: Inserting as branch current")
               (org-end-of-subtree)
               (insert "\n" (make-string new-level ?*) " " new-title "\n"))
              
              ('branch-parent
               (message "Debug: Inserting as branch parent")
               (when (org-up-heading-safe)
                 (org-end-of-subtree)
                 (insert "\n" (make-string new-level ?*) " " new-title "\n")))
              
              ('main
               (message "Debug: Inserting as main")
               (goto-char (point-max))
               (unless (bolp) (insert "\n"))
               (insert "\n" (make-string new-level ?*) " " new-title "\n"))
              
              (_
               (message "Debug: Fallback insertion for type: %S" option-type)
               (org-end-of-subtree t)
               (insert "\n" (make-string new-level ?*) " " new-title "\n")))
            
            ;; 确保缓冲区被标记为已修改
            (set-buffer-modified-p t)
            
            ;; 立即更新数据库
            (save-excursion
              (forward-line -1)
              (org-back-to-heading t)
              (org-supertag-node-sync-at-point))))
        
        ;; 返回新编号
        number))))

(defun org-supertag-node-number-add ()
  "Add Luhmann number to current heading if it doesn't have one."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Must be on a heading"))
  
  (let* ((context (org-supertag-node--get-current-context))
         (title (org-get-heading t t t t))
         (has-number (org-supertag-node--parse-luhmann-number title))
         (current-level (org-outline-level)))
    
    (if has-number
        (message "Current heading already has a number: %s" has-number)
      
      ;; 只检查前一个同级标题
      (save-excursion
        (let ((prev-number nil))
          ;; 尝试移动到前一个同级标题
          (when (org-backward-heading-same-level 1)
            (let* ((prev-title (org-get-heading t t t t)))
              ;; 从解析结果中提取实际的数字并转换为字符串
              (let ((parsed (org-supertag-node--parse-luhmann-number prev-title)))
                (when parsed
                  (setq prev-number (format "%s"
                                          (if (listp parsed)
                                              (plist-get (car parsed) :number)
                                            parsed)))))))
          
          (message "Debug: Found previous number: %s" prev-number)
          
          ;; 构建选项
          (let* ((options (org-supertag-node--get-number-options 
                          (if prev-number
                              (list :current prev-number
                                    :level current-level)
                            context)
                          'add-node))
                 (choice (completing-read "Select numbering option: "
                                        (mapcar #'car options) nil t))
                 (option (assoc choice options))
                 (number (org-supertag-node--generate-number option context)))
            
            (when number
              ;; 获取纯标题文本
              (let* ((title-text (if (string-match "^\\(?:[0-9]+\\(?:[a-z]+\\)?\\(?:\\.[0-9]+\\(?:[a-z]+\\)?\\)*\\)?[[:space:]]*\\(.*\\)" title)
                                   (match-string 1 title)
                                 title))
                     (new-title (concat number " " title-text)))
                
                ;; 更新标题
                (org-back-to-heading t)
                (let ((inhibit-read-only t))
                  (delete-region (line-beginning-position)
                               (line-end-position))
                  (insert (make-string current-level ?*) " " new-title)
                  
                  ;; 确保缓冲区被标记为已修改
                  (set-buffer-modified-p t)
                  
                  ;; 立即更新数据库
                  (org-back-to-heading t)
                  (org-supertag-node-sync-at-point))))))))))

(defun org-supertag-node--apply-number (number)
  "Apply NUMBER to current node.
Preserves any existing title after the number."
  (when (and number (org-at-heading-p))
    (let* ((title (org-get-heading t t t t))
           (new-title (if (string-match "^\\(?:[0-9]+\\(?:[a-z]+\\)?\\(?:\\.[0-9]+\\(?:[a-z]+\\)?\\)*\\)?[[:space:]]*\\(.*\\)" title)
                         (concat number " " (match-string 1 title))
                       (concat number " " title))))
      (org-edit-headline new-title))))

(provide 'org-supertag-luhmann)


(ert-deftest org-supertag-test-parse-luhmann-number ()
  "Test Luhmann number parsing."
  (should (equal (org-supertag-node--parse-luhmann-number "1 test")
                 '((:number 1))))
  (should (equal (org-supertag-node--parse-luhmann-number "1.2 test")
                 '((:number 1) (:number 2))))
  (should (equal (org-supertag-node--parse-luhmann-number "1a test")
                 '((:number 1 :letter "a"))))
  (should (equal (org-supertag-node--parse-luhmann-number "1.2a test")
                 '((:number 1) (:number 2 :letter "a"))))
  (should (equal (org-supertag-node--parse-luhmann-number "1.1f.1 test")
                 '((:number 1) (:number 1 :letter "f") (:number 1)))))

(ert-deftest org-supertag-test-number-to-string ()
  "Test converting parsed number back to string."
  (should (equal (org-supertag-node--number-to-string 
                 '((:number 1)))
                "1"))
  (should (equal (org-supertag-node--number-to-string 
                 '((:number 1) (:number 2)))
                "1.2"))
  (should (equal (org-supertag-node--number-to-string 
                 '((:number 1 :letter "a")))
                "1a"))
  (should (equal (org-supertag-node--number-to-string 
                 '((:number 1) (:number 1 :letter "f") (:number 1)))
                "1.1f.1")))
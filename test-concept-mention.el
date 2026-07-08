;;; test-concept-mention.el --- self-checks for concept mentions -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q --eval '(package-initialize)' -L . -l test-concept-mention.el

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-id)

(when load-file-name
  (add-to-list 'load-path (file-name-directory load-file-name)))

(require 'supertag-core-store)
(require 'supertag-ops-node)
(require 'supertag-ops-relation)
(require 'supertag-services-sync)
(require 'supertag-concept)

(defmacro concept-test--with-env (&rest body)
  "Run BODY with an isolated store and temp directory."
  (declare (indent 0))
  `(let* ((tmp (make-temp-file "supertag-concept-test" t))
          (supertag-data-directory tmp)
          (supertag-db-file (expand-file-name "supertag-db.el" tmp))
          (supertag-db-backup-directory (expand-file-name "backups" tmp))
          (supertag--store nil)
          (supertag--store-origin nil)
          (org-id-locations nil)
          (org-id-files nil)
          (org-id-locations-file (expand-file-name "org-id-locations" tmp)))
     (unwind-protect
         (progn
           (supertag--ensure-store)
           ,@body)
       (ignore-errors
         (dolist (buffer (buffer-list))
           (when (and (buffer-file-name buffer)
                      (string-prefix-p tmp (buffer-file-name buffer)))
             (kill-buffer buffer))))
       (ignore-errors
         (delete-directory tmp t)))))

(defun concept-test--create-node (id title &optional aliases)
  "Create a concept node with ID, TITLE and optional ALIASES."
  (supertag-node-create
   (list :id id
         :title title
         :file (expand-file-name "concepts.org" supertag-data-directory)
         :level 1
         :position 1
         :properties (append (list :SUPERTAG_CONCEPT "t")
                             (when aliases
                               (list :SUPERTAG_ALIASES aliases))))))

(defun concept-test--text-property-at-search (text prop)
  "Search TEXT and return PROP at the match beginning."
  (goto-char (point-min))
  (search-forward text)
  (get-text-property (match-beginning 0) prop))

(ert-deftest concept-entries-use-title-and-alias-only-for-concepts ()
  "Concept entries include title/aliases from marked nodes only."
  (concept-test--with-env
    (concept-test--create-node "concept-id" "大语言模型" "LLM, 大模型")
    (supertag-node-create
     (list :id "ordinary-id" :title "普通节点" :level 1 :position 1))
    (let ((entries (supertag-concept-entries)))
      (should (equal (cdr (assoc "大语言模型" entries)) "concept-id"))
      (should (equal (cdr (assoc "LLM" entries)) "concept-id"))
      (should (equal (cdr (assoc "大模型" entries)) "concept-id"))
      (should-not (assoc "普通节点" entries)))))

(ert-deftest concept-mention-mode-highlights-plain-mentions-not-org-links ()
  "Mention mode uses its own face and skips explicit Org links."
  (concept-test--with-env
    (concept-test--create-node "concept-id" "注意力机制" "Attention")
    (with-temp-buffer
      (org-mode)
      (insert "注意力机制 and Attention\n[[id:concept-id][注意力机制]]\n")
      (supertag-concept-link-mode 1)
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "注意力机制")
      (should (equal (get-text-property (match-beginning 0) 'supertag-concept-node-id)
                     "concept-id"))
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'supertag-concept-mention-face))
      (should (eq (get-text-property (match-beginning 0) 'keymap)
                  supertag-concept-mention-map))
      (let (opened)
        (cl-letf (((symbol-function 'supertag-goto-node)
                   (lambda (node-id) (setq opened node-id))))
          (goto-char (match-beginning 0))
          (supertag-concept-open-at-point))
        (should (equal opened "concept-id")))
      (should (equal (concept-test--text-property-at-search "Attention"
                                                            'supertag-concept-node-id)
                     "concept-id"))
      (goto-char (point-min))
      (search-forward "[[id:concept-id][")
      (let ((link-desc-pos (point)))
        (should-not (get-text-property link-desc-pos 'supertag-concept-node-id))))))

(ert-deftest concept-mention-mode-prefers-longest-match ()
  "Overlapping concept terms prefer the longest title."
  (concept-test--with-env
    (concept-test--create-node "base-id" "大语言模型")
    (concept-test--create-node "long-id" "大语言模型微调")
    (with-temp-buffer
      (org-mode)
      (insert "大语言模型微调")
      (supertag-concept-link-mode 1)
      (font-lock-ensure)
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'supertag-concept-node-id)
                     "long-id")))))

(ert-deftest promote-concept-keeps-text-and-creates-one-reference ()
  "Promoting selected text leaves it plain and adds one current-node reference."
  (concept-test--with-env
    (let ((test-file (expand-file-name "notes.org" supertag-data-directory)))
      (with-temp-file test-file
        (org-mode)
        (insert "* Source\n:PROPERTIES:\n:ID:       source-id\n:END:\n\n这里讨论注意力机制。\n\n* 注意力机制\n:PROPERTIES:\n:ID:       concept-id\n:SUPERTAG_CONCEPT: t\n:END:\n\n"))
      (with-current-buffer (find-file-noselect test-file)
        (org-mode)
        (org-id-update-id-locations nil t)
        (goto-char (point-min))
        (org-back-to-heading t)
        (supertag-node-sync-at-point)
        (org-next-visible-heading 1)
        (supertag-node-sync-at-point)
        (goto-char (point-min))
        (search-forward "注意力机制")
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (goto-char (point-max))
          (supertag-promote-concept beg end)
          (goto-char (point-max))
          (supertag-promote-concept beg end))
        (goto-char (point-min))
        (org-back-to-heading t)
        (let ((source-end (save-excursion (org-end-of-subtree t t))))
          (should (re-search-forward "这里讨论注意力机制。" source-end t))
          (goto-char (point-min))
          (org-back-to-heading t)
          (should-not (re-search-forward "\\[\\[id:concept-id\\]" source-end t)))
        (should (= 1 (length (supertag-relation-find-between
                              "source-id" "concept-id" :reference))))))))

(when noninteractive
  (ert-run-tests-batch-and-exit))

(provide 'test-concept-mention)
;;; test-concept-mention.el ends here

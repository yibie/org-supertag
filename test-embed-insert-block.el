;;; test-embed-insert-block.el --- self-check for embed block insertion -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q --eval '(package-initialize)' -L . -l test-embed-insert-block.el

(require 'cl-lib)
(require 'supertag-ui-embed)

(defun test-embed-insert-block--select-node (&rest _)
  "Return a stable node id for embed insertion tests."
  "node-1")

(defun test-embed-insert-block--node-get (node-id)
  "Return fake NODE-ID data."
  (when (string= node-id "node-1")
    '(:id "node-1" :title "Node One")))

(defun test-embed-insert-block--generate-content (node-id)
  "Return fake embed content for NODE-ID."
  (when (string= node-id "node-1")
    "Body\n"))

(advice-add 'supertag-ui-select-node :override #'test-embed-insert-block--select-node)
(advice-add 'supertag-node-get :override #'test-embed-insert-block--node-get)
(advice-add 'supertag-ui-embed-generate-node-content
            :override #'test-embed-insert-block--generate-content)

(with-temp-buffer
  (insert "prefix")
  (supertag-ui-embed--insert-block)
  (cl-assert (string= (buffer-string)
                      "prefix\n#+begin_embed: node-1 [Node One]\nBody\n#+end_embed\n")
             nil "unexpected embed block: %S" (buffer-string)))

(with-temp-buffer
  (org-mode)
  (insert "[[id:node-1][Node One]]")
  (goto-char (point-min))
  (supertag-ui-embed--link-to-block)
  (cl-assert (string= (buffer-string)
                      "#+begin_embed: node-1 [Node One]\nBody\n#+end_embed\n")
             nil "unexpected converted embed block: %S" (buffer-string)))

(message "OK embed insert block keeps node-data in scope.")
(kill-emacs 0)

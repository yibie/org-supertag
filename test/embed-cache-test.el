;;; embed-cache-test.el --- Embed cache regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org-element)
(require 'supertag-services-embed)

(ert-deftest supertag-embed-render-keeps-org-element-cache-valid ()
  "Rendering into an open Org buffer must not leave stale parser positions."
  (let ((file (make-temp-file "supertag-embed-cache-" nil ".org"))
        buffer
        (save-hook-inhibited 'not-run)
        warnings)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* Target\n:PROPERTIES:\n:ID: node-1\n:END:\n")
            (dotimes (index 1200)
              (insert (format "- old item %04d with enough text to move cache positions\n"
                              index)))
            (insert "* After\nBody\n"))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (org-mode)
            (setq-local org-element-use-cache t)
            (org-element-cache-reset)
            (org-element-parse-buffer)
            (goto-char (point-min))
            (re-search-forward "^- old item 0800")
            (org-element-context)
            (add-hook 'after-save-hook
                      (lambda ()
                        (setq save-hook-inhibited inhibit-modification-hooks))
                      nil t)
            (cl-letf (((symbol-function 'supertag-node-get)
                       (lambda (_node-id)
                         (list :id "node-1"
                               :file file
                               :content "- replacement")))
                      ((symbol-function 'display-warning)
                       (lambda (type message &rest _)
                         (when (eq type 'org-element)
                           (push message warnings)))))
              (supertag-services-embed--render-node-to-file "node-1")
              (should-not (eq save-hook-inhibited 'not-run))
              (should-not save-hook-inhibited)
              (should (= org-element--cache-last-buffer-size (buffer-size)))
              (should (eq org-element--cache-change-tic
                          (buffer-chars-modified-tick)))
              (goto-char (point-min))
              (re-search-forward "^- replacement$")
              (org-element-context))
            (should-not warnings)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest supertag-embed-sync-does-not-leak-suppressed-hooks-to-writers ()
  "The save hook scanner must not dynamically suppress downstream writes."
  (with-temp-buffer
    (insert "#+begin_embed: node-1\nBody\n#+end_embed\n")
    (let (render-inhibited)
      (cl-letf (((symbol-function 'supertag-ui-embed-get-inner-block-region)
                 (lambda (_source-id)
                   (cons (point-min) (point-max))))
                ((symbol-function 'supertag-services-embed--update-node-in-db)
                 #'ignore)
                ((symbol-function 'supertag-services-embed--render-node-to-file)
                 (lambda (_source-id)
                   (setq render-inhibited inhibit-modification-hooks))))
        (supertag-embed-sync-modified-blocks))
      (should-not render-inhibited))))

(provide 'embed-cache-test)
;;; embed-cache-test.el ends here

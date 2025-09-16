;;; supertag-ui-completion.el --- Simple tag completion -*- lexical-binding: t; -*-

;; Simple, working tag completion for org-supertag.
;; Type `#` followed by tag name to get completion.

;;; Code:

(require 'org)
(require 'org-id nil t)

;; Optional requires
(require 'supertag-ops-tag nil t)
(require 'supertag-ops-node nil t)

;;; Core Functions

(defun supertag-ui-completion--tag-bounds ()
  "Return (START . END) for tag name after `#`, or nil."
  (let ((end (point)))
    (save-excursion
      (let ((pos (point)))
        ;; Skip back over tag characters
        (while (and (> pos (point-min))
                    (let ((ch (char-before pos)))
                      (or (and ch (or (= (char-syntax ch) ?w)
                                      (= (char-syntax ch) ?_)
                                      (= ch ?-))))))
          (setq pos (1- pos)))
        ;; Check if we have `#` before the tag
        (when (and (> pos (point-min))
                   (= (char-before pos) ?#))
          (cons pos end))))))

(defun supertag-ui-completion--get-tag-candidates ()
  "Get list of tag candidates."
  (cond
   ;; Try to get from database
   ((fboundp 'supertag-ops-tag-list)
    (ignore-errors 
      (let ((tags (supertag-ops-tag-list)))
        (mapcar (lambda (tag)
                  (if (stringp tag) tag (format "%s" tag)))
                tags))))
   ;; Fallback: scan buffer for tags
   (t
    (let (acc)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "#\\([[:alnum:]_-]+\\)" nil t)
          (push (match-string-no-properties 1) acc)))
      (delete-dups acc)))))

(defun supertag-ui-completion--apply-tag (tagname)
  "Apply TAGNAME to current node."
  (condition-case nil
      (save-excursion
        ;; Go to the heading of current section
        (unless (org-at-heading-p)
          (org-back-to-heading t))
        
        (let ((node-id (or (org-id-get) (org-id-get-create))))
          (when node-id
            ;; Ensure node exists in database
            (when (and (fboundp 'supertag-node-get)
                       (fboundp 'supertag-node-create)
                       (not (supertag-node-get node-id)))
              (let ((title (nth 4 (org-heading-components))))
                (supertag-node-create `(:id ,node-id :title ,(or title "Untitled")))))
            
            ;; Add tag to node
            (cond
             ((fboundp 'supertag-node-add-tag)
              (supertag-node-add-tag node-id tagname)
              (message "Added tag '%s'" tagname))
             (t
              (message "Tag '%s' added to buffer only" tagname))))))
    (error
     (message "Could not find heading for tag '%s'" tagname))))

(defun supertag-ui-completion--tag-exit (candidate _status)
  "Called after user selects tag CANDIDATE."
  (condition-case err
      (progn
        (supertag-ui-completion--apply-tag candidate)
        ;; Add space after tag
        (unless (looking-at-p "\\s-")
          (insert " ")))
    (error (message "Tag completion error: %S" err))))

;;; CAPF

(defun supertag-ui-completion--capf ()
  "Completion-at-point function for tags."
  (when-let ((bounds (supertag-ui-completion--tag-bounds)))
    (let ((start (car bounds))
          (end (cdr bounds))
          (candidates (supertag-ui-completion--get-tag-candidates)))
      (list start end candidates
            :exit-function #'supertag-ui-completion--tag-exit
            :category 'supertag-tag))))

;;; Auto-trigger

(defun supertag-ui-completion--auto-trigger ()
  "Auto-trigger completion when typing `#`."
  (when (eq (char-before) ?#)
    (completion-at-point)))

;;; Minor Mode

;;;###autoload
(define-minor-mode supertag-ui-completion-mode
  "Simple tag completion for org-supertag."
  :lighter " ST-C"
  (if supertag-ui-completion-mode
      (progn
        (add-hook 'completion-at-point-functions #'supertag-ui-completion--capf nil t)
        (add-hook 'post-self-insert-hook #'supertag-ui-completion--auto-trigger nil t))
    (remove-hook 'completion-at-point-functions #'supertag-ui-completion--capf t)
    (remove-hook 'post-self-insert-hook #'supertag-ui-completion--auto-trigger t)))

;;;###autoload
(defun supertag-ui-completion-enable ()
  "Enable tag completion in org-mode buffers."
  (when (derived-mode-p 'org-mode)
    (supertag-ui-completion-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-supertag-ui-completion-mode
  supertag-ui-completion-mode
  supertag-ui-completion-enable)

(provide 'supertag-ui-completion)

;;; supertag-ui-completion.el ends here

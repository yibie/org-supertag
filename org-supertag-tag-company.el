;; org-supertag-tag-company.el --- Company completion for org-supertag tags -*- lexical-binding: t -*-

(require 'company)

(defvar org-supertag-company-prefix-regexp
  "#\\([[:alnum:]_-]*\\)"
  "Regexp to match the prefix for company completion.
Only allows alphanumeric characters, underscore and hyphen after #.")

(defun org-supertag-company--make-candidate (tag-name)
  "Create a company candidate from TAG-NAME."
  (let* ((tag (org-supertag-tag-get tag-name))
         (fields (plist-get tag :fields))
         (field-count (length fields))
         (base-tag (org-supertag-tag-get-base tag-name))
         (help-text
          (if base-tag
              (format "Extension of #%s with %d field(s)" 
                      base-tag field-count)
            (format "Base tag with %d field(s)" field-count))))
    (propertize tag-name
                'tag tag
                'help-echo (format "SuperTag with %d field(s)" field-count))))

(defun org-supertag-company--candidates (prefix)
  "Get completion candidates for PREFIX."
  (let* ((prefix-no-hash (if (> (length prefix) 1)
                            (org-supertag-sanitize-tag-name (substring prefix 1))
                          ""))
         (all-tags (org-supertag-get-all-tags)))
    (cl-loop for tag-name in all-tags
             when (string-prefix-p prefix-no-hash tag-name t)
             collect (org-supertag-company--make-candidate tag-name))))

(defun org-supertag-company--post-completion (candidate)
  "Handle post-completion actions for CANDIDATE.
If CANDIDATE is a non-existent tag name, create it directly."
  (let* ((tag-name (if (get-text-property 0 'tag candidate)
                       (plist-get (get-text-property 0 'tag candidate) :name)
                     ;; For non-existent tags, use candidate directly
                     candidate))
         (node-id (org-id-get))
         (current-tags (and node-id (org-supertag-node-get-tags node-id))))
    
    ;; Delete the completion prefix including #
    (delete-region (- (point) (length candidate) 1) (point))
    
    ;; Only proceed if tag not already applied
    (if (and node-id (member tag-name current-tags))
        (message "Tag '%s' is already applied to this node" tag-name)
      ;; Create tag if it doesn't exist
      (unless (org-supertag-tag-exists-p tag-name)
        (org-supertag-tag-create tag-name))
      ;; Apply tag
      (org-supertag-tag-apply tag-name))))

;;;###autoload
(defun org-supertag-company-backend (command &optional arg &rest ignored)
  "Company backend for org-supertag completion.
COMMAND, ARG and IGNORED are standard arguments for company backends."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'org-supertag-company-backend))
    (prefix (and (eq major-mode 'org-mode)
                 (or (org-at-heading-p)  
                     (org-at-property-p)) 
                 (save-excursion
                   (when (looking-back org-supertag-company-prefix-regexp
                                     (line-beginning-position))
                     (match-string-no-properties 0)))))
    (candidates (org-supertag-company--candidates arg))
    (post-completion (org-supertag-company--post-completion arg))
    (annotation (let* ((tag (get-text-property 0 'tag arg))
                      (fields (and tag (plist-get tag :fields)))
                      (base-tag (and tag (org-supertag-tag-get-base (plist-get tag :id)))))
                 (when fields
                   (if base-tag
                       (format " [%d fields, extends %s]"
                               (length fields)
                               base-tag)
                     (format " [%d fields]" (length fields)))))
    (t nil))))

;;;###autoload
(defun org-supertag-setup-completion ()
  "Setup company completion for org-supertag."
  (when (and (eq major-mode 'org-mode)
             (featurep 'company))
    (add-to-list 'company-backends 'org-supertag-company-backend)
    (make-local-variable 'company-minimum-prefix-length)
    (setq-local company-minimum-prefix-length 1))) 

(add-hook 'org-mode-hook #'org-supertag-setup-completion)

(provide 'org-supertag-tag-company)
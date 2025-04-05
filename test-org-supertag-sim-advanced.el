;;; org-supertag-sim-advanced.el --- Advanced EPC operations for org-supertag

;; Copyright (C) 2023 

;; Author: 
;; Keywords: org-mode, nlp, epc

;; This file is not part of GNU Emacs.

;;; Commentary:
;; 
;; This module provides advanced operations for org-supertag using the EPC
;; communication mechanism defined in org-supertag-sim-epc.el.
;;
;; The main functions are:
;; - Interactive entity extraction from text
;; - Batch processing of entities from multiple files
;; - Interactive tag suggestion helpers
;; - Similarity-based tag navigation
;;

;;; Code:

(require 'org-supertag-sim-epc)
(require 'org-supertag-db)
(require 'org)
(require 'cl-lib)

(defgroup org-supertag-sim-advanced nil
  "Advanced NLP features for Org Supertag."
  :group 'org-supertag)

(defcustom org-supertag-sim-suggest-threshold 0.6
  "Threshold for tag similarity suggestions (0.0-1.0).
Higher values require greater similarity."
  :type 'float
  :group 'org-supertag-sim-advanced)

(defcustom org-supertag-sim-max-suggestions 5
  "Maximum number of tag suggestions to display."
  :type 'integer
  :group 'org-supertag-sim-advanced)

(defcustom org-supertag-sim-batch-size 10
  "Number of files to process in each batch for batch operations."
  :type 'integer
  :group 'org-supertag-sim-advanced)

(defcustom org-supertag-sim-disable-live-suggestions nil
  "When non-nil, disable real-time tag suggestions."
  :type 'boolean
  :group 'org-supertag-sim-advanced)

;;;###autoload
(defun org-supertag-sim-extract-from-region (begin end)
  "Extract entities from selected region and provide tag suggestions.
BEGIN and END are the start and end positions of the region."
  (interactive "r")
  (unless (region-active-p)
    (user-error "No region selected"))
  
  (let ((text (buffer-substring-no-properties begin end)))
    (message "Analyzing selected region...")
    
    ;; Use EPC asynchronous processing
    (org-supertag-sim-epc-extract-entities-async 
     text
     (lambda (entities)
       ;; Entity extraction callback
       (when entities
         (let ((buf (get-buffer-create "*org-supertag-entities*"))
               (entity-count (length entities)))
           (with-current-buffer buf
             (erase-buffer)
             (insert (format "Extracted %d entities from selected text:\n\n" entity-count))
             
             ;; Display extracted entities
             (dolist (entity entities)
               (let ((entity-text (cdr (assoc 'entity entity)))
                     (entity-type (cdr (assoc 'type entity))))
                 (insert (format "- %s (%s)\n" entity-text entity-type))))
             
             ;; If there are entities, provide tag suggestions
             (when entities
               (insert "\nGenerating tag suggestions based on extracted entities...\n")
               
               ;; Extract entity text
               (let ((entity-texts (mapcar (lambda (e) (cdr (assoc 'entity e))) entities)))
                 ;; Connect all entity texts
                 (let ((combined-text (mapconcat 'identity entity-texts " ")))
                   ;; Asynchronously get tag suggestions
                   (org-supertag-sim-epc-get-tag-suggestions-async 
                    combined-text
                    org-supertag-sim-max-suggestions
                    (lambda (suggestions)
                      ;; Tag suggestion callback
                      (with-current-buffer buf
                        (goto-char (point-max))
                        (insert "\nTag suggestions:\n\n")
                        
                        ;; Display suggested tags
                        (if suggestions
                            (dolist (suggestion suggestions)
                              (let ((tag-name (cdr (assoc 'tag suggestion)))
                                    (score (cdr (assoc 'score suggestion))))
                                (insert (format "- %s (%.2f)\n" tag-name score))))
                          (insert "No related tag suggestions found\n"))
                        
                        ;; Add apply suggestion button
                        (insert "\n")
                        (dolist (suggestion suggestions)
                          (let ((tag-name (cdr (assoc 'tag suggestion))))
                            (insert-button (format "[Apply tag: %s] " tag-name)
                                          'action `(lambda (_)
                                                    (org-supertag-sim-advanced--apply-tag ,tag-name))
                                          'follow-link t
                                          'help-echo (format "Apply tag '%s' to current entry" tag-name))
                          (insert " ")))
                        
                        ;; Add create all tags button
                        (when suggestions
                          (insert "\n\n")
                          (insert-button "[Apply all suggestions]"
                                        'action `(lambda (_)
                                                  (org-supertag-sim-advanced--apply-all-tags ',suggestions))
                                        'follow-link t
                                        'help-echo "Apply all suggested tags to current entry"))))))))))
           
           ;; Display result buffer
           (switch-to-buffer-other-window buf)))))))

(defun org-supertag-sim-advanced--apply-tag (tag-name)
  "Apply a tag to the current Org entry.
TAG-NAME is the name of the tag to apply."
  (when (and tag-name (org-supertag-db-find-by-name tag-name :tag))
    ;; If tag exists, apply directly
    (message "Applying tag: %s" tag-name)
    (org-back-to-heading t)
    (let ((tag-id (org-supertag-db-find-by-name tag-name :tag)))
      (when tag-id
        (org-supertag-add `(:tag ,tag-id)))))
  
  ;; If tag does not exist, prompt to create
  (when (and tag-name (not (org-supertag-db-find-by-name tag-name :tag)))
    (when (yes-or-no-p (format "Tag '%s' does not exist, create it? " tag-name))
      (let ((tag-id (org-supertag-upsert `(:name ,tag-name :type :tag))))
        (when tag-id
          (message "Created and applied tag: %s" tag-name)
          (org-back-to-heading t)
          (org-supertag-add `(:tag ,tag-id)))))))

(defun org-supertag-sim-advanced--apply-all-tags (suggestions)
  "Apply all suggested tags to the current Org entry.
SUGGESTIONS is the list of tag suggestions."
  (org-back-to-heading t)
  (let ((applied-count 0))
    (dolist (suggestion suggestions)
      (let* ((tag-name (cdr (assoc 'tag suggestion)))
             (tag-id (org-supertag-db-find-by-name tag-name :tag)))
        
        ;; If tag does not exist, create it
        (unless tag-id
          (setq tag-id (org-supertag-upsert `(:name ,tag-name :type :tag))))
        
        ;; Apply the tag
        (when tag-id
          (org-supertag-add `(:tag ,tag-id))
          (cl-incf applied-count))))
    
    (message "Applied %d tags" applied-count)))

;;;###autoload
(defun org-supertag-sim-suggest-for-buffer ()
  "Generate tag suggestions for the current buffer content."
  (interactive)
  (save-excursion
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (message "Analyzing buffer content...")
      
      ;; Asynchronously get tag suggestions
      (org-supertag-sim-epc-get-tag-suggestions-async 
       text
       org-supertag-sim-max-suggestions
       (lambda (suggestions)
         ;; Create an interactive selection interface
         (if suggestions
             (let* ((choices (mapcar (lambda (s) 
                                      (cons (format "%s (%.2f)" 
                                                    (cdr (assoc 'tag s)) 
                                                    (cdr (assoc 'score s)))
                                            s))
                                    suggestions))
                    (selection (completing-read "Select tag to apply: " choices nil t)))
               (when selection
                 (let* ((selected-suggestion (cdr (assoc selection choices)))
                        (tag-name (cdr (assoc 'tag selected-suggestion))))
                   (org-supertag-sim-advanced--apply-tag tag-name))))
           (message "No related tag suggestions found")))))))

;;;###autoload
(defun org-supertag-sim-extract-from-current-heading ()
  "Extract entities from the current Org heading and provide tag suggestions."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((heading-element (org-element-at-point))
           (heading-text (org-element-property :title heading-element))
           (section-end (or (org-element_property :contents-end heading-element)
                            (save-excursion 
                              (org-end-of-subtree t) 
                              (point))))
           (content-start (org-element-property :contents-begin heading-element))
           (content (when content-start
                      (buffer-substring-no-properties content-start section-end)))
           (full-text (concat heading-text " " (or content ""))))
      
      (message "Analyzing current heading content...")
      
      ;; Use EPC asynchronous processing
      (org-supertag-sim-epc-extract-entities-async 
       full-text
       (lambda (entities)
         ;; Entity extraction callback
         (when entities
           (let ((entity-texts (mapcar (lambda (e) (cdr (assoc 'entity e))) entities)))
             ;; Connect all entity texts
             (let ((combined-text (mapconcat 'identity entity-texts " ")))
               ;; Asynchronously get tag suggestions
               (org-supertag-sim-epc-get-tag-suggestions-async 
                combined-text
                org-supertag-sim-max-suggestions
                (lambda (suggestions)
                  ;; Create an interactive selection interface
                  (if suggestions
                      (let* ((choices (mapcar (lambda (s) 
                                               (cons (format "%s (%.2f)" 
                                                             (cdr (assoc 'tag s)) 
                                                             (cdr (assoc 'score s)))
                                                     s))
                                             suggestions))
                             (selection (completing-read "Select tag to apply: " choices nil t)))
                        (when selection
                          (let* ((selected-suggestion (cdr (assoc selection choices)))
                                 (tag-name (cdr (assoc 'tag selected-suggestion))))
                            (org-supertag-sim-advanced--apply-tag tag-name))))
                    (message "No related tag suggestions found"))))))))))))

;;;###autoload
(defun org-supertag-sim-find-similar-to-tag (tag-id)
  "Find tags similar to the specified tag and provide navigation options.
TAG-ID is the tag ID to find similar tags."
  (interactive (list (org-supertag-sim-advanced--select-tag)))
  
  (when tag-id
    (let* ((tag-props (org-supertag-db-get tag-id))
           (tag-name (plist-get tag-props :name)))
      (message "Finding tags similar to '%s'..." tag-name)
      
      ;; Asynchronously find similar tags
      (org-supertag-sim-epc-find-similar-async
       tag-name
       org-supertag-sim-max-suggestions
       (lambda (similar-tags)
         (if similar-tags
             (let* ((choices (mapcar (lambda (s)
                                      (cons (format "%s (%.2f)" 
                                                    (cdr (assoc 'tag s)) 
                                                    (cdr (assoc 'score s)))
                                            s))
                                    similar-tags))
                    (selection (completing-read "Select similar tag to navigate to: " choices nil t)))
               (when selection
                 (let* ((selected-tag (cdr (assoc selection choices)))
                        (tag-name (cdr (assoc 'tag selected-tag)))
                        (tag-id (org-supertag-db-find-by-name tag-name :tag)))
                   (when tag-id
                     (org-supertag-find tag-id)))))
           (message "No similar tags found for '%s'" tag-name)))))))

(defun org-supertag-sim-advanced--select-tag ()
  "Select a tag and return its ID."
  (let* ((tags (org-supertag-db-find-by-type :tag))
         (tag-names (mapcar (lambda (id)
                             (let ((props (org-supertag-db-get id)))
                               (cons (plist-get props :name) id)))
                           tags))
         (selection (completing-read "Select tag: " tag-names nil t)))
    (cdr (assoc selection tag-names))))

;;;###autoload
(defun org-supertag-sim-batch-process-org-files (org-files)
  "Batch process multiple Org files, extract entities and generate tag suggestions for each entry.
ORG-FILES is a list of Org file paths."
  (interactive (list (directory-files-recursively 
                      (read-directory-name "Select Org file directory: ") 
                      "\\.org$")))
  
  (when org-files
    (let ((total-files (length org-files))
          (processed-files 0)
          (report-buffer (get-buffer-create "*SimTag Batch Processing*"))
          (batch-timer nil))
      
      ;; Initialize the report buffer
      (with-current-buffer report-buffer
        (erase-buffer)
        (insert "SimTag Batch Processing Report\n")
        (insert "===================\n\n")
        (insert (format "Total files: %d\n\n" total-files))
        (insert "Processing...\n\n"))
      
      ;; Display the report buffer
      (display-buffer report-buffer)
      
      ;; Define the batch processing function
      (cl-labels 
          ((process-batch 
            (remaining-files)
            (if (null remaining-files)
                ;; All files processed
                (with-current-buffer report-buffer
                  (goto-char (point-max))
                  (insert "\nProcessing completed!\n")
                  (insert (format "Processed %d files\n" processed-files)))
              
              ;; Get the current batch of files
              (let* ((current-batch (cl-subseq remaining-files 0 
                                               (min org-supertag-sim-batch-size 
                                                    (length remaining-files))))
                     (next-batch (cl-subseq remaining-files 
                                            (min org-supertag-sim-batch-size 
                                                 (length remaining-files)))))
                
                ;; Process the current batch of files
                (process-file (car current-batch) 
                              (cdr current-batch) 
                              next-batch)))))
        
        ;; Define the single file processing function
        (process-file 
         (file remaining-files-in-batch next-batch)
         (if (null file)
             ;; Current batch processed, continue with the next batch
             (setq batch-timer 
                   (run-with-timer 0.5 nil #'process-batch next-batch))
           
           ;; Process the single file
           (with-current-buffer report-buffer
             (goto-char (point-max))
             (insert (format "Processing file: %s\n" file)))
           
           (with-temp-buffer
             (insert-file-contents file)
             (org-mode)
             (setq processed-files (1+ processed-files))
             
             ;; Process each heading in the file
             (org-map-entries
              (lambda ()
                (let* ((heading-element (org-element-at-point))
                       (heading-text (org-element-property :title heading-element))
                       (section-end (or (org-element_property :contents-end heading-element)
                                        (save-excursion 
                                          (org-end-of-subtree t) 
                                          (point))))
                       (content-start (org-element-property :contents-begin heading-element))
                       (content (when content-start
                                  (buffer-substring-no-properties content-start section-end)))
                       (full-text (concat heading-text " " (or content ""))))
                  
                  ;; Record the processed heading
                  (with-current-buffer report-buffer
                    (goto-char (point-max))
                    (insert (format "  - Heading: %s\n" heading-text)))
                  
                  ;; Asynchronously extract entities
                  (org-supertag-sim-epc-extract-entities-async 
                   full-text
                   (lambda (entities)
                     (when entities
                       ;; Record the extracted entities
                       (with-current-buffer report-buffer
                         (goto-char (point-max))
                         (insert (format "    - Extracted %d entities\n" (length entities))))
                       
                       ;; Combine entity texts and get tag suggestions
                       (let ((entity-texts (mapcar (lambda (e) 
                                                    (cdr (assoc 'entity e))) 
                                                  entities)))
                         (when entity-texts
                           (let ((combined-text (mapconcat 'identity entity-texts " ")))
                             ;; Asynchronously get tag suggestions
                             (org-supertag-sim-epc-get-tag-suggestions-async 
                              combined-text
                              org-supertag-sim-max-suggestions
                              (lambda (suggestions)
                                (when suggestions
                                  ;; Record the tag suggestions
                                  (with-current-buffer report-buffer
                                    (goto-char (point-max))
                                    (insert (format "    - Generated %d tag suggestions\n" 
                                                    (length suggestions)))
                                    (dolist (suggestion suggestions)
                                      (insert (format "      * %s (%.2f)\n" 
                                                      (cdr (assoc 'tag suggestion))
                                                      (cdr (assoc 'score suggestion)))))))))))))))))))
             
             ;; Current file processed, continue with the next file in the batch
             (process-file (car remaining-files-in-batch)
                           (cdr remaining-files-in-batch)
                           next-batch))))
        
        ;; Start processing the first batch of files
        (process-batch org-files)))))

;; Provide a hook function for automatic tag suggestions
(defvar org-supertag-sim-suggestion-timer nil
  "Timer for delayed tag suggestions.")

(defvar org-supertag-sim-last-content ""
  "Last analyzed content, used to avoid duplicate analysis.")

(defun org-supertag-sim-advanced--suggest-on-idle ()
  "Provide tag suggestions for the current heading when idle."
  (unless org-supertag-sim-disable-live-suggestions
    (when (and (eq major-mode 'org-mode)
               (not (minibufferp)))
      ;; Cancel the previous timer
      (when org-supertag-sim-suggestion-timer
        (cancel-timer org-supertag-sim-suggestion-timer))
      
      ;; Set a new timer
      (setq org-supertag-sim-suggestion-timer
            (run-with-idle-timer 
             2 nil
             (lambda ()
               (when (and (eq major-mode 'org-mode)
                          (not (minibufferp)))
                 (save-excursion
                   (when (org-at-heading-p)
                     (let* ((heading-element (org-element-at-point))
                            (heading-text (org-element-property :title heading-element))
                            (full-text heading-text))
                       (unless (string= full-text org-supertag-sim-last-content)
                         (setq org-supertag-sim-last-content full-text)
                         (org-supertag-sim-epc-get-tag-suggestions-async 
                          full-text
                          (lambda (suggestions)
                            (when (and suggestions 
                                       (> (length suggestions) 0))
                              (message "Tag suggestions: %s" 
                                       (mapconcat 
                                        (lambda (s) 
                                          (format "%s" (cdr (assoc 'tag s))))
                                        suggestions ", ")))))))))))))))
(add-hook 'post-command-hook 'org-supertag-sim-advanced--suggest-on-idle)

;;;###autoload
(defun org-supertag-sim-toggle-live-suggestions ()
  "Toggle real-time tag suggestions."
  (interactive)
  (setq org-supertag-sim-disable-live-suggestions 
        (not org-supertag-sim-disable-live-suggestions))
  (message "Real-time tag suggestions are now %s" 
           (if org-supertag-sim-disable-live-suggestions "disabled" "enabled")))

(provide 'org-supertag-sim-advanced)
;;; org-supertag-sim-advanced.el ends here 
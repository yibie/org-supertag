;;; org-supertag/ui/services.el --- Reusable UI component services -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'supertag-services-query)


(defun supertag-ui--adjust-content-level (content from-level to-level)
  "Adjust all heading levels in CONTENT string.
Moves the top-level heading from FROM-LEVEL to TO-LEVEL, and adjusts
all subheadings proportionally."
  (if (or (not from-level) (not to-level) (= from-level to-level))
      content ; No adjustment needed
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (let ((level-diff (- to-level from-level)))
        ;; Use while to adjust all headlines in the content block.
        (while (re-search-forward "^\\(\\*+\\)\\s-+\\(.*\\)" nil t)
          (let* ((current-stars-str (match-string 1))
                 (title-text (match-string 2))
                 (current-level (length current-stars-str))
                 (new-level (+ current-level level-diff)))
            ;; Ensure we don't create a level 0 or negative heading.
            (when (> new-level 0)
              ;; Replace the entire matched line (stars + space + title)
              ;; with a correctly formatted new one.
              (replace-match (concat (make-string new-level ?*) " " title-text)
                             t ; fixedcase
                             t ; literal
                             )))))
      (buffer-string))))

  (defun supertag-ui-select-insert-position (file)
    "Interactively let user select an insert position in FILE.
  This provides a two-step selection for clarity.
  Returns a plist (:position POS :level LVL)."
    (with-current-buffer (find-file-noselect file)
      (let* ((headlines (org-map-entries
                         (lambda ()
                           (list (org-get-heading t t) (point) (org-outline-level)))
                         t 'file))
             (options '("File Top" "File End" "Under Heading..." "After Heading..."))
             (choice (completing-read "Insert position: " options nil t)))
        (cond
         ((string= choice "File Top")
          (list :position (point-min) :level 1))
         ((string= choice "File End")
          (list :position (point-max) :level 1))
         ((or (string= choice "Under Heading...") (string= choice "After Heading..."))
          (let* ((headline-titles (mapcar #'car headlines))
                 (selected-title (completing-read "Select target heading: " headline-titles nil t))
                 (headline-info (assoc selected-title headlines)))
            (when headline-info
              (let* ((pos (nth 1 headline-info))
                     (level (nth 2 headline-info)))
                (goto-char pos)
                (if (string= choice "Under Heading...")
                    (list :position (save-excursion (org-end-of-subtree t) (point))
                          :level (1+ level))
                  ;; After Heading...
                  (list :position (save-excursion (org-end-of-subtree t t) (point))
                        :level level))))))
         (t nil)))))

(defun supertag-goto-node (node-id &optional other-window)
  "Navigate to the location of NODE-ID based on data in the supertag store.
If OTHER-WINDOW is non-nil, open in another window."
  (when-let* ((node (supertag-get (list :nodes node-id)))
              (file (plist-get node :file)))
    (if (not (and file (file-exists-p file)))
        (message "Error: File for node %s does not exist or is not set." node-id)
      (let ((buffer (find-file-noselect file)))
        (if other-window
            (pop-to-buffer buffer)
          (switch-to-buffer buffer))
        (with-current-buffer buffer
          (goto-char (point-min))
          (if (re-search-forward (format ":ID:[ \t]+%s" (regexp-quote node-id)) nil t)
              (progn
                (org-back-to-heading t)
                (message "Jumped to node: %s" (or (plist-get node :raw-value) (plist-get node :title))))
            (message "Error: Could not find ID %s in file %s" node-id file)))))))

(defvar supertag-ui--node-cache nil
  "Cache for node selection candidates to improve performance.")

(defvar supertag-ui--cache-timestamp nil
  "Timestamp of when the node cache was last updated.")

(defun supertag-ui--clear-node-cache ()
  "Clear the node selection cache to force refresh on next access."
  (setq supertag-ui--node-cache nil
        supertag-ui--cache-timestamp nil))

(defun supertag-ui--get-cached-nodes ()
  "Get cached node candidates, refreshing if necessary."
  (let ((current-time (current-time)))
    ;; Refresh cache if it's older than 30 seconds or doesn't exist
    (when (or (null supertag-ui--cache-timestamp)
              (null supertag-ui--node-cache)
              (time-less-p (time-add supertag-ui--cache-timestamp 30) current-time))
      (message "Refreshing node cache...")
      (setq supertag-ui--node-cache (supertag-ui--build-node-candidates)
            supertag-ui--cache-timestamp current-time)
      (message "Node cache refreshed. Found %d nodes." (length supertag-ui--node-cache)))
    supertag-ui--node-cache))

(defun supertag-ui--build-node-candidates ()
  "Build the node candidates list efficiently."
  (let* ((nodes-hash (supertag-get '(:nodes)))
         (candidates '()))
    (when (hash-table-p nodes-hash)
      (maphash
       (lambda (id node-data)
         (when node-data
           (let* ((raw-title (or (plist-get node-data :raw-value)
                                 (plist-get node-data :title)
                                 "Untitled"))
                  (olp (plist-get node-data :olp))
                  (file (plist-get node-data :file))
                  (display-path (if olp
                                    (concat (mapconcat 'identity olp " / ") " / " raw-title)
                                  raw-title))
                  (display-str (if file
                                   (format "%s  (in %s)" display-path (file-name-nondirectory file))
                                 (format "%s  [orphaned]" display-path))))
             (push (cons display-str id) candidates))))
       nodes-hash))
    (sort candidates (lambda (a b) (string< (car a) (car b))))))

(defun supertag-ui-select-node (&optional prompt use-cache)
  "Interactively prompt user to select a node.
PROMPT is the prompt string (defaults to 'Select node: ').
USE-CACHE when non-nil uses cached data for better performance.
Returns the selected node's ID, or nil."
  (let* ((prompt-str (or prompt "Select node: "))
         (candidates (if use-cache
                         (supertag-ui--get-cached-nodes)
                       (supertag-ui--build-node-candidates)))
         (selected (completing-read prompt-str candidates nil t)))
    (when selected
      (cdr (assoc selected candidates)))))


(defun supertag-ui-select-reference-to-remove (from-node-id)
  "Interactively select a reference to remove from a given node.
FROM-NODE-ID is the ID of the node whose references are to be listed.
Returns the ID of the selected node to unlink."
  (let* ((source-node (supertag-get (list :nodes from-node-id)))
         (ref-to-ids (plist-get source-node :ref-to)))
    (if (not ref-to-ids)
        (progn (message "Node has no outgoing references.") nil)
      (let* ((candidates
              (mapcar (lambda (node-id)
                        (let* ((node-data (supertag-get (list :nodes node-id)))
                               (title (or (plist-get node-data :title) "Untitled"))
                               (file (plist-get node-data :file)))
                          (cons (if file
                                    (format "%s  (in %s)" title (file-name-nondirectory file))
                                  (format "%s  [orphaned]" title))
                                node-id)))
                      ref-to-ids))
             (selected-display (completing-read "Remove reference to: " candidates nil t)))
        (when selected-display
          (cdr (assoc selected-display candidates)))))))

(defun supertag-ui-read-field-value (field-def current-value)
  "Interactively read a new value for a field based on its definition.
FIELD-DEF is the field's schema definition.
CURRENT-VALUE is the existing value, used as a default.
Returns the new value entered by the user."
  (let* ((field-name (plist-get field-def :name))
         (field-type (plist-get field-def :type))
         (prompt (format "New value for %s: " field-name))
         (options (plist-get field-def :options)))
    (pcase field-type
      (:options (completing-read prompt options nil t nil))
      (:tag (supertag-ui--read-tag-field current-value))
      ;; TODO: Add more specific readers for other types like :date, :number
      (_ (read-string prompt current-value)))))

(defun supertag-ui-create-field-definition ()
  "Interactively create a new field definition.
Returns a field definition plist, or nil if cancelled."
  (message "DEBUG: supertag-field-types is %S" supertag-field-types)
  (let* ((name (read-string "Field name: ")))
    (when (and name (not (string-empty-p name)))
      (let* ((type-and-options (supertag-field-read-type-with-options :string))
             (type (car type-and-options))
             (options (cdr type-and-options))
             (default (read-string "Default value (optional): "))
             (field-def (list :name name :type type)))
        (unless (string-empty-p default)
          (setq field-def (plist-put field-def :default default)))
        ;; Add options for :options type
        (when (eq type :options)
          (setq field-def (plist-put field-def :options options)))
        field-def))))

(defun supertag-ui-select-tag-on-node (node-id)
  "Interactively select a tag that is present on NODE-ID.
Returns the selected tag ID, or nil if none."
  (let* ((relations (supertag-relation-find-by-from node-id :node-tag))
         (tag-ids (sort (mapcar (lambda (rel) (plist-get rel :to)) relations) #'string<)))
    (if (not tag-ids)
        (progn
          (message "Node has no tags.")
          nil)
      (completing-read "Select tag: " tag-ids nil t))))

(defun supertag-ui--read-tag-field (current-value)
  "Read tag field value with multi-selection support.
CURRENT-VALUE is the existing value (can be string or list).
Returns a comma-separated string of selected tags."
  (let* ((all-tags (mapcar #'car (supertag-query :tags)))
         (current-tags (cond
                        ((stringp current-value)
                         (if (string-empty-p current-value)
                             nil
                           (split-string current-value "," t "[ \t\n\r]+")))
                        ((listp current-value) current-value)
                        (t nil)))
         (selected-tags '())
         (continue t))
    (while continue
      (let* ((remaining-tags (cl-remove-if (lambda (tag) (member tag selected-tags)) all-tags))
             (prompt (if selected-tags
                         (format "Selected: %s. Add another tag (or empty to finish): "
                                 (string-join selected-tags ", "))
                       "Select tag (or empty to finish): "))
             (choice (if remaining-tags
                         (completing-read prompt
                                          (append remaining-tags '(""))
                                          nil t)
                       "")))
        (if (string-empty-p choice)
            (setq continue nil)
          (push choice selected-tags))))
    
    ;; Allow manual input via comma-separated string as fallback
    (when (and (null selected-tags) (not (string-empty-p (or current-value ""))))
      (let ((manual-input (read-string "Enter tags (comma-separated) or leave empty: "
                                       (if (stringp current-value) current-value ""))))
        (unless (string-empty-p manual-input)
          (setq selected-tags (split-string manual-input "," t "[ \t\n\r]+")))))
    
    ;; Return as comma-separated string for storage
    (if selected-tags
        (string-join (nreverse selected-tags) ",")
      "")))

;; Setup cache invalidation on data changes
(defun supertag-ui--invalidate-cache-on-change (path _old-value _new-value)
  "Invalidate UI cache when node data changes."
  (when (and path (eq (car path) :nodes))
    (supertag-ui--clear-node-cache)))

;; Register the cache invalidation hook (if the event system is available)
(when (fboundp 'supertag-register-listener)
  (supertag-register-listener :store-changed #'supertag-ui--invalidate-cache-on-change))

(provide 'supertag-services-ui)

;;; supertag-services-ui.el ends here

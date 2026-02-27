;;; org-supertag/ui/services.el --- Reusable UI component services -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'subr-x)
(require 'supertag-services-query)
(require 'supertag-core-store)
(require 'supertag-ops-node)
(require 'supertag-ops-field)  ; For type-specific field input assistance
(require 'supertag-ops-tag)
(require 'supertag-ops-relation)
(require 'supertag-services-sync) ; For supertag-node-sync-at-point
(require 'supertag-view-api)


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
    (unless file
      (user-error "FILE parameter cannot be nil"))
    (unless (file-exists-p file)
      (user-error "File does not exist: %s" file))
    (with-current-buffer (find-file-noselect file)
      (let* ((headlines (org-map-entries
                         #'(lambda ()
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
  (when-let* ((node (supertag-view-api-get-entity :nodes node-id))
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

;;; --- Shared Node View State Builder ---

(defun supertag-view--resolve-node-tags (node-id)
  "Return tag IDs for NODE-ID, preferring relations and falling back to node tags."
  (when (and node-id (stringp node-id))
    (let* ((rel-tags (mapcar (lambda (rel) (plist-get rel :to))
                             (supertag-relation-find-by-from node-id :node-tag)))
           (node (supertag-view-api-get-entity :nodes node-id))
           (node-tags (when node (plist-get node :tags)))
           (merged (cl-delete-duplicates (append rel-tags (or node-tags '()))
                                         :test #'equal)))
      (cl-remove-if-not #'stringp merged))))

(defun supertag-view-build-node-state (node-id)
  "Build a reusable view state plist for NODE-ID.

The returned plist is data-only (no buffer operations) and is intended
to be consumed by different UI layouts (node detail view, table-based
detail panes, previews, etc.).

Returned keys (current contract):
- :id           — NODE-ID
- :node         — Raw node plist from `supertag-node-get'
- :tags         — List of tag IDs attached to this node
- :fields       — List of field descriptors for this node:
                  each element is a plist
                  (:tag-id TAG-ID :field-def FIELD-DEF :value VALUE)
- :refs-to      — List of node IDs this node references (:reference)
- :refs-from    — List of node IDs that reference this node (:reference)
- :field-count  — Total number of fields across all tags
- :ref-count    — Total number of references (:refs-to + :refs-from)"
  (when (and node-id (stringp node-id))
    (let* ((node (supertag-view-api-get-entity :nodes node-id)))
      (when node
        (let* ((tag-ids (supertag-view--resolve-node-tags node-id))
               (fields '()))
          ;; Collect all fields for each tag on this node.
          (dolist (tag-id tag-ids)
            (let ((tag-fields (ignore-errors (supertag-tag-get-all-fields tag-id))))
              (dolist (field-def tag-fields)
                (let* ((fname (plist-get field-def :name))
                       (value (and fname
                                   (supertag-view-api-node-field-in-tag node-id tag-id fname))))
                  (push (list :tag-id tag-id
                              :field-def field-def
                              :value value)
                        fields)))))
          (setq fields (nreverse fields))
          ;; Compute reference info from :reference relations.
          (let* ((refs-to-rels   (supertag-relation-find-by-from node-id :reference))
                 (refs-to        (mapcar (lambda (rel) (plist-get rel :to)) refs-to-rels))
                 (all-relations  (supertag-view-api-get-collection :relations))
                 (refs-from '()))
            (when (hash-table-p all-relations)
              (maphash
               (lambda (_ rel-data)
                 (let* ((rel (if (hash-table-p rel-data)
                                 (let (plist)
                                   (maphash (lambda (k v)
                                              (setq plist (plist-put plist k v)))
                                            rel-data)
                                   plist)
                               rel-data)))
                   (when (and (eq (plist-get rel :type) :reference)
                              (equal (plist-get rel :to) node-id))
                     (push (plist-get rel :from) refs-from))))
               all-relations))
            (setq refs-from (nreverse refs-from))
            ;; Build final state plist.
            (list :id node-id
                  :node node
                  :tags tag-ids
                  :fields fields
                  :refs-to refs-to
                  :refs-from refs-from
                  :field-count (length fields)
                  :ref-count (+ (length refs-to) (length refs-from)))))))))

(defvar supertag-ui--node-cache nil
  "Cache for node selection candidates to improve performance.")

(defvar supertag-ui--cache-timestamp nil
  "Timestamp of when the node cache was last updated.")

(defvar supertag-ui--select-node-candidates nil
  "A list of node candidates used by `supertag-ui-select-node`.
For completion framework integration, e.g., live previews.")

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
  (let* ((nodes-hash (supertag-store-get-collection :nodes))
         (candidates '()))
    (maphash
     (lambda (id node-data)
       (when node-data
         (let ((display (supertag-ui--format-node-display node-data)))
           (push (cons display id) candidates))))
     nodes-hash)
    (sort candidates (lambda (a b) (string< (car a) (car b))))))

(defun supertag-ui--format-node-display (node-data)
  "Return a human-readable display string for NODE-DATA."
  (let* ((raw-title (or (plist-get node-data :raw-value)
                        (plist-get node-data :title)
                        "Untitled"))
         (olp (plist-get node-data :olp))
         (file (plist-get node-data :file))
         (display-path (if (and (listp olp) (> (length olp) 1))
                           ;; Node has parents, show "Parent / Child"
                           (concat (string-join (butlast olp) " / ") " / " raw-title)
                         ;; Node is top-level, just show its title
                         raw-title)))
    (if file
        (format "%s  (in %s)" display-path (file-name-nondirectory file))
      (format "%s  [orphaned]" display-path))))

(defun supertag-ui-select-node (&optional prompt use-cache with-preview)
  "Interactively prompt user to select a node.
PROMPT is the prompt string (defaults to 'Select node: ').
USE-CACHE when non-nil uses cached data for better performance.
WITH-PREVIEW when non-nil enables live preview in another window
if a supported completion framework (Ivy, Vertico) is active.
Returns the selected node's ID, or nil."
  (let* ((prompt-str (or prompt "Select node: "))
         (candidates (if use-cache
                         (supertag-ui--get-cached-nodes)
                       (supertag-ui--build-node-candidates)))
         (supertag-ui--select-node-candidates candidates)) ; For external hooks
    (if (not (and with-preview candidates))
        (let ((selected (completing-read prompt-str candidates nil t)))
          (when selected (cdr (assoc selected candidates))))
      (let ((preview-func (lambda (id) (when id (supertag-goto-node id t)))))
        (cond
         ((and (bound-and-true-p ivy-mode) (fboundp 'ivy-read))
          (let* ((ivy-update-fn
                  (lambda (_)
                    (let* ((sel (ivy-current-match))
                           (id (cdr (assoc sel candidates))))
                      (funcall preview-func id))))
                 (selection (ivy-read prompt-str (mapcar #'car candidates)
                                      :require-match t
                                      :history 'supertag-ui-select-node-history
                                      :caller 'supertag-ui-select-node)))
            (when selection (cdr (assoc selection candidates)))))
         ((bound-and-true-p vertico-mode)
          (let ((selection nil)
                (preview-hook
                 (lambda ()
                   (let* ((sel (vertico-current-candidate))
                          (id (cdr (assoc sel candidates))))
                     (funcall preview-func id)))))
            (unwind-protect
                (progn
                  (add-hook 'vertico-selection-hook preview-hook)
                  (setq selection (completing-read prompt-str candidates nil t)))
              (remove-hook 'vertico-selection-hook preview-hook))
            (when selection (cdr (assoc selection candidates)))))
          (t
           (message "Live preview supported with Ivy or Vertico. Falling back to default.")
           (let ((selected (completing-read prompt-str candidates nil t)))
             (when selected (cdr (assoc selected candidates))))))))))

(defun supertag-ui-select-multiple-nodes (&optional prompt use-cache initial with-preview)
  "Interactively select zero or more nodes and return their IDs.
PROMPT customizes the base prompt, USE-CACHE mirrors `supertag-ui-select-node'
for candidate retrieval, INITIAL is a pre-selected list (or single ID),
and WITH-PREVIEW is reserved for future live preview support."
  (ignore with-preview)
  (let* ((prompt-base (or prompt "Select nodes"))
         (candidates (if use-cache
                         (supertag-ui--get-cached-nodes)
                       (supertag-ui--build-node-candidates)))
         (id->display (let ((table (make-hash-table :test 'equal)))
                        (dolist (pair candidates table)
                          (puthash (cdr pair) (car pair) table))
                        table))
         (selection (copy-sequence (supertag-field-normalize-node-reference-list initial))))
    (cl-labels ((format-id (id)
                           (or (gethash id id->display) id))
                (refresh-candidates ()
                  (when use-cache
                    (supertag-ui--clear-node-cache))
                  (setq candidates (if use-cache
                                       (supertag-ui--get-cached-nodes)
                                     (supertag-ui--build-node-candidates)))
                  (setq id->display (let ((table (make-hash-table :test 'equal)))
                                      (dolist (pair candidates table)
                                        (puthash (cdr pair) (car pair) table))
                                      table)))
                (selection-summary ()
                                    (if selection
                                        (string-join (mapcar #'format-id selection) ", ")
                                      "none")))
      (catch 'done
        (while t
          (let* ((actions (append '("Add node..." "Create node...")
                                  (when selection '("Remove node..."))
                                  '("Done")))
                 (action (completing-read
                          (format "%s [%s]" prompt-base (selection-summary))
                          actions nil t nil nil "Done")))
            (pcase action
              ("Done"
               (throw 'done (copy-sequence selection)))
              ("Add node..."
               (let* ((available (cl-remove-if
                                  (lambda (pair) (member (cdr pair) selection))
                                  candidates)))
                 (if (null available)
                     (message "All nodes are already selected")
                   (let* ((choice (completing-read "Add node: "
                                                   (mapcar #'car available) nil t))
                          (match (assoc choice candidates)))
                     (when match
                       (let ((node-id (cdr match)))
                         (unless (member node-id selection)
                           (setq selection (append selection (list node-id))))))))))
              ("Create node..."
               (condition-case err
                   (let ((new-node-id (supertag-node-reference-and-create)))
                     (when new-node-id
                       (refresh-candidates)
                       (let* ((node-data (supertag-node-get new-node-id))
                              (display (and node-data (supertag-ui--format-node-display node-data))))
                         (when display
                           (puthash new-node-id display id->display)))
                       (unless (member new-node-id selection)
                         (setq selection (append selection (list new-node-id))))))
                 (quit (signal 'quit nil))
                 (error (message "%s" (error-message-string err)))))
              ("Remove node..."
               (if (null selection)
                   (message "No nodes to remove")
                 (let* ((choices (mapcar (lambda (id)
                                            (cons (format-id id) id))
                                          selection))
                        (choice (completing-read "Remove node: "
                                                 (mapcar #'car choices) nil t))
                        (match (assoc choice choices)))
                   (when match
                     (setq selection (delete (cdr match) selection))))))
              (_ (user-error "Unsupported action: %s" action)))))))))

(defun supertag-node-reference-and-create ()
  "Create a new node for use in node-reference fields and return its ID.
Prompts for a title, destination file, and insert position."
  (interactive)
  (let* ((title-input (read-string "New node title: "))
         (title (string-trim title-input)))
    (when (string-empty-p title)
      (user-error "Node title cannot be empty"))
    (let* ((target-file (read-file-name "Store node in file: " nil nil t))
           (insert-info (supertag-ui-select-insert-position target-file)))
      (unless insert-info
        (user-error "No valid insert position selected"))
      (let* ((insert-pos (plist-get insert-info :position))
             (insert-level (max 1 (or (plist-get insert-info :level) 1)))
             (node-id (org-id-new)))
        (with-current-buffer (find-file-noselect target-file)
          (org-with-wide-buffer
           (goto-char insert-pos)
           (unless (bolp)
             (insert "\n"))
           (let ((heading-start (point)))
             (insert (format "%s %s\n:PROPERTIES:\n:ID:       %s\n:END:\n\n"
                             (make-string insert-level ?*)
                             title
                             node-id))
             (goto-char heading-start)
             (supertag-node-sync-at-point))
           (save-buffer)))
        (supertag-ui--clear-node-cache)
        (message "Node '%s' created." title)
        node-id))))


(defun supertag-ui-select-reference-to-remove (from-node-id)
  "Interactively select a reference to remove from a given node.
FROM-NODE-ID is the ID of the node whose references are to be listed.
Returns the ID of the selected node to unlink."
  (let* ((source-node (supertag-node-get from-node-id))
         (ref-to-ids (plist-get source-node :ref-to)))
    (if (not ref-to-ids)
        (progn (message "Node has no outgoing references.") nil)
      (let* ((candidates
              (mapcar (lambda (node-id)
                        (let* ((node-data (supertag-node-get node-id))
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
      (:node-reference
       (let* ((initial (supertag-field-normalize-node-reference-list current-value))
              (selected (supertag-ui-select-multiple-nodes
                         (format "Edit links for %s" field-name)
                         t
                         initial))
              (packed (supertag-field-pack-node-reference-value selected)))
         packed))
      (:date (supertag-field-read-date-value prompt))
      (:timestamp (supertag-field-read-timestamp-value prompt))
      (:boolean (if (y-or-n-p (format "%s: " (replace-regexp-in-string ": $" "" prompt))) "true" "false"))
      (:integer (read-string prompt (if current-value (format "%s" current-value) "")))
      (:number (read-string prompt (if current-value (format "%s" current-value) "")))
      (_ (read-string prompt current-value)))))

(defun supertag-ui--sanitize-type-input (type-str)
  "Return a keyword type from TYPE-STR, stripping leading colons/whitespace."
  (when (and type-str (not (string-empty-p type-str)))
    (let* ((clean (string-trim type-str)))
      (when (string-prefix-p ":" clean)
        (setq clean (substring clean 1)))
      (when (not (string-empty-p clean))
        (intern (concat ":" clean))))))

(defun supertag-ui-create-field-definition ()
  "Interactively create a new field definition.
Returns a field definition plist, or nil if cancelled."
  (let* ((name (read-string "Field name: ")))
    (when (and name (not (string-empty-p name)))
      (let* ((type-keywords (mapcar (lambda (sym) (substring (symbol-name sym) 1)) supertag-field-types))
             (type-str (completing-read "Field type: " type-keywords nil t))
             (type (supertag-ui--sanitize-type-input type-str))
             (options nil))
        (when type
          (when (eq type :options)
            (let* ((options-input (read-string "Options (comma separated): "))
                   (options-list (split-string options-input "," t "[ \t\n\r]+")))
              (setq options options-list)))
          (let ((default (read-string "Default value (optional): " "")))
            (let ((field-def (list :name name :type type)))
              (unless (string-empty-p default)
                (setq field-def (plist-put field-def :default default)))
              (when (eq type :options)
                (setq field-def (plist-put field-def :options options)))
              field-def)))))))

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

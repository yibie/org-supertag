;;; supertag-concept.el --- Concept mentions for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; CJK-friendly concept mentions:
;; - promote selected text into a concept node
;; - create one explicit reference from the current node to that concept
;; - render other exact title/alias occurrences as dynamic mentions

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'subr-x)
(require 'supertag-core-store)
(require 'supertag-ops-node)
(require 'supertag-ops-relation)
(require 'supertag-services-ui)
(require 'supertag-ui-commands)
(require 'supertag-view-helper)

(defgroup supertag-concept nil
  "Concept mention support for Org-Supertag."
  :group 'org-supertag)

(defcustom supertag-concept-min-term-length 2
  "Minimum character length for a concept title or alias mention."
  :type 'integer
  :group 'supertag-concept)

(defcustom supertag-concept-alias-separator-regexp "[,，;；]"
  "Regexp used to split concept aliases stored in SUPERTAG_ALIASES."
  :type 'regexp
  :group 'supertag-concept)

(defconst supertag-concept--marker-property :SUPERTAG_CONCEPT)
(defconst supertag-concept--aliases-property :SUPERTAG_ALIASES)
(defconst supertag-concept--org-marker-property "SUPERTAG_CONCEPT")
(defconst supertag-concept--org-aliases-property "SUPERTAG_ALIASES")

(defface supertag-concept-mention-face
  '((((class color) (background light))
     :foreground "#4A3100"
     :background "#FFF3B0")
    (((class color) (background dark))
     :foreground "#FFE6A3"
     :background "#3A2F0B")
    (t
     :weight bold))
  "Face for dynamic concept mentions.
This intentionally does not inherit from `org-link'."
  :group 'supertag-concept)

(defvar-local supertag-concept--entries nil
  "Buffer-local concept entries used by font-lock.
Each entry is (TERM . NODE-ID).")

(defvar-local supertag-concept--font-lock-keywords nil
  "Buffer-local font-lock keywords for concept mentions.")

(defvar supertag-concept-mention-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supertag-concept-open-at-point)
    (define-key map [mouse-1] #'supertag-concept-open-at-mouse)
    map)
  "Keymap used on concept mention text.")

(defun supertag-concept--node-prop (node prop)
  "Return PROP from NODE, accepting plist or hash-table NODE data."
  (cond
   ((hash-table-p node) (gethash prop node))
   ((listp node) (plist-get node prop))
   (t nil)))

(defun supertag-concept--node-properties (node)
  "Return NODE user properties as a plist."
  (let ((props (supertag-concept--node-prop node :properties)))
    (cond
     ((hash-table-p props)
      (let (plist)
        (maphash (lambda (k v) (setq plist (plist-put plist k v))) props)
        plist))
     ((listp props) props)
     (t nil))))

(defun supertag-concept--truthy-p (value)
  "Return non-nil when VALUE marks a node as a concept."
  (member (downcase (format "%s" value)) '("t" "true" "yes" "1" "concept")))

(defun supertag-concept-node-p (node)
  "Return non-nil when NODE is marked as a concept node."
  (or (supertag-concept--truthy-p (supertag-concept--node-prop node :concept))
      (supertag-concept--truthy-p
       (plist-get (supertag-concept--node-properties node)
                  supertag-concept--marker-property))))

(defun supertag-concept--split-aliases (value)
  "Split alias VALUE into a clean alias list."
  (cond
   ((null value) nil)
   ((listp value)
    (cl-remove-if #'string-empty-p
                  (mapcar (lambda (v) (string-trim (format "%s" v))) value)))
   ((stringp value)
    (cl-remove-if #'string-empty-p
                  (mapcar #'string-trim
                          (split-string value supertag-concept-alias-separator-regexp t))))
   (t nil)))

(defun supertag-concept-node-aliases (node)
  "Return aliases for concept NODE."
  (supertag-concept--split-aliases
   (plist-get (supertag-concept--node-properties node)
              supertag-concept--aliases-property)))

(defun supertag-concept--valid-term-p (term)
  "Return non-nil when TERM is worth matching as a mention."
  (and (stringp term)
       (not (string-empty-p (string-trim term)))
       (>= (length (string-trim term)) supertag-concept-min-term-length)))

(defun supertag-concept-entries ()
  "Return all known concept mention entries as (TERM . NODE-ID), longest first."
  (let ((nodes (supertag-store-get-collection :nodes))
        (seen (make-hash-table :test 'equal))
        entries)
    (when (hash-table-p nodes)
      (maphash
       (lambda (id node)
         (when (supertag-concept-node-p node)
           (dolist (term (cons (or (supertag-concept--node-prop node :title)
                                   (supertag-concept--node-prop node :raw-value))
                               (supertag-concept-node-aliases node)))
             (let ((clean (and term (string-trim (format "%s" term)))))
               (when (and (supertag-concept--valid-term-p clean)
                          (not (gethash clean seen)))
                 (puthash clean t seen)
                 (push (cons clean id) entries))))))
       nodes))
    (sort entries
          (lambda (a b)
            (> (length (car a)) (length (car b)))))))

(defun supertag-concept--regexp (entries)
  "Build a longest-first exact phrase regexp from ENTRIES."
  (when entries
    (concat "\\(?:" (mapconcat (lambda (entry) (regexp-quote (car entry)))
                               entries "\\|")
            "\\)")))

(defun supertag-concept--inside-org-link-p ()
  "Return non-nil when the current match is inside an Org link."
  (when (and (derived-mode-p 'org-mode) (fboundp 'org-element-context))
    (let ((pos (match-beginning 0)))
      (save-excursion
        (goto-char pos)
        (let ((element (org-element-context)))
          (and (eq (org-element-type element) 'link)
               (<= (org-element-property :begin element) pos)
               (< pos (org-element-property :end element))))))))

(defun supertag-concept--valid-match-p ()
  "Return non-nil when the current concept match should be rendered."
  (let ((pos (max (point-min) (or (match-beginning 0) (point-min)))))
    (and (derived-mode-p 'org-mode)
         (not (supertag-concept--inside-org-link-p))
         (not (supertag-view-helper--in-src-block-p pos))
         (not (supertag-view-helper--at-table-p pos))
         (not (supertag-view-helper--at-commented-p pos))
         (not (eq (get-text-property pos 'face) 'org-verbatim)))))

(defun supertag-concept--match-handler ()
  "Font-lock handler for concept mention matches."
  (let ((start (match-beginning 0))
        (end (match-end 0)))
    (when (and start end
               (<= (point-min) start)
               (<= end (point-max))
               (save-match-data
                 (supertag-concept--valid-match-p)))
      (let* ((term (buffer-substring-no-properties start end))
             (node-id (cdr (assoc term supertag-concept--entries))))
        (when node-id
          (add-text-properties
           start end
           `(supertag-concept-node-id ,node-id
             mouse-face highlight
             help-echo ,(format "Mention: %s -> RET jump" term)
             keymap ,supertag-concept-mention-map))
          'supertag-concept-mention-face)))))

(defun supertag-concept--refresh-font-lock-keywords ()
  "Rebuild concept font-lock keywords in the current buffer."
  (when supertag-concept--font-lock-keywords
    (font-lock-remove-keywords nil supertag-concept--font-lock-keywords))
  (setq supertag-concept--entries (supertag-concept-entries))
  (let ((regexp (supertag-concept--regexp supertag-concept--entries)))
    (setq supertag-concept--font-lock-keywords
          (when regexp
            `((,regexp (0 (supertag-concept--match-handler) t)))))
    (when supertag-concept--font-lock-keywords
      (font-lock-add-keywords nil supertag-concept--font-lock-keywords t))))

;;;###autoload
(define-minor-mode supertag-concept-link-mode
  "Highlight known concept titles and aliases as dynamic mentions."
  :lighter " ST-Concept"
  :group 'supertag-concept
  (if supertag-concept-link-mode
      (progn
        (make-local-variable 'font-lock-extra-managed-props)
        (dolist (prop '(keymap help-echo mouse-face supertag-concept-node-id))
          (cl-pushnew prop font-lock-extra-managed-props))
        (supertag-concept--refresh-font-lock-keywords)
        (supertag-view-helper--refresh-fontification))
    (when supertag-concept--font-lock-keywords
      (font-lock-remove-keywords nil supertag-concept--font-lock-keywords))
    (setq supertag-concept--font-lock-keywords nil
          supertag-concept--entries nil)
    (supertag-view-helper--refresh-fontification)))

;;;###autoload
(defun supertag-concept-refresh ()
  "Refresh concept mentions in the current buffer."
  (interactive)
  (when supertag-concept-link-mode
    (supertag-concept--refresh-font-lock-keywords)
    (supertag-view-helper--refresh-fontification)))

(defun supertag-concept--refresh-all-buffers ()
  "Refresh concept mention highlighting in all enabled buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when supertag-concept-link-mode
        (supertag-concept-refresh)))))

(defun supertag-concept--node-id-at-point ()
  "Return concept node id at point, checking point and previous char."
  (or (get-text-property (point) 'supertag-concept-node-id)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'supertag-concept-node-id))))

;;;###autoload
(defun supertag-concept-open-at-point ()
  "Open the concept mention at point."
  (interactive)
  (let ((node-id (supertag-concept--node-id-at-point)))
    (unless node-id
      (user-error "No concept mention at point"))
    (supertag-goto-node node-id)))

;;;###autoload
(defun supertag-concept-open-at-mouse (event)
  "Open the concept mention clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (supertag-concept-open-at-point))

(defun supertag-concept--find-concept-id-by-term (term)
  "Return concept node id whose title or alias is TERM."
  (cdr (assoc term (supertag-concept-entries))))

(defun supertag-concept--find-node-id-by-title (title)
  "Return the first node id whose title or raw value exactly equals TITLE."
  (let ((nodes (supertag-store-get-collection :nodes))
        found)
    (when (hash-table-p nodes)
      (maphash
       (lambda (id node)
         (when (and (not found)
                    (member title
                            (delq nil (list (supertag-concept--node-prop node :title)
                                            (supertag-concept--node-prop node :raw-value)))))
           (setq found id)))
       nodes))
    found))

(defun supertag-concept--put-node-property (node key value)
  "Return NODE with user property KEY set to VALUE."
  (let* ((copy (copy-sequence node))
         (props (copy-sequence (or (plist-get copy :properties) '()))))
    (plist-put copy :properties (plist-put props key value))))

(defun supertag-concept--mark-node (node-id)
  "Mark NODE-ID as a concept node in the store and, when possible, its Org file."
  (supertag-node-update
   node-id
   (lambda (node)
     (when node
       (supertag-concept--put-node-property
        node supertag-concept--marker-property "t"))))
  (when-let* ((marker (supertag-ui--find-node-marker node-id)))
    (with-current-buffer (marker-buffer marker)
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (save-excursion
        (goto-char marker)
        (when (org-at-heading-p)
          (org-entry-put nil supertag-concept--org-marker-property "t")
          (save-buffer)))))
  node-id)

(defun supertag-concept--create-node (title)
  "Create a new concept node titled TITLE and return its node id."
  (let* ((target-file (read-file-name "Create concept in file: " nil nil t))
         (insert-info (when (and target-file (file-exists-p target-file))
                        (supertag-ui-select-insert-position target-file)))
         (insert-pos (plist-get insert-info :position))
         (insert-level (plist-get insert-info :level))
         (node-id (org-id-new)))
    (unless insert-info
      (user-error "No valid insert position selected"))
    (with-current-buffer (find-file-noselect target-file)
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (goto-char insert-pos)
      (unless (or (bobp) (looking-back "\n" 1))
        (insert "\n"))
      (insert (format "%s %s\n:PROPERTIES:\n:ID:       %s\n:%s: t\n:END:\n"
                      (make-string insert-level ?*) title node-id
                      supertag-concept--org-marker-property))
      (org-id-add-location node-id target-file)
      (save-buffer))
    (supertag-node-create
     (list :id node-id
           :title title
           :file target-file
           :position insert-pos
           :level insert-level
           :properties (list supertag-concept--marker-property "t")))
    node-id))

(defun supertag-concept--ensure-node (title)
  "Return a concept node id for TITLE, creating or marking one if needed."
  (or (supertag-concept--find-concept-id-by-term title)
      (when-let* ((existing (supertag-concept--find-node-id-by-title title)))
        (supertag-concept--mark-node existing))
      (supertag-concept--create-node title)))

;;;###autoload
(defun supertag-promote-concept (beg end)
  "Promote selected text from BEG to END into a concept mention.
Creates or reuses a concept node, creates one reference from the containing node
to that concept, and leaves the selected text unchanged."
  (interactive
   (unless (use-region-p)
     (user-error "Select text to promote as a concept"))
   (list (region-beginning) (region-end)))
  (unless (and beg end (< beg end))
    (user-error "Select text to promote as a concept"))
  (unless (derived-mode-p 'org-mode)
    (user-error "Concept promotion only works in Org buffers"))
  (let* ((title (string-trim
                 (replace-regexp-in-string
                  "[ \t\n\r]+" " "
                  (buffer-substring-no-properties beg end))))
         (from-id (save-excursion
                    (goto-char beg)
                    (supertag-ui--get-containing-node-at-point))))
    (when (string-empty-p title)
      (user-error "Selected text is empty"))
    (supertag-ui--ensure-node-synced from-id)
    (let ((concept-id (supertag-concept--ensure-node title)))
      (unless (equal from-id concept-id)
        (unless (supertag-relation-add-reference from-id concept-id)
          (let* ((err (and (fboundp 'supertag-relation-last-error)
                           (supertag-relation-last-error)))
                 (msg (or (plist-get err :message)
                          "Failed to add concept reference")))
            (user-error "%s" msg))))
      (supertag-concept--refresh-all-buffers)
      (message "Promoted concept mention: %s" title)
      concept-id)))

(provide 'supertag-concept)
;;; supertag-concept.el ends here

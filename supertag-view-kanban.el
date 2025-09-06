;;; supertag-view-kanban.el --- Kanban board UI for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides a Kanban-style board view for the Org-Supertag
;; data-centric architecture. It follows the principle that UI components
;; should be stateless and only handle rendering and user interaction,
;; while data operations are delegated to dedicated service modules.

;;; Code:

(require 'cl-lib)
(require 'supertag-core-store)
(require 'supertag-services-query)
(require 'supertag-ops-node)
(require 'supertag-ops-field)
(require 'supertag-ops-tag)
(require 'supertag-core-notify)

;;; --- State Management ---

(defvar-local supertag-view-kanban--config nil
  "Configuration for the current Kanban view.
Contains :base-tag, :group-field, and :columns.")

(defvar-local supertag-view-kanban--grouped-nodes nil
  "Hash table of nodes grouped by field value for the current view.")

(defvar-local supertag-view-kanban--column-values nil
  "The ordered list of column values for the current view.")

(defvar-local supertag-view-kanban--subscription-id nil
  "ID of the subscription for reactive updates.")

;;; --- Configuration ---

(defun supertag-view-kanban-create-config (base-tag group-field &optional column-order)
  "Create a Kanban configuration.
BASE-TAG is the tag to filter nodes.
GROUP-FIELD is the field name to group nodes by.
COLUMN-ORDER is an optional list of column values in specific order."
  (list :base-tag base-tag
        :group-field group-field
        :columns column-order))

(defun supertag-view-kanban--get-all-tags ()
  "Get list of all tag names."
  (let ((tags (supertag-get (list :tags)))
        (tag-names '()))
    (when (hash-table-p tags)
      (maphash (lambda (id tag-data)
                 (when (plist-get tag-data :name)
                   (push (plist-get tag-data :name) tag-names)))
               tags))
    (sort tag-names #'string<)))

;;; --- Data Querying ---

(defun supertag-view-kanban--query-nodes (base-tag)
  "Query all nodes with BASE-TAG using the optimized index."
  (supertag-find-nodes-by-tag base-tag))

(defun supertag-view-kanban--group-nodes-by-field (nodes base-tag group-field)
  "Group NODES by the value of GROUP-FIELD.
Returns hash table where keys are field values and values are lists of (id . node-data)."
  (let ((grouped (make-hash-table :test 'equal)))
    (dolist (node-pair nodes)
      (let* ((node-id (car node-pair))
             (field-value (supertag-field-get node-id base-tag group-field))
             (key (or field-value "Uncategorized")))
        (push node-pair (gethash key grouped '()))))
    grouped))

(defun supertag-view-kanban--get-column-values (grouped-nodes config)
  "Get column values in the correct order."
  (let* ((base-tag (plist-get config :base-tag))
         (group-field (plist-get config :group-field))
         (all-fields (supertag-tag-get-all-fields base-tag))
         (field-def (cl-find-if (lambda (f) (string= (plist-get f :name) group-field)) all-fields))
         (predefined-options (if (and field-def (eq (plist-get field-def :type) :options))
                                 (plist-get field-def :options)
                               nil))
         (actual-values (hash-table-keys grouped-nodes)))
    (if predefined-options
        ;; Use predefined options as the base order, and append any other
        ;; values that are in use but not in the options list.
        (append predefined-options (cl-set-difference actual-values predefined-options :test #'string=))
      ;; Otherwise, fall back to existing behavior.
      (sort actual-values #'string<))))

;;; --- Rendering Engine (Old Style) ---

(defun supertag-view-kanban--pad-string (str width &optional align)
  "Pad STR to WIDTH with spaces.
ALIGN can be 'left, 'right, or 'center."
  (let* ((len (string-width str))
         (diff (- width len)))
    (if (<= diff 0)
        str
      (pcase align
        ('right (concat (make-string diff ?\ ) str))
        ('center (let* ((left-pad (floor (/ diff 2.0)))
                       (right-pad (ceiling (/ diff 2.0))))
                  (concat (make-string left-pad ?\ ) str (make-string right-pad ?\ ))))
        (_ ; 'left is default
         (concat str (make-string diff ?\ )))))))

(defun supertag-view-kanban--wrap-text (text width)
  "Wrap TEXT to a list of strings, each no wider than WIDTH.
This is a self-contained implementation."
  (let ((words (split-string text "\\s-+" t))
        (lines '())
        (current-line ""))
    (dolist (word words)
      (if (string-empty-p current-line)
          (setq current-line word)
        (if (<= (string-width (concat current-line " " word)) width)
            (setq current-line (concat current-line " " word))
          (push current-line lines)
          (setq current-line word))))
    (when (not (string-empty-p current-line))
      (push current-line lines))
    (nreverse lines)))

(defun supertag-view-kanban--format-card (node-pair width config)
  "Format a NODE-PAIR into a bordered card of fixed WIDTH.
Returns the card as a list of strings, each correctly padded."
  (let* ((node-id (car node-pair))
         (node-data (cdr node-pair))
         (title (or (plist-get node-data :title) "No Title"))
         (inner-width (- width 4)) ; For "│ text │"
         (wrapped-lines (supertag-view-kanban--wrap-text title inner-width))
         (card-lines '()))
    ;; Top border
    (push (format "┌%s┐" (make-string (- width 2) ?─)) card-lines)
    ;; Content lines
    (dolist (line wrapped-lines)
      (push (format "│ %s │" (supertag-view-kanban--pad-string line inner-width 'left)) card-lines))
    ;; Bottom border
    (push (format "└%s┘" (make-string (- width 2) ?─)) card-lines)
    ;; Propertize all lines and return
    (let ((final-lines (nreverse card-lines)))
      (mapcar (lambda (line)
                (propertize line
                            'node-id node-id
                            'base-tag (plist-get config :base-tag)
                            'group-field (plist-get config :group-field)))
              final-lines))))

(defun supertag-view-kanban-render (config &optional node-to-focus)
  "Render the Kanban board based on CONFIG using the old visual style."
  (let* ((base-tag (plist-get config :base-tag))
         (group-field (plist-get config :group-field))
         (nodes (supertag-view-kanban--query-nodes base-tag))
         (grouped-nodes (supertag-view-kanban--group-nodes-by-field nodes base-tag group-field)))

    (setq-local supertag-view-kanban--column-values (supertag-view-kanban--get-column-values grouped-nodes config))
    (let* ((column-values supertag-view-kanban--column-values)
           (column-width 40)
           (separator "  ")
           (inhibit-read-only t))

      ;; Store state for interactive operations
      (setq-local supertag-view-kanban--config config)
      (setq-local supertag-view-kanban--grouped-nodes grouped-nodes)

      (erase-buffer)

      ;; Header
      (insert (propertize (format "Kanban Board: %s / %s\n\n" base-tag group-field)
                          'face '(:height 1.5 :weight bold)))
      (insert (propertize "Operations:\n" 'face '(:weight bold)))
      (insert " [b/f] Move Card  [n/p] Navigate  [g] Refresh  [q] Quit\n\n")

      (if (zerop (length nodes))
          (insert "\n  No nodes found for this tag.\n")
        (progn
          ;; 1. Insert column headers
          (dotimes (i (length column-values))
            (let* ((header-text (format "%s (%d)" (nth i column-values) (length (gethash (nth i column-values) grouped-nodes))))
                   (padded-header (supertag-view-kanban--pad-string header-text column-width 'center)))
              (insert padded-header)
              (when (< i (1- (length column-values))) (insert separator))))
          (insert "\n")
          (dotimes (i (length column-values))
            (insert (make-string column-width ?─))
            (when (< i (1- (length column-values))) (insert separator)))
          (insert "\n\n")

          ;; 2. Pre-render each column into a list of its lines.
          (let* ((rendered-columns
                  (mapcar
                   (lambda (col-value)
                     (let ((lines '())
                           (nodes-in-col (gethash col-value grouped-nodes)))
                       (dolist (node-pair nodes-in-col)
                         (setq lines (append lines (supertag-view-kanban--format-card node-pair column-width config))))
                       lines))
                   column-values))
                 (max-height (apply #'max 0 (mapcar #'length rendered-columns))))

            ;; 3. Print the board row by row, ensuring alignment.
            (dotimes (line-idx max-height)
              (dotimes (col-idx (length column-values))
                (let* ((col-lines (nth col-idx rendered-columns))
                       (line-to-insert (or (nth line-idx col-lines) (make-string column-width ?\ )))
                       (group-value (nth col-idx column-values)))
                  (let ((final-line (copy-sequence line-to-insert)))
                    ;; Restore the crucial logic to apply the group-value property.
                    (add-text-properties 0 (length final-line) `(group-value ,group-value) final-line)
                    (insert final-line)))
                (when (< col-idx (1- (length column-values))) (insert separator)))
              (insert "\n"))))))

    (setq buffer-read-only t)
    ;; After rendering, position the cursor.
    (if node-to-focus
        (let ((found-pos nil))
          (goto-char (point-min))
          (while (and (not found-pos) (re-search-forward "┌" nil t))
            (when (equal (get-text-property (point) 'node-id) node-to-focus)
              (setq found-pos (point))))
          (when found-pos (goto-char found-pos)))
      ;; If no specific node, go to the first card if any exist.
      (goto-char (point-min))
      (when (re-search-forward "┌" nil t)
        (goto-char (match-beginning 0))))
    (message "Kanban board rendered for tag '%s'" (plist-get supertag-view-kanban--config :base-tag))))

;;; --- Interactive Operations ---

(defun supertag-view-kanban--get-card-info ()
  "Get information about the card at point.
Returns plist with :node-id, :current-value, and other card info."
  (save-excursion
    (let (node-id current-value)
      ;; Search backwards from point for the beginning of a card
      (when (re-search-backward "┌" nil t)
        ;; Move forward one char to be inside the card's properties
        (forward-char 1)
        (setq node-id (get-text-property (point) 'node-id))
        (setq current-value (get-text-property (point) 'group-value)))

      (when node-id
        (list :node-id node-id
              :current-value current-value
              :base-tag (plist-get supertag-view-kanban--config :base-tag)
              :group-field (plist-get supertag-view-kanban--config :group-field)))))) 

(defun supertag-view-kanban-move-card (direction)
  "Move the current card in DIRECTION (:left or :right)."
  (interactive)
  (let* ((info (supertag-view-kanban--get-card-info))
         (config supertag-view-kanban--config))
    
    (when info
      (let* ((node-id (plist-get info :node-id))
             (current-value (plist-get info :current-value))
             (base-tag (plist-get info :base-tag))
             (group-field (plist-get info :group-field))
             (column-values supertag-view-kanban--column-values)
             (current-idx (cl-position current-value column-values :test #'string=))
             (target-idx (when current-idx (+ current-idx (if (eq direction :left) -1 1)))))
        (if (and current-idx (>= target-idx 0) (< target-idx (length column-values)))
            (let ((new-value (nth target-idx column-values)))
              (supertag-field-set node-id base-tag group-field new-value)
              (supertag-view-kanban-refresh node-id)
              (message "Moved card to '%s'" new-value))
          (message "Cannot move further in that direction."))))))
          
(defun supertag-view-kanban-move-card-left ()
  "Move current card to the left column."
  (interactive)
  (supertag-view-kanban-move-card :left))

(defun supertag-view-kanban-move-card-right ()
  "Move current card to the right column."
  (interactive)
  (supertag-view-kanban-move-card :right))

(defun supertag-view-kanban-next-card ()
  "Move point to the next card."
  (interactive)
  (let ((start-point (point)))
    (re-search-forward "┌" nil t)
    ;; If search wraps around and finds the same spot, it means no other card found
    (when (eobp)
      (goto-char (point-min))
      (re-search-forward "┌" nil t))
    (when (eq (point) start-point)
      (message "No next card."))))

(defun supertag-view-kanban-previous-card ()
  "Move point to the previous card."
  (interactive)
  (re-search-backward "┌" nil t))

;;; --- Reactive Updates ---

(defun supertag-view-kanban--subscribe-updates ()
  "Subscribe to node update events for reactive Kanban updates."
  (setq-local supertag-view-kanban--subscription-id
              (supertag-subscribe :node-updated
                                 (lambda (path old-value new-value)
                                   (when (and (listp path) (eq (car path) :nodes))
                                     (supertag-view-kanban-refresh))))))

(defun supertag-view-kanban--unsubscribe-updates ()
  "Unsubscribe from update events."
  (when supertag-view-kanban--subscription-id
    (supertag-unsubscribe supertag-view-kanban--subscription-id)
    (setq-local supertag-view-kanban--subscription-id nil)))

;;; --- Main Interface ---

(defun supertag-view-kanban-refresh (&optional node-to-focus)
  "Refresh the Kanban view with current data."
  (interactive)
  (when supertag-view-kanban--config
    (supertag-view-kanban-render supertag-view-kanban--config node-to-focus)))

;;; --- Mode Definition ---

(defvar supertag-view-kanban-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'supertag-view-kanban-refresh)
    (define-key map (kbd "b") #'supertag-view-kanban-move-card-left)
    (define-key map (kbd "f") #'supertag-view-kanban-move-card-right)
    (define-key map (kbd "p") #'supertag-view-kanban-previous-card)
    (define-key map (kbd "n") #'supertag-view-kanban-next-card)
    map)
  "Keymap for supertag-view-kanban-mode.")

(define-derived-mode supertag-view-kanban-mode special-mode "Supertag-Kanban"
  "Major mode for Supertag Kanban board views."
  (setq buffer-read-only t))

(defun supertag-view-kanban-cleanup ()
  "Clean up Kanban view subscriptions."
  (interactive)
  (supertag-view-kanban--unsubscribe-updates))

(add-hook 'kill-buffer-hook #'supertag-view-kanban-cleanup)

(provide 'supertag-view-kanban)
;;; supertag-view-kanban.el ends here

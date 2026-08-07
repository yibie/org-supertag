;;; supertag-view-stream.el --- Read nodes as a tag stream -*- lexical-binding: t; -*-

;;; Commentary:

;; Stream View presents every node carrying a tag (or one of its transitive
;; `:extends' descendants) as a chronological title list.  The buffer is a
;; normal View Runtime instance rendered through the existing Widget DSL.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'time-date)
(require 'supertag-ops-node)
(require 'supertag-services-sync)
(require 'supertag-view-api)
(require 'supertag-view-framework)
(require 'supertag-view-node)

(defgroup supertag-view-stream nil
  "Chronological title views for tagged nodes."
  :group 'org-supertag)

(defface supertag-view-stream-title-face
  '((t :inherit org-level-2 :height 1.15 :weight semi-bold))
  "Face for node titles in Stream View."
  :group 'supertag-view-stream)

(defface supertag-view-stream-current-face
  '((((class color) (background light))
     :background "#F1F5F9" :extend t)
    (((class color) (background dark))
     :background "#334155" :extend t)
    (t :inherit region :extend t))
  "Background face for the selected Stream node."
  :group 'supertag-view-stream)

(defvar-local supertag-view-stream--origin-window-configuration nil
  "Window configuration to restore when the Stream quits.")

(defvar-local supertag-view-stream--selection-overlay nil
  "Selection overlay in a Stream buffer.")

(defvar-local supertag-view-stream-edit--return-buffer nil
  "Stream buffer to refresh after an indirect edit finishes.")

(defvar-local supertag-view-stream-edit--window-configuration nil
  "Window configuration to restore after an indirect edit.")

(defvar-local supertag-view-stream-edit--node-id nil
  "Node ID being edited in the current indirect buffer.")

(defvar supertag-view-stream-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "n") #'supertag-view-stream-next-node)
    (define-key map (kbd "p") #'supertag-view-stream-previous-node)
    (define-key map (kbd "e") #'supertag-view-stream-edit)
    (define-key map (kbd "v") #'supertag-view-stream-open-node-view)
    (define-key map (kbd "g") #'supertag-view-refresh)
    (define-key map (kbd "q") #'supertag-view-stream-quit)
    map)
  "Keymap for `supertag-view-stream-mode'.")

(define-derived-mode supertag-view-stream-mode org-mode "Supertag-Stream"
  "Major mode for browsing tagged nodes as a title stream."
  :keymap supertag-view-stream-mode-map
  (setq buffer-read-only t
        truncate-lines nil))

(defvar supertag-view-stream-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'supertag-view-stream-edit-finish)
    map)
  "Keymap for `supertag-view-stream-edit-mode'.")

(define-minor-mode supertag-view-stream-edit-mode
  "Minor mode used while editing a narrowed Stream node."
  :lighter " Stream-Edit"
  :keymap supertag-view-stream-edit-mode-map)

(defun supertag-view-stream--created-time (node)
  "Return NODE's creation value as an Emacs time, or nil."
  (let ((value (plist-get node :created-at)))
    (cond
     ((stringp value) (ignore-errors (date-to-time value)))
     ((or (listp value) (integerp value) (floatp value)) value))))

(defun supertag-view-stream--node-before-p (left right)
  "Return non-nil when LEFT should appear before RIGHT."
  (let ((left-time (supertag-view-stream--created-time left))
        (right-time (supertag-view-stream--created-time right))
        (left-id (or (plist-get left :id) ""))
        (right-id (or (plist-get right :id) "")))
    (cond
     ((and left-time right-time)
      (cond
       ((time-less-p left-time right-time) t)
       ((time-less-p right-time left-time) nil)
       (t (string< left-id right-id))))
     (left-time t)
     (right-time nil)
     (t (string< left-id right-id)))))

(defun supertag-view-stream--build-state (input)
  "Build data-only Stream state from Runtime INPUT."
  (let ((tag (plist-get input :tag)))
    (unless (and (stringp tag) (not (string-empty-p tag)))
      (user-error "Stream View requires a tag"))
    (let* ((node-ids (supertag-view-api-nodes-by-tag tag t))
           (nodes (supertag-view-api-get-entities :nodes node-ids)))
      (list :tag tag
            :nodes (cl-stable-sort (copy-sequence nodes)
                                   #'supertag-view-stream--node-before-p)))))

(defun supertag-view-stream--node-title (node)
  "Return the display title for NODE."
  (let ((title (or (plist-get node :title)
                   (plist-get node :raw-value))))
    (if (and (stringp title) (not (string-empty-p title)))
        title
      "Untitled")))

(defun supertag-view-stream--node-widget (node)
  "Return the Widget tree for NODE."
  (list :type :text
        :key (plist-get node :id)
        :content (propertize (supertag-view-stream--node-title node)
                             'font-lock-face
                             'supertag-view-stream-title-face)))

(defun supertag-view-stream--widgets (state)
  "Return the Stream Widget tree for STATE."
  (let ((nodes (plist-get state :nodes)))
    (if nodes
        (list (list :type :stack :spacing 0
                    :children (mapcar #'supertag-view-stream--node-widget
                                      nodes)))
      (list (list :type :text
                  :content (format "No nodes for #%s."
                                   (plist-get state :tag)))))))

(defun supertag-view-stream--add-entity-properties ()
  "Copy stable Widget keys to the shared entity ID property."
  (let ((position (point-min)))
    (while (< position (point-max))
      (let* ((key (get-text-property position 'supertag-widget-key))
             (end (or (next-single-property-change
                       position 'supertag-widget-key nil (point-max))
                      (point-max))))
        (when (stringp key)
          (put-text-property position end 'supertag-entity-id key))
        (setq position end)))))

(defun supertag-view-stream--render (state)
  "Render Stream STATE in the current Runtime buffer."
  (when (overlayp supertag-view-stream--selection-overlay)
    (delete-overlay supertag-view-stream--selection-overlay)
    (setq supertag-view-stream--selection-overlay nil))
  (supertag-view-widget--render-tree
   (supertag-view-stream--widgets state) state)
  (supertag-view-stream--add-entity-properties)
  (setq header-line-format
        (format " #%s   %d nodes "
                (plist-get state :tag)
                (length (plist-get state :nodes))))
  (font-lock-flush))

(defun supertag-view-stream--buffer-name (input)
  "Return a Stream buffer name for INPUT."
  (format "*Supertag Stream: %s*" (plist-get input :tag)))

(defun supertag-view-stream--resolve-main-buffer ()
  "Return the current Stream buffer, or nil outside Stream View."
  (when (derived-mode-p 'supertag-view-stream-mode)
    (current-buffer)))

(defun supertag-view-stream--node-ids (main)
  "Return ordered node IDs from MAIN's current Runtime state."
  (with-current-buffer main
    (mapcar (lambda (node) (plist-get node :id))
            (plist-get (plist-get supertag-view--instance :state) :nodes))))

(defun supertag-view-stream--current-node-id ()
  "Return the stable Stream node ID at point, or nil."
  (let ((position (if (and (eobp) (> (point) (point-min)))
                      (1- (point))
                    (point))))
    (or (get-text-property position 'supertag-entity-id)
        (get-text-property position 'supertag-widget-key)
        (when (> position (point-min))
          (or (get-text-property (1- position) 'supertag-entity-id)
              (get-text-property (1- position) 'supertag-widget-key))))))

(defun supertag-view-stream--find-entity (id)
  "Return the first position carrying entity ID."
  (let ((position (point-min))
        found)
    (while (and (< position (point-max)) (not found))
      (if (equal (get-text-property position 'supertag-entity-id) id)
          (setq found position)
        (setq position
              (or (next-single-property-change
                   position 'supertag-entity-id nil (point-max))
                  (point-max)))))
    found))

(defun supertag-view-stream--entity-range (id)
  "Return the current buffer range carrying entity ID."
  (when-let* ((start (supertag-view-stream--find-entity id)))
    (cons start
          (or (next-single-property-change
               start 'supertag-entity-id nil (point-max))
              (point-max)))))

(defun supertag-view-stream--highlight (id)
  "Highlight entity ID in the current Stream buffer."
  (when (overlayp supertag-view-stream--selection-overlay)
    (delete-overlay supertag-view-stream--selection-overlay)
    (setq supertag-view-stream--selection-overlay nil))
  (when-let* ((range (and id (supertag-view-stream--entity-range id))))
    (setq supertag-view-stream--selection-overlay
          (make-overlay (car range) (cdr range)))
    (overlay-put supertag-view-stream--selection-overlay
                 'face 'supertag-view-stream-current-face)))

(defun supertag-view-stream--select-node (main id)
  "Select node ID in MAIN and reveal its title."
  (unless (buffer-live-p main)
    (user-error "Stream buffer is not live"))
  (let ((position
         (with-current-buffer main
           (if-let* ((position (supertag-view-stream--find-entity id)))
               (progn
                 (goto-char position)
                 (supertag-view-stream--highlight id)
                 position)
             (user-error "Node %s is no longer in this Stream" id)))))
    (when-let* ((window (get-buffer-window main t)))
      (set-window-point window position)
      (set-window-start window position)))
  id)

(defun supertag-view-stream--restore-selection (selection)
  "Restore Widget SELECTION in the Stream title list."
  (supertag-view-widget--restore-selection selection)
  (let* ((main (current-buffer))
         (id (or (supertag-view-stream--current-node-id)
                 (car (supertag-view-stream--node-ids main)))))
    (when id
      (supertag-view-stream--select-node main id))))

(defun supertag-view-stream--subscribe (_input _state refresh)
  "Subscribe the Stream to relevant Store changes using REFRESH."
  (supertag-view-api-subscribe
   :store-changed
   (lambda (path _old-value _new-value)
     (when (and (listp path) (memq (car path) '(:nodes :tags)))
       (funcall refresh)))))

(defun supertag-view-stream--register-view ()
  "Register the Stream Adapter when needed."
  (unless (supertag-view-get 'stream)
    (supertag-view-register
     :id 'stream
     :name "Stream"
     :selectable nil
     :buffer-name-fn #'supertag-view-stream--buffer-name
     :mode-fn #'supertag-view-stream-mode
     :state-fn #'supertag-view-stream--build-state
     :render-fn #'supertag-view-stream--render
     :subscribe-fn #'supertag-view-stream--subscribe
     :capture-selection-fn #'supertag-view-widget--capture-selection
     :restore-selection-fn #'supertag-view-stream--restore-selection
     :display-action '(display-buffer-same-window))))

;;;###autoload
(defun supertag-view-stream (&optional tag)
  "Open a title Stream for TAG and all `:extends' descendants."
  (interactive
   (list (plist-get (supertag-view--read-tag) :value)))
  (unless (and (stringp tag) (not (string-empty-p tag)))
    (user-error "Stream View requires a tag"))
  (supertag-view-stream--register-view)
  (let* ((origin (current-window-configuration))
         (buffer (supertag-view-open
                  'stream (list :tag tag))))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (setq-local supertag-view-stream--origin-window-configuration origin))
    (let ((id (car (supertag-view-stream--node-ids buffer))))
      (when id
        (supertag-view-stream--select-node buffer id)))
    buffer))

(defun supertag-view-stream--move (delta)
  "Move DELTA nodes in the current Stream."
  (let* ((main (or (supertag-view-stream--resolve-main-buffer)
                   (user-error "Not in a Stream View")))
         (ids (supertag-view-stream--node-ids main))
         (current (supertag-view-stream--current-node-id))
         (index (or (cl-position current ids :test #'equal) 0)))
    (unless ids
      (user-error "This Stream has no nodes"))
    (supertag-view-stream--select-node
     main (nth (max 0 (min (1- (length ids)) (+ index delta))) ids))))

(defun supertag-view-stream-next-node ()
  "Move to the next Stream node."
  (interactive)
  (supertag-view-stream--move 1))

(defun supertag-view-stream-previous-node ()
  "Move to the previous Stream node."
  (interactive)
  (supertag-view-stream--move -1))

(defun supertag-view-stream-open-node-view ()
  "Open Node View for the current Stream node."
  (interactive)
  (let ((id (or (supertag-view-stream--current-node-id)
                (user-error "No Stream node at point"))))
    (supertag-view-node-open id)))

(defun supertag-view-stream--edit-range (node-id level)
  "Return the source range for NODE-ID at LEVEL in the current Org buffer."
  (widen)
  (if (zerop level)
      (cons (point-min) (point-max))
    (unless (supertag-node--goto-location node-id)
      (user-error "Could not locate node %s in its source file" node-id))
    (org-back-to-heading t)
    (let ((start (point))
          (end (save-excursion
                 (outline-next-heading)
                 (point))))
      (cons start end))))

(defun supertag-view-stream-edit ()
  "Edit the current Stream node in an indirect narrowed Org buffer."
  (interactive)
  (let* ((main (or (supertag-view-stream--resolve-main-buffer)
                   (user-error "Not in a Stream View")))
         (node-id (or (supertag-view-stream--current-node-id)
                      (user-error "No Stream node at point")))
         (node (supertag-view-api-get-entity :nodes node-id))
         (file (plist-get node :file))
         (level (or (plist-get node :level) 1)))
    (unless (and (stringp file) (file-exists-p file))
      (user-error "Source file for node %s is unavailable" node-id))
    (let* ((base (find-file-noselect file))
           (window-config (current-window-configuration))
           range
           edit)
      (with-current-buffer base
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (setq range (supertag-view-stream--edit-range node-id level))
        (goto-char (car range))
        (setq edit
              (clone-indirect-buffer
               (generate-new-buffer-name
                (format "*Supertag Edit: %s*"
                        (supertag-view-stream--node-title node)))
               nil)))
      (with-current-buffer edit
        (widen)
        (narrow-to-region (car range) (cdr range))
        (goto-char (point-min))
        (setq-local supertag-view-stream-edit--return-buffer main
                    supertag-view-stream-edit--window-configuration window-config
                    supertag-view-stream-edit--node-id node-id)
        (supertag-view-stream-edit-mode 1))
      (pop-to-buffer edit)
      edit)))

(defun supertag-view-stream-edit-finish ()
  "Finish the current Stream indirect edit and return to its Stream."
  (interactive)
  (unless supertag-view-stream-edit-mode
    (user-error "Not editing a Stream node"))
  (let ((edit (current-buffer))
        (main supertag-view-stream-edit--return-buffer)
        (window-config supertag-view-stream-edit--window-configuration)
        (node-id supertag-view-stream-edit--node-id))
    (save-restriction
      (widen)
      (when (supertag-node--goto-location node-id)
        (supertag-node-sync-at-point)))
    (kill-buffer edit)
    (when (window-configuration-p window-config)
      (set-window-configuration window-config))
    (when (buffer-live-p main)
      (supertag-view-refresh main))
    main))

(defun supertag-view-stream-quit ()
  "Quit the current Stream and restore its original window configuration."
  (interactive)
  (let* ((main (or (supertag-view-stream--resolve-main-buffer)
                   (user-error "Not in a Stream View")))
         (window-config
          (buffer-local-value
           'supertag-view-stream--origin-window-configuration main)))
    (when (buffer-live-p main)
      (kill-buffer main))
    (when (window-configuration-p window-config)
      (set-window-configuration window-config))))

(supertag-view-stream--register-view)

(provide 'supertag-view-stream)

;;; supertag-view-stream.el ends here

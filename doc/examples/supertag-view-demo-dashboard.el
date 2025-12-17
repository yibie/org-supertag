;;; supertag-view-demo-dashboard.el --- Demo dashboard view plugin -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is a working example of a simple view plugin:
;; - Read data via `supertag-view-api` (UI-agnostic).
;; - Render a custom dashboard buffer.
;; - Subscribe to store changes and refresh with throttling.
;;
;; Usage:
;;   (add-to-list 'load-path "/path/to/org-supertag/doc/examples/")
;;   (require 'supertag-view-demo-dashboard)
;;   M-x supertag-view-demo-dashboard-open

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'supertag-view-api)
(require 'supertag-services-ui) ; For `supertag-goto-node`

(defconst supertag-view-demo-dashboard--buffer-name "*Supertag Demo Dashboard*")

(defvar-local supertag-view-demo-dashboard--tag nil
  "Current tag name for this dashboard buffer.")

(defvar-local supertag-view-demo-dashboard--unsubscribers nil
  "List of unsubscribe functions for this dashboard buffer.")

(defvar-local supertag-view-demo-dashboard--refresh-timer nil
  "Idle timer used to throttle refreshes.")

(defun supertag-view-demo-dashboard--cleanup ()
  "Cleanup subscriptions/timers for current dashboard buffer."
  (when (timerp supertag-view-demo-dashboard--refresh-timer)
    (cancel-timer supertag-view-demo-dashboard--refresh-timer))
  (setq supertag-view-demo-dashboard--refresh-timer nil)
  (dolist (unsub supertag-view-demo-dashboard--unsubscribers)
    (when (functionp unsub)
      (ignore-errors (funcall unsub))))
  (setq supertag-view-demo-dashboard--unsubscribers nil))

(defun supertag-view-demo-dashboard--node-title (node)
  "Return a display title for NODE plist."
  (or (plist-get node :raw-value)
      (plist-get node :title)
      (plist-get node :id)
      "<untitled>"))

(defun supertag-view-demo-dashboard--insert-node-line (node)
  "Insert one clickable line for NODE."
  (let* ((node-id (plist-get node :id))
         (title (supertag-view-demo-dashboard--node-title node))
         (file (or (plist-get node :file) "")))
    (insert (format "- %s" title))
    (when (and (stringp file) (not (string-empty-p file)))
      (insert (format "  (%s)" (file-name-nondirectory file))))
    (when node-id
      (add-text-properties
       (line-beginning-position) (line-end-position)
       (list 'supertag-node-id node-id
             'mouse-face 'highlight
             'help-echo "RET: open node")))
    (insert "\n")))

(defun supertag-view-demo-dashboard--render (tag)
  "Render dashboard content for TAG into current buffer."
  (let* ((node-ids (supertag-view-api-nodes-by-tag tag))
         (nodes (supertag-view-api-get-entities :nodes node-ids))
         (sorted
          (sort nodes
                (lambda (a b)
                  (string<
                   (downcase (supertag-view-demo-dashboard--node-title a))
                   (downcase (supertag-view-demo-dashboard--node-title b)))))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Supertag Demo Dashboard\n\n"))
      (insert (format "Tag: %s\n" tag))
      (insert (format "Nodes: %d\n\n" (length sorted)))
      (insert "Keys: g refresh, RET open node, q quit\n\n")
      (if (null sorted)
          (insert "No nodes found for this tag.\n")
        (dolist (node sorted)
          (supertag-view-demo-dashboard--insert-node-line node)))
      (goto-char (point-min)))))

(defun supertag-view-demo-dashboard-refresh ()
  "Refresh dashboard buffer."
  (interactive)
  (unless (and supertag-view-demo-dashboard--tag
               (stringp supertag-view-demo-dashboard--tag)
               (not (string-empty-p supertag-view-demo-dashboard--tag)))
    (user-error "Dashboard tag is not set"))
  (supertag-view-demo-dashboard--render supertag-view-demo-dashboard--tag))

(defun supertag-view-demo-dashboard--schedule-refresh ()
  "Schedule a throttled refresh for current dashboard buffer."
  (when (timerp supertag-view-demo-dashboard--refresh-timer)
    (cancel-timer supertag-view-demo-dashboard--refresh-timer))
  (setq supertag-view-demo-dashboard--refresh-timer
        (run-with-idle-timer
         0.2 nil
         (lambda (buf)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (when (derived-mode-p 'supertag-view-demo-dashboard-mode)
                 (ignore-errors (supertag-view-demo-dashboard-refresh))))))
         (current-buffer))))

(defun supertag-view-demo-dashboard--handle-event (&rest args)
  "Handle store/node events and schedule refresh when relevant.

This demo keeps the handler defensive: different runtimes may emit different
payload shapes. We only care about the first argument being a PATH list."
  (let ((path (car args)))
    (when (and (listp path)
               (memq (car path) '(:nodes :relations :field-values :tags)))
      (supertag-view-demo-dashboard--schedule-refresh))))

(defun supertag-view-demo-dashboard-open-node-at-point ()
  "Open the node at point."
  (interactive)
  (let ((node-id (get-text-property (point) 'supertag-node-id)))
    (unless (and node-id (stringp node-id))
      (user-error "No node at point"))
    (supertag-goto-node node-id)))

(defvar supertag-view-demo-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'supertag-view-demo-dashboard-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'supertag-view-demo-dashboard-open-node-at-point)
    map)
  "Keymap for `supertag-view-demo-dashboard-mode'.")

(define-derived-mode supertag-view-demo-dashboard-mode special-mode "Supertag-Dashboard"
  "Demo dashboard view mode for org-supertag.")

(defun supertag-view-demo-dashboard--setup-subscriptions ()
  "Setup subscriptions for current dashboard buffer."
  (setq supertag-view-demo-dashboard--unsubscribers nil)
  ;; Subscribe to both legacy-style and canonical events.
  ;; If `:node-updated` is not emitted in the current runtime, `:store-changed`
  ;; will still refresh the dashboard.
  (push (supertag-view-api-subscribe :node-updated #'supertag-view-demo-dashboard--handle-event)
        supertag-view-demo-dashboard--unsubscribers)
  (push (supertag-view-api-subscribe :store-changed #'supertag-view-demo-dashboard--handle-event)
        supertag-view-demo-dashboard--unsubscribers)
  (add-hook 'kill-buffer-hook #'supertag-view-demo-dashboard--cleanup nil t))

(defun supertag-view-demo-dashboard-open (&optional tag)
  "Open a demo dashboard for TAG.

When TAG is nil, prompt the user to choose one from known tags."
  (interactive)
  (let* ((tag (or tag
                  (let ((tags (supertag-view-api-list-tags)))
                    (unless tags
                      (user-error "No tags found in store"))
                    (completing-read "Dashboard tag: " tags nil t)))))
    (with-current-buffer (get-buffer-create supertag-view-demo-dashboard--buffer-name)
      (supertag-view-demo-dashboard-mode)
      (setq supertag-view-demo-dashboard--tag tag)
      (supertag-view-demo-dashboard--setup-subscriptions)
      (supertag-view-demo-dashboard-refresh)
      (pop-to-buffer (current-buffer)))))

(provide 'supertag-view-demo-dashboard)

;;; supertag-view-demo-dashboard.el ends here

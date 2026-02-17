;;; supertag-graph-ui.el --- Web-based graph visualization for org-supertag -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025

;; This file is part of org-supertag.

;;; Commentary:

;; Provides a force-directed graph visualization of org-supertag nodes
;; and relations in the browser.  Requires optional packages `websocket'
;; and `simple-httpd'.
;;
;; Usage:
;;   M-x supertag-graph-ui-start   — start servers and open browser
;;   M-x supertag-graph-ui-stop    — stop servers
;;   M-x supertag-graph-ui-toggle-follow — sync graph focus to Emacs cursor

;;; Code:

(require 'json)
(require 'supertag-view-api)
(require 'supertag-core-schema)

;;; --- Configuration ---

(defcustom supertag-graph-ui-http-port 8080
  "HTTP port for serving graph UI static files."
  :type 'integer
  :group 'org-supertag)

(defcustom supertag-graph-ui-ws-port 35904
  "WebSocket port for live graph data."
  :type 'integer
  :group 'org-supertag)

(defcustom supertag-graph-ui-open-browser t
  "Whether to open browser automatically on start."
  :type 'boolean
  :group 'org-supertag)

;;; --- State ---

(defvar supertag-graph-ui--ws-server nil
  "WebSocket server instance.")

(defvar supertag-graph-ui--ws-clients nil
  "List of connected WebSocket clients.")

(defvar supertag-graph-ui--unsubscribe-fn nil
  "Unsubscribe function for store change events.")

(defvar supertag-graph-ui--update-timer nil
  "Debounce timer for graph updates.")

(defvar supertag-graph-ui--follow-mode nil
  "Non-nil when follow mode is active.")

(defvar supertag-graph-ui--last-focused-id nil
  "Last node ID sent as focus.")

;;; --- Data Serialization ---

(defun supertag-graph-ui--build-graph-data ()
  "Build graph data as an alist suitable for `json-encode'.
Returns alist with `nodes' and `links' arrays."
  (let ((nodes-out '())
        (links-out '())
        (node-ids-set (make-hash-table :test 'equal)))
    ;; Collect nodes
    (let ((nodes-ht (supertag-view-api-get-collection :nodes)))
      (when (hash-table-p nodes-ht)
        (maphash
         (lambda (id data)
           (when (and id (stringp id) data)
             (puthash id t node-ids-set)
             (push `((id . ,id)
                     (title . ,(or (plist-get data :title) "Untitled"))
                     (tags . ,(vconcat (or (plist-get data :tags) '())))
                     (file . ,(or (plist-get data :file) "")))
                   nodes-out)))
         nodes-ht)))
    ;; Collect relations (only where both endpoints exist)
    (let ((rels-ht (supertag-view-api-get-collection :relations)))
      (when (hash-table-p rels-ht)
        (maphash
         (lambda (_id data)
           (when data
             (let* ((rel (if (hash-table-p data)
                             (let (plist)
                               (maphash (lambda (k v) (setq plist (plist-put plist k v))) data)
                               plist)
                           data))
                    (from (plist-get rel :from))
                    (to (plist-get rel :to))
                    (type (plist-get rel :type))
                    (type-str (and type (substring (symbol-name type) 1)))
                    (meta (and type (supertag-relation-type-get type))))
               (when (and from to
                          (gethash from node-ids-set)
                          (gethash to node-ids-set))
                 (push `((source . ,from)
                         (target . ,to)
                         (type . ,(or type-str "unknown"))
                         (label . ,(or (and meta (plist-get meta :name)) type-str ""))
                         (color . ,(or (and meta (plist-get meta :color)) ""))
                         (style . ,(let ((s (and meta (plist-get meta :style))))
                                     (if (eq s :dashed) "dashed" "solid"))))
                       links-out)))))
         rels-ht)))
    `((nodes . ,(vconcat (nreverse nodes-out)))
      (links . ,(vconcat (nreverse links-out))))))

;;; --- WebSocket Server ---

(defun supertag-graph-ui--ws-on-open (ws)
  "Handle new WebSocket connection WS."
  (push ws supertag-graph-ui--ws-clients)
  ;; Send initial graph data
  (supertag-graph-ui--send-graph-data ws))

(defun supertag-graph-ui--ws-on-close (ws)
  "Handle WebSocket close for WS."
  (setq supertag-graph-ui--ws-clients
        (delq ws supertag-graph-ui--ws-clients)))

(defun supertag-graph-ui--ws-on-message (_ws frame)
  "Handle incoming WebSocket message FRAME."
  (condition-case err
      (let* ((text (websocket-frame-text frame))
             (msg (json-read-from-string text))
             (type (alist-get 'type msg)))
        (cond
         ((equal type "requestGraphData")
          (supertag-graph-ui--broadcast-graph-data))
         ((equal type "open")
          (let ((id (alist-get 'id (alist-get 'data msg))))
            (when id
              (supertag-graph-ui--open-node id))))))
    (error (message "supertag-graph-ui: WebSocket message error: %s" err))))

(defun supertag-graph-ui--send-graph-data (ws)
  "Send full graph data to WebSocket client WS."
  (let* ((data (supertag-graph-ui--build-graph-data))
         (msg (json-encode `((type . "graphdata") (data . ,data)))))
    (condition-case nil
        (websocket-send-text ws msg)
      (error nil))))

(defun supertag-graph-ui--broadcast-graph-data ()
  "Broadcast graph data to all connected clients."
  (when supertag-graph-ui--ws-clients
    (let* ((data (supertag-graph-ui--build-graph-data))
           (msg (json-encode `((type . "graphdata") (data . ,data)))))
      (dolist (ws (copy-sequence supertag-graph-ui--ws-clients))
        (condition-case nil
            (websocket-send-text ws msg)
          (error
           (setq supertag-graph-ui--ws-clients
                 (delq ws supertag-graph-ui--ws-clients))))))))

(defun supertag-graph-ui--broadcast-focus (node-id)
  "Broadcast focus on NODE-ID to all connected clients."
  (when (and node-id supertag-graph-ui--ws-clients)
    (let ((msg (json-encode `((type . "focus") (data . ((id . ,node-id)))))))
      (dolist (ws (copy-sequence supertag-graph-ui--ws-clients))
        (condition-case nil
            (websocket-send-text ws msg)
          (error
           (setq supertag-graph-ui--ws-clients
                 (delq ws supertag-graph-ui--ws-clients))))))))

;;; --- Node Interaction ---

(defun supertag-graph-ui--open-node (node-id)
  "Open NODE-ID in Emacs."
  (when node-id
    (condition-case nil
        (progn
          (require 'org-id)
          (org-id-goto node-id)
          (select-frame-set-input-focus (selected-frame)))
      (error (message "supertag-graph-ui: Cannot find node %s" node-id)))))

;;; --- Store Change Listener ---

(defun supertag-graph-ui--on-store-change (&rest _args)
  "Handle store change, debounce graph update."
  (when supertag-graph-ui--update-timer
    (cancel-timer supertag-graph-ui--update-timer))
  (setq supertag-graph-ui--update-timer
        (run-with-idle-timer 0.5 nil #'supertag-graph-ui--broadcast-graph-data)))

;;; --- Follow Mode ---

(defun supertag-graph-ui--follow-hook ()
  "Post-command hook for follow mode."
  (when (and supertag-graph-ui--follow-mode
             (derived-mode-p 'org-mode)
             supertag-graph-ui--ws-clients)
    (let ((id (org-entry-get nil "ID")))
      (when (and id (not (equal id supertag-graph-ui--last-focused-id)))
        (setq supertag-graph-ui--last-focused-id id)
        (supertag-graph-ui--broadcast-focus id)))))

;;;###autoload
(defun supertag-graph-ui-toggle-follow ()
  "Toggle follow mode: sync graph focus to Emacs cursor position."
  (interactive)
  (setq supertag-graph-ui--follow-mode (not supertag-graph-ui--follow-mode))
  (if supertag-graph-ui--follow-mode
      (add-hook 'post-command-hook #'supertag-graph-ui--follow-hook)
    (remove-hook 'post-command-hook #'supertag-graph-ui--follow-hook))
  (message "Graph follow mode %s" (if supertag-graph-ui--follow-mode "ON" "OFF")))

;;; --- Start / Stop ---

(defun supertag-graph-ui--static-dir ()
  "Return the path to ext/graph-ui/ directory."
  (expand-file-name "ext/graph-ui"
                    (file-name-directory (locate-library "supertag-graph-ui"))))

;;;###autoload
(defun supertag-graph-ui-start ()
  "Start graph visualization servers and open browser."
  (interactive)
  ;; Check dependencies
  (unless (require 'websocket nil t)
    (user-error "Package `websocket' is required. Install via M-x package-install RET websocket"))
  (unless (require 'simple-httpd nil t)
    (user-error "Package `simple-httpd' is required. Install via M-x package-install RET simple-httpd"))
  ;; Stop existing if running
  (when (or supertag-graph-ui--ws-server httpd-root)
    (supertag-graph-ui-stop))
  ;; Start HTTP server
  (let ((static-dir (supertag-graph-ui--static-dir)))
    (unless (file-directory-p static-dir)
      (user-error "Graph UI directory not found: %s" static-dir))
    (setq httpd-root static-dir)
    (setq httpd-port supertag-graph-ui-http-port)
    (httpd-start))
  ;; Start WebSocket server
  (setq supertag-graph-ui--ws-clients nil)
  (setq supertag-graph-ui--ws-server
        (websocket-server
         supertag-graph-ui-ws-port
         :host 'local
         :on-open #'supertag-graph-ui--ws-on-open
         :on-close #'supertag-graph-ui--ws-on-close
         :on-message #'supertag-graph-ui--ws-on-message))
  ;; Subscribe to store changes
  (setq supertag-graph-ui--unsubscribe-fn
        (supertag-view-api-subscribe :store-changed
                                     #'supertag-graph-ui--on-store-change))
  ;; Open browser
  (when supertag-graph-ui-open-browser
    (browse-url (format "http://localhost:%d" supertag-graph-ui-http-port)))
  (message "Graph UI started: http://localhost:%d (ws:%d)"
           supertag-graph-ui-http-port supertag-graph-ui-ws-port))

;;;###autoload
(defun supertag-graph-ui-stop ()
  "Stop graph visualization servers."
  (interactive)
  ;; Unsubscribe from store events
  (when (functionp supertag-graph-ui--unsubscribe-fn)
    (funcall supertag-graph-ui--unsubscribe-fn)
    (setq supertag-graph-ui--unsubscribe-fn nil))
  ;; Cancel pending timer
  (when supertag-graph-ui--update-timer
    (cancel-timer supertag-graph-ui--update-timer)
    (setq supertag-graph-ui--update-timer nil))
  ;; Stop follow mode
  (when supertag-graph-ui--follow-mode
    (supertag-graph-ui-toggle-follow))
  ;; Close WebSocket connections
  (dolist (ws supertag-graph-ui--ws-clients)
    (condition-case nil (websocket-close ws) (error nil)))
  (setq supertag-graph-ui--ws-clients nil)
  ;; Stop WebSocket server
  (when supertag-graph-ui--ws-server
    (websocket-server-close supertag-graph-ui--ws-server)
    (setq supertag-graph-ui--ws-server nil))
  ;; Stop HTTP server
  (when (fboundp 'httpd-stop)
    (httpd-stop))
  (message "Graph UI stopped."))

(provide 'supertag-graph-ui)
;;; supertag-graph-ui.el ends here

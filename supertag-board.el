;;; supertag-board.el --- Heptabase-style whiteboard for org-supertag -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026

;; This file is part of org-supertag.

;;; Commentary:

;; Provides a Heptabase-style whiteboard visualization of org-supertag nodes.
;; Uses React Flow (xyflow) for the canvas engine with a custom HTTP server
;; and WebSocket communication.
;;
;; Requires optional package `websocket'.
;;
;; Usage:
;;   M-x supertag-board-mode        — start/stop servers (browser opens automatically)
;;   M-x supertag-board-follow-mode — sync board focus to cursor

;;; Code:

(require 'json)
(require 'url-util)
(require 'supertag-view-api)
(require 'supertag-ops-tag)
(require 'supertag-ops-field)
(require 'supertag-board-ops)

;;; --- Configuration ---

(defgroup supertag-board nil
  "Whiteboard visualization for org-supertag."
  :group 'org-supertag
  :prefix "supertag-board-")

(defcustom supertag-board-port 35905
  "HTTP port for serving the board UI."
  :type 'integer
  :group 'supertag-board)

(defcustom supertag-board-ws-port 35907
  "WebSocket port for live board data."
  :type 'integer
  :group 'supertag-board)

(defcustom supertag-board-follow nil
  "Whether to enable follow mode on startup."
  :type 'boolean
  :group 'supertag-board)

(defvar supertag-board-app-build-dir
  (expand-file-name "ext/board-ui/out/"
                    (file-name-directory
                     (or load-file-name buffer-file-name
                         (locate-library "supertag-board"))))
  "Path to the built board UI frontend.")

;;; --- State ---

(defvar supertag-board--ws-server nil
  "WebSocket server instance.")

(defvar supertag-board--ws-socket nil
  "Current WebSocket connection.")

(defvar supertag-board--http-server nil
  "HTTP server process.")

(defvar supertag-board--http-clients nil
  "List of active HTTP client connections.")

(defvar supertag-board--unsubscribe-fn nil
  "Unsubscribe function for store change events.")

(defvar supertag-board--update-timer nil
  "Debounce timer for board updates.")

(defvar supertag-board--current-node nil
  "Last node ID sent via follow mode.")

;;; --- MIME Types ---

(defvar supertag-board--mime-types
  '(("html" . "text/html; charset=utf-8")
    ("htm"  . "text/html; charset=utf-8")
    ("js"   . "application/javascript; charset=utf-8")
    ("css"  . "text/css; charset=utf-8")
    ("json" . "application/json; charset=utf-8")
    ("png"  . "image/png")
    ("jpg"  . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif"  . "image/gif")
    ("svg"  . "image/svg+xml")
    ("ico"  . "image/x-icon")
    ("woff" . "font/woff")
    ("woff2" . "font/woff2")
    ("ttf"  . "font/ttf")
    ("txt"  . "text/plain; charset=utf-8"))
  "Mapping of file extensions to MIME types.")

(defun supertag-board--mime-type (filename)
  "Return MIME type for FILENAME."
  (let ((ext (file-name-extension filename)))
    (or (cdr (assoc ext supertag-board--mime-types))
        "application/octet-stream")))

;;; --- HTTP Server ---

(defun supertag-board--http-start ()
  "Start a lightweight HTTP server for the board UI."
  (when supertag-board--http-server
    (supertag-board--http-stop))
  (setq supertag-board--http-clients nil)
  (let ((proc (make-network-process
               :name "supertag-board-http"
               :server t
               :host "127.0.0.1"
               :service supertag-board-port
               :family 'ipv4
               :noquery t
               :sentinel #'supertag-board--http-sentinel
               :log #'supertag-board--http-log)))
    (set-process-coding-system proc 'binary 'binary)
    (setq supertag-board--http-server proc)))

(defun supertag-board--http-log (_server client _message)
  "Handle new client connection CLIENT on the HTTP server.
Set up filter and coding for the new client process."
  (push client supertag-board--http-clients)
  (set-process-coding-system client 'binary 'binary)
  (set-process-filter client #'supertag-board--http-filter)
  (set-process-sentinel client #'supertag-board--http-sentinel))

(defun supertag-board--http-stop ()
  "Stop the HTTP server."
  (dolist (client supertag-board--http-clients)
    (when (process-live-p client)
      (delete-process client)))
  (setq supertag-board--http-clients nil)
  (when (process-live-p supertag-board--http-server)
    (delete-process supertag-board--http-server))
  (setq supertag-board--http-server nil))

(defun supertag-board--http-sentinel (proc event)
  "Handle HTTP process PROC state change EVENT."
  (when (string-match-p "\\(connection broken\\|deleted\\|finished\\)" event)
    (setq supertag-board--http-clients
          (delq proc supertag-board--http-clients))))

(defun supertag-board--http-filter (proc data)
  "Handle incoming HTTP request from PROC with DATA."
  (unless (member proc supertag-board--http-clients)
    (push proc supertag-board--http-clients))
  (condition-case err
      (when (string-match "^GET \\(/[^ ]*\\) HTTP" data)
        (let* ((raw-path (match-string 1 data))
               (path (url-unhex-string (car (split-string raw-path "?"))))
               (clean-path (if (string= path "/") "/index.html" path)))
          ;; Check for /api/node/<id> endpoint
          (if (string-prefix-p "/api/node/" clean-path)
              (supertag-board--http-serve-node proc clean-path)
            (supertag-board--http-serve-file proc clean-path))))
    (error
     (supertag-board--http-respond proc 500 "text/plain"
                                   (format "Internal error: %s" err)))))

(defun supertag-board--http-serve-file (proc path)
  "Serve static file at PATH to PROC."
  (let ((filepath (expand-file-name (substring path 1)
                                    supertag-board-app-build-dir)))
    ;; Security: ensure file is within build dir
    (if (and (string-prefix-p (expand-file-name supertag-board-app-build-dir)
                              (expand-file-name filepath))
             (file-exists-p filepath)
             (not (file-directory-p filepath)))
        (let ((content (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally filepath)
                         (buffer-string)))
              (mime (supertag-board--mime-type filepath)))
          (supertag-board--http-respond proc 200 mime content))
      ;; Try path.html fallback (Next.js static export)
      (let ((html-path (concat filepath ".html")))
        (if (file-exists-p html-path)
            (let ((content (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally html-path)
                             (buffer-string))))
              (supertag-board--http-respond proc 200 "text/html; charset=utf-8" content))
          (supertag-board--http-respond proc 404 "text/plain" "Not found"))))))

(defun supertag-board--http-serve-node (proc path)
  "Serve node content for PATH to PROC."
  (let* ((id (url-unhex-string (substring path (length "/api/node/"))))
         (text (condition-case nil
                   (when (and id (not (string-empty-p id)))
                     (require 'org-id nil t)
                     (let ((marker (org-id-find id 'marker)))
                       (when marker
                         (with-current-buffer (marker-buffer marker)
                           (save-excursion
                             (goto-char marker)
                             (let ((beg (point))
                                   (end (save-excursion
                                          (org-end-of-subtree t t)
                                          (point))))
                               (buffer-substring-no-properties beg end)))))))
                 (error nil))))
    (supertag-board--http-respond proc 200
                                  "text/plain; charset=utf-8"
                                  (or text "(empty node)"))))

(defun supertag-board--http-respond (proc status content-type body)
  "Send HTTP response to PROC with STATUS, CONTENT-TYPE, and BODY."
  (when (process-live-p proc)
    (let* ((body-bytes (if (multibyte-string-p body)
                           (encode-coding-string body 'utf-8)
                         body))
           (response (concat
                      (format "HTTP/1.1 %d %s\r\n" status
                              (pcase status
                                (200 "OK") (404 "Not Found") (_ "Error")))
                      (format "Content-Type: %s\r\n" content-type)
                      (format "Content-Length: %d\r\n" (length body-bytes))
                      "Access-Control-Allow-Origin: *\r\n"
                      ;; Prevent browsers from caching HTML (so new builds are picked up immediately).
                      ;; Hashed JS/CSS assets can be cached forever since their names change on rebuild.
                      (if (string-prefix-p "text/html" content-type)
                          "Cache-Control: no-cache\r\n"
                        "Cache-Control: max-age=31536000, immutable\r\n")
                      "Connection: close\r\n"
                      "\r\n")))
      (process-send-string proc response)
      (process-send-string proc body-bytes)
      (run-at-time 0.1 nil (lambda () (when (process-live-p proc)
                                         (delete-process proc)))))))

;;; --- Minor Mode ---

;;;###autoload
(define-minor-mode supertag-board-mode
  "Enable org-supertag whiteboard visualization.
Starts HTTP and WebSocket servers to serve the board UI."
  :lighter " board"
  :global t
  :group 'supertag-board
  :init-value nil
  (cond
   (supertag-board-mode
    (unless (require 'websocket nil t)
      (supertag-board-mode -1)
      (user-error "Package `websocket' is required. Install via M-x package-install"))
    (unless (file-directory-p supertag-board-app-build-dir)
      (supertag-board-mode -1)
      (user-error "Board UI build directory not found: %s\nRun `cd ext/board-ui && npm run build` first"
                  supertag-board-app-build-dir))
    ;; Start HTTP server
    (supertag-board--http-start)
    ;; Start WebSocket server
    (setq supertag-board--ws-server
          (websocket-server
           supertag-board-ws-port
           :host 'local
           :on-open #'supertag-board--ws-on-open
           :on-message #'supertag-board--ws-on-message
           :on-close #'supertag-board--ws-on-close))
    ;; Subscribe to store changes
    (setq supertag-board--unsubscribe-fn
          (supertag-view-api-subscribe :store-changed
                                       #'supertag-board--on-store-change))
    (run-with-timer 0.5 nil
                    (lambda ()
                      (browse-url (format "http://localhost:%d" supertag-board-port))))
    (when supertag-board-follow
      (supertag-board-follow-mode 1))
    (message "supertag-board: servers started on ports %d (HTTP) and %d (WS)"
             supertag-board-port supertag-board-ws-port))
   (t
    (when supertag-board--ws-server
      (websocket-server-close supertag-board--ws-server)
      (setq supertag-board--ws-server nil))
    (supertag-board--http-stop)
    (when (functionp supertag-board--unsubscribe-fn)
      (funcall supertag-board--unsubscribe-fn)
      (setq supertag-board--unsubscribe-fn nil))
    (when supertag-board--update-timer
      (cancel-timer supertag-board--update-timer)
      (setq supertag-board--update-timer nil))
    (supertag-board-follow-mode -1)
    (setq supertag-board--ws-socket nil)
    (message "supertag-board: servers stopped"))))

;;; --- WebSocket Handlers ---

(defun supertag-board--ws-on-open (ws)
  "Handle new WebSocket connection WS."
  (setq supertag-board--ws-socket ws)
  ;; Send board list on connect
  (supertag-board--send-board-list)
  (message "Connection established with supertag board UI"))

(defun supertag-board--ws-on-message (_ws frame)
  "Handle incoming WebSocket message FRAME."
  (condition-case err
      (let* ((msg (json-parse-string
                   (websocket-frame-text frame) :object-type 'alist))
             (command (alist-get 'command msg))
             (data (alist-get 'data msg)))
        (pcase command
          ;; Board management
          ("list-boards"    (supertag-board--send-board-list))
          ("open-board"     (supertag-board--on-open-board data))
          ("create-board"   (supertag-board--on-create-board data))
          ("delete-board"   (supertag-board--on-delete-board data))
          ;; Node operations
          ("add-node"       (supertag-board--on-add-node data))
          ("remove-node"    (supertag-board--on-remove-node data))
          ("move-node"      (supertag-board--on-move-node data))
          ("resize-node"    (supertag-board--on-resize-node data))
          ;; Edge operations
          ("add-edge"       (supertag-board--on-add-edge data))
          ("remove-edge"    (supertag-board--on-remove-edge data))
          ("update-edge"    (supertag-board--on-update-edge data))
          ;; Group operations
          ("add-group"      (supertag-board--on-add-group data))
          ("update-group"   (supertag-board--on-update-group data))
          ("remove-group"   (supertag-board--on-remove-group data))
          ;; Navigation
          ("open-node"      (supertag-board--on-open-node data))
          ("update-node-title" (supertag-board--on-update-title data))
          ;; Viewport
          ("save-viewport"  (supertag-board--on-save-viewport data))
          ;; Node list for palette
          ("list-nodes"     (supertag-board--send-node-list))
          (_ nil)))
    (error (message "supertag-board: message error: %s" err))))

(defun supertag-board--ws-on-close (_ws)
  "Handle WebSocket close."
  (setq supertag-board--ws-socket nil))

;;; --- WebSocket Send Helpers ---

(defun supertag-board--ws-send (type data)
  "Send JSON message with TYPE and DATA to the WebSocket client."
  (when (and supertag-board--ws-socket
             (websocket-openp supertag-board--ws-socket))
    (condition-case nil
        (websocket-send-text
         supertag-board--ws-socket
         (json-encode `((type . ,type) (data . ,data))))
      (error nil))))

;;; --- Board Management Handlers ---

(defun supertag-board--send-board-list ()
  "Send list of all boards to the frontend."
  (let ((boards (supertag-board-list))
        (result '()))
    (dolist (board boards)
      (push `((id . ,(plist-get board :id))
              (title . ,(plist-get board :title)))
            result))
    (supertag-board--ws-send "board-list" (vconcat (nreverse result)))))

(defun supertag-board--on-open-board (data)
  "Send full board data for board specified in DATA."
  (let* ((board-id (alist-get 'boardId data))
         (board (when board-id (supertag-board-get board-id))))
    (cond
     ((null board-id)
      (message "supertag-board: open-board received no boardId"))
     ((null board)
      (message "supertag-board: board not found: %s" board-id))
     (t
      (condition-case err
          (supertag-board--send-board-data board)
        (error
         (message "supertag-board: error sending board data for %s: %s" board-id err)))))))

(defun supertag-board--send-board-data (board)
  "Serialize and send BOARD data to the frontend."
  (let* ((board-id (plist-get board :id))
         (placements (plist-get board :node-placements))
         (board-edges (plist-get board :board-edges))
         (groups (plist-get board :groups))
         (viewport (plist-get board :viewport))
         ;; Build node data with supertag info
         (nodes-data '())
         (edges-data '())
         (groups-data '())
         ;; Also gather global relations between placed nodes
         (placed-ids (mapcar #'car placements))
         (global-edges (supertag-board--get-global-edges placed-ids)))
    ;; Build nodes
    (dolist (placement placements)
      (let* ((node-id (car placement))
             (pos (cdr placement))
             (node-store (and (stringp node-id) (not (string-empty-p node-id))
                              (ignore-errors (supertag-view-api-get-entity :nodes node-id))))
             (tags (when node-store (plist-get node-store :tags)))
             (tag-fields (supertag-board--node-tag-fields-preview node-id tags)))
        (push `((id . ,node-id)
                (title . ,(if node-store
                              (or (plist-get node-store :title) "Untitled")
                            "Unknown"))
                (tags . ,(vconcat (or tags '())))
                (tagFields . ,tag-fields)
                (content . ,(or (and node-store (plist-get node-store :content)) ""))
                (x . ,(plist-get pos :x))
                (y . ,(plist-get pos :y))
                (width . ,(or (plist-get pos :width) 280))
                (height . ,(plist-get pos :height))
                (collapsed . ,(if (plist-get pos :collapsed) t :json-false)))
              nodes-data)))
    ;; Build board-local edges
    (dolist (edge-entry board-edges)
      (let ((edge (cdr edge-entry)))
        (push `((id . ,(plist-get edge :id))
                (from . ,(plist-get edge :from))
                (to . ,(plist-get edge :to))
                (label . ,(or (plist-get edge :label) ""))
                (style . ,(or (plist-get edge :style) "solid"))
                (color . ,(plist-get edge :color))
                (sourceHandle . ,(or (plist-get edge :source-handle) "right"))
                (targetHandle . ,(or (plist-get edge :target-handle) "left"))
                (isGlobal . :json-false))
              edges-data)))
    ;; Add global relation edges
    (dolist (ge global-edges)
      (push ge edges-data))
    ;; Build groups
    (dolist (group-entry groups)
      (let ((group (cdr group-entry)))
        (push `((id . ,(plist-get group :id))
                (label . ,(or (plist-get group :label) ""))
                (x . ,(plist-get group :x))
                (y . ,(plist-get group :y))
                (width . ,(plist-get group :width))
                (height . ,(plist-get group :height))
                (color . ,(or (plist-get group :color) "#e8f0fe"))
                (nodeIds . ,(vconcat (or (plist-get group :node-ids) '()))))
              groups-data)))
    ;; Send
    (supertag-board--ws-send "board-data"
     `((boardId . ,board-id)
       (title . ,(plist-get board :title))
       (nodes . ,(vconcat (nreverse nodes-data)))
       (edges . ,(vconcat (nreverse edges-data)))
       (groups . ,(vconcat (nreverse groups-data)))
       (viewport . ,(if viewport
                        `((x . ,(plist-get viewport :x))
                          (y . ,(plist-get viewport :y))
                          (zoom . ,(plist-get viewport :zoom)))
                      '((x . 0) (y . 0) (zoom . 1.0))))))))

(defun supertag-board--field-value-to-string (value)
  "Normalize field VALUE to a compact display string."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((vectorp value)
    (mapconcat #'supertag-board--field-value-to-string (append value nil) ", "))
   ((listp value)
    (mapconcat #'supertag-board--field-value-to-string value ", "))
   ((eq value :json-false) "false")
   ((eq value t) "true")
   (t (format "%s" value))))

(defun supertag-board--node-tag-fields-preview (node-id tags)
  "Return per-tag fields for NODE-ID using TAGS.
Result shape is an alist: (TAG-ID . [((name . ..) (value . ..)) ...])."
  (let ((result '()))
    (dolist (tag-id tags (nreverse result))
      (let* ((field-defs (ignore-errors (supertag-tag-get-all-fields tag-id)))
             (items '()))
        (dolist (field-def (or field-defs '()))
          (let* ((field-name (or (plist-get field-def :name)
                                 (plist-get field-def :id)))
                 (raw-value (and field-name
                                 (ignore-errors
                                   (supertag-field-get-with-default node-id tag-id field-name)))))
            (when field-name
              (push `((name . ,field-name)
                      (value . ,(supertag-board--field-value-to-string raw-value)))
                    items))))
        (push (cons tag-id (vconcat (nreverse items))) result)))))

(defun supertag-board--get-global-edges (node-ids)
  "Get global relations between NODE-IDS as edge alists."
  (let ((rels-ht (supertag-view-api-get-collection :relations))
        (id-set (make-hash-table :test 'equal))
        (result '()))
    (dolist (id node-ids)
      (puthash id t id-set))
    (when (hash-table-p rels-ht)
      (maphash
       (lambda (_id data)
         (when data
           (let* ((rel (if (hash-table-p data)
                           (let (plist)
                             (maphash (lambda (k v)
                                        (setq plist (plist-put plist k v)))
                                      data)
                             plist)
                         data))
                  (from (plist-get rel :from))
                  (to (plist-get rel :to))
                  (type (plist-get rel :type)))
             (when (and (gethash from id-set)
                        (gethash to id-set))
               (let ((meta (and type (supertag-relation-type-get type))))
                 (push `((id . ,(format "global-%s-%s" from to))
                         (from . ,from)
                         (to . ,to)
                         (label . ,(if meta (or (plist-get meta :name) "") ""))
                         (style . ,(if meta
                                       (let ((s (plist-get meta :style)))
                                         (if (eq s :dashed) "dashed" "solid"))
                                     "solid"))
                         (color . ,(when meta (plist-get meta :color)))
                         (sourceHandle . "right")
                         (targetHandle . "left")
                         (isGlobal . t))
                       result))))))
       rels-ht))
    result))

(defun supertag-board--on-create-board (data)
  "Create a new board from DATA and send updated list."
  (let* ((title (alist-get 'title data))
         (board (supertag-board-create (or title "Untitled Board"))))
    (supertag-board--send-board-list)
    (supertag-board--send-board-data board)))

(defun supertag-board--on-delete-board (data)
  "Delete board specified in DATA."
  (let ((board-id (alist-get 'boardId data)))
    (when board-id
      (supertag-board-delete board-id)
      (supertag-board--send-board-list))))

;;; --- Node Operation Handlers ---

(defun supertag-board--on-add-node (data)
  "Add node to board from DATA."
  (let ((board-id (alist-get 'boardId data))
        (node-id (alist-get 'nodeId data))
        (x (or (alist-get 'x data) 100))
        (y (or (alist-get 'y data) 100))
        (width (alist-get 'width data))
        (height (alist-get 'height data)))
    (when (and board-id node-id)
      (supertag-board-add-node board-id node-id x y width height)
      (let ((board (supertag-board-get board-id)))
        (when board (supertag-board--send-board-data board))))))

(defun supertag-board--on-remove-node (data)
  "Remove node from board from DATA."
  (let ((board-id (alist-get 'boardId data))
        (node-id (alist-get 'nodeId data)))
    (when (and board-id node-id)
      (supertag-board-remove-node board-id node-id)
      (let ((board (supertag-board-get board-id)))
        (when board (supertag-board--send-board-data board))))))

(defun supertag-board--on-move-node (data)
  "Move node on board from DATA."
  (let ((board-id (alist-get 'boardId data))
        (node-id (alist-get 'nodeId data))
        (x (alist-get 'x data))
        (y (alist-get 'y data)))
    (when (and board-id node-id x y)
      (supertag-board-move-node board-id node-id x y))))

(defun supertag-board--on-resize-node (data)
  "Resize node on board from DATA."
  (let ((board-id (alist-get 'boardId data))
        (node-id (alist-get 'nodeId data))
        (width (alist-get 'width data))
        (height (alist-get 'height data)))
    (when (and board-id node-id)
      (supertag-board-resize-node board-id node-id width height))))

;;; --- Edge Operation Handlers ---

(defun supertag-board--on-add-edge (data)
  "Add edge to board from DATA."
  (let ((board-id (alist-get 'boardId data))
        (from (alist-get 'from data))
        (to (alist-get 'to data))
        (label (alist-get 'label data))
        (style (alist-get 'style data))
        (color (alist-get 'color data))
        (source-handle (alist-get 'sourceHandle data))
        (target-handle (alist-get 'targetHandle data)))
    (when (and board-id from to)
      (supertag-board-add-edge board-id from to label style color
                               source-handle target-handle)
      (let ((board (supertag-board-get board-id)))
        (when board (supertag-board--send-board-data board))))))

(defun supertag-board--on-remove-edge (data)
  "Remove edge from board from DATA."
  (let ((board-id (alist-get 'boardId data))
        (edge-id (alist-get 'edgeId data)))
    (when (and board-id edge-id)
      (supertag-board-remove-edge board-id edge-id)
      (let ((board (supertag-board-get board-id)))
        (when board (supertag-board--send-board-data board))))))

(defun supertag-board--on-update-edge (data)
  "Update edge label/style/color from DATA."
  (let ((board-id (alist-get 'boardId data))
        (edge-id (alist-get 'edgeId data))
        (changes '()))
    (when-let ((v (alist-get 'label data)))
      (setq changes (plist-put changes :label v)))
    (when-let ((v (alist-get 'style data)))
      (setq changes (plist-put changes :style v)))
    (when-let ((v (alist-get 'color data)))
      (setq changes (plist-put changes :color v)))
    (when (and board-id edge-id changes)
      (supertag-board-update-edge board-id edge-id changes)
      (let ((board (supertag-board-get board-id)))
        (when board (supertag-board--send-board-data board))))))

;;; --- Group Operation Handlers ---

(defun supertag-board--on-add-group (data)
  "Add group to board from DATA."
  (let ((board-id (alist-get 'boardId data))
        (label (or (alist-get 'label data) "Group"))
        (x (or (alist-get 'x data) 0))
        (y (or (alist-get 'y data) 0))
        (width (or (alist-get 'width data) 400))
        (height (or (alist-get 'height data) 300))
        (color (alist-get 'color data))
        (node-ids (alist-get 'nodeIds data)))
    (when board-id
      (supertag-board-add-group board-id label x y width height color
                                (when node-ids (append node-ids nil)))
      (let ((board (supertag-board-get board-id)))
        (when board (supertag-board--send-board-data board))))))

(defun supertag-board--on-update-group (data)
  "Update group on board from DATA."
  (let ((board-id (alist-get 'boardId data))
        (group-id (alist-get 'groupId data))
        (changes '()))
    (when (alist-get 'label data)
      (setq changes (plist-put changes :label (alist-get 'label data))))
    (when (alist-get 'x data)
      (setq changes (plist-put changes :x (alist-get 'x data))))
    (when (alist-get 'y data)
      (setq changes (plist-put changes :y (alist-get 'y data))))
    (when (alist-get 'width data)
      (setq changes (plist-put changes :width (alist-get 'width data))))
    (when (alist-get 'height data)
      (setq changes (plist-put changes :height (alist-get 'height data))))
    (when (alist-get 'color data)
      (setq changes (plist-put changes :color (alist-get 'color data))))
    (when (alist-get 'nodeIds data)
      (setq changes (plist-put changes :node-ids
                                (append (alist-get 'nodeIds data) nil))))
    (when (and board-id group-id changes)
      (supertag-board-update-group board-id group-id changes)
      (let ((board (supertag-board-get board-id)))
        (when board (supertag-board--send-board-data board))))))

(defun supertag-board--on-remove-group (data)
  "Remove group from board from DATA."
  (let ((board-id (alist-get 'boardId data))
        (group-id (alist-get 'groupId data)))
    (when (and board-id group-id)
      (supertag-board-remove-group board-id group-id)
      (let ((board (supertag-board-get board-id)))
        (when board (supertag-board--send-board-data board))))))

;;; --- Navigation ---

(defun supertag-board--on-open-node (data)
  "Open node in Emacs from DATA."
  (let ((id (alist-get 'id data)))
    (when id
      (condition-case nil
          (progn
            (require 'org-id)
            (let ((marker (org-id-find id 'marker)))
              (when marker
                (pop-to-buffer (marker-buffer marker))
                (goto-char marker)
                (org-show-context)
                (select-frame-set-input-focus (selected-frame)))))
        (error (message "supertag-board: Cannot find node %s" id))))))

(defun supertag-board--on-update-title (data)
  "Update node title from the board."
  (let ((node-id (alist-get 'nodeId data))
        (title (alist-get 'title data)))
    (when (and node-id title)
      (condition-case nil
          (progn
            (require 'org-id)
            (let ((marker (org-id-find node-id 'marker)))
              (when marker
                (with-current-buffer (marker-buffer marker)
                  (save-excursion
                    (goto-char marker)
                    (org-edit-headline title)
                    (save-buffer))))))
        (error (message "supertag-board: Cannot update title for %s" node-id))))))

;;; --- Viewport ---

(defun supertag-board--on-save-viewport (data)
  "Save viewport state from DATA."
  (let ((board-id (alist-get 'boardId data))
        (x (alist-get 'x data))
        (y (alist-get 'y data))
        (zoom (alist-get 'zoom data)))
    (when (and board-id x y zoom)
      (supertag-board-save-viewport board-id x y zoom))))

;;; --- Node List (for palette) ---

(defun supertag-board--send-node-list ()
  "Send list of all available nodes to the frontend."
  (let ((nodes-ht (supertag-view-api-get-collection :nodes))
        (result '()))
    (when (hash-table-p nodes-ht)
      (maphash
       (lambda (id data)
         (when (and id (stringp id) data)
           (push `((id . ,id)
                   (title . ,(or (plist-get data :title) "Untitled"))
                   (tags . ,(vconcat (or (plist-get data :tags) '())))
                   (file . ,(or (plist-get data :file) "")))
                 result)))
       nodes-ht))
    (supertag-board--ws-send "node-list" (vconcat (nreverse result)))))

;;; --- Store Change Listener ---

(defun supertag-board--on-store-change (&rest _args)
  "Handle store change, debounce board update."
  (when supertag-board--update-timer
    (cancel-timer supertag-board--update-timer))
  (setq supertag-board--update-timer
        (run-with-idle-timer 0.5 nil #'supertag-board--on-store-update)))

(defun supertag-board--on-store-update ()
  "Send updated node data for the current board."
  (when supertag-board--ws-socket
    ;; Notify the frontend to refresh node data
    (supertag-board--ws-send "store-changed" nil)))

;;; --- Follow Mode ---

(defun supertag-board--update-current-node ()
  "Send the current node ID to the board UI for highlight."
  (when (and supertag-board--ws-socket
             (websocket-openp supertag-board--ws-socket)
             (derived-mode-p 'org-mode)
             (buffer-file-name (buffer-base-buffer)))
    (let ((id (org-entry-get nil "ID")))
      (when (and id (not (equal id supertag-board--current-node)))
        (setq supertag-board--current-node id)
        (supertag-board--ws-send "follow" `((id . ,id)))))))

;;;###autoload
(define-minor-mode supertag-board-follow-mode
  "Sync the board UI focus to the current node in Emacs."
  :lighter " bf"
  :global t
  :group 'supertag-board
  :init-value nil
  (if supertag-board-follow-mode
      (add-hook 'post-command-hook #'supertag-board--update-current-node)
    (remove-hook 'post-command-hook #'supertag-board--update-current-node)))

(provide 'supertag-board)
;;; supertag-board.el ends here

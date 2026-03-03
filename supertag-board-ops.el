;;; supertag-board-ops.el --- Board CRUD operations for org-supertag -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026

;; This file is part of org-supertag.

;;; Commentary:

;; Provides CRUD operations for the :boards collection.
;; Boards are whiteboard layouts that contain positioned nodes,
;; edges (connections), and groups.

;;; Code:

(require 'cl-lib)
(require 'supertag-core-store)

;;; --- Board CRUD ---

(defun supertag-board-create (title)
  "Create a new board with TITLE. Return the board plist."
  (let* ((id (org-id-uuid))
         (now (current-time))
         (board `(:id ,id
                  :title ,title
                  :node-placements nil
                  :board-edges nil
                  :groups nil
                  :viewport (:x 0 :y 0 :zoom 1.0)
                  :created-at ,now
                  :modified-at ,now)))
    (supertag-store-put-entity :boards id board t)
    board))

(defun supertag-board-delete (board-id)
  "Delete board BOARD-ID."
  (supertag-store-remove-entity :boards board-id))

(defun supertag-board-get (board-id)
  "Return board plist for BOARD-ID, or nil."
  (supertag-store-get-entity :boards board-id))

(defun supertag-board-list ()
  "Return list of all board plists."
  (let ((ht (supertag-store-get-collection :boards))
        result)
    (when (hash-table-p ht)
      (maphash (lambda (_id board) (push board result)) ht))
    (nreverse result)))

(defun supertag-board--update (board-id fn)
  "Apply FN to board BOARD-ID, store and return updated board.
FN receives the board plist and should return the modified plist."
  (let* ((board (supertag-board-get board-id))
         (updated (when board
                    (plist-put (funcall fn board)
                               :modified-at (current-time)))))
    (when updated
      (supertag-store-put-entity :boards board-id updated t))
    updated))

;;; --- Node Placement ---

(defun supertag-board-add-node (board-id node-id x y &optional width height)
  "Add NODE-ID to BOARD-ID at position X, Y."
  (supertag-board--update board-id
    (lambda (board)
      (let ((placements (plist-get board :node-placements))
            (placement `(:x ,x :y ,y
                         :width ,(or width 180)
                         :height ,height
                         :collapsed nil)))
        (plist-put board :node-placements
                   (cons (cons node-id placement)
                         (assoc-delete-all node-id placements)))))))

(defun supertag-board-remove-node (board-id node-id)
  "Remove NODE-ID from BOARD-ID.
Also cleans up edges referencing NODE-ID and removes NODE-ID from groups."
  (supertag-board--update board-id
    (lambda (board)
      ;; 1. Remove node placement
      (plist-put board :node-placements
                 (assoc-delete-all node-id
                                   (plist-get board :node-placements)))
      ;; 2. Remove edges that reference this node (from or to)
      (let ((edges (plist-get board :board-edges)))
        (plist-put board :board-edges
                   (cl-remove-if
                    (lambda (edge-entry)
                      (let ((edge (cdr edge-entry)))
                        (or (equal (plist-get edge :from) node-id)
                            (equal (plist-get edge :to) node-id))))
                    edges)))
      ;; 3. Remove node-id from all groups' :node-ids
      (let ((groups (plist-get board :groups)))
        (plist-put board :groups
                   (mapcar (lambda (group-entry)
                             (let* ((gid (car group-entry))
                                    (group (cdr group-entry))
                                    (nids (plist-get group :node-ids))
                                    (new-group (copy-sequence group)))
                               (plist-put new-group :node-ids
                                          (remove node-id nids))
                               (cons gid new-group)))
                           groups)))
      board)))

(defun supertag-board-move-node (board-id node-id x y)
  "Update position of NODE-ID on BOARD-ID to X, Y."
  (supertag-board--update board-id
    (lambda (board)
      (let* ((placements (plist-get board :node-placements))
             (old (cdr (assoc node-id placements))))
        (when old
          (let ((new-placement (copy-sequence old)))
            (plist-put new-placement :x x)
            (plist-put new-placement :y y)
            (plist-put board :node-placements
                       (cons (cons node-id new-placement)
                             (assoc-delete-all node-id placements)))))
        board))))

(defun supertag-board-resize-node (board-id node-id width height)
  "Update size of NODE-ID on BOARD-ID to WIDTH, HEIGHT."
  (supertag-board--update board-id
    (lambda (board)
      (let* ((placements (plist-get board :node-placements))
             (old (cdr (assoc node-id placements))))
        (when old
          (let ((new-placement (copy-sequence old)))
            (when width (plist-put new-placement :width width))
            (when height (plist-put new-placement :height height))
            (plist-put board :node-placements
                       (cons (cons node-id new-placement)
                             (assoc-delete-all node-id placements)))))
        board))))

;;; --- Board Edges ---

(defun supertag-board-add-edge (board-id from to &optional label style color
                                         source-handle target-handle)
  "Add an edge from FROM to TO on BOARD-ID. Return the edge id.
Returns nil if FROM or TO are not on the board, or if a duplicate edge exists."
  (let* ((board (supertag-board-get board-id))
         (placements (plist-get board :node-placements))
         (edges (plist-get board :board-edges))
         ;; Check validity up-front
         (valid (and (assoc from placements)
                     (assoc to placements)))
         (duplicate (and valid
                         (cl-some (lambda (edge-entry)
                                    (let ((e (cdr edge-entry)))
                                      (and (equal (plist-get e :from) from)
                                           (equal (plist-get e :to) to))))
                                  edges))))
    (cond
     ((not valid)
      (message "supertag-board-add-edge: endpoint not on board (from=%s to=%s)" from to)
      nil)
     (duplicate
      nil)
     (t
      (let ((edge-id (org-id-uuid)))
        (supertag-board--update board-id
          (lambda (board)
            (let ((edges (plist-get board :board-edges))
                  (edge `(:id ,edge-id
                          :from ,from :to ,to
                          :label ,(or label "")
                          :style ,(or style "solid")
                          :color ,color
                          :source-handle ,(or source-handle "right")
                          :target-handle ,(or target-handle "left"))))
              (plist-put board :board-edges
                         (cons (cons edge-id edge) edges)))))
        edge-id)))))

(defun supertag-board-remove-edge (board-id edge-id)
  "Remove edge EDGE-ID from BOARD-ID."
  (supertag-board--update board-id
    (lambda (board)
      (plist-put board :board-edges
                 (assoc-delete-all edge-id
                                   (plist-get board :board-edges))))))

(defun supertag-board-update-edge (board-id edge-id changes)
  "Update edge EDGE-ID on BOARD-ID with CHANGES plist.
CHANGES may contain :label, :style, :color keys."
  (supertag-board--update board-id
    (lambda (board)
      (let* ((edges (plist-get board :board-edges))
             (old (cdr (assoc edge-id edges))))
        (when old
          (let ((new-edge (copy-sequence old)))
            (while changes
              (plist-put new-edge (car changes) (cadr changes))
              (setq changes (cddr changes)))
            (plist-put board :board-edges
                       (cons (cons edge-id new-edge)
                             (assoc-delete-all edge-id edges)))))
        board))))

;;; --- Groups ---

(defun supertag-board-add-group (board-id label x y width height
                                          &optional color node-ids)
  "Add a group region to BOARD-ID. Return the group id."
  (let ((group-id (org-id-uuid)))
    (supertag-board--update board-id
      (lambda (board)
        (let ((groups (plist-get board :groups))
              (group `(:id ,group-id
                       :label ,label
                       :x ,x :y ,y
                       :width ,width :height ,height
                       :color ,(or color "#e8f0fe")
                       :node-ids ,(or node-ids '()))))
          (plist-put board :groups
                     (cons (cons group-id group) groups)))))
    group-id))

(defun supertag-board-update-group (board-id group-id changes)
  "Update group GROUP-ID on BOARD-ID with CHANGES plist."
  (supertag-board--update board-id
    (lambda (board)
      (let* ((groups (plist-get board :groups))
             (old (cdr (assoc group-id groups))))
        (when old
          (let ((new-group (copy-sequence old)))
            (while changes
              (plist-put new-group (car changes) (cadr changes))
              (setq changes (cddr changes)))
            (plist-put board :groups
                       (cons (cons group-id new-group)
                             (assoc-delete-all group-id groups)))))
        board))))

(defun supertag-board-remove-group (board-id group-id)
  "Remove group GROUP-ID from BOARD-ID."
  (supertag-board--update board-id
    (lambda (board)
      (plist-put board :groups
                 (assoc-delete-all group-id
                                   (plist-get board :groups))))))

;;; --- Viewport ---

(defun supertag-board-save-viewport (board-id x y zoom)
  "Save viewport state for BOARD-ID."
  (supertag-board--update board-id
    (lambda (board)
      (plist-put board :viewport `(:x ,x :y ,y :zoom ,zoom)))))

(provide 'supertag-board-ops)
;;; supertag-board-ops.el ends here

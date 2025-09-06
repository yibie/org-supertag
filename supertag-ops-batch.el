;;; org-supertag/ops/batch.el --- Batch operations for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides standardized batch operations for entities in the
;; Org-Supertag data-centric architecture. These operations leverage
;; individual entity operations and the core transform mechanism for atomicity.

;;; Code:

(require 'cl-lib)
(require 'supertag-ops-node)    ; For supertag-node-create, supertag-node-update, supertag-node-delete
(require 'supertag-ops-tag)     ; For supertag-tag-create, supertag-tag-update, supertag-tag-delete
(require 'supertag-ops-relation) ; For supertag-relation-create, supertag-relation-update, supertag-relation-delete
(require 'supertag-core-transform)   ; For supertag-with-transaction (if used internally)

;;; --- Batch Operations ---

(defun supertag-batch-create (type data-list)
  "Batch create entities.
TYPE is the entity type (e.g., :node, :tag, :relation).
DATA-LIST is a list of entity data (plists).
Returns a list of created entities."
  (supertag-with-transaction
   (let ((create-fn (pcase type
                       (:node #'supertag-node-create)
                       (:tag #'supertag-tag-create)
                       (:relation #'supertag-relation-create)
                       (_ (error "Unknown entity type for batch create: %s" type))))
         (results '()))
     (dolist (data data-list)
       (push (funcall create-fn data) results))
     (nreverse results))))

(defun supertag-batch-update (type id-updater-list)
  "Batch update entities.
TYPE is the entity type (e.g., :node, :tag, :relation).
ID-UPDATER-LIST is a list of (id . updater-function) pairs.
Returns a list of updated entities."
  (supertag-with-transaction
   (let ((update-fn (pcase type
                       (:node #'supertag-node-update)
                       (:tag #'supertag-tag-update)
                       (:relation #'supertag-relation-update)
                       (_ (error "Unknown entity type for batch update: %s" type))))
         (results '()))
     (dolist (pair id-updater-list)
       (let ((id (car pair))
             (updater (cdr pair)))
         (push (funcall update-fn id updater) results)))
     (nreverse results))))

(defun supertag-batch-delete (type id-list)
  "Batch delete entities.
TYPE is the entity type (e.g., :node, :tag, :relation).
ID-LIST is a list of entity IDs.
Returns a list of deleted entities."
  (supertag-with-transaction
   (let ((delete-fn (pcase type
                       (:node #'supertag-node-delete)
                       (:tag #'supertag-tag-delete)
                       (:relation #'supertag-relation-delete)
                       (_ (error "Unknown entity type for batch delete: %s" type))))
         (results '()))
     (dolist (id id-list)
       (push (funcall delete-fn id) results))
     (nreverse results))))

;; Note: The supertag-with-transaction macro is defined in supertag-transform.el
;; and can be used to wrap any sequence of these operations for atomicity.

(provide 'supertag-ops-batch)
;;; org-supertag/ops/node.el --- Node operations for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Note on Path Format:
;; Functions like `supertag-get` and `supertag-query` expect a LIST of keys
;; for the path argument, even if it's a single top-level collection.
;; For example, to get all nodes, use `'( :nodes )` instead of `:nodes`.
;;
;; This file provides standardized operations for Node entities in the
;; Org-Supertag data-centric architecture. All operations leverage
;; the core transform mechanism and adhere to the defined schema.

;;; Code:

(require 'cl-lib)
(require 'org-id)            ; For org-id-new
(require 'supertag-core-store)    ; For supertag-get, supertag-update
(require 'supertag-core-schema)   ; For validation functions
(require 'supertag-core-transform) ; For supertag-transform

;;; --- Internal Helper ---

(defun supertag--validate-node-data (data)
  "Strict validation for node data. Fails fast on any inconsistency.
Implements immediate error reporting as preferred by the user."
  (unless (plist-get data :id)
    (error "Node missing required :id field: %S" data))
  (unless (plist-get data :title)
    (error "Node missing required :title field: %S" data))
  ;; Validate time format compliance (Emacs native format)
  (when-let ((created-at (plist-get data :created-at)))
    (unless (and (listp created-at) (= (length created-at) 4))
      (error "Node :created-at must use Emacs time format, got: %S" created-at)))
  (when-let ((modified-at (plist-get data :modified-at)))
    (unless (and (listp modified-at) (= (length modified-at) 4))
      (error "Node :modified-at must use Emacs time format, got: %S" modified-at)))
  ;; Validate file path if present
  (when-let ((file (plist-get data :file)))
    (unless (stringp file)
      (error "Node :file must be a string, got: %S" file))))

;; ID generation is now handled by supertag-id-utils.el

;;; --- Node Operations ---

;; 2.1 Basic Operations

(defun supertag-node-create (props)
  "Create a new node using hybrid architecture.
Combines old architecture performance with new architecture safety.
Provides strict validation with immediate error reporting."
  (let* ((id (or (plist-get props :id) (org-id-new)))
         ;; Ensure :file property is always normalized and present
         (file (plist-get props :file))
         (normalized-file (when file (expand-file-name file)))
         (props (if file (plist-put props :file normalized-file) props))
         ;; Build final props with required fields (hybrid approach)
         (final-props (plist-put props :id id))
         (final-props (plist-put final-props :type :node))
         (final-props (plist-put final-props :created-at 
                                 (or (plist-get final-props :created-at) (current-time))))
         (final-props (plist-put final-props :modified-at (current-time))))
    
    (message "DEBUG: supertag-node-create: Adding node with ID: %S, file: %S"
             id (plist-get final-props :file))
    
    ;; Strict validation (fail-fast principle)
    (supertag--validate-node-data final-props)
    
    ;; Direct storage for optimal performance (old-style efficiency)
    (supertag-store-direct-set :nodes id final-props)
    
    final-props))

(defun supertag-node-get (id)
  "Get node data.
ID is the unique identifier of the node.
Returns node data, or nil if it does not exist."
  (supertag-get (list :nodes id)))

(defun supertag-node-update (id updater)
  "Update node data using hybrid architecture.
ID is the unique identifier of the node.
UPDATER is a function that receives the current node data and returns the updated data.
Returns the updated node data."
  (let ((node (supertag-node-get id)))
    (when node
      (let ((updated-node (funcall updater node)))
        (when updated-node
          ;; Add/update timestamps
          (let ((final-node (plist-put updated-node :modified-at (current-time))))
            ;; Strict validation (fail-fast principle)
            (supertag--validate-node-data final-node)
            ;; Direct storage for optimal performance
            (supertag-store-direct-set :nodes id final-node)
            final-node))))))

(defun supertag-node-delete (node-id)
  "Delete a node and all of its relationships from the store.
This operation is atomic and ensures no dangling references remain."
  (when node-id
    (supertag-with-transaction
      (let ((relations-to-delete '()))
        ;; 1. Find all relations involving this node.
        (let ((all-relations (supertag-get '(:relations))))
          (when (hash-table-p all-relations)
            (maphash (lambda (rel-id rel-data)
                       (when (or (equal (plist-get rel-data :from) node-id)
                                 (equal (plist-get rel-data :to) node-id))
                         (push rel-id relations-to-delete)))
                     all-relations)))
        ;; 2. Delete all found relations.
        (dolist (rel-id relations-to-delete)
          (supertag-delete (list :relations rel-id)))
        ;; 3. Delete the node itself.
        (supertag-delete (list :nodes node-id))))))

;; 2.2 Tag Operations

(defun supertag-node-add-tag (node-id tag-id)
  "Add a tag to a node.
NODE-ID is the unique identifier of the node.
TAG-ID is the unique identifier of the tag.
Returns the updated node data."
  (supertag-transform
   (list :nodes node-id :tags)
   (lambda (tags)
     (if (member tag-id (or tags '()))
         (or tags '())  ; Tag already exists, no change
       (cons tag-id (or tags '())))))) ; Add new tag

(defun supertag-node-remove-tag (node-id tag-id)
  "Remove a tag from a node.
NODE-ID is the unique identifier of the node.
TAG-ID is the unique identifier of the tag.
Returns the updated node data."
  (supertag-transform
   (list :nodes node-id :tags)
   (lambda (tags)
     (remove tag-id (or tags '())))))

(defun supertag-node-has-tag-p (node-id tag-id)
  "Check if a node has a specific tag.
NODE-ID is the unique identifier of the node.
TAG-ID is the unique identifier of the tag.
Returns t if the node has the tag, otherwise nil."
  (let ((node (supertag-node-get node-id)))
    (when node
      (let ((tags (plist-get node :tags)))
        (and tags (member tag-id tags))))))

(defun supertag-node-toggle-tag (node-id tag-id)
  "Toggle the tag status of a node.
NODE-ID is the unique identifier of the node.
TAG-ID is the unique identifier of the tag.
If the node has the tag, it is removed; otherwise, it is added.
Returns the updated node data."
  (if (supertag-node-has-tag-p node-id tag-id)
      (supertag-node-remove-tag node-id tag-id)
    (supertag-node-add-tag node-id tag-id)))


;; 2.4 Content Operations (Placeholders for now)

;; (defun supertag-node-set-content (node-id content) ...)
;; (defun supertag-node-get-content (node-id) ...)
;; (defun supertag-node-append-content (node-id content) ...)

(defun supertag-node-set-location (node-id new-file new-position)
 "Update the file path and position for a node in the store.
This is used when a node is moved from one file to another."
 (when-let ((node (supertag-get (list :nodes node-id))))
   (let* ((p-node (plist-put node :file new-file))
          (p-node (plist-put p-node :position new-position)))
     (supertag-update (list :nodes node-id) p-node))))

(defun supertag-node-sync-at-point ()
  "Re-sync the node at point with the database.
Parses the current state of the headline and updates the store."
  (when (org-at-heading-p)
    (let* ((props (supertag--parse-node-at-point)))
      (when props
        (supertag-node-create props)))))
(provide 'supertag-ops-node)

;;; org-supertag/ops/node.el ends here
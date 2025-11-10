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
(require 'org-id)
(require 'supertag-core-store)
(require 'supertag-core-schema)
(require 'supertag-core-persistence)
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
  "Create a new node using the unified commit system.
PROPS is a plist of node properties.
Returns the created node data."
  (let* ((id (or (plist-get props :id) (org-id-new)))
         ;; Build final props with required fields
         (final-props (plist-put props :id id))
         (final-props (plist-put final-props :type :node))
         (final-props (plist-put final-props :created-at
                                 (or (plist-get final-props :created-at) (supertag-current-time))))
         (final-props (plist-put final-props :modified-at (supertag-current-time))))


    ;; (message "DEBUG: supertag-node-create: Creating node with ID: %S, file: %S"
    ;;          id (plist-get final-props :file))

    ;; Use unified commit system
    (supertag-ops-commit
     :operation :create
     :collection :nodes
     :id id
     :previous nil
     :new final-props
     :perform (lambda ()
                (supertag-store-put-entity :nodes id final-props)
                final-props))))

(defun supertag-node-get (id)
  "Get node data.
ID is the unique identifier of the node.
Returns node data, or nil if it does not exist."
  (supertag-store-get-entity :nodes id))

(defun supertag-node-update (id updater)
  "Update node data using the unified commit system.
ID is the unique identifier of the node.
UPDATER is a function that receives the current node data and returns the updated data.
Returns the updated node data."
  (let ((previous (supertag-node-get id)))
    (when previous
      (supertag-ops-commit
       :operation :update
       :collection :nodes
       :id id
       :previous previous
       :perform (lambda ()
                  (let* ((updated-node (funcall updater previous)))
                    (when updated-node
                      (let* ((final-node (plist-put updated-node :modified-at (supertag-current-time)))
                             (final-node (plist-put final-node :type (or (plist-get updated-node :type) (plist-get previous :type)))))
                        (supertag--validate-node-data final-node)
                        (supertag-store-put-entity :nodes id final-node)
                        final-node))))))))

(defun supertag-node-delete (node-id)
  "Delete a node and all of its relationships from the store.
This operation is atomic and ensures no dangling references remain."
  (when node-id
    (let ((previous (supertag-node-get node-id)))
      (when previous
        (supertag-ops-commit
         :operation :delete
         :collection :nodes
         :id node-id
         :previous previous
         :perform (lambda ()
                    ;; Custom deletion logic for relations and fields
                    (let ((relations-table (supertag-store-get-collection :relations))
                          (relations-to-delete '()))
                      ;; Collect relations to delete
                      (maphash
                       (lambda (rel-id rel-data)
                         (let ((relation
                                (if (hash-table-p rel-data)
                                    (let (plist)
                                      (maphash (lambda (k v)
                                                 (setq plist (plist-put plist k v)))
                                               rel-data)
                                      plist)
                                  rel-data)))
                           (when (or (equal (plist-get relation :from) node-id)
                                     (equal (plist-get relation :to) node-id))
                             (push rel-id relations-to-delete))))
                       relations-table)
                      ;; Delete relations
                      (dolist (rel-id relations-to-delete)
                        (supertag-store-remove-entity :relations rel-id)))
                    ;; Remove field values
                    (let ((fields-table (supertag-store-get-collection :fields)))
                      (when (hash-table-p fields-table)
                        (supertag-store-remove-entity :fields node-id)))
                    (supertag-store-remove-entity :nodes node-id)
                    nil))))))

;; 2.2 Tag Operations

(defun supertag-node-add-tag (node-id tag-id)
  "Add a tag to a node.
NODE-ID is the unique identifier of the node.
TAG-ID is the unique identifier of the tag.
Returns the updated node data."
  (supertag-node-update
   node-id
   (lambda (node)
     (when node
       (let* ((tags (plist-get node :tags))
              (present (and tags (member tag-id tags))))
         (unless present
           (let* ((copy (copy-sequence node))
                  (new-tags (cons tag-id (or tags '()))))
             (plist-put copy :tags new-tags))))))))

(defun supertag-node-remove-tag (node-id tag-id)
  "Remove a tag from a node.
NODE-ID is the unique identifier of the node.
TAG-ID is the unique identifier of the tag.
Returns the updated node data."
  (supertag-node-update
   node-id
   (lambda (node)
     (when node
       (let* ((tags (plist-get node :tags))
              (filtered (remove tag-id (or tags '()))))
         (unless (equal filtered tags)
           (let ((copy (copy-sequence node)))
             (plist-put copy :tags filtered))))))))

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
 (when-let ((node (supertag-node-get node-id)))
   (supertag-node-update node-id
     (lambda (n)
       (let* ((p-node (plist-put n :file new-file))
              (p-node (plist-put p-node :position new-position)))
         p-node)))))

(provide 'supertag-ops-node)

;;; org-supertag/ops/node.el ends here

;;; supertag-ops-global-field.el --- Global field operations for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal ops layer for the global field model:
;; - Global field CRUD stored in :field-definitions
;; - Tag/field association list stored in :tag-field-associations
;; - Node/field values stored in :field-values
;; Intended to coexist with legacy tag-scoped model during transition.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'supertag-core-store)
(require 'supertag-core-schema)
(declare-function supertag-tag--normalize-field-def "supertag-ops-tag" (field-def))

(defun supertag--assoc-entry-field-id (entry)
  "Return field-id string from association ENTRY.
ENTRY may be a plist like (:field-id \"refs\" ...) or a bare string \"refs\"."
  (cond
   ((and (listp entry) (plist-member entry :field-id))
    (plist-get entry :field-id))
   ((stringp entry) entry)
   (t nil)))

(defun supertag--normalize-tag-field-associations (entries)
  "Normalize association ENTRIES into a list of plists.
Accepts both legacy string lists and plist entries."
  (let ((result '())
        (idx 0))
    (dolist (entry (or entries '()) (nreverse result))
      (let ((fid (supertag--assoc-entry-field-id entry)))
        (when (and fid (stringp fid) (not (string-empty-p fid)))
          (push (if (and (listp entry) (plist-member entry :field-id))
                    entry
                  (list :field-id fid :order idx))
                result)
          (setq idx (1+ idx)))))))

(defun supertag--dedupe-tag-field-associations (entries)
  "Deduplicate association ENTRIES by :field-id, preserving first occurrence."
  (let ((seen (make-hash-table :test 'equal))
        (result '()))
    (dolist (entry (supertag--normalize-tag-field-associations entries) (nreverse result))
      (let ((fid (plist-get entry :field-id)))
        (unless (gethash fid seen)
          (puthash fid t seen)
          (push entry result))))))

(defun supertag-global-field--normalize (props)
  "Normalize PROPS for global field creation/update."
  (unless (fboundp 'supertag-tag--normalize-field-def)
    (ignore-errors (require 'supertag-ops-tag)))
  (let* ((name (plist-get props :name))
         (id (or (plist-get props :id)
                 (and name (supertag-sanitize-field-id name)))))
    (unless id
      (error "Global field must have :id or :name"))
    (let ((normalized (plist-put props :id id)))
      (supertag-tag--normalize-field-def normalized))))

(defun supertag-global-field-create (props)
  "Create a global field definition using PROPS plist."
  (let* ((field (supertag-global-field--normalize props))
         (id (plist-get field :id)))
    (when (supertag-store-get-field-definition id)
      (error "Global field '%s' already exists" id))
    (supertag-store-put-field-definition id field t)
    (supertag-schema-rebuild-global-field-caches)
    field))

(defun supertag-global-field-get (field-id)
  "Get global field definition by FIELD-ID."
  (supertag-store-get-field-definition field-id))

(defun supertag-global-field-update (field-id updater)
  "Update global field FIELD-ID using UPDATER fn returning new props."
  (let ((previous (supertag-store-get-field-definition field-id)))
    (unless previous
      (error "Global field '%s' not found" field-id))
    (let* ((updated (funcall updater (copy-tree previous)))
           (normalized (supertag-global-field--normalize updated)))
      (supertag-store-put-field-definition field-id normalized t)
      (supertag-schema-rebuild-global-field-caches)
      normalized)))

(defun supertag-global-field-delete (field-id &optional prune-values)
  "Delete global field FIELD-ID. When PRUNE-VALUES, drop node values and associations."
  (let ((previous (supertag-store-get-field-definition field-id)))
    (unless previous
      (error "Global field '%s' not found" field-id))
    (supertag-store-remove-field-definition field-id)
    (when prune-values
      ;; remove from tag associations
      (let ((assoc-table (supertag-store-get-collection :tag-field-associations)))
        (when (hash-table-p assoc-table)
          (maphash
           (lambda (tag-id entries)
             (when (listp entries)
               (let* ((filtered (cl-remove field-id entries
                                           :key (lambda (e) (plist-get e :field-id))
                                           :test #'equal)))
                 (unless (equal filtered entries)
                   (supertag-store-put-tag-field-associations tag-id filtered t)))))
           assoc-table)))
      ;; remove node values
      (let ((vals (supertag-store-get-collection :field-values)))
        (when (hash-table-p vals)
          (maphash
           (lambda (node-id table)
             (when (hash-table-p table)
               (when (ht-contains? table field-id)
                 (remhash field-id table)
                 (supertag-emit-event :store-changed (list :field-values node-id field-id) previous nil))))
           vals))))
    (supertag-schema-rebuild-global-field-caches)
    previous))

(defun supertag-tag-associate-field (tag-id field-id &optional order)
  "Associate FIELD-ID with TAG-ID. ORDER defaults to append."
  (let* ((assoc-table (supertag-store-get-collection :tag-field-associations))
         (raw (gethash tag-id assoc-table))
         (entries (supertag--dedupe-tag-field-associations raw))
         (existing (cl-find field-id entries :key (lambda (e) (plist-get e :field-id)) :test #'equal))
         (next-order (length entries))
         (new-order (or order next-order))
         (entry (list :field-id field-id :order new-order)))
    (cond
     (existing
      ;; Update order if provided
      (when order
        (setf entries (mapcar (lambda (e)
                                (if (equal (plist-get e :field-id) field-id)
                                    (plist-put (copy-sequence e) :order new-order)
                                  e))
                              entries))
        (supertag-store-put-tag-field-associations tag-id entries t)))
     (t
      (supertag-store-put-tag-field-associations tag-id (append entries (list entry)) t)))
    (supertag-schema-rebuild-global-field-caches)
    (gethash tag-id (supertag-store-get-collection :tag-field-associations))))

(defun supertag-tag-disassociate-field (tag-id field-id)
  "Remove association of FIELD-ID from TAG-ID."
  (let* ((raw (supertag-store-get-tag-field-associations tag-id))
         (entries (supertag--dedupe-tag-field-associations raw))
         (filtered (cl-remove field-id entries :key (lambda (e) (plist-get e :field-id)) :test #'equal)))
    (when (listp raw)
      ;; Important: persist even when FILTERED is empty, otherwise last-entry removal is impossible.
      (supertag-store-put-tag-field-associations tag-id filtered t)
      (supertag-schema-rebuild-global-field-caches))
    filtered))

(defun supertag-node-set-global-field (node-id field-id value)
  "Set VALUE for FIELD-ID on NODE-ID using global field storage."
  (supertag-store-put-field-value node-id field-id value t))

(defun supertag-node-get-global-field (node-id field-id &optional default)
  "Get VALUE for FIELD-ID on NODE-ID, or DEFAULT."
  (supertag-store-get-field-value node-id field-id default))

;;; --- Interactive Field Editing with Pre-filled Values ---

(defun supertag-global-field--sanitize-type-input (type-str)
  "Convert TYPE-STR to keyword."
  (let ((sym (intern (concat ":" type-str))))
    (if (memq sym supertag-field-types)
        sym
      :text)))

(defun supertag-global-field-edit-interactive (field-id)
  "Interactively edit existing global field FIELD-ID with pre-filled values.
Prompts for field properties with current values as defaults."
  (interactive
   (list (let ((fields nil))
           (maphash (lambda (id def)
                      (push (cons (format "%s (%s)" id (plist-get def :name)) id)
                            fields))
                    (supertag-store-get-collection :field-definitions))
           (cdr (assoc (completing-read "Select field to edit: " fields nil t)
                      fields)))))
  
  (let* ((current-def (supertag-global-field-get field-id))
         (current-name (plist-get current-def :name))
         (current-type (plist-get current-def :type))
         (current-options (plist-get current-def :options))
         (current-default (plist-get current-def :default)))
    
    (unless current-def
      (error "Field '%s' not found" field-id))
    
    ;; Edit name (with current as default)
    (let ((name (read-string "Field name: " current-name)))
      (if (or (null name) (string-empty-p name))
          (message "Field edit cancelled.")
        
        ;; Edit type (with current as default)
        (let* ((type-keywords (mapcar (lambda (sym) (substring (symbol-name sym) 1)) 
                                      supertag-field-types))
               (current-type-str (substring (symbol-name current-type) 1))
               (type-str (completing-read "Field type: " 
                                          type-keywords 
                                          nil t nil nil current-type-str))
               (type (supertag-global-field--sanitize-type-input type-str))
               (options nil)
               (default nil))
          
          (when type
            ;; Edit options if :options type
            (when (eq type :options)
              (let* ((current-options-str (if current-options 
                                             (string-join current-options ", ")
                                           ""))
                     (options-input (read-string "Options (comma separated): "
                                                current-options-str)))
                (setq options (split-string options-input "," t "[ \t\n\r]+"))))
            
            ;; Edit default value
            (let ((default-input (read-string "Default value (optional): "
                                              (or current-default ""))))
              (unless (string-empty-p default-input)
                (setq default default-input)))
            
            ;; Update the field
            (let ((field-def (list :name name :type type)))
              (when default
                (setq field-def (plist-put field-def :default default)))
              (when (eq type :options)
                (setq field-def (plist-put field-def :options options)))
              
              (supertag-global-field-update field-id (lambda (_) field-def))
              (message "Field '%s' updated successfully!" field-id))))))))

;; Shortcut alias
(defalias 'supertag-edit-field #'supertag-global-field-edit-interactive)

(provide 'supertag-ops-global-field)

;;; supertag-ops-global-field.el ends here

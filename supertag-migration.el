;;; supertag-migration.el --- Standalone data migration script

;;; Commentary:
;; This file contains a self-contained function to migrate data from the
;; old `org-supertag-db.el` format to the new data-centric architecture.
;; It is broken into smaller helper functions to ensure correctness and readability.

;;; Code:

(require 'ht)
(require 'cl-lib)
(require 'org-id)    ; For org-id-new
(require 'sha1)      ; For secure-hash

;;; --- Helper Functions ---

(defun supertag-migrate--sanitize-tag-name (name)
  "Sanitize a string into a valid tag name.
Removes leading/trailing whitespace, a leading '#', and converts
internal whitespace to single underscores."
  (if (or (null name) (string-empty-p name))
      (error "Tag name cannot be empty")
    (let* ((clean-name (substring-no-properties name))
           (trimmed (string-trim clean-name))
           (no-hash (if (string-prefix-p "#" trimmed)
                        (substring trimmed 1)
                      trimmed))
           (sanitized (replace-regexp-in-string "\\s-+" "_" no-hash)))
      (if (string-empty-p sanitized)
          (error "Invalid tag name: %s" name)
        sanitized))))

(defun supertag-migrate--generate-relation-id (from-id to-id type)
  "Generate a deterministic relation ID using SHA1.
Uses the same format as the current system."
  (format "rel-%s" (secure-hash 'sha1 (format "%s|%s|%s" from-id to-id type))))

(defun supertag-migrate--unescape-string (str)
  "Convert escaped octal sequences back to UTF-8 characters.
Input like '\\346\\265\\201\\351\\207\\217' becomes '流量'."
  (when (stringp str)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      ;; Replace octal escape sequences
      (while (re-search-forward "\\\\\\([0-7]\\{3\\}\\)" nil t)
        (let* ((octal-str (match-string 1))
               (char-code (string-to-number octal-str 8))
               (char (if (and (>= char-code 32) (<= char-code 126))
                         ;; ASCII range
                         (char-to-string char-code)
                       ;; For non-ASCII, we need to handle as bytes and decode
                       (char-to-string char-code))))
          (replace-match char)))
      ;; Try to decode the result as UTF-8 if it contains high bytes
      (let ((result (buffer-string)))
        (condition-case nil
            (decode-coding-string result 'utf-8)
          (error result))))))

(defun supertag-migrate--clean-plist-strings (plist)
  "Recursively clean escaped strings in a plist."
  (when plist
    (let ((result '()))
      (while plist
        (let ((key (car plist))
              (value (cadr plist)))
          (push key result)
          (push (cond
                 ((stringp value)
                  (supertag-migrate--unescape-string value))
                 ((listp value)
                  (mapcar (lambda (item)
                            (if (stringp item)
                                (supertag-migrate--unescape-string item)
                              item))
                          value))
                 (t value))
                result)
          (setq plist (cddr plist))))
      (nreverse result))))

(defun supertag-migrate--ensure-node-location-data (node-props)
  "Ensure node has necessary location data for jumping.
Migrates old field names (:file-path -> :file, :pos -> :position) and
returns updated node properties with required location fields."
  (let ((file (plist-get node-props :file))
        (position (plist-get node-props :position))
        (raw-value (plist-get node-props :raw-value))
        (file-path (plist-get node-props :file-path))
        (pos (plist-get node-props :pos)))
    
    ;; Migrate old field names to new ones
    (when (and file-path (not file))
      (setq node-props (plist-put node-props :file file-path))
      (setq file file-path))
    
    (when (and pos (not position))
      (setq node-props (plist-put node-props :position pos))
      (setq position pos))
    
    ;; If we don't have :raw-value but have :title, copy it
    (unless raw-value
      (let ((title (plist-get node-props :title)))
        (when title
          (setq node-props (plist-put node-props :raw-value title)))))
    
    ;; Warn only if we still don't have location data after migration attempt
    (unless file
      (message "Warning: Node missing :file attribute, navigation may not work"))
    (unless position
      (message "Warning: Node missing :position attribute, navigation may not work"))
    
    node-props))

(defun supertag-migrate--safely-load-data (file)
  "Read FILE and extract `org-supertag-db--object` and `org-supertag-db--link`.
This function works by evaluating the entire file in a lexical context
where the hash-table variables are pre-defined."
  (let ((org-supertag-db--object (ht-create))
        (org-supertag-db--link (ht-create))
        (org-supertag-db--embeds (ht-create)))
    (with-temp-buffer
      ;; Force UTF-8 encoding for reading
      (let ((coding-system-for-read 'utf-8-unix)
            (buffer-file-coding-system 'utf-8-unix))
        (insert-file-contents file))
      (set-buffer-file-coding-system 'utf-8-unix)
      (goto-char (point-min))
      (let ((eof (cons 'eof nil)))
        (while (not (eobp))
          (let ((form (condition-case nil
                          (read (current-buffer))
                        (end-of-file eof))))
            (unless (eq form eof)
              ;; Eval each form. This will populate the lexically bound hash tables.
              (eval form t))))))
    (list org-supertag-db--object org-supertag-db--link org-supertag-db--embeds)))

(defun ht-deep-copy-table (table)
  "Create a deep copy of hash TABLE.
This function recursively copies nested hash tables."
  (let ((copy (ht-create)))
    (maphash
     (lambda (key value)
       (puthash key
                (if (hash-table-p value)
                    (ht-deep-copy-table value)
                  value)
                copy))
     table)
    copy))

(defun supertag-migrate--backup-db (file-to-backup)
  "Create a timestamped backup of the specified database file."
  (when (and file-to-backup (file-exists-p file-to-backup))
    (let ((backup-file (format "%s.bak-%s"
                               file-to-backup
                               (format-time-string "%Y%m%d-%H%M%S"))))
      (copy-file file-to-backup backup-file t)
      (message "Old database backed up to: %s" backup-file))))

(defun supertag-migrate--process-data (old-objects old-links old-embeds)
  "Process old data and return a new store and indexes."
  (let ((store (ht-create))
        (nodes-ht (ht-create))
        (tags-ht (ht-create))
        (fields-ht (ht-create))
        (relations-ht (ht-create))
        (embeds-ht (ht-create))
        (id-mapping (ht-create)))  ; Track old-id -> new-id mappings

    (puthash :nodes nodes-ht store)
    (puthash :tags tags-ht store)
    (puthash :fields fields-ht store)
    (puthash :relations relations-ht store)
    (when old-embeds
      (puthash :embeds embeds-ht store))

    ;; Process objects
    (maphash
     (lambda (id props)
       (let ((type (plist-get props :type))
             ;; Clean escaped strings in the properties
             (cleaned-props (supertag-migrate--clean-plist-strings props)))
         (cond ((eq type :node)
                ;; Preserve existing node-id to maintain file consistency
                (let* ((enhanced-props (supertag-migrate--ensure-node-location-data cleaned-props))
                       (final-props (plist-put enhanced-props :id id)))
                  ;; Handle tags stored directly on the node object's :tags property.
                  (let ((old-tags (plist-get final-props :tags)))
                    (if (listp old-tags)
                        ;; If :tags exists and is a list, sanitize the tag names
                        ;; into the new ID format and replace the property.
                        (let ((new-tags-list
                               (delq nil
                                     (mapcar (lambda (tag-name)
                                               (when (and (stringp tag-name) (not (string-empty-p tag-name)))
                                                 (let ((sanitized-tag (supertag-migrate--sanitize-tag-name tag-name)))
                                                   ;; CRITICAL: Ensure a corresponding tag definition exists in the central registry.
                                                   ;; If not, create a minimal one on-demand.
                                                   (unless (gethash sanitized-tag tags-ht)
                                                     (puthash sanitized-tag
                                                              `(:id ,sanitized-tag :name ,tag-name :type :tag)
                                                              tags-ht))
                                                   sanitized-tag)))
                                             old-tags))))
                          (setq final-props (plist-put final-props :tags new-tags-list)))
                      ;; If :tags is not a list or is nil, remove it to ensure clean data.
                      (setq final-props (plist-remove final-props :tags))))
                  (message "Migrating node (preserving ID): %s" id)
                  (puthash id final-props nodes-ht)
                  ;; No ID mapping needed since we preserve the original ID
                  (puthash id id id-mapping)))
               ((eq type :tag)
                ;; Convert old tag-id to semantic name format
                (let* ((tag-name (or (plist-get cleaned-props :name) id))
                       (new-tag-id (supertag-migrate--sanitize-tag-name tag-name))
                       (final-props (plist-put cleaned-props :id new-tag-id)))
                  (message "Migrating tag: %s -> %s" id new-tag-id)
                  (puthash new-tag-id final-props tags-ht)
                  ;; Store mapping for updating relations later
                  (puthash id new-tag-id id-mapping)))
               (t (message "Skipping unknown object type: %s" type)))))
     old-objects)

    ;; Process embeds if they exist
    (when old-embeds
      (maphash
       (lambda (id props)
         (puthash id props embeds-ht))
       old-embeds))

    ;; Process links
    (maphash
     (lambda (link-id link-props)
       (let* ((type (plist-get link-props :type))
              (from (plist-get link-props :from))
              (to (plist-get link-props :to)))
         (pcase type
           (:node-tag
            ;; Map old IDs to new IDs
            (let* ((new-from (gethash from id-mapping from))
                   (new-to (gethash to id-mapping to))
                   (node-data (gethash new-from nodes-ht)))
              ;; Ensure both node and tag exist before creating the link.
              (when (and node-data new-to)
                (puthash new-from
                         (plist-put node-data :tags (cl-adjoin new-to (plist-get node-data :tags)))
                         nodes-ht))))
           (:node-field
            ;; Map old IDs to new IDs
            (let* ((new-from (gethash from id-mapping from))
                   (old-tag-id (plist-get link-props :tag-id))
                   (new-tag-id (gethash old-tag-id id-mapping))
                   (value (plist-get link-props :value)))
              ;; Ensure the node, the associated tag, and the value all exist.
              (when (and new-from to new-tag-id value)
                (let ((node-fields (gethash new-from fields-ht (ht-create))))
                  (puthash new-from node-fields fields-ht)
                  (let ((tag-fields (gethash new-tag-id node-fields (ht-create))))
                    (puthash new-tag-id tag-fields node-fields)
                    (puthash to value tag-fields))))))
           (:node-ref
            ;; Map old IDs to new IDs
            (let* ((new-from (gethash from id-mapping from))
                   (new-to (gethash to id-mapping to))
                   (node-data (gethash new-from nodes-ht)))
              (when (and node-data new-to)
                (puthash new-from
                         (plist-put node-data :refs (cl-adjoin new-to (plist-get node-data :refs)))
                         nodes-ht))))
           (_
            ;; Map old IDs to new IDs for relations
            (let* ((new-from (gethash from id-mapping from))
                   (new-to (gethash to id-mapping to)))
              ;; Only create the relation if both endpoints were successfully migrated.
              (when (and new-from new-to)
                (let* ((new-rel-id (supertag-migrate--generate-relation-id new-from new-to type))
                       (updated-props (plist-put (plist-put link-props :from new-from) :to new-to)))
                  (message "Migrating relation: %s -> %s" link-id new-rel-id)
                  (puthash new-rel-id updated-props relations-ht))))))))
     old-links)

    ;; Return the new store
    store))

(defun supertag-migrate--build-indexes (store)
  "Build all performance indexes from the STORE."
  (unless (hash-table-p store)
    (error "Invalid store: expected hash table, got %s" (type-of store)))
  
  (let ((indexes (ht-create))
        (nodes-ht (gethash :nodes store)))
    
    ;; Validate nodes-ht
    (unless (hash-table-p nodes-ht)
      (error "Invalid nodes hash table: expected hash table, got %s" (type-of nodes-ht)))
    
    (puthash :tags (ht-create) indexes)
    (puthash :words (ht-create) indexes)
    (puthash :dates (ht-create) indexes)
    
    (let ((tag-idx (gethash :tags indexes))
          (word-idx (gethash :words indexes))
          (date-idx (gethash :dates indexes)))
        
      ;; Only proceed if nodes-ht is valid and not empty
      (when (and (hash-table-p nodes-ht) (> (hash-table-count nodes-ht) 0))
        (maphash
         (lambda (node-id node-data)
           ;; 1. Build tag index
           (let ((tags (plist-get node-data :tags)))
             (when tags
               (dolist (tag tags)
                 (when tag
                   (let ((nodes-list (gethash tag tag-idx '())))
                     (unless (member node-id nodes-list)
                       (puthash tag (cons node-id nodes-list) tag-idx)))))))
           
           ;; 2. Build word index from title and content
           (dolist (text (list (plist-get node-data :title)
                               (plist-get node-data :content)))
             (when (stringp text)
               (dolist (word (split-string (downcase text) "[^[:word:]]+" t))
                 (when (> (length word) 2)
                   (let ((nodes-list (gethash word word-idx '())))
                     (unless (member node-id nodes-list)
                       (puthash word (cons node-id nodes-list) word-idx)))))))
           
           ;; 3. Build date index
           (let ((created (plist-get node-data :created-at))
                 (modified (plist-get node-data :modified-at)))
             (when created
               (let ((date-map (gethash :created-at date-idx (ht-create))))
                 (puthash :created-at date-map date-idx)
                 (puthash node-id created date-map)))
             (when modified
               (let ((date-map (gethash :modified-at date-idx (ht-create))))
                 (puthash :modified-at date-map date-idx)
                 (puthash node-id modified date-map)))))
         nodes-ht)))
    indexes))

(defun supertag-migrate--save-new-db (store indexes)
  "Save the new STORE and INDEXES to the conventional file location."
  (let* ((default-dir (expand-file-name "org-supertag" user-emacs-directory))
         (new-db-file (expand-file-name "supertag-db.el" default-dir)))
    (make-directory default-dir t)
    (message "Saving new database to %s..." new-db-file)
    (with-temp-file new-db-file
      ;; Use the most robust method to ensure UTF-8 output, by controlling
      ;; the buffer-local coding system, the file-writing coding system,
      ;; and the printer's escaping behavior.
      (let ((buffer-file-coding-system 'utf-8-unix)
            (coding-system-for-write 'utf-8-unix)
            (coding-system-for-read 'utf-8-unix)
            (print-escape-nonascii nil)
            (print-escape-multibyte nil)
            (print-escape-control-characters nil)
            (print-quoted-char-oneline nil)
            (print-escape-newlines nil)
            (print-continuous-numbering nil)
            (print-gensym nil))
        (setq-local buffer-file-coding-system 'utf-8-unix)
        (setq-local coding-system-for-write 'utf-8-unix)
        (let ((print-level nil)
              (print-length nil)
              (print-circle nil)
              (print-escape-nonascii nil)
              (print-escape-multibyte nil)
              (print-escape-control-characters nil)
              (print-quoted-char-oneline nil)
              (print-escape-newlines nil)
              (print-continuous-numbering nil)
              (print-gensym nil))
          (insert ";;; org-supertag.db --- Data store for org-supertag\n")
          (insert ";;; -*- coding: utf-8 -*-\n\n")
          ;; Output store data directly as hash table (compatible with supertag-store.el)
          (insert ";; supertag--store data\n")
          (prin1 (ht-deep-copy-table store) (current-buffer))
          (insert "\n\n")
          ;; Output index data directly as hash table
          (insert ";; supertag--store-indexes data\n")
          (prin1 (ht-deep-copy-table indexes) (current-buffer))
          (insert "\n"))))
    (message "Data migration successfully completed!")
    (message "New database file is at: %s" new-db-file))) 

;;;###autoload
(defun supertag-migrate-database-to-new-arch ()
  "Interactively migrate an old org-supertag-db.el file to the new data-centric store."
  (interactive)
  (let ((old-db-file (read-file-name "Select your old org-supertag-db.el file: ")))
    (unless (and old-db-file (file-exists-p old-db-file))
      (user-error "Migration cancelled: File not found."))

    (when (yes-or-no-p (format "Migrate from %s? (A backup will be created)" old-db-file))
      (message "Starting self-contained migration...")
      (supertag-migrate--backup-db old-db-file)

      (message "Safely loading data from %s..." old-db-file)
      (let* ((old-data (supertag-migrate--safely-load-data old-db-file))
             (old-objects (car old-data))
             (old-links (cadr old-data))
             (old-embeds (nth 2 old-data)))

        (message "Loaded %d objects and %d links."
                 (hash-table-count old-objects)
                 (hash-table-count old-links))

        (message "Processing data into new format...")
        (let* ((new-store (supertag-migrate--process-data old-objects old-links old-embeds))
               (new-indexes (supertag-migrate--build-indexes new-store)))

          (message "Saving new database to file...")
          (supertag-migrate--save-new-db new-store new-indexes))))))

(provide 'supertag-migration)

;;; supertag-migration.el ends here

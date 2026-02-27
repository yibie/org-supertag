;;; supertag-migration.el --- Standalone data migration script  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains a self-contained function to migrate data from the
;; old `org-supertag-db.el` format to the new data-centric architecture.
;; It is broken into smaller helper functions to ensure correctness and readability.

;;; Code:

(require 'ht)
(require 'cl-lib)
(require 'org-id)    ; For org-id-new
(require 'sha1)      ; For secure-hash
(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'supertag-core-store)
(require 'supertag-core-schema)
(require 'supertag-core-persistence)
(require 'supertag-ops-node)
(require 'supertag-view-helper)
(require 'org-id)
(require 'org)
(require 'org-element)

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

;; ------------------------------------------------------------------
;; Legacy :tag: migration (optional utility)
;; ------------------------------------------------------------------

(defun supertag--detect-headline-tag-style (headline)
  "Detect tag style used on HEADLINE element. Returns 'inline, 'org, 'both or 'none."
  (let* ((raw (org-element-property :raw-value headline))
         (org-tags (org-element-property :tags headline))
         (has-inline (and raw (string-match-p "#[^[:space:]#]+" raw)))
         (has-org (and org-tags (> (length org-tags) 0))))
    (cond
     ((and has-inline has-org) 'both)
     (has-inline 'inline)
     (has-org 'org)
     (t 'none))))

(defun supertag--rewrite-headline-tags (headline style)
  "Rewrite HEADLINE tags to STYLE. Returns plist (:changedp t :begin BEG :end END) or nil."
  (let* ((beg (org-element-property :begin headline))
         (end (save-excursion (goto-char beg) (end-of-line) (point)))
         (level (org-element-property :level headline))
         (title (org-element-property :raw-value headline))
         (tags (org-element-property :tags headline))
         (clean-title (string-trim (replace-regexp-in-string ":[[:alnum:]_@#%]+:" ""
                                       (replace-regexp-in-string "#[^[:space:]#]+" "" title))))
         (inline-part (when tags (mapconcat (lambda (tag) (concat "#" tag)) tags " ")))
         (org-part (when tags (concat ":" (mapconcat #'identity tags ":") ":")))
         (new-line (pcase style
                     ('inline (format "%s %s%s"
                                      (make-string level ?*) clean-title
                                      (if inline-part (concat " " inline-part) "")))
                     ('org    (format "%s %s%s"
                                      (make-string level ?*) clean-title
                                      (if org-part (concat " " org-part) "")))
                     ('both   (format "%s %s%s%s"
                                      (make-string level ?*) clean-title
                                      (if inline-part (concat " " inline-part) "")
                                      (if org-part (concat " " org-part) "")))
                     (_ nil))))
    (when new-line
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert new-line))
      (list :changedp t :begin beg :end (save-excursion (goto-char beg) (end-of-line) (point))))))

(defun supertag-migration--backup-file (file)
  "Create a timestamped backup for FILE. Return backup path."
  (let* ((backup (format "%s.bak-%s" file (format-time-string "%Y%m%d-%H%M%S"))))
    (copy-file file backup t)
    backup))

(defun supertag-migration--restore-backup (file backup)
  "Restore FILE from BACKUP. Return t on success."
  (when (and (file-exists-p backup))
    (copy-file backup file t)
    t))

(defun supertag-migrate-legacy-tags-file (file &optional dry-run)
  "Migrate org native :tag: in FILE to inline #tags. Returns report plist.
When DRY-RUN is non-nil, do not modify the file; only report changes."
  (unless (file-exists-p file)
    (error "File not found: %s" file))
  (let ((changed 0) (errors '()) (backups '()) (headlines 0))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (let ((ast (org-element-parse-buffer)))
          (org-element-map ast 'headline
            (lambda (hl)
              (setq headlines (1+ headlines))
              (let ((style (supertag--detect-headline-tag-style hl)))
                (when (memq style '(org both))
                  (condition-case err
                      (unless dry-run
                        ;; Ensure single backup per file
                        (unless (assoc file backups)
                          (push (cons file (supertag-migration--backup-file file)) backups))
                        (when (supertag--rewrite-headline-tags hl 'inline)
                          (setq changed (1+ changed))))
                    (error (push (format "%s" err) errors))))))
          (unless dry-run (save-buffer)))))
    (list :file file :changed changed :headlines headlines :backups (mapcar #'cdr backups) :errors errors))))

(defun supertag-migrate-legacy-tags-directory (dir &optional dry-run)
  "Migrate all .org files in DIR (recursively). Returns report plist."
  (let ((files (directory-files-recursively dir "\\.org$" t))
        (total 0) (changed 0) (errors '()) (reports '()) (backups '()))
    (dolist (f files)
      (setq total (1+ total))
      (let ((r (supertag-migrate-legacy-tags-file f dry-run)))
        (setq changed (+ changed (plist-get r :changed)))
        (setq reports (cons r reports))
        (setq backups (append backups (plist-get r :backups)))
        (setq errors (append errors (plist-get r :errors)))))
    (list :files (length files) :changed changed :reports (nreverse reports) :backups backups :errors errors)))

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
                                                   ;; If not, create a minimal one on-demand, preserving any existing fields.
                                                   (unless (gethash sanitized-tag tags-ht)
                                                     (puthash sanitized-tag
                                                              `(:id ,sanitized-tag
                                                                :name ,tag-name
                                                                :type :tag
                                                                :fields nil
                                                                :extends nil)
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
                       ;; Preserve existing fields and extends, or set to nil if not present
                       (fields (plist-get cleaned-props :fields))
                       (extends (plist-get cleaned-props :extends))
                       (final-props (plist-put
                                    (plist-put
                                     (plist-put cleaned-props :id new-tag-id)
                                     :fields (or fields nil))
                                    :extends (or extends nil))))
                  (message "Migrating tag: %s -> %s (fields: %s, extends: %s)"
                          id new-tag-id
                          (if fields "yes" "no")
                          (if extends "yes" "no"))
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

;; ------------------------------------------------------------------
;; Global field model migration (modern store -> global fields)
;; ------------------------------------------------------------------

(defcustom supertag-migration-log-buffer "*supertag-migration*"
  "Buffer name for migration logs."
  :type 'string
  :group 'supertag-migration)

(defcustom supertag-migration-dry-run t
  "When non-nil, migration commands default to dry-run (no writes)."
  :type 'boolean
  :group 'supertag-migration)

(defvar supertag-migration--stats nil
  "Plist of migration stats for the current run.")

(defun supertag-migration--log (fmt &rest args)
  "Log formatted message to `supertag-migration-log-buffer'."
  (with-current-buffer (get-buffer-create supertag-migration-log-buffer)
    (goto-char (point-max))
    (insert (apply #'format (concat fmt "\n") args))))

(defun supertag-migration--reset-stats ()
  "Reset migration stats."
  (setq supertag-migration--stats
        '(:fields-created 0
          :associations-created 0
          :values-migrated 0
          :conflicts nil
          :skipped 0))
  (supertag-migration--log "Stats reset: %S" supertag-migration--stats))

(defun supertag-migration--increment (key &optional delta)
  "Increment KEY in stats by DELTA (default 1)."
  (let* ((delta (or delta 1))
         (current (plist-get supertag-migration--stats key)))
    (setq supertag-migration--stats
          (plist-put supertag-migration--stats key (+ (or current 0) delta)))))

(defun supertag-migration--record-conflict (item)
  "Record conflict ITEM in stats."
  (let* ((entry (if (and item (listp item))
                    item
                  (list :reason :unspecified :raw item)))
         (existing (plist-get supertag-migration--stats :conflicts))
         (updated (cons entry existing)))
    (setq supertag-migration--stats
          (plist-put supertag-migration--stats :conflicts updated))
    (supertag-migration--log "Conflict recorded entry=%S updated=%S" entry updated)))

(defun supertag-migration--dry-run-p (&optional force)
  "Return t when dry-run is active, unless FORCE is non-nil."
  (and (not force)
       (or supertag-migration-dry-run
           (bound-and-true-p current-prefix-arg))))

(defun supertag-migration--ensure-flag ()
  "Ensure global field model is enabled for migration."
  (unless supertag-use-global-fields
    (error "Set `supertag-use-global-fields' to t before running migration")))

(defun supertag-migration--compare-field-defs (a b)
  "Return non-nil when field definitions A and B are compatible.
Compares :type and :config/ :options, ignoring ordering of plist."
  (and (eq (plist-get a :type) (plist-get b :type))
       (equal (plist-get a :config) (plist-get b :config))
       (equal (plist-get a :options) (plist-get b :options))))

(defun supertag-migration--sanitize-field-id (field-name)
  "Return sanitized field id for FIELD-NAME or nil."
  (supertag-sanitize-field-id field-name))

(defun supertag-migration--collect-tag-fields ()
  "Return list of (TAG-ID . FIELD-PLISTS) from legacy tag definitions."
  (let ((tags (supertag-store-get-collection :tags))
        result)
    (when (hash-table-p tags)
      (maphash
       (lambda (tag-id tag-data)
         (let* ((plist (cond
                        ((hash-table-p tag-data)
                         (let (p)
                           (maphash (lambda (k v) (setq p (plist-put p k v))) tag-data)
                           p))
                        ((listp tag-data) tag-data)
                        (t nil)))
                (fields (plist-get plist :fields)))
           (when (listp fields)
             (push (cons tag-id fields) result))))
       tags))
    (nreverse result)))

(defun supertag-migration--migrate-field-definitions (dry-run)
  "Create global field definitions from tag field specs. Respects DRY-RUN.
Returns a hash-table of field-id -> definition (includes newly collected
defs even in dry-run)."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (entry (supertag-migration--collect-tag-fields))
      (let ((fields (cdr entry)))
        (dolist (field fields)
          (let* ((name (plist-get field :name))
                 (fid (and name (supertag-migration--sanitize-field-id name))))
            (when fid
              (let* ((existing (gethash fid seen))
                     (payload (plist-put (copy-tree field) :id fid)))
                (if existing
                    (unless (supertag-migration--compare-field-defs existing payload)
                      (supertag-migration--record-conflict
                       (list :field fid :reason :type-mismatch
                             :existing existing :incoming payload)))
                  (puthash fid payload seen))))))))
    (maphash
     (lambda (fid def)
       (let ((already (supertag-store-get-field-definition fid)))
         (cond
          ((and already (supertag-migration--compare-field-defs already def))
           (supertag-migration--increment :skipped))
          ((and already (not (supertag-migration--compare-field-defs already def)))
           (supertag-migration--record-conflict
            (list :field fid :reason :store-mismatch :existing already :incoming def)))
          (t
           (unless dry-run
             (supertag-store-put-field-definition fid def t))
           (supertag-migration--increment :fields-created)))))
     seen)
    seen))

(defun supertag-migration--migrate-tag-associations (dry-run)
  "Create tag→field ordered associations. Respects DRY-RUN."
  (dolist (entry (supertag-migration--collect-tag-fields))
    (let* ((tag-id (car entry))
           (fields (cdr entry))
           (assoc-list '())
           (order 0))
      (dolist (field fields)
        (let* ((name (plist-get field :name))
               (fid (and name (supertag-migration--sanitize-field-id name))))
          (when fid
            (push (list :field-id fid :order order) assoc-list)
            (setq order (1+ order)))))
      (setq assoc-list (nreverse assoc-list))
      (when assoc-list
        (let ((existing (supertag-store-get-tag-field-associations tag-id)))
          (cond
           ((equal existing assoc-list)
            (supertag-migration--increment :skipped))
           (t
            (unless dry-run
              (supertag-store-put-tag-field-associations tag-id assoc-list t))
            (supertag-migration--increment :associations-created
                                           (length assoc-list)))))))))

(defun supertag-migration--migrate-field-values (dry-run defs-table)
  "Rewrite legacy node→tag→field values into node→field values. Respects DRY-RUN.
DEFS-TABLE is hash of known field definitions (from store plus newly collected)."
  (let ((root (supertag-store-get-collection :fields)))
    (when (hash-table-p root)
      (maphash
       (lambda (node-id tag-table)
         (when (hash-table-p tag-table)
           (maphash
            (lambda (_tag-id field-table)
              (when (hash-table-p field-table)
                (maphash
                 (lambda (fname value)
                   (let* ((fid (supertag-migration--sanitize-field-id fname))
                          (fdef (and fid (gethash fid defs-table))))
                     (cond
                      ((null fid)
                       (supertag-migration--record-conflict
                        (list :node node-id :field fname :reason :no-id)))
                      ((not fdef)
                       ;; During dry-run, skip missing-definition conflicts since defs not written.
                       (unless dry-run
                         (supertag-migration--record-conflict
                          (list :node node-id :field fid :reason :missing-definition))))
                      (t
                       (let ((existing (supertag-store-get-field-value node-id fid supertag--not-found)))
                         (cond
                          ((not (eq existing supertag--not-found))
                           (supertag-migration--increment :skipped))
                          (t
                           (unless dry-run
                             (supertag-store-put-field-value node-id fid value t))
                           (supertag-migration--increment :values-migrated))))))))
                 field-table)))
            tag-table)))
       root))))

(defun supertag-migration--report (&optional dry-run)
  "Summarize migration run. Annotate DRY-RUN status."
  (let ((fields (plist-get supertag-migration--stats :fields-created))
        (assocs (plist-get supertag-migration--stats :associations-created))
        (vals (plist-get supertag-migration--stats :values-migrated))
        (conflicts (plist-get supertag-migration--stats :conflicts))
        (skipped (plist-get supertag-migration--stats :skipped)))
    (supertag-migration--log "Migration summary (dry-run=%s): fields=%d associations=%d values=%d skipped=%d conflicts=%d"
                             (if dry-run "yes" "no") fields assocs vals skipped (length conflicts))
    (when conflicts
      (supertag-migration--log "Conflicts: %S" conflicts))
    ;; Detailed conflict listing for debugging
    (when conflicts
      (let ((idx 0))
        (dolist (c conflicts)
          (setq idx (1+ idx))
          (supertag-migration--log "Conflict[%d]: %S" idx c))))
    (supertag-migration--log "Stats raw: %S" supertag-migration--stats)
    (message "Supertag migration finished (dry-run=%s): fields=%d associations=%d values=%d skipped=%d conflicts=%d"
             (if dry-run "yes" "no") fields assocs vals skipped (length conflicts))))

;;;###autoload
(defun supertag-migration-run-global-fields (&optional force-write)
  "Run migration to global field model.
With FORCE-WRITE non-nil (or prefix arg), perform writes; otherwise dry-run."
  (interactive "P")
  (supertag-migration--ensure-flag)
  (supertag-migration--reset-stats)
  (let* ((dry-run (supertag-migration--dry-run-p force-write))
         (defs-table nil))
    (with-current-buffer (get-buffer-create supertag-migration-log-buffer)
      (erase-buffer))
    (supertag-migration--log "--- Supertag global field migration start (dry-run=%s) ---"
                             (if dry-run "yes" "no"))
    (unless dry-run
      (supertag-migration--log "Reminder: ensure a fresh backup exists before applying migration.")
      (message "[supertag] Applying migration (not a dry-run). Consider running `supertag-backup-database-now` first."))
    (setq defs-table (supertag-migration--migrate-field-definitions dry-run))
    (supertag-migration--log "Stats after field definitions: %S" supertag-migration--stats)
    (supertag-migration--migrate-tag-associations dry-run)
    (supertag-migration--log "Stats after associations: %S" supertag-migration--stats)
    (supertag-migration--migrate-field-values dry-run defs-table)
    (supertag-migration--log "Stats after values: %S" supertag-migration--stats)
    (supertag-migration--report dry-run)))

;; ------------------------------------------------------------------
;; Org Properties → SuperTag Fields Migration (Simplified Version)
;; ------------------------------------------------------------------

(defun supertag-migration--collect-all-properties ()
  "Collect all org properties from existing nodes in the database.
Returns a hash table: property-name -> (list of (node-id . value) pairs).
This function reads from the already-synced :properties field in nodes,
avoiding the need to re-parse org files."
  (let ((props-table (make-hash-table :test 'equal))
        (nodes-table (supertag-store-get-collection :nodes)))
    (when (hash-table-p nodes-table)
      (maphash
       (lambda (node-id node-data)
         (let ((props (plist-get node-data :properties)))
           ;; properties can be a plist: (:KEY "value" ...) or alist: ((:KEY . "value") ...)
           (when (and props (listp props))
             (if (plistp props)
                 ;; Handle plist format
                 (let ((plist props))
                   (while plist
                     (let* ((key (car plist))
                            (value (cadr plist)))
                       (when (keywordp key)
                         (let* ((key-name (substring (symbol-name key) 1))
                                (existing (gethash key-name props-table)))
                           ;; Skip org-mode internal properties
                           (unless (member (upcase key-name) '("ID" "CUSTOM_ID" "CATEGORY"))
                             (puthash key-name
                                      (cons (cons node-id value) existing)
                                      props-table))))
                       (setq plist (cddr plist)))))
               ;; Handle alist format (legacy compatibility)
               (dolist (prop props)
                 (when (and (consp prop) (car prop))
                   (let* ((key (car prop))
                          (key-name (if (keywordp key)
                                        (substring (symbol-name key) 1)
                                      (format "%s" key)))
                          (value (cdr prop))
                          (existing (gethash key-name props-table)))
                     ;; Skip org-mode internal properties
                     (unless (member (upcase key-name) '("ID" "CUSTOM_ID" "CATEGORY"))
                       (puthash key-name
                                (cons (cons node-id value) existing)
                                props-table)))))))))
       nodes-table))
    props-table))

(defun supertag-migration--property-stats (props-table)
  "Generate statistics from PROPS-TABLE.
Returns a sorted list of (property-name count sample-values) tuples."
  (let ((stats '()))
    (maphash
     (lambda (prop-name occurrences)
       (let* ((count (length occurrences))
              ;; Get up to 3 sample values
              (samples (cl-subseq occurrences 0 (min 3 count)))
              (sample-values (mapcar #'cdr samples)))
         (push (list prop-name count sample-values) stats)))
     props-table)
    ;; Sort by count descending
    (sort stats (lambda (a b) (> (nth 1 a) (nth 1 b))))))

;;;###autoload
(defun supertag-analyze-org-properties ()
  "Analyze org properties in the database and display a report.
This is a read-only operation - it does not modify any data.
Shows which properties exist and how often they appear."
  (interactive)
  (let* ((props-table (supertag-migration--collect-all-properties))
         (stats (supertag-migration--property-stats props-table))
         (buf (get-buffer-create "*Supertag Property Analysis*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Org Properties Analysis ===\n\n")
      (if (null stats)
          (insert "No org properties found in the database.\n\n")
        (insert (format "Found %d unique property names across nodes.\n\n"
                        (length stats)))
        (insert "Property Name            | Count | Sample Values\n")
        (insert "-------------------------|-------|---------------------------\n")
        (dolist (stat stats)
          (let ((name (nth 0 stat))
                (count (nth 1 stat))
                (samples (nth 2 stat)))
            (insert (format "%-24s | %5d | %s\n"
                            (if (> (length name) 24)
                                (concat (substring name 0 21) "...")
                              name)
                            count
                            (mapconcat (lambda (v)
                                         (let ((s (format "%s" v)))
                                           (if (> (length s) 15)
                                               (concat (substring s 0 12) "...")
                                             s)))
                                       samples ", "))))))
      (insert "\n\nTo convert a property to a tag field, use:\n")
      (insert "  M-x supertag-convert-properties-to-field\n"))
    (display-buffer buf)
    (message "Found %d unique properties. See *Supertag Property Analysis* buffer."
             (length stats))))

(defun supertag-migration--nodes-with-property (props-table property-name)
  "Get list of (node-id . value) pairs for PROPERTY-NAME from PROPS-TABLE."
  (gethash property-name props-table))

(defun supertag-migration--infer-field-type (values)
  "Infer the most likely field type from a list of VALUES.
Returns a keyword like :string, :number, :date, etc."
  (let ((sample-values (cl-remove-if-not #'stringp
                                         (mapcar #'cdr (cl-subseq values 0 (min 10 (length values)))))))
    (cond
     ;; Check for date patterns
     ((cl-every (lambda (v)
                  (or (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" v)
                      (string-match-p "^<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" v)))
                sample-values)
      :date)
     ;; Check for numbers
     ((cl-every (lambda (v)
                  (string-match-p "^-?[0-9]+\\(?:\\.[0-9]+\\)?$" v))
                sample-values)
      :number)
     ;; Check for boolean-like values
     ((cl-every (lambda (v)
                  (member (downcase v) '("yes" "no" "true" "false" "t" "nil" "1" "0")))
                sample-values)
      :boolean)
     ;; Default to string
     (t :string))))

;;;###autoload
(defun supertag-convert-properties-to-field (property-name)
  "Convert an org property to a tag field.
PROPERTY-NAME is the name of the org property to convert.

This function:
1. Prompts for a tag (existing or new) to associate with
2. Creates the tag if it doesn't exist
3. Creates a field definition on the tag
4. For each node that has both the property AND the tag,
   copies the property value to the field value
5. Inserts #tag in the org file for nodes that didn't have the tag"
  (interactive
   (let* ((props-table (supertag-migration--collect-all-properties))
          (prop-names (let ((names '()))
                        (maphash (lambda (k _v) (push k names)) props-table)
                        (sort names #'string<))))
     (unless prop-names
       (user-error "No org properties found in the database"))
     (list (completing-read "Property to convert: " prop-names nil t))))

  ;; Validate property name
  (unless (and property-name (not (string-empty-p property-name)))
    (user-error "Property name is required"))

  ;; Prompt for tag (allow creating new ones)
  (let* ((all-tags (mapcar #'car (supertag-query :tags)))
         (tag-input (completing-read
                     (format "Associate '%s' with tag (or enter new tag name): " property-name)
                     all-tags nil nil))
         (tag-id (supertag-sanitize-tag-name tag-input)))

    (unless (and tag-id (not (string-empty-p tag-id)))
      (user-error "Tag name is required"))

    ;; Create tag if it doesn't exist
    (unless (supertag-tag-get tag-id)
      (if (yes-or-no-p (format "Tag '%s' doesn't exist. Create it? " tag-id))
          (progn
            (supertag-tag-create `(:id ,tag-id :name ,tag-id))
            (message "Created tag '%s'" tag-id))
        (user-error "Tag '%s' was not created" tag-id)))

    ;; Now do the conversion
    (let* ((props-table (supertag-migration--collect-all-properties))
           (occurrences (supertag-migration--nodes-with-property props-table property-name))
           (field-type (supertag-migration--infer-field-type occurrences))
           (field-id (supertag-sanitize-field-id property-name))
           (values-set 0)
           (tags-added 0)
           (nodes-skipped 0))

      (unless occurrences
        (user-error "Property '%s' not found in any nodes" property-name))

      ;; Show confirmation with inferred type
      (unless (yes-or-no-p
               (format "Convert property '%s' (found in %d nodes) to field type '%s' on tag '%s'? "
                       property-name (length occurrences) field-type tag-id))
        (user-error "Conversion cancelled"))

      ;; Step 1: Create or update field definition on the tag
      (message "Creating field '%s' (type: %s) on tag '%s'..."
               property-name field-type tag-id)

      (let ((field-def `(:name ,property-name
                         :id ,field-id
                         :type ,field-type)))
        ;; Use supertag-tag-add-field which handles both global and legacy models
        (supertag-tag-add-field tag-id field-def))

      ;; Step 2: For each node with this property, set the field value
      ;; but ONLY if the node also has the specified tag
      (message "Migrating property values to field values...")

      (dolist (occurrence occurrences)
        (let* ((node-id (car occurrence))
               (value (cdr occurrence))
               (node (supertag-node-get node-id)))
          (if node
              (let* ((node-tags (plist-get node :tags))
                     (has-tag (and node-tags (member tag-id node-tags))))
                (unless has-tag
                  ;; Add tag to database
                  (let ((updated (supertag-node-add-tag node-id tag-id)))
                    (when updated
                      (setq node updated
                            node-tags (plist-get updated :tags)
                            has-tag (and node-tags (member tag-id node-tags)))
                      ;; Also insert #tag in the org file
                      (supertag-migration--insert-tag-in-org-file node-id tag-id)
                      (cl-incf tags-added))))
                (if has-tag
                    (progn
                      (supertag-field-set node-id tag-id property-name value)
                      (cl-incf values-set))
                  (cl-incf nodes-skipped)))
            (cl-incf nodes-skipped))))

      ;; Report results
      (message "Conversion complete on tag '%s': %d values migrated, %d tags added automatically (in DB and org files), %d nodes skipped (missing node data or failed tag assignment)"
               tag-id values-set tags-added nodes-skipped)

      (list :property property-name
            :tag tag-id
            :field-id field-id
            :field-type field-type
            :values-set values-set
            :tags-added tags-added
            :nodes-skipped nodes-skipped))))

;;;###autoload
(defun supertag-batch-convert-properties-to-fields ()
  "Batch convert multiple properties to fields, one at a time.
For each selected property:
1. Shows property statistics
2. Prompts for a tag (existing or new)
3. Converts the property to a field on that tag
4. Continues to next property until all are done or user cancels."
  (interactive)
  (let* ((props-table (supertag-migration--collect-all-properties))
         (prop-names (let ((names '()))
                       (maphash (lambda (k _v) (push k names)) props-table)
                       (sort names #'string<)))
         (selected-props (completing-read-multiple
                          "Select properties to convert (comma-separated): "
                          prop-names nil t))
         (results '())
         (cancelled nil))

    (unless prop-names
      (user-error "No org properties found in the database"))

    (unless selected-props
      (user-error "No properties selected"))

    (message "Starting batch conversion of %d properties..." (length selected-props))

    ;; Process each property one by one
    (catch 'batch-cancelled
      (dolist (prop selected-props)
        (when cancelled
          (throw 'batch-cancelled nil))

        (let* ((all-tags (mapcar #'car (supertag-query :tags)))
               (occurrences (supertag-migration--nodes-with-property props-table prop))
               (field-type (supertag-migration--infer-field-type occurrences)))

          (unless occurrences
            (message "Skipping '%s': not found in any nodes" prop)
            (push (list :property prop :status 'skipped :reason "No occurrences") results)
            (cl-return))

          ;; Show property info and ask for tag
          (message "\n--- Property: %s (found in %d nodes, inferred type: %s) ---"
                   prop (length occurrences) field-type)

          (let ((tag-input (completing-read
                            (format "[%s] Associate with tag (or enter new, or C-g to skip): "
                                    prop)
                            all-tags nil nil)))

            (if (or (null tag-input) (string-empty-p tag-input))
                ;; User cancelled or entered empty string - skip this property
                (progn
                  (message "Skipped property: %s" prop)
                  (push (list :property prop :status 'skipped :reason "User skipped") results))

              ;; Process this property
              (let ((tag-id (supertag-sanitize-tag-name tag-input)))

                ;; Create tag if it doesn't exist
                (unless (supertag-tag-get tag-id)
                  (if (yes-or-no-p (format "Tag '%s' doesn't exist. Create it? " tag-id))
                      (progn
                        (supertag-tag-create `(:id ,tag-id :name ,tag-id))
                        (message "Created tag '%s'" tag-id))
                    (message "Cancelled: tag '%s' was not created. Skipping property '%s'."
                             tag-id prop)
                    (push (list :property prop :status 'skipped :reason "Tag not created") results)
                    (cl-return)))

                ;; Convert the property
                (condition-case err
                    (let ((result (supertag-migration--convert-single-property
                                   prop tag-id props-table)))
                      (push result results)
                      (message "✓ Converted: %s → tag '%s' (%d values migrated, %d tags added, %d nodes skipped)"
                               prop tag-id
                               (plist-get result :values-set)
                               (or (plist-get result :tags-added) 0)
                               (plist-get result :nodes-skipped)))
                  (error
                   (message "✗ Error converting '%s': %s" prop (error-message-string err))
                   (push (list :property prop
                               :status 'error
                               :error (error-message-string err))
                         results)))))))))

    ;; Summary
    (let* ((completed (cl-count-if (lambda (r)
                                     (and (plist-get r :values-set)
                                          (>= (plist-get r :values-set) 0)))
                                   results))
           (skipped (cl-count-if (lambda (r) (eq (plist-get r :status) 'skipped)) results))
           (errors (cl-count-if (lambda (r) (eq (plist-get r :status) 'error)) results)))
      (message "\n=== Batch Conversion Summary ===")
      (message "Total: %d properties" (length selected-props))
      (message "Completed: %d" completed)
      (message "Skipped: %d" skipped)
      (message "Errors: %d" errors)
      (nreverse results))))

(defun supertag-migration--insert-tag-in-org-file (node-id tag-id)
  "Insert #TAG-ID in the org file for NODE-ID.
This function finds the node's location in the org file and inserts the tag
at the end of the headline (e.g., '* My Heading #tag')."
  (when-let* ((node (supertag-node-get node-id))
              (file (plist-get node :file))
              (position (plist-get node :position)))
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char position)
          ;; Make sure we're at a heading
          (when (org-at-heading-p)
            ;; Go to end of headline (before any org native tags like :tag:)
            (let* ((element (org-element-at-point))
                   (tags-start (org-element-property :tags element))
                   (line-end (line-end-position)))
              ;; Position at end of headline text, before org native tags
              (if tags-start
                  ;; If there are org native tags, find where they start
                  (progn
                    (end-of-line)
                    (when (re-search-backward "\\s-+:[[:alnum:]_@#%:]+:\\s-*$" (line-beginning-position) t)
                      (goto-char (match-beginning 0))))
                ;; No org native tags, just go to end of line
                (end-of-line))
              ;; Insert the tag with proper spacing
              (insert " #" tag-id)
              (save-buffer))))))))

(defun supertag-migration--convert-single-property (property-name tag-id props-table)
  "Internal function to convert a single PROPERTY-NAME to a field on TAG-ID.
PROPS-TABLE is the pre-collected properties table."
  (let* ((occurrences (supertag-migration--nodes-with-property props-table property-name))
         (field-type (supertag-migration--infer-field-type occurrences))
         (field-id (supertag-sanitize-field-id property-name))
         (values-set 0)
         (tags-added 0)
         (nodes-skipped 0))

    (unless occurrences
      (error "Property '%s' not found in any nodes" property-name))

    ;; Create field definition on the tag
    (let ((field-def `(:name ,property-name
                       :id ,field-id
                       :type ,field-type)))
      (supertag-tag-add-field tag-id field-def))

    ;; For each node with this property, set the field value
    (dolist (occurrence occurrences)
      (let* ((node-id (car occurrence))
             (value (cdr occurrence))
             (node (supertag-node-get node-id)))
        (if node
            (let* ((node-tags (plist-get node :tags))
                   (has-tag (and node-tags (member tag-id node-tags))))
              (unless has-tag
                ;; Add tag to database
                (let ((updated (supertag-node-add-tag node-id tag-id)))
                  (when updated
                    (setq node updated
                          node-tags (plist-get updated :tags)
                          has-tag (and node-tags (member tag-id node-tags)))
                    ;; Also insert #tag in the org file
                    (supertag-migration--insert-tag-in-org-file node-id tag-id)
                    (cl-incf tags-added))))
              (if has-tag
                  (progn
                    (supertag-field-set node-id tag-id property-name value)
                    (cl-incf values-set))
                (cl-incf nodes-skipped)))
          (cl-incf nodes-skipped))))

    (list :property property-name
          :tag tag-id
          :field-id field-id
          :field-type field-type
          :values-set values-set
          :tags-added tags-added
          :nodes-skipped nodes-skipped)))

;;;###autoload
(defun supertag-migration-add-ids-to-org-headings (directory)
  "Add :ID: properties to all Org headings in DIRECTORY that don't have one.
This function recursively processes all .org files in DIRECTORY and adds
:ID: properties to headings that are missing them. This is useful for
preparing Org files for Supertag synchronization.

DIRECTORY should be an absolute path to a directory containing .org files.

Returns a plist with statistics: (:files-processed N :ids-added N :errors N)"
  (interactive "DDirectory to process: ")
  (unless (and directory (file-directory-p directory))
    (user-error "Invalid directory: %s" directory))

  (let* ((files (directory-files-recursively directory "\\.org$" nil))
         (total-files (length files))
         (stats '(:files-processed 0 :ids-added 0 :errors 0)))

    (message "Starting to add IDs to Org headings in %s..." directory)
    (message "Found %d .org files to process" total-files)

    (dolist (file files)
      (condition-case err
          (let ((file-stats (supertag-migration--add-ids-to-file file)))
            (cl-incf (plist-get stats :files-processed))
            (cl-incf (plist-get stats :ids-added) (plist-get file-stats :ids-added))
            (message "[%d/%d] %s: %d IDs added"
                     (plist-get stats :files-processed) total-files
                     (file-name-nondirectory file)
                     (plist-get file-stats :ids-added)))
        (error
         (cl-incf (plist-get stats :errors))
         (message "Error processing %s: %s" (file-name-nondirectory file) (error-message-string err)))))

    (message "Completed! Processed %d files, added %d IDs, %d errors"
             (plist-get stats :files-processed)
             (plist-get stats :ids-added)
             (plist-get stats :errors))
    stats))

(defun supertag-migration--add-ids-to-file (file)
  "Add :ID: properties to headings in FILE that don't have one.
Returns a plist with (:ids-added N)."
  (let ((ids-added 0)
        (existing-buffer (find-buffer-visiting file))
        (buffer (condition-case nil
                   (find-file-noselect file t)
                 (error nil))))

    (unless buffer
      (error "Cannot open file: %s" file))

    (with-current-buffer buffer
      (save-excursion
        (org-mode)  ; Ensure org-mode is active
        (goto-char (point-min))

        ;; Visit each heading
        (while (re-search-forward org-heading-regexp nil t)
          (when (org-at-heading-p)
            (let ((existing-id (org-entry-get nil "ID")))
              (unless existing-id
                ;; No ID exists, add one
                (org-id-get-create)
                (cl-incf ids-added))))))

      ;; Save the buffer if we modified it
      (when (> ids-added 0)
        (save-buffer))

      ;; Close buffer if we opened it
      (unless existing-buffer
        (kill-buffer buffer)))

    (list :ids-added ids-added)))

(provide 'supertag-migration)

;;; supertag-migration.el ends here

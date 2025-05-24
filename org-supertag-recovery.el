;;; org-supertag-recovery.el --- Complete recovery suite for org-supertag database -*- lexical-binding: t; -*-


;;; Commentary:
;; Complete recovery suite for the org-supertag database.
;; Integrates all recovery functions: database recovery, tag recovery, relation recovery, field recovery.

;;; Code:

;; Add current directory to load-path
(add-to-list 'load-path ".")
(add-to-list 'load-path "./lisp")

;; Load necessary libraries
(require 'org-supertag-db)
(require 'org-supertag-sync)
(require 'org-supertag-relation)

;; Recovery state tracking
(defvar org-supertag-recovery--state (make-hash-table :test 'equal)
  "Tracks the state of the recovery process.")

;;; =================================================================
;;; Diagnosis Functions
;;; =================================================================

(defun org-supertag-recovery-diagnose ()
  "Diagnose the current state of the database."
  (interactive)
  (message "=== org-supertag Database Diagnosis ===")
  (message "")

  (let ((diagnosis (make-hash-table :test 'equal)))

    ;; Basic information
    (puthash :db-file-exists (file-exists-p org-supertag-db-file) diagnosis)
    (puthash :db-file-size (when (file-exists-p org-supertag-db-file)
                             (nth 7 (file-attributes org-supertag-db-file))) diagnosis)

    ;; Database object state
    (puthash :db-loaded (and (boundp 'org-supertag-db--object)
                            (hash-table-p org-supertag-db--object)) diagnosis)

    (when (gethash :db-loaded diagnosis)
      (puthash :total-entities (hash-table-count org-supertag-db--object) diagnosis)
      (puthash :total-links (hash-table-count org-supertag-db--link) diagnosis)

      ;; Check if database needs reloading
      (let ((file-size (gethash :db-file-size diagnosis))
            (total-entities (hash-table-count org-supertag-db--object)))
        (when (and file-size (> file-size 1000000) (= total-entities 0))
          (puthash :needs-reload t diagnosis)
          (message "Anomaly detected: Database file size %.1fMB but no data in memory, may need reload"
                  (/ (float file-size) 1024 1024))))

      ;; Analyze entity types
      (let ((entity-types (make-hash-table :test 'equal)))
        (maphash (lambda (k v)
                  (let ((type (plist-get v :type)))
                    (puthash type (1+ (gethash type entity-types 0)) entity-types)))
                org-supertag-db--object)
        (puthash :entity-types entity-types diagnosis))

      ;; Check metadata
      (let ((metadata (org-supertag-db-get "metadata")))
        (puthash :has-metadata (not (null metadata)) diagnosis)
        (when metadata
          (let ((metadata-hash (plist-get metadata :data)))
            (puthash :metadata-hash-valid (hash-table-p metadata-hash) diagnosis)
            (when (hash-table-p metadata-hash)
              (puthash :metadata-size (hash-table-count metadata-hash) diagnosis))))))

    ;; Check database file content (when memory is empty but file exists)
    (when (and (gethash :db-file-exists diagnosis)
               (gethash :db-loaded diagnosis)
               (= (gethash :total-entities diagnosis 0) 0)
               (> (gethash :db-file-size diagnosis 0) 100000))
      (puthash :file-content-check (org-supertag-recovery--check-file-content) diagnosis))

    ;; Backup information
    (let ((backup-dir org-supertag-db-backup-directory))
      (puthash :backup-dir-exists (file-exists-p backup-dir) diagnosis)
      (when (file-exists-p backup-dir)
        (let ((backup-files (directory-files backup-dir t "db-.*\\.el$")))
          (puthash :backup-count (length backup-files) diagnosis))))

    ;; Display diagnosis results
    (org-supertag-recovery--display-diagnosis diagnosis)

    ;; If reload is needed, offer fix option
    (when (gethash :needs-reload diagnosis)
      (when (yes-or-no-p "Detected database might not be loaded correctly, attempt to reload?")
        (org-supertag-recovery--reload-database)
        ;; Re-diagnose
        (message "")
        (message "=== Diagnosis results after reloading ===")
        (org-supertag-recovery-diagnose)))

    ;; Save diagnosis results
    (puthash :last-diagnosis diagnosis org-supertag-recovery--state)
    diagnosis))

(defun org-supertag-recovery--check-file-content ()
  "Check the content of the database file."
  (let ((content-info (make-hash-table :test 'equal)))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents org-supertag-db-file nil 0 50000) ; Read first 50KB
          (goto-char (point-min))

          ;; Check for expected data structures
          (puthash :has-ht-set (not (null (search-forward "ht-set!" nil t))) content-info)
          (goto-char (point-min))
          (puthash :has-object-table (not (null (search-forward "org-supertag-db--object" nil t))) content-info)
          (goto-char (point-min))
          (puthash :has-link-table (not (null (search-forward "org-supertag-db--link" nil t))) content-info)
          (goto-char (point-min))
          (puthash :has-metadata (not (null (search-forward "\"metadata\"" nil t))) content-info)

          ;; Rough estimate of entity count
          (goto-char (point-min))
          (let ((entity-count 0))
            (while (search-forward "ht-set! org-supertag-db--object" nil t)
              (setq entity-count (1+ entity-count)))
            (puthash :estimated-entities entity-count content-info)))
      (error
       (puthash :read-error (format "%S" err) content-info)))

    content-info))

(defun org-supertag-recovery--reload-database ()
  "Reload the database."
  (message "Reloading database...")
  (condition-case err
      (progn
        ;; Clear current database
        (when (boundp 'org-supertag-db--object)
          (clrhash org-supertag-db--object))
        (when (boundp 'org-supertag-db--link)
          (clrhash org-supertag-db--link))

        ;; Reload
        (org-supertag-db-load)

        (message "Database reload complete!")
        (message "Current entity count: %d, link count: %d"
                (hash-table-count org-supertag-db--object)
                (hash-table-count org-supertag-db--link)))
    (error
     (message "Reload failed: %S" err))))

(defun org-supertag-recovery--display-diagnosis (diagnosis)
  "Display the diagnosis results."
  (message "=== Database Status ===")
  (message "Database file exists: %s"
          (if (gethash :db-file-exists diagnosis) "✓Yes" "✗No"))

  (when (gethash :db-file-exists diagnosis)
    (let ((size (gethash :db-file-size diagnosis)))
      (message "Database file size: %.1fMB" (/ (float size) 1024 1024))))

  (message "Database loaded: %s"
          (if (gethash :db-loaded diagnosis) "✓Yes" "✗No"))

  (when (gethash :db-loaded diagnosis)
    (message "Total entities: %d" (gethash :total-entities diagnosis 0))
    (message "Total links: %d" (gethash :total-links diagnosis 0))

    ;; Display reload warning
    (when (gethash :needs-reload diagnosis)
      (message "⚠️  Warning: Database file is not empty but no data in memory"))

    ;; Display entity type distribution
    (let ((entity-types (gethash :entity-types diagnosis)))
      (if (and entity-types (> (hash-table-count entity-types) 0))
          (progn
            (message "Entity type distribution:")
            (maphash (lambda (type count)
                      (message "  %s: %d" (or type "Unknown") count))
                    entity-types))
        (message "Entity type distribution: Empty")))

    ;; Display file content check results
    (let ((file-check (gethash :file-content-check diagnosis)))
      (when file-check
        (message "")
        (message "=== File Content Check ===")
        (if (gethash :read-error file-check)
            (message "File read error: %s" (gethash :read-error file-check))
          (progn
            (message "Contains hash table set: %s"
                    (if (gethash :has-ht-set file-check) "✓Yes" "✗No"))
            (message "Contains object table: %s"
                    (if (gethash :has-object-table file-check) "✓Yes" "✗No"))
            (message "Contains link table: %s"
                    (if (gethash :has-link-table file-check) "✓Yes" "✗No"))
            (message "Contains metadata: %s"
                    (if (gethash :has-metadata file-check) "✓Yes" "✗No"))
            (when (gethash :estimated-entities file-check)
              (message "Estimated entity count: %d" (gethash :estimated-entities file-check)))))))

    ;; Metadata information
    (message "")
    (message "Contains metadata: %s"
            (if (gethash :has-metadata diagnosis) "✓Yes" "✗No"))

    (when (gethash :has-metadata diagnosis)
      (message "Metadata structure valid: %s"
              (if (gethash :metadata-hash-valid diagnosis) "✓Yes" "✗No"))
      (when (gethash :metadata-size diagnosis)
        (message "Metadata entry count: %d" (gethash :metadata-size diagnosis)))))

  ;; Backup information
  (message "")
  (message "=== Backup Status ===")
  (message "Backup directory exists: %s"
          (if (gethash :backup-dir-exists diagnosis) "✓Yes" "✗No"))
  (when (gethash :backup-count diagnosis)
    (message "Backup file count: %d" (gethash :backup-count diagnosis)))

  ;; Provide suggestions
  (message "")
  (message "=== Suggestions ===")
  (cond
   ((not (gethash :db-file-exists diagnosis))
    (message "• Database file does not exist, suggest performing a full rebuild"))

   ((not (gethash :db-loaded diagnosis))
    (message "• Database not loaded, suggest checking if org-supertag is initialized correctly"))

   ((gethash :needs-reload diagnosis)
    (message "• Database needs reloading, the system will automatically offer a fix option"))

   ((= (gethash :total-entities diagnosis 0) 0)
    (message "• Database is empty, suggest restoring from backup or rebuilding the database"))

   ((not (gethash :has-metadata diagnosis))
    (message "• Metadata is missing, suggest recovering tag relations"))

   (t
    (message "• Database status is normal"))))

;;; =================================================================
;;; Database Level Recovery (from recover-db.el)
;;; =================================================================

(defun org-supertag-recovery-from-backup ()
  "Restore the database from a backup."
  (interactive)
  (let ((backup-dir org-supertag-db-backup-directory))
    (if (not (file-exists-p backup-dir))
        (message "Backup directory does not exist: %s" backup-dir)
      (let ((backup-files (directory-files backup-dir t "db-.*\\.el$")))
        (if (not backup-files)
            (message "No backup files found")
          (let* ((latest-backup (car (sort backup-files
                                         (lambda (a b)
                                           (file-newer-than-file-p a b)))))
                 (backup-size (nth 7 (file-attributes latest-backup))))
            (when (> backup-size 0)
              (when (yes-or-no-p
                     (format "Found backup file %s (%d bytes), restore?"
                            (file-name-nondirectory latest-backup)
                            backup-size))
                (condition-case err
                    (progn
                      ;; Backup current file (if exists)
                      (when (file-exists-p org-supertag-db-file)
                        (let ((backup-current (format "%s.before-recovery"
                                                     org-supertag-db-file)))
                          (copy-file org-supertag-db-file backup-current t)
                          (message "Current database file backed up to: %s" backup-current)))

                      ;; Restore backup
                      (copy-file latest-backup org-supertag-db-file t)

                      ;; Reload database
                      (org-supertag-db-load)

                      (message "Database restored successfully! Entity count: %d, Link count: %d"
                              (hash-table-count org-supertag-db--object)
                              (hash-table-count org-supertag-db--link))

                      (puthash :backup-recovery-success t org-supertag-recovery--state))
                  (error
                   (message "Restore failed: %S" err)
                   (puthash :backup-recovery-success nil org-supertag-recovery--state)))))))))))

(defun org-supertag-recovery-rebuild-database ()
  "Rebuild the database."
  (interactive)
  (when (yes-or-no-p "This will rebuild the entire database, continue?")
    (condition-case err
        (progn
          ;; Clear current database
          (setq org-supertag-db--object (ht-create)
                org-supertag-db--link (ht-create))

          ;; Resync all files
          (message "Starting database rebuild...")
          (org-supertag-sync-all)

          ;; Save database
          (org-supertag-db-save)

          (message "Database rebuild complete! Entity count: %d, Link count: %d"
                  (hash-table-count org-supertag-db--object)
                  (hash-table-count org-supertag-db--link))

          (puthash :rebuild-success t org-supertag-recovery--state))
      (error
       (message "Rebuild failed: %S" err)
       (puthash :rebuild-success nil org-supertag-recovery--state)))))

;;; =================================================================
;;; Tag Definition Recovery (from recover-from-nodes.el)
;;; =================================================================

(defun org-supertag-recovery-analyze-node-tags ()
  "Analyze tag usage in nodes."
  (let ((tag-usage (make-hash-table :test 'equal))
        (node-count 0)
        (nodes-with-tags 0))

    ;; Iterate through all nodes, collect tag usage info
    (maphash (lambda (k v)
              (when (eq (plist-get v :type) :node)
                (setq node-count (1+ node-count))
                (let ((tags (plist-get v :tags)))
                  (when tags
                    (setq nodes-with-tags (1+ nodes-with-tags))
                    (dolist (tag tags)
                      (let ((clean-tag (if (string-prefix-p "#" tag)
                                           (substring tag 1)
                                         tag)))
                        (let ((current-info (gethash clean-tag tag-usage)))
                          (if current-info
                              (progn
                                (puthash clean-tag
                                        (list :count (1+ (plist-get current-info :count))
                                              :nodes (cons k (plist-get current-info :nodes)))
                                        tag-usage))
                            (puthash clean-tag
                                    (list :count 1 :nodes (list k))
                                    tag-usage)))))))))
            org-supertag-db--object)

    (puthash :tag-analysis-result tag-usage org-supertag-recovery--state)
    (message "Analysis complete: Total nodes %d, nodes with tags %d, tags found %d"
            node-count nodes-with-tags (hash-table-count tag-usage))
    tag-usage))

(defun org-supertag-recovery-rebuild-tags-from-nodes ()
  "Rebuild tag definitions from node data."
  (interactive)
  (message "Starting to rebuild tag definitions from node data...")

  (let ((tag-usage (org-supertag-recovery-analyze-node-tags))
        (created-count 0)
        (updated-count 0))

    (when (> (hash-table-count tag-usage) 0)
      (message "=== Starting Tag Definition Rebuild ===")

      ;; Create definitions for each tag
      (maphash (lambda (tag-name usage-info)
                (let ((existing-tag (org-supertag-db-get tag-name)))
                  (if (and existing-tag (eq (plist-get existing-tag :type) :tag))
                      (progn
                        ;; Update usage info for existing tag
                        (plist-put existing-tag :usage-count (plist-get usage-info :count))
                        (plist-put existing-tag :source-nodes
                                  (seq-take (plist-get usage-info :nodes) 10))
                        (org-supertag-db-add tag-name existing-tag)
                        (setq updated-count (1+ updated-count))
                        (message "Updating tag: %s" tag-name))
                    ;; Create new tag definition
                    (let ((tag-def (list
                                   :type :tag
                                   :id tag-name
                                   :fields '()
                                   :created-at (current-time)
                                   :description (format "Rebuilt from node usage (used %d times, %d nodes)"
                                                       (plist-get usage-info :count)
                                                       (length (plist-get usage-info :nodes)))
                                   :usage-count (plist-get usage-info :count)
                                   :source-nodes (seq-take (plist-get usage-info :nodes) 10))))
                      (org-supertag-db-add tag-name tag-def)
                      (setq created-count (1+ created-count))
                      (message "Creating tag: %s (used %d times)"
                              tag-name (plist-get usage-info :count))))))
              tag-usage)

      ;; Save database
      (when (or (> created-count 0) (> updated-count 0))
        (org-supertag-db-save)
        (message "=== Tag Rebuild Complete ===")
        (message "New tags created: %d" created-count)
        (message "Tags updated: %d" updated-count)

        (puthash :tag-rebuild-result (list :created created-count :updated updated-count)
                org-supertag-recovery--state))

      (when (and (= created-count 0) (= updated-count 0))
        (message "No tag definitions needed creation or update")))))

;;; =================================================================
;;; Field Definition Recovery (from recover-fields-from-links.el)
;;; =================================================================

(defun org-supertag-recovery-analyze-field-links ()
  "Analyze field link relationships."
  (let ((field-usage (make-hash-table :test 'equal))
        (total-links 0)
        (field-links 0))

    ;; Iterate through all links
    (maphash (lambda (link-id link-data)
              (setq total-links (1+ total-links))
              ;; Identify field links by link ID pattern
              (when (and (stringp link-id)
                        (string-prefix-p ":node-field:" link-id))
                (setq field-links (1+ field-links))
                (let* ((field-name (plist-get link-data :to))
                       (field-value (plist-get link-data :value))
                       (tag-id (plist-get link-data :tag-id))
                       (node-id (plist-get link-data :from)))

                  (when (and field-name tag-id)
                    ;; Get or create field info for this tag
                    (let ((tag-fields (gethash tag-id field-usage (make-hash-table :test 'equal))))
                      (let ((field-info (gethash field-name tag-fields)))
                        (if field-info
                            ;; Update existing field info
                            (progn
                              (puthash :count (1+ (gethash :count field-info)) field-info)
                              (let ((values (gethash :values field-info)))
                                (unless (member field-value values)
                                  (puthash :values (cons field-value values) field-info))))
                          ;; Create new field info
                          (let ((new-field-info (make-hash-table :test 'equal)))
                            (puthash :name field-name new-field-info)
                            (puthash :count 1 new-field-info)
                            (puthash :values (list field-value) new-field-info)
                            (puthash field-name new-field-info tag-fields))))
                      (puthash tag-id tag-fields field-usage))))))
            org-supertag-db--link)

    (puthash :field-analysis-result field-usage org-supertag-recovery--state)
    (message "Field analysis complete: Total links %d, field links %d, tags using fields found %d"
            total-links field-links (hash-table-count field-usage))
    field-usage))

(defun org-supertag-recovery-infer-field-type (values)
  "Infer the field type."
  (if (null values)
      'string
    (let ((sample-values (seq-take values 10)))
      (cond
       ;; Check if all are numbers
       ((cl-every (lambda (v)
                   (and (stringp v)
                        (string-match-p "^[0-9]+\\.?[0-9]*$" v)))
                 sample-values)
        'number)

       ;; Check if date format
       ((cl-every (lambda (v)
                   (and (stringp v)
                        (or (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" v)
                            (string-match-p "^<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" v))))
                 sample-values)
        'date)

       ;; Check if time format
       ((cl-every (lambda (v)
                   (and (stringp v)
                        (string-match-p "[0-9]\\{2\\}:[0-9]\\{2\\}" v)))
                 sample-values)
        'time)

       ;; Check if looks like an options list
       ((let ((unique-count (length (delete-dups (copy-sequence sample-values))))
              (total-count (length sample-values)))
          (and (> total-count 3)
               (< unique-count (/ total-count 2))))
        'options)

       ;; Default to string
       (t 'string)))))

(defun org-supertag-recovery-rebuild-field-definitions ()
  "Rebuild field definitions."
  (interactive)
  (message "Starting to recover field definitions from link relationships...")

  (let ((field-usage (org-supertag-recovery-analyze-field-links))
        (updated-tags 0)
        (total-fields 0))

    (if (= (hash-table-count field-usage) 0)
        (message "No field usage records found, cannot recover field definitions")

      ;; Generate and apply field definitions
      (maphash (lambda (tag-id tag-fields)
                (let ((field-definitions '()))
                  (maphash (lambda (field-name field-info)
                            (let* ((values (gethash :values field-info))
                                   (count (gethash :count field-info))
                                   (inferred-type (org-supertag-recovery-infer-field-type values))
                                   (unique-values (delete-dups (copy-sequence values))))

                              (push (list :name field-name
                                         :type inferred-type
                                         :usage-count count
                                         :unique-values (length unique-values)
                                         :sample-values (seq-take unique-values 5)
                                         :description (format "Inferred from usage (used %d times)" count))
                                   field-definitions)))
                          tag-fields)

                  ;; Apply to tag
                  (let ((tag-data (org-supertag-db-get tag-id)))
                    (when tag-data
                      (plist-put tag-data :fields field-definitions)
                      (plist-put tag-data :fields-recovered-at (current-time))
                      (org-supertag-db-add tag-id tag-data)
                      (setq updated-tags (1+ updated-tags))
                      (setq total-fields (+ total-fields (length field-definitions)))
                      (message "Updating tag %s: Added %d field definitions" tag-id (length field-definitions))))))
              field-usage)

      (when (> updated-tags 0)
        (org-supertag-db-save)
        (message "=== Field Definition Recovery Complete ===")
        (message "Updated %d tags" updated-tags)
        (message "Total %d field definitions recovered" total-fields)

        (puthash :field-rebuild-result (list :updated-tags updated-tags :total-fields total-fields)
                org-supertag-recovery--state)))))

;;; =================================================================
;;; Tag Relation Recovery (from recover-tag-relations.el)
;;; =================================================================

(defun org-supertag-recovery-extract-cooccurrence-from-metadata ()
  "Extract cooccurrence relation data from metadata."
  (let ((metadata (org-supertag-db-get "metadata"))
        (cooccurrence-pairs '())
        (metadata-hash nil))

    (if (not metadata)
        (progn
          (message "Metadata record not found")
          nil)

      (setq metadata-hash (plist-get metadata :data))

      (cond
       ((not metadata-hash)
        (message "No data hash table in metadata")
        nil)

       ((not (hash-table-p metadata-hash))
        (message "metadata :data is not a hash table type")
        nil)

       (t
        (let ((found-pairs 0))

          (message "Metadata hash table size: %d" (hash-table-count metadata-hash))

          ;; Use simple error handling
          (ignore-errors
            (maphash (lambda (key value)
                      (when (and (symbolp key) (stringp (symbol-name key)))
                        (let ((key-str (symbol-name key)))
                          ;; Look for keys starting with tag-cooccur:
                          (when (string-prefix-p "tag-cooccur:" key-str)
                            (let* ((pair-part (substring key-str (length "tag-cooccur:")))
                                   (tags (split-string pair-part ":"))
                                   (tag1 (car tags))
                                   (tag2 (cadr tags))
                                   (count value))

                              (when (and tag1 tag2 (numberp count) (> count 0))
                                ;; Get corresponding PMI value
                                (let* ((pmi-key (intern (format "tag-pmi:%s:%s" tag1 tag2)))
                                       (pmi-value (gethash pmi-key metadata-hash 0))
                                       ;; Calculate strength
                                       (strength (if (> pmi-value 0)
                                                    (max 0.1 (min 1.0 (/ pmi-value 6.0)))
                                                  (max 0.1 (min 1.0 (/ count 10.0))))))

                                  ;; Only keep pairs where tag1 < tag2 to avoid duplicates
                                  (when (string< tag1 tag2)
                                    (push (list :tag1 tag1
                                              :tag2 tag2
                                              :count count
                                              :pmi pmi-value
                                              :strength strength
                                              :source "metadata")
                                          cooccurrence-pairs)
                                    (setq found-pairs (1+ found-pairs))))))))))
                    metadata-hash))

          (message "Extracted %d cooccurrence pairs from metadata" found-pairs)

          ;; Sort by strength
          (when cooccurrence-pairs
            (setq cooccurrence-pairs (sort cooccurrence-pairs
                                          (lambda (a b) (> (plist-get a :strength) (plist-get b :strength))))))

          (puthash :relation-analysis-result cooccurrence-pairs org-supertag-recovery--state)
          cooccurrence-pairs))))))

(defun org-supertag-recovery-rebuild-tag-relations ()
  "Rebuild tag relations."
  (interactive)
  (message "Starting to recover tag relations from metadata...")

  (let ((cooccurrence-pairs (org-supertag-recovery-extract-cooccurrence-from-metadata))
        (min-strength 0.3)
        (recovered-count 0))

    (if (null cooccurrence-pairs)
        (message "No recoverable relation data found")

      (message "=== Recovering Cooccurrence Relations (Min strength: %.2f) ===" min-strength)

      (dolist (pair cooccurrence-pairs)
        (let ((tag1 (plist-get pair :tag1))
              (tag2 (plist-get pair :tag2))
              (strength (plist-get pair :strength)))

          (when (>= strength min-strength)
            ;; Get tag IDs
            (let ((tag1-id (org-supertag-tag-get-id-by-name tag1))
                  (tag2-id (org-supertag-tag-get-id-by-name tag2)))

              (when (and tag1-id tag2-id)
                ;; Check if relation already exists
                (unless (org-supertag-relation-get tag1-id tag2-id 'cooccurrence)
                  ;; Add bidirectional cooccurrence relation
                  (org-supertag-relation-add-relation tag1-id tag2-id 'cooccurrence strength)
                  (org-supertag-relation-add-relation tag2-id tag1-id 'cooccurrence strength)
                  (setq recovered-count (+ recovered-count 2))
                  (message "  Adding cooccurrence relation: %s <-> %s (Strength: %.3f)" tag1 tag2 strength)))))))

      (when (> recovered-count 0)
        (org-supertag-db-save)
        (message "=== Relation Recovery Complete ===")
        (message "Recovered %d cooccurrence relations" recovered-count)

        (puthash :relation-rebuild-result (list :recovered recovered-count)
                org-supertag-recovery--state)))))

;;; =================================================================
;;; Main Recovery Flow
;;; =================================================================

(defun org-supertag-recovery-full-suite ()
  "Complete recovery process."
  (interactive)
  (message "=== org-supertag Complete Recovery Suite ===")
  (message "")

  ;; 1. First perform diagnosis
  (let ((diagnosis (org-supertag-recovery-diagnose)))

    ;; 2. Provide recovery suggestions based on diagnosis
    (message "=== Recovery Suggestions ===")
    (cond
     ;; If database file does not exist or is corrupted
     ((not (gethash :db-loaded diagnosis))
      (message "Database not loaded or corrupted, suggest:")
      (message "1. Attempt to restore from backup")
      (message "2. If no backup, rebuild the database"))

     ;; If no tag definitions
     ((= (gethash :tag (gethash :entity-types diagnosis) 0) 0)
      (message "No tag definitions found, suggest:")
      (message "1. Rebuild tags from node data")
      (message "2. Recover field definitions from links")
      (message "3. Recover relations from metadata"))

     ;; Data is mostly intact but relations might need recovery
     (t
      (message "Data is mostly intact, can selectively recover:")
      (message "1. Field definition recovery")
      (message "2. Tag relation recovery")))

    (message "")

    ;; 3. Provide interactive selection
    (org-supertag-recovery-interactive-menu)))

(defun org-supertag-recovery-interactive-menu ()
  "Interactive recovery menu."
  (interactive)
  (let ((choice (read-char-choice
                 "Select recovery option:
1. Diagnose database status
2. Restore database from backup
3. Rebuild entire database
4. Rebuild tag definitions from nodes
5. Recover field definitions from links
6. Recover tag relations from metadata
7. Execute full recovery process
8. View recovery status
9. Exit
Select (1-9): "
                 '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))))
    (cond
     ((eq choice ?1)
      (org-supertag-recovery-diagnose)
      (org-supertag-recovery--wait-and-continue))

     ((eq choice ?2)
      (org-supertag-recovery-from-backup)
      (org-supertag-recovery--wait-and-continue))

     ((eq choice ?3)
      (org-supertag-recovery-rebuild-database)
      (org-supertag-recovery--wait-and-continue))

     ((eq choice ?4)
      (org-supertag-recovery-rebuild-tags-from-nodes)
      (org-supertag-recovery--wait-and-continue))

     ((eq choice ?5)
      (org-supertag-recovery-rebuild-field-definitions)
      (org-supertag-recovery--wait-and-continue))

     ((eq choice ?6)
      (org-supertag-recovery-rebuild-tag-relations)
      (org-supertag-recovery--wait-and-continue))

     ((eq choice ?7)
      (org-supertag-recovery-execute-full-recovery)
      (org-supertag-recovery--wait-and-continue))

     ((eq choice ?8)
      (org-supertag-recovery-show-status)
      (org-supertag-recovery--wait-and-continue))

     ((eq choice ?9)
      (message "Exiting recovery suite"))

     (t
      (message "Invalid selection")
      (sit-for 1)
      (org-supertag-recovery-interactive-menu)))))

(defun org-supertag-recovery--wait-and-continue ()
  "Wait for user confirmation before deciding whether to continue."
  (message "")
  (message "=== Operation Complete ===")
  (let ((continue (read-char-choice
                   "Select: [c]ontinue with menu [q]uit [r]e-diagnose: "
                   '(?c ?q ?r))))
    (cond
     ((eq continue ?c)
      (message "")
      (org-supertag-recovery-interactive-menu))
     ((eq continue ?q)
      (message "Exiting recovery suite"))
     ((eq continue ?r)
      (message "")
      (org-supertag-recovery-diagnose)
      (org-supertag-recovery--wait-and-continue))
     (t
      (message "Invalid selection")
      (sit-for 1)
      (org-supertag-recovery--wait-and-continue)))))

(defun org-supertag-recovery-execute-full-recovery ()
  "Execute the full recovery process."
  (interactive)
  (when (yes-or-no-p "This will execute the full recovery process, which may take a long time. Continue?")
    (message "=== Starting Full Recovery Process ===")

    (catch 'cancel
      ;; 1. Diagnose
      (message "Step 1/5: Diagnosing database status...")
      (sit-for 1)
      (org-supertag-recovery-diagnose)
      (message "")
      (when (not (yes-or-no-p "Diagnosis complete, continue to next step?"))
        (message "Recovery process cancelled")
        (throw 'cancel nil))

      ;; 2. Rebuild tag definitions
      (message "Step 2/5: Rebuilding tag definitions from nodes...")
      (sit-for 1)
      (org-supertag-recovery-rebuild-tags-from-nodes)
      (message "")
      (when (not (yes-or-no-p "Tag rebuild complete, continue to next step?"))
        (message "Recovery process cancelled")
        (throw 'cancel nil))

      ;; 3. Recover field definitions
      (message "Step 3/5: Recovering field definitions from links...")
      (sit-for 1)
      (org-supertag-recovery-rebuild-field-definitions)
      (message "")
      (when (not (yes-or-no-p "Field recovery complete, continue to next step?"))
        (message "Recovery process cancelled")
        (throw 'cancel nil))

      ;; 4. Recover tag relations
      (message "Step 4/5: Recovering tag relations from metadata...")
      (sit-for 1)
      (org-supertag-recovery-rebuild-tag-relations)
      (message "")
      (when (not (yes-or-no-p "Relation recovery complete, continue to final verification?"))
        (message "Recovery process cancelled")
        (throw 'cancel nil))

      ;; 5. Final verification
      (message "Step 5/5: Final verification...")
      (sit-for 1)
      (org-supertag-recovery-diagnose)

      (message "")
      (message "=== Full Recovery Process Complete ===")
      (org-supertag-recovery-show-status)
      (message "")
      (message "Recovery process finished successfully!"))))

(defun org-supertag-recovery-show-status ()
  "Display recovery status."
  (interactive)
  (message "=== Recovery Status Report ===")

  (let ((tag-result (gethash :tag-rebuild-result org-supertag-recovery--state))
        (field-result (gethash :field-rebuild-result org-supertag-recovery--state))
        (relation-result (gethash :relation-rebuild-result org-supertag-recovery--state)))

    (if tag-result
        (message "Tag recovery: Created %d, Updated %d"
                (plist-get tag-result :created) (plist-get tag-result :updated))
      (message "Tag recovery: Not executed"))

    (if field-result
        (message "Field recovery: Updated %d tags, Total %d fields"
                (plist-get field-result :updated-tags) (plist-get field-result :total-fields))
      (message "Field recovery: Not executed"))

    (if relation-result
        (message "Relation recovery: Recovered %d relations"
                (plist-get relation-result :recovered))
      (message "Relation recovery: Not executed")))

  (message ""))

;; Provide convenient entry point
(defun org-supertag-emergency-recovery ()
  "Emergency recovery entry point."
  (interactive)
  (message "org-supertag Emergency Recovery System")
  (message "")
  (org-supertag-recovery-full-suite))

;; Loading complete message
(message "org-supertag Complete Recovery Suite loaded")
(message "Run: M-x org-supertag-recovery-full-suite to start recovery")
(message "Run: M-x org-supertag-emergency-recovery for emergency recovery")

(provide 'org-supertag-recovery)

;;; org-supertag-recovery.el ends here

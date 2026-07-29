;;; supertag-ops-tag-merge.el --- Transactional tag consolidation -*- lexical-binding: t; -*-

;;; Commentary:
;; Builds a conflict-free migration plan before touching data, then applies
;; the store and Org-file changes as one recoverable operation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ht)
(require 'supertag-core-store)
(require 'supertag-core-transform)
(require 'supertag-core-tag-path)
(require 'supertag-core-index)
(require 'supertag-ops-node)
(require 'supertag-ops-tag)
(require 'supertag-ops-field)
(require 'supertag-ops-global-field)
(require 'supertag-ops-relation)
(require 'supertag-ops-schema)
(require 'supertag-view-helper)

(defvar supertag-query-saved nil)
(defvar supertag--view-configs (make-hash-table :test 'eq))

(defconst supertag-tag-merge--missing (make-symbol "supertag-tag-merge-missing"))

(defconst supertag-tag-merge--multi-value-types
  '(:options :tag :node-reference)
  "Field types whose existing schema permits multiple stored values.")

(defconst supertag-tag-merge--tag-slot-keys
  '(:tag :tags :tag-id :target-tag :source-tag :scope-tag :base-tag
    :from-tag :to-tag)
  "Structured plist keys whose values are tag identifiers.")

(defun supertag-tag-merge--unique (items)
  "Return ITEMS without duplicates, preserving order."
  (let ((seen (make-hash-table :test 'equal)) result)
    (dolist (item items (nreverse result))
      (unless (gethash item seen)
        (puthash item t seen)
        (push item result)))))

(defun supertag-tag-merge--field-key (field)
  "Return the merge key for FIELD in the active field model."
  (if supertag-use-global-fields
      (or (plist-get field :id)
          (supertag-sanitize-field-id (plist-get field :name)))
    (plist-get field :name)))

(defun supertag-tag-merge--field-map (tag-id)
  "Return an equal-keyed map of resolved fields for TAG-ID."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (field (or (supertag-tag-get-all-fields tag-id) '()) table)
      (when-let* ((key (supertag-tag-merge--field-key field)))
        (unless (gethash key table)
          (puthash key field table))))))

(defun supertag-tag-merge--field-groups (tag-ids)
  "Group resolved fields from TAG-IDS by merge key.
Returns an alist of (FIELD-KEY . ENTRY-LIST), where each entry contains
`:tag-id' and `:definition'."
  (let ((groups (make-hash-table :test 'equal)) order)
    (dolist (tag-id tag-ids)
      (maphash
       (lambda (key field)
         (unless (gethash key groups)
           (push key order))
         (puthash key
                  (append (gethash key groups)
                          (list (list :tag-id tag-id :definition field)))
                  groups))
       (supertag-tag-merge--field-map tag-id)))
    (mapcar (lambda (key) (cons key (gethash key groups))) (nreverse order))))

(defun supertag-tag-merge--definitions-compatible-p (left right)
  "Return non-nil when LEFT and RIGHT describe the same field contract."
  (cl-every (lambda (key) (equal (plist-get left key) (plist-get right key)))
            '(:type :options :config :required :default :validator)))

(defun supertag-tag-merge--source-definition (key entries field-sources)
  "Choose KEY's definition from ENTRIES using FIELD-SOURCES.
Returns (DEFINITION . CONFLICT), with either side possibly nil."
  (let* ((requested-tag (cdr (assoc key field-sources)))
         (requested (and requested-tag
                         (cl-find requested-tag entries
                                  :key (lambda (entry) (plist-get entry :tag-id))
                                  :test #'equal)))
         (definitions (mapcar (lambda (entry) (plist-get entry :definition)) entries))
         (first (car definitions)))
    (cond
     (requested (cons (plist-get requested :definition) nil))
     ((or (null (cdr definitions))
          (cl-every (lambda (definition)
                      (supertag-tag-merge--definitions-compatible-p first definition))
                    (cdr definitions)))
      (cons first nil))
     (t
      (cons nil (list :kind :field-definition
                      :field key
                      :candidates entries))))))

(defun supertag-tag-merge--affected-nodes (source-ids)
  "Return node plists whose `:tags' intersect SOURCE-IDS."
  (let (nodes)
    (maphash
     (lambda (node-id raw-node)
       (let ((node (supertag--ensure-plist raw-node)))
         (when (cl-intersection (or (plist-get node :tags) '()) source-ids
                                :test #'equal)
           (push (plist-put (copy-sequence node) :id node-id) nodes))))
     (supertag-store-get-collection :nodes))
    (nreverse nodes)))

(defun supertag-tag-merge--file-conflicts (nodes)
  "Return preflight conflicts for source files referenced by NODES."
  (let (conflicts)
    (dolist (node nodes)
      (let ((file (plist-get node :file)))
        (cond
         ((or (not (stringp file)) (string-empty-p file) (not (file-exists-p file)))
          (push (list :kind :missing-file :node-id (plist-get node :id) :file file)
                conflicts))
         ((not (file-writable-p file))
          (push (list :kind :unwritable-file :node-id (plist-get node :id) :file file)
                conflicts))
         ((when-let* ((buffer (get-file-buffer file)))
            (buffer-modified-p buffer))
          (push (list :kind :unsaved-buffer :node-id (plist-get node :id) :file file)
                conflicts)))))
    (nreverse conflicts)))

(defun supertag-tag-merge--replace-tag-value (value source-ids target-id)
  "Replace SOURCE-IDS in structured VALUE with TARGET-ID."
  (cond
   ((stringp value) (if (member value source-ids) target-id value))
   ((listp value)
    (supertag-tag-merge--unique
     (mapcar (lambda (item)
               (supertag-tag-merge--replace-tag-value item source-ids target-id))
             value)))
   (t value)))

(defun supertag-tag-merge--plist-p (value)
  "Return non-nil when VALUE has a keyword plist shape."
  (and (consp value)
       (keywordp (car value))
       (zerop (% (length value) 2))))

(defun supertag-tag-merge--rewrite-structured (form source-ids target-id)
  "Rewrite SOURCE-IDS in structured FORM to TARGET-ID."
  (cond
   ((atom form) form)
   ((memq (car form) '(has-tag tag))
    (cons (car form)
          (cons (supertag-tag-merge--replace-tag-value
                 (cadr form) source-ids target-id)
                (mapcar (lambda (item)
                          (supertag-tag-merge--rewrite-structured item source-ids target-id))
                        (cddr form)))))
   ((memq (car form) '(has-any-tag has-all-tags))
    (cons (car form)
          (supertag-tag-merge--unique
           (mapcar (lambda (item)
                     (supertag-tag-merge--replace-tag-value item source-ids target-id))
                   (cdr form)))))
   ((supertag-tag-merge--plist-p form)
    (let (result)
      (while form
        (let ((key (pop form))
              (value (pop form)))
          (setq result
                (append result
                        (list key
                              (if (memq key supertag-tag-merge--tag-slot-keys)
                                  (supertag-tag-merge--replace-tag-value
                                   value source-ids target-id)
                                (supertag-tag-merge--rewrite-structured
                                 value source-ids target-id)))))))
      result))
   (t
    (mapcar (lambda (item)
              (supertag-tag-merge--rewrite-structured item source-ids target-id))
            form))))

(defun supertag-tag-merge--string-mentions-source-p (string source-ids)
  "Return non-nil when STRING mentions one of SOURCE-IDS."
  (cl-some (lambda (source)
             (string-match-p (regexp-quote source) string))
           source-ids))

(defun supertag-tag-merge--saved-query-changes (source-ids target-id)
  "Return saved-query updates and warnings for SOURCE-IDS and TARGET-ID."
  (let (updates warnings)
    (when (boundp 'supertag-query-saved)
      (dolist (entry supertag-query-saved)
        (let ((name (car entry)) (text (cdr entry)))
          (when (and (stringp text)
                     (supertag-tag-merge--string-mentions-source-p text source-ids))
            (condition-case err
                (pcase-let* ((`(,form . ,end) (read-from-string text))
                             (tail (substring text end))
                             (rewritten (supertag-tag-merge--rewrite-structured
                                         form source-ids target-id)))
                  (if (not (string-match-p "\\`[[:space:]]*\\'" tail))
                      (push (list :kind :free-text-query :name name :text text) warnings)
                    (unless (equal form rewritten)
                      (push (list :name name :old text :new (prin1-to-string rewritten))
                            updates))))
              (error
               (push (list :kind :free-text-query :name name :text text
                           :error (error-message-string err))
                     warnings)))))))
    (cons (nreverse updates) (nreverse warnings))))

(defun supertag-tag-merge--inheritance-conflicts (source-ids target-id target-exists-p)
  "Return conflicts caused by TARGET-ID's ancestry containing SOURCE-IDS."
  (let (conflicts)
    (when target-exists-p
      (let ((cursor (plist-get (supertag--ensure-plist (supertag-tag-get target-id))
                               :extends))
            (seen (make-hash-table :test 'equal)))
        (while (and cursor (not (gethash cursor seen)))
          (puthash cursor t seen)
          (when (member cursor source-ids)
            (push (list :kind :inheritance-cycle :target target-id :source cursor)
                  conflicts))
          (setq cursor
                (when-let* ((parent (supertag-tag-get cursor)))
                  (plist-get (supertag--ensure-plist parent) :extends))))))
    (nreverse conflicts)))

(defun supertag-tag-merge--value-candidates
    (node-id node-tags field-key definition tag-field-maps)
  "Return source-tag/value pairs for NODE-ID and FIELD-KEY.
NODE-TAGS is the node's current membership.  DEFINITION is the chosen
destination contract.  TAG-FIELD-MAPS maps each participant tag to its
resolved fields."
  (if supertag-use-global-fields
      (let ((relevant
             (cl-some (lambda (tag-id)
                        (and (member tag-id node-tags)
                             (gethash field-key (gethash tag-id tag-field-maps))))
                      (let (tags)
                        (maphash (lambda (tag-id _map) (push tag-id tags)) tag-field-maps)
                        tags))))
        (when relevant
          (let ((value (supertag-store-get-field-value
                        node-id field-key supertag-tag-merge--missing)))
            (unless (eq value supertag-tag-merge--missing)
              (list (list :tag-id :global :value value))))))
    (let (candidates)
      (maphash
       (lambda (tag-id field-map)
         (when (member tag-id node-tags)
           (when-let* ((source-definition (gethash field-key field-map)))
             (when (supertag-tag-merge--definitions-compatible-p
                    definition source-definition)
               (let* ((field-name (plist-get source-definition :name))
                    (value (supertag-field-get
                            node-id tag-id field-name supertag-tag-merge--missing)))
                 (unless (eq value supertag-tag-merge--missing)
                   (push (list :tag-id tag-id :value value) candidates)))))))
       tag-field-maps)
      (nreverse candidates))))

(defun supertag-tag-merge--resolve-value (node-id field-key definition candidates resolutions)
  "Resolve CANDIDATES for NODE-ID/FIELD-KEY.
DEFINITION determines whether multiple values are valid.  RESOLUTIONS maps
node/field keys to explicit choices.
Returns (VALUE-P VALUE CONFLICT), where VALUE-P distinguishes a missing
value from a resolved nil value."
  (let* ((values (supertag-tag-merge--unique
                  (mapcar (lambda (candidate) (plist-get candidate :value)) candidates)))
         (resolution-cell (assoc (list node-id field-key) resolutions))
         (resolution (cdr resolution-cell))
         (multi-p (memq (plist-get definition :type)
                        supertag-tag-merge--multi-value-types)))
    (cond
     ((null values) (list nil nil nil))
     ((null (cdr values)) (list t (car values) nil))
     ((null resolution-cell)
      (list nil nil (list :kind :field-value :node-id node-id :field field-key
                          :definition definition :candidates candidates)))
     ((and (listp resolution) (plist-member resolution :merge-values))
      (let* ((chosen (plist-get resolution :merge-values))
             (available
              (supertag-tag-merge--unique
               (apply #'append
                      (mapcar (lambda (value)
                                (if (listp value) (copy-sequence value) (list value)))
                              values)))))
        (if (and multi-p
                 (listp chosen)
                 (cl-every (lambda (value) (member value available)) chosen))
            (list t (supertag-tag-merge--unique chosen) nil)
          (list nil nil
                (list :kind :invalid-resolution :node-id node-id :field field-key
                      :definition definition :candidates candidates
                      :resolution resolution)))))
     ((cl-some (lambda (value) (equal value resolution)) values)
      (list t resolution nil))
     (t
      (list nil nil
            (list :kind :invalid-resolution :node-id node-id :field field-key
                  :definition definition :candidates candidates
                  :resolution resolution))))))

(cl-defun supertag-tag-merge-plan (tag-ids target-id
                                           &key selected-fields field-sources resolutions)
  "Build and return a tag merge plan without mutating data.
TAG-IDS are the tags participating in the merge.  TARGET-ID may name one
of them, another existing tag, or a new tag.  SELECTED-FIELDS is `:all' or
a list of source field keys; nil selects no source fields.  FIELD-SOURCES
chooses a source definition for incompatible same-key fields.  RESOLUTIONS
maps (NODE-ID FIELD-KEY) to a chosen value or `(:merge-values VALUES)'."
  (let* ((participants
          (supertag-tag-merge--unique
           (mapcar (lambda (id)
                     (unless (stringp id) (error "Invalid tag id: %S" id))
                     (supertag-sanitize-tag-name id))
                   tag-ids)))
         (target (supertag-sanitize-tag-name target-id)))
    (unless (>= (length participants) 2)
      (error "Tag merge requires at least two participating tags"))
    (dolist (tag-id participants)
      (unless (supertag-tag-get tag-id)
        (error "Tag '%s' does not exist" tag-id)))
    (let* ((target-exists-p (and (supertag-tag-get target) t))
           (source-ids (if (member target participants)
                           (remove target participants)
                         participants))
           (groups (supertag-tag-merge--field-groups source-ids))
           (available-keys (mapcar #'car groups))
           (selected-keys
            (cond
             ((eq selected-fields :all) available-keys)
             ((null selected-fields) nil)
             (supertag-use-global-fields
              (mapcar #'supertag-sanitize-field-id selected-fields))
             (t selected-fields)))
           (target-field-map (and target-exists-p
                                  (supertag-tag-merge--field-map target)))
           (field-definitions nil)
           (field-warnings nil)
           (conflicts (supertag-tag-merge--inheritance-conflicts
                       source-ids target target-exists-p)))
      (dolist (key selected-keys)
        (unless (member key available-keys)
          (error "Source field '%s' does not exist" key))
        (let* ((target-definition (and target-field-map (gethash key target-field-map)))
               (choice (if target-definition
                           (cons target-definition nil)
                         (supertag-tag-merge--source-definition
                          key (cdr (assoc key groups)) field-sources))))
          (if (cdr choice)
              (push (cdr choice) conflicts)
            (let* ((definition (car choice))
                   (ignored-tags
                    (unless supertag-use-global-fields
                      (cl-loop for candidate in (cdr (assoc key groups))
                               unless (supertag-tag-merge--definitions-compatible-p
                                       definition (plist-get candidate :definition))
                               collect (plist-get candidate :tag-id)))))
              (when ignored-tags
                (push (list :kind :incompatible-source-field-definition
                            :field key :ignored-tags ignored-tags
                            :kept-definition definition)
                      field-warnings))
              (push (cons key definition) field-definitions)))))
      (setq field-definitions (nreverse field-definitions))
      (let* ((nodes (supertag-tag-merge--affected-nodes source-ids))
             (files (supertag-tag-merge--unique
                     (delq nil (mapcar (lambda (node) (plist-get node :file)) nodes))))
             (tag-field-maps (make-hash-table :test 'equal))
             value-writes)
        (dolist (tag-id (supertag-tag-merge--unique
                         (append source-ids (when target-exists-p (list target)))))
          (puthash tag-id (supertag-tag-merge--field-map tag-id) tag-field-maps))
        (dolist (node nodes)
          (let ((node-id (plist-get node :id))
                (node-tags (plist-get node :tags)))
            (dolist (entry field-definitions)
              (let* ((key (car entry))
                     (definition (cdr entry))
                     (candidates (supertag-tag-merge--value-candidates
                                  node-id node-tags key definition tag-field-maps))
                     (resolved (supertag-tag-merge--resolve-value
                                node-id key definition candidates resolutions)))
                (when (nth 2 resolved)
                  (push (nth 2 resolved) conflicts))
                (when (car resolved)
                  (push (list :node-id node-id :field key
                              :field-name (plist-get definition :name)
                              :value (nth 1 resolved))
                        value-writes))))))
        (setq conflicts (append (nreverse conflicts)
                                (supertag-tag-merge--file-conflicts nodes)))
        (pcase-let* ((`(,query-updates . ,query-warnings)
                       (supertag-tag-merge--saved-query-changes source-ids target)))
          (list :participants participants
                :source-ids source-ids
                :target-id target
                :target-exists-p target-exists-p
                :selected-fields selected-keys
                :field-definitions field-definitions
                :value-writes (nreverse value-writes)
                :nodes nodes
                :files files
                :saved-query-updates query-updates
                :conflicts conflicts
                :warnings (append (nreverse field-warnings)
                                  query-warnings)))))))

(defun supertag-tag-merge--rewrite-node-tags (tags source-ids target-id)
  "Replace SOURCE-IDS in TAGS with TARGET-ID and deduplicate."
  (supertag-tag-merge--unique
   (mapcar (lambda (tag-id) (if (member tag-id source-ids) target-id tag-id))
           tags)))

(defun supertag-tag-merge--install-target-schema (plan)
  "Create or extend PLAN's destination schema."
  (let ((target (plist-get plan :target-id))
        (definitions (plist-get plan :field-definitions)))
    (unless (plist-get plan :target-exists-p)
      (supertag-tag-create
       (list :id target :name target
             :fields (unless supertag-use-global-fields
                       (mapcar (lambda (entry) (copy-tree (cdr entry))) definitions)))))
    (dolist (entry definitions)
      (let ((key (car entry)) (definition (cdr entry)))
        (unless (supertag-tag-get-field target key)
          (if supertag-use-global-fields
              (supertag-tag-associate-field target key)
            (supertag-tag-add-field target (copy-tree definition))))))))

(defun supertag-tag-merge--write-field-values (plan)
  "Write resolved legacy field values described by PLAN."
  (unless supertag-use-global-fields
    (dolist (write (plist-get plan :value-writes))
      (supertag-field-set (plist-get write :node-id)
                          (plist-get plan :target-id)
                          (plist-get write :field-name)
                          (plist-get write :value)))))

(defun supertag-tag-merge--rewrite-nodes (plan)
  "Rewrite affected node tag lists from PLAN."
  (let ((sources (plist-get plan :source-ids))
        (target (plist-get plan :target-id)))
    (dolist (node (plist-get plan :nodes))
      (let ((node-id (plist-get node :id)))
        (supertag-node-update
         node-id
         (lambda (current)
           (let ((copy (copy-sequence current)))
             (plist-put copy :tags
                        (supertag-tag-merge--rewrite-node-tags
                         (or (plist-get current :tags) '()) sources target)))))))))

(defun supertag-tag-merge--tag-has-global-field-p (tag-id field-id)
  "Return non-nil when TAG-ID resolves FIELD-ID in global field mode."
  (gethash field-id (supertag-tag-merge--field-map tag-id)))

(defun supertag-tag-merge--cleanup-source-fields (plan)
  "Remove field storage owned only by PLAN's source tags."
  (let ((sources (plist-get plan :source-ids)))
    (if supertag-use-global-fields
        (let ((source-field-ids
               (supertag-tag-merge--unique
                (apply #'append
                       (mapcar (lambda (tag-id)
                                 (let (keys)
                                   (maphash (lambda (key _field) (push key keys))
                                            (supertag-tag-merge--field-map tag-id))
                                   keys))
                               sources)))))
          (dolist (node (plist-get plan :nodes))
            (let* ((node-id (plist-get node :id))
                   (remaining-tags (plist-get (supertag-node-get node-id) :tags)))
              (dolist (field-id source-field-ids)
                (unless (cl-some (lambda (tag-id)
                                   (supertag-tag-merge--tag-has-global-field-p tag-id field-id))
                                 remaining-tags)
                  (supertag-store-remove-field-value node-id field-id))))))
      (dolist (node (plist-get plan :nodes))
        (dolist (source sources)
          (supertag-store-remove-legacy-tag-fields (plist-get node :id) source))))))

(defun supertag-tag-merge--rewrite-inheritance-and-delete-sources (plan)
  "Reparent source children and remove source definitions from PLAN."
  (let ((sources (plist-get plan :source-ids))
        (target (plist-get plan :target-id))
        updates)
    (maphash
     (lambda (tag-id raw-tag)
       (let ((tag (supertag--ensure-plist raw-tag)))
         (when (and (not (member tag-id sources))
                    (member (plist-get tag :extends) sources))
           (push tag-id updates))))
     (supertag-store-get-collection :tags))
    (dolist (tag-id updates)
      (supertag--set-tag-parent tag-id target))
    (dolist (source sources)
      (when supertag-use-global-fields
        (supertag-store-remove-tag-field-associations source))
      (supertag-tag-delete source))))

(defun supertag-tag-merge--rewrite-relations (source-ids target-id)
  "Rewrite relation endpoints and identities from SOURCE-IDS to TARGET-ID."
  (let ((relations (supertag-store-get-collection :relations))
        (result (make-hash-table :test 'equal))
        unaffected affected)
    (maphash
     (lambda (_id relation)
       (if (or (member (plist-get relation :from) source-ids)
               (member (plist-get relation :to) source-ids))
           (push relation affected)
         (push relation unaffected)))
     relations)
    (dolist (relation (append unaffected affected))
      (let* ((copy (copy-tree relation))
             (from (supertag-tag-merge--replace-tag-value
                    (plist-get copy :from) source-ids target-id))
             (to (supertag-tag-merge--replace-tag-value
                  (plist-get copy :to) source-ids target-id))
             (type (plist-get copy :type))
             (id (supertag-generate-relation-id from to type)))
        (setq copy (plist-put copy :from from))
        (setq copy (plist-put copy :to to))
        (setq copy (plist-put copy :id id))
        (unless (gethash id result)
          (puthash id copy result))))
    (supertag-update (list :relations) result)))

(defun supertag-tag-merge--rewrite-automations (source-ids target-id)
  "Rewrite SOURCE-IDS to TARGET-ID in stored automations."
  (let ((automations (supertag-store-get-collection :automations)) updates)
    (maphash
     (lambda (id automation)
       (let ((rewritten (supertag-tag-merge--rewrite-structured
                         automation source-ids target-id)))
         (unless (equal automation rewritten)
           (push (cons id rewritten) updates))))
     automations)
    (dolist (entry updates)
      (supertag-store-put-entity :automations (car entry) (cdr entry) t))
    (length updates)))

(defun supertag-tag-merge--copy-view-configs ()
  "Return a deep copy of loaded view configs, or nil when unavailable."
  (when (and (boundp 'supertag--view-configs)
             (hash-table-p supertag--view-configs))
    (let ((copy (make-hash-table :test (hash-table-test supertag--view-configs))))
      (maphash (lambda (key value) (puthash key (copy-tree value) copy))
               supertag--view-configs)
      copy)))

(defun supertag-tag-merge--rewrite-view-configs (source-ids target-id)
  "Rewrite SOURCE-IDS to TARGET-ID in loaded view configurations."
  (when (and (boundp 'supertag--view-configs)
             (hash-table-p supertag--view-configs))
    (maphash
     (lambda (key config)
       (puthash key
                (supertag-tag-merge--rewrite-structured config source-ids target-id)
                supertag--view-configs))
     supertag--view-configs)))

(defun supertag-tag-merge--apply-query-updates (updates)
  "Apply saved query UPDATES, rejecting stale source text."
  (when (and updates (boundp 'supertag-query-saved))
    (dolist (update updates)
      (let* ((name (plist-get update :name))
             (cell (assoc name supertag-query-saved)))
        (unless (and cell (equal (cdr cell) (plist-get update :old)))
          (error "Saved query '%s' changed after merge preview" name))
        (setcdr cell (plist-get update :new))))))

(defun supertag-tag-merge--snapshot-files (files)
  "Copy FILES to a temporary recovery directory."
  (when files
    (let ((dir (make-temp-file "supertag-tag-merge-backup" t))
          snapshots
          (index 0))
      (dolist (file files)
        (let ((backup (expand-file-name (number-to-string index) dir)))
          (copy-file file backup t t)
          (push (cons file backup) snapshots)
          (setq index (1+ index))))
      (list :dir dir :files (nreverse snapshots)))))

(defun supertag-tag-merge--restore-files (snapshot)
  "Restore files from SNAPSHOT and refresh visiting buffers."
  (dolist (entry (plist-get snapshot :files))
    (copy-file (cdr entry) (car entry) t t)
    (when-let* ((buffer (get-file-buffer (car entry))))
      (with-current-buffer buffer
        (revert-buffer t t t)))))

(defun supertag-tag-merge--delete-snapshot (snapshot)
  "Delete temporary SNAPSHOT data."
  (when-let* ((dir (plist-get snapshot :dir)))
    (ignore-errors (delete-directory dir t))))

(defun supertag-tag-merge--rebuild-derived-state ()
  "Rebuild caches and indexes touched by a tag merge."
  (supertag-index-rebuild-relations)
  (supertag-ops-schema-rebuild-cache)
  (when (fboundp 'supertag-rebuild-rule-index)
    (supertag-rebuild-rule-index)))

(defun supertag-tag-merge--rewrite-files (plan)
  "Rewrite source tag text in PLAN's Org files and return change count."
  (let ((total 0)
        (target (plist-get plan :target-id))
        (files (plist-get plan :files)))
    (dolist (source (plist-get plan :source-ids) total)
      (setq total
            (+ total
               (or (supertag-view-helper-rename-tag-text-in-files
                    source target files)
                   0))))))

(defun supertag-tag-merge-execute (plan)
  "Execute a conflict-free tag merge PLAN.
Signals before writing when PLAN contains conflicts.  Store changes roll
back through `supertag-with-transaction'; Org files and loaded registries
are restored from snapshots if any later step fails."
  (when (plist-get plan :conflicts)
    (error "Tag merge has %d unresolved conflict(s)"
           (length (plist-get plan :conflicts))))
  (let* ((fresh-file-conflicts
          (supertag-tag-merge--file-conflicts (plist-get plan :nodes)))
         (query-before (and (boundp 'supertag-query-saved)
                            (copy-tree supertag-query-saved)))
         (views-before (supertag-tag-merge--copy-view-configs))
         (snapshot nil)
         (keep-snapshot nil))
    (when fresh-file-conflicts
      (error "Tag merge file preflight failed: %S" fresh-file-conflicts))
    (setq snapshot (supertag-tag-merge--snapshot-files (plist-get plan :files)))
    (unwind-protect
        (condition-case err
            (let ((file-changes
                   (supertag-with-transaction
                     (supertag-tag-merge--install-target-schema plan)
                     (supertag-tag-merge--write-field-values plan)
                     (supertag-tag-merge--rewrite-nodes plan)
                     (supertag-tag-merge--cleanup-source-fields plan)
                     (supertag-tag-merge--rewrite-inheritance-and-delete-sources plan)
                     (supertag-tag-merge--rewrite-relations
                      (plist-get plan :source-ids) (plist-get plan :target-id))
                     (supertag-tag-merge--rewrite-automations
                      (plist-get plan :source-ids) (plist-get plan :target-id))
                     (supertag-tag-merge--rewrite-view-configs
                      (plist-get plan :source-ids) (plist-get plan :target-id))
                     (supertag-tag-merge--apply-query-updates
                      (plist-get plan :saved-query-updates))
                     (let ((count (supertag-tag-merge--rewrite-files plan)))
                       ;; A cache/index failure is still a merge failure, so keep
                       ;; it inside the store transaction and restore the files.
                       (supertag-tag-merge--rebuild-derived-state)
                       count))))
              (list :status :merged
                    :target-id (plist-get plan :target-id)
                    :source-ids (plist-get plan :source-ids)
                    :node-count (length (plist-get plan :nodes))
                    :file-change-count file-changes
                    :warnings (plist-get plan :warnings)))
          (error
           (let (restore-error)
             (when snapshot
               (condition-case file-error
                   (supertag-tag-merge--restore-files snapshot)
                 (error
                  (setq restore-error file-error)
                  (setq keep-snapshot t))))
             (when (boundp 'supertag-query-saved)
               (setq supertag-query-saved query-before))
             (when (and (boundp 'supertag--view-configs) views-before)
               (setq supertag--view-configs views-before))
             (ignore-errors (supertag-tag-merge--rebuild-derived-state))
             (if restore-error
                 (error "Tag merge failed (%s), and file recovery failed (%s).  Backups kept at %s"
                        (error-message-string err)
                        (error-message-string restore-error)
                        (plist-get snapshot :dir))
               (signal (car err) (cdr err))))))
      (unless keep-snapshot
        (supertag-tag-merge--delete-snapshot snapshot)))))

;;; --- Namespace branch rename ---

(defun supertag-tag-path-rename--mapped (value mapping)
  "Return VALUE's replacement from MAPPING, or VALUE when unchanged."
  (or (cdr (assoc value mapping)) value))

(defun supertag-tag-path-rename--rewrite-values (value mapping)
  "Recursively rewrite exact tag identifiers in VALUE using MAPPING."
  (cond
   ((stringp value)
    (supertag-tag-path-rename--mapped value mapping))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash
       (lambda (key item)
         (puthash key
                  (supertag-tag-path-rename--rewrite-values item mapping)
                  copy))
       value)
      copy))
   ((consp value)
    (mapcar
     (lambda (item)
       (supertag-tag-path-rename--rewrite-values item mapping))
     value))
   (t value)))

(defun supertag-tag-path-rename--rewrite-structured (form mapping)
  "Rewrite tag identifiers in structured FORM according to MAPPING."
  (cond
   ((atom form) form)
   ((memq (car form) '(has-tag tag))
    (cons (car form)
          (cons (supertag-tag-path-rename--mapped (cadr form) mapping)
                (mapcar
                 (lambda (item)
                   (supertag-tag-path-rename--rewrite-structured item mapping))
                 (cddr form)))))
   ((memq (car form) '(has-any-tag has-all-tags))
    (cons (car form)
          (mapcar
           (lambda (item)
             (supertag-tag-path-rename--mapped item mapping))
           (cdr form))))
   ((supertag-tag-merge--plist-p form)
    (let (result)
      (while form
        (let ((key (pop form))
              (value (pop form)))
          (setq result
                (append
                 result
                 (list
                  key
                  (if (memq key supertag-tag-merge--tag-slot-keys)
                      (cond
                       ((stringp value)
                        (supertag-tag-path-rename--mapped value mapping))
                       ((listp value)
                        (mapcar
                         (lambda (item)
                           (supertag-tag-path-rename--mapped item mapping))
                         value))
                       (t value))
                    (supertag-tag-path-rename--rewrite-structured
                     value mapping)))))))
      result))
   (t
    (mapcar
     (lambda (item)
       (supertag-tag-path-rename--rewrite-structured item mapping))
     form))))

(defun supertag-tag-path-rename--saved-query-changes (mapping)
  "Return (UPDATES . CONFLICTS) for saved queries touched by MAPPING."
  (let ((sources (mapcar #'car mapping))
        updates conflicts)
    (when (boundp 'supertag-query-saved)
      (dolist (entry supertag-query-saved)
        (let ((name (car entry))
              (text (cdr entry)))
          (when (and (stringp text)
                     (supertag-tag-merge--string-mentions-source-p
                      text sources))
            (condition-case err
                (pcase-let* ((`(,form . ,end) (read-from-string text))
                             (tail (substring text end))
                             (rewritten
                              (supertag-tag-path-rename--rewrite-structured
                               form mapping)))
                  (if (string-match-p "\\`[[:space:]]*\\'" tail)
                      (unless (equal form rewritten)
                        (push (list :name name :old text
                                    :new (prin1-to-string rewritten))
                              updates))
                    (push (list :kind :free-text-query
                                :name name :text text)
                          conflicts)))
              (error
               (push (list :kind :invalid-saved-query
                           :name name :text text
                           :error (error-message-string err))
                     conflicts)))))))
    (cons (nreverse updates) (nreverse conflicts))))

(cl-defun supertag-tag-path-rename-plan (old-root new-root)
  "Build a conflict-checked rename plan for OLD-ROOT and its descendants."
  (let* ((old (supertag-sanitize-tag-name old-root))
         (new (supertag-sanitize-tag-name new-root)))
    (unless (and (supertag-tag-path-valid-p old)
                 (supertag-tag-path-valid-p new))
      (error "Tag paths cannot contain empty segments"))
    (unless (supertag-tag-get old)
      (error "Tag '%s' not found" old))
    (when (or (equal old new)
              (supertag-tag-path-descendant-p new old))
      (error "Cannot move tag path '%s' into its own namespace '%s'"
             old new))
    (let (sources)
      (maphash
       (lambda (tag-id _tag)
         (when (or (equal tag-id old)
                   (supertag-tag-path-descendant-p tag-id old))
           (push tag-id sources)))
       (supertag-store-get-collection :tags))
      (setq sources (sort sources #'string<))
      (let* ((mapping
              (mapcar
               (lambda (source)
                 (cons source
                       (supertag-tag-path-rebase source old new)))
               sources))
             (source-set sources)
             (collision-conflicts
              (cl-loop
               for (_source . target) in mapping
               when (and (supertag-tag-get target)
                         (not (member target source-set)))
               collect (list :kind :tag-id-collision :target-id target)))
             (nodes (supertag-tag-merge--affected-nodes sources))
             (file-backed-nodes
              (cl-remove-if-not
               (lambda (node) (stringp (plist-get node :file)))
               nodes))
             (files
              (supertag-tag-merge--unique
               (mapcar (lambda (node) (plist-get node :file))
                       file-backed-nodes)))
             (query-result
              (supertag-tag-path-rename--saved-query-changes mapping)))
        (list :old-root old
              :target-id new
              :mapping mapping
              :nodes nodes
              :files files
              :saved-query-updates (car query-result)
              :conflicts
              (append collision-conflicts
                      (supertag-tag-merge--file-conflicts file-backed-nodes)
                      (cdr query-result)))))))

(defun supertag-tag-path-rename--rewrite-tags (mapping)
  "Rekey tag entities and inheritance references using MAPPING."
  (let ((result (make-hash-table :test 'equal)))
    (maphash
     (lambda (tag-id raw-tag)
       (let* ((new-id (supertag-tag-path-rename--mapped tag-id mapping))
              (tag
               (supertag-tag-path-rename--rewrite-structured
                (copy-tree (supertag--ensure-plist raw-tag))
                mapping))
              (extends (plist-get tag :extends)))
         (when (assoc tag-id mapping)
           (setq tag (plist-put tag :id new-id))
           (setq tag (plist-put tag :name new-id)))
         (when extends
           (setq tag
                 (plist-put
                  tag :extends
                  (supertag-tag-path-rename--mapped extends mapping))))
         (when (gethash new-id result)
           (error "Tag rename produced duplicate id '%s'" new-id))
         (puthash new-id tag result)))
     (supertag-store-get-collection :tags))
    (supertag-update '(:tags) result)))

(defun supertag-tag-path-rename--rewrite-nodes (mapping)
  "Rewrite node tag lists using MAPPING."
  (let* ((nodes (supertag-store-get-collection :nodes))
         (result (copy-hash-table nodes)))
    (maphash
     (lambda (node-id raw-node)
       (let* ((node (copy-tree (supertag--ensure-plist raw-node)))
              (tags (plist-get node :tags))
              (rewritten
               (mapcar
                (lambda (tag-id)
                  (supertag-tag-path-rename--mapped tag-id mapping))
                tags)))
         (unless (equal tags rewritten)
           (puthash node-id
                    (plist-put node :tags
                               (supertag-tag-merge--unique rewritten))
                    result))))
     nodes)
    (supertag-update '(:nodes) result)))

(defun supertag-tag-path-rename--rewrite-relations (mapping)
  "Rewrite relation endpoints and deterministic IDs using MAPPING."
  (let ((result (make-hash-table :test 'equal)))
    (maphash
     (lambda (_relation-id raw-relation)
       (let* ((relation
               (supertag-tag-path-rename--rewrite-structured
                (copy-tree raw-relation) mapping))
              (from (supertag-tag-path-rename--mapped
                     (plist-get relation :from) mapping))
              (to (supertag-tag-path-rename--mapped
                   (plist-get relation :to) mapping))
              (type (plist-get relation :type))
              (new-id (supertag-generate-relation-id from to type)))
         (setq relation (plist-put relation :from from))
         (setq relation (plist-put relation :to to))
         (setq relation (plist-put relation :id new-id))
         (when (gethash new-id result)
           (error "Tag rename produced duplicate relation '%s'" new-id))
         (puthash new-id relation result)))
     (supertag-store-get-collection :relations))
    (supertag-update '(:relations) result)))

(defun supertag-tag-path-rename--rewrite-associations (mapping)
  "Rekey global field associations using MAPPING."
  (let ((result (make-hash-table :test 'equal)))
    (maphash
     (lambda (tag-id associations)
       (let ((new-id (supertag-tag-path-rename--mapped tag-id mapping)))
         (when (gethash new-id result)
           (error "Tag rename produced duplicate field associations '%s'"
                  new-id))
         (puthash new-id (copy-tree associations) result)))
     (supertag-store-get-collection :tag-field-associations))
    (supertag-update '(:tag-field-associations) result)))

(defun supertag-tag-path-rename--rewrite-legacy-fields (mapping)
  "Rekey per-node legacy field tables using MAPPING."
  (let ((fields-root (supertag-store-get-collection :fields)))
    (maphash
     (lambda (node-id tag-table)
       (let ((new-tag-table (make-hash-table :test 'equal)))
         (maphash
          (lambda (tag-id fields)
            (let ((new-id
                   (supertag-tag-path-rename--mapped tag-id mapping))
                  (new-fields (make-hash-table :test 'equal)))
              (when (gethash new-id new-tag-table)
                (error "Tag rename produced duplicate legacy fields '%s'"
                       new-id))
              (maphash
               (lambda (field-name value)
                 (puthash
                  field-name
                  (if (eq (plist-get
                           (supertag-tag-get-field new-id field-name)
                           :type)
                          :tag)
                      (supertag-tag-path-rename--rewrite-values value mapping)
                    value)
                  new-fields))
               fields)
              (puthash new-id new-fields new-tag-table)))
          tag-table)
         (unless (equal tag-table new-tag-table)
           ;; `:fields' is a nested legacy collection.  Replace one node
           ;; bucket directly so batch notifications never misclassify the
           ;; whole hash table as a field entity.
           (supertag--transaction-record-old-value
            (list :fields node-id) t tag-table)
           (puthash node-id new-tag-table fields-root))))
     fields-root)))

(defun supertag-tag-path-rename--rewrite-global-field-values (mapping)
  "Rewrite exact tag references in global field values using MAPPING."
  (let (updates)
    (maphash
     (lambda (node-id field-table)
       (when (hash-table-p field-table)
         (maphash
          (lambda (field-id value)
            (when (eq (plist-get
                       (supertag-store-get-field-definition field-id)
                       :type)
                      :tag)
              (let ((rewritten
                     (supertag-tag-path-rename--rewrite-values value mapping)))
                (unless (equal value rewritten)
                  (push (list node-id field-id rewritten) updates)))))
          field-table)))
     (supertag-store-get-collection :field-values))
    (dolist (update updates)
      (supertag-store-put-field-value
       (nth 0 update) (nth 1 update) (nth 2 update) t))))

(defun supertag-tag-path-rename--rewrite-store-configs (mapping)
  "Rewrite structured tag references in Store configuration collections."
  (dolist (collection '(:automations :boards))
    (let ((bucket (supertag-store-get-collection collection))
          (result (make-hash-table :test 'equal)))
      (maphash
       (lambda (id value)
         (puthash id
                  (supertag-tag-path-rename--rewrite-structured
                   value mapping)
                  result))
       bucket)
      (supertag-update (list collection) result))))

(defun supertag-tag-path-rename--rewrite-view-configs (mapping)
  "Rewrite tag references in loaded view configurations."
  (when (and (boundp 'supertag--view-configs)
             (hash-table-p supertag--view-configs))
    (maphash
     (lambda (id config)
       (puthash id
                (supertag-tag-path-rename--rewrite-structured
                 config mapping)
                supertag--view-configs))
     supertag--view-configs)))

(defun supertag-tag-path-rename--rewrite-files (plan)
  "Apply PLAN's complete path mapping to its Org files."
  (let ((total 0)
        (files (plist-get plan :files)))
    (dolist (entry (plist-get plan :mapping) total)
      (setq total
            (+ total
               (supertag-view-helper-rename-tag-text-in-files
                (car entry) (cdr entry) files))))))

(defun supertag-tag-path-rename-execute (plan)
  "Execute conflict-free namespace rename PLAN with rollback."
  (when (plist-get plan :conflicts)
    (error "Tag path rename has %d conflict(s): %S"
           (length (plist-get plan :conflicts))
           (plist-get plan :conflicts)))
  (when-let* ((fresh-conflicts
               (supertag-tag-merge--file-conflicts
                (cl-remove-if-not
                 (lambda (node) (stringp (plist-get node :file)))
                 (plist-get plan :nodes)))))
    (error "Tag path rename file preflight failed: %S" fresh-conflicts))
  (let* ((mapping (plist-get plan :mapping))
         (query-before (and (boundp 'supertag-query-saved)
                            (copy-tree supertag-query-saved)))
         (views-before (supertag-tag-merge--copy-view-configs))
         (snapshot (supertag-tag-merge--snapshot-files
                    (plist-get plan :files)))
         (keep-snapshot nil))
    (unwind-protect
        (condition-case err
            (let ((file-changes
                   (supertag-with-transaction
                     (supertag-tag-path-rename--rewrite-tags mapping)
                     (supertag-tag-path-rename--rewrite-nodes mapping)
                     (supertag-tag-path-rename--rewrite-relations mapping)
                     (supertag-tag-path-rename--rewrite-associations mapping)
                     (supertag-tag-path-rename--rewrite-legacy-fields mapping)
                     (supertag-tag-path-rename--rewrite-global-field-values
                      mapping)
                     (supertag-tag-path-rename--rewrite-store-configs mapping)
                     (supertag-tag-path-rename--rewrite-view-configs mapping)
                     (supertag-tag-merge--apply-query-updates
                      (plist-get plan :saved-query-updates))
                     (let ((count
                            (supertag-tag-path-rename--rewrite-files plan)))
                       (supertag-tag-merge--rebuild-derived-state)
                       count))))
              (list :status :renamed
                    :target-id (plist-get plan :target-id)
                    :mapping mapping
                    :node-count (length (plist-get plan :nodes))
                    :file-change-count file-changes))
          (error
           (let (restore-error)
             (when snapshot
               (condition-case file-error
                   (supertag-tag-merge--restore-files snapshot)
                 (error
                  (setq restore-error file-error)
                  (setq keep-snapshot t))))
             (when (boundp 'supertag-query-saved)
               (setq supertag-query-saved query-before))
             (when (and (boundp 'supertag--view-configs) views-before)
               (setq supertag--view-configs views-before))
             (ignore-errors (supertag-tag-merge--rebuild-derived-state))
             (if restore-error
                 (error
                  "Tag path rename failed (%s), file recovery failed (%s); backups kept at %s"
                  (error-message-string err)
                  (error-message-string restore-error)
                  (plist-get snapshot :dir))
               (signal (car err) (cdr err))))))
      (unless keep-snapshot
        (supertag-tag-merge--delete-snapshot snapshot)))))

(provide 'supertag-ops-tag-merge)
;;; supertag-ops-tag-merge.el ends here

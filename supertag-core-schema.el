;;; org-supertag/schema.el --- Data schema definitions and validation for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides essential type definitions and conversion functions
;; for the hybrid architecture. Unlike the old schema system, this simplified
;; version only handles type conversion for field operations, without complex
;; normalization or validation.
;;
;; What's removed in hybrid architecture:
;; - Complex schema definitions with defaults and validators
;; - Schema normalization process (supertag-normalize)
;; - Schema caching mechanisms
;;
;; What's retained:
;; - Field type definitions (for field operations)
;; - Relation type definitions (for relation validation)
;; - Type conversion functions (for field value processing)

;;; Code:

(require 'cl-lib) ; For cl-find, cl-loop, etc.
(require 'ht)
(require 'subr-x)
(require 'supertag-core-store)

;;; --- Cache for schema definitions ---

(defvar supertag--schema-cache (make-hash-table :test 'eq)
  "Cache for compiled schema definitions to improve performance.")

(defvar supertag-schema--registry (make-hash-table :test 'eq)
  "User-registered schema overrides keyed by TYPE keyword.")

(defvar supertag--registered-entity-types nil
  "Extra entity types registered at runtime.
This is used by validation helpers (e.g. `supertag--valid-entity-type-p`).")

(defcustom supertag-schema-registration-functions nil
  "Functions called to register custom schemas and entity types.

Each function is called with no arguments during `supertag-init`.
Use `supertag-schema-register' and/or `supertag-register-entity-type' inside
these functions."
  :type '(repeat function)
  :group 'org-supertag)

(defvar supertag--global-field-cache (make-hash-table :test 'equal)
  "Cache of global field definitions keyed by field-id (slug).")

(defvar supertag--tag-field-order-cache (make-hash-table :test 'equal)
  "Cache of tag -> ordered field-ids for the global field model.")

(defcustom supertag-use-global-fields nil
  "When non-nil, enable the global field model (opt-in).
This gates new storage collections (:field-definitions, :tag-field-associations,
and :field-values). Legacy nested field storage remains untouched while this
flag is nil."
  :type 'boolean
  :group 'org-supertag)

(defun supertag--maybe-rebuild-global-field-caches ()
  "Rebuild global field caches when enabled."
  (when supertag-use-global-fields
    (supertag-schema-rebuild-global-field-caches)))

(defun supertag-sanitize-field-id (name)
  "Return sanitized field id slug for NAME.
Lowercase, trim whitespace, convert internal whitespace to underscores."
  (when (and name (stringp name))
    (let* ((trimmed (string-trim name))
           (lower (downcase trimmed))
           (collapsed (replace-regexp-in-string "[ \t\r\n]+" "_" lower)))
      (when (not (string-empty-p collapsed))
        collapsed))))

;;; --- Core Type Definitions for Hybrid Architecture ---

;; Note: In hybrid architecture, we use direct validation functions
;; instead of complex schema definitions. These type definitions
;; are only used for field operations and relation validation.

;;; --- Entity Types ---

(defconst supertag-entity-types
  '(:node    ; Org node (headline with tags)
    :tag     ; Tag (supertag with field definitions)
    :database ; Virtual database entity
    :behavior ; Behavior entity
    :automation ; Automation entity
    :embed)   ; Embed entity for embedded content blocks
  "Entity types supported by the system.")

;;; --- Field Types ---

(defconst supertag-field-types
  '(:string    ; String type
    :number    ; Numeric type
    :integer   ; Integer type
    :boolean   ; Boolean type
    :date      ; Date type
    :timestamp ; Timestamp type
    :options   ; Options type
    :url       ; URL type
    :email     ; Email type
    :tag       ; Tag reference type
    :node-reference) ; Node reference type
  "List of supported field types.")

;;; --- Relation Types ---

(defconst supertag-relation-types
  "List of supported relation types including Notion-style relations."
  '(:node-tag      ; Node-tag relationship
    :node-node     ; Node-node relationship  
    :tag-tag       ; Tag-tag relationship
    :node-field    ; Node-field relationship
    :tag-field     ; Tag-field relationship
    :parent-child  ; Parent-child relationship
    :reference     ; Reference relationship
    :custom        ; Custom relationship
    ;; Notion-style relations
    :one-to-one    ; One-to-one relationship
    :one-to-many   ; One-to-many relationship
    :many-to-many  ; Many-to-many relationship
    :rollup        ; Rollup calculation relationship
    :formula       ; Formula-based relationship
    :sync-field ; Field synchronization relationship
    :automation))  ; Automation trigger relationship

;;; --- Behavior Types ---

(defconst supertag-behavior-trigger-types
  "List of supported behavior trigger types."
  '(:on-add        ; Execute when tag is added to node
    :on-remove     ; Execute when tag is removed from node
    :on-change     ; Execute when node with tag is modified
    :on-schedule   ; Execute at scheduled times
    :on-field-change ; Execute when specific field changes
    :on-relation-add    ; Execute when relation is added
    :on-relation-remove ; Execute when relation is removed
    :always        ; Execute on all events
    :manual))      ; Execute only when manually triggered

(defconst supertag-automation-action-types
  "List of supported automation action types."
  '(:update-property     ; Update node property
    :sync-field       ; Sync field between related nodes
    :calculate-rollup    ; Calculate rollup value
    :execute-formula     ; Execute formula
    :send-notification   ; Send notification
    :create-relation     ; Create new relation
    :update-relation     ; Update existing relation
    :delete-relation     ; Delete relation
    :run-script         ; Execute external script
    :call-function))    ; Call custom function

;;; --- Global field caches (opt-in) ---

(defun supertag-schema-clear-global-field-caches ()
  "Clear global field caches."
  (clrhash supertag--global-field-cache)
  (clrhash supertag--tag-field-order-cache))

(defun supertag-schema--normalize-plist (data)
  "Return DATA as a plist, converting hash tables when needed."
  (cond
   ((hash-table-p data)
    (let (plist)
      (maphash (lambda (k v)
                 (setq plist (plist-put plist k v)))
               data)
      plist))
   ((listp data) data)
   (t nil)))

(defun supertag-schema--load-global-fields ()
  "Populate global field caches from the store when enabled."
  (when supertag-use-global-fields
    (let ((defs (supertag-store-get-collection :field-definitions))
          (assoc-table (supertag-store-get-collection :tag-field-associations)))
      (supertag-schema-clear-global-field-caches)
      (when (hash-table-p defs)
        (maphash
         (lambda (fid raw)
           (let* ((plist (supertag-schema--normalize-plist raw))
                  (sanitized (or (plist-get plist :id) (supertag-sanitize-field-id fid))))
             (when sanitized
               (puthash sanitized plist supertag--global-field-cache))))
         defs))
      (when (hash-table-p assoc-table)
        (maphash
         (lambda (tag-id entries)
           (let* ((order
                   (cond
                    ;; preferred: list of plists with :field-id and optional :order
                    ((and (listp entries) (plistp (car entries)))
                     (mapcar (lambda (entry) (plist-get entry :field-id)) entries))
                    ;; fallback: list of field ids
                    ((listp entries) entries)
                    (t nil))))
             (when order
               (puthash tag-id order supertag--tag-field-order-cache))))
         assoc-table)))))

(defun supertag-schema-rebuild-global-field-caches ()
  "Rebuild global field caches from store (no-op unless flag is enabled)."
  (interactive)
  (if (not supertag-use-global-fields)
      (progn
        (supertag-schema-clear-global-field-caches)
        (when (called-interactively-p 'interactive)
          (message "Global field caches cleared (global fields disabled).")))
    (supertag-schema--load-global-fields)
    (when (called-interactively-p 'interactive)
      (message "Global field caches rebuilt (%d fields, %d tag associations)."
               (hash-table-count supertag--global-field-cache)
               (hash-table-count supertag--tag-field-order-cache)))))

;;; --- Database Schema Types ---

(defconst supertag-database-field-types
  "Field types for Tag-based virtual databases."
  '(:title       ; Title field (single line text)
    :text        ; Multi-line text
    :number      ; Number field
    :select      ; Single select from options
    :multi-select ; Multiple select from options
    :date        ; Date field
    :person      ; Person/user field
    :files       ; File attachments
    :checkbox    ; Boolean checkbox
    :url         ; URL field
    :email       ; Email field
    :phone       ; Phone number
    :formula     ; Calculated formula field
    :relation    ; Relation to other database
    :rollup      ; Rollup from related records
    :created-time ; Auto-created timestamp
    :created-by  ; Auto-created by user
    :last-edited-time ; Auto-updated timestamp
    :last-edited-by)) ; Auto-updated by user

;;; --- 3. Data Validation and Normalization ---

;;; --- 2. Type Conversion Functions ---
;; Used for field value conversion in hybrid architecture

(defun supertag--get-schema (type)
  "Get the schema for TYPE, including user-registered overrides."
  (or (gethash type supertag--schema-cache)
      (let ((schema (or (gethash type supertag-schema--registry)
                        (pcase type
                      (:node '(:id (:type :string :required t :validator supertag--valid-id-p)
                               :title (:type :string :required t :default "")
                               :tags (:type :list :default nil)
                               :content (:type :string :default "")
                               :file (:type :string :required nil)
                               :position (:type :integer :required t)
                               :created-at (:type :timestamp :default (lambda () (current-time)))
                               :modified-at (:type :timestamp :default (lambda () (current-time)))
                               :properties (:type :plist :default nil)
                               :hash (:type :string :default nil)
                               :raw-value (:type :string :default nil)
                               :pos (:type :integer :default nil)
                               :olp (:type :list :default nil)
                               :level (:type :integer :default nil)
                               :scheduled (:type :timestamp :default nil)
                               :deadline (:type :timestamp :default nil)
                               :todo (:type :string :default nil)
                               :priority (:type :string :default nil)
                               :ref-to (:type :list :default nil)
                               :ref-from (:type :list :default nil)
                               :ref-count (:type :integer :default 0)))
                      (:tag '(:id (:type :string :required t :validator supertag--valid-id-p)
                              :name (:type :string :required t :default "")
                              :fields (:type :list :default nil)
                              :created-at (:type :timestamp :default (lambda () (current-time)))
                              :modified-at (:type :timestamp :default (lambda () (current-time)))
                              :description (:type :string :default "")
                              :icon (:type :string :default nil)
                              :extends (:type :string :default nil)
                              :color (:type :string :default nil)
                              :behaviors (:type :list :default nil)
                              ;; Virtual database configuration
                              :database-type (:type :keyword :default nil)
                              :views (:type :list :default nil)))
                      (:field '(:name (:type :string :required t)
                                :type (:type :keyword :required t :validator supertag--valid-field-type-p)
                                :options (:type :list :default nil)
                                :default (:type :any)
                                :required (:type :boolean :default nil)
                                :validator (:type :function :default nil)))
                      (:relation '(:type (:type :keyword :required t :validator supertag--valid-relation-type-p)
                                  :from (:type :string :required t :validator supertag--valid-id-p)
                                  :to (:type :string :required t :validator supertag--valid-id-p)
                                  :props (:type :plist :default nil)
                                  :created-at (:type :timestamp :default (lambda () (current-time)))
                                  :strength (:type :number :default 1.0)
                                  ;; Notion-style relation properties
                                  :sync-direction (:type :keyword :default :unidirectional)
                                  :sync-fields (:type :list :default nil)
                                  :rollup-field (:type :string :default nil)
                                  :rollup-function (:type :function :default nil)))
                      (:behavior '(:id (:type :string :required t :validator supertag--valid-id-p)
                                  :name (:type :string :required t)
                                  :trigger (:type :keyword :required t :validator supertag--valid-behavior-trigger-p)
                                  :condition (:type :list :default nil)
                                  :action (:type :keyword :required t :validator supertag--valid-automation-action-p)
                                  :params (:type :plist :default nil)
                                  :schedule (:type :string :default nil)
                                  :enabled (:type :boolean :default t)
                                  :created-at (:type :timestamp :default (lambda () (current-time)))
                                  :modified-at (:type :timestamp :default (lambda () (current-time)))))
                      (:automation '(:id (:type :string :required t :validator supertag--valid-id-p)
                                    :name (:type :string :required t)
                                    :description (:type :string :default "")
                                    :trigger (:type :keyword :required t :validator supertag--valid-behavior-trigger-p)
                                    :condition (:type :list :required t)
                                    :actions (:type :list :required t)
                                    :enabled (:type :boolean :default t)
                                    :created-at (:type :timestamp :default (lambda () (current-time)))
                                    :modified-at (:type :timestamp :default (lambda () (current-time)))))
                      (:database '(:id (:type :string :required t :validator supertag--valid-id-p)
                                 :name (:type :string :required t :default "")
                                 :description (:type :string :default "")
                                 :icon (:type :string :default nil)
                                 :color (:type :string :default nil)
                                 :fields (:type :list :default nil)
                                 :views (:type :list :default nil)
                                 :relations (:type :list :default nil)
                                 :created-at (:type :timestamp :default (lambda () (current-time)))
                                 :modified-at (:type :timestamp :default (lambda () (current-time)))))
                      (:embed '(:id (:type :string :required t :validator supertag--valid-id-p)
                                :source-type (:type :keyword :required t)    ; :node or :query
                                :source-id (:type :string :required t :validator supertag--valid-id-p)
                                :embedded-file (:type :string :required t)
                                :embedded-pos (:type :integer :required t)
                                :source-file (:type :string :default nil)
                                :source-pos (:type :integer :default nil)
                                :content-hash (:type :string :default nil)
                                :source-hash (:type :string :default nil)
                                :created (:type :timestamp :default (lambda () (current-time)))
                                :modified (:type :timestamp :default (lambda () (current-time)))
                                :sync-status (:type :keyword :default :synced) ; :synced, :dirty, :conflict
                                :user-data (:type :any :default nil)))
                        (_ (error "Unknown schema type: %s" type))))))
        (puthash type schema supertag--schema-cache)
        schema)))
 

(defun supertag--convert-type (value type)
  "Convert VALUE to the specified TYPE."
  ;; Convert string type to keyword if needed
  (setq type (if (stringp type)
                 (intern (concat ":" type))
               type))
  (pcase type
    (:string (if (stringp value) value (format "%s" value)))
    (:number (cond ((numberp value) value)
                   ((stringp value) (string-to-number value))
                   (t (error "Cannot convert to number: %s" value))))
    (:integer (cond ((integerp value) value)
                    ((numberp value) (truncate value))
                    ((stringp value) (truncate (string-to-number value)))
                    (t (error "Cannot convert to integer: %s" value))))
    (:boolean (cond ((eq value t) t)
                    ((eq value nil) nil)
                    ((eq value 'true) t)
                    ((eq value 'false) nil)
                    ((equal value "true") t)
                    ((equal value "false") nil)
                    ((numberp value) (not (zerop value)))
                    (t (error "Cannot convert to boolean: %s" value))))
    (:date (supertag--convert-to-date value))
    (:timestamp (supertag--convert-to-timestamp value))
    (:options (cond ((listp value) value)
                    ((stringp value) (split-string value "," t))
                    (t (list value))))
    (:url (if (stringp value) value (format "%s" value)))
    (:email (if (stringp value) value (format "%s" value)))
    (:tag (cond ((listp value) value)
                ((stringp value) (if (string-empty-p value)
                                     nil
                                   (split-string value "," t "[ \t\n\r]+")))
                (t (list (format "%s" value)))))
    (:any value) ; No conversion, return as is
    (_ (error "Unknown type: %s" type))))

;; Helper for date conversion
(defun supertag--convert-to-date (value)
  "Convert VALUE to a date (time object representing a date)."
  (cond
   ((numberp value) (seconds-to-time (* value 24 3600))) ; Assume days since epoch
   ((stringp value) (org-parse-time-string value)) ; Org-mode date string
   ((listp value) (apply #'encode-time value)) ; List from decode-time
   (t (error "Cannot convert to date: %S" value))))


;; Helper for timestamp conversion (Org-mode specific)
(defun supertag--convert-to-timestamp (value)
  "Convert VALUE to appropriate timestamp format.
For internal storage, returns Emacs time format (high low micro pico).
Supports user-friendly input formats like:
- '2024-01-15' (date only)
- '2024-01-15 14:30' (date and time)
- 'today', 'tomorrow', 'next week'
- Org timestamps like '<2024-01-15 Mon 14:30>'
- Unix timestamps (numbers)"
  (cond
   ((stringp value)
    (cond
     ;; Handle special keywords
     ((string= value "today") (current-time))
     ((string= value "tomorrow") (time-add (current-time) (days-to-time 1)))
     ((string= value "yesterday") (time-subtract (current-time) (days-to-time 1)))
     ((string= value "now") (current-time))
     ;; Handle relative time expressions
     ((string-match "^\\([+-]?[0-9]+\\)\\s-*\\(day\\|week\\|month\\|year\\)s?$" value)
      (let ((amount (string-to-number (match-string 1 value)))
            (unit (match-string 2 value)))
        (pcase unit
          ("day" (time-add (current-time) (days-to-time amount)))
          ("week" (time-add (current-time) (days-to-time (* amount 7))))
          ("month" (time-add (current-time) (days-to-time (* amount 30))))
          ("year" (time-add (current-time) (days-to-time (* amount 365)))))))
     ;; Handle ISO 8601 date format (YYYY-MM-DD)
     ((string-match "^\\([0-9]\\{4\\}\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)$" value)
      (let ((year (string-to-number (match-string 1 value)))
            (month (string-to-number (match-string 2 value)))
            (day (string-to-number (match-string 3 value))))
        (encode-time 0 0 0 day month year)))
     ;; Handle ISO 8601 datetime format (YYYY-MM-DD HH:MM)
     ((string-match "^\\([0-9]\\{4\\}\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)\\s-+\\([0-9]\\{1,2\\}\\):\\([0-9]\\{1,2\\}\\)$" value)
      (let ((year (string-to-number (match-string 1 value)))
            (month (string-to-number (match-string 2 value)))
            (day (string-to-number (match-string 3 value)))
            (hour (string-to-number (match-string 4 value)))
            (minute (string-to-number (match-string 5 value))))
        (encode-time 0 minute hour day month year)))
     ;; Check if already an Org timestamp
     ((string-match-p org-ts-regexp value) value)
     ;; Try org-parse-time-string as fallback
     (t (condition-case nil
            (org-parse-time-string value)
          (error (error "Invalid timestamp format: %s. Try formats like '2024-01-15', '2024-01-15 14:30', 'today', 'tomorrow', or '+7 days'" value))))))
   ((numberp value) (seconds-to-time value)) ; Convert to internal time format
   ((listp value) value) ; Already in internal time format, return as-is
   (t (error "Cannot convert to timestamp: %S" value))))

;;; --- 3.3 Validation Helper Functions ---

(defun supertag-schema--merge-plists (base overlay)
  "Merge OVERLAY plist into BASE plist and return a new plist.

Keys in OVERLAY override keys in BASE."
  (let ((result (copy-sequence base)))
    (while overlay
      (setq result (plist-put result (pop overlay) (pop overlay))))
    result))

(defun supertag-schema-register (type schema &optional replace)
  "Register SCHEMA for TYPE.

TYPE is a keyword identifying the schema.
SCHEMA is a plist describing fields and their specs.

When REPLACE is non-nil, replace existing registered schema for TYPE.
Otherwise, merge SCHEMA onto the existing registered schema (or the built-in
schema when no registered one exists)."
  (unless (keywordp type)
    (error "TYPE must be a keyword, got: %S" type))
  (unless (listp schema)
    (error "SCHEMA must be a plist, got: %S" schema))
  (let* ((existing (gethash type supertag-schema--registry))
         (base (cond
                (existing existing)
                (replace nil)
                (t (supertag--get-schema type))))
         (merged (if replace
                     schema
                   (supertag-schema--merge-plists base schema))))
    (puthash type merged supertag-schema--registry)
    (remhash type supertag--schema-cache)
    merged))

(defun supertag-schema-unregister (type)
  "Unregister any user schema override for TYPE."
  (unless (keywordp type)
    (error "TYPE must be a keyword, got: %S" type))
  (remhash type supertag-schema--registry)
  (remhash type supertag--schema-cache))

(defun supertag-register-entity-type (type schema)
  "Register a new entity TYPE and its SCHEMA.

TYPE is a keyword (e.g. :my-entity). SCHEMA is a plist.
This makes TYPE valid for `supertag--valid-entity-type-p' and installs a
schema retrievable by `supertag--get-schema'."
  (unless (keywordp type)
    (error "TYPE must be a keyword, got: %S" type))
  (unless (listp schema)
    (error "SCHEMA must be a plist, got: %S" schema))
  (add-to-list 'supertag--registered-entity-types type)
  (supertag-schema-register type schema t))

(defun supertag-schema-apply-registrations ()
  "Apply user schema registrations from `supertag-schema-registration-functions'."
  (interactive)
  (dolist (fn supertag-schema-registration-functions)
    (when (functionp fn)
      (funcall fn))))

(defun supertag--valid-id-p (id)
  "Validate if ID is a valid string ID."
  (and (stringp id) (not (string-empty-p id))))

(defun supertag--valid-entity-type-p (type)
  "Validate if TYPE is a valid entity type."
  (or (memq type supertag-entity-types)
      (memq type supertag--registered-entity-types)
      (gethash type supertag-schema--registry)))

(defun supertag--valid-field-type-p (type)
  "Validate if field TYPE is supported."
  (memq type supertag-field-types))

(defun supertag--valid-relation-type-p (type)
  "Validate if relation TYPE is supported."
  (memq type supertag-relation-types))

(defun supertag--valid-behavior-trigger-p (trigger)
  "Validate if behavior TRIGGER is supported."
  (memq trigger supertag-behavior-trigger-types))

(defun supertag--valid-automation-action-p (action)
  "Validate if automation ACTION is supported."
  (memq action supertag-automation-action-types))

(defun supertag--valid-database-field-type-p (type)
  "Validate if database field TYPE is supported."
  (memq type supertag-database-field-types))

(defun supertag--valid-tag-field-p (value)
  "Validate if VALUE is a valid tag field reference.
  VALUE can be a single tag name (string) or a list of tag names."
  (if (listp value)
      (cl-every (lambda (v) 
                  (and (stringp v) 
                       (not (string-empty-p v))
                       (not (string-match-p "[: \t\n\r]" v)))) value)
    (and (stringp value) 
         (not (string-empty-p value))
         (not (string-match-p "[: \t\n\r]" value)))))

;;; --- 4. Advanced Features ---

;; 4.1 Schema Extension
(defun supertag-extend-schema (type extensions)
  "Extend the schema for the specified TYPE.
TYPE is the type to extend (e.g., :node, :tag).
EXTENSIONS is a list of field specifications to add."
  (let ((var-name (intern (format "supertag-%s-schema" type))))
    (unless (boundp var-name)
      (error "Schema for type %s not found." type))
    (let ((current-schema (symbol-value var-name))
          (new-schema (copy-sequence current-schema)))
      (dolist (ext extensions)
        (let ((field-name (car ext)))
          ;; Check if field already exists, if so, update it
          (let ((existing-field (cl-find field-name new-schema :key #'car)))
            (if existing-field
                (setf (cdr existing-field) (cdr ext)) ; Update existing
              (setq new-schema (append new-schema (list ext))))))) ; Add new
      (set var-name new-schema)
      ;; Clear cache for this schema type
      (remhash type supertag--schema-cache))))

;; 4.2 Custom Type Registration
(defun supertag-register-field-type (type &optional validator converter)
  "Register a new field type.
TYPE is the name of the new type (keyword).
VALIDATOR is an optional validation function.
CONVERTER is an optional conversion function."
  (unless (memq type supertag-field-types)
    (setq supertag-field-types (cons type supertag-field-types)))

  ;; Store validator and converter as properties of the type symbol
  (when validator
    (put type 'supertag-validator validator))

  (when converter
    (put type 'supertag-converter converter)))

;; 4.3 Custom Validation Rules
(defun supertag-register-validator (field-name validator)
  "Register a custom validation rule for a specific field.
FIELD-NAME is the field name (keyword).
VALIDATOR is the validation function."
  (put field-name 'supertag-custom-validator validator))

;;; --- 5. Performance Considerations ---

;; 5.1 Caching Schema Definitions (already integrated above)
;; Note: In hybrid architecture, schema caching is still useful for 
;; type conversion functions that may be used by validation functions.



;;; --- 统一数据验证入口 ---

(defun supertag-validate-data (data &optional type)
  "统一的数据验证入口函数。
DATA 是要验证的数据。
TYPE 是可选的数据类型提示 (:node, :tag, :store 等)。
返回 t 如果验证通过，否则返回 nil 或抛出错误。"
  (pcase type
    (:node (supertag--validate-node data))
    (:tag (supertag--validate-tag data))
    (:store (supertag--validate-store data))
    (:field (supertag--validate-field data))
    (:relation (supertag--validate-relation data))
    (_ (supertag--validate-any data))))

(defun supertag--validate-node (node)
  "验证节点数据格式。
NODE 应该是包含节点信息的 plist。"
  (and (listp node)
       (plist-get node :id)
       (stringp (plist-get node :id))
       (plist-get node :type)
       (eq (plist-get node :type) :node)
       (supertag--validate-time (plist-get node :created-at))
       (supertag--validate-time (plist-get node :modified-at))))

(defun supertag--validate-tag (tag)
  "验证标签数据格式。
TAG 应该是包含标签信息的 plist。"
  (and (listp tag)
       (plist-get tag :id)
       (stringp (plist-get tag :id))
       (plist-get tag :name)
       (stringp (plist-get tag :name))
       (plist-get tag :type)
       (eq (plist-get tag :type) :tag)
       (supertag--validate-time (plist-get tag :created-at))
       (supertag--validate-time (plist-get tag :modified-at))))

(defun supertag--validate-store (store)
  "验证存储数据格式。
STORE 应该是主数据存储哈希表。"
  (and (hash-table-p store)
       ;; 检查基本集合是否存在
       (hash-table-p (gethash :nodes store))
       (hash-table-p (gethash :tags store))
       (hash-table-p (gethash :relations store))))

(defun supertag--validate-field (field)
  "验证字段定义格式。
FIELD 应该是包含字段定义的 plist。"
  (and (listp field)
       (plist-get field :name)
       (stringp (plist-get field :name))
       (plist-get field :type)
       (supertag--valid-field-type-p (plist-get field :type))))

(defun supertag--validate-relation (relation)
  "验证关系数据格式。
RELATION 应该是包含关系信息的 plist。"
  (and (listp relation)
       (plist-get relation :type)
       (supertag--valid-relation-type-p (plist-get relation :type))
       (plist-get relation :from)
       (stringp (plist-get relation :from))
       (plist-get relation :to)
       (stringp (plist-get relation :to))))

(defun supertag--validate-any (data)
  "通用数据验证函数。
DATA 可以是任何类型的数据。
执行基本的结构完整性检查。"
  (cond
   ((null data) t) ; 空数据是有效的
   ((listp data)
    ;; 如果是 plist，检查基本结构
    (when (plist-get data :type)
      (supertag-validate-data data (plist-get data :type))))
   ((hash-table-p data) t) ; 哈希表假设是有效的
   (t t))) ; 其他类型假设是有效的

(defun supertag--validate-time (time-value)
  "验证时间值是否为有效的 Emacs 时间格式。
TIME-VALUE 应该是四元素列表 (high low micro pico)。"
  (or (null time-value) ; 允许空时间值
      (and (listp time-value)
           (= (length time-value) 4)
           (cl-every #'integerp time-value))))

(provide 'supertag-core-schema)

;;; org-supertag/schema.el ends here

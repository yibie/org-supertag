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
(require 'ht)     ; For hash-table operations (if used in validators/converters)

;;; --- Cache for schema definitions ---

(defvar supertag--schema-cache (make-hash-table :test 'eq)
  "Cache for compiled schema definitions to improve performance.")

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
  '(:string   ; String type
    :number   ; Numeric type
    :integer  ; Integer type
    :boolean  ; Boolean type
    :date     ; Date type
    :timestamp ; Timestamp type
    :options  ; Options type
    :url      ; URL type
    :email    ; Email type
    :tag)     ; Tag reference type
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
  "Get the schema for the specified TYPE, prioritizing from cache."
  (or (gethash type supertag--schema-cache)
      (let ((schema (pcase type
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
                              :extends (:type :string :default nil)
                              :created-at (:type :timestamp :default (lambda () (current-time)))
                              :modified-at (:type :timestamp :default (lambda () (current-time)))
                              :description (:type :string :default "")
                              :icon (:type :string :default nil)
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
                        (_ (error "Unknown schema type: %s" type))))
        (puthash type schema supertag--schema-cache)
        schema))))
 

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
For display purposes, returns Org-mode timestamp string."
  (cond
   ((stringp value)
    (if (string-match-p org-ts-regexp value) ; Check if already an Org timestamp
        value
      (org-parse-time-string value))) ; Convert to internal time format
   ((numberp value) (seconds-to-time value)) ; Convert to internal time format
   ((listp value) value) ; Already in internal time format, return as-is
   (t (error "Cannot convert to timestamp: %S" value))))

;;; --- 3.3 Validation Helper Functions ---

(defun supertag--valid-id-p (id)
  "Validate if ID is a valid string ID."
  (and (stringp id) (not (string-empty-p id))))

(defun supertag--valid-entity-type-p (type)
  "Validate if TYPE is a valid entity type."
  (memq type supertag-entity-types))

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



(provide 'supertag-core-schema)

;;; org-supertag/schema.el ends here

;;; org-supertag-db.el --- Database layer for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides fundamental data storage and query functionality
;;
;; Core Concepts:
;; - Everything is an entity, everything is a relationship
;; - Entities are nodes in relationships, relationships are connections between nodes
;; - Connect entities through relationships - using type, from, to to express relationships
;;
;; Main Features:
;; - Entity Management: Create, update, delete, query entities
;; - Relationship Management: Create, delete, query relationships  
;; - Data Persistence: Save, load, backup data
;; - Cache System: Improve query performance
;;
;; Usage:
;; 1. Entity Operations
;;    - Add: (org-supertag-db-add id props)
;;    - Query: (org-supertag-db-get id)
;;    - Delete: (org-supertag-db-remove-object id)
;;
;; 2. Relationship Operations
;;    - Add: (org-supertag-db-link type from to)
;;    - Query: (org-supertag-db-find-links type)
;;    - Delete: (org-supertag-db-remove-link type from to)
;;
;; 3. Parser
;;    - Parse: (org-supertag-db-parse-file file-path)
;;
;; 4. Data Management
;;    - Save: (org-supertag-db-save)
;;    - Load: (org-supertag-db-load)
;;
;; 5. Event System
;;    - Add Listener: (org-supertag-db-add-listener event-type function)
;;    - Trigger Event: (org-supertag-db-trigger-event event-type)
;;
;; 6. Cache System
;;    - Set Cache: (org-supertag-db-cache-set key value)
;;    - Get Cache: (org-supertag-db-cache-get key)
;;    - Remove Cache: (org-supertag-db-cache-remove key)
;;
;; 7. Data Persistence
;;    - Save: (org-supertag-db-save)
;;    - Load: (org-supertag-db-load)
;;
;; 8. Backup
;;    - Backup: (org-supertag-db-backup)
;;    - Restore: (org-supertag-db-restore)

;; This file does not support breaking changes. If existing functions are insufficient,
;; implement new functions instead of modifying existing ones.

;;; Code:

(require 'ht)
(require 'cl-lib)
(require 'org-element)


;;------------------------------------------------------------------------------
;; Type System
;;------------------------------------------------------------------------------    
;; Field values are stored in Node-Field-Value relationship table
;; Node -----> Tag -----> Field Definition
;;   |          |          |
;;   |          |          |
;;   v          v          v
;; Node-Tag Relations
;;      |          
;;      v          
;; Node-Field-Value Relations

;; Entity type definitions
(defconst org-supertag-db-object-type
  '(:node    ; Org node (headline with tags)
    :tag)    ; Tag (supertag with field definitions)
  "Entity types supported by the system.")

;; Field value type definitions  
(defconst org-supertag-db-field-type
  '(:string    ; Plain text
    :number    ; Numbers for calculation and statistics
    :date      ; Date using org-mode date-stamp format
    :time      ; Time using org-mode time-stamp format
    :list      ; List of values
    :options   ; Selection from predefined options
    :ref       ; Reference to fields and values from other tags
    :behavior) ; Behavior definition
  "Supported field value types.")

(defconst org-supertag-db-object-structure
  '((:type :node
     :required (;; Basic Information
                :id          ; Unique node identifier
                :title       ; Node title
                :file-path   ; File path
                ;; Position Information
                :pos         ; Node position
                :olp         ; Outline path (ancestor titles)
                :level       ; Level (0 for file level)
                )         
     :optional (;; Hash Information
                :hash        ; Node property hash
                :content-hash ; Content hash
                ;; Task Information
                :scheduled   ; Scheduled time
                :deadline    ; Deadline time
                :todo       ; Todo state
                :priority    ; Priority
                :tags       ; Tags
                :content     ; Content
                ;; Reference Relations
                :ref-to     ; Referenced nodes
                :ref-from   ; Nodes referencing this
                :ref-count  ; Reference count
                ;; Event Information
                :created-at  ; Creation time
                :modified-at ; Modification time
                ;; Property Information
                :properties  ; Property drawer contents
                :raw-value  ; Raw headline value
                ))
    
    (:type :tag
     :required (;; Basic Information
                :id          ; Tag identifier (using tag name)
                ;; Field Definitions
                :fields)     ; ((:name "field-name" :type field-type) ...)
     :optional (;; Meta Information
                :type       ; Tag type (:node, :field, :relation)
                :relation-type ; Relation type (:cooccurrence, :parent-child, :causal, etc.)
                :from-tag   ; Tag that this tag references
                :to-tag     ; Tag that references this tag
                :created-at  ; Creation time
                :behaviors  ; Behaviors associated with the tag
                :description ; Description of tag purpose
                :icon       ; Icon for visual identification
                :color      ; Color scheme (background and foreground)
                :modified-at)) ; Modification time
  "Entity structure definitions."))

;; Link type definition
(defconst org-supertag-db-link-type
  '(:node-tag     ; Node-tag relationship
    :node-field   ; Node-field relationship  
    :tag-ref      ; Tag reference relationship
    :tag-tag      ; Tag-tag relationship
    :relation-group      ; Relation group definition
    :relation-member)    ; Relation group membership
  "System supported link types.")

(defconst org-supertag-db-link-structure
  '((:type :node-tag
     :required (:from :to)    ; node-id -> tag-id
     :optional (:created-at)) ; Creation time
    
    (:type :node-field
     :required (:from          ; node-id
                :to            ; field-name
                :tag-id        ; Associated tag
                :value)        ; Field value
     :optional (:created-at   
                :modified-at))
    
    (:type :tag-ref
     :required (:from :to)              ; tag-id -> tag-id
     :optional (:ref-type))             ; Reference type

    (:type :tag-tag
     :required (:from :to)              ; tag-id -> tag-id
     :optional (:relation-type          ; Relation type (:cooccurrence, :parent-child, :causal, etc.)
                :strength               ; Strength (0.0-1.0)
                :created-at             ; Creation time
                :updated-at)))           ; Update time
  "Entity link structure definitions.")

;; Behavior System Definitions
(defconst org-supertag-behavior-timing
  '(:immediate    ; Execute immediately when condition met
    :deferred     ; Execute at next suitable time
    :scheduled    ; Execute at specific time
    :periodic)    ; Execute periodically
  "When the behavior should execute.")

(defconst org-supertag-behavior-condition
  '(:node        ; Node state conditions
    :time        ; Time-based conditions
    :field       ; Field value conditions
    :reference   ; Reference relation conditions
    :custom)     ; Custom predicate conditions
  "What conditions trigger the behavior.")

(defconst org-supertag-behavior-operation
  '(:transform   ; Transform node content/properties
    :create      ; Create new nodes/content
    :delete      ; Delete nodes/content
    :move        ; Move nodes/content
    :export      ; Export to external formats
    :notify      ; Send notifications
    :custom)     ; Custom operations
  "What operations the behavior can perform.")

(defconst org-supertag-behavior-method
  '(:sync       ; Synchronous execution
    :async      ; Asynchronous execution
    :batch      ; Batch processing
    :transact)  ; Transactional execution
  "How the behavior should execute.")

(defconst org-supertag-db-behavior-structure
  '(:required (:when         ; Timing and conditions
              :what         ; Operation to perform
              :how)         ; Execution method
    :optional (:description ; Behavior description
              :error       ; Error handling
              :compose))   ; Composition rules
  "Behavior structure definition.")

;; Event Types
(defconst org-supertag-db-events
  '(entity:changed    ; Entity update event
    link:created      ; Link creation event  
    link:removed      ; Link removal event
    field:changed     ; Field value change event
    ref:created       ; Reference creation event
    ref:removed       ; Reference removal event 
    ref:updated)      ; Reference update event
  "Event types supported by the system.")
  
;;------------------------------------------------------------------------------
;; Type Validation
;;------------------------------------------------------------------------------    

;; Entity Type Validation
(defun org-supertag-db-valid-object-type-p (type)
  "Check if TYPE is a valid entity type.
TYPE must be :node or :tag.

Returns:
- t if valid
- nil if invalid"
  (memq type org-supertag-db-object-type))

(defun org-supertag-db-valid-object-p (type props)
  "Validate entity data.
TYPE is the entity type
PROPS is the property list

Returns:
- t if valid
- Signals error if invalid"
  (let* ((struct (cl-find type org-supertag-db-object-structure
                         :key (lambda (x) (plist-get x :type))))
         (required (plist-get struct :required))
         (missing-props (when required
                          (cl-remove-if (lambda (key) (plist-member props key))
                                       required))))
    (cond
     ;; Check required properties exist
     (missing-props
      (error "Invalid object properties for type %S. Missing required keys: %S" type missing-props))
     ;; Special validation for tag type
     ((eq type :tag)
      (let ((fields (plist-get props :fields)))
        (or (null fields) ; Fields can be empty
            (org-supertag-db--validate-fields fields))))
     ;; Default: valid
     (t t))))

(defun org-supertag-db--validate-fields (fields)
  "Validate field definition list.
FIELDS is a list of field definitions, each should be a plist.

Returns:
- t if valid
- Signals error if invalid"
  (when fields
    (unless (listp fields)
      (error "Fields must be a list"))
    
    (dolist (field fields)
      ;; Check required properties
      (unless (plist-get field :name)
        (error "Field must have a name: %S" field))
      (unless (plist-get field :type)
        (error "Field must have a type: %S" field))
      
      ;; Check if type is supported
      (let ((type (plist-get field :type)))
        (unless (alist-get type org-supertag-field-types)
          (error "Unsupported field type: %s" type)))))
  t)

;; Link Type Validation
(defun org-supertag-db-valid-link-type-p (type)
  "Check if TYPE is a valid link type.
Valid types are :node-tag, :node-field, :tag-ref, and :tag-tag.

Returns:
- t if valid
- nil if invalid"
  (memq type '(:node-tag :node-field :tag-ref :tag-tag)))

(defun org-supertag-db-valid-link-p (type from to props)
  "Validate link data.
TYPE is the link type
FROM is the source entity ID
TO is the target entity ID
PROPS is the link properties

Returns:
- t if valid
- nil if invalid"
  (let* ((structure (org-supertag-db-get-link-type type))
         (required (plist-get structure :required))
         (from-obj (org-supertag-db-get from))
         (to-obj (org-supertag-db-get to)))
    ;; 1. Validate required properties
    (and (cl-every (lambda (key) 
                     (plist-member props key)) 
                   required)
         ;; 2. Type-specific validation
         (pcase type
           (:node-field
            (and (eq (plist-get from-obj :type) :node)
                 (stringp to)
                 (plist-get props :value)))
           (:node-tag
            (and (eq (plist-get from-obj :type) :node)
                 (eq (plist-get to-obj :type) :tag)))
           
           (:tag-ref
            (and (eq (plist-get from-obj :type) :tag)
                 (eq (plist-get to-obj :type) :tag)))
           (_ t)))))

;; Entity Property Validation
(defun org-supertag-db--validate-props (type props)
  "Validate entity properties against structure definition.
TYPE must be :node or :tag
PROPS is the entity property list

Returns:
- t if valid
- Signals error if invalid"
  (let* ((struct (cl-find type org-supertag-db-object-structure
                         :key (lambda (x) (plist-get x :type))))
         (required (plist-get struct :required)))
    (unless struct
      (error "Invalid entity type: %s" type))
    ;; Validate required properties
    (let ((missing-props
           (cl-remove-if
            (lambda (key) (plist-member props key))
            required)))
      (when missing-props
        (error "Missing required properties: %s" missing-props)))))

;;; Tag Entity Structure
;;; (:type :tag :name "name" :fields (field-def ...))
;;; field-def structure:
;;; (:name "name" :type type :display-name "display" :description "desc" ...)

(defun org-supertag-db--validate-tag-def (props)
  "Validate tag definition.
PROPS is the tag property list.

Returns:
- t if valid
- nil if invalid"
  (let ((type (plist-get props :type))
        (name (plist-get props :name))
        (fields (plist-get props :fields)))
    (and (eq type :tag)
         (stringp name)
         (not (string-empty-p name))
         ;; Validate field list
         (or (null fields)  ; Fields can be empty
             (and (listp fields)
                  (cl-every #'org-supertag-db--validate-field-def fields))))))

(defun org-supertag-db--validate-field-def (field)
  "Validate field definition.
FIELD is the field definition plist.

Returns:
- t if valid
- nil if invalid"
  (let ((name (plist-get field :name))
        (type (plist-get field :type)))
    (and (stringp name)
         (not (string-empty-p name))
         (symbolp type)
         (alist-get type org-supertag-field-types))))


;;------------------------------------------------------------------------------
;; Core Data Tables
;;------------------------------------------------------------------------------    

(defvar org-supertag-db--object (ht-create)
  "Entity storage - id -> plist.")

(defvar org-supertag-db--link (ht-create)
  "Link storage - rel-id -> (type from to props).")

;; Event System
(defvar org-supertag-db--events (ht-create)
  "Event system - store event handlers.")

;; Cache System
(defvar org-supertag-db--cache (ht-create)
  "Cache system - store query cache.")

;;---------------------------------------------------------------------------------
;; Data Operation: Add
;;---------------------------------------------------------------------------------

(defun org-supertag-db-add (id props)
  "Add or update entity.
ID is entity unique identifier
PROPS is entity properties

Returns:
- Success: entity ID
- Error: throws error"
  (condition-case err
      (let* ((type (plist-get props :type))
             (old-props (org-supertag-db-get id))
             (is-update (not (null old-props)))
             ;; Normalize properties
             (clean-props (org-supertag-db--normalize-props props))
             (new-props (if is-update
                           ;; Update: new props take precedence, but preserve content
                           (org-supertag-db--normalize-props
                            (append
                             clean-props
                             (when (and (not (plist-get clean-props :content))
                                      (plist-get old-props :content))
                               (list :content (plist-get old-props :content)))
                             (list :created-at (plist-get old-props :created-at)
                                   :modified-at (current-time))))
                         ;; New creation
                         (org-supertag-db--normalize-props
                          (append
                           clean-props
                           (list :created-at (current-time)))))))
        ;; 1. Validation
        ;; 1.1 Validate type
        (unless (org-supertag-db-valid-object-type-p type)
          (error "Invalid object type: %s" type))
        ;; 1.2 Validate properties
        (unless (org-supertag-db-valid-object-p type new-props)
          (error "Invalid object properties"))
        ;; 2. Pre-storage processing
        (when is-update
          ;; 2.1 Handle type changes
          (let ((old-type (plist-get old-props :type)))
            (unless (eq old-type type)
              ;; Clear all related caches on type change
              (org-supertag-db--cache-clear-for-type old-type id))))
        ;; 3. Store entity
        (ht-set! org-supertag-db--object id new-props)
        ;; 4. Cache management
        ;; 4.1 Clear entity cache
        (org-supertag-db--cache-remove 'entity id)
        ;; 4.2 Clear query cache
        (org-supertag-db--cache-remove 'query (format "type:%s" type))
        ;; 4.3 Clear type-specific caches
        (pcase type
          (:node
           ;; Clear node-related caches
           (org-supertag-db--cache-remove 'query (format "node-tags:%s" id))
           (org-supertag-db--cache-remove 'query (format "node-fields:%s" id))
           (org-supertag-db--cache-remove 'query (format "node-refs:%s" id)))
          (:tag
           ;; Clear tag-related caches
           (org-supertag-db--cache-remove 'query (format "tag-fields:%s" id))
           (org-supertag-db--cache-remove 'query (format "tag-refs:%s" id))))
        ;; 5. Trigger events
        (if is-update
            (progn
              (org-supertag-db-emit 'entity:before-update type id old-props new-props)
              (org-supertag-db-emit 'entity:updated type id old-props new-props))
          (progn
            (org-supertag-db-emit 'entity:before-create type id new-props)
            (org-supertag-db-emit 'entity:created type id new-props)))
        ;; 6. Database state management
        ;; 6.1 Mark database as dirty
        (org-supertag-db--mark-dirty)
        ;; 6.2 Schedule delayed save
        (org-supertag-db-save)
        ;; 7. Return ID
        id)
    ;; Error handling
    (error
     (message "[org-supertag-db-add] Error in entity add/update: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

(defun org-supertag-db-exists-p (id)
  "Check if entity exists.
ID is the entity identifier.

Returns:
- t if exists
- nil if not exists"
  (ht-contains-p org-supertag-db--object id))

;;---------------------------------------------------------------------------------
;; Data Operation: Link
;;---------------------------------------------------------------------------------

(defun org-supertag-db-link (type from to &optional props)
  "Create or update a link between entities.
TYPE: Link type (:node-tag, :node-field, :tag-ref)
FROM: Source entity ID
TO: Target entity ID
PROPS: Optional link properties

Returns:
- Link ID if successful
- Signals error if validation fails"
  (let* ((base-props (list :from from :to to))
         (full-props (if props 
                        (append base-props props)
                      base-props))
         (rel-id (format "%s:%s->%s" type from to)))
    ;; 1. Validate
    ;; 1.1 Validate link type
    (unless (org-supertag-db-valid-link-type-p type)
      (error "Invalid link type: %s" type))
    ;; 1.2 Validate link data
    (unless (org-supertag-db-valid-link-p type from to full-props)
      (error "Invalid link data"))
    
    ;; 2. Check if link already exists
    (if-let* ((existing (ht-get org-supertag-db--link rel-id)))
        ;; If exists with same props, return ID
        (if (equal existing full-props)
            rel-id
          ;; Otherwise update link
          (progn
            (ht-set! org-supertag-db--link rel-id full-props)
            rel-id))
      ;; 3. Create new link if not exists
      (progn
        (ht-set! org-supertag-db--link rel-id full-props)
        ;; 4. Clear caches
        (org-supertag-db--cache-remove 'link rel-id)
        (org-supertag-db--cache-remove 'query (format "links:%s:%s" type from))
        (org-supertag-db--cache-remove 'query (format "links:%s:%s" type to))
        ;; 5. Clear type-specific caches
        (pcase type
          (:node-field
           (org-supertag-db--cache-remove 'query (format "node-fields:%s" from)))
          (:node-tag
           (org-supertag-db--cache-remove 'query (format "node-tags:%s" from)))
          (:tag-ref
           (org-supertag-db--cache-remove 'query (format "tag-refs:%s" from))
           (org-supertag-db--cache-remove 'query (format "tag-refs:%s" to))))
        ;; 6. Trigger event
        (org-supertag-db-emit 'link:created type from to props)
        ;; 7. Mark database as dirty
        (org-supertag-db--mark-dirty)
        ;; 8. Schedule delayed save
        (org-supertag-db--schedule-save)
        ;; 9. Return link ID
        rel-id))))

(defun org-supertag-db-unlink (type from to &optional dry-run)
  "Remove a link between entities.
TYPE: Link type (:node-tag, :node-field, :tag-ref)
FROM: Source entity ID
TO: Target entity ID
DRY-RUN: If non-nil, only return data to be deleted without actual deletion

Returns:
- t if removed or exists (in dry-run mode)
- nil if link does not exist"
  (let* ((rel-id (format "%s:%s->%s" type from to))
         (exists (ht-contains-p org-supertag-db--link rel-id))
         (link-props (and exists (ht-get org-supertag-db--link rel-id))))
    (when (and exists (not dry-run))
      ;; 1. Remove link
      (ht-remove! org-supertag-db--link rel-id)
      ;; 2. Clear caches
      ;; 2.1 Clear link cache
      (org-supertag-db--cache-remove 'link rel-id)
      ;; 2.2 Clear query caches
      (org-supertag-db--cache-remove 'query (format "links:%s:%s" type from))
      (org-supertag-db--cache-remove 'query (format "links:%s:%s" type to))
      ;; 3. Clear type-specific caches
      (pcase type
        (:node-field
         (org-supertag-db--cache-remove 'query (format "node-fields:%s" from)))
        (:node-tag
         (org-supertag-db--cache-remove 'query (format "node-tags:%s" from)))
        (:tag-ref
         (org-supertag-db--cache-remove 'query (format "tag-refs:%s" from))
         (org-supertag-db--cache-remove 'query (format "tag-refs:%s" to))))
      ;; 4. Trigger event
      (org-supertag-db-emit 'link:removed type from to link-props)
      ;; 5. Mark database as dirty
      (org-supertag-db--mark-dirty)
      ;; 6. Schedule delayed save
      (org-supertag-db--schedule-save))
    ;; Return result
    exists))

(defun org-supertag-db-unlink-all (from &optional type)
  "Remove all links or links of specific type from an entity.
FROM: Source entity ID
TYPE: Optional link type filter
Returns:
- Number of removed links"
  (let ((count 0)
        (removed-links nil))  ; Store removed links for events
    ;; 1. Collect links to remove
    (ht-map (lambda (k v)
              (when (and (equal from (plist-get v :from))
                        (or (null type)
                            (equal type (plist-get v :type))))
                ;; Save link info for removal
                (push (cons k v) removed-links)))
            org-supertag-db--link)
    ;; 2. Process removals
    (dolist (link removed-links)
      (let* ((rel-id (car link))
             (props (cdr link))
             (link-type (plist-get props :type))
             (to (plist-get props :to)))
        ;; 2.1 Remove link
        (ht-remove! org-supertag-db--link rel-id)
        ;; 2.2 Clear link caches
        (org-supertag-db--cache-remove 'link rel-id)
        (org-supertag-db--cache-remove 'query (format "links:%s:%s" link-type from))
        (org-supertag-db--cache-remove 'query (format "links:%s:%s" link-type to))
        
        ;; 2.3 Clear type-specific caches
        (pcase link-type
          (:node-field
           (org-supertag-db--cache-remove 'query (format "node-fields:%s" from)))
          (:node-tag
           (org-supertag-db--cache-remove 'query (format "node-tags:%s" from)))
          (:tag-ref
           (org-supertag-db--cache-remove 'query (format "tag-refs:%s" from))
           (org-supertag-db--cache-remove 'query (format "tag-refs:%s" to))))
        
        ;; 2.4 Trigger single link removal event
        (org-supertag-db-emit 'link:removed link-type from to props)
        (cl-incf count)))
    ;; 3. If any links were removed
    (when (> count 0)
      ;; 3.1 Trigger batch removal event
      (org-supertag-db-emit 'links:batch-removed from type count)
      ;; 3.2 Mark database as dirty
      (org-supertag-db--mark-dirty)
      ;; 3.3 Schedule delayed save
      (org-supertag-db--schedule-save))
    ;; 4. Return removal count
    count))


;;---------------------------------------------------------------------------------
;; Data Operation: Get
;;---------------------------------------------------------------------------------
;; org-supertag-db-get (Single Entity Query)
;;     ├── org-supertag-db-get-prop (Property Access)
;;     │       └── org-supertag-db-get-type (Type Access Helper)

(defun org-supertag-db-get-node-tags (node-id)
  "Get all tags for a node.
NODE-ID is the node identifier."
  (org-supertag-db-get-link :node-tag node-id))

(defun org-supertag-db-get-node-fields (node-id)
  "Get all field values for a node.
NODE-ID is the node identifier."
  (org-supertag-db-get-link :node-field node-id))

(defun org-supertag-db-get-tag-refs (tag-id)
  "Get all references for a tag.
TAG-ID is the tag identifier."
  (org-supertag-db-get-link :tag-ref tag-id))

(defun org-supertag-db-get (id &optional default)
  "Get property list for an entity.
ID is the entity identifier
DEFAULT is the value returned if entity doesn't exist

Returns:
- Property list if entity exists
- DEFAULT (nil if not provided) if entity doesn't exist
- Property list format: (:type type :prop1 value1 ...)

First tries to read from cache, falls back to entity table if cache miss."
  (or (org-supertag-db--cache-get 'entity id)
      (let ((value (or (ht-get org-supertag-db--object id)
                      default)))
        (when value
          (org-supertag-db--cache-set 'entity id value))
        value)))

(defun org-supertag-db-get-prop (id prop &optional default)
  "Get specific property value for an entity.
ID is the entity identifier
PROP is the property to get
DEFAULT is returned if entity or property doesn't exist."
  (if-let* ((props (org-supertag-db-get id)))
      (or (plist-get props prop) default)
    default))

(defun org-supertag-db-get-type (id)
  "Get entity type.
ID is the entity identifier."
  (org-supertag-db-get-prop id :type))

;; Get Link
(defun org-supertag-db-get-link-type (type)
  "Get link type definition.
TYPE is the link type."
  (pcase type
    (:node-field
     '(:type :node-field
       :required (:from :to :tag-id :value)
       :optional (:created-at :modified-at)))
    
    (:node-tag
     '(:type :node-tag
       :required (:from :to)
       :optional (:created-at :modified-at)))
    
    (:tag-ref
     '(:type :tag-ref
       :required (:from :to :ref-type)
       :optional (:created-at :modified-at)))
    
    (:tag-tag
     '(:type :tag-tag
       :required (:from :to)
       :optional (:relation-type
                 :strength
                 :created-at
                 :updated-at)))
    
    (_ nil)))

(defun org-supertag-db-get-link (type from)
  "Get links.
TYPE is the link type
FROM is the source entity ID."
  (let ((links nil)
        (prefix (format "%s:%s->" type from)))
    (ht-map (lambda (key value)
              (when (string-prefix-p prefix key)
                (push (cons key value) links)))
            org-supertag-db--link)
    links))

(defun org-supertag-db-get-links-type (link-type)
  "Get all links of specified type.
LINK-TYPE is the link type like :node-tag, :node-field etc."
  (let ((links '()))
    (maphash (lambda (k v)
               (when (and (stringp k)
                         (string-prefix-p (format ":%s:" link-type) k))
                 (push (cons k v) links)))
             org-supertag-db--link)
    links))

(defun org-supertag-db-get-link-reverse (type to)
  "Get reverse links.
TYPE is the link type
TO is the target entity ID

Returns list in format ((from props) ...)"
  (let (results)
    (ht-map (lambda (k v)
              (when (and (equal type (car v))
                        (equal to (nth 2 v))
                        (equal (plist-get v :from) (plist-get v :to)))
                (push (list (cadr v) (nth 3 v)) results)))
            org-supertag-db--link)
    (nreverse results)))

(defun org-supertag-db-get-all-link ()
  "Get all links.
Returns list in format ((type from to props) ...)"
  (let (results)
    (ht-map (lambda (k v)
              (push v results))
            org-supertag-db--link)
    results))

;; Get Ref Link
(defun org-supertag-db-get-ref-context (ref-id)
  "Get reference context information.
REF-ID is the reference link ID

Returns:
- Reference context if successful
- nil if failed"
  (when-let* ((props (org-supertag-db-get-link ref-id))
              (context (plist-get props :ref-context))
              (ref-node (org-supertag-db-get ref-id))
              (ref-from (plist-get ref-node :ref-from))
              (ref-to (plist-get ref-node :ref-to))
              (ref-count (plist-get ref-node :ref-count)))
    context))

(defun org-supertag-db-get-ref-pos (ref-id)
  "Get reference position information.
REF-ID is the reference link ID

Returns:
- Reference position if successful
- nil if failed"
  (when-let* ((props (org-supertag-db-get-link ref-id))
              (pos (plist-get props :ref-pos))
              (ref-node (org-supertag-db-get ref-id))
              (ref-from (plist-get ref-node :ref-from))
              (ref-to (plist-get ref-node :ref-to))
              (ref-count (plist-get ref-node :ref-count)))
    pos))

(defun org-supertag-db-get-all ()
  "Get all entities in database."
  (ht-items org-supertag-db--object))

(defun org-supertag-db-get-tag-nodes (tag-id)
  "Get all nodes that have TAG-ID.
Returns a list of node IDs."
  (let ((nodes nil))
    ;; Iterate over all relationships
    (maphash (lambda (_key relation)
               (when (and (eq (plist-get relation :type) :node-tag)
                         (equal (plist-get relation :to) tag-id))
                 (push (plist-get relation :from) nodes)))
             org-supertag-db--link)
    nodes))

(defun org-supertag-get-all-files ()
  "Get list of all org files in database.
Returns:
- List of absolute file paths
- nil if no files found

Notes:
1. Only returns files associated with nodes
2. Removes duplicates
3. Ensures paths exist"
  (let ((files nil))
    (maphash
     (lambda (id props)
       (when (and (eq (plist-get props :type) :node)
                 (plist-get props :file-path))
         (let ((file-path (plist-get props :file-path)))
           (when (file-exists-p file-path)
             (push file-path files)))))
     org-supertag-db--object)
    (delete-dups (nreverse files))))

;;---------------------------------------------------------------------------------
;; Data Operation: Find
;;---------------------------------------------------------------------------------
;; org-supertag-db-find (base)
;;     ├── org-supertag-db-find-by-props (Generic Property Query)
;;     │       └── org-supertag-db-find-by-type (Type Query Helper)

(defun org-supertag-db-find (pred)
  "Find entities that match the condition.
PRED is the predicate function.

Returns:
- List of matching entities
- nil if no matches found"
  (let ((cache-key (format "%s" (sxhash pred))))
    ;; Try to get from cache  
    (or (org-supertag-db--cache-get 'query cache-key)
        ;; Cache miss, execute query
        (let (results)
          (ht-map (lambda (k v)
                    (when (funcall pred k v)
                      (push (cons k v) results)))
                  org-supertag-db--object)
          ;; Cache and return results
          (let ((final-results (nreverse results)))
            (org-supertag-db--cache-set 'query cache-key final-results)
            final-results)))))

(defun org-supertag-db--check-match (props rules)
  "Check if properties match the condition.
PROPS is the properties to check
RULES is the rules to match

Returns:
- t if all rules match
- nil if any rule fails"
  (cl-loop for (key value) on rules by #'cddr
           always (equal (plist-get props key) value)))

(defun org-supertag-db-find-by-props (props &optional predicate)
  "Find entities that match the property condition.
PROPS is the properties to check
PREDICATE is an optional function that takes a property list and returns boolean

Returns:
- List of matching entities
- nil if no matches found"
  (org-supertag-db-find 
   (lambda (_k v)
     (and (org-supertag-db--check-match v props)
          (or (null predicate)
              (funcall predicate v))))))

(defun org-supertag-db-find-by-type (type &optional predicate)
  "Find entities of specified type with optional predicate.
TYPE is the entity type to find (:node, :tag, etc)
PREDICATE is an optional function that takes a property list and returns boolean

Returns:
- List of matching entity IDs
- nil if no matches found"
  (let ((base-pred (lambda (k v) 
                    (eq (plist-get v :type) type))))
    (if predicate
        (mapcar #'car 
                (org-supertag-db-find 
                 (lambda (k v)
                   (and (funcall base-pred k v)
                        (funcall predicate v)))))
      (mapcar #'car 
              (org-supertag-db-find base-pred)))))

(defun org-supertag-db-find-nodes-by-tag (tag)
  "Find all nodes that have TAG.
TAG can be with or without # prefix.
Returns a list of node IDs that have the specified tag."
  (let ((normalized-tag (if (string-prefix-p "#" tag)
                           (substring tag 1)
                         tag)))
    (cl-loop for (id . node) in (ht->alist org-supertag-db--object)
             when (and (eq (plist-get node :type) :node)
                      (member normalized-tag 
                             (mapcar (lambda (t) 
                                      (if (string-prefix-p "#" t)
                                          (substring t 1)
                                        t))
                                    (plist-get node :tags))))
             collect id)))

(defun org-supertag-db-find-nodes-by-field-value (field-name value &optional tag-id)
  "Find nodes with the specified field value.
FIELD-NAME is the field name to check
VALUE is the value to check
TAG-ID is the optional tag ID to filter by

Returns:
- List of matching nodes
- nil if no matches found"
  (let ((links (org-supertag-db-find-links :type :node-field)))
        (cl-remove-if-not
         (lambda (link)
           (and (equal (plist-get (nth 3 link) :value) value)
                (or (null tag-id)
                    (equal (plist-get (nth 3 link) :tag-id) tag-id))))
         links)))

(defun org-supertag-db-find-tags-by-field (field-name)
  "Find tags that contain the specified field.
FIELD-NAME is the field name to check

Returns:
- List of matching tags
- nil if no matches found"
  (org-supertag-db-find
   (lambda (_k v)
     (and (eq (plist-get v :type) :tag)
          (assoc field-name (plist-get v :fields))))))

(defun org-supertag-db-find-links (type from to)
  "Find links matching the specified criteria.
TYPE is the link type to find
FROM is the optional source node ID
TO is the optional target node ID

Returns:
- List of matching links
- nil if no matches found"
  (let (results)
    (ht-map (lambda (k v)
              (when (and (or (null type) (eq (plist-get v :type) type))
                        (or (null from) (equal (plist-get v :from) from))
                        (or (null to) (equal (plist-get v :to) to)))
                (push v results)))
            org-supertag-db--link)
    results))

;;---------------------------------------------------------------------------------
;; Data Operation: Remove
;;---------------------------------------------------------------------------------

(defun org-supertag-db-remove-object (id &optional dry-run)
  "Remove an entity and all its related relationships.
ID: Entity identifier
DRY-RUN: If non-nil, only return data to be deleted without actual deletion

Returns:
A cons cell in the form (entity . links) where:
- entity is the deleted entity data
- links is the list of deleted relationships
Returns nil if entity does not exist"
  (condition-case err
      (progn
        ;; 1. Get entity and check existence
        (let ((entity (org-supertag-db-get id)))
          (if (null entity)
              (progn 
                (message "Entity %s not found, nothing to remove" id)
                nil) 
            ;; 2. Entity exists, get type
            (let ((type (plist-get entity :type)))
              (if (null type)
                  (message "Entity %s has no type, cannot remove properly" id)                
                ;; 3. Get related links (use empty list as default)
                (let ((outgoing-links (or (org-supertag-db-get-link nil id) '()))
                      (incoming-links (or (org-supertag-db-get-link-reverse nil id) '()))
                      (removed-data (list :entity entity)))
                  
                  ;; 4. Trigger events
                  (org-supertag-db-emit 'entity:before-remove type id entity)
                  
                  ;; 5. Execute deletion (unless dry-run)
                  (unless dry-run
                    ;; 5.1 Delete all outgoing links
                    (let ((link-count (org-supertag-db-unlink-all id)))
                      (message "Removed %d outgoing links for entity %s" link-count id))
                    
                    ;; 5.2 Delete all incoming links
                    (dolist (rev-link incoming-links)
                      (when (car rev-link) ;; Ensure source exists
                        (org-supertag-db-unlink nil (car rev-link) id)))
                    
                    ;; 5.3 Execute specific cleanup based on type
                    (pcase type
                      (:node
                       (org-supertag-db--cache-remove 'query (format "node-tags:%s" id))
                       (org-supertag-db--cache-remove 'query (format "node-fields:%s" id))
                       (org-supertag-db--cache-remove 'query (format "node-refs:%s" id)))
                      (:tag
                       (org-supertag-db--cache-remove 'query (format "tag-fields:%s" id))
                       (org-supertag-db--cache-remove 'query (format "tag-refs:%s" id))))
                    
                    ;; 5.4 Remove entity from database
                    (message "Removing entity %s of type %s from database" id type)
                    (ht-remove! org-supertag-db--object id)
                    
                    ;; 5.5 Clean up cache
                    (org-supertag-db--cache-remove 'entity id)
                    (org-supertag-db--cache-remove 'query (format "type:%s" type))
                    
                    ;; 5.6 Trigger deletion completion event
                    (org-supertag-db-emit 'entity:removed type id entity removed-data)
                    
                    ;; 5.7 Mark database as dirty and execute save
                    (message "Marking database as dirty")
                    (org-supertag-db--mark-dirty)
                    (message "Scheduling database save")
                    (org-supertag-db--schedule-save))
                  
                  ;; 6. Return deleted data
                  (cons entity (append outgoing-links incoming-links))))))))
    
    ;; Error handling
    (error
     (message "Error removing entity %s: %s" id (error-message-string err))
     nil))) ;; Return nil instead of signaling an error

(defun org-supertag-db-remove-link (type from to &optional dry-run)
  "Remove a specific relationship.
TYPE: Relationship type (:node-tag, :node-field, :tag-ref)
FROM: Source entity ID
TO: Target entity ID
DRY-RUN: If non-nil, only return data to be deleted without actual deletion

Returns:
- The deleted relationship data if found and removed
- nil if relationship does not exist
- Throws error if validation fails"
  (condition-case err
      (let* ((rel-id (format "%s:%s->%s" type from to))
             (link (ht-get org-supertag-db--link rel-id)))
        (when link
          ;; 1. Validate relationship
          (unless (and (plist-get link :from)
                      (plist-get link :to)
                      (org-supertag-db-valid-link-type-p type))
            (error "Invalid link data: %S" link))
          ;; 2. Trigger pre-removal event
          (org-supertag-db-emit 'link:before-remove type from to link)
          ;; 3. Execute actual deletion if not dry-run
          (unless dry-run
            ;; 3.1 Remove relationship
            (ht-remove! org-supertag-db--link rel-id)
            ;; 3.2 Clear caches
            ;; Main cache
            (org-supertag-db--cache-remove 'link rel-id)
            ;; Query caches
            (org-supertag-db--cache-remove 'query (format "links:%s:%s" type from))
            (org-supertag-db--cache-remove 'query (format "links:%s:%s" type to))
            ;; 3.3 Clear type-specific caches
            (pcase type
              (:node-field
               ;; Clear node field cache
               (org-supertag-db--cache-remove 'query (format "node-fields:%s" from)))
              (:node-tag
               ;; Clear node tag cache
               (org-supertag-db--cache-remove 'query (format "node-tags:%s" from)))
               (:tag-ref
                ;; Clear tag reference cache
                (org-supertag-db--cache-remove 'query (format "tag-refs:%s" from))
                (org-supertag-db--cache-remove 'query (format "tag-refs:%s" to))))
            ;; 3.4 Trigger completion event
            (org-supertag-db-emit 'link:removed type from to link)
            ;; 3.5 Mark database as dirty and schedule save
            (org-supertag-db--mark-dirty)
            (org-supertag-db--schedule-save))
          ;; 4. Return removed data
          link))
    ;; Error handling
    (error
     (message "Error removing link %s:%s->%s: %s"
              type from to (error-message-string err))
              (signal (car err) (cdr err)))))

;;------------------------------------------------------------------------------  
;; Property Operation
;;------------------------------------------------------------------------------

(defun org-supertag-db-set-property (id property value)
  "Set PROPERTY of entity with ID to VALUE.
Returns t if successful, nil if entity not found."
  (let ((entity (org-supertag-db-get id)))
    (when entity
      ;; update property  
      (let ((new-props (plist-put (copy-sequence entity) 
                                 (intern (concat ":" property))
                                 value)))
        ;; save updated entity
        (org-supertag-db-add id new-props)
        t))))

(defun org-supertag-db-get-property (id property)
  "Get PROPERTY value of entity with ID.
Returns property value if found, nil otherwise."
  (let ((entity (org-supertag-db-get id)))
    (when entity
      (plist-get entity (intern (concat ":" property))))))

;;------------------------------------------------------------------------------
;; Org Element 解析
;;------------------------------------------------------------------------------    

(require 'org-element)

(defun org-supertag-db--normalize-props (props)
  "Normalize property list to ensure correct order and no duplicates.
PROPS is the property list

Returns:
- Normalized property list"
  (let ((result nil)
        (seen-keys nil))
    ;; 1. Ensure we have a type
    (let ((type-value (or (plist-get props :type)
                         ;; If no type but has file-path, assume it's a node
                         (when (plist-get props :file-path) :node))))
      (when type-value
        (push :type seen-keys)
        (setq result (list :type type-value))))
    
    ;; 2. Process other properties
    (let ((rest-props props))
      (while rest-props
        (let ((key (car rest-props))
              (value (cadr rest-props)))
          (unless (memq key seen-keys)  ; Avoid duplicates
            (push key seen-keys)
            (setq result (append result (list key value)))))
        (setq rest-props (cddr rest-props))))
    
    ;; 3. Validate result
    (let ((final-type (plist-get result :type)))
      (unless (memq final-type '(:node :tag))
        (error "Invalid or missing type in props: %S" props)))
    
    result))

(defun org-supertag-db--clean-text (text)
  "Clean text by removing all text properties and ensuring it's a plain string.
TEXT is the text to clean.
Returns a clean string without any text properties."
  (when text
    (let ((str (if (stringp text)
                   text
                 (format "%s" text))))
      (substring-no-properties (string-trim str)))))

(defun org-supertag-db--get-node-title ()
  "Get current node title in plain text format."
  (condition-case err
      (org-supertag-db--clean-text
       (or (org-get-heading t t t t)  ; Remove tags, TODO states etc
           (save-excursion
             (org-back-to-heading t)
             (let ((raw (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))))
               (replace-regexp-in-string
                "^\\*+ \\(?:\\[#[A-Z0-9]\\] \\)?\\(?:TODO\\|DONE\\|\\[.*?\\]\\|\\) *" 
                "" raw)))
           ""))
    (error
     (message "Error getting node title: %s" (error-message-string err))
     "")))

(defun org-supertag-db--parse-node-at-point ()
  "Parse node data at current point."
  (save-excursion
    (condition-case err
        (let* ((element (org-element-at-point))
               (type (org-element-type element)))
          (message "Parsing node at point: type=%s" type)
          (when (and (eq type 'headline)
                     ;; Only process nodes with existing ID
                     (org-entry-get nil "ID"))
            ;; Parse headline
            (let* ((title (org-supertag-db--get-node-title))
                   (level (org-element-property :level element))
                   (id (org-entry-get nil "ID"))
                   ;; Task properties - Use more reliable retrieval methods
                   (todo-state (org-entry-get nil "TODO"))
                   (priority (org-entry-get nil "PRIORITY"))
                   (scheduled (condition-case nil
                                (org-get-scheduled-time (point))
                              (error nil)))
                   (deadline (condition-case nil
                               (org-get-deadline-time (point))
                             (error nil)))
                   ;; Get outline path
                   (olp (org-get-outline-path t))
                   ;; Other properties
                   (tags (org-get-tags))
                   (properties (org-entry-properties nil 'standard))
                   ;; parse reference relations
                   (refs-to nil)
                   (refs-from nil)
                   ;; Get node content directly
                   (content (save-excursion
                             (org-end-of-meta-data t)
                             (let* ((begin (point))
                                    (end (org-element-property :contents-end element))
                                    ;; Find next heading position
                                    (next-heading (save-excursion
                                                  (org-next-visible-heading 1)
                                                  (point))))
                               ;; go to begin
                               (goto-char begin)
                               ;; use more accurate boundary
                               (let ((content-end (cond
                                                 ;; if there is a next heading and in the current node content range
                                                 ((and next-heading end (< next-heading end))
                                                  (1- next-heading))
                                                 ;; use the content end position of the node
                                                 (end end)
                                                 ;; if none, use the end position of the current node
                                                 (t (org-entry-end-position)))))
                                 ;; collect references
                                 (while (re-search-forward org-link-any-re content-end t)
                                   (let* ((link (org-element-context))
                                          (link-type (org-element-property :type link))
                                          (link-path (org-element-property :path link)))
                                     (when (and (equal link-type "id")
                                              (org-uuidgen-p link-path))
                                       (push link-path refs-to))))
                                 ;; only return content when begin and end are valid and have content
                                 (when (and begin content-end (< begin content-end))
                                   (org-supertag-db--clean-text
                                    (buffer-substring-no-properties begin content-end)))))))
                   ;; get other nodes that reference this node (keep existing reference relations)
                   (existing-node (org-supertag-db-get id))
                   (refs-from (when existing-node
                              (plist-get existing-node :ref-from))))
              
              (message "Parsed headline: id=%s title=%s level=%s todo=%s priority=%s refs=%d" 
                      id title level todo-state priority (length refs-to))
              
              (list :type :node
                    :id id
                    :title title
                    :file-path (buffer-file-name)
                    :pos (org-element-property :begin element)
                    :olp olp
                    :level level
                    ;; Task properties
                    :todo todo-state
                    :priority priority
                    :scheduled scheduled
                    :deadline deadline
                    ;; Other properties
                    :properties properties
                    :tags tags
                    :content content
                    ;; Reference relations
                    :ref-to (delete-dups refs-to)
                    :ref-from (or refs-from nil)
                    :ref-count (+ (length refs-to) (length refs-from))
                    :created-at (current-time)))))
      (error
       (message "Error parsing node at point: %s" (error-message-string err))
       nil))))

(defun org-supertag-db--validate-node-props (props)
  "Validate node property completeness."
  (let ((required '(:id :title :file-path :type)))
    (cl-every (lambda (key)
                (when-let* ((value (plist-get props key)))
                  (not (string-empty-p (format "%s" value)))))
                required)))

;;------------------------------------------------------------------------------
;; Batch Node Parsing
;;------------------------------------------------------------------------------    

(defun org-supertag-db-get-pos (node-id)
  "Get buffer position for node with NODE-ID.
Returns position number or marker if found, nil otherwise."
  (condition-case nil
      (org-id-find node-id t)
    (error nil)))

;;------------------------------------------------------------------------------
;; Node Reference Parsing
;;------------------------------------------------------------------------------    

(defun org-supertag-db--parse-node-all-ref ()
  "Parse all references in current node.
Returns list of referenced node IDs.
References are parsed from org links in node content."
  (let ((ref-to nil)
        (node-id (org-supertag-db--get-current-node-id)))
    (when node-id
      ;; Parse links in entire node range
      (let ((start (org-entry-beginning-position))
            (end (org-entry-end-position)))
        (save-excursion
          (goto-char start)
          (while (re-search-forward org-link-any-re end t)
            (let* ((link (org-element-context))
                   (type (org-element-property :type link))
                   (path (org-element-property :path link)))
              (when (and (equal type "id")
                       (org-uuidgen-p path))
                (push path ref-to)))))
        ;; Update reference relationships
        (let ((ref-to (delete-dups (nreverse ref-to))))
          (org-supertag-db--update-node-all-ref node-id ref-to)
          ref-to)))))

(defun org-supertag-db--update-node-all-ref (node-id ref-to)
  "Update all reference relationships for a node.
NODE-ID is the current node ID
REF-TO is a list of referenced node IDs"
  (message "Starting to update node references - Node ID: %s" node-id)
  (message "New reference list: %S" ref-to)
  (let* ((node (org-supertag-db-get node-id))
         (old-refs (plist-get node :ref-to)))
    (message "Original reference list: %S" old-refs)
    ;; Update current node's references
    (org-supertag-db-add node-id
                         (plist-put node :ref-to ref-to))
    (message "Updated current node's references")
    ;; Update back-references of referenced nodes and trigger new reference events
    (dolist (ref-id ref-to)
      (message "Processing referenced node: %s" ref-id)
      (unless (member ref-id old-refs)  ; Only trigger events for new references
        (message "Found new reference, triggering event")
        (org-supertag-db-emit 'ref:created node-id ref-id nil))
      (when-let* ((ref-node (org-supertag-db-get ref-id)))
        (let* ((ref-from (plist-get ref-node :ref-from))
               (new-ref-from (cons node-id (delete node-id ref-from))))
          (message "Updating back-references of referenced node: %S" new-ref-from)
          (org-supertag-db-add ref-id
                              (plist-put ref-node 
                                        :ref-from 
                                        (delete-dups new-ref-from))))))
    ;; Clean up old references and trigger removal events
    (dolist (old-ref old-refs)
      (unless (member old-ref ref-to)
        (message "Cleaning up old reference: %s" old-ref)
        (org-supertag-db-emit 'ref:removed node-id old-ref nil)
        (when-let* ((ref-node (org-supertag-db-get old-ref)))
          (let ((ref-from (delete node-id (plist-get ref-node :ref-from))))
            (message "Updating back-references of previously referenced node: %S" ref-from)
            (org-supertag-db-add old-ref
                                (plist-put ref-node :ref-from ref-from))))))
    (message "Node reference relationships update completed")))

(defun org-supertag-db--get-current-node-id ()
  "Get the ID of current node."
  (save-excursion
    (org-back-to-heading t)
    (let ((id (or (org-id-get)
                  (org-id-get-create))))
      (message "Got current node ID: %s" id)
      id)))


;;------------------------------------------------------------------------------
;; Event System
;;------------------------------------------------------------------------------    

;; Event System Examples:
;; 1. Event Types
;;    - entity:changed  ; Entity update event, triggered when entity properties change
;;    - link:created    ; Link creation event, triggered when new link is created
;;    - link:removed    ; Link removal event, triggered when link is deleted
;;    - field:changed   ; Field value change event, triggered when field value updates
;; 2. Register Event Handler
;; (org-supertag-db-on 'entity:changed handler)
;; Handler function receives event-related parameters:
;; - entity:changed: (id props)     ; Entity ID and new properties
;; - link:created:  (type from to)  ; Link type, source, target
;; - link:removed:  (type from to)  ; Link type, source, target
;; - field:changed: (field-id value); Field ID and new value
;; 3. Example: Listen for entity update event
;; (org-supertag-db-on 'entity:changed
;;                     (lambda (id props)
;;                       (message "Entity %s updated: %S" id props)))
;; 4. Event Triggering
;; Events are automatically triggered when calling data operation functions:
;; (org-supertag-db-add "node-1" '(:type :node :title "test")) ; Triggers entity:changed

(defvar org-supertag-db--events (ht-create)
  "Event handler mapping table.
Maps event-name -> (handler1 handler2 ...)")

(defun org-supertag-db-on (event handler)
  "Register an event handler.
EVENT: Event name, e.g. 'entity:changed
HANDLER: Handler function that receives event-related parameters"
  (let ((handlers (ht-get org-supertag-db--events event)))
    (ht-set! org-supertag-db--events event 
             (cons handler handlers))))

(defun org-supertag-db-off (event handler)
  "Remove an event handler.
EVENT: Event name
HANDLER: Handler function to remove"
  (let* ((handlers (ht-get org-supertag-db--events event))
         (new-handlers (remove handler handlers)))
    (ht-set! org-supertag-db--events event new-handlers)))

(defun org-supertag-db-emit (event &rest args)
  "Trigger an event.
EVENT: Event name
ARGS: Parameters passed to handlers"
  (dolist (handler (ht-get org-supertag-db--events event))
    (condition-case err
        (apply handler args)
      (error 
       (message "Event handler error [%s]: %S" event err)))))



;;------------------------------------------------------------------------------
;; Status
;;------------------------------------------------------------------------------    

(defun org-supertag-db-check-status ()
  "Check database status."
  (interactive)
  (message "\n=== Database Status ===")
  (message "Database file: %s" org-supertag-db-file)
  (message "File exists: %s" (file-exists-p org-supertag-db-file))
  (message "Current entities: %d" (ht-size org-supertag-db--object))
  (message "Current links: %d" (ht-size org-supertag-db--link)))

(defun org-supertag-db-test ()
  "Test database operations."
  (interactive)
  (message "\n=== Testing Database Operations ===")
  
  ;; 1. Add a test entity
  (org-supertag-db-add "test-tag" '(:type :tag :name "test"))
  (message "Size after adding entity: %d" (ht-size org-supertag-db--object))
  
  ;; 2. Add a test link
  (org-supertag-db-link :node-tag "test-node" "test-tag")
  (message "Size after adding link: %d" (ht-size org-supertag-db--link))
  
  ;; 3. Save database
  (org-supertag-db-save)
  (message "Data saved")
  
  ;; 4. Reload
  (org-supertag-db-load)
  (message "After reload:")
  (message "Entities: %d" (ht-size org-supertag-db--object))
  (message "Links: %d" (ht-size org-supertag-db--link)))

;;-----------------------------------------------------------------------------
;; Cache System
;;-----------------------------------------------------------------------------

(defvar org-supertag-db--cache (ht-create)
  "Simple query cache.
Key format: 'query:hash' -> query results
Key format: 'entity:id' -> entity data")

(defun org-supertag-db--cache-key (type key)
  "Generate cache key.
TYPE: Cache type (query or entity)
KEY: Specific key"
  (format "%s:%s" type key))

(defun org-supertag-db--cache-get (type key)
  "Get cache value."
  (ht-get org-supertag-db--cache 
          (org-supertag-db--cache-key type key)))

(defun org-supertag-db--cache-set (type key value)
  "Set cache value."
  (ht-set! org-supertag-db--cache 
           (org-supertag-db--cache-key type key)
           value))

(defun org-supertag-db--cache-remove (type key)
  "Remove cache entry."
  (ht-remove! org-supertag-db--cache 
              (org-supertag-db--cache-key type key)))

(defun org-supertag-db--cache-clear ()
  "Clear all cache entries."
  (ht-clear! org-supertag-db--cache))

(defun org-supertag-db--cache-clear-for-type (type id)
  "Clear all related caches for specific entity type.
TYPE: Entity type
ID: Entity ID"
  (pcase type
    (:node
     (org-supertag-db--cache-remove 'query (format "node-tags:%s" id))
     (org-supertag-db--cache-remove 'query (format "node-fields:%s" id))
     (org-supertag-db--cache-remove 'query (format "node-refs:%s" id)))
    (:tag
     (org-supertag-db--cache-remove 'query (format "tag-fields:%s" id))
     (org-supertag-db--cache-remove 'query (format "tag-refs:%s" id)))))

;;------------------------------------------------------------------------------
;; Dirty State
;;------------------------------------------------------------------------------    

(defvar org-supertag-db--dirty nil
  "Flag indicating if database has unsaved changes.")

(defun org-supertag-db--mark-dirty ()
  "Mark database as having unsaved changes."
  (setq org-supertag-db--dirty t))

(defun org-supertag-db--clear-dirty ()
  "Clear database unsaved changes flag."
  (setq org-supertag-db--dirty nil))

(defun org-supertag-db--dirty-p ()
  "Check if database has unsaved changes."
  org-supertag-db--dirty)

;;------------------------------------------------------------------------------
;; Database Custom
;;------------------------------------------------------------------------------  

(defcustom org-supertag-data-directory
  (expand-file-name "org-supertag" user-emacs-directory)
  "Directory for storing org-supertag data.
Defaults to org-supertag subdirectory under Emacs config directory."
  :type 'directory
  :group 'org-supertag)


(defun org-supertag-db-ensure-data-directory ()
  "Ensure database and backup directories exist."
  (let ((db-dir (file-name-directory org-supertag-db-file)))
    ;; 1. Ensure database directory exists
    (unless (file-exists-p db-dir)
      (make-directory db-dir t))
    ;; 2. Ensure backup directory exists  
    (unless (file-exists-p org-supertag-db-backup-directory)
      (make-directory org-supertag-db-backup-directory t))
    ;; 3. Verify directory creation
    (unless (and (file-exists-p db-dir)
                 (file-exists-p org-supertag-db-backup-directory))
      (error "Failed to create required directories: %s or %s"
             db-dir org-supertag-db-backup-directory))))

(defun org-supertag-data-file (filename)
  "Get full path for data file.
FILENAME is relative to data directory."
  (expand-file-name filename org-supertag-data-directory))

(defcustom org-supertag-db-file
  (org-supertag-data-file "supertag-db.el")
  "Database file path."
  :type 'file
  :group 'org-supertag)

;;------------------------------------------------------------------------------
;; Data Backup
;;------------------------------------------------------------------------------      

(defcustom org-supertag-db-backup-directory
  (org-supertag-data-file "backups")
  "Directory for database backups."
  :type 'directory
  :group 'org-supertag)

(defcustom org-supertag-db-backup-max-days 3
  "Maximum number of days to keep backups."
  :type 'integer
  :group 'org-supertag)

(defun org-supertag-db-cleanup-backups ()
  "Clean up old backup files, keeping only recent days."
  (let* ((now (float-time))
         (max-age (* org-supertag-db-backup-max-days 24 60 60))
         (cutoff (- now max-age)))
    (dolist (file (directory-files org-supertag-db-backup-directory t "db-.*\\.el$"))
      (let ((mtime (float-time (file-attribute-modification-time
                               (file-attributes file)))))
        (when (< mtime cutoff)
          (delete-file file))))))

(defun org-supertag-db-backup ()
  "Create database backup."
  (when (file-exists-p org-supertag-db-file)
    (let* ((backup-name (format "db-%s.el" 
                               (format-time-string "%Y%m%d-%H%M%S")))
           (backup-file (expand-file-name backup-name org-supertag-db-backup-directory)))
      (copy-file org-supertag-db-file backup-file t)
      (org-supertag-db-cleanup-backups))))

;;------------------------------------------------------------------------------
;; Data Save&Load
;;------------------------------------------------------------------------------

(defun org-supertag-db-save ()
  "Save database to file.
This function:
1. Checks if database is dirty
2. Creates backup if needed
3. Writes data with proper print settings
4. Runs save hooks before and after
Returns t on success, nil on failure."
  (when (org-supertag-db--dirty-p)
    (message "Saving database...")
    (condition-case err
        (progn
          ;; Run pre-save hooks
          (run-hooks 'org-supertag-db-before-save-hook)
          
          ;; Create backup
          (when (file-exists-p org-supertag-db-file)
            (org-supertag-db-backup))
          
          ;; Ensure directory exists
          (make-directory (file-name-directory org-supertag-db-file) t)
          
          ;; Write to file
          (with-temp-file org-supertag-db-file
            (let ((print-level nil)
                  (print-length nil)
                  (print-circle t))
              (insert ";; -*- lexical-binding: t -*-\n")
              (insert ";;; Database file - Do not edit manually\n\n")
              (insert "(require 'ht)\n")
              (insert "(setq org-supertag-db--object (ht-create))\n")
              (insert "(setq org-supertag-db--link (ht-create))\n\n")
              
              ;; Save data using temp variables
              (let ((db-copy (ht-copy org-supertag-db--object)))
                (ht-map (lambda (k v)
                         (insert (format "(ht-set! org-supertag-db--object %S '%S)\n" 
                                       k v)))
                       db-copy))
              (ht-map (lambda (k v)
                       (insert (format "(ht-set! org-supertag-db--link %S '%S)\n" 
                                     k v)))
                     org-supertag-db--link)))
          ;; Clear dirty flag
          (org-supertag-db--clear-dirty)
          ;; Run post-save hooks
          (run-hooks 'org-supertag-db-after-save-hook)
          (message "Database saved successfully")
          t)
      (error
       (message "Save failed: %S" err)
       nil))))

(defun org-supertag-db-load ()
  "Load database from file.
This function:
1. Checks if file exists
2. Initializes empty database
3. Loads data from file
4. Runs load hooks before and after
Returns t on success, nil on failure."
  (message "Loading database...")
  (condition-case err
      (if (file-exists-p org-supertag-db-file)
          (progn
            ;; Run pre-load hooks
            (run-hooks 'org-supertag-db-before-load-hook)
            
            ;; Initialize empty database
            (setq org-supertag-db--object (ht-create)
                  org-supertag-db--link (ht-create))  
            ;; Load file
            (load org-supertag-db-file nil t)
            
            ;; Clear dirty flag
            (org-supertag-db--clear-dirty)
            
            ;; Run post-load hooks
            (run-hooks 'org-supertag-db-after-load-hook)
            
            (message "Database loaded: %d entities, %d links"
                    (ht-size org-supertag-db--object)
                    (ht-size org-supertag-db--link))
            t)
        (message "Database file does not exist")
        nil)
    (error
     (message "Load failed: %S" err)
     nil)))

;;------------------------------------------------------------------------------
;; Delayed Save
;;------------------------------------------------------------------------------    

(defvar org-supertag-db--save-timer nil
  "Timer for delayed save.")

(defun org-supertag-db--schedule-save ()
  "Schedule a delayed save.
Waits for 2 seconds of idle time before saving to avoid frequent saves."
  (when org-supertag-db--save-timer
    (cancel-timer org-supertag-db--save-timer))
  (setq org-supertag-db--save-timer
        (run-with-idle-timer 2 nil #'org-supertag-db-save)))

;;------------------------------------------------------------------------------
;; Data Save&Load Hooks
;;------------------------------------------------------------------------------    

(defvar org-supertag-db-before-save-hook nil
  "Hook run before saving database.
Runs before data is written to file.")

(defvar org-supertag-db-after-save-hook nil
  "Hook run after saving database.
Runs after data is successfully written to file.")

(defvar org-supertag-db-before-load-hook nil
  "Hook run before loading database.
Runs before reading data from file.")

(defvar org-supertag-db-after-load-hook nil
  "Hook run after loading database.
Runs after data is successfully loaded into memory.")

;;------------------------------------------------------------------------------
;; Auto Save
;;------------------------------------------------------------------------------  

(defcustom org-supertag-db-auto-save-interval 300
  "Auto-save interval in seconds.
Set to nil to disable auto-save."
  :type '(choice (const :tag "Disable" nil)
                (integer :tag "Interval (seconds)"))
  :group 'org-supertag)

(defvar org-supertag-db--auto-save-timer nil
  "Timer for auto-save.")

(defun org-supertag-db--setup-auto-save ()
  "Set up auto-save timer."
  (when (and org-supertag-db-auto-save-interval
             (null org-supertag-db--auto-save-timer))
    (setq org-supertag-db--auto-save-timer
          (run-with-timer org-supertag-db-auto-save-interval
                         org-supertag-db-auto-save-interval
                         #'org-supertag-db-save))))

(defun org-supertag-db--cleanup-auto-save ()
  "Clean up auto-save timer."
  (when org-supertag-db--auto-save-timer
    (cancel-timer org-supertag-db--auto-save-timer)
    (setq org-supertag-db--auto-save-timer nil)))

;;------------------------------------------------------------------------------
;; Init
;;------------------------------------------------------------------------------  

(defun org-supertag-db-init ()
  "Initialize database.
Steps:
1. Try to load existing database
2. Create empty database if load fails
3. Set up auto-save
4. Set up change listeners for auto-save"
  (message "Initializing database...")
  ;; Try loading existing database
  (unless (org-supertag-db-load)
    ;; Create empty database if load fails
    (setq org-supertag-db--object (ht-create)
          org-supertag-db--link (ht-create))  
    (message "Created empty database"))
  ;; Set up change listeners
  (org-supertag-db-on 'entity:changed
                    (lambda (type id props)
                      (org-supertag-db--mark-dirty)
                      (org-supertag-db--schedule-save)))
  (org-supertag-db-on 'entity:added 
                    (lambda (type id props)
                      (org-supertag-db--mark-dirty)
                      (org-supertag-db--schedule-save)))
  (org-supertag-db-on 'entity:removed
                    (lambda (type id entity removed-data)
                      (org-supertag-db--mark-dirty)
                      (org-supertag-db--schedule-save)))
  (org-supertag-db-on 'link:created
                    (lambda (type from to props)
                      (org-supertag-db--mark-dirty)
                      (org-supertag-db--schedule-save)))
  (org-supertag-db-on 'link:removed
                    (lambda (type from to props)
                      (org-supertag-db--mark-dirty)
                      (org-supertag-db--schedule-save)))

  ;; Set up auto-save
  (when (and org-supertag-db-auto-save-interval
             (null org-supertag-db--auto-save-timer))
    (setq org-supertag-db--auto-save-timer
          (run-with-timer org-supertag-db-auto-save-interval
                         org-supertag-db-auto-save-interval
                         #'org-supertag-db-save))))

;; Save on Emacs exit
(add-hook 'kill-emacs-hook #'org-supertag-db-save)

;; Clear cache before loading
(add-hook 'org-supertag-db-before-load-hook #'org-supertag-db--cache-clear)



;;------------------------------------------------------------------------------
;; Metadata Storage
;;------------------------------------------------------------------------------  

(defvar org-supertag-db--metadata (ht-create)
  "Hash table for storing global metadata.
Metadata is stored as key-value pairs and is used for configuration,
statistics, and relationship data that doesn't fit the entity model.")

(defun org-supertag-db-get-metadata (key &optional default)
  "Get metadata value for KEY.
If KEY does not exist, return DEFAULT or nil if DEFAULT is not provided.
KEY should be a string or symbol identifying the metadata.
Returns the stored value or DEFAULT if not found."
  (ht-get org-supertag-db--metadata (if (symbolp key) key (intern key)) default))

(defun org-supertag-db-set-metadata (key value)
  "Set metadata KEY to VALUE.
KEY should be a string or symbol identifying the metadata.
VALUE can be any Lisp object.
Returns VALUE if successful.

This is used for storing configuration, statistics, and
relationship data that doesn't fit into the entity model.
Example uses:
- Cooccurrence statistics
- Tag frequency data
- Usage history
- Global settings"
  (let ((k (if (symbolp key) key (intern key))))
    ;; Store the value
    (ht-set! org-supertag-db--metadata k value)
    ;; Mark database as dirty and schedule save
    (org-supertag-db--mark-dirty)
    (org-supertag-db--schedule-save)
    ;; Emit event for tracking
    (org-supertag-db-emit 'metadata:changed k value)
    ;; Return the value
    value))

(defun org-supertag-db-remove-metadata (key)
  "Remove metadata entry with KEY.
KEY should be a string or symbol identifying the metadata.
Returns t if the key existed and was removed, nil otherwise."
  (let ((k (if (symbolp key) key (intern key))))
    (when (ht-contains-p org-supertag-db--metadata k)
      ;; Remove the entry
      (ht-remove! org-supertag-db--metadata k)
      ;; Mark database as dirty and schedule save
      (org-supertag-db--mark-dirty)
      (org-supertag-db--schedule-save)
      ;; Emit event for tracking
      (org-supertag-db-emit 'metadata:removed k)
      t)))

(defun org-supertag-db-list-metadata (&optional prefix)
  "List all metadata keys, optionally filtered by PREFIX.
If PREFIX is provided, only return keys that start with PREFIX.
Returns an alist of (key . value) pairs."
  (let ((results nil))
    (ht-map (lambda (k v)
              (when (or (null prefix)
                        (string-prefix-p prefix (symbol-name k)))
                (push (cons k v) results)))
            org-supertag-db--metadata)
    (nreverse results)))

;; Add metadata save/load support to existing save/load functions
(add-hook 'org-supertag-db-before-save-hook
          (lambda ()
            (ht-set! org-supertag-db--object "metadata"
                   `(:type :metadata :data ,org-supertag-db--metadata))))

(add-hook 'org-supertag-db-after-load-hook
          (lambda ()
            (when-let* ((metadata-entry (org-supertag-db-get "metadata")))
              (setq org-supertag-db--metadata (or (plist-get metadata-entry :data)
                                                 (ht-create))))))

(defun org-supertag-db-test-metadata ()
  "Test metadata storage functions."
  (interactive)
  (message "\n=== Testing Metadata Functions ===")
  
  ;; Set some test metadata
  (org-supertag-db-set-metadata 'test-key "Test Value")
  (org-supertag-db-set-metadata "numeric-key" 42)
  (org-supertag-db-set-metadata 'list-key '(a b c))
  
  ;; Display current metadata
  (message "Current metadata keys: %S" 
           (mapcar #'car (org-supertag-db-list-metadata)))
  
  ;; Retrieve and verify values
  (let ((test-val (org-supertag-db-get-metadata 'test-key))
        (num-val (org-supertag-db-get-metadata "numeric-key"))
        (list-val (org-supertag-db-get-metadata 'list-key))
        (missing-val (org-supertag-db-get-metadata 'nonexistent "default")))
    (message "test-key: %S (expected: \"Test Value\")" test-val)
    (message "numeric-key: %S (expected: 42)" num-val)
    (message "list-key: %S (expected: (a b c))" list-val)
    (message "nonexistent: %S (expected: \"default\")" missing-val))
  
  ;; Remove a key
  (org-supertag-db-remove-metadata 'test-key)
  (message "After removal: %S" 
           (mapcar #'car (org-supertag-db-list-metadata)))
  
  ;; Save and reload test
  (when (yes-or-no-p "Save database to test persistence? ")
    (org-supertag-db-save)
    (setq org-supertag-db--metadata (ht-create)) ;; Clear in-memory
    (message "Metadata table cleared. Current keys: %S" 
             (mapcar #'car (org-supertag-db-list-metadata)))
    (org-supertag-db-load)
    (message "After reload: %S" 
             (mapcar #'car (org-supertag-db-list-metadata)))))

(provide 'org-supertag-db)
;;; org-supertag-db.el ends here
  

;;; supertag-view-api.el --- UI-agnostic data API for Org-Supertag views -*- lexical-binding: t; -*-

;;; Commentary:
;; This module defines the internal public "View Data API" for org-supertag.
;;
;; Goal:
;; - Provide a stable, UI-agnostic read interface to the underlying DB/Store.
;; - Allow view plugins to build any UI (table, cards, dashboards, graphs, etc.)
;;   while using the same data access contract.
;;
;; Non-goal:
;; - This module does not define UI widgets/components.
;; - This module does not write data; writes should go through ops functions
;;   (e.g. `supertag-field-set', `supertag-node-update') and transactions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'supertag-core-store)
(require 'supertag-core-notify)
(require 'supertag-core-scan) ; scan-based queries like nodes-by-tag
(require 'supertag-ops-node)
(require 'supertag-ops-field)

;; --- Query & Entity Fetch ---

(defun supertag-view-api-list-entity-ids (query-spec)
  "Return entity IDs for QUERY-SPEC.

QUERY-SPEC is a plist describing the dataset, for example:
- (:type :tag :value \"foo\")
- (:type :nodes)
- (:type :tags)
- (:type :automations)

This function is UI-agnostic and read-only."
  (let ((type (plist-get query-spec :type)))
    (pcase type
      (:tag
       (let ((tag (plist-get query-spec :value)))
         (unless (and tag (stringp tag) (not (string-empty-p tag)))
           (error "Query :tag requires a non-empty :value string"))
         (supertag-index-get-nodes-by-tag tag)))
      ((or :nodes :tags :relations :embeds
           ;; Some query specs use singular names in UI layers; accept them here.
           :automation :automations
           :behavior :behaviors
           :database :databases)
       (let* ((collection (pcase type
                            (:automation :automations)
                            (:behavior :behaviors)
                            (:database :databases)
                            (_ type)))
              (bucket (supertag-store-get-collection collection))
             (ids '()))
         (when (hash-table-p bucket)
           (maphash (lambda (id _v) (push id ids)) bucket))
         (nreverse ids)))
      (_
       (error "Unsupported query type: %S" type)))))

(defun supertag-view-api-get-collection (collection)
  "Return the underlying store collection hash table for COLLECTION.

This is an internal public API intended for view data access only.
Callers MUST treat the returned hash table as read-only."
  (let ((normalized
         (pcase collection
           (:node :nodes)
           (:tag :tags)
           (:relation :relations)
           (:embed :embeds)
           (:automation :automations)
           (:behavior :behaviors)
           (:database :databases)
           (_ collection))))
    (supertag-store-get-collection normalized)))

(defun supertag-view-api-get-entity (type entity-id)
  "Return entity plist for TYPE and ENTITY-ID (read-only)."
  (unless (and entity-id (stringp entity-id) (not (string-empty-p entity-id)))
    (error "ENTITY-ID must be a non-empty string"))
  (let ((normalized
         (pcase type
           (:node :nodes)
           (:tag :tags)
           (:relation :relations)
           (:embed :embeds)
           (:automation :automations)
           (:behavior :behaviors)
           (:database :databases)
           (_ type))))
    (pcase normalized
    (:nodes (supertag-node-get entity-id))
    (:tags (supertag-store-get-entity :tags entity-id))
    (:automations (supertag-store-get-entity :automations entity-id))
    (_ (supertag-store-get-entity normalized entity-id)))))

(defun supertag-view-api-get-entities (type entity-ids)
  "Return list of entities for TYPE and ENTITY-IDS.

This is a convenience function; callers can still batch on their own.
Entities that do not exist are skipped."
  (let (result)
    (dolist (entity-id entity-ids (nreverse result))
      (let ((entity (and entity-id (supertag-view-api-get-entity type entity-id))))
        (when entity
          (push entity result))))))

;; --- Tag Helpers ---

(defun supertag-view-api-list-tags ()
  "Return tag names (sorted)."
  (let ((tags (supertag-view-api-get-collection :tags))
        (names '()))
    (when (hash-table-p tags)
      (maphash
       (lambda (_id tag-data)
         (when-let ((name (plist-get tag-data :name)))
           (push name names)))
       tags))
    (sort (delete-dups names) #'string<)))

(defun supertag-view-api-tag-id (tag-name)
  "Return tag ID for TAG-NAME, or nil."
  (unless (and (stringp tag-name) (not (string-empty-p tag-name)))
    (error "TAG-NAME must be a non-empty string"))
  (let ((tags (supertag-view-api-get-collection :tags)))
    (when (hash-table-p tags)
      (catch 'found
        (maphash
         (lambda (id tag-data)
           (when (equal (plist-get tag-data :name) tag-name)
             (throw 'found id)))
         tags)
        nil))))

(defun supertag-view-api-nodes-by-tag (tag-name)
  "Return node IDs that have TAG-NAME."
  (supertag-index-get-nodes-by-tag tag-name))

;; --- Field Access (UI-agnostic) ---

(defun supertag-view-api-node-base-field (node key)
  "Read KEY from NODE plist."
  (plist-get node key))

(defun supertag-view-api-node-field-in-tag (node-id tag-id field-name)
  "Read FIELD-NAME for NODE-ID within TAG-ID context.

FIELD-NAME is a string (as used by `supertag-field-get-with-default')."
  (unless (and (stringp field-name) (not (string-empty-p field-name)))
    (error "FIELD-NAME must be a non-empty string"))
  (supertag-field-get-with-default node-id tag-id field-name))

;; --- Subscription ---

(defun supertag-view-api-subscribe (event fn)
  "Subscribe FN to EVENT and return an unsubscribe function.

EVENT is a keyword (e.g. :node-updated) or a store path list.
FN is called with arguments determined by the event publisher."
  (unless (functionp fn)
    (error "FN must be a function"))
  (supertag-subscribe event fn))

(provide 'supertag-view-api)
;;; supertag-view-api.el ends here

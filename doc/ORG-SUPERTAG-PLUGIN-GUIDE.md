# ORG-SUPERTAG-PLUGIN-GUIDE: Full Plugin Developer Guide

This is the canonical developer guide for building org-supertag plugins.

Core principles:

- DB/Store is the single source of truth.
- Plugins primarily extend *views* (any UI), not schemas.
- Plugins MUST read data through the **UI-agnostic View Data API** (`supertag-view-api.el`).
- Writes go through **ops** APIs and (usually) `supertag-with-transaction`.

Working example plugin:

- `doc/examples/supertag-view-demo-dashboard.el`

Chinese version:

- `doc/ORG-SUPERTAG-PLUGIN-GUIDE_cn.md`

## 0) Data model & conventions

### Entities and storage

org-supertag stores its state in a central store (backed by hash tables). In
most user-facing discussions, we call this “the database”.

Common collections and their types:

- `:nodes` — node entities (plist)
- `:tags` — tag entities (plist)
- `:relations` — relation entities (plist)
- `:field-definitions`, `:tag-field-associations`, `:field-values` — global field model

### Entity representation

- Entities are **plists** (property lists), usually containing `:id` plus other keys.
- Entity IDs are strings.
- Keys are keywords (e.g. `:title`, `:file`, `:tags`).

### Read vs write contract

- Read APIs return plists and MUST be treated as immutable snapshots by plugin code.
- Write APIs accept plists or update functions; they update store and emit events.

## 1) Read APIs (View Data API)

The View Data API is **internal public** and UI-agnostic. Use it for *all* data reads
in plugins (even if you do not use table UI).

File:

- `supertag-view-api.el`

### Query spec

Many read APIs take a `QUERY-SPEC` plist, e.g.:

- `(:type :tag :value "foo")` → nodes that have tag "foo"
- `(:type :nodes)` → all node IDs
- `(:type :tags)` → all tag IDs

### API list (read-only)

**Dataset**

- `(supertag-view-api-list-tags) -> (list string)`  
  All tag names (sorted).

- `(supertag-view-api-tag-id TAG-NAME) -> string-or-nil`  
  Tag name → tag id.

- `(supertag-view-api-list-entity-ids QUERY-SPEC) -> (list string)`  
  Entry point to get IDs for a dataset.

**Entity fetch**

- `(supertag-view-api-get-entity TYPE ID) -> plist-or-nil`  
  Fetch one entity. `TYPE` supports aliases like `:node/:nodes`, `:tag/:tags`, etc.

- `(supertag-view-api-get-entities TYPE IDS) -> (list plist)`  
  Batch fetch (recommended for performance).

**Raw collections (advanced)**

- `(supertag-view-api-get-collection COLLECTION) -> hash-table`  
  Returns the underlying store collection hash table. Treat as **read-only**.
  Use only when you need to scan/aggregate (e.g. backlinks reverse scan, schema
  browsers). Prefer query helpers when available.

**Field access**

- `(supertag-view-api-node-field-in-tag NODE-ID TAG-ID FIELD-NAME) -> value`  
  Read a node field value within a tag context (field values).

**Subscription**

- `(supertag-view-api-subscribe EVENT FN) -> unsubscribe-fn`  
  Subscribe to changes. `EVENT` is a keyword like `:node-updated` / `:store-changed`
  or a store path list. Returns an `unsubscribe-fn` you should call on cleanup.

## 2) Write APIs (Ops layer)

Plugins that modify data MUST use ops APIs, not raw store mutation.

### Transactions (recommended)

Most write flows should be wrapped in:

```elisp
(supertag-with-transaction
  ;; multiple ops here
  ...)
```

This batches notifications and makes the UI react once per logical change.

### API list (common writes)

**Nodes**

- `(supertag-node-create PROPS) -> node-plist`  
- `(supertag-node-update NODE-ID UPDATER) -> node-plist-or-nil`  
- `(supertag-node-delete NODE-ID) -> deleted-node-or-nil`

**Tags**

- `(supertag-tag-create PROPS) -> tag-plist`  
- `(supertag-tag-update TAG-ID UPDATER) -> tag-plist-or-nil`  
- `(supertag-tag-delete TAG-ID) -> deleted-tag-or-nil`  
- `(supertag-tag-add-field TAG-ID FIELD-DEF) -> tag-plist`  
- `(supertag-tag-remove-field TAG-ID FIELD-NAME) -> tag-plist`

**Fields**

- `(supertag-field-set NODE-ID TAG-ID FIELD-NAME VALUE) -> VALUE`  
- `(supertag-field-set-many NODE-ID SPECS) -> plist`  
  `SPECS` is a list of `(:tag-id TAG-ID :field FIELD-NAME :value VALUE)` items.

**Relations**

- `(supertag-relation-add-reference FROM-ID TO-ID) -> relation-plist`  
- `(supertag-relation-delete RELATION-ID) -> deleted-relation-or-nil`

Data conventions for writes:

- IDs are strings.
- UPDATER functions receive the current plist and return the updated plist (or nil to abort).
- Prefer calling ops functions inside a transaction when you do multiple writes.

## 2.5) Schema Registration (Advanced)

org-supertag allows users to register/override schemas at init time.
This is intended for advanced setups (custom entities or extended validation),
and does not provide automatic migrations.

Recommended configuration pattern:

```elisp
(setq supertag-schema-registration-functions
      (list
       (lambda ()
         ;; Override/extend an existing schema (merge by default).
         (supertag-schema-register :node '(:my-field (:type :string :default "")))

         ;; Or register a brand new entity type + schema.
         (supertag-register-entity-type
          :my-entity
          '(:id (:type :string :required t)
            :name (:type :string :default "")))))))
```

## 3) Transaction system (what it actually means)

File:

- `supertag-core-transform.el`

API:

- `(supertag-with-transaction ...)`

Semantics:

- It **suppresses notifications** during the body and emits a **batch** of changes
  after the body finishes.
- It provides `supertag--transaction-active` and collects a transaction log.
- It currently focuses on notification batching; do not assume full rollback
  semantics unless explicitly implemented in code.

## 4) Reference plugin: non-table UI dashboard

File:

- `doc/examples/supertag-view-demo-dashboard.el`

It demonstrates:

- reading node IDs via `(supertag-view-api-nodes-by-tag TAG)`
- batch fetching nodes via `(supertag-view-api-get-entities :nodes IDS)`
- subscribing to `:node-updated` and refreshing with throttling
- rendering a custom buffer with clickable entries

## 5) Validation checklist (manual)

1. Load the demo:

```elisp
(add-to-list 'load-path "/path/to/org-supertag/doc/examples/")
(require 'supertag-view-demo-dashboard)
```

2. Open:

`M-x supertag-view-demo-dashboard-open`

3. Verify:

- dashboard shows node count and a clickable list
- `RET` jumps to the node
- after changing a node, the dashboard auto-refreshes (or press `g`)

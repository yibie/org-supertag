# Org-SuperTag 5.3+: Pure Emacs Lisp Knowledge Management

> **⚠️ IMPORTANT (5.3.0 and later):**
> If you are upgrading from a pre‑5.2.0 version, you **must run the global field database migration** before enabling `supertag-use-global-fields`.
> See `doc/GLOBAL-FIELD-MIGRATION-GUIDE.md` for step‑by‑step instructions.
>
> **🆕 NEW (5.3.0):** Property to Field Migration System
> Convert your existing Org `:PROPERTIES:` to structured database fields for better querying and automation.
> See the "Property to Field Migration" section below for complete instructions.

[English](./README.md) | [中文](./README_CN.md)

## ⚡ What Changed in 5.0

**One line summary**: We deleted 44% of the code, removed Python completely, and made everything faster.

- **Pure Emacs Lisp**: No more Python dependencies
- **Single source of truth**: All data in one hash table
- **5x faster sync**: No more EPC overhead
- **Simpler installation**: Just load the package

## 🚀 30-Second Quick Start

```emacs-lisp
;; 1. Install
(straight-use-package '(org-supertag :host github :repo "yibie/org-supertag"))

;; 2. Configure
(setq org-supertag-sync-directories '("~/org/"))

;; 3. Initialize (run once)
M-x supertag-sync-full-initialize
```

**That's it.** No Python, no virtualenv, no complexity.

## 🎯 Core Concept: Tags as Database Tables

Traditional Org-mode:

```org
* Project Ideas :project:
```

Org-SuperTag 5.0:

```org
* Project Ideas #project
  - status: planning
  - priority: high
  - due: 2024-12-31
```

Each `#tag` becomes a database table. Each heading becomes a record.

## 📋 Essential Commands

| Command                | Key | What it does                 |
| ---------------------- | --- | ---------------------------- |
| `supertag-add-tag`     | -   | Add a tag to current heading |
| `supertag-view-node`   | -   | View/edit structured data    |
| `supertag-search`      | -   | Query your knowledge base    |
| `supertag-capture`     | -   | Quick note capture           |
| `supertag-view-kanban` | -   | Visual task management       |

## 🔍 Real Examples

### Academic Research

```org
* Attention Is All You Need #paper
  - authors: Vaswani et al.
  - year: 2017
  - venue: NIPS
  - status: read
  - rating: 5
  - notes: Revolutionary attention mechanism
```

### Project Management

```org
* Website Redesign #project
  - status: in-progress
  - priority: high
  - due: 2024-12-15
  - owner: @team
```

### Meeting Notes

```org
* Sprint Planning #meeting
  - type: planning
  - date: 2024-11-15
  - participants: Alice, Bob, Carol
  - decisions: Use new framework
```

## 🎨 Smart Queries

Find all high-priority projects due this month:

```lisp
(supertag-search '(and (tag "project")
                       (field "priority" "high")
                       (field "due" "2024-12")))
```

Find papers you haven't read yet:

```lisp
(supertag-search '(and (tag "paper")
                       (field "status" "unread")))
```

## 🔄 Migration from 4.x

**One-time migration required:**

1. Backup your old database
2. `M-x load-file RET supertag-migration.el RET`
3. `M-x supertag-migrate-database-to-new-arch RET`
4. Restart Emacs

## 🔄 Property to Field Migration

**Convert Org Properties to Structured Fields**

Org-SuperTag 5.3 introduces a powerful migration system to convert your existing Org `:PROPERTIES:` drawers into structured database fields. This enables better querying, automation, and data management.

### Two Migration Paths

#### Path 1: Already Using Org-roam (Headings have `:ID:` properties)

If your Org files already have `:ID:` properties on headings:

1. **Add IDs to all headings** (if missing):
   ```elisp
   M-x supertag-migration-add-ids-to-org-headings
   ```
   Select your Org directory to add `:ID:` properties to all headings.

2. **Sync to database**:
   ```elisp
   M-x supertag-sync-full-rescan
   ```
   This imports all nodes with their properties into the database.

3. **Convert properties to fields**:
   ```elisp
   M-x supertag-convert-properties-to-field
   ```
   Choose a property (e.g., "LOCATION"), select/create a tag, and convert.

#### Path 2: Not Using Org-roam (Headings lack `:ID:` properties)

If your Org files don't have `:ID:` properties:

1. **Add IDs to all headings**:
   ```elisp
   M-x supertag-migration-add-ids-to-org-headings
   ```
   This adds `:ID:` properties to all headings.

2. **Sync to database**:
   ```elisp
   M-x supertag-sync-full-rescan
   ```

3. **Convert properties to fields**:
   ```elisp
   M-x supertag-convert-properties-to-field
   ```

### What Happens During Conversion

When you convert a property like `:LOCATION: London`:

1. **Database updates**:
   - Node gets the specified tag (e.g., `#place`)
   - Field value is stored in database (`LOCATION = "London"`)

2. **Org file updates**:
   - Tag is inserted in headline: `* My Heading #place`
   - Properties drawer remains unchanged for compatibility

3. **Query capabilities**:
   - Find by field: `(field "LOCATION" "London")`
   - Find by tag: `(tag "place")`
   - Combined queries work seamlessly

### Batch Conversion

Convert multiple properties at once:

```elisp
M-x supertag-batch-convert-properties-to-fields
```

This shows statistics for each property and lets you choose tags for each conversion.

### Before/After Example

**Before**:
```org
* Project Meeting
:PROPERTIES:
:LOCATION: Conference Room A
:ATTENDEES: Alice, Bob, Carol
:END:
```

**After** (in database):
- Node has tag: `meeting`
- Fields: `LOCATION = "Conference Room A"`, `ATTENDEES = "Alice, Bob, Carol"`

**Org file**:
```org
* Project Meeting #meeting
:PROPERTIES:
:LOCATION: Conference Room A
:ATTENDEES: Alice, Bob, Carol
:END:
```

### Benefits

- **Better queries**: Search by field values, not just text
- **Automation**: Trigger rules based on field changes
- **Data integrity**: Structured data with validation
- **Views**: Table views show field columns automatically
- **Backward compatibility**: Original properties remain intact

## ⚙️ Configuration

Minimal config:

```emacs-lisp
(setq org-supertag-sync-directories '("~/org/"))
```

With AI features (optional):

```emacs-lisp
(setq org-supertag-bridge-enable-ai t)  ; Uses gptel
```

## 🆚 Why Not Use...

| Tool     | Org-SuperTag Advantage    |
| -------- | ------------------------- |
| Org-roam | Structured data + queries |
| Obsidian | Native Emacs integration  |
| Notion   | Offline + programmable    |

## 🐛 Troubleshooting

**Database corrupted?**

```lisp
M-x supertag-sync-cleanup-database
```

**Want to see what's in your database?**

```lisp
M-x supertag-search
```

**Need to debug sync issues?**

```lisp
M-x supertag-sync-analyze-file
```

## 📊 Technical Details

- **Lines of code**: ~16K (down from ~30K)
- **Dependencies**: Just Emacs
- **Data storage**: ~/.emacs.d/org-supertag/
- **Backup**: Automatic daily snapshots

---

_Made for people who want Notion's structure with Org-mode's soul._

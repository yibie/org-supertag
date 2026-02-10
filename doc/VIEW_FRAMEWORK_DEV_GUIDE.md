# View Framework Developer Guide

## Overview

`supertag-view-framework.el` provides a framework for Elisp developers to create custom data visualizations for org-supertag.

**Philosophy**: Code-first, maximum flexibility. No GUI configurators.

---

## Quick Start

### 1. Define a Simple View

```elisp
(require 'supertag-view-framework)

(define-supertag-view my-simple-view "My Simple View"
  "A description of what this view shows."
  (tag nodes)
  (supertag-view--with-buffer "Simple" tag
    (supertag-view--header (format "View for #%s" tag))
    (insert (format "Total nodes: %d\n\n" (length nodes)))
    (dolist (node nodes)
      (insert (format "- %s\n" (plist-get node :title))))))
```

### 2. Test Your View

```elisp
;; Render with mock data
(supertag-view-render 'my-simple-view
  (list :tag "project"
        :nodes (list
                (list :title "Project A" :id "abc-123")
                (list :title "Project B" :id "def-456"))))
```

### 3. Use the View Interactively

```
M-x supertag-view-schema
Navigate to #project
Press v v
Select "My Simple View"
```

Or programmatically:
```elisp
(supertag-view-select-and-render "project")
```

---

## Core API Reference

### `define-supertag-view` Macro

**Syntax**:
```elisp
(define-supertag-view ID NAME [DOCSTRING] ARGLIST &rest BODY)
```

**Parameters**:
- `ID` - Symbol identifier (e.g., `my-view`)
- `NAME` - Display name string (e.g., `"My View"`)
- `DOCSTRING` - Optional description
- `ARGLIST` - Must be `(TAG NODES)` - these are extracted from context
- `BODY` - Your rendering code

**What it does**:
1. Creates a render function: `supertag-view--render-ID`
2. Registers the view with `supertag-view-register`
3. Sets up proper metadata

### Low-Level Registration

If you need more control, use `supertag-view-register` directly:

```elisp
(supertag-view-register
 :id 'my-custom-view
 :name "My Custom View"
 :description "Optional description"
 :category :project-management
 :render-fn #'my-render-function
 :valid-for '("project" "task"))  ; nil means all tags
```

### View Lookup & Rendering

```elisp
;; Get a view definition
(supertag-view-get 'my-view)  ; Returns plist with :id, :name, :render-fn, etc.

;; List all views
(supertag-view-list)  ; Returns sorted list of view plists

;; List views for a specific tag
(supertag-view-list-for-tag "project")

;; Render a view programmatically
(supertag-view-render 'my-view
                      (list :tag "project"
                            :nodes node-list
                            :virtual-columns vc-list))
```

---

## Rendering Utilities

### Buffer Management

```elisp
(supertag-view--with-buffer "Buffer Name" tag
  ;; All code here runs in a dedicated buffer
  ;; Buffer is automatically named: "*View: Buffer Name - tag*"
  ;; Made read-only after body executes
  (insert "Your content here\n"))
```

### Headers and Structure

```elisp
(supertag-view--header "Main Title")        ; Underlined with ===
(supertag-view--subheader "Section")        ; Underlined with ---
(supertag-view--separator)                  ; Line of dashes across window
(supertag-view--separator ?=)               ; Line of equals
```

### Data Display

```elisp
;; Progress bar
(supertag-view--progress-bar 75)            ; [███████░░░] 75%
(supertag-view--progress-bar 50 10)         ; [█████░░░░░] 50% (width 10)

;; Statistics row
(supertag-view--stat-row
 '(("Total" . 10)
   ("Completed" . 7)
   ("Pending" . 3)))
```

### Data Access

```elisp
;; Get virtual column value for a node
(supertag-view--get-vc node-id "progress-percent" default-value)

;; Get global field value
(supertag-view--get-global-field node-id "priority" default-value)
```

---

## Widget System

Widgets are reusable UI components for declarative view construction.

### Built-in Widgets

```elisp
(supertag-widget-render 'header '(:text "Dashboard"))
(supertag-widget-render 'subheader '(:text "Overview"))
(supertag-widget-render 'text '(:content "Description here"))
(supertag-widget-render 'progress-bar '(:value 75 :max 100 :width 20))
(supertag-widget-render 'stats-row '(:stats (("A" . 1) ("B" . 2))))
(supertag-widget-render 'separator '(:char ?-))
(supertag-widget-render 'list '(:items ("Item 1" "Item 2")))
(supertag-widget-render 'table '(:headers ("Name" "Value") :rows (("A" "1")) :widths (10 10)))
(supertag-widget-render 'card '(:title "Card Title" :children (...)))
(supertag-widget-render 'field '(:items (("Owner" . "Alice") ("Priority" . "High"))))
(supertag-widget-render 'badge '(:text "High"))
(supertag-widget-render 'badge '(:items ("High" "Medium" "Low")))
(supertag-widget-render 'empty '(:title "No items" :message "Add a tag to begin."))
(supertag-widget-render 'toolbar '(:items ("g refresh" "q quit")))
```

### Container Widgets (DSL v2)

```elisp
(supertag-widget-render 'section '(:title "Summary" :children (...)))
(supertag-widget-render 'stack '(:children (...) :spacing 1))
(supertag-widget-render 'columns
  '(:columns ((:width 24 :children (...))
              (:width 24 :children (...)))))
(supertag-widget-render 'card '(:title "Card" :children (...)))
(supertag-widget-render 'toolbar '(:items (("Refresh" . "g") ("Quit" . "q"))))
```

### Custom Widgets

```elisp
;; Register a custom widget
(supertag-widget-register 'my-widget
  (lambda (props &optional context)
    (let ((text (plist-get props :text)))
      (insert (format ">>> %s <<<\n" text)))))

;; Use it
(supertag-widget-render 'my-widget '(:text "Hello"))
```

---

## Declarative DSL (Primary)

Use the declarative configuration as the primary entry for user-defined views.
Widget `:type` accepts either a symbol (`header`) or keyword (`:header`); keywords are normalized at render time.
Widget props can be functions (`lambda (ctx) ...`) for data binding.
Container widgets use `:children` for nesting.

```elisp
(supertag-view-define-from-config
 (list :id 'mini-view
       :name "Mini View"
       :tag "project"
       :widgets
       (list
        (list :type :section :title "Summary"
              :children
              (list (list :type :stats-row
                          :stats (lambda (ctx)
                                   (list (cons "Total" (length (plist-get ctx :nodes))))))))
        (list :type :stack
              :children
              (list (list :type :text :content "Notes")
                    (list :type :progress-bar
                          :value (lambda (ctx)
                                   (or (plist-get (car (plist-get ctx :nodes)) :progress) 0)))))))))
```

---

## Complete Examples

### Example 1: Node Counter

```elisp
(define-supertag-view node-counter "Node Counter"
  "Simple statistics view."
  (tag nodes)
  (supertag-view--with-buffer "Counter" tag
    (supertag-view--header (format "Statistics for #%s" tag))
    (supertag-view--stat-row
     `(("Total Nodes" . ,(length nodes))
       ("First Node" . ,(or (plist-get (car nodes) :title) "N/A"))))
    (supertag-view--separator)
    (dolist (node nodes)
      (insert (format "  • %s\n" (plist-get node :title))))))
```

### Example 2: Progress Dashboard

```elisp
(define-supertag-view progress-dashboard "Progress Dashboard"
  "Show progress for all projects."
  (tag nodes)
  (supertag-view--with-buffer "Progress" tag
    (supertag-view--header "Project Progress")
    (dolist (node nodes)
      (let* ((id (plist-get node :id))
             (title (plist-get node :title))
             (progress (supertag-view--get-vc id "progress-percent" 0)))
        (insert (format "%s\n" title))
        (supertag-view--progress-bar progress 15)
        (insert "\n")))))
```

### Example 3: Field Inspector

```elisp
(define-supertag-view field-inspector "Field Inspector"
  "Inspect all fields of nodes."
  (tag nodes)
  (supertag-view--with-buffer "Fields" tag
    (supertag-view--header (format "Field Values for #%s" tag))
    (dolist (node nodes)
      (let ((title (plist-get node :title))
            (id (plist-get node :id)))
        (supertag-view--subheader title)
        ;; Show some fields
        (let ((priority (supertag-view--get-global-field id "priority" "unset"))
              (status (supertag-view--get-global-field id "status" "unknown")))
          (supertag-view--stat-row
           `(("Priority" . ,priority)
             ("Status" . ,status))))
        (insert "\n")))))
```

---

## Configuration Persistence

### Save View Configurations

```elisp
;; Export all view configs as Elisp code
(supertag-view-config-export-all-elisp)

;; Save to file
(supertag-view-config-save-to-file "~/my-views.el")

;; Load from file
(supertag-view-config-load-from-file "~/my-views.el")
```

### Single View Export

```elisp
(supertag-view-config-export-elisp 'my-view)
```

---

## Interactive Commands

| Command | Description |
|---------|-------------|
| `M-x supertag-view-list-interactive` | Show all registered views |
| `M-x supertag-view-select-and-render` | Pick a view and render it |
| `M-x supertag-view-select-from-schema` | Select view from schema context |
| `M-x supertag-view-dsl-example` | Demo the DSL (creates example view) |
| `M-x supertag-view-refresh` | Refresh current view buffer |
| `M-x supertag-view-framework-init` | Reset all views (clear registry) |

---

## Minimal Verification

1. Evaluate the DSL example and render it with `M-x supertag-view-dsl-example`.
2. In the view buffer, change underlying data (tags/fields/nodes) and re-run `M-x supertag-view-refresh`.
3. Confirm the view re-renders and reflects updated data.

---

## Best Practices

### 1. Naming Conventions

```elisp
;; View IDs: descriptive with hyphens
my-project-dashboard
my-task-matrix

;; Render functions (auto-generated by macro)
supertag-view--render-my-view

;; Your helper functions
my-view--format-node
my-view--calculate-stats
```

### 2. Error Handling

```elisp
(define-supertag-view safe-view "Safe View"
  "View with error handling."
  (tag nodes)
  (condition-case err
      (supertag-view--with-buffer "Safe" tag
        ;; Your rendering logic
        (supertag-view--header "Safe View")
        ;; ...
        )
    (error
     (message "View error: %s" (error-message-string err))
     (with-current-buffer (get-buffer-create "*View Error*")
       (erase-buffer)
       (insert "View Error\n==========\n\n")
       (insert (format "Error: %s\n" (error-message-string err)))
       (special-mode))
     (pop-to-buffer "*View Error*"))))
```

### 3. Testing Views

```elisp
(defun my-view-demo ()
  "Test the view with mock data."
  (interactive)
  (supertag-view-render 'my-view
    (list :tag "demo"
          :nodes (list
                  (list :title "Node A" :id "id-1")
                  (list :title "Node B" :id "id-2")))))

;; M-x my-view-demo
```

---

## File Locations

- **Framework**: `supertag-view-framework.el`
- **Built-in views**: `supertag-view-progress-dashboard.el`
- **Built-in views**: `supertag-view-priority-matrix.el`
- **Effort distribution**: `supertag-view-effort-distribution.el`

---

## See Also

- `doc/VIRTUAL_COLUMNS.md` - Virtual column system documentation
- `doc/VIRTUAL_COLUMNS_QUICKSTART.md` - Quick start for virtual columns
- `supertag-view-table.el` - Table view implementation example
- `supertag-view-node.el` - Node view implementation example

---

**Document Version**: 2026-02-02  
**API Version**: supertag-view-framework.el (org-supertag 5.6+)

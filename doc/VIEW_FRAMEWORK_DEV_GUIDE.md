# View Framework Developer Guide

## Overview

`supertag-view-framework.el` gives every view one lifecycle: register a definition, open it through the Runtime, and refresh the same View Instance. The Runtime owns buffer creation, major-mode installation, display, refresh ordering, subscriptions, cleanup, and selection restoration. A view adapter owns only state building and rendering.

## Minimal Runtime View

```elisp
(require 'supertag-view-framework)
(require 'supertag-view-api)

(defun my-project-view--state (input)
  "Build project view state from INPUT."
  (let* ((tag (plist-get input :tag))
         (node-ids (supertag-view-api-nodes-by-tag tag)))
    (list :tag tag
          :nodes (supertag-view-api-get-entities :nodes node-ids))))

(defun my-project-view--render (state)
  "Render project view STATE in the current buffer."
  (erase-buffer)
  (supertag-view--header
   (format "Projects tagged #%s" (plist-get state :tag)))
  (dolist (node (plist-get state :nodes))
    (insert (format "- %s\n" (or (plist-get node :title) "Untitled"))))
  (goto-char (point-min)))

(supertag-view-register
 :id 'my-project-view
 :name "My Project View"
 :description "List projects in a read-only buffer"
 :valid-for '("project")
 :buffer-name-fn
 (lambda (input)
   (format "*View: Projects - %s*" (plist-get input :tag)))
 :mode-fn #'special-mode
 :state-fn #'my-project-view--state
 :render-fn #'my-project-view--render
 :display-action '(display-buffer-pop-up-window))
```

Open and refresh it through the public Runtime entry points:

```elisp
(supertag-view-open 'my-project-view '(:tag "project"))
(supertag-view-refresh)
```

Registered, selectable views also appear in `M-x supertag-view-select-and-render` and the Schema View view picker.

## Definition Keys

| Key | Purpose |
| --- | --- |
| `:id` | Stable symbol identifier; required |
| `:name` | User-facing name; required |
| `:render-fn` | Render state into the current buffer; required |
| `:state-fn` | Build refreshable state from the original input |
| `:buffer-name` / `:buffer-name-fn` | Runtime buffer identity |
| `:mode-fn` | Major-mode installer; defaults to `special-mode` |
| `:display-action` | Native `display-buffer` action |
| `:valid-for` | Tags for which the picker offers the view; nil means all |
| `:selectable` | Set to nil for internal adapters |
| `:subscribe-fn` | Install listeners and return cleanup callbacks |
| `:capture-selection-fn` | Capture an opaque selection before refresh |
| `:restore-selection-fn` | Restore that selection after rendering |

The renderer modifies only the current buffer. It must not create or display another buffer, subscribe to events, or write to the Store. The state function may read through `supertag-view-api.el`, but must not mutate buffers or Store data.

## Refresh, Subscription, and Selection

`supertag-view-refresh` always performs capture → state build → render → restore. The Runtime reuses the original input stored in the buffer-local View Instance.

A subscription function receives `INPUT`, initial `STATE`, and a `REFRESH` callback. Return one cleanup function or a list of cleanup functions. The Runtime calls all of them when the view is reopened or killed.

```elisp
:subscribe-fn
(lambda (_input _state refresh)
  (let ((unsubscribe (my-events-subscribe refresh)))
    (lambda () (funcall unsubscribe))))
```

Selection hooks should exchange stable identities, not displayed text. Use text properties such as `supertag-entity-id` when a renderer needs to locate an entity after refresh.

## Rendering Helpers

The framework provides small insertion helpers for code-first views:

```elisp
(supertag-view--header "Main title")
(supertag-view--subheader "Section")
(supertag-view--separator)
(supertag-view--progress-bar 75 20)
(supertag-view--stat-row '(("Total" . 10) ("Done" . 7)))
```

## Declarative Widget DSL

For views that fit the built-in widgets, register a configuration instead of writing a renderer:

```elisp
(supertag-view-define-from-config
 (list :id 'project-summary
       :name "Project Summary"
       :tag "project"
       :widgets
       (list
        (list :type :section
              :title "Overview"
              :children
              (list
               (list :type :stats-row
                     :stats
                     (lambda (context)
                       (list (cons "Total"
                                   (length (plist-get context :nodes))))))))
        (list :type :list
              :items '("Review" "Plan" "Ship")))))
```

Built-in widget types include `header`, `subheader`, `text`, `progress-bar`, `stats-row`, `separator`, `list`, `table`, `section`, `stack`, `columns`, `card`/`panel`, `field`/`kv`, `badge`, `empty`, `toolbar`, `button`, `link`, and `editable-field`. `button` and `link` require a zero-argument `:action`; `editable-field` requires a string `:value`, a positive display-column `:width`, and may provide a one-argument `:on-change` callback. An initial value wider than the declared field width is rejected instead of silently breaking its container layout.

`:widgets` may also be a function of the current context that returns a fresh widget tree. Add a stable `:key` to repeated or interactive nodes when selection must survive a full refresh. The DSL renderer preserves that logical key and the point offset within its range; if the key disappears, point falls back to the start of the view. Function-valued properties are context bindings except for the literal `:key`, `:action`, and `:on-change` properties.

Buttons use Emacs `button.el`; only editable fields use `widget.el`. Interactive leaves inside `columns` and `card` are materialized after layout in the final buffer, so callbacks and field state are not copied from a temporary buffer. `TAB` and `S-TAB` move across both buttons and fields. Set `:persist nil` for built-in or programmatic views that should not appear in exported developer configuration.

The DSL uses the same Runtime; open it with `supertag-view-open` or the view picker, and refresh it with `supertag-view-refresh`.

## Testing a View

Exercise the public lifecycle rather than calling the renderer directly:

```elisp
(ert-deftest my-project-view-opens-through-runtime ()
  (cl-letf (((symbol-function 'display-buffer) #'ignore))
    (let ((buffer (supertag-view-open 'my-project-view '(:tag "project"))))
      (unwind-protect
          (with-current-buffer buffer
            (should (eq (plist-get supertag-view--instance :view-id)
                        'my-project-view))
            (should (derived-mode-p 'special-mode)))
        (kill-buffer buffer)))))
```

At minimum, verify the buffer name, mode, visible content, manual refresh, cleanup, and any selection identity promised by the adapter.

For a complete in-memory showcase, add `doc/examples/` to `load-path`, require
`supertag-view-demo-dashboard`, then run `M-x supertag-view-demo-dashboard-open`.
The Dashboard renders every registered Widget type; its button, link, and editable
field update demo-only memory and never read or write user data.

## Interactive Commands

| Command | Description |
| --- | --- |
| `M-x supertag-view-list-interactive` | List registered views |
| `M-x supertag-view-select-and-render` | Select a view for a tag |
| `M-x supertag-view-select-from-schema` | Select a view from Schema context |
| `M-x supertag-view-refresh` | Refresh the current Runtime view |
| `M-x supertag-view-dsl-example` | Register the DSL example |

## Implementations to Read

- `supertag-ui-search.el` — fixed result buffer and origin restoration
- `supertag-view-table.el` — editable table state and selection
- `supertag-view-kanban.el` — grouped cards and Store subscription
- `supertag-view-node.el` — side-window display and follow lifecycle
- `supertag-view-progress-dashboard.el` — minimal read-only adapter

**Document Version**: 2026-08-05
**Runtime Baseline**: Emacs 29.1+

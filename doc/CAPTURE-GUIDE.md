# Org-Supertag Capture System

Note: Legacy Org `:tag:` remain readable during sync/import. New capture writes inline `#tag` as before.

## 🚀 What is the Capture System?

The Org-Supertag Capture System provides a powerful and flexible node creation mechanism, supporting dynamic templates, content generators, and automatic field filling. This system follows the data separation principle, storing node content and extended properties separately in Org files and the database.

### 🎯 Core Concepts

In traditional Org-mode, creating nodes requires manually entering titles, tags, and properties. In the Org-Supertag Capture System:

- 🔄 **Template-Driven** - Quickly create structured nodes using predefined templates
- 🧠 **Smart Filling** - Automatically get content from clipboard, selection, or functions
- 🏷️ **Smart Tagging** - Interactive tag selection and auto-completion
- 📝 **Field Enrichment** - Automatically set tag field values

### ⚡ Quick Experience

```org
;; Quickly create a task node using a template
M-x supertag-capture-with-template RET t RET

;; Automatically generated result:
* Fix login page bug #task
  :PROPERTIES:
  :ID: a1b2c3d4-e5f6-7890-g1h2-i3j4k5l6m7n8
  :END:

  Creation date: 2025-09-06
  Status: To Start
```

## 📖 User Guide

### Core Workflows at a Glance

- **Org-capture based capture** (`org-capture` + Supertag): reuse `org-capture-templates`, Supertag handles ID, database, fields, moves, and tags.
- **Template capture** (`supertag-capture-with-template`): advanced DSL with dynamic generators and field specs.
- **Independent capture** (`supertag-capture`): quick, one-off node creation.

The core engine underneath all of these is a single finalization API that
turns an Org heading into a Supertag node and applies tag fields.

### Org-Capture-Based Capture (`org-capture` + Supertag)

This is the most natural workflow if you already use `org-capture`:

1. Enable Supertag integration:
   ```elisp
   (setq supertag-org-capture-auto-enable t)
   ;; or:
   ;; (supertag-enable-org-capture-integration)
   ```
2. Add `:supertag` (and optional extras) to your capture templates:
   ```elisp
(add-to-list 'org-capture-templates
             '("t" "Task with Supertag" entry
               (file+headline "~/org/tasks.org" "Inbox")
               "* TODO %^{Task}\n  %?\n"
               :supertag t
               :supertag-tags-prompt t
               :supertag-template ((:tag "task" :field "status" :value "todo"))
               :supertag-move 'link))  ;; move and leave a link
   ```

Flow:

- `org-capture` inserts the heading as usual.
- Supertag finalizes the node: ensures `ID`, syncs to the database, writes fields.
- If `:supertag-tags-prompt t` is set, you get a Supertag-aware tag prompt:
  - choose existing tags with completion, or type new tags (auto-created).
- If `:supertag-move` is set, Supertag can:
  - `:supertag-move t` / `node` → run `supertag-move-node` (choose file + position)
  - `:supertag-move link` / `:link` → run `supertag-move-node-and-link`
  - `:supertag-move within-target` / `:within-target` → only choose position **within the capture target file**, skipping the file prompt

### Two Capture Methods

#### 1. Independent Capture (`supertag-capture`)

The simplest node creation method, suitable for temporary or one-time node creation.

```
M-x supertag-capture
```

**Operation Flow**:

1. Enter node title
2. Select tags (optional, supports multiple selection)
3. Select target file and insertion position
4. System creates Org node with ID
5. Optionally enrich field values

**Use Cases**:

- Temporary idea recording
- Simple task creation
- Quick content capture

#### 2. Template-Driven Capture (`supertag-capture-with-template`)

Quickly create structured nodes using predefined templates, suitable for repetitive workflows.

```
M-x supertag-capture-with-template
```

**Operation Flow**:

1. Select a configured template
2. Provide content according to template specifications
3. If template has `:file`, node is created directly at that location
4. If template has no `:file`, user selects target file and insertion position
5. System automatically generates complete node
6. Optionally enrich with additional fields

**Use Cases**:

- Standardized document creation
- Repetitive task recording
- Structured information collection

## 🎨 Template System

### Template Configuration Structure

```elisp
(setq supertag-capture-templates
      '((template-key "template-description"
         :file "target-file-path"  ; Optional: if omitted, user selects target file
         :node-spec
         ((node-specification-list...)))))
```

### Node Specification Details

Each node specification item contains two core parts:

| Parameter | Type   | Required | Description                                            |
| --------- | ------ | -------- | ------------------------------------------------------ |
| `:part`   | Symbol | Yes      | Node part type (`:title`, `:tags`, `:body`, `:fields`) |
| `:get`    | List   | Yes      | Content generation specification                       |

### Content Generators

Content generators determine how to fill content for different parts of the node.

#### Static Value Generator `:static`

Directly use specified values without user input.

```elisp
;; Example: Fixed tags
(:part :tags :get (:static ("work" "important")))

;; Example: Fixed field values
(:part :fields :get (:static (((:tag "project" :field "status" :value "in-progress")))))
```

#### Interactive Prompt Generator `:prompt`

Prompt user to enter content, with optional default values.

```elisp
;; Example: Enter title
(:part :title :get (:prompt "Task title: "))

;; Example: Enter tags (with default value)
(:part :tags :get (:prompt "Tags: " :initial-input "task,"))

;; Example: Enter field values
(:part :fields :get (:static (((:tag "task" :field "priority" :get (:prompt "Priority: "))))))
```

#### Clipboard Content Generator `:clipboard`

Use current clipboard content as node body.

```elisp
(:part :body :get (:clipboard))
```

#### Region Content Generator `:region`

Use currently selected text as content. Errors if no selection.

```elisp
(:part :body :get (:region))
```

#### Region or Clipboard Generator `:region-or-clipboard`

Prioritize selection, use clipboard if no selection.

```elisp
(:part :body :get (:region-or-clipboard))
```

#### Template String Generator `:template-string`

Generate content using template strings with placeholder replacement.

```elisp
(:part :body :get (:template-string "Creation date: %date\nContent: %clipboard\nStatus: To Process"))
```

**Supported Placeholders**:

##### ⏰ Time-Related Placeholders

| Placeholder  | Description         | Example Output      |
| ------------ | ------------------- | ------------------- |
| `%date`      | Current date        | 2025-09-06          |
| `%time`      | Current time        | 14:30               |
| `%datetime`  | Date and time       | 2025-09-06 14:30    |
| `%timestamp` | Full timestamp      | 2025-09-06 14:30:45 |
| `%week`      | Current week number | W36                 |
| `%month`     | Current month       | September           |
| `%year`      | Current year        | 2025                |

##### 👤 User Information Placeholders

| Placeholder | Description     | Example Output |
| ----------- | --------------- | -------------- |
| `%user`     | User login name | chenyibin      |
| `%fullname` | User full name  | Chen Yibin     |
| `%hostname` | System hostname | MacBook-Pro    |

##### 📁 File Context Placeholders

| Placeholder  | Description              | Example Output              |
| ------------ | ------------------------ | --------------------------- |
| `%filename`  | Current buffer filename  | project.org                 |
| `%filepath`  | Current buffer full path | /Users/user/org/project.org |
| `%directory` | Current buffer directory | /Users/user/org/            |

##### 🔗 Node Context Placeholders

| Placeholder           | Description           | Example Output            |
| --------------------- | --------------------- | ------------------------- |
| `%current-node-title` | Current node title    | Project Management System |
| `%current-node-id`    | Current node ID       | a1b2c3d4-e5f6-7890        |
| `%current-tags`       | Current node tag list | project, important        |

##### 🛠️ Utility Placeholders

| Placeholder  | Description           | Example Output      |
| ------------ | --------------------- | ------------------- |
| `%clipboard` | Clipboard content     | [Text in clipboard] |
| `%random`    | Random 4-digit number | 1234                |
| `%uuid`      | Newly generated UUID  | f47ac10b-58cc-4372  |

Example usage:

```elisp
;; Create template with multiple information
(:part :body :get (:template-string "Report date: %date\nReporter: %fullname\nHostname: %hostname\nContent:\n%clipboard"))
```

#### Custom Function Generator `:function`

Call custom functions to generate content.

```elisp
(:part :body :get (:function my-custom-content-generator))
```

### Field Setting Specification

Field specifications are used to automatically set tag field values for nodes.

```elisp
(:part :fields :get (:static (((:tag "project" :field "status" :value "in-progress")
                               (:tag "project" :field "priority" :value "high")))))
```

Field specification format:

- `:tag` - Tag ID
- `:field` - Field name
- `:value` - Field value (static)
- `:get` - Field value generator (dynamic)

## 📋 Use Cases and Template Examples

### Daily Task Creation

```elisp
;; Template configuration
("t" "Quick Task"
 :file "~/org/tasks.org"
 :node-spec
 ((:part :title :get (:prompt "Task: "))
  (:part :tags  :get (:static ("task")))
  (:part :body  :get (:template-string "Created: %date\nStatus: To Start\n"))))
```

Usage:

1. `M-x supertag-capture-with-template RET t RET`
2. Enter task name: "Fix login page bug"
3. System automatically generates structured task node

### Learning Note Capture

```elisp
;; Template configuration
("l" "Learning Notes"
 :file "~/org/learning.org"
 :node-spec
 ((:part :title :get (:prompt "Learning Topic: "))
  (:part :tags  :get (:prompt "Tags: " :initial-input "learning,"))
  (:part :body  :get (:region-or-clipboard))
  (:part :fields :get (:static (((:tag "learning" :field "difficulty" :get (:prompt "Difficulty (1-5): "))
                                 (:tag "learning" :field "source" :get (:prompt "Learning Source: "))))))))
```

Usage:

1. Copy learning material to clipboard
2. `M-x supertag-capture-with-template RET l RET`
3. Enter learning topic and related information
4. System automatically generates learning note node

### Meeting Record Template

```elisp
;; Template configuration
("m" "Meeting Record"
 :file "~/org/meetings.org"
 :node-spec
 ((:part :title :get (:prompt "Meeting Topic: "))
  (:part :tags  :get (:static ("meeting")))
  (:part :body  :get (:template-string "Time: %date\nParticipants: \n\nAgenda:\n\nDiscussion Points:\n\nAction Items:\n"))
  (:part :fields :get (:static (((:tag "meeting" :field "type" :get (:prompt "Meeting Type: "))
                                 (:tag "meeting" :field "status" :value "completed")))))))
```

### Project Planning Template

```elisp
("p" "Project Planning"
 :file "~/org/projects.org"
 :node-spec
 ((:part :title :get (:prompt "Project Name: "))
  (:part :tags  :get (:static ("project" "planning")))
  (:part :body  :get (:template-string "Start Date: %date\n\nGoals:\n\nMilestones:\n\nResource Requirements:\n"))
  (:part :fields :get (:static (((:tag "project" :field "status" :value "planning")
                                 (:tag "project" :field "owner" :get (:prompt "Owner: "))))))))
```

### Flexible File Selection Templates

Templates can work without specifying a target file, allowing users to choose the destination dynamically:

```elisp
;; Template without fixed file - user selects destination
("f" "Flexible Task"
 :node-spec
 ((:part :title :get (:prompt "Task: "))
  (:part :tags  :get (:static ("task")))
  (:part :body  :get (:template-string "Created: %date\nStatus: To Start\n"))))
```

Usage:

1. `M-x supertag-capture-with-template RET f RET`
2. Enter task name: "Review project proposal"
3. Select target file from file browser
4. Choose insertion position (file top, file end, under heading, or after heading)
5. System creates node at selected location

This approach is ideal for:

- Ad-hoc task creation
- Flexible content organization
- Multi-project workflows
- Dynamic file selection based on context

## ⚙️ Advanced Features

### Automatic Field Filling

Templates can automatically set field values for nodes, supporting both static values and dynamic generation:

```elisp
(:part :fields :get (:static (((:tag "project" :field "status" :value "in-progress")
                               (:tag "project" :field "creator" :get (:function user-full-name))
                               (:tag "project" :field "due-date" :get (:prompt "Due Date: "))))))
```

### Interactive Field Enrichment

After capture is complete, users can continue to add field values:

1. System prompts whether to enrich the node
2. User selects tags on the node
3. Select available fields for that tag
4. Enter field values
5. Repeat until complete

### Smart Position Selection

When templates don't specify a target file, the system provides flexible position selection:

1. **File Selection**: Choose any existing Org file from the file browser
2. **Position Options**:
   - **File Top**: Insert at the beginning of the file
   - **File End**: Insert at the end of the file
   - **Under Heading**: Insert as a sub-item of the selected heading
   - **After Heading**: Insert at the same level as the selected heading

This gives users complete control over where their captured content is placed, making the system highly flexible for different organizational needs.

## 🔧 Configuration and Extension

### Basic Configuration

```elisp
;; Set capture templates
(setq supertag-capture-templates
      '(
        ;; Quick task
        ("t" "Quick Task"
         :file "~/org/tasks.org"
         :node-spec
         ((:part :title :get (:prompt "Task: "))
          (:part :tags  :get (:static ("task")))
          (:part :body  :get (:template-string "Created: %date\nStatus: To Start\n"))))

        ;; Learning notes
        ("l" "Learning Notes"
         :file "~/org/learning.org"
         :node-spec
         ((:part :title :get (:prompt "Learning Topic: "))
          (:part :tags  :get (:prompt "Tags: " :initial-input "learning,"))
          (:part :body  :get (:region-or-clipboard))
          (:part :fields :get (:static (((:tag "learning" :field "difficulty" :get (:prompt "Difficulty (1-5): "))
                                         (:tag "learning" :field "source" :get (:prompt "Learning Source: "))))))))

        ;; Meeting record
        ("m" "Meeting Record"
         :file "~/org/meetings.org"
         :node-spec
         ((:part :title :get (:prompt "Meeting Topic: "))
          (:part :tags  :get (:static ("meeting")))
          (:part :body  :get (:template-string "Time: %date\nParticipants: \n\nAgenda:\n\nDiscussion Points:\n\nAction Items:\n"))
          (:part :fields :get (:static (((:tag "meeting" :field "type" :get (:prompt "Meeting Type: "))
                                         (:tag "meeting" :field "status" :value "completed")))))))

        ;; Flexible task (no fixed file)
        ("f" "Flexible Task"
         :node-spec
         ((:part :title :get (:prompt "Task: "))
          (:part :tags  :get (:static ("task")))
          (:part :body  :get (:template-string "Created: %date\nStatus: To Start\n"))))))
```

### Custom Content Generators

Create your own content generators to meet specific needs:

```elisp
(defun my-custom-generator ()
  "Custom content generator example"
  (format "Project Number: PRJ-%d\nCreator: %s\n"
          (random 10000)
          (user-full-name)))

;; Use in template:
(:part :body :get (:function my-custom-generator))
```

### Extending Template Variables

Template string processors can be extended to support more placeholders:

```elisp
;; Add %time placeholder support
(setq template (replace-regexp-in-string "%time" (format-time-string "%H:%M") template t t))
```

## 📚 API Reference

### User Commands

| Command                          | Description                       | Usage                                          |
| -------------------------------- | --------------------------------- | ---------------------------------------------- |
| `supertag-capture`               | Independent capture command       | `M-x supertag-capture`                         |
| `supertag-capture-with-template` | Template-based capture command    | `M-x supertag-capture-with-template`           |
| `supertag-capture-enrich-node`   | Interactive node field enrichment | `M-x supertag-capture-enrich-node RET node-id` |

### Org-Capture Integration (Optional)

Org-Supertag can work as an extension layer on top of `org-capture`,
reusing your existing capture templates and only adding Supertag-specific
metadata and field management.

#### Enable Integration

```elisp
;; Enable org-capture integration (global toggle)
(setq supertag-org-capture-auto-enable t)
;; or call interactively:
;; M-x supertag-enable-org-capture-integration
```

This adds a post-processing hook to `org-capture-after-finalize-hook`
and only activates for templates that explicitly opt in.

#### Opt-in from org-capture-templates

Add `:supertag t` to any template that should be synchronized with
the Supertag store. Optional `:supertag-template` can pre-fill tag fields
on the created node:

```elisp
(add-to-list 'org-capture-templates
             '("t" "Task with Supertag" entry
               (file+headline "~/org/tasks.org" "Inbox")
               "* TODO %^{Task}  #task\n  %?\n"
               :supertag t
               :supertag-template ((:tag "task" :field "status" :value "todo"))))
```

When this template finishes, Supertag will:

- Ensure the captured node has a stable `ID`
- Sync it into the Supertag database
- Apply `:supertag-template` via the Tag/Field/Value model

#### Combine org-capture with `supertag-move-node`

You can chain capture → Supertag sync → move using the existing
`supertag-move-node` / `supertag-move-node-and-link` commands.

```elisp
(add-to-list 'org-capture-templates
             '("m" "Task with Supertag & move"
               entry
               (file "~/org/inbox.org")
               "* TODO %^{Task}  #task\n  %?\n"
               :supertag t
               :supertag-move t))   ;; use supertag-move-node

(add-to-list 'org-capture-templates
             '("l" "Task with Supertag & move+link"
               entry
               (file "~/org/inbox.org")
               "* %^{Title}  #task\n  %?\n"
               :supertag t
               :supertag-move 'link))   ;; use supertag-move-node-and-link
```

- `:supertag-move t` or `:supertag-move 'node`  
  → after capture, call `supertag-move-node` interactively  
- `:supertag-move 'link` or `:supertag-move :link`  
  → after capture, call `supertag-move-node-and-link`, move the node and
    leave a backlink at the original position

This mirrors the classic “org-capture + org-refile” workflow, but the second
step is powered by Supertag’s move APIs and database-aware location updates.

#### Add Supertag tags interactively during org-capture

You can enable a Supertag-aware tag prompt after capture by setting
`:supertag-tags-prompt t` on a template (and enabling the integration
as shown above):

```elisp
(add-to-list 'org-capture-templates
             '("s" "Supertag task with tag prompt"
               entry
               (file "~/org/inbox.org")
               "* TODO %^{Task}\n  %?\n"
               :supertag t
               :supertag-tags-prompt t))
```

Flow:

1. Ensure org-capture integration is enabled, e.g.:
   ```elisp
   (setq supertag-org-capture-auto-enable t)
   ;; or: (supertag-enable-org-capture-integration)
   ```
2. Run org-capture with the template above; org-capture inserts the heading
3. Supertag finalizes the node (ID + database sync)
4. A prompt appears: `Supertag tags (comma separated):`
5. You can either:
   - Select tags from existing Supertag tags (with completion), or
   - Type new tag names; missing tags are created automatically
6. Supertag updates both the database and the Org headline with inline `#tag`

### Content Generator Functions

| Function                                            | Description                    |
| --------------------------------------------------- | ------------------------------ |
| `supertag-capture--get-from-static (args)`          | Static value generator         |
| `supertag-capture--get-from-prompt (args)`          | Interactive prompt generator   |
| `supertag-capture--get-from-clipboard ()`           | Clipboard content generator    |
| `supertag-capture--get-from-region ()`              | Region content generator       |
| `supertag-capture--get-from-region-or-clipboard ()` | Region or clipboard generator  |
| `supertag-capture--get-from-template-string (args)` | Template string processor      |
| `supertag-capture--get-from-function (args)`        | Custom function call generator |

## 🆚 Comparison with Org-Capture

| Feature                | Org-Capture             | Supertag-Capture                        |
| ---------------------- | ----------------------- | --------------------------------------- |
| Template Configuration | Static string templates | Dynamic content generators              |
| Tag Support            | Manual input            | Interactive selection + auto-completion |
| Field Management       | Property drawers        | Database field system                   |
| Content Sources        | Fixed format            | Multiple generators                     |
| Extensibility          | Limited                 | Highly extensible                       |

## 🐛 Troubleshooting

### Common Issues

**Issue**: Template execution error "Template doesn't exist"
**Solution**: Check `supertag-capture-templates` configuration and template key name

**Issue**: Field setting failed
**Solution**: Ensure tag exists and corresponding fields are defined

**Issue**: File write location error
**Solution**: Check if target file exists and is writable

### Debugging Tips

1. Use `M-x supertag-capture` to test basic functionality
2. Check error messages in the `*Messages*` buffer
3. Verify template configuration syntax correctness
4. Confirm all dependent files and tags exist

## 🧩 Developer Notes: Core Capture API

For advanced integration or custom frontends, you can call the core
finalization API directly on any Org heading that should become a
Supertag node:

```elisp
;; At an Org heading you want to turn into a Supertag node:
(supertag-capture-finalize-node-at-point
 '((:tag "task" :field "status" :value "todo")
   (:tag "task" :field "priority" :value "high")))
```

This will:

- Ensure the heading has a stable `ID` (creating one if needed)
- Register the ID/file mapping with `org-id`
- Sync the node into the Supertag database
- Apply the given Tag/Field/Value specs via `supertag-field-set-many`

Org-capture integration and `supertag-capture-with-template` both use
this function internally. When building new capture flows, prefer
reusing this API rather than reimplementing sync logic.

---

_This document describes the complete functionality and usage of the Org-Supertag v2.0 Capture System._

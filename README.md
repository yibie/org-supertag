# Org-SuperTag: Supercharge Org-mode with Modern Note-Taking Capabilities

[English](./README.md) | [中文](./README_CN.md)

## ⚠️ Org-SuperTag 5.0 Upgrade Notice

Org-SuperTag 5.0 represents a major architectural overhaul with significant improvements but also breaking changes that require your attention:

### 🏗️ Key Architecture Changes

The new version features a completely redesigned architecture with these major improvements:

- **Pure Emacs Lisp Implementation**: Eliminated all Python dependencies for a lighter, more maintainable codebase (~47% reduction in code size)
- **Data-Centric Architecture**: Introduced a single source of truth with `supertag--store` hash table
- **One-Way Data Flow**: Implemented strict Action -> Ops -> Transform -> Store -> Notify pipeline for better predictability

For detailed architecture comparison, see [Compare New/Old Architecture](doc/COMPARE-NEW-OLD-ARCHITECTURE.md)

### 😍 First-time Setup for New Users

**After configuring org-supertag and restarting Emacs, new users must run the following command first to initialize the database**:

`M-x supertag-sync-full-initialize`

This command performs a full scan of all files in the synchronized directories (`org-supertag-sync-directories`) to complete the database initialization.

Once this command finishes executing, org-supertag will be ready to serve you.

### 🔄 Database Migration Required

**Before using Org-SuperTag 5.0, you MUST migrate your existing database to the new format:**

1. **Migration Process**:
   - Load the migration script: `M-x load-file RET supertag-migration.el RET`
   - Run the migration: `M-x supertag-migrate-database-to-new-arch RET`
   - Select your old `org-supertag-db.el` file when prompted
   - A backup of your old database will be automatically created

2. **Important**: After migration is complete, you **must restart Emacs** immediately to ensure proper operation with the new architecture.

Failure to perform this migration will result in incompatibility with the new version and potential data loss.

### 🏷️ Compatibility with legacy :tag:

Org-SuperTag now reads both inline `#tag` and Org's native `:tag:` during sync/import. This preserves your existing files without modification and makes historical tags available in the database. Capture continues to create inline `#tag` in new headings; export behavior is unchanged.

## 🚀 What is Org-SuperTag?

> Org-SuperTag is a revolutionary Org-mode extension that upgrades the traditional tagging system into an intelligent knowledge management engine.  
>
> Imagine: Each of your tags can carry structured data, automatically execute tasks, and help you discover hidden connections between knowledge through AI assistants.

### 🎯 Core Concept: Tags as Databases

In traditional Org-mode, tags are just simple text markers. In Org-SuperTag:

- 🏷️ **Tags become data tables** - Each tag can define fields and types
- 🔗 **Nodes become data records** - Each title automatically gets an ID and structured storage
- 🤖 **Tags become smart assistants** - Can automatically execute actions and tasks
- 🔍 **Queries become data analysis** - Supports complex relational queries and visualization

### ⚡ 30-Second Core Feature Experience

```org
* My Project Ideas #project
  :PROPERTIES:
  :ID: abc123
  :END:
  
  This is an idea about improving the note-taking system...
```

When you type =#project=, Org-SuperTag automatically:
1. Adds a unique ID to the title
2. Creates a node record in the database
3. Establishes tag relationships
4. Provides a field editing interface (status, priority, due date, etc.)
5. Enables intelligent queries and visualization

### 🎬 Feature Demonstrations

#### 📝 Smart Tag Input
**Note**: Auto-completion is temporarily unavailable, so I've modified the example.
```org
* Learning Machine Learning (At this point M-x supertag-add-tag)
              
Candidate tags:
project 
learning 
research
```
- After selecting a tag, it will be automatically added and added to the node.
- Enter a new tag and press Enter to automatically record the new tag in the database and add it to the node.

#### 🗂️ Structured Field Management
Use `M-x supertag-view-node` to open the node view, move the cursor to the `Fields` field below the `#project` tag, and follow the instructions to edit.

![Structured Field Management](./picture/figure16.png)

#### 🔍 Powerful Query System
Use `M-x supertag-search` to open the query view, enter query conditions, then press `C-c C-c` to execute.

![Powerful Query System](./picture/figure17.gif)

### 🎨 Diverse View System

#### 📊 Kanban View
Use `M-x supertag-view-kanban` to open the kanban view, then follow the instructions to operate.

![Kanban View](./picture/figure19.gif)

#### ~~Discover View~~

This view is temporarily removed in the 5.0 new version.

#### 💬 AI Chat View
Use `M-x supertag-chat` to open the AI chat view, then follow the instructions to operate.

```org
You: Help me summarize all ongoing projects
AI: Based on your knowledge base, there are currently 3 ongoing projects:
    1. Machine Learning Project - High priority, due December 31
    2. Website Refactoring - Medium priority, needs frontend support
    3. Data Analysis - Low priority, waiting for data source
    
    I recommend focusing on the machine learning project first as it has a closer deadline.
```

##### AI Chat View Command System

- **Smart Slash**: `/` inserts a slash and can optionally show command menu
- **Smart Command Mode**: Commands can take parameters and execute immediately
  - `/bs Microsoft` → Switch to bs mode and execute immediately, using "Microsoft" as input, subsequent conversations remain in the selected mode until switched with `/default`
- **Type /commands to see current commands**
- **Type /define to customize conversation modes**
  - **Supports multiple formats**:
    - `/define name "prompt content"`
    - `/define name` (empty prompt)
    - `/define "name" "prompt"` (double quote format)

You can freely create your own commands, which will be named as .prompt files and stored in the `~/.emacs.d/org-supertag/prompts/` directory.

### 🛠️ Quick Start

#### Step 1: Installation and Configuration

```shell
# Clone the repository
git clone https://github.com/yibie/org-supertag.git ~/org-supertag
```

```emacs-lisp
(straight-use-package 'ht)
(straight-use-package 'gptel)

(straight-use-package '(org-supertag :host github :repo "yibie/org-supertag"))
(setq org-supertag-sync-directories '("Your/Path/To/Org-Files/"))
(eval-after-load 'gptel
  '(require 'org-supertag))
```

#### Step 2: Create Your First Smart Note

1. Open any .org file
2. Create a title: * My First Project
3. Type # and select or create a tag
4. 🎉 Congratulations! You've created a smart node

#### Step 3: Explore Powerful Features

- `M-x supertag-view-node` - View node details (including AI tag suggestions)
- `M-x supertag-search` - Smart search
- `M-x supertag-view-kanban` - Kanban view
- `M-x supertag-chat` - AI chat

### 🎯 Use Cases

#### 📚 Academic Research
```org
#paper + fields[journal, impact factor, reading status, notes]
#experiment + fields[hypothesis, method, results, conclusion]
#idea + fields[source of inspiration, feasibility, priority]
```

#### 💼 Project Management  
```org
#project + fields[status, priority, assignee, due date]
#task + fields[type, estimated time, dependencies, completion]
#meeting + fields[participants, agenda, decisions, follow-up actions]
```

### 🚀 Advanced Features

#### 🤖 Intelligent Automation System (Automation 2.0)

Version 5.0 brings a brand new automation system, implemented in pure Emacs Lisp, with better performance and more powerful features:

- ✅ **Unified Tag System**: Every tag is a fully-featured "database" with custom fields and automation capabilities
- ✅ **True Event-Driven**: Responds to precise data changes in real-time, rather than polling scans
- ✅ **Automatic Rule Indexing**: Automatically builds high-performance indexes for rules in the background, without users needing to worry about performance optimization details
- ✅ **Multiple Action Execution**: A single rule can trigger a series of sequentially executed actions
- ✅ **Scheduled Tasks**: Supports time-based and periodic automation, driven by an integrated scheduler
- ✅ **Relationships and Calculations**: Supports advanced features like bidirectional relationships, property synchronization, and Rollup calculations
- ✅ **Formula Fields**: Calculates and displays data in real-time in table views without persistent storage

| Feature | Old Version (Behavior) | New Version (Automation 2.0) |
|---------|------------------------|------------------------------|
| **Module Structure** | Dispersed multiple modules with circular dependencies | Unified single module, eliminating dependency issues |
| **Rule Management** | Manually attached to tags, requiring user management | Automatic indexing, intelligently managed by the system |
| **Performance** | O(n) traversal of all rules | O(1) index lookup, high performance |
| **API Consistency** | Multiple different API interfaces | Unified API interface, low learning cost |
| **Maintainability** | Complex inter-module relationships | Simple cohesive design, easy to maintain |

For details, see [Automation System Guide](doc/AUTOMATION-SYSTEM-GUIDE.md)

#### 📸 Capture System (Capture System)

Version 5.0 introduces a brand new capture system with more natural template syntax:

- ✅ **Intuitive Template Syntax** - Write templates directly like org-capture
- ✅ **Automatic Parsing** - Automatically recognize titles, tags, fields, and content
- ✅ **Smart Filling** - Support interactive prompts and placeholders
- ✅ **Flexible Configuration** - Specify files or let users choose

##### Configuring Capture Templates

Define templates using the `supertag-capture-templates` variable:

```emacs-lisp
(setq supertag-capture-templates
      '(
        ;; Quick task
        ("t" "Quick Task"
         :file "~/Documents/notes/plan.org"
         :node-spec
         (:template
          "* %^{Task:} #task
   - status: todo
   - priority: medium
   - create-at: %datetime

  %?"))

        ;; Question record
        ("q" "Question"
         :node-spec
         (:template
          "* #question %^{Question:}

  %^{Detailed Content:}

  %?"))

        ;; Meeting record
        ("m" "Meeting Record"
         :file "~/org/meetings.org"
         :node-spec
         (:template
          "* %^{Meeting Topic:} #meeting
   - type: %^{Meeting Type:}
   - status: completed
   - location: %^{Location:}

  Time: %date
  Participants:

  Agenda:

  Discussion Points:

  Action Items:

  %?"))))
```

##### Template Syntax

**Basic Structure:**
- First line: Title and inline tags `* Title #tag1 #tag2`
- Field lines: `- field_name: value`
- After empty line: Body content

**Interactive Prompts:**
- `%^{Prompt Text}` - Prompt user for input

**Placeholders:**
- `%date` - Current date (YYYY-MM-DD)
- `%datetime` - Date and time (YYYY-MM-DD HH:MM)
- `%time` - Current time
- `%?` - Set cursor position

**Configuration Options:**
- `:file` - Target file (optional, prompts user if not specified)
- `:node-spec` - Node specification containing `:template` string

##### Usage

1. **Template Capture**: `M-x supertag-capture-with-template`, select template
2. **Direct Capture**: `M-x supertag-capture`, use default template

For details, see [Capture Guide](doc/CAPTURE-GUIDE.md)

### ⌨️ Keyboard Shortcuts

Org-SuperTag no longer installs global keybindings automatically. To recreate the classic `C-c s` prefix, add the snippet below to your init file (after loading `org-supertag`):

```emacs-lisp
(with-eval-after-load 'org-supertag
  (define-prefix-command 'supertag-prefix-map)
  (define-key org-mode-map (kbd "C-c s") 'supertag-prefix-map)

  ;; Capture
  (define-key supertag-prefix-map (kbd "C") #'supertag-capture)
  (define-key supertag-prefix-map (kbd "t") #'supertag-capture-with-template)

  ;; Tag management
  (define-key supertag-prefix-map (kbd "a") #'supertag-add-tag)
  (define-key supertag-prefix-map (kbd "r") #'supertag-remove-tag-from-node)
  (define-key supertag-prefix-map (kbd "n") #'supertag-rename-tag)
  (define-key supertag-prefix-map (kbd "d") #'supertag-delete-tag-everywhere)
  (define-key supertag-prefix-map (kbd "c") #'supertag-change-tag-at-point)
  (define-key supertag-prefix-map (kbd "x") #'supertag-set-child)
  (define-key supertag-prefix-map (kbd "X") #'supertag-clear-parent)

  ;; Node + reference management
  (define-key supertag-prefix-map (kbd "m") #'supertag-move-node-and-link)
  (define-key supertag-prefix-map (kbd "l") #'supertag-add-reference)
  (define-key supertag-prefix-map (kbd "L") #'supertag-add-reference-and-create)
  (define-key supertag-prefix-map (kbd "R") #'supertag-remove-reference)
  (define-key supertag-prefix-map (kbd "h") #'supertag-back-to-heading)
  (define-key supertag-prefix-map (kbd "N") #'supertag-create-node)
  (define-key supertag-prefix-map (kbd "D") #'supertag-delete-node)
  (define-key supertag-prefix-map (kbd "f") #'supertag-find-node)
  (define-key supertag-prefix-map (kbd "o") #'supertag-find-node-other-window)
  (define-key supertag-prefix-map (kbd "M") #'supertag-move-node)
  (define-key supertag-prefix-map (kbd "u") #'supertag-update-node-at-point)

  ;; Query & views
  (define-key supertag-prefix-map (kbd "i") #'supertag-insert-query-block)
  (define-key supertag-prefix-map (kbd "s") #'supertag-search)
  (define-key supertag-prefix-map (kbd "e") #'supertag-search-export-results-to-file)
  (define-key supertag-prefix-map (kbd "E") #'supertag-search-export-results-to-new-file)
  (define-key supertag-prefix-map (kbd "I") #'supertag-search-insert-at-point)
  (define-key supertag-prefix-map (kbd "g") #'supertag-chat)
  (define-key supertag-prefix-map (kbd "v") #'supertag-view-node)
  (define-key supertag-prefix-map (kbd "T") #'supertag-view-table)
  (define-key supertag-prefix-map (kbd "k") #'supertag-view-kanban)

  ;; Embeds & maintenance
  (define-key supertag-prefix-map (kbd "b") #'supertag-insert-embed)
  (define-key supertag-prefix-map (kbd "B") #'supertag-convert-link-to-embed)
  (define-key supertag-prefix-map (kbd "C-r") #'supertag-services-embed-refresh-all)
  (define-key supertag-prefix-map (kbd "C-c") #'supertag-sync-cleanup-database))
```

| Key | Command | Description |
|-----|---------|-------------|
| `C-c s C` | supertag-capture | Direct capture |
| `C-c s t` | supertag-capture-with-template | Capture with template |
| `C-c s a` | supertag-add-tag | Add a tag to the current node |
| `C-c s r` | supertag-remove-tag-from-node | Remove a tag from the current node |
| `C-c s n` | supertag-rename-tag | Rename a tag |
| `C-c s d` | supertag-delete-tag-everywhere | Delete a tag everywhere |
| `C-c s c` | supertag-change-tag-at-point | Change tag at point |
| `C-c s x` | supertag-set-child | Assign child tags |
| `C-c s X` | supertag-clear-parent | Clear tag parent |
| `C-c s m` | supertag-move-node-and-link | Move node and leave backlink |
| `C-c s l` | supertag-add-reference | Add reference to node |
| `C-c s L` | supertag-add-reference-and-create | Add reference and create node if missing |
| `C-c s R` | supertag-remove-reference | Remove reference from node |
| `C-c s h` | supertag-back-to-heading | Back to heading |
| `C-c s N` | supertag-create-node | Create new node |
| `C-c s D` | supertag-delete-node | Delete node |
| `C-c s f` | supertag-find-node | Find node |
| `C-c s o` | supertag-find-node-other-window | Find node in other window |
| `C-c s M` | supertag-move-node | Move node |
| `C-c s u` | supertag-update-node-at-point | Update node at point |
| `C-c s i` | supertag-insert-query-block | Insert query block |
| `C-c s s` | supertag-search | Open query interface |
| `C-c s e` | supertag-search-export-results-to-file | Export query results to file |
| `C-c s E` | supertag-search-export-results-to-new-file | Export query results to new file |
| `C-c s I` | supertag-search-insert-at-point | Insert query results at point |
| `C-c s g` | supertag-chat | Open chat view |
| `C-c s v` | supertag-view-node | View node details |
| `C-c s T` | supertag-view-table | Open table view |
| `C-c s k` | supertag-view-kanban | Open kanban view |
| `C-c s b` | supertag-insert-embed | Insert embed |
| `C-c s B` | supertag-convert-link-to-embed | Convert link to embed |
| `C-c s C-r` | supertag-services-embed-refresh-all | Refresh all embeds |
| `C-c s C-c` | supertag-sync-cleanup-database | Clean database |

These bindings are optional—adapt or trim them to fit your workflow.

### 🔧 Configuration Guide

#### Basic Configuration
```emacs-lisp
;; Core configuration
(setq org-supertag-sync-directories '("~/notes/" "~/projects/"))
```

#### AI Service Configuration
```emacs-lisp
;; Control whether to enable AI services
;; Set to nil to disable AI services, t by default
(setq org-supertag-bridge-enable-ai nil)
```

#### Advanced Configuration
```emacs-lisp
;; Custom field types
(add-to-list 'org-supertag-field-types
  '(rating . (:validator org-supertag-validate-rating
              :formatter org-supertag-format-rating
              :description "Rating (1-5)")))

;; Custom query commands
(defun my-urgent-projects ()
  "Find all urgent projects"
  (interactive)
  (supertag-search '(and (tag "project") (tag "urgent"))))
```

### 🆚 Comparison with Other Tools

| Feature | Org-SuperTag | Org-roam | Obsidian | Notion |
|---------|--------------|----------|----------|--------|
| Structured Data | ✅ Native support | ❌ | ⚠️ Plugin | ✅ |
| Complex Queries | ✅ S-expressions | ⚠️ Basic | ⚠️ Basic | ✅ |
| Automated Actions | ✅ Powerful | ❌ | ⚠️ Limited | ⚠️ Limited |
| AI Integration | ✅ Deep integration | ❌ | ⚠️ Plugin | ✅ |
| Offline Use | ✅ | ✅ | ✅ | ❌ |
| Learning Curve | ⚠️ Moderate | ⚠️ Moderate | ✅ Simple | ✅ Simple |

### 🤝 Community and Support

- 📖 [Detailed Documentation](https://github.com/yibie/org-supertag/wiki)
- 🐛 [Issue Feedback](https://github.com/yibie/org-supertag/issues)
- 💬 [Community Discussions](https://github.com/yibie/org-supertag/discussions)

### Changelog
See [CHANGELOG](./CHANGELOG.org).

#### 🆘 Frequently Asked Questions

##### Q: What if the database gets corrupted?
A: Use =M-x supertag-sync-cleanup-database= for complete recovery.

##### Q: How do I backup my data?
A: Org-SuperTag provides automatic daily backup functionality:

**Automatic Backup**:
- Daily backups are automatically created in `~/.emacs.d/org-supertag/backups/`
- Keeps backups for the last 3 days (configurable)
- Old backups are automatically cleaned up
- Backup files are named: `supertag-db-YYYY-MM-DD.el`

**Manual Backup**:
- Force backup: `M-x supertag-backup-database-now`
- Manual backup of entire directory: `~/.emacs.d/org-supertag/`

**Configuration Options**:
```emacs-lisp
;; Set backup interval (seconds, default 86400=24 hours)
(setq supertag-db-backup-interval 86400)

;; Set backup retention days (default 3 days)
(setq supertag-db-backup-keep-days 3)

;; Disable automatic backup
(setq supertag-db-backup-interval nil)
```

##### Q: How do I get AI tag suggestions?
A: In the node view (=M-x supertag-view-node=), click "💡 Get AI Tag Suggestions" or press =s=. This is manually triggered and won't interfere with your workflow.

##### Q: What configuration is needed for AI features?
A: AI features use the default Ollama configuration, no additional setup required. All AI features are integrated into the existing view system, making them simple and intuitive to use.

### 🎉 Get Started Now

> Don't be intimidated by the complex features! The design philosophy of Org-SuperTag is "start simple, then go deeper".
>
> Start by adding your first =#tag=, then gradually explore structured data, intelligent queries, AI assistants, and other advanced features.
>
> Every feature is designed to make your knowledge management more efficient and intelligent.


---

*Made with ❤️ by [Yibie](https://github.com/yibie) | Inspired by [Tana](https://tana.inc), [ekg](https://github.com/ahyatt/ekg), [org-node](https://github.com/meedstrom/org-node)*

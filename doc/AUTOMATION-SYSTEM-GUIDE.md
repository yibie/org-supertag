# Automation System 2.0 - User Guide and Examples

## 🚀 Overview

`org-supertag` Automation System 2.0 is a modern, event-driven automation framework. It empowers every tag with powerful automation capabilities, allowing you to build truly intelligent, hands-free org-mode workflows.

### Core Features

- ✅ **Unified Tag System**: Every tag is a fully-featured "database" with custom fields and automation capabilities.
- ✅ **True Event-Driven**: Responds to precise data changes in real-time, rather than polling scans.
- ✅ **Automatic Rule Indexing**: Automatically builds high-performance indexes for rules in the background, without users needing to worry about performance optimization details.
- ✅ **Multiple Action Execution**: A single rule can trigger a series of sequentially executed actions.
- ✅ **Scheduled Tasks**: Supports time-based and periodic automation, driven by an integrated scheduler.
- ✅ **Relationships and Calculations**: Supports advanced features like bidirectional relationships, property synchronization, and Rollup calculations.
- ✅ **Formula Fields**: Calculates and displays data in real-time in table views without persistent storage.
- ✅ **Backward Compatibility**: Provides a compatibility layer for legacy APIs to ensure smooth transitions.

## 🏗️ Unified Architecture (Automation System 2.0)

The new version adopts a unified module architecture:
- **Single Module**: `supertag-automation.el` contains all automation functionality
- **Eliminated Dependencies**: No more circular dependencies between modules
- **Unified API**: All functionality accessed through consistent interfaces
- **High Performance**: Cohesive design brings better performance

The core of the new architecture is **simplicity** and **automation**. We've eliminated the distinction between "regular tags" and "database tags," replacing the manual "behavior attachment" process with an intelligent "rule indexing" system.

### Architecture Advantages

| Feature | Old Version (1.0) | New Version (2.0) |
|------|-------------|-------------|
| **Module Structure** | Dispersed multiple modules with circular dependencies | Unified single module, eliminating dependency issues |
| **Rule Management** | Manually attached to tags, requiring user management | Automatic indexing, intelligently managed by the system |
| **Performance** | O(n) traversal of all rules | O(1) index lookup, high performance |
| **API Consistency** | Multiple different API interfaces | Unified API interface, low learning cost |
| **Maintainability** | Complex inter-module relationships | Simple cohesive design, easy to maintain |

### Module Loading

```elisp
;; New unified module loading approach
(require 'supertag-automation)  ; Unified automation module
```

```mermaid
graph LR
    subgraph "Old Architecture"
        A1[Data Change] --> B1[Event Wakeup]
        B1 --> C1[Traverse/Filter Rules]
        C1 --> D1[Execute]
    end
    
    subgraph "New Architecture"
        A2[Rule Definition] --> B2[Automatic Index Building in Background]
        C2[Data Change] --> D2[Event Notification]
        D2 --> E2[O(1) Index Query]
        E2 --> F2[Precise Condition Evaluation]
        F2 --> G2[Execute]
    end
    
    style B2 fill:#e8f5e8
    style E2 fill:#e8f5e8
    style G2 fill:#e1f5fe
```

## 📚 Quick Start: Creating Your First Automation Rule

Under the new system, you no longer need to "attach" behaviors. You simply **define a rule**, and the system automatically handles everything else.

*(Note: All examples below assume you have pre-defined the required tags (such as `#task`, `#project`) and relationships using `supertag-tag-create` and `supertag-relation-create`. We focus here on creating the automation rules themselves.)*

**Scenario**: When any node is tagged with `#task`, automatically set its `TODO` state to "TODO".

```elisp
;; Create an automation rule
(supertag-automation-create
 '(:name "auto-set-todo-on-task"
   ;; Trigger: When a node is tagged with "task"
   :trigger (:on-tag-added "task")
   ;; Action list: Can contain one or more actions
   :actions '((:action :update-property
              :params (:property :todo :value "TODO")))))
```

**That's it!**

#### Simulated Effect

**Before Operation**:
```org
* A regular heading
```

**Operation**: In Org Mode, with cursor on the heading, press `C-c C-q` (`org-set-tags-command`) and enter `task`.

**After Operation**:
```org
* TODO A regular heading :task:
```
**Effect Analysis**: The rule is activated by the `:on-tag-added` trigger, executing the `:update-property` action in the `:actions` list, automatically adding the `TODO` state.

---

## Core Concept: Relationships

Relationships are one of `org-supertag`'s core capabilities, responsible for establishing meaningful links between different types of data (defined by tags). For example, linking "project" notes with "task" notes.

You can define a relationship using the `supertag-relation-create` function.

### Relationship Types

When defining relationships, the most important property is `:type`, which determines how data is associated (cardinality).

| Type | Format | Description | Example |
| :--- | :--- | :--- | :--- |
| **One-to-One** | `:one-to-one` | A "source" node can be associated with at most one "target" node. | A `User` can have only one `Profile`. A task can have at most one "predecessor task". |
| **One-to-Many** | `:one-to-many` | A "source" node can be associated with **multiple** "target" nodes, but each "target" node can only be associated with one "source" node. | A `#Project` can contain multiple `#Task`s. A `#Notebook` can contain multiple `#Note`s. |
| **Many-to-Many** | `:many-to-many` | "Source" and "target" nodes can be arbitrarily associated with each other, with no limit on quantity. | An `#Article` can have multiple `#Keyword`s; a `#Keyword` can also be used in multiple `#Article`s. |

### Advanced Feature: Rollup

Rollup is a powerful feature of the relationship system that allows "one" end nodes (such as `#Project`) to automatically collect data from all associated "many" end nodes (such as multiple `#Task`s) and perform real-time calculations.

You can configure rollup by adding a `:rollup` property when defining `:one-to-many` or `:many-to-many` relationships.

#### **Rollup Configuration Parameters**

The `:rollup` property itself is a property list (plist) containing the following three key parameters:

| Parameter | Description | Example |
| :--- | :--- | :--- |
| **`:from-field`** | Specifies which property to collect data from on the "many" end nodes (source). | Collect `:hours` property values from all `#Task` nodes. |
| **`:to-field`** | Specifies which property on the "one" end node (target) to write the calculation result to. | Write the calculation result to the `:total_hours` property of the `#Project` node. |
| **`:function`** | Specifies which function to use to process the collected data. | Use the `sum` function to add up all the hours. |

#### **Available Rollup Functions (`:function`)**

The system has built-in several commonly used calculation functions:

| Function Name | Description | Test Status |
| :--- | :--- | :------- |
| `sum` | Calculate the sum of all numeric values. | ✅ Verified |
| `count` | Count the total number of associated nodes. | ✅ Verified |
| `average` | Calculate the average of all numeric values. | ✅ Verified |
| `min` / `max` | Find the minimum or maximum value among all numeric values. | ✅ Verified |
| `unique-count` | Count how many unique property values there are. | ✅ Verified |
| `concat` | Concatenate all property values (usually text) into a string. | ✅ Verified |
| `first` / `last` | Return the first or last value. | ✅ Supported |

**Planned Features (Future Versions)**:
| `count-where-filled` | Count nodes with non-empty values. | 🔄 Planned |
| `percent-done` | Calculate completion percentage. | 🔄 Planned |

---

## Core Concept: Formula Fields

Formula fields are an innovative feature of `org-supertag` that allows you to define "virtual columns" in table views, whose values are calculated in real-time based on other fields. The calculation results of formula fields are **not** stored in node properties; they are only calculated and displayed when the table view is rendered.

### How to Define Formula Fields

In tag definitions, you can declare formula fields just like regular fields, but their `:type` is `:formula`, and they include a `:formula` property to define the calculation expression.

```elisp
(supertag-tag-create
 '(:id "task"
   :name "Task"
   :fields ((:name "due_date" :type :date)
            (:name "completed_date" :type :date)
            (:name "progress" :type :number)
            ;; Example: Calculate remaining days
            (:name "days_left" :type :formula
                   :formula "(days-until (get-property :due_date))")
            ;; Example: Calculate completion percentage
            (:name "completion_percentage" :type :formula
                   :formula "(* (/ (get-property :progress) 100) 100))")))
```

### Formula Language and Available Functions

Formula expressions use a safe subset of Emacs Lisp. In the formula environment, you can access the following functions:

*   `get-property :prop-name`: Get the value of a specified property of the current node.
*   `days-until date-list`: Calculate the number of days from the current date to the date specified by `date-list` (`date-list` format is `(year month day)`).
*   And other standard Emacs Lisp functions (within a safe sandbox).

### Differences Between Formula Fields, Automation Rules, and Rollup

| Feature | Formula Fields | Automation Rules | Rollup |
| :--- | :--- | :--- | :--- |
| **Purpose** | Display calculation results in real-time **in views** | **Modify persistent data** in the underlying database based on events | Aggregate related node data, **persistently store** results |
| **Trigger Timing** | When table view is rendered | Data change events (such as property changes, tag additions/removals) | When relationships or related node properties change |
| **Data Persistence** | **Does not** store results in database | **Will** store results in database | **Will** store results in database |
| **Use Cases** | Lightweight, instant display calculations without changing original data | Need persistent data changes, trigger complex workflows | Cross-node data aggregation, need persistent rollup results |

---

## Core Concept: Scheduled Tasks

In addition to responding to real-time data changes, Automation System 2.0 can also be time-driven to execute pre-scheduled tasks. This is supported by an integrated, reliable scheduler service (`supertag-services-scheduler.el`).

You don't need to interact with the scheduler directly. You simply define an Automation with a time rule, and the system will automatically schedule everything for you.

### How to Define Scheduled Tasks

A scheduled task is essentially a regular Automation rule, but it must follow these conventions:

1.  The value of **trigger (`:trigger`)** must be `:on-schedule`.
2.  The actions in the **actions (`:actions`)** list must be of type `:call-function`. Because scheduled tasks don't have a single context node, they need to call a more generic function to perform batch operations.
3.  A **`:schedule`** property must be provided to define execution time.

#### **Schedule (`:schedule`) Parameter Details**

The `:schedule` property is a property list (plist) used to precisely describe the task execution time.

| Parameter | Type | Description |
| :--- | :--- | :--- |
| **`:type`** | Keyword | The type of scheduled task. Currently supports `:daily` (daily type). |
| **`:time`** | String | The time when the task executes, in `"HH:MM"` format (24-hour). |
| **`:days-of-week`**| List (Optional) | A list of numbers specifying which days of the week to execute. `0` represents Sunday, `1` represents Monday, ..., `6` represents Saturday. If this parameter is omitted, the task will execute at the specified time every day. |

### Complete Example: Weekly Task Review

This is a complete, directly usable example.

**Scenario**: Every Friday at 8:00 PM, automatically find all incomplete high-priority tasks and add a `#review` tag to them for weekend review.

```elisp
;; 1. Define scheduled task rule
(supertag-automation-create
 '(:name "weekly-review-high-priority-tasks"
   :trigger :on-schedule
   :schedule (:type :daily :time "20:00" :days-of-week '(5)) ; 5 represents Friday
   :actions '((:action :call-function
              :params (:function #'my-app-flag-tasks-for-review)))))

;; 2. Implement the function called by the rule
(defun my-app-flag-tasks-for-review ()
  "Find all incomplete high-priority tasks and add review tag."
  (interactive)
  (let ((tasks-to-review 
         (supertag-query-nodes
          '(and (has-tag "task")
                (not (property-equals :status "Done"))
                (property-equals :priority "High")))))
    (dolist (task tasks-to-review)
      (let ((task-id (plist-get task :id)))
        (supertag-node-add-tag task-id "review")
        (message "Task %s flagged for weekly review." (plist-get task :title))))
    (message "%d tasks flagged for review." (length tasks-to-review))))
```

### Activate Scheduler

**Important Note**: To start scheduled tasks running, you must manually start the scheduler service once in your Emacs configuration file (such as `init.el`). This operation only needs to be performed once.

```elisp
(supertag-scheduler-start)
```

Once started, the scheduler will run continuously in the background and execute your defined scheduled tasks precisely at the times you specify.

---

## 📖 Automation Rule Reference Manual

To create your own rules, you need to understand the three core components of a rule: **Triggers (WHEN)**, **Conditions (IF)**, and **Actions (THEN)**.

### 1. Triggers - `WHEN`

The `trigger` field defines "when" to check this rule. A precise trigger is the cornerstone of high performance.

| Trigger Type | Format | Description |
| :--- | :--- | :--- |
| **Property Change** | `:on-property-change` | Triggered when any property of any node changes. This is the most commonly used but most generic trigger, usually requiring `condition` for precision. |
| **Tag Added** | `(:on-tag-added "tag-name")` | Triggered when a node is **first** tagged with the specified tag. |
| **Tag Removed** | `(:on-tag-removed "tag-name")` | Triggered when a specified tag is removed from a node. |
| **Relationship Change** | `:on-relation-change` | Triggered when a node's "relationship" changes (e.g., when a task is linked to a project). |
| **Scheduled Task** | `:on-schedule` | Time-based trigger, requires `:schedule` property. |
| **Node Creation** | `:on-create` | Triggered when a new node with tags is created. |

### 2. Conditions - `IF`

The `condition` field defines the "preconditions" that must be met for the rule to execute. It is a Lisp-style logical expression.

| Condition Type | Format | Description |
| :--- | :--- | :--- |
| **Logical Combinations** | `(and ...)` `(or ...)` `(not ...)` | Used to combine multiple conditions to implement complex logical judgments. |
| **Has Tag** | `(has-tag "tag-name")` | Check if the current node has the specified tag. |
| **Property Equals** | `(property-equals :prop-name "value")` | Check if a node's property equals a specific value. |
| **Property Changed**| `(property-changed :prop-name)` | Check if this event was caused by a change in the specified property. |
| **Property Test**| `(property-test :prop-name #'> 8)` | Use a function to test the property value. |
| **Formula Condition**| `(:formula "(> (get-property :hours) 8)")` | Execute a complete formula for maximum flexibility. |

### 3. Actions - `THEN`

The `actions` field (note the plural) defines one or more actions that the system should execute sequentially when both trigger and conditions are satisfied. It is an **action list**.

Each action is a `plist` in the format `(:action :action-type :params (...))`.

| Action Type (`:action-type`) | `:params` Parameters | Description |
| :--- | :--- | :--- |
| **`:update-property`** | `(:property :prop-name :value new-value)` | Update or add a property to the node. `new-value` can be a direct value or a Lisp expression that returns a value. |
| **`:add-tag`** | `(:tag "tag-name")` | Add a new tag to the current node. |
| **`:remove-tag`** | `(:tag "tag-name")` | Remove a tag from the current node. |
| **`:call-function`** | `(:function #'your-function)` | Call an Emacs Lisp function you've defined yourself. This is the "ultimate weapon" for implementing complex logic. The function will receive `(node-id context)` as two parameters. |
| **`:create-node`** | `(:title "..." :tags '("...") ...)` | Create a completely new node. |

---

## 🎯 Usage Examples

### Example 1: Powerful Task Management

This example will demonstrate some features that are difficult or impossible to achieve with native Org Mode, showcasing the unique value of the new system.

*(Note: This example assumes that a `#task` tag with `status`, `priority`, and `hours` fields has been pre-defined, as well as a one-to-one relationship named `depends_on` from `task` to `task`.)*

#### 1. Creating Truly "Smart" Automation Rules

**Rule A: Automatically Set Priority Based on Estimated Hours**

```elisp
(supertag-automation-create
 '(:name "auto-set-priority-by-effort"
   :trigger :on-property-change
   :condition (and
               (has-tag "task")
               (property-changed :hours)
               (:formula "(> (get-property :hours) 8)"))
   :actions '((:action :update-property
              :params (:property :priority :value "High")))))
```
*(Note: `property-changed` and `get-property` are envisioned richer condition functions to clearly express intent)*

**Rule B: Automatically Unlock the Next Dependent Task When One Task is Completed**

```elisp
(supertag-automation-create
 '(:name "unlock-next-dependent-task"
   :trigger :on-property-change
   :condition (and (has-tag "task")
                   (property-equals :status "Done"))
   :actions '((:action :call-function
              :params (:function #'my-app-unlock-next-task)))))

;; Implement custom function for the rule above
(defun my-app-unlock-next-task (node-id context)
  "Find the next task dependent on the just-completed task and update its status from 'Waiting' to 'Todo'."
  (interactive)
  (let* ((completed-task-id node-id)
         (dependent-tasks (supertag-relation-get-reverse-related-nodes
                           completed-task-id "depends_on")))
    (dolist (task-info dependent-tasks)
      (let ((task-id (plist-get task-info :id))
            (task-data (supertag-get `(:nodes ,(plist-get task-info :id)))))
        (when (equal (plist-get task-data :status) "Waiting")
          (supertag-node-update-property task-id :status "Todo")
          (message "Task %s unlocked, status set to TODO." (plist-get task-data :title)))))))
```

#### 2. Simulated Effects

*   **Smart Priority**:
    *   **Before Operation**: Node `#task`'s `:priority:` property is `Low`.
    *   **Operation**: Change the node's `:hours:` property value from `4` to `10`.
    *   **After Operation**: The node's `:priority:` property automatically becomes `High`.

*   **Dependency Unlocking**:
    *   **Before Operation**: "Task A" and "Task B" are both `#task`. "Task B" depends on "Task A" through the `depends_on` relationship, and "Task B"'s `:status:` is `Waiting`.
    *   **Operation**: Change "Task A"'s `:status:` to `Done`.
    *   **After Operation**: "Task B"'s `:status:` automatically changes from `Waiting` to `Todo`.

### Example 2: Project-Task Integration

This example will showcase the powerful capabilities of "Relationships" and "Rollup".

*(Note: This example assumes that a `#Project` tag with `status` and `total_hours` fields has been pre-defined, as well as a one-to-many relationship named `tasks` from `Project` to `task`, configured with a `sum` rollup from `:hours` to `:total_hours`.)*

#### 1. Creating Automation Rules

When a subtask's status changes, we want to check if all tasks in the parent project are completed.

```elisp
(supertag-automation-create
 '(:name "auto-complete-project"
   :trigger :on-property-change
   :condition (and (has-tag "task") (property-equals :status "Done"))
   :actions '((:action :call-function
              :params (:function #'my-app-check-project-completion)))))

;; Implement function to check project completion status
(defun my-app-check-project-completion (task-id context)
  "When a task is completed, check if all tasks in its project are completed."
  (interactive)
  ;; 1. Find the project this task belongs to
  (when-let ((projects (supertag-relation-get-related-nodes task-id "tasks" :reverse t)))
    (let* ((project-id (plist-get (car projects) :id))
           (all-tasks (supertag-relation-get-related-nodes project-id "tasks"))
           (all-done t))
      ;; 2. Check if all tasks under the project are completed
      (dolist (task all-tasks)
        (unless (equal (plist-get (supertag-get `(:nodes ,(plist-get task :id))) :status) "Done")
          (setq all-done nil)))
      ;; 3. If all tasks are completed, update project status
      (when all-done
        (supertag-node-update-property project-id :status "Done")
        (message "All tasks in Project %s are done. Project status updated." project-id))))) 
```

#### 2. Simulated Effects

*   **Automatic Hours Rollup (Driven by Relationship Definition)**:
    *   **Before Operation**: `#Project`'s `:total_hours:` is `5`. It's associated with a `#task` with `:hours:` of `5`.
    *   **Operation**: Associate a new `#task` with the project and set its `:hours:` to `3`.
    *   **After Operation**: `#Project`'s `:total_hours:` automatically updates to `8`.

*   **Automatic Project Completion (Driven by This Rule)**:
    *   **Before Operation**: `#Project` is associated with two `#task`s, one with `Done` status and another with `Todo` status.
    *   **Operation**: Change the `#task` with `Todo` status to `Done`.
    *   **After Operation**: `#Project`'s `:status:` automatically becomes `Done`.

### Example 3: Multi-Action

This is the best demonstration of the new automation engine's powerful capabilities: a single rule can execute multiple actions sequentially.

**Scenario**: When a task's status is set to `Done`, automatically record the completion date and tag it with `#archived`.

```elisp
(supertag-automation-create
 '(:name "process-completed-task"
   :trigger :on-property-change
   :condition (and (has-tag "task")
                   (property-equals :status "Done"))
   :actions
   '((:action :update-property
      :params (:property :completed_date :value (current-time)))
     (:action :add-tag
      :params (:tag "archived")))))
```

#### Simulated Effects

*   **Before Operation**: A `#task` node with `:status:` of `Doing`, no `:completed_date:` property, and no `#archived` tag.
*   **Operation**: Change the node's `:status:` to `Done`.
*   **After Operation**:
    1.  The node gains a `:completed_date:` property with the current time as its value.
    2.  The node is automatically tagged with `#archived`.

---

## 🔧 Core Philosophy Review

After our refactoring, the system's core philosophy has become clearer:

1.  **Tags are Core**: All data structures (fields) and behaviors (automation) are organized around tags.
2.  **Rules are Declarative**: You only need to "declare" in rules what events and targets it cares about using `:trigger` and `:condition`, and the system will automatically apply it to the right place.
3.  **Backend is Intelligent**: You don't need to worry about performance. The system automatically builds indexes for your rules, ensuring fast response times even with hundreds or thousands of rules.
4.  **No Class Distinctions**: Any tag, whether simple or complex, can have

---

## 🔧 Testing and Debugging Automation Rules

### Manual Testing of Individual Rules

After creating automation rules, you can use the following methods to test and debug:

```elisp
;; Simulate rule triggering
(supertag-rule-execute rule node-id context)

;; Rebuild rule index
(supertag-rebuild-rule-index)

;; View current index status
(hash-table-count supertag--rule-index)
```

### Debugging Tips

- **Add Logging**: Use the `message` function to add log output in rules
- **Check Trigger Conditions**: Confirm that rule trigger conditions match correctly
- **Validate Data Paths**: Check that property names and data paths are correct

```elisp
;; Add debugging information in custom functions
(defun my-debug-function (node-id context)
  (message "Debug: Processing node %s with context %s" node-id context)
  ;; Your logic code
  )

;; Check node properties
(supertag-get `(:nodes ,node-id))

;; Check if rule is correctly indexed
(gethash "your-trigger-key" supertag--rule-index)
```

### Testing Workflow

1. **Create Test Nodes**: Create sample nodes for testing
2. **Trigger Events**: Manually modify properties or add tags to trigger rules
3. **Verify Results**: Check data changes after rule execution
4. **Debug Issues**: If results don't match expectations, check triggers, conditions, and actions

```elisp
;; Example: Test automatic task priority setting rule
(let ((test-node-id (supertag-node-create
                     :title "Test Task"
                     :tags '("task"))))
  ;; Trigger rule
  (supertag-node-update-property test-node-id :hours 10)
  ;; Verify results
  (let ((priority (supertag-node-get-property test-node-id :priority)))
    (message "Priority after update: %s" priority)))
```

---

## 📦 Batch Operations and Maintenance

### System Maintenance Commands

Automation System 2.0 provides a series of maintenance commands to ensure healthy system operation:

```elisp
;; Recalculate all rollup values
(supertag-automation-recalculate-all-rollups)

;; Synchronize all property relationships
(supertag-automation-sync-all-properties)

;; Clean up and rebuild index
(supertag-automation-cleanup)
(supertag-automation-init)
```

### Batch Data Operations

When you need to perform the same operation on large amounts of data, you can use the following pattern:

```elisp
;; Batch update priority of all incomplete tasks
(defun batch-update-task-priority ()
  "Set priority of all incomplete tasks to Normal."
  (interactive)
  (let ((tasks (supertag-query-nodes
                '(and (has-tag "task")
                      (not (property-equals :status "Done"))))))
    (dolist (task tasks)
      (let ((task-id (plist-get task :id)))
        (supertag-node-update-property task-id :priority "Normal")))
    (message "Updated %d tasks" (length tasks))))

;; Batch clean up archived tasks older than 90 days
(defun batch-cleanup-archived-tasks ()
  "Delete archived tasks older than 90 days."
  (interactive)
  (let* ((cutoff-date (time-subtract (current-time) (days-to-time 90)))
         (old-tasks (supertag-query-nodes
                     `(and (has-tag "task")
                           (has-tag "archived")
                           (property-test :completed_date
                                          (lambda (date)
                                            (time-less-p date ,cutoff-date)))))))
    (dolist (task old-tasks)
      (supertag-node-delete (plist-get task :id)))
    (message "Cleaned up %d old archived tasks" (length old-tasks))))
```

### Data Import and Export

```elisp
;; Batch import task data from CSV
(defun import-tasks-from-csv (csv-file)
  "Batch import tasks from CSV file."
  (interactive "fCSV file: ")
  (with-temp-buffer
    (insert-file-contents csv-file)
    (let ((lines (split-string (buffer-string) "\n" t)))
      (dolist (line (cdr lines)) ; Skip header row
        (let* ((fields (split-string line ","))
               (title (nth 0 fields))
               (priority (nth 1 fields))
               (hours (string-to-number (nth 2 fields))))
          (let ((node-id (supertag-node-create :title title :tags '("task"))))
            (supertag-node-update-property node-id :priority priority)
            (supertag-node-update-property node-id :hours hours)))))
    (message "Tasks imported successfully")))

;; Export project summary
(defun export-project-summary (project-tag output-file)
  "Export project summary report to file."
  (interactive "sProject tag: \nFOutput file: ")
  (let* ((projects (supertag-query-nodes `(has-tag ,project-tag)))
         (report-lines '("Project Summary Report" "=========================")))
    (dolist (project projects)
      (let* ((project-id (plist-get project :id))
             (title (plist-get project :title))
             (total-hours (supertag-node-get-property project-id :total_hours))
             (status (supertag-node-get-property project-id :status)))
        (push (format "Project: %s | Status: %s | Hours: %s"
                      title status total-hours) report-lines)))
    (with-temp-file output-file
      (insert (string-join (reverse report-lines) "\n")))
    (message "Report exported to %s" output-file)))
```

---

## ⚡ Performance Monitoring and Optimization

### Index System Working Principle

The core performance advantage of Automation System 2.0 comes from its intelligent indexing system:

- **Rule Indexing**: Rules are automatically indexed based on trigger sources
- **O(1) Lookup**: No need to traverse all rules, achieving constant-time lookup
- **Precise Matching**: Supports precise matching for property changes and tag changes
- **Automatic Maintenance**: Indexes are automatically created and updated in the background

```elisp
;; View index status
(defun supertag-check-index-status ()
  "Check rule index status and statistics."
  (interactive)
  (let ((total-rules (hash-table-count supertag--rule-index))
        (trigger-types (make-hash-table :test 'equal)))
    (maphash (lambda (key value)
               (let ((trigger-type (car (split-string key ":"))))
                 (puthash trigger-type (1+ (gethash trigger-type trigger-types 0)) trigger-types)))
             supertag--rule-index)
    (message "Total indexed rules: %d" total-rules)
    (maphash (lambda (type count)
               (message "  %s: %d rules" type count))
             trigger-types)))

;; Benchmark rule lookup performance
(defun supertag-benchmark-rule-lookup (iterations)
  "Benchmark rule lookup performance."
  (interactive "nIterations: ")
  (let ((start-time (current-time))
        (test-key "on-property-change:status"))
    (dotimes (i iterations)
      (gethash test-key supertag--rule-index))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "Performed %d lookups in %.4f seconds (%.2f μs per lookup)" 
               iterations elapsed (* (/ elapsed iterations) 1000000)))))
```

### Performance Best Practices

#### 1. Trigger Optimization

```elisp
;; Good practice: Use specific triggers
(supertag-automation-create
 '(:name "specific-trigger"
   :trigger (:on-tag-added "task")  ; Specific tag trigger
   :actions '((:action :update-property :params (:property :status :value "Todo")))))

;; Avoid: Overly generic triggers
(supertag-automation-create
 '(:name "generic-trigger"
   :trigger :on-property-change  ; Generic trigger, requires additional condition filtering
   :condition (has-tag "task")
   :actions '((:action :update-property :params (:property :status :value "Todo")))))
```

#### 2. Condition Logic Optimization

```elisp
;; Good practice: Put most likely to fail conditions first
(supertag-automation-create
 '(:name "optimized-conditions"
   :trigger :on-property-change
   :condition (and 
               (property-equals :priority "High")  ; Specific value match, quick failure
               (has-tag "task")                    ; Tag check
               (:formula "(> (get-property :hours) 8)"))  ; Complex calculation last
   :actions '((:action :add-tag :params (:tag "urgent")))))
```

#### 3. Avoid Circular Dependencies

```elisp
;; Dangerous: Could cause infinite loops
(supertag-automation-create
 '(:name "potential-loop"
   :trigger :on-property-change
   :condition (property-equals :status "Done")
   :actions '((:action :update-property :params (:property :completed_date :value (current-time)))
              (:action :update-property :params (:property :status :value "Archived")))))  ; This will trigger the rule again

;; Safe practice: Use different trigger conditions or avoid modifying trigger properties
(supertag-automation-create
 '(:name "safe-completion"
   :trigger :on-property-change
   :condition (and (property-equals :status "Done")
                   (not (property-exists :completed_date)))  ; Prevent repeated triggering
   :actions '((:action :update-property :params (:property :completed_date :value (current-time))))))
```

### Performance Monitoring Tools

```elisp
;; Rule execution statistics
(defvar supertag--rule-stats (make-hash-table :test 'equal)
  "Rule execution statistics.")

(defun supertag-track-rule-execution (rule-name execution-time)
  "Record rule execution statistics."
  (let ((stats (gethash rule-name supertag--rule-stats '(:count 0 :total-time 0))))
    (puthash rule-name 
             `(:count ,(1+ (plist-get stats :count))
               :total-time ,(+ (plist-get stats :total-time) execution-time))
             supertag--rule-stats)))

(defun supertag-show-performance-report ()
  "Display rule execution performance report."
  (interactive)
  (let ((report '()))
    (maphash (lambda (rule-name stats)
               (let* ((count (plist-get stats :count))
                      (total-time (plist-get stats :total-time))
                      (avg-time (/ total-time count)))
                 (push `(,rule-name ,count ,total-time ,avg-time) report)))
             supertag--rule-stats)
    (setq report (sort report (lambda (a b) (> (nth 2 a) (nth 2 b)))))
    (with-output-to-temp-buffer "*Supertag Performance Report*"
      (princ "Rule Performance Report\n")
      (princ "======================\n\n")
      (princ "Rule Name | Executions | Total Time | Avg Time\n")
      (princ "---------|------------|------------|----------\n")
      (dolist (entry report)
        (princ (format "%s | %d | %.4fs | %.4fs\n"
                       (nth 0 entry) (nth 1 entry) 
                       (nth 2 entry) (nth 3 entry)))))))
```

### Large-Scale Data Optimization

When processing large numbers of nodes, consider the following optimization strategies:

```elisp
;; Use transactions for batch operations
(defun batch-update-with-transaction (node-ids update-func)
  "Batch update nodes in transaction for better performance."
  (supertag-with-transaction
    (dolist (node-id node-ids)
      (funcall update-func node-id))))

;; Deferred calculation and caching
(defvar supertag--computation-cache (make-hash-table :test 'equal))

(defun expensive-computation-with-cache (key compute-func)
  "Expensive calculation with caching."
  (or (gethash key supertag--computation-cache)
      (let ((result (funcall compute-func)))
        (puthash key result supertag--computation-cache)
        result)))
```

---

## 🚨 Error Handling and Troubleshooting

### Common Issues and Solutions

#### Issue 1: Rules Not Executing

**Symptoms**: Automation rules are defined, but don't execute when trigger conditions are met.

**Possible Causes**:
- Trigger doesn't match actual events
- Condition logic not satisfied
- Rules not properly indexed

**Diagnostic Steps**:
```elisp
;; 1. Check if rule is properly indexed
(gethash "your-trigger-key" supertag--rule-index)

;; 2. Check trigger type is correct
(supertag-check-index-status)

;; 3. Test condition logic
(let ((node-id "test-node-id"))
  (and (has-tag "task")
       (property-equals :status "Done")))
```

**Solutions**:
- Confirm trigger format is correct (e.g., `:on-tag-added` vs `(:on-tag-added "task")`)
- Use more specific triggers
- Check that property names and values in conditions are correct
- Rebuild rule index: `(supertag-rebuild-rule-index)`

#### Issue 2: Performance Problems

**Symptoms**: Slow system response, especially during data changes.

**Possible Causes**:
- Using overly generic triggers
- Complex condition logic
- Time-consuming operations in `:call-function`
- Circular dependencies causing repeated triggering

**Diagnostic Steps**:
```elisp
;; Check performance report
(supertag-show-performance-report)

;; Benchmark rule lookup
(supertag-benchmark-rule-lookup 10000)

;; Check rule distribution
(supertag-check-index-status)
```

**Solutions**:
- Use specific triggers instead of generic ones
- Optimize condition logic, putting quick-fail conditions first
- Move time-consuming operations to async tasks
- Check and eliminate circular dependencies

#### Issue 3: Data Inconsistency

**Symptoms**: Some property values are incorrect, or rollup calculations don't match expectations.

**Possible Causes**:
- Race conditions from concurrent modifications
- Logic errors in rollup calculations
- Rule execution order issues

**Diagnostic Steps**:
```elisp
;; Check node properties
(supertag-get `(:nodes ,node-id))

;; Manually recalculate rollup
(supertag-automation-recalculate-all-rollups)

;; Verify relationship data
(supertag-relation-get-related-nodes node-id "relation-name")
```

**Solutions**:
- Use transactions to ensure atomic operations
- Avoid directly modifying storage in automation rules
- Run data consistency checks regularly
- Use synchronization mechanisms to handle concurrent access

### Debug Mode

Enable debug mode to get more detailed log information:

```elisp
;; Enable debug mode
(setq supertag-automation-debug t)

;; Set log level
(setq supertag-log-level 'debug)

;; View debug logs
(switch-to-buffer "*supertag-debug*")
```

### System Health Checks

Run system health checks regularly to ensure data integrity:

```elisp
(defun supertag-system-health-check ()
  "Perform system health check."
  (interactive)
  (let ((issues '()))
    ;; Check rule index integrity
    (unless (hash-table-p supertag--rule-index)
      (push "Rule index is not properly initialized" issues))
    
    ;; Check relationship consistency
    (dolist (relation (supertag-get-all-relations))
      (let ((relation-name (plist-get relation :name)))
        (unless (supertag-relation-validate relation-name)
          (push (format "Relation %s has consistency issues" relation-name) issues))))
    
    ;; Check rollup calculations
    (supertag-automation-recalculate-all-rollups)
    
    ;; Report results
    (if issues
        (message "Health check found %d issues: %s" (length issues) (string-join issues "; "))
      (message "System health check passed successfully"))))
```

---

## 🔄 Migration from Behavior System 1.0

### API Mapping

| 1.0 API | 2.0 API | Description |
|---------|---------|------|
| `supertag-behavior-create` | `supertag-automation-create` | Unified creation interface |
| `supertag-behavior-attach` | *Automatic Indexing* | No manual attachment needed |
| `org-supertag-behavior-register` | `supertag-automation-create` | Modern interface |
| `supertag-behavior-detach` | `supertag-automation-delete` | Delete rules |

### Configuration Structure Changes

#### 1.0 Configuration Format:
```elisp
(org-supertag-behavior-register
 "task"
 '(:trigger :on-property-change
   :condition (property-equals :status "Done")
   :action (:update-property :completed_date (current-time))))
```

#### 2.0 Configuration Format:
```elisp
(supertag-automation-create
 '(:name "complete-task-behavior"
   :trigger :on-property-change
   :condition (and (has-tag "task")
                   (property-equals :status "Done"))
   :actions '((:action :update-property
              :params (:property :completed_date :value (current-time))))))
```

### Migration Steps

#### Step 1: Update Require Statements

```elisp
;; Old version
(require 'supertag-ops-behavior)
(require 'supertag-automation-engine)

;; New version
(require 'supertag-automation)  ; Unified module
```

#### Step 2: Convert Rule Definitions

```elisp
;; Migration script example
(defun migrate-behavior-to-automation ()
  "Migrate behavior rules from old version to new automation system."
  (interactive)
  ;; 1. Collect all old rules
  (let ((old-behaviors (supertag-get-all-behaviors)))  ; Hypothetical API
    (dolist (behavior old-behaviors)
      (let ((tag (plist-get behavior :tag))
            (trigger (plist-get behavior :trigger))
            (condition (plist-get behavior :condition))
            (action (plist-get behavior :action)))
        ;; 2. Convert to new format
        (supertag-automation-create
         `(:name ,(format "migrated-%s-behavior" tag)
           :trigger ,trigger
           :condition (and (has-tag ,tag) ,condition)  ; Add tag condition
           :actions (,(list :action (car action) :params (cdr action)))))  ; Convert action format
        ;; 3. Delete old rule
        (supertag-behavior-delete behavior)))
    (message "Migrated %d behaviors to automation rules" (length old-behaviors))))
```

#### Step 3: Test Verification

```elisp
;; Verify migration results
(defun verify-migration ()
  "Verify that migrated rules work correctly."
  (interactive)
  ;; Create test node
  (let ((test-node (supertag-node-create :title "Migration Test" :tags '("task"))))
    ;; Trigger rule
    (supertag-node-update-property test-node :status "Done")
    ;; Check results
    (let ((completed-date (supertag-node-get-property test-node :completed_date)))
      (if completed-date
          (message "Migration successful: completed_date = %s" completed-date)
        (message "Migration failed: no completed_date set")))
    ;; Clean up test node
    (supertag-node-delete test-node)))
```

### Compatibility Considerations

1. **Single Action vs Multiple Actions**: 1.0 uses `:action`, 2.0 uses `:actions` list
2. **Automatic Indexing**: 2.0 no longer requires manual behavior attachment to tags
3. **Trigger Format**: Some trigger formats have changed
4. **Enhanced Conditions**: Need to explicitly add tag conditions `(has-tag "tag-name")`

---

## 🎯 Best Practices Guide

### Rule Design Principles

#### 1. Single Responsibility Principle
Each rule should handle only one specific scenario, avoiding multiple different logics in a single rule.

```elisp
;; Good practice: Separation of concerns
(supertag-automation-create
 '(:name "set-task-todo"
   :trigger (:on-tag-added "task")
   :actions '((:action :update-property :params (:property :status :value "Todo")))))

(supertag-automation-create
 '(:name "set-task-priority"
   :trigger (:on-tag-added "task")
   :actions '((:action :update-property :params (:property :priority :value "Normal")))))

;; Avoid: Multiple logics in one rule
(supertag-automation-create
 '(:name "setup-task-everything"
   :trigger (:on-tag-added "task")
   :actions '((:action :update-property :params (:property :status :value "Todo"))
              (:action :update-property :params (:property :priority :value "Normal"))
              (:action :call-function :params (:function #'complex-task-setup)))))
```

#### 2. Descriptive Naming
Use descriptive rule names that clearly express the rule's purpose.

```elisp
;; Good practice: Descriptive naming
(supertag-automation-create
 '(:name "auto-archive-completed-tasks"
   :trigger :on-property-change
   :condition (and (has-tag "task") (property-equals :status "Done"))
   :actions '((:action :add-tag :params (:tag "archived")))))

;; Avoid: Vague naming
(supertag-automation-create
 '(:name "rule1"
   :trigger :on-property-change
   :condition (and (has-tag "task") (property-equals :status "Done"))
   :actions '((:action :add-tag :params (:tag "archived")))))
```

#### 3. Precise Triggers
Use the most specific trigger types, avoiding overly generic triggers.

```elisp
;; Good practice: Precise triggers
(supertag-automation-create
 '(:name "handle-project-completion"
   :trigger (:on-tag-added "completed")
   :condition (has-tag "project")
   :actions '((:action :call-function :params (:function #'celebrate-project-completion)))))

;; Avoid: Overly generic triggers
(supertag-automation-create
 '(:name "handle-any-change"
   :trigger :on-property-change
   :condition (and (has-tag "project") (property-equals :status "completed"))
   :actions '((:action :call-function :params (:function #'celebrate-project-completion)))))
```

#### 4. Simplify Conditions
Avoid overly complex logical nesting, moving complex logic to custom functions.

```elisp
;; Good practice: Simplified conditions
(supertag-automation-create
 '(:name "flag-urgent-tasks"
   :trigger :on-property-change
   :condition (and (has-tag "task")
                   (:formula "(is-task-urgent-p (current-node))"))
   :actions '((:action :add-tag :params (:tag "urgent")))))

(defun is-task-urgent-p (node)
  "Check if task is urgent."
  (let ((priority (supertag-node-get-property node :priority))
        (due-date (supertag-node-get-property node :due-date))
        (days-left (days-until due-date)))
    (and (equal priority "High")
         (< days-left 3)
         (not (equal (supertag-node-get-property node :status) "Done")))))

;; Avoid: Complex nested conditions
(supertag-automation-create
 '(:name "complex-urgent-check"
   :trigger :on-property-change
   :condition (and (has-tag "task")
                   (property-equals :priority "High")
                   (:formula "(< (days-until (get-property :due-date)) 3)")
                   (not (property-equals :status "Done")))
   :actions '((:action :add-tag :params (:tag "urgent")))))
```

### Performance Optimization Tips

#### 1. Condition Optimization Order
Put most likely to fail conditions first to achieve quick short-circuiting.

```elisp
;; Good practice: Quick-fail conditions first
(supertag-automation-create
 '(:name "high-priority-task-alert"
   :trigger :on-property-change
   :condition (and 
               (property-equals :priority "Critical")  ; Most selective condition
               (has-tag "task")                        ; Tag check
               (not (property-equals :status "Done"))  ; Status check
               (:formula "(< (days-until (get-property :due-date)) 1)"))  ; Complex calculation last
   :actions '((:action :call-function :params (:function #'send-urgent-alert)))))
```

#### 2. Batch Operations
For operations that need to process large amounts of data, use batch processing patterns.

```elisp
;; Good practice: Batch processing
(defun batch-update-project-status ()
  "Batch update project status."
  (let ((projects-to-update (supertag-query-nodes
                            '(and (has-tag "project")
                                  (property-equals :all-tasks-done t)))))
    (supertag-with-transaction
      (dolist (project projects-to-update)
        (supertag-node-update-property (plist-get project :id) :status "Completed")))))

;; Avoid: Processing one by one
(supertag-automation-create
 '(:name "update-each-project"
   :trigger :on-property-change
   :condition (and (has-tag "project") (property-equals :all-tasks-done t))
   :actions '((:action :update-property :params (:property :status :value "Completed")))))
```

### Data Modeling Best Practices

#### 1. Reasonable Tag Hierarchy
Design a clear tag hierarchy structure, avoiding overly complex nesting.

```elisp
;; Good practice: Clear hierarchy
;; Basic tags: task, project, note
;; Status tags: todo, doing, done, archived
;; Priority tags: low, normal, high, critical

;; Avoid: Overly complex nesting
;; task-personal-work-high-priority-urgent-due-tomorrow
```

#### 2. Standardized Field Naming
Use consistent field naming conventions.

```elisp
;; Good practice: Standardized naming
;; Date fields: created_date, due_date, completed_date
;; Status fields: status, priority, progress
;; Relationship fields: parent_id, assigned_to, depends_on

;; Avoid: Inconsistent naming
;; create_time, dueDate, finished_at, stat, prio, prog
```

### Maintenance and Monitoring

#### 1. Regular Health Checks
Establish regular system health check mechanisms.

```elisp
;; Daily health check
(supertag-automation-create
 '(:name "daily-health-check"
   :trigger :on-schedule
   :schedule (:type :daily :time "02:00")
   :actions '((:action :call-function :params (:function #'supertag-system-health-check)))))
```

#### 2. Monitor Rule Execution
Track rule execution performance and frequency.

```elisp
;; Performance monitoring report
(supertag-automation-create
 '(:name "weekly-performance-report"
   :trigger :on-schedule
   :schedule (:type :daily :time "09:00" :days-of-week '(1))  ; Every Monday
   :actions '((:action :call-function :params (:function #'supertag-show-performance-report)))))
```

#### 3. Data Backup Strategy
Regularly back up important configurations and data.

```elisp
(defun backup-supertag-configuration ()
  "Backup Supertag configuration."
  (interactive)
  (let ((backup-file (format "supertag-backup-%s.el" 
                            (format-time-string "%Y%m%d-%H%M%S"))))
    (with-temp-file backup-file
      (insert ";; Supertag Configuration Backup\n")
      (insert ";; Generated: " (current-time-string) "\n\n")
      ;; Backup tag definitions
      (insert "(setq supertag-tags-backup\n")
      (pp (supertag-get-all-tags) (current-buffer))
      (insert ")\n\n")
      ;; Backup relationship definitions
      (insert "(setq supertag-relations-backup\n")
      (pp (supertag-get-all-relations) (current-buffer))
      (insert ")\n\n")
      ;; Backup automation rules
      (insert "(setq supertag-automation-rules-backup\n")
      (pp (supertag-get-all-automation-rules) (current-buffer))
      (insert ")\n"))
    (message "Configuration backed up to %s" backup-file)))
```
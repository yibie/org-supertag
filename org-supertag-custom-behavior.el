;;; org-supertag-custom-behavior.el --- User defined behavior -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic behaviors serve as the system's "primitives"
;; 
;; This file is for defining your custom behaviors
;; 
;; After org-supertag is configured, this file will be automatically copied
;; to your org-supertag directory under .emacs.d

;; 
;; Trigger Types:
;; -------------
;; :on-add      - Execute when tag is added to a node
;; :on-remove   - Execute when tag is removed from a node
;; :on-change   - Execute when node with tag is modified
;; :on-schedule - Execute at scheduled times
;; :always      - Execute on all events
;;
;; Hook Points:
;; -----------
;; 1. Org Mode Standard Hooks:
;;    - org-after-todo-state-change-hook
;;    - org-property-changed-functions
;;    - org-after-tags-change-hook
;;    - org-timestamp-change-hook
;;    - org-cycle-hook
;;
;; 2. Supertag Specific Hooks:
;;    - org-supertag-after-tag-apply-hook
;;    - org-supertag-after-node-change-hook
;;    - org-supertag-after-tag-add-hook
;;    - org-supertag-after-tag-remove-hook
;;    - org-supertag-after-load-hook
;;
;; Example Behavior Definitions:
;;
;; 1. Basic behavior with direct action:
;; (org-supertag-behavior-register "@custom-todo"
;;   :trigger :on-add
;;   :action #'org-supertag-behavior--set-todo
;;   :params '(state)
;;   :style '(:face (:foreground "blue" :weight bold)
;;           :prefix "☐"))
;;
;; 2. Behavior with parameter handling:
;; (org-supertag-behavior-register "@custom-priority"
;;   :trigger :on-change
;;   :action #'org-supertag-behavior--set-priority
;;   :params '(priority)
;;   :style '(:face (:foreground "red")
;;           :prefix "★"))
;;
;; 3. Workflow behavior using behavior list:
;; (org-supertag-behavior-register "@custom-done"
;;   :trigger :on-add
;;   :list '("@todo=DONE"
;;           "@property=COMPLETED_TIME,${date:now}"))
;;   :style '(:face (:foreground "green")
;;           :prefix "✓"))
;;
;; 4. Behavior with hooks:
;; (org-supertag-behavior-register "@custom-track"
;;   :trigger :on-change
;;   :action #'org-supertag-behavior--set-property
;;   :params '(name value)
;;   :hooks '((org-after-todo-state-change-hook . my-track-function)
;;           (org-after-tags-change-hook . my-tag-function))
;;   :style '(:face (:foreground "purple")
;;           :prefix "◉"))
;;
;; Hook Function Example:
;; (defun my-track-function ()
;;   "Example hook function for tracking changes.
;; This function will be called when TODO state changes."
;;   (when-let* ((node-id (org-id-get))
;;               (new-state (org-get-todo-state)))
;;     (message "Node %s changed to state: %s" node-id new-state)))
;;
;; Behavior System Architecture:
;; -------------------------
;; The behavior system is organized in three layers:
;;
;; 1. Basic Behaviors (Foundation Layer)
;;    - Atomic, single-purpose functions
;;    - Direct interface with org-mode operations
;;    - Highly parameterized for flexibility
;;    - Examples: @todo, @priority, @property
;;    - Used as building blocks for higher layers
;;
;; 2. Shortcut Behaviors (Specialization Layer)
;;    - Built on top of basic behaviors
;;    - Pre-configured parameter sets
;;    - Specialized for specific use cases
;;    - Examples: @done (@todo=DONE), @urgent (@priority=A)
;;    - Simplify common operations
;;
;; 3. Workflow Behaviors (Integration Layer)
;;    - Compose multiple behaviors together
;;    - Create complex workflows
;;    - Chain actions in meaningful sequences
;;    - Examples: @done+archive (@todo=DONE + @archive)
;;    - Implement higher-level functionality
;;
;; Design Guidelines:
;; ----------------
;; 1. When creating new behaviors:
;;    - Start with basic behaviors for new primitive operations
;;    - Create derived behaviors for common parameter combinations
;;    - Use combined behaviors for workflow automation
;;
;; 2. Behavior Properties:
;;    - Each behavior should have a clear, single responsibility
;;    - Parameters should be meaningful and well-documented
;;    - Visual styling should indicate behavior type
;;    - Hooks and triggers should be appropriate for the use case
;;
;; 3. Development Flow:
;;    - First develop and test basic behaviors
;;    - Then create derived behaviors for common use cases
;;    - Finally compose combined behaviors for workflows

;;; Code:

(require 'org-supertag-behavior)
(require 'org-supertag-behavior-library)

(defcustom my/ai-generate-topics-prompt
  "我是一个专注于\"用AI提升人生体验\"的创作者。\n请为我生成5个具体、可操作的小选题，要求：\n- 聚焦个人成长、体验优化、创造力释放、情绪能量管理\n- 每个选题都小到可以用一篇短文或一条动态阐述\n- 尽量具体而非抽象，例如\"如何用ChatGPT优化晨间例行\"而不是\"AI和习惯养成\"\n- 带一点轻盈感或哲思感，避免太重学术气息"
  "用于 @GenerateTopics 行为的默认 AI Prompt。"
  :type 'string
  :group 'org-supertag-behavior) ; 或者你自己的 custom group

;;------------------------------------------------------------------------------
;; Basic Behaviors - Core functionality
;;------------------------------------------------------------------------------

;; Node Movement
(org-supertag-behavior-register "@move"
  :trigger :on-add
  :action #'org-supertag-behavior--move-node
  :params '(target-file)
  :description "Move node to target file")

;; TODO State Management
(org-supertag-behavior-register "@todo"
  :trigger :on-add
  :action #'org-supertag-behavior--set-todo
  :params '(state)
  :description "Set TODO state for node")

;; Priority Management
(org-supertag-behavior-register "@priority"
  :trigger :on-add
  :action #'org-supertag-behavior--set-priority
  :params '(priority)
  :description "Set priority for node")

;; Property Management
(org-supertag-behavior-register "@property"
  :trigger :on-add
  :action #'org-supertag-behavior--set-property
  :params '(name value)
  :description "Set property for node")

;; Clock Management
(org-supertag-behavior-register "@clock"
  :trigger :on-add
  :action #'org-supertag-behavior--clock-in
  :params '(switch-state)
  :description "Control node clock")

;; State Toggle
(org-supertag-behavior-register "@state"
  :trigger :on-add
  :action #'org-supertag-behavior--toggle-state
  :params '(states)
  :description "Toggle between specified states")

;; Deadline Check
(org-supertag-behavior-register "@deadline-check"
  :trigger :on-add
  :action #'org-supertag-behavior--check-deadline
  :params '(scope days action)
  :description "Check node deadline")

;; State Propagation
(org-supertag-behavior-register "@propagate"
  :trigger :on-add
  :action #'org-supertag-behavior--propagate-state
  :params '(state recursive)
  :description "Propagate state to child nodes")

;; Drawer Management
(org-supertag-behavior-register "@drawer"
  :trigger :on-add
  :action #'org-supertag-behavior--insert-drawer
  :params '(name content region)
  :description "Manage node drawers")

;; Log Management
(org-supertag-behavior-register "@log"
  :trigger :on-add
  :action #'org-supertag-behavior--log-into-drawer
  :params '(enabled name note)
  :description "Configure node logging")

;; Archive Management
(org-supertag-behavior-register "@archive"
  :trigger :on-add
  :action #'org-supertag-behavior--archive-subtree
  :description "Archive node")

;; Node Operations
(org-supertag-behavior-register "@children"
  :trigger :on-change
  :action #'org-supertag-behavior--get-children
  :description "Get children node information")

;; Parent Node Search
(org-supertag-behavior-register "@parent"
  :trigger :on-add
  :action #'org-supertag-behavior--find-parent-with-tag
  :params '(tag-id)
  :description "Find parent node with specific tag")

;; Heading Management
(org-supertag-behavior-register "@heading"
  :trigger :on-add
  :action #'org-supertag-behavior--set-heading
  :params '(title)
  :description "Modify node title")

;; Progress Calculation
(org-supertag-behavior-register "@progress"
  :trigger :on-change
  :action #'org-supertag-behavior--calculate-progress
  :description "Calculate node progress")

;;------------------------------------------------------------------------------
;; Shortcut Behaviors - Based on Basic Behaviors
;;------------------------------------------------------------------------------

;; Task State Shortcuts
(org-supertag-behavior-register "@done"
  :trigger :on-add
  :list '("@todo=DONE")
  :description "Mark node as done"
  :style '(:face (:foreground "green" :weight bold)
          :prefix "✓"))

(org-supertag-behavior-register "@start"
  :trigger :on-add
  :list '("@todo=DOING")
  :description "Start processing node"
  :style '(:prefix "▶"))

;; Priority Shortcuts
(org-supertag-behavior-register "@urgent"
  :trigger :on-add
  :list '("@priority=A")
  :description "Mark node as urgent"
  :style '(:face (:foreground "red" :weight bold)
          :prefix "🔥"))

;; Time-Related Shortcuts
(org-supertag-behavior-register "@deadline"
  :trigger :on-add
  :list '("@property=DEADLINE,${date:now+1d}")
  :description "Set node deadline")

(org-supertag-behavior-register "@scheduled"
  :trigger :on-add
  :list '("@property=SCHEDULED,${date:now}")
  :description "Set node scheduled time")

;; Move-Related Shortcuts
(org-supertag-behavior-register "@move-to-project"
  :trigger :on-add
  :list '("@move=/Users/chenyibin/Documents/notes/project.org")
  :description "Move node to project file"
  :style '(:prefix "📁"))

;;------------------------------------------------------------------------------
;; Workflow Behaviors - Complex Functionality
;;------------------------------------------------------------------------------

;; Complete and Archive
(org-supertag-behavior-register "@done+archive"
  :trigger :on-add
  :list '("@todo=DONE"
          "@property=COMPLETED_TIME,${date:now}"
          "@archive")
  :description "Complete and archive node"
  :style '(:face (:foreground "gray50")
          :prefix "📦"))

;; Start Task and Clock In
(org-supertag-behavior-register "@start+clock"
  :trigger :on-add
  :list '("@todo=DOING"
          "@property=STARTED_TIME,${date:now}"
          "@clock=start")
  :description "Start task and clock in"
  :style '(:prefix "⏱"))

;; Urgent Task
(org-supertag-behavior-register "@urgent+deadline"
  :trigger :on-add
  :list '("@priority=A"
          "@property=DEADLINE,${date:now+1d}"
          "@property=URGENT,true")
  :description "Create urgent task"
  :style '(:face (:foreground "red" :weight bold)
          :prefix "🚨"))

;; AI Content Generation
(org-supertag-behavior-register "@GenerateTopics"
  :trigger :on-add
  :action #'org-supertag-behavior--generate-topics-action
  :description "Generate content topics with AI"
  :style '(:prefix "🤖"))

;;------------------------------------------------------------------------------
;; Command Execution Behaviors
;;------------------------------------------------------------------------------

;; Basic Command Execution
(org-supertag-behavior-register "@execute"
  :trigger :on-schedule
  :action #'org-supertag-behavior--execute-command
  :params '(command args)
  :description "Execute an Emacs command")


;;Auto run org-zettel-ref-run-python-script
(org-supertag-behavior-register "@run-python-script"
  :trigger :on-schedule
  :schedule "0 22 * * *"  ; Every 5 minutes
  :action #'org-zettel-ref-run-python-script
  :description "Run Python script every 5 minutes")


;;------------------------------------------------------------------------------
;; Script Execution Behaviors
;;------------------------------------------------------------------------------

;; Base script execution (async by default)
(org-supertag-behavior-register "@script"
  :trigger :on-add
  :action #'org-supertag-behavior--execute-script
  :params '(:script-path "${prop:SCRIPT_PATH}"
            :async t)
  :description "Execute script asynchronously from SCRIPT_PATH property")

;; Sync script execution (when you need to wait for result)
(org-supertag-behavior-register "@script-sync"
  :trigger :on-add
  :action #'org-supertag-behavior--execute-script
  :params '(:script-path "${prop:SCRIPT_PATH}"
            :async nil)
  :description "Execute script synchronously from SCRIPT_PATH property")

;; Python script with callback
(org-supertag-behavior-register "@python"
  :trigger :on-add
  :action #'org-supertag-behavior--execute-script
  :params '(:script-path "${prop:SCRIPT_PATH}"
            :async t
            :callback org-supertag-behavior--script-completion-callback)
  :description "Execute Python script with completion notification")

;; Scheduled Python execution
(org-supertag-behavior-register "@python-schedule"
  :trigger :on-schedule
  :schedule "0 23 * * *"
  :action #'org-supertag-behavior--execute-script
  :params '(:script-path "${prop:SCRIPT_PATH}"
            :async t
            :callback org-supertag-behavior--scheduled-script-callback)
  :description "Execute Python script daily at 23:00")

;; Shell script execution
(org-supertag-behavior-register "@shell"
  :trigger :on-add
  :action #'org-supertag-behavior--execute-script
  :params '(:script-path "${prop:SCRIPT_PATH}"
            :async t)
  :description "Execute Shell script asynchronously")

;; Quick data processing (your lex-scraper example)
(org-supertag-behavior-register "@lex-scraper"
  :trigger :on-add
  :action #'org-supertag-behavior--execute-script
  :params '(:script-path "~/Documents/prj/lex-scraper/lex-scraper.py"
            :async t
            :callback org-supertag-behavior--lex-scraper-callback)
  :description "Run lex-scraper and notify when complete"
  :style '(:prefix "📺"))

;; Development script runner
(org-supertag-behavior-register "@dev-script"
  :trigger :on-add
  :action #'org-supertag-behavior--execute-script
  :params '(:script-path "${prop:SCRIPT_PATH}"
            :args ("${prop:SCRIPT_ARGS}")
            :async t
            :callback org-supertag-behavior--dev-script-callback)
  :description "Run development script with arguments"
  :style '(:prefix "🔧"))

;; Data processing with smart callback
(org-supertag-behavior-register "@data-process"
  :trigger :on-add
  :action #'org-supertag-behavior--execute-script
  :params '(:script-path "${prop:SCRIPT_PATH}"
            :async t
            :callback org-supertag-behavior--data-processing-callback)
  :description "Run data processing script with statistics"
  :style '(:prefix "📊"))

;; Backup script
(org-supertag-behavior-register "@backup"
  :trigger :on-add
  :action #'org-supertag-behavior--execute-script
  :params '(:script-path "${prop:SCRIPT_PATH}"
            :async t
            :callback org-supertag-behavior--backup-script-callback)
  :description "Run backup script with file count summary"
  :style '(:prefix "💾"))

(provide 'org-supertag-custom-behavior)
;;; org-supertag-custom-behavior.el ends here 
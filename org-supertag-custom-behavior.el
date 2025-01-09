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
;; 3. Combined behavior using behavior list:
;; (org-supertag-behavior-register "@custom-done"
;;   :trigger :on-add
;;   :list '("@todo=DONE"
;;           "@property=COMPLETED_TIME=now")
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
;; 2. Derived Behaviors (Specialization Layer)
;;    - Built on top of basic behaviors
;;    - Pre-configured parameter sets
;;    - Specialized for specific use cases
;;    - Examples: @done (@todo=DONE), @urgent (@priority=A)
;;    - Simplify common operations
;;
;; 3. Combined Behaviors (Integration Layer)
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

;;------------------------------------------------------------------------------
;; Basic Behaviors - Core functionality
;;------------------------------------------------------------------------------

;; Node Movement - Basic move operation
(org-supertag-behavior-register "@move"
  :trigger :on-add
  :action #'org-supertag-behavior--move-node
  :params '(target-file target-level mode keep-tags add-tags))

;; 1. Task State - Basic behavior, other states through parameters
(org-supertag-behavior-register "@todo"
  :trigger :on-add
  :action #'org-supertag-behavior--set-todo
  :params '(state))

;; 2. Priority - Set specific levels through parameters
(org-supertag-behavior-register "@priority"
  :trigger :on-add
  :action #'org-supertag-behavior--set-priority
  :params '(priority))

;; 3. Timestamp - Different types through parameters
(org-supertag-behavior-register "@timestamp"
  :trigger :on-add
  :action #'org-supertag-behavior--set-property
  :params '(name value))

;; 4. Property Setting - Set any property through parameters
(org-supertag-behavior-register "@property"
  :trigger :on-add
  :action #'org-supertag-behavior--set-property
  :params '(name value))

;; 5. Clock Management - Control through parameters
(org-supertag-behavior-register "@clock"
  :trigger :on-add
  :action #'org-supertag-behavior--clock-in
  :params '(switch-state))

;; State Toggle Basic Behavior
(org-supertag-behavior-register "@state"
  :trigger :on-add
  :action #'org-supertag-behavior--toggle-state
  :params '(states))

;; State Propagation Basic Behavior
(org-supertag-behavior-register "@propagate"
  :trigger :on-add
  :action #'org-supertag-behavior--propagate-state
  :params '(state recursive))

;; Drawer Management Basic Behavior
(org-supertag-behavior-register "@drawer"
  :trigger :on-add
  :action #'org-supertag-behavior--insert-drawer
  :params '(name content region))

;; Log Drawer Basic Behavior
(org-supertag-behavior-register "@log"
  :trigger :on-add
  :action #'org-supertag-behavior--log-into-drawer
  :params '(enabled name note))

;; Clock Report Basic Behavior
(org-supertag-behavior-register "@report"
  :trigger :on-add
  :action #'org-supertag-behavior--clock-report
  :params '(scope range))

;; Clock Control Basic Behavior (clock-out and cancel)
(org-supertag-behavior-register "@clock-out"
  :trigger :on-add
  :action #'org-supertag-behavior--clock-out
  :params '(switch-state note))

;; Archive Basic Behavior
(org-supertag-behavior-register "@archive"
  :trigger :on-add
  :action #'org-supertag-behavior--archive-subtree
  :params '(location mark-done save-context))

;; Archive Location Basic Behavior
(org-supertag-behavior-register "@archive-to"
  :trigger :on-add
  :action #'org-supertag-behavior--set-archive-location
  :params '(file headline scope inherit-tags))

;; 7. Node Operations - Get Child Node Information
(org-supertag-behavior-register "@children"
  :trigger :on-change
  :action #'org-supertag-behavior--get-children)

;; 8. Parent Node Search - Find Parent with Specific Tag
(org-supertag-behavior-register "@parent"
  :trigger :on-add
  :action #'org-supertag-behavior--find-parent-with-tag
  :params '(tag-id))

;; 6. Heading Management - Modify Heading Text
(org-supertag-behavior-register "@heading"
  :trigger :on-add
  :action #'org-supertag-behavior--set-heading
  :params '(title))

;; 9. Progress Calculation - Based on Child Task States
(org-supertag-behavior-register "@progress"
  :trigger :on-change
  :action #'org-supertag-behavior--calculate-progress)



;;------------------------------------------------------------------------------
;; Derived Behaviors - Based on Basic Behaviors
;;------------------------------------------------------------------------------

;; 1. Task State Derivatives
(org-supertag-behavior-register "@done"
  :trigger :on-add
  :list '("@todo=DONE")
  :style '(:face (:foreground "green" :weight bold)
          :prefix "✓"))

(org-supertag-behavior-register "@start"
  :trigger :on-add
  :list '("@todo=STARTED")
  :style '(:face (:foreground "orange" :weight bold)
          :prefix "▶"))

(org-supertag-behavior-register "@cancel"
  :trigger :on-add
  :list '("@todo=CANCELLED")
  :style '(:face (:foreground "gray" :strike-through t)
          :prefix "✗"))

;; 2. Priority Derivatives
(org-supertag-behavior-register "@urgent"
  :trigger :on-add
  :list '("@priority=A")
  :style '(:face (:foreground "red" :weight bold)
          :prefix "⚠"))

(org-supertag-behavior-register "@low"
  :trigger :on-add
  :list '("@priority=C")
  :style '(:face (:foreground "gray")
          :prefix "▽"))

;; 3. Time-Related Derivatives
(org-supertag-behavior-register "@deadline"
  :trigger :on-add
  :list '("@timestamp=DEADLINE")
  :style '(:face (:foreground "red")
          :prefix "⏰"))

(org-supertag-behavior-register "@scheduled"
  :trigger :on-add
  :list '("@timestamp=SCHEDULED")
  :style '(:face (:foreground "blue")
          :prefix "📅"))

;;------------------------------------------------------------------------------
;; Combined Behaviors - Complex Functionality
;;------------------------------------------------------------------------------

;; 1. Complete and Archive
(org-supertag-behavior-register "@done+archive"
  :trigger :on-add
  :list '("@todo=DONE"                        ; Set state to DONE
          "@property=ARCHIVE_TIME=now"         ; Set archive timestamp
          "@archive")                          ; Execute archive
  :style '(:face (:foreground "gray50" :strike-through t)
          :prefix "📦"))

;; 2. Start Task and Clock In
(org-supertag-behavior-register "@start+clock"
  :trigger :on-add
  :list '("@todo=STARTED" "@clock=start")
  :style '(:face (:foreground "orange" :weight bold)
          :prefix "⏱"))

;; 3. Urgent Task (High Priority + Deadline)
(org-supertag-behavior-register "@urgent+deadline"
  :trigger :on-add
  :list '("@priority=A" "@deadline")
  :style '(:face (:foreground "red" :weight bold)
          :prefix "🚨"))

;; 4. Project Node
(org-supertag-behavior-register "@project"
  :trigger :on-add
  :list '("@property=CATEGORY=PROJECT" "@property=PROJECT_ID=auto")
  :style '(:face (:foreground "blue" :weight bold)
          :prefix "📂"))

(provide 'org-supertag-custom-behavior)
;;; org-supertag-custom-behavior.el ends here 
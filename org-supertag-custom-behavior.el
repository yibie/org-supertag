;;; org-supertag-custom-behavior.el --- User defined behavior -*- lexical-binding: t; -*-

;;; Commentary:

;; 基础行为是系统的"原语"
;; 
;; 这个文件用于定义你的自定义行为
;; 
;; 在 org-supertag 配置好之后，该文件将自动复制到你的 .emacs.d 目录对应的 org-supertag 目录下
;;
;; 行为定义示例:
;;
;; (org-supertag-behavior-register "@my-behavior"
;;   :trigger :on-change
;;   :action (lambda ()
;;            ;; 你的行为逻辑
;;            )
;;   :style '(:face (:foreground "blue")))
;;
;; 行为组合示例:
;;
;; (org-supertag-behavior-register "@combined"
;;   :trigger :on-change
;;   :action (list "@behavior1" "@behavior2"))
;; 
;; 设计原则：
;; 1. 基础行为：实现单一、原子的功能，并接受参数
;; 2. 组合行为：通过参数化和组合基础行为实现复杂功能
;; 3. 清晰文档：每个行为都有清晰的文档说明其用途和参数

;;; Code:

(require 'org-supertag-behavior)
(require 'org-supertag-behavior-library)

;;------------------------------------------------------------------------------
;; Basic Behaviors - 基础行为
;;------------------------------------------------------------------------------

;; 1. 任务状态 - 最基础的行为，其他状态通过参数实现
(org-supertag-behavior-register "@todo"
  :trigger :on-add
  :action #'org-supertag-behavior--set-todo
  :params '(state)
  :style '(:face (:foreground "blue" :weight bold)
          :prefix "☐"))

;; 2. 优先级 - 通过参数设置具体级别
(org-supertag-behavior-register "@priority"
  :trigger :on-add
  :action #'org-supertag-behavior--set-priority
  :params '(priority)
  :style '(:face (:foreground "orange")
          :prefix "★"))

;; 3. 时间戳 - 通过参数设置不同类型
(org-supertag-behavior-register "@timestamp"
  :trigger :on-add
  :action #'org-supertag-behavior--set-property
  :params '(name value)
  :style '(:face (:foreground "gray50")
          :prefix "⏰"))

;; 4. 属性设置 - 通过参数设置任意属性
(org-supertag-behavior-register "@property"
  :trigger :on-add
  :action #'org-supertag-behavior--set-property
  :params '(name value)
  :style '(:face (:foreground "purple")
          :prefix "⚑"))

;; 5. 时钟管理 - 通过参数控制行为
(org-supertag-behavior-register "@clock"
  :trigger :on-add
  :action #'org-supertag-behavior--clock-in
  :params '(switch-state)
  :style '(:face (:foreground "green")
          :prefix "⏱"))

;; 状态切换基础行为
(org-supertag-behavior-register "@state"
  :trigger :on-add
  :action #'org-supertag-behavior--toggle-state
  :params '(states)
  :style '(:face (:foreground "purple")
          :prefix "↺"))

;; 状态传播基础行为
(org-supertag-behavior-register "@propagate"
  :trigger :on-add
  :action #'org-supertag-behavior--propagate-state
  :params '(state recursive)
  :style '(:face (:foreground "blue")
          :prefix "⇊"))

;; 抽屉管理基础行为
(org-supertag-behavior-register "@drawer"
  :trigger :on-add
  :action #'org-supertag-behavior--insert-drawer
  :params '(name content region)
  :style '(:face (:foreground "gray50")
          :prefix "▤"))

;; 日志抽屉基础行为
(org-supertag-behavior-register "@log"
  :trigger :on-add
  :action #'org-supertag-behavior--log-into-drawer
  :params '(enabled name note)
  :style '(:face (:foreground "gray70")
          :prefix "📝"))

;; 时钟报告基础行为
(org-supertag-behavior-register "@report"
  :trigger :on-add
  :action #'org-supertag-behavior--clock-report
  :params '(scope range)
  :style '(:face (:foreground "blue")
          :prefix "📊"))

;; 时钟控制基础行为（补充 clock-out 和 cancel）
(org-supertag-behavior-register "@clock-out"
  :trigger :on-add
  :action #'org-supertag-behavior--clock-out
  :params '(switch-state note)
  :style '(:face (:foreground "red")
          :prefix "⏹"))

;; 归档基础行为
(org-supertag-behavior-register "@archive"
  :trigger :on-add
  :action #'org-supertag-behavior--archive-subtree
  :params '(location mark-done save-context)
  :style '(:face (:foreground "gray50")
          :prefix "📦"))

;; 归档位置基础行为
(org-supertag-behavior-register "@archive-to"
  :trigger :on-add
  :action #'org-supertag-behavior--set-archive-location
  :params '(file headline scope inherit-tags)
  :style '(:face (:foreground "gray70")
          :prefix "📍"))

;; 7. 节点操作 - 获取子节点信息
(org-supertag-behavior-register "@children"
  :trigger :on-change
  :action #'org-supertag-behavior--get-children
  :style '(:face (:foreground "blue")
          :prefix "⚏"))

;; 8. 父节点查找 - 查找特定标签的父节点
(org-supertag-behavior-register "@parent"
  :trigger :on-add
  :action #'org-supertag-behavior--find-parent-with-tag
  :params '(tag-id)
  :style '(:face (:foreground "purple")
          :prefix "⤴"))

;; 6. 标题管理 - 修改标题文本
(org-supertag-behavior-register "@heading"
  :trigger :on-add
  :action #'org-supertag-behavior--set-heading
  :params '(title)
  :style '(:face (:foreground "cyan")
          :prefix "✎"))

;; 9. 进度计算 - 基于子任务状态
(org-supertag-behavior-register "@progress"
  :trigger :on-change
  :action #'org-supertag-behavior--calculate-progress
  :style '(:face (:foreground "green")
          :prefix "📊"))

;;------------------------------------------------------------------------------
;; Derived Behaviors - 派生行为（基于基础行为）
;;------------------------------------------------------------------------------

;; 1. 任务状态派生
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

;; 2. 优先级派生
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

;; 3. 时间相关派生
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
;; Combined Behaviors - 组合行为
;;------------------------------------------------------------------------------

;; 1. 完成并归档
(org-supertag-behavior-register "@done+archive"
  :trigger :on-add
  :list '("@todo=DONE"                        ; 设置状态为 DONE
          "@property=ARCHIVE_TIME=now"         ; 设置归档时间
          "@archive")                          ; 执行归档
  :style '(:face (:foreground "gray50" :strike-through t)
          :prefix "📦"))

;; 2. 开始任务并计时
(org-supertag-behavior-register "@start+clock"
  :trigger :on-add
  :list '("@todo=STARTED" "@clock=start")
  :style '(:face (:foreground "orange" :weight bold)
          :prefix "⏱"))

;; 3. 紧急任务（高优先级+截止时间）
(org-supertag-behavior-register "@urgent+deadline"
  :trigger :on-add
  :list '("@priority=A" "@deadline")
  :style '(:face (:foreground "red" :weight bold)
          :prefix "🚨"))

;; 4. 项目节点
(org-supertag-behavior-register "@project"
  :trigger :on-add
  :list '("@property=CATEGORY=PROJECT" "@property=PROJECT_ID=auto")
  :style '(:face (:foreground "blue" :weight bold)
          :prefix "📂"))

(provide 'org-supertag-custom-behavior)
;;; org-supertag-custom-behavior.el ends here 
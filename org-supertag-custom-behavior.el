;;; org-supertag-custom-behavior.el --- User defined behavior -*- lexical-binding: t; -*-

;;; Commentary:
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

;;; Code:

(require 'org-supertag-behavior)

;;------------------------------------------------------------------------------
;; 基础行为
;;------------------------------------------------------------------------------

;; 归档行为 :TODO 需要查看 org-supertag-behavior--do-archive 的实现
(org-supertag-behavior-register "@archive"
  :trigger :on-add
  :action (lambda (node-id)
            (org-supertag-behavior--do-archive node-id))
  :style '(:face (:foreground "gray50")
          :prefix "📦"))

;; 重要标记行为
(org-supertag-behavior-register "@important"
  :trigger :always
  :style '(:face (:foreground "red" :weight bold :background "yellow")
          :prefix "🔥"))

;; TODO 状态管理
(org-supertag-behavior-register "@todo"
  :trigger :on-add
  :action (lambda () (org-todo "TODO")))

;; DONE 状态管理
(org-supertag-behavior-register "@done"
  :trigger :on-add
  :action (lambda () (org-todo "DONE")))

;; 高优先级
(org-supertag-behavior-register "@priority-a"
  :trigger :on-add
  :action (lambda () (org-priority ?A))
  )

;; 低优先级
(org-supertag-behavior-register "@priority-c"
  :trigger :on-add
  :action (lambda () (org-priority ?C))
 )

;; 添加截止日期
(org-supertag-behavior-register "@deadline"
  :trigger :on-add
  :action (lambda () (call-interactively 'org-deadline))
  :style '(:face (:foreground "orange")
          :prefix "⏰"))

;; 添加计划时间
(org-supertag-behavior-register "@scheduled"
  :trigger :on-add
  :action (lambda () (call-interactively 'org-schedule))
  :style '(:face (:foreground "purple")
          :prefix "📅"))

;; 添加创建时间
(org-supertag-behavior-register "@created"
  :trigger :on-add
  :action (lambda ()
            (org-set-property "CREATED" 
                            (format-time-string "[%Y-%m-%d %a %H:%M]")))
  :style '(:face (:foreground "gray50")))

;; 添加作者
(org-supertag-behavior-register "@author"
  :trigger :on-add
  :action (lambda ()
            (org-set-property "AUTHOR" user-full-name)))

;;------------------------------------------------------------------------------
;; 项目管理行为
;;------------------------------------------------------------------------------

;; 1. 进度计算行为
(org-supertag-behavior-register "@progress-calc"
  :trigger :on-change
  :action (lambda ()
            (let* ((progress-data (org-supertag-behavior--calculate-progress))
                   (progress (nth 2 progress-data)))
              (org-entry-put nil "Progress" 
                            (format "%.1f" progress))
              progress)))

;; 2. 进度显示行为
(org-supertag-behavior-register "@progress-display"
  :trigger :on-change
  :action (lambda ()
            (when-let* ((progress-str (org-entry-get nil "Progress"))
                       (progress (string-to-number progress-str)))
              (let* ((title (org-get-heading t t t t))
                     (new-title (org-supertag-behavior--update-progress-display 
                               title progress)))
                (org-edit-headline new-title)))))

;;------------------------------------------------------------------------------
;; 属性管理行为
;;------------------------------------------------------------------------------

;; 继承属性行为
(org-supertag-behavior-register "@inherit"
  :trigger :always
  :action (lambda ()
            (let ((inherited-props (org-entry-properties nil 'inherited)))
              (dolist (prop inherited-props)
                (unless (org-entry-get nil (car prop))
                  (org-entry-put nil (car prop) (cdr prop))))))
  :style '(:face (:foreground "gray70")
          :prefix "⤴"))

;; 强制 ID 属性
(org-supertag-behavior-register "@force-id"
  :trigger :on-add
  :action (lambda ()
            (unless (org-id-get)
              (org-id-get-create)))
  :style '(:face (:foreground "gray50")))

;; 加密属性
(org-supertag-behavior-register "@encrypt"
  :trigger :on-add
  :action (lambda ()
            (org-set-property "CRYPTKEY" 
                            (or (getenv "CRYPTKEY") 
                                (read-string "Enter encryption key: ")))
            (org-encrypt-entry))
  :style '(:face (:foreground "purple")
          :prefix "🔒"))

;;------------------------------------------------------------------------------
;; 统计和分析行为
;;------------------------------------------------------------------------------

;; 子树统计
(org-supertag-behavior-register "@count-todos"
  :trigger :on-change
  :action (lambda ()
            (let ((count (length 
                         (org-map-entries t "/+TODO" 'tree))))
              (org-set-property "TODO_COUNT" 
                              (number-to-string count))))
  :style '(:face (:foreground "cyan")
          :prefix "📊"))

;; 完成率统计
(org-supertag-behavior-register "@completion-rate"
  :trigger :on-change
  :action (lambda ()
            (let* ((total (length (org-map-entries t nil 'tree)))
                   (done (length (org-map-entries t "/+DONE" 'tree)))
                   (rate (if (> total 0)
                            (* 100.0 (/ done total))
                          0.0)))
              (org-set-property "COMPLETION_RATE" 
                              (format "%.1f%%" rate))))
  :style '(:face (:foreground "green")
          :prefix "📈"))


;;------------------------------------------------------------------------------
;; 你的自定义行为可添加在这里
;;------------------------------------------------------------------------------  

(provide 'org-supertag-custom-behaviors) 
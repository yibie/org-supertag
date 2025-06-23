;;; org-supertag-behavior-template.el --- Behavior template system -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; Implements template system for org-supertag behaviors.
;; Supports variables like:
;; - ${input:prompt} - User input
;; - ${date:format} - Date functions
;; - ${prop:property} - Property values
;; - {if:condition} - Conditional checks

;;; Code:

;;------------------------------------------------------------------------------
;; Template System
;;------------------------------------------------------------------------------

(defvar org-supertag-behavior-template-handlers
  (make-hash-table :test 'equal)
  "Registry for behavior template handlers.")

(defun org-supertag-behavior-template-register (type handler)
  "Register HANDLER function for template TYPE."
  (puthash type handler org-supertag-behavior-template-handlers))

(cl-defstruct org-supertag-behavior-context
  node-id              ; 当前节点
  (chain-data         ; 执行过程中的临时数据
   (make-hash-table :test 'equal))
  last-result)        ; 上一步的结果

(org-supertag-behavior-template-register
 "context"
 (lambda (key ctx)
   "Get value from context with KEY."
   (gethash key (org-supertag-behavior-context-chain-data ctx))))

(defvar org-supertag-behavior--template-expand-depth 0
  "Current template expansion depth.")

(defvar org-supertag-behavior--max-template-expand-depth 10
  "Maximum allowed template expansion depth.")

(defun org-supertag-behavior-template-expand (template ctx-or-node-id)
  "Expand TEMPLATE string in context of CTX-OR-NODE-ID.
CTX-OR-NODE-ID can be either a context object or a node-id string."
  (let ((ctx (if (org-supertag-behavior-context-p ctx-or-node-id)
                 ctx-or-node-id
               (make-org-supertag-behavior-context 
                :node-id ctx-or-node-id))))
    (with-temp-buffer
      (if (stringp template)
          (let ((processed-vars (make-hash-table :test 'equal)))
            ;; 先收集所有需要处理的变量
            (with-temp-buffer
              (insert template)
              (goto-char (point-min))
              (while (re-search-forward "\\${\\([^:]+\\):\\([^}]+\\)}" nil t)
                (let* ((type (match-string 1))
                       (value (match-string 2))
                       (key (concat type ":" value)))
                  (unless (gethash key processed-vars)
                    (when-let* ((handler (gethash type org-supertag-behavior-template-handlers))
                               (result (funcall handler value ctx)))
                      (puthash key result processed-vars))))))
            ;; 然后一次性替换所有变量
            (insert template)
            (goto-char (point-min))
            (while (re-search-forward "\\${\\([^:]+\\):\\([^}]+\\)}" nil t)
              (let* ((type (match-string 1))
                     (value (match-string 2))
                     (key (concat type ":" value))
                     (result (gethash key processed-vars)))
                (replace-match (or (format "%s" result) "")))))
        (insert (format "%s" template)))
      (buffer-string))))
      
;;------------------------------------------------------------------------------
;; Built-in Handlers
;;------------------------------------------------------------------------------


;; input 使用范围：
;; - 用于需要用户输入的场景
;; - prompt 参数会作为输入提示
;; 
;; 示例：
;; "${input:请输入审阅人}"  -> 提示"请输入审阅人："并返回用户输入
;; "${input:截止天数}"      -> 提示"截止天数："并返回用户输入
(org-supertag-behavior-template-register
 "input"
 (lambda (prompt ctx)
   (let ((result (read-string (format "%s: " prompt))))
     ;; 保存输入到上下文
     (puthash prompt result 
              (org-supertag-behavior-context-chain-data ctx))
     result)))


;; 使用范围：
;; - 处理日期和时间
;; - format 参数使用 format-time-string 的格式字符串
;; 
;; 示例：
;; "${date:now}"           -> "2024-03-14"（当前日期）
;; "${date:%Y-%m-%d %H:%M}" -> "2024-03-14 15:30"（当前日期时间）
;; "${date:%H:%M}"         -> "15:30"（当前时间）
(defun org-supertag-behavior-template--parse-date-spec (spec)
  "Parse date spec like 'now-7d' or 'now+1m'.
Supports:
- now: current time
- +/-Nd: add/subtract N days
- +/-Nw: add/subtract N weeks
- +/-Nm: add/subtract N months
- +/-Ny: add/subtract N years"
  (if (string= spec "now")
      (current-time)
    (when (string-match "^now\\([+-]\\)\\([0-9]+\\)\\([dwmy]\\)$" spec)
      (let* ((op (match-string 1 spec))
             (num (string-to-number (match-string 2 spec)))
             (unit (match-string 3 spec))
             (seconds (pcase unit
                       ("d" (* num 86400))    ; days
                       ("w" (* num 604800))   ; weeks
                       ("m" (* num 2592000))  ; months (approx)
                       ("y" (* num 31557600)) ; years (approx)
                       (_ 0))))
        (time-add (current-time)
                 (if (string= op "+")
                     seconds
                   (- seconds)))))))

(org-supertag-behavior-template-register
 "date"
 (lambda (spec ctx)
   (let ((time
          (if (string-match-p "%" spec)
              ;; 如果包含 % 则视为格式字符串
              (current-time)
            ;; 否则尝试解析日期规范
            (org-supertag-behavior-template--parse-date-spec spec))))
     (if (string-match-p "%" spec)
         (format-time-string spec time)
       (format-time-string "%Y-%m-%d" time)))))

;; 使用范围：
;; - 获取节点的属性值
;; - prop-name 是属性的名称
;; 
;; 示例：
;; "${prop:REVIEWER}"     -> 获取 REVIEWER 属性的值
;; "${prop:DEADLINE}"     -> 获取 DEADLINE 属性的值
;; "${prop:CREATED}"      -> 获取 CREATED 属性的值
(org-supertag-behavior-template-register
 "prop"
 (lambda (prop-name ctx)
   (when-let* ((pos (org-supertag-db-get-pos 
                    (org-supertag-behavior-context-node-id ctx))))
     (org-with-point-at pos
       (org-entry-get nil prop-name)))))

;;------------------------------------------------------------------------------
;; Condition Evaluation
;;------------------------------------------------------------------------------

(defun org-supertag-behavior-template--eval-condition (condition ctx-or-node-id)
  "Evaluate template CONDITION for CTX-OR-NODE-ID."
  (let ((ctx (if (org-supertag-behavior-context-p ctx-or-node-id)
                 ctx-or-node-id
               (make-org-supertag-behavior-context 
                :node-id ctx-or-node-id))))
    (let ((conditions (split-string condition ",")))
      (cl-every
       (lambda (cond-expr)
         (cond
          ;; 日期比较 DEADLINE<${date:now}
          ((string-match "\\(.+\\)<\\(.+\\)" cond-expr)
           (let ((date1 (org-supertag-behavior-template-expand 
                        (match-string 1 cond-expr) ctx))
                 (date2 (org-supertag-behavior-template-expand 
                        (match-string 2 cond-expr) ctx)))
             (time-less-p 
              (org-time-string-to-time date1)
              (org-time-string-to-time date2))))
          
          ;; 属性相等 TODO=DONE
          ((string-match "\\(.+\\)=\\(.+\\)" cond-expr)
           (let* ((prop (match-string 1 cond-expr))
                  (value (match-string 2 cond-expr))
                  (pos (org-supertag-db-get-pos    
                       (org-supertag-behavior-context-node-id ctx))))
             (string= (org-entry-get pos prop) value)))
          
          ;; 属性不等 PRIORITY!=A
          ((string-match "\\(.+\\)!=\\(.+\\)" cond-expr)
           (let* ((prop (match-string 1 cond-expr))
                  (value (match-string 2 cond-expr))
                  (pos (org-supertag-db-get-pos    
                       (org-supertag-behavior-context-node-id ctx))))
             (not (string= (org-entry-get pos prop) value))))))
       conditions))))

(provide 'org-supertag-behavior-template)
;;; org-supertag-behavior-template.el ends here

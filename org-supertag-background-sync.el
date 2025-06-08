;;; org-supertag-background-sync.el --- Background sync engine for org-supertag -*- lexical-binding: t; -*-

;; 背景同步引擎：负责周期性地将 Elisp 数据同步到 Python 后端
;; 使用基于哈希值的增量检测机制，而非时间戳

;;; Commentary:
;; 
;; 此模块实现与 org-supertag-sync.el 解耦的背景同步：
;; - org-supertag-sync.el: 文件 → Elisp DB（高频，轻量）
;; - org-supertag-background-sync.el: Elisp DB → Python（低频，重量）
;;
;; 关键特性：
;; 1. 基于哈希值的增量检测（借鉴 org-supertag-sync.el 的机制）
;; 2. 状态管理，防止并发运行
;; 3. 详细的同步报告和调试信息
;; 4. 12小时同步间隔

;;; Code:

(require 'org-supertag-db)
(require 'org-supertag-bridge)

;;; 配置变量

(defcustom org-supertag-background-sync-interval (* 12 3600) ; 12小时
  "背景同步间隔（秒）。"
  :type 'integer
  :group 'org-supertag)

(defcustom org-supertag-background-sync-auto-start t
  "是否在启动时自动开始背景同步。"
  :type 'boolean
  :group 'org-supertag)

;;; 运行时变量

(defvar org-supertag-background-sync--timer nil
  "背景同步定时器。")

(defvar org-supertag-background-sync--state :idle
  "背景同步状态。可能的值：
- :idle            - 空闲状态
- :waiting-backend - 等待Python后端就绪
- :syncing         - 正在同步")

(defvar org-supertag-background-sync--last-sync-time nil
  "最后一次成功同步的时间。")

(defvar org-supertag-background-sync--last-sync-hashes (make-hash-table :test 'equal)
  "最后一次同步时各对象的哈希值。
Key: 对象ID
Value: 哈希值字符串")

(defvar org-supertag-background-sync--stats 
  '(:synced-nodes 0 :synced-tags 0 :synced-links 0 :total-objects 0)
  "最后一次同步的统计信息。")

(defvar org-supertag-background-sync--backend-check-timer nil
  "用于检查Python后端状态的定时器。")

(defvar org-supertag-background-sync--backend-check-interval 5
  "检查Python后端状态的间隔（秒）。")

(defvar org-supertag-background-sync--hash-file
  (expand-file-name "sync_hashes.el" org-supertag-data-directory)
  "哈希记录的持久化文件路径。")

;;; 哈希计算函数

(defun org-supertag-background-sync--calculate-object-hash (obj-id obj-props)
  "计算对象的哈希值。
OBJ-ID: 对象ID
OBJ-PROPS: 对象属性plist"
  (let* ((obj-type (plist-get obj-props :type))
         (hash-content 
          (cond
           ;; 节点哈希：使用与 org-supertag-sync.el 相同的方法
           ((eq obj-type :node)
            (format "%s%s%s%s%s%s"
                    (or (plist-get obj-props :raw-value) "")
                    (or (plist-get obj-props :tags) "")
                    (or (plist-get obj-props :todo-type) "")
                    (or (plist-get obj-props :priority) "")
                    (or (plist-get obj-props :properties) "")
                    (or (plist-get obj-props :file-path) "")))
           
           ;; 标签哈希：基于名称和字段定义
           ((eq obj-type :tag)
            (format "%s%s%s"
                    (or (plist-get obj-props :name) "")
                    (or (plist-get obj-props :fields) "")
                    (or (plist-get obj-props :description) "")))
           
           ;; 链接哈希：基于类型、源、目标
           ((eq obj-type :link)
            (format "%s%s%s%s"
                    (or (plist-get obj-props :link-type) "")
                    (or (plist-get obj-props :from) "")
                    (or (plist-get obj-props :to) "")
                    (or (plist-get obj-props :value) "")))
           
           ;; 其他类型：序列化整个属性
           (t (format "%s" obj-props)))))
    (secure-hash 'sha1 hash-content)))

(defun org-supertag-background-sync--get-changed-objects ()
  "获取自上次同步以来发生变化的对象。
返回 (changed-nodes changed-tags changed-links) 的列表。"
  (let ((changed-nodes '())
        (changed-tags '())
        (changed-links '())
        (checked-count 0)
        (changed-count 0))
    
    ;; 检查变化的节点
    (maphash 
     (lambda (id props)
       (cl-incf checked-count)
       (let* ((current-hash (org-supertag-background-sync--calculate-object-hash id props))
              (last-hash (gethash id org-supertag-background-sync--last-sync-hashes)))
         (unless (string= current-hash (or last-hash ""))
           (cl-incf changed-count)
           (push (cons id props) changed-nodes))))
     org-supertag-db--node)
    
    ;; 检查变化的标签
    (maphash 
     (lambda (id props)
       (cl-incf checked-count)
       (let* ((current-hash (org-supertag-background-sync--calculate-object-hash id props))
              (last-hash (gethash id org-supertag-background-sync--last-sync-hashes)))
         (unless (string= current-hash (or last-hash ""))
           (cl-incf changed-count)
           (push (cons id props) changed-tags))))
     org-supertag-db--tag)
    
    ;; 检查变化的链接
    (maphash 
     (lambda (id props)
       (cl-incf checked-count)
       (let* ((current-hash (org-supertag-background-sync--calculate-object-hash id props))
              (last-hash (gethash id org-supertag-background-sync--last-sync-hashes)))
         (unless (string= current-hash (or last-hash ""))
           (cl-incf changed-count)
           (push (cons id props) changed-links))))
     org-supertag-db--link)
    
    ;; (message "[背景同步] 检查了 %d 个对象，发现 %d 个变化" checked-count changed-count)
    (list changed-nodes changed-tags changed-links)))

;;; 主同步函数

(defun org-supertag-background-sync--do-sync ()
  "执行一次完整的背景同步操作。"
  ;; (message "[背景同步] 开始同步...")
  (setq org-supertag-background-sync--state :syncing)
  
  (condition-case err
      (let* ((start-time (current-time))
             (changes (org-supertag-background-sync--get-changed-objects))
             (changed-nodes (nth 0 changes))
             (changed-tags (nth 1 changes))
             (changed-links (nth 2 changes))
             (total-changed (+ (length changed-nodes) 
                              (length changed-tags) 
                              (length changed-links))))
        
        (if (= total-changed 0)
            (progn
              ;; (message "[背景同步] 没有发现变化，跳过同步")
              ;; 重置状态
              (setq org-supertag-background-sync--state :idle))
          
          ;; (message "[背景同步] 发现变化：节点 %d，标签 %d，链接 %d"
          ;;          (length changed-nodes) (length changed-tags) (length changed-links))
          
          ;; 启动进度跟踪
          (org-supertag-background-sync--start-progress total-changed)
          
          ;; 使用 bulk_process_snapshot 方法进行同步
          (let* ((nodes-data (org-supertag-background-sync--prepare-nodes-for-python changed-nodes))
                 (tags-data (org-supertag-background-sync--prepare-tags-for-python changed-tags))
                 (links-data (org-supertag-background-sync--prepare-links-for-python changed-links))
                 (snapshot-data `(:objects ,nodes-data :links ,links-data :sync_timestamp ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))))
            
            (org-supertag-bridge-call-async
             "bulk_process_snapshot"
             (lambda (result)
               (let ((end-time (current-time)))
                 (if (and result (string= (plist-get result :status) "success"))
                     (progn
                       ;; 更新哈希记录
                       (org-supertag-background-sync--update-hashes changed-nodes changed-tags changed-links)
                       
                       ;; 更新统计信息
                       (setq org-supertag-background-sync--stats
                             (list :synced-nodes (length changed-nodes)
                                   :synced-tags (length changed-tags)
                                   :synced-links (length changed-links)
                                   :total-objects total-changed))
                       
                       ;; 记录成功时间
                       (setq org-supertag-background-sync--last-sync-time end-time)
                       
                       ;; (let ((duration (float-time (time-subtract end-time start-time))))
                       ;;   (message "[背景同步] 完成同步：%d 个对象，耗时 %.2f 秒" 
                       ;;            total-changed duration))
                       )
                   
                   ;; (message "[背景同步] 同步失败：%s" (or (plist-get result :message) "Unknown error"))
                   )
                 
                 ;; 结束进度跟踪
                 (org-supertag-background-sync--finish-progress)
                 
                 ;; 重置状态
                 (setq org-supertag-background-sync--state :idle)))
             snapshot-data))))
    
    (error
     (setq org-supertag-background-sync--state :idle)
     (org-supertag-background-sync--finish-progress)
     ;; (message "[背景同步] 同步过程中发生错误：%s" (error-message-string err))
     )))

(defun org-supertag-background-sync--update-hashes (nodes tags links)
  "更新已同步对象的哈希记录。"
  (dolist (node-pair nodes)
    (let* ((id (car node-pair))
           (props (cdr node-pair))
           (hash (org-supertag-background-sync--calculate-object-hash id props)))
      (puthash id hash org-supertag-background-sync--last-sync-hashes)))
  
  (dolist (tag-pair tags)
    (let* ((id (car tag-pair))
           (props (cdr tag-pair))
           (hash (org-supertag-background-sync--calculate-object-hash id props)))
      (puthash id hash org-supertag-background-sync--last-sync-hashes)))
  
  (dolist (link-pair links)
    (let* ((id (car link-pair))
           (props (cdr link-pair))
           (hash (org-supertag-background-sync--calculate-object-hash id props)))
      (puthash id hash org-supertag-background-sync--last-sync-hashes)))
  
  ;; 保存更新后的哈希记录
  (org-supertag-background-sync--save-hashes))

(defun org-supertag-background-sync--ensure-data-directory ()
  "确保数据目录存在。"
  (unless (file-exists-p org-supertag-data-directory)
    (make-directory org-supertag-data-directory t)))

(defun org-supertag-background-sync--save-hashes ()
  "保存哈希记录到文件。"
  (when (hash-table-p org-supertag-background-sync--last-sync-hashes)
    ;; 确保数据目录存在
    (org-supertag-background-sync--ensure-data-directory)
    
    ;; 保存哈希记录
    (with-temp-file org-supertag-background-sync--hash-file
      (let ((print-length nil)
            (print-level nil))
        (prin1 (let ((data (make-hash-table :test 'equal)))
                 (maphash (lambda (k v)
                           (puthash k v data))
                         org-supertag-background-sync--last-sync-hashes)
                 data)
               (current-buffer))))))

(defun org-supertag-background-sync--load-hashes ()
  "从文件加载哈希记录。"
  (when (file-exists-p org-supertag-background-sync--hash-file)
    (condition-case err
        (let ((loaded-data
               (with-temp-buffer
                 (insert-file-contents org-supertag-background-sync--hash-file)
                 (read (current-buffer)))))
          (when (hash-table-p loaded-data)
            (setq org-supertag-background-sync--last-sync-hashes
                  (make-hash-table :test 'equal))
            (maphash (lambda (k v)
                      (puthash k v org-supertag-background-sync--last-sync-hashes))
                    loaded-data)))
      (error
       (message "Error loading hash records: %s" (error-message-string err))
       ;; 如果加载失败，创建一个新的空哈希表
       (setq org-supertag-background-sync--last-sync-hashes
             (make-hash-table :test 'equal)))))
  ;; 如果文件不存在，创建一个新的空哈希表
  (unless (file-exists-p org-supertag-background-sync--hash-file)
    (setq org-supertag-background-sync--last-sync-hashes
          (make-hash-table :test 'equal))))

;;; 定时器管理

(defun org-supertag-background-sync--python-ready-p ()
  "检查Python后端是否就绪。"
  (and (featurep 'org-supertag-bridge)
       (boundp 'org-supertag-bridge--ready-p)
       org-supertag-bridge--ready-p
       (boundp 'org-supertag-bridge--python-epc-manager)
       org-supertag-bridge--python-epc-manager
       (fboundp 'org-supertag-bridge-epc-live-p)
       (org-supertag-bridge-epc-live-p org-supertag-bridge--python-epc-manager)))

(defun org-supertag-background-sync--try-start-backend ()
  "尝试启动Python后端（如果需要的话）。"
  (when (and (not (org-supertag-background-sync--python-ready-p))
             (fboundp 'org-supertag-init-vectorization))
    (condition-case err
        (org-supertag-init-vectorization)
      (error
       (org-supertag-background-sync--log-debug "启动Python后端失败：%s" (error-message-string err))))))

(defun org-supertag-background-sync--wait-for-backend ()
  "等待Python后端就绪。"
  (setq org-supertag-background-sync--state :waiting-backend)
  
  ;; 尝试启动Python后端（如果还没有启动的话）
  (org-supertag-background-sync--try-start-backend)
  
  (setq org-supertag-background-sync--backend-check-timer
        (run-with-timer org-supertag-background-sync--backend-check-interval
                        org-supertag-background-sync--backend-check-interval
                        #'org-supertag-background-sync--check-backend)))

(defun org-supertag-background-sync--check-backend ()
  "检查Python后端状态的定时回调。"
  (if (org-supertag-background-sync--python-ready-p)
      (progn
        ;; 后端就绪，停止检查并启动同步
        (when org-supertag-background-sync--backend-check-timer
          (cancel-timer org-supertag-background-sync--backend-check-timer)
          (setq org-supertag-background-sync--backend-check-timer nil))
        (org-supertag-background-sync--start-sync-timer))
    ;; 继续等待
    (org-supertag-background-sync--log-debug "仍在等待Python后端就绪...")))

(defun org-supertag-background-sync--start-sync-timer ()
  "启动实际的同步定时器。"
  (setq org-supertag-background-sync--state :idle)
  (setq org-supertag-background-sync--timer
        (run-with-timer 0 ; 立即开始第一次同步
                        org-supertag-background-sync-interval
                        #'org-supertag-background-sync--timer-function))
  (org-supertag-background-sync--log-debug "背景同步已启动，间隔 %d 小时" (/ org-supertag-background-sync-interval 3600)))

(defun org-supertag-background-sync--timer-function ()
  "定时器回调函数。"
  (when (eq org-supertag-background-sync--state :idle)
    ;; 到这里时Python后端应该已经就绪，但为了安全起见还是检查一下
    (if (org-supertag-background-sync--python-ready-p)
        (org-supertag-background-sync--do-sync)
      ;; 如果后端又不可用了，重新进入等待状态
      ;; (message "[背景同步] Python后端连接丢失，重新等待...")
      (org-supertag-background-sync--wait-for-backend))))

(defun org-supertag-background-sync-start ()
  "启动背景同步。"
  (interactive)
  ;; 停止现有定时器
  (org-supertag-background-sync-stop)
  
  ;; 检查Python后端状态
  (if (org-supertag-background-sync--python-ready-p)
      (org-supertag-background-sync--start-sync-timer)
    (org-supertag-background-sync--wait-for-backend)))

(defun org-supertag-background-sync-stop ()
  "停止背景同步。"
  (interactive)
  (let ((was-running (or org-supertag-background-sync--timer 
                        org-supertag-background-sync--backend-check-timer
                        (not (eq org-supertag-background-sync--state :idle)))))
    
    ;; 停止同步定时器
    (when org-supertag-background-sync--timer
      (cancel-timer org-supertag-background-sync--timer)
      (setq org-supertag-background-sync--timer nil))
    
    ;; 停止后端检查定时器
    (when org-supertag-background-sync--backend-check-timer
      (cancel-timer org-supertag-background-sync--backend-check-timer)
      (setq org-supertag-background-sync--backend-check-timer nil))
    
    ;; 重置状态
    (setq org-supertag-background-sync--state :idle)
    
    ;; 只在实际停止了什么东西时才输出消息
    (when (and was-running (called-interactively-p 'any))
      ;; (message "[背景同步] 已停止")
      )))

(defun org-supertag-background-sync-restart ()
  "重启背景同步。"
  (interactive)
  (org-supertag-background-sync-stop)
  (org-supertag-background-sync-start))

;;; 手动同步和状态查询

(defun org-supertag-background-sync-run-now ()
  "立即运行一次背景同步。"
  (interactive)
  (cond 
   ((eq org-supertag-background-sync--state :syncing)
    ;; (message "[背景同步] 已在运行中，请稍候...")
    )
   
   ((eq org-supertag-background-sync--state :waiting-backend)
    ;; (message "[背景同步] 正在等待Python后端就绪，请稍候...")
    )
   
   ((not (org-supertag-background-sync--python-ready-p))
    ;; (message "[背景同步] Python后端未就绪。请先启动Python后端：M-x org-supertag-bridge-start-process")
    )
   
   (t 
    (org-supertag-background-sync--do-sync))))

(defun org-supertag-background-sync-status ()
  "显示背景同步状态。"
  (interactive)
  (let* ((python-ready (org-supertag-background-sync--python-ready-p))
         (timer-status (cond 
                       (org-supertag-background-sync--timer "同步定时器运行中")
                       (org-supertag-background-sync--backend-check-timer "等待后端定时器运行中")
                       (t "已停止")))
         (status-msg
          (format "背景同步状态：
- 状态：%s
- 定时器：%s
- Python后端：%s
- 同步间隔：%d 小时
- 最后同步：%s
- 最后同步统计：节点 %d，标签 %d，链接 %d总计 %d
- 哈希记录数：%d"
                  org-supertag-background-sync--state
                  timer-status
                  (if python-ready "已就绪" "未就绪")
                  (/ org-supertag-background-sync-interval 3600)
                  (if org-supertag-background-sync--last-sync-time
                      (format-time-string "%Y-%m-%d %H:%M:%S" org-supertag-background-sync--last-sync-time)
                    "从未同步")
                  (plist-get org-supertag-background-sync--stats :synced-nodes)
                  (plist-get org-supertag-background-sync--stats :synced-tags)
                  (plist-get org-supertag-background-sync--stats :synced-links)
                  (plist-get org-supertag-background-sync--stats :total-objects)
                  (hash-table-count org-supertag-background-sync--last-sync-hashes))))
    (message "%s" status-msg)
    status-msg))

;;; 初始化

(defun org-supertag-background-sync-reset-hashes ()
  "重置所有哈希记录，强制下次全量同步。"
  (interactive)
  (clrhash org-supertag-background-sync--last-sync-hashes)
  (setq org-supertag-background-sync--last-sync-time nil)
  ;; (message "[背景同步] 已重置哈希记录，下次将执行全量同步")
  )

(defun org-supertag-background-sync-initialize ()
  "初始化背景同步系统。"
  ;; 加载历史哈希记录
  (org-supertag-background-sync--load-hashes)
  
  (when org-supertag-background-sync-auto-start
    (org-supertag-background-sync-start)))

;; 这个模块现在通过 org-supertag.el 直接调用初始化
;; 不需要自动钩子，避免重复初始化

;;; 调试函数

(defun org-supertag-background-sync--log-debug (msg &rest args)
  "输出调试信息，只在启用调试时显示。"
  (when (and (boundp 'org-supertag-debug) org-supertag-debug)
    (apply #'message (concat "[背景同步-调试] " msg) args)))

(defun org-supertag-background-sync-debug-hashes (&optional limit)
  "显示当前哈希记录（用于调试）。
LIMIT: 最多显示的记录数，默认10。"
  (interactive "P")
  (let ((count 0)
        (limit (or limit 10)))
    (message "哈希记录（最多显示 %d 条）：" limit)
    (maphash 
     (lambda (id hash)
       (when (< count limit)
         (message "  %s: %s" id (substring hash 0 8))
         (cl-incf count)))
     org-supertag-background-sync--last-sync-hashes)
    (message "总计 %d 条哈希记录" (hash-table-count org-supertag-background-sync--last-sync-hashes))))

;;; 进度处理

(defvar org-supertag-background-sync--current-progress nil
  "当前同步进度信息，格式为 (current total start-time)。")

(defun org-supertag-background-sync--update-progress (current total)
  "更新同步进度。由Python后端调用。"
  (when org-supertag-background-sync--current-progress
    (let* ((start-time (nth 2 org-supertag-background-sync--current-progress))
           (elapsed (float-time (time-subtract (current-time) start-time)))
           (percentage (if (> total 0) (/ (* current 100.0) total) 0))
           (eta (if (and (> current 0) (> total current))
                    (/ (* elapsed (- total current)) current)
                  0)))
      
      (setq org-supertag-background-sync--current-progress (list current total start-time))
      
      ;; (message "[背景同步] 进度：%d/%d (%.1f%%) - 已用时 %.1fs%s"
      ;;          current total percentage elapsed
      ;;          (if (> eta 0) (format " - 预计剩余 %.1fs" eta) ""))
      )))

(defun org-supertag-background-sync--start-progress (total)
  "开始进度跟踪。"
  (setq org-supertag-background-sync--current-progress 
        (list 0 total (current-time)))
  ;; (message "[背景同步] 开始同步 %d 个对象..." total)
  )

(defun org-supertag-background-sync--finish-progress ()
  "结束进度跟踪。"
  (setq org-supertag-background-sync--current-progress nil))

(defun org-supertag-background-sync--prepare-nodes-for-python (changed-nodes)
  "准备节点数据供Python处理。返回节点数据列表，每个节点包含6个字段。"
  (let ((nodes-list '()))
    (dolist (node-pair changed-nodes)
      (let* ((id (car node-pair))
             (props (cdr node-pair))
             ;; 创建包含6个字段的节点数据列表：[ID, TITLE, CONTENT, TAGS, FILE_PATH, MODIFIED_AT]
             (node-data (list 
                        id                                          ; node_id
                        (or (plist-get props :title) "")           ; title
                        (or (plist-get props :raw-value) "")       ; content
                        (or (plist-get props :tags) '())           ; tags
                        (or (plist-get props :file-path) "")       ; file_path
                        (plist-get props :modified-at))))          ; modified_at
        (push node-data nodes-list)))
    (nreverse nodes-list)))

(defun org-supertag-background-sync--prepare-tags-for-python (changed-tags)
  "准备标签数据供Python处理。"
  (let ((tags-list '()))
    (dolist (tag-pair changed-tags)
      (let* ((id (car tag-pair))
             (props (cdr tag-pair))
             (tag-data (list id
                           (or (plist-get props :name) "")
                           (plist-get props :created-at)
                           (plist-get props :modified-at))))
        (push tag-data tags-list)))
    (nreverse tags-list)))

(defun org-supertag-background-sync--prepare-links-for-python (changed-links)
  "准备链接数据供Python处理。返回链接数据列表，每个链接包含6个字段。"
  (let ((links-list '()))
    (dolist (link-pair changed-links)
      (let* ((id (car link-pair))
             (props (cdr link-pair))
             ;; 创建包含6个字段的链接数据列表：[LINK_ID, TYPE, FROM_ID, TO_ID, PROPERTIES, MODIFIED_AT]
             (link-data (list id                                     ; link_id
                            (or (plist-get props :type) "")          ; type
                            (or (plist-get props :from) "")          ; from_id
                            (or (plist-get props :to) "")            ; to_id
                            (or (plist-get props :properties) '())   ; properties
                            (plist-get props :modified-at))))        ; modified_at
        (push link-data links-list)))
    (nreverse links-list)))

(provide 'org-supertag-background-sync)
;;; org-supertag-background-sync.el ends here 

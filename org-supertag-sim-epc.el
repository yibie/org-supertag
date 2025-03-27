;;; org-supertag-sim-epc.el --- tag similarity service based on EPC -*- lexical-binding: t; -*-

;; 依赖
(require 'json)
(require 'org-supertag-db)
(require 'cl-lib)
(require 'epc)  ;; 使用标准 EPC 库

;; 自定义选项
(defgroup org-supertag-sim-epc nil
  "基于EPC的标签相似度服务."
  :group 'org-supertag)

(defcustom org-supertag-sim-epc-python-path 
  (or (executable-find "python3")
      (executable-find "python"))
  "Python解释器路径."
  :type 'string
  :group 'org-supertag-sim-epc)

(defcustom org-supertag-sim-epc-script-path
  (expand-file-name "simtag_epc.py"
                   (file-name-directory
                    (or load-file-name buffer-file-name)))
  "simtag_epc.py脚本路径."
  :type 'string
  :group 'org-supertag-sim-epc)

(defcustom org-supertag-sim-epc-vector-file
  (expand-file-name "tag_vectors.json"
                   (file-name-directory
                    (if (boundp 'org-supertag-db-file)
                        org-supertag-db-file
                      (expand-file-name "supertag-db.el" org-supertag-data-directory))))
  "标签向量文件路径."
  :type 'string
  :group 'org-supertag-sim-epc)

(defcustom org-supertag-sim-epc-request-timeout 30
  "请求超时时间（秒）."
  :type 'integer
  :group 'org-supertag-sim-epc)

(defvar org-supertag-sim-epc-manager nil
  "EPC管理器对象.")

(defvar org-supertag-sim-epc-initialized nil
  "标记系统是否已初始化.")

(defvar org-supertag-sim-epc--startup-timer nil
  "服务器启动定时器.")

(defcustom org-supertag-sim-epc-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "SimTag EPC 服务器目录路径."
  :type 'directory
  :group 'org-supertag-sim-epc)

(defcustom org-supertag-sim-epc-data-dir
  (expand-file-name "data" org-supertag-sim-epc-dir)
  "SimTag 数据目录路径."
  :type 'directory
  :group 'org-supertag-sim-epc)

(defcustom org-supertag-sim-epc-venv-dir
  (expand-file-name ".venv" org-supertag-sim-epc-dir)
  "Python虚拟环境目录."
  :type 'directory
  :group 'org-supertag-sim-epc)

;; 确保数据目录存在
(unless (file-exists-p org-supertag-sim-epc-data-dir)
  (make-directory org-supertag-sim-epc-data-dir t))

;; 日志函数
(defun org-supertag-sim-epc-log (format-string &rest args)
  "记录日志信息.
FORMAT-STRING 是格式化字符串
ARGS 是格式化参数"
  (let ((msg (apply #'format format-string args)))
    (with-current-buffer (get-buffer-create "*simtag-epc-log*")
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
      (insert msg)
      (insert "\n"))
    (message "SimTag EPC: %s" msg)))

(defun org-supertag-sim-epc--ensure-db-file ()
  "确保数据库文件存在并返回其路径和标签数据."
  (unless (boundp 'org-supertag-db-file)
    (error "数据库文件路径未定义"))
  (let* ((db-file org-supertag-db-file)
         (tags (org-supertag-db-find-by-type :tag)))
    ;; 确保数据库文件存在
    (unless (file-exists-p db-file)
      (org-supertag-db-save))
    
    ;; 将标签数据转换为列表格式
    (let ((tag-list
           (cl-loop for tag in tags
                    for props = (org-supertag-db-get tag)
                    for name = (and props (plist-get props :name))
                    when name
                    collect (list :id tag :name name))))
      (list db-file tag-list))))

(defun org-supertag-sim-epc-debug-env ()
  "调试Python环境设置."
  (let* ((python-exe org-supertag-sim-epc-python-path)
         (script-dir (file-name-directory org-supertag-sim-epc-script-path))
         (debug-script "
import sys
import os

print('=== Python Environment Debug ===')
print(f'Python version: {sys.version}')
print(f'Current working directory: {os.getcwd()}')
print(f'PYTHONPATH: {os.environ.get(\"PYTHONPATH\", \"Not set\")}')
print(f'Sys.path: {sys.path}')
print('=== End Debug ===')
"))
    (with-temp-buffer
      (call-process python-exe nil t nil "-c" debug-script)
      (message "Python环境信息:\n%s" (buffer-string)))))

(defun org-supertag-sim-epc-start-server ()
  "启动SimTag EPC服务器."
  (interactive)
  ;; 取消已有的启动定时器
  (when org-supertag-sim-epc--startup-timer
    (cancel-timer org-supertag-sim-epc--startup-timer))
  
  ;; 设置新的启动定时器，在Emacs空闲1秒后执行
  (setq org-supertag-sim-epc--startup-timer
        (run-with-idle-timer 
         1 nil  ; 1秒后执行一次
         (lambda ()
           (condition-case err
               (org-supertag-sim-epc--start-server-internal)
             (error
              (org-supertag-sim-epc-log "服务器启动失败: %s" err)
              (message "SimTag EPC服务器启动失败，将在下次空闲时重试")))))))

(defun org-supertag-sim-epc--start-server-internal ()
  "内部函数：启动SimTag EPC服务器."
  (let* ((python-exe org-supertag-sim-epc-python-path)
         (python-file org-supertag-sim-epc-script-path)
         (vector-file org-supertag-sim-epc-vector-file)  ; 使用正确的自定义变量
         (db-file org-supertag-db-file)                  ; 使用正确的自定义变量
         (base-dir org-supertag-sim-epc-dir)
         (process-environment 
          (cons (format "PYTHONPATH=%s" base-dir)
                process-environment))
         (default-directory base-dir)
         (process-buffer (get-buffer-create "*simtag-epc-process*")))
    
    ;; 确保环境设置正确
    (org-supertag-sim-epc-setup-venv)
    (org-supertag-sim-epc-check-module-structure)
    
    ;; 记录启动信息
    (org-supertag-sim-epc-log "启动服务器...")
    (org-supertag-sim-epc-log "工作目录: %s" default-directory)
    (org-supertag-sim-epc-log "PYTHONPATH: %s" (getenv "PYTHONPATH"))
    
    (org-supertag-sim-epc-log "启动EPC服务器...")
    (org-supertag-sim-epc-log "Python路径: %s" python-exe)
    (org-supertag-sim-epc-log "脚本路径: %s" python-file)
    (org-supertag-sim-epc-log "启动参数: %S" (list python-file "--vector-file" vector-file "--db-file" db-file "--debug"))
    
    ;; 清空进程缓冲区
    (with-current-buffer process-buffer
      (erase-buffer))
    
    ;; 创建进程
    (make-process
     :name "simtag-epc"
     :buffer process-buffer
     :command (cons python-exe (list python-file "--vector-file" vector-file "--db-file" db-file "--debug"))
     :filter (lambda (proc output)
               (org-supertag-sim-epc-log "进程输出: %s" output)
               (with-current-buffer (process-buffer proc)
                 (goto-char (point-max))
                 (insert output))
               ;; 检查端口号
               (when (string-match "\\([0-9]+\\)[\n\r]" output)
                 (let ((port (string-to-number (match-string 1 output))))
                   (org-supertag-sim-epc-log "找到端口号: %d" port)
                   ;; 创建EPC连接
                   (condition-case err
                       (progn
                         (setq org-supertag-sim-epc-manager 
                               (epc:start-epc python-exe (list python-file "--vector-file" vector-file "--db-file" db-file "--debug")))
                         (org-supertag-sim-epc-log "EPC连接已创建"))
                     (error
                      (org-supertag-sim-epc-log "EPC连接失败: %s" 
                                               (error-message-string err))))))))))

(defun org-supertag-sim-epc-stop-server ()
  "停止 SimTag EPC 服务器."
  (when (and org-supertag-sim-epc-manager
             (epc:live-p org-supertag-sim-epc-manager))
    (org-supertag-sim-epc-log "停止EPC服务器...")
    (epc:stop-epc org-supertag-sim-epc-manager)
    (org-supertag-sim-epc-log "EPC服务器已停止"))
  
  (setq org-supertag-sim-epc-manager nil
        org-supertag-sim-epc-initialized nil))

(defun org-supertag-sim-epc-server-running-p ()
  "检查EPC服务器是否在运行."
  (and org-supertag-sim-epc-manager
       (epc:live-p org-supertag-sim-epc-manager)))

(defun org-supertag-sim-epc-ensure-server ()
  "确保EPC服务器已启动，如果未启动则启动它."
  (unless (org-supertag-sim-epc-server-running-p)
    (org-supertag-sim-epc-start-server)))

;; 功能接口
(defun org-supertag-sim-epc-init ()
  "初始化标签相似度引擎."
  (interactive)
  (org-supertag-sim-epc-log "开始初始化...")
  (org-supertag-sim-epc-ensure-server)
  
  (let* ((db-info (org-supertag-sim-epc--ensure-db-file))
         (db-file (car db-info))
         (tag-list (cadr db-info)))
    
    (org-supertag-sim-epc-log "数据库文件: %s" db-file)
    (org-supertag-sim-epc-log "向量文件: %s" org-supertag-sim-epc-vector-file)
    
    (condition-case err
        (progn
          (org-supertag-sim-epc-log "调用initialize方法...")
          ;; 直接传递文件路径字符串
          (let ((response (epc:call-sync org-supertag-sim-epc-manager 
                                        'initialize 
                                        (list org-supertag-sim-epc-vector-file 
                                              db-file))))
            (org-supertag-sim-epc-log "初始化返回结果: %S" response)
            (if (string= (plist-get response :status) "success")
                (progn
                  (setq org-supertag-sim-epc-initialized t)
                  (org-supertag-sim-epc-log "初始化成功")
                  (plist-get response :result))
              (error "初始化失败: %S" response))))
      (error
       (org-supertag-sim-epc-log "初始化过程出错: %s" (error-message-string err))
       nil))))

;; 辅助函数
(defun org-supertag-sim-epc-restart-server ()
  "重启 SimTag EPC 服务器."
  (interactive)
  (org-supertag-sim-epc-stop-server)
  (sleep-for 1)  ; 等待进程完全终止
  (org-supertag-sim-epc-start-server)
  (when (org-supertag-sim-epc-server-running-p)
    (message "SimTag EPC 服务器已重启")))

(defun org-supertag-sim-epc-show-vector-file-info ()
  "显示向量文件的信息."
  (interactive)
  (message "向量文件路径: %s" org-supertag-sim-epc-vector-file)
  (when (boundp 'org-supertag-db-file)
    (message "数据库文件路径: %s" org-supertag-db-file))
  (if (file-exists-p org-supertag-sim-epc-vector-file)
      (let ((size (nth 7 (file-attributes org-supertag-sim-epc-vector-file)))
            (mod-time (format-time-string "%Y-%m-%d %H:%M:%S"
                                          (nth 5 (file-attributes org-supertag-sim-epc-vector-file)))))
        (message "向量文件存在 (大小: %d 字节, 更新时间: %s)" size mod-time))
    (message "向量文件不存在，将在初始化时创建")))

(defun org-supertag-sim-epc-clean-python-cache ()
  "清理Python缓存文件."
  (interactive)
  (let ((script-dir (file-name-directory org-supertag-sim-epc-script-path)))
    (message "清理Python缓存文件...")
    (shell-command (format "find %s -name '__pycache__' -type d -exec rm -rf {} +; find %s -name '*.pyc' -delete" 
                           script-dir script-dir))
    (message "Python缓存文件已清理")))

(defun org-supertag-sim-epc-force-kill ()
  "强制终止所有SimTag EPC相关进程."
  (interactive)
  (message "强制终止SimTag EPC进程...")
  
  ;; 终止已知进程
  (when (org-supertag-sim-epc-server-running-p)
    (org-supertag-sim-epc-stop-server))
  
  ;; 清理Python进程
  (message "终止相关Python进程...")
  (shell-command "pkill -f 'python.*simtag_epc\\.py' || true")
  
  ;; 重置状态
  (setq org-supertag-sim-epc-manager nil)
  (setq org-supertag-sim-epc-initialized nil)
  
  (message "SimTag EPC进程已终止"))

(defun org-supertag-sim-epc-emergency-restart ()
  "紧急重启EPC服务器."
  (interactive)
  (message "正在进行紧急重启...")
  (org-supertag-sim-epc-force-kill)
  (sit-for 1)
  (org-supertag-sim-epc-start-server)
  (sit-for 2)
  (if (org-supertag-sim-epc-server-running-p)
      (message "EPC服务器已重启")
    (message "EPC服务器重启失败")))

(defun org-supertag-sim-epc-echo-test ()
  "测试EPC连接."
  (interactive)
  (org-supertag-sim-epc-ensure-server)
  (condition-case err
      (let ((result (epc:call-sync org-supertag-sim-epc-manager 'echo '("测试消息"))))
        (message "Echo测试成功: %S" result)
        t)
    (error
     (message "Echo测试失败: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim-epc-show-log ()
  "显示SimTag EPC日志buffer."
  (interactive)
  (let ((log-buffer (get-buffer-create "*simtag-epc-log*")))
    (with-current-buffer log-buffer
      (special-mode)  ; 使buffer只读
      (goto-char (point-max)))
    (display-buffer log-buffer)))

(defun org-supertag-sim-epc-setup-venv ()
  "设置Python虚拟环境."
  (interactive)
  (let ((venv-dir org-supertag-sim-epc-venv-dir))
    (unless (file-exists-p venv-dir)
      (org-supertag-sim-epc-log "创建虚拟环境...")
      (make-directory venv-dir t)
      (shell-command-to-string 
       (format "python3 -m venv %s" venv-dir)))
    
    ;; 安装依赖
    (let ((pip (expand-file-name "bin/pip" venv-dir)))
      (org-supertag-sim-epc-log "安装依赖...")
      (shell-command-to-string 
       (format "%s install epc sentence-transformers torch numpy requests" pip)))
    
    ;; 更新Python解释器路径
    (let ((python-path (expand-file-name "bin/python" venv-dir)))
      (when (file-exists-p python-path)
        (setq org-supertag-sim-epc-python-path python-path)
        (org-supertag-sim-epc-log "Python路径已更新: %s" python-path)))))

(defun org-supertag-sim-epc-check-module-structure ()
  "检查并创建必要的模块结构."
  (interactive)
  (let* ((base-dir org-supertag-sim-epc-dir)
         (simtag-dir (expand-file-name "simtag" base-dir))
         (init-file (expand-file-name "__init__.py" simtag-dir)))
    
    ;; 创建simtag目录
    (unless (file-exists-p simtag-dir)
      (make-directory simtag-dir t))
    
    ;; 创建__init__.py
    (unless (file-exists-p init-file)
      (with-temp-file init-file
        (insert "# SimTag package\n")))
    
    ;; 检查必要的Python文件
    (dolist (file '("config.py" "epc_server.py"))
      (let ((file-path (expand-file-name file simtag-dir)))
        (unless (file-exists-p file-path)
          (org-supertag-sim-epc-log "缺少必要文件: %s" file-path))))
    
    (org-supertag-sim-epc-log "模块结构检查完成")))

(defun org-supertag-sim-epc-test-server ()
  "全面测试服务器功能."
  (interactive)
  (org-supertag-sim-epc-log "开始测试服务器...")
  
  ;; 1. 测试基本连接
  (condition-case err
      (let ((result (epc:call-sync org-supertag-sim-epc-manager 'echo '("test"))))
        (org-supertag-sim-epc-log "Echo测试成功: %S" result))
    (error
     (org-supertag-sim-epc-log "Echo测试失败: %s" (error-message-string err))
     (error "Echo测试失败")))
  
  ;; 2. 测试模块导入
  (condition-case err
      (let ((result (epc:call-sync org-supertag-sim-epc-manager 'check_imports '())))
        (org-supertag-sim-epc-log "模块导入测试成功: %S" result))
    (error
     (org-supertag-sim-epc-log "模块导入测试失败: %s" (error-message-string err))
     (error "模块导入测试失败")))
  
  ;; 3. 测试配置
  (condition-case err
      (let ((result (epc:call-sync org-supertag-sim-epc-manager 'get_config '())))
        (org-supertag-sim-epc-log "配置测试成功: %S" result))
    (error
     (org-supertag-sim-epc-log "配置测试失败: %s" (error-message-string err))
     (error "配置测试失败")))
  
  (org-supertag-sim-epc-log "服务器测试完成"))

(defun org-supertag-sim-epc-test-initialization ()
  "测试标签相似度引擎初始化."
  (interactive)
  (org-supertag-sim-epc-log "开始初始化测试...")
  
  ;; 1. 确保服务器运行
  (org-supertag-sim-epc-ensure-server)
  
  ;; 2. 初始化引擎
  (deferred:$
    (deferred:next
      (lambda ()
        (org-supertag-sim-epc-log "调用初始化...")
        (epc:call-deferred org-supertag-sim-epc-manager 'initialize nil)))
    
    (deferred:nextc it
      (lambda (result)
        (let ((status (plist-get result :status))
              (message (plist-get result :message)))
          (org-supertag-sim-epc-log "初始化结果: %S" result)
          
          ;; 检查初始化是否成功
          (if (string= status "error")
              (error "初始化失败: %s" (or message "未知错误"))
            
            ;; 测试引擎基本功能
            (deferred:$
              (deferred:next
                (lambda ()
                  (org-supertag-sim-epc-log "测试引擎功能...")
                  (epc:call-deferred org-supertag-sim-epc-manager 
                                    'test_engine 
                                    '("这是一个测试句子"))))
              
              (deferred:nextc it
                (lambda (test-result)
                  (let* ((result (plist-get test-result :result))
                         (vector-data (plist-get result :vector))
                         ;; 直接计算向量长度
                         (dimensions (length vector-data)))
                    (org-supertag-sim-epc-log "测试结果: %S" test-result)
                    (unless (and vector-data 
                               (= dimensions 384))  ; MiniLM-L6 维度
                      (error "引擎测试失败：向量维度不正确 (got %d, expected 384)" 
                             dimensions))
                    (org-supertag-sim-epc-log "引擎功能测试通过")
                    (setq org-supertag-sim-epc-initialized t)))))))))
    
    (deferred:error it
      (lambda (err)
        (org-supertag-sim-epc-log "测试过程出错: %s" err)
        (error "测试过程出错: %s" err)))))

(provide 'org-supertag-sim-epc)
;;; org-supertag-sim-epc.el ends here

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

(defun org-supertag-sim-epc-check-ollama-installed ()
  "检查Ollama是否已安装，与ollama_bridge.py中的check_ollama_installed功能类似."
  (org-supertag-sim-epc-log "检查Ollama是否已安装...")
  (let ((result 
         (cond
          ;; Windows系统
          ((string-match-p "windows" (symbol-name system-type))
           (let ((possible-paths '("C:/Program Files/Ollama/ollama.exe"
                                   "C:/Program Files (x86)/Ollama/ollama.exe"
                                   "~/AppData/Local/Programs/Ollama/ollama.exe"
                                   "~/scoop/apps/ollama/current/ollama.exe"))
                 (found nil))
             (dolist (path possible-paths)
               (when (file-exists-p (expand-file-name path))
                 (setq found t)))
             found))
          ;; Unix系统(Linux/macOS)
          (t
           (= 0 (call-process "which" nil nil nil "ollama"))))))
    
    (if result
        (progn
          (org-supertag-sim-epc-log "Ollama已安装")
          t)
      (org-supertag-sim-epc-log "Ollama未安装")
      nil)))

(defun org-supertag-sim-epc-get-ollama-install-instruction ()
  "获取安装Ollama的指令，类似ollama_bridge.py中的get_install_command."
  (let ((system-type (symbol-name system-type)))
    (cond
     ((string-match-p "darwin" system-type)  ;; macOS
      "curl -fsSL https://ollama.com/install.sh | sh")
     ((string-match-p "gnu/linux" system-type)  ;; Linux
      "curl -fsSL https://ollama.com/install.sh | sh")
     ((string-match-p "windows" system-type)  ;; Windows
      "Windows安装选项:
1. 使用winget(推荐):
   winget install Ollama.Ollama

2. 使用Scoop:
   scoop bucket add main
   scoop install ollama

3. 直接下载安装包:
   访问 https://ollama.com/download")
     (t
      "请访问 https://ollama.com 获取安装指南"))))

(defun org-supertag-sim-epc-check-ollama ()
  "检查Ollama服务是否运行.
检查方式类似于ollama_bridge.py中的is_service_running方法."
  (org-supertag-sim-epc-log "检查Ollama服务...")
  (let ((result (condition-case nil
                    (with-timeout (3)  ;; 设置3秒超时
                      (let ((output (shell-command-to-string "curl -s --connect-timeout 2 http://localhost:11434/api/tags")))
                        (with-temp-buffer
                          (insert output)
                          (goto-char (point-min))
                          ;; 验证是否返回了有效的JSON响应
                          (and (> (length output) 2)
                               (or (looking-at "\\[")    ;; 应该以JSON数组开始
                                   (looking-at "{"))))))  ;; 或JSON对象开始
                  (error nil))))
    (if result
        (progn
          (org-supertag-sim-epc-log "Ollama服务正在运行")
          t)
      (org-supertag-sim-epc-log "无法连接到Ollama服务！请确保Ollama已启动")
      (message "警告：无法连接到Ollama服务！请确保Ollama已启动")
      nil)))

(defun org-supertag-sim-epc-start-ollama ()
  "启动Ollama服务，基于当前系统类型选择适当的启动方式."
  (org-supertag-sim-epc-log "尝试启动Ollama服务...")
  (let ((system-type (symbol-name system-type)))
    (cond
     ((or (string-match-p "darwin" system-type)
          (string-match-p "gnu/linux" system-type))
      (start-process "ollama-start" nil "ollama" "serve"))
     ((string-match-p "windows" system-type)
      (start-process "ollama-start" nil "ollama.exe" "serve"))
     (t (message "不支持的系统类型: %s" system-type)
        nil))))

(defun org-supertag-sim-epc-ensure-ollama-running ()
  "确保Ollama服务正在运行，类似于ollama_bridge.py中的_ensure_service流程.
1. 检查Ollama是否已安装
2. 如果未安装，提供安装指南
3. 检查服务是否运行
4. 如果未运行，启动服务并等待其就绪"
  ;; 1. 检查Ollama是否已安装
  (unless (org-supertag-sim-epc-check-ollama-installed)
    (let ((install-instruction (org-supertag-sim-epc-get-ollama-install-instruction)))
      (org-supertag-sim-epc-log "Ollama未安装，请安装后再试")
      (message "Ollama未安装，请运行以下命令安装:\n%s" install-instruction)
      (error "Ollama未安装，无法继续")))
  
  ;; 2. 检查服务是否运行
  (unless (org-supertag-sim-epc-check-ollama)
    (message "Ollama服务未运行，正在启动...")
    
    ;; 3. 尝试启动服务
    (let ((system-type (symbol-name system-type)))
      (cond
       ((or (string-match-p "darwin" system-type)
            (string-match-p "gnu/linux" system-type))
        (start-process "ollama-start" nil "ollama" "serve"))
       ((string-match-p "windows" system-type)
        (start-process "ollama-start" nil "ollama.exe" "serve"))
       (t (message "不支持的系统类型: %s" system-type))))
    
    ;; 4. 等待服务启动
    (let ((max-attempts 5)
          (attempt 0)
          (success nil))
      (while (and (< attempt max-attempts) (not success))
        (setq attempt (1+ attempt))
        (message "等待Ollama服务启动... 尝试 %d/%d" attempt max-attempts)
        (sleep-for 2)  ;; 每次等待2秒
        (setq success (org-supertag-sim-epc-check-ollama))
        (when success
          (message "Ollama服务已成功启动"))
        (when (and (not success) (= attempt max-attempts))
          (message "警告: Ollama服务启动失败，请手动启动")
          (error "Ollama服务启动失败，请手动启动"))))))

(defun org-supertag-sim-epc-verify-ollama-model (model-name)
  "验证Ollama模型是否存在，类似于ollama_bridge.py中的ensure_model_exists.
MODEL-NAME是要验证的模型名称."
  (org-supertag-sim-epc-log "验证模型 %s 是否存在..." model-name)
  (let ((result (condition-case err
                    (with-timeout (3)
                      (let ((output (shell-command-to-string 
                                     "curl -s --connect-timeout 2 http://127.0.0.1:11434/api/tags")))
                        (org-supertag-sim-epc-log "API响应: %s" (truncate-string-to-width output 100))
                        (with-temp-buffer
                          (insert output)
                          (goto-char (point-min))
                          ;; 检查返回的JSON是否包含指定模型
                          (and (looking-at "{")  ;; 应该是JSON对象
                               (condition-case err
                                   (let* ((json-object-type 'hash-table)
                                          (json-array-type 'list)
                                          (json-key-type 'string)
                                          (json-data (json-read))
                                          (models (gethash "models" json-data))
                                          (found nil))
                                     ;; 遍历models数组，检查是否包含指定模型
                                     (when models
                                       (dolist (model models)
                                         (let ((name (gethash "name" model)))
                                           (org-supertag-sim-epc-log "检查模型: %s" name)
                                           (when (and name (string= name model-name))
                                             (setq found t)))))
                                     found)
                                 (error
                                  (org-supertag-sim-epc-log "JSON解析错误: %s" (error-message-string err))
                                  nil))))))
                  (error
                   (org-supertag-sim-epc-log "验证模型时出错: %s" (error-message-string err))
                   nil))))
    (if result
        (progn
          (org-supertag-sim-epc-log "模型 %s 存在" model-name)
          t)
      (org-supertag-sim-epc-log "模型 %s 不存在" model-name)
      nil)))

(defun org-supertag-sim-epc-pull-ollama-model (model-name)
  "拉取Ollama模型，类似于ollama_bridge.py中的pull_model.
MODEL-NAME是要拉取的模型名称."
  (org-supertag-sim-epc-log "拉取模型 %s..." model-name)
  (message "正在拉取Ollama模型 %s，这可能需要一些时间..." model-name)
  (let ((process (start-process "ollama-pull" nil "ollama" "pull" model-name)))
    (set-process-sentinel 
     process
     (lambda (proc event)
       (if (string-match-p "finished" event)
           (progn
             (org-supertag-sim-epc-log "模型 %s 拉取成功" model-name)
             (message "Ollama模型 %s 拉取成功" model-name))
         (org-supertag-sim-epc-log "模型 %s 拉取失败: %s" model-name event)
         (message "警告: Ollama模型 %s 拉取失败: %s" model-name event))))
    ;; 返回进程以便可以跟踪
    process))

(defun org-supertag-sim-epc-ensure-ollama-model (model-name)
  "确保Ollama模型存在且可用，如果不存在则拉取.
MODEL-NAME是需要的模型名称，默认为gemma-3-4b。"
  (let ((model (or model-name "hf.co/unsloth/gemma-3-4b-it-GGUF:latest")))
    ;; 首先确保服务正在运行
    (org-supertag-sim-epc-ensure-ollama-running)
    
    ;; 检查模型是否存在
    (unless (org-supertag-sim-epc-verify-ollama-model model)
      (message "模型 %s 不存在，正在拉取..." model)
      (org-supertag-sim-epc-pull-ollama-model model)
      ;; 这里不等待拉取完成，因为可能需要较长时间
      ;; 实际应用中可能需要某种回调机制
      )))

;; 完整的Ollama检查与初始化流程
(defun org-supertag-sim-epc-setup-ollama ()
  "完整设置Ollama环境，包括检查安装、启动服务和准备默认模型."
  (interactive)
  (message "正在设置Ollama环境...")
  (condition-case err
      (progn
        ;; 1. 确保Ollama已安装
        (unless (org-supertag-sim-epc-check-ollama-installed)
          (let ((install-instruction (org-supertag-sim-epc-get-ollama-install-instruction)))
            (message "Ollama未安装，请运行以下命令安装:\n%s" install-instruction)
            (error "Ollama未安装")))
        
        ;; 2. 确保服务运行
        (org-supertag-sim-epc-ensure-ollama-running)
        
        ;; 3. 准备默认模型（可选）
        ;; 这里可以选择是否检查和准备默认模型
        ;; 由于模型下载可能耗时较长，默认只检查不下载
        (when (org-supertag-sim-epc-verify-ollama-model "hf.co/unsloth/gemma-3-4b-it-GGUF:latest")
          (org-supertag-sim-epc-log "默认模型已准备就绪"))
        
        (message "Ollama环境设置完成"))
    (error
     (message "Ollama环境设置失败: %s" (error-message-string err))
     nil)))

;; 功能接口
(defun org-supertag-sim-epc-init ()
  "初始化标签相似度引擎的EPC服务和Ollama服务.
这是供org-supertag-sim-init调用的内部函数。
普通用户应该使用org-supertag-sim-init作为主要入口点。"
  (org-supertag-sim-epc-log "开始初始化...")
  (org-supertag-sim-epc-ensure-server)
  
  ;; 确保Ollama服务已设置并运行（不再捕获错误）
  (org-supertag-sim-epc-log "确保Ollama服务已运行...")
  (org-supertag-sim-epc-setup-ollama)
  
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
       (error "初始化过程出错: %s" (error-message-string err)))))) ;; 修改这里，直接抛出错误

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

(defun org-supertag-sim-epc-extract-entities-async (text callback)
  "异步从TEXT中提取实体并将结果传递给CALLBACK函数.
TEXT是要分析的文本
CALLBACK是接收实体列表的回调函数"
  (org-supertag-sim-epc-log "异步提取实体，文本长度: %d" (length text))
  (deferred:$
    (deferred:try
      (deferred:$
        (org-supertag-sim-epc-ensure-server)
        (deferred:nextc it
          (lambda (_)
            (epc:call-deferred org-supertag-sim-epc-manager
                              'extract_entities
                              (list text))))
        (deferred:nextc it
          (lambda (response)
            (let ((status (plist-get response :status))
                  (result (plist-get response :result)))
              (if (string= status "success")
                  (progn
                    (org-supertag-sim-epc-log "实体提取成功，找到 %d 个实体" 
                                              (length result))
                    (funcall callback result))
                (error "提取实体失败: %s" 
                       (or (plist-get response :message) "未知错误")))))))
      :catch
      (lambda (err)
        (org-supertag-sim-epc-log "提取实体出错: %s" (error-message-string err))
        (message "提取实体出错: %s" (error-message-string err))
        (funcall callback nil)))))

(defun org-supertag-sim-epc-get-tag-suggestions-async (text limit callback)
  "异步获取文本的标签建议.
TEXT是要分析的文本
LIMIT是返回的建议数量上限
CALLBACK是接收建议的回调函数"
  (org-supertag-sim-epc-log "异步获取标签建议，文本长度: %d，上限: %d" 
                           (length text) limit)
  (deferred:$
    (deferred:try
      (deferred:$
        (org-supertag-sim-epc-ensure-server)
        (deferred:nextc it
          (lambda (_)
            (epc:call-deferred org-supertag-sim-epc-manager
                              'suggest_tags
                              (list text limit))))
        (deferred:nextc it
          (lambda (response)
            (let ((status (plist-get response :status))
                  (result (plist-get response :result)))
              (if (string= status "success")
                  (progn
                    (org-supertag-sim-epc-log "标签建议成功，找到 %d 个建议" 
                                              (length result))
                    (funcall callback result))
                (error "获取标签建议失败: %s" 
                       (or (plist-get response :message) "未知错误")))))))
      :catch
      (lambda (err)
        (org-supertag-sim-epc-log "获取标签建议出错: %s" (error-message-string err))
        (message "获取标签建议出错: %s" (error-message-string err))
        (funcall callback nil)))))

(defun org-supertag-sim-epc-find-similar-async (tag-name limit callback)
  "异步查找与TAG-NAME相似的标签.
LIMIT是返回的相似标签数量上限
CALLBACK是接收相似标签的回调函数"
  (org-supertag-sim-epc-log "异步查找相似标签: %s，上限: %d" tag-name limit)
  (deferred:$
    (deferred:try
      (deferred:$
        (org-supertag-sim-epc-ensure-server)
        (deferred:nextc it
          (lambda (_)
            (epc:call-deferred org-supertag-sim-epc-manager
                              'find_similar
                              (list tag-name limit))))
        (deferred:nextc it
          (lambda (response)
            (let ((status (plist-get response :status))
                  (result (plist-get response :result)))
              (if (string= status "success")
                  (progn
                    (org-supertag-sim-epc-log "查找相似标签成功，找到 %d 个相似标签" 
                                              (length result))
                    (funcall callback result))
                (error "查找相似标签失败: %s" 
                       (or (plist-get response :message) "未知错误")))))))
      :catch
      (lambda (err)
        (org-supertag-sim-epc-log "查找相似标签出错: %s" (error-message-string err))
        (message "查找相似标签出错: %s" (error-message-string err))
        (funcall callback nil)))))

(defun org-supertag-sim-epc-test-ollama-interaction (prompt &optional system)
  "直接测试与Ollama的交互.
发送PROMPT到Ollama并获取响应，可选的SYSTEM参数用于设置系统提示。
这个函数可以用来验证EPC服务器和Ollama之间的连接是否正常工作。"
  (interactive "sPrompt: ")
  (message "正在发送消息到Ollama...")
  
  ;; 确保系统已初始化
  (require 'org-supertag-sim)
  (unless (and org-supertag-sim--initialized org-supertag-sim-epc-initialized)
    (org-supertag-sim-init))
  
  ;; 创建交互测试缓冲区
  (let ((buffer (get-buffer-create "*org-supertag-ollama-test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "===== Ollama交互测试 =====\n\n")
      (insert (format "时间: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "提示: %s\n" prompt))
      (when system
        (insert (format "系统提示: %s\n" system)))
      (insert "\n正在等待响应...\n\n"))
    
    (display-buffer buffer)
    
    ;; 使用EPC调用Ollama
    (deferred:$
      (deferred:next
        (lambda ()
          (org-supertag-sim-epc-log "调用Ollama...")
          ;; 构建参数列表 - 如果有系统提示则包含
          (let ((args (if system
                          (list prompt system)
                        (list prompt))))
            (epc:call-deferred org-supertag-sim-epc-manager
                              'run_ollama
                              args))))
      
      (deferred:nextc it
        (lambda (response)
          (with-current-buffer buffer
            (goto-char (point-max))
            (let ((status (plist-get response :status))
                  (result (plist-get response :result))
                  (message (plist-get response :message)))
              (if (string= status "success")
                  (progn
                    (insert "===== Ollama响应 =====\n\n")
                    (insert result)
                    (insert "\n\n===== 响应结束 =====\n")
                    (message "收到Ollama响应"))
                (insert (format "\n错误: %s\n" (or message "未知错误")))
                (message "Ollama响应失败: %s" (or message "未知错误")))))))
      
      (deferred:error it
        (lambda (err)
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert (format "\n错误: %s\n" (error-message-string err))))
          (message "调用Ollama出错: %s" (error-message-string err)))))))

(defun org-supertag-sim-epc-interactive-ollama ()
  "启动一个交互式的Ollama对话界面.
创建一个缓冲区，用户可以在其中与Ollama进行对话。"
  (interactive)
  ;; 确保系统已初始化
  (require 'org-supertag-sim)
  (unless (and org-supertag-sim--initialized org-supertag-sim-epc-initialized)
    (org-supertag-sim-init))
  
  ;; 创建或切换到交互缓冲区
  (let ((buffer (get-buffer-create "*org-supertag-ollama-chat*")))
    (switch-to-buffer buffer)
    
    ;; 如果缓冲区是新的，设置它
    (when (= (buffer-size) 0)
      (insert "===== Ollama 交互式对话 =====\n\n")
      (insert "在此输入消息，按 C-c C-c 发送\n\n")
      (insert "系统提示 (可选):\n")
      (insert "--------------------------\n")
      (insert "你是一个专注于Emacs和org-mode的AI助手，专为org-supertag项目提供帮助。\n")
      (insert "--------------------------\n\n")
      (insert "用户消息:\n")
      (insert "请输入您的问题...\n")
      
      ;; 设置本地按键映射
      (local-set-key (kbd "C-c C-c") 'org-supertag-sim-epc--send-message)
      
      ;; 告知用户如何使用
      (message "输入您的消息，然后按 C-c C-c 发送给Ollama"))))

(defun org-supertag-sim-epc--send-message ()
  "从交互式Ollama缓冲区发送消息."
  (interactive)
  (with-current-buffer "*org-supertag-ollama-chat*"
    ;; 提取用户消息和系统提示
    (let ((system-prompt nil)
          (user-message nil))
      
      ;; 获取系统提示
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "--------------------------\n" nil t)
          (let ((start (point)))
            (when (search-forward "--------------------------\n" nil t)
              (setq system-prompt (buffer-substring-no-properties 
                                  start
                                  (- (point) 
                                     (length "--------------------------\n"))))))))
      
      ;; 获取用户消息
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "用户消息:\n" nil t)
          (setq user-message (buffer-substring-no-properties 
                             (point)
                             (point-max)))))
      
      ;; 清空用户消息区域并准备接收回复
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (when (search-forward "用户消息:\n" nil t)
            (delete-region (point) (point-max))
            (insert user-message)
            (insert "\n\n等待Ollama响应...\n"))))
      
      ;; 发送消息到Ollama
      (when user-message
        (deferred:$
          (deferred:next
            (lambda ()
              (let ((args (if system-prompt
                              (list user-message system-prompt)
                            (list user-message))))
                (epc:call-deferred org-supertag-sim-epc-manager
                                  'run_ollama
                                  args))))
          
          (deferred:nextc it
            (lambda (response)
              (with-current-buffer "*org-supertag-ollama-chat*"
                (save-excursion
                  (goto-char (point-max))
                  (let ((status (plist-get response :status))
                        (result (plist-get response :result))
                        (message (plist-get response :message)))
                    (delete-region (- (point-max) 
                                     (length "\n\n等待Ollama响应...\n")) 
                                  (point-max))
                    (if (string= status "success")
                        (progn
                          (insert "\n\nOllama回复:\n")
                          (insert result)
                          (insert "\n\n------\n\n用户消息:\n"))
                      (insert "\n\n错误: " (or message "未知错误") "\n\n用户消息:\n"))))
                (goto-char (point-max)))))
          
          (deferred:error it
            (lambda (err)
              (with-current-buffer "*org-supertag-ollama-chat*"
                (save-excursion
                  (goto-char (point-max))
                  (delete-region (- (point-max) 
                                   (length "\n\n等待Ollama响应...\n")) 
                                (point-max))
                  (insert "\n\n错误: " (error-message-string err) "\n\n用户消息:\n"))
                (goto-char (point-max))))))))))

(provide 'org-supertag-sim-epc)
;;; org-supertag-sim-epc.el ends here

;; 最小化的 EPC 客户端测试
;; 
;; 按照 python-epc 文档实现：
;; https://python-epc.readthedocs.io/en/latest/

(require 'epc)
(require 'deferred)

(defvar test-epc-client-process nil
  "EPC 客户端进程")

(defvar test-epc-client-root-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (expand-file-name (or load-file-name buffer-file-name)))))
  "项目根目录")

(defun test-epc-client-find-python ()
  "查找 Python 可执行文件"
  (or
   ;; 1. 先尝试环境变量中的 PYTHON
   (getenv "PYTHON")
   ;; 2. 尝试常见的 Python 可执行文件名
   (executable-find "python3")
   (executable-find "python")
   ;; 3. 尝试常见的绝对路径
   (let ((common-paths '("/usr/local/bin/python3"
                        "/usr/bin/python3"
                        "/usr/local/bin/python"
                        "/usr/bin/python")))
     (cl-find-if #'file-executable-p common-paths))
   ;; 4. 如果都找不到，报错
   (error "找不到 Python 可执行文件。请设置 PYTHON 环境变量或确保 Python 在 PATH 中")))

(defun test-epc-client-init ()
  "初始化 EPC 客户端"
  (message "开始初始化 EPC 客户端...")
  (when test-epc-client-process
    (message "EPC 客户端已经在运行，先清理...")
    (test-epc-client-cleanup))
  
  (let* ((python-path (test-epc-client-find-python))
         (server-script (expand-file-name "tests/test_epc_server.py" test-epc-client-root-directory)))
    
    (message "项目根目录: %s" test-epc-client-root-directory)
    (message "使用 Python: %s" python-path)
    (message "Python 版本信息:")
    (shell-command (format "%s --version" python-path))
    (message "Python 服务器脚本路径: %s" server-script)
    
    (unless (file-exists-p server-script)
      (error "找不到服务器脚本: %s" server-script))
    
    ;; 设置 Python 环境
    (setenv "PYTHONPATH" test-epc-client-root-directory)
    (message "设置 PYTHONPATH: %s" (getenv "PYTHONPATH"))
    
    (condition-case err
        (progn
          (message "启动 EPC 进程...")
          (let ((default-directory test-epc-client-root-directory)) ; 设置默认目录
            (setq test-epc-client-process
                  (epc:start-epc python-path (list server-script))))
          (message "等待服务器初始化...")
          (sleep-for 1)
          (if (epc:live-p test-epc-client-process)
              (progn
                (message "服务器启动成功")
                (let ((methods (epc:sync test-epc-client-process
                                       (epc:query-methods-deferred test-epc-client-process))))
                  (message "可用方法: %S" methods))
                (message "EPC 客户端初始化成功"))
            (error "服务器未能成功启动")))
      (error
       (setq test-epc-client-process nil)
       (error "EPC 客户端初始化失败: %S" err)))))

(defun test-epc-client-ensure-initialized ()
  "确保 EPC 客户端已初始化"
  (unless (and test-epc-client-process
               (epc:live-p test-epc-client-process))
    (message "EPC 客户端未初始化，正在初始化...")
    (test-epc-client-init)))

(defun test-epc-client-suggest-tags (text &optional sync)
  "测试标签生成
TEXT 是要生成标签的文本
SYNC 非 nil 时使用同步调用"
  (interactive "sEnter text: ")
  
  ;; 确保已初始化
  (condition-case err
      (test-epc-client-ensure-initialized)
    (error
     (error "EPC 客户端初始化失败: %S" err)))

  ;; 检查连接状态  
  (unless (epc:live-p test-epc-client-process)
    (setq test-epc-client-process nil)
    (error "EPC 客户端已断开连接，请重新初始化"))
  
  (message "发送文本: %s" text)
  (if sync
      ;; 同步调用
      (condition-case err
          (let ((tags (epc:call-sync test-epc-client-process 'suggest_tags (list text))))
            (message "收到标签: %S" tags)
            tags)
        (error
         (message "错误: %S" err)
         nil))
    ;; 异步调用
    (deferred:$
      (epc:call-deferred test-epc-client-process 'suggest_tags (list text))
      (deferred:nextc it
        (lambda (tags)
          (message "收到标签: %S" tags)
          tags))
      (deferred:error it
        (lambda (err)
          (message "错误: %S" err)
          nil)))))

(defun test-epc-client-suggest-tags-sync (text)
  "同步方式测试标签生成"
  (interactive "sEnter text: ")
  (test-epc-client-suggest-tags text t))

(defun test-epc-client-cleanup ()
  "清理 EPC 客户端"
  (interactive)
  (when test-epc-client-process
    (condition-case nil
        (epc:stop-epc test-epc-client-process)
      (error nil))
    (setq test-epc-client-process nil)
    (message "EPC 客户端已停止")))

(defun test-epc-client-status ()
  "显示 EPC 客户端状态"
  (interactive)
  (if test-epc-client-process
      (if (epc:live-p test-epc-client-process)
          (message "EPC 客户端状态:
- 连接状态: 正在运行
- 进程 ID: %s
- 连接地址: %s
- 可用方法: %S"
                   (process-id (epc:connection-process test-epc-client-process))
                   (process-contact (epc:connection-process test-epc-client-process))
                   (condition-case err
                       (epc:sync test-epc-client-process
                                (epc:query-methods-deferred test-epc-client-process))
                     (error "获取方法列表失败")))
        (message "EPC 客户端已断开连接"))
    (message "EPC 客户端未初始化")))

;; 使用示例：
;; 1. 检查状态：(test-epc-client-status)
;; 2. 初始化客户端：(test-epc-client-init)
;; 3. 异步测试：(test-epc-client-suggest-tags "你的测试文本")
;; 4. 同步测试：(test-epc-client-suggest-tags-sync "你的测试文本")
;; 5. 清理：(test-epc-client-cleanup) 

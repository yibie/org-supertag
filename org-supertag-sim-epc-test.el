;;; org-supertag-sim-epc-test.el --- Test for SimTag EPC connection

;; Copyright (C) 2023 

;; Author: 
;; Keywords: org-mode, nlp, epc, test

;;; Commentary:
;; 
;; 这个文件提供了测试Org-SuperTag SimTag EPC连接的功能。
;; 它可以帮助诊断EPC服务器的连接问题。
;;

;;; Code:

(require 'cl-lib)
(require 'epc)
(require 'json)

;; 全局变量，保存测试结果
(defvar org-supertag-sim-epc-test-results nil
  "保存测试结果.")

(defun org-supertag-sim-epc-test-display-log ()
  "显示测试日志."
  (interactive)
  (with-current-buffer (get-buffer-create "*simtag-test-log*")
    (display-buffer (current-buffer))))

(defun org-supertag-sim-epc-test-log (format-string &rest args)
  "记录测试日志信息.
FORMAT-STRING 是格式化字符串
ARGS 是格式化参数"
  (let ((msg (apply #'format format-string args)))
    (with-current-buffer (get-buffer-create "*simtag-test-log*")
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
      (insert msg)
      (insert "\n"))
    (message "SimTag Test: %s" msg)))

(defun org-supertag-sim-epc-test-connect-port (port)
  "测试连接到指定端口的EPC服务器.
PORT 是要连接的端口号"
  (interactive "nPort: ")
  (org-supertag-sim-epc-test-log "测试连接到端口 %d" port)
  (condition-case err
      (let ((epc-process nil))
        (org-supertag-sim-epc-test-log "尝试创建EPC连接...")
        (setq epc-process (epc:start-epc "127.0.0.1" port))
        (if (epc:live-p epc-process)
            (progn 
              (org-supertag-sim-epc-test-log "EPC连接成功")
              (let ((result (org-supertag-sim-epc-test-echo epc-process "测试消息")))
                (org-supertag-sim-epc-test-log "Echo测试结果: %S" result))
              (epc:stop-epc epc-process)
              t)
          (org-supertag-sim-epc-test-log "EPC连接失败")
          nil))
    (error
     (org-supertag-sim-epc-test-log "连接出错: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim-epc-test-echo (epc-process msg)
  "测试EPC服务器的echo功能.
EPC-PROCESS 是EPC连接
MSG 是要发送的消息"
  (org-supertag-sim-epc-test-log "测试Echo功能，发送消息: %s" msg)
  (condition-case err
      (let ((result (epc:call-sync epc-process 'echo (list msg) 3)))
        (org-supertag-sim-epc-test-log "Echo响应: %S" result)
        result)
    (error
     (org-supertag-sim-epc-test-log "Echo测试出错: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim-epc-test-status (epc-process)
  "获取EPC服务器状态.
EPC-PROCESS 是EPC连接"
  (org-supertag-sim-epc-test-log "获取服务器状态...")
  (condition-case err
      (let ((result (epc:call-sync epc-process 'get_server_status nil 3)))
        (org-supertag-sim-epc-test-log "服务器状态: %S" result)
        result)
    (error
     (org-supertag-sim-epc-test-log "获取状态出错: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim-epc-test-extract-entities (epc-process text)
  "测试实体提取功能.
EPC-PROCESS 是EPC连接
TEXT 是要提取实体的文本"
  (org-supertag-sim-epc-test-log "测试实体提取，文本: %s" text)
  (condition-case err
      (let ((result (epc:call-sync epc-process 'extract_entities (list text) 5)))
        (org-supertag-sim-epc-test-log "提取结果: %S" result)
        result)
    (error
     (org-supertag-sim-epc-test-log "实体提取出错: %s" (error-message-string err))
     nil)))

;;;###autoload
(defun org-supertag-sim-epc-test-check-python-script ()
  "检查Python脚本是否存在."
  (interactive)
  (let ((script-path (expand-file-name "simtag_epc.py"
                                      (file-name-directory (locate-library "org-supertag-sim-epc")))))
    (if (file-exists-p script-path)
        (progn
          (org-supertag-sim-epc-test-log "Python脚本存在: %s" script-path)
          (let ((size (nth 7 (file-attributes script-path)))
                (mod-time (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (nth 5 (file-attributes script-path)))))
            (org-supertag-sim-epc-test-log "脚本大小: %d 字节, 修改时间: %s" size mod-time)))
      (org-supertag-sim-epc-test-log "Python脚本不存在: %s" script-path))))

;;;###autoload
(defun org-supertag-sim-epc-test-check-env-script ()
  "检查环境脚本是否存在."
  (interactive)
  (let ((script-path (expand-file-name "run_simtag_epc_venv.sh"
                                       (file-name-directory (locate-library "org-supertag-sim-epc")))))
    (if (file-exists-p script-path)
        (progn
          (org-supertag-sim-epc-test-log "环境脚本存在: %s" script-path)
          (let ((size (nth 7 (file-attributes script-path)))
                (mod-time (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (nth 5 (file-attributes script-path))))
                (executable (file-executable-p script-path)))
            (org-supertag-sim-epc-test-log "脚本大小: %d 字节, 修改时间: %s, 可执行: %s" 
                                          size mod-time executable)
            (unless executable
              (org-supertag-sim-epc-test-log "警告: 脚本不可执行"))))
      (org-supertag-sim-epc-test-log "环境脚本不存在: %s" script-path))))

;;;###autoload
(defun org-supertag-sim-epc-test-python-version ()
  "检查Python版本."
  (interactive)
  (org-supertag-sim-epc-test-log "检查Python版本...")
  (let ((output (shell-command-to-string "python3 --version 2>&1")))
    (org-supertag-sim-epc-test-log "Python版本: %s" output)
    ;; 检查Python是否可用
    (if (string-match "Python 3\\.[0-9]+" output)
        (org-supertag-sim-epc-test-log "Python 3可用")
      (org-supertag-sim-epc-test-log "警告: 未检测到Python 3"))))

;;;###autoload
(defun org-supertag-sim-epc-test-port (port)
  "检查端口是否被占用.
PORT 是要检查的端口号"
  (interactive "nPort: ")
  (org-supertag-sim-epc-test-log "检查端口 %d 是否被占用..." port)
  (let ((output (shell-command-to-string (format "lsof -i :%d" port))))
    (if (string-empty-p output)
        (org-supertag-sim-epc-test-log "端口 %d 未被占用" port)
      (org-supertag-sim-epc-test-log "端口 %d 已被占用:\n%s" port output))))

;;;###autoload
(defun org-supertag-sim-epc-run-tests ()
  "运行所有EPC服务器测试."
  (interactive)
  ;; 创建或清空日志缓冲区
  (with-current-buffer (get-buffer-create "*simtag-test-log*")
    (erase-buffer)
    (display-buffer (current-buffer)))
  
  (setq org-supertag-sim-epc-test-results nil)
  
  (org-supertag-sim-epc-test-log "开始SimTag EPC测试...")
  
  ;; 检查脚本文件
  (org-supertag-sim-epc-test-check-python-script)
  (org-supertag-sim-epc-test-check-env-script)
  
  ;; 检查Python版本
  (org-supertag-sim-epc-test-python-version)
  
  ;; 检查端口
  (org-supertag-sim-epc-test-port 21278)
  
  ;; 测试EPC连接
  (let ((port 21278))
    (org-supertag-sim-epc-test-log "===============================================")
    (org-supertag-sim-epc-test-log "测试EPC连接到端口 %d" port)
    (org-supertag-sim-epc-test-log "===============================================")
    
    (condition-case err
        (let ((epc-process nil))
          (org-supertag-sim-epc-test-log "尝试创建EPC连接...")
          (setq epc-process (epc:start-epc "127.0.0.1" port))
          (if (epc:live-p epc-process)
              (progn 
                (org-supertag-sim-epc-test-log "EPC连接成功")
                
                ;; 测试echo功能
                (let ((echo-result (org-supertag-sim-epc-test-echo epc-process "测试消息")))
                  (push `(:test "echo" :result ,echo-result) org-supertag-sim-epc-test-results))
                
                ;; 测试获取状态
                (let ((status-result (org-supertag-sim-epc-test-status epc-process)))
                  (push `(:test "status" :result ,status-result) org-supertag-sim-epc-test-results))
                
                ;; 测试实体提取
                (let ((extract-result (org-supertag-sim-epc-test-extract-entities epc-process "这是一个测试文本，用于测试实体提取功能。")))
                  (push `(:test "extract" :result ,extract-result) org-supertag-sim-epc-test-results))
                
                ;; 停止EPC进程
                (epc:stop-epc epc-process)
                (org-supertag-sim-epc-test-log "EPC连接已关闭"))
            (org-supertag-sim-epc-test-log "EPC连接失败")))
      (error
       (org-supertag-sim-epc-test-log "连接出错: %s" (error-message-string err)))))
  
  ;; 显示测试结果摘要
  (org-supertag-sim-epc-test-log "===============================================")
  (org-supertag-sim-epc-test-log "测试结果摘要")
  (org-supertag-sim-epc-test-log "===============================================")
  
  (let ((all-passed t))
    (dolist (result org-supertag-sim-epc-test-results)
      (let ((test-name (plist-get result :test))
            (test-result (plist-get result :result)))
        (if test-result
            (org-supertag-sim-epc-test-log "测试 '%s': 通过" test-name)
          (org-supertag-sim-epc-test-log "测试 '%s': 失败" test-name)
          (setq all-passed nil))))
    
    (org-supertag-sim-epc-test-log "===============================================")
    (if all-passed
        (org-supertag-sim-epc-test-log "所有测试通过！")
      (org-supertag-sim-epc-test-log "有测试失败，请检查日志详情。")))
  
  (org-supertag-sim-epc-test-log "测试完成"))

(provide 'org-supertag-sim-epc-test)
;;; org-supertag-sim-epc-test.el ends here 
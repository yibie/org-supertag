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
(require 'org-supertag-sim-epc)

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
      (let ((result (epc:call-sync epc-process 'echo (list msg))))
        (org-supertag-sim-epc-test-log "Echo响应: %S" result)
        result)
    (error
     (org-supertag-sim-epc-test-log "Echo测试出错: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim-epc-test-initialize (epc-process)
  "测试初始化功能.
EPC-PROCESS 是EPC连接"
  (org-supertag-sim-epc-test-log "测试初始化功能...")
  (condition-case err
      (let* ((vector-file org-supertag-sim-epc-vector-file)
             (db-file org-supertag-db-file)
             (result (epc:call-sync epc-process 'initialize (list vector-file db-file))))
        (org-supertag-sim-epc-test-log "初始化结果: %S" result)
        result)
    (error
     (org-supertag-sim-epc-test-log "初始化测试出错: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim-epc-test-extract-entities (epc-process text)
  "测试实体提取功能.
EPC-PROCESS 是EPC连接
TEXT 是要提取实体的文本"
  (org-supertag-sim-epc-test-log "测试实体提取，文本: %s" text)
  (condition-case err
      (let ((result (epc:call-sync epc-process 'extract_entities (list text))))
        (org-supertag-sim-epc-test-log "提取结果: %S" result)
        (let ((status (plist-get result :status))
              (entities (plist-get result :result))
              (error-msg (plist-get result :message)))
          (when (string= status "success")
            (org-supertag-sim-epc-test-log "提取成功，发现 %d 个实体" (length entities))
            (dolist (entity entities)
              (let ((text (cdr (assoc 'text entity)))
                    (type (cdr (assoc 'type entity))))
                (org-supertag-sim-epc-test-log "  实体: %s [%s]" text type))))
          (when error-msg
            (org-supertag-sim-epc-test-log "错误信息: %s" error-msg)))
        result)
    (error
     (org-supertag-sim-epc-test-log "实体提取出错: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim-epc-test-suggest-tags (epc-process text)
  "测试标签建议功能.
EPC-PROCESS 是EPC连接
TEXT 是要生成标签的文本"
  (org-supertag-sim-epc-test-log "测试标签建议，文本: %s" text)
  (condition-case err
      (let ((result (epc:call-sync epc-process 'suggest_tags (list text))))
        (org-supertag-sim-epc-test-log "标签建议结果: %S" result)
        (let ((status (plist-get result :status))
              (tags (plist-get result :result))
              (error-msg (plist-get result :message)))
          (when (string= status "success")
            (org-supertag-sim-epc-test-log "生成成功，建议 %d 个标签" (length tags))
            (org-supertag-sim-epc-test-log "  标签: %s" (mapconcat #'identity tags ", ")))
          (when error-msg
            (org-supertag-sim-epc-test-log "错误信息: %s" error-msg)))
        result)
    (error
     (org-supertag-sim-epc-test-log "标签建议出错: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim-epc-test-find-similar (epc-process tag-name)
  "测试相似标签查找功能.
EPC-PROCESS 是EPC连接
TAG-NAME 是要查找相似标签的标签名"
  (org-supertag-sim-epc-test-log "测试相似标签查找，标签: %s" tag-name)
  (condition-case err
      (let ((result (epc:call-sync epc-process 'find_similar (list tag-name 5))))
        (org-supertag-sim-epc-test-log "相似标签结果: %S" result)
        result)
    (error
     (org-supertag-sim-epc-test-log "相似标签查找出错: %s" (error-message-string err))
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
(defun org-supertag-sim-epc-test-check-module-structure ()
  "检查Python模块结构."
  (interactive)
  (let* ((base-dir (file-name-directory (locate-library "org-supertag-sim-epc")))
         (simtag-dir (expand-file-name "simtag" base-dir))
         (entity-extractor (expand-file-name "entity_extractor.py" simtag-dir))
         (epc-server (expand-file-name "epc_server.py" simtag-dir))
         (init-file (expand-file-name "__init__.py" simtag-dir)))
    
    (org-supertag-sim-epc-test-log "检查Python模块结构...")
    
    ;; 检查simtag目录
    (if (file-exists-p simtag-dir)
        (org-supertag-sim-epc-test-log "模块目录存在: %s" simtag-dir)
      (org-supertag-sim-epc-test-log "警告: 模块目录不存在: %s" simtag-dir))
    
    ;; 检查初始化文件
    (if (file-exists-p init-file)
        (org-supertag-sim-epc-test-log "初始化文件存在: %s" init-file)
      (org-supertag-sim-epc-test-log "警告: 初始化文件不存在: %s" init-file))
    
    ;; 检查实体提取器文件
    (if (file-exists-p entity-extractor)
        (org-supertag-sim-epc-test-log "实体提取器文件存在: %s" entity-extractor)
      (org-supertag-sim-epc-test-log "警告: 实体提取器文件不存在: %s" entity-extractor))
    
    ;; 检查EPC服务器文件
    (if (file-exists-p epc-server)
        (org-supertag-sim-epc-test-log "EPC服务器文件存在: %s" epc-server)
      (org-supertag-sim-epc-test-log "警告: EPC服务器文件不存在: %s" epc-server))))

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
(defun org-supertag-sim-epc-test-current-server ()
  "测试当前运行的EPC服务器."
  (interactive)
  ;; 创建或清空日志缓冲区
  (with-current-buffer (get-buffer-create "*simtag-test-log*")
    (erase-buffer)
    (display-buffer (current-buffer)))
  
  (setq org-supertag-sim-epc-test-results nil)
  
  (org-supertag-sim-epc-test-log "开始测试当前EPC服务器...")
  
  (if (not (and org-supertag-sim-epc-manager 
               (epc:live-p org-supertag-sim-epc-manager)))
      (org-supertag-sim-epc-test-log "错误: 当前没有活动的EPC服务器连接")
    
    (org-supertag-sim-epc-test-log "测试当前EPC连接...")
    
    ;; 测试echo功能
    (condition-case err
        (let ((echo-result (epc:call-sync org-supertag-sim-epc-manager 'echo '("测试消息"))))
          (org-supertag-sim-epc-test-log "Echo测试结果: %S" echo-result)
          (push `(:test "echo" :result ,echo-result) org-supertag-sim-epc-test-results))
      (error
       (org-supertag-sim-epc-test-log "Echo测试出错: %s" (error-message-string err))
       (push `(:test "echo" :result nil) org-supertag-sim-epc-test-results)))
    
    ;; 测试文本分析
    (let ((test-texts '("这是一个测试文本，用于测试实体提取功能。"
                         "Python是一种流行的编程语言，被广泛应用于Web开发、数据分析和机器学习。"
                         "中国的首都是北京，上海是中国最大的城市之一。")))
      
      (dolist (text test-texts)
        ;; 测试实体提取
        (condition-case err
            (let ((extract-result (epc:call-sync org-supertag-sim-epc-manager 'extract_entities (list text))))
              (org-supertag-sim-epc-test-log "实体提取测试结果: %S" extract-result)
              (push `(:test ,(format "extract:%s" (substring text 0 10)) :result ,extract-result) 
                    org-supertag-sim-epc-test-results))
          (error
           (org-supertag-sim-epc-test-log "实体提取测试出错: %s" (error-message-string err))
           (push `(:test ,(format "extract:%s" (substring text 0 10)) :result nil) 
                 org-supertag-sim-epc-test-results)))
        
        ;; 测试标签建议
        (condition-case err
            (let ((suggest-result (epc:call-sync org-supertag-sim-epc-manager 'suggest_tags (list text))))
              (org-supertag-sim-epc-test-log "标签建议测试结果: %S" suggest-result)
              (push `(:test ,(format "suggest:%s" (substring text 0 10)) :result ,suggest-result) 
                    org-supertag-sim-epc-test-results))
          (error
           (org-supertag-sim-epc-test-log "标签建议测试出错: %s" (error-message-string err))
           (push `(:test ,(format "suggest:%s" (substring text 0 10)) :result nil) 
                 org-supertag-sim-epc-test-results))))))
  
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

;;;###autoload
(defun org-supertag-sim-epc-test-ollama ()
  "测试Ollama服务是否可用."
  (interactive)
  (org-supertag-sim-epc-test-log "测试Ollama服务可用性...")
  (let* (;; 获取配置或使用默认值
         (ollama-host (if (boundp 'org-supertag-sim-ollama-host) 
                         org-supertag-sim-ollama-host 
                        "127.0.0.1"))
         (ollama-port (if (boundp 'org-supertag-sim-ollama-port) 
                         org-supertag-sim-ollama-port 
                        11434))
         (default-model (if (boundp 'org-supertag-sim-ollama-model)
                           org-supertag-sim-ollama-model
                          "hf.co/unsloth/gemma-3-4b-it-GGUF:latest"))
         (url (format "http://%s:%d/api/tags" ollama-host ollama-port))
         (buffer (generate-new-buffer "*simtag-ollama-test*"))
         (status nil)
         (result nil))
    
    (org-supertag-sim-epc-test-log "使用Ollama配置: 主机=%s, 端口=%d, 默认模型=%s" 
                                   ollama-host ollama-port default-model)
    
    (with-current-buffer buffer
      (setq mode-line-process '(":%s"))
      (let ((url-request-method "GET"))
        (condition-case err
            (progn
              (org-supertag-sim-epc-test-log "请求 Ollama API: %s" url)
              (url-insert-file-contents url)
              (setq status t)
              (goto-char (point-min))
              (condition-case json-err
                  (progn
                    (setq result (json-read))
                    (org-supertag-sim-epc-test-log "Ollama服务可用，已获取模型列表"))
                (error
                 (org-supertag-sim-epc-test-log "解析API响应失败: %s" (error-message-string json-err)))))
          (error
           (setq status nil)
           (org-supertag-sim-epc-test-log "连接Ollama服务失败: %s" (error-message-string err))))))
    
    (when result
      (let ((models (cdr (assoc 'models result)))
            (model-list '()))
        (org-supertag-sim-epc-test-log "已安装的模型:")
        (condition-case err
            (if (vectorp models)
                ;; 向量形式
                (dotimes (i (length models))
                  (let* ((model (aref models i))
                         (name (cdr (assoc 'name model))))
                    (push name model-list)
                    (org-supertag-sim-epc-test-log "- %s" name)))
              ;; 列表形式（兼容旧版本）
              (dolist (model models)
                (let ((name (cdr (assoc 'name model))))
                  (push name model-list)
                  (org-supertag-sim-epc-test-log "- %s" name))))
          (error
           (org-supertag-sim-epc-test-log "处理模型列表时出错: %s" (error-message-string err))))
        
        (if (member default-model model-list)
            (org-supertag-sim-epc-test-log "当前配置的默认模型 '%s' 可用" default-model)
          (org-supertag-sim-epc-test-log "警告: 当前配置的默认模型 '%s' 未安装" default-model))))
    
    (kill-buffer buffer)
    status))

(defun org-supertag-sim-epc-test-ollama-connection ()
  "测试EPC服务器与Ollama的连接是否正常工作."
  (interactive)
  (org-supertag-sim-epc-test-log "测试EPC服务器到Ollama的连接...")
  
  (if (not (and org-supertag-sim-epc-manager 
               (epc:live-p org-supertag-sim-epc-manager)))
      (org-supertag-sim-epc-test-log "错误: 当前没有活动的EPC服务器连接，请先初始化EPC服务器")
    
    (condition-case err
        (let ((result (epc:call-sync org-supertag-sim-epc-manager 'test_ollama_connection nil)))
          (org-supertag-sim-epc-test-log "Ollama连接测试结果: %S" result)
          (let ((status (plist-get result :status))
                (models (plist-get result :models))
                (message (plist-get result :message)))
            (if (string= status "success")
                (progn
                  (org-supertag-sim-epc-test-log "Ollama连接成功!")
                  (when models
                    (org-supertag-sim-epc-test-log "可用模型:")
                    (dolist (model models)
                      (org-supertag-sim-epc-test-log "- %s" model))))
              (org-supertag-sim-epc-test-log "Ollama连接失败: %s" (or message "未知错误")))))
      (error
       (org-supertag-sim-epc-test-log "测试Ollama连接时出错: %s" (error-message-string err))))))

;;;###autoload
(defun org-supertag-sim-epc-test-generate-sample-text ()
  "测试使用Ollama生成示例文本."
  (interactive)
  (org-supertag-sim-epc-test-log "测试使用Ollama生成示例文本...")
  
  (if (not (and org-supertag-sim-epc-manager 
               (epc:live-p org-supertag-sim-epc-manager)))
      (org-supertag-sim-epc-test-log "错误: 当前没有活动的EPC服务器连接，请先初始化EPC服务器")
    
    (condition-case err
        (let* ((prompt "请生成一段关于人工智能的短文本，不超过50个字")
               (result (epc:call-sync org-supertag-sim-epc-manager 'generate_text (list prompt))))
          (org-supertag-sim-epc-test-log "文本生成测试结果: %S" result)
          (let ((status (plist-get result :status))
                (text (plist-get result :text))
                (message (plist-get result :message)))
            (if (string= status "success")
                (org-supertag-sim-epc-test-log "成功生成文本: %s" text)
              (org-supertag-sim-epc-test-log "生成文本失败: %s" (or message "未知错误")))))
      (error
       (org-supertag-sim-epc-test-log "测试生成文本时出错: %s" (error-message-string err))))))

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
  (org-supertag-sim-epc-test-check-module-structure)
  
  ;; 检查Python版本
  (org-supertag-sim-epc-test-python-version)
  
  ;; 测试Ollama服务
  (let ((ollama-status (org-supertag-sim-epc-test-ollama)))
    (push `(:test "ollama" :result ,ollama-status) org-supertag-sim-epc-test-results))
  
  ;; 检查端口
  (let ((port (+ 10000 (random 10000))))
    (org-supertag-sim-epc-test-port port))
  
  ;; 检查当前服务器
  (if (and org-supertag-sim-epc-manager
          (epc:live-p org-supertag-sim-epc-manager))
      (progn
        (org-supertag-sim-epc-test-log "当前有活动的EPC服务器连接")
        (org-supertag-sim-epc-test-current-server)
        
        ;; 测试EPC服务器与Ollama的连接
        (org-supertag-sim-epc-test-log "测试EPC服务器与Ollama的连接...")
        (condition-case err
            (let ((ollama-conn-result (epc:call-sync org-supertag-sim-epc-manager 'test_ollama_connection nil)))
              (org-supertag-sim-epc-test-log "EPC-Ollama连接测试结果: %S" ollama-conn-result)
              (push `(:test "epc-ollama" :result ollama-conn-result) org-supertag-sim-epc-test-results))
          (error
           (org-supertag-sim-epc-test-log "EPC-Ollama连接测试失败: %s" (error-message-string err))
           (push `(:test "epc-ollama" :result nil) org-supertag-sim-epc-test-results))))
    
    ;; 如果没有活动服务器，尝试启动一个新的
    (org-supertag-sim-epc-test-log "启动新的EPC服务器进行测试...")
    (condition-case err
        (progn
          (org-supertag-sim-epc-log "启动EPC服务器...")
          (org-supertag-sim-epc-start-server)
          (sit-for 3)  ;; 给服务器一些启动时间
          
          (if (and org-supertag-sim-epc-manager
                  (epc:live-p org-supertag-sim-epc-manager))
              (progn
                (org-supertag-sim-epc-test-log "EPC服务器启动成功")
                (org-supertag-sim-epc-test-current-server))
            (org-supertag-sim-epc-test-log "EPC服务器启动失败")))
      (error
       (org-supertag-sim-epc-test-log "启动EPC服务器出错: %s" (error-message-string err)))))
  
  (org-supertag-sim-epc-test-log "测试完成"))

(provide 'org-supertag-sim-epc-test)
;;; org-supertag-sim-epc-test.el ends here 
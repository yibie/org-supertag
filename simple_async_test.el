;;; simple_async_test.el --- 简单的异步测试 -*- lexical-binding: t; -*-

(require 'org-supertag-bridge)

(defvar test-async-start-time nil)
(defvar test-async-sessions-before nil)
(defvar test-async-sessions-after nil)
(defvar test-async-deferred nil)

;; 全局调试开关
(defvar org-supertag-debug-mode nil
  "全局调试模式开关。设置为 t 时显示详细的调试信息。")

(defun org-supertag-toggle-debug-mode ()
  "切换调试模式。"
  (interactive)
  (setq org-supertag-debug-mode (not org-supertag-debug-mode))
  (when org-supertag-debug-mode
    ;; 启用时设置所有调试选项
    (setq org-supertag-bridge-epc-debug t)
    (setq org-supertag-bridge-deferred-debug t)
    (setq org-supertag-bridge-enable-verbose-async-debug t))
  (message "Org SuperTag 调试模式: %s" 
           (if org-supertag-debug-mode "已启用" "已禁用")))

(defun test-variable-scope ()
  "测试异步回调中的变量作用域问题"
  (interactive)
  (let ((test-var "我是外层变量")
        (test-list '(1 2 3)))
    
    (message "=== 变量作用域测试 ===")
    (message "调用前 test-var: %s" test-var)
    (message "调用前 test-list: %S" test-list)
    
    ;; 异步调用，回调中访问外层变量
    (let ((call-deferred (org-supertag-bridge-epc-call-deferred
                          org-supertag-bridge--python-epc-manager
                          'get_status
                          '())))
      
      (org-supertag-bridge-deferred-nextc 
       call-deferred
       (lambda (result)
         (message "🔍 回调中访问变量:")
         (message "  test-var: %s" test-var)
         (message "  test-list: %S" test-list)
         (message "  result status: %s" (plist-get result :status)))))))

(defun simple-async-test ()
  "测试异步调用"
  (interactive)
  ;; 启用调试
  ;; (setq org-supertag-bridge-epc-debug t)
  ;; (setq org-supertag-bridge-deferred-debug t)  ; 启用 deferred 调试
  
  (let ((test-async-start-time (current-time)))
    (message "=== 简单异步测试 ===")
    
    ;; 清空日志缓冲区
    (when (get-buffer "*org-supertag-bridge-epc-log*")
      (with-current-buffer "*org-supertag-bridge-epc-log*"
        (erase-buffer)))
    (when (get-buffer "*org-supertag-bridge-deferred-log*")
      (with-current-buffer "*org-supertag-bridge-deferred-log*"
        (erase-buffer)))
    
    (message "调用前 sessions: %S" (org-supertag-bridge-epc-manager-sessions org-supertag-bridge--python-epc-manager))
    
    ;; 创建一个简单的 deferred 对象来测试
    (let* ((simple-callback (lambda (result)
                              (message "🎉 简单回调执行！结果: %S" result)
                              (let ((elapsed (float-time (time-since test-async-start-time))))
                                (message "回调耗时: %.3f秒" elapsed))))
           (deferred-obj (org-supertag-bridge-deferred-new simple-callback)))
      
      ;; 直接调用 EPC
      (let ((call-deferred (org-supertag-bridge-epc-call-deferred
                            org-supertag-bridge--python-epc-manager
                            'get_status
                            '())))
        
        ;; 链接回调
        (org-supertag-bridge-deferred-nextc call-deferred simple-callback)
        
        (message "调用后 sessions: %S" (org-supertag-bridge-epc-manager-sessions org-supertag-bridge--python-epc-manager))
        (message "Deferred 对象: %S" call-deferred)
        
        ;; 2秒后检查结果
        (run-with-timer 2 nil
                        (lambda ()
                          (message "=== 2秒后检查结果 ===")
                          (message "当前 sessions: %S" (org-supertag-bridge-epc-manager-sessions org-supertag-bridge--python-epc-manager))
                          (message "Deferred 状态: %S" (org-supertag-bridge-deferred-object-status call-deferred))
                          (message "Deferred 值: %S" (org-supertag-bridge-deferred-object-value call-deferred))
                          
                          ;; 显示日志内容
                          (when (get-buffer "*org-supertag-bridge-epc-log*")
                            (message "=== EPC 日志内容 ===")
                            (with-current-buffer "*org-supertag-bridge-epc-log*"
                              (message "%s" (buffer-string))))
                          
                          (when (get-buffer "*org-supertag-bridge-deferred-log*")
                            (message "=== Deferred 日志内容 ===")
                            (with-current-buffer "*org-supertag-bridge-deferred-log*"
                              (message "%s" (buffer-string))))))))))

(defun test-deferred-chain-debug ()
  "详细跟踪 deferred 链的执行过程"
  (interactive)
  (message "=== Deferred 链调试测试 ===")
  
  ;; 启用所有调试
  ;; (setq org-supertag-bridge-epc-debug t)
  ;; (setq org-supertag-bridge-deferred-debug t)
  ;; (setq org-supertag-bridge-enable-verbose-async-debug t)
  
  ;; 清空日志
  (when (get-buffer "*org-supertag-bridge-deferred-log*")
    (with-current-buffer "*org-supertag-bridge-deferred-log*"
      (erase-buffer)))
  
  (let ((d (org-supertag-bridge-epc-call-deferred
            org-supertag-bridge--python-epc-manager
            'get_status
            '())))
    
    (message "1. 创建了 EPC deferred: %S" d)
    (message "   初始状态: %S" (org-supertag-bridge-deferred-object-status d))
    (message "   初始值: %S" (org-supertag-bridge-deferred-object-value d))
    
    ;; 添加回调
    (let ((result-deferred 
           (org-supertag-bridge-deferred-nextc d
             (lambda (result)
               (message "🎯 用户回调被执行！")
               (message "   result: %S" result)
               result))))
      
      (message "2. 添加回调后的 deferred: %S" result-deferred)
      (message "   原始 deferred 的 next: %S" (org-supertag-bridge-deferred-object-next d))
      
      ;; 5秒后检查状态
      (run-with-timer 5 nil
                      (lambda ()
                        (message "=== 5秒后状态检查 ===")
                        (message "原始 deferred 状态: %S" (org-supertag-bridge-deferred-object-status d))
                        (message "原始 deferred 值: %S" (org-supertag-bridge-deferred-object-value d))
                        (message "结果 deferred 状态: %S" (org-supertag-bridge-deferred-object-status result-deferred))
                        (message "结果 deferred 值: %S" (org-supertag-bridge-deferred-object-value result-deferred))
                        
                        ;; 显示 deferred 日志
                        (when (get-buffer "*org-supertag-bridge-deferred-log*")
                          (message "=== Deferred 日志 ===")
                          (with-current-buffer "*org-supertag-bridge-deferred-log*"
                            (message "%s" (buffer-string)))))))))

(defun test-bridge-callback ()
  "直接测试 bridge 层的回调处理"
  (interactive)
  (message "=== 测试 Bridge 层回调 ===")
  
  ;; 启用详细调试
  ;; (setq org-supertag-bridge-enable-verbose-async-debug t)
  
  (org-supertag-bridge-call-async
   "get_status"
   nil
   (lambda (result)
     (message "🔥 Bridge 回调执行！")
     (message "  result: %S" result)
     (message "  status: %s" (if result (plist-get result :status) "nil"))))
  
  (message "Bridge 调用已发送，等待回调..."))

(defun test-api-bulk-process ()
  "测试 API bulk process 调用"
  (interactive)
  (message "=== 测试 API bulk process ===")
  
  ;; 创建一个简单的测试数据
  (let ((test-data `(("nodes" . ())
                     ("links" . ())
                     ("ids_to_delete" . ())
                     ("sync_timestamp" . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))))
    
    (message "发送测试数据: %S" test-data)
    
    (org-supertag-api-bulk-process-snapshot
     test-data
     (lambda (result)
       (message "🎯 API 回调执行！")
       (message "  result: %S" result)
       (message "  status: %s" (if result (plist-get result :status) "nil"))))))

;; 运行测试
;;(simple-async-test)
;;(test-variable-scope)
;;(test-api-bulk-process)
;;(test-deferred-chain-debug)

;; 使用说明：
;; 1. 启用调试模式：M-x org-supertag-toggle-debug-mode
;; 2. 运行测试：(test-deferred-chain-debug) 或其他测试函数
;; 3. 禁用调试模式：再次运行 M-x org-supertag-toggle-debug-mode

  配置方法

  您需要在您的 Emacs 配置文件（例如 init.el）中，通过 setq 来定义 org-supertag-capture-templates 变量。

  这个变量是一个列表（list），其中每个元素都代表一个捕获模板。每个模板本身也是一个列表，格式如下：

  `emacs-lisp
  ("模板快捷名"
   :description "模板的描述文字"
   :file "目标文件路径"
   :position 插入位置
   :level 标题级别)
   1 
   2 #### 参数详解：
   3 
   * `"模板快捷名"`: 一个简短的字符串，用于在选择模板时识别它（例如 "t" 代表任务，"n" 代表笔记）。
   * `:description`: 对这个模板的详细描述。
   * `:file`:
       * 指定一个绝对文件路径（例如 "/path/to/your/inbox.org"），捕获的内容将直接存入该文件。
       * 如果设置为 nil，每次捕获时都会提示您选择一个文件。
   * `:position`:
       * :prompt: 每次捕获时提示您选择插入位置（文件头、文件尾、某个标题下等）。
       * :end: 插入到文件的末尾。
       * :content-start: 插入到文件的内容区域的开始处（跳过文件头部的 #+TITLE: 等设置）。
   * `:level`:
       * :auto: 自动推断标题级别。
       * 1, 2, 3...: 指定一个固定的标题级别。

  配置示例

  您可以将以下代码添加到您的 Emacs 配置文件中，并根据自己的需求修改。这段代码定义了几个常用的捕获模板：

  `emacs-lisp
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org-supertag 捕获模板配置
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq org-supertag-capture-templates
        '(;; 模板一：一个灵活的、每次都会询问细节的默认模板
          ("default"
           :description "Default flexible capture"
           :file nil         ; 每次都询问要存到哪个文件
           :position :prompt ; 每次都询问要插入到文件的哪个位置
           :level :auto)     ; 自动决定标题级别

          ;; 模板二：快速将任务存入收件箱
          ("inbox"
           :description "Quick inbox capture"
           :file "~/org/inbox.org" ; 直接存到 inbox.org
           :position :end          ; 放在文件末尾
           :level 1)               ; 标题级别为 1

          ;; 模板三：记录日常笔记
          ("notes"
           :description "Daily notes"
           :file "~/org/notes.org"  ; 直接存到 notes.org
           :position :end           ; 放在文件末尾
           :level 2)                ; 标题级别为 2

          ;; 模板四：记录想法
          ("ideas"
           :description "Ideas and thoughts"
           :file "~/org/ideas.org"  ; 直接存到 ideas.org
           :position :content-start ; 放在文件内容的最前面
           :level 1)                ; 标题级别为 1
          ))

  ;; (可选) 设置一个默认的快速捕获模板
  ;; 当您使用 org-supertag-node-capture-quick (如果定义了该命令) 时会使用此模板
  (setq org-supertag-capture-default-template "inbox")

   1 
   2 ### 如何使用
   3 
   1. 将上面的配置代码加入您的 Emacs 初始化文件。
   2. 确保修改文件路径（例如 ~/org/inbox.org）为您自己实际使用的路径。
   3. 重启 Emacs 或执行 M-x eval-buffer 使配置生效。
   4. 当您调用 org-supertag-node-capture 或其他相关的捕获命令时，Emacs 就会根据您的配置来执行操作了。

  文件中还提到了一个 org-supertag-node-setup-capture-keys 函数，您可以执行 M-x org-supertag-node-setup-capture-keys 来绑定推荐的快捷键，方便您调用这些捕获功能。

  
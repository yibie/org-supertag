  项目最终计划：实现原生 `org-supertag-capture` 系统

  核心目标：构建一个功能强大、语法清晰、高度可扩展的原生捕获系统。该系统将封装在独立的 org-supertag-capture.el 文件中，以 field 体系为核心，同时为用户提供将字段同步至
  Org :PROPERTIES: 抽屉的选项，实现数据力量与视觉直观性的完美结合。

  ---

  第一阶段：架构重构与配置体系建立

  此阶段的目标是搭建好舞台，定义好用户与新系统交互的接口。

   * 步骤 1：创建并独立文件
       * 创建新文件 org-supertag-capture.el。
       * 将 org-supertag-node.el 中所有与“捕获”功能相关的函数迁移至此新文件。
       * 在新旧文件中正确设置 require 依赖关系，并在项目主入口文件（如 org-supertag.el）中加载新文件，确保项目结构清晰、无误。

   * 步骤 2：定义捕获模板变量
       * 在 org-supertag-capture.el 中，使用 defcustom 定义 org-supertag-capture-templates。
       * 为其提供一个默认值，该值应为一个或多个使用我们最终设计的“纯粹”原生 plist 语法的模板示例，以引导用户。

   * 步骤 3：定义属性同步开关
       * 同样在 org-supertag-capture.el 中，使用 defcustom 定义一个新的布尔型用户配置变量 org-supertag-capture-sync-fields-to-properties。
       * 为其编写清晰的文档，解释其作用：当开启时，通过 :field 捕获的数据会同时写入数据库和 Org 文件的 :PROPERTIES: 抽屉；关闭时，则只写入数据库。
       * 设定默认值为 `nil`（关闭），以鼓励用户优先使用 org-supertag 更强大、更原生的数据库功能。

  第二阶段：核心模板处理引擎实现

  此阶段是项目的技术核心，负责解析我们设计的原生模板。

   * 步骤 4：设计核心处理器函数
       * 创建一个内部函数，例如 org-supertag-capture--process-template，它接收一个模板 plist 作为输入。
       * 此函数执行后，需要返回一个包含两个元素的列表：
           1. 节点字符串 (Node String)：一个由 :headline 和 :body 内容块拼接而成的、准备插入到 Org 文件中的字符串。
           2. 待办字段列表 (Fields-to-Set List)：一个由所有 :field 内容块处理后生成的、准备写入数据库的数据列表（例如，'(("URL" . "https://...") ("Source" . "Web"))）。

   * 步骤 5：实现内容块解析与分类处理
       * 处理器函数的核心是一个循环，遍历模板中 :content 列表里的每一个“内容块”。
       * 在循环中，使用 pcase 或 cond 根据内容块的 :type 进行分发：
           * 如果 :type 是 :headline 或 :body，则调用相应的内容生成器（见第三阶段）获取文本，并将其追加到“节点字符串”变量中。
           * 如果 :type 是 :field，则处理其 :name 和 :value，并将生成的键值对添加（push）到“待办字段列表”变量中。

  第三阶段：内容与数据生成器开发

  此阶段为引擎提供获取动态数据的具体工具。

   * 步骤 6：开发各类数据来源的生成器
       * 为每一种在 :source 中定义的数据来源，编写一个独立的辅助函数。
       * 必须实现的函数包括：--get-from-prompt (处理用户输入), --get-from-clipboard (处理剪贴板), --get-from-region (处理选中区域), --get-from-function (处理函数调用), 和
         --get-from-static (处理静态值)。

   * 步骤 7：实现文本内容的格式化
       * 确保在生成“节点字符串”时，能根据 :type 对内容进行正确的文本包装（例如，为 :headline 添加 *  前缀和换行符）。

  第四阶段：前端命令与最终集成逻辑

  此阶段将所有后端逻辑连接起来，呈现给用户一个完整的、可交互的命令。

   * 步骤 8：重写主交互命令 `org-supertag-capture`
       * 此 interactive 命令的完整执行流程如下：
           1. 选择模板：通过 completing-read 让用户从 org-supertag-capture-templates 中选择一个模板。
           2. 调用引擎：将选中的模板 plist 传递给 org-supertag-capture--process-template 处理器，获取“节点字符串”和“待办字段列表”。
           3. 插入文本：将“节点字符串”插入到模板指定的目标文件中。
           4. 同步节点并获取ID：在插入位置调用 org-supertag-node-sync-at-point，将文本注册为节点，并获取返回的 node-id。
           5. 处理字段：遍历“待办字段列表”。对于列表中的每一个字段：
               * A. 写入数据库 (始终执行)：调用 org-supertag-field-set-value，将字段名和值与 node-id 关联，存入数据库。
               * B. 检查开关 (条件执行)：检查 org-supertag-capture-sync-fields-to-properties 开关的值。
               * C. 写入属性抽屉 (条件执行)：如果开关为 t，则调用一个新的辅助函数 org-supertag-capture--write-to-properties-drawer，使用 org-entry-put
                 将同一字段名和值写入到 Org 文件中该节点的 :PROPERTIES: 抽屉内。

  第五阶段：文档、示例与用户引导

  此阶段的目标是让用户能够理解和使用我们创造的强大新功能。

   * 步骤 9：编写高质量的文档
       * 为所有新的 defcustom 变量和核心的用户交互命令编写清晰、详尽的文档字符串 (docstrings)。

   * 步骤 10：创建丰富的模板示例
       * 在 org-supertag-capture.el 文件的注释中，或在 defcustom
         的文档里，提供多个精心设计的模板示例，全面展示新系统的各种功能组合（如静态字段、动态字段、函数生成标题等）。

   * 步骤 11：更新项目主文档
       * 在项目的 README.md 或用户手册中，增加一个专门的章节来介绍新的捕获系统。清晰地解释其设计哲学、如何配置模板，以及 sync-fields-to-properties 开关的用法。
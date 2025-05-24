**提案：org-supertag AI 工作流引擎 - 基于 Org Headline 的原生实现**

**1. 目标**

构建一个 AI 工作流引擎，允许用户直接在 Org mode 文件中定义、配置和执行多步骤的 AI 任务。该引擎将 Org Headline 作为核心构建块，利用 Org 的结构和属性系统来驱动工作流。

**2. 核心概念**

*   **Headline 即节点 (Headline as Node):** Org 文件中的每一个（或特定标记的）Headline 都可以代表工作流中的一个处理节点或步骤。
*   **层级结构 (Hierarchy for Structure):** Org 的大纲层级提供了一种自然的视觉组织方式，也可以作为默认的线性执行顺序的基础。
*   **属性驱动配置 (Properties for Configuration):** 每个作为节点的 Headline 通过其 `:PROPERTIES:` drawer 进行详细配置，包括节点类型、行为参数（如 Prompt）、执行逻辑以及节点间的跳转关系。
*   **LLM 后端集成 (LLM Integration):** 利用现有的 `org-supertag-sim-epc.el` 模块作为与后端大型语言模型（如通过 Ollama 运行的模型）通信的桥梁。
*   **上下文传递 (Context Passing):** 节点执行过程中产生的中间结果、思考过程或最终输出，将被记录在该 Headline 内容区域的特定命名块 (Named Blocks) 中。这些块的内容可以被后续节点读取并用作其执行的上下文。

**3. 推荐架构：Org 元素原生引擎 (Org-Element Native Engine)**

*   **描述:** 该引擎直接操作 Org 缓冲区，使用 `org-element.el` 提供的函数（如 `org-element-map`, `org-element-property`, `org-element-contents` 等）来解析节点配置、读取上下文、执行逻辑以及写入结果。引擎在执行过程中动态地与 Org 缓冲区交互，而不是先将其转换为内部数据结构。
*   **理由:**
    *   **紧密集成:** 与 Org mode 的工作方式高度契合，提供最"Org 化"的体验。
    *   **直接操作:** 直接读取和写入 Org 缓冲区，概念清晰，状态反映实时。
    *   **简化:** 避免了 Org 结构到独立 Elisp 数据结构之间的转换层和同步问题。
    *   **利用现有能力:** 充分利用 Emacs Lisp 和 `org-element.el` 的强大功能。

**4. 关键实现组件 (Elisp 函数)**

需要实现一组核心函数来驱动引擎：

*   `org-supertag-ai-workflow-find-node(headline-title)`: 根据标题查找对应的 Headline 元素。
*   `org-supertag-ai-workflow-get-node-properties(node-element)`: 解析并返回指定 Headline 元素的属性列表。
*   `org-supertag-ai-workflow-get-context(node-element, context-sources)`: 从前序节点（根据 `context-sources` 定义）查找并读取指定命名块的内容作为上下文。
*   `org-supertag-ai-workflow-write-output(node-element, output, block-name)`: 在当前节点的内容区域插入或更新指定名称的块（如 `#+BEGIN_AI_OUTPUT`）。
*   `org-supertag-ai-workflow-execute-node(node-element, shared-context)`: 负责单个节点的完整生命周期：读取配置、准备输入（含上下文）、执行（调用 LLM、函数等）、处理输出（写入块）、确定下一个动作/节点。
*   `org-supertag-ai-workflow-run(start-headline-title, initial-context)`: 工作流的主入口和执行循环，负责协调节点的顺序执行。

**5. 节点定义 (Org Properties 示例)**

Headline 通过 `:PROPERTIES:` drawer 定义其行为：

```org
* Node A: Analyze Task :ID: NODEA_ID:
:PROPERTIES:
:NODE_TYPE: llm
:AI_PROMPT: "Analyze the following user task and break it down into steps: {{input_context}}"
:MODEL: "hf.co/unsloth/gemma-3-4b-it-GGUF:latest" ; Optional, defaults to global config
:AI_SUCCESSORS: (("default" . "NODEB_ID")) ; Defines the next node using its Org ID
:AI_OUTPUT_BLOCK: AI_ANALYSIS ; Name of the block to store output
:END:

* Node B: Execute Steps :ID: NODEB_ID:
:PROPERTIES:
:NODE_TYPE: llm
:AI_PROMPT: "Based on the analysis below, execute the task:\n\n{{context_AI_ANALYSIS}}"
:AI_CONTEXT_SOURCES: (("Node A: Analyze Task" . "AI_ANALYSIS")) ; Read AI_ANALYSIS block from Node A (Title lookup still needed for context)
:AI_OUTPUT_BLOCK: AI_EXECUTION_RESULT
:AI_SUCCESSORS: (("success" . "NODEC_ID") ("failure" . "ERROR_HANDLER_ID")) ; Conditional transition using Org IDs
:END:

* Node C: Summarize :ID: NODEC_ID:
:PROPERTIES:
:NODE_TYPE: output
:AI_TEMPLATE: "Task completed. Summary:\n{{context_AI_EXECUTION_RESULT}}"
:END:

* Error Handler Node :ID: ERROR_HANDLER_ID:
:PROPERTIES:
:NODE_TYPE: function
:AI_FUNCTION: "org-supertag-ai--log-error"
:END:
```

*   **核心属性:**
    *   `:NODE_TYPE:` (必需): 节点类型 (如 `llm`, `function`, `behavior`, `input`, `output`).
    *   `:AI_SUCCESSORS:` (可选): 定义后续节点及其触发条件 `((<action-string> . <target-node-id-string>) ...)`。**目标节点必须通过其标准的 Org `:ID:` 属性字符串指定**。若缺失或无匹配 Action，可按大纲顺序执行。
*   **类型特定属性:**
    *   `:AI_PROMPT:` (用于 `llm`): Prompt 模板，可包含占位符如 `{{input_context}}` 或 `{{context_BLOCK_NAME}}`.
    *   `:AI_MODEL:` (用于 `llm`): 指定 LLM 模型。
    *   `:AI_FUNCTION:` (用于 `function`): 要调用的 Elisp 函数名。
    *   `:AI_BEHAVIOR_NAME:` (用于 `behavior`): 要执行的已注册行为名称。
    *   `:AI_BEHAVIOR_PARAMS:` (用于 `behavior`): 传递给行为的参数（字符串或 Plist 字符串）。
    *   `:CONDITIONS:` (用于 `conditional` - 未来扩展): 定义条件逻辑以决定 Action。
*   **上下文与输出:**
    *   `:AI_CONTEXT_SOURCES:` (可选): 指定从哪些前序节点读取哪些块作为输入上下文 `((<Source Node Title String> . <Block Name String>) ...)`。注意：上下文源查找仍基于标题，因为 ID 主要用于流程跳转。
    *   `:AI_OUTPUT_BLOCK:` (可选): 指定存储此节点主要输出的块名称。
    *   `:AI_THOUGHTS_BLOCK:` (可选): 指定存储此节点"思考过程"的块名称。

**6. 上下文传递机制**

*   **写入:** 节点执行后，其结果或思考过程通过 `org-supertag-ai-workflow-write-output` 写入到其 Headline 下具有特定名称的块中（例如 `#+BEGIN_AI_ANALYSIS ... #+END_AI_ANALYSIS`）。
*   **读取:** 后续节点在其 `:CONTEXT_SOURCES:` 属性中声明需要哪些前序节点的哪些块。执行引擎在执行该节点前，使用 `org-supertag-ai-workflow-get-context` 读取这些块的内容，并将其填充到 Prompt 模板或作为输入参数传递给函数。

**7. 执行流程**

1.  用户通过命令启动工作流，指定 Org 文件和起始 Headline 的标题或 ID。
2.  引擎使用 `org-supertag-ai-workflow-find-node` 定位到起始 Headline 元素（优先使用 ID）。
3.  进入循环：
    a.  获取当前节点的属性。
    b.  获取上下文数据。
    c.  执行节点逻辑。
    d.  根据执行结果 (`action`) 和 `:AI_SUCCESSORS:` 属性确定下一个节点的 **ID**。
    e.  使用 `org-supertag-ai-workflow-find-node` 定位到下一个 Headline 元素（**通过 ID 查找**）。如果找不到指定的 ID，则为流程定义错误。
    f.  若无下一个节点或出错，循环结束。
4.  返回最终结果或状态。

**8. 初步验证计划**

1.  创建一个包含两个 LLM 节点的简单 Org 文件，节点 A 的输出作为节点 B 的输入。
2.  配置节点的 `:NODE_TYPE:`, `:PROMPT:`, `:SUCCESSORS:`, `:OUTPUT_BLOCK:`, `:CONTEXT_SOURCES:` 属性。
3.  运行工作流。
4.  验证：
    *   属性被正确解析。
    *   `org-supertag-sim-epc.el` 被正确调用以执行 LLM 推理。
    *   节点 A 的输出被写入指定的 `#+BEGIN_...` 块。
    *   节点 B 被正确触发。
    *   节点 B 的 Prompt 正确地包含了从节点 A 读取的上下文。
    *   整个流程按预期顺序执行。

**9. 后续步骤 (技术实现细化)**

以下是从技术角度出发，对实现原生 Org AI 工作流引擎所需步骤的详细规划：

**9.1. 属性定义与解析 (Property Definition & Parsing) [部分完成]**

*   **命名规范:** 确定属性键名。建议使用简洁、描述性的名称，如 `:NODE_TYPE:`, `:AI_PROMPT:`, `:AI_SUCCESSORS:`。暂不使用统一前缀，除非遇到命名冲突。 [完成]    
*   **核心属性细化:**
    *   `:NODE_TYPE:`: 定义初始支持的类型（`llm`, `function`, `input`, `output`）。考虑未来扩展性（如 `conditional`, `subflow`）。
    *   `:AI_SUCCESSORS:`: 确认格式为 `((<action-string> . <target-node-id-string>) ...)`。**目标节点必须通过其标准的 Org `:ID:` 属性字符串指定。** 定义默认跳转逻辑：
        1.  优先匹配 `action-string` 找到对应的 `target-node-id`。
        2.  若无匹配，查找 `"default"` 或 `"*"` action 对应的 `target-node-id`。
        3.  若无 `:AI_SUCCESSORS:` 或无任何匹配，默认按 Org 大纲顺序查找下一个 sibling headline 作为节点（此方式较脆弱，建议明确定义跳转）。
*   **类型特定属性:**
    *   `:AI_PROMPT:`: 确定模板语法，使用 `{{variable_name}}`。变量名需规范（如 `input_context`, `shared_KEY`, `context_BLOCK_NAME`）。需要实现一个简单的模板渲染函数（如 `org-supertag-ai--render-template`）。
    *   `:AI_MODEL:`: 字符串，对应 `org-supertag-sim-epc.el` 接受的模型标识。需支持全局默认配置。
    *   `:AI_FUNCTION:`: 字符串，包含要调用的 Elisp 函数名。函数签名约定为 `(node-element context-hash)`，函数需返回 `(cons action-string result)`。
    *   `:AI_BEHAVIOR_NAME:` (用于 `behavior`): 要执行的已注册行为名称。
    *   `:AI_BEHAVIOR_PARAMS:` (用于 `behavior`): 传递给行为的参数（字符串或 Plist 字符串）。
    *   `:CONDITIONS:` (用于 `conditional` - 未来扩展): 定义条件逻辑以决定 Action。
*   **上下文与输出属性:**
    *   `:AI_CONTEXT_SOURCES:`: 确认格式为 `((<Source Node Title String> . <Block Name String>) ...)`。`get-context` 函数应将这些源的内容收集到一个 hash-table 中，键为 `Block Name String`，值为块内容字符串。模板变量形如 `{{context_BLOCK_NAME}}`。
    *   `:AI_OUTPUT_BLOCK:` / `:AI_THOUGHTS_BLOCK:`: 字符串，定义输出块的名称。块类型默认为名称本身。
*   **解析实现:**
    *   实现 `org-supertag-ai-workflow-get-node-properties`，使用 `org-element-property` 读取属性。
    *   对 `:AI_SUCCESSORS:`, `:AI_CONTEXT_SOURCES:` 等结构化属性，使用 `read-from-string` 将其从字符串解析为 Elisp 列表/结构。

**9.2. 核心函数签名与实现 (Core Function Signatures & Implementation)**

*   **`org-supertag-ai-workflow-find-node(node-identifier &optional buffer)`:**
    *   输入: `node-identifier` (String, **优先视为 Org ID，其次可作为 Headline Title 回退**), `buffer` (Optional).
    *   输出: `org-element` (Headline element) or `nil`.
    *   实现: **优先使用 `org-id-find` 通过 ID 查找节点。如果 `node-identifier` 看起来不像 ID 或 `org-id-find` 失败，可以尝试按 Headline Title 查找（作为用户启动时的便利，或旧格式兼容），但这应被视为次要方式。工作流内部跳转应强制使用 ID。**
*   **`org-supertag-ai-workflow-get-node-properties(node-element)`:**
    *   输入: `node-element` (Headline element).
    *   输出: `hash-table` (Properties, keywords as keys, parsed values).
*   **`org-supertag-ai-workflow-get-context(current-node-element context-sources-prop-value shared-context-hash)`:**
    *   输入: 当前 `node-element`, `:AI_CONTEXT_SOURCES:` 的原始字符串值, 共享上下文 `hash-table`.
    *   输出: `hash-table` (包含所有可用上下文变量: 从源块读取的 + 共享的).
    *   实现:
        1.  解析 `context-sources-prop-value` 字符串为 `((SourceTitle . BlockName) ...)` 列表。
        2.  遍历列表，对每个条目：**仍需按标题查找源节点** (`find-node` 按标题模式) -> 在源节点内查找指定 `BlockName` 的 `special-block` -> 提取其 `:value`。
        3.  将提取的上下文存入新的 hash-table (Key: `BlockName`, Value: Content)。
        4.  合并 `shared-context-hash` 到新 hash-table 中（注意优先级或合并策略）。
*   **`org-supertag-ai-workflow-write-output(node-element output-string block-name-string &optional block-type-string)`:**
    *   输入: `node-element`, `output-string`, `block-name-string`, `block-type` (Optional, defaults to `block-name`).
    *   输出: `t` or `nil`.
    *   实现: 在 `node-element` 的内容区查找或创建 `#+BEGIN_...` / `#+END_...` 块。需小心处理 buffer 修改，确保幂等性（更新现有块），可考虑使用 `org-sbe` 或 `org-element` API 进行插入/更新。
*   **`org-supertag-ai--render-template(template-string context-hash)`:**
    *   输入: 模板字符串, 上下文 `hash-table`.
    *   输出: 渲染后的字符串.
    *   实现: 查找 `{{...}}` 占位符并替换为 `context-hash` 中的对应值。
*   **`org-supertag-ai-workflow-execute-node(node-element shared-context-hash)`:**
    *   输入: `node-element`, 共享上下文 `hash-table`.
    *   输出: `(cons action-string updated-shared-context-hash)`.
    *   实现:
        1.  获取节点属性。
        2.  获取上下文 (`get-context`)。
        3.  根据 `:NODE_TYPE:` 分派到具体的执行函数 (e.g., `--execute-llm-node`, `--execute-function-node`)。
        4.  调用执行函数，获取 `(action . result)`。
        5.  如果指定了 `:AI_OUTPUT_BLOCK:` 或 `:AI_THOUGHTS_BLOCK:`，调用 `write-output` 写入结果。
        6.  更新 `shared-context-hash`（例如，将 `result` 存入特定键 `:last_result`）。
        7.  返回 `(cons action updated-shared-context)`。
*   **`org-supertag-ai--execute-llm-node(node-element context-hash properties)`:**
    *   输入: `node-element`, 完整上下文 `hash-table`, 节点属性 `hash-table`.
    *   输出: `(cons action-string result-string)`.
    *   实现:
        1.  渲染 Prompt (`render-template`)。
        2.  调用下面的 `org-supertag-ai--invoke-llm-sync` 与 EPC 通信。
        3.  处理返回结果，确定 `action` (默认为 "default") 和 `result`。
*   **`org-supertag-ai--execute-function-node(node-element context-hash properties)`:**
    *   输入: `node-element`, 完整上下文 `hash-table`, 节点属性 `hash-table`.
    *   输出: `(cons action-string result)`.
    *   实现:
        1.  获取 `:AI_FUNCTION:` 属性值（函数名）。
        2.  使用 `funcall` 或 `apply` 调用该函数，传递 `node-element` 和 `context-hash`。
        3.  函数本身负责返回 `(cons action result)`。
*   **(新增) `org-supertag-ai--execute-behavior-node(node-element context-hash properties)`:**
    *   输入: `node-element`, 上下文 `hash-table`, 节点属性 `hash-table`.
    *   输出: `(cons action-string result)`. (行为通常不直接返回结果用于上下文，action 可能总是 "default")
    *   实现:
        1.  获取 `:AI_BEHAVIOR_NAME:` 和 `:AI_BEHAVIOR_PARAMS:`。
        2.  解析参数字符串为 Plist (如果需要)。
        3.  调用 `org-supertag-behavior-execute`。
        4.  返回 `(cons "default" nil)` (或根据行为约定)。
*   **`org-supertag-ai-workflow-run(start-node-identifier &optional initial-context buffer)`:**
    *   输入: 起始节点标识符 (String, **ID 或 Title**), 初始上下文 (Optional, hash-table), `buffer` (Optional).
    *   输出: 最终的共享上下文 `hash-table`.
    *   实现: 核心循环，管理 `current-node-element` 和 `shared-context`，调用 `execute-node`，根据返回的 `action` 和节点的 `:AI_SUCCESSORS:` **查找下一个节点的 ID**，调用 `find-node` **通过 ID 定位**下一个节点，直到无后续节点或出错。

**9.3. 与 `org-supertag-sim-epc.el` 对接 (EPC Integration)**

*   **确定接口:** 需要 `org-supertag-sim-epc.el` 提供一个稳定的、可编程调用的异步函数来执行 LLM 推理，例如 `org-supertag-sim-epc-run-ollama-async(prompt system-prompt model callback)`。如果不存在，需要添加。
*   **同步封装:** 在 `org-supertag-ai.el` 中实现一个内部的、看起来同步的封装函数 `org-supertag-ai--invoke-llm-sync(prompt system-prompt model)`。
    *   该函数内部调用 `org-supertag-sim-epc-run-ollama-async`。
    *   使用 `deferred` 或简单的循环+`accept-process-output` (带超时) 来等待异步回调完成。
    *   返回从 EPC 获取的结果 (成功则为结果字符串，失败则为错误信息或 `nil`)。这可以简化主工作流循环的逻辑，避免显式处理 `deferred` 对象。
*   **参数传递:** 确保 `org-supertag-ai--execute-llm-node` 正确地从属性和上下文中提取 `prompt`, `system-prompt`, `model` 并传递给 `invoke-llm-sync`。
*   **EPC 服务初始化:** 在 `org-supertag-ai-workflow-run` 开始时，或提供一个独立的设置命令，确保调用 `org-supertag-sim-epc-ensure-server`, `org-supertag-sim-epc-ensure-ollama-running`, `org-supertag-sim-epc-ensure-ollama-model` 来准备好 EPC 和 Ollama 环境。

**9.4. 关键技术考量 (Technical Considerations)**

*   **错误处理:** 设计健壮的错误处理机制。在 `execute-node` 和 `run` 中使用 `condition-case` 捕获解析、**节点 ID 查找失败**、函数执行、LLM 调用等环节的错误。根据错误类型和 `:ON_ERROR:` 属性（如果实现）决定是停止、记录日志、还是跳转到特定错误处理节点。
*   **状态管理:** `shared-context` 使用 `hash-table` 传递。明确其结构和标准键（如 `:input`, `:last_result`, `:current_node_title`）。
*   **异步性:** 虽然初始采用同步封装简化开发，但需注意这可能阻塞 Emacs。未来可考虑将整个引擎改造为完全异步，返回 `deferred` 对象。
*   **测试:**
    *   单元测试 (`ert`): 针对属性解析、模板渲染、上下文获取、节点查找等纯函数。
    *   集成测试: 创建测试 Org 文件，模拟或使用真实的 `org-supertag-sim-epc.el` 服务，验证端到端的工作流执行。
*   **用户交互:** 设计 `org-supertag-ai-workflow-run` 的交互方式。使用 `completing-read` 让用户选择当前 buffer 中的 Headline 作为起点？或允许从 point 处启动？
*   **缓冲区修改:** `write-output` 函数必须极其小心，避免破坏 Org 文件结构。优先使用 `org-element` API 操作，并在修改前后进行验证。考虑执行期间将 buffer 设为只读，或在副本上操作的选项。

**10. 验证计划 (Refined)**

维持第 8 节的初步验证计划，并增加以下验证点：

*   验证默认的、基于大纲顺序的节点跳转（当 `:AI_SUCCESSORS:` 缺失或不匹配时）。
*   验证 `:AI_FUNCTION:` 节点类型，创建一个简单的 Elisp 函数并由工作流调用。
*   验证错误处理：故意制造错误（如引用不存在的上下文块、调用不存在的函数），检查引擎是否按预期停止或报告错误。
*   验证 `write-output` 是否能在不同情况下（首次写入、更新现有块）正确修改 Org 文件。
*   **增加:** 验证使用 `:ID:` 的 `:AI_SUCCESSORS:` 属性进行节点跳转的正确性。
*   **增加:** 验证当 `:AI_SUCCESSORS:` 中指定的 ID 无效或找不到时，引擎能正确报错。
*   **增加:** 验证 `:NODE_TYPE: behavior` 节点类型，调用一个已注册的行为。

**11. 设计演进与借鉴 (Design Evolution & Lessons Learned)**

在最终确定当前基于 Org Headline 属性的 AI 工作流引擎设计（以下简称"节点属性模型"）之前，我们曾构思过一个基于模拟组织层级（如总经理、组长、执行者）的方案（以下简称"层级 Agent 模型"）。通过比较，我们得出以下结论和可借鉴的经验：

*   **核心差异:**
    *   **隐喻:** 层级 Agent 模型模拟"团队"，节点属性模型构建"流程图"。
    *   **流程控制:** 层级 Agent 模型依赖 Org 层级和层级消息；节点属性模型依赖 Headline 属性 (`:AI_SUCCESSORS:`, `:ID:`) 进行显式、灵活的跳转。
    *   **配置:** 层级 Agent 模型配置存储在 Elisp 对象中；节点属性模型配置完全存储在 Org Headline 的 `:PROPERTIES:` 中。
    *   **上下文:** 层级 Agent 模型使用固定 Special Blocks 和消息传递；节点属性模型通过 `:AI_CONTEXT_SOURCES:` 显式声明来源。

*   **节点属性模型的优势:**
    *   **灵活性:** 支持任意流程结构（分支、循环等），不受层级限制。
    *   **Org 原生性:** 与 Org mode 结合紧密，配置直观且自包含。
    *   **清晰可控:** 节点行为和数据流向通过属性明确定义。
    *   **易扩展:** 添加新节点类型更方便。

*   **层级 Agent 模型的借鉴价值:**
    *   **标准化的块名称:** 早期定义的 `task`, `log`, `result` 等块名可作为推荐或默认的块名 (`:AI_OUTPUT_BLOCK:`, `:AI_THOUGHTS_BLOCK:` 的值)，提升工作流的一致性和可读性。
    *   **Prompt 模板化管理:** 为不同角色/任务定义可复用的 Prompt 模板的想法很有价值。当前模型应考虑支持在 `:AI_PROMPT:` 属性中引用预定义的模板，而非总是内联完整的 Prompt 文本。这可以通过单独的 Org 文件或特定 Headline 来管理模板库。

*   **结论:**
    当前的**节点属性模型**是构建通用、灵活、与 Org 高度集成的 AI 工作流引擎的更优选择。我们将继续沿此方向开发，并积极吸纳层级 Agent 模型中关于**标准化块名**和**Prompt 模板化管理**的有益思想，将其作为最佳实践或未来功能融入设计中。

这个提案为我们接下来的开发工作提供了一个清晰的蓝图。你认为这个提案是否足够清晰和完整？是否有需要调整或补充的地方？


好的，基于我们之前的讨论，这里为您整理一份关于将 Simon Willison 的 `llm` CLI 工具集成到 `org-supertag` 中以实现 Agentic AI 工作流的开发文档草案。

---

## Org-Supertag Agentic AI 工作流开发文档 (基于 `llm` CLI)

**版本:** 0.1 (草案)

**日期:** 2025-04-05

### 1. 目标

本文档旨在规划和指导如何将 Simon Willison 开发的 `llm` 命令行工具 ([https://github.com/simonw/llm](https://github.com/simonw/llm)) 集成到 `org-supertag` 项目中，以替代现有的基于 EPC 的方案 (`simtag_epc.py`, `org-supertag-sim-epc.el`) 来与大型语言模型 (LLM) 进行交互，并构建一个具备初步 Agentic 能力的 AI 工作流。

目标是利用 `llm` CLI 的统一模型访问、插件系统和日志记录等功能，结合 `org-supertag` 现有的行为系统 (`org-supertag-behavior.el`) 和初步的 AI 工作流框架 (`org-supertag-ai.el`)，实现以下能力：

*   通过 `org-supertag` 行为触发 AI 工作流。
*   AI 工作流能够调用 `llm` CLI 与用户配置的各种 LLM (本地或远程) 进行交互。
*   LLM 能够理解并决定调用 `org-supertag` 的行为库 (`org-supertag-behavior-library.el`) 中的函数作为其可用的 "工具" (Tools)。
*   LLM 的输出 (工具调用请求) 能被安全地解析和执行，从而影响 Org-mode 节点的状态或内容。

### 2. 背景

*   `org-supertag` 已具备一个行为系统 (`org-supertag-behavior.el`)，允许标签触发预定义的 Emacs Lisp 函数。
*   存在一个初步的 AI 工作流框架草案 (`org-supertag-ai.el`)，定义了基于节点 (Node)、生命周期 (prep/exec/post) 和状态转移的工作流引擎。
*   之前探索了使用 Python EPC (`simtag_epc.py`, `org-supertag-sim-epc.el`) 与 LLM (通过 Ollama) 交互的方案。
*   `llm` CLI 是一个成熟的、功能强大的工具，可以统一访问多种 LLM，并由社区积极维护。

**采用 `llm` CLI 的优势:**

*   **统一接口:** 无需为不同模型编写不同的集成代码。
*   **简化配置:** 用户只需安装 `llm` 及相关插件，并使用 `llm keys set` 配置 API 密钥。
*   **插件生态:** 可利用 `llm` 插件支持本地模型 (如 Llama, Mistral) 和新的云服务。
*   **功能复用:** 利用 `llm` 的日志记录 (SQLite)、模板等功能。
*   **降低维护成本:** 无需维护自定义的 Python EPC 服务器和相关 Elisp 代码。

### 3. 核心设计

整合的核心思想是 **"将 `org-supertag` 行为作为 AI Agent 的工具"**，并使用 `llm` CLI 作为 Agent 与 LLM "大脑" 沟通的桥梁。

1.  **`llm` CLI 调用:**
    *   在 `org-supertag-ai.el` 的 AI 节点 (特别是 `org-supertag-ai-llm-node`) 的 `exec` 阶段，通过 Emacs Lisp 的 `call-process` 或 `start-process` 函数调用 `llm` CLI。
    *   传递必要的参数，包括模型名称 (`-m`)、系统提示 (`-s`) 和用户提示。
    *   捕获 `llm` CLI 的标准输出 (stdout) 作为 LLM 的响应。

2.  **行为即工具 (Behaviors as Tools):**
    *   `org-supertag-behavior-library.el` 中定义的 `org-supertag-behavior--*` 函数被视为 Agent 可以使用的工具。
    *   系统提示 (`-s`) 需要向 LLM 清晰地描述这些可用的工具、它们的功能以及如何请求调用它们。

3.  **Agentic 工作流:**
    *   LLM 根据当前任务和上下文 (来自 `prep` 阶段准备的数据) 进行思考。
    *   LLM 的输出要么是最终答案/文本，要么是一个请求调用特定 "工具" (行为) 的指令。
    *   `post` 阶段负责解析 LLM 的输出。如果是工具调用请求，则安全地执行对应的 `org-supertag` 行为。
    *   工具执行的结果可以反馈给 LLM (进入下一轮循环)，或者工作流根据结果决定下一步走向。

4.  **LLM 输出格式:**
    *   为了让 Emacs Lisp 能够解析并执行工具调用，LLM 需要被指示输出特定格式的指令。推荐使用 **Emacs Lisp S-expression** 格式，例如：
        ```emacs-lisp
        ;; LLM 输出示例: 请求设置属性
        (org-supertag-behavior--set-property :node-id "CURRENT_NODE_ID" :params '(:name "Summary" :value "这是节点内容的总结..."))
        ```

### 4. 实现步骤

1.  **依赖管理:**
    *   在文档和安装说明中，要求用户安装 `llm` CLI 及其可能需要的插件 (例如 `llm-gpt4all`, `llm-llama-cpp` 等)。
    *   指导用户使用 `llm keys set <service>` 配置 API 密钥。
    *   在 Elisp 代码中，检查 `llm` 命令是否可用 (`executable-find "llm"`).

2.  **修改 AI 工作流 (`org-supertag-ai.el`):**
    *   **`org-supertag-ai-llm-node` 结构:** 可能需要添加 `:model-alias` 字段来指定要使用的 `llm` 模型。
    *   **`org-supertag-ai-node-exec` (for LLM Node):**
        *   从 `prep-result` 和节点参数中获取 `prompt`, `system-prompt`, `model-alias`。
        *   构建 `llm` 命令行参数列表。
        *   使用 `call-process` 执行 `llm` 命令。
        *   返回捕获的 `stdout`。
    *   **`org-supertag-ai-node-post` (for LLM Node):**
        *   接收 `exec-result` (即 `llm` 的输出)。
        *   **解析:** 判断输出是普通文本还是工具调用 S-expression。可以使用正则表达式或更健壮的 Lisp 读取器 (`read-from-string`) 来尝试解析。
        *   **验证:** 如果解析为 S-expression，**必须验证**：
            *   函数名是否在允许的 `org-supertag-behavior--*` 函数白名单内。
            *   参数格式是否符合预期。
        *   **执行:** 如果验证通过，使用 `apply` 或 `funcall` 安全地执行该行为函数。**绝对禁止直接 `eval` 未经验证的 LLM 输出。**
        *   **决策:** 根据解析/执行结果，决定下一个 `action` (例如，`"default"`, `"tool_executed"`, `"error"`) 并更新 `shared` 状态。

3.  **定义 Tool (行为) 描述:**
    *   创建一个函数或机制，用于生成传递给 `llm` 的系统提示 (`-s`)。
    *   此系统提示应包含：
        *   Agent 的总体目标或角色。
        *   可用的 `org-supertag-behavior--*` 函数列表 (作为 Tools)。
        *   每个 Tool 的功能描述。
        *   每个 Tool 的参数说明。
        *   要求 LLM 在需要调用 Tool 时，输出指定格式的 S-expression。

4.  **安全执行:**
    *   实现一个安全的包装函数，例如 `org-supertag-ai--safe-execute-behavior`，它接收解析出的函数名和参数，进行白名单检查和必要的参数清理，然后才调用 `apply`/`funcall`。

5.  **触发机制:**
    *   创建一个新的行为，例如 `org-supertag-behavior--start-ai-workflow`。
    *   此行为可以被附加到特定标签 (如 `@ai_task`) 上。
    *   当标签被添加时，该行为触发，收集当前节点信息作为初始 `shared` 状态，并调用 `org-supertag-ai-flow-run` 启动 AI 工作流。

6.  **逐步淘汰 EPC:**
    *   一旦基于 `llm` CLI 的交互稳定，可以移除 `simtag_epc.py` 文件和 `org-supertag-sim-epc.el` 中的大部分功能（特别是与 LLM 调用相关的部分）。
    *   如果 EPC 仍用于非 LLM 的特定计算（如本地句向量相似度），则保留该部分。

### 5. 关键函数/修改点

*   `org-supertag-ai.el`:
    *   `org-supertag-ai-llm-node` (struct): 添加 `:model-alias`。
    *   `org-supertag-ai-node-exec` (method for `org-supertag-ai-llm-node`): 实现 `call-process` 调用 `llm`。
    *   `org-supertag-ai-node-post` (method for `org-supertag-ai-llm-node`): 实现响应解析、验证和安全执行工具调用。
*   `org-supertag-behavior.el` (或新文件):
    *   `org-supertag-ai--generate-system-prompt`: 生成包含工具描述的系统提示。
    *   `org-supertag-ai--safe-execute-behavior`: 安全执行行为的包装函数。
    *   `org-supertag-behavior--start-ai-workflow`: 触发 AI 工作流的新行为。

### 6. 安全注意事项

*   **严禁直接 `eval` LLM 输出:** 这是最大的安全风险。必须对 LLM 请求执行的函数和参数进行严格的白名单验证和清理。
*   **限制 Tool 权限:** 确保暴露给 LLM 的 "Tool" (行为函数) 不会执行破坏性操作（如删除文件、执行任意 shell 命令），除非有明确的用户许可和风险认知。
*   **错误处理:** `llm` 命令执行可能失败，API 可能不可用，LLM 输出可能格式错误或包含有害指令。需要健壮的错误处理。
*   **资源消耗:** LLM 调用可能消耗 API 配额或本地计算资源。需要考虑成本和性能。

### 7. 示例场景 (总结节点内容)

1.  用户在 Org 节点上添加 `@ai_summarize` 标签。
2.  触发 `org-supertag-behavior--start-ai-workflow` (假设已绑定到该标签)。
3.  该行为收集节点内容，创建初始 `shared` 状态 `{ "content": "节点原始内容..." }`。
4.  启动 AI 工作流，第一个节点是 `summarizer` (一个 `org-supertag-ai-llm-node`)。
5.  `summarizer` 节点的 `prep` 阶段准备 `prompt="请总结以下内容：${content}"` 和 `system-prompt="(包含工具列表，但此任务不需要)"`, `model-alias="gpt-4o-mini"`。
6.  `exec` 阶段调用 `llm -m gpt-4o-mini -s "..." "请总结以下内容：节点原始内容..."`。
7.  `post` 阶段接收到总结文本 `summary_text`。判断不是工具调用。决定 `action="default"`。更新 `shared` 状态 `{ "content": "...", "summary": summary_text }`。
8.  工作流根据 `action="default"` 进入下一个节点 `update_property`。
9.  `update_property` 节点 (可以是 Lisp 节点，非 LLM 节点) 的 `exec` 阶段直接调用 `org-supertag-behavior--set-property`，使用 `shared` 状态中的 `summary` 更新节点的 `SUMMARY` 属性。
10. 工作流结束。

### 8. 未来展望

*   **异步处理:** 对于耗时操作 (LLM 调用)，使用 `start-process` 和回调实现异步执行，避免阻塞 Emacs。
*   **Memory:** 为 Agent 实现更复杂的 Memory 机制，支持多轮对话和上下文保持。
*   **更精细的错误处理:** 提供重试、用户反馈等机制。
*   **流式响应:** 如果 `llm` 支持，处理流式响应以更快显示结果。

---

这份文档提供了一个初步的框架和方向。在实际开发中，需要根据具体情况进行调整和细化。

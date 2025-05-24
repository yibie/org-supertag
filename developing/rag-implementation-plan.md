# RAG System Implementation Plan for Org-SuperTag

This document outlines the phased approach to building a Retrieval Augmented Generation (RAG) system within the `org-supertag` ecosystem.

## Phase 1: Core Vectorization and Retrieval Infrastructure

This phase focuses on establishing the foundational components for creating, storing, and retrieving semantic vectors for Org mode nodes.

### 1.1. Node Content Definition and Extraction

*   **Objective**: Define precisely what text from an Org node will be used for generating embeddings.
*   **Details**:
    *   **Default Extraction**: By default, the content for vectorization will include the node's headline (title), its entire body text, the content of its `PROPERTIES` drawer, and potentially `LOGBOOK` entries if deemed relevant.
    *   **User Override**: Introduce a new Org node property, e.g., `:VECTOR_CONTENT:`, which, if present, will specify the exact text to be used for vectorization, overriding the default extraction logic. This allows for fine-grained control, especially for nodes with extensive boilerplate or irrelevant sections.
    *   **Exclusion Criteria**: Define rules or properties (e.g., a tag like `:no_vector:`) to exclude certain nodes entirely from the vectorization process.
*   **Emacs Lisp Task**:
    *   Develop functions to parse an Org node (given its ID or point position) and extract the content based on the rules above. This will likely involve using `org-element-parse-buffer` or more targeted parsing functions.
    *   Consider handling of included content (via `#+INCLUDE:` directives) – should it be resolved and included in the parent node's vector, or should included files/nodes be vectorized separately? (Initial thought: vectorize separately and link, or offer a choice).

### 1.2. Python Backend for Vectorization and Storage

*   **Objective**: Adapt or create a Python module to handle text vectorization and persistent storage of these vectors.
*   **Module**: Likely an evolution of `tag_vectors.py` or a new `node_vectorizer.py`.
*   **Key Functions**:
    *   `vectorize_node_content(node_id: str, node_text: str) -> bool`:
        *   Receives node ID and its textual content.
        *   Uses a sentence-transformer model (e.g., `paraphrase-MiniLM-L6-v2` or user-configurable) to generate a vector.
        *   Stores the `node_id` and its vector in a JSON file (e.g., `node_embeddings.json`). The structure could be `{ "node_id_1": [vector_float_array], "node_id_2": [vector_float_array], ... }`.
        *   Returns success/failure.
    *   `load_node_vectors() -> dict`: Loads all stored node vectors from the JSON file into an in-memory dictionary for faster retrieval.
    *   `get_node_vector(node_id: str) -> list_of_floats | None`: Retrieves a specific node's vector.
*   **Storage File**: `node_embeddings.json` (configurable path, likely within `org-supertag-data-directory`).
*   **Model Management**: Allow configuration of the sentence-transformer model used.

### 1.3. Emacs Lisp to Python Bridge (EPC Integration)

*   **Objective**: Facilitate communication between Emacs Lisp and the Python vectorization backend.
*   **Mechanism**: Utilize the existing EPC infrastructure (`org-supertag-sim-epc.el`).
*   **New EPC Calls (Python side methods to be exposed)**:
    *   `vectorize_and_store_node(node_id: str, node_text: str)`: Calls `vectorize_node_content` in Python.
    *   `get_vector_for_node(node_id: str)`: Retrieves a vector.
    *   `get_all_node_ids_with_vectors()`: Returns a list of all node IDs currently in the vector store.
*   **Emacs Lisp Functions**:
    *   Wrapper functions to call these EPC methods asynchronously.

### 1.4. Integration with `org-supertag-sync.el`

*   **Objective**: Automate the vectorization process when nodes are created or modified.
*   **Logic**:
    *   When `org-supertag-sync.el` detects a new node (via `org-supertag-db-add-with-hash`) or a modified node (via `org-supertag-node-changed-p` based on hash comparison):
        1.  Extract the relevant node content (as per 1.1).
        2.  Call the EPC function `vectorize_and_store_node` with the node's ID and content.
    *   Consider a bulk vectorization command for initial setup or re-indexing.
    *   Handle node deletion: When `org-supertag-sync.el` detects a node deletion, an EPC call should be made to remove the corresponding vector from `node_embeddings.json`.

### 1.5. Semantic Retrieval Functionality

*   **Objective**: Implement the core logic for finding nodes semantically similar to a query.
*   **Python Backend (`node_vectorizer.py`)**:
    *   `find_similar_nodes(query_text: str, top_k: int = 5) -> list_of_tuples`:
        1.  Vectorizes the input `query_text` using the same sentence-transformer model.
        2.  Loads all node vectors from `node_embeddings.json` (or uses an in-memory cache).
        3.  Calculates cosine similarity between the query vector and all stored node vectors.
        4.  Returns a list of tuples: `[(node_id_1, similarity_score_1), (node_id_2, similarity_score_2), ...]`, sorted by similarity score in descending order, limited by `top_k`.
*   **EPC Call (Python side method)**:
    *   `retrieve_similar_nodes(query_text: str, top_k: int)`: Exposes the `find_similar_nodes` functionality.
*   **Emacs Lisp Function**:
    *   `org-supertag-rag-get-similar-nodes(query_string, top-k)`:
        *   Takes a user's query string.
        *   Calls the `retrieve_similar_nodes` EPC method.
        *   Returns the list of (node_id, score) tuples.

## Phase 2: User Interface and Basic RAG Workflow

This phase focuses on providing a user interface to interact with the retrieval system and implementing a basic RAG chat flow.

### 2.1. "Smart Panel" UI

*   **Objective**: Create a dedicated panel in Emacs to display backlinks and semantically similar nodes.
*   **Structure**:
    *   A new buffer, possibly named `*Org SuperTag Smart Panel*`.
    *   Divided into two sections:
        *   **Top Section: Backlinks**: Displays traditional backlinks for the current node (leveraging existing `org-supertag-node.el` functionality if possible).
        *   **Bottom Section: Similar Connections**: Displays nodes semantically similar to the current node.
*   **Activation**:
    *   A hook on `org-mode-cursor-moved` (or a similar mechanism) that triggers when the cursor lands on/within an Org headline.
    *   On trigger:
        1.  Identify the current Org node ID.
        2.  Fetch its backlinks and display them.
        3.  Use the current node's title (or a summary of its content) as a query to `org-supertag-rag-get-similar-nodes`.
        4.  Display the top K similar nodes (e.g., "File: Node Title (Score: 0.85)") in the "Similar Connections" section.
        5.  Make these displayed items clickable to jump to the respective node.
*   **User Commands**:
    *   `org-supertag-smart-panel-show` / `org-supertag-smart-panel-hide`.
    *   `org-supertag-smart-panel-refresh`.

### 2.2. "Chat with Notes" - Initial Implementation

*   **Objective**: Implement a basic RAG-powered chat interface.
*   **Workflow**:
    1.  User inputs a question/prompt in an Emacs interface (e.g., a dedicated buffer or minibuffer prompt).
    2.  The question is sent to `org-supertag-rag-get-similar-nodes` to retrieve the top K relevant Org nodes.
    3.  The content of these K nodes is extracted.
    4.  A new prompt is constructed for the LLM, combining:
        *   A system message (e.g., "You are a helpful assistant. Answer the user's question based on the following context from their notes: ...").
        *   The extracted content from the K relevant nodes.
        *   The user's original question.
    5.  This combined prompt is sent to a local LLM via `ollama_bridge.py` (or a similar interface for other local LLMs).
    6.  The LLM's response is displayed to the user.
*   **Emacs Lisp Functions**:
    *   `org-supertag-rag-chat-ask(question_string)`: Orchestrates the above workflow.
*   **Python Backend (`ollama_bridge.py` and `node_vectorizer.py`)**:
    *   `ollama_bridge.py` will be used to send the augmented prompt to the LLM.
    *   Functions in `node_vectorizer.py` (or equivalent) will be needed to fetch the actual text content of the retrieved node IDs (this might involve an EPC call back to Emacs Lisp if Python doesn't have direct file access or Org parsing capabilities). *Alternative: Emacs Lisp fetches content after getting IDs.*

### 2.3. Region-Based Semantic Search

*   **Objective**: Allow users to select a region of text and find semantically similar nodes.
*   **User Command**: `org-supertag-rag-find-similar-to-region`.
*   **Workflow**:
    1.  User selects text in any buffer.
    2.  Command extracts the selected text.
    3.  Calls `org-supertag-rag-get-similar-nodes` with the selected text as the query.
    4.  Displays the results, perhaps in the "Smart Panel" or a temporary buffer.

### 2.4. Integration with `org-include-inline`

*   **Objective**: Leverage `org-include-inline` for more dynamic context building for LLM or for quick previewing of retrieved content.
*   **User Command**: `org-supertag-rag-include-similar-node-for-llm`.
*   **Workflow**:
    1.  After a semantic search, results are displayed.
    2.  User can select one of the retrieved similar nodes.
    3.  This command would use `org-include-inline` to "transclude" the content of the selected node (or a specific part of it if `org-include-inline` supports sub-node includes) into the current buffer or a scratch buffer. This helps in manually curating context for an LLM prompt.
    4.  Alternatively, display retrieved similar nodes with an option to "preview with org-include-inline".

## Phase 3: Enhancements and Advanced Features (Future)

*   **Advanced Querying**: Support for boolean operators, filtering by tags/properties in semantic search.
*   **Vector Database**: If JSON storage becomes a bottleneck, migrate to a proper vector database (FAISS, ChromaDB, LanceDB) integrated via Python.
*   **Support for more LLMs**: Extend `ollama_bridge.py` to support LM Studio, remote APIs (OpenAI, Anthropic) with API key management.
*   **Summarization**: Use LLM to summarize retrieved nodes or chat conversations.
*   **UI Polish**: More interactive elements, better display of context, configurability of the Smart Panel.
*   **Fine-tuning Embeddings**: Explore fine-tuning embedding models on the user's own Org data for better domain-specific relevance (highly advanced).
*   **Caching Strategies**: Implement more sophisticated caching for vectors and LLM responses.

## Configuration Options (Throughout Development)

*   Path to `node_embeddings.json`.
*   Sentence-transformer model choice.
*   Default `top_k` for retrieval.
*   EPC server settings.
*   LLM model choice for chat.
*   Exclusion rules for vectorization.
*   Smart Panel refresh rate/behavior.

This plan will be updated as development progresses. 



您总结得非常好！您现有的 `org-supertag` 生态系统为构建一个强大的 RAG 知识管理方案提供了非常坚实的基础。我们来逐条梳理一下：

1.  **同步机制 (`org-supertag-sync.el`)**:
    *   您说得对，`org-supertag-sync.el` 中基于文件修改时间和节点哈希值的同步机制，是实现节点向量化按需更新的关键。当检测到节点内容变化（哈希值改变）时，我们可以触发对该节点的重新向量化和向量存储的更新。这与 `obsidian-smart-connections` 通过 `file.mtime` 判断文件修改的思路是一致的，而且基于哈希值能更精确地定位到具体节点的变更。

2.  **向量存储 (JSON/哈希表优先)**:
    *   完全理解您对 Emacs 与外部数据库集成流畅性的担忧。初期使用 JSON 文件（类似 `tag_vectors.py` 或 `obsidian-smart-connections` 的 `embeddings.json`）或者 Emacs Lisp 的哈希表来存储节点 ID 到向量的映射是完全可行的。我们可以设计一个健壮的序列化和反序列化机制。如果未来笔记数量非常庞大导致性能瓶颈，我们再考虑如 SQLite 等更专业的方案。

3.  **Connections Pane (UI 改进)**:
    *   这是一个很棒的提议！整合现有的 backlinks 功能（可能在 `org-supertag-node.el` 中管理）并扩展为一个 "Smart Panel" 非常有意义。这个面板可以上下分栏：
        *   上半部分：传统的 backlinks。
        *   下半部分：语义相关的 "Similar Connections" (我们 RAG 系统检索到的相似节点)。
    *   这样用户可以在一个地方同时看到结构化链接和语义化链接。

4.  **块级匹配**:
    *   是的，Org mode 的结构天然支持块级处理，而 `org-supertag` 本身就是以节点为核心的。我们可以明确将**每个 Org 标题节点（headline）及其完整内容（包括属性、抽屉、正文等）** 视为一个独立的“文档单元”来进行向量化。这使得我们可以进行非常细致和精确的语义匹配。

5.  **高亮查找与内容引用 (`org-include-inline`)**:
    *   您提到的 `org-include-inline` ([yibie/org-include-inline on GitHub](https://github.com/yibie/org-include-inline)) 是一个非常巧妙的 Emacs 特色解决方案！它可以用于：
        *   **辅助查询**: 用户高亮一段文本，我们可以提取这段文本作为查询条件，进行语义搜索，找到相关的 Org 节点。
        *   **上下文增强**: 在 RAG 的“生成”阶段，如果检索到的相关节点很多，或者用户希望从某个特定相关节点中精确地选取一部分内容喂给 LLM，可以通过 `org-include-inline` 动态地将这部分内容嵌入到当前的上下文或一个临时 buffer 中，然后再传递给 LLM。
    *   我们可以设计一个函数，例如 `org-supertag-rag-find-similar-to-region`，它获取当前高亮区域的文本，然后调用向量检索功能。另一个函数可以是 `org-supertag-rag-include-node-for-llm`，它允许用户选择一个已检索到的相似节点，并使用 `org-include-inline` 将其内容（或部分内容）准备好，用于后续的 LLM 交互。

6.  **Chat with Notes (`ollama_bridge.py`, `org-supertag-sim.el`, `org-supertag-sim-epc.el`)**:
    *   您已经通过 `ollama_bridge.py` 和 EPC (在 `org-supertag-sim.el` 和 `org-supertag-sim-epc.el` 中实现) 搭建了与本地 LLM (Ollama) 对话的基础框架，这非常好！
    *   完整的 RAG 聊天流程将会是：
        1.  用户在 Emacs 中输入查询。
        2.  查询文本被发送到 Python 后端（通过 EPC），使用 `sentence-transformers` 模型（类似 `tag_vectors.py` 中的逻辑，但针对节点内容）进行向量化。
        3.  在节点向量库中进行相似性搜索，找出最相关的 K 个 Org 节点。
        4.  提取这 K 个节点的文本内容。
        5.  将用户的原始查询和这 K 个节点的文本内容一起组合成一个新的 prompt。
        6.  这个组合后的 prompt 通过 `ollama_bridge.py` 发送给 Ollama (或其他 LLM)。
        7.  LLM 返回的答案通过 EPC 返回给 Emacs 并显示给用户。
    *   现有的 `org-supertag-sim-epc.el` 中的 `org-supertag-sim-epc-interactive-ollama` 函数可以作为构建此聊天界面的起点。

7.  **设置**:
    *   我们会在系统中加入必要的 `defcustom` 变量，允许用户配置排除的文件、目录、特定标签的节点等，以及向量模型的选择、相似度阈值等。

8.  **本地模型支持 (LM Studio 等)**:
    *   是的，扩展 `ollama_bridge.py` 以支持更多本地 LLM 服务（如 LM Studio）是一个很好的方向。这通常意味着在 Python 代码中增加对不同 API 端点和请求/响应格式的处理逻辑。

**行动建议与后续步骤讨论**

基于您的反馈和我们现有的基础，我建议我们可以按以下步骤推进：

1.  **节点内容提取与向量化**:
    *   **定义节点内容**: 明确一个 Org 节点用于向量化的具体内容范围。默认可以是标题 + 所有子内容（包括 PROPERTIES drawer, LOGBOOK, 以及正文）。可以考虑增加一个节点属性（如 `:VECTOR_CONTENT:`）允许用户覆盖默认行为，只向量化特定部分。
    *   **修改 `tag_vectors.py` (或创建一个新的 Python 模块)**:
        *   使其能够接收 Org 节点的完整文本内容进行向量化，而不仅仅是标签名。
        *   调整其向量存储/加载逻辑，以节点 ID (或 hash) 为键，存储在 JSON 文件中。
    *   **Emacs Lisp 端**: 编写函数来遍历 Org 文件，提取每个节点的 ID 和指定内容，然后通过 EPC 将这些信息发送给 Python 端进行向量化和存储。`org-supertag-sync.el` 中的节点扫描和哈希机制可以复用。

2.  **实现语义检索**:
    *   **Python 端**: 在 `tag_vectors.py` (或新模块) 中，实现接收一个查询向量，然后计算与存储中所有节点向量的余弦相似度，返回 top-K 相似的节点 ID。
    *   **Emacs Lisp 端**: 创建一个函数，接收用户查询字符串，将其发送到 Python 端获取查询向量，然后调用上述检索功能，获取相似节点 ID 列表。

3.  **初步 UI 集成 (Smart Panel)**:
    *   开始设计 `Smart Panel`。当光标停留在某个 Org 节点时，触发一次语义检索，并将结果（例如，最相似的5个节点的标题和文件路径）显示在 "Similar Connections" 部分。

您觉得这个初步的行动计划如何？我们是否应该首先聚焦于**节点内容的准确提取和第一版的向量化存储与检索流程**？这似乎是构建后续所有功能的基础。

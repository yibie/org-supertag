# org-supertag 系统架构文档

## 🏗️ 整体系统架构

```mermaid
graph TB
    subgraph "用户界面层"
        UI[Emacs用户界面]
        Views[多种视图系统]
        Commands[交互命令]
    end
    
    subgraph "核心业务层"
        TagSystem[标签系统]
        NodeSystem[节点系统] 
        BehaviorSystem[行为系统]
        QuerySystem[查询系统]
        RelationSystem[关系系统]
    end
    
    subgraph "AI服务层"
        AICore[AI核心模块]
        SimilarityEngine[相似度引擎]
        EPCService[EPC通信服务]
    end
    
    subgraph "数据持久化层"
        Database[(SQLite数据库)]
        SyncEngine[同步引擎]
        Cache[缓存系统]
        Recovery[恢复系统]
    end
    
    subgraph "Python后端"
        OllamaService[Ollama服务]
        MLModels[ML模型]
        VectorDB[向量数据库]
    end
    
    %% 连接关系
    UI --> TagSystem
    UI --> NodeSystem
    UI --> BehaviorSystem
    UI --> QuerySystem
    
    Views --> TagSystem
    Views --> NodeSystem
    Views --> QuerySystem
    
    Commands --> BehaviorSystem
    Commands --> RelationSystem
    
    TagSystem --> Database
    NodeSystem --> Database
    BehaviorSystem --> Database
    QuerySystem --> Database
    RelationSystem --> Database
    
    AICore --> EPCService
    SimilarityEngine --> EPCService
    EPCService --> OllamaService
    EPCService --> MLModels
    
    SimilarityEngine --> VectorDB
    
    Database --> SyncEngine
    Database --> Cache
    Database --> Recovery
    
    style UI fill:#e1f5fe
    style TagSystem fill:#f3e5f5
    style AICore fill:#fff3e0
    style Database fill:#e8f5e8
    style OllamaService fill:#fff8e1
```

## 🧩 核心子系统详细架构

### 1. 标签系统架构

```mermaid
graph TD
    subgraph "标签系统核心"
        TagManager[标签管理器]
        TagRelation[标签关系]
        TagBehavior[标签行为]
        InlineTag[内联标签]
    end
    
    subgraph "标签操作"
        TagAdd[添加标签]
        TagEdit[编辑标签]
        TagDelete[删除标签]
        TagComplete[标签补全]
    end
    
    subgraph "标签视图"
        TagTable[表格视图]
        TagDiscover[发现视图]
        TagColumn[列比较视图]
    end
    
    TagManager --> TagAdd
    TagManager --> TagEdit
    TagManager --> TagDelete
    TagManager --> TagComplete
    
    TagRelation --> TagTable
    TagRelation --> TagDiscover
    TagRelation --> TagColumn
    
    TagBehavior --> TagManager
    InlineTag --> TagManager
    
    style TagManager fill:#ffebee
    style TagRelation fill:#f3e5f5
    style TagBehavior fill:#e8f5e8
```

### 2. AI后端系统架构

```mermaid
graph TB
    subgraph "Emacs Lisp AI模块"
        AIInterface[AI接口]
        SimilarityCalc[相似度计算]
        EPCClient[EPC客户端]
        AIWorkflow[AI工作流]
    end
    
    subgraph "Python EPC服务"
        EPCServer[EPC服务器]
        ModelManager[模型管理器]
        TaskProcessor[任务处理器]
    end
    
    subgraph "AI模型层"
        Ollama[Ollama服务]
        Gemma[Gemma-3-4B模型]
        SentenceTransformer[句子转换器]
        Torch[PyTorch后端]
    end
    
    subgraph "数据处理"
        TextProcessor[文本处理]
        VectorStore[向量存储]
        Embedding[嵌入生成]
    end
    
    AIInterface --> EPCClient
    SimilarityCalc --> EPCClient
    AIWorkflow --> EPCClient
    
    EPCClient --> EPCServer
    EPCServer --> ModelManager
    EPCServer --> TaskProcessor
    
    ModelManager --> Ollama
    ModelManager --> Gemma
    ModelManager --> SentenceTransformer
    
    TaskProcessor --> TextProcessor
    TaskProcessor --> VectorStore
    TaskProcessor --> Embedding
    
    Gemma --> Torch
    SentenceTransformer --> Torch
    
    style AIInterface fill:#e3f2fd
    style EPCServer fill:#fff3e0
    style Ollama fill:#f1f8e9
    style Torch fill:#fce4ec
```

### 3. 数据持久化架构

```mermaid
graph TD
    subgraph "数据访问层"
        DBInterface[数据库接口]
        QueryBuilder[查询构建器]
        Transaction[事务管理]
    end
    
    subgraph "SQLite数据库"
        NodeTable[(节点表)]
        TagTable[(标签表)]
        RelationTable[(关系表)]
        FieldTable[(字段表)]
        BehaviorTable[(行为表)]
    end
    
    subgraph "缓存系统"
        MemoryCache[内存缓存]
        FileCache[文件缓存]
        QueryCache[查询缓存]
    end
    
    subgraph "同步机制"
        AutoSync[自动同步]
        ManualSync[手动同步]
        ConflictResolver[冲突解决]
    end
    
    subgraph "备份恢复"
        AutoBackup[自动备份]
        RecoveryTool[恢复工具]
        DataMigration[数据迁移]
    end
    
    DBInterface --> NodeTable
    DBInterface --> TagTable
    DBInterface --> RelationTable
    DBInterface --> FieldTable
    DBInterface --> BehaviorTable
    
    QueryBuilder --> Transaction
    Transaction --> DBInterface
    
    DBInterface --> MemoryCache
    DBInterface --> FileCache
    DBInterface --> QueryCache
    
    AutoSync --> DBInterface
    ManualSync --> DBInterface
    ConflictResolver --> AutoSync
    
    AutoBackup --> NodeTable
    RecoveryTool --> AutoBackup
    DataMigration --> RecoveryTool
    
    style DBInterface fill:#e8eaf6
    style NodeTable fill:#f3e5f5
    style MemoryCache fill:#e0f2f1
    style AutoSync fill:#fff8e1
```

## 📊 模块依赖关系图

```mermaid
graph LR
    subgraph "主入口"
        Main[org-supertag.el]
    end
    
    subgraph "核心模块"
        DB[org-supertag-db.el]
        Node[org-supertag-node.el]
        Tag[org-supertag-tag.el]
        Query[org-supertag-query.el]
        Behavior[org-supertag-behavior.el]
        Relation[org-supertag-relation.el]
    end
    
    subgraph "AI模块"
        AI[org-supertag-ai.el]
        Sim[org-supertag-sim.el]
        EPC[org-supertag-sim-epc.el]
    end
    
    subgraph "界面模块"
        View[org-supertag-view.el]
        Table[org-supertag-view-table.el]
        Column[org-supertag-view-column.el]
        Discover[org-supertag-view-discover.el]
        Inline[org-supertag-inline.el]
    end
    
    subgraph "支持模块"
        Field[org-supertag-field.el]
        Sync[org-supertag-sync.el]
        Recovery[org-supertag-recovery.el]
        Backlink[org-supertag-backlink.el]
    end
    
    %% 依赖关系
    Main --> DB
    Main --> Node
    Main --> Tag
    Main --> Query
    Main --> Behavior
    Main --> Relation
    Main --> AI
    Main --> Sim
    Main --> EPC
    Main --> View
    Main --> Inline
    Main --> Sync
    Main --> Recovery
    Main --> Backlink
    
    Node --> DB
    Tag --> DB
    Query --> DB
    Behavior --> DB
    Relation --> DB
    
    AI --> EPC
    Sim --> EPC
    
    Table --> View
    Column --> View
    Discover --> View
    
    Field --> DB
    Sync --> DB
    Recovery --> DB
    Backlink --> Node
    
    style Main fill:#ffcdd2
    style DB fill:#c8e6c9
    style AI fill:#ffe0b2
    style View fill:#e1bee7
```

## 🔄 数据流架构

```mermaid
sequenceDiagram
    participant User as 用户
    participant UI as 用户界面
    participant Core as 核心系统
    participant DB as 数据库
    participant AI as AI后端
    participant Python as Python服务
    
    User->>UI: 执行标签操作
    UI->>Core: 调用业务逻辑
    Core->>DB: 查询/更新数据
    DB-->>Core: 返回结果
    
    opt AI功能调用
        Core->>AI: 请求AI处理
        AI->>Python: EPC通信
        Python->>Python: 模型推理
        Python-->>AI: 返回结果
        AI-->>Core: 处理结果
    end
    
    Core->>UI: 更新界面
    UI-->>User: 显示结果
    
    Note over DB: 异步同步机制
    DB->>DB: 自动备份
```

## 🛠️ 技术栈映射

| 层级 | 技术栈 | 主要组件 |
|------|--------|----------|
| **用户界面** | Emacs Lisp | org-mode, 自定义界面 |
| **业务逻辑** | Emacs Lisp | 核心模块、行为系统 |
| **AI服务** | Python + Emacs Lisp | EPC通信、模型推理 |
| **数据存储** | SQLite + 文件系统 | 数据库、缓存、备份 |
| **AI模型** | PyTorch生态 | Ollama、Transformers |
| **通信协议** | EPC (Emacs-Python) | 异步消息传递 |

## 📈 性能和扩展性考虑

### 性能优化点
- **缓存策略**: 多层缓存减少数据库访问
- **异步处理**: AI推理和数据同步异步化
- **索引优化**: 数据库查询性能优化
- **增量同步**: 只同步变更部分

### 扩展性设计
- **模块化架构**: 各模块相对独立
- **插件机制**: 行为系统支持自定义扩展
- **配置驱动**: 多数功能可通过配置调整
- **版本兼容**: 数据迁移和向后兼容

---
*架构文档 - PLAN模式生成* 
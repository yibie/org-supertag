"""
Configuration Management Module
Provides project configuration and state management
"""
import os
import logging
import os.path as osp
from dataclasses import field
from typing import Optional, Dict, Any, List
import toml # <--- NEW: Import toml for dynamic config persistence
from pydantic import BaseModel, Field

# Define the default analysis config dictionary at the module level
analysis_config = {
    "enable_inferred_relations": True,
    "inference_model": "phi4-mini:3.8b",
    "processing_workers": 4,
}

# Define a default for processing_config as well
processing_config = {
    "processing_workers": 4, # You can add other defaults here
}

class Config(BaseModel):
    """Main configuration class, loaded from simtag.yaml."""
    config_data: Dict[str, Any] = Field(default_factory=dict, repr=False)
    # --- NEW: Central Data Directory ---
    data_directory: str = Field(default_factory=lambda: os.environ.get("ORG_SUPERTAG_DATA_DIRECTORY", os.path.expanduser("~/.emacs.d/org-supertag")))

    # Vector database path
    vector_db_path: str = Field(default_factory=lambda: os.path.expanduser("~/.emacs.d/org-supertag/supertag_vector.db"))
    # --- NEW: Path for dynamic config TOML file ---
    dynamic_config_file_path: str = Field(default_factory=lambda: os.path.expanduser("~/.emacs.d/org-supertag/runtime_config.toml"))
    
    # --- LLM Client Configuration ---
    llm_client_config: Dict[str, Any] = Field(default_factory=lambda: {
        'provider': 'ollama', # Default provider
        'base_url': 'http://localhost:11434', # Default for Ollama
        'default_model': 'hf.co/unsloth/gemma-3-4b-it-GGUF', # Default generation model
        #'default_embedding_model': 'nomic-embed-text', # Default embedding model
        'default_embedding_model': 'qwen3:0.6b', # 使用Qwen3 0.6B作为默认嵌入模型
        'api_key': None, # For providers like OpenAI
        'timeout': 120 # Default request timeout
    })

    # Deprecated Ollama-specific fields (values moved to llm_client_config)
    # ollama_model: str = "gemma"
    # ollama_base_url: str = "http://localhost:11434"
    
    # Embedding model name (can also be part of llm_client_config if desired, or kept separate for now)
    # embedding_model: str = "sentence-transformers/all-MiniLM-L6-v2" # Removed, use llm_client_config['default_embedding_model']
    
    # Whether to use cache
    use_cache: bool = True
    
    # Cache size
    cache_size: int = 1000
    
    # Whether to output debug information
    debug: bool = False

    # --- RAG Retrieval Configuration ---
    rag_graph_depth: int = 2
    rag_vector_results: int = 10
    rag_time_range_days: Optional[int] = None  # Nil for no limit, e.g., 365 for last year
    rag_semantic_threshold: float = 0.7
    rag_cooccurrence_threshold: int = 3
    rag_time_decay: bool = True
    rag_retrieval_mode: str = "balanced"  # 'precise', 'balanced', 'exploratory'

    # --- RAG Context Window Management ---
    rag_context_window_size: int = 4000  # Total tokens
    rag_context_core_content_ratio: float = 0.4
    rag_context_supplementary_ratio: float = 0.35
    rag_context_background_ratio: float = 0.25
    rag_context_priorities: List[str] = Field(default_factory=lambda: [
        "current_node",
        "recent_nodes",
        "high_frequency_concepts",
        "historical_dialogue", # Optional
        "cross_domain_relations" # Optional
    ])
    rag_context_auto_truncate: bool = True
    rag_context_keep_summary: bool = True

    # --- Dialogue Mode RAG Presets ---
    # Stored as a dictionary mapping mode name to a dict of RAG config overrides
    rag_mode_presets: Dict[str, Dict[str, Any]] = Field(default_factory=lambda: {
        "normal": {
            "rag_retrieval_mode": "precise",
            "rag_graph_depth": 1,
            "rag_vector_results": 5,
            "rag_semantic_threshold": 0.8,
            "rag_context_window_size": 2000
        },
        "explore": {
            "rag_retrieval_mode": "exploratory",
            "rag_graph_depth": 3,
            "rag_vector_results": 15,
            "rag_semantic_threshold": 0.6,
            # "cross_domain_priority": True # Example of a custom flag for a mode
        },
        "socratic": {
            "rag_retrieval_mode": "balanced",
            # "include_dialogue_history": True,
            # "search_counterarguments": True,
            "rag_context_window_size": 3000
        }
    })

    # --- Memory Mechanism Configuration ---
    memory_preferences_limit: int = 50
    memory_patterns_limit: int = 30
    memory_dialogues_limit: int = 200
    memory_cleanup_threshold: float = 0.3 # Confidence threshold for cleanup
    memory_token_budget_preferences: float = 0.15 # Percentage of total context
    memory_token_budget_patterns: float = 0.15
    memory_token_budget_dialogue_history: float = 0.10

    # --- Entity Extractor Configuration ---
    entity_extractor_config: Dict[str, Any] = Field(default_factory=lambda: {
        "entity_types": None, # Example: [{'name': 'PERSON', 'description': '...'}]
        "extraction_prompt_template": None, # Optional: Path to a custom prompt template or the template string itself
        "gleaning_prompt_template": None, # Optional
        "max_gleaning_rounds": 2,  # 保留精化轮次以保证质量
        "max_entities_per_extraction": 15,  # 限制每次提取的实体数量
        "max_relations_per_extraction": 10,   # 限制每次提取的关系数量
        "llm_timeout": 180,  # LLM调用超时时间（秒）
        "max_retries": 2     # 最大重试次数
    })
    
    # --- SmartNER Service Configuration ---
    use_smart_ner_service: bool = True  # 启用 SmartNERServiceV2 优化实现
    
    # --- Multicore Processing Configuration ---
    multicore_config: Dict[str, Any] = Field(default_factory=lambda: {
        "enabled": True,  # 启用多核心处理
        "max_workers": None,  # 工作进程数：None为自动优化（推荐2），用户可设置1-16
        "max_ollama_instances": None,  # Ollama实例数：None为自动优化（推荐2），用户可设置1-8
        "enable_multi_ollama": True,  # 启用多Ollama实例支持
        "process_type": "process",  # "process" 或 "thread" - 使用process实现真正并行
        "embedding_batch_threshold": 3,  # 嵌入任务启用多核心的最小任务数
        "ner_batch_threshold": 2,  # NER任务启用多核心的最小任务数
        "timeout": 300,  # 任务超时时间（秒）
        "memory_threshold_percent": 85,  # 内存使用率阈值，超过则禁用多核心
        # 🎯 基于性能测试的推荐配置（用户可覆盖）
        "recommended_workers": 2,  # 推荐的工作进程数（基于测试发现的最优值）
        "recommended_instances": 2,  # 推荐的Ollama实例数（基于测试发现的最优值）
    })
    
    # --- Embedding Service Configuration ---
    embedding_config: Dict[str, Any] = Field(default_factory=lambda: {
        "primary_backend": "llama_cpp",  # 使用 llama.cpp 作为主要后端
        "fallback_backends": ["ollama"],  # 只使用ollama作为备用后端，避免本地模型下载问题
        "cache_enabled": True,
        "batch_size": 16,  # 减小批处理大小以避免llama.cpp序列ID问题
        "max_retries": 3,  # 增加重试次数
        "local_model": "sentence-transformers/all-MiniLM-L6-v2",  # 保留配置但不使用
        # "ollama_model": "nomic-embed-text",
        # "ollama_timeout": 300,
        "llama_cpp_model_path": "~/.models/Qwen3-Embedding-0.6B-GGUF/Qwen3-Embedding-0.6B-Q8_0.gguf",  # Qwen3模型路径
        "llama_cpp_binary": "llama-embedding",  # 二进制文件名
        "llama_cpp_pooling": "cls",  # 改为cls池化策略以避免mean pooling的序列ID问题
        "llama_cpp_threads": None,  # 线程数，None表示自动检测
        "llama_cpp_batch_size": 8,  # 添加llama.cpp专用的批处理大小
        "llama_cpp_max_context": 512,  # 最大上下文长度
        
        # 长文本处理配置
        "long_text_strategy": "chunk",  # 长文本处理策略: "truncate" | "chunk"
        "chunk_size": 400,  # 分块大小（字符数）
        "chunk_overlap": 50,  # 分块重叠（字符数）
        "chunk_aggregation": "mean",  # 聚合策略: "mean" | "weighted_mean" | "max_pool"
        "max_chunks": 10,  # 最大分块数量（防止过长文本）
        "force_update": True,  # 强制更新所有内容
        "incremental_update": False,  # 禁用增量更新
        "embedding_enabled": True,  # 强制启用嵌入
    })
    
    # ==============================================================================
    # Analysis and Post-Processing Configuration
    # ==============================================================================
    processing_config: dict = Field(default_factory=dict)
    analysis_config: dict = Field(default_factory=dict)
    retrieval_config: dict = Field(default_factory=dict)
    log_file: Optional[str] = None

    def __init__(self, **data):
        super().__init__(**data)
        self.llm_client_config = self.config_data.get('llm_client_config', self.llm_client_config)
        self.processing_config = self.config_data.get('processing_config', processing_config.copy())
        self.analysis_config = self.config_data.get('analysis_config', analysis_config.copy())
        self.retrieval_config = self.config_data.get('retrieval_config', {
            "enable_inferred_relations": True,
            "inference_model": "phi4-mini:3.8b",
            "processing_workers": 4,
        })
        self._dynamic_config_values: Dict[str, Any] = {} 
        self._load_dynamic_config() # <--- NEW: Load dynamic config on init

        # Check environment variable configuration
        env_vector_db = os.environ.get("ORG_SUPERTAG_VECTOR_DB")
        if env_vector_db:
            self.vector_db_path = env_vector_db
            logging.getLogger("config").info(f"Loaded vector database path from environment: {self.vector_db_path}")
            
        # Environment variable overrides for LLM Client Config
        # Example: ORG_SUPERTAG_LLM_PROVIDER, ORG_SUPERTAG_LLM_BASE_URL, etc.
        env_llm_provider = os.environ.get("ORG_SUPERTAG_LLM_PROVIDER")
        if env_llm_provider:
            self.llm_client_config['provider'] = env_llm_provider
            logging.getLogger("config").info(f"LLM Provider set from environment: {env_llm_provider}")

        env_llm_base_url = os.environ.get("ORG_SUPERTAG_LLM_BASE_URL")
        if env_llm_base_url:
            self.llm_client_config['base_url'] = env_llm_base_url
            logging.getLogger("config").info(f"LLM Base URL set from environment: {env_llm_base_url}")

        env_llm_default_model = os.environ.get("ORG_SUPERTAG_LLM_DEFAULT_MODEL")
        if env_llm_default_model:
            self.llm_client_config['default_model'] = env_llm_default_model
            logging.getLogger("config").info(f"LLM Default Model set from environment: {env_llm_default_model}")

        env_llm_embedding_model = os.environ.get("ORG_SUPERTAG_LLM_EMBEDDING_MODEL")
        if env_llm_embedding_model:
            self.llm_client_config['default_embedding_model'] = env_llm_embedding_model
            logging.getLogger("config").info(f"LLM Default Embedding Model set from environment: {env_llm_embedding_model}")
        
        env_llm_api_key = os.environ.get("ORG_SUPERTAG_LLM_API_KEY")
        if env_llm_api_key:
            self.llm_client_config['api_key'] = env_llm_api_key
            # Be cautious logging API keys, even parts of them.
            logging.getLogger("config").info(f"LLM API Key loaded from environment (not displaying value).")

        # Environment variable overrides for Entity Extractor Config
        env_ee_entity_types_json = os.environ.get("ORG_SUPERTAG_EE_ENTITY_TYPES_JSON")
        if env_ee_entity_types_json:
            try:
                import json
                self.entity_extractor_config['entity_types'] = json.loads(env_ee_entity_types_json)
                logging.getLogger("config").info(f"Loaded Entity Extractor entity types from environment.")
            except json.JSONDecodeError:
                logging.getLogger("config").warning("Failed to parse ORG_SUPERTAG_EE_ENTITY_TYPES_JSON from environment.")

        env_ee_llm_model_override = os.environ.get("ORG_SUPERTAG_EE_LLM_MODEL_OVERRIDE")
        if env_ee_llm_model_override:
            self.entity_extractor_config['llm_model_override'] = env_ee_llm_model_override
            logging.getLogger("config").info(f"Entity Extractor LLM model override set from environment: {env_ee_llm_model_override}")

        # Set log file path
        self.log_file = self.get_log_file_path()
        # Ensure log directory exists
        log_dir = os.path.dirname(self.log_file)
        if log_dir and not os.path.exists(log_dir):
            os.makedirs(log_dir, exist_ok=True)
        
        # Configure file log handler
        if not logging.getLogger("config").handlers:
            file_handler = logging.FileHandler(self.log_file)
            formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
            file_handler.setFormatter(formatter)
            logging.getLogger("config").addHandler(file_handler)
        
        logging.getLogger("config").info(f"Configuration initialization complete - Log file: {self.log_file}")
        
        # Ensure vector database directory exists
        if self.vector_db_path:
            db_dir = osp.dirname(self.vector_db_path)
            if db_dir:  # Only create if directory path is not empty
                os.makedirs(db_dir, exist_ok=True)
                logging.getLogger("config").info(f"Created storage directory: {db_dir}")
    
    def get_log_file_path(self):
        """Get log file path consistent with Emacs configuration"""
        # Get Emacs data directory from environment variable
        emacs_data_dir = self.data_directory
        
        if emacs_data_dir:
            # Use Emacs configured directory
            return osp.join(emacs_data_dir, "simtag_epc.log")
        elif self.vector_db_path:
            # Fallback to vector file directory
            return osp.join(osp.dirname(self.vector_db_path), "simtag_epc.log")
        else:
            # Finally use current directory
            return "simtag_epc.log"
    
    @property
    def status(self) -> dict:
        """Get current configuration status"""
        return {
            "vector_db_exists": os.path.exists(self.vector_db_path) if self.vector_db_path else False,
            "vector_db_path": self.vector_db_path,
            # "ollama_model": self.ollama_model, # Deprecated
            # "ollama_configured": bool(self.ollama_model), # Deprecated
            "llm_provider": self.llm_client_config.get('provider'),
            "llm_default_model": self.llm_client_config.get('default_model'),
            "llm_base_url": self.llm_client_config.get('base_url'),
            "llm_configured": bool(self.llm_client_config.get('provider') and self.llm_client_config.get('default_model')),
            "log_file": self.log_file
        }
    
    def update_vector_db_path(self, new_path: str):
        """Update vector database path"""
        self.vector_db_path = new_path
        # Ensure directory exists
        if new_path:
            db_dir = osp.dirname(new_path)
            if db_dir:  # Ensure directory path is not empty
                os.makedirs(db_dir, exist_ok=True)
        # Update log path
        self.log_file = self.get_log_file_path()
        return self.status

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary format
        
        Returns:
            Configuration dictionary
        """
        return {
            "vector_db_path": self.vector_db_path,
            # "ollama_model": self.ollama_model, # Deprecated
            # "embedding_model": self.embedding_model, # Removed
            "llm_client_config": self.llm_client_config,
            "use_cache": self.use_cache,
            "cache_size": self.cache_size,
            "entity_extractor_config": self.entity_extractor_config,
            "multicore_config": self.multicore_config,
            "embedding_config": self.embedding_config,
            "debug": self.debug,
            "processing_config": self.processing_config,
            "analysis_config": self.analysis_config,
            "retrieval_config": self.retrieval_config
        }

    # --- NEW: Methods for dynamic config values like timestamps ---
    def get_config_value(self, key: str, default: Any = None) -> Any:
        """
        Retrieves a configuration value that is not a predefined dataclass field.
        These are typically set at runtime (e.g., last sync timestamps).
        """
        return self._dynamic_config_values.get(key, default)

    def set_config_value(self, key: str, value: Any) -> None:
        """
        Sets a configuration value that is not a predefined dataclass field.
        Persists dynamic config to TOML file.
        """
        logger = logging.getLogger("config") # Use a logger instance
        self._dynamic_config_values[key] = value
        logger.info(f"Dynamic config value set: {{'{key}': '{value}'}}")
        self._save_dynamic_config() # <--- NEW: Save after setting a value
    # --- END NEW ---

    # --- NEW: Private methods for TOML persistence ---
    def _load_dynamic_config(self) -> None:
        """Loads dynamic configuration from the TOML file."""
        logger = logging.getLogger("config") # Use a logger instance
        if self.dynamic_config_file_path and os.path.exists(self.dynamic_config_file_path):
            try:
                with open(self.dynamic_config_file_path, 'r', encoding='utf-8') as f:
                    self._dynamic_config_values = toml.load(f)
                logger.info(f"Loaded dynamic configuration from {self.dynamic_config_file_path}")
            except Exception as e:
                logger.warning(f"Failed to load dynamic configuration from {self.dynamic_config_file_path}: {e}")
                self._dynamic_config_values = {} # Ensure it's a dict on failure
        else:
            logger.info(f"Dynamic configuration file not found or path not set. Initializing with empty dynamic config.")
            self._dynamic_config_values = {}

    def _save_dynamic_config(self) -> None:
        """Saves the current dynamic configuration to the TOML file."""
        logger = logging.getLogger("config") # Use a logger instance
        if self.dynamic_config_file_path:
            try:
                # Ensure directory for dynamic config file exists
                dynamic_config_dir = os.path.dirname(self.dynamic_config_file_path)
                if dynamic_config_dir and not os.path.exists(dynamic_config_dir):
                    os.makedirs(dynamic_config_dir, exist_ok=True)
                    logger.info(f"Created directory for dynamic config: {dynamic_config_dir}")

                with open(self.dynamic_config_file_path, 'w', encoding='utf-8') as f:
                    toml.dump(self._dynamic_config_values, f)
                logger.info(f"Saved dynamic configuration to {self.dynamic_config_file_path}")
            except Exception as e:
                logger.error(f"Failed to save dynamic configuration to {self.dynamic_config_file_path}: {e}")
    # --- END NEW ---

    @classmethod
    def get_fast_ner_models(cls) -> Dict[str, str]:
        """获取推荐的快速实体识别模型列表
        
        Returns:
            Dict[model_name, description]: 模型名称和描述的映射
        """
        return {
            "qwen2.5:1.5b": "Qwen2.5 1.5B - 轻量级，推荐选择",
            "gemma3:1b": "Google Gemma3 1B - 超快速处理",
            "qwen3:0.6b": "Qwen3 0.6B - 最轻量级选择", 
            "tinyllama:1.1b": "TinyLlama 1.1B - 极速处理",
            "smollm:latest": "SmolLM - 小型语言模型",
        }
    
    def get_vector_dimension_for_model(self) -> int:
        """获取当前嵌入模型的向量维度
        
        Returns:
            int: 向量维度
        """
        # 获取当前配置的嵌入模型
        embedding_model = self.llm_client_config.get('default_embedding_model', '')
        
        # 预定义的模型维度映射
        model_dimensions = {
            # Qwen系列模型
            'qwen3:0.6b': 1024,
            'qwen2.5:1.5b': 1536,
            'qwen2:1.5b': 1536,
            'qwen:7b': 4096,
            'qwen:14b': 5120,
            
            # Nomic Embed模型
            'nomic-embed-text': 768,
            'nomic-embed-text:v1.5': 768,
            
            # Sentence Transformers系列
            'sentence-transformers/all-MiniLM-L6-v2': 384,
            'sentence-transformers/all-mpnet-base-v2': 768,
            'sentence-transformers/all-distilroberta-v1': 768,
            
            # Ollama其他常见模型
            'gemma:2b': 2048,
            'gemma:7b': 3072,
            'gemma3:1b': 2048,
            'llama3.2:1b': 2048,
            'llama3.2:3b': 3072,
            
            # 默认维度
            'default': 768
        }
        
        # 尝试精确匹配
        if embedding_model in model_dimensions:
            dimension = model_dimensions[embedding_model]
            logging.getLogger("config").info(f"Found exact dimension match for {embedding_model}: {dimension}")
            return dimension
        
        # 尝试模糊匹配（处理版本号等）
        for model_pattern, dimension in model_dimensions.items():
            if model_pattern != 'default' and model_pattern in embedding_model:
                logging.getLogger("config").info(f"Found pattern match for {embedding_model} -> {model_pattern}: {dimension}")
                return dimension
        
        # 使用默认维度
        default_dim = model_dimensions['default']
        logging.getLogger("config").warning(f"No dimension mapping found for {embedding_model}, using default: {default_dim}")
        return default_dim

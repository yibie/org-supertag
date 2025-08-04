"""
Configuration Management Module
Provides project configuration and state management
"""
import os
import logging
import os.path as osp
from typing import Optional, Dict, Any, List
import toml # <--- NEW: Import toml for dynamic config persistence
from pydantic import BaseModel, Field
from pydantic_settings import BaseSettings, SettingsConfigDict


class OllamaBackendConfig(BaseModel):
    base_url: str = "http://localhost:11434"
    default_model: str = "gemma3:12b"
    #default_model: str = "qwen3:14b-q4_K_M"
    timeout: int = 60

class LLMConfig(BaseModel):
    provider: str = "ollama"
    primary_backend: str = "ollama"
    backends: Dict[str, OllamaBackendConfig] = Field(default_factory=lambda: {
        "ollama": OllamaBackendConfig()
    })

class EmbeddingConfig(BaseModel):
    provider: str = "ollama"  # Now defaults to "ollama"
    max_input_tokens: int = 512
    use_cache: bool = True
    cache_size: int = 1000
    
    llama_cpp: Dict[str, Any] = Field(default_factory=lambda: {

        "model_path": "/path/to/your/gguf/Qwen3-Embedding-0.6B-GGUF/qwen3-embedding-0.6b.q8_0.gguf"
    })
    
    ollama: Dict[str, Any] = Field(default_factory=lambda: {
        "base_url": "http://localhost:11434",
        "model_name": "nomic-embed-text",
        "dimension": None  # Optional: specify dimension if needed
    })

    # Whether to output debug information
    debug: bool = False

class Config(BaseSettings):
    """Main configuration class, loaded from simtag.yaml."""
    model_config = SettingsConfigDict(
        env_file=os.getenv("SIMTAG_CONFIG_PATH", ".env"),
        env_file_encoding='utf-8'
    )
    
    # --- NEW: Central Data Directory ---
    data_directory: str = Field(default_factory=lambda: os.environ.get("ORG_SUPERTAG_DATA_DIRECTORY", os.path.expanduser("~/.emacs.d/org-supertag")))

    # --- Dynamically constructed paths ---
    vector_db_path: str = "" # Will be set in __init__
    dynamic_config_file_path: str = "" # Will be set in __init__
    user_roles_file_path: str = "" # Will be set in __init__

    rag_graph_depth: int = 2
    
    llm: LLMConfig = Field(default_factory=LLMConfig)
    embedding: EmbeddingConfig = Field(default_factory=EmbeddingConfig)
    
    config_data: Dict[str, Any] = Field(default_factory=dict, repr=False)
    
    

    # Deprecated Ollama-specific fields (values moved to llm_client_config)
    # ollama_model: str = "gemma"
    # ollama_base_url: str = "http://localhost:11434"
    
    # Embedding model name (can also be part of llm_client_config if desired, or kept separate for now)
    # embedding_model: str = "sentence-transformers/all-MiniLM-L6-v2" # Removed, use llm_client_config['default_embedding_model']
    
    # Whether to use cache
    # use_cache: bool = True # MOVED to EmbeddingConfig
    
    # Cache size
    # cache_size: int = 1000 # MOVED to EmbeddingConfig
    
    # Whether to output debug information
    # debug: bool = False # MOVED to EmbeddingConfig

    # --- RAG Retrieval Configuration ---
    rag_vector_results: int = 10
    rag_time_range_days: Optional[int] = None  # Nil for no limit, e.g., 365 for last year
    rag_semantic_threshold: float = 0.7
    rag_cooccurrence_threshold: int = 3
    rag_time_decay: bool = True
    rag_retrieval_mode: str = "balanced"  # 'precise', 'balanced', 'exploratory'

    # --- New Hybrid Retrieval Parameters ---
    rag_neighbor_depth: int = 2             # BFS depth when expanding neighbors
    rag_vector_graph_ratio: float = 0.5     # Ratio of vector-based vs graph-based nodes
    rag_snippet_len: int = 200              # Max chars per snippet when building context

    rag_context_auto_truncate: bool = True
    rag_context_keep_summary: bool = True

    rag_bm25_top_k: int = 10  # Number of BM25 results to retrieve
    rag_bm25_weight: float = 0.5  # Weight multiplier for BM25 score when computing relevance

    # Path to BM25 index file (single-file pickle). Default inside simtag/ directory
    bm25_index_path: str = Field(default_factory=lambda: os.path.join(os.path.dirname(__file__), "bm25.idx"))

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
    
    # --- Sync Handler Configuration ---
    sync_max_concurrent_llm_tasks: int = 2 # Max concurrent LLM tasks during sync (increased for better performance)
    use_semantic_embedding_ids: bool = True # Use semantic IDs for embeddings instead of UUIDs

    
    # ==============================================================================
    # Analysis and Post-Processing Configuration
    # ==============================================================================
    processing_config: dict = Field(default_factory=dict)
    analysis_config: dict = Field(default_factory=dict)
    retrieval_config: dict = Field(default_factory=dict)
    
    # --- Chat Roles Configuration ---
    chat_roles: Dict[str, str] = Field(default_factory=dict)
    
    log_file: Optional[str] = None

    def __init__(self, config_dict: Optional[Dict] = None, **values: Any):
        """
        Custom initializer to bridge dictionary-based configuration with Pydantic models.
        """
        if config_dict is None:
            config_dict = {}

        # Allow individual values to override dictionary values
        init_data = {**config_dict, **values}
        
        # Pydantic V2 requires explicit initialization of sub-models from dicts
        # if the __init__ is overridden.
        if 'llm' in init_data and isinstance(init_data['llm'], dict):
            init_data['llm'] = LLMConfig(**init_data['llm'])
            
        if 'embedding' in init_data and isinstance(init_data['embedding'], dict):
            init_data['embedding'] = EmbeddingConfig(**init_data['embedding'])
            
        super().__init__(**init_data)

        # --- Dynamically construct paths based on data_directory ---
        if not self.vector_db_path:
            self.vector_db_path = os.path.join(self.data_directory, "supertag_vector.db")
        if not self.dynamic_config_file_path:
            self.dynamic_config_file_path = os.path.join(self.data_directory, "runtime_config.toml")
        if not self.user_roles_file_path:
            self.user_roles_file_path = os.path.join(self.data_directory, "roles.toml")
        
        # Keep the rest of the logic for dynamic/env var loading
        self._dynamic_config_values: Dict[str, Any] = {}
        self.llm_client_config: Dict[str, Any] = {}  # Initialize llm_client_config
        self.multicore_config: Dict[str, Any] = {}  # Initialize multicore_config
        self.embedding_config: Dict[str, Any] = {}  # Initialize embedding_config
        self.debug: bool = False  # Initialize debug flag
        self._load_dynamic_config()
        self._load_user_roles()

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
            if 'ollama' in self.llm.backends:
                self.llm.backends['ollama'].default_model = env_llm_default_model
                logging.getLogger("config").info(f"LLM Default Model set from environment: {env_llm_default_model}")

        env_llm_embedding_model = os.environ.get("ORG_SUPERTAG_LLM_EMBEDDING_MODEL")
        if env_llm_embedding_model:
            self.llm_client_config['default_embedding_model'] = env_llm_embedding_model
            logging.getLogger("config").info(f"LLM Default Embedding Model set from environment: {env_llm_embedding_model}")
        
        env_llm_api_key = os.environ.get("ORG_SUPERTAG_LLM_API_KEY")
        if env_llm_api_key:
            self.llm_client_config['api_key'] = env_llm_api_key
            # Be cautious logging API keys, even parts of them.
            logging.getLogger("config").info("LLM API Key loaded from environment (not displaying value).")

        # Environment variable overrides for Entity Extractor Config
        env_ee_entity_types_json = os.environ.get("ORG_SUPERTAG_EE_ENTITY_TYPES_JSON")
        if env_ee_entity_types_json:
            try:
                import json
                self.entity_extractor_config['entity_types'] = json.loads(env_ee_entity_types_json)
                logging.getLogger("config").info("Loaded Entity Extractor entity types from environment.")
            except json.JSONDecodeError:
                logging.getLogger("config").warning("Failed to parse ORG_SUPERTAG_EE_ENTITY_TYPES_JSON from environment.")

        env_ee_llm_model_override = os.environ.get("ORG_SUPERTAG_EE_LLM_MODEL_OVERRIDE")
        if env_ee_llm_model_override:
            self.entity_extractor_config['llm_model_override'] = env_ee_llm_model_override
            logging.getLogger("config").info(f"Entity Extractor LLM model override set from environment: {env_ee_llm_model_override}")

        # Set log file path
        self.log_file = os.path.join(self.data_directory, "simtag_epc.log")
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
                logging.getLogger("config").info(f"Ensured storage directory exists: {db_dir}")
    
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
            logger.info("Dynamic configuration file not found or path not set. Initializing with empty dynamic config.")
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

    def _load_user_roles(self):
        """Loads user-defined roles from the TOML file."""
        self.chat_roles = {}  # Start with an empty dictionary
        if os.path.exists(self.user_roles_file_path):
            try:
                with open(self.user_roles_file_path, 'r', encoding='utf-8') as f:
                    user_roles = toml.load(f)
                self.chat_roles.update(user_roles)
                logging.getLogger("config").info(f"Loaded and merged user-defined roles from {self.user_roles_file_path}")
            except Exception as e:
                logging.getLogger("config").warning(f"Failed to load user-defined roles from {self.user_roles_file_path}: {e}")

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
        embedding_model = self.embedding.llama_cpp.get("model_path", "")
        
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

class RAGConfig(BaseModel):
    enabled: bool = True
    search_top_k: int = 5
    traversal_max_hops: int = 1

class BehaviorConfig(BaseModel):
    proactive_tag_enabled: bool = True

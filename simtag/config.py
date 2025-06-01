"""
Configuration Management Module
Provides project configuration and state management
"""
import os
import logging
import os.path as osp
from dataclasses import dataclass, field
from typing import Optional, Dict, Any, List

@dataclass
class Config:
    """Configuration class"""
    
    # Vector database path
    vector_db_path: str = field(default_factory=lambda: os.path.expanduser("~/.emacs.d/org-supertag/supertag_vector.db"))
    
    # Ollama model name
    ollama_model: str = "mistral"
    
    # Embedding model name
    embedding_model: str = "sentence-transformers/all-MiniLM-L6-v2"
    
    # Whether to use cache
    use_cache: bool = True
    
    # Cache size
    cache_size: int = 1000
    
    # Whether to output debug information
    debug: bool = False
    
    def __post_init__(self):
        """Post-initialization processing"""
        logger = logging.getLogger("config")
        
        # Check environment variable configuration
        env_vector_db = os.environ.get("ORG_SUPERTAG_VECTOR_DB")
        if env_vector_db:
            self.vector_db_path = env_vector_db
            logger.info(f"Loaded vector database path from environment: {self.vector_db_path}")
            
        env_ollama_model = os.environ.get("ORG_SUPERTAG_OLLAMA_MODEL")
        if env_ollama_model:
            self.ollama_model = env_ollama_model
            logger.info(f"Loaded Ollama model from environment: {self.ollama_model}")
            
        env_embedding_model = os.environ.get("ORG_SUPERTAG_EMBEDDING_MODEL")
        if env_embedding_model:
            self.embedding_model = env_embedding_model
            logger.info(f"Loaded embedding model from environment: {self.embedding_model}")
        
        # Set log file path
        self.log_file = self.get_log_file_path()
        # Ensure log directory exists
        log_dir = os.path.dirname(self.log_file)
        if log_dir and not os.path.exists(log_dir):
            os.makedirs(log_dir, exist_ok=True)
        
        # Configure file log handler
        if not logger.handlers:
            file_handler = logging.FileHandler(self.log_file)
            formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
            file_handler.setFormatter(formatter)
            logger.addHandler(file_handler)
        
        logger.info(f"Configuration initialization complete - Log file: {self.log_file}")
        
        # Ensure vector database directory exists
        if self.vector_db_path:
            db_dir = osp.dirname(self.vector_db_path)
            if db_dir:  # Only create if directory path is not empty
                os.makedirs(db_dir, exist_ok=True)
                logger.info(f"Created storage directory: {db_dir}")
    
    def get_log_file_path(self):
        """Get log file path consistent with Emacs configuration"""
        # Get Emacs data directory from environment variable
        emacs_data_dir = os.environ.get("ORG_SUPERTAG_DATA_DIRECTORY")
        
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
            "ollama_model": self.ollama_model,
            "ollama_configured": bool(self.ollama_model),
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
            "ollama_model": self.ollama_model,
            "embedding_model": self.embedding_model,
            "use_cache": self.use_cache,
            "cache_size": self.cache_size,
            "debug": self.debug
        }

"""
SimTag Configuration Management Module
Handles all SimTag related configuration options and environment settings
"""

import os
import sys
import logging
import json
import subprocess
from typing import Dict, Any, Optional
from .utils.logging import setup_logging

def check_dependencies() -> bool:
    """Check if necessary dependencies are installed
    
    Returns:
        bool: Whether the dependencies are met
    """
    try:
        import torch
        import sentence_transformers
        import requests
        import epc
        import numpy
        import urllib3
        
        # Log dependency version information
        logging.info(f"Python version: {sys.version.split()[0]}")
        logging.info(f"PyTorch version: {torch.__version__}")
        logging.info(f"Sentence-Transformers version: {sentence_transformers.__version__}")
        logging.info(f"Urllib3 version: {urllib3.__version__}")
        
        return True
    except ImportError as e:
        logging.error(f"Missing necessary dependencies: {e}")
        return False

def check_environment() -> bool:
    """Check if the running environment meets the requirements
    
    Returns:
        bool: Whether it is in the correct environment
    """
    # Check Python version
    if sys.version_info < (3, 9):  # Relax version requirement to 3.9
        logging.warning(f"Current Python version {sys.version_info.major}.{sys.version_info.minor} may be too low")
        return False
        
    # Check dependencies
    return check_dependencies()

def ensure_environment():
    """Ensure that the running environment meets the requirements"""
    if not check_environment():
        msg = """
The running environment does not meet the requirements:
1. Requires Python 3.9 or higher
2. Requires the installation of the following dependencies:
   - torch
   - sentence-transformers
   - requests
   - epc
   - numpy
   - urllib3

If there is a lack of dependencies, they can be installed using the following command:
uv pip install torch sentence-transformers requests epc numpy urllib3
"""
        raise RuntimeError(msg)

class Config:
    """SimTag Configuration Management Class"""
    
    DEFAULT_MODEL_NAME = "hf.co/unsloth/gemma-3-4b-it-GGUF:latest"
    
    def __init__(self, 
                 vector_file: str = None,
                 db_file: str = None,
                 model_name: str = DEFAULT_MODEL_NAME,
                 debug: bool = False,
                 log_file: str = None,
                 host: str = '127.0.0.1',
                 port: int = 0):
        """Initialize the configuration
        
        Args:
            vector_file: Vector file path (specified by org-supertag-sim-epc-vector-file)
            db_file: Database file path (specified by org-supertag-db-file)
            model_name: Ollama model name
            debug: Whether to enable debug mode
            log_file: Log file path
            host: Server address
            port: Server port
        """
        # Check the environment
        ensure_environment()
        
        # Use the file paths passed directly
        self.vector_file = vector_file
        self.db_file = db_file
        
        # Validate the file paths
        if not self.vector_file:
            raise ValueError("Vector file path not specified")
        if not self.db_file:
            raise ValueError("Database file path not specified")
            
        # Log file path - use the directory where the vector file is located
        log_dir = os.path.dirname(self.vector_file)
        self.log_file = log_file or os.path.join(log_dir, "simtag_epc.log")
        
        # Other settings
        self.model_name = model_name
        self.debug = debug
        self.log_level = logging.DEBUG if debug else logging.INFO
        self.host = host
        self.port = port
        
        # Create the log directory
        if self.log_file:
            os.makedirs(os.path.dirname(self.log_file), exist_ok=True)
            
        # Log the path information
        logging.info(f"Configuration initialized:")
        logging.info(f"Vector file: {self.vector_file}")
        logging.info(f"Database file: {self.db_file}")
        logging.info(f"Log file: {self.log_file}")

    def ensure_ollama(self) -> bool:
        """Ensure Ollama is available"""
        try:
            # Simple check if the ollama command is available
            subprocess.run(["ollama", "--version"], capture_output=True, check=True)
            return True
        except (subprocess.CalledProcessError, FileNotFoundError):
            logging.error("Ollama is not installed or not available")
            return False

    def initialize_server(self) -> bool:
        """Initialize the server environment"""
        try:
            # Ensure the environment meets the requirements
            ensure_environment()
            
            # Ensure Ollama is available
            if not self.ensure_ollama():
                raise RuntimeError("Ollama is not installed or not available")
            
            # Create the necessary directories
            os.makedirs(os.path.dirname(self.vector_file), exist_ok=True)
            os.makedirs(os.path.dirname(self.log_file), exist_ok=True)
            
            # Set up the logging, passing specific parameters instead of self
            setup_logging(
                log_file=self.log_file,
                log_level=self.log_level,
                debug=self.debug
            )
            
            return True
            
        except Exception as e:
            logging.error(f"Failed to initialize the server environment: {e}")
            return False

    def setup(self):
        """Set up the running environment"""
        # Apply environment variables
        for key, value in self.env_vars.items():
            if key == "PYTHONPATH":
                # For PYTHONPATH, we need to append instead of overwrite
                current_path = os.environ.get("PYTHONPATH", "")
                if current_path:
                    os.environ["PYTHONPATH"] = f"{value}:{current_path}"
                else:
                    os.environ["PYTHONPATH"] = value
            else:
                os.environ[key] = value
        
        # Create the necessary directories
        os.makedirs(os.path.dirname(self.vector_file), exist_ok=True)
        os.makedirs(os.path.dirname(self.log_file), exist_ok=True)
        
    def to_dict(self) -> Dict[str, Any]:
        """Convert the configuration to a dictionary"""
        return {
            "vector_file": self.vector_file,
            "db_file": self.db_file,
            "model_name": self.model_name,
            "debug": self.debug,
            "log_file": self.log_file,
            "is_initialized": self.is_initialized,
            "env_vars": self.env_vars,
            "host": self.host,
            "port": self.port
        }
        
    def save(self, filepath: str) -> None:
        """Save the configuration to a file"""
        with open(filepath, 'w') as f:
            json.dump(self.to_dict(), f, indent=2)
            
    @classmethod
    def load(cls, filepath: str) -> 'Config':
        """Load the configuration from a file"""
        if not os.path.exists(filepath):
            return cls()
            
        with open(filepath, 'r') as f:
            config_dict = json.load(f)
            
        config = cls(
            vector_file=config_dict.get("vector_file"),
            db_file=config_dict.get("db_file"),
            model_name=config_dict.get("model_name"),
            debug=config_dict.get("debug", False),
            log_file=config_dict.get("log_file"),
            host=config_dict.get("host", '127.0.0.1'),
            port=config_dict.get("port", 0)
        )
        
        config.is_initialized = config_dict.get("is_initialized", False)
        if "env_vars" in config_dict:
            config.env_vars.update(config_dict["env_vars"])
            
        return config
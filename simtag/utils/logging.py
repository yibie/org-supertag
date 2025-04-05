"""
SimTag Logging Utility Module
Provides unified logging functionality
"""

import os
import sys
import logging
from typing import Dict, Any, Optional

def setup_logging(log_file: Optional[str], log_level: int = logging.INFO):
    """Set up logging configuration
    
    Args:
        log_file: Log file path, if None, only output to console
        log_level: Log level
    """
    # Basic configuration
    logging.basicConfig(
        level=log_level,
        format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S'
    )
    
    # If a log file is specified, add a file handler
    if log_file:
        # Ensure the log directory exists
        log_dir = os.path.dirname(log_file)
        if log_dir and not os.path.exists(log_dir):
            os.makedirs(log_dir)
            
        # Create a file handler
        file_handler = logging.FileHandler(log_file)
        file_handler.setLevel(log_level)
        file_handler.setFormatter(logging.Formatter(
            '%(asctime)s [%(levelname)s] %(name)s: %(message)s',
            '%Y-%m-%d %H:%M:%S'
        ))
        
        # Add to the root logger
        logging.getLogger().addHandler(file_handler)

def get_logger(name: str) -> logging.Logger:
    """Get a logger with the specified name
    
    Args:
        name: Logger name
        
    Returns:
        Named logger
    """
    return logging.getLogger(name) 
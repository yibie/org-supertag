"""
SimTag 日志工具模块
提供统一的日志功能
"""

import os
import sys
import logging
from typing import Dict, Any, Optional

def setup_logging(log_file: Optional[str], log_level: int = logging.INFO):
    """设置日志配置
    
    Args:
        log_file: 日志文件路径，如果为None则只输出到控制台
        log_level: 日志级别
    """
    # 基本配置
    logging.basicConfig(
        level=log_level,
        format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S'
    )
    
    # 如果指定了日志文件，添加文件处理器
    if log_file:
        # 确保日志目录存在
        log_dir = os.path.dirname(log_file)
        if log_dir and not os.path.exists(log_dir):
            os.makedirs(log_dir)
            
        # 创建文件处理器
        file_handler = logging.FileHandler(log_file)
        file_handler.setLevel(log_level)
        file_handler.setFormatter(logging.Formatter(
            '%(asctime)s [%(levelname)s] %(name)s: %(message)s',
            '%Y-%m-%d %H:%M:%S'
        ))
        
        # 添加到根日志器
        logging.getLogger().addHandler(file_handler)

def get_logger(name: str) -> logging.Logger:
    """获取指定名称的日志器
    
    Args:
        name: 日志器名称
        
    Returns:
        命名日志器
    """
    return logging.getLogger(name) 
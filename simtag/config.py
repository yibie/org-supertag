"""
SimTag 配置管理模块
处理所有SimTag相关的配置选项和环境设置
"""

import os
import sys
import logging
import json
import subprocess
from typing import Dict, Any, Optional
from .utils.logging import setup_logging

def check_dependencies() -> bool:
    """检查必要的依赖是否已安装
    
    Returns:
        bool: 是否满足依赖要求
    """
    try:
        import torch
        import sentence_transformers
        import requests
        import epc
        import numpy
        import urllib3
        
        # 记录依赖版本信息
        logging.info(f"Python版本: {sys.version.split()[0]}")
        logging.info(f"PyTorch版本: {torch.__version__}")
        logging.info(f"Sentence-Transformers版本: {sentence_transformers.__version__}")
        logging.info(f"Urllib3版本: {urllib3.__version__}")
        
        return True
    except ImportError as e:
        logging.error(f"缺少必要的依赖: {e}")
        return False

def check_environment() -> bool:
    """检查运行环境是否满足要求
    
    Returns:
        bool: 是否在正确的环境中
    """
    # 检查Python版本
    if sys.version_info < (3, 9):  # 放宽版本要求到 3.9
        logging.warning(f"当前Python版本 {sys.version_info.major}.{sys.version_info.minor} 可能过低")
        return False
        
    # 检查依赖
    return check_dependencies()

def ensure_environment():
    """确保运行环境满足要求"""
    if not check_environment():
        msg = """
运行环境不满足要求:
1. 需要 Python 3.9 或更高版本
2. 需要安装以下依赖:
   - torch
   - sentence-transformers
   - requests
   - epc
   - numpy
   - urllib3

如果缺少依赖，可以使用以下命令安装:
uv pip install torch sentence-transformers requests epc numpy urllib3
"""
        raise RuntimeError(msg)

class Config:
    """SimTag 配置管理类"""
    
    DEFAULT_MODEL_NAME = "hf.co/unsloth/gemma-3-4b-it-GGUF:latest"
    
    def __init__(self, 
                 vector_file: str = None,
                 db_file: str = None,
                 model_name: str = DEFAULT_MODEL_NAME,
                 debug: bool = False,
                 log_file: str = None,
                 host: str = '127.0.0.1',
                 port: int = 0):
        """初始化配置
        
        Args:
            vector_file: 向量文件路径 (由 org-supertag-sim-epc-vector-file 指定)
            db_file: 数据库文件路径 (由 org-supertag-db-file 指定)
            model_name: Ollama模型名称
            debug: 是否启用调试模式
            log_file: 日志文件路径
            host: 服务器地址
            port: 服务器端口
        """
        # 检查环境
        ensure_environment()
        
        # 直接使用传入的文件路径
        self.vector_file = vector_file
        self.db_file = db_file
        
        # 验证文件路径
        if not self.vector_file:
            raise ValueError("向量文件路径未指定")
        if not self.db_file:
            raise ValueError("数据库文件路径未指定")
            
        # 日志文件路径 - 使用向量文件所在目录
        log_dir = os.path.dirname(self.vector_file)
        self.log_file = log_file or os.path.join(log_dir, "simtag_epc.log")
        
        # 其他设置
        self.model_name = model_name
        self.debug = debug
        self.log_level = logging.DEBUG if debug else logging.INFO
        self.host = host
        self.port = port
        
        # 创建日志目录
        if self.log_file:
            os.makedirs(os.path.dirname(self.log_file), exist_ok=True)
            
        # 记录路径信息
        logging.info(f"配置初始化完成:")
        logging.info(f"向量文件: {self.vector_file}")
        logging.info(f"数据库文件: {self.db_file}")
        logging.info(f"日志文件: {self.log_file}")

    def ensure_ollama(self) -> bool:
        """确保Ollama可用"""
        try:
            # 简单检查ollama命令是否可用
            subprocess.run(["ollama", "--version"], capture_output=True, check=True)
            return True
        except (subprocess.CalledProcessError, FileNotFoundError):
            logging.error("Ollama未安装或不可用")
            return False

    def initialize_server(self) -> bool:
        """初始化服务器环境"""
        try:
            # 确保环境满足要求
            ensure_environment()
            
            # 确保Ollama可用
            if not self.ensure_ollama():
                raise RuntimeError("Ollama未安装或不可用")
            
            # 创建必要的目录
            os.makedirs(os.path.dirname(self.vector_file), exist_ok=True)
            os.makedirs(os.path.dirname(self.log_file), exist_ok=True)
            
            # 设置日志，传递具体的参数而不是self
            setup_logging(
                log_file=self.log_file,
                log_level=self.log_level,
                debug=self.debug
            )
            
            return True
            
        except Exception as e:
            logging.error(f"初始化服务器环境失败: {e}")
            return False

    def setup(self):
        """设置运行环境"""
        # 应用环境变量
        for key, value in self.env_vars.items():
            if key == "PYTHONPATH":
                # 对于PYTHONPATH，我们需要追加而不是覆盖
                current_path = os.environ.get("PYTHONPATH", "")
                if current_path:
                    os.environ["PYTHONPATH"] = f"{value}:{current_path}"
                else:
                    os.environ["PYTHONPATH"] = value
            else:
                os.environ[key] = value
        
        # 创建必要的目录
        os.makedirs(os.path.dirname(self.vector_file), exist_ok=True)
        os.makedirs(os.path.dirname(self.log_file), exist_ok=True)
        
    def to_dict(self) -> Dict[str, Any]:
        """转换配置为字典"""
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
        """保存配置到文件"""
        with open(filepath, 'w') as f:
            json.dump(self.to_dict(), f, indent=2)
            
    @classmethod
    def load(cls, filepath: str) -> 'Config':
        """从文件加载配置"""
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
"""
SimTag 主包
提供标签向量相似度搜索、实体提取和标签生成功能
"""

from .config import Config
from .entity_extractor import EntityExtractor
from .ollama_bridge import OllamaBridge
from .tag_vectors import TagVectorEngine

__version__ = "1.0.0"
__all__ = ['Config', 'EntityExtractor', 'OllamaBridge', 'TagVectorEngine'] 
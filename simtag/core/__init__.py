"""
SimTag core package
提供核心的标签处理和存储功能
"""

from .tagging import TaggingEngine
from .storage import VectorStorage

__all__ = ['TaggingEngine', 'VectorStorage']

# This file makes Python treat the `core` directory as a sub-package of `simtag`. 
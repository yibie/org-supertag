"""
SimTag Utils Package
===================

通用工具包，提供跨组件使用的通用功能
"""

# 导出统一标签处理器相关类
from .unified_tag_processor import (
    UnifiedTagProcessor,
    TagResult,
    NoteResult,
    BatchResult
)

# 导出统一 Prompt 管理
from ..prompts import (
    create_prompt,
    log_prompt_usage,
    DEFAULT_ENTITY_TYPES
)

# 导出其他现有工具
try:
    from .utils import *
except ImportError:
    pass

try:
    from .logging import *
except ImportError:
    pass

try:
    from .serialization import *
except ImportError:
    pass

__all__ = [
    # 统一标签处理器
    'UnifiedTagProcessor',
    'TagResult', 
    'NoteResult',
    'BatchResult',
    
    # 统一 Prompt 管理
    'create_prompt',
    'log_prompt_usage',
    'DEFAULT_ENTITY_TYPES',
] 
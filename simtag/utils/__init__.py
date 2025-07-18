"""
SimTag Utils Package
===================

General utility package providing cross-component functionality
"""

# Export unified tag processor related classes
from .unified_tag_processor import (
    TagResult,
    NoteResult,
    BatchResult
)

# Export unified Prompt management
from ..prompts import (
    DEFAULT_ENTITY_TYPES
)

# Export other existing utilities
try:
    from .utils import *
except ImportError:
    pass

try:
    from .serialization import *
except ImportError:
    pass

__all__ = [
    # Unified tag processor
    'TagResult', 
    'NoteResult',
    'BatchResult',
    
    # Unified Prompt management
    'DEFAULT_ENTITY_TYPES',
] 
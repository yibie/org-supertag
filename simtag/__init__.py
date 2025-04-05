"""
SimTag Main Package
Provides tag vector similarity search, entity extraction, and tag generation functionality
"""

from .config import Config
from .entity_extractor import EntityExtractor
from .ollama_bridge import OllamaBridge
from .tag_vectors import TagVectorEngine

__version__ = "1.0.0"
__all__ = ['Config', 'EntityExtractor', 'OllamaBridge', 'TagVectorEngine'] 
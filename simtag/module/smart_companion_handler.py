import logging
import json
import asyncio
from typing import Dict, Any, List, Optional
from dataclasses import dataclass

from ..utils.unified_tag_processor import normalize_payload

logger = logging.getLogger(__name__)


@dataclass
class TagSuggestion:
    """Data structure for tag suggestions"""
    name: str
    confidence: float
    source_type: str = "llm"  # llm, vector, graph
    reasoning: Optional[str] = None


class SmartCompanionHandler:
    """
    Smart Knowledge Companion Handler
    
    Provides tag suggestions for untagged nodes by delegating to auto-tag infrastructure
    """
    
    def __init__(self, autotag_handler):
        self.autotag_handler = autotag_handler
        self.logger = logging.getLogger(__name__)
        
    async def analyze_tag_context(self, payload: Dict) -> Dict[str, Any]:
        """
        Analyze tag context and return intelligent suggestions.
        
        This method now delegates to the autotag handler for tag suggestions.
        The smart companion frontend should call autotag endpoints directly.
        
        Args:
            payload: Dictionary containing node information for tag suggestions
            
        Returns:
            Dictionary containing tag suggestions results
        """
        try:
            data = normalize_payload(payload)
            self.logger.info("Smart companion delegating to autotag handler for tag suggestions")
            
            # Delegate to autotag handler
            if hasattr(self.autotag_handler, 'generate_tags_for_nodes'):
                result = await self.autotag_handler.generate_tags_for_nodes(payload)
                return result
            else:
                return {
                    "status": "error", 
                    "message": "Autotag handler not available for tag suggestions"
                }
                
        except Exception as e:
            self.logger.error(f"Smart companion analysis failed: {e}", exc_info=True)
            return {"status": "error", "message": str(e)}

    # Additional methods can be added here for future smart companion features
    # For now, we delegate all tag suggestion logic to the autotag handler
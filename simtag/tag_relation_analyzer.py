"""
SimTag tag relation analysis module
Analyzes semantic relationships between tags
"""

import logging
from typing import List, Dict, Any, Optional

# Predefined relation types
RELATIONS = {
    "CONTRAST": "Comparison or contrast relationship",
    "RELATE": "General association relationship",
    "INFLUENCE": "Influence relationship",
    "CONTAIN": "Containment relationship (parent)",
    "BELONG": "Subordinate relationship (child)",
    "PARALLEL": "Parallel relationship",
    "DEPENDENCY": "Dependency relationship",
    "CAUSE": "Causal relationship (cause)",
    "EFFECT": "Causal relationship (effect)",
    "COOCCURRENCE": "Co-occurrence relationship"
}

class TagRelationAnalyzer:
    """Tag relation analyzer"""
    
    def __init__(self, ollama_bridge: Any = None):
        """Initialize tag relation analyzer
        
        Args:
            ollama_bridge: Ollama bridge object for relation analysis
        """
        self.logger = logging.getLogger("simtag.tag_relation")
        self.ollama = ollama_bridge
        
    def analyze_relations(self, tag: str, tags: List[str]) -> List[Dict[str, Any]]:
        """Analyze relationships between tags
        
        Args:
            tag: Target tag
            tags: List of tags to analyze
            
        Returns:
            List of tag relationships, each containing:
                - tag: Related tag
                - relation: Relation type
                - reason: Relation explanation
        """
        if not self.ollama:
            self.logger.error("No Ollama instance provided, cannot analyze tag relations")
            return []
            
        system = """You are a tag relationship analyzer. Your task is to determine the relationship between two tags.

Available relationship types:
CONTRAST   - Tags represent contrasting or comparable concepts
RELATE     - Tags have a general association
INFLUENCE  - First tag has significant impact on second tag
CONTAIN    - First tag is a broader category that includes second tag
BELONG     - First tag is a subset or member of second tag
PARALLEL   - Tags represent similar-level concepts
DEPENDENCY - First tag requires or depends on second tag
CAUSE      - First tag leads to or causes second tag
EFFECT     - First tag is a result of second tag
COOCCURRENCE - Tags commonly appear together

Response format requirements:
1. Use EXACTLY this format (including newline):
   RELATION: <TYPE>
   REASON: <brief explanation>
2. Choose only ONE relationship type from the list above
3. Provide a clear, concise reason (1-2 sentences)
4. Use technical language when appropriate
5. Be specific about the relationship direction

Example response:
RELATION: BELONG
REASON: Python is a specific programming language, making it a subset of programming.

DO NOT include any other text or explanations."""

        results = []
        for related_tag in tags:
            prompt = f"""How is "{related_tag}" related to "{tag}"?

Choose ONE relationship type and explain why.
Use EXACTLY this format (including newline):
RELATION: <TYPE>
REASON: <explanation>"""

            try:
                response = self.ollama.run(prompt, system)
                response = response.strip()
                
                # Validate response format
                if not ('\nREASON:' in response and response.startswith('RELATION:')):
                    self.logger.warning(f"Invalid response format for tag '{related_tag}', retrying...")
                    response = self.ollama.run(prompt, system)
                    if not ('\nREASON:' in response and response.startswith('RELATION:')):
                        self.logger.warning(f"Still invalid format after retry, skipping tag '{related_tag}'")
                        continue
                
                # Parse response
                lines = response.strip().split('\n')
                relation_line = next(line for line in lines if line.startswith('RELATION:'))
                reason_line = next(line for line in lines if line.startswith('REASON:'))
                
                relation = relation_line.split(':', 1)[1].strip().upper()
                reason = reason_line.split(':', 1)[1].strip()
                
                # Validate relation type
                if relation not in RELATIONS:
                    self.logger.warning(f"Invalid relation type '{relation}' for tag '{related_tag}'")
                    continue
                
                results.append({
                    'tag': related_tag,
                    'relation': relation.lower(),
                    'reason': reason
                })
                
            except Exception as e:
                self.logger.error(f"Error parsing response for tag '{related_tag}': {e}")
                self.logger.debug(f"Raw response:\n{response}")
                continue
                
        return results

# Global singleton instance
_analyzer_instance = None

def analyze_tag_relations(tag: str, tags: List[str]) -> List[Dict[str, Any]]:
    """Global function for analyzing tag relations, called by EPC server"""
    global _analyzer_instance
    
    if _analyzer_instance is None:
        _analyzer_instance = TagRelationAnalyzer()
        
    return _analyzer_instance.analyze_relations(tag, tags)
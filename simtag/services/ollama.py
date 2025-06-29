"""
Ollama Service Adapter - Pluggable Component
Provides standardized interface for Ollama API interaction
"""
import requests
import json
import logging

class OllamaService:
    """Ollama Service Abstraction Layer"""
    
    def __init__(self, base_url: str = "http://localhost:11434", model: str = "gemma:3b"):
        self.base_url = base_url
        self.model = model
        self.logger = logging.getLogger("ollama")
        self.available = self.check_availability()
    
    def check_availability(self) -> bool:
        """Check if Ollama service is available"""
        try:
            resp = requests.get(f"{self.base_url}/api/tags", timeout=3)
            return resp.status_code == 200
        except requests.exceptions.RequestException as e:
            self.logger.warning(f"Ollama service unavailable: {str(e)}")
            return False
    
    def generate_tags(self, text: str, limit: int = 5) -> list:
        """
        Generate tags using Ollama
        Return format: ["tag1", "tag2", ...]
        """
        if not self.available:
            self.logger.warning("Ollama service unavailable, skipping tag generation")
            return []
        
        try:
            # TODO: Refactor this prompt to use a centralized prompt management system
            # like the one in `prompts.py` to improve maintainability.
            # Build generation prompt
            prompt = f"""
            Extract {limit} most relevant tags from the following text, return only comma-separated tag list:
            
            {text[:2000]}... [truncated]
            """
            
            # Call API
            payload = {
                "model": self.model,
                "prompt": prompt,
                "options": {"temperature": 0.3, "num_predict": limit*2}
            }
            resp = requests.post(
                f"{self.base_url}/api/generate", 
                json=payload,
                timeout=30
            )
            resp.raise_for_status()
            
            # Parse response
            response_data = resp.json()
            raw_tags = response_data.get("response", "").strip()
            
            # Process tag format
            tags = [tag.strip() for tag in raw_tags.split(",") if tag.strip()]
            return tags[:limit]
            
        except Exception as e:
            self.logger.error(f"Tag generation failed: {str(e)}")
            return []
    
    def extract_entities(self, text: str) -> list:
        """
        Entity recognition
        Return format: [{"entity": "entity", "type": "type", "start": position, "end": position}]
        """
        if not self.available:
            return []
        
        try:
            # TODO: Refactor this prompt to use a centralized prompt management system
            # like the one in `prompts.py` to improve maintainability.
            # Entity recognition system prompt
            system_prompt = """
            You are an entity recognition expert, please identify the following entity types in the text:
            PERSON - Person names
            ORG - Organizations/Institutions
            LOCATION - Locations
            TECH - Technical terms
            Return format must be a valid JSON array.
            """
            
            # Call API
            payload = {
                "model": self.model,
                "system": system_prompt,
                "prompt": text[:3000],
                "format": "json",
                "options": {"temperature": 0.2}
            }
            resp = requests.post(
                f"{self.base_url}/api/generate", 
                json=payload,
                timeout=180  # 增加到3分钟
            )
            resp.raise_for_status()
            
            # Parse JSON response
            return json.loads(resp.json().get("response", "[]"))
            
        except Exception as e:
            self.logger.error(f"Entity recognition failed: {str(e)}")
            return []
    
    def analyze_relations(self, tag: str, related_tags: list) -> list:
        """
        Analyze tag relationships
        Return format: [{"tag": "related_tag", "relation": "relation_type", "reason": "reason"}]
        """
        if not self.available or not related_tags:
            return []
        
        try:
            # TODO: Refactor this prompt to use a centralized prompt management system
            # like the one in `prompts.py` to improve maintainability.
            # Relationship analysis prompt
            prompt = f"""
            Analyze the relationship between "{tag}" and the following tags:
            {", ".join(related_tags)}
            
            Relation types: contains, related, similar, opposite
            Return format must be a valid JSON array.
            """
            
            # Call API
            payload = {
                "model": self.model,
                "prompt": prompt,
                "format": "json",
                "options": {"temperature": 0.4}
            }
            resp = requests.post(
                f"{self.base_url}/api/generate", 
                json=payload,
                timeout=30
            )
            resp.raise_for_status()
            
            # Parse JSON response
            return json.loads(resp.json().get("response", "[]"))
            
        except Exception as e:
            self.logger.error(f"Relationship analysis failed: {str(e)}")
            return []

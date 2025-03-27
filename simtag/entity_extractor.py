"""
SimTag 实体提取模块
提供文本中实体的识别和分类功能
"""

import logging
import json
import traceback
from typing import List, Dict, Any, Optional

# 全局单例实例
_extractor_instance = None

def extract_entities(text: str) -> List[Dict[str, Any]]:
    """从文本中提取实体的全局函数，供EPC服务器调用
    
    Args:
        text: 要分析的文本
        
    Returns:
        实体列表，每个实体包含:
            - entity: 实体文本
            - type: 实体类型
            - start: 开始位置
            - end: 结束位置
    """
    global _extractor_instance
    
    if _extractor_instance is None:
        _extractor_instance = EntityExtractor()
        
    return _extractor_instance.extract(text)

class EntityExtractor:
    """实体提取器类"""
    
    def __init__(self, ollama_bridge: Any = None):
        """初始化实体提取器
        
        Args:
            ollama_bridge: Ollama桥接对象，用于实体提取
        """
        self.logger = logging.getLogger("simtag.entity_extractor")
        self.ollama = ollama_bridge
        
    def extract(self, text: str) -> List[Dict[str, Any]]:
        """从文本中提取实体
        
        Args:
            text: 待分析文本
            
        Returns:
            实体列表，每个实体包含:
                - entity: 实体文本
                - type: 实体类型
                - start: 开始位置
                - end: 结束位置
        """
        if not self.ollama:
            self.logger.error("未提供Ollama实例，无法提取实体")
            return []
            
        system = """You are an expert in Named Entity Recognition (NER). Your task is to identify and classify named entities in the given text. Focus on these entity types:

1. PERSON - Names of people
2. ORG - Organizations, companies, institutions
3. PRODUCT - Products, software, technologies
4. CONCEPT - Technical concepts, methodologies
5. TECH - Programming languages, frameworks, tools

For each entity found:
1. Extract the exact text as it appears
2. Classify its type from the above categories
3. Find the start and end position in the text

Return your result as a valid JSON array of entity objects:
[
  {"entity": "entity_text", "type": "ENTITY_TYPE", "start": start_pos, "end": end_pos},
  ...
]

The start and end positions should be character indices where the entity appears in the text."""

        prompt = f"""Extract all named entities from this text:

{text}

Return ONLY a valid JSON array of entities with no comments or explanations."""

        try:
            response = self.ollama.run(prompt)
            entities = json.loads(response)
            
            # 验证和清理实体
            valid_entities = []
            for entity in entities:
                if isinstance(entity, dict) and 'entity' in entity and 'type' in entity:
                    valid_entities.append(entity)
                    
            return valid_entities
            
        except Exception as e:
            self.logger.error(f"提取实体过程出错: {e}")
            self.logger.error(traceback.format_exc())
            return []

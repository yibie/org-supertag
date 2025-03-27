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
_ollama_instance = None

def extract_entities(text: str, ollama_bridge: Any = None) -> List[Dict[str, Any]]:
    """从文本中提取实体的全局函数，供EPC服务器调用
    
    Args:
        text: 待分析文本
        ollama_bridge: 可选参数，Ollama桥接对象
        
    Returns:
        实体列表
    """
    global _extractor_instance, _ollama_instance
    
    # 如果提供了新的ollama_bridge，更新全局实例
    if ollama_bridge and _ollama_instance != ollama_bridge:
        _ollama_instance = ollama_bridge
        _extractor_instance = EntityExtractor(ollama_bridge)
    # 如果没有实例但有全局ollama实例，用全局ollama创建
    elif _extractor_instance is None and _ollama_instance:
        _extractor_instance = EntityExtractor(_ollama_instance)
    # 如果都没有，创建一个没有ollama的实例（将在extract中报错）
    elif _extractor_instance is None:
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
                - text: 实体文本
                - type: 实体类型
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

Return your result as a valid JSON array of entity objects:
[
  {"text": "entity_text", "type": "ENTITY_TYPE"},
  ...
]

Make sure to always include the text and type fields for each entity."""

        prompt = f"""Extract all named entities from this text:

{text}

Return ONLY a valid JSON array of entities with no comments or explanations.
Remember to use the format: [{{"text": "...", "type": "..."}}]"""

        try:
            response = self.ollama.run(prompt, system)
            self.logger.info(f"原始响应: {response[:200]}...")
            
            # 尝试解析JSON响应
            try:
                entities = json.loads(response)
                
                # 验证和清理实体
                valid_entities = []
                for entity in entities:
                    # 检查旧格式并转换为新格式
                    if isinstance(entity, dict):
                        new_entity = {}
                        
                        # 检查并转换entity字段为text
                        if 'entity' in entity and 'text' not in entity:
                            new_entity['text'] = entity['entity']
                        elif 'text' in entity:
                            new_entity['text'] = entity['text']
                        else:
                            continue  # 缺少必要字段
                            
                        # 复制type字段
                        if 'type' in entity:
                            new_entity['type'] = entity['type']
                        else:
                            continue  # 缺少必要字段
                            
                        valid_entities.append(new_entity)
                
                self.logger.info(f"提取到 {len(valid_entities)} 个有效实体")
                return valid_entities
                
            except json.JSONDecodeError as e:
                self.logger.error(f"JSON解析错误: {e}")
                self.logger.error(f"响应内容: {response}")
                # 尝试自己构建一个基本实体
                return [{"text": text.split()[0] if text.split() else text, 
                         "type": "CONCEPT"}]
            
        except Exception as e:
            self.logger.error(f"提取实体过程出错: {e}")
            self.logger.error(traceback.format_exc())
            return []

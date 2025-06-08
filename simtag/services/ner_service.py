# simtag/services/ner_service.py

import logging
from typing import List, Dict, Any, Optional

# Assuming LLMClient has an async generate method
from simtag.services.llm_client import LLMClient
import json
import time

logger = logging.getLogger(__name__)

class NERService:
    """
    A service dedicated to NER-based features like tag suggestion
    and relationship discovery. Supports batch processing for improved performance.
    """
    def __init__(self, llm_client: LLMClient, config=None):
        self.llm_client = llm_client
        self.config = config
        if not self.llm_client:
            logger.error("NERService initialized without a valid LLMClient.")
            raise ValueError("LLMClient is required for NERService.")
    
    def _clean_json_response(self, response_str: str) -> str:
        """
        清理LLM响应，移除markdown代码块格式
        
        Args:
            response_str: 原始LLM响应字符串
            
        Returns:
            清理后的JSON字符串
        """
        # 移除markdown代码块标记
        cleaned = response_str.strip()
        
        # 移除 ```json 开始标记
        if cleaned.startswith('```json'):
            cleaned = cleaned[7:]  # 移除 '```json'
        elif cleaned.startswith('```'):
            cleaned = cleaned[3:]  # 移除 '```'
            
        # 移除结尾的 ``` 标记
        if cleaned.endswith('```'):
            cleaned = cleaned[:-3]
            
        # 清理多余的空白字符
        cleaned = cleaned.strip()
        
        return cleaned

    async def suggest_tags_for_note(
        self, note_content: str, existing_tags: Optional[List[str]] = None
    ) -> List[Dict[str, Any]]:
        """
        Suggests new tags for a given note content based on NER.

        Args:
            note_content: The text content of the note.
            existing_tags: A list of tags already applied to the note.

        Returns:
            A list of suggested tags, where each tag is a dictionary
            with keys like 'tag_name', 'confidence', and 'reasoning'.
        """
        if not note_content:
            return []
        
        existing_tags_str = ", ".join(f"'{t}'" for t in existing_tags) if existing_tags else "None"

        prompt = f"""
Analyze the following text from a user's note. Identify key concepts, topics, tools, or names that would make good tags for categorization and retrieval.

The user's note content:
---
{note_content[:4000]}
---

The note already has the following tags: [{existing_tags_str}].

Your task is to suggest NEW tags. Do not suggest any tags that are already in the existing list.
Respond with a JSON array of objects. Each object must have three keys:
1. "tag_name": The suggested tag (string).
2. "confidence": A score from 0.0 to 1.0 indicating your confidence that this is a good tag.
3. "reasoning": A brief explanation (string) of why you are suggesting this tag based on the text.

Example response format:
[
  {{"tag_name": "performance_tuning", "confidence": 0.9, "reasoning": "The note discusses optimizing code execution speed."}},
  {{"tag_name": "elisp", "confidence": 1.0, "reasoning": "The note explicitly mentions Emacs Lisp code."}}
]

Generate the JSON response now.
"""
        
        try:
            response_str = await self.llm_client.generate(prompt)
            
            # 清理LLM响应，移除markdown代码块格式
            cleaned_response = self._clean_json_response(response_str)
            suggested_tags = json.loads(cleaned_response)
            
            # Basic validation
            if not isinstance(suggested_tags, list):
                logger.warning(f"LLM returned non-list for tag suggestions: {suggested_tags}")
                return []
            
            # Further validation can be added here (e.g., check for required keys)
            return suggested_tags

        except json.JSONDecodeError:
            logger.error(f"Failed to decode JSON from LLM response for tag suggestion: {response_str}")
            return []
        except Exception as e:
            logger.error(f"An unexpected error occurred in suggest_tags_for_note: {e}", exc_info=True)
            return []

    async def discover_tag_relationships(
        self, note_content: str
    ) -> List[Dict[str, Any]]:
        """
        Discovers potential new relationships between tags (entities) from note content.

        Args:
            note_content: The text content of the note.

        Returns:
            A list of discovered relationships, where each is a dictionary like
            {'from_tag': 'elisp', 'to_tag': 'performance', 'relation_type': 'concerns', 'confidence': 0.8, 'sentence': '...'}.
        """
        if not note_content:
            return []

        prompt = f"""
Analyze the following text from a user's note. Your goal is to identify and extract meaningful relationships between concepts, which can be thought of as tags.

The user's note content:
---
{note_content[:4000]}
---

Your task is to identify pairs of concepts and describe the relationship between them.
Respond with a JSON array of objects. Each object represents a single directed relationship and must have five keys:
1. "from_tag": The source concept/tag (string).
2. "to_tag": The target concept/tag (string).
3. "relation_type": A brief, verb-based description of the relationship (e.g., "uses", "improves", "is_part_of", "is_antonym_of").
4. "confidence": A score from 0.0 to 1.0 indicating your confidence in this relationship.
5. "sentence": The exact sentence from the text that provides evidence for this relationship.

Example response format:
[
  {{
    "from_tag": "lexical-binding",
    "to_tag": "performance",
    "relation_type": "improves",
    "confidence": 0.85,
    "sentence": "Enabling lexical-binding is a key step for improving performance in modern Emacs Lisp."
  }}
]

Generate the JSON response now.
"""
        try:
            response_str = await self.llm_client.generate(prompt)
            
            # 清理LLM响应，移除markdown代码块格式
            cleaned_response = self._clean_json_response(response_str)
            discovered_relations = json.loads(cleaned_response)
            
            if not isinstance(discovered_relations, list):
                logger.warning(f"LLM returned non-list for relationship discovery: {discovered_relations}")
                return []
                
            return discovered_relations
            
        except json.JSONDecodeError:
            logger.error(f"Failed to decode JSON from LLM response for relationship discovery: {response_str}")
            return []
        except Exception as e:
            logger.error(f"An unexpected error occurred in discover_tag_relationships: {e}", exc_info=True)
            return []

    async def suggest_tags_batch(self, 
                               note_contents: List[str], 
                               existing_tags_list: Optional[List[List[str]]] = None) -> List[List[Dict[str, Any]]]:
        """
        真正的多进程并行标签建议 - 使用独立进程处理每个笔记
        
        Args:
            note_contents: 笔记内容列表
            existing_tags_list: 每个笔记对应的现有标签列表
            
        Returns:
            每个笔记对应的标签建议列表
        """
        if not note_contents:
            return []
        
        # 如果没有提供现有标签，创建空列表
        if existing_tags_list is None:
            existing_tags_list = [[] for _ in note_contents]
        
        logger.info(f"🚀 开始真正的多进程并行处理 {len(note_contents)} 个笔记的标签建议")
        
        # 使用新的TrueMultiCoreManager
        from ..multicore_manager import TrueMultiCoreManager
        
        # 获取LLM配置 - 使用Entity Extractor配置中的专用模型
        if self.config and hasattr(self.config, 'entity_extractor_config'):
            ner_model = self.config.entity_extractor_config.get('llm_model_override', 'qwen2.5:1.5b')
        else:
            # 从配置中获取实体识别专用模型
            from ..config import Config
            config = Config()
            ner_model = config.entity_extractor_config.get('llm_model_override', 'qwen2.5:1.5b')
        
        llm_config = {
            "base_url": getattr(self.llm_client, 'base_url', 'http://localhost:11434'),
            "model": ner_model,  # 使用专门的NER模型
            "timeout": getattr(self.llm_client, 'timeout', 120)
        }
        
        logger.info(f"🔧 Using NER model: {ner_model}")
        
        # 使用多核心管理器进行真正的并行处理
        multicore_manager = TrueMultiCoreManager({
            "enabled": True,
            "max_workers": None,  # 自动检测
            "timeout": 300
        })
        
        try:
            with multicore_manager:
                results = await multicore_manager.process_ner_parallel(
                    note_contents=note_contents,
                    existing_tags_list=existing_tags_list,
                    llm_config=llm_config
                )
                
                logger.info(f"✅ 多进程并行标签建议处理完成: {len(results)} 个结果")
                return results
                
        except Exception as e:
            logger.error(f"❌ 多进程并行处理失败: {e}")
            raise  # 直接抛出异常，不再降级处理

    async def discover_relationships_batch(self, 
                                         note_contents: List[str]) -> List[List[Dict[str, Any]]]:
        """
        真正的批量发现笔记中的关系 - 使用单个LLM调用处理所有笔记
        
        Args:
            note_contents: 笔记内容列表
            
        Returns:
            每个笔记对应的关系发现列表
        """
        if not note_contents:
            return []
            
        # 构建批量处理的提示词
        batch_entries = []
        for i, content in enumerate(note_contents):
            batch_entries.append(f"""
Note {i+1}:
Content: {content[:2000]}
""")
        
        batch_prompt = f"""
You are analyzing {len(note_contents)} notes for relationship discovery. For each note, identify meaningful relationships between concepts, which can be thought of as tags.

{chr(10).join(batch_entries)}

Your task is to identify pairs of concepts and describe the relationship between them in each note.
Respond with a JSON array where each element corresponds to a note (in order). Each element should be an array of objects representing relationships, with five keys:
1. "from_tag": The source concept/tag (string).
2. "to_tag": The target concept/tag (string).
3. "relation_type": A brief, verb-based description of the relationship (e.g., "uses", "improves", "is_part_of").
4. "confidence": A score from 0.0 to 1.0 indicating your confidence in this relationship.
5. "sentence": The exact sentence from the text that provides evidence for this relationship.

Example response format for 2 notes:
[
  [
    {{
      "from_tag": "lexical-binding",
      "to_tag": "performance",
      "relation_type": "improves",
      "confidence": 0.85,
      "sentence": "Enabling lexical-binding is a key step for improving performance in modern Emacs Lisp."
    }}
  ],
  [
    {{
      "from_tag": "database",
      "to_tag": "application",
      "relation_type": "supports",
      "confidence": 0.9,
      "sentence": "The database supports the application by storing user data."
    }}
  ]
]

Generate the JSON response now.
"""
        
        try:
            logger.info(f"🚀 真正批量处理 {len(note_contents)} 个笔记的关系发现")
            response_str = await self.llm_client.generate(batch_prompt)
            
            # 清理LLM响应，移除markdown代码块格式
            cleaned_response = self._clean_json_response(response_str)
            batch_results = json.loads(cleaned_response)
            
            # 验证结果格式
            if not isinstance(batch_results, list):
                logger.warning(f"LLM返回非列表格式的批量关系结果: {batch_results}")
                # 降级到单独处理
                return await self._fallback_to_individual_relationship_processing(note_contents)
            
            if len(batch_results) != len(note_contents):
                logger.warning(f"批量关系结果数量不匹配: 期望 {len(note_contents)}, 实际 {len(batch_results)}")
                # 调整长度
                if len(batch_results) < len(note_contents):
                    batch_results.extend([[]] * (len(note_contents) - len(batch_results)))
                else:
                    batch_results = batch_results[:len(note_contents)]
            
            logger.info(f"✅ 批量关系发现处理成功: {len(batch_results)} 个结果")
            return batch_results
            
        except json.JSONDecodeError as e:
            logger.error(f"批量关系发现JSON解析失败: {e}")
            logger.error(f"原始响应: {response_str[:500]}...")
            # 降级到单独处理
            return await self._fallback_to_individual_relationship_processing(note_contents)
        except Exception as e:
            logger.error(f"批量关系发现处理出错: {e}", exc_info=True)
            # 降级到单独处理
            return await self._fallback_to_individual_relationship_processing(note_contents)
    
    async def _fallback_to_individual_relationship_processing(self, 
                                                            note_contents: List[str]) -> List[List[Dict[str, Any]]]:
        """
        关系发现降级到单独处理模式
        """
        logger.info("🔄 关系发现降级到并发单独处理模式")
        
        # 并行处理所有笔记
        import asyncio
        tasks = [
            self.discover_tag_relationships(content)
            for content in note_contents
        ]
        
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # 处理异常结果
        final_results = []
        for i, result in enumerate(results):
            if isinstance(result, Exception):
                logger.error(f"Error processing note {i}: {result}")
                final_results.append([])
            else:
                final_results.append(result)
                
        return final_results 

 
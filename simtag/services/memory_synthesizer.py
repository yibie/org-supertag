#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
from typing import List, Dict, Any, Optional
import asyncio
import dataclasses
import uuid
import json

from simtag.config import Config
from simtag.core.memory_engine import MemoryEngine, MemoryItem, MemoryItemType
from simtag.services.llm_client import LLMClient

logger = logging.getLogger(__name__)

@dataclasses.dataclass
class CandidateMemory:
    """Represents a potential memory item synthesized from user interactions, awaiting user approval."""
    candidate_id: str
    proposed_item: MemoryItem
    justification: str  # Explanation from the LLM on why this memory is proposed
    confidence_score: float  # Value between 0.0 and 1.0
    source_data: Dict[str, Any]  # The data used to generate this candidate

class MemorySynthesizer:
    """
    Analyzes user interactions and dialogue history to synthesize new,
    long-term memories for the system. Includes vectorization capabilities.
    """
    def __init__(self, config: Config, memory_engine: MemoryEngine, llm_client: LLMClient):
        self.config = config
        self.memory_engine = memory_engine
        self.llm_client = llm_client
        logger.info("MemorySynthesizer initialized.")
        # In-memory storage for candidates before they are approved/rejected.
        # In a larger system, this might be a database table.
        self.candidate_store: Dict[str, CandidateMemory] = {}
        self.synthesis_prompt_template = """
You are a meticulous knowledge analyst reviewing a conversation between a user and an AI assistant.
Your task is to identify and extract key pieces of information that should be saved as long-term memories for the AI.
Focus on durable facts, user preferences, stated goals, or important conclusions.

Analyze the following dialogue:
---
{dialogue_text}
---

Based on the dialogue, extract potential memories. For each potential memory, provide:
1. `type`: The type of memory. Must be one of: {valid_types}.
2. `content`: The specific information to be remembered. For a preference, this should be a dictionary. For a fact, a string.
3. `justification`: A brief explanation of why this piece of information is important to remember.
4. `confidence_score`: A float between 0.0 and 1.0, indicating your certainty that this is a valuable, lasting memory.

Respond with a JSON object containing a single key "candidates", which is a list of these memory objects.
If no valuable memories are found, return a JSON object with an empty "candidates" list.
Example for a user preference: {"type": "USER_PREFERENCE", "content": {{"key": "preferred_language", "value": "Python"}}, "justification": "User explicitly stated they prefer Python for new projects.", "confidence_score": 0.95}}
Example for a fact: {"type": "FACT", "content": "The project 'X' uses a microservices architecture.", "justification": "This is a key architectural detail mentioned by the user.", "confidence_score": 0.9}

Your JSON response:
"""

    async def synthesize_from_dialogue(self, session_id: str) -> List[CandidateMemory]:
        """
        Analyzes a given dialogue session and proposes new memories.
        This is the core logic for turning conversations into knowledge.
        """
        logger.info(f"Starting memory synthesis for dialogue session: {session_id}")

        history = await self.memory_engine.get_dialogue_history(session_id)
        if not history or not history.turns:
            logger.info(f"No dialogue history found for session {session_id}, skipping synthesis.")
            return []

        dialogue_text = "\n".join([f"{turn.speaker.upper()}: {turn.text}" for turn in history.turns])
        
        valid_types = [MemoryItemType.FACT.value, MemoryItemType.USER_PREFERENCE.value]
        
        prompt = self.synthesis_prompt_template.format(
            dialogue_text=dialogue_text,
            valid_types=valid_types
        )

        try:
            response_str = await self.llm_client.generate(prompt)
            logger.debug(f"LLM response for memory synthesis: {response_str}")
            
            response_data = json.loads(response_str)
            extracted_candidates = response_data.get("candidates", [])

            if not extracted_candidates:
                logger.info("LLM analysis found no new memory candidates.")
                return []

            new_candidates = []
            for cand_data in extracted_candidates:
                try:
                    mem_type_str = cand_data.get('type')
                    mem_type = MemoryItemType(mem_type_str)

                    # Create the MemoryItem dataclass
                    item_id = f"{mem_type.value.lower()}_{uuid.uuid4().hex[:8]}"
                    proposed_item = MemoryItem(
                        id=item_id,
                        type=mem_type,
                        content=cand_data.get('content'),
                        metadata={'source': 'synthesis', 'session_id': session_id}
                    )
                    
                    # Create the CandidateMemory dataclass
                    candidate_id = f"cand_{uuid.uuid4().hex[:12]}"
                    candidate = CandidateMemory(
                        candidate_id=candidate_id,
                        proposed_item=proposed_item,
                        justification=cand_data.get('justification', 'No justification provided.'),
                        confidence_score=float(cand_data.get('confidence_score', 0.5)),
                        source_data={'session_id': session_id, 'dialogue': dialogue_text}
                    )
                    new_candidates.append(candidate)
                    self.candidate_store[candidate_id] = candidate
                    logger.info(f"Successfully created and stored candidate {candidate_id} of type {mem_type_str}.")

                except (ValueError, TypeError, KeyError) as e:
                    logger.error(f"Failed to parse a candidate from LLM response. Data: {cand_data}. Error: {e}")
                    continue # Skip this malformed candidate

            logger.info(f"Memory synthesis complete. Generated {len(new_candidates)} new candidates.")
            return new_candidates

        except json.JSONDecodeError:
            logger.error(f"Failed to decode JSON from LLM synthesis response: {response_str}")
            return []
        except Exception as e:
            logger.error(f"An unexpected error occurred during synthesis: {e}", exc_info=True)
            return []

    def get_pending_candidates(self) -> List[CandidateMemory]:
        """Returns a list of all memory candidates awaiting review."""
        return list(self.candidate_store.values())

    def get_candidate_by_id(self, candidate_id: str) -> Optional[CandidateMemory]:
        """Retrieves a specific candidate by its ID."""
        return self.candidate_store.get(candidate_id)

    def discard_candidate(self, candidate_id: str) -> bool:
        """Removes a candidate from the store (user rejected)."""
        if candidate_id in self.candidate_store:
            del self.candidate_store[candidate_id]
            logger.info(f"Discarded candidate memory: {candidate_id}")
            return True
        return False

    async def vectorize_memories(self, memory_items: List[MemoryItem]) -> Dict[str, List[float]]:
        """
        为记忆项生成向量化表示，支持批量处理
        
        Args:
            memory_items: 记忆项列表
            
        Returns:
            记忆ID到向量的映射
        """
        if not memory_items:
            return {}
            
        # 提取文本内容用于向量化
        texts_to_vectorize = []
        memory_ids = []
        
        for item in memory_items:
            # 根据记忆类型提取合适的文本
            if item.type == MemoryItemType.FACT:
                text = str(item.content)
            elif item.type == MemoryItemType.USER_PREFERENCE:
                # 对于用户偏好，组合key和value
                if isinstance(item.content, dict):
                    key = item.content.get('key', '')
                    value = item.content.get('value', '')
                    text = f"{key}: {value}"
                else:
                    text = str(item.content)
            else:
                text = str(item.content)
            
            texts_to_vectorize.append(text)
            memory_ids.append(item.id)
        
        try:
            # 使用LLM客户端的批量嵌入功能
            logger.info(f"Vectorizing {len(texts_to_vectorize)} memory items")
            embeddings = await self.llm_client.get_embeddings_batch(
                texts_to_vectorize,
                use_multicore=True
            )
            
            # 构建ID到向量的映射
            memory_vectors = {}
            for memory_id, embedding in zip(memory_ids, embeddings):
                if embedding:  # 确保嵌入向量不为空
                    memory_vectors[memory_id] = embedding
                else:
                    logger.warning(f"Empty embedding for memory {memory_id}")
            
            logger.info(f"Successfully vectorized {len(memory_vectors)} memory items")
            return memory_vectors
            
        except Exception as e:
            logger.error(f"Error during memory vectorization: {e}", exc_info=True)
            return {}

    async def find_similar_memories(self, 
                                  query_text: str, 
                                  stored_memory_vectors: Dict[str, List[float]],
                                  top_k: int = 5) -> List[Dict[str, Any]]:
        """
        基于向量相似性查找相似的记忆
        
        Args:
            query_text: 查询文本
            stored_memory_vectors: 已存储的记忆向量
            top_k: 返回最相似的k个记忆
            
        Returns:
            相似记忆列表，包含ID和相似度分数
        """
        if not query_text or not stored_memory_vectors:
            return []
        
        try:
            # 为查询文本生成向量
            query_embedding = await self.llm_client.get_embedding(query_text)
            if not query_embedding:
                logger.warning("Failed to generate embedding for query text")
                return []
            
            # 计算相似度
            similarities = []
            for memory_id, memory_vector in stored_memory_vectors.items():
                # 简单的余弦相似度计算
                similarity = self._cosine_similarity(query_embedding, memory_vector)
                similarities.append({
                    'memory_id': memory_id,
                    'similarity': similarity
                })
            
            # 按相似度排序并返回top_k
            similarities.sort(key=lambda x: x['similarity'], reverse=True)
            return similarities[:top_k]
            
        except Exception as e:
            logger.error(f"Error during similarity search: {e}", exc_info=True)
            return []

    def _cosine_similarity(self, vec1: List[float], vec2: List[float]) -> float:
        """计算两个向量的余弦相似度"""
        try:
            import math
            
            # 计算点积
            dot_product = sum(a * b for a, b in zip(vec1, vec2))
            
            # 计算向量长度
            norm1 = math.sqrt(sum(a * a for a in vec1))
            norm2 = math.sqrt(sum(a * a for a in vec2))
            
            # 避免除零
            if norm1 == 0 or norm2 == 0:
                return 0.0
                
            return dot_product / (norm1 * norm2)
            
        except Exception as e:
            logger.error(f"Error calculating cosine similarity: {e}")
            return 0.0 
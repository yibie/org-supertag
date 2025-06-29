#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
import json
import asyncio
from typing import Dict, Any, List

from simtag.core.graph_service import GraphService
from simtag.core.entity_extractor import LLMEntityExtractor
from simtag.services.embedding_service import EmbeddingService # Added import
from simtag.core.graph_service import NodeType, RelationType # Added import
from simtag.prompts import ENTITY_MERGING_PROMPT, RELATIONSHIP_EXTRACTION_PROMPT # Added import
from simtag.core.rag_engine import OrgSupertagRAGEngine

logger = logging.getLogger(__name__)

class ReasoningHandler:
    """
    Handles asynchronous, post-sync reasoning tasks like relation inference.
    This service is designed to be called periodically to enrich nodes that
    have already been embedded.
    """
    def __init__(self, config, graph_service: GraphService, entity_extractor: LLMEntityExtractor, embedding_service: EmbeddingService, rag_engine: OrgSupertagRAGEngine):
        self.config = config
        self.graph_service = graph_service
        self.entity_extractor = entity_extractor
        self.embedding_service = embedding_service
        self.rag_engine = rag_engine
        logger.info("ReasoningHandler initialized.")

    async def infer_relations_for_node(self, node_id: str) -> bool:
        """
        Infers and stores relations for a single node.
        
        This method fetches the node's content, uses the LLM to extract entities
        and relations, and then persists them to the graph database.
        """
        logger.debug(f"Starting relation inference for node {node_id}.")
        try:
            node_data = self.graph_service.get_node_by_id(node_id)
            if not node_data:
                logger.warning(f"Could not find node {node_id} for relation inference.")
                return False

            content = node_data.get('content', '')
            title = node_data.get('title', '')
            
            if not content.strip() and not title.strip():
                logger.debug(f"Node {node_id} has no content or title, skipping relation inference.")
                self.graph_service.mark_node_relations_inferred(node_id)
                return True

            # Combine title and content for a richer query
            query_text = f"{title}\n\n{content}".strip()

            # 1. 上下文检索：获取语义相似的节点和现有相关实体
            # 获取查询文本的嵌入
            query_embedding_result = await self.embedding_service.get_embedding(query_text)
            if not query_embedding_result.success or not query_embedding_result.embedding:
                logger.error(f"Failed to get query embedding for node {node_id}.")
                return False
            query_embedding = query_embedding_result.embedding

            # 使用 RAG 引擎的 retrieve_context 来获取结构化上下文
            # 这里我们使用 'mini' 策略来获取实体、关系和文档
            retrieved_context = await self.rag_engine.retrieve_context(
                query_text=query_text,
                query_embedding=query_embedding,
                strategy="mini",
                top_k=self.config.get('reasoning_retrieval_top_k', 5),
                max_reasoning_depth=self.config.get('reasoning_max_depth', 2)
            )
            
            # 提取上下文中的实体和关系，用于 LLM 提示
            context_entities = retrieved_context.get('entities', [])
            context_relations = retrieved_context.get('relations', [])
            context_documents = retrieved_context.get('documents', [])

            # 格式化上下文，以便传递给 LLM
            formatted_context = self._format_context_for_llm(context_entities, context_relations, context_documents)
            
            existing_tags = node_data.get('tags', []) # Existing tags from the node itself
            
            # 2. 调用 LLMEntityExtractor 执行各种“原子知识增益”任务
            # 2.1 实体和关系抽取 (基础)
            extraction_result = await self.entity_extractor.extract(
                text=query_text,
                existing_entities=existing_tags,
                task_type="extract_entities_relations",
                context_text=formatted_context # Pass formatted context to LLM
            )
            
            inferred_entities = extraction_result.get("entities", [])
            inferred_relations = extraction_result.get("relations", [])

            # 2.2 实体合并建议 (如果配置启用)
            if self.config.get('enable_entity_merging', False) and inferred_entities:
                merge_prompt_context = f"Node content: {query_text}\n\nContext: {formatted_context}"
                merge_suggestions_raw = await self.entity_extractor.extract(
                    text=merge_prompt_context,
                    task_type="entity_merging",
                    entities_to_consider=inferred_entities # Pass inferred entities for merging
                )
                merge_suggestions = merge_suggestions_raw.get("merge_suggestions", [])
                await self._process_merge_suggestions(merge_suggestions)

            # 2.3 关系发现 (如果配置启用)
            if self.config.get('enable_relation_discovery', False) and inferred_entities:
                relation_prompt_context = f"Node content: {query_text}\n\nContext: {formatted_context}"
                discovered_relations_raw = await self.entity_extractor.extract(
                    text=relation_prompt_context,
                    task_type="relationship_extraction",
                    entities_to_consider=inferred_entities # Pass inferred entities for relation discovery
                )
                discovered_relations = discovered_relations_raw.get("relations", [])
                inferred_relations.extend(discovered_relations)

            # 3. 根据 LLM 的判断，调用 GraphService 的方法来更新知识图谱
            # 3.1 更新实体
            if inferred_entities:
                entities_to_upsert = []
                for entity in inferred_entities:
                    entities_to_upsert.append({
                        'name': entity.name,
                        'type': entity.type, # Use the new NodeType enum
                        'properties': entity.attributes, # Store attributes in properties
                        'confidence': entity.confidence,
                        'reasoning': entity.reasoning
                    })
                self.graph_service.bulk_upsert_entity_nodes(entities_to_upsert)
                logger.debug(f"Upserted {len(entities_to_upsert)} entities for node {node_id}")

            # 3.2 更新关系
            if inferred_relations:
                relations_to_upsert = []
                for rel in inferred_relations:
                    source_id = self.graph_service._get_entity_id_by_name(rel.source)
                    target_id = self.graph_service._get_entity_id_by_name(rel.target)
                    if source_id and target_id:
                        relations_to_upsert.append({
                            'relation_id': f"{source_id}-{rel.type.value}-{target_id}", # Generate a unique ID
                            'source_id': source_id,
                            'target_id': target_id,
                            'type': rel.type, # Use the new RelationType enum
                            'weight': rel.confidence,
                            'properties': {
                                'description': rel.description,
                                'reasoning': rel.reasoning,
                                'confidence': rel.confidence,
                                'source_node_id': node_id
                            }
                        })
                
                if relations_to_upsert:
                    self.graph_service.bulk_upsert_relations(relations_to_upsert)
                    logger.debug(f"Upserted {len(relations_to_upsert)} relations for node {node_id}")

            # Mark the node as processed
            self.graph_service.mark_node_relations_inferred(node_id)
            logger.info(f"Successfully completed relation inference for node {node_id}.")
            return True

        except Exception as e:
            logger.error(f"Failed to infer relations for node {node_id}: {e}", exc_info=True)
            return False

    async def _process_merge_suggestions(self, merge_suggestions: List[Dict[str, Any]]):
        """Processes LLM-generated entity merge suggestions."""
        for suggestion in merge_suggestions:
            primary_name = suggestion.get('primary_name')
            aliases = suggestion.get('aliases', [])
            confidence = suggestion.get('confidence', 0.0)
            reasoning = suggestion.get('reasoning', '')

            if not primary_name or not aliases:
                logger.warning(f"Skipping malformed merge suggestion: {suggestion}")
                continue

            primary_id = self.graph_service._get_entity_id_by_name(primary_name)
            if not primary_id:
                # If primary entity doesn't exist, create it as a CONCEPT or ENTITY
                primary_id, _ = self.graph_service._prepare_entity_node(primary_name, NodeType.CONCEPT)
                self.graph_service._upsert_nodes_internal([{
                    'node_id': primary_id,
                    'type': NodeType.CONCEPT,
                    'name': primary_name.upper(),
                    'title': primary_name,
                    'properties': json.dumps({'reasoning': reasoning, 'confidence': confidence})
                }])
                logger.info(f"Created primary entity {primary_name} for merge suggestion.")

            for alias_name in aliases:
                alias_id = self.graph_service._get_entity_id_by_name(alias_name)
                if alias_id and alias_id != primary_id:
                    # Create IS_ALIAS_OF relation
                    relation_id = f"{alias_id}-{RelationType.IS_ALIAS_OF.value}-{primary_id}"
                    relation_data = {
                        'relation_id': relation_id,
                        'source_id': alias_id,
                        'target_id': primary_id,
                        'type': RelationType.IS_ALIAS_OF,
                        'weight': confidence,
                        'properties': {'reasoning': reasoning, 'confidence': confidence}
                    }
                    self.graph_service.bulk_upsert_relations([relation_data])
                    logger.info(f"Created IS_ALIAS_OF relation: {alias_name} -> {primary_name}")

                    # Optionally, update the alias node to point to the primary node
                    # Or mark the alias node as ALIAS type and link to primary
                    alias_node_data = self.graph_service.get_node_by_id(alias_id)
                    if alias_node_data:
                        alias_node_data['type'] = NodeType.ALIAS
                        alias_node_data['aliases'] = json.dumps([primary_name]) # Store primary name as alias target
                        self.graph_service._upsert_nodes_internal([alias_node_data])
                        logger.debug(f"Updated alias node {alias_name} to type ALIAS.")
                elif not alias_id:
                    logger.warning(f"Alias entity {alias_name} not found in graph. Skipping merge relation.")

    def _format_context_for_llm(self, entities: List[Dict[str, Any]], relations: List[Dict[str, Any]], documents: List[Dict[str, Any]]) -> str:
        """Formats retrieved context into a string for LLM consumption."""
        parts = []

        if documents:
            parts.append("--- Relevant Documents ---")
            for doc in documents:
                parts.append(f"ID: {doc.get('id')}\nTitle: {doc.get('title', 'N/A')}\nContent: {doc.get('text', '')[:500]}...")
            parts.append("\n")

        if entities:
            parts.append("--- Related Entities ---")
            for entity in entities:
                parts.append(f"Name: {entity.get('name')}, Type: {entity.get('type', 'N/A')}, Description: {entity.get('description', '')}")
            parts.append("\n")

        if relations:
            parts.append("--- Existing Relationships ---")
            for rel in relations:
                parts.append(f"Source: {rel.get('source')}, Target: {rel.get('target')}, Type: {rel.get('type', 'RELATED_TO')}, Description: {rel.get('description', '')}")
            parts.append("\n")

        return "\n".join(parts).strip()

    async def _async_run_inference_cycle(self, limit: int = 5) -> Dict[str, Any]:
        """
        Runs one cycle of relation inference for nodes needing it.
        """
        logger.info(f"Starting reasoning cycle, processing up to {limit} nodes.")
        
        nodes_to_process = self.graph_service.get_nodes_needing_relation_inference(
            limit=limit,
            order_by="priority_score", # Example: order by priority score
            order_direction="DESC",
            min_priority_score=0.1 # Example: only process nodes with a minimum priority
        )
        
        if not nodes_to_process:
            logger.info("Reasoning cycle finished: No nodes found requiring relation inference.")
            return {"status": "success", "processed_count": 0, "message": "No nodes to process."}

        processed_count = 0
        failed_count = 0
        
        for node_id in nodes_to_process:
            try:
                # Process one by one to avoid resource contention
                success = await self.infer_relations_for_node(node_id)
                if success:
                    processed_count += 1
                else:
                    failed_count += 1
            except Exception as e:
                logger.error(f"Error processing node {node_id} in reasoning cycle: {e}")
                failed_count += 1
        
        logger.info(f"Reasoning cycle complete. Processed: {processed_count}, Failed: {failed_count}.")
        return {
            "status": "success",
            "processed_count": processed_count,
            "failed_count": failed_count,
            "remaining": len(nodes_to_process) - processed_count - failed_count
        }

    def run_inference_cycle(self, limit: int = 5) -> Dict[str, Any]:
        """Synchronous EPC wrapper for the async inference cycle."""
        try:
            return asyncio.run(self._async_run_inference_cycle(limit))
        except Exception as e:
            logger.error(f"Error in synchronous reasoning cycle: {e}", exc_info=True)
            return {"status": "error", "message": str(e)} 
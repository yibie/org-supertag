#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
from typing import Dict, Any, List, TYPE_CHECKING
import json
import traceback
import asyncio
import concurrent.futures
import numpy as np

if TYPE_CHECKING:
    from ..core.graph_service import GraphService
    from simtag.services.embedding_service import EmbeddingService
    from simtag.services.rag_service import RAGService
else:
    from ..core.graph_service import GraphService
    from simtag.services.embedding_service import EmbeddingService
    from simtag.services.rag_service import RAGService
from simtag.utils.unified_tag_processor import normalize_payload
from ..utils.text_processing import prepare_node_text_for_embedding, generate_semantic_id
from ..config import Config



logger = logging.getLogger(__name__)

class SyncHandler:
    """
    Coordinates the synchronization of data between Emacs and the Python backend.
    It orchestrates GraphService, EmbeddingService, and RAGService to process
    and store nodes, embeddings, and inferred relations.
    """
    def __init__(
        self,
        graph_service: "GraphService",
        embedding_service: "EmbeddingService", 
        rag_service: "RAGService",
        config: Config
    ):
        self.graph_service = graph_service
        self.embedding_service = embedding_service
        self.rag_service = rag_service
        self.config = config
        # Create a semaphore to limit concurrent LLM calls
        self.llm_semaphore = asyncio.Semaphore(config.sync_max_concurrent_llm_tasks)
        logger.info(f"SyncHandler initialized. LLM concurrency limit: {config.sync_max_concurrent_llm_tasks}")

    def _get_content_for_node(self, node_data: Dict) -> str:
        """
        Determines the most relevant text content for processing.
        """
        content = node_data.get('content')
        if content and isinstance(content, str) and content.strip():
            return content.strip()
        title = node_data.get('title')
        if title and isinstance(title, str) and title.strip():
            return title.strip()
        return ""

    async def _process_knowledge_with_limit(self, raw_content: str, node_id: str):
        """
        A wrapper function that acquires a semaphore before calling the LLM service,
        thus limiting concurrency.
        """
        async with self.llm_semaphore:
            logger.debug(f"Semaphore acquired for node {node_id}. Processing knowledge...")
            await self.rag_service.process_and_store_text(raw_content, node_id)
            logger.debug(f"Semaphore released for node {node_id}.")

    async def _process_single_entity_for_sync(self, entity: Dict[str, Any]):
        """
        处理单个实体：存储数据并根据实体类型生成嵌入向量
        
        现在期望接收已经验证过的实体数据字典
        """
        entity_id = entity.get('id')  # 注意：使用 'id' 而不是 'node_id'
        entity_type = entity.get('type')

        if not entity_id:
            logger.warning(f"Skipping entity without an ID. Original data: {entity}")
            return

        try:
            # 将 'id' 映射为 'node_id' 以兼容 graph_service
            if 'id' in entity and 'node_id' not in entity:
                entity['node_id'] = entity['id']

            # 只要是节点类型，就将其状态设置为 PENDING，以便知识周期可以处理
            if entity_type == 'node':
                # 确保 properties 存在
                if 'properties' not in entity or not isinstance(entity.get('properties'), dict):
                    entity['properties'] = {}
                # 设置状态以进行知识提取
                entity['properties']['status'] = 'PENDING'
                logger.debug(f"Set status to PENDING for node {entity_id}")

            # 如果 content 为空但 title 不为空，则将 title 作为 content 以便后续嵌入和 RAG 使用
            if entity_type == 'node' and (not entity.get('content')) and entity.get('title'):
                entity['content'] = entity['title']
            # 1. Upsert the entity's data first.
            self.graph_service.upsert_entity(entity)

            # 2. If it's a node, proceed with embedding and knowledge extraction.
            if entity_type == 'node':
                raw_content = entity.get('content', '') or entity.get('title', '')

                # Fast path：只做嵌入，不做耗时的 LLM 推理
                logger.info(f"Starting embedding process for node {entity_id}")
                embedding_result = await self.embedding_service.get_node_embedding(entity)

                if embedding_result is not None:
                    # 根据配置决定是否使用语义标识符
                    if self.config.use_semantic_embedding_ids:
                        semantic_id = generate_semantic_id(entity)
                        self.graph_service.upsert_entity_embedding(semantic_id, embedding_result)
                        logger.info(f"Successfully embedded entity {entity_id} with semantic ID '{semantic_id}' and shape {embedding_result.shape}")
                    else:
                        self.graph_service.upsert_entity_embedding(entity_id, embedding_result)
                        logger.info(f"Successfully embedded entity {entity_id} with shape {embedding_result.shape}")
                else:
                    logger.warning(f"Failed to get embedding for entity {entity_id}")

            # 3. If it's a tag, we'll handle embedding later in batch
            elif entity_type == 'tag':
                logger.debug(f"Tag entity {entity_id} will be processed in batch embedding")

        except Exception as e:
            logger.error(f"Error processing entity {entity_id} in worker: {e}", exc_info=True)

    async def bulk_process_snapshot(self, snapshot_data: Any) -> Dict[str, Any]:
        """
        Main async entry point for periodic background sync.
        Processes an incremental snapshot of changes from the Elisp database.
        
        Now receiving data that has been validated by normalize_payload
        """
        # Data has been validated and cleaned by normalize_payload
        normalized_data = normalize_payload(snapshot_data)
        entities_to_upsert = normalized_data.get("entities", [])
        links_to_upsert = normalized_data.get("links", [])
        ids_to_delete = normalized_data.get("ids_to_delete", [])

        logger.info(f"Received snapshot: {len(entities_to_upsert)} entities, {len(links_to_upsert)} links, {len(ids_to_delete)} deletions.")

        # 1. Handle Deletions
        if ids_to_delete:
            self.graph_service.delete_nodes_by_ids(ids_to_delete)
            logger.info(f"Deleted {len(ids_to_delete)} entities.")

        # 2. Process Entities (Nodes and Tags) Concurrently
        tag_ids_to_embed = []
        if entities_to_upsert:
            # Separate tags and nodes for subsequent batch embedding
            for entity in entities_to_upsert:
                if entity.get('type') == 'tag':
                    tag_ids_to_embed.append(entity.get('id'))

            # Process all entities concurrently
            tasks = [self._process_single_entity_for_sync(entity) for entity in entities_to_upsert]
            await asyncio.gather(*tasks)
            logger.info(f"Finished processing {len(entities_to_upsert)} entities.")

        # 3. Process Tags for Embedding (Batch process tag embedding)
        if tag_ids_to_embed:
            tag_ids_to_embed = [tid for tid in tag_ids_to_embed if tid]
            logger.info(f"Starting embedding process for {len(tag_ids_to_embed)} tags.")
            loop = asyncio.get_running_loop()
            with concurrent.futures.ThreadPoolExecutor() as pool:
                await loop.run_in_executor(
                    pool,
                    self.embedding_service.batch_embed_tags,
                    tag_ids_to_embed
                )
            logger.info(f"Finished processing {len(tag_ids_to_embed)} tags.")

        # 4. Upsert Org-mode Links as Relations
        if links_to_upsert:
            for link in links_to_upsert:
                # Data has been validated, use directly
                self.graph_service.upsert_relationship(
                    source_id=link['source'],
                    target_id=link['target'],
                    type=link.get('type', 'REF_TO'),
                    properties={'raw_link': link}
                )
            logger.info(f"Upserted {len(links_to_upsert)} link-based relations.")

        # 4.5 Refresh embeddings for STALE tags created by new HAS_TAG relations
        await self.embedding_service.refresh_stale_tags()
        
        # 5. Commit any pending transactions
        try:
            self.graph_service._get_connection().commit()
            logger.info("Final commit successful.")
        except Exception as e:
            logger.error(f"Error during final commit: {e}", exc_info=True)

        return {
            "status": "success", 
            "processed_entities": len(entities_to_upsert),
            "processed_links": len(links_to_upsert),
            "deleted_entities": len(ids_to_delete)
        }

    async def _embed_and_store_entity(self, entity_id, entity_data):
        """Embeds entity data and stores the embedding for a given entity."""
        logger.info(f"Embedding content for entity {entity_id}...")
        embedding_result = await self.embedding_service.get_node_embedding(entity_data)
        if embedding_result is not None:
            self.graph_service.upsert_entity_embedding(entity_id, embedding_result)
            logger.info(f"Successfully embedded and stored entity {entity_id}")
        else:
            logger.error(f"Failed to embed entity {entity_id}")



    def sync_deleted_nodes(self, payload):
        """
        Receives a list of deleted node IDs and removes them from the graph.
        """
        deleted_node_ids = normalize_payload(payload)
        logger.info(f"Received request to delete {len(deleted_node_ids)} nodes.")
        if not deleted_node_ids:
            return
        
        try:
            self.graph_service.delete_nodes_by_ids(deleted_node_ids)
            logger.info(f"Successfully deleted {len(deleted_node_ids)} nodes.")
        except Exception as e:
            logger.error(f"Failed to delete nodes: {e}", exc_info=True)

 
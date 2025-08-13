#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
KnowledgeHandler
================
Background knowledge processor responsible for staged entity extraction and relation inference.
Stage definitions:
1. PENDING              —— Pending entity extraction
2. ENTITIES_EXTRACTED   —— Entities extracted, pending relation inference
3. COMPLETED            —— Relations written, process finished

This implementation focuses on the framework and state transitions; specific LLM Prompt and parsing logic can be refined later.
"""
import logging
import asyncio
from typing import Dict, Any, List

from simtag.core.graph_service import GraphService
from simtag.services.embedding_service import EmbeddingService
from simtag.services.llm_client import LLMClient
from simtag.services.rag_service import RAGService, Relationship
from ..prompts import (
    ENTITY_EXTRACTION_PROMPT,
    QUERY_ANALYSIS_PROMPT,
    QA_PROMPT,
    DEFAULT_ENTITY_TYPES,
    RELATION_INFERENCE_PROMPT,
    ENTITY_ONLY_PROMPT,
)

logger = logging.getLogger(__name__)

class KnowledgeHandler:
    """Staged background knowledge generation processor."""

    def __init__(self, config, graph_service: GraphService, embedding_service: EmbeddingService,
                 rag_service: RAGService, llm_client: LLMClient):
        self.config = config
        self.graph_service = graph_service
        self.embedding_service = embedding_service
        self.rag_service = rag_service
        self.llm_client = llm_client
        self.llm_semaphore = asyncio.Semaphore(config.sync_max_concurrent_llm_tasks)
        logger.info("KnowledgeHandler initialized. Concurrency limit: %s", config.sync_max_concurrent_llm_tasks)

    # ---------------------------------------------------------------------
    # Public EPC entry
    # ---------------------------------------------------------------------
    async def run_extraction_cycle(self, limit: int = 5) -> Dict[str, Any]:
        """Runs one knowledge processing cycle, including entity extraction and relation inference stages."""
        logger.info("KnowledgeHandler: starting extraction cycle with limit=%d", limit)
        stage_a_processed = await self._run_entity_extraction(limit)
        stage_b_processed = await self._run_relation_inference(limit)
        total = stage_a_processed + stage_b_processed
        return {
            "status": "success",
            "stage_a_processed": stage_a_processed,
            "stage_b_processed": stage_b_processed,
            "total_processed": total,
        }

    # ------------------------------------------------------------------
    # Simple Tag Generation (replaces complex autotag functionality)
    # ------------------------------------------------------------------
    
    async def generate_simple_tags(self, payload: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Simple tag generation for nodes - replaces complex autotag functionality."""
        try:
            nodes = payload.get('nodes', [])
            if not nodes:
                return []
            
            results = []
            for node_dict in nodes:
                node_id = node_dict.get('id')
                content = node_dict.get('content', '')
                
                if not content or len(content.strip()) < 20:
                    continue
                
                # Simple prompt for tag generation
                prompt = f"""Analyze the following text and suggest 3-5 relevant tags.
Return only a JSON object with a "tags" array containing simple, lowercase tag names.

Text: {content[:1000]}

Example response: {{"tags": ["project", "research", "important"]}}"""
                
                try:
                    llm_result = await self.llm_client.generate(prompt, format_json=False)
                    if llm_result.success:
                        import json
                        import re
                        
                        # Clean up the response
                        response_text = llm_result.content.strip()
                        response_text = re.sub(r'^```[a-zA-Z]*\s*', '', response_text)
                        response_text = re.sub(r'```$', '', response_text)
                        
                        response_json = json.loads(response_text)
                        tags = response_json.get('tags', [])
                        
                        if tags:
                            results.append({
                                'node_id': node_id,
                                'tags': tags
                            })
                except Exception as e:
                    logger.warning(f"Failed to generate tags for node {node_id}: {e}")
                    continue
            
            return results
            
        except Exception as e:
            logger.error(f"Error in generate_simple_tags: {e}")
            return []

    # ------------------------------------------------------------------
    # Stage A: Entity Extraction
    # ------------------------------------------------------------------

    async def extract_entities(self, text: str) -> List[Dict[str, Any]]:
        """Return list of entity dicts (name,type,description) using lightweight prompt."""
        try:
            prompt = ENTITY_ONLY_PROMPT.format(entity_types=DEFAULT_ENTITY_TYPES, input_text=text)
            llm_res = await self.llm_client.generate(prompt, use_chat_endpoint=False)
            if not llm_res or not llm_res.content:
                return []
            txt = llm_res.content.strip()
            # strip fences if any
            import re, json
            txt = re.sub(r"^```[a-zA-Z]*\s*", "", txt).strip()
            txt = re.sub(r"```$", "", txt).strip()
            data = json.loads(txt)
            return data.get("entities", []) if isinstance(data, dict) else []
        except Exception as e:
            logger.error("extract_entities failed: %s", e)
            return []
        
    async def _run_entity_extraction(self, limit: int) -> int:
        node_ids = self.graph_service.get_nodes_by_status("PENDING", limit)
        if not node_ids:
            return 0
        logger.info("Stage A: %d nodes pending entity extraction", len(node_ids))
        tasks = [self._extract_entities_for_node(nid) for nid in node_ids]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        success_count = sum(1 for r in results if r is True)
        return success_count

    async def _extract_entities_for_node(self, node_id: str) -> bool:
        """Placeholder implementation: only handles state transition, custom Prompt can be integrated later."""
        try:
            node = self.graph_service.get_node_by_id(node_id)
            if not node:
                logger.warning("Node %s not found during entity extraction", node_id)
                return False

            raw_text = node.get("content") or node.get("title") or ""
            if not raw_text.strip():
                # If no content, skip entity extraction and mark as completed.
                self.graph_service.update_node_status(node_id, "COMPLETED")
                return True

            # --- Entity Extraction Logic ---
            try:
                entities_data = await self.extract_entities(raw_text)
                entities = [e.get("name") for e in entities_data if e.get("name")]
            except Exception as inner_e:
                logger.error("_extract_structured_data_from_text failed for node %s: %s", node_id, inner_e, exc_info=True)
                entities = []

            # Temporarily write entities to properties.extracted_entities
            node_props = node.get("properties", {}) or {}
            node_props["extracted_entities"] = entities
            node_updated = {
                "node_id": node_id,
                "properties": node_props,
            }
            self.graph_service.upsert_entity(node_updated)

            # Update status
            new_status = "ENTITIES_EXTRACTED" if entities else "COMPLETED"
            self.graph_service.update_node_status(node_id, new_status)
            return True
        except Exception as e:
            logger.error("Entity extraction failed for node %s: %s", node_id, e, exc_info=True)
            return False

    # ------------------------------------------------------------------
    # Stage B: Relation Inference
    # ------------------------------------------------------------------
    async def _run_relation_inference(self, limit: int) -> int:
        node_ids = self.graph_service.get_nodes_by_status("ENTITIES_EXTRACTED", limit)
        if not node_ids:
            return 0
        logger.info("Stage B: %d nodes pending relation inference", len(node_ids))
        tasks = [self._infer_relations_for_node(nid) for nid in node_ids]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        success_count = sum(1 for r in results if r is True)
        return success_count

    async def _infer_relations_for_node(self, node_id: str) -> bool:
        """Placeholder implementation: infers relations and writes back to database."""
        try:
            node = self.graph_service.get_node_by_id(node_id)
            if not node:
                logger.warning("Node %s not found during relation inference", node_id)
                return False

            props = node.get("properties", {}) or {}
            entities: List[str] = props.get("extracted_entities", [])
            raw_text = node.get("content") or node.get("title") or ""

            if not entities:
                # No entities -> No relation inference, mark as completed directly
                self.graph_service.update_node_status(node_id, "COMPLETED")
                return True

            # --- Relation Inference Logic ---
            try:
                relations = await self.rag_service.infer_relations(raw_text, entities)
                for rel in relations:
                    self.graph_service.upsert_relationship(rel.source, rel.target, rel.type)
            except Exception as inner_e:
                logger.error("infer_relations failed for node %s: %s", node_id, inner_e, exc_info=True)

            # Completed status
            self.graph_service.update_node_status(node_id, "COMPLETED")
            return True
        except Exception as e:
            logger.error("Relation inference failed for node %s: %s", node_id, e, exc_info=True)
            return False

    # ------------------------------------------------------------------
    # Diagnostics helpers
    # ------------------------------------------------------------------

    def get_queue_status(self) -> Dict[str, Any]:
        """Returns the count of nodes in each knowledge status, for frontend display."""
        try:
            pending = len(self.graph_service.get_nodes_by_status("PENDING", 100000))
            extracted = len(self.graph_service.get_nodes_by_status("ENTITIES_EXTRACTED", 100000))
            return {
                "status": "success",
                "pending": pending,
                "entities_extracted": extracted,
            }
        except Exception as e:
            logger.error("Failed to get knowledge queue status: %s", e, exc_info=True)
            return {"status": "error", "message": str(e)}
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
import json
import asyncio
from typing import Dict, Any

from simtag.core.graph_service import GraphService
from simtag.core.entity_extractor import LLMEntityExtractor

logger = logging.getLogger(__name__)

class ReasoningHandler:
    """
    Handles asynchronous, post-sync reasoning tasks like relation inference.
    This service is designed to be called periodically to enrich nodes that
    have already been embedded.
    """
    def __init__(self, config, graph_service: GraphService, entity_extractor: LLMEntityExtractor):
        self.config = config
        self.graph_service = graph_service
        self.entity_extractor = entity_extractor
        logger.info("ReasoningHandler initialized.")

    async def infer_relations_for_node(self, node_id: str) -> bool:
        """
        Infers and stores relations for a single node.
        
        This method fetches the node's content, uses the LLM to extract entities
        and relations, and then persists them to the graph database.
        """
        logger.debug(f"Starting relation inference for node {node_id}.")
        try:
            node_data = self.graph_service.get_node(node_id)
            if not node_data:
                logger.warning(f"Could not find node {node_id} for relation inference.")
                return False

            content = node_data.get('content', '')
            if not content.strip():
                logger.debug(f"Node {node_id} has no content, skipping relation inference.")
                # Mark as processed to avoid re-checking
                self.graph_service.mark_node_relations_inferred(node_id)
                return True

            existing_tags = node_data.get('tags', [])
            
            extraction_result = await self.entity_extractor.extract(content, existing_tags)
            
            inferred_entities_data = extraction_result.get("entities", [])
            inferred_relations_data = extraction_result.get("relations", [])
            
            if inferred_entities_data:
                entities_to_add = []
                for entity_data in inferred_entities_data:
                    entities_to_add.append({
                        'name': entity_data.get('name'),
                        'description': entity_data.get('description', '')
                    })
                self.graph_service.bulk_upsert_entity_nodes(entities_to_add)
                logger.debug(f"Upserted {len(entities_to_add)} entities for node {node_id}")

            if inferred_relations_data:
                relations_to_add = []
                for rel_data in inferred_relations_data:
                    source_name = rel_data.get('source')
                    target_name = rel_data.get('target')
                    if not target_name or not source_name:
                        logger.warning(f"Skipping malformed relation, missing source/target. Data: {rel_data}")
                        continue
                    
                    relations_to_add.append({
                        'source_name': source_name,
                        'target_name': target_name,
                        'type': rel_data.get('type', 'RELATED_TO'),
                        'properties': json.dumps({'description': rel_data.get('description', '')})
                    })

                # Here we need to resolve names to IDs before upserting relations
                final_relations = []
                for rel in relations_to_add:
                    source_id = self.graph_service._get_entity_id_by_name(rel['source_name'])
                    target_id = self.graph_service._get_entity_id_by_name(rel['target_name'])
                    if source_id and target_id:
                        final_relations.append({
                            'source_id': source_id,
                            'target_id': target_id,
                            'type': rel['type'],
                            'properties': rel['properties']
                        })
                
                if final_relations:
                    self.graph_service.bulk_upsert_relations(final_relations)
                    logger.debug(f"Upserted {len(final_relations)} relations for node {node_id}")

            # Mark the node as processed
            self.graph_service.mark_node_relations_inferred(node_id)
            logger.info(f"Successfully completed relation inference for node {node_id}.")
            return True

        except Exception as e:
            logger.error(f"Failed to infer relations for node {node_id}: {e}", exc_info=True)
            return False

    async def _async_run_inference_cycle(self, limit: int = 5) -> Dict[str, Any]:
        """
        Runs one cycle of relation inference for nodes needing it.
        """
        logger.info(f"Starting reasoning cycle, processing up to {limit} nodes.")
        
        nodes_to_process = self.graph_service.get_nodes_needing_relation_inference(limit)
        
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
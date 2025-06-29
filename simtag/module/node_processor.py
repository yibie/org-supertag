#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
import asyncio
import json
from typing import List, Dict, Any, Optional
import numpy as np
import concurrent.futures

from simtag.core.entity_extractor import LLMEntityExtractor
from simtag.core.graph_service import GraphService
from simtag.services.embedding_service import EmbeddingService


logger = logging.getLogger(__name__)

class NodeProcessor:
    """
    Handles the processing of individual nodes and batches of nodes,
    including vectorization, NER, and knowledge graph updates.
    """
    def __init__(self, config, llm_client, ner_service, graph_service: GraphService, content_processor, emacs_client=None, embedding_service: EmbeddingService = None, entity_extractor: LLMEntityExtractor = None):
        self.config = config
        self.llm_client = llm_client
        self.ner_service = ner_service
        self.graph_service = graph_service
        self.content_processor = content_processor
        self.emacs_client = emacs_client
        self.embedding_service = embedding_service
        self.entity_extractor = entity_extractor
        logger.info("NodeProcessor initialized.")

    def _eval_in_emacs(self, *args):
        # This is a placeholder. The actual implementation is in the bridge.
        # This should be passed in or handled via a callback.
        if self.emacs_client:
            # A real implementation would call self.emacs_client.call(...)
            logger.info(f"Placeholder: Would call Emacs with: {args}")
            pass
        else:
            logger.warning("Emacs client not available to NodeProcessor.")

    def _get_content_for_embedding(self, node_data: Dict) -> str:
        """
        Determines the most relevant text content for embedding.
        Prioritizes the 'content' field if it's not empty, otherwise falls back to 'title'.
        A node is considered to have no content only if both are empty.
        """
        content = node_data.get('content')
        if content and isinstance(content, str) and content.strip():
            return content.strip()

        title = node_data.get('title')
        if title and isinstance(title, str) and title.strip():
            return title.strip()
            
        return ""

    def process_nodes_batch(self, nodes: List[Dict[str, Any]], total_items: int) -> Dict[str, Any]:
        """
        Processes a batch of nodes from the snapshot in parallel.
        Worker threads handle I/O-bound tasks (LLM calls), and the main thread
        handles all database writes to prevent locking issues.
        """
        valid_nodes = [node for node in nodes if isinstance(node, dict) and 'id' in node]
        invalid_nodes_count = len(nodes) - len(valid_nodes)

        if invalid_nodes_count > 0:
            logger.warning(f"Found {invalid_nodes_count} nodes failing validation.")

        max_workers = self.config.processing_config.get('processing_workers', 4)
        logger.info(f"✅ Starting to process {len(valid_nodes)} valid nodes with up to {max_workers} parallel workers...")

        successful_count = 0
        failed_nodes = []
        all_processing_results = []

        with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
            future_to_node = {executor.submit(self._process_single_node, node, is_batch=True): node for node in valid_nodes}
            
            for future in concurrent.futures.as_completed(future_to_node):
                node = future_to_node[future]
                try:
                    result_package = future.result()
                    if result_package:
                        all_processing_results.append(result_package)
                    successful_count += 1
                except Exception as e:
                    node_id = node.get('id', 'N/A')
                    logger.error(f"Error processing node {node_id}: {e}", exc_info=True)
                    failed_nodes.append({'id': node_id, 'error': str(e)})

        logger.info(f"Parallel processing completed. Success: {successful_count}/{len(valid_nodes)}")

        # --- SEQUENTIAL DATABASE WRITES ---
        if all_processing_results:
            logger.info(f"Starting sequential database writes for {len(all_processing_results)} processed nodes...")
            # 1. Collect all data to be written
            nodes_to_upsert = []
            embeddings_to_upsert = []
            entities_to_upsert = []
            relations_to_upsert = []

            for result in all_processing_results:
                if result.get('node_for_upsert'):
                    nodes_to_upsert.append(result['node_for_upsert'])
                if result.get('embedding_vector') is not None:
                    embeddings_to_upsert.append((result['node_id'], result['embedding_vector']))
                if result.get('inferred_entities'):
                    entities_to_upsert.extend(result['inferred_entities'])
                if result.get('inferred_relations'):
                    relations_to_upsert.extend(result['inferred_relations'])

            # 2. Perform batch writes
            try:
                if nodes_to_upsert:
                    # Note: upsert_text_node handles one at a time, but its internals could be batched.
                    # For now, we call it sequentially. A future optimization could be a bulk version.
                    for node_up in nodes_to_upsert:
                        self.graph_service.upsert_text_node(node_up)
                    logger.info(f"Upserted metadata for {len(nodes_to_upsert)} text nodes.")

                if embeddings_to_upsert:
                    for node_id, vector in embeddings_to_upsert:
                        self.graph_service.upsert_node_embedding(node_id, vector)
                    logger.info(f"Upserted {len(embeddings_to_upsert)} embeddings.")

                if entities_to_upsert:
                    self.graph_service.bulk_upsert_entity_nodes(entities_to_upsert)
                    logger.info(f"Upserted {len(entities_to_upsert)} inferred entity nodes.")

                if relations_to_upsert:
                    # This step needs pre-calculation of entity IDs after they are upserted.
                    # For simplicity, we get IDs now. A more optimized way would be to get them in bulk.
                    final_relations = []
                    for rel in relations_to_upsert:
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
                        logger.info(f"Upserted {len(final_relations)} inferred relations.")

            except Exception as e:
                logger.error(f"A database error occurred during sequential writes: {e}", exc_info=True)
                # Here you might want to add logic to handle partial writes or rollback.

        if failed_nodes:
            logger.error(f"Failed to process {len(failed_nodes)} nodes. First 5 failures:")
            for i, failure in enumerate(failed_nodes[:5]):
                logger.error(f"  Node {failure.get('id')} failed: {failure.get('error')}")

        return {
            'total_processed': len(valid_nodes),
            'successful_count': successful_count,
            'failed_count': len(failed_nodes),
            'format_errors': invalid_nodes_count
        }

    def _process_single_node(self, node: Dict[str, Any], is_batch: bool) -> Optional[Dict[str, Any]]:
        """
        Prepares all data for a single node (embeddings, inferred relations) without
        writing to the database. Returns a dictionary of data to be written by the main thread.
        """
        try:
            loop = asyncio.get_running_loop()
        except RuntimeError:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)

        node_id = node.get('id')
        content_for_embedding = self._get_content_for_embedding(node)
        logger.debug(f"Preparing node {node_id}. Content length: {len(content_for_embedding)}")
        
        # 1. Prepare node data for upsert
        node_for_upsert = node.copy()
        properties_raw = node_for_upsert.get('properties', {})
        properties_dict = properties_raw[0] if isinstance(properties_raw, list) and properties_raw else properties_raw
        
        if isinstance(properties_dict, dict):
            tags_string = properties_dict.get('TAGS', '')
            if isinstance(tags_string, str):
                node_for_upsert['tags'] = [tag.strip() for tag in tags_string.split(':') if tag.strip()]

        for key in ['properties', 'olp', 'tags', 'ref_to', 'ref_from', 'priority']:
            if key in node_for_upsert and isinstance(node_for_upsert[key], (dict, list)):
                node_for_upsert[key] = json.dumps(node_for_upsert[key])
        
        node_for_upsert['node_id'] = node_for_upsert.pop('id')

        # This will hold all results from this thread
        result_package = {
            "node_id": node_id,
            "node_for_upsert": node_for_upsert,
            "embedding_vector": None,
            "inferred_entities": [],
            "inferred_relations": []
        }

        # 2. Generate embedding if there is content
        if content_for_embedding:
            embedding_config = self.config.embedding_config
            embedding_result = loop.run_until_complete(self.embedding_service.get_embedding(
                content_for_embedding,
                backend=embedding_config.get('primary_backend'),
                model=self.config.llm_client_config.get('default_embedding_model')
            ))
            if embedding_result.success:
                result_package["embedding_vector"] = np.array(embedding_result.embedding, dtype=np.float32)
            else:
                logger.error(f"Embedding generation failed for node {node_id}: {embedding_result.error_message}")
        
        logger.debug(f"Finished preparing node {node_id} for embedding and metadata.")
        return result_package

    async def process_single_node_data(self, node_id: str, content: str, tags: list, title: str) -> bool:
        """
        Processes a single node for interactive updates (embedding and metadata only).
        This is the entry point for non-batch, single-node sync operations. It performs
        embedding and metadata processing and writes directly to the database.
        Relation inference is handled by a separate process.
        """
        logger.debug(f"Starting interactive processing for single node {node_id}.")
        node_data = {
            "id": node_id,
            "content": content,
            "tags": tags,
            "title": title
        }
        
        try:
            # Call the internal worker with is_batch=False. It now only does embedding/metadata.
            result_package = self._process_single_node(node_data, is_batch=False)

            if not result_package:
                logger.warning(f"Processing returned no result for node {node_id}.")
                return False

            # --- Perform immediate, sequential database writes for the single node ---
            node_for_upsert = result_package.get('node_for_upsert')
            if node_for_upsert:
                self.graph_service.upsert_text_node(node_for_upsert)
                logger.debug(f"Upserted metadata for single node {node_id}.")

            embedding_vector = result_package.get('embedding_vector')
            if embedding_vector is not None:
                self.graph_service.upsert_node_embedding(node_id, embedding_vector)
                logger.debug(f"Upserted embedding for single node {node_id}.")

            logger.info(f"Successfully processed and saved single node {node_id} (metadata and embedding).")
            return True

        except Exception as e:
            logger.error(f"Error during single node processing for {node_id}: {e}", exc_info=True)
            return False

    def find_similar_nodes(self, node_id: str, top_k: int = 5) -> List[Dict]:
        """
        Find nodes similar to the given node_id.
        """
        try:
            # 1. Get the content of the source node
            source_node = self.graph_service.get_node(node_id)
            if not source_node:
                logger.warning(f"Cannot find similar nodes: source node {node_id} not found in graph.")
                return []
            
            content = source_node.get('content')
            if not content:
                logger.warning(f"Source node {node_id} has no content to compare.")
                return []

            # 2. Use the embedding service to find similar documents
            similar_items = self.embedding_service.search(content, top_k=top_k)
            
            # 3. Format results
            results = []
            for item in similar_items:
                # Assuming the search result item has 'id' and 'score'
                results.append({
                    "id": item.get('id'),
                    "title": item.get('title', 'N/A'),
                    "score": item.get('score', 0.0)
                })
            
            return results
        except Exception as e:
            logger.error(f"Error finding similar nodes for {node_id}: {e}", exc_info=True)
            return [] 

    def get_all_nodes(self):
        # This seems out of place for a "processor". Should be in a query handler.
        # Removing for now to clarify responsibilities.
        pass

    def _sanitize_value(self, value: Any) -> Any:
        """
        Recursively sanitizes values to be compatible with SQLite.
        """
        if isinstance(value, (dict, list)):
            return json.dumps(value)
        elif isinstance(value, str):
            return value.strip()
        else:
            return value 
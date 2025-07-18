#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
import time
import traceback

from typing import Dict, Any, List

from ..core.graph_service import GraphService
from ..utils.unified_tag_processor import normalize_payload

try:
    import numpy as np
except ImportError:
    np = None

logger = logging.getLogger(__name__)

class QueryHandler:
    """
    Handles simple, direct queries against the knowledge graph, such as
    keyword search and fetching specific node details.
    """
    def __init__(self, graph_service: GraphService):
        self.graph_service = graph_service
        self.logger = logging.getLogger(__name__)

    def search(self, payload: Dict) -> List[Dict[str, Any]]:
        """
        Performs a keyword search against node titles and content.
        """
        try:
            data = normalize_payload(payload)
            query_text = data.get("query")
            limit = data.get("limit", 10)

            if not query_text:
                self.logger.warning("Search query is empty.")
                return []

            self.logger.info(f"Performing keyword search for: '{query_text}' with limit {limit}")
            return self.graph_service.search_nodes_by_title_content(
                search_query=query_text,
                limit=limit
            )

        except Exception as e:
            self.logger.error(f"Error during keyword search: {e}", exc_info=True)
            return []

    def get_node_details(self, payload: Dict) -> Dict[str, Any]:
        """
        Retrieves full details for a given node_id, including its neighbors.
        """
        try:
            data = normalize_payload(payload)
            node_id = data.get("node_id")

            if not node_id:
                self.logger.warning("Node ID is missing for get_node_details.")
                return {"error": "Node ID is required."}

            self.logger.info(f"Fetching details for node: {node_id}")
            node_details = self.graph_service.get_node_with_neighbors(node_id)

            if not node_details:
                return {"error": f"Node with ID '{node_id}' not found."}

            return node_details

        except Exception as e:
            self.logger.error(f"Error fetching node details for {node_id}: {e}", exc_info=True)
            return {"error": "An internal error occurred."}

    async def get_similar_entities(self, *args) -> Dict[str, Any]:
        """
        Finds nodes with similar content based on vector embeddings.
        This is an async method intended to be called from the EPC bridge.
        """
        node_id = None  # Initialize for logging in case of early failure
        try:
            payload = normalize_payload(args)
            node_id = payload.get("node_id")
            top_k = payload.get("top_k", 5)

            if not node_id:
                self.logger.warning("Node ID is missing for get_similar_entities.")
                return {"status": "error", "message": "Node ID is required."}

            self.logger.info(f"Fetching similar entities for node: {node_id}")

            # Note: graph_service methods are synchronous.
            # We don't need to await them unless they are converted to async.
            query_embedding = self.graph_service.get_entity_embedding_by_id(node_id)

            if query_embedding is None:
                self.logger.warning(f"No embedding found for node {node_id}.")
                # It's not an error if a node simply has no embedding yet.
                # Return success with an empty list.
                return {"status": "success", "result": []}

            # The find_similar_nodes expects a list of floats
            if np and isinstance(query_embedding, np.ndarray):
                embedding_list = query_embedding.tolist()
            else:
                embedding_list = query_embedding


            similar_nodes = self.graph_service.find_similar_nodes(
                query_embedding=embedding_list,
                top_k=top_k + 1  # Fetch one more to account for the source node
            )

            # Exclude the query node itself from the results
            results = [node for node in similar_nodes if node.get("node_id") != node_id]

            # Trim to top_k if necessary
            results = results[:top_k]

            self.logger.info(f"Found {len(results)} similar entities for node {node_id}.")
            return {"status": "success", "result": results}

        except Exception as e:
            self.logger.error(f"Error fetching similar entities for {node_id}: {e}", exc_info=True)
            return {"status": "error", "message": f"An internal error occurred: {e}"}

    def get_similar_nodes(self, query_input: str, top_k: int = 10):
        logger.info(f"QueryHandler.get_similar_nodes: '{query_input[:100]}...', top_k: {top_k}")
        start_time = time.time()
        try:
            # This is a temporary solution. The engine concept will be removed.
            # Directly call embedding service and graph service.
            try:
                similar_nodes_result = self.graph_service.search_nodes_by_title_content(query_input, limit=top_k)
                logger.info(f"QueryHandler.get_similar_nodes completed in {time.time() - start_time:.4f} seconds. Found {len(similar_nodes_result)} nodes.")
                return {"status": "success", "result": similar_nodes_result}
            except Exception as search_error:
                logger.error(f"Text search also failed: {search_error}")
                return {"status": "error", "message": "Both embedding and text search failed"}
        except Exception as e:
            logger.error(f"QueryHandler.get_similar_nodes failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)} 

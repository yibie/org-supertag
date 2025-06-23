#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
from typing import Dict, Any, List
import time
import traceback

from simtag.core.graph_service import GraphService

logger = logging.getLogger(__name__)

class QueryHandler:
    def __init__(self, engine, user_interface, graph_service: GraphService, emacs_client=None):
        self.engine = engine
        self.user_interface = user_interface
        self.graph_service = graph_service
        self.emacs_client = emacs_client
        logger.info("QueryHandler initialized.")

    def get_similar_nodes(self, query_input: str, top_k: int = 10):
        """
        Finds similar nodes using the core tagging engine.
        Moved from SimTagBridge.
        """
        logger.info(f"QueryHandler.get_similar_nodes called for: '{query_input[:100]}...', top_k: {top_k}")
        start_time = time.time()
        try:
            if not hasattr(self, 'engine') or self.engine is None:
                logger.error("QueryHandler.get_similar_nodes: TaggingEngine (self.engine) is not initialized.")
                return {"status": "error", "message": "TaggingEngine not initialized"}
            
            similar_nodes_result = self.engine.find_similar_nodes(query_input, top_k)
            
            logger.info(f"QueryHandler.get_similar_nodes for '{query_input[:100]}...' completed in {time.time() - start_time:.4f} seconds. Found {len(similar_nodes_result)} nodes.")
            return {"status": "success", "result": similar_nodes_result}
        except Exception as e:
            logger.error(f"QueryHandler.get_similar_nodes for '{query_input[:100]}...' failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def find_similar_nodes_friendly(self, query_input: str, top_k: int = 10):
        """
        User-friendly method to find similar nodes, returning rich context.
        Moved from SimTagBridge.
        """
        logger.info(f"QueryHandler.find_similar_nodes_friendly: '{query_input[:100]}...', top_k: {top_k}")
        start_time = time.time()
        
        try:
            if not hasattr(self, 'user_interface') or self.user_interface is None:
                logger.error("QueryHandler.find_similar_nodes_friendly: UserInterfaceService not initialized.")
                return {"status": "error", "message": "UserInterfaceService not initialized"}
            
            if not hasattr(self, 'engine') or self.engine is None:
                logger.error("QueryHandler.find_similar_nodes_friendly: TaggingEngine not initialized.")
                return {"status": "error", "message": "TaggingEngine not initialized"}
            
            # Resolve user input to UUID if possible
            resolved_query = self.user_interface.resolve_user_input_to_uuid(query_input) or query_input
            
            # Use the engine to find similar nodes (returns UUIDs)
            uuid_results = self.engine.find_similar_nodes(resolved_query, top_k)
            
            # Convert to user-friendly format
            friendly_results = self.user_interface.convert_similar_nodes_to_user_friendly(uuid_results)
            
            logger.info(f"QueryHandler.find_similar_nodes_friendly: Completed for '{query_input[:100]}...' in {time.time() - start_time:.4f}s. Found {len(friendly_results)} nodes.")
            
            return {
                "status": "success", 
                "result": friendly_results,
                "query_info": {
                    "original_query": query_input,
                    "resolved_query": resolved_query,
                    "total_results": len(friendly_results)
                }
            }
            
        except Exception as e:
            logger.error(f"QueryHandler.find_similar_nodes_friendly failed for '{query_input[:100]}...': {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def search_nodes_by_content(self, search_query: str, top_k: int = 10, fuzzy_match: bool = True):
        """
        Searches for nodes by their content (title, keywords, etc.).
        Moved from SimTagBridge.
        """
        logger.info(f"QueryHandler.search_nodes_by_content: '{search_query}', top_k: {top_k}, fuzzy: {fuzzy_match}")
        start_time = time.time()
        
        try:
            if not hasattr(self, 'user_interface') or self.user_interface is None:
                logger.error("QueryHandler.search_nodes_by_content: UserInterfaceService not initialized.")
                return {"status": "error", "message": "UserInterfaceService not initialized"}
            
            # Use the user interface service to perform the search
            search_results = self.user_interface.search_nodes_by_title_or_content(
                query=search_query,
                top_k=top_k,
                fuzzy_match=fuzzy_match
            )
            
            logger.info(f"QueryHandler.search_nodes_by_content: Completed for '{search_query}' in {time.time() - start_time:.4f}s. Found {len(search_results)} results.")
            
            return {
                "status": "success",
                "result": search_results,
                "search_info": {
                    "query": search_query,
                    "total_results": len(search_results),
                    "fuzzy_match_enabled": fuzzy_match
                }
            }
            
        except Exception as e:
            logger.error(f"QueryHandler.search_nodes_by_content failed for '{search_query}': {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def get_node_context_friendly(self, node_identifier: str):
        """
        Gets the full context for a node in a user-friendly format.
        Moved from SimTagBridge.
        """
        logger.info(f"QueryHandler.get_node_context_friendly: '{node_identifier}'")
        start_time = time.time()
        
        try:
            if not hasattr(self, 'user_interface') or self.user_interface is None:
                logger.error("QueryHandler.get_node_context_friendly: UserInterfaceService not initialized.")
                return {"status": "error", "message": "UserInterfaceService not initialized"}
            
            # Resolve user input to a UUID
            node_uuid = self.user_interface.resolve_user_input_to_uuid(node_identifier)
            if not node_uuid:
                return {"status": "error", "message": f"Could not resolve node identifier: {node_identifier}"}
            
            # Get the node context
            context_info = self.user_interface.get_node_context_by_uuid(node_uuid)
            if not context_info:
                return {"status": "error", "message": f"Node not found: {node_identifier}"}
            
            logger.info(f"QueryHandler.get_node_context_friendly: Completed for '{node_identifier}' in {time.time() - start_time:.4f}s.")
            
            return {
                "status": "success",
                "result": context_info,
                "node_info": {
                    "identifier": node_identifier,
                    "resolved_uuid": node_uuid
                }
            }
            
        except Exception as e:
            logger.error(f"QueryHandler.get_node_context_friendly failed for '{node_identifier}': {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)} 
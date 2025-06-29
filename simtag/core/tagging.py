"""
Tag Processing Core Engine
Integrates entity extraction, tag generation, and relationship analysis functionality
"""
import logging
from .graph_service import GraphService
import time
import traceback
import numpy as np
from typing import List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    pass

class TaggingEngine:
    def __init__(self, config, graph_service: GraphService, llm_client):
        self.config = config
        self.graph_service = graph_service
        self.llm_client = llm_client
        self.logger = logging.getLogger("simtag_bridge.tagging_engine")
        self.logger.info("TaggingEngine initialized.")

        # Initialize SyncOrchestrator for node vectorization
        # self.sync_orchestrator = SyncOrchestrator(storage=self.storage,
        #                                         llm_client=self.llm_client,
        #                                         logger=self.logger)
        self.logger.warning("TaggingEngine: SyncOrchestrator is temporarily disabled during refactoring.")

    def _generate_vector_for_text(self, text: str) -> Optional[np.ndarray]:
        """Generates an embedding vector for a given text string.

        Args:
            text: The text to embed.

        Returns:
            A NumPy array of the embedding, or None if generation fails.
        """
        if not text or not text.strip():
            self.logger.warning("_generate_vector_for_text: Input text is empty or whitespace. Skipping embedding.")
            return None
        try:
            if self.llm_client:
                self.logger.debug(f"Using LLMClient to generate semantic vector for text: '{text[:100]}...'")
                # LLMClient might not have get_embedding_sync, let's assume it should be async
                import asyncio
                # This is a workaround for calling async from sync code
                try:
                    loop = asyncio.get_running_loop()
                except RuntimeError:
                    loop = asyncio.new_event_loop()
                    asyncio.set_event_loop(loop)
                
                vector_list = loop.run_until_complete(self.llm_client.get_embedding(text))
                
                if vector_list:
                    return np.array(vector_list, dtype=np.float32)
                else:
                    self.logger.warning(f"_generate_vector_for_text: LLMClient returned None or empty vector for text: '{text[:100]}...'")
                    return None
            else:
                self.logger.error("LLMClient unavailable, cannot generate vector for text.")
                return None
        except Exception as e:
            self.logger.error(f"Failed to generate text vector for '{text[:100]}...': {str(e)}\n{traceback.format_exc()}")
            return None

    def find_similar_nodes(self, query_input: str, top_k: int = 10) -> list:
        """Finds similar nodes based on a query input (either a node_id or text).

        Args:
            query_input: A node_id (str) or a text string (str) to find similar nodes for.
            top_k: The number of similar nodes to return.

        Returns:
            A list of tuples, where each tuple is (node_id, distance_score).
            Returns an empty list if no query vector can be obtained or if an error occurs.
        """
        engine_start_time = time.time()
        self.logger.info(f"[TaggingEngine.find_similar_nodes] Entered for query_input: '{query_input[:100]}...', top_k: {top_k}")

        query_vector: Optional[np.ndarray] = None

        # Attempt to retrieve vector if query_input might be a node_id
        if self.graph_service.has_vector_ext:
            self.logger.debug(f"[TaggingEngine.find_similar_nodes] Checking if '{query_input}' is a node_id with a stored embedding.")
            potential_vector = self.graph_service.get_node_embedding_by_id(query_input)
            if potential_vector is not None:
                query_vector = potential_vector
                self.logger.info(f"[TaggingEngine.find_similar_nodes] Using stored embedding for node_id: '{query_input}'.")

        # If no stored vector was found, treat query_input as text and generate embedding
        if query_vector is None:
            self.logger.info(f"[TaggingEngine.find_similar_nodes] No stored vector found for '{query_input}'. Treating as text and generating new embedding.")
            query_vector = self._generate_vector_for_text(query_input)

        # If we have a vector, query the storage
        if query_vector is not None and query_vector.size > 0:
            self.logger.debug("[TaggingEngine.find_similar_nodes] Querying storage for similar nodes.")
            storage_query_start_time = time.time()
            similar_nodes = self.graph_service.find_similar_nodes(query_vector, top_k=top_k)
            storage_query_end_time = time.time()
            self.logger.info(f"[TaggingEngine.find_similar_nodes] Storage query took {storage_query_end_time - storage_query_start_time:.4f}s. Found {len(similar_nodes)} nodes.")
            return similar_nodes
        else:
            self.logger.warning(f"[TaggingEngine.find_similar_nodes] Could not obtain a query vector for input: '{query_input[:100]}...'. Returning empty list.")
            return []

    def sync_full_snapshot(self, db_snapshot):
        """
        Processes a full database snapshot to create or update node embeddings.
        This has been simplified to only handle node vectorization.
        """
        self.logger.warning("sync_full_snapshot is temporarily disabled during refactoring.")
        return {"status": "disabled", "message": "This feature is disabled during refactoring."}
        # self.logger.info("Starting simplified full snapshot sync for node embeddings.")
        # start_time = time.time()
        #
        # nodes_data = db_snapshot.get('nodes', [])
        # if not nodes_data:
        #     self.logger.info("No nodes in the snapshot to process.")
        #     return {"status": "success", "message": "No nodes to process."}
        #
        # # Use SyncOrchestrator to handle the node processing logic
        # summary = self.sync_orchestrator.sync_nodes(nodes_data)
        #
        # end_time = time.time()
        # self.logger.info(f"Full snapshot sync for nodes completed in {end_time - start_time:.2f} seconds. "
        #                  f"Processed: {summary['processed']}, "
        #                  f"Updated: {summary['updated']}, "
        #                  f"Skipped: {summary['skipped']}, "
        #                  f"Errors: {summary['errors']}.")
        #
        # return {
        #     "status": "success",
        #     "message": "Node embedding synchronization complete.",
        #     "summary": summary
        # }

    def get_node_embedding(self, node_id: str) -> Optional[List[float]]:
        """
        Retrieves the embedding for a specific node ID.
        """
        self.logger.debug(f"Attempting to retrieve embedding for node_id: {node_id}")
        vector = self.graph_service.get_node_embedding_by_id(node_id)
        if vector is not None:
            return vector.tolist()
        return None

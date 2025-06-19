#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
import json
from typing import Dict, Any, List

from simtag.core.graph_service import GraphService
from simtag.services.llm_client import LLMClient

logger = logging.getLogger(__name__)

class ResonanceHandler:
    """
    Handles the logic for the "Core Resonance Loop" MVP.
    Provides proactive suggestions by finding non-obvious connections
    between the current note and existing notes in the knowledge base.
    """
    def __init__(self, graph_service: GraphService, llm_client: LLMClient, config: Dict[str, Any] = None):
        """
        Initializes the ResonanceHandler.

        Args:
            graph_service: The service for interacting with the knowledge graph.
            llm_client: The client for interacting with Large Language Models.
            config: Optional configuration dictionary.
        """
        self.graph_service = graph_service
        self.llm_client = llm_client
        self.config = config or {}
        self.top_k = self.config.get("resonance_top_k", 5)
        self.min_results_for_resonance = self.config.get("min_results_for_resonance", 2)
        logger.info(f"ResonanceHandler initialized. Top-K: {self.top_k}, Min Results: {self.min_results_for_resonance}")

    def _build_prompt(self, source_content: str, related_nodes: List[Dict[str, Any]]) -> str:
        """Builds the prompt for the LLM to find conceptual resonance."""
        
        source_summary = (source_content[:200] + '...') if len(source_content) > 200 else source_content
        
        related_items_str = ""
        for i, node in enumerate(related_nodes):
            title = node.get('title', f"Node {node.get('id')}")
            content_summary = node.get('content', '')
            summary = (content_summary[:150] + '...') if len(content_summary) > 150 else content_summary
            related_items_str += f"{i+1}. Title: {title}\n   Summary: {summary}\n\n"

        prompt = f"""
Analyze the following source text and a list of related items from a knowledge base. Your task is to identify a single, non-obvious, and insightful connection, theme, or contradiction between the source text and the related items. Express this connection in one compelling sentence.

**Source Text:**
---
{source_summary}
---

**Related Items:**
---
{related_items_str}
---

**Your Task:**
In one sentence, what is the most interesting and non-obvious connection?
"""
        return prompt.strip()

    def get_resonance(self, content: str, current_node_id: str = None) -> Dict[str, Any]:
        """
        The core logic for finding and describing a conceptual resonance.

        Args:
            content: The text content of the current note being edited.
            current_node_id: The ID of the node to exclude from results.

        Returns:
            A dictionary containing the resonance description and related nodes,
            or an empty dictionary if no resonance is found.
        """
        logger.debug(f"Getting resonance for content (node_id: {current_node_id})")
        
        try:
            # 1. Vector search for top-k similar nodes
            exclude_ids = [current_node_id] if current_node_id else []
            similar_nodes = self.graph_service.search_nodes_by_content(
                query_text=content, 
                top_k=self.top_k, 
                exclude_ids=exclude_ids
            )

            if not similar_nodes or len(similar_nodes) < self.min_results_for_resonance:
                logger.info("Not enough similar nodes found to generate a meaningful resonance.")
                return {"status": "no_resonance", "message": "Not enough connections found."}

            # 2. Build a prompt for the LLM
            prompt = self._build_prompt(content, similar_nodes)
            logger.debug(f"Generated resonance prompt:\n{prompt}")

            # 3. Call LLM to get the connection insight
            llm_response_str = self.llm_client.generate(prompt)
            if not llm_response_str:
                logger.warning("LLM returned an empty response for resonance prompt.")
                return {"status": "error", "message": "LLM returned empty response."}

            # For simplicity, we assume the LLM returns just the sentence.
            # In a real scenario, we might want to parse a JSON object.
            insight = llm_response_str.strip()
            
            # 4. Format and return the result
            related_node_ids = [node.get('id') for node in similar_nodes]

            result = {
                "status": "success",
                "insight": insight,
                "related_nodes": related_node_ids,
                "full_prompt_for_debug": prompt
            }
            logger.info(f"Successfully found resonance: '{insight}'")
            return result

        except Exception as e:
            logger.error(f"Error getting resonance: {e}", exc_info=True)
            return {"status": "error", "message": str(e)} 
import logging
import json
import asyncio
from typing import Dict, Any, List

from .. import prompts
from ..services.llm_client import LLMClient
from ..utils.unified_tag_processor import normalize_payload, TagResult
from ..prompts import AUTOTAG_SUGGESTION_PROMPT
from ..utils.utils import extract_json_from_response

logger = logging.getLogger(__name__)

class AutotagHandler:
    """
    Handles autotagging of notes by generating suggested tags using an LLM.
    """
    def __init__(self, llm_client: LLMClient):
        self.llm_client = llm_client
        self.logger = logging.getLogger(__name__)

    async def _suggest_tags_for_node(self, node: Dict[str, Any], model_config: Dict) -> List[str]:
        """
        Core logic to generate tags for a single, clean node dictionary.
        This now uses the dedicated AUTOTAG_SUGGESTION_PROMPT.
        """
        if not isinstance(node, dict):
            raise ValueError(f"Expected a 'node' dictionary, got {type(node)}")

        content = node.get('content', '')
        if not content:
            return []

        prompt = AUTOTAG_SUGGESTION_PROMPT.format(text_content=content)
        llm_result = await self.llm_client.generate(prompt, format_json=False, **model_config)

        if not llm_result.success:
            self.logger.error(f"LLM generation failed for node {node.get('id')}: {llm_result.error_message}")
            return []

        response_json = extract_json_from_response(llm_result.content)
        if response_json and isinstance(response_json.get("tags"), list):
            return response_json["tags"]
        
        self.logger.warning(f"Failed to extract valid 'tags' list from LLM response for node {node.get('id')}")
        return []

    async def suggest_tags_for_single_node_elisp(self, payload: Any) -> Dict[str, Any]:
        """
        Adapter method for single node tag generation from Elisp.
        It normalizes the payload and then calls the core logic.
        """
        node_id = 'unknown'
        try:
            data_dict = normalize_payload(payload)
            node = data_dict.get('node')
            if not node:
                nodes = data_dict.get('nodes', [])
                if nodes and len(nodes) > 0:
                    node = nodes[0]

            if not node:
                return {'node_id': 'unknown', 'tags': [], 'status': 'no_node_data'}

            node_id = node.get('id', 'unknown')
            model_config = data_dict.get("model_config", {})

            tags = await self._suggest_tags_for_node(node, model_config)

            return {'node_id': node_id, 'tags': tags, 'status': 'success'}

        except Exception as e:
            self.logger.error(f"Error in suggest_tags_for_single_node_elisp (node: {node_id}): {str(e)}", exc_info=True)
            return {'node_id': node_id, 'tags': [], 'status': 'error', 'error': str(e)}

    async def batch_generate_tags(self, payload: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Generates tags for a batch of nodes concurrently by calling the core node processing logic.
        """
        data = normalize_payload(payload)
        nodes = data.get("nodes")
        model_config = data.get("model_config", {})

        if not nodes:
            logger.warning("batch_generate_tags called with no 'nodes' in payload.")
            return [{"error": "Payload missing 'nodes' key."}]

        logger.info(f"Starting batch tag generation for {len(nodes)} nodes.")

        tasks = [self._suggest_tags_for_node(node, model_config) for node in nodes]

        results = await asyncio.gather(*tasks, return_exceptions=True)

        processed_results = []
        for i, result in enumerate(results):
            node_info = nodes[i] if i < len(nodes) else {}
            node_id = node_info.get('id', f'unknown_node_{i}')

            if isinstance(result, Exception):
                logger.error(f"Error processing node {node_id} in batch: {result}", exc_info=result)
                processed_results.append({
                    "node_id": node_id,
                    "tags": [],
                    "error": str(result)
                })
            else:
                processed_results.append({
                    "node_id": node_id,
                    "tags": result
                })

        logger.info(f"Batch tag generation completed for {len(nodes)} nodes.")
        return processed_results

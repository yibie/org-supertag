import logging
from typing import Dict, Any

from ..services.rag_service import RAGService
from ..utils.unified_tag_processor import normalize_payload
from ..prompts import generate_query_with_language_instruction

logger = logging.getLogger(__name__)

class RAGHandler:
    """
    A clean, simple handler that acts as an API endpoint for the RAGService.
    """
    def __init__(self, rag_service: RAGService):
        self.rag_service = rag_service
        self.logger = logging.getLogger(__name__)

    async def query(self, payload: Dict) -> Dict[str, Any]:
        """
        Receives a query payload from the Elisp front-end, passes it to the
        RAGService, and returns the result.
        """
        try:
            # Handle chat query data format (different from snapshot data)
            data = self._normalize_chat_payload(payload)
            self.logger.debug(f"Normalized chat data keys: {list(data.keys())}")
            query_text = data.get("query") or data.get("query_text")
            history = data.get("history", [])
            lang = data.get("lang") # Extract language setting
            command = data.get("command") # Extract command setting

            if not query_text:
                self.logger.error("Query payload did not contain 'query' text.")
                return {"error": "Query text is missing."}

            if isinstance(query_text, list):
                query_text = " ".join(str(x) for x in query_text)

            # Generate a language-specific query if lang is provided and no command is active
            # Skip language prefix for custom commands as they already contain complete instructions
            if lang and not command:
                final_query = generate_query_with_language_instruction(lang, query_text)
            else:
                final_query = query_text

            self.logger.info(f"RAGHandler received query: '{final_query}' (history len {len(history)}, command: {command})")
            return await self.rag_service.query(final_query, history=history, command=command)

        except Exception as e:
            self.logger.error(f"Error in RAGHandler query: {e}", exc_info=True)
            return {"error": f"An unexpected error occurred in RAGHandler: {e}"}

    def _normalize_chat_payload(self, payload: Any) -> Dict:
        """
        Normalize chat query payload from Elisp.
        This is a simplified version for chat data that doesn't use the full snapshot validation.
        """
        logger.debug(f"_normalize_chat_payload received raw payload of type: {type(payload)}")

        current_data = payload
        # Elisp often wraps data in a single-element list or tuple
        while isinstance(current_data, (list, tuple)) and len(current_data) == 1:
            current_data = current_data[0]
            logger.debug(f"Unwrapped payload, current type: {type(current_data)}")

        # After unwrapping, we should have the core alist (as a list/tuple of lists/tuples)
        if isinstance(current_data, (list, tuple)):
            from ..utils.unified_tag_processor import _parse_elisp_data
            parsed_dict = _parse_elisp_data(current_data)
            if isinstance(parsed_dict, dict):
                logger.debug(f"Successfully parsed chat payload to dict: {list(parsed_dict.keys())}")
                return parsed_dict
            else:
                logger.error(f"Parsed chat data is not a dict, but {type(parsed_dict)}. Returning empty dict.")
                return {}
        
        if isinstance(current_data, dict):
            return current_data

        logger.error(f"Could not normalize chat payload. Final type was {type(current_data)}. Returning empty dict.")
        return {}
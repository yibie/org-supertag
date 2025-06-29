# simtag/services/ner_service.py

import logging
from typing import List, Dict, Any, Optional

# Assuming LLMClient has an async generate method
from simtag.services.llm_client import LLMClient
from simtag.services.smart_ner_service import SmartNERService

logger = logging.getLogger(__name__)

class NERService:
    """
    A simplified service that acts as a facade, delegating all NER-based 
    operations to the SmartNERService.
    """
    def __init__(self, llm_client: LLMClient, config: Dict[str, Any], smart_ner_service: SmartNERService):
        """
        Initializes the NERService.

        Args:
            llm_client: The LLM client instance.
            config: The application configuration.
            smart_ner_service: The high-performance NER implementation. This is a required dependency.
        """
        self.llm_client = llm_client
        self.config = config
        self.smart_ner_service = smart_ner_service

        if not self.llm_client:
            logger.error("NERService initialized without a valid LLMClient.")
            raise ValueError("LLMClient is required for NERService.")

        if not self.smart_ner_service:
            # This check is now for robustness, but dependency injection should prevent it.
            logger.error("NERService initialized without a valid SmartNERService.")
            raise ValueError("SmartNERService is a required dependency for NERService.")
        
        logger.info("✅ NERService initialized, delegating to SmartNERService.")

    async def suggest_tags_batch(self, 
                               note_contents: List[str], 
                               existing_tags_list: Optional[List[List[str]]] = None) -> List[List[Dict[str, Any]]]:
        """
        Delegates batch tag suggestion directly to SmartNERService.
        
        Args:
            note_contents: A list of note contents.
            existing_tags_list: A list of existing tag lists for each note.
            
        Returns:
            A list of tag suggestion lists for each note.
        """
        if not note_contents:
            return []
        
        logger.info(f"🧠 Delegating tag suggestion for {len(note_contents)} notes to SmartNERService.")
        
        try:
            # Use the parallel version if available, otherwise fall back to serial batch.
            if hasattr(self.smart_ner_service, 'suggest_tags_batch_parallel'):
                logger.debug("🚀 Using parallel processing in SmartNERService.")
                results = await self.smart_ner_service.suggest_tags_batch_parallel(
                    note_contents=note_contents,
                    existing_tags_lists=existing_tags_list
                )
            else:
                logger.debug("🔄 Using serial batch processing in SmartNERService.")
                # Fix: SmartNERService uses batch_generate_tags, not suggest_tags_batch
                # We need to convert the input format to match SmartNERService expectations
                nodes_data = []
                for i, content in enumerate(note_contents):
                    existing_tags = existing_tags_list[i] if existing_tags_list and i < len(existing_tags_list) else []
                    node_data = {
                        'id': f'temp_node_{i}',
                        'title': '',
                        'content': content,
                        'file_path': '',
                        'level': 1,
                        'existing_tags': existing_tags
                    }
                    nodes_data.append(node_data)
                
                batch_result = await self.smart_ner_service.batch_generate_tags(nodes_data)
                
                # Convert BatchProcessingResult back to the expected format
                results = []
                # batch_result.suggestions has format: [{'node_id': 'xxx', 'suggestions': [tag_data, ...]}]
                for suggestion_group in batch_result.suggestions:
                    node_suggestions = []
                    for tag_data in suggestion_group.get('suggestions', []):
                        tag_dict = {
                            'tag_name': tag_data.get('tag_name', ''),
                            'confidence': tag_data.get('confidence', 0.0),
                            'reasoning': tag_data.get('reasoning', '')
                        }
                        node_suggestions.append(tag_dict)
                    results.append(node_suggestions)
            
            logger.info(f"✅ SmartNERService processing complete for {len(note_contents)} notes. Avg time: 0.000s/note")
            return results

        except Exception as e:
            logger.error(f"An error occurred while delegating to SmartNERService: {e}", exc_info=True)
            # Return a list of empty lists to match the expected output format on failure.
            return [[] for _ in note_contents]

    async def discover_relationships_batch(self, 
                                         note_contents: List[str]) -> List[List[Dict[str, Any]]]:
        """
        Delegates batch relationship discovery directly to SmartNERService.
        
        Args:
            note_contents: A list of note contents.
            
        Returns:
            A list of relationship lists for each note.
        """
        if not note_contents:
            return []
            
        logger.info(f"🧠 Delegating relationship discovery for {len(note_contents)} notes to SmartNERService.")

        try:
            # The parallel version is preferred if available.
            if hasattr(self.smart_ner_service, 'discover_relationships_batch_parallel'):
                logger.debug("🚀 Using parallel relationship discovery in SmartNERService.")
                results = await self.smart_ner_service.discover_relationships_batch_parallel(note_contents)
            else:
                logger.debug("🔄 Using serial relationship discovery in SmartNERService.")
                results = await self.smart_ner_service.discover_relationships_batch(note_contents)
            
            logger.info("✅ SmartNERService relationship discovery complete.")
            return results
        
        except Exception as e:
            logger.error(f"An error occurred during relationship discovery delegation: {e}", exc_info=True)
            return [[] for _ in note_contents]

 
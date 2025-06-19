"""
Org SuperTag Entity Extractor Module

This module is responsible for extracting entities from org-mode content
by leveraging a unified, prompt-based approach with an LLM.
"""
import asyncio
import logging
import json
import re
from typing import Dict, List, Any, Optional, Callable, Awaitable, Tuple
from dataclasses import dataclass, field
from abc import ABC, abstractmethod

# Import unified prompt management and tag processor
try:
    from ..prompts import create_prompt, DEFAULT_ENTITY_TYPES, INFER_RELATIONS_PROMPT
    from ..utils.unified_tag_processor import UnifiedTagProcessor, TagResult
except ImportError:
    # Fallback for direct execution
    import sys
    import os
    sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    from prompts import create_prompt, DEFAULT_ENTITY_TYPES, INFER_RELATIONS_PROMPT
    from utils.unified_tag_processor import UnifiedTagProcessor, TagResult

from simtag.services.llm_client import LLMClient

logger = logging.getLogger(__name__)

@dataclass
class ExtractedEntity:
    """Represents a single entity (tag) extracted from the text."""
    name: str
    confidence: Optional[float] = None
    reasoning: Optional[str] = None
    source_node_id: Optional[str] = None # ID of the org-node from which it was extracted
    # The 'type' field is retained for future use but is not populated by the current prompt.
    type: str = "concept"
    attributes: Dict[str, Any] = field(default_factory=dict)

class ExtractedRelation:
    def __init__(self, source: str, target: str, relationship: str, description: str):
        self.source = source
        self.target = target
        self.relationship = relationship
        self.description = description

class BaseExtractor(ABC):
    @abstractmethod
    def extract(self, text: str) -> Tuple[List[ExtractedEntity], List[ExtractedRelation]]:
        pass

class LLMEntityExtractor(BaseExtractor):
    """
    Uses an LLM to extract entities and their relationships from unstructured text.
    """
    def __init__(self, llm_client: LLMClient, config: Dict[str, Any]):
        self.llm_client = llm_client
        self.config = config
        self.prompt_template = INFER_RELATIONS_PROMPT

    async def extract(self, text: str, existing_entities: List[str] = None) -> Dict[str, List[Dict[str, Any]]]:
        """
        Extracts entities and relations from the given text.

        Args:
            text: The text content to analyze.
            existing_entities: A list of known entity names to provide context.

        Returns:
            A dictionary containing 'entities' and 'relations' found in the text.
            Returns empty lists if extraction fails or text is empty.
        """
        if not text or not text.strip():
            return {"entities": [], "relations": []}

        try:
            prompt = self.prompt_template.format(
                text_content=text,
                existing_entities=", ".join(existing_entities) if existing_entities else "N/A"
            )
            
            # Use the dedicated model for inference if specified in the config
            inference_model = self.config.get("inference_model")

            # Await the asynchronous call to the LLM client, passing the specific model
            response_text = await self.llm_client.generate(prompt, model=inference_model)

            # The response is expected to be a JSON string, possibly with markdown code fences.
            # We need to robustly parse it.
            return self._parse_llm_response(response_text)

        except Exception as e:
            logger.error(f"Failed to extract entities/relations from text: {e}", exc_info=True)
            return {"entities": [], "relations": []}

    def _parse_llm_response(self, response_text: str) -> Dict[str, List[Dict[str, Any]]]:
        """
        Parses the JSON response from the LLM, handling potential markdown fences.
        """
        try:
            # Clean up markdown code block fences if they exist
            if response_text.strip().startswith("```json"):
                response_text = response_text.strip()[7:-3].strip()
            elif response_text.strip().startswith("```"):
                 response_text = response_text.strip()[3:-3].strip()

            data = json.loads(response_text)
            
            # Validate basic structure
            entities = data.get("entities", [])
            relations = data.get("relations", [])
            
            if not isinstance(entities, list) or not isinstance(relations, list):
                logger.warning(f"LLM response has invalid structure: {data}")
                return {"entities": [], "relations": []}

            return {"entities": entities, "relations": relations}

        except json.JSONDecodeError:
            logger.error(f"Failed to decode LLM JSON response: {response_text}")
            return {"entities": [], "relations": []}
        except Exception as e:
            logger.error(f"An unexpected error occurred while parsing LLM response: {e}")
            return {"entities": [], "relations": []}

# You can add other extractor implementations here if needed,
# e.g., a SpaCy-based one for faster, less-detailed extraction.

class OrgSupertagEntityExtractor:
    """
    Extracts entities from org-mode node content using a single-pass LLM call.
    This class has been simplified to align with a unified prompt that returns
    a standard JSON format, removing the need for complex parsing and multi-stage
    "gleaning" rounds.
    """

    def __init__(self,
                 llm_async_callable: Callable[[str], Awaitable[str]],
                 entity_extractor_config: Optional[Dict[str, Any]] = None):
        """
        Initializes the OrgSupertagEntityExtractor.

        Args:
            llm_async_callable: An asynchronous function that takes a prompt string
                                and returns the LLM completion string.
            entity_extractor_config: Configuration dictionary for the extractor.
                                     Supported keys:
                                     - 'entity_types': Optional[List[str]]
        """
        self.llm_async_callable = llm_async_callable
        config = entity_extractor_config if entity_extractor_config else {}

        self.entity_types = config.get('entity_types', DEFAULT_ENTITY_TYPES)
        
        # 使用统一标签处理器
        self.unified_processor = UnifiedTagProcessor()

        logger.info(f"OrgSupertagEntityExtractor initialized with entity types: {self.entity_types}")

    def _convert_tag_result_to_extracted_entity(self, tag_result: TagResult, node_id: Optional[str]) -> ExtractedEntity:
        """将 TagResult 转换为 ExtractedEntity 以保持 API 兼容性"""
        return ExtractedEntity(
            name=tag_result.tag_name,
            confidence=tag_result.confidence,
            reasoning=tag_result.reasoning,
            source_node_id=node_id,
            type="concept",  # 默认类型
            attributes={"source": tag_result.source}
        )

    async def _parse_llm_result(self, llm_output: str, node_id: Optional[str]) -> List[ExtractedEntity]:
        """
        使用统一标签处理器解析 LLM 输出为 ExtractedEntity 对象列表。
        这个版本使用 UnifiedTagProcessor 来处理所有 JSON 清理和解析逻辑。
        """
        entities: List[ExtractedEntity] = []
        
        try:
            # 使用统一处理器处理 LLM 响应
            # 为单个节点创建临时 note_id 列表
            temp_note_id = node_id or "temp_node"
            note_results = self.unified_processor.process_llm_response(
                response_str=llm_output,
                note_ids=[temp_note_id]
            )
            
            # 检查是否有处理错误
            if note_results and note_results[0].error:
                logger.error(f"UnifiedTagProcessor failed for node {node_id}: {note_results[0].error}")
                return entities
            
            # 转换 TagResult 为 ExtractedEntity
            if note_results and note_results[0].tags:
                for tag_result in note_results[0].tags:
                    entity = self._convert_tag_result_to_extracted_entity(tag_result, node_id)
                    entities.append(entity)
            
            logger.debug(f"Successfully parsed {len(entities)} entities using UnifiedTagProcessor")
            
        except Exception as e:
            logger.error(f"Unexpected error in _parse_llm_result for node {node_id}: {e}")
            
        return entities

    async def extract_from_org_node(
        self,
        node_content: str,
        node_id: str,
        existing_tags: Optional[List[str]] = None
    ) -> List[ExtractedEntity]:
        """
        Performs the full entity extraction process for a single org-mode node.

        This process involves:
        1. Creating a prompt using the unified `create_prompt` function.
        2. Calling the LLM with the generated prompt.
        3. Parsing the structured JSON result from the LLM.
        4. Returning the list of extracted entities.

        Args:
            node_content: The text content of the org-mode node.
            node_id: The unique identifier for the org-mode node.
            existing_tags: A list of tags already associated with the node to avoid duplicates.

        Returns:
            A list of ExtractedEntity objects.
        """
        logger.debug(f"Starting entity extraction for node_id: {node_id}")

        if not node_content.strip():
            logger.debug("Node content is empty, skipping extraction.")
            return []
        
        # 1. Create the prompt
        prompt = create_prompt(
            contents=[node_content],
            entity_types=self.entity_types,
            existing_tags_lists=[existing_tags or []]
        )

        # 2. Call the LLM
        try:
            llm_response = await self.llm_async_callable(prompt)
        except Exception as e:
            logger.error(f"LLM call failed during entity extraction for node {node_id}: {e}")
            return []

        if not llm_response:
            logger.warning(f"LLM returned an empty response for node {node_id}.")
            return []

        # 3. Parse the result
        extracted_entities = await self._parse_llm_result(llm_response, node_id)

        logger.info(f"Extracted {len(extracted_entities)} entities from node {node_id}.")
        return extracted_entities

# Example usage for testing
async def main_test_entity_extractor():
    """A simple main function to test the entity extractor."""
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

    # A mock LLM callable for testing purposes
    async def mock_llm_callable(prompt: str) -> str:
        print("\n--- LLM Prompt ---\n")
        print(prompt)
        print("\n--- Mock LLM Response ---\n")
        # Simulate a valid JSON response based on the prompt
        mock_response = json.dumps([
            {"tag_name": "Python", "confidence": 0.9, "reasoning": "The text explicitly mentions Python programming."},
            {"tag_name": "Asyncio", "confidence": 0.85, "reasoning": "The example uses asyncio for concurrency."}
        ])
        print(mock_response)
        return mock_response

    extractor = OrgSupertagEntityExtractor(llm_async_callable=mock_llm_callable)

    test_content = """
    This is a test document about Python programming.
    We are using the asyncio library to handle concurrent tasks.
    It's important for modern application development.
    Existing tags should be respected.
    """
    test_node_id = "test-node-123"
    existing_tags = ["Programming"]

    entities = await extractor.extract_from_org_node(
        node_content=test_content,
        node_id=test_node_id,
        existing_tags=existing_tags
    )

    print(f"\n--- Final Extracted Entities ({len(entities)}) ---")
    for entity in entities:
        print(entity)

if __name__ == '__main__':
    # To run this test, execute `python -m simtag.core.entity_extractor` from the project root
    asyncio.run(main_test_entity_extractor()) 
"""
Org SuperTag Entity Extractor Module

This module is responsible for extracting entities from org-mode content
by leveraging a unified, prompt-based approach with an LLM.
"""
import asyncio
import logging
import json
from typing import Dict, List, Any, Optional, Callable, Awaitable, Tuple
from dataclasses import dataclass, field
from abc import ABC, abstractmethod

# Import unified prompt management and tag processor
try:
    from ..prompts import create_prompt, DEFAULT_ENTITY_TYPES, INFER_RELATIONS_PROMPT
    from ..utils.unified_tag_processor import UnifiedTagProcessor, TagResult, parse_llm_json_response
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
    """Represents a single entity extracted from the text."""
    name: str
    type: str = "concept"  # Default type, can be PERSON, ORGANIZATION, etc.
    confidence: Optional[float] = None
    reasoning: Optional[str] = None
    source_node_id: Optional[str] = None # ID of the org-node from which it was extracted
    attributes: Dict[str, Any] = field(default_factory=dict) # For additional properties like gender, occupation, etc.

@dataclass
class ExtractedRelation:
    """Represents a single relationship extracted from the text."""
    source: str
    target: str
    type: str = "RELATED_TO" # Default relation type, can be IS_ALIAS_OF, HAS_PART, etc.
    description: Optional[str] = None
    confidence: Optional[float] = None
    reasoning: Optional[str] = None
    source_node_id: Optional[str] = None # ID of the org-node from which it was extracted
    properties: Dict[str, Any] = field(default_factory=dict) # For additional relation properties

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

    async def extract(self, text: str, existing_entities: Optional[List[str]] = None,
                      task_type: str = "extract_entities_relations",
                      **kwargs: Any) -> Dict[str, List[Any]]:
        """
        Extracts entities and relations from the given text based on task_type.

        Args:
            text: The text content to analyze.
            existing_entities: A list of known entity names to provide context.
            task_type: The type of extraction task (e.g., "extract_entities_relations",
                       "entity_merging", "relationship_extraction", "attribute_filling",
                       "entity_disambiguation").
            **kwargs: Additional arguments specific to the task_type (e.g., entities_to_consider,
                      ambiguous_entity_name, entity_name).

        Returns:
            A dictionary containing 'entities' and 'relations' found in the text,
            or other structured data based on task_type.
            Returns empty lists/dicts if extraction fails or text is empty.
        """
        if not text or not text.strip():
            return {"entities": [], "relations": []} # Default return for entity/relation extraction

        prompt = ""
        if task_type == "extract_entities_relations":
            prompt = INFER_RELATIONS_PROMPT.format(
                text_content=text,
                existing_entities=", ".join(existing_entities) if existing_entities else "N/A"
            )
        elif task_type == "entity_merging":
            prompt = ENTITY_MERGING_PROMPT.format(
                context_text=text, # Here text is the context
                entities_to_consider=kwargs.get("entities_to_consider", "N/A")
            )
        elif task_type == "relationship_extraction":
            prompt = RELATIONSHIP_EXTRACTION_PROMPT.format(
                text_content=text,
                # entities_to_consider=kwargs.get("entities_to_consider", "N/A") # Not in this prompt
            )
        elif task_type == "attribute_filling":
            prompt = ATTRIBUTE_FILLING_PROMPT.format(
                entity_name=kwargs.get("entity_name", "N/A"),
                text_content=text
            )
        elif task_type == "entity_disambiguation":
            prompt = ENTITY_DISAMBIGUATION_PROMPT.format(
                ambiguous_entity_name=kwargs.get("ambiguous_entity_name", "N/A"),
                contexts=text # Here text is the contexts
            )
        else:
            logger.error(f"Unknown task_type: {task_type}")
            return {"entities": [], "relations": []}

        try:
            inference_model = self.config.get("inference_model")
            response_text = await self.llm_client.generate(prompt, model=inference_model)
            parsed_data = parse_llm_json_response(response_text)

            if not parsed_data:
                logger.error(f"Failed to parse LLM response for task_type {task_type}.")
                return {"entities": [], "relations": []} # Default return

            # --- Process parsed_data based on task_type ---
            if task_type == "extract_entities_relations":
                entities = [ExtractedEntity(**e) for e in parsed_data.get("entities", []) if isinstance(e, dict)]
                relations = [ExtractedRelation(**r) for r in parsed_data.get("relations", []) if isinstance(r, dict)]
                return {"entities": entities, "relations": relations}
            elif task_type == "entity_merging":
                # Expected: List of dicts, each with primary_name, aliases, reasoning, confidence
                return {"merge_suggestions": parsed_data}
            elif task_type == "relationship_extraction":
                # Expected: List of dicts, each with source, target, relationship_type, description, confidence
                relations = [ExtractedRelation(
                    source=r.get('source'),
                    target=r.get('target'),
                    type=r.get('relationship_type', 'RELATED_TO'),
                    description=r.get('description'),
                    confidence=r.get('confidence'),
                    source_node_id=kwargs.get('source_node_id') # Pass source_node_id if available
                ) for r in parsed_data if isinstance(r, dict)]
                return {"relations": relations}
            elif task_type == "attribute_filling":
                # Expected: Dict with entity_name as key, and attributes dict as value
                return {"attributes": parsed_data}
            elif task_type == "entity_disambiguation":
                # Expected: Dict with is_ambiguous, disambiguations, reasoning, confidence
                return {"disambiguation_result": parsed_data}
            else:
                logger.warning(f"Unhandled task_type for parsing: {task_type}. Returning raw parsed data.")
                return {"raw_parsed_data": parsed_data}

        except Exception as e:
            logger.error(f"Failed to extract entities/relations for task_type {task_type}: {e}", exc_info=True)
            return {"entities": [], "relations": []} # Default return

    

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
        # self.llm_async_callable = llm_async_callable # No longer directly used
        self.config = entity_extractor_config if entity_extractor_config else {}

        # Use the new LLMEntityExtractor internally
        self.llm_entity_extractor = LLMEntityExtractor(
            llm_client=LLMClient(config_dict=self.config), # Assuming config has llm_client_config
            config=self.config
        )
        
        self.entity_types = self.config.get('entity_types', DEFAULT_ENTITY_TYPES)
        
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
            # Use the unified processor to process LLM response
            # For a single node, create a temporary note_id list
            temp_note_id = node_id or "temp_node"
            note_results = self.unified_processor.process_llm_response(
                response_str=llm_output,
                note_ids=[temp_note_id]
            )
            
            # Check for processing errors
            if note_results and note_results[0].error:
                logger.error(f"UnifiedTagProcessor failed for node {node_id}: {note_results[0].error}")
                return entities
            
            # Convert TagResult to ExtractedEntity
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
        
        # Use the new LLMEntityExtractor for the actual extraction
        extraction_result = await self.llm_entity_extractor.extract(
            text=node_content,
            existing_entities=existing_tags,
            task_type="extract_entities_relations" # Specify the task type
        )
        
        # Convert the result from LLMEntityExtractor (which returns dicts) to ExtractedEntity objects
        extracted_entities = [ExtractedEntity(**e) for e in extraction_result.get("entities", []) if isinstance(e, dict)]

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
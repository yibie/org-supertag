"""
Org SuperTag Entity Extractor Module

This module is responsible for extracting entities and relationships 
from org-mode content, leveraging LLMs and potentially existing tagging engines.
"""
import asyncio
import logging
import re
from typing import Dict, List, Any, Optional, Tuple, Callable, Awaitable
from dataclasses import dataclass, field

# Assuming LLMClient and potentially TaggingEngine will be defined elsewhere
# from ..services.ollama import OllamaService # Or a more general LLMClient
# from .tagging import TaggingEngine

logger = logging.getLogger(__name__)

@dataclass
class ExtractedEntity:
    id: str # Globally unique ID for this instance of extraction, or canonical ID if available
    name: str
    type: str
    description: Optional[str] = None
    source_node_id: Optional[str] = None # ID of the org-node from which it was extracted
    attributes: Dict[str, Any] = field(default_factory=dict)
    # Confidence score from the extractor, if available
    confidence: Optional[float] = None 

@dataclass
class ExtractedRelation:
    id: str # Globally unique ID for this instance of extraction
    source_entity_id: str # ID of the source ExtractedEntity
    target_entity_id: str # ID of the target ExtractedEntity
    type: str # Type of relationship (e.g., RELATED_TO, USES, IS_A)
    description: Optional[str] = None
    source_node_id: Optional[str] = None
    attributes: Dict[str, Any] = field(default_factory=dict)
    # Confidence score from the extractor, if available
    confidence: Optional[float] = None 

class OrgSupertagEntityExtractor:
    """
    Extracts entities and relationships from org-mode node content.
    Follows the design in living-doc-features.org (10.7.A.1).
    """

    DEFAULT_ENTITY_TYPES = [
        "concept", "person", "project", "method", "resource", 
        "org_tag", "temporal", "domain", "task", "event", "location",
        "organization", "software", "hardware", "api", "metric", "dataset"
    ]

    def __init__(self, 
                 llm_async_callable: Callable[[str], Awaitable[str]], 
                 entity_extractor_config: Optional[Dict[str, Any]] = None,
                 tagging_engine: Optional[Any] = None): # Keep tagging_engine for now if needed
        """
        Initializes the OrgSupertagEntityExtractor.

        Args:
            llm_async_callable: An asynchronous function that takes a prompt string 
                                and returns the LLM completion string.
            entity_extractor_config: Configuration dictionary for the extractor.
                                     Expected keys:
                                     - 'entity_types': Optional[List[str]] or List[Dict[str,str]]
                                     - 'extraction_prompt_template': Optional[str]
                                     - 'gleaning_prompt_template': Optional[str]
                                     - 'llm_model_override': Optional[str] (for specific extraction LLM)
                                     - 'max_gleaning_rounds': int (default 2)
            tagging_engine: Optional instance of TaggingEngine for auxiliary tasks.
        """
        self.llm_async_callable = llm_async_callable
        self.tagging_engine = tagging_engine # Placeholder for now
        
        config = entity_extractor_config if entity_extractor_config else {}

        # Parse entity types: can be List[str] or List[Dict[str,str]] containing names
        raw_entity_types = config.get('entity_types')
        parsed_entity_types: List[str] = []
        if isinstance(raw_entity_types, list):
            for item in raw_entity_types:
                if isinstance(item, str):
                    parsed_entity_types.append(item)
                elif isinstance(item, dict) and 'name' in item and isinstance(item['name'], str):
                    parsed_entity_types.append(item['name'])
        
        self.entity_types = parsed_entity_types if parsed_entity_types else self.DEFAULT_ENTITY_TYPES
        
        self.max_gleaning_rounds = config.get('max_gleaning_rounds', 2)
        self.extraction_prompt_template_override = config.get('extraction_prompt_template') # Store for use in _extract_base_entities_llm
        self.gleaning_prompt_template_override = config.get('gleaning_prompt_template') # Store for use in _gleaning_round_llm
        self.llm_model_override = config.get('llm_model_override') # Store for use in llm calls if needed

        logger.info(f"OrgSupertagEntityExtractor initialized. Gleaning rounds: {self.max_gleaning_rounds}. Entity types: {self.entity_types}")
        if self.llm_model_override:
            logger.info(f"Using LLM model override for extraction: {self.llm_model_override}")

    def _generate_entity_id(self, entity_name: str, entity_type: str, node_id: Optional[str] = None) -> str:
        """Generates a somewhat stable ID for an entity based on its name, type, and optionally node_id."""
        base_id = f"{entity_type.upper()}_{entity_name.replace(' ', '_').upper()}"
        if node_id:
            return f"{node_id}_{base_id}"
        return base_id

    def _generate_relation_id(self, src_id: str, tgt_id: str, rel_type: str, node_id: Optional[str]=None) -> str:
        """Generates a somewhat stable ID for a relation."""
        base_id = f"REL_{src_id}_{tgt_id}_{rel_type.upper()}"
        if node_id:
            return f"{node_id}_{base_id}"
        return base_id

    async def _parse_llm_extraction_result(self, llm_output: str, node_id: Optional[str]) -> Tuple[List[ExtractedEntity], List[ExtractedRelation]]:
        """
        Parses the LLM output string into lists of ExtractedEntity and ExtractedRelation objects.
        Expected LLM output format (example from living-doc-features.org):
        ("entity"|entity_name|entity_type|description)
        ("relationship"|source_entity_name|target_entity_name|relation_description|keywords_as_type|strength_as_confidence)
        Uses ## as record delimiter and <|> as tuple delimiter.
        """
        entities: List[ExtractedEntity] = []
        relations_raw: List[Dict[str, Any]] = [] # Store raw relations to map names to IDs later
        entity_name_to_id_map: Dict[str, str] = {}

        records = llm_output.split('##')

        for record in records:
            record = record.strip()
            if not record:
                continue
            
            parts = record.split('<|>')
            if not parts:
                continue

            record_type_token = parts[0].lower().strip('()\"\'')
            
            try:
                if record_type_token == "entity" and len(parts) >= 4:
                    raw_name = parts[1].strip().strip('\"\'')
                    raw_type = parts[2].strip().strip('\"\'')
                    description = parts[3].strip().strip('()\"\'')
                    
                    if not raw_name or not raw_type:
                        logger.warning(f"Skipping entity with missing name or type in record: {record}")
                        continue

                    # Normalize entity type against known types if possible
                    entity_type_normalized = raw_type if raw_type in self.entity_types else "concept" # Default
                    if raw_type not in self.entity_types:
                        logger.debug(f"LLM proposed entity type '{raw_type}' not in predefined list. Normalized to '{entity_type_normalized}'. Original was: {raw_name}")

                    entity_id = self._generate_entity_id(raw_name, entity_type_normalized, node_id)
                    entity_name_to_id_map[raw_name.lower()] = entity_id # For relation mapping
                    
                    entities.append(ExtractedEntity(
                        id=entity_id,
                        name=raw_name,
                        type=entity_type_normalized,
                        description=description,
                        source_node_id=node_id
                    ))
                elif record_type_token == "relationship" and len(parts) >= 6:
                    src_name = parts[1].strip().strip('\"\'')
                    tgt_name = parts[2].strip().strip('\"\'')
                    rel_desc = parts[3].strip().strip('()\"\'')
                    rel_type_keywords = parts[4].strip().strip('\"\'') # Use as relation type
                    rel_confidence_str = parts[5].strip().strip('()\"\'')
                    
                    if not src_name or not tgt_name or not rel_type_keywords:
                        logger.warning(f"Skipping relation with missing src/tgt name or type in record: {record}")
                        continue

                    confidence = None
                    try:
                        confidence = float(rel_confidence_str)
                    except ValueError:
                        logger.debug(f"Could not parse confidence '{rel_confidence_str}' for relation as float.")

                    relations_raw.append({
                        'src_name': src_name,
                        'tgt_name': tgt_name,
                        'type': rel_type_keywords, # Directly use keywords as type
                        'description': rel_desc,
                        'confidence': confidence,
                        'source_node_id': node_id
                    })
                else:
                    logger.debug(f"Skipping unrecognized record format: {record}")
            except IndexError as e:
                logger.warning(f"Error parsing record '{record}': {e}")
            except Exception as e:
                logger.error(f"Unexpected error parsing record '{record}': {e}")

        # Second pass to create ExtractedRelation objects with mapped entity IDs
        final_relations: List[ExtractedRelation] = []
        for rel_raw in relations_raw:
            src_id = entity_name_to_id_map.get(rel_raw['src_name'].lower())
            tgt_id = entity_name_to_id_map.get(rel_raw['tgt_name'].lower())

            if src_id and tgt_id:
                relation_id = self._generate_relation_id(src_id, tgt_id, rel_raw['type'], node_id)
                final_relations.append(ExtractedRelation(
                    id=relation_id,
                    source_entity_id=src_id,
                    target_entity_id=tgt_id,
                    type=rel_raw['type'],
                    description=rel_raw['description'],
                    confidence=rel_raw['confidence'],
                    source_node_id=rel_raw['source_node_id']
                ))
            else:
                logger.warning(f"Could not map entity names to IDs for relation: {rel_raw['src_name']} -> {rel_raw['tgt_name']}")
        
        return entities, final_relations

    async def _extract_base_entities_llm(self, content: str, node_id: Optional[str]) -> Tuple[List[ExtractedEntity], List[ExtractedRelation]]:
        """Performs the initial entity and relation extraction using LLM."""
        entity_types_str = ", ".join(self.entity_types)
        
        # Use override template if available, otherwise use default
        if self.extraction_prompt_template_override:
            prompt = self.extraction_prompt_template_override.format(
                entity_types_str=entity_types_str,
                content=content
            )
        else:
            prompt = f"""
            Analyze the following text from an org-mode document. Your goal is to identify entities and their relationships.

            Available Entity Types: {entity_types_str}.
            Prioritize these types. If a suitable type is not listed, you may use a closely related common type.

            Text Content:
            --- BEGIN TEXT ---
            {content}
            --- END TEXT ---

            Output Format Instructions:
            1.  Represent each entity as: ("entity"<|>ENTITY_NAME<|>ENTITY_TYPE<|>DESCRIPTION_OF_ENTITY)
            2.  Represent each relationship as: ("relationship"<|>SOURCE_ENTITY_NAME<|>TARGET_ENTITY_NAME<|>RELATIONSHIP_DESCRIPTION<|>RELATIONSHIP_TYPE_KEYWORDS<|>CONFIDENCE_SCORE_0_TO_1)
            3.  Use '##' as a delimiter between records (i.e., after each entity or relationship entry).
            4.  ENTITY_NAME, ENTITY_TYPE, etc. should be the actual extracted values.
            5.  RELATIONSHIP_TYPE_KEYWORDS should be a concise term like 'IS_A', 'USES_TOOL', 'PART_OF', 'DEFINES', 'REFERENCES'.
            6.  CONFIDENCE_SCORE should be a float between 0.0 and 1.0 indicating your certainty about the relationship.

            Example Output:
            ("entity"<|>Machine Learning<|>concept<|>A field of artificial intelligence that uses statistical techniques.)##
            ("entity"<|>Python<|>software<|>A programming language widely used in Machine Learning.)##
            ("relationship"<|>Machine Learning<|>Python<|>Machine Learning frequently utilizes Python for implementation.<|>USES_TOOL<|>0.9)##
            
            Begin extraction:
            """
        
        logger.debug(f"Sending base extraction prompt to LLM for node {node_id}:\n{prompt[:500]}...")
        
        # If self.llm_model_override is set, pass it to the llm_async_callable.
        # This assumes llm_async_callable (i.e., LLMClient.generate) accepts a 'model' kwarg.
        if self.llm_model_override:
            llm_output = await self.llm_async_callable(prompt, model=self.llm_model_override)
        else:
            llm_output = await self.llm_async_callable(prompt)
        
        logger.debug(f"LLM output for base extraction node {node_id}: {llm_output[:500]}...")
        
        return await self._parse_llm_extraction_result(llm_output, node_id)

    async def _gleaning_round_llm(self, content: str, extracted_entities: List[ExtractedEntity], 
                                 extracted_relations: List[ExtractedRelation], node_id: Optional[str]) -> Tuple[List[ExtractedEntity], List[ExtractedRelation]]:
        """Performs a gleaning round to find additional entities and relations based on existing ones."""
        if not extracted_entities and not extracted_relations:
            logger.info("Skipping gleaning round as no prior entities/relations exist.")
            return [], []

        existing_entity_names = [e.name for e in extracted_entities]
        # Could also include relation details in the prompt if useful
        
        if self.gleaning_prompt_template_override:
            prompt = self.gleaning_prompt_template_override.format(
                existing_entity_names_str = ", ".join(existing_entity_names),
                content=content,
                entity_types_str=", ".join(self.entity_types) # Ensure entity_types_str is available if template uses it
            )
        else:
            prompt = f"""
            You are performing a second pass analysis (gleaning round) on an org-mode document's text.
            Previously, these entities were extracted: {', '.join(existing_entity_names)}
            
            Available Entity Types: {", ".join(self.entity_types)}.

            Original Text Content:
            --- BEGIN TEXT ---
            {content}
            --- END TEXT ---

            Task: Review the original text AGAIN. Identify any ADDITIONAL entities or relationships that were missed in the first pass, 
            especially those that connect to or expand upon the already identified entities. 
            Focus on technical terms, org-mode specific concepts (like tags if they appear as text), 
            and cross-references if any are apparent in the text.
            Use the same output format as the initial extraction (entities as ("entity"...) and relations as ("relationship"...)).
            If no new items are found, output "NO_NEW_ITEMS_FOUND".

            Begin gleaning pass:
            """
        
        logger.debug(f"Sending gleaning prompt to LLM for node {node_id}:\n{prompt[:500]}...")
        
        if self.llm_model_override:
            llm_output = await self.llm_async_callable(prompt, model=self.llm_model_override)
        else:
            llm_output = await self.llm_async_callable(prompt)
            
        logger.debug(f"LLM output for gleaning round node {node_id}: {llm_output[:500]}...")
        
        if "NO_NEW_ITEMS_FOUND" in llm_output:
            return [], []
            
        return await self._parse_llm_extraction_result(llm_output, node_id)

    def _integrate_org_tags_as_entities(self, org_tags: List[str], node_id: str) -> List[ExtractedEntity]:
        """Converts org-mode tags into ExtractedEntity objects."""
        tag_entities: List[ExtractedEntity] = []
        for tag_name in org_tags:
            if not tag_name or not tag_name.strip():
                continue
            entity_id = self._generate_entity_id(tag_name, "org_tag", node_id)
            tag_entities.append(ExtractedEntity(
                id=entity_id,
                name=tag_name,
                type="org_tag",
                description=f"Org-mode tag: {tag_name}",
                source_node_id=node_id
            ))
        return tag_entities

    def _create_relations_from_tags_to_content(self, 
                                               tag_entities: List[ExtractedEntity], 
                                               content_entities: List[ExtractedEntity],
                                               node_id: str) -> List[ExtractedRelation]:
        """Creates relations between org_tag entities and content entities within the same node."""
        relations: List[ExtractedRelation] = []
        if not tag_entities or not content_entities:
            return relations

        for tag_entity in tag_entities:
            for content_entity in content_entities:
                # Avoid self-relation for tags if a tag somehow became a content_entity name
                if tag_entity.id == content_entity.id: 
                    continue
                
                # Simple heuristic: if tag name is part of content entity name or description (case-insensitive)
                # This could be made more sophisticated (e.g., semantic similarity, LLM-based check)
                related = False
                if tag_entity.name.lower() in content_entity.name.lower():
                    related = True
                elif content_entity.description and tag_entity.name.lower() in content_entity.description.lower():
                    related = True
                
                # Predefined semantic mappings (example from living-doc)
                # This is a simplified version. A more robust system might use an external mapping or LLM call.
                semantic_mappings = {
                    'ai': ['artificial intelligence', 'machine learning', 'neural network'],
                    'nlp': ['natural language', 'text processing', 'language model'],
                    'ml': ['machine learning', 'algorithm', 'model', 'training'],
                    'deep learning': ['neural network', 'cnn', 'rnn', 'transformer']
                }
                if tag_entity.name.lower() in semantic_mappings:
                    for keyword in semantic_mappings[tag_entity.name.lower()]:
                        if keyword in content_entity.name.lower() or \
                           (content_entity.description and keyword in content_entity.description.lower()):
                            related = True
                            break
                
                if related:
                    relation_id = self._generate_relation_id(tag_entity.id, content_entity.id, "TAG_CONTEXTUALIZES", node_id)
                    relations.append(ExtractedRelation(
                        id=relation_id,
                        source_entity_id=tag_entity.id,
                        target_entity_id=content_entity.id,
                        type="TAG_CONTEXTUALIZES", # Or SEMANTICALLY_RELATED as in living-doc
                        description=f"Org-tag '{tag_entity.name}' provides context for entity '{content_entity.name}' in node {node_id}.",
                        source_node_id=node_id,
                        confidence=0.7 # Default confidence for this heuristic
                    ))
        return relations

    async def extract_from_org_node(
        self, node_content: str, node_id: str, org_tags: Optional[List[str]] = None
    ) -> Tuple[List[ExtractedEntity], List[ExtractedRelation]]:
        """
        Main method to extract entities and relations from a single org-mode node's content.
        Includes base extraction, gleaning rounds, and integration of org-tags.

        Args:
            node_content: The textual content of the org-mode node.
            node_id: A unique identifier for the org-mode node.
            org_tags: Optional list of org-mode tags associated with the node.

        Returns:
            A tuple containing a list of ExtractedEntity objects and a list of ExtractedRelation objects.
        """
        if not node_content.strip():
            logger.info(f"Node {node_id} has no content to extract from.")
            # Still process tags if provided
            if org_tags:
                tag_entities = self._integrate_org_tags_as_entities(org_tags, node_id)
                return tag_entities, []
            return [], []

        all_entities_map: Dict[str, ExtractedEntity] = {}
        all_relations_map: Dict[str, ExtractedRelation] = {}

        # 1. Base Extraction
        logger.info(f"Starting base entity extraction for node {node_id}...")
        base_entities, base_relations = await self._extract_base_entities_llm(node_content, node_id)
        for entity in base_entities:
            all_entities_map[entity.id] = entity
        for relation in base_relations:
            all_relations_map[relation.id] = relation
        logger.info(f"Base extraction for node {node_id} found {len(base_entities)} entities, {len(base_relations)} relations.")

        # 2. Gleaning Rounds
        current_entities_for_gleaning = list(all_entities_map.values())
        current_relations_for_gleaning = list(all_relations_map.values())
        for i in range(self.max_gleaning_rounds):
            logger.info(f"Starting gleaning round {i+1}/{self.max_gleaning_rounds} for node {node_id}...")
            gleaned_entities, gleaned_relations = await self._gleaning_round_llm(
                node_content, current_entities_for_gleaning, current_relations_for_gleaning, node_id
            )
            if not gleaned_entities and not gleaned_relations:
                logger.info(f"Gleaning round {i+1} for node {node_id} yielded no new information. Stopping gleaning.")
                break
            
            new_entities_count = 0
            for entity in gleaned_entities:
                if entity.id not in all_entities_map:
                    all_entities_map[entity.id] = entity
                    new_entities_count +=1
            
            new_relations_count = 0
            for relation in gleaned_relations:
                if relation.id not in all_relations_map:
                    all_relations_map[relation.id] = relation
                    new_relations_count += 1
            
            logger.info(f"Gleaning round {i+1} for node {node_id} added {new_entities_count} new entities, {new_relations_count} new relations.")
            if new_entities_count == 0 and new_relations_count == 0: # no new unique items
                logger.info(f"Gleaning round {i+1} for node {node_id} added no unique items after merging. Stopping gleaning.")
                break
            
            # Update lists for the next potential gleaning round prompt
            current_entities_for_gleaning = list(all_entities_map.values())
            current_relations_for_gleaning = list(all_relations_map.values())
        else: # Executed if loop finishes without break
            logger.info(f"Completed all {self.max_gleaning_rounds} gleaning rounds for node {node_id}.")

        # 3. Integrate org-mode tags as entities
        tag_entities: List[ExtractedEntity] = []
        if org_tags:
            logger.info(f"Integrating {len(org_tags)} org-tags for node {node_id} as entities.")
            tag_entities = self._integrate_org_tags_as_entities(org_tags, node_id)
            for tag_entity in tag_entities:
                if tag_entity.id not in all_entities_map: # Add if not already extracted by LLM somehow
                    all_entities_map[tag_entity.id] = tag_entity
        
        # 4. Create relations from org_tag entities to content entities
        if tag_entities and all_entities_map:
            # Use all content entities (excluding the tags themselves if they were added to all_entities_map)
            content_entities_for_linking = [e for e in all_entities_map.values() if e.type != 'org_tag' or e not in tag_entities]
            if content_entities_for_linking:
                logger.info(f"Creating relations between {len(tag_entities)} org-tags and {len(content_entities_for_linking)} content entities for node {node_id}.")
                tag_to_content_relations = self._create_relations_from_tags_to_content(
                    tag_entities, content_entities_for_linking, node_id
                )
                for relation in tag_to_content_relations:
                    if relation.id not in all_relations_map:
                        all_relations_map[relation.id] = relation
                logger.info(f"Created {len(tag_to_content_relations)} relations between tags and content for node {node_id}.")

        final_entities = list(all_entities_map.values())
        final_relations = list(all_relations_map.values())
        
        logger.info(f"Total extraction for node {node_id}: {len(final_entities)} entities, {len(final_relations)} relations.")
        return final_entities, final_relations

# Example usage / test function
async def main_test_entity_extractor():
    import asyncio
    from simtag.config import Config
    from simtag.services.llm_client import LLMClient # For a real test, or mock it

    # Basic logger for test output
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    test_logger = logging.getLogger("entity_extractor_test")

    test_logger.info("Starting Entity Extractor Test")

    # 1. Setup Config and LLMClient (or a mock)
    # For a real test, ensure Ollama is running and configured.
    # For a predictable unit test, mock llm_async_callable.
    config = Config()
    # Ensure a default llm_client_config for the test if not fully set up elsewhere
    if not config.llm_client_config.get('base_url'): 
        config.llm_client_config['base_url'] = "http://localhost:11434"
        config.llm_client_config['default_model'] = "gemma:2b" # Use a small model for testing
        config.llm_client_config['default_embedding_model'] = "nomic-embed-text"

    # Mock the LLM callable for predictable output
    async def mock_llm_callable(prompt: str, model: Optional[str] = None) -> str:
        test_logger.info(f"Mock LLM called with model: {model if model else 'default'}")
        test_logger.info(f"Mock LLM Prompt (first 200 chars): {prompt[:200]}...")
        # Simulate LLM output for entities and relations
        # Based on the expected format in _parse_llm_extraction_result
        return (
            '("entity"<|>Knowledge Management<|>concept<|>The process of creating, sharing, using and managing the knowledge and information of an organization.)##'
            '("entity"<|>Zettelkasten<|>method<|>A note-taking method.)##'
            '("relationship"<|>Knowledge Management<|>Zettelkasten<|>Zettelkasten is a method for Knowledge Management.<|>USES_METHOD<|>0.85)##'
        )

    # You could also use a real LLMClient for integration testing
    # llm_client = LLMClient(llm_config=config.llm_client_config)
    # if not await llm_client.check_availability():
    #     test_logger.error("LLM Service not available. Aborting test.")
    #     await llm_client.close()
    #     return
    # llm_callable = llm_client.generate

    llm_callable = mock_llm_callable

    # 2. Initialize EntityExtractor
    # Use a specific entity_extractor_config for the test if desired
    test_ee_config = {
        "entity_types": ["concept", "method", "person", "project"], # Simplified list for test
        "max_gleaning_rounds": 1
    }
    extractor = OrgSupertagEntityExtractor(
        llm_async_callable=llm_callable,
        entity_extractor_config=test_ee_config
    )

    # 3. Sample content for extraction
    sample_org_content = ("""
    * Understanding Zettelkasten for Knowledge Management
      :PROPERTIES:
      :ID:       node_abc_123
      :END:
      The Zettelkasten method is a powerful technique for personal knowledge management. 
      It was popularized by Niklas Luhmann. It helps in creating a network of thoughts.
    """)
    sample_node_id = "test_node_001"
    sample_org_tags = ["pkm", "writing"]

    test_logger.info(f"Extracting from content (ID: {sample_node_id}):\n{sample_org_content}")

    # 4. Call extraction method
    try:
        entities, relations = await extractor.extract_from_org_node(
            node_content=sample_org_content,
            node_id=sample_node_id,
            org_tags=sample_org_tags
        )
    except Exception as e:
        test_logger.error(f"Error during entity extraction: {e}", exc_info=True)
        # if 'llm_client' in locals(): await llm_client.close()
        return

    # 5. Print results
    test_logger.info("--- Extracted Entities ---")
    if entities:
        for entity in entities:
            test_logger.info(f"  ID: {entity.id}, Name: {entity.name}, Type: {entity.type}, Desc: {entity.description}, Source: {entity.source_node_id}")
    else:
        test_logger.info("  No entities extracted.")

    test_logger.info("--- Extracted Relations ---")
    if relations:
        for relation in relations:
            test_logger.info(f"  ID: {relation.id}, Src: {relation.source_entity_id}, Tgt: {relation.target_entity_id}, Type: {relation.type}, Desc: {relation.description}")
    else:
        test_logger.info("  No relations extracted.")
    
    test_logger.info("--- Org Tags Integrated as Entities (if any) ---")
    # This part tests _integrate_org_tags_as_entities implicitly called by extract_from_org_node
    # Search for entities of type 'org_tag' that match sample_org_tags
    org_tag_entities_found = [e for e in entities if e.type == 'org_tag' and e.name in sample_org_tags]
    if org_tag_entities_found:
        for tag_entity in org_tag_entities_found:
            test_logger.info(f"  Found org_tag entity: Name: {tag_entity.name}, ID: {tag_entity.id}")
    else:
        test_logger.info("  No specific 'org_tag' type entities matching input tags were found in the main list.")
        test_logger.info(f"  (Note: Check if the mock LLM output or parsing logic creates these as 'org_tag')")

    # if 'llm_client' in locals(): await llm_client.close() # If using real LLMClient
    test_logger.info("Entity Extractor Test Finished")

if __name__ == '__main__':
    asyncio.run(main_test_entity_extractor()) 
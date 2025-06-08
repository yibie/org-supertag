"""
Org SuperTag RAG Engine Module

This module implements the RAG (Retrieval Augmented Generation) engine 
for org-supertag, providing different strategies for context retrieval.
"""
import asyncio
import logging
from typing import Dict, List, Any, Optional, Tuple, TypedDict
import os

# Corrected and new imports
from simtag.config import Config
from .graph_store import OrgSupertagKnowledgeGraph, Entity as GraphEntity, Relation as GraphRelation # Renamed to avoid conflict if any local Entity
from .storage import VectorStorage
from ..services.llm_client import LLMClient # Assuming simtag/services/llm_client.py
from .entity_extractor import ExtractedEntity

logger = logging.getLogger(__name__)

class StructuredContextOutput(TypedDict):
    """
    Defines the structured output from retrieval methods.
    """
    entities: List[GraphEntity]
    relations: List[GraphRelation]
    documents: List[Dict[str, Any]] # Existing document format


class OrgSupertagRAGEngine:
    """
    Implements the RAG engine with naive, light, and mini query strategies.
    Integrates Knowledge Graph and Vector Storage.
    Based on living-doc-features.org section 10.7.A.3
    """

    def __init__(self,
                 knowledge_graph: OrgSupertagKnowledgeGraph,
                 vector_storage: VectorStorage,
                 llm_client: LLMClient,
                 config: Config
                 ):
        """
        Initializes the OrgSupertagRAGEngine.

        Args:
            knowledge_graph: Instance of OrgSupertagKnowledgeGraph.
            vector_storage: Instance of a vector storage solution.
            llm_client: Client for interacting with an LLM for generation.
            config: Configuration object.
        """
        self.knowledge_graph = knowledge_graph
        self.vector_storage = vector_storage
        self.llm_client = llm_client
        self.config = config
        logger.info("OrgSupertagRAGEngine initialized.")

    async def _naive_retrieval(
        self,
        query_text: str,
        query_embedding: Optional[List[float]], # Kept for now, as vector_storage might use it
        top_k: int
    ) -> StructuredContextOutput: # Changed return type
        """Performs naive vector store retrieval."""
        logger.debug(f"Performing naive retrieval for query: '{query_text[:50]}...'")
        if not self.vector_storage:
            logger.warning("Vector storage is not available for naive retrieval.")
            return StructuredContextOutput(entities=[], relations=[], documents=[])

        raw_documents = await self.vector_storage.query(query_text, top_k=top_k)
        
        standardized_documents: List[Dict[str, Any]] = []
        for i, doc_data in enumerate(raw_documents):
            standardized_doc = {
                "id": doc_data.get("id", f"naive_{query_text[:10]}_{i}"), # Fallback ID generation
                "text": doc_data.get("text", doc_data.get("content", "Content unavailable")),
                "score": doc_data.get("score", 0.0),
                "retrieval_source_type": "vector_search",
                # Preserve other fields from doc_data
                **{k: v for k, v in doc_data.items() if k not in ["id", "text", "score", "retrieval_source_type"]}
            }
            standardized_documents.append(standardized_doc)
        
        logger.debug(f"Naive retrieval found {len(standardized_documents)} documents.")
        return StructuredContextOutput(entities=[], relations=[], documents=standardized_documents)

    async def _light_retrieval(
        self,
        query_text: str,
        query_embedding: Optional[List[float]], # Kept Optional for consistency
        extracted_entities: List[ExtractedEntity],
        top_k: int,
        max_neighbor_depth: int,
        vector_graph_ratio: Optional[float] = None # New parameter for balancing
    ) -> StructuredContextOutput: # Changed return type
        """Performs light retrieval: naive search + K-hop graph neighbors for context."""
        logger.debug(f"Performing light retrieval for query: '{query_text[:50]}...', ratio: {vector_graph_ratio}")

        final_entities: List[GraphEntity] = []
        final_relations: List[GraphRelation] = [] # Remains empty for now
        combined_documents: List[Dict[str, Any]] = []

        # Determine split for naive vs graph results
        _ratio = vector_graph_ratio if vector_graph_ratio is not None and 0.0 <= vector_graph_ratio <= 1.0 else 0.5 # Default to 0.5
        
        num_naive_results = int(top_k * _ratio)
        num_graph_results_target = top_k - num_naive_results
        if top_k == 1 and num_naive_results == 0 : # Ensure at least one if top_k is 1
             num_naive_results = 1
             num_graph_results_target = 0
        elif top_k > 1 and num_naive_results == 0: # Ensure naive gets at least 1 if ratio is too low for it but top_k > 1
            num_naive_results = 1
            num_graph_results_target = top_k - 1

        # 1. Perform naive vector retrieval first
        if num_naive_results > 0:
            naive_output = await self._naive_retrieval(query_text, query_embedding, num_naive_results)
            combined_documents.extend(naive_output.documents)
        else:
            naive_output = StructuredContextOutput(entities=[], relations=[], documents=[]) # Ensure naive_output is defined

        # 2. Enhance with graph context (K-hop neighbors)
        graph_enhanced_docs_raw: List[Dict[str, Any]] = [] # Renamed to raw
        start_entities_for_graph: List[GraphEntity] = []

        if extracted_entities:
            start_entity_ids = [e.id for e in extracted_entities if e.id] 
            
            for ext_entity in extracted_entities:
                if ext_entity.id: 
                    graph_entity_candidate: GraphEntity = {
                        "id": ext_entity.id,
                        "name": ext_entity.name,
                        "type": ext_entity.entity_type, 
                        "description": ext_entity.attributes.get("description", "") if ext_entity.attributes else ""
                    }
                    start_entities_for_graph.append(graph_entity_candidate)
            
            final_entities.extend(start_entities_for_graph)

            if start_entity_ids and num_graph_results_target > 0:
                max_neighbors_calc = num_graph_results_target // len(start_entity_ids) if len(start_entity_ids) > 0 else num_graph_results_target
                max_neighbors_per_node_val = max(1, max_neighbors_calc) 
                
                # Assuming get_neighbor_documents returns a list of document-like dicts
                raw_neighbor_docs = await self.knowledge_graph.get_neighbor_documents(
                    start_entity_ids,
                    depth=max_neighbor_depth,
                    max_neighbors_per_node=max_neighbors_per_node_val 
                )
                # Standardize raw_neighbor_docs
                for i, doc_data in enumerate(raw_neighbor_docs):
                    standardized_neighbor_doc = {
                        "id": doc_data.get("id", f"neighbor_{ext_entity.id}_{i}"), # Fallback ID
                        "text": doc_data.get("text", doc_data.get("content", "Neighbor content unavailable")),
                        "score": doc_data.get("score", 0.70), # Default score for graph neighbors
                        "retrieval_source_type": "graph_neighbor",
                        # Preserve other fields
                        **{k: v for k, v in doc_data.items() if k not in ["id", "text", "score", "retrieval_source_type"]}
                    }
                    graph_enhanced_docs_raw.append(standardized_neighbor_doc) # Append to standardized list

            elif not start_entity_ids and num_graph_results_target > 0:
                logger.debug("Light retrieval: Graph enhancement intended but no start_entity_ids available.")
        
        # Add graph-enhanced documents, avoiding duplicates
        # combined_documents already contains standardized docs from naive_output
        seen_doc_ids = {doc.get('id') for doc in combined_documents if doc.get('id')}
        for doc in graph_enhanced_docs_raw: # Iterate standardized graph docs
            if doc.get('id') not in seen_doc_ids:
                combined_documents.append(doc)
                seen_doc_ids.add(doc.get('id'))

        # Simple sort by score, can be more sophisticated
        # This sort might need adjustment if scores are not uniformly present or comparable
        sorted_documents = sorted(combined_documents, key=lambda x: x.get('score', 0.0), reverse=True)

        logger.debug(f"Light retrieval found {len(final_entities)} entities and {len(sorted_documents)} documents.")
        
        return StructuredContextOutput(
            entities=final_entities, 
            relations=final_relations, 
            documents=sorted_documents[:top_k] # Return top_k overall documents
        )

    async def _mini_retrieval(
        self,
        query_text: str,
        extracted_entities: List[ExtractedEntity],
        target_entity_types: Optional[List[str]],
        top_k: int, # Overall top_k if specific ones aren't provided
        max_reasoning_depth: int,
        top_k_reasoning_paths: Optional[int] = None, # New specific top_k
        top_k_vector_supplement: Optional[int] = None  # New specific top_k
    ) -> StructuredContextOutput:
        """
        Perform RAG retrieval using the 'mini' strategy (GraphRAG-focused).
        Retrieves entities, relations, and supporting documents based on reasoning paths and vector search.
        """
        logger.debug(f"_mini_retrieval called with query: '{query_text}', entities: {extracted_entities}, types: {target_entity_types}")

        final_entities: List[GraphEntity] = []
        final_relations: List[GraphRelation] = []
        final_documents: List[Dict[str, Any]] = []
        
        entity_names_from_query = [entity.name for entity in extracted_entities if entity.name]

        # Determine top_k for paths and vector search
        _top_k_paths = top_k_reasoning_paths if top_k_reasoning_paths is not None else top_k
        _top_k_vector = top_k_vector_supplement if top_k_vector_supplement is not None else top_k
        # If only overall top_k is given, we might split it, e.g., 70% paths, 30% vector, or use it fully for both.
        # For simplicity now, if specific top_k not given, use overall top_k for both calls if they are > 0.
        # A more sophisticated split could be: top_k // 2 for each if specific ones are None.
        # Let's refine this: if specific are None, use the main top_k for paths, and maybe a smaller portion for vector supplement.
        if top_k_reasoning_paths is None:
            _top_k_paths = top_k # Default to overall top_k for paths
        if top_k_vector_supplement is None:
            _top_k_vector = max(1, top_k // 2) # Default to half for vector, ensuring at least 1 if top_k > 0
            if _top_k_paths == top_k: # If paths took full budget, maybe vector gets fewer
                _top_k_vector = max(1, top_k // 3) 

        if _top_k_paths <= 0 and _top_k_vector <= 0:
            logger.warning("_mini_retrieval: Both path and vector top_k are zero. Returning empty.")
            return StructuredContextOutput(entities=[], relations=[], documents=[])

        # 1. Find reasoning paths from the knowledge graph
        if _top_k_paths > 0:
            path_nodes_data, path_relations_str_desc, path_description_docs_raw = \
                await self.knowledge_graph.find_reasoning_paths_with_details(
                    entity_names_from_query,
                    target_entity_types,
                    max_paths=_top_k_paths, 
                    max_depth=max_reasoning_depth
                )
            
            seen_entity_ids = set()
            for node_data in path_nodes_data:
                if node_data and node_data.get('id') not in seen_entity_ids:
                    final_entities.append(node_data) 
                    seen_entity_ids.add(node_data.get('id'))
            
            logger.debug(f"Path relations description (string): {path_relations_str_desc[:200]}...")
            # Standardize path_description_docs
            for doc_data in path_description_docs_raw:
                standardized_path_doc = {
                    "id": doc_data.get("id", f"path_{len(final_documents)}"), # Ensure ID
                    "text": doc_data.get("text", doc_data.get("content", "Path description unavailable")),
                    "score": doc_data.get("score", 0.75), # Default score for paths if not present
                    "retrieval_source_type": "reasoning_path",
                    # Preserve other fields from doc_data if any
                    **{k: v for k, v in doc_data.items() if k not in ["id", "text", "score", "retrieval_source_type"]}
                }
                final_documents.append(standardized_path_doc)
        else:
            path_relations_str_desc = "" 
            # path_description_docs_raw = [] # Not needed if not fetched

        # 2. Vector search for additional relevant documents
        if self.vector_storage and _top_k_vector > 0:
            vector_results_docs_raw = await self.vector_storage.query(query_text, top_k=_top_k_vector)
            
            seen_doc_ids_for_vector = {doc.get('id') for doc in final_documents if doc.get('id')}
            for doc_data in vector_results_docs_raw:
                if doc_data.get('id') not in seen_doc_ids_for_vector:
                    standardized_vector_doc = {
                        "id": doc_data.get("id"),
                        "text": doc_data.get("text", doc_data.get("content", "Content unavailable")),
                        "score": doc_data.get("score", 0.0),
                        "retrieval_source_type": "vector_search",
                        # Preserve other fields from doc_data
                        **{k: v for k, v in doc_data.items() if k not in ["id", "text", "score", "retrieval_source_type"]}
                    }
                    final_documents.append(standardized_vector_doc)
        else:
            if not self.vector_storage and _top_k_vector > 0:
                logger.warning("Vector store not available for _mini_retrieval vector supplement.")
            # vector_results_docs_raw = [] # Not needed if not fetched

        logger.info(f"_mini_retrieval found {len(final_entities)} entities, {len(final_relations)} relations (from str: '{path_relations_str_desc[:50]}...'), and {len(final_documents)} documents (before final sort/truncate).")
        
        # TODO: Consider re-sorting all final_documents if scores are comparable, then truncating to an overall top_k if needed.
        # For now, returning all collected documents up to the sum of _top_k_paths (indirectly) and _top_k_vector.

        return StructuredContextOutput(
            entities=final_entities,
            relations=final_relations, 
            documents=final_documents
        )

    async def retrieve_context(
        self,
        query_text: str,
        strategy: str = "naive",
        top_k: Optional[int] = None,
        target_entity_types: Optional[List[str]] = None, 
        max_reasoning_depth: Optional[int] = None, 
        max_neighbor_depth: Optional[int] = None, 
        vector_graph_ratio: Optional[float] = None, # For light strategy
        top_k_reasoning_paths: Optional[int] = None, # For mini strategy
        top_k_vector_supplement: Optional[int] = None # For mini strategy
    ) -> StructuredContextOutput:
        """
        Retrieves context based on the specified strategy.
        All strategy-specific methods now return StructuredContextOutput.
        """
        logger.info(f"Retrieving context for query '{query_text[:50]}...' with strategy '{strategy}'")
        
        query_embedding: Optional[List[float]] = None
        if strategy != "mini": # _mini_retrieval no longer takes query_embedding directly
            query_embedding = await self.llm_client.get_embedding(query_text)
            if query_embedding is None:
                logger.error("Failed to get query embedding.")
                # Return empty structured context on failure
                return StructuredContextOutput(entities=[], relations=[], documents=[])

        # Entity extraction - common for light and mini strategies
        _extracted_entities: List[ExtractedEntity] = []
        if strategy in ["light", "mini"]:
            _extracted_entities = await self.entity_extractor.extract_entities_from_text(query_text)
            logger.debug(f"Extracted entities for '{strategy}' strategy: {[e.name for e in _extracted_entities]}")

        cfg_top_k = top_k if top_k is not None else self.config.rag_default_top_k

        if strategy == "naive":
            # _naive_retrieval expects Optional[List[float]] for query_embedding
            return await self._naive_retrieval(query_text, query_embedding, cfg_top_k)
        
        elif strategy == "light":
            cfg_max_neighbor_depth = max_neighbor_depth if max_neighbor_depth is not None else self.config.rag_light_max_neighbor_depth
            # _light_retrieval expects Optional[List[float]] for query_embedding
            return await self._light_retrieval(query_text, query_embedding, _extracted_entities, cfg_top_k, cfg_max_neighbor_depth, vector_graph_ratio)
        
        elif strategy == "mini":
            cfg_max_reasoning_depth = max_reasoning_depth if max_reasoning_depth is not None else self.config.rag_mini_max_reasoning_depth
            _target_entity_types = target_entity_types if target_entity_types is not None else getattr(self.config, 'rag_mini_default_target_types', ['concept', 'project', 'method'])
            return await self._mini_retrieval(
                query_text, 
                _extracted_entities, 
                _target_entity_types, 
                cfg_top_k, 
                cfg_max_reasoning_depth,
                top_k_reasoning_paths=top_k_reasoning_paths, # Pass through
                top_k_vector_supplement=top_k_vector_supplement # Pass through
            )
        
        else:
            logger.warning(f"Unknown retrieval strategy: {strategy}. Defaulting to naive.")
            # Ensure query_embedding is available for the default naive call
            if query_embedding is None: # It would be None if strategy was initially "mini"
                 query_embedding = await self.llm_client.get_embedding(query_text)
                 if query_embedding is None:
                    logger.error("Failed to get query embedding for default naive strategy.")
                    return StructuredContextOutput(entities=[], relations=[], documents=[])
            return await self._naive_retrieval(query_text, query_embedding, cfg_top_k)

    async def generate_response(
        self,
        query_text: str,
        structured_context: StructuredContextOutput, # Changed parameter name and type
        prompt_template: Optional[str] = None,
        # TODO: Add other LLM params like temperature, max_tokens from config or method args
    ) -> str:
        """
        Generates a response using the LLM based on the query and retrieved structured context.
        """
        logger.debug(f"Generating response for query: '{query_text[:50]}...'")

        if not structured_context or (
            not structured_context.get('entities') and 
            not structured_context.get('relations') and 
            not structured_context.get('documents')
        ):
            logger.warning("No context provided to generate_response. Returning a default message.")
            return "I don't have enough information to answer that query."

        # 1. Format the structured context into a string for the LLM
        context_str = self._format_structured_context_for_llm(structured_context)

        # 2. Prepare the prompt
        final_prompt_template = prompt_template or self.config.rag_default_prompt_template
        
        # Ensure the template can handle 'query' and 'context_str'
        # Example template: "Context: {context_str}\n\nQuestion: {query}\n\nAnswer:"
        if "{context_str}" not in final_prompt_template or "{query}" not in final_prompt_template:
            logger.warning("Prompt template does not contain {{context_str}} or {{query}}. Using a basic fallback.")
            # Basic fallback prompt that includes the structured context string
            prompt = f"Based on the following information:\n{context_str}\n\nAnswer the question: {query_text}"
        else:
            try:
                prompt = final_prompt_template.format(context_str=context_str, query=query_text)
            except KeyError as e:
                logger.error(f"Error formatting prompt template with key {e}. Using basic fallback.")
                prompt = f"Based on the following information:\n{context_str}\n\nAnswer the question: {query_text}"

        # 3. Call the LLM
        try:
            response_text = await self.llm_client.generate_text(
                prompt,
                # TODO: Pass through LLM generation parameters like max_tokens, temperature from config
                # max_tokens=self.config.llm_max_tokens_response,
                # temperature=self.config.llm_temperature_response
            )
            return response_text
        except Exception as e:
            logger.error(f"Error generating response from LLM: {e}")
            return "Sorry, I encountered an error while trying to generate a response."

    def _format_structured_context_for_llm(self, context: StructuredContextOutput) -> str:
        """
        Formats the structured context (entities, relations, documents) into a single string.
        """
        parts = []

        if context.get('entities'):
            parts.append("Relevant Entities:")
            for entity in context['entities']:
                # Assuming entity is a dict-like structure (e.g., GraphEntity TypedDict)
                name = entity.get('name', entity.get('id', 'Unknown Entity'))
                ent_type = entity.get('type', 'N/A')
                desc = entity.get('description', 'No description')
                parts.append(f"  - Entity: {name} (Type: {ent_type})\n    Description: {desc}")
            parts.append("\n") # Add a newline for separation

        if context.get('relations'):
            parts.append("Key Relationships:")
            for relation in context['relations']:
                # Assuming relation is a dict-like structure (e.g., GraphRelation TypedDict)
                source_id = relation.get('source_id', 'Unknown Source')
                target_id = relation.get('target_id', 'Unknown Target')
                rel_type = relation.get('type', 'related to')
                desc = relation.get('description', 'No description')
                # TODO: Future enhancement - resolve source/target IDs to names if feasible here
                parts.append(f"  - Relation: {source_id} --[{rel_type}]--> {target_id}\n    Description: {desc}")
            parts.append("\n")

        if context.get('documents'):
            parts.append("Supporting Content/Documents:")
            for i, doc in enumerate(context['documents']):
                doc_id = doc.get('id', f'doc_{i+1}')
                content = doc.get('text', 'No content') # Simplified default for content
                # Use the new retrieval_source_type field
                source_type = doc.get('retrieval_source_type', 'Unknown Source Type') 
                score_info = f" (Score: {doc['score']:.2f})" if isinstance(doc.get('score'), float) else ""
                parts.append(f"  - Document ID: {doc_id} (Source Type: {source_type}){score_info}\n    Content: {content[:500]}...") # Display first 500 chars
            parts.append("\n")
        
        return "\n".join(parts).strip()

    async def process_query_and_generate(
        self,
        query_text: str,
        strategy: str = "naive",
        top_k: Optional[int] = None,
        target_entity_types: Optional[List[str]] = None,
        max_reasoning_depth: Optional[int] = None,
        max_neighbor_depth: Optional[int] = None,
        vector_graph_ratio: Optional[float] = None,
        top_k_reasoning_paths: Optional[int] = None, # New
        top_k_vector_supplement: Optional[int] = None  # New
    ) -> str:
        """
        Retrieves context and generates a response based on the specified strategy.
        """
        logger.info(f"Processing query: '{query_text[:50]}...' with strategy '{strategy}'")
        
        structured_context_output = await self.retrieve_context(
            query_text=query_text,
            strategy=strategy,
            top_k=top_k,
            target_entity_types=target_entity_types,
            max_reasoning_depth=max_reasoning_depth,
            max_neighbor_depth=max_neighbor_depth,
            vector_graph_ratio=vector_graph_ratio,
            top_k_reasoning_paths=top_k_reasoning_paths,         # Pass through
            top_k_vector_supplement=top_k_vector_supplement  # Pass through
        )
        
        response_text = await self.generate_response(
            query_text=query_text,
            structured_context=structured_context_output # Changed from context_docs
        )
        
        return response_text

# Example usage / test function
async def main_test_rag_engine():
    import asyncio
    import tempfile # For temporary vector DB
    from simtag.config import Config
    from simtag.core.graph_store import OrgSupertagKnowledgeGraph, Entity as GraphEntity, Relation as GraphRelation
    from simtag.core.storage import VectorStorage
    from simtag.services.llm_client import LLMClient # For mocking or real use
    from simtag.core.entity_extractor import ExtractedEntity # For query_entities

    # Basic logger for test output
    logging.basicConfig(level=logging.DEBUG, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    test_logger = logging.getLogger("rag_engine_test")
    test_logger.info("Starting RAG Engine Test")

    # 1. Setup Mocks and Real Components
    config = Config()
    if not config.llm_client_config.get('base_url'): 
        config.llm_client_config['base_url'] = "http://localhost:11434"
        config.llm_client_config['default_model'] = "gemma:2b"
        config.llm_client_config['default_embedding_model'] = "nomic-embed-text"

    # Mock LLMClient methods
    async def mock_get_embedding(text: str, model: Optional[str] = None) -> List[float]:
        test_logger.info(f"Mock LLM get_embedding called for: '{text[:50]}...'")
        # Return a fixed-size vector based on text length for simplicity
        return [float(len(text) % 100) / 100.0] * 10 # Dummy 10-dim embedding

    async def mock_generate_response(prompt: str, model: Optional[str] = None) -> str:
        test_logger.info(f"Mock LLM generate_response called with prompt (first 100): {prompt[:100]}...")
        return f"Mocked LLM response to: {prompt.splitlines()[-1][:50]}..."

    mock_llm_client = LLMClient(llm_config=config.llm_client_config) # Init real one to pass around
    mock_llm_client.get_embedding = mock_get_embedding
    mock_llm_client.generate = mock_generate_response # Overwrite with mock

    # Setup in-memory KnowledgeGraph
    knowledge_graph = OrgSupertagKnowledgeGraph(config=config) # Assumes in-memory for now
    await knowledge_graph.upsert_entities([
        GraphEntity(id="concept_ml", type="concept", name="Machine Learning", description="Field of AI", source_nodes={"node1"}),
        GraphEntity(id="tool_python", type="tool", name="Python", description="Programming language", source_nodes={"node1"}),
        GraphEntity(id="method_dl", type="method", name="Deep Learning", description="Subfield of ML", source_nodes={"node2"})
    ])
    await knowledge_graph.upsert_relations([
        GraphRelation(id="rel1", source_entity_id="method_dl", target_entity_id="concept_ml", type="IS_SUBFIELD_OF", source_nodes={"node2"})
    ])

    # Setup temporary VectorStorage
    temp_db_file = tempfile.NamedTemporaryFile(suffix=".db", delete=False)
    vector_storage_path = temp_db_file.name
    temp_db_file.close() # Close it so VectorStorage can open it
    vector_storage = VectorStorage(db_path=vector_storage_path)
    vector_storage.ensure_table_exists()
    # Add some sample vectors
    await vector_storage.add_item("doc1", "Machine Learning is cool", await mock_get_embedding("Machine Learning is cool"), {'type': 'concept', 'source_node':'node1'})
    await vector_storage.add_item("doc2", "Python is a versatile language", await mock_get_embedding("Python is a versatile language"), {'type': 'tool', 'source_node':'node1'})
    await vector_storage.add_item("doc3", "Deep Learning models are powerful", await mock_get_embedding("Deep Learning models are powerful"), {'type': 'method', 'source_node':'node2'})

    # 2. Initialize RAG Engine
    rag_engine = OrgSupertagRAGEngine(
        knowledge_graph=knowledge_graph,
        vector_storage=vector_storage,
        llm_client=mock_llm_client,
        config=config
    )

    # 3. Test retrieve_context
    test_query = "Tell me about Deep Learning and its relation to ML."
    test_query_embedding = await mock_get_embedding(test_query)
    # Mock extracted entities from the query for 'mini' strategy
    mock_query_entities = [
        ExtractedEntity(id="q_dl", name="Deep Learning", type="method"),
        ExtractedEntity(id="q_ml", name="ML", type="concept") # Assuming ML is an alias or variant
    ]

    test_logger.info(f"\n--- Testing retrieve_context with query: '{test_query}' ---")
    for strategy in ["naive", "light", "mini"]:
        test_logger.info(f"  Strategy: {strategy}")
        try:
            context_docs = await rag_engine.retrieve_context(
                query_text=test_query,
                strategy=strategy,
                top_k=3
            )
            test_logger.info(f"    Retrieved {len(context_docs)} documents:")
            for i, doc in enumerate(context_docs):
                test_logger.info(f"      Doc {i+1}: ID={doc.get('id')}, Score={doc.get('score', 'N/A')}, Text='{doc.get('text','')[:50]}...'")
        except Exception as e:
            test_logger.error(f"    Error retrieving context with strategy {strategy}: {e}", exc_info=True)

    # 4. Test generate_response
    test_logger.info(f"\n--- Testing generate_response with query: '{test_query}' ---")
    # Use context from a previous retrieval (e.g., 'light' strategy)
    sample_context_for_generation = await rag_engine.retrieve_context(
        query_text=test_query, strategy="light", top_k=2
    )
    try:
        response_text = await rag_engine.generate_response(
            query_text=test_query,
            structured_context=sample_context_for_generation
        )
        test_logger.info(f"  Generated response: {response_text}")
    except Exception as e:
        test_logger.error(f"    Error generating response: {e}", exc_info=True)

    # Cleanup temporary vector DB file
    if os.path.exists(vector_storage_path):
        os.remove(vector_storage_path)
        test_logger.info(f"Cleaned up temporary vector DB: {vector_storage_path}")

    await mock_llm_client.close() # Close the httpx client if it were real
    test_logger.info("RAG Engine Test Finished")

if __name__ == '__main__':
    asyncio.run(main_test_rag_engine()) 
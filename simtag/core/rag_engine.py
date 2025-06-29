"""
Org SuperTag RAG Engine Module

This module implements the RAG (Retrieval Augmented Generation) engine 
for org-supertag, providing different strategies for context retrieval.
"""
import asyncio
import logging
from typing import Dict, List, Any, Optional, Tuple, TypedDict, Callable
import os
import numpy as np
import tempfile # For temporary vector DB

# Corrected and new imports
from simtag.config import Config
from .graph_service import GraphService
from ..services.llm_client import LLMClient
from .entity_extractor import LLMEntityExtractor, ExtractedEntity
from ..services.embedding_service import EmbeddingService

logger = logging.getLogger(__name__)

class StructuredContextOutput(TypedDict):
    """
    Defines the structured output from retrieval methods.
    """
    entities: List[Dict[str, Any]] # Changed from GraphEntity
    relations: List[Dict[str, Any]] # Changed from GraphRelation
    documents: List[Dict[str, Any]] # Existing document format


class OrgSupertagRAGEngine:
    """
    Implements the RAG engine with naive, light, and mini query strategies.
    Integrates Knowledge Graph and Vector Storage.
    Based on living-doc-features.org section 10.7.A.3
    """

    def __init__(self,
                 graph_service: GraphService,
                 llm_client: LLMClient,
                 embedding_service: EmbeddingService,
                 config: Config
                 ):
        """
        Initializes the OrgSupertagRAGEngine.

        Args:
            graph_service: Instance of the unified GraphService.
            llm_client: Client for interacting with an LLM for generation.
            embedding_service: Service for generating embeddings.
            config: Configuration object.
        """
        self.graph_service = graph_service
        self.llm_client = llm_client
        self.embedding_service = embedding_service
        self.config = config
        
        # Use the new, unified LLMEntityExtractor
        self.entity_extractor = LLMEntityExtractor(
            llm_client=llm_client,
            config=config.analysis_config # Use the analysis config block
        )
        
        logger.info("OrgSupertagRAGEngine initialized.")

    async def _naive_retrieval(
        self,
        query_text: str,
        query_embedding: Optional[np.ndarray],
        top_k: int
    ) -> StructuredContextOutput:
        """Performs naive vector store retrieval."""
        logger.debug(f"Performing naive retrieval for query: '{query_text[:50]}...'")
        
        if not self.graph_service.has_vector_ext:
            logger.warning("Vector extension is not available for naive retrieval.")
            return StructuredContextOutput(entities=[], relations=[], documents=[])

        if query_embedding is None:
            logger.debug("No pre-computed embedding, generating one for the query.")
            query_embedding_result = await self.embedding_service.get_embedding(query_text)
            if not query_embedding_result.success or not query_embedding_result.embedding:
                logger.error("Failed to generate embedding for query.")
                return StructuredContextOutput(entities=[], relations=[], documents=[])
            query_embedding = query_embedding_result.embedding
        
        # Find similar node IDs
        similar_node_tuples = self.graph_service.find_similar_nodes(query_embedding, top_k=top_k)
        if not similar_node_tuples:
            return StructuredContextOutput(entities=[], relations=[], documents=[])

        node_ids = [node_id for node_id, score in similar_node_tuples]
        scores_map = {node_id: score for node_id, score in similar_node_tuples}

        # Get node details
        nodes_data = self.graph_service.get_nodes_by_ids(node_ids)

        standardized_documents: List[Dict[str, Any]] = []
        for doc_data in nodes_data:
            node_id = doc_data.get("node_id")
            standardized_doc = {
                "id": node_id,
                "text": doc_data.get("content", "Content unavailable"),
                "score": scores_map.get(node_id, 0.0), # Use score from similarity search
                "retrieval_source_type": "vector_search",
                **{k: v for k, v in doc_data.items() if k not in ["id", "text", "score"]}
            }
            standardized_documents.append(standardized_doc)
        
        logger.debug(f"Naive retrieval found {len(standardized_documents)} documents.")
        return StructuredContextOutput(entities=[], relations=[], documents=standardized_documents)

    async def _light_retrieval(
        self,
        query_text: str,
        query_embedding: Optional[np.ndarray],
        extracted_entities: List[Dict[str, Any]],
        top_k: int,
        max_neighbor_depth: int,
        vector_graph_ratio: Optional[float] = None # New parameter for balancing
    ) -> StructuredContextOutput: # Changed return type
        """Performs light retrieval: naive search + K-hop graph neighbors for context."""
        logger.debug(f"Performing light retrieval for query: '{query_text[:50]}...', ratio: {vector_graph_ratio}")

        final_entities: List[Dict[str, Any]] = []
        final_relations: List[Dict[str, Any]] = [] # Remains empty for now
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
        graph_enhanced_docs_raw: List[Dict[str, Any]] = []
        start_entities_for_graph: List[Dict[str, Any]] = []

        if extracted_entities:
            # Convert extracted entities to a simpler dict format for consistency
            for ext_entity in extracted_entities:
                if ext_entity.get('id'): 
                    # We first need to check if this entity exists in our graph to get its full data
                    graph_entity = await self.graph_service.get_tag_by_name(ext_entity.get('name')) # Assuming tags are the main entities for now
                    if graph_entity:
                        start_entities_for_graph.append(graph_entity)

            final_entities.extend(start_entities_for_graph)
            
            start_entity_ids = [e['id'] for e in start_entities_for_graph]

            if start_entity_ids and num_graph_results_target > 0:
                neighbor_nodes = []
                for start_id in start_entity_ids:
                    # NOTE: get_neighbors is a simplified replacement for the old get_neighbor_documents
                    # max_neighbor_depth is not used yet, would require recursive calls.
                    neighbors = self.graph_service.get_neighbors(start_id)
                    neighbor_nodes.extend(neighbors)

                # Standardize neighbor_nodes to document format
                for i, node_data in enumerate(neighbor_nodes):
                    standardized_neighbor_doc = {
                        "id": node_data.get("node_id", f"neighbor_{i}"),
                        "text": node_data.get("content", "Neighbor content unavailable"),
                        "score": 0.70, # Default score for graph neighbors
                        "retrieval_source_type": "graph_neighbor",
                        **{k: v for k, v in node_data.items() if k not in ["id", "text", "score"]}
                    }
                    graph_enhanced_docs_raw.append(standardized_neighbor_doc)
        
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
        extracted_entities: List[Dict[str, Any]],
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

        final_entities: List[Dict[str, Any]] = []
        final_relations: List[Dict[str, Any]] = []
        final_documents: List[Dict[str, Any]] = []
        
        entity_names_from_query = [entity.get('name') for entity in extracted_entities if entity.get('name')]

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
            # Ensure query_embedding is available for search_expand
            if query_embedding is None:
                query_embedding_result = await self.embedding_service.get_embedding(query_text)
                if not query_embedding_result.success or not query_embedding_result.embedding:
                    logger.error("Failed to get query embedding for reasoning path retrieval.")
                    return StructuredContextOutput(entities=[], relations=[], documents=[])
                query_embedding = query_embedding_result.embedding

            # Call graph_service.search_expand for topology-enhanced search
            expanded_search_results = self.graph_service.search_expand(
                query_embedding,
                top_k=_top_k_paths, # Use top_k_paths for initial vector search
                expansion_hops=max_reasoning_depth
            )

            path_nodes_data = expanded_search_results.get("initial", []) + expanded_search_results.get("expanded", [])
            # Relations are not directly returned by search_expand, but can be inferred or fetched if needed
            path_relations_data = [] # Placeholder for now, will be populated by explicit relation fetching
            path_description_docs_raw = []

            seen_entity_ids = set()
            for node_data in path_nodes_data:
                if node_data and node_data.get('node_id') not in seen_entity_ids:
                    # Convert node_data to a format compatible with final_entities if needed
                    # For now, assuming node_data from graph_service is sufficient
                    final_entities.append(node_data)
                    seen_entity_ids.add(node_data.get('node_id'))
                    # Add node content as a document
                    path_description_docs_raw.append({
                        "id": node_data.get("node_id"),
                        "text": node_data.get("content", ""),
                        "score": 1.0, # Assign a high score for graph-retrieved docs
                        "retrieval_source_type": "graph_expansion",
                        "title": node_data.get("title")
                    })
            
            # Populate final_relations based on relationships between path_nodes_data
            # This would involve iterating through path_nodes_data and fetching their relations
            # For simplicity, this is a placeholder for now.
            for node_data in path_nodes_data:
                node_relations = self.graph_service.get_neighbors(node_data.get('node_id')) # Get all neighbors/relations
                for rel in node_relations:
                    # Assuming get_neighbors returns relation-like dicts or can be converted
                    if rel.get('node_id') not in seen_entity_ids: # Avoid adding entities already processed
                        final_entities.append(rel) # Add related entities
                        seen_entity_ids.add(rel.get('node_id'))
                    # This part needs careful mapping from get_neighbors output to ExtractedRelation
                    # For now, just adding to final_relations if it's a relation object
                    # This is a simplification, proper relation objects need to be constructed
                    if 'source_id' in rel and 'target_id' in rel and 'type' in rel:
                        final_relations.append(rel)

            seen_relation_ids = set()
            for rel_data in path_relations_data:
                if rel_data and rel_data.get('id') not in seen_relation_ids:
                    final_relations.append(rel_data)
                    seen_relation_ids.add(rel_data.get('id'))

            final_documents.extend(path_description_docs_raw)
        else:
            path_relations_str_desc = "" 
            # path_description_docs_raw = [] # Not needed if not fetched

        # 2. Supplement with a naive vector search
        if _top_k_vector > 0:
            # Embedding is passed as None to let _naive_retrieval generate it
            vector_supplement_output = await self._naive_retrieval(query_text, query_embedding, _top_k_vector)
            
            # Combine results, avoiding duplicates
            seen_doc_ids = {doc['id'] for doc in final_documents if doc.get('id')}
            for doc in vector_supplement_output['documents']:
                if doc.get('id') not in seen_doc_ids:
                    final_documents.append(doc)
                    seen_doc_ids.add(doc.get('id'))

        # Simple sort by score, can be more sophisticated
        sorted_documents = sorted(final_documents, key=lambda x: x.get('score', 0.0), reverse=True)
        
        logger.debug(f"Mini retrieval found {len(final_entities)} entities, {len(final_relations)} relations, and {len(sorted_documents)} documents.")

        return StructuredContextOutput(
            entities=final_entities,
            relations=final_relations,
            documents=sorted_documents[:top_k]
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
        Main entry point for retrieving context using a specified strategy.
        """
        # Set defaults from config if not provided
        cfg_top_k = top_k if top_k is not None else self.config.retrieval_config.get('default_top_k', 5)
        cfg_max_reasoning_depth = max_reasoning_depth if max_reasoning_depth is not None else self.config.retrieval_config.get('default_max_reasoning_depth', 3)
        cfg_max_neighbor_depth = max_neighbor_depth if max_neighbor_depth is not None else self.config.retrieval_config.get('default_max_neighbor_depth', 2)

        query_embedding: Optional[np.ndarray] = None
        extracted_entities: List[Dict[str, Any]] = []

        # Generate embedding and extract entities if needed by the strategy
        if strategy in ["light", "mini", "naive"]:
            tasks = []
            if strategy in ["light", "mini"]:
                tasks.append(self.entity_extractor.extract(query_text))
            
            # Always get embedding if strategy is light, mini, or naive
            tasks.append(self.embedding_service.get_embedding(query_text))
            
            results = await asyncio.gather(*tasks)
            
            if strategy in ["light", "mini"]:
                extraction_result = results.pop(0)
                extracted_entities = extraction_result.get("entities", [])
            
            query_embedding_result = results.pop(0)
            if not query_embedding_result.success or not query_embedding_result.embedding:
                logger.error("Failed to get query embedding for retrieval.")
                return StructuredContextOutput(entities=[], relations=[], documents=[])
            query_embedding = query_embedding_result.embedding

        logger.info(f"Retrieving context for query '{query_text[:50]}...' with strategy '{strategy}'")

        retrieved_context: StructuredContextOutput

        if strategy == "naive":
            retrieved_context = await self._naive_retrieval(query_text, query_embedding, cfg_top_k)
        
        elif strategy == "light":
            retrieved_context = await self._light_retrieval(
                query_text,
                query_embedding,
                extracted_entities,
                cfg_top_k,
                cfg_max_neighbor_depth,
                vector_graph_ratio
            )
        
        elif strategy == "mini":
            _target_entity_types = target_entity_types if target_entity_types is not None else getattr(self.config, 'rag_mini_default_target_types', ['concept', 'project', 'method'])
            retrieved_context = await self._mini_retrieval(
                query_text,
                query_embedding, # Pass query_embedding to _mini_retrieval
                extracted_entities, 
                _target_entity_types, 
                cfg_top_k, 
                cfg_max_reasoning_depth,
                top_k_reasoning_paths=top_k_reasoning_paths, # Pass through
                top_k_vector_supplement=top_k_vector_supplement # Pass through
            )
        
        else:
            logger.warning(f"Unknown retrieval strategy: {strategy}. Defaulting to naive.")
            retrieved_context = await self._naive_retrieval(query_text, query_embedding, cfg_top_k)

        # Dynamic Context Construction (Token Budget Allocation)
        final_entities = []
        final_relations = []
        final_documents = []

        # Estimate token counts for each component
        # This is a simplified estimation. A proper tokenizer should be used for accuracy.
        def estimate_tokens(text_content: str) -> int:
            return len(text_content.split()) # Simple word count

        # Prioritize and truncate components based on config
        total_tokens_budget = self.config.rag_context_window_size
        current_tokens = 0

        # Sort priorities as defined in config
        priorities = self.config.rag_context_priorities

        # Helper to add items within budget
        def add_to_context(item_list: List[Any], item_type: str, token_ratio: float, formatter: Callable[[Any], str]):
            nonlocal current_tokens
            component_budget = int(total_tokens_budget * token_ratio)
            for item in item_list:
                item_str = formatter(item)
                item_tokens = estimate_tokens(item_str)
                if current_tokens + item_tokens <= component_budget:
                    if item_type == "entities":
                        final_entities.append(item)
                    elif item_type == "relations":
                        final_relations.append(item)
                    elif item_type == "documents":
                        final_documents.append(item)
                    current_tokens += item_tokens
                else:
                    # Truncate if possible, or skip
                    remaining_budget = component_budget - current_tokens
                    if remaining_budget > 0:
                        truncated_item_str = item_str[:int(remaining_budget * 4)] + "..." # Rough char estimate for tokens
                        if item_type == "entities":
                            # For entities, truncating description or properties
                            truncated_item = item.copy()
                            truncated_item['description'] = truncated_item_str
                            final_entities.append(truncated_item)
                        elif item_type == "relations":
                            # For relations, truncating description or properties
                            truncated_item = item.copy()
                            truncated_item['properties'] = {'description': truncated_item_str}
                            final_relations.append(truncated_item)
                        elif item_type == "documents":
                            truncated_item = item.copy()
                            truncated_item['text'] = truncated_item_str
                            final_documents.append(truncated_item)
                        current_tokens += estimate_tokens(truncated_item_str)
                    break # No more space for this component

        # Apply prioritization and budget allocation
        for priority_item in priorities:
            if priority_item == "current_node":
                # Assuming current_node is the query_text itself or a primary document
                # This is handled by the initial retrieval, so we prioritize its full inclusion
                pass # Already in query_text, or will be part of retrieved_context
            elif priority_item == "recent_nodes":
                add_to_context(retrieved_context.get("documents", []), "documents", self.config.rag_context_core_content_ratio, lambda x: x.get("text", ""))
            elif priority_item == "high_frequency_concepts":
                add_to_context(retrieved_context.get("entities", []), "entities", self.config.rag_context_supplementary_ratio, lambda x: x.get("name", "") + " " + x.get("description", ""))
            elif priority_item == "cross_domain_relations":
                add_to_context(retrieved_context.get("relations", []), "relations", self.config.rag_context_background_ratio, lambda x: x.get("description", ""))
            # Add more priority items as needed

        logger.debug(f"Dynamic context construction complete. Total tokens used: {current_tokens}/{total_tokens_budget}")

        return StructuredContextOutput(
            entities=final_entities,
            relations=final_relations,
            documents=final_documents
        )

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

        llm_generation_params = {
            "max_tokens": self.config.llm_client_config.get("max_tokens_response"),
            "temperature": self.config.llm_client_config.get("temperature", 0.7)
        }

        # 3. Call the LLM
        try:
            response_text = await self.llm_client.generate(
                prompt,
                **llm_generation_params
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


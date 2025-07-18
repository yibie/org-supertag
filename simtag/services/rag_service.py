# simtag/services/rag_service.py
import logging
from typing import Dict, Any, List, Optional, Tuple
import numpy as np
import asyncio
import time
import traceback
import json
import re
from dataclasses import dataclass, field
from collections import deque

from simtag.config import Config
from simtag.core.graph_service import GraphService, NodeType
from simtag.services.embedding_service import EmbeddingService, EmbeddingResult
from simtag.services.llm_client import LLMClient
from simtag.utils.unified_tag_processor import TagResult, normalize_payload
from simtag import prompts # Import the entire prompts module
from ..prompts import (
    ENTITY_EXTRACTION_PROMPT,
    QUERY_ANALYSIS_PROMPT,
    QA_PROMPT,
    DEFAULT_ENTITY_TYPES,
    RELATION_INFERENCE_PROMPT,
    ENTITY_ONLY_PROMPT,
)
from simtag.services.bm25_service import BM25Service

logger = logging.getLogger(__name__)


# --- Data Models ---
@dataclass
class Entity:
    name: str
    type: str
    description: Optional[str] = None

@dataclass
class Relationship:
    source: str
    target: str
    type: str
    description: Optional[str] = None
    properties: Dict[str, Any] = field(default_factory=dict)

class RAGService:
    """
    Provides RAG functionalities by querying the knowledge graph.
    """
    def __init__(self, llm_client: LLMClient, graph_service: GraphService, embedding_service: EmbeddingService, config: Config):
        self.llm_client = llm_client
        self.graph_service = graph_service
        self.embedding_service = embedding_service
        self.config = config

        # Initialize BM25 service (lightweight)
        try:
            self.bm25_service = BM25Service(graph_service=self.graph_service, index_path=self.config.bm25_index_path)
        except Exception as e:
            logger.warning(f"BM25Service initialization failed: {e}")
            self.bm25_service = None
            
        # Cache for intent analysis results to reduce LLM calls
        self._intent_cache = {}
        self._cache_max_size = 100
        self._cache_ttl = 3600  # 1 hour

    def _parse_llm_json_output(self, llm_output: str) -> Dict[str, List[Any]]:
        """
        Parses the JSON output from the LLM. It robustly finds the JSON block
        and loads it.
        """
        # Regex to find a JSON object, allowing for surrounding text or markdown.
        match = re.search(r'\{.*\}', llm_output, re.DOTALL)
        if not match:
            logger.warning("No JSON object found in the LLM output.")
            return {"entities": [], "relationships": [], "keywords": []}

        json_str = match.group(0)
        try:
            data = json.loads(json_str)
            # Ensure top-level keys exist
            data.setdefault("entities", [])
            data.setdefault("relationships", [])
            data.setdefault("keywords", [])
            return data
        except json.JSONDecodeError:
            logger.error(f"Failed to decode JSON from LLM output: {json_str}")
            return {"entities": [], "relationships": [], "keywords": []}

    def process_llm_output(self, llm_output: str, source_node_id: str):
        """
        Parses the LLM's JSON output, processes it, and adds it to the graph.
        """
        # 1. Parse the raw string into a structured dictionary
        parsed_data = self._parse_llm_json_output(llm_output)
        
        # 2. Process entities and relationships from the parsed data
        entities = self.process_entities(parsed_data.get("entities", []))
        relationships = self.process_relationships(parsed_data.get("relationships", []))

        # 3. Add to Graph
        for entity in entities:
            self.graph_service.upsert_entity({
                'node_id': entity.name,
                'type': NodeType.TAG.value,
                'title': entity.name,
                'content': entity.description,
                'properties': {'type': entity.type}
            })

        for rel in relationships:
            # 验证关系的必要字段
            if not rel.source or not rel.target:
                logger.warning(f"Skipping relationship with missing source or target: {rel}")
                continue
            self.graph_service.upsert_relationship(
                source_id=rel.source,
                target_id=rel.target,
                type=rel.type,
                properties=rel.properties
            )
        
        # Link the source document to the newly created entities
        for entity in entities:
            self.graph_service.upsert_relationship(
                source_id=source_node_id,
                target_id=entity.name,
                type="CONTAINS_ENTITY"
            )

    async def process_and_store_text(self, text_content: str, source_node_id: str):
        """
        Processes a given text to extract entities and relationships,
        and then stores them in the knowledge graph.
        """
        try:
            # 1. Extract Entities and Relationships in one go using the JSON-based prompt
            # Manually construct the final prompt without using .format on the main template
            final_prompt = ENTITY_EXTRACTION_PROMPT.format(
                entity_types=DEFAULT_ENTITY_TYPES,
                input_text=text_content,
            )
            llm_result = await self.llm_client.generate(final_prompt, use_chat_endpoint=False)
            if not llm_result or not llm_result.content:
                logger.warning("LLM failed to return content for entity extraction.")
                return

            self.process_llm_output(llm_result.content, source_node_id)

        except Exception as e:
            logger.error(f"Error during LLM-based processing for doc {source_node_id}: {e}", exc_info=True)

    def process_entities(self, entity_list: List[Dict[str, Any]]) -> List[Entity]:
        """Converts a list of entity dictionaries into Entity objects."""
        if not isinstance(entity_list, list):
            logger.warning(f"Invalid entity data received for processing: {entity_list}")
            return []
        
        processed_entities = []
        for item in entity_list:
            if isinstance(item, dict) and 'name' in item and 'type' in item:
                processed_entities.append(Entity(
                    name=item['name'], 
                    type=item['type'], 
                    description=item.get('description', '')
                ))
            else:
                logger.warning(f"Skipping malformed entity item: {item}")
        return processed_entities

    def process_relationships(self, relationship_list: List[Dict[str, Any]]) -> List[Relationship]:
        """Converts a list of relationship dictionaries into Relationship objects."""
        if not isinstance(relationship_list, list):
            logger.warning(f"Invalid relationship data received for processing: {relationship_list}")
            return []
            
        processed_relationships = []
        for item in relationship_list:
            if isinstance(item, dict) and 'source' in item and 'target' in item and 'type' in item:
                # 验证 source 和 target 不为空
                source = item['source']
                target = item['target']
                if not source or not target:
                    logger.warning(f"Skipping relationship with empty source or target: {item}")
                    continue
                processed_relationships.append(Relationship(
                    source=source,
                    target=target,
                    type=item['type'],
                    description=item.get('description', ''),
                    properties={'strength': item.get('strength', 0.0)}
                ))
            else:
                logger.warning(f"Skipping malformed relationship item: {item}")
        return processed_relationships

    async def query(self, query_text: str, history: Optional[list] = None, is_chat: bool = True, command: Optional[str] = None) -> Dict[str, Any]:
        """
        Answers a query using the refactored RAG pipeline.
        The `is_chat` flag determines whether to use the /api/chat endpoint.
        The `command` parameter indicates if this is a custom command from the frontend.
        """
        history = history or []
        
        # Check if this is a command-based query and route accordingly
        command_result = await self._handle_command_query(query_text, history, is_chat, command)
        if command_result:
            return command_result
        
        # Regular RAG query processing
        return await self._process_rag_query(query_text, history, is_chat)

    async def _handle_command_query(self, query_text: str, history: Optional[list], is_chat: bool, command: Optional[str] = None) -> Optional[Dict[str, Any]]:
        """
        Handle command-based queries with specialized logic.
        Returns None if this is not a command query.
        """
        # If command is provided from frontend, use it directly
        if command:
            logger.info(f"Custom command detected from frontend: {command}")
            return await self._execute_custom_command(command, query_text, history, is_chat)
        
        # Detect command patterns for built-in commands
        command_patterns = {
            'create-question': [
                r'please list all important questions related to (.+)',
                r'create questions for (.+)',
                r'generate questions about (.+)'
            ],
            'analyze': [
                r'analyze (.+)',
                r'explain (.+)',
                r'describe (.+)'
            ],
            'compare': [
                r'compare (.+) and (.+)',
                r'difference between (.+) and (.+)'
            ]
        }
        
        import re
        for command_type, patterns in command_patterns.items():
            for pattern in patterns:
                match = re.search(pattern, query_text, re.IGNORECASE)
                if match:
                    logger.info(f"Built-in command detected: {command_type} with topic: {match.groups()}")
                    return await self._execute_command(command_type, match.groups(), query_text, history, is_chat)
        
        return None

    async def _execute_command(self, command_type: str, topics: tuple, original_query: str, history: Optional[list], is_chat: bool) -> Dict[str, Any]:
        """
        Execute a specific command with its own logic.
        """
        if command_type == 'create-question':
            return await self._execute_create_question_command(topics[0], original_query, history, is_chat)
        elif command_type == 'analyze':
            return await self._execute_analyze_command(topics[0], original_query, history, is_chat)
        elif command_type == 'compare':
            return await self._execute_compare_command(topics[0], topics[1], original_query, history, is_chat)
        
        # Fallback to regular RAG processing
        return None

    async def _execute_create_question_command(self, topic: str, original_query: str, history: Optional[list], is_chat: bool) -> Dict[str, Any]:
        """
        Execute create-question command with specialized prompt.
        """
        # Create a specialized prompt for question generation
        system_prompt = """You are an expert at generating insightful questions about various topics. 
Your task is to create a comprehensive list of important questions about the given topic.
Focus on questions that:
1. Help understand the core concepts
2. Explore practical applications
3. Consider different perspectives
4. Address common challenges or misconceptions
5. Lead to deeper learning

Provide clear, well-structured questions that would be valuable for someone learning about this topic.

**IMPORTANT: Always format your response using Markdown syntax** - use headers (# ##), lists (- *), bold (**text**), italic (*text*), and other Markdown formatting as appropriate."""

        user_prompt = f"Generate a comprehensive list of important questions about: {topic}"

        messages = [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt}
        ]

        if is_chat:
            chat_res = await self.llm_client.chat(messages=messages, format_json=False)
            if chat_res.success and chat_res.content:
                return {
                    "answer": chat_res.content,
                    "source_nodes": [],
                    "note": f"Generated questions about {topic} using LLM knowledge"
                }
        else:
            llm_result = await self.llm_client.generate(user_prompt, use_chat_endpoint=False)
            if llm_result and llm_result.content:
                return {
                    "answer": llm_result.content,
                    "source_nodes": [],
                    "note": f"Generated questions about {topic} using LLM knowledge"
                }
        
        return {"error": "Failed to generate questions"}

    async def _execute_analyze_command(self, topic: str, original_query: str, history: Optional[list], is_chat: bool) -> Dict[str, Any]:
        """
        Execute analyze command with specialized prompt.
        """
        system_prompt = """You are an expert analyst who provides comprehensive analysis of various topics.
Your analysis should be:
1. Well-structured and logical
2. Based on current knowledge and best practices
3. Practical and actionable
4. Balanced in perspective
5. Clear and accessible

Provide a thorough analysis that helps the user understand the topic deeply.

**IMPORTANT: Always format your response using Markdown syntax** - use headers (# ##), lists (- *), bold (**text**), italic (*text*), code blocks (```), and other Markdown formatting as appropriate."""

        user_prompt = f"Provide a comprehensive analysis of: {topic}"

        messages = [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt}
        ]

        if is_chat:
            chat_res = await self.llm_client.chat(messages=messages, format_json=False)
            if chat_res.success and chat_res.content:
                return {
                    "answer": chat_res.content,
                    "source_nodes": [],
                    "note": f"Analysis of {topic} using LLM knowledge"
                }
        else:
            llm_result = await self.llm_client.generate(user_prompt, use_chat_endpoint=False)
            if llm_result and llm_result.content:
                return {
                    "answer": llm_result.content,
                    "source_nodes": [],
                    "note": f"Analysis of {topic} using LLM knowledge"
                }
        
        return {"error": "Failed to analyze topic"}

    async def _execute_compare_command(self, topic1: str, topic2: str, original_query: str, history: Optional[list], is_chat: bool) -> Dict[str, Any]:
        """
        Execute compare command with specialized prompt.
        """
        system_prompt = """You are an expert at comparing and contrasting different topics, technologies, or concepts.
Your comparison should be:
1. Structured and systematic
2. Fair and balanced
3. Focused on key differences and similarities
4. Practical and actionable
5. Based on current knowledge and best practices

Provide a comprehensive comparison that helps users make informed decisions.

**IMPORTANT: Always format your response using Markdown syntax** - use headers (# ##), lists (- *), bold (**text**), italic (*text*), tables, and other Markdown formatting as appropriate."""

        user_prompt = f"Compare and contrast: {topic1} vs {topic2}"

        messages = [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt}
        ]

        if is_chat:
            chat_res = await self.llm_client.chat(messages=messages, format_json=False)
            if chat_res.success and chat_res.content:
                return {
                    "answer": chat_res.content,
                    "source_nodes": [],
                    "note": f"Comparison of {topic1} vs {topic2} using LLM knowledge"
                }
        else:
            llm_result = await self.llm_client.generate(user_prompt, use_chat_endpoint=False)
            if llm_result and llm_result.content:
                return {
                    "answer": llm_result.content,
                    "source_nodes": [],
                    "note": f"Comparison of {topic1} vs {topic2} using LLM knowledge"
                }
        
        return {"error": "Failed to compare topics"}

    async def _execute_custom_command(self, command_name: str, query_text: str, history: Optional[list], is_chat: bool) -> Dict[str, Any]:
        """
        Execute a custom command from the frontend.
        The query_text here is the final processed text (with $input replaced).
        This method combines RAG retrieval with LLM knowledge for the best results.
        """
        logger.info(f"Executing custom command: {command_name} with query: {query_text}")
        
        # First, try to get relevant context from RAG
        context_result = await self._get_context_for_custom_command(query_text)
        context_text = context_result['context']
        source_nodes = context_result['source_nodes']
        
        # Create a specialized prompt for custom commands that can use both context and LLM knowledge
        system_prompt = f"""You are an AI assistant executing a custom command called "{command_name}".
This command was defined by the user with a specific purpose and prompt template.

Your task is to:
1. Understand the intent of the custom command "{command_name}"
2. Provide a response that fulfills the command's purpose
3. Use both the provided context (if any) and your own knowledge
4. If the context contains relevant information, incorporate it into your response
5. If the context is insufficient or irrelevant, rely on your knowledge
6. Be helpful, accurate, and relevant to the user's request
7. **IMPORTANT: Always format your response using Markdown syntax** - use headers (# ##), lists (- *), bold (**text**), italic (*text*), code blocks (```), and other Markdown formatting as appropriate

The user's input has been processed according to the command's template and is: {query_text}"""

        # Prepare the user prompt with context if available
        if context_text and "No relevant local documents found" not in context_text:
            user_prompt = f"""Context from local documents:
{context_text}

User request: {query_text}

Please provide a comprehensive response that combines the context above with your knowledge."""
        else:
            user_prompt = query_text

        messages = [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt}
        ]

        if is_chat:
            chat_res = await self.llm_client.chat(messages=messages, format_json=False)
            if chat_res.success and chat_res.content:
                return {
                    "answer": chat_res.content,
                    "source_nodes": source_nodes,
                    "note": f"Executed custom command '{command_name}' with RAG + LLM knowledge"
                }
        else:
            llm_result = await self.llm_client.generate(user_prompt, use_chat_endpoint=False)
            if llm_result and llm_result.content:
                return {
                    "answer": llm_result.content,
                    "source_nodes": source_nodes,
                    "note": f"Executed custom command '{command_name}' with RAG + LLM knowledge"
                }
        
        return {"error": f"Failed to execute custom command '{command_name}'"}

    async def _get_context_for_custom_command(self, query_text: str) -> Dict[str, Any]:
        """
        Get relevant context for custom commands using RAG retrieval.
        This is a simplified version that focuses on getting the most relevant documents.
        Returns both context text and source nodes.
        """
        try:
            # Extract keywords from the query for better retrieval
            import re
            words = re.findall(r'\b\w{4,}\b', query_text.lower())
            entities_from_query = list(set(words))
            
            # Filter out common stop words
            stop_words = {'what', 'how', 'when', 'where', 'why', 'who', 'which', 'this', 'that', 'these', 'those', 'and', 'or', 'but', 'with', 'for', 'from', 'the', 'a', 'an', 'is', 'are', 'was', 'were', 'be', 'been', 'have', 'has', 'had', 'do', 'does', 'did', 'will', 'would', 'could', 'should', 'can', 'may', 'might', 'must', 'please', 'list', 'all', 'important', 'questions', 'related', 'analyze', 'explain', 'describe', 'compare', 'difference', 'between'}
            entities_from_query = [word for word in entities_from_query if word not in stop_words]
            
            logger.info(f"Custom command context search - Query: '{query_text}', Keywords: {entities_from_query}")
            
            # Try vector search first
            query_embedding_result = await self.embedding_service.get_embedding(query_text)
            if not query_embedding_result.success:
                logger.warning("Embedding failed for custom command context search")
                return {
                    "context": "--- Context from Notes ---\nNo relevant local documents found for this query.",
                    "source_nodes": []
                }
            
            # Get similar nodes
            top_k = getattr(self.config, 'rag_search_top_k', 5)  # Use fewer results for custom commands
            similar_nodes = self.graph_service.find_similar_nodes(query_embedding_result.embedding, top_k=top_k)
            
            # Also try entity-based search
            similar_entities = []
            if entities_from_query:
                entity_query_text = " ".join(entities_from_query)
                entity_embedding_result = await self.embedding_service.get_embedding(entity_query_text)
                if entity_embedding_result.success:
                    similar_entities = self.graph_service.find_similar_entities(entity_embedding_result.embedding, top_k=top_k)
            
            # Combine results
            all_nodes = []
            seen_ids = set()
            
            for node in similar_nodes:
                if node['node_id'] not in seen_ids:
                    all_nodes.append(node)
                    seen_ids.add(node['node_id'])
            
            for entity in similar_entities:
                if entity['node_id'] not in seen_ids:
                    all_nodes.append(entity)
                    seen_ids.add(entity['node_id'])
            
            # If no vector results, try text search
            if not all_nodes:
                text_results = self.graph_service.search_nodes_by_title_content(query_text, limit=top_k)
                if text_results:
                    all_nodes.extend(text_results)
                else:
                    # Try searching with extracted keywords
                    for ent in entities_from_query:
                        if len(ent) >= 3:
                            ent_results = self.graph_service.search_nodes_by_title_content(ent, limit=2)
                            if ent_results:
                                all_nodes.extend(ent_results)
                            if len(all_nodes) >= top_k:
                                break
            
            if not all_nodes:
                return {
                    "context": "--- Context from Notes ---\nNo relevant local documents found for this query.",
                    "source_nodes": []
                }
            
            # Prepare source nodes for frontend
            source_nodes = []
            for node in all_nodes[:top_k]:
                source_nodes.append({
                    "id": node['node_id'],
                    "title": node.get('title', node.get('name', '')),
                    "snippet": ((node.get('content') or '') if node.get('content') else (node.get('title', node.get('name',''))) )[:200]
                })
            
            # Synthesize context
            context_parts = ["--- Context from Notes ---"]
            for node in all_nodes[:top_k]:  # Limit to top_k results
                title = node.get('title', node.get('name', ''))
                content = node.get('content', '')
                if not content and title:
                    content = title
                snippet = (content or '')[:300]  # Longer snippets for custom commands
                context_parts.append(f"- {title}\n  {snippet}")
            
            context_text = "\n".join(context_parts)
            logger.info(f"Custom command found {len(all_nodes)} relevant documents")
            return {
                "context": context_text,
                "source_nodes": source_nodes
            }
            
        except Exception as e:
            logger.error(f"Error getting context for custom command: {e}")
            return {
                "context": "--- Context from Notes ---\nError retrieving local documents.",
                "source_nodes": []
            }

    async def _process_rag_query(self, query_text: str, history: Optional[list] = None, is_chat: bool = True) -> Dict[str, Any]:
        """
        Process regular RAG queries (non-command queries).
        """
        # 1. Smart keyword extraction without LLM call
        # Skip expensive LLM analysis for simple queries
        entities_from_query = []
        
        # Basic keyword extraction from query text
        import re
        # Extract potential keywords (words longer than 3 characters)
        words = re.findall(r'\b\w{4,}\b', query_text.lower())
        entities_from_query = list(set(words))  # Remove duplicates
        
        # Filter out common stop words
        stop_words = {'what', 'how', 'when', 'where', 'why', 'who', 'which', 'this', 'that', 'these', 'those', 'and', 'or', 'but', 'with', 'for', 'from', 'the', 'a', 'an', 'is', 'are', 'was', 'were', 'be', 'been', 'have', 'has', 'had', 'do', 'does', 'did', 'will', 'would', 'could', 'should', 'can', 'may', 'might', 'must'}
        entities_from_query = [word for word in entities_from_query if word not in stop_words]

        # Add debug logging for keyword extraction
        logger.info(f"RAG Query Debug - Original query: '{query_text}'")
        logger.info(f"RAG Query Debug - Extracted keywords: {entities_from_query}")

        # 2. Embed the query AND the extracted entities
        query_embedding_result = await self.embedding_service.get_embedding(query_text)
        if not query_embedding_result.success:
            logger.warning("Embedding failed; falling back to direct LLM answer without RAG context.")
            llm_fallback = await self.llm_client.generate(query_text, use_chat_endpoint=True)
            if llm_fallback.success:
                return {
                    "answer": llm_fallback.content,
                    "source_nodes": [],
                    "note": "Answer generated without vector search due to embedding failure"
                }
            else:
                return {"error": llm_fallback.error_message or "LLM fallback failed"}

        # 3. Multi-pronged search
        top_k = getattr(self.config, 'rag_search_top_k', 10)  # 使用默认值
        # a) Vector search on the query
        similar_nodes = self.graph_service.find_similar_nodes(query_embedding_result.embedding, top_k=top_k)
        logger.info(f"RAG Query Debug - Vector search results count: {len(similar_nodes)}")
        
        # b) Vector search on extracted entities
        similar_entities = []
        if entities_from_query:
            entity_query_text = " ".join(entities_from_query)
            entity_embedding_result = await self.embedding_service.get_embedding(entity_query_text)
            if entity_embedding_result.success:
                similar_entities = self.graph_service.find_similar_entities(entity_embedding_result.embedding, top_k=top_k)
                logger.info(f"RAG Query Debug - Entity search results count: {len(similar_entities)}")

        # Combine initial results to get starting points for traversal
        initial_node_ids = {node['node_id'] for node in similar_nodes}
        initial_node_ids.update(entity['node_id'] for entity in similar_entities)
        
        if not initial_node_ids:
            # Text-search fallback when vector search yields nothing.
            # 1) try query string
            context_nodes = []  # ensure variable exists
            text_results = self.graph_service.search_nodes_by_title_content(query_text, limit=top_k)
            if text_results:
                initial_node_ids.update(node['node_id'] for node in text_results)
                context_nodes.extend(text_results)
                logger.info(f"RAG Query Debug - Text search results count: {len(text_results)}")
            # 2) try each extracted entity separately to capture keyword hits
            if (not initial_node_ids) and entities_from_query:
                for ent in entities_from_query:
                    # Skip very short tokens to avoid noise
                    if len(ent) < 3:
                        continue
                    ent_results = self.graph_service.search_nodes_by_title_content(ent, limit=3)
                    if ent_results:
                        initial_node_ids.update(n['node_id'] for n in ent_results)
                        # ensure list exists
                        if context_nodes is None:
                            context_nodes = []
                        context_nodes.extend(ent_results)
                        logger.info(f"RAG Query Debug - Entity '{ent}' text search results count: {len(ent_results)}")
                    if len(initial_node_ids) >= top_k:
                        break
         
        if not initial_node_ids:
            logger.info(f"RAG Query Debug - No initial nodes found, falling back to chat")
            return await self._chat_fallback(history, query_text)
        
        # 4. Traversal: Expand from initial nodes to build a rich context
        context_nodes = []
        max_hops = getattr(self.config, 'rag_traversal_max_hops', 2)  
        
        for node_id in initial_node_ids:
            # get nodes
            node = self.graph_service.get_node_by_id(node_id)
            if node:
                context_nodes.append(node)
                # get neighbors
                neighbors = self.graph_service.get_neighbors(node_id)
                context_nodes.extend(neighbors[:5])  # limit neighbors to 5
        
        logger.debug(f"Context nodes count: {len(context_nodes)}, ids: {[n['node_id'] for n in context_nodes]}")

        # 5. Synthesize context from the final node list
        context_text = self._synthesize_context_from_nodes(context_nodes)

        # 6. Generate a response
        if is_chat:
            # Use the centralized prompt from prompts.py
            prompt = prompts.MINIRAG_RAG_RESPONSE_PROMPT.format(context=context_text, query=query_text)
            # For chat models, the prompt becomes the user message, and we can have a simpler system message.
            messages = [
                {"role": "system", "content": "You are a helpful AI assistant. **IMPORTANT: Always format your responses using Markdown syntax** - use headers (# ##), lists (- *), bold (**text**), italic (*text*), code blocks (```), and other Markdown formatting as appropriate."},
                {"role": "user", "content": prompt}
            ]

            chat_res = await self.llm_client.chat(messages=messages, format_json=False)
            if chat_res.success and chat_res.content:
                return {
                    "answer": chat_res.content,
                    "source_nodes": [
                        {"id": n['node_id'],
                         "title": n.get('title', n.get('name', '')),
                         "snippet": ((n.get('content') or '') if n.get('content') else (n.get('title', n.get('name',''))) )[:200]}
                        for n in context_nodes]
                }
            # If chat failed, fallback
            return await self._chat_fallback(history, query_text)
        else:
            prompt = QA_PROMPT.format(context=context_text, question=query_text)
            llm_result = await self.llm_client.generate(prompt, use_chat_endpoint=False)
            if llm_result and llm_result.content:
                return {
                    "answer": llm_result.content,
                    "source_nodes": [
                        {"id": n['node_id'],
                         "title": n.get('title', n.get('name', '')),
                         "snippet": ((n.get('content') or '') if n.get('content') else (n.get('title', n.get('name',''))) )[:200]}
                        for n in context_nodes]
                }
            return await self._chat_fallback(history, query_text)

    async def _chat_fallback(self, history: list, query_text: str) -> Dict[str, Any]:
        """Fallback to pure chat with provided history when RAG retrieval fails."""
        messages = []
        for item in reversed(history):
            # 支持多种格式的历史项：dict、plist 转 dict、字符串
            if isinstance(item, (list, tuple)):
                try:
                    item = dict(item)
                except Exception:
                    pass
            if isinstance(item, dict):
                role = item.get('role') or item.get(':role')
                content = item.get('content') or item.get(':content')
                if role and content:
                    messages.append({"role": str(role), "content": str(content)})
            elif isinstance(item, str):
                # 若无法得知角色，默认视为 user 历史输入
                if item.strip():
                    messages.append({"role": "user", "content": item.strip()})
            else:
                # 其他类型忽略或记录
                logger.debug(f"Skipped unsupported history item type: {type(item)}")
        
        # Add system message to ensure Markdown formatting
        system_message = {
            "role": "system", 
            "content": "You are a helpful AI assistant. **IMPORTANT: Always format your responses using Markdown syntax** - use headers (# ##), lists (- *), bold (**text**), italic (*text*), code blocks (```), and other Markdown formatting as appropriate."
        }
        messages.insert(0, system_message)
        messages.append({"role": "user", "content": query_text})

        chat_result = await self.llm_client.chat(messages=messages, use_chat_endpoint=True)
        if chat_result.success:
            return {"answer": chat_result.content, "source_nodes": []}
        return {"answer": "I could not find any relevant information."}

    def _synthesize_context_from_nodes(self, nodes: List[Dict]) -> str:
        """Creates a text context from a list of nodes."""
        context_parts = []
        seen_ids = set()
        context_parts.append("--- Context from Notes ---")
        for node in nodes:
            node_id = node.get('node_id')
            if node_id in seen_ids:
                continue
            seen_ids.add(node_id)
            title = node.get('title', node.get('name', ''))
            content = node.get('content', '')
            if not content and title:
                content = title
            snippet = (content or '')[:200]
            context_parts.append(f"- {title}\n  {snippet}")
        return "\n".join(context_parts)

    # ==============================================================================
    # Phase 2: High-Precision RAG/Query Methods (inspired by MiniRAG)
    # ==============================================================================

    async def _analyze_intent_and_get_entities(self, query_text: str, is_chat: bool = False) -> Dict[str, Any]:
        """
        Analyzes the user's query to understand intent and extract key entities.
        Implements caching to reduce duplicate LLM calls while preserving intent analysis.
        """
        logger.debug(f"Analyzing intent for query: '{query_text}'")
        
        # Check cache first
        cache_key = f"{query_text}_{is_chat}"
        current_time = time.time()
        
        # Clean expired cache entries
        expired_keys = [
            key for key, (timestamp, _) in self._intent_cache.items()
            if current_time - timestamp > self._cache_ttl
        ]
        for key in expired_keys:
            del self._intent_cache[key]
        
        # Check if we have a cached result
        if cache_key in self._intent_cache:
            cached_timestamp, cached_result = self._intent_cache[cache_key]
            if current_time - cached_timestamp <= self._cache_ttl:
                logger.debug(f"Using cached intent analysis for query: '{query_text[:50]}...'")
                return cached_result
        
        try:
            # For the answer type pool, we can use the defined NodeType enums
            # In a more advanced system, this could be dynamic.
            answer_type_pool = {nt.value: [nt.value] for nt in NodeType}

            prompt = prompts.MINIRAG_QUERY_TO_KEYWORDS_PROMPT.format(
                query=query_text,
                answer_type_pool=json.dumps(answer_type_pool, indent=2)
            )

            llm_result = await self.llm_client.generate(prompt, use_chat_endpoint=is_chat)
            response_json_str = llm_result.content
            
            # The LLM might return a markdown code block, so we need to clean it
            if response_json_str.strip().startswith("```json"):
                response_json_str = response_json_str.strip()[7:-3].strip()

            intent_data = json.loads(response_json_str)
            logger.info(f"Intent analysis complete: {intent_data}")
            
            # Cache the result
            if len(self._intent_cache) >= self._cache_max_size:
                # Remove oldest entry when cache is full
                oldest_key = min(self._intent_cache.keys(), key=lambda k: self._intent_cache[k][0])
                del self._intent_cache[oldest_key]
            
            self._intent_cache[cache_key] = (current_time, intent_data)
            
            return intent_data

        except Exception as e:
            logger.error(f"Failed to analyze query intent: {e}", exc_info=True)
            # Fallback to a simple keyword extraction if intent analysis fails
            fallback_result = {
                "answer_type_keywords": [],
                "entities_from_query": query_text.split() 
            }
            
            # Cache the fallback result too to avoid repeated failures
            if len(self._intent_cache) < self._cache_max_size:
                self._intent_cache[cache_key] = (current_time, fallback_result)
            
            return fallback_result

    async def _search_and_score_paths(
        self,
        query_text: str,
        entities_from_query: List[str],
        answer_type_keywords: List[str]
    ) -> Dict[str, Any]:
        """Hybrid vector + graph retrieval. Returns subgraph with scored nodes."""
        cfg = self.config
        neighbor_depth = getattr(cfg, 'rag_neighbor_depth', 2)
        ratio = getattr(cfg, 'rag_vector_graph_ratio', 0.5)
        top_k_total = getattr(cfg, 'rag_vector_results', 10)
        vec_k = max(1, int(top_k_total * ratio))

        bm25_k = getattr(cfg, 'rag_bm25_top_k', 10)
        bm25_weight = getattr(cfg, 'rag_bm25_weight', 0.5)

        logger.debug(
            f"Hybrid search: query='{query_text[:60]}…', entities={entities_from_query}, vec_k={vec_k}, depth={neighbor_depth}")

        # 1. Build start nodes from explicit entities
        start_node_ids: List[str] = []
        for ent in entities_from_query:
            node_d = self.graph_service.get_node_by_id(ent)
            if node_d:
                start_node_ids.append(node_d['node_id'])

        # 2. Vector search to get similar TAG/ENTITY nodes
        vec_node_map: Dict[str, Dict] = {}
        try:
            emb_res: EmbeddingResult = await self.embedding_service.get_embedding(query_text)
            if emb_res.success:
                similar_entities = self.graph_service.find_similar_entities(emb_res.embedding, top_k=vec_k)
                for idx, ent in enumerate(similar_entities):
                    ent_id = ent['node_id']
                    ent['distance_rank'] = idx  # keep rank
                    vec_node_map[ent_id] = ent
                    start_node_ids.append(ent_id)  # treat similar tags as start nodes too
        except Exception as e:
            logger.warning(f"Vector search failed: {e}")

        # 2.5 BM25 keyword search for additional candidate nodes
        bm25_node_map: Dict[str, Dict] = {}
        max_bm25_raw = 0.0
        if self.bm25_service:
            try:
                bm25_results = self.bm25_service.query(query_text, top_k=bm25_k)
                for node_id, raw_score in bm25_results:
                    max_bm25_raw = max(max_bm25_raw, raw_score)
                    node_d = self.graph_service.get_node_by_id(node_id)
                    if node_d:
                        bm25_node_map[node_id] = {**node_d, "bm25_raw": raw_score}
                        start_node_ids.append(node_id)
            except Exception as e:
                logger.warning(f"BM25 retrieval failed: {e}")

        # Deduplicate start ids
        start_node_ids = list(dict.fromkeys(start_node_ids))
        if not start_node_ids:
            logger.warning("No start nodes found, returning empty subgraph")
            return {"nodes": {}, "relations": {}}

        # 3. Graph expansion (BFS)
        graph_nodes = self._bfs_expand_neighbors(start_node_ids, max_depth=neighbor_depth)

        # Merge vector nodes into graph_nodes (may add extra metadata)
        for nid, ndata in vec_node_map.items():
            if nid not in graph_nodes:
                graph_nodes[nid] = ndata
            else:
                graph_nodes[nid].update(ndata)

        # Merge bm25 nodes
        for nid, ndata in bm25_node_map.items():
            # Normalize bm25 score now that max is known
            raw = ndata.get("bm25_raw", 0.0)
            if max_bm25_raw > 0:
                ndata["bm25_score"] = raw / max_bm25_raw
            else:
                ndata["bm25_score"] = 0.0
            if nid not in graph_nodes:
                graph_nodes[nid] = ndata
            else:
                graph_nodes[nid].update(ndata)

        # 4. Score nodes (vector + bm25 + heuristics)
        scored_nodes: Dict[str, Dict] = {}
        def _score(node: Dict) -> float:
            score = 0.0
            # Vector similarity: 1 - distance (if present)
            dist = node.get('distance')
            if dist is not None:
                score += max(0.0, 1.0 - float(dist)) * 2.0
            # BM25 score (already normalized 0~1)
            bm25_s = node.get('bm25_score')
            if bm25_s is not None:
                score += bm25_s * bm25_weight
            # Start nodes boost
            if node['node_id'] in start_node_ids:
                score += 0.5
            # Type keyword boost
            if answer_type_keywords and node.get('type') in answer_type_keywords:
                score += 0.5
            return score

        for nid, node in graph_nodes.items():
            node['relevance_score'] = _score(node)
            scored_nodes[nid] = node

        # TODO: relations scoring (future)
        subgraph = {"nodes": scored_nodes, "relations": {}}
        logger.info(f"Hybrid search complete. Nodes: {len(scored_nodes)}")
        return subgraph

    def _build_context_from_subgraph(self, subgraph: Dict[str, Any], top_k: int = 5):
        """
        根据打分后的子图构建用于 LLM 的上下文文本，同时生成前端需要的 source_nodes 列表。

        Args:
            subgraph: 包含节点与关系的字典，节点已带有 `relevance_score` 字段。
            top_k: 需纳入上下文的最高得分节点数量。

        Returns:
            Tuple[str, List[Dict]]: (context_str, source_nodes)
        """
        nodes = list(subgraph.get("nodes", {}).values())
        relations = list(subgraph.get("relations", {}).values())

        if not nodes:
            return "No relevant information found in the knowledge graph.", []

        # 1. 依据相关度排序
        nodes.sort(key=lambda x: x.get('relevance_score', 0.0), reverse=True)

        # 2. 取前 top_k，并按标题去重（忽略大小写）
        seen_titles: set = set()
        top_nodes: list = []
        for node in nodes:
            title = (node.get('title') or node.get('name') or '').strip()
            if not title:
                continue
            title_lc = title.lower()
            if title_lc in seen_titles:
                continue
            seen_titles.add(title_lc)
            top_nodes.append(node)
            if len(top_nodes) >= top_k:
                break

        top_node_ids = {n['node_id'] for n in top_nodes}

        # 3. 构建上下文文本
        context_parts: list = ["--- Relevant Concepts and Entities ---"]
        source_nodes: list = []
        snippet_len = getattr(self.config, 'rag_snippet_len', 200)

        for node in top_nodes:
            title = node.get('title') or node.get('name') or 'Untitled Node'
            score = node.get('relevance_score', 0.0)
            snippet = self._get_snippet(node.get('content') or title, limit=snippet_len)

            context_parts.append(f"- {title} (ID: {node['node_id']}, Type: {node.get('type')}, Score: {score:.2f})")
            if snippet:
                context_parts.append(f"  - Snippet: {snippet}")

            source_nodes.append({
                "id": node['node_id'],
                "title": title,
                "snippet": snippet
            })

        # 4. 关系信息
        context_parts.append("\n--- Identified Relationships ---")
        included_relations_count = 0
        for rel in relations:
            if rel.get('source_id') in top_node_ids and rel.get('target_id') in top_node_ids:
                source_title = subgraph['nodes'].get(rel['source_id'], {}).get('title', 'Unknown')
                target_title = subgraph['nodes'].get(rel['target_id'], {}).get('title', 'Unknown')
                rel_type = rel.get('type', 'RELATED_TO')
                context_parts.append(f"- ({source_title}) --[{rel_type}]--> ({target_title})")
                included_relations_count += 1

        if included_relations_count == 0:
            context_parts.append("No direct relationships found among the top entities.")

        context_str = "\n".join(context_parts)
        return context_str, source_nodes

    # ==============================================================================
    # Phase 3: High-Quality Data Ingestion & Autotagging
    # ==============================================================================

    



    

    async def infer_relations(self, text: str, entities: List[str]) -> List[Relationship]:
        """Infer relationships between provided entities within the given text."""
        if not entities:
            return []
        try:
            ent_list = ", ".join(entities)
            prompt = RELATION_INFERENCE_PROMPT.format(entities=ent_list, input_text=text)
            llm_res = await self.llm_client.generate(prompt, use_chat_endpoint=False)
            if not llm_res or not llm_res.content:
                logger.warning("LLM returned empty result for relation inference.")
                return []
            parsed = self._parse_llm_json_output_for_relations(llm_res.content)
            return parsed
        except Exception as e:
            logger.error(f"infer_relations failed: {e}", exc_info=True)
            return []

    def _parse_llm_json_output_for_relations(self, llm_output: str) -> List[Relationship]:
        """Parse JSON object with relationships key from LLM."""
        try:
            match = re.search(r'\{.*\}', llm_output, re.DOTALL)
            if not match:
                return []
            data = json.loads(match.group(0))
            rels = data.get("relationships", [])
            parsed = []
            for item in rels:
                if all(k in item for k in ("source", "target", "type")):
                    parsed.append(Relationship(source=item["source"], target=item["target"], type=item["type"]))
            return parsed
        except Exception as e:
            logger.error("Failed to parse relation JSON: %s", e)
            return []



    # --- Utility Helpers ---
    def _get_snippet(self, text: str, limit: int = 200) -> str:
        """Return a clean snippet up to <limit> chars, ending at nearest sentence boundary."""
        if not text:
            return ""
        text = text.strip().replace("\n", " ")
        if len(text) <= limit:
            return text
        cut = text[:limit]
        # Try to cut at last period
        last_period = cut.rfind('.')
        if last_period > limit * 0.5:
            return cut[:last_period+1]
        # Else cut at space
        last_space = cut.rfind(' ')
        if last_space > 0:
            return cut[:last_space] + '…'
        return cut + '…'

    def _bfs_expand_neighbors(self, start_ids: List[str], max_depth: int = 2, relation_whitelist: Optional[List[str]] = None, per_node_limit: int = 5) -> Dict[str, Dict]:
        """Breadth-first traverse neighbors up to depth, return node dicts."""
        relation_whitelist = relation_whitelist or ["HAS_TAG", "REF_TO"]
        visited = set(start_ids)
        queue = deque([(nid, 0) for nid in start_ids])
        nodes: Dict[str, Dict] = {}
        while queue:
            node_id, depth = queue.popleft()
            node = self.graph_service.get_node_by_id(node_id)
            if node:
                nodes[node_id] = node
            if depth >= max_depth:
                continue
            neighbors = self.graph_service.get_neighbors(node_id)
            cnt = 0
            for nb in neighbors:
                if cnt >= per_node_limit:
                    break
                nb_id = nb.get('node_id')
                rel_type = None  # placeholder, GraphService doesn't return relation type here
                if nb_id and nb_id not in visited and (not relation_whitelist or rel_type in relation_whitelist):
                    visited.add(nb_id)
                    queue.append((nb_id, depth+1))
                    cnt += 1
        return nodes

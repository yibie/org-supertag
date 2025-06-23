#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
import asyncio
import json
import dataclasses
import time
from typing import Dict, Any, List, Optional
import traceback

from simtag.core.memory_engine import MemoryItem, MemoryItemType
from simtag.services.content_processor import ContentProcessor, ProcessingConfig, ContentItem
from simtag.core.entity_extractor import ExtractedEntity
from simtag.utils.unified_tag_processor import normalize_payload
from simtag.core.graph_service import GraphService

logger = logging.getLogger(__name__)

class EPCHandler:
    def __init__(self, entity_extractor, graph_service: GraphService, memory_engine, rag_engine, config, content_processor, memory_synthesizer, engine, llm_client, ner_service, node_processor):
        self.entity_extractor = entity_extractor
        self.graph_service = graph_service
        self.memory_engine = memory_engine
        self.rag_engine = rag_engine
        self.config = config
        self.content_processor = content_processor
        self.memory_synthesizer = memory_synthesizer
        self.engine = engine
        self.llm_client = llm_client
        self.ner_service = ner_service
        self.node_processor = node_processor
        logger.info("EPCHandler initialized with extended dependencies (including node processor).")

    async def _async_analyze_input(self, input_data: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """
        Asynchronous core logic for analyzing input.
        """
        try:
            logger.info(f"_async_analyze_input called with data keys: {list(input_data.keys())}")
            text = input_data.get('text')
            node_id = input_data.get('node_id')
            org_tags = input_data.get('tags') # Optional list of strings

            if not text or not node_id:
                logger.warning("analyze_input: 'text' or 'node_id' missing from input_data.")
                return {'status': 'error', 'message': "'text' and 'node_id' are required."}

            # 1. Extract entities and relations
            extracted_entities = await self.entity_extractor.extract_from_org_node(
                node_content=text,
                node_id=node_id,
                existing_tags=org_tags
            )
            # Relations extraction is not yet implemented in the entity extractor
            extracted_relations = []
            logger.info(f"Extracted {len(extracted_entities)} entities and {len(extracted_relations)} relations from node {node_id}.")

            # 2. Convert to format expected by GraphService and upsert
            if extracted_entities:
                tags_for_gs = [{
                    'tag_id': e.name, # Use name as ID for tag entities
                    'name': e.name,
                    'description': e.attributes.get('description', '')
                } for e in extracted_entities]
                self.graph_service.bulk_upsert_tags(tags_for_gs)

            if extracted_relations:
                # Assuming extracted_relations are in a compatible format
                self.graph_service.bulk_upsert_relations(extracted_relations)

            logger.info(f"Upserted entities and relations for node {node_id} into GraphService.")

            # 3. Record analysis in memory
            analysis_summary = {
                'node_id': node_id,
                'num_entities_extracted': len(extracted_entities),
                'num_relations_extracted': len(extracted_relations),
                'tags_provided': org_tags
            }
            mem_item = MemoryItem(
                id=f"analysis_{node_id}_{int(time.time())}",
                type=MemoryItemType.SYSTEM_STATE,
                content=analysis_summary,
                metadata={'source': 'analyze_input'}
            )
            await self.memory_engine.add_memory_item(mem_item)

            return {
                'status': 'success',
                'message': f"Analyzed node {node_id}.",
                'num_entities': len(extracted_entities),
                'num_relations': len(extracted_relations)
            }

        except Exception as e:
            logger.error(f"Error in _async_analyze_input: {e}\n{traceback.format_exc()}")
            return {'status': 'error', 'message': str(e)}

    def analyze_input(self, input_data: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """
        EPC method: Analyzes input text, extracts entities/relations, updates KG, and stores analysis in memory.
        """
        return asyncio.run(self._async_analyze_input(input_data))

    async def _async_ask_question(self, question_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Asynchronous core logic for asking a question.
        """
        try:
            query_text = question_data.get('query_text')
            session_id = question_data.get('session_id', f"session_{int(time.time())}")
            logger.info(f"_async_ask_question called for session {session_id} with query: '{query_text[:50]}...'")

            if not query_text:
                logger.warning("ask_question: 'query_text' missing.")
                return {'answer': "Error: 'query_text' is required.", 'session_id': session_id, 'status': 'error', 'metadata': {}}

            dialogue_mode = await self.memory_engine.get_user_preference(key=f"session_mode_{session_id}", default='normal')
            logger.info(f"Using dialogue mode '{dialogue_mode}' for session {session_id}")

            embedding_config = ProcessingConfig(mode=ProcessingConfig.EMBEDDING_ONLY)
            embedding_processor = ContentProcessor(embedding_config)
            
            content_item = ContentItem(id=f"query_{session_id}_{int(time.time())}", text=query_text)
            processing_result = await embedding_processor.process_single(content_item)
            
            if not (processing_result.success and processing_result.embedding_result and processing_result.embedding_result.success):
                logger.error("Failed to get query embedding for ask_question.")
                return {'answer': "Error: Could not generate query embedding.", 'session_id': session_id, 'status': 'error', 'metadata': {}}
            
            query_embedding = processing_result.embedding_result.embedding
            query_extracted_entities = await self.entity_extractor.extract_from_org_node(
                node_content=query_text, node_id=f"query_epc_{session_id}_{int(time.time())}"
            )

            active_mode_config = self.config.rag_mode_presets.get(dialogue_mode, self.config.rag_mode_presets.get('normal', {}))
            rag_strategy_name = active_mode_config.get('rag_retrieval_mode', getattr(self.config, 'rag_retrieval_mode', 'light'))
            rag_top_k = active_mode_config.get('rag_vector_results', getattr(self.config, 'rag_vector_results', 10))

            rag_context_docs = await self.rag_engine.retrieve_context(
                query_text=query_text,
                query_embedding=query_embedding,
                extracted_entities=query_extracted_entities,
                strategy=rag_strategy_name,
                top_k=rag_top_k,
            )

            prompt_template_override = active_mode_config.get('prompt_template')
            final_answer_text: str
            response_metadata = {'mode': dialogue_mode}

            if dialogue_mode == 'socratic':
                socratic_prompt_template = prompt_template_override or """...""" # Socratic prompt
                socratic_response_str = await self.rag_engine.generate_response(
                    query_text=query_text, context_docs=rag_context_docs, prompt_template=socratic_prompt_template
                )
                try:
                    socratic_data = json.loads(socratic_response_str)
                    final_answer_text = socratic_data.get("question", "I'm not sure what to ask next. Could you elaborate?")
                    response_metadata['hint'] = socratic_data.get("hint", "")
                except (json.JSONDecodeError, TypeError):
                    final_answer_text = socratic_response_str
                    response_metadata['hint'] = "Error: The AI response was not in the expected structured format."
            else:
                final_answer_text = await self.rag_engine.generate_response(
                    query_text=query_text, context_docs=rag_context_docs, prompt_template=prompt_template_override
                )

            await self.memory_engine.add_dialogue_turn(session_id, "user", query_text, metadata={'mode': dialogue_mode})
            ai_turn_metadata = {'mode': dialogue_mode, 'rag_docs_count': len(rag_context_docs)}
            if 'hint' in response_metadata and response_metadata['hint']:
                ai_turn_metadata['hint'] = response_metadata['hint']
            await self.memory_engine.add_dialogue_turn(session_id, "ai", final_answer_text, metadata=ai_turn_metadata)
            
            snapshot_content = {
                'query': query_text, 'dialogue_mode': dialogue_mode, 'rag_strategy': rag_strategy_name,
                'retrieved_docs_count': len(rag_context_docs),
                'retrieved_docs_preview': [dataclasses.asdict(doc) if not isinstance(doc, dict) else doc for doc in rag_context_docs[:3]],
            }
            await self.memory_engine.record_context_snapshot(
                triggering_query=query_text, context_content=snapshot_content, llm_response=final_answer_text
            )

            return {'answer': final_answer_text, 'session_id': session_id, 'status': 'success', 'metadata': response_metadata}

        except Exception as e:
            logger.error(f"Error in _async_ask_question: {e}\n{traceback.format_exc()}")
            return {'answer': f"An error occurred: {str(e)}", 'session_id': question_data.get('session_id', 'error_session'), 'status': 'error', 'metadata': {}}

    def ask_question(self, question_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        EPC method: Handles a question from the user.
        """
        return asyncio.run(self._async_ask_question(question_data))

    async def _async_get_suggestions(self, context_data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Asynchronous core logic for getting suggestions.
        """
        try:
            if isinstance(context_data, list) and len(context_data) == 1 and isinstance(context_data[0], dict):
                context_data = context_data[0]

            node_content = context_data.get('current_node_content')
            node_id = context_data.get('current_node_id')

            if not node_content or not node_id:
                return []

            embedding_config = ProcessingConfig(mode=ProcessingConfig.EMBEDDING_ONLY)
            embedding_processor = ContentProcessor(embedding_config)
            
            content_item = ContentItem(id=node_id, text=node_content)
            processing_result = await embedding_processor.process_single(content_item)
            
            if not (processing_result.success and processing_result.embedding_result and processing_result.embedding_result.success):
                return []
            
            content_embedding = processing_result.embedding_result.embedding
            extracted_node_entities = await self.entity_extractor.extract_from_org_node(
                node_content=node_content, node_id=node_id
            )

            suggestion_rag_strategy = getattr(self.config, 'suggestion_rag_strategy', 'light') 
            suggestion_top_k = getattr(self.config, 'suggestion_top_k', 5)

            related_docs = await self.rag_engine.retrieve_context(
                query_text=node_content[:1000],
                query_embedding=content_embedding,
                extracted_entities=extracted_node_entities,
                strategy=suggestion_rag_strategy,
                top_k=suggestion_top_k 
            )

            suggestions = []
            for doc in related_docs:
                if doc.get('id') == node_id: continue

                suggestions.append({
                    'type': doc.get('source', 'related_document'),
                    'id': str(doc.get('id', '')),
                    'title': doc.get('text', '')[:80] + "..." if len(doc.get('text', '')) > 80 else doc.get('text', ''),
                    'snippet': doc.get('text', '')[:200] + "..." if len(doc.get('text', '')) > 200 else doc.get('text', ''),
                    'score': float(doc.get('score', 0.0)),
                    'metadata': {'entity_type': doc.get('entity_type')} if doc.get('entity_type') else {}
                })
            
            suggestions.sort(key=lambda s: s.get('score', 0.0), reverse=True)
            
            return suggestions[:suggestion_top_k]

        except Exception as e:
            logger.error(f"Error in _async_get_suggestions: {e}\n{traceback.format_exc()}")
            return []

    def get_suggestions(self, context_data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        EPC method: Provides suggestions based on the current context.
        """
        return asyncio.run(self._async_get_suggestions(context_data))

    async def _async_set_dialogue_mode(self, session_id: str, mode_name: str, mode_config: Optional[Dict[str, Any]] = None) -> bool:
        """
        Asynchronous core logic for setting dialogue mode.
        """
        try:
            if mode_config:
                await self.memory_engine.update_user_preference(
                    key=f"dialogue_mode_custom_config_{mode_name}", 
                    value=mode_config
                )
            
            session_mode_key = f"session_mode_{session_id}"
            await self.memory_engine.update_user_preference(key=session_mode_key, value=mode_name)
            
            return True
        except Exception as e:
            logger.error(f"Error in _async_set_dialogue_mode for session '{session_id}': {e}\n{traceback.format_exc()}")
            return False

    def set_dialogue_mode(self, session_id: str, mode_name: str, mode_config: Optional[Dict[str, Any]] = None) -> bool:
        """
        EPC method: Sets the active dialogue mode for a specific session.
        """
        return asyncio.run(self._async_set_dialogue_mode(session_id, mode_name, mode_config))

    async def _async_get_memory_dashboard(self) -> Dict[str, Any]:
        """
        Asynchronous core logic for getting the memory dashboard.
        """
        try:
            dashboard_data = await self.memory_engine.get_memory_dashboard_data()
            return dashboard_data
        except Exception as e:
            logger.error(f"Error in _async_get_memory_dashboard: {e}\n{traceback.format_exc()}")
            return {'status': 'error', 'message': str(e)}

    def get_memory_dashboard(self) -> Dict[str, Any]:
        """
        EPC method: Retrieves data for the memory management dashboard.
        """
        return asyncio.run(self._async_get_memory_dashboard())

    async def _async_update_memory_epc(self, update_data: Dict[str, Any]) -> bool:
        """
        Asynchronous core logic for updating memory via EPC.
        """
        try:
            if isinstance(update_data, list) and len(update_data) == 1 and isinstance(update_data[0], dict):
                update_data = update_data[0]

            item_type_str = update_data.get('item_type')
            
            if not item_type_str:
                return False

            if item_type_str == MemoryItemType.USER_PREFERENCE.value:
                key = update_data.get('key')
                value = update_data.get('value')
                if key is not None:
                    await self.memory_engine.update_user_preference(key, value)
                    return True
                else:
                    return False
            
            elif item_type_str == MemoryItemType.USER_FEEDBACK.value:
                feedback_content = update_data.get('content')
                if feedback_content and isinstance(feedback_content, dict):
                    feedback_item = MemoryItem(
                        id=f"feedback_{int(time.time())}",
                        type=MemoryItemType.USER_FEEDBACK,
                        content=feedback_content,
                        metadata=update_data.get('metadata', {})
                    )
                    await self.memory_engine.add_memory_item(feedback_item)
                    return True
                else:
                    return False
            
            else:
                return False

        except Exception as e:
            logger.error(f"Error in _async_update_memory_epc: {e}\n{traceback.format_exc()}")
            return False

    def update_memory_epc(self, update_data: Dict[str, Any]) -> bool:
        """
        EPC method: Allows Emacs to update specific memory items.
        """
        return asyncio.run(self._async_update_memory_epc(update_data))

    async def _async_trigger_memory_synthesis(self, session_id: str) -> Dict[str, Any]:
        """Triggers the memory synthesis process for a given session."""
        try:
            candidates = await self.memory_synthesizer.synthesize_from_dialogue(session_id)
            return {"status": "success", "message": f"Found {len(candidates)} new candidates.", "new_candidate_count": len(candidates)}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    def trigger_memory_synthesis_for_session(self, session_id: str) -> Dict[str, Any]:
        """EPC Method: Triggers memory synthesis for a session."""
        return asyncio.run(self._async_trigger_memory_synthesis(session_id))

    def get_candidate_memories(self) -> Dict[str, Any]:
        """Returns all pending candidate memories, formatted for EPC."""
        try:
            candidates = self.memory_synthesizer.get_pending_candidates()
            serializable_candidates = [dataclasses.asdict(c) for c in candidates]
            for cand_dict in serializable_candidates:
                if 'proposed_item' in cand_dict and isinstance(cand_dict['proposed_item'], MemoryItem):
                     cand_dict['proposed_item'] = dataclasses.asdict(cand_dict['proposed_item'])
            return {"status": "success", "result": serializable_candidates}
        except Exception as e:
            return {"status": "error", "message": str(e)}
            
    async def _async_process_candidate_memory(self, candidate_id: str, action: str) -> Dict[str, Any]:
        """Processes a user's decision on a candidate memory."""
        candidate = self.memory_synthesizer.get_candidate_by_id(candidate_id)
        if not candidate:
            return {"status": "error", "message": f"Candidate with ID {candidate_id} not found."}

        try:
            if action == "accept":
                await self.memory_engine.add_memory_item(candidate.proposed_item)
                self.memory_synthesizer.discard_candidate(candidate_id)
                return {"status": "success", "message": f"Accepted candidate {candidate_id}."}
            elif action == "reject":
                self.memory_synthesizer.discard_candidate(candidate_id)
                return {"status": "success", "message": f"Rejected candidate {candidate_id}."}
            else:
                return {"status": "error", "message": f"Invalid action '{action}'."}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    def process_candidate_memory(self, candidate_id: str, action: str) -> Dict[str, Any]:
        """EPC Method: Processes user decision on a candidate memory."""
        return asyncio.run(self._async_process_candidate_memory(candidate_id, action))

    # --- Knowledge Archaeology ---
    async def _async_knowledge_archaeology_dig(self, query_text: str, top_k: int = 50) -> Dict[str, Any]:
        """Finds nodes related to a query and returns them sorted by date."""
        try:
            similar_nodes_result = await asyncio.to_thread(self.engine.find_similar_nodes, query_text, top_k)
            if not similar_nodes_result: return {"status": "success", "result": []}
            node_ids = [node_id for node_id, score in similar_nodes_result]
            node_details = self.graph_service.get_nodes_by_ids(node_ids)
            if not node_details: return {"status": "success", "result": []}
            valid_nodes = [n for n in node_details if n.get('document_date')]
            sorted_nodes = sorted(valid_nodes, key=lambda x: x['document_date'], reverse=True)
            return {"status": "success", "result": sorted_nodes}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    def knowledge_archaeology_dig(self, query_text: str, top_k: int = 50) -> Dict[str, Any]:
        """EPC method for the Knowledge Archaeology feature."""
        return asyncio.run(self._async_knowledge_archaeology_dig(query_text, top_k))

    # --- NER-based Features ---
    async def _async_get_ner_tag_suggestions(self, payload: Dict) -> Dict[str, Any]:
        try:
            payload = normalize_payload(payload)
            note_content = payload.get("note_content")
            if not note_content: return {"status": "error", "message": "note_content is required."}
            suggestions = await self.ner_service.suggest_tags_for_note(note_content, payload.get("existing_tags"))
            return {"status": "success", "result": suggestions}
        except Exception as e:
            return {"status": "error", "message": str(e)}
        
    def get_ner_tag_suggestions(self, payload):
        return asyncio.run(self._async_get_ner_tag_suggestions(payload))

    async def _async_discover_inferred_relationships(self, payload: Dict) -> Dict[str, Any]:
        try:
            payload = normalize_payload(payload)
            note_content = payload.get("note_content")
            if not note_content: return {"status": "error", "message": "note_content is required."}
            relationships = await self.ner_service.discover_tag_relationships(note_content)
            return {"status": "success", "result": relationships}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    def discover_inferred_relationships(self, payload: Dict) -> Dict[str, Any]:
        return asyncio.run(self._async_discover_inferred_relationships(payload))

    # --- Generic Text Generation ---
    async def _async_generate_text(self, prompt: str) -> Dict[str, Any]:
        try:
            if not prompt: return {"status": "error", "message": "Prompt cannot be empty."}
            response_text = await self.llm_client.generate(prompt)
            return {"status": "success", "result": response_text}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    def generate_text(self, prompt: str) -> Dict[str, Any]:
        return asyncio.run(self._async_generate_text(prompt))

    # --- MemoryEngine EPC Helpers ---
    def epc_get_user_preference(self, key: str, default: Optional[Any] = None) -> Optional[Any]:
        try:
            return asyncio.run(self.memory_engine.get_user_preference(key, default))
        except Exception:
            return default

    def epc_add_dialogue_turn(self, session_id: str, speaker: str, text: str, metadata: Optional[Dict[str, Any]]=None) -> Dict[str, Any]:
        try:
            dialogue_history_obj = asyncio.run(self.memory_engine.add_dialogue_turn(session_id, speaker, text, metadata or {}))
            if dialogue_history_obj:
                return {"status": "success", "session_id": dialogue_history_obj.session_id, "turn_count": len(dialogue_history_obj.turns)}
            return {"status": "error", "message": "Failed to add dialogue turn"}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    def epc_get_dialogue_history(self, session_id: str, max_turns: Optional[int] = None) -> Optional[Dict[str, Any]]:
        try:
            history_obj = asyncio.run(self.memory_engine.get_dialogue_history(session_id, max_turns))
            if history_obj:
                return {
                    "id": history_obj.id, "session_id": history_obj.session_id,
                    "turns": [{"speaker": t.speaker, "text": t.text, "timestamp": t.timestamp, "metadata": t.metadata} for t in history_obj.turns],
                    "summary": history_obj.summary, "timestamp": history_obj.timestamp, "metadata": history_obj.metadata
                }
            return None
        except Exception:
            return None

    def epc_summarize_dialogue_history(self, dialogue_history_id: str) -> Dict[str, Any]:
        try:
            success = asyncio.run(self.memory_engine.summarize_dialogue_history(dialogue_history_id))
            return {"status": "success" if success else "failure"}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    async def _async_analyze_conceptual_resonance(self, context_data: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """
        Asynchronous core logic for analyzing conceptual resonance.
        """
        if isinstance(context_data, list) and len(context_data) == 1 and isinstance(context_data[0], dict):
            context_data = context_data[0]

        source_node_id = context_data.get('source_node_id')
        target_node_id = context_data.get('target_node_id')

        if not source_node_id or not target_node_id:
            return None

        try:
            # 1. Fetch node details
            node_details_list = self.graph_service.get_nodes_by_ids([source_node_id, target_node_id])
            if len(node_details_list) < 2:
                return None
            
            nodes_by_id = {n['node_id']: n for n in node_details_list}
            source_node = nodes_by_id[source_node_id]
            target_node = nodes_by_id[target_node_id]

            # 2. Find common neighbors/tags
            source_neighbors = self.graph_service.get_neighbors(source_node_id)
            target_neighbors = self.graph_service.get_neighbors(target_node_id)
            source_neighbor_ids = {n['node_id'] for n in source_neighbors}
            target_neighbor_ids = {n['node_id'] for n in target_neighbors}
            common_neighbor_ids = list(source_neighbor_ids.intersection(target_neighbor_ids))

            # 3. Formulate a prompt for the LLM
            prompt = f"""
Analyze the conceptual resonance between two nodes.

Source Node:
Title: {source_node.get('title', 'N/A')}
Content: {source_node.get('content', '')[:500]}...

Target Node:
Title: {target_node.get('title', 'N/A')}
Content: {target_node.get('content', '')[:500]}...

They share {len(common_neighbor_ids)} common neighbors/tags.

Based on this information, provide:
1.  A "resonance_score" between 0.0 and 1.0.
2.  A brief, one-paragraph "explanation" of their relationship.
3.  A list of "connecting_concepts" (strings).

Return the response as a single JSON object.
"""
            # 4. Generate response from LLM
            response_text = await self.llm_client.generate(prompt)
            
            # 5. Parse and return the structured response
            try:
                structured_response = json.loads(response_text)
                return {"status": "success", "result": structured_response}
            except json.JSONDecodeError:
                return {"status": "error", "message": "LLM returned non-JSON response.", "raw_response": response_text}

        except Exception as e:
            logger.error(f"Error in _async_analyze_conceptual_resonance: {e}", exc_info=True)
            return {"status": "error", "message": str(e)}

    def analyze_conceptual_resonance(self, context_data: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        return asyncio.run(self._async_analyze_conceptual_resonance(context_data))

    async def _async_analyze_node_context(self, context_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Asynchronous core logic for analyzing a node's context.
        """
        logger.warning("analyze_node_context is temporarily simplified during refactoring.")
        if isinstance(context_data, list) and len(context_data) == 1 and isinstance(context_data[0], dict):
            context_data = context_data[0]
        
        node_id = context_data.get('node_id')
        if not node_id:
            return {"status": "error", "message": "node_id is required."}

        # Simplified implementation: just get the node and its direct neighbors.
        node_data = self.graph_service.get_node_by_id(node_id)
        if not node_data:
            return {"status": "error", "message": f"Node with ID {node_id} not found."}
        
        neighbors = self.graph_service.get_neighbors(node_id)
        
        return {
            "status": "success",
            "result": {
                "node": node_data,
                "neighbors": neighbors,
                "relations": [] # Stubbed for now
            }
        }

    def analyze_node_context(self, context_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        EPC method: Gets a node's local context from the knowledge graph.
        """
        return asyncio.run(self._async_analyze_node_context(context_data)) 
# simtag/module/rag_handler.py
import logging
from dependency_injector.wiring import inject, Provide
from ..services.llm_client import LLMClient
from ..services.embedding_service import EmbeddingService
from ..core.rag_engine import OrgSupertagRAGEngine
from ..utils.unified_tag_processor import normalize_payload
from typing import List, Dict

logger = logging.getLogger(__name__)

class RAGHandler:
    @inject
    def __init__(self, 
                 rag_engine: OrgSupertagRAGEngine = Provide['rag_engine'], 
                 llm_client: LLMClient = Provide['llm_client'],
                 embedding_service: EmbeddingService = Provide['embedding_service']):
        self.rag_engine = rag_engine
        self.llm_client = llm_client
        self.embedding_service = embedding_service
        self.methods = {
            "analyze_note": self.analyze_note,
            "continue_conversation": self.continue_conversation,
        }

    def _error(self, message: str) -> dict:
        """Logs an error and returns a formatted error dictionary."""
        logger.error(message, exc_info=True)
        return {"status": "error", "message": message}

    async def analyze_note(self, *args):
        """
        Analyzes a note using RAG and returns a summary.
        
        Payload from Elisp should now include more node data:
        '(("node-id" . "...") ("title" . "...") ("content" . "..."))
        """
        payload = normalize_payload(args)

        # The keys from Elisp are strings with a leading colon, e.g., ":id"
        node_id = payload.get(":id")
        content = payload.get(":content")
        node_data = payload

        if not node_id or not content:
            return self._error("Missing node-id or content in payload")
        
        try:
            # 1. Prepare the query text using the specialized method
            query_text = self.embedding_service.prepare_node_text_for_embedding(node_data)
            logger.info(f"Using prepared query text for RAG: '{query_text}'")

            # 2. Retrieve context with RAG using the prepared query text
            retrieved_context_result = await self.rag_engine.retrieve_context(query_text, top_k=3)
            retrieved_context = retrieved_context_result.get('documents', [])
            
            # --- DEBUG LOGGING ---
            logger.info(f"RAG retrieved context: {retrieved_context}")
            # --- END DEBUG LOGGING ---

            context_str = "\n\n".join([item['content'] for item in retrieved_context])

            # 3. Build the prompt for analysis
            prompt = f"""
Here is a note I am working on:
--- NOTE ---
{content}
--- END NOTE ---

Here is some additional context that might be relevant:
--- CONTEXT ---
{context_str}
--- END CONTEXT ---

Please provide a concise analysis and summary of my note, and context provided. Your analysis should:
1. Identify the main topic and key points of the note.
2. Synthesize the information from the provided context to add depth and connections.
3. Propose potential next steps or questions to explore based on the note and context.
4. Respond in the primary language used in the note.    

Keep your response focused and directly related to the provided material.
"""
            # 4. Call LLM for analysis
            analysis_response = await self.llm_client.generate(prompt, temperature=0.5)

            # 5. Return the result
            return {
                "node_id": node_id,
                "analysis": analysis_response or "",
                "conversation_history": [
                    {"role": "user", "content": prompt},
                    {"role": "assistant", "content": analysis_response or ""}
                ]
            }
        except Exception as e:
            # Use self.error to log and return a formatted error
            return self._error(f"Failed to analyze note: {e}")

    def _build_prompt_with_history(self, current_prompt: str, history: List[Dict]) -> str:
        """Builds a single string prompt from the current prompt and conversation history."""
        # Simple implementation: join assistant and user messages.
        # A more sophisticated approach could be used depending on the model's expected format.
        full_conversation = []
        for entry in history:
            if entry.get("role") == "user":
                full_conversation.append(f"User: {entry.get('content')}")
            elif entry.get("role") == "assistant":
                full_conversation.append(f"Assistant: {entry.get('content')}")

        full_conversation.append(f"User: {current_prompt}")
        
        return "\n".join(full_conversation)

    async def continue_conversation(self, *args):
        """
        Continues a conversation based on history and a new prompt.
        """
        try:
            payload = normalize_payload(args)
            history = payload.get("conversation-history", [])
            user_prompt = payload.get("prompt")

            if not user_prompt:
                return self._error("Prompt is missing.")

            # Build a single prompt string from history and the new prompt
            full_prompt = self._build_prompt_with_history(user_prompt, history)

            # Call the LLM with the consolidated prompt
            response_text = await self.llm_client.generate(
                prompt=full_prompt,
                conversation_history=[]  # History is now part of the prompt
            )

            if not response_text:
                return self._error("LLM returned an empty response.")

            # Update history for the next turn
            new_history = history + [
                {"role": "user", "content": user_prompt},
                {"role": "assistant", "content": response_text}
            ]

            return {
                "response": response_text,
                "conversation-history": new_history
            }
        except Exception as e:
            logger.error(f"Error continuing conversation: {e}", exc_info=True)
            return self._error(f"Failed to continue conversation: {e}") 
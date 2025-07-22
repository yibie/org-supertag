#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
from typing import Dict, Any, List

from simtag.services.rag_service import RAGService
from simtag.core.graph_service import GraphService
from simtag.utils.unified_tag_processor import normalize_payload

logger = logging.getLogger(__name__)

class AIHandler:
    """
    Dedicated handler for AI-powered chat commands.
    Provides specialized endpoints for tag suggestions, content expansion, and knowledge connections.
    """
    
    def __init__(self, rag_service: RAGService, graph_service: GraphService):
        self.rag_service = rag_service
        self.graph_service = graph_service
        self.logger = logging.getLogger(__name__)

    async def suggest_tags(self, payload: Dict) -> Dict[str, Any]:
        """
        Generate intelligent tag suggestions for given content.
        
        Args:
            payload: Dict containing 'content' key with text to analyze
            
        Returns:
            Dict with suggested tags and confidence scores
        """
        try:
            data = normalize_payload(payload)
            content = data.get('content')
            
            if not content:
                return {"error": "Content is required for tag suggestions"}
                
            self.logger.info(f"Generating tag suggestions for content: {content[:100]}...")
            
            # Use RAG service for intelligent tag generation
            prompt = f"""
            Analyze the following content and suggest relevant tags for organizing this information.
            Focus on key concepts, topics, themes, and actionable tags that would help categorize this content.
            
            Content: {content}
            
            Provide 5-10 relevant tags as a JSON array of strings.
            """
            
            response = await self.rag_service.query(prompt)
            
            # Extract tags from response
            if response.get("status") == "success":
                answer = response.get("answer", "")
                # Parse tags from the response
                import re
                tags = re.findall(r'["\']([^"\']+)["\']', answer)
                if not tags:
                    # Fallback: split by comma or newlines
                    tags = [tag.strip() for tag in re.split(r'[,\n]', answer) if tag.strip()]
                
                return {
                    "status": "success",
                    "suggestions": tags[:10],  # Limit to 10 tags
                    "source": "ai-analysis"
                }
            else:
                return {"error": "Failed to generate tag suggestions", "details": response}
                
        except Exception as e:
            self.logger.error(f"Error in suggest_tags: {e}", exc_info=True)
            return {"error": f"Failed to generate tags: {str(e)}"}

    async def find_connections(self, payload: Dict) -> Dict[str, Any]:
        """
        Find knowledge connections for a given tag based on co-occurrence.
        """
        try:
            data = normalize_payload(payload)
            tag_name = data.get('tag')

            if not tag_name:
                return {"error": "Tag name is required for finding connections"}

            self.logger.info(f"Finding connections for tag: {tag_name}")

            # 1. get tag node
            tag_node = self.graph_service.get_node_by_id(tag_name)
            if not tag_node:
                return {"error": f"Tag '{tag_name}' not found in the graph."}
            tag_id = tag_node['node_id']

            # 2. get nodes with tag
            nodes_with_tag = self.graph_service.get_nodes_linked_to_tag(tag_id)

            # 3. count co-occurrence related tags
            co_occurring_tags = {}
            for node in nodes_with_tag:
                # get all tags of the node
                tag_neighbors = self.graph_service.get_neighbors(node['node_id'], relation_type='HAS_TAG')
                for rel_node in tag_neighbors:
                    rel_tag_id = rel_node['node_id']
                    if rel_tag_id != tag_id:
                        co_occurring_tags[rel_tag_id] = co_occurring_tags.get(rel_tag_id, 0) + 1

            # 4. sort by co-occurrence frequency
            sorted_related_tags = sorted(co_occurring_tags.items(), key=lambda item: item[1], reverse=True)
            related_tag_names = [tag_id for tag_id, _ in sorted_related_tags[:5]]

            # 5. AI analysis
            prompt = f"""
            Analyze the connections for the tag \"{tag_name}\".\nIt frequently co-occurs with these tags: {', '.join(related_tag_names)}.\nBased on this, what are the likely relationships, underlying themes, or common use cases?\nProvide a brief analysis.
            """
            ai_insights = await self.rag_service.query(prompt)

            return {
                "status": "success",
                "tag": tag_name,
                "nodes_count": len(nodes_with_tag),
                "related_tags": [{"tag": tag_id, "count": count} for tag_id, count in sorted_related_tags[:10]],
                "ai_insights": ai_insights.get("answer", "") if ai_insights.get("status") == "success" else "Could not generate AI insights.",
                "nodes": [n['node_id'] for n in nodes_with_tag[:5]]
            }

        except Exception as e:
            self.logger.error(f"Error in find_connections: {e}", exc_info=True)
            return {"error": f"Failed to find connections: {str(e)}"}

    async def expand_content(self, payload: Dict) -> Dict[str, Any]:
        """
        Expand and elaborate on given content or topic.
        
        Args:
            payload: Dict containing 'content' and optional 'context' keys
            
        Returns:
            Dict with expanded content and insights
        """
        try:
            data = normalize_payload(payload)
            content = data.get('content')
            context_text = data.get('context', '')
            
            if not content:
                return {"error": "Content is required for expansion"}
                
            self.logger.info(f"Expanding content: {content[:100]}...")
            
            # Build contextual prompt
            if context_text:
                prompt = f"""
                Expand on the following topic within the given context:
                
                Topic to expand: {content}
                
                Context: {context_text}
                
                Provide detailed insights, examples, related concepts, and practical applications.
                Structure the response with clear sections and actionable information.
                """
            else:
                prompt = f"""
                Expand on the following topic:
                
                {content}
                
                Provide comprehensive insights including:
                1. Detailed explanation
                2. Practical examples
                3. Related concepts
                4. Implementation guidance
                5. Common use cases
                """
            
            response = await self.rag_service.query(prompt)
            
            if response.get("status") == "success":
                return {
                    "status": "success",
                    "expanded_content": response.get("answer", ""),
                    "original_content": content,
                    "context_used": bool(context_text)
                }
            else:
                return {"error": "Failed to expand content", "details": response}
                
        except Exception as e:
            self.logger.error(f"Error in expand_content: {e}", exc_info=True)
            return {"error": f"Failed to expand content: {str(e)}"}
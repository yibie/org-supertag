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
        Find knowledge connections for a given tag.
        
        Args:
            payload: Dict containing 'tag' key with tag name
            
        Returns:
            Dict with related nodes, tags, and connection insights
        """
        try:
            data = normalize_payload(payload)
            tag = data.get('tag')
            
            if not tag:
                return {"error": "Tag is required for finding connections"}
                
            self.logger.info(f"Finding connections for tag: {tag}")
            
            # Get nodes with this tag
            nodes_with_tag = await self.graph_service.find_nodes_by_tag(tag)
            
            # Get related tags via co-occurrence
            related_tags = await self.graph_service.get_related_tags(tag)
            
            # Get similar concepts
            prompt = f"""
            Find knowledge connections and relationships for the tag "{tag}".
            Consider:
            1. Related concepts and themes
            2. Practical applications and use cases
            3. Connections to other knowledge areas
            4. Common workflows or processes involving this tag
            
            Provide insights about how this tag connects to the broader knowledge graph.
            """
            
            ai_insights = await self.rag_service.query(prompt)
            
            return {
                "status": "success",
                "tag": tag,
                "nodes_count": len(nodes_with_tag),
                "related_tags": related_tags[:10],
                "ai_insights": ai_insights.get("answer", "") if ai_insights.get("status") == "success" else "",
                "nodes": nodes_with_tag[:5]  # Limit to 5 nodes for brevity
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
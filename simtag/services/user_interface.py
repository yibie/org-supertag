"""
User interface service layer

This module provides a user-friendly interface, responsible for converting between the UUID technical layer and the user display layer.
The user sees titles and content snippets, while the system still uses UUIDs for precision.
"""

import logging
from typing import Dict, List, Any, Optional, Tuple
import re
from ..core.graph_service import GraphService

logger = logging.getLogger(__name__)

class UserFriendlyNode:
    """User-friendly node representation"""
    def __init__(self, uuid: str, title: str, content_snippet: str, 
                 file_path: str = "", score: float = 0.0, metadata: Dict[str, Any] = None):
        self.uuid = uuid
        self.title = title
        self.content_snippet = content_snippet
        self.file_path = file_path
        self.score = score
        self.metadata = metadata or {}
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary format, for EPC transmission"""
        return {
            "title": self.title,
            "snippet": self.content_snippet,
            "file_path": self.file_path,
            "score": self.score,
            "uuid": self.uuid,  # UUID retained but not main display information
            "metadata": self.metadata
        }

class UserInterfaceService:
    """User interface service, providing user-friendly interaction methods"""
    
    def __init__(self, graph_service: GraphService):
        self.graph_service = graph_service
        logger.info("UserInterfaceService initialized")
    
    def _generate_content_snippet(self, content: str, max_length: int = 100) -> str:
        """Generate content snippet for display"""
        if not content:
            return "No content"
        
        # Clean content: remove extra whitespace and newlines
        cleaned = re.sub(r'\s+', ' ', content.strip())
        
        if len(cleaned) <= max_length:
            return cleaned
        
        # Truncate at word boundary
        truncated = cleaned[:max_length]
        last_space = truncated.rfind(' ')
        if last_space > max_length * 0.7:  # If space position is reasonable
            truncated = truncated[:last_space]
        
        return truncated + "..."
    
    def _enhance_node_with_metadata(self, node_uuid: str, base_score: float = 0.0) -> Optional[UserFriendlyNode]:
        """Enhance node information with metadata"""
        try:
            # Get node details
            nodes_details = self.graph_service.get_nodes_by_ids([node_uuid])
            if not nodes_details:
                logger.warning(f"Node metadata not found: {node_uuid}")
                return UserFriendlyNode(
                    uuid=node_uuid,
                    title=f"Node {node_uuid[:8]}...",
                    content_snippet="Metadata unavailable",
                    score=base_score
                )
            
            node_detail = nodes_details[0]
            title = node_detail.get('title', '') or f"未命名节点 {node_uuid[:8]}..."
            content = node_detail.get("content", "")
            content_snippet = self._generate_content_snippet(content, max_length=100)
            
            return UserFriendlyNode(
                uuid=node_uuid,
                title=title,
                content_snippet=content_snippet,
                score=base_score,
                metadata={
                    'document_date': node_detail.get('document_date'),
                    'node_id': node_uuid,
                    'content_length': len(content)
                }
            )
            
        except Exception as e:
            logger.error(f"Error enhancing node metadata {node_uuid}: {e}")
            return UserFriendlyNode(
                uuid=node_uuid,
                title=f"Node {node_uuid[:8]}...",
                content_snippet="Error processing",
                score=base_score
            )
    
    def convert_similar_nodes_to_user_friendly(
        self, 
        uuid_results: List[Tuple[str, float]], 
        include_content: bool = True
    ) -> List[Dict[str, Any]]:
        """
        Convert UUID-based similar node results to user-friendly format
        
        Args:
            uuid_results: [(node_uuid, similarity_score), ...] format results
            include_content: Whether to include content snippet
            
        Returns:
            User-friendly node list
        """
        user_friendly_results = []
        
        for node_uuid, score in uuid_results:
            try:
                friendly_node = self._enhance_node_with_metadata(node_uuid, score)
                if friendly_node:
                    user_friendly_results.append(friendly_node.to_dict())
                else:
                    # fallback: basic information
                    user_friendly_results.append({
                        "title": f"Node {node_uuid[:8]}...",
                        "snippet": "Information unavailable",
                        "uuid": node_uuid,
                        "score": score,
                        "metadata": {}
                    })
            except Exception as e:
                logger.error(f"Error converting node {node_uuid}: {e}")
                continue
        
        return user_friendly_results
    
    def search_nodes_by_title_or_content(
        self, 
        query: str, 
        top_k: int = 10,
        fuzzy_match: bool = True
    ) -> List[Dict[str, Any]]:
        """
        Search nodes by title or content (user-friendly interface)
        
        Args:
            query: Search query (can be title snippet or content keyword)
            top_k: Number of results to return
            fuzzy_match: Whether to enable fuzzy matching
            
        Returns:
            User-friendly search results
        """
        try:
            logger.info(f"User-friendly search: '{query}', top_k={top_k}")
            
            # Use storage layer's search functionality
            search_results = self.graph_service.search_nodes_by_title_content(query, limit=top_k)
            
            # Convert to user-friendly format
            user_friendly_results = []
            for result in search_results:
                content_snippet = self._generate_content_snippet(result.get('content', ''), max_length=100)
                
                friendly_result = {
                    "title": result.get('title', f"Node {result['node_id'][:8]}..."),
                    "snippet": content_snippet,
                    "uuid": result['node_id'],
                    "score": 1.0,  # Text search gives fixed score
                    "file_path": "",  # If available, can be obtained from metadata
                    "metadata": {
                        'document_date': result.get('document_date'),
                        'node_id': result['node_id'],
                        'search_type': 'title_content',
                        'content_length': len(result.get('content', ''))
                    }
                }
                user_friendly_results.append(friendly_result)
            
            logger.info(f"Search '{query}' found {len(user_friendly_results)} results")
            return user_friendly_results
            
        except Exception as e:
            logger.error(f"Error searching nodes: {e}")
            return []
    
    def get_node_context_by_uuid(self, node_uuid: str) -> Optional[Dict[str, Any]]:
        """
        Get complete context information for a node by UUID
        
        Args:
            node_uuid: Node UUID
            
        Returns:
            Dictionary containing node details and related context
        """
        try:
            friendly_node = self._enhance_node_with_metadata(node_uuid)
            if not friendly_node:
                return None
            
            # TODO: Add related nodes, tags, etc. context information
            context = friendly_node.to_dict()
            context.update({
                "related_nodes": [],  # Related nodes
                "tags": [],          # Related tags
                "references": [],    # Reference relationships
            })
            
            return context
            
        except Exception as e:
            logger.error(f"Error getting node context {node_uuid}: {e}")
            return None
    
    def resolve_user_input_to_uuid(self, user_input: str) -> Optional[str]:
        """
        Parse user input (title snippet, content keyword, etc.) into specific node UUID
        
        Args:
            user_input: User input query
            
        Returns:
            Most matching node UUID, or None if not found
        """
        try:
            # Check if already UUID format
            if self._is_uuid_format(user_input):
                return user_input
            
            # Try to find the most matching node by title search
            search_results = self.graph_service.search_nodes_by_title_content(user_input, limit=1)
            if search_results:
                best_match = search_results[0]
                logger.info(f"Parsed user input '{user_input}' to UUID: {best_match['node_id']}")
                return best_match['node_id']
            
            logger.info(f"Cannot parse user input to UUID: '{user_input}'")
            return None
            
        except Exception as e:
            logger.error(f"Error parsing user input: {e}")
            return None
    
    def _is_uuid_format(self, text: str) -> bool:
        """Check if text is in UUID format"""
        uuid_pattern = r'^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$'
        return bool(re.match(uuid_pattern, text.lower())) 
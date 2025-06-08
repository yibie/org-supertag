"""
用户友好交互服务层

该模块提供用户友好的交互接口，负责在UUID技术层和用户显示层之间进行转换。
用户看到的是标题和内容片段，而系统内部依然使用UUID保证精确性。
"""

import logging
from typing import Dict, List, Any, Optional, Tuple
import re
from ..core.storage import VectorStorage

logger = logging.getLogger(__name__)

class UserFriendlyNode:
    """用户友好的节点表示"""
    def __init__(self, uuid: str, title: str, content_snippet: str, 
                 file_path: str = "", score: float = 0.0, metadata: Dict[str, Any] = None):
        self.uuid = uuid
        self.title = title
        self.content_snippet = content_snippet
        self.file_path = file_path
        self.score = score
        self.metadata = metadata or {}
    
    def to_dict(self) -> Dict[str, Any]:
        """转换为字典格式，用于EPC传输"""
        return {
            "title": self.title,
            "snippet": self.content_snippet,
            "file_path": self.file_path,
            "score": self.score,
            "uuid": self.uuid,  # UUID保留但不是主要显示信息
            "metadata": self.metadata
        }

class UserInterfaceService:
    """用户界面服务，提供用户友好的交互方法"""
    
    def __init__(self, storage: VectorStorage):
        self.storage = storage
        logger.info("UserInterfaceService 初始化完成")
    
    def _generate_content_snippet(self, content: str, max_length: int = 100) -> str:
        """生成内容片段用于显示"""
        if not content:
            return "无内容"
        
        # 清理内容：移除多余的空白和换行
        cleaned = re.sub(r'\s+', ' ', content.strip())
        
        if len(cleaned) <= max_length:
            return cleaned
        
        # 在单词边界截断
        truncated = cleaned[:max_length]
        last_space = truncated.rfind(' ')
        if last_space > max_length * 0.7:  # 如果空格位置合理
            truncated = truncated[:last_space]
        
        return truncated + "..."
    
    def _enhance_node_with_metadata(self, node_uuid: str, base_score: float = 0.0) -> Optional[UserFriendlyNode]:
        """使用元数据增强节点信息"""
        try:
            # 获取节点详细信息
            node_details = self.storage.get_node_details_by_ids([node_uuid])
            if not node_details:
                logger.warning(f"未找到节点元数据: {node_uuid}")
                return UserFriendlyNode(
                    uuid=node_uuid,
                    title=f"节点 {node_uuid[:8]}...",
                    content_snippet="元数据不可用",
                    score=base_score
                )
            
            node_detail = node_details[0]
            title = node_detail.get('title', '') or f"未命名节点 {node_uuid[:8]}..."
            
            # 获取实际内容
            content_dict = self.storage.get_node_content_by_ids([node_uuid])
            content = content_dict.get(node_uuid, "")
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
            logger.error(f"增强节点元数据时出错 {node_uuid}: {e}")
            return UserFriendlyNode(
                uuid=node_uuid,
                title=f"节点 {node_uuid[:8]}...",
                content_snippet="处理错误",
                score=base_score
            )
    
    def convert_similar_nodes_to_user_friendly(
        self, 
        uuid_results: List[Tuple[str, float]], 
        include_content: bool = True
    ) -> List[Dict[str, Any]]:
        """
        将UUID格式的相似节点结果转换为用户友好格式
        
        Args:
            uuid_results: [(node_uuid, similarity_score), ...] 格式的结果
            include_content: 是否包含内容片段
            
        Returns:
            用户友好的节点列表
        """
        user_friendly_results = []
        
        for node_uuid, score in uuid_results:
            try:
                friendly_node = self._enhance_node_with_metadata(node_uuid, score)
                if friendly_node:
                    user_friendly_results.append(friendly_node.to_dict())
                else:
                    # fallback：基本信息
                    user_friendly_results.append({
                        "title": f"节点 {node_uuid[:8]}...",
                        "snippet": "信息不可用",
                        "uuid": node_uuid,
                        "score": score,
                        "metadata": {}
                    })
            except Exception as e:
                logger.error(f"转换节点 {node_uuid} 时出错: {e}")
                continue
        
        return user_friendly_results
    
    def search_nodes_by_title_or_content(
        self, 
        query: str, 
        top_k: int = 10,
        fuzzy_match: bool = True
    ) -> List[Dict[str, Any]]:
        """
        根据标题或内容搜索节点（用户友好接口）
        
        Args:
            query: 搜索查询（可以是标题片段或内容关键词）
            top_k: 返回结果数量
            fuzzy_match: 是否启用模糊匹配
            
        Returns:
            用户友好的搜索结果
        """
        try:
            logger.info(f"用户友好搜索: '{query}', top_k={top_k}")
            
            # 使用storage层的搜索功能
            search_results = self.storage.search_nodes_by_title_content(query, limit=top_k)
            
            # 转换为用户友好格式
            user_friendly_results = []
            for result in search_results:
                content_snippet = self._generate_content_snippet(result.get('content', ''), max_length=100)
                
                friendly_result = {
                    "title": result.get('title', f"节点 {result['node_id'][:8]}..."),
                    "snippet": content_snippet,
                    "uuid": result['node_id'],
                    "score": 1.0,  # 文本搜索给固定分数
                    "file_path": "",  # 如果有的话可以从metadata获取
                    "metadata": {
                        'document_date': result.get('document_date'),
                        'node_id': result['node_id'],
                        'search_type': 'title_content',
                        'content_length': len(result.get('content', ''))
                    }
                }
                user_friendly_results.append(friendly_result)
            
            logger.info(f"搜索 '{query}' 找到 {len(user_friendly_results)} 个结果")
            return user_friendly_results
            
        except Exception as e:
            logger.error(f"搜索节点时出错: {e}")
            return []
    
    def get_node_context_by_uuid(self, node_uuid: str) -> Optional[Dict[str, Any]]:
        """
        根据UUID获取节点的完整上下文信息
        
        Args:
            node_uuid: 节点UUID
            
        Returns:
            包含节点详细信息和相关上下文的字典
        """
        try:
            friendly_node = self._enhance_node_with_metadata(node_uuid)
            if not friendly_node:
                return None
            
            # TODO: 添加相关节点、标签等上下文信息
            context = friendly_node.to_dict()
            context.update({
                "related_nodes": [],  # 相关节点
                "tags": [],          # 相关标签
                "references": [],    # 引用关系
            })
            
            return context
            
        except Exception as e:
            logger.error(f"获取节点上下文时出错 {node_uuid}: {e}")
            return None
    
    def resolve_user_input_to_uuid(self, user_input: str) -> Optional[str]:
        """
        将用户输入（标题片段、内容关键词等）解析为具体的节点UUID
        
        Args:
            user_input: 用户输入的查询
            
        Returns:
            最匹配的节点UUID，如果没有找到则返回None
        """
        try:
            # 检查是否已经是UUID格式
            if self._is_uuid_format(user_input):
                return user_input
            
            # 尝试通过标题搜索找到最匹配的节点
            search_results = self.storage.search_nodes_by_title_content(user_input, limit=1)
            if search_results:
                best_match = search_results[0]
                logger.info(f"解析用户输入 '{user_input}' 为UUID: {best_match['node_id']}")
                return best_match['node_id']
            
            logger.info(f"无法解析用户输入为UUID: '{user_input}'")
            return None
            
        except Exception as e:
            logger.error(f"解析用户输入时出错: {e}")
            return None
    
    def _is_uuid_format(self, text: str) -> bool:
        """检查文本是否为UUID格式"""
        uuid_pattern = r'^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$'
        return bool(re.match(uuid_pattern, text.lower())) 
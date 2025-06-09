from typing import Dict, Any

class Config:
    # ... existing methods ...
    
    @classmethod
    def get_node_embedding_config(cls) -> Dict[str, Any]:
        """
        获取 node 向量化相关配置
        
        Returns:
            Dict: node 向量化配置
        """
        return {
            # 内容截取配置
            'max_content_chars': cls.get('node_embedding.max_content_chars', 500),
            'min_content_chars': cls.get('node_embedding.min_content_chars', 50),
            
            # 上下文配置
            'include_context': cls.get('node_embedding.include_context', True),
            'max_context_levels': cls.get('node_embedding.max_context_levels', 2),
            
            # 标签配置
            'include_tags': cls.get('node_embedding.include_tags', True),
            'max_tags': cls.get('node_embedding.max_tags', 5),
            
            # 截断策略
            'truncate_at_sentence': cls.get('node_embedding.truncate_at_sentence', True),
            'min_truncate_ratio': cls.get('node_embedding.min_truncate_ratio', 0.7),
            
            # 文本组织
            'separator': cls.get('node_embedding.separator', ' | '),
            'include_level_info': cls.get('node_embedding.include_level_info', False)
        }
    
    @classmethod
    def get_chunking_strategy(cls) -> str:
        """
        获取文本分块策略
        
        Returns:
            str: 'node-based' 或 'traditional'
        """
        return cls.get('embedding.chunking_strategy', 'node-based')

    # ... existing code ... 
import os
import json
import logging
import numpy as np
from typing import List, Dict, Any, Tuple
from pathlib import Path

logger = logging.getLogger(__name__)

class TagSearch:
    """标签搜索类，用于查找相似标签"""
    
    def __init__(self, vector_file: str = None):
        """初始化标签搜索
        
        Args:
            vector_file: 包含标签向量的JSON文件路径
        """
        self.vector_file = vector_file
        self.tags = []
        self.vectors = None
        
        if vector_file and os.path.exists(vector_file):
            self._load_vectors(vector_file)
        else:
            logger.warning(f"向量文件不存在或未提供: {vector_file}")
    
    def _load_vectors(self, vector_file: str) -> bool:
        """加载标签向量
        
        Args:
            vector_file: 向量文件路径
            
        Returns:
            bool: 是否成功加载
        """
        try:
            logger.info(f"加载标签向量文件: {vector_file}")
            
            with open(vector_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
            
            # 提取标签和向量
            if isinstance(data, list):
                # 假设格式为 [{"tag": "标签名", "vector": [0.1, 0.2, ...]}]
                self.tags = [item.get('tag', '') for item in data if 'tag' in item and 'vector' in item]
                vectors = [item.get('vector', []) for item in data if 'tag' in item and 'vector' in item]
                
                if vectors:
                    self.vectors = np.array(vectors, dtype=np.float32)
                    logger.info(f"成功加载 {len(self.tags)} 个标签向量，维度: {self.vectors.shape[1]}")
                    return True
                else:
                    logger.warning("向量文件中没有有效的向量数据")
            else:
                logger.error(f"向量文件格式不正确: {type(data)}")
            
            return False
            
        except Exception as e:
            logger.error(f"加载向量文件失败: {e}")
            return False
    
    def find_similar(self, query: str, max_results: int = 10) -> List[str]:
        """查找与查询相似的标签
        
        Args:
            query: 查询文本
            max_results: 最大结果数量
            
        Returns:
            list: 相似标签列表
        """
        if not self.vectors is not None or len(self.tags) == 0:
            logger.warning("标签向量未加载，无法执行相似查询")
            return []
        
        # 简单的字符串匹配
        results = []
        query_lower = query.lower()
        
        for tag in self.tags:
            if query_lower in tag.lower():
                results.append(tag)
                if len(results) >= max_results:
                    break
        
        logger.info(f"查找与 '{query}' 相似的标签，找到 {len(results)} 个结果")
        return results 
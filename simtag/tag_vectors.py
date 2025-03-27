"""
SimTag 标签向量处理模块
提供标签向量的生成、存储和相似性查询功能
"""

import os
import json
import logging
import traceback
import re
import time
from datetime import datetime
from typing import List, Dict, Any, Tuple, Optional
import numpy as np
from sentence_transformers import SentenceTransformer
import torch

class TagVectorEngine:
    """标签向量引擎类"""
    
    def __init__(self, vector_file: str = None):
        """初始化标签向量引擎
        
        Args:
            vector_file: 向量文件路径
        """
        self.logger = logging.getLogger("simtag.tag_vectors")
        self.vector_file = vector_file
        self.tag_vectors = {}  # 标签向量字典
        self.is_initialized = False
        self.model_name = 'sentence-transformers/paraphrase-MiniLM-L6-v2'
        self._model = None
        
        if vector_file and os.path.exists(vector_file):
            self.load_vectors(vector_file)
            
    @property
    def model(self):
        """延迟加载模型"""
        if self._model is None:
            self.logger.info(f"加载模型: {self.model_name}")
            self._model = SentenceTransformer(
                self.model_name,
                cache_folder=os.path.join(os.path.dirname(__file__), 'models')
            )
            # 设置设备
            device = self._get_device()
            if device.type != 'cpu':
                self._model = self._model.to(device)
                self.logger.info(f"启用 {device.type.upper()} 加速")
                
        return self._model
        
    def _get_device(self):
        """获取最佳可用设备"""
        if torch.cuda.is_available():
            return torch.device('cuda')
        elif torch.backends.mps.is_available():
            return torch.device('mps')
        return torch.device('cpu')
        
    def status(self) -> Dict[str, Any]:
        """获取引擎状态
        
        Returns:
            状态信息字典
        """
        return {
            "initialized": self.is_initialized,
            "vector_file": self.vector_file,
            "vector_count": len(self.tag_vectors),
            "file_exists": os.path.exists(self.vector_file) if self.vector_file else False,
            "file_size": os.path.getsize(self.vector_file) if self.vector_file and os.path.exists(self.vector_file) else 0
        }
            
    def load_vectors(self, vector_file: str) -> bool:
        """加载标签向量
        
        Args:
            vector_file: 向量文件路径
            
        Returns:
            是否成功加载
        """
        try:
            if not os.path.exists(vector_file):
                self.logger.error(f"向量文件不存在: {vector_file}")
                return False
                
            self.logger.info(f"加载向量文件: {vector_file}")
            with open(vector_file, 'r') as f:
                data = json.load(f)
                
            if not isinstance(data, dict) or 'tags' not in data:
                self.logger.error(f"无效的向量文件格式")
                return False
                
            # 更新向量
            self.tag_vectors = {
                tag_id: np.array(info['vector']) 
                for tag_id, info in data['tags'].items()
            }
            
            self.vector_file = vector_file
            self.is_initialized = True
            self.logger.info(f"成功加载 {len(self.tag_vectors)} 个标签向量")
            return True
            
        except Exception as e:
            self.logger.error(f"加载向量文件出错: {e}")
            self.logger.error(traceback.format_exc())
            return False
            
    def find_similar(self, tag_name: str, top_k: int = 5) -> List[Tuple[str, float]]:
        """查找与给定标签相似的标签
        
        Args:
            tag_name: 标签名称
            top_k: 返回的结果数量
            
        Returns:
            相似标签列表，每个元素是 (tag_name, similarity_score)
        """
        self.logger.info(f"查找相似标签: tag={tag_name}, top_k={top_k}")
        
        # 检查向量文件
        if not self.is_initialized:
            if not self.vector_file or not os.path.exists(self.vector_file):
                self.logger.error("向量文件未指定或不存在")
                return []
            if not self.load_vectors(self.vector_file):
                self.logger.error("无法加载向量文件")
                return []
        
        # 检查标签向量字典
        if not self.tag_vectors:
            self.logger.error("没有可用的标签向量")
            return []
            
        # 获取目标标签向量
        if tag_name not in self.tag_vectors:
            try:
                self.logger.info(f"标签 '{tag_name}' 不在向量库中，生成向量...")
                target_vector = self.model.encode(tag_name)
                self.logger.info(f"向量生成成功，维度: {target_vector.shape}")
            except Exception as e:
                self.logger.error(f"生成向量出错: {e}")
                return []
        else:
            target_vector = self.tag_vectors[tag_name]
            self.logger.info(f"标签 '{tag_name}' 向量已存在")
            
        # 计算相似度
        start_time = time.time()
        similarities = []
        self.logger.info(f"开始计算与 {len(self.tag_vectors)} 个标签的相似度...")
        
        for other_tag, other_vector in self.tag_vectors.items():
            if other_tag != tag_name:
                try:
                    # 计算余弦相似度
                    sim = self._compute_similarity(target_vector, other_vector)
                    similarities.append((other_tag, sim))
                except Exception as e:
                    self.logger.error(f"计算与标签 '{other_tag}' 的相似度出错: {e}")
                    continue
        
        # 按相似度排序
        similarities.sort(key=lambda x: x[1], reverse=True)
        
        # 返回前 top_k 个结果
        results = similarities[:top_k]
        
        elapsed = time.time() - start_time
        self.logger.info(f"相似度计算完成，耗时: {elapsed:.2f}秒，找到 {len(results)} 个相似标签")
        
        return results
        
    def _compute_similarity(self, vec1, vec2) -> float:
        """计算两个向量之间的相似度
        
        Args:
            vec1: 第一个向量
            vec2: 第二个向量
            
        Returns:
            相似度分数
        """
        try:
            # 确保向量是numpy数组
            if not isinstance(vec1, np.ndarray):
                vec1 = np.array(vec1)
            if not isinstance(vec2, np.ndarray):
                vec2 = np.array(vec2)
            
            # 确保向量是2D
            if len(vec1.shape) == 1:
                vec1 = vec1.reshape(1, -1)
            if len(vec2.shape) == 1:
                vec2 = vec2.reshape(1, -1)
            
            # 计算余弦相似度
            sim = np.dot(vec1, vec2.T) / (np.linalg.norm(vec1) * np.linalg.norm(vec2))
            return float(sim[0][0])  # 确保返回Python原生浮点数
        except Exception as e:
            self.logger.error(f"计算相似度出错: {e}")
            self.logger.error(traceback.format_exc())
            raise
            
    def test_engine(self, test_text: str) -> np.ndarray:
        """测试引擎是否正常工作
        
        Args:
            test_text: 测试文本
            
        Returns:
            生成的向量
            
        Raises:
            Exception: 如果引擎不能正常工作
        """
        self.logger.info("测试引擎功能...")
        
        # 确保模型已加载
        if not self.model:
            raise RuntimeError("文本相似度模型未加载")
            
        try:
            # 生成向量
            vector = self.model.encode(test_text)
            
            # 验证向量
            if not isinstance(vector, np.ndarray):
                raise TypeError(f"向量类型错误: {type(vector)}")
                
            if vector.shape[0] != 384:  # MiniLM-L6 维度
                raise ValueError(f"向量维度错误: {vector.shape}")
                
            self.logger.info("引擎测试成功")
            return vector.tolist()  # 转换为列表以便序列化
            
        except Exception as e:
            self.logger.error(f"引擎测试失败: {e}")
            self.logger.error(traceback.format_exc())
            raise

    def initialize(self, db_file: str, vector_file: str, tag_data: List[Dict] = None) -> Dict[str, Any]:
        """初始化标签库
        
        Args:
            db_file: 数据库文件路径
            vector_file: 向量文件输出路径
            tag_data: 标签数据列表
            
        Returns:
            初始化结果信息
        """
        try:
            # 确保参数类型正确
            if not isinstance(db_file, str):
                self.logger.error(f"db_file 参数类型错误: {type(db_file)}")
                return {
                    "status": "error",
                    "message": f"数据库文件路径必须是字符串，而不是 {type(db_file)}",
                    "result": None
                }
                
            if not isinstance(vector_file, str):
                self.logger.error(f"vector_file 参数类型错误: {type(vector_file)}")
                return {
                    "status": "error",
                    "message": f"向量文件路径必须是字符串，而不是 {type(vector_file)}",
                    "result": None
                }
            
            # 记录初始化参数
            self.logger.info(f"初始化标签库:")
            self.logger.info(f"- 数据库文件: {db_file}")
            self.logger.info(f"- 向量文件: {vector_file}")
            self.logger.info(f"- 标签数据: {len(tag_data) if tag_data else '无'}")
            
            self.vector_file = vector_file
            
            # 首先测试引擎
            try:
                test_vector = self.test_engine("test sentence for initialization")
                self.logger.info("引擎功能测试通过")
            except Exception as e:
                return {
                    "status": "error",
                    "message": f"引擎功能测试失败: {e}",
                    "result": {
                        "vector_file": vector_file,
                        "db_file": db_file,
                        "model": None
                    }
                }

            # 首先确保模型加载成功
            if not self.model:
                return {
                    "status": "error",
                    "message": "无法加载文本相似度模型",
                    "result": {
                        "vector_file": vector_file,
                        "db_file": db_file,
                        "model": None
                    }
                }
            
            # 测试模型是否能正常工作
            try:
                test_text = "test sentence for model verification"
                test_vector = self.model.encode(test_text)
                if not isinstance(test_vector, np.ndarray) or test_vector.shape[0] != 384:  # MiniLM-L6 维度
                    raise ValueError(f"模型输出向量维度不正确: {test_vector.shape}")
            except Exception as e:
                self.logger.error(f"模型功能测试失败: {e}")
                return {
                    "status": "error",
                    "message": f"模型功能测试失败: {e}",
                    "result": {
                        "vector_file": vector_file,
                        "db_file": db_file,
                        "model": None
                    }
                }

            # 确保输出目录存在
            os.makedirs(os.path.dirname(vector_file), exist_ok=True)
            
            # 解析标签数据
            tag_info = {}
            if tag_data:
                for tag_dict in tag_data:
                    # 处理不同格式的标签数据
                    if isinstance(tag_dict, dict):
                        tag_data_dict = tag_dict
                    elif isinstance(tag_dict, list):
                        # 将列表转换为字典
                        tag_data_dict = {}
                        for item in tag_dict:
                            if isinstance(item, tuple) and len(item) == 2:
                                key, value = item
                                if isinstance(value, list) and len(value) == 1:
                                    value = value[0]
                                tag_data_dict[key] = value
                    else:
                        self.logger.warning(f"跳过无效的标签数据: {tag_dict}")
                        continue
                        
                    # 提取标签ID
                    tag_id = tag_data_dict.get('id') or tag_data_dict.get('name')
                    if not tag_id:
                        continue
                        
                    # 如果tag_id是列表，取第一个元素
                    if isinstance(tag_id, list):
                        tag_id = tag_id[0]
                        
                    # 处理字段
                    fields = self._process_fields(tag_data_dict.get('fields', []))
                    
                    # 处理行为
                    behaviors = self._process_behaviors(tag_data_dict.get('behaviors', []))
                        
                    # 处理关系
                    relations = self._process_relations(tag_data_dict.get('relations', []))
                        
                    # 存储标签信息
                    tag_info[tag_id] = {
                        'name': tag_id,
                        'type': 'tag',
                        'fields': fields,
                        'behaviors': behaviors,
                        'relations': relations,
                    }
            else:
                # 从数据库文件解析
                tag_info = self.parse_supertag_db(db_file)
            
            self.logger.info(f"处理了 {len(tag_info)} 个标签")
            
            if not tag_info:
                self.logger.error("没有找到有效的标签数据")
                return {"status": "error", "message": "没有找到有效的标签数据"}
            
            # 生成标签向量
            tag_vectors = {}
            for tag_id, info in tag_info.items():
                try:
                    # 生成向量
                    tag_vector = self.model.encode(tag_id)
                    tag_vectors[tag_id] = tag_vector
                except Exception as e:
                    self.logger.error(f"生成标签 '{tag_id}' 的向量出错: {e}")
                    continue
            
            # 构建缓存数据
            cache_data = {
                'tags': {
                    tag_id: {
                        'name': info['name'],
                        'vector': tag_vectors[tag_id].tolist() if tag_id in tag_vectors else [],
                        'info': info
                    }
                    for tag_id, info in tag_info.items() if tag_id in tag_vectors
                },
                'metadata': {
                    'total_tags': len(tag_vectors),
                    'vector_dim': 384,  # MiniLM-L6 维度
                    'created_at': datetime.now().isoformat(),
                    'model_name': 'sentence-transformers/paraphrase-MiniLM-L6-v2'
                }
            }
            
            # 保存到文件
            with open(vector_file, 'w') as f:
                json.dump(cache_data, f, indent=2)
                
            self.logger.info(f"标签库初始化完成，保存到 {vector_file}")
            self.logger.info(f"文件大小: {os.path.getsize(vector_file)} 字节")
            
            # 更新状态
            self.tag_vectors = {
                tag_id: np.array(vector) for tag_id, vector in tag_vectors.items()
            }
            self.is_initialized = True
            
            return {
                "status": "success",
                "result": {
                    "vector_file": vector_file,
                    "db_file": db_file,
                    "model": 'sentence-transformers/paraphrase-MiniLM-L6-v2',
                    "tag_count": len(tag_vectors),
                    "file_size": os.path.getsize(vector_file)
                }
            }
            
        except Exception as e:
            self.logger.error(f"初始化标签库出错: {e}")
            self.logger.error(traceback.format_exc())
            return {
                "status": "error", 
                "message": str(e),
                "result": {
                    "vector_file": vector_file,
                    "db_file": db_file,
                    "model": 'sentence-transformers/paraphrase-MiniLM-L6-v2'
                }
            }
    
    def _process_fields(self, raw_fields):
        """处理字段数据"""
        fields = []
        if raw_fields:
            for field in raw_fields:
                if isinstance(field, dict):
                    fields.append(field)
                elif isinstance(field, (list, tuple)):
                    field_dict = {}
                    for i in range(0, len(field), 2):
                        if i + 1 < len(field):
                            key = field[i]
                            value = field[i + 1]
                            if key == 'name':
                                field_dict['name'] = value
                            elif key == 'type':
                                field_dict['type'] = value
                            elif key == 'description':
                                field_dict['description'] = value
                            elif key == 'options':
                                if isinstance(value, list):
                                    field_dict['options'] = value
                    if field_dict:
                        fields.append(field_dict)
        return fields
    
    def _process_behaviors(self, raw_behaviors):
        """处理行为数据"""
        behaviors = []
        if isinstance(raw_behaviors, dict):
            behaviors = list(raw_behaviors.keys())
        elif isinstance(raw_behaviors, list):
            behaviors = [b for b in raw_behaviors if b]
        return behaviors
    
    def _process_relations(self, raw_relations):
        """处理关系数据"""
        relations = []
        if isinstance(raw_relations, list):
            relations = [r for r in raw_relations if r]
        return relations
            
    def parse_supertag_db(self, db_file_path: str) -> Dict[str, Dict]:
        """解析supertag-db.el文件，提取标签信息
        
        Args:
            db_file_path: 数据库文件路径
            
        Returns:
            标签信息字典
        """
        self.logger.info(f"解析数据库文件: {db_file_path}")
        tag_info = {}
        
        try:
            with open(db_file_path) as f:
                content = f.read()
                
            # 提取标签定义
            tag_pattern = r'\(ht-set!\s+org-supertag-db--object\s+"([^"]+)"\s+\'(\(:type\s+:tag.*?\))\)'
            for match in re.finditer(tag_pattern, content, re.DOTALL):
                tag_id = match.group(1)
                tag_props = match.group(2)
                
                # 跳过元数据
                if tag_id == "metadata":
                    continue
                    
                # 提取字段定义
                fields = []
                fields_match = re.search(r':fields\s+(\(.*?\)|nil)(?=\s+:|$)', tag_props, re.DOTALL)
                if fields_match and fields_match.group(1) != 'nil':
                    field_str = fields_match.group(1)
                    # 解析字段列表
                    field_pattern = r'\(([^)]+)\)'
                    for field_match in re.finditer(field_pattern, field_str):
                        field_def = field_match.group(1)
                        field_parts = field_def.strip().split()
                        if len(field_parts) >= 2:
                            field = {
                                'name': field_parts[0].strip(':'),
                                'type': field_parts[1].strip(':')
                            }
                            if len(field_parts) > 2:
                                field['description'] = ' '.join(field_parts[2:]).strip('"')
                            fields.append(field)
                
                # 提取行为定义
                behaviors = []
                behaviors_match = re.search(r':behaviors\s+(\(.*?\)|nil)(?=\s+:|$)', tag_props, re.DOTALL)
                if behaviors_match and behaviors_match.group(1) != 'nil':
                    behavior_str = behaviors_match.group(1).strip('()')
                    behaviors = [b.strip('"') for b in behavior_str.split()]
                
                # 提取关系定义
                relations = []
                relations_match = re.search(r':relations\s+(\(.*?\)|nil)(?=\s+:|$)', tag_props, re.DOTALL)
                if relations_match and relations_match.group(1) != 'nil':
                    relation_str = relations_match.group(1).strip('()')
                    relations = [r.strip('"') for r in relation_str.split()]
                
                # 存储标签信息
                tag_info[tag_id] = {
                    'name': tag_id,
                    'type': 'tag',
                    'fields': fields,
                    'behaviors': behaviors,
                    'relations': relations
                }
            
            self.logger.info(f"找到 {len(tag_info)} 个标签定义")
            return tag_info
            
        except Exception as e:
            self.logger.error(f"解析数据库文件出错: {e}")
            self.logger.error(traceback.format_exc())
            return {}
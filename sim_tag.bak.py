#!/usr/bin/env python3
# sim-tag.py
import sys
import json
import os
import shutil
import subprocess
import time
import re
from functools import wraps
import numpy as np
import torch
from datetime import datetime
from typing import List, Dict, Any, Optional, Tuple, Set
import multiprocessing
from collections import defaultdict
from scipy.sparse import csr_matrix
import scipy.sparse as sp
from sklearn.metrics.pairwise import cosine_similarity
import argparse
import pickle

def timer(func):
    """Performance analysis decorator"""
    @wraps(func)
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        print(f"{func.__name__} execution time: {(end - start)*1000:.2f}ms")
        return result
    return wrapper

def ensure_env():
    """Ensure running in the correct environment"""
    try:
        # Skip environment check if running through EPC
        if os.environ.get("SIMTAG_EPC_MODE") == "1":
            return True
            
        # Check if in the simtag environment
        if os.environ.get("CONDA_DEFAULT_ENV") == "simtag":
            return True
            
        # Check if conda is installed
        conda_executable = shutil.which('conda')
        if not conda_executable:
            print("Please run setup_env.py to install and set up the environment")
            sys.exit(1)
            
        # Get the path of the simtag environment
        env_base = os.path.dirname(os.path.dirname(conda_executable))
        env_path = os.path.join(env_base, "envs", "simtag")
        
        if not os.path.exists(env_path):
            print("Please run setup_env.py to install and set up the environment")
            sys.exit(1)
            
        # Re-execute the script with the Python interpreter in the environment
        python_path = os.path.join(env_path, "bin", "python")
        if not os.path.exists(python_path):
            print("Python interpreter not found in the environment")
            sys.exit(1)
            
        # Set environment variables
        env = os.environ.copy()
        env["CONDA_DEFAULT_ENV"] = "simtag"
        env["CONDA_PREFIX"] = env_path
        env["PATH"] = f"{os.path.dirname(python_path)}:{env['PATH']}"
        
        # Re-execute the script
        os.execve(python_path, [python_path] + sys.argv, env)
        
    except Exception as e:
        print(f"Environment check error: {e}")
        sys.exit(1)

# Ensure running in the correct environment
ensure_env()

# Import dependencies
from sentence_transformers import SentenceTransformer

class BatchProcessor:
    """Batch processor, used to manage parallel tasks"""
    
    def __init__(self, batch_size: int = 32, max_workers: Optional[int] = None):
        """Initialize batch processor
        
        Args:
            batch_size: Batch size
            max_workers: Maximum number of workers
        """
        self.batch_size = batch_size
        self.max_workers = max_workers or multiprocessing.cpu_count()
        self.executor = ProcessPoolExecutor(max_workers=self.max_workers)
        
    def _split_batches(self, items: List[Any]) -> List[List[Any]]:
        """Split items into batches"""
        return [items[i:i + self.batch_size] 
                for i in range(0, len(items), self.batch_size)]
                
    def process(self, items: List[Any], process_func: callable) -> List[Any]:
        """Process items
        
        Args:
            items: Items to process
            process_func: Processing function
            
        Returns:
            Processing results
        """
        if not items:
            return []
            
        batches = self._split_batches(items)
        futures = []
        
        for batch in batches:
            future = self.executor.submit(process_func, batch)
            futures.append(future)
            
        results = []
        for future in futures:
            result = future.result()
            if isinstance(result, list):
                results.extend(result)
            else:
                results.append(result)
                
        return results

class CooccurrenceMatrix:
    """标签共现矩阵，用于存储和计算标签间的关联关系"""
    
    def __init__(self):
        self.tag_to_idx = {}  # 标签到索引的映射
        self.idx_to_tag = {}  # 索引到标签的映射
        self.cooccur_matrix = None  # 共现矩阵
        self.tag_count = defaultdict(int)  # 标签出现次数
        
    def _get_or_create_index(self, tag: str) -> int:
        """获取标签索引，如果不存在则创建"""
        if tag not in self.tag_to_idx:
            idx = len(self.tag_to_idx)
            self.tag_to_idx[tag] = idx
            self.idx_to_tag[idx] = tag
        return self.tag_to_idx[tag]
        
    def add_tag_group(self, tags: Set[str]):
        """添加一组共现的标签"""
        # 更新标签计数
        for tag in tags:
            self.tag_count[tag] += 1
            
        # 获取标签索引
        indices = [self._get_or_create_index(tag) for tag in tags]
        
        # 更新矩阵大小
        n = len(self.tag_to_idx)
        if self.cooccur_matrix is None:
            self.cooccur_matrix = sp.lil_matrix((n, n), dtype=np.float32)
        elif n > self.cooccur_matrix.shape[0]:
            new_matrix = sp.lil_matrix((n, n), dtype=np.float32)
            new_matrix[:self.cooccur_matrix.shape[0], 
                      :self.cooccur_matrix.shape[1]] = self.cooccur_matrix
            self.cooccur_matrix = new_matrix
            
        # 更新共现计数
        for i in indices:
            for j in indices:
                if i != j:
                    self.cooccur_matrix[i, j] += 1
                    
    def get_top_cooccurring(self, tag: str, top_k: int = 5) -> List[Tuple[str, float]]:
        """获取与给定标签最常共现的标签"""
        if tag not in self.tag_to_idx:
            return []
            
        idx = self.tag_to_idx[tag]
        vector = self.cooccur_matrix[idx].toarray().flatten()
        
        # 获取前k个最高值的索引
        top_indices = np.argsort(vector)[-top_k:][::-1]
        
        return [
            (self.idx_to_tag[i], float(vector[i]))
            for i in top_indices
            if vector[i] > 0
        ]
        
    def save(self, filepath: str):
        """保存共现矩阵到文件"""
        data = {
            'tag_to_idx': self.tag_to_idx,
            'idx_to_tag': self.idx_to_tag,
            'tag_count': dict(self.tag_count),
            'matrix': self.cooccur_matrix.tocsr() if self.cooccur_matrix is not None else None
        }
        with open(filepath, 'wb') as f:
            pickle.dump(data, f)
            
    def load(self, filepath: str):
        """从文件加载共现矩阵"""
        with open(filepath, 'rb') as f:
            data = pickle.load(f)
            
        self.tag_to_idx = data['tag_to_idx']
        self.idx_to_tag = data['idx_to_tag']
        self.tag_count = defaultdict(int, data['tag_count'])
        self.cooccur_matrix = data['matrix'].tolil() if data['matrix'] is not None else None

class SimtagBridge:
    _instance = None
    _model = None
    _vector_store = None
    _batch_processor = None
    _cooccur_matrix = None
    _ollama = None
    
    def __new__(cls):
        """Singleton pattern"""
        if cls._instance is None:
            cls._instance = super(SimtagBridge, cls).__new__(cls)
        return cls._instance
        
    def __init__(self):
        """Initialize the tag engine"""
        if not hasattr(self, 'initialized'):
            self.model_name = 'sentence-transformers/paraphrase-MiniLM-L6-v2'
            self.tag_vectors = {}
            self.vector_file = None
            self._cooccur_matrix = CooccurrenceMatrix()
            self.initialized = True
            
    @property
    def ollama(self):
        """获取 Ollama 客户端实例"""
        if SimtagBridge._ollama is None:
            try:
                from ollama_bridge_bak import OllamaBridge
                SimtagBridge._ollama = OllamaBridge()
                print("Ollama integration enabled")
            except Exception as e:
                print(f"Failed to initialize Ollama: {e}")
                SimtagBridge._ollama = None
        return SimtagBridge._ollama

    def hybrid_find_similar(self, tag_name: str, content: str = "", top_k: int = 5) -> List[Tuple[str, float]]:
        """使用混合方式查找相似标签
        
        Args:
            tag_name: 标签名称
            content: 相关内容文本
            top_k: 返回的标签数量
            
        Returns:
            相似标签列表，每个元素是 (tag_name, similarity_score)
        """
        # 获取向量相似度结果
        vector_results = self.find_similar(tag_name, top_k=top_k)
        
        # 如果没有 Ollama 或内容为空，直接返回向量结果
        if not self.ollama or not content:
            return vector_results
            
        try:
            # 获取 LLM 推荐的标签
            existing_tags = [tag for tag, _ in vector_results]
            llm_tags = self.ollama.suggest_tags(content, existing_tags)
            
            # 分析标签关系
            if llm_tags:
                relations = self.ollama.analyze_tag_relations(tag_name, llm_tags)
                
                # 合并结果
                all_results = {}
                
                # 添加向量结果
                for tag, score in vector_results:
                    all_results[tag] = score
                    
                # 添加 LLM 结果
                for rel in relations:
                    tag = rel["tag"]
                    score = rel["score"]
                    if tag in all_results:
                        # 如果标签已存在，取较高的分数
                        all_results[tag] = max(all_results[tag], score)
                    else:
                        all_results[tag] = score
                        
                # 排序并返回前 top_k 个结果
                sorted_results = sorted(
                    all_results.items(),
                    key=lambda x: x[1],
                    reverse=True
                )[:top_k]
                
                return sorted_results
                
        except Exception as e:
            print(f"Error in hybrid search: {e}")
            
        # 如果 LLM 处理失败，返回向量结果
        return vector_results

    def _get_device(self):
        """Get the best available device"""
        if torch.cuda.is_available():
            return torch.device('cuda')
        elif torch.backends.mps.is_available():
            return torch.device('mps')
        return torch.device('cpu')
            
    @property
    def model(self):
        """Lazy load model"""
        if SimtagBridge._model is None:
            SimtagBridge._model = SentenceTransformer(
                self.model_name,
                cache_folder=os.path.join(os.path.dirname(__file__), 'models')
            )
            device = self._get_device()
            if device.type != 'cpu':
                SimtagBridge._model = SimtagBridge._model.to(device)
                print(f"Enabled {device.type.upper()} acceleration")
                
        return SimtagBridge._model
        
    @property
    def vector_store(self):
        """Get vector database instance"""
        if SimtagBridge._vector_store is None:
            import faiss
            # Use GPU if available
            if faiss.get_num_gpus() > 0:
                res = faiss.StandardGpuResources()
                config = faiss.GpuIndexFlatIPConfig()
                config.device = 0
                SimtagBridge._vector_store = faiss.GpuIndexFlatIP(
                    res, 
                    384,  # MiniLM-L6 dimension
                    config
                )
                print("Enabled GPU accelerated vector database")
            else:
                SimtagBridge._vector_store = faiss.IndexFlatIP(384)
                print("Using CPU vector database")
        return SimtagBridge._vector_store

    @property
    def cooccur_matrix(self):
        """获取共现矩阵实例"""
        return self._cooccur_matrix

    @timer
    def _get_vectors(self, tags, batch_size=32):
        """Get tag vectors
        
        Args:
            tags: Tag list
            batch_size: Batch size
            
        Returns:
            Vector list
        """
        vectors = []
        uncached_tags = []
        
        # Check vector database
        for tag in tags:
            if tag in self.tag_vectors:
                vectors.append(self.tag_vectors[tag])
            else:
                uncached_tags.append(tag)
        
        # Process uncached tags
        if uncached_tags:
            with torch.no_grad():
                new_vectors = self.model.encode(
                    uncached_tags,
                    convert_to_tensor=True,
                    batch_size=batch_size,
                    show_progress_bar=False,
                    normalize_embeddings=True
                )
                
                device = self._get_device()
                if device.type != 'cpu':
                    new_vectors = new_vectors.cpu()
                    
                new_vectors = new_vectors.numpy()
            
            # Update vector database
            for tag, vec in zip(uncached_tags, new_vectors):
                self.tag_vectors[tag] = vec
                vectors.append(vec)
                
        return np.array(vectors)

    @timer
    def find_similar(self, tag_name, top_k=5, vector_file=None):
        """Find similar tags to the specified tag
        
        Args:
            tag_name: Tag name
            top_k: Number of similar tags to return
            vector_file: Optional vector file path
            
        Returns:
            Similar tag list, each element is (tag_name, similarity_score)
        """
        # Check vector file
        if vector_file and not os.path.exists(vector_file):
            print(f"错误：向量文件不存在: {vector_file}")
            return []
        
        # If a vector file is provided, load it first
        if vector_file and os.path.exists(vector_file):
            try:
                print(f"Loading vector file: {vector_file}")
                with open(vector_file, 'r') as f:
                    data = json.load(f)
                    if not isinstance(data, dict) or 'tags' not in data:
                        print(f"错误：无效的向量文件格式")
                        return []
                    # Update vectors
                    self.tag_vectors = {
                        tag_id: np.array(info['vector']) 
                        for tag_id, info in data['tags'].items()
                    }
                    print(f"Successfully loaded {len(self.tag_vectors)} tag vectors")
            except Exception as e:
                print(f"Error loading vector file: {e}")
                import traceback
                traceback.print_exc()
                return []
        
        # Check if there is vector data
        if not self.tag_vectors:
            print("Error: No available tag vectors")
            return []
        
        print(f"Searching for similar tags to '{tag_name}'...")
        
        # Get the target tag vector
        if tag_name not in self.tag_vectors:
            try:
                print(f"Tag '{tag_name}' not in vector library, generating vector...")
                target_vector = self.model.encode(tag_name)
                print(f"Vector generation successful, dimension: {target_vector.shape}")
            except Exception as e:
                print(f"Error generating vector: {e}")
                return []
        else:
            target_vector = self.tag_vectors[tag_name]
            print(f"Tag '{tag_name}' vector already exists")
            
        # 计算相似度
        similarities = []
        print(f"Starting to calculate similarity with {len(self.tag_vectors)} tags...")
        for other_tag, other_vector in self.tag_vectors.items():
            if other_tag != tag_name:
                try:
                    sim = self._compute_similarity(target_vector, other_vector)
                    similarities.append((other_tag, sim))
                except Exception as e:
                    print(f"Error calculating similarity with tag '{other_tag}': {e}")
                    continue
        
        # Sort by similarity
        similarities.sort(key=lambda x: x[1], reverse=True)
        
        # Return the top top_k results
        results = similarities[:top_k]
        
        print(f"Found {len(results)} similar tags:")
        for tag, score in results:
            print(f"  {tag}: {score:.4f}")
        
        return results

    @timer
    def search_tags(self, query_tags, weights=None):
        """Search tag combinations
        
        Args:
            query_tags: Query tag list
            weights: Weight list
        Returns:
            Encoded vector
        """
        try:
            if not query_tags:
                return []
                
            # Encode query tags
            vectors = self._get_vectors(query_tags)
            
            # If no weights are provided, use uniform weights
            if weights is None:
                weights = [1.0/len(query_tags)] * len(query_tags)
                
            # Calculate weighted average
            weights = np.array(weights)
            weights = weights / weights.sum()
            return np.average(vectors, axis=0, weights=weights).tolist()
            
        except Exception as e:
            print(f"Error searching tags: {e}")
            return []

    def parse_supertag_db(self, db_file_path):
        """Parse supertag-db.el file, extract tag information
        
        Args:
            db_file_path: Database file path
            
        Returns:
            dict: Tag information dictionary
        """
        print(f"Parsing database file: {db_file_path}")
        tag_info = {}
        
        try:
            with open(db_file_path) as f:
                content = f.read()
                
            # Extract tag definitions
            tag_pattern = r'\(ht-set!\s+org-supertag-db--object\s+"([^"]+)"\s+\'(\(:type\s+:tag.*?\))\)'
            for match in re.finditer(tag_pattern, content, re.DOTALL):
                tag_id = match.group(1)
                tag_props = match.group(2)
                
                # Skip metadata
                if tag_id == "metadata":
                    continue
                    
                # Extract field definitions
                fields = []
                fields_match = re.search(r':fields\s+(\(.*?\)|nil)(?=\s+:|$)', tag_props, re.DOTALL)
                if fields_match and fields_match.group(1) != 'nil':
                    field_str = fields_match.group(1)
                    # Parse field list
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
                
                # Extract behavior definitions
                behaviors = []
                behaviors_match = re.search(r':behaviors\s+(\(.*?\)|nil)(?=\s+:|$)', tag_props, re.DOTALL)
                if behaviors_match and behaviors_match.group(1) != 'nil':
                    behavior_str = behaviors_match.group(1).strip('()')
                    behaviors = [b.strip('"') for b in behavior_str.split()]
                
                # Extract relation definitions
                relations = []
                relations_match = re.search(r':relations\s+(\(.*?\)|nil)(?=\s+:|$)', tag_props, re.DOTALL)
                if relations_match and relations_match.group(1) != 'nil':
                    relation_str = relations_match.group(1).strip('()')
                    relations = [r.strip('"') for r in relation_str.split()]
                
                # Store tag information
                tag_info[tag_id] = {
                    'name': tag_id,
                    'type': 'tag',
                    'fields': fields,
                    'behaviors': behaviors,
                    'relations': relations
                }
            
            print(f"Found {len(tag_info)} tag definitions")
            return tag_info
            
        except Exception as e:
            print(f"Error parsing database file: {e}")
            import traceback
            traceback.print_exc()
            return {}
            
    def _parse_fields(self, fields_str):
        """Parse field definition list
        
        Args:
            fields_str: Field definition string
            
        Returns:
            list: Field definition list
        """
        fields = []
        if not fields_str or fields_str == 'nil':
            return fields
            
        # Remove outer parentheses
        fields_str = fields_str.strip('()')
        
        # Match each field definition
        field_pattern = r'\(:name\s+"([^"]+)"\s+:type\s+([^\s:]+)(?:\s+:description\s+"([^"]+)")?(?:\s+:options\s+\(([^)]+)\))?\)'
        for match in re.finditer(field_pattern, fields_str):
            name, type_, desc, options = match.groups()
            
            field = {
                'name': name,
                'type': type_
            }
            
            if desc:
                field['description'] = desc
            if options:
                field['options'] = [opt.strip('"') for opt in options.split()]
                
            fields.append(field)
            
        return fields
        
    def _to_serializable(self, obj):
        """Convert object to serializable format.
        
        Args:
            obj: Object to convert
            
        Returns:
            Serialized object
        """
        if isinstance(obj, dict):
            return {k: self._to_serializable(v) for k, v in obj.items()}
        elif isinstance(obj, list):
            return [self._to_serializable(v) for v in obj]
        elif isinstance(obj, tuple):
            return tuple(self._to_serializable(v) for v in v)
        elif isinstance(obj, np.ndarray):
            return obj.tolist()
        elif isinstance(obj, (np.int32, np.int64)):
            return int(obj)
        elif isinstance(obj, (np.float32, np.float64)):
            return float(obj)
        elif isinstance(obj, datetime):
            return obj.isoformat()
        return obj
            
    @timer
    def init_tag_library(self, db_file_path, output_path, tag_data=None):
        """Initialize tag library
        
        Args:
            db_file_path: Path to supertag-db.el
            output_path: Output file path
            tag_data: Optional direct tag data
        """
        print("Parsing tag data...")
        
        # Ensure output directory exists
        output_dir = os.path.dirname(output_path)
        if not os.path.exists(output_dir):
            os.makedirs(output_dir, exist_ok=True)
        
        if tag_data:
            # Use directly provided tag data
            tag_info = {}
            print(f"Received {len(tag_data)} tags")
            for tag_dict in tag_data:
                # Ensure tag_dict is a dictionary format
                if isinstance(tag_dict, dict):
                    tag_data_dict = tag_dict
                elif isinstance(tag_dict, list):
                    # Convert list to dictionary
                    tag_data_dict = {}
                    for item in tag_dict:
                        if isinstance(item, tuple) and len(item) == 2:
                            key, value = item
                            # If the value is a list and has only one element, take the element
                            if isinstance(value, list) and len(value) == 1:
                                value = value[0]
                            tag_data_dict[key] = value
                else:
                    print(f"Warning: Skipping invalid tag data: {tag_dict}")
                    continue
                    
                tag_id = tag_data_dict.get('id') or tag_data_dict.get('name')
                if not tag_id:
                    continue
                    
                # If tag_id is a list, take the first element
                if isinstance(tag_id, list):
                    tag_id = tag_id[0]
                    
                # Process fields
                fields = []
                raw_fields = tag_data_dict.get('fields', [])
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
                            
                # Process behaviors
                behaviors = []
                raw_behaviors = tag_data_dict.get('behaviors', [])
                if isinstance(raw_behaviors, dict):
                    behaviors = list(raw_behaviors.keys())
                elif isinstance(raw_behaviors, list):
                    behaviors = [b for b in raw_behaviors if b]
                    
                # Process relations
                relations = []
                raw_relations = tag_data_dict.get('relations', [])
                if isinstance(raw_relations, list):
                    relations = [r for r in raw_relations if r]
                    
                tag_info[tag_id] = {
                    'name': tag_id,
                    'type': 'tag',
                    'fields': fields,
                    'behaviors': behaviors,
                    'relations': relations,
                }
                print(f"Processed tag: {tag_id}")
        else:
            # Parse from file
            tag_info = self.parse_supertag_db(db_file_path)
            
        print(f"Found {len(tag_info)} tags")
        
        if not tag_info:
            print("Error: No valid tag data found")
            return
        
        # Build cache data
        cache_data = {
            'tags': {
                tag_id: {
                    'name': info['name'],
                    'vector': vector.tolist() if isinstance(vector, np.ndarray) else vector,
                    'info': info
                }
                for tag_id, (info, vector) in zip(tag_info.keys(), zip(tag_info.values(), tag_vectors.values()))
            },
            'metadata': {
                'total_tags': len(tag_info),
                'vector_dim': 768,
                'created_at': datetime.now().isoformat(),
                'model_name': self.model_name,
                'cooccurrence_matrix': {
                    'size': len(self.cooccur_matrix.tag_to_idx),
                    'method': 'pmi',
                    'updated_at': datetime.now().isoformat()
                }
            }
        }
        
        print(f"Saving to file: {output_path}")
        try:
            # Save to file
            with open(output_path, 'w') as f:
                json.dump(self._to_serializable(cache_data), f, indent=2)
            print(f"Tag library initialized, saved to {output_path}")
            print(f"File size: {os.path.getsize(output_path)} bytes")
            
            # Verify file is saved correctly
            if not os.path.exists(output_path):
                print(f"Error: Vector file not saved to {output_path}")
                return
        except Exception as e:
            print(f"Error saving file: {e}")
            import traceback
            traceback.print_exc()
            return

    def _get_state_path(self):
        """Get state file path"""
        cache_dir = os.path.join(os.path.dirname(__file__), 'cache')
        os.makedirs(cache_dir, exist_ok=True)
        return os.path.join(cache_dir, 'simtag_state.json')

    def _load_state(self):
        """Load tag engine state"""
        try:
            state_path = self._get_state_path()
            if os.path.exists(state_path):
                with open(state_path, 'r') as f:
                    saved_state = json.load(f)
                    # Update state, keep default values
                    self.state.update(saved_state)
        except Exception as e:
            print(f"Error loading state: {e}")

    def _save_state(self):
        """Save tag engine state"""
        try:
            state_path = self._get_state_path()
            with open(state_path, 'w') as f:
                json.dump(self.state, f, indent=2)
        except Exception as e:
            print(f"Error saving state: {e}")

    def _compute_similarity(self, vec1, vec2):
        """Calculate similarity between two vectors
        
        Args:
            vec1: First vector
            vec2: Second vector
            
        Returns:
            Similarity score
        """
        try:
            # Ensure vectors are numpy arrays
            if not isinstance(vec1, np.ndarray):
                vec1 = np.array(vec1)
            if not isinstance(vec2, np.ndarray):
                vec2 = np.array(vec2)
            
            # Ensure vectors are 2D
            if len(vec1.shape) == 1:
                vec1 = vec1.reshape(1, -1)
            if len(vec2.shape) == 1:
                vec2 = vec2.reshape(1, -1)
            
            # Calculate cosine similarity
            similarity = cosine_similarity(vec1, vec2)[0][0]
            
            return float(similarity)  # Ensure return Python native float
        except Exception as e:
            print(f"Error calculating similarity: {e}")
            print(f"Vector 1: shape={vec1.shape if isinstance(vec1, np.ndarray) else type(vec1)}")
            print(f"Vector 2: shape={vec2.shape if isinstance(vec2, np.ndarray) else type(vec2)}")
            raise

    def _build_cooccurrence_matrix(self, tag_info: Dict[str, Dict]):
        """构建共现矩阵"""
        print("Building cooccurrence matrix...")
        
        # 处理每个标签
        for tag_id, info in tag_info.items():
            # 基础组：标签本身
            base_group = {tag_id}
            
            # 添加关联标签
            for rel in info.get('relations', []):
                if isinstance(rel, str):
                    base_group.add(rel)
                elif isinstance(rel, dict) and 'target' in rel:
                    base_group.add(rel['target'])
            
            # 如果组中有多个标签，添加到共现矩阵
            if len(base_group) > 1:
                self.cooccur_matrix.add_tag_group(base_group)
                
        print(f"Built cooccurrence matrix with {len(self.cooccur_matrix.tag_to_idx)} tags")
        
        # 保存矩阵
        matrix_path = os.path.join(
            os.path.dirname(__file__),
            'cache',
            'cooccur_matrix.pkl'
        )
        self.cooccur_matrix.save(matrix_path)

def extract_entities(input_file, output_file):
    """Extract named entities from text
    
    Args:
        input_file: Input file path (containing text)
        output_file: Output file path for entities
    """
    try:
        # Read input file
        print(f"Reading input file: {input_file}")
        with open(input_file, 'r') as f:
            input_data = json.load(f)
            
        # Parse input data
        if isinstance(input_data, list) and len(input_data) >= 1:
            text = input_data[0]  # Text to analyze
            
            # Initialize Ollama Bridge for NER
            try:
                from ollama_bridge_bak import OllamaBridge
                ollama = OllamaBridge()
                
                # Build NER system prompt
                system = """You are a Named Entity Recognition (NER) specialist. Extract all entities from the given text and categorize them.

For each entity found, include:
1. Entity text (exact as it appears)
2. Entity type (select from: PERSON, ORG, LOC, TECH, CONCEPT, PRODUCT, EVENT, TIME, PROJ)
3. Confidence level (High, Medium, Low)

Return your result as a valid JSON array of entity objects:
[
  {"text": "entity_text", "type": "ENTITY_TYPE", "confidence": "confidence_level"},
  ...
]

Remember to extract ALL meaningful entities, not just the obvious ones."""

                # Build prompt
                prompt = f"""Extract all named entities from this text:

{text}

Return ONLY a valid JSON array with no comments or explanations."""

                # Generate response with Ollama
                response = ollama.generate(prompt, system)
                
                # Parse entities from response
                try:
                    entities = json.loads(response)
                    if not isinstance(entities, list):
                        print(f"Error: Response is not a list: {response}")
                        entities = []
                except json.JSONDecodeError:
                    print(f"Error: Could not parse JSON response: {response}")
                    entities = []
                    
                # Save results
                with open(output_file, 'w') as f:
                    json.dump(entities, f, indent=2)
                print(f"Entities saved to: {output_file}")
                
            except ImportError:
                print("Error: Ollama Bridge not available")
                return
        else:
            print(f"Error: Invalid input data format: {input_data}")
            return
            
    except Exception as e:
        print(f"Error extracting entities: {e}")
        return

def suggest_tags_from_text(input_file, output_file):
    """Suggest tags from text using NER
    
    Args:
        input_file: Input file path (containing text)
        output_file: Output file path for tags
    """
    try:
        # Read input file
        print(f"Reading input file: {input_file}")
        with open(input_file, 'r') as f:
            input_data = json.load(f)
            
        # Parse input data
        if isinstance(input_data, list) and len(input_data) >= 1:
            text = input_data[0]  # Text to analyze
            
            # Initialize Ollama Bridge for tag suggestions
            try:
                from ollama_bridge_bak import OllamaBridge
                ollama = OllamaBridge()
                
                # Get tag suggestions
                tags = ollama.suggest_tags(text)
                
                # Save results
                with open(output_file, 'w') as f:
                    json.dump(tags, f, indent=2)
                print(f"Tags saved to: {output_file}")
                
            except ImportError:
                print("Error: Ollama Bridge not available")
                return
        else:
            print(f"Error: Invalid input data format: {input_data}")
            return
            
    except Exception as e:
        print(f"Error suggesting tags: {e}")
        return

def main():
    import argparse
    
    parser = argparse.ArgumentParser(description='Tag similarity calculation tool')
    parser.add_argument('command', choices=['init', 'find_similar', 'extract_entities', 'suggest_tags_from_text'],
                      help='Command to execute: init, find_similar, extract_entities, or suggest_tags_from_text')
    parser.add_argument('input_file', help='Input file path')
    parser.add_argument('output_file', help='Output file path')
    parser.add_argument('--tag', help='Target tag name to find similar tags')
    parser.add_argument('--top-k', type=int, default=5,
                      help='Return the top K most similar tags')
    
    args = parser.parse_args()
    
    engine = SimtagBridge()
    
    try:
        # Read input file
        print(f"Reading input file: {args.input_file}")
        with open(args.input_file, 'r') as f:
            input_data = json.load(f)
            
        if args.command == 'init':
            # Parse input data - now expect a list with three elements
            if isinstance(input_data, list) and len(input_data) == 3:
                db_file = input_data[0]      # Database file path
                vector_file = input_data[1]  # Vector file path
                tag_data = input_data[2]     # Tag data list
                
                if isinstance(tag_data, list):
                    # Use vector_file as output path directly
                    engine.init_tag_library(db_file, vector_file, tag_data)
                else:
                    print(f"Error: Invalid tag data format: {tag_data}")
            else:
                print(f"Error: Invalid input data format: {input_data}")
        elif args.command == 'find_similar':
            # Parse input data
            if isinstance(input_data, list) and len(input_data) == 3:
                tag_name = input_data[0]  # First element is tag name
                top_k = input_data[1]     # Second element is top_k
                vector_file = input_data[2]  # Third element is vector file path
                
                print(f"Finding similar tags:")
                print(f"   Tag name: {tag_name}")
                print(f"   Return number: {top_k}")
                print(f"   Vector file: {vector_file}")
                
                # Check if vector file exists
                if not os.path.exists(vector_file):
                    print(f"Error: Vector file does not exist: {vector_file}")
                    return
                    
                # Find similar tags
                try:
                    results = engine.find_similar(tag_name, top_k, vector_file)
                    
                    # Save results
                    with open(args.output_file, 'w') as f:
                        json.dump(results, f, indent=2)
                    print(f"Results saved to: {args.output_file}")
                except Exception as e:
                    print(f"Error finding similar tags: {e}")
            else:
                print(f"Error: Invalid input data format: {input_data}")
        elif args.command == 'extract_entities':
            # Import NER extension
            try:
                from ollama_bridge_bak import extract_entities_from_file
                extract_entities_from_file(args.input_file, args.output_file)
            except ImportError:
                print("Error: NER extension not available")
        elif args.command == 'suggest_tags_from_text':
            # Import NER extension
            try:
                from ollama_bridge_bak import suggest_tags_from_file
                suggest_tags_from_file(args.input_file, args.output_file)
            except ImportError:
                print("Error: NER extension not available")
    except json.JSONDecodeError as e:
        print(f"Error parsing JSON file: {e}")
    except FileNotFoundError as e:
        print(f"File not found: {e}")
    except Exception as e:
        print(f"Error executing command: {e}")

if __name__ == "__main__":
    raise Exception("Simulated error for testing")

if __name__ == '__main__':
    main()
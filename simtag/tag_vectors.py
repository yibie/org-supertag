"""
SimTag tag vector processing module
Provides functions for generating, storing, and querying tag vectors
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
from transformers import AutoModel, AutoTokenizer

# Import SQLite-vec bridge for optional vector database support
try:
    from .sqlite_vec_bridge import SqliteVecBridge
    SQLITE_VEC_AVAILABLE = True
except ImportError:
    SQLITE_VEC_AVAILABLE = False

class TagVectorEngine:
    """Tag vector engine class"""
    
    def __init__(self, vector_file: str = None, use_sqlite_vec: bool = True, 
                 sqlite_db_path: str = None):
        """Initializes the tag vector engine
        
        Args:
            vector_file: Path to the vector file (for JSON storage, will be used as base for SQLite-vec path)
            use_sqlite_vec: Whether to use SQLite-vec for storage (defaults to True)
            sqlite_db_path: Path to SQLite database (if None, will use vector_file with .db extension)
        """
        self.logger = logging.getLogger("simtag.tag_vectors")
        self.vector_file = vector_file
        self.tag_vectors = {}  # Tag vector dictionary
        self.is_initialized = False
        self.model_name = 'intfloat/multilingual-e5-small'
        self._model = None
        
        # SQLite-vec support
        self.use_sqlite_vec = use_sqlite_vec and SQLITE_VEC_AVAILABLE
        self.sqlite_db_path = sqlite_db_path or (vector_file.replace('.json', '.db') if vector_file else None)
        self.sqlite_bridge = None
        
        if self.use_sqlite_vec:
            if not SQLITE_VEC_AVAILABLE:
                self.logger.warning("SQLite-vec not available, falling back to JSON storage")
                self.use_sqlite_vec = False
            else:
                self.logger.info(f"Initializing with SQLite-vec backend: {self.sqlite_db_path}")
                self.sqlite_bridge = SqliteVecBridge(self.sqlite_db_path)
                if self.sqlite_bridge.connect():
                    self.logger.info("SQLite-vec bridge connected successfully")
                else:
                    self.logger.error("Failed to connect SQLite-vec bridge, falling back to JSON")
                    self.use_sqlite_vec = False
                    self.sqlite_bridge = None
        
        # Load existing vectors
        if self.use_sqlite_vec and self.sqlite_bridge:
            self._load_vectors_from_sqlite()
        elif vector_file and os.path.exists(vector_file):
            self.load_vectors(vector_file)
            
    @property
    def model(self):
        """Lazily loads the model"""
        if self._model is None:
            self.logger.info(f"loading model: {self.model_name}")
            self._model = SentenceTransformer(
                self.model_name,
                cache_folder=os.path.join(os.path.dirname(__file__), 'models')
            )
            # Set device
            device = self._get_device()
            if device.type != 'cpu':
                self._model = self._model.to(device)
                self.logger.info(f"enabled {device.type.upper()} acceleration")
                
        return self._model
        
    def _get_device(self):
        """Gets the best available device"""
        if torch.cuda.is_available():
            return torch.device('cuda')
        elif torch.backends.mps.is_available():
            return torch.device('mps')
        return torch.device('cpu')
        
    def status(self) -> Dict[str, Any]:
        """Gets the engine status
        
        Returns:
            Status information dictionary
        """
        status = {
            "initialized": self.is_initialized,
            "vector_count": len(self.tag_vectors),
            "use_sqlite_vec": self.use_sqlite_vec,
            "sqlite_vec_available": SQLITE_VEC_AVAILABLE,
        }
        
        if self.use_sqlite_vec:
            status.update({
                "storage_backend": "SQLite-vec",
                "sqlite_db_path": self.sqlite_db_path,
                "sqlite_connected": self.sqlite_bridge is not None and self.sqlite_bridge.db is not None,
                "db_exists": os.path.exists(self.sqlite_db_path) if self.sqlite_db_path else False,
                "db_size": os.path.getsize(self.sqlite_db_path) if self.sqlite_db_path and os.path.exists(self.sqlite_db_path) else 0
            })
        else:
            status.update({
                "storage_backend": "JSON",
                "vector_file": self.vector_file,
                "file_exists": os.path.exists(self.vector_file) if self.vector_file else False,
                "file_size": os.path.getsize(self.vector_file) if self.vector_file and os.path.exists(self.vector_file) else 0
            })
        
        return status
            
    def load_vectors(self, vector_file: str) -> bool:
        """Loads tag vectors
        
        Args:
            vector_file: Path to the vector file
            
        Returns:
            True if loaded successfully, False otherwise
        """
        try:
            if not os.path.exists(vector_file):
                self.logger.error(f"vector file not found: {vector_file}")
                return False
                
            self.logger.info(f"loading vector file: {vector_file}")
            with open(vector_file, 'r') as f:
                data = json.load(f)
                
            if not isinstance(data, dict) or 'tags' not in data:
                self.logger.error(f"invalid vector file format")
                return False
                
            # Update vectors
            self.tag_vectors = {
                tag_id: np.array(info['vector']) 
                for tag_id, info in data['tags'].items()
            }
            
            self.vector_file = vector_file
            self.is_initialized = True
            self.logger.info(f"successfully loaded {len(self.tag_vectors)} tag vectors")
            return True
            
        except Exception as e:
            self.logger.error(f"error loading vector file: {e}")
            self.logger.error(traceback.format_exc())
            return False
    
    def _load_vectors_from_sqlite(self) -> bool:
        """Load tag vectors from SQLite-vec database
        
        Returns:
            True if loaded successfully, False otherwise
        """
        try:
            if not self.sqlite_bridge or not self.sqlite_bridge.db:
                self.logger.error("SQLite-vec bridge not connected")
                return False
            
            # Check if table exists
            cursor = self.sqlite_bridge.db.execute(
                "SELECT name FROM sqlite_master WHERE type='table' AND name='tag_vectors'"
            )
            if not cursor.fetchone():
                self.logger.info("SQLite-vec table 'tag_vectors' not found, will create on first save")
                self.is_initialized = True
                return True
            
            # Load vectors from database
            cursor = self.sqlite_bridge.db.execute(
                "SELECT rowid, content FROM tag_vectors"
            )
            rows = cursor.fetchall()
            
            self.tag_vectors = {}
            for rowid, tag_name in rows:
                # Get vector by rowid
                vector_data = self.sqlite_bridge.get_vector_by_rowid('tag_vectors', rowid)
                if vector_data and 'embedding' in vector_data:
                    # Convert binary embedding back to numpy array
                    embedding_bytes = vector_data['embedding']
                    vector = np.frombuffer(embedding_bytes, dtype=np.float32)
                    self.tag_vectors[tag_name] = vector
            
            self.is_initialized = True
            self.logger.info(f"Successfully loaded {len(self.tag_vectors)} tag vectors from SQLite-vec")
            return True
            
        except Exception as e:
            self.logger.error(f"Error loading vectors from SQLite-vec: {e}")
            self.logger.error(traceback.format_exc())
            return False
            
    def find_similar(self, tag_name: str, top_k: int = 5) -> List[Tuple[str, float]]:
        """Finds tags similar to the given tag
        
        Args:
            tag_name: Tag name
            top_k: Number of results to return
            
        Returns:
            List of similar tags, each element is (tag_name, similarity_score)
        """
        self.logger.info(f"finding similar tags: tag={tag_name}, top_k={top_k}")
        
        # Use SQLite-vec optimized search if available
        if self.use_sqlite_vec and self.sqlite_bridge:
            try:
                # Generate query vector
                query_vector = self.model.encode(tag_name)
                
                # Search using SQLite-vec
                results = self.sqlite_bridge.vector_search(
                    'tag_vectors',
                    query_vector.tolist(),
                    top_k
                )
                
                if results:
                    # Convert results to expected format
                    similar_tags = [
                        (result['content'], result['similarity'])
                        for result in results
                    ]
                    return similar_tags
                else:
                    self.logger.warning("No similar tags found in SQLite-vec")
                    return []
                
            except Exception as e:
                self.logger.error(f"SQLite-vec search failed: {e}")
                self.logger.error(traceback.format_exc())
                # Fall back to traditional method
        
        # Fallback to traditional method
        self.logger.warning("Falling back to traditional similarity search")
        
        # Check vector file
        if not self.is_initialized:
            if not self.vector_file or not os.path.exists(self.vector_file):
                self.logger.error("vector file not specified or not found")
                return []
            if not self.load_vectors(self.vector_file):
                self.logger.error("unable to load vector file")
                return []
        
        # Check tag vector dictionary
        if not self.tag_vectors:
            self.logger.error("no available tag vectors")
            return []
            
        # Get target tag vector
        if tag_name not in self.tag_vectors:
            try:
                self.logger.info(f"tag '{tag_name}' not in vector library, generating vector...")
                target_vector = self.model.encode(tag_name)
                self.logger.info(f"vector generated successfully, dimension: {target_vector.shape}")
            except Exception as e:
                self.logger.error(f"error generating vector: {e}")
                return []
        else:
            target_vector = self.tag_vectors[tag_name]
            self.logger.info(f"tag '{tag_name}' vector already exists")
            
        # Calculate similarity
        start_time = time.time()
        similarities = []
        for other_tag, other_vector in self.tag_vectors.items():
            if other_tag == tag_name:
                continue
            
            # Calculate cosine similarity
            similarity = np.dot(target_vector, other_vector) / (
                np.linalg.norm(target_vector) * np.linalg.norm(other_vector)
            )
            similarities.append((other_tag, similarity))
        
        # Sort by similarity
        similarities.sort(key=lambda x: x[1], reverse=True)
        
        # Return top k results
        results = similarities[:top_k]
        self.logger.info(f"found {len(results)} similar tags in {time.time() - start_time:.3f}s")
        return results
    
    def _find_similar_sqlite_vec(self, tag_name: str, top_k: int) -> List[Tuple[str, float]]:
        """Find similar tags using SQLite-vec vector search
        
        Args:
            tag_name: Tag name
            top_k: Number of results to return
            
        Returns:
            List of similar tags, each element is (tag_name, similarity_score)
        """
        try:
            # Ensure the table exists
            if not self._ensure_sqlite_table():
                self.logger.error("Failed to ensure SQLite-vec table exists")
                return []
            
            # Get or generate the target vector
            if tag_name in self.tag_vectors:
                target_vector = self.tag_vectors[tag_name]
                self.logger.info(f"Tag '{tag_name}' vector found in cache")
            else:
                self.logger.info(f"Tag '{tag_name}' not in vector library, generating vector...")
                target_vector = self.model.encode(tag_name)
                # Add to cache
                self.tag_vectors[tag_name] = target_vector
                
            # Convert to list for SQLite-vec
            query_vector = target_vector.tolist()
            
            # Use SQLite-vec for similarity search
            start_time = time.time()
            search_results = self.sqlite_bridge.vector_search('tag_vectors', query_vector, top_k + 1)
            
            if not search_results:
                self.logger.warning("No results from SQLite-vec search")
                return []
            
            # Convert results and filter out the query tag itself
            results = []
            for result in search_results:
                rowid = result['rowid']
                distance = result['distance']
                
                # Get tag name by rowid
                vector_data = self.sqlite_bridge.get_vector_by_rowid('tag_vectors', rowid)
                if vector_data and 'content' in vector_data:
                    result_tag_name = vector_data['content']
                    
                    # Skip the query tag itself
                    if result_tag_name != tag_name:
                        # SQLite-vec returns squared euclidean distance, convert to cosine similarity
                        # For normalized vectors, cosine distance = euclidean distance^2 / 2
                        # Cosine similarity = 1 - cosine distance
                        cosine_distance = distance / 2.0
                        similarity = 1.0 - cosine_distance
                        results.append((result_tag_name, similarity))
                        self.logger.debug(f"Found similar tag: {result_tag_name}, similarity: {similarity:.4f}")
                        
                        # Stop when we have enough results
                        if len(results) >= top_k:
                            break
                    else:
                        self.logger.debug(f"Skipping query tag itself: {result_tag_name}")
            
            elapsed = time.time() - start_time
            self.logger.info(f"SQLite-vec similarity search completed, time: {elapsed:.2f} seconds, found {len(results)} similar tags")
            
            return results
            
        except Exception as e:
            self.logger.error(f"Error in SQLite-vec similarity search: {e}")
            self.logger.error(traceback.format_exc())
            return []
        
    def _compute_similarity(self, vec1, vec2) -> float:
        """Computes the similarity between two vectors
        
        Args:
            vec1: The first vector
            vec2: The second vector
            
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
            sim = np.dot(vec1, vec2.T) / (np.linalg.norm(vec1) * np.linalg.norm(vec2))
            return float(sim[0][0])  # Ensure to return Python native float
        except Exception as e:
            self.logger.error(f"error calculating similarity: {e}")
            self.logger.error(traceback.format_exc())
            raise
            
    def test_engine(self, test_text: str) -> np.ndarray:
        """Tests if the engine is working correctly
        
        Args:
            test_text: Test text
            
        Returns:
            Generated vector
            
        Raises:
            Exception: If the engine is not working correctly
        """
        self.logger.info("testing engine functionality...")
        
        # Ensure the model is loaded
        if not self.model:
            raise RuntimeError("text similarity model not loaded")
            
        try:
            # Generate vector
            vector = self.model.encode(test_text)
            
            # Verify vector
            if not isinstance(vector, np.ndarray):
                raise TypeError(f"vector type error: {type(vector)}")
                
            if vector.shape[0] != 384:  # E5-small dimension
                raise ValueError(f"vector dimension error: {vector.shape}")
                
            self.logger.info("engine test successful")
            return vector.tolist()  # Convert to list for serialization
            
        except Exception as e:
            self.logger.error(f"engine test failed: {e}")
            self.logger.error(traceback.format_exc())
            raise

    def initialize(self, db_file: str, vector_file: str, tag_data: List[Dict] = None) -> Dict[str, Any]:
        """Initializes the tag library
        
        Args:
            db_file: Path to the database file
            vector_file: Output path for the vector file (used for SQLite-vec path)
            tag_data: List of tag data
            
        Returns:
            Initialization result information
        """
        try:
            # Ensure parameter types are correct
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
                    "message": f"vector file path must be a string, not {type(vector_file)}",
                    "result": None
                }
            
            # Log initialization parameters
            self.logger.info(f"initializing tag library:")
            self.logger.info(f"- database file: {db_file}")
            self.logger.info(f"- vector file: {vector_file}")
            self.logger.info(f"- tag data: {len(tag_data) if tag_data else 'none'}")
            
            # Set vector file path (will be used for SQLite-vec database)
            self.vector_file = vector_file
            self.sqlite_db_path = vector_file.replace('.json', '.db')
            
            # Initialize SQLite-vec bridge if not already done
            if self.use_sqlite_vec and not self.sqlite_bridge:
                self.sqlite_bridge = SqliteVecBridge(self.sqlite_db_path)
                if not self.sqlite_bridge.connect():
                    self.logger.error("Failed to connect SQLite-vec bridge")
                    return {
                        "status": "error",
                        "message": "Failed to connect SQLite-vec bridge",
                        "result": None
                    }
            
            # Test the engine first
            try:
                test_vector = self.test_engine("test sentence for initialization")
                self.logger.info("engine test successful")
            except Exception as e:
                return {
                    "status": "error",
                    "message": f"engine test failed: {e}",
                    "result": {
                        "vector_file": vector_file,
                        "db_file": db_file,
                        "model": None
                    }
                }

            # First ensure the model is loaded successfully
            if not self.model:
                return {
                    "status": "error",
                    "message": "unable to load text similarity model",
                    "result": {
                        "vector_file": vector_file,
                        "db_file": db_file,
                        "model": None
                    }
                }
            
            # Test if the model can work normally
            try:
                test_text = "test sentence for model verification"
                test_vector = self.model.encode(test_text)
                if not isinstance(test_vector, np.ndarray) or test_vector.shape[0] != 384:  # E5-small dimension
                    raise ValueError(f"model output vector dimension error: {test_vector.shape}")
            except Exception as e:
                self.logger.error(f"model functionality test failed: {e}")
                return {
                    "status": "error",
                    "message": f"model functionality test failed: {e}",
                    "result": {
                        "vector_file": vector_file,
                        "db_file": db_file,
                        "model": None
                    }
                }

            # Parse tag data
            tag_info = {}
            if tag_data:
                for tag_dict in tag_data:
                    # Handle tag data in different formats
                    if isinstance(tag_dict, dict):
                        tag_data_dict = tag_dict
                    elif isinstance(tag_dict, list):
                        # Convert list to dictionary
                        tag_data_dict = {}
                        for item in tag_dict:
                            if isinstance(item, (list, tuple)):
                                if len(item) == 2:
                                    # 处理 (key . value) 格式
                                    key, value = item
                                    if isinstance(value, list) and len(value) == 1:
                                        value = value[0]
                                    tag_data_dict[key] = value
                                elif len(item) == 3:
                                    # 处理 ['key', '.', 'value'] 或 ['key', Symbol('.'), 'value'] 格式
                                    key, symbol, value = item
                                    # 只要中间是分隔符（'.' 或 Symbol('.')），就接受这个格式
                                    if (isinstance(symbol, str) and symbol == '.') or \
                                       (hasattr(symbol, '__name__') and symbol.__name__ == '.'):
                                        if isinstance(value, list) and len(value) == 1:
                                            value = value[0]
                                        tag_data_dict[key] = value
                                    else:
                                        self.logger.debug(f"Skipping unknown format: {item}")
                            else:
                                self.logger.debug(f"Skipping non-list item: {item}")
                        
                        # If we couldn't convert the list, skip it
                        if not tag_data_dict:
                            self.logger.debug(f"No valid data extracted from: {tag_dict}")
                            continue
                    else:
                        self.logger.debug(f"Skipping unsupported data type: {type(tag_dict)}")
                        continue

                    # Extract tag ID
                    tag_id = tag_data_dict.get('id') or tag_data_dict.get('name')
                    if not tag_id:
                        continue
                        
                    # If tag_id is a list, take the first element
                    if isinstance(tag_id, list):
                        tag_id = tag_id[0]
                        
                    # Process fields
                    fields = self._process_fields(tag_data_dict.get('fields', []))
                    
                    # Process behaviors
                    behaviors = self._process_behaviors(tag_data_dict.get('behaviors', []))
                        
                    # Process relations
                    relations = self._process_relations(tag_data_dict.get('relations', []))
                        
                    # Store tag information
                    tag_info[tag_id] = {
                        'name': tag_id,
                        'type': 'tag',
                        'fields': fields,
                        'behaviors': behaviors,
                        'relations': relations,
                    }
            else:
                # Parse from database file
                tag_info = self.parse_supertag_db(db_file)
            
            self.logger.info(f"processed {len(tag_info)} tags")
            
            if not tag_info:
                self.logger.error("no valid tag data found")
                return {"status": "error", "message": "no valid tag data found"}
            
            # Ensure SQLite-vec table exists
            if not self._ensure_sqlite_table():
                self.logger.error("Failed to create SQLite-vec table")
                return {
                    "status": "error",
                    "message": "Failed to create SQLite-vec table",
                    "result": None
                }
            
            # Clear existing data
            self.sqlite_bridge.db.execute("DELETE FROM tag_vectors")
            self.sqlite_bridge.db.commit()
            
            # Generate and save tag vectors
            saved_count = 0
            for tag_id, info in tag_info.items():
                try:
                    # Generate vector
                    tag_vector = self.model.encode(tag_id)
                    
                    # Save to SQLite-vec
                    success = self.sqlite_bridge.insert_vector(
                        'tag_vectors',
                        tag_vector.tolist(),
                        {'content': tag_id, 'type': 'tag'},
                        None  # Let SQLite assign rowid
                    )
                    
                    if success:
                        saved_count += 1
                        # Cache in memory
                        self.tag_vectors[tag_id] = tag_vector
                    else:
                        self.logger.warning(f"Failed to save vector for tag: {tag_id}")
                        
                except Exception as e:
                    self.logger.error(f"Error processing tag '{tag_id}': {e}")
                    continue
            
            self.logger.info(f"Successfully saved {saved_count}/{len(tag_info)} tag vectors")
            
            # Update status
            self.is_initialized = True
            
            return {
                "status": "success",
                "result": {
                    "vector_file": vector_file,
                    "db_file": db_file,
                    "model": self.model_name,
                    "tag_count": saved_count,
                    "file_size": os.path.getsize(self.sqlite_db_path) if os.path.exists(self.sqlite_db_path) else 0
                }
            }
            
        except Exception as e:
            self.logger.error(f"error initializing tag library: {e}")
            self.logger.error(traceback.format_exc())
            return {
                "status": "error", 
                "message": str(e),
                "result": {
                    "vector_file": vector_file,
                    "db_file": db_file,
                    "model": self.model_name
                }
            }
    
    def _process_fields(self, raw_fields):
        """Processes field data"""
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
        """Processes behavior data"""
        behaviors = []
        if isinstance(raw_behaviors, dict):
            behaviors = list(raw_behaviors.keys())
        elif isinstance(raw_behaviors, list):
            behaviors = [b for b in raw_behaviors if b]
        return behaviors
    
    def _process_relations(self, raw_relations):
        """Processes relation data"""
        relations = []
        if isinstance(raw_relations, list):
            relations = [r for r in raw_relations if r]
        return relations
            
    def parse_supertag_db(self, db_file_path: str) -> Dict[str, Dict]:
        """Parses the supertag-db.el file to extract tag information
        
        Args:
            db_file_path: Path to the database file
            
        Returns:
            Dictionary of tag information
        """
        self.logger.info(f"parsing database file: {db_file_path}")
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
            
            self.logger.info(f"found {len(tag_info)} tag definitions")
            return tag_info
            
        except Exception as e:
            self.logger.error(f"error parsing database file: {e}")
            self.logger.error(traceback.format_exc())
            return {}

    def add_tag(self, tag_id: str, tag_name: str = None) -> bool:
        """Add a new tag to the vector library
        
        Args:
            tag_id: Tag ID
            tag_name: Tag name (if different from ID)
            
        Returns:
            True if added successfully, False otherwise
        """
        try:
            # Use tag_name if provided, otherwise use tag_id
            text_to_encode = tag_name or tag_id
            
            self.logger.info(f"Adding tag vector: {tag_id} -> '{text_to_encode}'")
            
            # Generate vector
            tag_vector = self.model.encode(text_to_encode)
            
            # Add to memory
            self.tag_vectors[tag_id] = tag_vector
            
            # Save to file
            self._save_vectors()
            
            self.logger.info(f"Successfully added tag: {tag_id}")
            return True
            
        except Exception as e:
            self.logger.error(f"Error adding tag '{tag_id}': {e}")
            self.logger.error(traceback.format_exc())
            return False
    
    def update_tag(self, tag_id: str, tag_name: str = None) -> bool:
        """Update an existing tag in the vector library
        
        Args:
            tag_id: Tag ID
            tag_name: Tag name (if different from ID)
            
        Returns:
            True if updated successfully, False otherwise
        """
        try:
            # Use tag_name if provided, otherwise use tag_id
            text_to_encode = tag_name or tag_id
            
            self.logger.info(f"Updating tag vector: {tag_id} -> '{text_to_encode}'")
            
            # Generate new vector
            tag_vector = self.model.encode(text_to_encode)
            
            # Update in memory
            self.tag_vectors[tag_id] = tag_vector
            
            # Save to file
            self._save_vectors()
            
            self.logger.info(f"Successfully updated tag: {tag_id}")
            return True
            
        except Exception as e:
            self.logger.error(f"Error updating tag '{tag_id}': {e}")
            self.logger.error(traceback.format_exc())
            return False
    
    def remove_tag(self, tag_id: str) -> bool:
        """Remove a tag from the vector library
        
        Args:
            tag_id: Tag ID to remove
            
        Returns:
            True if removed successfully, False otherwise
        """
        try:
            self.logger.info(f"Removing tag vector: {tag_id}")
            
            # Remove from memory
            if tag_id in self.tag_vectors:
                del self.tag_vectors[tag_id]
                
                # Save to file
                self._save_vectors()
                
                self.logger.info(f"Successfully removed tag: {tag_id}")
                return True
            else:
                self.logger.warning(f"Tag '{tag_id}' not found in vector library")
                return False
                
        except Exception as e:
            self.logger.error(f"Error removing tag '{tag_id}': {e}")
            self.logger.error(traceback.format_exc())
            return False
    
    def _save_vectors(self) -> bool:
        """Save current vectors to storage
        
        Returns:
            True if saved successfully, False otherwise
        """
        if self.use_sqlite_vec and self.sqlite_bridge:
            return self._save_vectors_to_sqlite()
        else:
            return self._save_vectors_to_json()
    
    def _save_vectors_to_sqlite(self) -> bool:
        """Save current vectors to SQLite-vec database
        
        Returns:
            True if saved successfully, False otherwise
        """
        try:
            if not self.sqlite_bridge or not self.sqlite_bridge.db:
                self.logger.error("SQLite-vec bridge not connected")
                return False
            
            # Ensure table exists
            if not self._ensure_sqlite_table():
                self.logger.error("Failed to create SQLite-vec table")
                return False
            
            # Clear existing data to avoid duplicates
            self.sqlite_bridge.db.execute("DELETE FROM tag_vectors")
            self.sqlite_bridge.db.commit()
            
            # Save each vector
            saved_count = 0
            for tag_id, vector in self.tag_vectors.items():
                try:
                    # Convert vector to list for storage
                    vector_list = vector.tolist() if isinstance(vector, np.ndarray) else vector
                    
                    # Insert vector
                    success = self.sqlite_bridge.insert_vector(
                        'tag_vectors',
                        vector_list,
                        {'content': tag_id, 'type': 'tag'},
                        None  # Let SQLite assign rowid
                    )
                    
                    if success:
                        saved_count += 1
                    else:
                        self.logger.warning(f"Failed to save vector for tag: {tag_id}")
                        
                except Exception as e:
                    self.logger.error(f"Error saving vector for tag '{tag_id}': {e}")
                    continue
            
            self.logger.info(f"Saved {saved_count}/{len(self.tag_vectors)} vectors to SQLite-vec database")
            return saved_count > 0
            
        except Exception as e:
            self.logger.error(f"Error saving vectors to SQLite-vec: {e}")
            self.logger.error(traceback.format_exc())
            return False
    
    def _save_vectors_to_json(self) -> bool:
        """Save current vectors to JSON file
        
        Returns:
            True if saved successfully, False otherwise
        """
        try:
            if not self.vector_file:
                self.logger.error("No vector file specified")
                return False
            
            # Build cache data
            cache_data = {
                'tags': {
                    tag_id: {
                        'name': tag_id,
                        'vector': vector.tolist(),
                        'info': {'name': tag_id, 'type': 'tag'}
                    }
                    for tag_id, vector in self.tag_vectors.items()
                },
                'metadata': {
                    'total_tags': len(self.tag_vectors),
                    'vector_dim': 384,  # E5-small dimension
                    'updated_at': datetime.now().isoformat(),
                    'model_name': self.model_name
                }
            }
            
            # Ensure directory exists if there is a directory path
            vector_dir = os.path.dirname(self.vector_file)
            if vector_dir:  # Only create directory if there is a directory path
                os.makedirs(vector_dir, exist_ok=True)
            
            # Save to file
            with open(self.vector_file, 'w') as f:
                json.dump(cache_data, f, indent=2)
                
            self.logger.info(f"Vectors saved to {self.vector_file}")
            return True
            
        except Exception as e:
            self.logger.error(f"Error saving vectors: {e}")
            self.logger.error(traceback.format_exc())
            return False
    
    def _ensure_sqlite_table(self) -> bool:
        """Ensures the SQLite-vec tables exist with correct schema
        
        Returns:
            True if tables exist or were created successfully, False otherwise
        """
        try:
            if not self.use_sqlite_vec or not self.sqlite_bridge:
                self.logger.error("SQLite-vec not available")
                return False
            
            # Check and create tag_vectors table
            cursor = self.sqlite_bridge.db.execute(
                "SELECT name FROM sqlite_master WHERE type='table' AND name='tag_vectors'"
            )
            if not cursor.fetchone():
                self.logger.info("Creating SQLite-vec table 'tag_vectors'")
                success = self.sqlite_bridge.create_vector_table(
                    'tag_vectors',
                    384,  # E5-small dimension
                    {
                        'content': 'TEXT',  # tag name
                        'type': 'TEXT',     # always 'tag'
                        'metadata': 'TEXT'   # JSON string for additional metadata
                    }
                )
                if not success:
                    self.logger.error("Failed to create tag_vectors table")
                    return False
            
            # Check and create node_vectors table
            cursor = self.sqlite_bridge.db.execute(
                "SELECT name FROM sqlite_master WHERE type='table' AND name='node_vectors'"
            )
            if not cursor.fetchone():
                self.logger.info("Creating SQLite-vec table 'node_vectors'")
                success = self.sqlite_bridge.create_vector_table(
                    'node_vectors',
                    384,  # E5-small dimension
                    {
                        'content': 'TEXT',      # node content
                        'node_id': 'TEXT',      # org node ID
                        'title': 'TEXT',        # node title
                        'tags': 'TEXT',         # JSON array of tags
                        'type': 'TEXT',         # always 'node'
                        'metadata': 'TEXT',      # JSON string for additional metadata
                        'created_at': 'TEXT',   # timestamp
                        'updated_at': 'TEXT'    # timestamp
                    }
                )
                if not success:
                    self.logger.error("Failed to create node_vectors table")
                    return False
            
            return True
            
        except Exception as e:
            self.logger.error(f"Error ensuring SQLite-vec tables: {e}")
            self.logger.error(traceback.format_exc())
            return False

    def sync_from_tags(self, tag_data: List[Dict[str, str]]) -> Dict[str, Any]:
        """Synchronize the vector library with the provided tag data
        
        Args:
            tag_data: List of dictionaries, each containing tag information
                     Expected format: [{"id": tag_id, "name": tag_name}, ...]
                     
        Returns:
            Dictionary with synchronization results
        """
        try:
            self.logger.info(f"Starting synchronization with {len(tag_data)} tags")
            
            # Parse tag data
            processed_tags = {}
            for tag_dict in tag_data:
                # Handle tag data in different formats  
                if isinstance(tag_dict, dict):
                    tag_data_dict = tag_dict
                elif isinstance(tag_dict, list):
                    # Convert list to dictionary
                    tag_data_dict = {}
                    for item in tag_dict:
                        if isinstance(item, (list, tuple)):
                            if len(item) == 2:
                                # 处理 (key . value) 格式
                                key, value = item
                                if isinstance(value, list) and len(value) == 1:
                                    value = value[0]
                                tag_data_dict[key] = value
                            elif len(item) == 3:
                                # 处理 ['key', '.', 'value'] 或 ['key', Symbol('.'), 'value'] 格式
                                key, symbol, value = item
                                # 只要中间是分隔符（'.' 或 Symbol('.')），就接受这个格式
                                if (isinstance(symbol, str) and symbol == '.') or \
                                   (hasattr(symbol, '__name__') and symbol.__name__ == '.'):
                                    if isinstance(value, list) and len(value) == 1:
                                        value = value[0]
                                    tag_data_dict[key] = value
                                else:
                                    self.logger.debug(f"Skipping unknown format: {item}")
                        else:
                            self.logger.debug(f"Skipping non-list item: {item}")
                    
                    # If we couldn't convert the list, skip it
                    if not tag_data_dict:
                        self.logger.debug(f"No valid data extracted from: {tag_dict}")
                        continue
                        
                    tag_dict = tag_data_dict
                else:
                    self.logger.debug(f"Skipping unsupported data type: {type(tag_dict)}")
                    continue
                
                # Extract tag ID and name
                tag_id = tag_dict.get('id') or tag_dict.get('name')
                tag_name = tag_dict.get('name') or tag_id
                
                if not tag_id:
                    self.logger.warning(f"Skipping tag without ID: {tag_dict}")
                    continue
                
                # If tag_id is a list, take the first element
                if isinstance(tag_id, list):
                    tag_id = tag_id[0]
                if isinstance(tag_name, list):
                    tag_name = tag_name[0]
                
                processed_tags[tag_id] = tag_name
            
            self.logger.info(f"Processed {len(processed_tags)} valid tags")
            
            if not processed_tags:
                self.logger.warning("No valid tags to synchronize")
                return {
                    "status": "success",
                    "added": 0,
                    "updated": 0,
                    "removed": 0,
                    "total": 0
                }
            
            # Track statistics
            added_count = 0
            updated_count = 0
            
            # Process each tag
            for tag_id, tag_name in processed_tags.items():
                try:
                    if tag_id in self.tag_vectors:
                        # Update existing tag
                        if self.update_tag(tag_id, tag_name):
                            updated_count += 1
                            self.logger.debug(f"Updated tag: {tag_id}")
                    else:
                        # Add new tag
                        if self.add_tag(tag_id, tag_name):
                            added_count += 1
                            self.logger.debug(f"Added tag: {tag_id}")
                        
                except Exception as e:
                    self.logger.error(f"Error processing tag '{tag_id}': {e}")
                    continue
            
            # Remove tags that are no longer in the sync data
            current_tags = set(self.tag_vectors.keys())
            new_tags = set(processed_tags.keys())
            tags_to_remove = current_tags - new_tags
            
            removed_count = 0
            for tag_id in tags_to_remove:
                try:
                    if self.remove_tag(tag_id):
                        removed_count += 1
                        self.logger.debug(f"Removed tag: {tag_id}")
                except Exception as e:
                    self.logger.error(f"Error removing tag '{tag_id}': {e}")
                    continue
            
            # Final save (in case individual operations didn't save)
            if added_count > 0 or updated_count > 0 or removed_count > 0:
                self._save_vectors()
            
            result = {
                "status": "success",
                "added": added_count,
                "updated": updated_count,
                "removed": removed_count,
                "total": len(self.tag_vectors)
            }
            
            self.logger.info(f"Synchronization completed: {result}")
            return result
            
        except Exception as e:
            self.logger.error(f"Error during synchronization: {e}")
            self.logger.error(traceback.format_exc())
            return {
                "status": "error",
                "message": str(e),
                "added": 0,
                "updated": 0,
                "removed": 0,
                "total": len(self.tag_vectors) if hasattr(self, 'tag_vectors') else 0
            }

    def vectorize_node(self, node_id: str, content: str, title: str = None, tags: List[str] = None) -> bool:
        """Vectorize a node and store it in the database
        
        Args:
            node_id: Node ID
            content: Node content
            title: Node title (optional)
            tags: List of tags (optional)
            
        Returns:
            True if vectorized successfully, False otherwise
        """
        try:
            self.logger.info(f"Vectorizing node: {node_id}")
            
            # Prepare text for vectorization (combine title and content)
            text_to_encode = f"{title}\n{content}" if title else content
            
            # Generate vector
            node_vector = self.model.encode(text_to_encode)
            
            # Prepare metadata
            metadata = {
                'vector_model': self.model_name,
                'vector_dim': len(node_vector),
                'vectorized_at': datetime.now().isoformat()
            }
            
            # Save to SQLite-vec
            if self.use_sqlite_vec and self.sqlite_bridge:
                # Ensure table exists
                if not self._ensure_sqlite_table():
                    self.logger.error("Failed to ensure SQLite-vec tables exist")
                    return False
                
                # Convert tags to JSON string
                tags_json = json.dumps(tags) if tags else '[]'
                
                # Current timestamp
                now = datetime.now().isoformat()
                
                # Insert into node_vectors table
                success = self.sqlite_bridge.insert_vector(
                    'node_vectors',
                    node_vector.tolist(),
                    {
                        'content': content,
                        'node_id': node_id,
                        'title': title or '',
                        'tags': tags_json,
                        'type': 'node',
                        'metadata': json.dumps(metadata),
                        'created_at': now,
                        'updated_at': now
                    },
                    None  # Let SQLite assign rowid
                )
                
                if success:
                    self.logger.info(f"Successfully vectorized node: {node_id}")
                    return True
                else:
                    self.logger.error(f"Failed to save vector for node: {node_id}")
                    return False
            else:
                self.logger.error("SQLite-vec not available")
                return False
            
        except Exception as e:
            self.logger.error(f"Error vectorizing node '{node_id}': {e}")
            self.logger.error(traceback.format_exc())
            return False

    def find_similar_nodes(self, query: str, top_k: int = 5, include_content: bool = True) -> List[Dict[str, Any]]:
        """Find nodes similar to the query text
        
        Args:
            query: Query text
            top_k: Number of results to return
            include_content: Whether to include node content in results
            
        Returns:
            List of similar nodes with metadata
        """
        try:
            self.logger.info(f"Finding nodes similar to query: {query}")
            
            if not self.use_sqlite_vec or not self.sqlite_bridge:
                self.logger.error("SQLite-vec not available")
                return []
            
            # Generate query vector
            query_vector = self.model.encode(query)
            
            # Search using SQLite-vec
            results = self.sqlite_bridge.vector_search(
                'node_vectors',
                query_vector.tolist(),
                top_k
            )
            
            if not results:
                self.logger.warning("No similar nodes found")
                return []
            
            # Process results
            nodes = []
            for result in results:
                # 确保所有键都存在，使用get并提供默认值
                node = {
                    'node_id': result.get('node_id', 'unknown_node_id'),
                    'title': result.get('title', ''),
                    'similarity': result.get('similarity', 0.0),
                    'tags': json.loads(result.get('tags', '[]')),
                    'metadata': json.loads(result.get('metadata', '{}'))
                }
                if include_content:
                    node['content'] = result.get('content', '')
                nodes.append(node)
            
            return nodes
            
        except Exception as e:
            self.logger.error(f"Error finding similar nodes: {e}")
            self.logger.error(traceback.format_exc())
            return []

    def hybrid_search(self, query: str, top_k: int = 5) -> List[Dict[str, Any]]:
        """Hybrid search combining tag and node vectors
        
        Args:
            query: Query text
            top_k: Number of results to return
            
        Returns:
            List of results with metadata
        """
        try:
            self.logger.info(f"Performing hybrid search for: {query}")
            
            if not self.use_sqlite_vec or not self.sqlite_bridge:
                self.logger.error("SQLite-vec not available")
                return []
            
            # Generate query vector
            query_vector = self.model.encode(query)
            
            # Search both tables
            tag_results = self.sqlite_bridge.vector_search(
                'tag_vectors',
                query_vector.tolist(),
                top_k
            )
            
            node_results = self.sqlite_bridge.vector_search(
                'node_vectors',
                query_vector.tolist(),
                top_k
            )
            
            # Combine and process results
            results = []
            
            # Process tag results
            for result in (tag_results or []):
                results.append({
                    'type': 'tag',
                    'content': result.get('content', ''),  # Use get() with default value
                    'similarity': result.get('similarity', 0.0),
                    'metadata': json.loads(result.get('metadata', '{}'))
                })
            
            # Process node results
            for result in (node_results or []):
                results.append({
                    'type': 'node',
                    'node_id': result.get('node_id', ''),
                    'title': result.get('title', ''),
                    'content': result.get('content', ''),
                    'similarity': result.get('similarity', 0.0),
                    'tags': json.loads(result.get('tags', '[]')),
                    'metadata': json.loads(result.get('metadata', '{}'))
                })
            
            # Sort by similarity
            results.sort(key=lambda x: x['similarity'], reverse=True)
            
            # Return top_k results
            return results[:top_k]
            
        except Exception as e:
            self.logger.error(f"Error in hybrid search: {e}")
            self.logger.error(traceback.format_exc())
            return []

    def remove_node_vector(self, node_id: str) -> bool:
        """Remove a node's vector from the database
        
        Args:
            node_id: Node ID
            
        Returns:
            True if removed successfully, False otherwise
        """
        try:
            self.logger.info(f"Removing vector for node: {node_id}")
            
            if not self.use_sqlite_vec or not self.sqlite_bridge:
                self.logger.error("SQLite-vec not available")
                return False
            
            # Delete from node_vectors table
            cursor = self.sqlite_bridge.db.execute(
                "DELETE FROM node_vectors WHERE node_id = ?",
                (node_id,)
            )
            self.sqlite_bridge.db.commit()
            
            if cursor.rowcount > 0:
                self.logger.info(f"Successfully removed vector for node: {node_id}")
                return True
            else:
                self.logger.warning(f"No vector found for node: {node_id}")
                return False
            
        except Exception as e:
            self.logger.error(f"Error removing vector for node '{node_id}': {e}")
            self.logger.error(traceback.format_exc())
            return False

    def update_node_vector(self, node_id: str, content: str, title: str = None, tags: List[str] = None) -> bool:
        """Update a node's vector in the database
        
        Args:
            node_id: Node ID
            content: New content
            title: New title (optional)
            tags: New tags (optional)
            
        Returns:
            True if updated successfully, False otherwise
        """
        try:
            self.logger.info(f"Updating vector for node: {node_id}")
            
            # First remove old vector
            self.remove_node_vector(node_id)
            
            # Then add new vector
            return self.vectorize_node(node_id, content, title, tags)
            
        except Exception as e:
            self.logger.error(f"Error updating vector for node '{node_id}': {e}")
            self.logger.error(traceback.format_exc())
            return False

    def batch_vectorize_nodes(self, nodes: List[Dict[str, Any]], batch_size: int = 50) -> Dict[str, Any]:
        """Batch vectorize nodes
        
        Args:
            nodes: List of node dictionaries, each containing:
                - node_id: Node ID
                - content: Node content
                - title: Node title (optional)
                - tags: List of tags (optional)
            batch_size: Number of nodes to process in each batch
            
        Returns:
            Dictionary with vectorization results
        """
        try:
            self.logger.info(f"Starting batch vectorization of {len(nodes)} nodes")
            
            if not self.use_sqlite_vec or not self.sqlite_bridge:
                self.logger.error("SQLite-vec not available")
                return {
                    "status": "error",
                    "message": "SQLite-vec not available",
                    "success_count": 0,
                    "failed_count": 0,
                    "total": len(nodes)
                }
            
            success_count = 0
            failed_nodes = []
            
            # Process nodes in batches
            for i in range(0, len(nodes), batch_size):
                batch = nodes[i:i + batch_size]
                self.logger.info(f"Processing batch {i//batch_size + 1}, size: {len(batch)}")
                
                for node in batch:
                    try:
                        # Extract data from node dictionary
                        node_id = node.get("node_id", "")
                        content = node.get("content", "")
                        title = node.get("title", "")
                        tags = node.get("tags", [])
                        
                        # Skip empty nodes
                        if not content.strip():
                            self.logger.warning(f"Skipping empty node: {node_id}")
                            failed_nodes.append(node_id)
                            continue
                        
                        # Generate vector
                        node_vector = self.model.encode(content)
                        
                        # Save to SQLite-vec
                        if self.use_sqlite_vec and self.sqlite_bridge:
                            # Convert tags to JSON string
                            tags_json = json.dumps(tags)
                            
                            # Current timestamp
                            now = datetime.now().isoformat()
                            
                            # Insert into node_vectors table
                            success = self.sqlite_bridge.insert_vector(
                                'node_vectors',
                                node_vector.tolist(),
                                {
                                    'content': content,
                                    'node_id': node_id,
                                    'title': title,
                                    'tags': tags_json,
                                    'type': 'node',
                                    'metadata': '{}',
                                    'created_at': now,
                                    'updated_at': now
                                },
                                None  # Let SQLite assign rowid
                            )
                            
                            if success:
                                success_count += 1
                                self.logger.debug(f"Successfully vectorized node: {node_id}")
                            else:
                                failed_nodes.append(node_id)
                                self.logger.warning(f"Failed to save vector for node: {node_id}")
                                
                    except Exception as e:
                        failed_nodes.append(node.get("node_id", "unknown"))
                        self.logger.error(f"Error processing node: {e}")
                        self.logger.debug(traceback.format_exc())
                        continue
            
            self.logger.info(f"Batch vectorization completed. Success: {success_count}, Failed: {len(failed_nodes)}")
            
            return {
                "status": "success",
                "message": "Batch vectorization completed",
                "success_count": success_count,
                "failed_count": len(failed_nodes),
                "failed_nodes": failed_nodes,
                "total": len(nodes)
            }
            
        except Exception as e:
            self.logger.error(f"Error in batch vectorization: {e}")
            self.logger.error(traceback.format_exc())
            return {
                "status": "error",
                "message": str(e),
                "success_count": success_count,
                "failed_count": len(nodes),
                "total": len(nodes)
            }

    def batch_vectorize_nodes_backend(self, nodes_data: List[Dict[str, Any]]) -> Dict[str, Any]:
        """在后端批量向量化节点
        
        Args:
            nodes_data: List of node dictionaries, each containing:
                - node_id: Node ID
                - content: Node content
                - title: Node title (optional)
                - tags: List of tags (optional)
            
        Returns:
            Dictionary with vectorization results
        """
        try:
            self.logger.info(f"Starting backend batch vectorization of {len(nodes_data)} nodes.")
            
            if not self.use_sqlite_vec or not self.sqlite_bridge:
                self.logger.error("SQLite-vec not available for batch vectorization.")
                return {
                    "status": "error",
                    "message": "SQLite-vec not available",
                    "success_count": 0,
                    "failed_nodes": []
                }
            
            self.logger.info(f"Using SQLite-vec for storage: {self.sqlite_db_path}")
            success_count = 0
            failed_nodes_info = [] # Store more info about failed nodes
            
            # 批量收集节点文本
            batch_texts_to_encode = []
            batch_node_metadata = [] # Store corresponding metadata for each text
            
            self.logger.info(f"Preparing {len(nodes_data)} nodes for batch vectorization...")
            for idx, node_input in enumerate(nodes_data):
                try:
                    # Elisp端传递过来的是alist，需要转换
                    if isinstance(node_input, list):
                        node = {item[0]: item[2] if len(item) == 3 and item[1] == '.' else item[1] for item in node_input}
                    elif isinstance(node_input, dict):
                        node = node_input
                    else:
                        self.logger.warning(f"Skipping node with unexpected data type: {type(node_input)} at index {idx}")
                        failed_nodes_info.append({
                            "node_id": f"unknown_index_{idx}",
                            "error": f"Unexpected data type: {type(node_input)}"
                        })
                        continue

                    node_id = node.get('node_id', '')
                    content = node.get('content', '')
                    title = node.get('title', '')
                    tags = node.get('tags', [])

                    self.logger.debug(f"Processing node {idx + 1}/{len(nodes_data)}: ID='{node_id}', Title='{title[:30]}...'")

                    if not node_id:
                        self.logger.warning(f"Skipping node at index {idx} due to missing 'node_id'. Data: {node}")
                        failed_nodes_info.append({
                            "node_id": "MISSING_ID",
                            "original_index": idx,
                            "error": "Missing node_id"
                        })
                        continue
                    
                    if not content.strip() and not title.strip():
                        self.logger.warning(f"Skipping node '{node_id}' due to empty content and title.")
                        failed_nodes_info.append({
                            "node_id": node_id,
                            "error": "Empty content and title"
                        })
                        continue
                    
                    text_to_encode = f"{title}\n{content}".strip()
                    batch_texts_to_encode.append(text_to_encode)
                    batch_node_metadata.append({
                        'node_id': node_id,
                        'content': content, # Keep original content for DB
                        'title': title,
                        'tags': tags
                    })
                    
                except Exception as e:
                    self.logger.error(f"Error preparing node at index {idx} (ID: {node.get('node_id', 'unknown')}): {e}", exc_info=True)
                    failed_nodes_info.append({
                        "node_id": node.get('node_id', f"error_idx_{idx}"),
                        "error": f"Preparation error: {str(e)}"
                    })
                    continue
            
            self.logger.info(f"Prepared {len(batch_texts_to_encode)} texts for encoding.")

            # 批量向量化
            if batch_texts_to_encode:
                self.logger.info(f"Encoding {len(batch_texts_to_encode)} texts in a batch...")
                try:
                    batch_vectors = self.model.encode(batch_texts_to_encode, batch_size=32) # Adjust batch_size as needed
                    self.logger.info(f"Successfully encoded {len(batch_vectors)} vectors. Shape of first vector: {batch_vectors[0].shape if len(batch_vectors) > 0 else 'N/A'}")
                    
                    # 批量保存到数据库
                    self.logger.info(f"Attempting to save {len(batch_vectors)} vectors to database...")
                    for i, node_meta in enumerate(batch_node_metadata):
                        try:
                            vector = batch_vectors[i]
                            tags_json = json.dumps(node_meta['tags'])
                            now = datetime.now().isoformat()
                            
                            db_metadata = {
                                'content': node_meta['content'], # Store original content
                                'node_id': node_meta['node_id'],
                                'title': node_meta['title'],
                                'tags': tags_json,
                                'type': 'node',
                                'metadata': json.dumps({'vectorized_at': now, 'model': self.model_name}), # Store vectorization time and model
                                'created_at': now, # Assuming new inserts, or should be from node data if available
                                'updated_at': now
                            }
                            
                            self.logger.debug(f"Inserting vector for node_id: {node_meta['node_id']}")
                            success = self.sqlite_bridge.insert_vector(
                                'node_vectors',
                                vector.tolist(),
                                db_metadata,
                                None # Let SQLite assign rowid
                            )
                            
                            if success:
                                success_count += 1
                                self.logger.debug(f"Successfully saved vector for node: {node_meta['node_id']}")
                            else:
                                self.logger.warning(f"Failed to save vector for node: {node_meta['node_id']} (insert_vector returned False)")
                                failed_nodes_info.append({
                                    "node_id": node_meta['node_id'],
                                    "error": "sqlite_bridge.insert_vector returned False"
                                })
                                
                        except Exception as e:
                            self.logger.error(f"Error saving vector for node {node_meta['node_id']}: {e}", exc_info=True)
                            failed_nodes_info.append({
                                "node_id": node_meta['node_id'],
                                "error": f"DB insertion error: {str(e)}"
                            })
                            
                except Exception as e:
                    self.logger.error(f"Batch encoding or saving process failed: {e}", exc_info=True)
                    # If batch encoding fails, all nodes in this batch are considered failed
                    for node_meta in batch_node_metadata:
                        if not any(fn['node_id'] == node_meta['node_id'] for fn in failed_nodes_info):
                            failed_nodes_info.append({
                                "node_id": node_meta['node_id'],
                                "error": f"Batch encoding/saving error: {str(e)}"
                            })
            else:
                self.logger.info("No texts to encode in this batch.")
            
            self.logger.info(f"Backend batch vectorization completed. Success: {success_count}, Failed: {len(failed_nodes_info)}")
            if failed_nodes_info:
                self.logger.warning(f"Details of failed nodes: {failed_nodes_info}")

            return {
                "status": "success",
                "success_count": success_count,
                "failed_nodes": failed_nodes_info # Return detailed failure info
            }
            
        except Exception as e:
            error_msg = f"Critical error in backend_batch_vectorize_nodes: {str(e)}"
            self.logger.error(error_msg, exc_info=True)
            return {
                "status": "error",
                "message": error_msg,
                "success_count": 0,
                "failed_nodes": []
            }

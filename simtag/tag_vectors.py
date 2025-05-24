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

class TagVectorEngine:
    """Tag vector engine class"""
    
    def __init__(self, vector_file: str = None):
        """Initializes the tag vector engine
        
        Args:
            vector_file: Path to the vector file
        """
        self.logger = logging.getLogger("simtag.tag_vectors")
        self.vector_file = vector_file
        self.tag_vectors = {}  # Tag vector dictionary
        self.is_initialized = False
        self.model_name = 'sentence-transformers/paraphrase-MiniLM-L6-v2'
        self._model = None
        
        if vector_file and os.path.exists(vector_file):
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
        return {
            "initialized": self.is_initialized,
            "vector_file": self.vector_file,
            "vector_count": len(self.tag_vectors),
            "file_exists": os.path.exists(self.vector_file) if self.vector_file else False,
            "file_size": os.path.getsize(self.vector_file) if self.vector_file and os.path.exists(self.vector_file) else 0
        }
            
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
            
    def find_similar(self, tag_name: str, top_k: int = 5) -> List[Tuple[str, float]]:
        """Finds tags similar to the given tag
        
        Args:
            tag_name: Tag name
            top_k: Number of results to return
            
        Returns:
            List of similar tags, each element is (tag_name, similarity_score)
        """
        self.logger.info(f"finding similar tags: tag={tag_name}, top_k={top_k}")
        
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
        self.logger.info(f"calculating similarity with {len(self.tag_vectors)} tags...")
        
        for other_tag, other_vector in self.tag_vectors.items():
            if other_tag != tag_name:
                try:
                    # Calculate cosine similarity
                    sim = self._compute_similarity(target_vector, other_vector)
                    similarities.append((other_tag, sim))
                except Exception as e:
                    self.logger.error(f"error calculating similarity with tag '{other_tag}': {e}")
                    continue
        
        # Sort by similarity
        similarities.sort(key=lambda x: x[1], reverse=True)
        
        # Return top_k results
        results = similarities[:top_k]
        
        elapsed = time.time() - start_time
        self.logger.info(f"similarity calculation completed, time: {elapsed:.2f} seconds, found {len(results)} similar tags")
        
        return results
        
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
                
            if vector.shape[0] != 384:  # MiniLM-L6 dimension
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
            vector_file: Output path for the vector file
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
            
            self.vector_file = vector_file
            
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
                if not isinstance(test_vector, np.ndarray) or test_vector.shape[0] != 384:  # MiniLM-L6 dimension
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

            # Ensure the output directory exists
            os.makedirs(os.path.dirname(vector_file), exist_ok=True)
            
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
                            if isinstance(item, (list, tuple)) and len(item) == 2:
                                key, value = item
                                # Handle Symbol('.') and other special cases
                                if hasattr(value, '__name__') and value.__name__ == '.':
                                    # Skip Symbol('.') entries
                                    continue
                                if isinstance(value, list) and len(value) == 1:
                                    value = value[0]
                                tag_data_dict[key] = value
                            elif isinstance(item, (list, tuple)) and len(item) == 3:
                                # Handle triplet format like ['id', Symbol('.'), 'c_maker']
                                key, symbol, value = item
                                if hasattr(symbol, '__name__') and symbol.__name__ == '.':
                                    # This is a valid triplet with Symbol('.') separator
                                    tag_data_dict[key] = value
                                else:
                                    self.logger.warning(f"Unknown triplet format: {item}")
                        
                        # If we couldn't convert the list, skip it
                        if not tag_data_dict:
                            self.logger.warning(f"skipping invalid tag data: {tag_dict}")
                            continue
                    else:
                        self.logger.warning(f"skipping invalid tag data: {tag_dict}")
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
            
            # Generate tag vectors
            tag_vectors = {}
            for tag_id, info in tag_info.items():
                try:
                    # Generate vector
                    tag_vector = self.model.encode(tag_id)
                    tag_vectors[tag_id] = tag_vector
                except Exception as e:
                    self.logger.error(f"error generating tag '{tag_id}' vector: {e}")
                    continue
            
            # Build cache data
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
                    'vector_dim': 384,  # MiniLM-L6 dimension
                    'created_at': datetime.now().isoformat(),
                    'model_name': 'sentence-transformers/paraphrase-MiniLM-L6-v2'
                }
            }
            
            # Save to file
            with open(vector_file, 'w') as f:
                json.dump(cache_data, f, indent=2)
                
            self.logger.info(f"tag library initialized, saved to {vector_file}")
            self.logger.info(f"file size: {os.path.getsize(vector_file)} bytes")
            
            # Update status
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
            self.logger.error(f"error initializing tag library: {e}")
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
        """Save current vectors to file
        
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
                    'vector_dim': 384,  # MiniLM-L6 dimension
                    'updated_at': datetime.now().isoformat(),
                    'model_name': self.model_name
                }
            }
            
            # Ensure directory exists
            os.makedirs(os.path.dirname(self.vector_file), exist_ok=True)
            
            # Save to file
            with open(self.vector_file, 'w') as f:
                json.dump(cache_data, f, indent=2)
                
            self.logger.info(f"Vectors saved to {self.vector_file}")
            return True
            
        except Exception as e:
            self.logger.error(f"Error saving vectors: {e}")
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
                        if isinstance(item, (list, tuple)) and len(item) == 2:
                            key, value = item
                            # Handle Symbol('.') and other special cases
                            if hasattr(value, '__name__') and value.__name__ == '.':
                                # Skip Symbol('.') entries
                                continue
                            if isinstance(value, list) and len(value) == 1:
                                value = value[0]
                            tag_data_dict[key] = value
                        elif isinstance(item, (list, tuple)) and len(item) == 3:
                            # Handle triplet format like ['id', Symbol('.'), 'c_maker']
                            key, symbol, value = item
                            if hasattr(symbol, '__name__') and symbol.__name__ == '.':
                                # This is a valid triplet with Symbol('.') separator
                                tag_data_dict[key] = value
                            else:
                                self.logger.warning(f"Unknown triplet format: {item}")
                    
                    # If we couldn't convert the list, skip it
                    if not tag_data_dict:
                        self.logger.warning(f"Skipping invalid tag data: {tag_dict}")
                        continue
                        
                    tag_dict = tag_data_dict
                else:
                    self.logger.warning(f"Skipping invalid tag data: {tag_dict}")
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
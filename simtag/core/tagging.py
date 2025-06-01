"""
Tag Processing Core Engine
Integrates entity extraction, tag generation, and relationship analysis functionality
"""
import logging
from .storage import VectorStorage
from ..services.ollama import OllamaService
from ..embeddings import get_embedding_model

class TaggingEngine:
    def __init__(self, config, storage: VectorStorage):
        self.config = config
        self.storage = storage
        self.logger = logging.getLogger("tagging")
        self.ollama = OllamaService()
        
        # Initialize embedding model
        self.embedding_model = get_embedding_model(config.embedding_model)
        if self.embedding_model is None:
            self.logger.error(
                f"Critical error: Failed to load embedding model '{config.embedding_model}'. Vector-related features will be unavailable."
            )
            self.logger.error(
                "For detailed errors, please check the logs from the 'embeddings' module."
            )
            self.logger.error(
                "Common causes: Network connection issues, proxy configuration errors, or missing dependencies."
            )
            self.logger.error(
                "Recommended action: Check your network/proxy settings and try running the `simtag/setup.sh` script from the project root (e.g., execute `./simtag/setup.sh` or `bash simtag/setup.sh`) for installation and configuration help. If executing directly, ensure the script has execution permissions (can be set using `chmod +x simtag/setup.sh`)."
            )
        else:
            self.logger.info(f"Successfully loaded and using embedding model: {self.embedding_model.model_name}")
        
    def generate_tags(self, text: str, limit: int = 5, use_ai: bool = True) -> list:
        """
        Generate tags
        :param use_ai: Whether to use AI generation (otherwise use basic algorithm)
        """
        if use_ai and self.ollama.available:
            self.logger.info("Using Ollama to generate AI tags")
            return self.ollama.generate_tags(text, limit)
        
        # Basic keyword extraction algorithm
        self.logger.info("Using basic algorithm to generate tags")
        words = [w.strip() for w in text.split() if len(w) > 3]
        return list(set(words))[:limit]
    
    def extract_entities(self, text: str, use_ai: bool = True) -> list:
        """Entity recognition"""
        if use_ai and self.ollama.available:
            return self.ollama.extract_entities(text)
        
        # Basic entity recognition
        entities = []
        for i, word in enumerate(text.split()):
            if word.istitle() and len(word) > 2:
                entities.append({
                    "entity": word,
                    "type": "PROPER_NOUN",
                    "start": i,
                    "end": i + len(word)
                })
        return entities
    
    def analyze_relations(self, tag: str, related_tags: list, use_ai: bool = True) -> list:
        """Analyze tag relationships"""
        if use_ai and self.ollama.available:
            return self.ollama.analyze_relations(tag, related_tags)
        
        # Basic relationship analysis
        return [{
            "tag": rt,
            "relation": "RELATED",
            "reason": f"Frequently co-occurs with {tag}"
        } for rt in related_tags]
    
    def find_similar_tags(self, tag: str, limit: int = 10) -> list:
        """
        Find similar tags
        
        Args:
            tag: Tag name or ID
            limit: Result count limit
            
        Returns:
            List of similar tags, each element is a (tag_id, similarity_score) tuple
        """
        try:
            self.logger.info(f"Finding similar tags: {tag}, limit={limit}")
            
            # First try to treat input as tag ID, get its vector
            vector = None
            
            # Check if it's a known tag ID
            if self.storage.has_tag_vector(tag):
                self.logger.info(f"Found tag ID: {tag}")
                vector = self.storage.get_tag_vector(tag)
            else:
                # If not a known ID, generate vector for tag name
                self.logger.info(f"Generating vector for tag name: {tag}")
                vector = self._generate_vector_for_tag(tag)
            
            if vector is not None:
                # Use vector to query similar tags
                similar_tags = self.storage.find_similar_tags(vector, limit)
                self.logger.info(f"Found {len(similar_tags)} similar tags")
                return similar_tags
            else:
                self.logger.warning(f"Unable to generate vector for tag: {tag}")
                return []
                
        except Exception as e:
            self.logger.error(f"Failed to find similar tags: {str(e)}")
            return []
    
    def _generate_vector_for_tag(self, tag_name: str) -> list:
        """
        Generate vector for tag name
        
        Args:
            tag_name: Tag name
            
        Returns:
            Tag vector
        """
        try:
            if self.embedding_model is not None:
                # Use embedding model to generate true semantic vector
                self.logger.info(f"Using embedding model to generate semantic vector for tag: {tag_name}")
                vector = self.embedding_model.get_embedding(tag_name)
                return vector
            else:
                # Embedding model unavailable, raise error
                self.logger.error(f"Embedding model unavailable, cannot generate vector for tag: {tag_name}")
                raise ValueError(f"Embedding model is not available, cannot generate vector for tag: {tag_name}")
        except Exception as e:
            self.logger.error(f"Failed to generate tag vector: {str(e)}")
            # Re-raise the exception if it's the ValueError we just raised,
            # otherwise return None or handle other exceptions as before.
            if isinstance(e, ValueError) and "Embedding model is not available" in str(e):
                raise
            return None
    
    def sync_tags(self, db_file: str, tag_data: list) -> dict:
        """
        Synchronize tag vector database
        
        Args:
            db_file: Database file path
            tag_data: Tag data list, each element in [tag_id, tag_name] format
            
        Returns:
            Synchronization result statistics
        """
        self.logger.info(f"Starting tag library sync: database={db_file}, tag count={len(tag_data)}")
        
        # Record received data type and format sample
        if tag_data:
            self.logger.info(f"Received data type: {type(tag_data)}, first data: {tag_data[0]}, type: {type(tag_data[0])}")
        
        # Record initial state
        initial_stats = self.storage.get_stats()
        
        # Get current stored tag ID list
        existing_tag_ids = set(self.storage.list_tag_ids())
        
        # Convert incoming tag data to dictionary format {tag_id: tag_name}
        new_tags = {}
        for tag_item in tag_data:
            # Support multiple formats, from simplest to complex processing
            try:
                # 1. Array/tuple/list format [id, name]
                if isinstance(tag_item, (list, tuple)) and len(tag_item) == 2:
                    tag_id, tag_name = tag_item
                    
                # 2. Dictionary format {"id": id, "name": name}
                elif isinstance(tag_item, dict) and "id" in tag_item and "name" in tag_item:
                    tag_id = tag_item["id"]
                    tag_name = tag_item["name"]
                    
                # 3. Other serialized formats (try direct parsing)
                else:
                    self.logger.warning(f"Attempting to parse unknown format tag data: {tag_item}")
                    # Last attempt: if it's an indexable object
                    if hasattr(tag_item, "__getitem__"):
                        tag_id = tag_item[0] if len(tag_item) > 0 else None
                        tag_name = tag_item[1] if len(tag_item) > 1 else None
                    else:
                        # Cannot parse, skip this item
                        self.logger.error(f"Unparseable tag data format: {tag_item}")
                        continue
                    
                # Ensure data is valid
                if tag_id and tag_name:
                    # Convert tag ID and name to strings to ensure type consistency
                    tag_id = str(tag_id)
                    tag_name = str(tag_name)
                    new_tags[tag_id] = tag_name
            except Exception as e:
                self.logger.error(f"Error processing tag data item: {e}, data: {tag_item}")
                continue
        
        self.logger.info(f"Processed tag count: {len(new_tags)}")
        
        # Calculate tags to add and remove
        new_tag_ids = set(new_tags.keys())
        tags_to_add = new_tag_ids - existing_tag_ids
        tags_to_update = existing_tag_ids & new_tag_ids
        tags_to_remove = existing_tag_ids - new_tag_ids
        
        # Record processing plan
        self.logger.info(f"Processing plan: add={len(tags_to_add)}, update={len(tags_to_update)}, remove={len(tags_to_remove)}")
        
        # Execute add and update operations
        added_count = 0
        updated_count = 0
        for tag_id in tags_to_add:
            tag_name = new_tags[tag_id]
            if self.update_tag_vector(tag_id, tag_name):
                added_count += 1
        
        for tag_id in tags_to_update:
            tag_name = new_tags[tag_id]
            if self.update_tag_vector(tag_id, tag_name):
                updated_count += 1
        
        # Execute removal operations
        removed_count = 0
        for tag_id in tags_to_remove:
            if self.remove_tag_vector(tag_id):
                removed_count += 1
        
        # Get final state
        final_stats = self.storage.get_stats()
        
        # Return sync results
        return {
            "initial_count": initial_stats.get("tag_vectors", 0),
            "final_count": final_stats.get("tag_vectors", 0),
            "added": added_count,
            "updated": updated_count,
            "removed": removed_count,
            "total_processed": len(tag_data)
        }
    
    def update_tag_vector(self, tag_id: str, tag_name: str) -> bool:
        """
        Update vector for a single tag
        
        Args:
            tag_id: Tag ID
            tag_name: Tag name
            
        Returns:
            Whether update was successful
        """
        try:
            self.logger.info(f"Updating tag vector: id={tag_id}, name={tag_name}")
            
            # Generate vector
            vector = self._generate_vector_for_tag(tag_name)
            
            if vector is None:
                return False
                
            # Get vector dimension
            vector_dim = len(vector)
            
            # Store vector
            self.storage.save_tag_vector(tag_id, vector, vector_dim)
            
            return True
        except Exception as e:
            self.logger.error(f"Failed to update tag vector: {str(e)}")
            return False
    
    def remove_tag_vector(self, tag_id: str) -> bool:
        """
        Remove tag vector
        
        Args:
            tag_id: Tag ID
            
        Returns:
            Whether removal was successful
        """
        try:
            self.logger.info(f"Removing tag vector: id={tag_id}")
            self.storage.delete_tag_vector(tag_id)
            return True
        except Exception as e:
            self.logger.error(f"Failed to remove tag vector: {str(e)}")
            return False

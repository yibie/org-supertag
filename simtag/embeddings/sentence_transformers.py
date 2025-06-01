"""
Embedding model implemented using sentence-transformers
"""
import logging
import numpy as np
from typing import List, Union
from . import BaseEmbeddingModel

class SentenceTransformersModel(BaseEmbeddingModel):
    """Embedding model based on sentence-transformers"""
    
    def __init__(self, model_name: str):
        """Initialize the model
        
        Args:
            model_name: Model name or path
        """
        super().__init__(model_name)
        self.logger = logging.getLogger("embeddings.sentence_transformers")
        
        try:
            # Import sentence-transformers
            from sentence_transformers import SentenceTransformer
            
            # Load the model
            self.logger.info(f"Loading sentence-transformers model: {model_name}")
            self.model = SentenceTransformer(model_name)
            
            # Get the embedding dimension
            self.embedding_dim = self.model.get_sentence_embedding_dimension()
            self.logger.info(f"Model loaded successfully, embedding dimension: {self.embedding_dim}")
        
        except ImportError:
            self.logger.error("Unable to import sentence_transformers module, please use pip install sentence-transformers to install")
            raise
        except Exception as e:
            self.logger.error(f"Failed to load sentence-transformers model: {e}")
            raise
    
    def get_embedding(self, text: str) -> List[float]:
        """Get the text embedding vector
        
        Args:
            text: Input text
            
        Returns:
            Embedding vector
        """
        if not text or not isinstance(text, str):
            self.logger.warning(f"Invalid input text: {text}")
            # Return zero vector
            return [0.0] * self.embedding_dim
        
        try:
            # Use the model to generate the embedding
            embedding = self.model.encode(text, convert_to_numpy=True)
            
            # If the result is a numpy array, convert it to a Python list
            if isinstance(embedding, np.ndarray):
                return embedding.tolist()
            return embedding
        
        except Exception as e:
            self.logger.error(f"Failed to generate embedding vector: {e}")
            # Return zero vector when an error occurs
            return [0.0] * self.embedding_dim
    
    def get_batch_embeddings(self, texts: List[str]) -> List[List[float]]:
        """Get the text embedding vectors in batch
        
        Args:
            texts: List of input texts
            
        Returns:
            List of embedding vectors
        """
        if not texts:
            return []
        
        try:
            # Use the model to generate the embeddings in batch
            embeddings = self.model.encode(texts, convert_to_numpy=True)
            
            # Convert to Python list
            return embeddings.tolist()
        
        except Exception as e:
            self.logger.error(f"Failed to generate batch embedding vectors: {e}")
            # Return a list of zero vectors when an error occurs
            return [[0.0] * self.embedding_dim for _ in range(len(texts))] 
"""
Embedding model implemented using OpenAI API
"""
import logging
import os
from typing import List, Optional, Dict, Any, TYPE_CHECKING
from . import BaseEmbeddingModel

# Type hint
if TYPE_CHECKING:
    from openai import OpenAI

class OpenAIEmbeddingModel(BaseEmbeddingModel):
    """Embedding model based on OpenAI API"""
    
    # Model dimension reference table
    MODEL_DIMENSIONS = {
        "text-embedding-3-small": 1536,
        "text-embedding-3-large": 3072,
        "text-embedding-ada-002": 1536,
    }
    
    client: "OpenAI"  # Type hint
    
    def __init__(self, model_name: str = "text-embedding-3-small"):
        """Initialize the model
        
        Args:
            model_name: Model name, defaults to "text-embedding-3-small"
        """
        super().__init__(model_name)
        self.logger = logging.getLogger("embeddings.openai")
        
        # Set the embedding dimension
        self.embedding_dim = self.MODEL_DIMENSIONS.get(model_name, 1536)
        
        try:
            # Import OpenAI client - add type hint
            from openai import OpenAI as OpenAIClient
            global OpenAI
            OpenAI = OpenAIClient
            
            # Get API key
            api_key = os.environ.get("OPENAI_API_KEY")
            if not api_key:
                self.logger.warning("OPENAI_API_KEY environment variable not found, embedding function may not be available")
            
            # Create client
            self.client = OpenAI(api_key=api_key)
            self.logger.info(f"OpenAI client initialized successfully, using model: {model_name}, dimension: {self.embedding_dim}")
            
        except ImportError:
            self.logger.error("Unable to import openai module, please use pip install openai to install")
            raise
        except Exception as e:
            self.logger.error(f"Failed to initialize OpenAI client: {e}")
            raise
    
    def get_embedding(self, text: str) -> List[float]:
        """Get text embedding vector
        
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
            # Use OpenAI API to generate embedding
            response = self.client.embeddings.create(
                input=text,
                model=self.model_name
            )
            
            # Extract embedding vector
            embedding = response.data[0].embedding
            
            return embedding
        
        except Exception as e:
            self.logger.error(f"Failed to generate OpenAI embedding vector: {e}")
            # Return zero vector when an error occurs
            return [0.0] * self.embedding_dim
    
    def get_batch_embeddings(self, texts: List[str]) -> List[List[float]]:
        """Get text embedding vectors in batch
        
        Args:
            texts: List of input texts
            
        Returns:
            List of embedding vectors
        """
        if not texts:
            return []
        
        try:
            # Use OpenAI API to generate embedding in batch
            response = self.client.embeddings.create(
                input=texts,
                model=self.model_name
            )
            
            # Extract embedding vectors and sort in input order
            sorted_embeddings = sorted(response.data, key=lambda x: x.index)
            embeddings = [item.embedding for item in sorted_embeddings]
            
            return embeddings
        
        except Exception as e:
            self.logger.error(f"Failed to generate OpenAI embedding vectors in batch: {e}")
            # Return zero vector list when an error occurs
            return [[0.0] * self.embedding_dim for _ in range(len(texts))]

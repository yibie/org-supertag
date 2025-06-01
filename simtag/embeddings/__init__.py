"""
Embedding Model Management Module
Provides a unified interface for various embedding models
"""
import logging
from typing import Optional, Dict, Any

logger = logging.getLogger("embeddings")

# Global model cache
_embedding_models = {}

def get_embedding_model(model_name: str = None) -> Optional["BaseEmbeddingModel"]:
    """
    Get embedding model by name, attempt to load if not exists
    
    Args:
        model_name: Model name, defaults to None to load default model
        
    Returns:
        Embedding model instance, returns None if loading fails
    """
    # Use default model name
    if model_name is None:
        model_name = "sentence-transformers/all-MiniLM-L6-v2"
    
    # Check if model is already loaded
    if model_name in _embedding_models:
        logger.info(f"Using cached embedding model: {model_name}")
        return _embedding_models[model_name]
    
    # Try loading different model types
    model = None
    
    # Try different model implementations by priority
    if model is None:
        try:
            from .sentence_transformers import SentenceTransformersModel
            model = SentenceTransformersModel(model_name)
            logger.info(f"Successfully loaded SentenceTransformers model: {model_name}")
        except ImportError:
            logger.warning(f"Unable to import sentence_transformers module, trying other model types")
        except Exception as e:
            logger.error(f"Failed to load SentenceTransformers model: {e}")
    
    # Try loading OpenAI model
    if model is None:
        try:
            from .openai_embeddings import OpenAIEmbeddingModel
            if model_name.startswith("openai:"):
                actual_model = model_name.split(":", 1)[1]
                model = OpenAIEmbeddingModel(actual_model)
                logger.info(f"Successfully loaded OpenAI embedding model: {actual_model}")
        except ImportError:
            logger.warning(f"Unable to import openai module")
        except Exception as e:
            logger.error(f"Failed to load OpenAI model: {e}")
    
    # Try loading Hugging Face model
    if model is None:
        try:
            from .huggingface import HuggingFaceEmbeddingModel
            model = HuggingFaceEmbeddingModel(model_name)
            logger.info(f"Successfully loaded HuggingFace model: {model_name}")
        except ImportError:
            logger.warning(f"Unable to import transformers module")
        except Exception as e:
            logger.error(f"Failed to load HuggingFace model: {e}")
    
    # If all methods fail, log error
    if model is None:
        logger.error(f"All embedding model loading attempts failed: {model_name}")
        return None
    
    # Cache model for future use
    _embedding_models[model_name] = model
    return model


class BaseEmbeddingModel:
    """Base class for embedding models"""
    
    def __init__(self, model_name: str):
        """Initialize model
        
        Args:
            model_name: Model name or path
        """
        self.model_name = model_name
        self.model = None
        self.embedding_dim = 0
        self.logger = logging.getLogger(f"embeddings.{self.__class__.__name__}")
    
    def get_embedding(self, text: str) -> list:
        """Get text embedding vector
        
        Args:
            text: Input text
            
        Returns:
            Embedding vector
        """
        raise NotImplementedError("Subclasses must implement this method")
    
    def get_batch_embeddings(self, texts: list) -> list:
        """Get batch text embedding vectors
        
        Args:
            texts: List of input texts
            
        Returns:
            List of embedding vectors
        """
        return [self.get_embedding(text) for text in texts]
    
    def get_embedding_dimension(self) -> int:
        """Get embedding vector dimension
        
        Returns:
            Embedding vector dimension
        """
        return self.embedding_dim
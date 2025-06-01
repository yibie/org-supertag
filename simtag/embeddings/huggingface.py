"""
Embedding model implemented using Hugging Face transformers
"""
import logging
import torch
import numpy as np
from typing import List, Optional, Dict, Any
from . import BaseEmbeddingModel

class HuggingFaceEmbeddingModel(BaseEmbeddingModel):
    """Embedding model based on Hugging Face transformers"""
    
    def __init__(self, model_name: str):
        """Initialize the model
        
        Args:
            model_name: Model name or path
        """
        super().__init__(model_name)
        self.logger = logging.getLogger("embeddings.huggingface")
        
        try:
            # Import necessary libraries
            from transformers import AutoTokenizer, AutoModel
            import torch
            
            # Record device information
            self.device = "cuda" if torch.cuda.is_available() else "cpu"
            self.logger.info(f"Using device: {self.device}")
            
            # Load tokenizer and model
            self.logger.info(f"Loading Hugging Face model: {model_name}")
            self.tokenizer = AutoTokenizer.from_pretrained(model_name)
            self.model = AutoModel.from_pretrained(model_name).to(self.device)
            
            # Get model information
            self.embedding_dim = self.model.config.hidden_size
            self.logger.info(f"Model loaded successfully, embedding dimension: {self.embedding_dim}")
            
            # Save torch module as instance variable for easy access
            self.torch = torch
            
        except ImportError:
            self.logger.error("Unable to import transformers or torch module, please use pip install transformers torch to install")
            raise
        except Exception as e:
            self.logger.error(f"Failed to load Hugging Face model: {e}")
            raise
    
    def _mean_pooling(self, model_output, attention_mask):
        """Perform mean pooling on the model output to get the sentence embedding
        
        Args:
            model_output: Model output
            attention_mask: Attention mask
            
        Returns:
            Sentence embedding
        """
        # First, change the shape of token_embeddings from [batch_size, seq_len, hidden_size] to [batch_size, seq_len, hidden_size]
        token_embeddings = model_output[0]
        
        # Expand the attention mask to the same shape as token_embeddings
        input_mask_expanded = attention_mask.unsqueeze(-1).expand(token_embeddings.size()).float()
        
        # Perform weighted sum on token_embeddings
        sum_embeddings = torch.sum(token_embeddings * input_mask_expanded, 1)
        sum_mask = torch.clamp(input_mask_expanded.sum(1), min=1e-9)
        
        # Divide by the sum of the mask to get the mean
        return sum_embeddings / sum_mask
    
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
            # Use torch.no_grad() to avoid calculating gradients
            with self.torch.no_grad():
                # Tokenize the text
                encoded_input = self.tokenizer(text, padding=True, truncation=True, return_tensors='pt')
                
                # Move the input to the correct device
                encoded_input = {k: v.to(self.device) for k, v in encoded_input.items()}
                
                # Get the model output
                outputs = self.model(**encoded_input)
                
                # Perform mean pooling on the output to get the sentence embedding
                sentence_embeddings = self._mean_pooling(outputs, encoded_input['attention_mask'])
                
                # Normalize the embedding
                sentence_embeddings = self.torch.nn.functional.normalize(sentence_embeddings, p=2, dim=1)
                
                # Convert to numpy array, then to Python list
                embedding = sentence_embeddings[0].cpu().numpy().tolist()
                
                return embedding
            
        except Exception as e:
            self.logger.error(f"Failed to generate embedding vector: {e}")
            # Return zero vector when an error occurs
            return [0.0] * self.embedding_dim
    
    def get_batch_embeddings(self, texts: List[str]) -> List[List[float]]:
        """Get the text embedding vectors in batches
        
        Args:
            texts: List of input texts
            
        Returns:
            List of embedding vectors
        """
        if not texts:
            return []
        
        try:
            # Use torch.no_grad() to avoid calculating gradients
            with self.torch.no_grad():
                # Tokenize the texts
                encoded_input = self.tokenizer(texts, padding=True, truncation=True, return_tensors='pt')
                
                # Move the input to the correct device
                encoded_input = {k: v.to(self.device) for k, v in encoded_input.items()}
                
                # Get the model output
                outputs = self.model(**encoded_input)
                
                # Perform mean pooling on the output to get the sentence embeddings
                sentence_embeddings = self._mean_pooling(outputs, encoded_input['attention_mask'])
                
                # Normalize the embeddings
                sentence_embeddings = self.torch.nn.functional.normalize(sentence_embeddings, p=2, dim=1)
                
                # Convert to numpy array, then to Python list
                embeddings = sentence_embeddings.cpu().numpy().tolist()
                
                return embeddings
            
        except Exception as e:
            self.logger.error(f"Failed to generate batch embedding vectors: {e}")
            # Return zero vector list when an error occurs
            return [[0.0] * self.embedding_dim for _ in range(len(texts))] 
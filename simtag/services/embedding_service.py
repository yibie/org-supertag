"""
Embedding service that abstracts embedding generation with multiple backend support.
Inspired by MiniRAG's EmbeddingFunc design pattern.
"""

import asyncio
import logging
import hashlib
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Optional, Dict, Any
import numpy as np
import concurrent.futures
import multiprocessing as mp
import os

# Import required dependencies
from ..config import Config
from ..utils.text_processing import prepare_node_text_for_embedding

# Forward declaration to avoid circular imports
from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from ..core.graph_service import GraphService

# LlamaCpp backend import
try:
    from .llama_cpp_embedding_service import LlamaCppEmbeddingService
    LLAMA_CPP_AVAILABLE = True
except ImportError:
    LLAMA_CPP_AVAILABLE = False

logger = logging.getLogger(__name__)

@dataclass
class EmbeddingResult:
    """Result wrapper containing embedding vector and metadata"""
    success: bool
    embedding: Optional[List[float]] = None
    error_message: Optional[str] = None
    model_used: Optional[str] = None
    backend_used: Optional[str] = None
    dimension: Optional[int] = None
    processing_time: Optional[float] = None

class EmbeddingBackend(ABC):
    """Abstract base class for embedding backends"""
    
    @property
    @abstractmethod
    def backend_name(self) -> str:
        pass
    
    @property
    @abstractmethod
    def default_dimension(self) -> int:
        pass
    
    @abstractmethod
    async def get_embedding(self, text: str, model: Optional[str] = None) -> EmbeddingResult:
        pass
    
    @abstractmethod
    async def get_embeddings_batch(self, texts: List[str], model: Optional[str] = None) -> List[EmbeddingResult]:
        pass

class OllamaEmbeddingBackend(EmbeddingBackend):
    """Ollama Embedding Backend"""
    
    def __init__(self, base_url: str = "http://localhost:11434", 
                 default_model: str = "nomic-embed-text", timeout: int = 300):
        self.base_url = base_url
        self.default_model = default_model
        self.timeout = timeout
        
    @property
    def backend_name(self) -> str:
        return "ollama"
    
    @property 
    def default_dimension(self) -> int:
        return 768  # Default dimension for nomic-embed-text
        
    async def get_embedding(self, text: str, model: Optional[str] = None) -> EmbeddingResult:
        start_time = time.time()
        target_model = model or self.default_model
        
        # Content validation
        if not text or not text.strip():
            return EmbeddingResult(
                success=False,
                error_message="Empty or whitespace-only text provided",
                backend_used=self.backend_name
            )
        
        try:
            # Use requests for synchronous calls to avoid httpx issues
            import requests
            
            api_url = f"{self.base_url}/api/embeddings"
            payload = {
                "model": target_model,
                "prompt": text.strip()
            }
            
            response = await asyncio.to_thread(
                requests.post,
                api_url,
                json=payload,
                headers={"Content-Type": "application/json"},
                timeout=self.timeout
            )
            
            response.raise_for_status()
            response_data = response.json()
            
            embedding = response_data.get("embedding")
            if embedding and isinstance(embedding, list):
                processing_time = time.time() - start_time
                return EmbeddingResult(
                    success=True,
                    embedding=embedding,
                    model_used=target_model,
                    backend_used=self.backend_name,
                    dimension=len(embedding),
                    processing_time=processing_time
                )
            else:
                return EmbeddingResult(
                    success=False,
                    error_message=f"Invalid embedding response from {target_model}",
                    model_used=target_model,
                    backend_used=self.backend_name
                )
                
        except Exception as e:
            logger.error(f"Ollama embedding error for model {target_model}: {e}")
            return EmbeddingResult(
                success=False,
                error_message=str(e),
                model_used=target_model,
                backend_used=self.backend_name
            )
    
    async def get_embeddings_batch(self, texts: List[str], model: Optional[str] = None) -> List[EmbeddingResult]:
        """Batch processing, calling get_embedding for each text (Ollama API doesn't directly support batch)"""
        # For Ollama, use async concurrency instead of multiprocessing (network calls)
        tasks = [self.get_embedding(text, model) for text in texts]
        return await asyncio.gather(*tasks)

class LocalEmbeddingBackend(EmbeddingBackend):
    """Local Embedding Backend (using Sentence Transformers etc.)"""
    
    def __init__(self, model_name: str = "sentence-transformers/all-MiniLM-L6-v2"):
        self.model_name = model_name
        self._model = None
        self._dimension = None
        
    @property
    def backend_name(self) -> str:
        return "local"
        
    @property
    def default_dimension(self) -> int:
        return self._dimension or 384  # Default dimension for all-MiniLM-L6-v2
        
    async def _load_model(self):
        """Lazy load the model"""
        if self._model is None:
            try:
                from sentence_transformers import SentenceTransformer
                self._model = await asyncio.to_thread(SentenceTransformer, self.model_name)
                # Get actual dimension
                test_embedding = await asyncio.to_thread(self._model.encode, ["test"])
                self._dimension = test_embedding.shape[1]
                logger.info(f"Loaded local embedding model {self.model_name} with dimension {self._dimension}")
            except ImportError:
                raise ImportError("sentence-transformers package required for local embedding backend")
            except Exception as e:
                raise Exception(f"Failed to load model {self.model_name}: {e}")
                
    async def get_embedding(self, text: str, model: Optional[str] = None) -> EmbeddingResult:
        start_time = time.time()
        
        if not text or not text.strip():
            return EmbeddingResult(
                success=False,
                error_message="Empty or whitespace-only text provided",
                backend_used=self.backend_name
            )
            
        try:
            await self._load_model()
            embedding = await asyncio.to_thread(self._model.encode, text.strip())
            processing_time = time.time() - start_time
            
            return EmbeddingResult(
                success=True,
                embedding=embedding.tolist(),
                model_used=self.model_name,
                backend_used=self.backend_name,
                dimension=len(embedding),
                processing_time=processing_time
            )
        except Exception as e:
            logger.error(f"Local embedding error: {e}")
            return EmbeddingResult(
                success=False,
                error_message=str(e),
                model_used=self.model_name,
                backend_used=self.backend_name
            )
    
    async def get_embeddings_batch(self, texts: List[str], model: Optional[str] = None) -> List[EmbeddingResult]:
        """Batch processing (local models support multi-core)"""
        start_time = time.time()
        
        # Filter empty texts
        valid_texts = []
        valid_indices = []
        for i, text in enumerate(texts):
            if text and text.strip():
                valid_texts.append(text.strip())
                valid_indices.append(i)
        
        if not valid_texts:
            return [EmbeddingResult(success=False, error_message="Empty or whitespace-only text provided", backend_used=self.backend_name) 
                   for _ in texts]
        
        try:
            await self._load_model()
            
            # Use multi-core processing for large batches
            if len(valid_texts) > 32:
                embeddings = await self._process_batch_multicore(valid_texts)
            else:
                embeddings = await asyncio.to_thread(self._model.encode, valid_texts)
                
            processing_time = time.time() - start_time
            
            results = [EmbeddingResult(success=False, error_message="Empty or whitespace-only text provided", backend_used=self.backend_name) 
                      for _ in texts]
            
            for i, embedding in enumerate(embeddings):
                original_index = valid_indices[i]
                results[original_index] = EmbeddingResult(
                    success=True,
                    embedding=embedding.tolist(),
                    model_used=self.model_name,
                    backend_used=self.backend_name,
                    dimension=len(embedding),
                    processing_time=processing_time / len(valid_texts)
                )
            
            return results
            
        except Exception as e:
            logger.error(f"Local batch embedding error: {e}")
            return [EmbeddingResult(success=False, error_message=str(e), model_used=self.model_name, backend_used=self.backend_name) 
                   for _ in texts]
    
    async def _process_batch_multicore(self, texts: List[str]) -> np.ndarray:
        """Process large batches of text embeddings using multiple cores"""
        cpu_count = os.cpu_count() or 4
        max_workers = min(cpu_count - 2, 12)  # Optimized for M4 Max
        
        chunk_size = max(len(texts) // max_workers, 16)
        text_chunks = [texts[i:i + chunk_size] for i in range(0, len(texts), chunk_size)]
        
        def process_chunk(chunk):
            """Process a chunk of text in a separate process"""
            try:
                from sentence_transformers import SentenceTransformer
                model = SentenceTransformer(self.model_name)
                return model.encode(chunk)
            except Exception as e:
                logger.error(f"Multicore embedding chunk failed: {e}")
                return np.array([])
        
        with concurrent.futures.ProcessPoolExecutor(
            max_workers=max_workers,
            mp_context=mp.get_context('spawn')
        ) as executor:
            # Submit all tasks
            futures = [executor.submit(process_chunk, chunk) for chunk in text_chunks]
            
            # Collect results
            chunk_results = []
            for future in concurrent.futures.as_completed(futures):
                try:
                    result = future.result()
                    if len(result) > 0:
                        chunk_results.append(result)
                except Exception as e:
                    logger.error(f"Multicore embedding future failed: {e}")
        
        # Concatenate all results
        if chunk_results:
            return np.concatenate(chunk_results)
        else:
            # fallback to single thread
            logger.warning("Multicore processing failed, falling back to single thread")
            return await asyncio.to_thread(self._model.encode, texts)

class EmbeddingService:
    """Service layer to manage all embedding backends"""

    def __init__(self, config: Config, graph_service: "GraphService"):
        self.config = config
        self.graph_service = graph_service
        self.logger = logging.getLogger(__name__)
        
        # Initialize backends configuration from config.embedding
        self.backends_config = {
            "ollama": self.config.embedding.ollama,
            "llama_cpp": self.config.embedding.llama_cpp,
            "local": getattr(self.config.embedding, 'local', {})
        }
        self.provider_name = self.config.embedding.provider
        
        # Initialize backends storage and cache
        self.backends = {}
        self.cache = {} if self.config.embedding.use_cache else None
        
        # Initialize backends
        self._init_backends()

    def _init_backends(self):
        """Initializes all available backends based on configuration"""
        # Always try to initialize ollama if configured
        ollama_config = self.backends_config.get("ollama", {})
        if ollama_config:
            self.backends["ollama"] = OllamaEmbeddingBackend(
                base_url=ollama_config.get("base_url", "http://localhost:11434"),
                default_model=ollama_config.get("model_name", "nomic-embed-text"),
                timeout=ollama_config.get("timeout", 300)
            )
            logger.info("Initialized 'ollama' embedding backend.")

        # Try to initialize local if configured
        local_config = self.backends_config.get("local", {})
        if local_config:
            self.backends["local"] = LocalEmbeddingBackend(
                model_name=local_config.get("model_name", "sentence-transformers/all-MiniLM-L6-v2")
            )
            logger.info("Initialized 'local' embedding backend.")

        # Try to initialize llama_cpp if configured and available
        llama_cpp_config = self.backends_config.get("llama_cpp", {})
        if llama_cpp_config and LLAMA_CPP_AVAILABLE:
            self.backends["llama_cpp"] = LlamaCppEmbeddingService(
                model_path=llama_cpp_config.get("model_path"),
                binary_path=llama_cpp_config.get("binary_path", "llama-embedding"),
                pooling=llama_cpp_config.get("pooling", "mean"),
                n_threads=llama_cpp_config.get("n_threads"),
                n_ctx=llama_cpp_config.get("n_ctx", 4096),
                config_dict=llama_cpp_config
            )
            logger.info("Initialized 'llama_cpp' embedding backend directly.")
        elif llama_cpp_config:
            logger.warning("Llama.cpp backend configured but not available. Skipping.")

        # Set the primary backend based on the provider name from config
        self.primary_backend = self.provider_name
        if self.primary_backend not in self.backends:
            logger.warning(
                f"Primary backend '{self.primary_backend}' not available. "
                f"Falling back to the first available backend."
            )
            if self.backends:
                self.primary_backend = list(self.backends.keys())[0]
            else:
                self.primary_backend = None
                logger.error("No embedding backends are available or configured.")

    def _get_cache_key(self, text: str, backend: str, model: Optional[str] = None) -> str:
        """Generates a cache key"""
        content = f"{backend}:{model}:{text}"
        return hashlib.md5(content.encode('utf-8')).hexdigest()
        
    def _get_from_cache(self, cache_key: str) -> Optional[EmbeddingResult]:
        """Retrieves result from cache"""
        if not self.cache:
            return None
            
        cached = self.cache.get(cache_key)
        cache_ttl = 3600  # Default cache TTL in seconds
        if cached and time.time() - cached['timestamp'] < cache_ttl:
            logger.debug(f"Cache hit for key: {cache_key[:16]}...")
            return cached['result']
        return None
        
    def _save_to_cache(self, cache_key: str, result: EmbeddingResult):
        """Saves result to cache"""
        if self.cache and result.success:
            self.cache[cache_key] = {
                'result': result,
                'timestamp': time.time()
            }
            
    async def get_embedding(self, text: str, 
                          backend: Optional[str] = None, 
                          model: Optional[str] = None) -> EmbeddingResult:
        """Gets embedding for a single text"""
        # Pre-check content
        if not text or not text.strip():
            return EmbeddingResult(
                success=False,
                error_message="Empty or whitespace-only text provided",
                backend_used="none"
            )
        
        primary_backend = self.provider_name
        fallback_backends = ["ollama", "llama_cpp", "local"]  # Try all available backends as fallback
        max_retries = 2
        
        backends_to_try = [backend] if backend else [primary_backend] + fallback_backends
        
        for backend_name in backends_to_try:
            if backend_name not in self.backends:
                logger.warning(f"Backend {backend_name} not available, skipping")
                continue
                
            # Check cache
            cache_key = self._get_cache_key(text, backend_name, model)
            cached_result = self._get_from_cache(cache_key)
            if cached_result:
                return cached_result
            
            backend = self.backends[backend_name]
            
            for attempt in range(max_retries + 1):
                try:
                    result = await backend.get_embedding(text, model)
                    if result.success:
                        self._save_to_cache(cache_key, result)
                        logger.debug(f"Successfully generated embedding using {backend_name} "
                                   f"(attempt {attempt + 1}, dim: {result.dimension})")
                        return result
                    else:
                        logger.warning(f"Embedding failed with {backend_name}: {result.error_message}")
                        break  # Do not retry on logical errors
                        
                except Exception as e:
                    logger.error(f"Embedding attempt {attempt + 1} failed with {backend_name}: {e}")
                    if attempt < max_retries:
                        await asyncio.sleep(2 ** attempt)  # Exponential backoff
                    
        # All backends failed
        return EmbeddingResult(
            success=False,
            error_message="All embedding backends failed",
            backend_used="none"
        )
    
    async def get_embeddings_batch(self, texts: List[str],
                                 backend: Optional[str] = None,
                                 model: Optional[str] = None) -> List[EmbeddingResult]:
        """Gets embeddings for a batch of texts"""
        if not texts:
            return []
            
        batch_size = 32  # Default batch size
        
        # Process in batches
        results = []
        for i in range(0, len(texts), batch_size):
            batch = texts[i:i + batch_size]
            batch_results = await self._process_batch(batch, backend, model)
            results.extend(batch_results)
            
        return results
    
    async def _process_batch(self, texts: List[str],
                           backend: Optional[str] = None,
                           model: Optional[str] = None) -> List[EmbeddingResult]:
        """Processes a single batch"""
        primary_backend = self.provider_name
        fallback_backends = ["ollama", "llama_cpp", "local"]  # Try all available backends as fallback
        
        backends_to_try = [backend] if backend else [primary_backend] + fallback_backends
        
        for backend_name in backends_to_try:
            if backend_name not in self.backends:
                continue
                
            backend = self.backends[backend_name]
            
            try:
                results = await backend.get_embeddings_batch(texts, model)
                # Check if any results were successful
                if any(r.success for r in results):
                    logger.debug(f"Batch processing with {backend_name}: "
                               f"{sum(1 for r in results if r.success)}/{len(results)} successful")
                    return results
            except Exception as e:
                logger.error(f"Batch processing failed with {backend_name}: {e}")
                continue
        
        # All backends failed, return failure results
        return [EmbeddingResult(success=False, error_message="All embedding backends failed", backend_used="none") 
               for _ in texts]
    
    def clear_cache(self):
        """Clears the cache"""
        if self.cache:
            self.cache.clear()
            logger.info("Embedding cache cleared")
    
    def get_stats(self) -> Dict[str, Any]:
        """Gets service statistics"""
        return {
            "config": {
                "primary_backend": self.provider_name,
                "cache_enabled": self.config.embedding.use_cache,
                "provider": self.config.embedding.provider,
                "max_input_tokens": self.config.embedding.max_input_tokens
            },
            "backends": list(self.backends.keys()),
            "cache_size": len(self.cache) if self.cache else 0
        }
    
    async def check_dimension_compatibility(self) -> Dict[str, Any]:
        """Check if all backend dimensions are consistent"""
        if not self.backends:
            return {"status": "error", "message": "No backends configured"}
        
        dimensions = {}
        for name, backend in self.backends.items():
            try:
                # Assuming backends have a 'default_dimension' property
                dim = backend.default_dimension
                if dim > 0:
                    dimensions[name] = dim
                else:
                    dimensions[name] = "Unknown"
            except Exception as e:
                dimensions[name] = f"Error: {e}"
        
        # Check for consistency
        unique_dims = {d for d in dimensions.values() if isinstance(d, int) and d > 0}
        
        if len(unique_dims) > 1:
            status = "warning"
            message = f"Inconsistent embedding dimensions found: {dimensions}"
        elif len(unique_dims) == 1:
            status = "success"
            message = f"All backends have a consistent dimension: {unique_dims.pop()}"
        else:
            status = "error"
            message = "Could not determine dimension for any backend."
            
        return {"status": status, "message": message, "dimensions": dimensions} 

    async def get_node_embedding(self, node_data: Dict[str, Any]) -> Optional[np.ndarray]:
        """
        Get the embedding vector of a node
        
        Args:
            node_data: Node data, containing content and title information
        """
        try:
            content = prepare_node_text_for_embedding(node_data)
            if not content:
                self.logger.warning(f"No content for embedding in node {node_data.get('id')}")
                return None

            result = await self.get_embedding(content)
            if result.success and result.embedding:
                return np.array(result.embedding, dtype=np.float32)
            return None
        except Exception as e:
            self.logger.error(f"Error generating node embedding: {e}", exc_info=True)
            return None

    async def get_tag_embedding(self, tag_data: Dict[str, Any]) -> Optional[np.ndarray]:
        """
        Get the embedding vector of a tag
        
        Args:
            tag_data: Tag data, containing name and fields information
        """
        try:
            # Generate embedding using tag name and field definitions
            tag_name = tag_data.get('name') or tag_data.get('title')
            if not tag_name:
                self.logger.warning(f"Tag {tag_data.get('id')} has no name")
                return None

            # Build semantic representation of the tag
            fields = tag_data.get('fields', [])
            field_texts = []
            for field in fields:
                if isinstance(field, dict):
                    field_name = field.get('name')
                    field_type = field.get('type')
                    if field_name and field_type:
                        field_texts.append(f"{field_name} ({field_type})")

            # Combine tag name and field information
            content = tag_name
            if field_texts:
                content += ": " + ", ".join(field_texts)

            result = await self.get_embedding(content)
            if result.success and result.embedding:
                return np.array(result.embedding, dtype=np.float32)
            return None
        except Exception as e:
            self.logger.error(f"Error generating tag embedding: {e}", exc_info=True)
            return None

    def _calculate_average_node_embeddings(self, tag_id: str) -> Optional[np.ndarray]:
        """
        Calculate the semantic vector of a tag by averaging the vectors of its linked nodes
        """
        try:
            # Get linked nodes
            linked_nodes = self.graph_service.get_nodes_linked_to_tag(tag_id)
            if not linked_nodes:
                self.logger.warning(f"Tag '{tag_id}' has no linked nodes.")
                return None

            # Collect node vectors
            embeddings = []
            for node in linked_nodes:
                node_id = node.get('node_id')
                if not node_id:
                    continue
                embedding = self.graph_service.get_entity_embedding_by_id(node_id)
                if embedding is not None and embedding.size > 0:
                    embeddings.append(embedding)

            if not embeddings:
                self.logger.warning(f"No valid embeddings found for nodes linked to tag '{tag_id}'")
                return None

            # Calculate average vector
            return np.mean([np.array(emb) for emb in embeddings], axis=0)
        except Exception as e:
            self.logger.error(f"Error calculating average embeddings for tag '{tag_id}': {e}", exc_info=True)
            return None

    async def _embed_tag_by_name(self, tag_data: Dict[str, Any]) -> Optional[np.ndarray]:
        """Helper to embed a tag by its name and fields."""
        return await self.get_tag_embedding(tag_data)

    async def batch_embed_tags(self, tag_ids: List[str], method: str = 'average') -> None:
        """
        Batch process tag embeddings using a specified method, with concurrency.
        """
        if not tag_ids:
            return

        self.logger.info(f"Starting batch embedding for {len(tag_ids)} tags using method: '{method}'")

        async def _process_single_tag(tag_id: str):
            try:
                tag_data = self.graph_service.get_node_by_id(tag_id)
                if not tag_data:
                    self.logger.warning(f"Tag '{tag_id}' not found in database, skipping.")
                    return

                embedding = None
                effective_method = method

                if method == 'average':
                    linked_nodes = self.graph_service.get_nodes_linked_to_tag(tag_id)
                    if len(linked_nodes) < 3:
                        self.logger.debug(f"Tag '{tag_id}' has < 3 nodes, falling back to 'name' method.")
                        effective_method = 'name'
                    else:
                        embedding = self._calculate_average_node_embeddings(tag_id)

                if effective_method == 'name':
                    embedding = await self._embed_tag_by_name(tag_data)

                if embedding is not None and embedding.size > 0:
                    self.graph_service.upsert_entity_embedding(tag_id, embedding)
                    self.logger.debug(f"Successfully embedded tag '{tag_id}' using method '{effective_method}'")
                else:
                    self.logger.warning(f"Failed to generate embedding for tag '{tag_id}' with method '{effective_method}'.")

            except Exception as e:
                self.logger.error(f"Failed to process tag '{tag_id}' in batch: {e}", exc_info=True)

        # Create concurrent tasks for all tags
        tasks = [_process_single_tag(tag_id) for tag_id in tag_ids]
        await asyncio.gather(*tasks)

        self.logger.info(f"Finished batch embedding for {len(tag_ids)} tags.") 

    async def refresh_stale_tags(self, batch_size: int = 20, method: str = 'average'):
        """
        查找所有 knowledge_status='STALE' 且 type='TAG' 的节点，批量重算嵌入并将状态设为 FRESH。
        """
        stale_tag_ids = []
        # 1. 查询所有 STALE 标签
        conn = self.graph_service._get_connection()
        cursor = conn.cursor()
        cursor.execute("SELECT node_id FROM nodes WHERE type = 'TAG' AND knowledge_status = 'STALE'")
        rows = cursor.fetchall()
        stale_tag_ids = [row[0] for row in rows]
        if not stale_tag_ids:
            self.logger.info("No STALE tags found for refresh.")
            return
        self.logger.info(f"Refreshing {len(stale_tag_ids)} STALE tags...")
        # 2. 分批处理
        for i in range(0, len(stale_tag_ids), batch_size):
            batch = stale_tag_ids[i:i+batch_size]
            await self.batch_embed_tags(batch, method=method)
            # 3. 更新状态为 FRESH
            for tag_id in batch:
                self.graph_service.update_node_status(tag_id, "FRESH")
        self.logger.info("Tag embedding refresh complete.") 
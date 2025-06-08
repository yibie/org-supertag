"""
Embedding service that abstracts embedding generation with multiple backend support.
Inspired by MiniRAG's EmbeddingFunc design pattern.
"""

import asyncio
import logging
import hashlib
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any, Union
import numpy as np
import concurrent.futures
import multiprocessing as mp
import os

# LlamaCpp backend import
try:
    from .llama_cpp_embedding_service import LlamaCppEmbeddingService
    LLAMA_CPP_AVAILABLE = True
except ImportError:
    LLAMA_CPP_AVAILABLE = False

logger = logging.getLogger(__name__)

@dataclass
class EmbeddingResult:
    """结果封装，包含嵌入向量和元数据"""
    success: bool
    embedding: Optional[List[float]] = None
    error_message: Optional[str] = None
    model_used: Optional[str] = None
    backend_used: Optional[str] = None
    dimension: Optional[int] = None
    processing_time: Optional[float] = None

class EmbeddingBackend(ABC):
    """嵌入后端抽象基类"""
    
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
    """Ollama 嵌入后端"""
    
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
        return 768  # nomic-embed-text 的默认维度
        
    async def get_embedding(self, text: str, model: Optional[str] = None) -> EmbeddingResult:
        start_time = time.time()
        target_model = model or self.default_model
        
        # 内容验证
        if not text or not text.strip():
            return EmbeddingResult(
                success=False,
                error_message="Empty or whitespace-only text provided",
                backend_used=self.backend_name
            )
        
        try:
            # 使用 requests 库进行同步调用，避免 httpx 的问题
            import requests
            import json
            
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
        """批量处理，逐个调用（Ollama API 不直接支持批量）"""
        # 对于Ollama，使用异步并发而不是多进程（网络调用）
        tasks = [self.get_embedding(text, model) for text in texts]
        return await asyncio.gather(*tasks)

class LocalEmbeddingBackend(EmbeddingBackend):
    """本地嵌入后端（使用句子转换器等）"""
    
    def __init__(self, model_name: str = "sentence-transformers/all-MiniLM-L6-v2"):
        self.model_name = model_name
        self._model = None
        self._dimension = None
        
    @property
    def backend_name(self) -> str:
        return "local"
        
    @property
    def default_dimension(self) -> int:
        return self._dimension or 384  # all-MiniLM-L6-v2 的默认维度
        
    async def _load_model(self):
        """延迟加载模型"""
        if self._model is None:
            try:
                from sentence_transformers import SentenceTransformer
                self._model = await asyncio.to_thread(SentenceTransformer, self.model_name)
                # 获取实际维度
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
        """批量处理（本地模型支持多核心）"""
        start_time = time.time()
        
        # 过滤空文本
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
            
            # 对于大批量，使用多核心处理
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
        """使用多核心处理大批量文本嵌入"""
        cpu_count = os.cpu_count() or 4
        max_workers = min(cpu_count - 2, 12)  # 为M4 Max优化
        
        chunk_size = max(len(texts) // max_workers, 16)
        text_chunks = [texts[i:i + chunk_size] for i in range(0, len(texts), chunk_size)]
        
        def process_chunk(chunk):
            """在单独进程中处理文本块"""
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
            # 提交所有任务
            futures = [executor.submit(process_chunk, chunk) for chunk in text_chunks]
            
            # 收集结果
            chunk_results = []
            for future in concurrent.futures.as_completed(futures):
                try:
                    result = future.result()
                    if len(result) > 0:
                        chunk_results.append(result)
                except Exception as e:
                    logger.error(f"Multicore embedding future failed: {e}")
        
        # 合并所有结果
        if chunk_results:
            return np.concatenate(chunk_results)
        else:
            # fallback到单线程
            logger.warning("Multicore processing failed, falling back to single thread")
            return await asyncio.to_thread(self._model.encode, texts)

@dataclass
class EmbeddingConfig:
    """嵌入服务配置"""
    primary_backend: str = "llama_cpp"  # 使用llama_cpp作为主要后端
    fallback_backends: List[str] = field(default_factory=lambda: ["ollama"])  # 只使用ollama作为备用后端
    cache_enabled: bool = True
    cache_ttl: int = 3600  # 缓存过期时间（秒）
    max_retries: int = 2
    batch_size: int = 32  # 批量处理大小
    
    # Ollama 配置
    ollama_base_url: str = "http://localhost:11434"
    ollama_model: str = "nomic-embed-text"
    ollama_timeout: int = 300
    
    # 本地模型配置  
    local_model: str = "sentence-transformers/all-MiniLM-L6-v2"
    
    # LlamaCpp 配置
    llama_cpp_model_path: Optional[str] = None  # 如果为 None 则自动查找
    llama_cpp_binary_path: Optional[str] = None  # 如果为 None 则自动查找
    llama_cpp_n_ctx: int = 2048
    llama_cpp_n_threads: Optional[int] = None  # 如果为 None 则使用 CPU 核心数
    llama_cpp_n_gpu_layers: int = 0  # GPU 层数，0 表示纯 CPU

class EmbeddingService:
    """统一的嵌入服务，支持多后端和故障容错"""
    
    def __init__(self, config: EmbeddingConfig):
        self.config = config
        self.backends: Dict[str, EmbeddingBackend] = {}
        self.cache: Dict[str, Dict] = {} if config.cache_enabled else None
        self._init_backends()
        
    def _init_backends(self):
        """初始化所有后端"""
        # Ollama 后端
        if "ollama" in [self.config.primary_backend] + self.config.fallback_backends:
            self.backends["ollama"] = OllamaEmbeddingBackend(
                base_url=self.config.ollama_base_url,
                default_model=self.config.ollama_model,
                timeout=self.config.ollama_timeout
            )
        
        # 本地后端 - 只有在配置中指定时才初始化
        if "local" in [self.config.primary_backend] + self.config.fallback_backends:
            self.backends["local"] = LocalEmbeddingBackend(
                model_name=self.config.local_model
            )
        
        # LlamaCpp 后端
        if "llama_cpp" in [self.config.primary_backend] + self.config.fallback_backends and LLAMA_CPP_AVAILABLE:
            try:
                from .llama_cpp_embedding_service import create_qwen3_embedding_service
                llama_cpp_service = create_qwen3_embedding_service(
                    model_path=self.config.llama_cpp_model_path,
                    binary_path=self.config.llama_cpp_binary_path
                )
                
                # 创建一个适配器来匹配 EmbeddingBackend 接口
                class LlamaCppBackendAdapter(EmbeddingBackend):
                    def __init__(self, service):
                        self.service = service
                    
                    @property
                    def backend_name(self) -> str:
                        return "llama_cpp"
                    
                    @property
                    def default_dimension(self) -> int:
                        return 1024  # Qwen3-Embedding-0.6B 的维度
                    
                    async def get_embedding(self, text: str, model: Optional[str] = None) -> EmbeddingResult:
                        result = await self.service.get_embedding(text)
                        # 转换为兼容的 EmbeddingResult 格式
                        return EmbeddingResult(
                            success=result.success,
                            embedding=result.embedding,
                            error_message=result.error_message,
                            model_used="qwen3-embedding-0.6b",
                            backend_used="llama_cpp",
                            dimension=result.dimension,
                            processing_time=result.processing_time
                        )
                    
                    async def get_embeddings_batch(self, texts: List[str], model: Optional[str] = None) -> List[EmbeddingResult]:
                        results = await self.service.get_embeddings_batch(texts)
                        # 转换为兼容的 EmbeddingResult 格式
                        converted_results = []
                        for result in results:
                            converted_results.append(EmbeddingResult(
                                success=result.success,
                                embedding=result.embedding,
                                error_message=result.error_message,
                                model_used="qwen3-embedding-0.6b",
                                backend_used="llama_cpp",
                                dimension=result.dimension,
                                processing_time=result.processing_time
                            ))
                        return converted_results
                
                self.backends["llama_cpp"] = LlamaCppBackendAdapter(llama_cpp_service)
                logger.info("LlamaCpp backend initialized successfully")
            except (ImportError, FileNotFoundError) as e:
                logger.warning(f"LlamaCpp backend not available: {e}")
            except Exception as e:
                logger.error(f"Failed to initialize LlamaCpp backend: {e}")
        elif "llama_cpp" in [self.config.primary_backend] + self.config.fallback_backends:
            logger.warning("LlamaCpp backend not available (import failed)")
        
    def _get_cache_key(self, text: str, backend: str, model: Optional[str] = None) -> str:
        """生成缓存键"""
        content = f"{backend}:{model}:{text}"
        return hashlib.md5(content.encode('utf-8')).hexdigest()
        
    def _get_from_cache(self, cache_key: str) -> Optional[EmbeddingResult]:
        """从缓存获取结果"""
        if not self.cache:
            return None
            
        cached = self.cache.get(cache_key)
        if cached and time.time() - cached['timestamp'] < self.config.cache_ttl:
            logger.debug(f"Cache hit for key: {cache_key[:16]}...")
            return cached['result']
        return None
        
    def _save_to_cache(self, cache_key: str, result: EmbeddingResult):
        """保存结果到缓存"""
        if self.cache and result.success:
            self.cache[cache_key] = {
                'result': result,
                'timestamp': time.time()
            }
            
    async def get_embedding(self, text: str, 
                          backend: Optional[str] = None, 
                          model: Optional[str] = None) -> EmbeddingResult:
        """获取单个文本的嵌入"""
        # 内容预检查
        if not text or not text.strip():
            return EmbeddingResult(
                success=False,
                error_message="Empty or whitespace-only text provided",
                backend_used="none"
            )
        
        backends_to_try = [backend] if backend else [self.config.primary_backend] + self.config.fallback_backends
        
        for backend_name in backends_to_try:
            if backend_name not in self.backends:
                logger.warning(f"Backend {backend_name} not available, skipping")
                continue
                
            # 检查缓存
            cache_key = self._get_cache_key(text, backend_name, model)
            cached_result = self._get_from_cache(cache_key)
            if cached_result:
                return cached_result
            
            backend = self.backends[backend_name]
            
            for attempt in range(self.config.max_retries + 1):
                try:
                    result = await backend.get_embedding(text, model)
                    if result.success:
                        self._save_to_cache(cache_key, result)
                        logger.debug(f"Successfully generated embedding using {backend_name} "
                                   f"(attempt {attempt + 1}, dim: {result.dimension})")
                        return result
                    else:
                        logger.warning(f"Embedding failed with {backend_name}: {result.error_message}")
                        break  # 不重试逻辑错误
                        
                except Exception as e:
                    logger.error(f"Embedding attempt {attempt + 1} failed with {backend_name}: {e}")
                    if attempt < self.config.max_retries:
                        await asyncio.sleep(2 ** attempt)  # 指数退避
                    
        # 所有后端都失败
        return EmbeddingResult(
            success=False,
            error_message="All embedding backends failed",
            backend_used="none"
        )
    
    async def get_embeddings_batch(self, texts: List[str],
                                 backend: Optional[str] = None,
                                 model: Optional[str] = None) -> List[EmbeddingResult]:
        """批量获取嵌入"""
        if not texts:
            return []
            
        # 分批处理
        results = []
        for i in range(0, len(texts), self.config.batch_size):
            batch = texts[i:i + self.config.batch_size]
            batch_results = await self._process_batch(batch, backend, model)
            results.extend(batch_results)
            
        return results
    
    async def _process_batch(self, texts: List[str],
                           backend: Optional[str] = None,
                           model: Optional[str] = None) -> List[EmbeddingResult]:
        """处理单个批次"""
        backends_to_try = [backend] if backend else [self.config.primary_backend] + self.config.fallback_backends
        
        for backend_name in backends_to_try:
            if backend_name not in self.backends:
                continue
                
            backend = self.backends[backend_name]
            
            try:
                results = await backend.get_embeddings_batch(texts, model)
                # 检查是否有成功的结果
                if any(r.success for r in results):
                    logger.debug(f"Batch processing with {backend_name}: "
                               f"{sum(1 for r in results if r.success)}/{len(results)} successful")
                    return results
            except Exception as e:
                logger.error(f"Batch processing failed with {backend_name}: {e}")
                continue
        
        # 所有后端都失败，返回失败结果
        return [EmbeddingResult(success=False, error_message="All embedding backends failed", backend_used="none") 
               for _ in texts]
    
    def clear_cache(self):
        """清除缓存"""
        if self.cache:
            self.cache.clear()
            logger.info("Embedding cache cleared")
    
    def get_stats(self) -> Dict[str, Any]:
        """获取服务统计信息"""
        return {
            "config": {
                "primary_backend": self.config.primary_backend,
                "fallback_backends": self.config.fallback_backends,
                "cache_enabled": self.config.cache_enabled,
                "max_retries": self.config.max_retries
            },
            "backends": list(self.backends.keys()),
            "cache_size": len(self.cache) if self.cache else 0
        }

# 全局嵌入服务实例
_embedding_service: Optional[EmbeddingService] = None

def get_embedding_service() -> EmbeddingService:
    """获取全局嵌入服务实例"""
    global _embedding_service
    if _embedding_service is None:
        config = EmbeddingConfig()
        _embedding_service = EmbeddingService(config)
    return _embedding_service

def init_embedding_service(config: EmbeddingConfig) -> EmbeddingService:
    """初始化全局嵌入服务"""
    global _embedding_service
    _embedding_service = EmbeddingService(config)
    return _embedding_service 
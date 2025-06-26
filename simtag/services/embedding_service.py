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

class EmbeddingService:
    """统一的嵌入服务，支持多后端和故障容错"""
    
    def __init__(self, config_dict: Dict[str, Any]):
        """
        初始化嵌入服务
        
        Args:
            config_dict: 来自config.py的embedding_config字典
        """
        self.config = config_dict
        self.backends: Dict[str, EmbeddingBackend] = {}
        self.cache: Dict[str, Dict] = {} if config_dict.get("cache_enabled", True) else None
        self._init_backends()
        
    def _init_backends(self):
        """初始化所有后端"""
        primary_backend = self.config.get("primary_backend", "llama_cpp")
        fallback_backends = self.config.get("fallback_backends", ["ollama"])
        
        all_backends = [primary_backend] + fallback_backends
        
        # Ollama 后端
        if "ollama" in all_backends:
            self.backends["ollama"] = OllamaEmbeddingBackend(
                base_url=self.config.get("ollama_base_url", "http://localhost:11434"),
                default_model=self.config.get("ollama_model", "nomic-embed-text"),
                timeout=self.config.get("ollama_timeout", 300)
            )
        
        # 本地后端 - 只有在配置中指定时才初始化
        if "local" in all_backends:
            self.backends["local"] = LocalEmbeddingBackend(
                model_name=self.config.get("local_model", "sentence-transformers/all-MiniLM-L6-v2")
            )
        
        # LlamaCpp 后端
        if "llama_cpp" in all_backends and LLAMA_CPP_AVAILABLE:
            try:
                from .llama_cpp_embedding_service import create_qwen3_embedding_service
                llama_cpp_service = create_qwen3_embedding_service(
                    model_path=self.config.get("llama_cpp_model_path"),
                    binary_path=self.config.get("llama_cpp_binary", "llama-embedding"),
                    config_dict=self.config  # 传入完整的配置字典
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
        elif "llama_cpp" in all_backends:
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
        cache_ttl = self.config.get("cache_ttl", 3600)
        if cached and time.time() - cached['timestamp'] < cache_ttl:
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
        
        primary_backend = self.config.get("primary_backend", "llama_cpp")
        fallback_backends = self.config.get("fallback_backends", ["ollama"])
        max_retries = self.config.get("max_retries", 2)
        
        backends_to_try = [backend] if backend else [primary_backend] + fallback_backends
        
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
                        break  # 不重试逻辑错误
                        
                except Exception as e:
                    logger.error(f"Embedding attempt {attempt + 1} failed with {backend_name}: {e}")
                    if attempt < max_retries:
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
            
        batch_size = self.config.get("batch_size", 32)
        
        # 分批处理
        results = []
        for i in range(0, len(texts), batch_size):
            batch = texts[i:i + batch_size]
            batch_results = await self._process_batch(batch, backend, model)
            results.extend(batch_results)
            
        return results
    
    async def _process_batch(self, texts: List[str],
                           backend: Optional[str] = None,
                           model: Optional[str] = None) -> List[EmbeddingResult]:
        """处理单个批次"""
        primary_backend = self.config.get("primary_backend", "llama_cpp")
        fallback_backends = self.config.get("fallback_backends", ["ollama"])
        
        backends_to_try = [backend] if backend else [primary_backend] + fallback_backends
        
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
                "primary_backend": self.config.get("primary_backend"),
                "fallback_backends": self.config.get("fallback_backends"),
                "cache_enabled": self.config.get("cache_enabled"),
                "max_retries": self.config.get("max_retries")
            },
            "backends": list(self.backends.keys()),
            "cache_size": len(self.cache) if self.cache else 0
        }
    
    async def check_dimension_compatibility(self) -> Dict[str, Any]:
        """检查当前模型与数据库的维度兼容性"""
        try:
            # 检测当前模型维度
            test_result = await self.get_embedding("测试文本")
            if not test_result.success:
                return {
                    "compatible": False,
                    "error": f"无法获取模型嵌入: {test_result.error_message}"
                }
            
            model_dimension = test_result.dimension
            
            # 检查数据库维度
            try:
                from simtag.config import Config
                import sqlite3
                import sqlite_vec
                
                config = Config()
                db_path = config.vector_db_path
                
                if not os.path.exists(db_path):
                    return {
                        "compatible": True,
                        "model_dimension": model_dimension,
                        "db_dimension": None,
                        "message": "数据库不存在，将自动创建"
                    }
                
                conn = sqlite3.connect(db_path)
                conn.enable_load_extension(True)
                sqlite_vec.load(conn)
                cursor = conn.cursor()
                
                # 尝试获取现有向量样本
                cursor.execute("SELECT embedding FROM node_embeddings_vss LIMIT 1")
                row = cursor.fetchone()
                
                if not row:
                    return {
                        "compatible": True,
                        "model_dimension": model_dimension,
                        "db_dimension": None,
                        "message": "数据库为空，可以直接使用"
                    }
                
                # 解析向量维度
                embedding_data = row[0]
                if isinstance(embedding_data, bytes):
                    import numpy as np
                    vector = np.frombuffer(embedding_data, dtype=np.float32)
                    db_dimension = len(vector)
                else:
                    import json
                    vector = json.loads(embedding_data) if isinstance(embedding_data, str) else embedding_data
                    db_dimension = len(vector)
                
                conn.close()
                
                compatible = model_dimension == db_dimension
                return {
                    "compatible": compatible,
                    "model_dimension": model_dimension,
                    "db_dimension": db_dimension,
                    "message": "维度匹配" if compatible else f"维度不匹配: 模型({model_dimension}) vs 数据库({db_dimension})"
                }
                
            except Exception as db_error:
                return {
                    "compatible": False,
                    "model_dimension": model_dimension,
                    "db_dimension": None,
                    "error": f"数据库检查失败: {db_error}"
                }
                
        except Exception as e:
            return {
                "compatible": False,
                "error": f"维度兼容性检查失败: {e}"
            }

    def prepare_node_text_for_embedding(self, node_data, max_total_tokens=280):
        """
        为 org-supertag node 准备向量化文本
        
        策略：标题 + 内容前N字，充分利用 org-mode 的结构化优势
        专门针对 LlamaCpp 的 512 token 限制进行优化
        
        Args:
            node_data: Node 数据字典
            max_total_tokens: 总的最大 token 数，默认400（留出安全余量）
            
        Returns:
            str: 准备好的文本
        """
        text_parts = []
        used_tokens = 0
        
        # Token 估算函数（粗略估算：1个中文字符≈1.5 tokens，1个英文单词≈1 token）
        def estimate_tokens(text):
            chinese_chars = len([c for c in text if '\u4e00' <= c <= '\u9fff'])
            other_chars = len(text) - chinese_chars
            return chinese_chars * 1.5 + other_chars * 0.3
        
        # 1. 优先添加标题（最重要的语义信息）
        title = node_data.get(':title', '').strip()
        if title:
            title_text = f"标题: {title}"
            title_tokens = estimate_tokens(title_text)
            if used_tokens + title_tokens < max_total_tokens:
                text_parts.append(title_text)
                used_tokens += title_tokens
        
        # 2. 添加关键上下文（如果空间允许）
        olp = node_data.get(':olp', [])
        if olp and len(olp) > 0 and used_tokens < max_total_tokens * 0.5:  # 只在token使用率<50%时添加
            context = " > ".join(olp[-2:])  # 只取最近的2级父标题
            context_text = f"上下文: {context}"
            context_tokens = estimate_tokens(context_text)
            if used_tokens + context_tokens < max_total_tokens * 0.7:  # 上下文不超过总量的70%
                text_parts.append(context_text)
                used_tokens += context_tokens
        
        # 3. 添加标签（如果空间允许且标签数量合理）
        tags = node_data.get(':tags', [])
        if tags and len(tags) <= 5 and used_tokens < max_total_tokens * 0.6:
            tags_text = f"标签: {', '.join(tags[:5])}"  # 最多5个标签
            tags_tokens = estimate_tokens(tags_text)
            if used_tokens + tags_tokens < max_total_tokens * 0.8:  # 标签不超过总量的80%
                text_parts.append(tags_text)
                used_tokens += tags_tokens
        
        # 4. 用剩余空间添加内容
        content = node_data.get(':content', '').strip()
        if content:
            remaining_tokens = max_total_tokens - used_tokens - 20  # 保留20 token的安全边界
            if remaining_tokens > 50:  # 至少要有50 token才添加内容
                # 根据剩余token数动态计算内容字符数限制
                max_content_chars = int(remaining_tokens / 1.2)  # 保守估算
                truncated_content = self._smart_truncate(content, max_content_chars)
                content_text = f"内容: {truncated_content}"
                text_parts.append(content_text)
        
        final_text = " | ".join(text_parts)
        
        # 最终检查：确保不超过限制
        final_tokens = estimate_tokens(final_text)
        if final_tokens > max_total_tokens:
            # 如果还是超过，优先保留标题和少量内容
            title_only = node_data.get(':title', '').strip()
            content = node_data.get(':content', '').strip()
            if content:
                remaining_chars = max_total_tokens * 0.6  # 大约60%给内容
                truncated = self._smart_truncate(content, int(remaining_chars))
                final_text = f"标题: {title_only} | 内容: {truncated}"
            else:
                final_text = f"标题: {title_only}"
        
        return final_text
    
    def _estimate_tokens(self, text):
        """
        实用的 token 数量估算
        
        基于实际测试优化，平衡精度和安全性
        
        Args:
            text: 输入文本
            
        Returns:
            float: 估算的 token 数量
        """
        # 基于实际观察调整：中文字符约1.2 tokens，英文单词约0.8 tokens
        chinese_chars = len([c for c in text if '\u4e00' <= c <= '\u9fff'])
        other_chars = len(text) - chinese_chars
        
        # 更贴近实际的估算
        estimated = chinese_chars * 1.2 + other_chars * 0.6
        
        # 适度的安全边界（10%）
        return estimated * 1.1
    
    def _smart_truncate(self, text, max_chars):
        """
        在句子边界智能截断文本
        
        Args:
            text: 原文本
            max_chars: 最大字符数
            
        Returns:
            str: 截断后的文本
        """
        if len(text) <= max_chars:
            return text
        
        # 在最大长度处向前查找句子边界
        truncated = text[:max_chars]
        
        # 查找句号、感叹号、问号等句子结束标记
        sentence_endings = ['。', '！', '？', '.', '!', '?', '\n\n']
        
        best_cut = 0
        for ending in sentence_endings:
            pos = truncated.rfind(ending)
            if pos > max_chars * 0.7:  # 确保截断位置不要太靠前
                best_cut = max(best_cut, pos + 1)
        
        if best_cut > 0:
            return text[:best_cut].strip()
        else:
            # 如果没找到合适的句子边界，就在单词边界截断
            words = truncated.split()
            return ' '.join(words[:-1]) + '...' if len(words) > 1 else truncated + '...'
    
    async def get_node_embedding(self, node_data, max_total_tokens=280):
        """
        为 org-supertag node 获取向量表示
        
        Args:
            node_data: Node 数据字典  
            max_total_tokens: 总的最大 token 数
            
        Returns:
            Dict: 包含向量和 node 元数据的字典
        """
        # 准备文本
        text = self.prepare_node_text_for_embedding(node_data, max_total_tokens)
        
        # 获取向量
        result = await self.get_embedding(text)
        
        # 构建包含 node 元数据的返回结果
        return {
            'embedding_result': result,
            'node_metadata': {
                'node_id': node_data.get('id'),
                'node_title': node_data.get('title'),
                'node_level': node_data.get('level'),
                'text_used': text,
                'text_length': len(text),
                'estimated_tokens': self._estimate_tokens(text),
                'max_tokens_limit': max_total_tokens,
                'original_content_length': len(node_data.get('content', ''))
            }
        }

# 全局嵌入服务实例
_embedding_service: Optional[EmbeddingService] = None

def get_embedding_service() -> EmbeddingService:
    """获取全局嵌入服务实例"""
    global _embedding_service
    if _embedding_service is None:
        # 导入并使用config.py的配置
        from simtag.config import Config
        config = Config()
        _embedding_service = EmbeddingService(config.embedding_config)
    return _embedding_service

def init_embedding_service(config_dict: Dict[str, Any]) -> EmbeddingService:
    """初始化全局嵌入服务"""
    global _embedding_service
    _embedding_service = EmbeddingService(config_dict)
    return _embedding_service 
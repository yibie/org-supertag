"""
内容处理服务：解耦嵌入和NER的统一处理接口

此服务参考 MiniRAG 的设计理念，将内容处理分解为独立的流水线：
1. 内容验证和预处理
2. 实体提取 (NER)
3. 嵌入生成
4. 结果整合

每个步骤都是独立的，失败不会影响其他步骤的执行。
"""

import asyncio
import logging
import time
from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any, Tuple
from enum import Enum

from simtag.services.embedding_service import get_embedding_service, EmbeddingResult
from simtag.core.entity_extractor import LLMEntityExtractor, ExtractedEntity
from simtag.config import Config

logger = logging.getLogger(__name__)

@dataclass
class NERResult:
    """NER 处理结果"""
    success: bool
    entities: List[ExtractedEntity] = field(default_factory=list)
    error_message: Optional[str] = None

class ProcessingMode(Enum):
    """处理模式"""
    EMBEDDING_ONLY = "embedding_only"  # 只处理嵌入
    NER_ONLY = "ner_only"  # 只处理实体提取
    FULL_PROCESSING = "full"  # 完整处理
    MINIMAL = "minimal"  # 最小处理（快速验证）

@dataclass
class ContentItem:
    """内容项"""
    id: str
    text: str
    title: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)
    hash: Optional[str] = None
    content_hash: Optional[str] = None

@dataclass 
class ProcessingResult:
    """处理结果"""
    content_id: str
    success: bool
    
    # 嵌入结果
    embedding_result: Optional[EmbeddingResult] = None
    
    # NER 结果（新格式，用于桥接代码兼容）
    ner_result: Optional[NERResult] = None
    
    # NER 结果（原格式，保持向后兼容）
    extracted_entities: List[ExtractedEntity] = field(default_factory=list)
    
    # 错误信息
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    
    # 性能指标
    processing_time: Optional[float] = None
    mode_used: Optional[ProcessingMode] = None
    
    # 哈希值
    hash: Optional[str] = None  # 添加哈希字段
    content_hash: Optional[str] = None  # 添加内容哈希字段

@dataclass
class ProcessingConfig:
    """处理配置"""
    mode: ProcessingMode = ProcessingMode.FULL_PROCESSING
    
    # 内容验证配置
    min_text_length: int = 1
    max_text_length: int = 50000
    skip_empty_content: bool = True
    
    # 嵌入配置
    embedding_enabled: bool = True
    embedding_backend: Optional[str] = None  # None = 使用默认
    
    # NER 配置  
    ner_enabled: bool = True
    entity_types: Optional[List[str]] = None  # None = 使用默认
    fast_ner_mode: bool = True  # 使用快速NER模式（小模型+单轮）
    
    # 并发配置
    max_concurrent_items: int = 10
    
    # 容错配置
    continue_on_embedding_failure: bool = True
    continue_on_ner_failure: bool = True
    
    # 增量更新配置
    incremental_update: bool = True  # 启用增量更新
    force_update: bool = False  # 强制更新所有内容

class ContentProcessor:
    """内容处理器"""
    
    def __init__(self, config: Config):
        """
        Initializes the ContentProcessor.

        Args:
            config: The main application Config object.
        """
        self.config = config
        self.processing_config = self._create_processing_config_from_main(config)
        
        self.embedding_service = get_embedding_service()
        # Lazily initialize the new extractor
        self.entity_extractor: Optional[LLMEntityExtractor] = None
        
    def _create_processing_config_from_main(self, main_config: Config) -> 'ProcessingConfig':
        """Creates a ProcessingConfig instance from the main Config."""
        # This allows mapping main config settings to the specific processing config
        return ProcessingConfig(
            embedding_enabled=main_config.embedding_config.get("embedding_enabled", True),
            ner_enabled=main_config.use_smart_ner_service,
            # Other fields can be mapped here as needed
        )
    
    async def _init_entity_extractor(self):
        """
        Lazily initializes the entity extractor based on the processing config.
        If fast_ner_mode is enabled, it uses a smaller, faster model configuration.
        """
        if self.entity_extractor is not None:
            return

        if not self.processing_config.ner_enabled:
            logger.info("NER is disabled in the configuration. Skipping entity extractor initialization.")
            return

        try:
            from simtag.services.llm_client import LLMClient
            
            # We now directly use the analysis_config, which is consistent
            # with the NodeProcessor implementation.
            analysis_conf = self.config.analysis_config

            self.entity_extractor = LLMEntityExtractor(
                llm_client=LLMClient(config_dict=self.config.llm_client_config),
                config=analysis_conf
            )
            logger.info(f"Entity extractor initialized using analysis_config.")
        except Exception as e:
            logger.error(f"Failed to initialize entity extractor: {e}", exc_info=True)
            self.entity_extractor = None
    
    def _validate_content(self, item: ContentItem) -> Tuple[bool, List[str]]:
        """验证内容"""
        errors = []
        
        if not item.text and self.processing_config.skip_empty_content:
            errors.append("Empty content skipped")
            return False, errors
            
        if not item.text.strip():
            errors.append("Content contains only whitespace")
            return False, errors
            
        text_length = len(item.text)
        if text_length < self.processing_config.min_text_length:
            errors.append(f"Content too short: {text_length} < {self.processing_config.min_text_length}")
            return False, errors
            
        if text_length > self.processing_config.max_text_length:
            errors.append(f"Content too long: {text_length} > {self.processing_config.max_text_length}")
            return False, errors
            
        return True, []
    
    async def _process_embedding(self, item: ContentItem) -> Optional[EmbeddingResult]:
        """处理嵌入"""
        if not self.processing_config.embedding_enabled:
            logger.debug(f"Embedding disabled for {item.id}")
            return None
            
        try:
            # The incremental update logic has been removed from here.
            # The decision to process a node should be made upstream (e.g., in SyncHandler)
            # based on whether the node is new or has been modified.
            # This processor will now embed any item it receives.
            
            # 合并标题和内容
            text_to_embed = f"{item.title}\n\n{item.text}" if item.title and item.text else item.title or item.text or ""
            
            text_to_embed = text_to_embed.strip()
            
            logger.info(f"Starting embedding generation for {item.id}: {len(text_to_embed)} chars")
            
            result = await self.embedding_service.get_embedding(
                text=text_to_embed,
                backend=self.processing_config.embedding_backend
            )
            
            if result.success:
                logger.info(f"✅ Embedding generated for {item.id}: dim={result.dimension}, backend={result.backend_used}")
            else:
                logger.warning(f"❌ Embedding failed for {item.id}: {result.error_message}")
                
            return result
            
        except Exception as e:
            logger.error(f"Error during embedding generation for {item.id}: {e}", exc_info=True)
            return EmbeddingResult(success=False, error_message=str(e))
    
    async def _process_ner(self, item: ContentItem) -> NERResult:
        """处理实体提取 (NER)"""
        if not self.processing_config.ner_enabled:
            logger.debug(f"NER disabled for {item.id}")
            return NERResult(success=True, entities=[]) # Return success with no entities

        # Lazy initialization of the entity extractor
        if self.entity_extractor is None:
            await self._init_entity_extractor()
            
        if self.entity_extractor is None:
            # Initialization must have failed
            return NERResult(success=False, error_message="Entity extractor could not be initialized.")

        try:
            logger.info(f"Starting NER for {item.id}: {len(item.text)} chars")
            
            # Here we assume existing tags are passed via metadata if needed
            existing_tags = item.metadata.get('existing_tags', [])
            
            # The new extractor expects a simple list of tag names and is now async
            extraction_result = await self.entity_extractor.extract(
                text=item.text,
                existing_entities=existing_tags
            )
            
            # Adapt the new dictionary-based result to the old NERResult structure
            extracted_entities = extraction_result.get("entities", [])

            # For compatibility, we can wrap this in the older ExtractedEntity structure if needed,
            # but for now, we'll assume the dict structure is acceptable downstream.
            
            logger.info(f"✅ NER completed for {item.id}, found {len(extracted_entities)} entities.")
            # Note: The new format is a list of dicts, not ExtractedEntity objects.
            # This part of the code might need further adaptation if the caller
            # strictly expects ExtractedEntity objects. For now, we pass the dicts.
            return NERResult(success=True, entities=extracted_entities)
            
        except Exception as e:
            logger.error(f"Error during NER processing for {item.id}: {e}", exc_info=True)
            return NERResult(success=False, error_message=str(e))

    async def process_single(self, item: ContentItem) -> ProcessingResult:
        """
        串行处理单个内容项（嵌入和NER）。
        """
        start_time = time.time()
        
        is_valid, validation_errors = self._validate_content(item)
        if not is_valid:
            logger.warning(f"Content item {item.id} is invalid: {', '.join(validation_errors)}")
            return ProcessingResult(
                content_id=item.id,
                success=False,
                errors=validation_errors,
                processing_time=time.time() - start_time,
                hash=item.hash,
                content_hash=item.content_hash
            )

        result = ProcessingResult(
            content_id=item.id,
            success=True,
            mode_used=self.processing_config.mode,
            hash=item.hash,
            content_hash=item.content_hash
        )

        # --- 嵌入处理 ---
        if self.processing_config.mode in [ProcessingMode.FULL_PROCESSING, ProcessingMode.EMBEDDING_ONLY]:
            embedding_start = time.time()
            embedding_result = await self._process_embedding(item)
            if embedding_result:
                result.embedding_result = embedding_result
                if not embedding_result.success:
                    result.success = False
                    error_msg = f"Embedding failed: {embedding_result.error_message}"
                    result.errors.append(error_msg)
                    if not self.processing_config.continue_on_embedding_failure:
                        result.processing_time = time.time() - start_time
                        return result
            logger.debug(f"Embedding for {item.id} took {time.time() - embedding_start:.2f}s")

        # --- NER 处理 ---
        if self.processing_config.mode in [ProcessingMode.FULL_PROCESSING, ProcessingMode.NER_ONLY]:
            ner_start = time.time()
            ner_result = await self._process_ner(item)
            result.ner_result = ner_result
            if ner_result.success:
                result.extracted_entities = ner_result.entities
            else:
                result.success = False
                error_msg = f"NER failed: {ner_result.error_message}"
                result.errors.append(error_msg)
                if not self.processing_config.continue_on_ner_failure:
                    result.processing_time = time.time() - start_time
                    return result
            logger.debug(f"NER for {item.id} took {time.time() - ner_start:.2f}s")
        
        result.processing_time = time.time() - start_time
        logger.info(f"Finished processing {item.id} in {result.processing_time:.2f}s. Success: {result.success}")
        return result

    async def process_batch(self, items: List[ContentItem]) -> List[ProcessingResult]:
        """
        以串行方式处理一批内容项。
        
        此方法将迭代项目列表，并按顺序一次处理一个。
        多进程/并行逻辑已被移除以确保稳定性。
        """
        if not items:
            return []

        logger.info(f"Starting serial processing for a batch of {len(items)} items.")
        start_time = time.time()
        
        results = []
        for i, item in enumerate(items):
            logger.info(f"Processing item {i+1}/{len(items)}: {item.id}")
            result = await self.process_single(item)
            results.append(result)

        total_time = time.time() - start_time
        logger.info(f"Finished serial processing of {len(items)} items in {total_time:.2f}s.")
        
        return results
    
    def get_stats(self) -> Dict[str, Any]:
        """获取处理器统计信息"""
        return {
            "config": {
                "mode": self.processing_config.mode.value,
                "embedding_enabled": self.processing_config.embedding_enabled,
                "ner_enabled": self.processing_config.ner_enabled,
                "max_concurrent_items": self.processing_config.max_concurrent_items
            },
            "embedding_service_available": self.embedding_service is not None,
            "entity_extractor_available": self.entity_extractor is not None
        }

# 便利函数
async def process_text(text: str, 
                      content_id: str = "default",
                      title: Optional[str] = None,
                      mode: ProcessingMode = ProcessingMode.FULL_PROCESSING) -> ProcessingResult:
    """便捷函数，用于处理单个文本片段"""
    from simtag.container import Container
    
    container = Container()
    config = container.config()
    content_processor = ContentProcessor(config=config)

    item = ContentItem(id=content_id, text=text, title=title)
    
    # 根据模式调整处理配置
    content_processor.processing_config.mode = mode
    
    return await content_processor.process_single(item)

async def process_node_data(node_id: str,
                           content: str,
                           title: Optional[str] = None,
                           embedding_only: bool = False) -> ProcessingResult:
    """
    处理来自Elisp的节点数据。
    这是一个兼容层，将旧的函数调用适配到新的ContentProcessor。
    """
    from simtag.container import Container

    container = Container()
    config = container.config()
    content_processor = ContentProcessor(config=config)

    item = ContentItem(id=node_id, text=content, title=title)
    
    # 根据 embedding_only 标志调整模式
    mode = ProcessingMode.EMBEDDING_ONLY if embedding_only else ProcessingMode.FULL_PROCESSING
    content_processor.processing_config.mode = mode
    
    return await content_processor.process_single(item) 
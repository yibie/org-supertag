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
from typing import List, Optional, Dict, Any, Tuple, Union
from enum import Enum

from .embedding_service import get_embedding_service, EmbeddingResult
from ..core.entity_extractor import OrgSupertagEntityExtractor, ExtractedEntity, ExtractedRelation

logger = logging.getLogger(__name__)

@dataclass
class NERResult:
    """NER 处理结果"""
    success: bool
    entities: List[ExtractedEntity] = field(default_factory=list)
    relations: List[ExtractedRelation] = field(default_factory=list)
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
    hash: Optional[str] = None  # 添加哈希字段
    content_hash: Optional[str] = None  # 添加内容哈希字段

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
    extracted_relations: List[ExtractedRelation] = field(default_factory=list)
    
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
    
    def __init__(self, config: ProcessingConfig):
        self.config = config
        self.embedding_service = get_embedding_service()
        self.entity_extractor = None  # 延迟初始化
        
    async def _init_entity_extractor(self):
        """延迟初始化实体提取器"""
        if self.entity_extractor is None and self.config.ner_enabled:
            try:
                # 这里需要根据实际的OrgSupertagEntityExtractor构造函数调整
                from simtag.services.llm_client import LLMClient
                from simtag.config import Config
                
                # 获取配置
                config = Config()
                
                # 如果启用快速模式，使用专门的配置
                entity_extractor_config = config.entity_extractor_config.copy()
                if self.config.fast_ner_mode:
                    # 使用用户偏好的配置覆盖
                    entity_extractor_config.update({
                        'llm_model_override': 'qwen2.5:1.5b',  # 使用用户偏好的小模型
                        'max_gleaning_rounds': 2,  # 保留精化轮次以保证质量
                        'max_entities_per_extraction': 15,  # 限制实体数量
                        'max_relations_per_extraction': 10   # 限制关系数量
                    })
                    logger.info("Using fast NER mode with user-preferred configuration (qwen2.5:1.5b, 2 rounds, limited entities)")
                
                llm_client = LLMClient(
                    provider=config.llm_client_config.get('provider', 'ollama'),
                    config=config.llm_client_config
                )
                
                # 创建 async callable 函数，适配 OrgSupertagEntityExtractor 的接口
                async def llm_async_callable(prompt: str, model: Optional[str] = None) -> str:
                    if model:
                        # 如果指定了模型，需要临时设置
                        result = await llm_client.generate(prompt, model=model)
                    else:
                        result = await llm_client.generate(prompt)
                    return result
                
                self.entity_extractor = OrgSupertagEntityExtractor(
                    llm_async_callable=llm_async_callable,
                    entity_extractor_config=entity_extractor_config
                )
                logger.info("Entity extractor initialized")
            except Exception as e:
                logger.error(f"Failed to initialize entity extractor: {e}")
                self.entity_extractor = None
    
    def _validate_content(self, item: ContentItem) -> Tuple[bool, List[str]]:
        """验证内容"""
        errors = []
        
        if not item.text and self.config.skip_empty_content:
            errors.append("Empty content skipped")
            return False, errors
            
        if not item.text.strip():
            errors.append("Content contains only whitespace")
            return False, errors
            
        text_length = len(item.text)
        if text_length < self.config.min_text_length:
            errors.append(f"Content too short: {text_length} < {self.config.min_text_length}")
            return False, errors
            
        if text_length > self.config.max_text_length:
            errors.append(f"Content too long: {text_length} > {self.config.max_text_length}")
            return False, errors
            
        return True, []
    
    async def _process_embedding(self, item: ContentItem) -> Optional[EmbeddingResult]:
        """处理嵌入"""
        if not self.config.embedding_enabled:
            logger.debug(f"Embedding disabled for {item.id}")
            return None
            
        try:
            # 检查增量更新
            if self.config.incremental_update and not self.config.force_update:
                if item.hash and item.content_hash:
                    # 从数据库中获取现有的向量
                    from simtag.core.graph_store import OrgSupertagKnowledgeGraph
                    graph = OrgSupertagKnowledgeGraph()
                    existing_vector = await graph.get_node_vector(item.id)
                    if existing_vector and existing_vector.get('hash') == item.hash:
                        logger.info(f"Skipping embedding for {item.id} - no changes detected")
                        return EmbeddingResult(
                            success=True,
                            embedding=existing_vector.get('vector'),
                            model_used=existing_vector.get('model'),
                            backend_used=existing_vector.get('backend'),
                            dimension=len(existing_vector.get('vector')),
                            processing_time=0
                        )
            
            # 合并标题和内容
            text_to_embed = ""
            if item.title:
                text_to_embed += item.title
            if item.text:
                text_to_embed += "\n\n" + item.text if text_to_embed else item.text
            
            text_to_embed = text_to_embed.strip()
            
            logger.info(f"Starting embedding generation for {item.id}: {len(text_to_embed)} chars")
            
            result = await self.embedding_service.get_embedding(
                text=text_to_embed,
                backend=self.config.embedding_backend
            )
            
            if result.success:
                logger.info(f"✅ Embedding generated for {item.id}: dim={result.dimension}, backend={result.backend_used}")
            else:
                logger.warning(f"❌ Embedding failed for {item.id}: {result.error_message}")
                
            return result
            
        except Exception as e:
            error_msg = f"Embedding processing error for {item.id}: {e}"
            logger.error(error_msg)
            return EmbeddingResult(success=False, error_message=str(e))
    
    async def _process_ner(self, item: ContentItem) -> Tuple[List[ExtractedEntity], List[ExtractedRelation]]:
        """处理实体提取"""
        if not self.config.ner_enabled:
            logger.debug(f"NER disabled for {item.id}")
            return [], []
            
        try:
            # 检查增量更新
            if self.config.incremental_update and not self.config.force_update:
                if item.hash and item.content_hash:
                    # 从数据库中获取现有的实体和关系
                    from simtag.core.graph_store import OrgSupertagKnowledgeGraph
                    graph = OrgSupertagKnowledgeGraph()
                    existing_entities = await graph.get_node_entities(item.id)
                    if existing_entities and existing_entities.get('hash') == item.hash:
                        logger.info(f"Skipping NER for {item.id} - no changes detected")
                        return existing_entities.get('entities', []), existing_entities.get('relations', [])
            
            # 初始化实体提取器
            await self._init_entity_extractor()
            if not self.entity_extractor:
                logger.error(f"Entity extractor not available for {item.id}")
                return [], []
            
            # 提取实体和关系 - 使用 OrgSupertagEntityExtractor 的接口
            text = item.text.strip()
            if not text:
                return [], []
            
            # 从 metadata 中获取 org_tags（如果有的话）
            org_tags = item.metadata.get('tags', [])
            
            # 调用统一的实体提取器
            entities, relations = await self.entity_extractor.extract_from_org_node(
                node_content=text,
                node_id=item.id,
                org_tags=org_tags
            )
            
            logger.info(f"✅ NER completed for {item.id}: {len(entities)} entities, {len(relations)} relations")
            return entities, relations
            
        except Exception as e:
            error_msg = f"NER processing error for {item.id}: {e}"
            logger.error(error_msg)
            return [], []
    
    async def process_single(self, item: ContentItem) -> ProcessingResult:
        """处理单个内容项"""
        start_time = time.time()
        result = ProcessingResult(
            content_id=item.id,
            success=False,
            mode_used=self.config.mode
        )
        
        # 1. 内容验证
        is_valid, validation_errors = self._validate_content(item)
        if not is_valid:
            result.errors.extend(validation_errors)
            result.processing_time = time.time() - start_time
            return result
        
        # 2. 根据模式决定处理任务
        tasks = []
        
        if self.config.mode in [ProcessingMode.EMBEDDING_ONLY, ProcessingMode.FULL_PROCESSING]:
            tasks.append(("embedding", self._process_embedding(item)))
            
        if self.config.mode in [ProcessingMode.NER_ONLY, ProcessingMode.FULL_PROCESSING]:
            tasks.append(("ner", self._process_ner(item)))
        
        if self.config.mode == ProcessingMode.MINIMAL:
            # 最小模式：只做基本验证
            result.success = True
            result.processing_time = time.time() - start_time
            return result
        
        # 3. 并发执行任务
        try:
            task_results = await asyncio.gather(
                *[task[1] for task in tasks],
                return_exceptions=True
            )
            
            # 4. 处理结果
            for i, (task_name, task_result) in enumerate(zip([t[0] for t in tasks], task_results)):
                if isinstance(task_result, Exception):
                    error_msg = f"{task_name} failed: {task_result}"
                    result.errors.append(error_msg)
                    logger.error(f"Task {task_name} failed for {item.id}: {task_result}")
                    continue
                    
                if task_name == "embedding":
                    result.embedding_result = task_result
                    if not task_result or not task_result.success:
                        error_detail = task_result.error_message if task_result else "No result returned"
                        logger.warning(f"🔴 Embedding processing failed for {item.id}: {error_detail}")
                        if not self.config.continue_on_embedding_failure:
                            result.errors.append(f"Embedding failure: {error_detail}")
                            result.processing_time = time.time() - start_time
                            return result
                        else:
                            result.warnings.append(f"Embedding failed ({error_detail}) but processing continued")
                    else:
                        logger.info(f"🟢 Embedding completed successfully for {item.id}")
                            
                elif task_name == "ner":
                    entities, relations = task_result
                    result.extracted_entities = entities
                    result.extracted_relations = relations
                    
                    # 设置新的 ner_result 格式用于桥接代码兼容
                    result.ner_result = NERResult(
                        success=bool(entities or relations),
                        entities=entities,
                        relations=relations
                    )
                    
                    if not entities and not relations:
                        if not self.config.continue_on_ner_failure:
                            result.errors.append("NER failure with continue_on_ner_failure=False")
                            result.processing_time = time.time() - start_time
                            return result
                        else:
                            result.warnings.append("No entities/relations extracted but processing continued")
            
            # 5. 判断总体成功状态
            has_embedding_success = (result.embedding_result and result.embedding_result.success) if self.config.embedding_enabled else True
            has_ner_success = (result.extracted_entities or result.extracted_relations) if self.config.ner_enabled else True
            
            result.success = has_embedding_success or has_ner_success or self.config.mode == ProcessingMode.MINIMAL
            
        except Exception as e:
            result.errors.append(f"Processing failed: {e}")
            logger.error(f"Processing failed for {item.id}: {e}")
        
        # 设置处理时间并记录最终状态
        result.processing_time = time.time() - start_time
        
        # 记录详细状态
        embedding_status = "✅" if (result.embedding_result and result.embedding_result.success) else "❌"
        ner_status = "✅" if (result.extracted_entities or result.extracted_relations) else "❌"
        overall_status = "✅" if result.success else "❌"
        
        logger.info(f"{overall_status} Processing completed for {item.id}: "
                   f"Embedding {embedding_status}, NER {ner_status} "
                   f"({result.processing_time:.3f}s)")
        
        return result
    
    async def process_batch(self, items: List[ContentItem]) -> List[ProcessingResult]:
        """批量处理内容项"""
        if not items:
            return []
            
        logger.info(f"Processing batch of {len(items)} items with mode {self.config.mode}")
        
        # 分批处理以控制并发
        semaphore = asyncio.Semaphore(self.config.max_concurrent_items)
        
        async def process_with_semaphore(item):
            async with semaphore:
                return await self.process_single(item)
        
        results = await asyncio.gather(
            *[process_with_semaphore(item) for item in items],
            return_exceptions=True
        )
        
        # 处理异常结果
        final_results = []
        for i, result in enumerate(results):
            if isinstance(result, Exception):
                error_result = ProcessingResult(
                    content_id=items[i].id,
                    success=False,
                    errors=[f"Processing exception: {result}"]
                )
                final_results.append(error_result)
            else:
                final_results.append(result)
        
        # 统计信息
        successful = sum(1 for r in final_results if r.success)
        logger.info(f"Batch processing completed: {successful}/{len(items)} successful")
        
        return final_results
    
    def get_stats(self) -> Dict[str, Any]:
        """获取处理器统计信息"""
        return {
            "config": {
                "mode": self.config.mode.value,
                "embedding_enabled": self.config.embedding_enabled,
                "ner_enabled": self.config.ner_enabled,
                "max_concurrent_items": self.config.max_concurrent_items
            },
            "embedding_service_available": self.embedding_service is not None,
            "entity_extractor_available": self.entity_extractor is not None
        }

# 便利函数
async def process_text(text: str, 
                      content_id: str = "default",
                      title: Optional[str] = None,
                      mode: ProcessingMode = ProcessingMode.FULL_PROCESSING) -> ProcessingResult:
    """便利函数：处理单个文本"""
    config = ProcessingConfig(mode=mode)
    processor = ContentProcessor(config)
    
    item = ContentItem(
        id=content_id,
        text=text,
        title=title
    )
    
    return await processor.process_single(item)

async def process_node_data(node_id: str,
                           content: str,
                           title: Optional[str] = None,
                           embedding_only: bool = False) -> ProcessingResult:
    """便利函数：处理节点数据（用于替换现有的节点处理逻辑）"""
    mode = ProcessingMode.EMBEDDING_ONLY if embedding_only else ProcessingMode.FULL_PROCESSING
    
    return await process_text(
        text=content,
        content_id=node_id,
        title=title,
        mode=mode
    ) 
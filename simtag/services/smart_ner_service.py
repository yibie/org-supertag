#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import asyncio
import logging
import time
from concurrent.futures import ThreadPoolExecutor
from typing import Dict, List, Any, Optional, Callable, Awaitable, Tuple
from dataclasses import dataclass

from simtag.prompts import create_prompt, ORG_MODE_TAG_EXTRACTION_PROMPT, DEFAULT_ENTITY_TYPES
from simtag.utils.unified_tag_processor import UnifiedTagProcessor, TagResult, NoteResult

logger = logging.getLogger(__name__)

@dataclass
class BatchProcessingResult:
    """批量处理结果"""
    generated_count: int
    failed_count: int
    failed_nodes: List[str]
    processing_time: float
    suggestions: List[Dict[str, Any]]

@dataclass
class NodeProcessingTask:
    """节点处理任务"""
    node_id: str
    title: str
    content: str
    file_path: str
    level: int
    existing_tags: List[str]
    retry_count: int = 0

class SmartNERService:
    """
    A specialized service for high-quality Named Entity Recognition (NER)
    tailored for Org-mode auto-tagging.

    This service uses a dedicated, fine-tuned prompt to extract relevant
    nouns (people, places, concepts) and formats them according to
    Org-mode's conventions (e.g., using underscores).
    
    Features:
    - Single node processing
    - Batch processing with multi-threading
    - Two-stage confidence evaluation
    - Progress tracking and error handling
    """

    def __init__(self,
                 llm_async_callable: Callable[[str], Awaitable[str]],
                 config: Optional[Dict[str, Any]] = None):
        """
        Initializes the SmartNERService.

        Args:
            llm_async_callable: An asynchronous function that takes a prompt
                                string and returns the LLM completion string.
            config: Optional configuration dictionary.
        """
        self.llm_async_callable = llm_async_callable
        self.config = config if config else {}
        self.entity_types = self.config.get('entity_types', DEFAULT_ENTITY_TYPES)
        self.unified_processor = UnifiedTagProcessor()
        
        # 批量处理配置
        self.max_workers = self.config.get('max_workers', 2)  # 2线程
        self.max_retries = self.config.get('max_retries', 3)  # 重试3次
        self.confidence_threshold_first = self.config.get('confidence_threshold_first', 0.3)  # 第一轮阈值
        self.confidence_threshold_final = self.config.get('confidence_threshold_final', 0.6)  # 最终阈值
        self.batch_size = self.config.get('batch_size', 10)  # 批处理大小
        
        # 进度回调
        self.progress_callback = None
        
        logger.info(f"SmartNERService initialized with {self.max_workers} workers, "
                   f"confidence thresholds: {self.confidence_threshold_first}/{self.confidence_threshold_final}")

    def set_progress_callback(self, callback: Optional[Callable[[Dict[str, Any]], None]]):
        """设置进度回调函数"""
        self.progress_callback = callback

    async def get_tags_for_content(
        self,
        content: str,
        node_id: str,
        existing_tags: Optional[List[str]] = None
    ) -> List[TagResult]:
        """
        Extracts high-quality, Org-mode formatted tags from text content.

        Args:
            content: The text content of the org-mode node.
            node_id: The unique identifier for the org-mode node.
            existing_tags: A list of tags already associated with the node.

        Returns:
            A list of TagResult objects.
        """
        logger.debug(f"SmartNERService: Starting tag extraction for node_id: {node_id}")

        if not content.strip():
            logger.debug("Content is empty, skipping extraction.")
            return []

        # 1. Create the specialized prompt for Org-mode
        try:
            prompt = create_prompt(
                contents=[content],
                entity_types=self.entity_types,
                existing_tags_lists=[existing_tags or []],
                prompt_template=ORG_MODE_TAG_EXTRACTION_PROMPT
            )
        except Exception as e:
            logger.error(f"Error creating prompt for node {node_id}: {e}", exc_info=True)
            return []

        # 2. Call the LLM
        try:
            llm_response = await self.llm_async_callable(prompt)
        except Exception as e:
            logger.error(f"LLM call failed in SmartNERService for node {node_id}: {e}")
            return []

        if not llm_response:
            logger.warning(f"LLM returned an empty response for node {node_id}.")
            return []

        logger.debug(f"LLM raw response: {llm_response}")

        # 3. Parse the result using the unified processor
        note_results: List[NoteResult] = self.unified_processor.process_llm_response(
            response_str=llm_response,
            note_ids=[node_id]
        )

        if not note_results or note_results[0].error:
            logger.error(f"Failed to parse LLM response for node {node_id}: {note_results[0].error if note_results else 'No result'}")
            return []

        tags = note_results[0].tags
        logger.info(f"SmartNERService extracted {len(tags)} tags from node {node_id}.")
        return tags

    async def batch_generate_tags(self, nodes_data: List[Dict[str, Any]]) -> BatchProcessingResult:
        """
        批量生成标签建议
        
        Args:
            nodes_data: 节点数据列表，每个元素包含：
                - id: 节点ID
                - title: 节点标题
                - content: 节点内容
                - file_path: 文件路径
                - level: 节点层级
                - existing_tags: 现有标签列表
        
        Returns:
            BatchProcessingResult: 批量处理结果
        """
        start_time = time.time()
        logger.info(f"开始批量处理 {len(nodes_data)} 个节点")
        
        # 转换为处理任务
        tasks = []
        for node_data in nodes_data:
            task = NodeProcessingTask(
                node_id=node_data.get('id', ''),
                title=node_data.get('title', ''),
                content=node_data.get('content', ''),
                file_path=node_data.get('file_path', ''),
                level=node_data.get('level', 1),
                existing_tags=node_data.get('existing_tags', [])
            )
            tasks.append(task)
        
        # 执行批量处理
        result = await self._process_tasks_in_batches(tasks)
        
        processing_time = time.time() - start_time
        result.processing_time = processing_time
        
        logger.info(f"批量处理完成: 生成 {result.generated_count} 个建议, "
                   f"失败 {result.failed_count} 个节点, 耗时 {processing_time:.2f}s")
        
        return result

    async def _process_tasks_in_batches(self, tasks: List[NodeProcessingTask]) -> BatchProcessingResult:
        """分批处理任务"""
        total_tasks = len(tasks)
        generated_count = 0
        failed_nodes = []
        all_suggestions = []
        
        # 分批处理
        for i in range(0, total_tasks, self.batch_size):
            batch = tasks[i:i + self.batch_size]
            batch_results = await self._process_batch_with_threading(batch, i, total_tasks)
            
            generated_count += batch_results['generated_count']
            failed_nodes.extend(batch_results['failed_nodes'])
            all_suggestions.extend(batch_results['suggestions'])
            
            # 更新进度
            if self.progress_callback:
                progress_data = {
                    'current': min(i + self.batch_size, total_tasks),
                    'total': total_tasks,
                    'current_batch_size': len(batch),
                    'generated_in_batch': batch_results['generated_count'],
                    'failed_in_batch': len(batch_results['failed_nodes'])
                }
                try:
                    self.progress_callback(progress_data)
                except Exception as e:
                    logger.warning(f"进度回调失败: {e}")
        
        return BatchProcessingResult(
            generated_count=generated_count,
            failed_count=len(failed_nodes),
            failed_nodes=failed_nodes,
            processing_time=0.0,  # 将在上层设置
            suggestions=all_suggestions
        )

    async def _process_batch_with_threading(self, batch: List[NodeProcessingTask], 
                                          batch_start_idx: int, total_tasks: int) -> Dict[str, Any]:
        """使用多线程处理一批任务"""
        logger.debug(f"处理批次 {batch_start_idx//self.batch_size + 1}, 包含 {len(batch)} 个任务")
        
        # 使用线程池执行批处理
        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            # 创建异步任务
            futures = []
            for task in batch:
                future = asyncio.get_event_loop().run_in_executor(
                    executor, 
                    self._process_single_task_sync, 
                    task
                )
                futures.append(future)
            
            # 等待所有任务完成
            results = await asyncio.gather(*futures, return_exceptions=True)
        
        # 统计结果
        generated_count = 0
        failed_nodes = []
        suggestions = []
        
        for i, result in enumerate(results):
            task = batch[i]
            if isinstance(result, Exception):
                logger.error(f"任务处理异常 {task.node_id}: {result}")
                failed_nodes.append(task.node_id)
            elif result is None:
                logger.warning(f"任务处理失败 {task.node_id}: 返回None")
                failed_nodes.append(task.node_id)
            else:
                # 成功处理
                task_suggestions = result.get('suggestions', [])
                if task_suggestions:
                    generated_count += len(task_suggestions)
                    suggestions.extend(task_suggestions)
                else:
                    logger.debug(f"节点 {task.node_id} 未生成标签建议")
        
        # 按节点组织建议
        suggestions_by_node = {}
        for suggestion in suggestions:
            node_id = suggestion.get('node_id')
            if node_id:
                if node_id not in suggestions_by_node:
                    suggestions_by_node[node_id] = []
                suggestions_by_node[node_id].append(suggestion)
        
        # 转换为Elisp期望的格式
        organized_suggestions = []
        for node_id, node_suggestions in suggestions_by_node.items():
            organized_suggestions.append({
                'node_id': node_id,
                'suggestions': node_suggestions
            })
        
        return {
            'generated_count': generated_count,
            'failed_nodes': failed_nodes,
            'suggestions': organized_suggestions
        }

    def _process_single_task_sync(self, task: NodeProcessingTask) -> Optional[Dict[str, Any]]:
        """同步处理单个任务（在线程池中运行）"""
        try:
            # 在新的事件循环中运行异步处理
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            try:
                return loop.run_until_complete(self._process_single_task_async(task))
            finally:
                loop.close()
        except Exception as e:
            logger.error(f"同步任务处理失败 {task.node_id}: {e}", exc_info=True)
            return None

    async def _process_single_task_async(self, task: NodeProcessingTask) -> Optional[Dict[str, Any]]:
        """异步处理单个任务"""
        max_retries = self.max_retries
        last_error = None
        
        for attempt in range(max_retries):
            try:
                # 第一阶段：生成候选标签
                candidate_tags = await self._generate_candidate_tags(task)
                if not candidate_tags:
                    logger.debug(f"节点 {task.node_id} 未生成候选标签")
                    return {'suggestions': []}
                
                # 第二阶段：置信度评估
                evaluated_tags = await self._evaluate_tag_confidence(task, candidate_tags)
                
                # 过滤低置信度标签
                final_tags = [tag for tag in evaluated_tags 
                             if tag.confidence >= self.confidence_threshold_final]
                
                if not final_tags:
                    logger.debug(f"节点 {task.node_id} 所有标签置信度过低")
                    return {'suggestions': []}
                
                # 转换为建议格式
                suggestions = []
                for tag in final_tags:
                    suggestion = {
                        'node_id': task.node_id,
                        'tag_name': tag.tag_name,
                        'confidence': tag.confidence,
                        'reasoning': tag.reasoning,
                        'source': 'smart_ner_batch',
                        'created_at': time.time()
                    }
                    suggestions.append(suggestion)
                
                logger.debug(f"节点 {task.node_id} 生成 {len(suggestions)} 个标签建议")
                return {'suggestions': suggestions}
                
            except Exception as e:
                last_error = e
                logger.warning(f"处理节点 {task.node_id} 失败 (尝试 {attempt + 1}/{max_retries}): {e}")
                if attempt < max_retries - 1:
                    await asyncio.sleep(1)  # 重试前等待
        
        logger.error(f"节点 {task.node_id} 处理最终失败: {last_error}")
        return None

    async def _generate_candidate_tags(self, task: NodeProcessingTask) -> List[TagResult]:
        """第一阶段：生成候选标签"""
        content = f"{task.title}\n\n{task.content}".strip()
        if not content:
            return []
        
        # 使用现有的标签提取逻辑
        tags = await self.get_tags_for_content(
            content=content,
            node_id=task.node_id,
            existing_tags=task.existing_tags
        )
        
        # 第一轮过滤：只保留置信度较高的候选标签
        candidate_tags = [tag for tag in tags 
                         if tag.confidence >= self.confidence_threshold_first]
        
        logger.debug(f"节点 {task.node_id} 生成 {len(candidate_tags)} 个候选标签")
        return candidate_tags

    async def _evaluate_tag_confidence(self, task: NodeProcessingTask, 
                                     candidate_tags: List[TagResult]) -> List[TagResult]:
        """第二阶段：评估标签置信度"""
        if not candidate_tags:
            return []
        
        # 构建置信度评估提示
        evaluation_prompt = self._create_confidence_evaluation_prompt(task, candidate_tags)
        
        try:
            # 调用LLM进行置信度评估
            llm_response = await self.llm_async_callable(evaluation_prompt)
            
            # 解析评估结果
            evaluated_tags = self._parse_confidence_evaluation(candidate_tags, llm_response)
            
            logger.debug(f"节点 {task.node_id} 完成置信度评估，{len(evaluated_tags)} 个标签")
            return evaluated_tags
            
        except Exception as e:
            logger.warning(f"置信度评估失败 {task.node_id}: {e}")
            # 评估失败时返回原始标签
            return candidate_tags

    def _create_confidence_evaluation_prompt(self, task: NodeProcessingTask, 
                                           candidate_tags: List[TagResult]) -> str:
        """创建置信度评估提示"""
        content = f"{task.title}\n\n{task.content}".strip()
        tag_list = [f"- {tag.tag_name}: {tag.reasoning}" for tag in candidate_tags]
        
        prompt = f"""
请评估以下标签对于给定内容的相关性和质量，为每个标签提供0-1之间的置信度分数。

内容:
{content}

候选标签:
{chr(10).join(tag_list)}

现有标签: {', '.join(str(tag) for tag in task.existing_tags) if task.existing_tags else '无'}

评估标准:
1. 相关性：标签是否准确描述内容主题
2. 特异性：标签是否足够具体，避免过于宽泛
3. 实用性：标签是否有助于知识组织和检索
4. 避重复：避免与现有标签重复或过于相似

请以JSON格式返回评估结果:
[
  {{"tag_name": "标签名", "confidence": 0.8, "reasoning": "评估理由"}},
  ...
]
"""
        return prompt.strip()

    def _parse_confidence_evaluation(self, candidate_tags: List[TagResult], 
                                   llm_response: str) -> List[TagResult]:
        """解析置信度评估结果"""
        try:
            # 使用统一处理器解析响应
            note_results = self.unified_processor.process_llm_response(
                response_str=llm_response,
                note_ids=['evaluation']
            )
            
            if not note_results or note_results[0].error:
                logger.warning("置信度评估解析失败，使用原始标签")
                return candidate_tags
            
            evaluated_tags = note_results[0].tags
            
            # 将评估结果映射回原始标签
            tag_map = {tag.tag_name: tag for tag in candidate_tags}
            final_tags = []
            
            for eval_tag in evaluated_tags:
                if eval_tag.tag_name in tag_map:
                    original_tag = tag_map[eval_tag.tag_name]
                    # 更新置信度和推理
                    original_tag.confidence = eval_tag.confidence
                    original_tag.reasoning = eval_tag.reasoning or original_tag.reasoning
                    final_tags.append(original_tag)
            
            return final_tags
            
        except Exception as e:
            logger.warning(f"置信度评估解析异常: {e}")
            return candidate_tags 
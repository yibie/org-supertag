#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import asyncio
import logging
import time
from concurrent.futures import ThreadPoolExecutor
from typing import Dict, List, Any, Optional, Callable, Awaitable
from dataclasses import dataclass

from simtag.prompts import create_prompt, ORG_MODE_TAG_EXTRACTION_PROMPT, DEFAULT_ENTITY_TYPES
from simtag.utils.unified_tag_processor import UnifiedTagProcessor, TagResult, NoteResult

logger = logging.getLogger(__name__)

@dataclass
class BatchProcessingResult:
    """Batch processing result"""
    generated_count: int
    failed_count: int
    failed_nodes: List[str]
    processing_time: float
    suggestions: List[Dict[str, Any]]

@dataclass
class NodeProcessingTask:
    """Node processing task"""
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
        
        # Batch processing configuration
        self.max_workers = self.config.get('max_workers', 2)  # 2 threads
        self.max_retries = self.config.get('max_retries', 3)  # 3 retries
        self.confidence_threshold_first = self.config.get('confidence_threshold_first', 0.3)  # First round threshold
        self.confidence_threshold_final = self.config.get('confidence_threshold_final', 0.6)  # Final threshold
        self.batch_size = self.config.get('batch_size', 10)  # Batch size
        
        # Progress callback
        self.progress_callback = None
        
        logger.info(f"SmartNERService initialized with {self.max_workers} workers, "
                   f"confidence thresholds: {self.confidence_threshold_first}/{self.confidence_threshold_final}")

    def set_progress_callback(self, callback: Optional[Callable[[Dict[str, Any]], None]]):
        """Sets the progress callback function"""
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
        Generates tag suggestions in batches.
        
        Args:
            nodes_data: List of node data, each element containing:
                - id: Node ID
                - title: Node title
                - content: Node content
                - file_path: File path
                - level: Node level
                - existing_tags: List of existing tags
        
        Returns:
            BatchProcessingResult: The batch processing result
        """
        start_time = time.time()
        logger.info(f"Starting batch processing for {len(nodes_data)} nodes")
        
        # Convert to processing tasks
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
        
        # Execute batch processing
        result = await self._process_tasks_in_batches(tasks)
        
        processing_time = time.time() - start_time
        result.processing_time = processing_time
        
        logger.info(f"Batch processing finished: Generated {result.generated_count} suggestions, "
                   f"failed {result.failed_count} nodes, took {processing_time:.2f}s")
        
        return result

    async def _process_tasks_in_batches(self, tasks: List[NodeProcessingTask]) -> BatchProcessingResult:
        """Processes tasks in batches"""
        total_tasks = len(tasks)
        generated_count = 0
        failed_nodes = []
        all_suggestions = []
        
        # Process in batches
        for i in range(0, total_tasks, self.batch_size):
            batch = tasks[i:i + self.batch_size]
            batch_results = await self._process_batch_with_threading(batch, i, total_tasks)
            
            generated_count += batch_results['generated_count']
            failed_nodes.extend(batch_results['failed_nodes'])
            all_suggestions.extend(batch_results['suggestions'])
            
            # Update progress
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
                    logger.warning(f"Progress callback failed: {e}")
        
        return BatchProcessingResult(
            generated_count=generated_count,
            failed_count=len(failed_nodes),
            failed_nodes=failed_nodes,
            processing_time=0.0,  # Will be set in the calling function
            suggestions=all_suggestions
        )

    async def _process_batch_with_threading(self, batch: List[NodeProcessingTask], 
                                          batch_start_idx: int, total_tasks: int) -> Dict[str, Any]:
        """Processes a batch of tasks using multi-threading"""
        logger.debug(f"Processing batch {batch_start_idx//self.batch_size + 1}, containing {len(batch)} tasks")
        
        # Create asynchronous tasks directly
        futures = []
        for task in batch:
            futures.append(self._process_single_task_async(task))
        
        # Wait for all tasks to complete
        results = await asyncio.gather(*futures, return_exceptions=True)
        
        # Aggregate results
        generated_count = 0
        failed_nodes = []
        suggestions = []
        
        for i, result in enumerate(results):
            task = batch[i]
            if isinstance(result, Exception):
                logger.error(f"Task processing exception for {task.node_id}: {result}")
                failed_nodes.append(task.node_id)
            elif result is None:
                logger.warning(f"Task processing failed for {task.node_id}: Returned None")
                failed_nodes.append(task.node_id)
            else:
                # Successfully processed
                task_suggestions = result.get('suggestions', [])
                if task_suggestions:
                    generated_count += len(task_suggestions)
                    suggestions.extend(task_suggestions)
                else:
                    logger.debug(f"Node {task.node_id} generated no tag suggestions")
        
        # Organize suggestions by node
        suggestions_by_node = {}
        for suggestion in suggestions:
            node_id = suggestion.get('node_id')
            if node_id:
                if node_id not in suggestions_by_node:
                    suggestions_by_node[node_id] = []
                suggestions_by_node[node_id].append(suggestion)
        
        # Convert to the format expected by Elisp
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

    # Removed _process_single_task_sync as it's no longer needed

    async def _process_single_task_async(self, task: NodeProcessingTask) -> Optional[Dict[str, Any]]:
        """Asynchronously processes a single task"""
        max_retries = self.max_retries
        last_error = None
        
        for attempt in range(max_retries):
            try:
                # Stage 1: Generate candidate tags
                candidate_tags = await self._generate_candidate_tags(task)
                if not candidate_tags:
                    logger.debug(f"Node {task.node_id} generated no candidate tags")
                    return {'suggestions': []}
                
                # Stage 2: Evaluate confidence
                evaluated_tags = await self._evaluate_tag_confidence(task, candidate_tags)
                
                # Filter low confidence tags
                final_tags = [tag for tag in evaluated_tags 
                             if tag.confidence >= self.confidence_threshold_final]
                
                if not final_tags:
                    logger.debug(f"Node {task.node_id}: All tags below final confidence threshold")
                    return {'suggestions': []}
                
                # Convert to suggestion format
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
                
                logger.debug(f"Node {task.node_id} generated {len(suggestions)} tag suggestions")
                return {'suggestions': suggestions}
                
            except Exception as e:
                last_error = e
                logger.warning(f"Processing node {task.node_id} failed (Attempt {attempt + 1}/{max_retries}): {e}")
                if attempt < max_retries - 1:
                    await asyncio.sleep(1)  # Wait before retrying
        
        logger.error(f"Node {task.node_id} processing ultimately failed: {last_error}")
        return None

    async def _generate_candidate_tags(self, task: NodeProcessingTask) -> List[TagResult]:
        """Stage 1: Generates candidate tags"""
        content = f"{task.title}\n\n{task.content}".strip()
        if not content:
            return []
        
        # Use the existing tag extraction logic
        tags = await self.get_tags_for_content(
            content=content,
            node_id=task.node_id,
            existing_tags=task.existing_tags
        )
        
        # First round filtering: Keep only candidate tags with higher confidence
        candidate_tags = [tag for tag in tags 
                         if tag.confidence >= self.confidence_threshold_first]
        
        logger.debug(f"Node {task.node_id} generated {len(candidate_tags)} candidate tags")
        return candidate_tags

    async def _evaluate_tag_confidence(self, task: NodeProcessingTask, 
                                     candidate_tags: List[TagResult]) -> List[TagResult]:
        """Stage 2: Evaluates tag confidence"""
        if not candidate_tags:
            return []
        
        # Build the confidence evaluation prompt
        evaluation_prompt = self._create_confidence_evaluation_prompt(task, candidate_tags)
        
        try:
            # Call LLM for confidence evaluation
            llm_response = await self.llm_async_callable(evaluation_prompt)
            
            # Parse the evaluation result
            evaluated_tags = self._parse_confidence_evaluation(candidate_tags, llm_response)
            
            logger.debug(f"Node {task.node_id} completed confidence evaluation, {len(evaluated_tags)} tags")
            return evaluated_tags
            
        except Exception as e:
            logger.warning(f"Confidence evaluation failed for {task.node_id}: {e}")
            # Return original tags if evaluation fails
            return candidate_tags

    def _create_confidence_evaluation_prompt(self, task: NodeProcessingTask, 
                                           candidate_tags: List[TagResult]) -> str:
        """Creates the confidence evaluation prompt"""
        content = f"{task.title}\n\n{task.content}".strip()
        tag_list = [f"- {tag.tag_name}: {tag.reasoning}" for tag in candidate_tags]
        
        prompt = f"""
Please evaluate the relevance and quality of the following tags for the given content, providing a confidence score between 0 and 1 for each tag.

Content:
{content}

Candidate Tags:
{chr(10).join(tag_list)}

Existing Tags: {', '.join(str(tag) for tag in task.existing_tags) if task.existing_tags else 'None'}

Evaluation Criteria:
1. Relevance: Does the tag accurately describe the content's topic?
2. Specificity: Is the tag specific enough, avoiding being too broad?
3. Utility: Will the tag help with knowledge organization and retrieval?
4. Avoid Duplication: Avoid tags that are redundant or too similar to existing tags.

Please return the evaluation results in JSON format:
[
  {{"tag_name": "Tag Name", "confidence": 0.8, "reasoning": "Evaluation reason"}},
  ...
]
"""
        return prompt.strip()

    def _parse_confidence_evaluation(self, candidate_tags: List[TagResult], 
                                   llm_response: str) -> List[TagResult]:
        """Parses the confidence evaluation result"""
        try:
            # Use the unified processor to parse the response
            note_results = self.unified_processor.process_llm_response(
                response_str=llm_response,
                note_ids=['evaluation']
            )
            
            if not note_results or note_results[0].error:
                logger.warning("Confidence evaluation parsing failed, using original tags")
                return candidate_tags
            
            evaluated_tags = note_results[0].tags
            
            # Map evaluation results back to original tags
            tag_map = {tag.tag_name: tag for tag in candidate_tags}
            final_tags = []
            
            for eval_tag in evaluated_tags:
                if eval_tag.tag_name in tag_map:
                    original_tag = tag_map[eval_tag.tag_name]
                    # Update confidence and reasoning
                    original_tag.confidence = eval_tag.confidence
                    original_tag.reasoning = eval_tag.reasoning or original_tag.reasoning
                    final_tags.append(original_tag)
            
            return final_tags
            
        except Exception as e:
            logger.warning(f"Confidence evaluation parsing exception: {e}")
            return candidate_tags 
import logging
import asyncio
from typing import List, Dict, Any
from dependency_injector.wiring import inject, Provide

from ..utils.unified_tag_processor import TagResult

logger = logging.getLogger(__name__)

# 注意：_parse_autotag_payload 函数已被移除
# 现在 Elisp 端遵循统一数据合约，使用标准的 normalize_payload 处理数据

class AutotagHandler:
    """处理自动标签生成的业务逻辑 (异步)"""
    
    @inject
    def __init__(self, 
                 llm_client=Provide['llm_client'],
                 ner_service=Provide['ner_service']):
        self.llm_client = llm_client
        self.ner_service = ner_service
        logger.info("AutotagHandler initialized with injected dependencies")

    async def batch_generate_tags(self, payload) -> Dict[str, Any]:
        """
        批量生成标签 (异步版本)
        
        使用原始的 normalize_payload 处理数据
        """
        try:
            logger.debug(f"batch_generate_tags received payload of type: {type(payload)}")
            
            # 使用原始的统一数据处理方式
            from ..utils.unified_tag_processor import normalize_payload
            data_dict = normalize_payload(payload)
            
            nodes_data = data_dict.get('nodes', [])
            model_config = data_dict.get('model_config', {})
            
            logger.debug(f"Extracted {len(nodes_data)} nodes and model_config with keys: {list(model_config.keys()) if isinstance(model_config, dict) else type(model_config)}")
            
            if not nodes_data:
                logger.warning("No nodes found in autotag payload.")
                logger.warning(f"Available keys in data_dict: {list(data_dict.keys())}")
                logger.warning(f"Full data_dict content: {data_dict}")
                return {'suggestions': [], 'failed_count': 0, 'total_processed': 0, 'successful': 0}

            # 确保 nodes_data 是列表格式
            if isinstance(nodes_data, dict):
                nodes_list = [nodes_data]
            elif isinstance(nodes_data, list):
                nodes_list = nodes_data
            else:
                logger.warning(f"Unexpected nodes_data type: {type(nodes_data)}")
                return {'suggestions': [], 'failed_count': 0, 'total_processed': 0, 'successful': 0}

            logger.info(f"Processing batch tag generation for {len(nodes_list)} nodes")

            # 使用 asyncio.gather 并发处理所有节点
            tasks = [self._generate_tags_for_node(node_data, model_config) for node_data in nodes_list]
            results = await asyncio.gather(*tasks, return_exceptions=True)

            # 处理结果
            final_results = []
            for i, res in enumerate(results):
                node_data = nodes_list[i]
                node_id = "unknown"
                try:
                    node_id = node_data.get('id', f'node_{i}') if isinstance(node_data, dict) else f'node_{i}'
                except (TypeError, ValueError):
                    logger.warning(f"Could not extract node_id from node_data at index {i}")

                if isinstance(res, Exception):
                    logger.error(f"Error processing node {i} (id: {node_id}): {res}", exc_info=True)
                    final_results.append({
                        'node_id': node_id,
                        'tags': [],
                        'status': 'error',
                        'error': str(res)
                    })
                else:
                    final_results.append({
                        'node_id': node_id,
                        'tags': [tag.__dict__ for tag in res],
                        'status': 'success'
                    })
            
            # Convert to the format expected by ELisp callback
            suggestions = []
            failed_count = 0
            
            for result in final_results:
                if result['status'] == 'success' and result['tags']:
                    # Convert tag format for ELisp
                    node_suggestions = []
                    for tag in result['tags']:
                        node_suggestions.append({
                            'tag_name': tag.get('tag_name', ''),
                            'confidence': tag.get('confidence', 0.5),
                            'reasoning': tag.get('reasoning', '')
                        })
                    
                    if node_suggestions:  # Only add if there are actual suggestions
                        suggestions.append({
                            'node_id': result['node_id'],
                            'suggestions': node_suggestions
                        })
                elif result['status'] == 'error':
                    failed_count += 1
            
            return {
                'suggestions': suggestions,
                'failed_count': failed_count,
                'total_processed': len(nodes_list),
                'successful': len([r for r in final_results if r['status'] == 'success'])
            }
            
        except Exception as e:
            logger.error(f"Error in batch_generate_tags: {str(e)}", exc_info=True)
            return {
                'error': str(e),
                'suggestions': [],
                'failed_count': 0,
                'total_processed': 0,
                'successful': 0
            }

    async def suggest_tags_for_single_node_elisp(self, payload) -> Dict[str, Any]:
        """
        为单个节点生成标签 (异步版本)，使用原始的 normalize_payload。
        """
        try:
            logger.debug(f"suggest_tags_for_single_node_elisp received payload of type: {type(payload)}")
            
            # 使用原始的统一数据处理方式
            from ..utils.unified_tag_processor import normalize_payload
            data_dict = normalize_payload(payload)

            # 对于单节点，可能直接在 nodes 中，或者在 node 键中
            node = data_dict.get('node')
            if not node:
                nodes = data_dict.get('nodes', [])
                if nodes and len(nodes) > 0:
                    node = nodes[0]  # 取第一个节点
            
            model_config = data_dict.get('model_config', {})

            if not isinstance(node, dict):
                raise ValueError(f"Expected a 'node' dictionary in payload, but got {type(node)}")

            node_id = node.get('id', 'unknown')
            logger.info(f"Processing single node tag generation for node: {node_id}")
            
            tags = await self._generate_tags_for_node(node, model_config)
            
            return {
                'node_id': node_id,
                'tags': [tag.__dict__ for tag in tags],
                'status': 'success'
            }
            
        except Exception as e:
            logger.error(f"Error in suggest_tags_for_single_node_elisp: {str(e)}", exc_info=True)
            return {
                'node_id': 'unknown',
                'tags': [],
                'status': 'error',
                'error': str(e)
            }

    async def _generate_tags_for_node(self, node_data: Dict, model_config: Dict[str, Any]) -> List[TagResult]:
        """
        为单个节点生成标签的内部方法 (异步版本)
        """
        if not isinstance(node_data, dict):
            logger.error(f"Could not process node_data, expected a dict but got {type(node_data)}")
            return []

        try:
            node_id = node_data.get('id', 'unknown')
            content = node_data.get('content', '')
            
            if not content:
                logger.warning(f"Node {node_id} has no content")
                return []
            
            # 使用正确的 NERService 方法: suggest_tags_batch (async)
            suggested_tags_raw_batch = await self.ner_service.suggest_tags_batch([content], [[]])
            suggested_tags_raw = suggested_tags_raw_batch[0] if suggested_tags_raw_batch else []
            logger.debug(f"NER service suggested {len(suggested_tags_raw)} tags for node {node_id}")

            # 将原始建议转换为 TagResult 格式
            tags = []
            for suggestion in suggested_tags_raw:
                if isinstance(suggestion, dict):
                    tag = TagResult(
                        tag_name=suggestion.get('tag_name', ''),
                        confidence=suggestion.get('confidence', 0.5),
                        reasoning=suggestion.get('reasoning', 'Suggested by LLM.'),
                        source="llm_ner"
                    )
                    tags.append(tag)
                else:
                    logger.warning(f"Received non-dict suggestion for node {node_id}: {suggestion}")
            
            logger.debug(f"Generated {len(tags)} tags for node {node_id}")
            return tags
            
        except Exception as e:
            logger.error(f"Error generating tags for node {node_data.get('id', 'unknown')}: {str(e)}", exc_info=True)
            return [] 
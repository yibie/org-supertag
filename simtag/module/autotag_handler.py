import logging
import asyncio
from platform import node
from typing import List, Dict, Any, Optional
from dependency_injector.wiring import inject, Provide

from ..utils.unified_tag_processor import TagResult, normalize_payload

logger = logging.getLogger(__name__)

class AutotagHandler:
    """处理自动标签生成的业务逻辑 (异步)"""
    
    @inject
    def __init__(self, 
                 llm_client=Provide['llm_client'],
                 ner_service=Provide['ner_service']):
        self.llm_client = llm_client
        self.ner_service = ner_service
        logger.info("AutotagHandler initialized with injected dependencies")

    async def batch_generate_tags(self, *args) -> Dict[str, Any]:
        """
        批量生成标签 (异步版本)
        
        The bridge passes the raw Elisp payload here. We use normalize_payload
        to convert it into a standard Python dictionary.
        """
        try:
            logger.debug(f"batch_generate_tags received raw args tuple: {args}")
            logger.debug(f"args length: {len(args)}")
            if args:
                logger.debug(f"first arg type: {type(args[0])}")
                logger.debug(f"first arg content: {args[0]}")
            
            # The entire payload is passed as the first argument.
            if not args:
                raise ValueError("No arguments provided to batch_generate_tags")
            
            # Use the new, unified payload normalizer.
            # The EPC bridge should already wrap the payload in a list.
            data_dict = normalize_payload(list(args))
            logger.debug(f"Normalized data_dict: {data_dict}")
            logger.debug(f"data_dict keys: {list(data_dict.keys()) if isinstance(data_dict, dict) else 'not a dict'}")

            if 'error' in data_dict:
                raise ValueError(f"Payload normalization failed: {data_dict.get('message')}")
            
            nodes_data = data_dict.get('nodes', [])
            model_config = data_dict.get('model_config', {})
            
            # Fix: Handle case where nodes_data is a dict instead of list
            if isinstance(nodes_data, dict):
                # If it's a single node dict, wrap it in a list
                nodes_list = [nodes_data]
                logger.debug(f"Single node detected, wrapped in list")
            elif isinstance(nodes_data, list):
                nodes_list = nodes_data
                logger.debug(f"Node list detected with {len(nodes_list)} items")
            else:
                logger.warning(f"Unexpected nodes data type: {type(nodes_data)}")
                nodes_list = []
            
            # Fix: Handle case where model_config is a list instead of dict
            if isinstance(model_config, list):
                # Convert plist-style list to dict
                config_dict = {}
                for i in range(0, len(model_config), 2):
                    if i + 1 < len(model_config):
                        key = model_config[i]
                        value = model_config[i + 1]
                        config_dict[key] = value
                model_config = config_dict
                logger.debug(f"Converted model_config list to dict: {model_config}")
            
            logger.debug(f"Final nodes_list length: {len(nodes_list)}")
            logger.debug(f"Final model_config: {model_config}")
            
            if not nodes_list:
                logger.warning("No nodes found in payload for batch tag generation.")
                logger.warning(f"Available keys in data_dict: {list(data_dict.keys()) if isinstance(data_dict, dict) else 'not a dict'}")
                logger.warning(f"Original nodes_data type: {type(nodes_data)}, value: {nodes_data}")
                return {'results': [], 'total_processed': 0, 'successful': 0, 'failed': 0}

            logger.info(f"Processing batch tag generation for {len(nodes_list)} nodes")

            # Use asyncio.gather to run all tag generations concurrently
            tasks = [self._generate_tags_for_node(node_data, model_config) for node_data in nodes_list]
            results = await asyncio.gather(*tasks, return_exceptions=True)

            # Process results
            final_results = []
            for i, res in enumerate(results):
                node_data = nodes_list[i]
                node_id = "unknown"
                try:
                    node_id = dict(node_data).get('id', f'node_{i}')
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
            
            return {
                'results': final_results,
                'total_processed': len(nodes_list),
                'successful': len([r for r in final_results if r['status'] == 'success']),
                'failed': len([r for r in final_results if r['status'] == 'error'])
            }
            
        except Exception as e:
            logger.error(f"Error in batch_generate_tags: {str(e)}", exc_info=True)
            return {
                'error': str(e),
                'results': [],
                'total_processed': 0,
                'successful': 0,
                'failed': 0
            }

    async def suggest_tags_for_single_node_elisp(self, *args) -> Dict[str, Any]:
        """
        为单个节点生成标签 (异步版本), a new wrapper for the EPC call.
        """
        try:
            logger.debug(f"suggest_tags_for_single_node_elisp received raw args tuple: {args}")
            
            if not args:
                raise ValueError("No arguments provided")

            data_dict = normalize_payload(list(args))
            if 'error' in data_dict:
                raise ValueError(f"Payload normalization failed: {data_dict.get('message')}")

            node = data_dict.get('node')
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
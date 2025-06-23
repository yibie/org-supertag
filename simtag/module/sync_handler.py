#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
from typing import Dict, Any, List
import json
import traceback
import asyncio
import concurrent.futures

logger = logging.getLogger(__name__)

class SyncHandler:
    def __init__(self, node_processor, engine, emacs_client=None):
        self.node_processor = node_processor
        self.engine = engine
        self.emacs_client = emacs_client
        logger.info("SyncHandler initialized.")

    def sync_library(self, db_file, db_snapshot_json_str):
        logger.info(f"sync_library called with db_file: {db_file}, receiving DB snapshot as JSON string.")
        
        num_tags = "N/A"
        num_nodes = "N/A"
        parsed_snapshot = None

        try:
            if not isinstance(db_snapshot_json_str, str):
                logger.error(f"DB snapshot is not a JSON string as expected (type: {type(db_snapshot_json_str)}). Aborting sync.")
                return {"status": "error", "message": "Invalid snapshot format (not a JSON string)"}

            logger.debug(f"Attempting to parse JSON snapshot string (length: {len(db_snapshot_json_str)} chars). Preview: {db_snapshot_json_str[:200]}...")
            parsed_snapshot = json.loads(db_snapshot_json_str) # Parse the JSON string
            logger.info("Successfully parsed DB snapshot JSON string.")

            if isinstance(parsed_snapshot, dict):
                num_tags = len(parsed_snapshot.get("tags", []))
                num_nodes = len(parsed_snapshot.get("nodes", []))
                logger.info(f"Parsed snapshot contains: tags={num_tags}, nodes={num_nodes}.")
            else:
                logger.error(f"Parsed snapshot is not a dictionary (type: {type(parsed_snapshot)}). Aborting sync.")
                return {"status": "error", "message": "Parsed snapshot is not a dictionary"}

            # Call the engine method with the parsed Python dictionary
            result = self.engine.sync_full_snapshot(parsed_snapshot) 
            
            return {"status": "success", "result": result}
        except json.JSONDecodeError as je:
            logger.error(f"JSON decoding failed for DB snapshot: {je}\nFull string was (approx first 500 chars): {db_snapshot_json_str[:500]}")
            return {"status": "error", "message": f"JSON decoding error: {je}"}
        except Exception as e:
            logger.error(f"Sync library with full snapshot failed: {e}\n{traceback.format_exc()}")
            # Include details about the snapshot if it was parsed, to help debug if error is in engine
            parsed_info = f"Parsed snapshot (tags: {num_tags}, nodes: {num_nodes})" if parsed_snapshot else "Snapshot not parsed."
            return {"status": "error", "message": f"General error during sync: {e}. {parsed_info}"}

    async def _async_sync_node_from_elisp(self, node_props: Dict[str, Any]) -> Dict[str, Any]:
        """
        Asynchronously processes a single node from Elisp.
        This is the new, preferred method for syncing individual nodes.
        The logic is moved here from SimTagBridge.
        """
        node_id = node_props.get("id")
        if not node_id:
            logger.error("sync_node_from_elisp: Received node properties without an 'id'.")
            return {"status": "error", "message": "Node 'id' is required."}

        logger.info(f"SyncHandler: Processing node {node_id}.")

        try:
            title = node_props.get("title", "")
            content_body = node_props.get("content", "")
            tags = node_props.get("tags", [])
            
            # The content for processing is the combination of title and body.
            content = f"{title}\n\n{content_body}".strip()

            # Delegate the core processing to NodeProcessor
            success = await self.node_processor.process_single_node_data(
                node_id=node_id, 
                content=content, 
                tags=tags, 
                title=title
            )

            if success:
                logger.info(f"SyncHandler: Successfully processed node {node_id}.")
                return {"status": "success", "node_id": node_id}
            else:
                logger.warning(f"SyncHandler: NodeProcessor failed to process node {node_id}.")
                return {"status": "error", "message": f"NodeProcessor failed for node {node_id}.", "node_id": node_id}

        except Exception as e:
            logger.error(f"SyncHandler: Error syncing node {node_id}: {e}", exc_info=True)
            return {"status": "error", "message": str(e), "node_id": node_id}

    def sync_node_from_elisp(self, node_props: Dict[str, Any]) -> Dict[str, Any]:
        """Synchronous EPC wrapper for _async_sync_node_from_elisp."""
        return asyncio.run(self._async_sync_node_from_elisp(node_props))

    async def _async_bulk_process_snapshot(self, snapshot_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Main entry point for periodic background sync.
        Processes an incremental snapshot of changes from the Elisp database.
        """
        try:
            nodes_to_upsert = snapshot_data.get("nodes", [])
            links_to_upsert = snapshot_data.get("links", [])
            ids_to_delete = snapshot_data.get("ids_to_delete", [])

            total_upsert = len(nodes_to_upsert)
            total_delete = len(ids_to_delete)
            logger.info(f"Received incremental snapshot: {total_upsert} nodes to upsert, {total_delete} IDs to delete, {len(links_to_upsert)} links to upsert.")

            # 1. Perform deletions first to handle nodes that might be re-added with a new type, etc.
            if ids_to_delete:
                logger.info(f"Deleting {len(ids_to_delete)} objects from graph.")
                self.node_processor.graph_service.delete_nodes_by_ids(ids_to_delete)

            # 2. Process nodes that need to be created or updated.
            # This will handle embedding and relation inference for new/changed content.
            if nodes_to_upsert:
                logger.info(f"Delegating upsert processing of {len(nodes_to_upsert)} nodes to NodeProcessor.")
                loop = asyncio.get_running_loop()
                with concurrent.futures.ThreadPoolExecutor() as executor:
                    report = await loop.run_in_executor(
                        executor, self.node_processor.process_nodes_batch, nodes_to_upsert, len(nodes_to_upsert)
                    )
                logger.info(f"NodeProcessor finished. Report: {report}")
            else:
                report = {"status": "no nodes to process"}

            # 3. Upsert link relationships. This is a direct graph operation.
            if links_to_upsert:
                relations_from_links = []
                for link in links_to_upsert:
                    source_id = link.get('source')
                    target_id = link.get('target')
                    if source_id and target_id:
                        relations_from_links.append({
                            'source_id': source_id,
                            'target_id': target_id,
                            'type': 'REF_TO',  # Correctly hardcode the relation type for org links
                            'properties': json.dumps({'raw_link': link})
                        })
                
                if relations_from_links:
                    logger.info(f"Upserting {len(relations_from_links)} link-based relations.")
                    self.node_processor.graph_service.bulk_upsert_relations(relations_from_links)

            logger.info("Incremental snapshot processing complete.")
            return {"status": "success", "result": report}
            
        except Exception as e:
            logger.error(f"SyncHandler: Error in _async_bulk_process_snapshot: {e}", exc_info=True)
            return {"status": "error", "message": str(e)}

    def bulk_process_snapshot(self, snapshot_data: Any) -> Dict[str, Any]:
        """Synchronous EPC wrapper for _async_bulk_process_snapshot."""
        # 统一使用 unified_tag_processor 进行所有数据标准化
        from simtag.utils.unified_tag_processor import normalize_payload
        
        try:
            # 按照规范，应该接收 [hash-table] 格式，但EPC可能序列化了hash-table
            normalized_data = normalize_payload(snapshot_data)
            logger.debug(f"Normalized data keys: {normalized_data.keys() if isinstance(normalized_data, dict) else 'N/A'}")
            
            return asyncio.run(self._async_bulk_process_snapshot(normalized_data))
            
        except Exception as e:
            logger.error(f"Failed to normalize snapshot data: {e}", exc_info=True)
            return {"status": "error", "message": f"Data normalization failed: {str(e)}"}

    def sync_tag_event(self, event_data: Dict[str, Any]) -> Dict[str, Any]:
        """同步标签事件到 Python 后端。
        
        Args:
            event_data: 事件数据，包含事件类型和相关数据
            
        Returns:
            同步结果
        """
        try:
            event_type = event_data.get('event_type')
            tag_data = event_data.get('tag_data')
            
            if not event_type or not tag_data:
                return {"status": "error", "message": "Missing event_type or tag_data"}
            
            # 根据事件类型处理
            if event_type == 'tag:status-changed':
                return self.update_tag_status(tag_data)
            elif event_type == 'tag:rules-changed':
                return self.update_tag_rules(tag_data)
            elif event_type == 'tag:relation-changed':
                return self.update_relation_type(tag_data)
            else:
                return {"status": "error", "message": f"Unknown event type: {event_type}"}
                
        except Exception as e:
            logger.error(f"Error in sync_tag_event: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def update_tag_status(self, tag_data: Dict[str, Any]) -> Dict[str, Any]:
        """更新标签状态。
        
        Args:
            tag_data: 标签数据，包含标签ID和新状态
            
        Returns:
            更新结果
        """
        try:
            tag_id = tag_data.get('id')
            new_status = tag_data.get('tag-status')
            
            if not tag_id or not new_status:
                return {"status": "error", "message": "Missing tag_id or new_status"}
            
            # 更新数据库中的标签状态
            self.engine.update_tag_status(tag_id, new_status)
            
            return {"status": "success", "tag_id": tag_id, "new_status": new_status}
            
        except Exception as e:
            logger.error(f"Error in update_tag_status: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def update_tag_rules(self, tag_data: Dict[str, Any]) -> Dict[str, Any]:
        """更新标签规则。
        
        Args:
            tag_data: 标签数据，包含标签ID和规则
            
        Returns:
            更新结果
        """
        try:
            tag_id = tag_data.get('id')
            rules = tag_data.get('tag-rules')
            
            if not tag_id or not rules:
                return {"status": "error", "message": "Missing tag_id or rules"}
            
            # 更新数据库中的标签规则
            self.engine.update_tag_rules(tag_id, rules)
            
            return {"status": "success", "tag_id": tag_id}
            
        except Exception as e:
            logger.error(f"Error in update_tag_rules: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def update_relation_type(self, relation_data: Dict[str, Any]) -> Dict[str, Any]:
        """更新标签关系类型。
        
        Args:
            relation_data: 关系数据，包含源标签ID、目标标签ID和关系类型
            
        Returns:
            更新结果
        """
        try:
            from_tag = relation_data.get('from-tag')
            to_tag = relation_data.get('to-tag')
            rel_type = relation_data.get('tag-rel-type')
            
            if not from_tag or not to_tag or not rel_type:
                return {"status": "error", "message": "Missing from_tag, to_tag, or rel_type"}
            
            # 更新数据库中的关系类型
            self.engine.update_relation_type(from_tag, to_tag, rel_type)
            
            return {"status": "success", "from_tag": from_tag, "to_tag": to_tag}
            
        except Exception as e:
            logger.error(f"Error in update_relation_type: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def update_relation_rules(self, relation_data: Dict[str, Any]) -> Dict[str, Any]:
        """更新标签关系规则。
        
        Args:
            relation_data: 关系数据，包含源标签ID、目标标签ID和规则
            
        Returns:
            更新结果
        """
        try:
            from_tag = relation_data.get('from-tag')
            to_tag = relation_data.get('to-tag')
            rules = relation_data.get('tag-rel-rules')
            
            if not from_tag or not to_tag or not rules:
                return {"status": "error", "message": "Missing from_tag, to_tag, or rules"}
            
            # 更新数据库中的关系规则
            self.engine.update_relation_rules(from_tag, to_tag, rules)
            
            return {"status": "success", "from_tag": from_tag, "to_tag": to_tag}
            
        except Exception as e:
            logger.error(f"Error in update_relation_rules: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)} 
"""
Sync Orchestrator Module
Handles the synchronization of full database snapshots.
"""
import logging
import numpy as np
import json
from typing import List, Optional, Dict, Any
import time

# Forward declare types for type hinting if VectorStorage and LLMClient are complex imports
# from .storage import VectorStorage # Assuming VectorStorage is in .storage
# from ..services.llm_client import LLMClient # Assuming LLMClient is in ..services

logger = logging.getLogger(__name__)

class SyncOrchestrator:
    def __init__(self, storage, llm_client, logger: logging.Logger):
        """
        Initializes the SyncOrchestrator.

        Args:
            storage: An instance of VectorStorage.
            llm_client: An instance of LLMClient.
            logger: A logging.Logger instance.
        """
        self.storage = storage
        self.llm_client = llm_client
        self.logger = logger
        self.logger.info("SyncOrchestrator initialized.")

    def sync_full_snapshot(self, db_snapshot: Dict[str, Any]) -> Dict[str, Any]:
        """
        Processes a full database snapshot containing tags and nodes.
        This method was moved from TaggingEngine.
        """
        self.logger.info(f"SyncOrchestrator: Starting sync with full DB snapshot.")
        if not isinstance(db_snapshot, dict):
            self.logger.error(f"SyncOrchestrator: db_snapshot is not a dictionary (type: {type(db_snapshot)}). Aborting sync.")
            return {"status": "error", "message": "Invalid snapshot format (not a dict)", "processed_tags": 0, "processed_nodes": 0}

        raw_tags = db_snapshot.get('tags', [])
        raw_nodes = db_snapshot.get('nodes', [])
        self.logger.info(f"SyncOrchestrator: Snapshot contains {len(raw_tags)} tags and {len(raw_nodes)} nodes.")

        # --- Add detailed logging for raw_tags itself --- 
        self.logger.info(f"SyncOrchestrator: raw_tags before access: {raw_tags!r} (type: {type(raw_tags)})")
        # --- End detailed logging ---

        # --- Add detailed logging for the first raw tag and node --- 
        if raw_tags:
            example_tag_entry = next(iter(raw_tags.values())) if isinstance(raw_tags, dict) and raw_tags else raw_tags[0] if raw_tags else None
            if example_tag_entry:
                self.logger.info(f"SyncOrchestrator: Example raw tag entry from snapshot: {example_tag_entry!r} (type: {type(example_tag_entry)})")
        if raw_nodes:
            example_node_entry = next(iter(raw_nodes.values())) if isinstance(raw_nodes, dict) and raw_nodes else raw_nodes[0] if raw_nodes else None
            if example_node_entry:
                 self.logger.info(f"SyncOrchestrator: Example raw node entry from snapshot: {example_node_entry!r} (type: {type(example_node_entry)})")
        # --- End detailed logging ---

        processed_tags_for_storage = [] # For metadata
        tag_embeddings_for_vss = [] # For VSS embeddings, if active
        vss_active = self.storage.has_vector_ext

        # --- Tag Processing --- 
        self.logger.info("SyncOrchestrator: Starting tag processing.")
        existing_tag_ids_in_db = set(self.storage.list_tag_ids())
        self.logger.info(f"SyncOrchestrator: Found {len(existing_tag_ids_in_db)} tag IDs currently in database.")
        incoming_tag_ids_from_snapshot = set()

        # Process tags
        raw_tags_list = [] 
        if isinstance(raw_tags, dict):
            for tag_id_key, props_list in raw_tags.items():
                if isinstance(props_list, list) and len(props_list) >= 1:
                    entry = [str(tag_id_key)] + props_list
                    raw_tags_list.append(entry)
                else:
                    self.logger.warning(f"SyncOrchestrator: Skipping malformed tag entry (dict value not list or too short) for key {tag_id_key}: value {props_list!r}")
        elif isinstance(raw_tags, list):
            raw_tags_list = raw_tags 
        else:
            self.logger.warning(f"SyncOrchestrator: raw_tags is neither a dict nor a list (type: {type(raw_tags)}). Cannot process tags.")

        for tag_entry_list in raw_tags_list:
            if isinstance(tag_entry_list, list) and len(tag_entry_list) >= 2:
                tag_id = str(tag_entry_list[0])
                name = tag_entry_list[1]
                created_at = tag_entry_list[2] if len(tag_entry_list) > 2 else None
                modified_at = tag_entry_list[3] if len(tag_entry_list) > 3 else None
                
                incoming_tag_ids_from_snapshot.add(tag_id) # Collect incoming tag ID

                # Prepare base metadata dictionary
                tag_meta_dict = {
                    'tag_id': tag_id,
                    'name': name,
                    'created_at': created_at,
                    'modified_at': modified_at,
                    # These are other potential fields for tag_metadata table
                    'fields': None, 
                    'tag_type': None,
                    'relation_type_hint': None,
                    'description': None,
                    'icon': None,
                    'color': None,
                    'behaviors': None
                }

                # Generate embedding for the tag name
                tag_vector_list = None
                if name and self.llm_client:
                    try:
                        self.logger.debug(f"SyncOrchestrator: Generating embedding for tag: {name} (ID: {tag_id})")
                        tag_vector_list = self.llm_client.get_embedding_sync(name)
                        if not tag_vector_list:
                            self.logger.warning(f"SyncOrchestrator: Embedding generation returned None/empty for tag: {name} (ID: {tag_id})")
                        else:
                            self.logger.debug(f"SyncOrchestrator: Successfully generated vector for tag: {name} (ID: {tag_id}). Dim: {len(tag_vector_list)}.")
                    except Exception as e:
                        self.logger.error(f"SyncOrchestrator: Failed to generate embedding for tag {name} (ID: {tag_id}): {e}")
                elif not name:
                     self.logger.warning(f"SyncOrchestrator: Tag name is empty for ID: {tag_id}. Skipping embedding generation.")

                if vss_active:
                    # For VSS, metadata goes to processed_tags_for_storage (without vector info explicitly)
                    # and embedding goes to tag_embeddings_for_vss
                    processed_tags_for_storage.append(tag_meta_dict)
                    if tag_vector_list:
                        tag_embeddings_for_vss.append({
                            'tag_id_ref': tag_id,
                            'embedding': tag_vector_list
                        })
                else:
                    # For non-VSS, vector info is added to the metadata dict itself
                    if tag_vector_list:
                        tag_meta_dict['vector'] = tag_vector_list # Stored as list, storage layer will convert to blob
                        tag_meta_dict['vector_dim'] = len(tag_vector_list)
                    else:
                        tag_meta_dict['vector'] = None
                        tag_meta_dict['vector_dim'] = 0
                    processed_tags_for_storage.append(tag_meta_dict)
            else:
                self.logger.warning(f"SyncOrchestrator: Skipping malformed tag entry (list item not list or too short): {tag_entry_list!r}")
        
        # Determine tags to delete (present in DB but not in current snapshot)
        tag_ids_to_delete = list(existing_tag_ids_in_db - incoming_tag_ids_from_snapshot)
        if tag_ids_to_delete:
            self.logger.info(f"SyncOrchestrator: Found {len(tag_ids_to_delete)} tags to delete from DB (stale entries): {tag_ids_to_delete}")
            try:
                self.storage.delete_tags_by_ids(tag_ids_to_delete)
                self.logger.info(f"SyncOrchestrator: Successfully deleted {len(tag_ids_to_delete)} stale tags.")
            except Exception as e:
                self.logger.error(f"SyncOrchestrator: Error during stale tag deletion: {e}")
        else:
            self.logger.info("SyncOrchestrator: No stale tags to delete from DB.")

        # Upsert tag metadata
        if processed_tags_for_storage:
            self.logger.info(f"SyncOrchestrator: Upserting {len(processed_tags_for_storage)} tag metadata records.")
            try:
                self.storage.bulk_insert_tag_vectors(processed_tags_for_storage)
                self.logger.info(f"SyncOrchestrator: Tag metadata upsert successful for {len(processed_tags_for_storage)} tags.")
            except Exception as e:
                self.logger.error(f"SyncOrchestrator: Error during tag metadata upsert: {e}")
        
        # Upsert tag embeddings to VSS table if VSS is active
        if vss_active and tag_embeddings_for_vss:
            self.logger.info(f"SyncOrchestrator: Upserting {len(tag_embeddings_for_vss)} tag embeddings to VSS table.")
            try:
                self.storage.bulk_upsert_tag_embeddings(tag_embeddings_for_vss)
                self.logger.info(f"SyncOrchestrator: Tag VSS embeddings upsert successful for {len(tag_embeddings_for_vss)} tags.")
            except Exception as e:
                self.logger.error(f"SyncOrchestrator: Error during tag VSS embeddings upsert: {e}")

        # --- Node Processing --- 
        self.logger.info("SyncOrchestrator: Starting node processing.")
        # Process nodes
        # MODIFICATION: Get existing node IDs from DB and prepare to collect incoming node IDs
        self.logger.info("SyncOrchestrator: Fetching existing node IDs from database.")
        existing_node_ids_in_db = self.storage.get_all_node_ids()
        self.logger.info(f"SyncOrchestrator: Found {len(existing_node_ids_in_db)} node IDs currently in database.")
        incoming_node_ids_from_snapshot = set()
        # END MODIFICATION

        processed_nodes_for_storage = []
        nodes_processed_count = 0 # Counts nodes attempted for processing
        failed_node_ids_during_embedding = []

        # NEW: Prepare separate list for VSS embeddings if sqlite-vec is active
        node_embeddings_for_vss = []

        # Determine if VSS is active via storage property
        vss_active = self.storage.has_vector_ext
        if vss_active:
            self.logger.info("SyncOrchestrator: sqlite-vec is active. Node embeddings will be stored in VSS table.")
        else:
            self.logger.info("SyncOrchestrator: sqlite-vec is NOT active. Node embeddings (if any) will be stored in main node_vectors table.")

        if isinstance(raw_nodes, dict):
            for node_id_key, properties_list in raw_nodes.items():
                actual_node_id = str(node_id_key)
                if isinstance(properties_list, list) and len(properties_list) >= 4:
                    title_val = properties_list[0]
                    content_val = properties_list[1]
                    tag_ids_list = properties_list[2] if isinstance(properties_list[2], list) else []
                    file_path = properties_list[3]
                    created_at = properties_list[4] if len(properties_list) > 4 else None
                    modified_at = properties_list[5] if len(properties_list) > 5 else None
                    # Attempt to get other potential fields from Elisp if provided, falling back to None
                    pos_val = properties_list[6] if len(properties_list) > 6 else None
                    olp_val = properties_list[7] if len(properties_list) > 7 else None
                    level_val = properties_list[8] if len(properties_list) > 8 else None
                    scheduled_val = properties_list[9] if len(properties_list) > 9 else None
                    deadline_val = properties_list[10] if len(properties_list) > 10 else None
                    todo_val = properties_list[11] if len(properties_list) > 11 else None
                    priority_val = properties_list[12] if len(properties_list) > 12 else None
                    props_json_val = properties_list[13] if len(properties_list) > 13 else None # Assuming this is already JSON string or dict
                    raw_value_val = properties_list[14] if len(properties_list) > 14 else None
                    hash_val = properties_list[15] if len(properties_list) > 15 else None
                    content_hash_val = properties_list[16] if len(properties_list) > 16 else None

                    log_title = title_val if title_val is not None else "[No Title]"
                    self.logger.info(f"SyncOrchestrator: Processing node ID: {actual_node_id}, Title: {log_title[:50]}...")

                    # MODIFIED: Combine title and body for embedding
                    title_for_embedding = title_val if title_val else ""
                    body_for_embedding = content_val if content_val else ""

                    if title_for_embedding and body_for_embedding:
                        text_to_embed = title_for_embedding + "\n" + body_for_embedding
                    elif title_for_embedding:
                        text_to_embed = title_for_embedding
                    elif body_for_embedding:
                        text_to_embed = body_for_embedding
                    else:
                        text_to_embed = ""
                    
                    content_for_embedding = text_to_embed.strip()
                    # END MODIFICATION
                    
                    node_metadata_item = {
                        "node_id": actual_node_id, # Renamed from 'id' to match schema
                        "title": title_val,
                        "content": content_for_embedding, # Storing full content in metadata table
                        "tags": json.dumps(tag_ids_list), # Renamed from 'tags_json'
                        "file_path": file_path,
                        "created_at": created_at,
                        "modified_at": modified_at,
                        "pos": pos_val,
                        "olp": olp_val,
                        "level": level_val,
                        "scheduled": scheduled_val,
                        "deadline": deadline_val,
                        "todo": todo_val,
                        "priority": priority_val,
                        "properties": props_json_val if isinstance(props_json_val, str) else json.dumps(props_json_val),
                        "raw_value": raw_value_val,
                        "hash": hash_val,
                        "content_hash": content_hash_val
                    }

                    # MODIFICATION: Collect incoming node ID
                    incoming_node_ids_from_snapshot.add(actual_node_id)
                    # END MODIFICATION

                    node_vector_list = None
                    if content_for_embedding and self.llm_client:
                        try:
                            self.logger.debug(f"SyncOrchestrator: Generating embedding for node {actual_node_id} (content length: {len(content_for_embedding)} chars)")
                            node_vector_list = self.llm_client.get_embedding_sync(content_for_embedding)
                            if not node_vector_list:
                                self.logger.warning(f"SyncOrchestrator: Embedding generation returned None/empty for node ID {actual_node_id}. Content snippet: '{content_for_embedding[:100]}...'")
                                failed_node_ids_during_embedding.append(actual_node_id)
                            else:
                                self.logger.debug(f"SyncOrchestrator: Successfully generated vector for node ID {actual_node_id}. Dim: {len(node_vector_list)}.")
                        except Exception as e:
                            self.logger.error(f"SyncOrchestrator: Failed to generate embedding for node {actual_node_id}: {e}")
                            failed_node_ids_during_embedding.append(actual_node_id)
                    elif not content_for_embedding:
                        self.logger.info(f"SyncOrchestrator: No content for embedding for node ID {actual_node_id}. Skipping embedding generation.")
                        failed_node_ids_during_embedding.append(actual_node_id)
                    
                    if vss_active:
                        if node_vector_list:
                            node_embeddings_for_vss.append({
                                "node_id_ref": actual_node_id,
                                "embedding": node_vector_list
                            })
                        # Metadata item already prepared, vector/vector_dim are not added to it when VSS is active
                    else: # Fallback: VSS not active, add vector blob and dim to metadata item
                        if node_vector_list:
                            node_metadata_item['vector'] = np.array(node_vector_list, dtype=np.float32).tobytes()
                            node_metadata_item['vector_dim'] = len(node_vector_list)
                        else:
                            node_metadata_item['vector'] = None
                            node_metadata_item['vector_dim'] = 0

                    processed_nodes_for_storage.append(node_metadata_item)
                    nodes_processed_count += 1
                else:
                    self.logger.warning(f"SyncOrchestrator: Skipping malformed node entry (dict value not list or too short) for key {node_id_key}: value {properties_list!r}")
        elif isinstance(raw_nodes, list):
            for node_entry_list in raw_nodes:
                if isinstance(node_entry_list, list) and len(node_entry_list) >= 5: # Base check, more specific indexing below
                    actual_node_id = str(node_entry_list[0])
                    title_val = node_entry_list[1]
                    content_val = node_entry_list[2]
                    tag_ids_list = node_entry_list[3] if isinstance(node_entry_list[3], list) else []
                    file_path = node_entry_list[4]
                    created_at = node_entry_list[5] if len(node_entry_list) > 5 else None
                    modified_at = node_entry_list[6] if len(node_entry_list) > 6 else None
                    pos_val = node_entry_list[7] if len(node_entry_list) > 7 else None
                    olp_val = node_entry_list[8] if len(node_entry_list) > 8 else None
                    level_val = node_entry_list[9] if len(node_entry_list) > 9 else None
                    scheduled_val = node_entry_list[10] if len(node_entry_list) > 10 else None
                    deadline_val = node_entry_list[11] if len(node_entry_list) > 11 else None
                    todo_val = node_entry_list[12] if len(node_entry_list) > 12 else None
                    priority_val = node_entry_list[13] if len(node_entry_list) > 13 else None
                    props_json_val = node_entry_list[14] if len(node_entry_list) > 14 else None
                    raw_value_val = node_entry_list[15] if len(node_entry_list) > 15 else None
                    hash_val = node_entry_list[16] if len(node_entry_list) > 16 else None
                    content_hash_val = node_entry_list[17] if len(node_entry_list) > 17 else None

                    log_title = title_val if title_val is not None else "[No Title]"
                    self.logger.info(f"SyncOrchestrator: Processing node ID: {actual_node_id}, Title: {log_title[:50]}...")

                    # MODIFIED: Combine title and body for embedding
                    title_for_embedding = title_val if title_val else ""
                    body_for_embedding = content_val if content_val else ""

                    if title_for_embedding and body_for_embedding:
                        text_to_embed = title_for_embedding + "\n" + body_for_embedding
                    elif title_for_embedding:
                        text_to_embed = title_for_embedding
                    elif body_for_embedding:
                        text_to_embed = body_for_embedding
                    else:
                        text_to_embed = ""
                        
                    content_for_embedding = text_to_embed.strip()
                    # END MODIFICATION

                    node_metadata_item = {
                        "node_id": actual_node_id,
                        "title": title_val,
                        "content": content_for_embedding,
                        "tags": json.dumps(tag_ids_list),
                        "file_path": file_path,
                        "created_at": created_at,
                        "modified_at": modified_at,
                        "pos": pos_val,
                        "olp": olp_val,
                        "level": level_val,
                        "scheduled": scheduled_val,
                        "deadline": deadline_val,
                        "todo": todo_val,
                        "priority": priority_val,
                        "properties": props_json_val if isinstance(props_json_val, str) else json.dumps(props_json_val),
                        "raw_value": raw_value_val,
                        "hash": hash_val,
                        "content_hash": content_hash_val
                    }

                    # MODIFICATION: Collect incoming node ID
                    incoming_node_ids_from_snapshot.add(actual_node_id)
                    # END MODIFICATION

                    node_vector_list = None
                    if content_for_embedding and self.llm_client:
                        try:
                            self.logger.debug(f"SyncOrchestrator: Generating embedding for node {actual_node_id} (content length: {len(content_for_embedding)} chars)")
                            node_vector_list = self.llm_client.get_embedding_sync(content_for_embedding)
                            if not node_vector_list:
                                self.logger.warning(f"SyncOrchestrator: Embedding generation returned None/empty for node ID {actual_node_id}. Content snippet: '{content_for_embedding[:100]}...'")
                                failed_node_ids_during_embedding.append(actual_node_id)
                            else:
                                self.logger.debug(f"SyncOrchestrator: Successfully generated vector for node ID {actual_node_id}. Dim: {len(node_vector_list)}.")
                        except Exception as e:
                            self.logger.error(f"SyncOrchestrator: Failed to generate embedding for node {actual_node_id}: {e}")
                            failed_node_ids_during_embedding.append(actual_node_id)
                    elif not content_for_embedding:
                        self.logger.info(f"SyncOrchestrator: No content for embedding for node ID {actual_node_id}. Skipping embedding generation.")
                        failed_node_ids_during_embedding.append(actual_node_id)

                    if vss_active:
                        if node_vector_list:
                            node_embeddings_for_vss.append({
                                "node_id_ref": actual_node_id,
                                "embedding": node_vector_list
                            })
                    else: # Fallback: VSS not active
                        if node_vector_list:
                            node_metadata_item['vector'] = np.array(node_vector_list, dtype=np.float32).tobytes()
                            node_metadata_item['vector_dim'] = len(node_vector_list)
                        else:
                            node_metadata_item['vector'] = None
                            node_metadata_item['vector_dim'] = 0
                            
                    processed_nodes_for_storage.append(node_metadata_item)
                    nodes_processed_count += 1
                else:
                    self.logger.warning(f"SyncOrchestrator: Skipping malformed node entry (list item not list or too short): {node_entry_list!r}")
        else:
            self.logger.warning(f"SyncOrchestrator: raw_nodes is neither a dict nor a list (type: {type(raw_nodes)}). Cannot process nodes.")

        if processed_nodes_for_storage:
            self.logger.info(f"SyncOrchestrator: Upserting {len(processed_nodes_for_storage)} node metadata items into storage.")
            try:
                self.storage.bulk_upsert_nodes(processed_nodes_for_storage) # This will now handle VSS active/inactive for metadata
                self.logger.info(f"SyncOrchestrator: Node metadata upsert successful for {len(processed_nodes_for_storage)} items.")
            except Exception as e:
                self.logger.error(f"SyncOrchestrator: Error during node metadata upsert: {e}")

        # NEW: Upsert embeddings to VSS table if active and available
        if vss_active and node_embeddings_for_vss:
            self.logger.info(f"SyncOrchestrator: Preparing to upsert {len(node_embeddings_for_vss)} node embeddings into VSS table (contents of node_embeddings_for_vss count).")
            try:
                self.storage.bulk_upsert_node_embeddings(node_embeddings_for_vss)
                self.logger.info(f"SyncOrchestrator: Node VSS embeddings upsert successful for {len(node_embeddings_for_vss)} items.")
            except Exception as e:
                self.logger.error(f"SyncOrchestrator: Error during node VSS embeddings upsert: {e}")
        elif vss_active and not node_embeddings_for_vss:
            self.logger.info("SyncOrchestrator: VSS is active, but no node embeddings were generated or prepared for VSS storage.")

        # MODIFICATION: Calculate and delete orphaned nodes
        nodes_deleted_count = 0
        if existing_node_ids_in_db is not None: # Ensure the fetch was successful
            node_ids_to_delete = list(existing_node_ids_in_db - incoming_node_ids_from_snapshot)
            if node_ids_to_delete:
                self.logger.info(f"SyncOrchestrator: Found {len(node_ids_to_delete)} orphaned node IDs to delete.")
                try:
                    self.storage.delete_nodes_by_ids(node_ids_to_delete)
                    nodes_deleted_count = len(node_ids_to_delete) # Assume all requested deletions were successful if no error
                    self.logger.info(f"SyncOrchestrator: Successfully deleted {nodes_deleted_count} orphaned nodes.")
                except Exception as e:
                    self.logger.error(f"SyncOrchestrator: Error during orphaned node deletion: {e}")
            else:
                self.logger.info("SyncOrchestrator: No orphaned nodes to delete.")
        else:
            self.logger.warning("SyncOrchestrator: Could not determine orphaned nodes as existing_node_ids_in_db was None (fetch might have failed).")
        # END MODIFICATION

        successfully_embedded_nodes_count = nodes_processed_count - len(failed_node_ids_during_embedding)
        
        # MODIFIED: Include the list of failed node IDs and deleted count in the log and return dict
        log_message = (
            f"SyncOrchestrator: Snapshot sync finished. "
            f"Tags for storage: {len(processed_tags_for_storage)}, "
            f"Nodes for storage: {nodes_processed_count}, "
            f"Successfully embedded nodes: {successfully_embedded_nodes_count}, "
            f"Failed embeddings: {len(failed_node_ids_during_embedding)}, "
            f"Nodes deleted: {nodes_deleted_count}" # Added deleted count
        )
        if failed_node_ids_during_embedding:
            # To avoid excessively long log messages if many IDs fail, log first few and total count
            max_ids_to_log = 10 
            failed_ids_snippet = failed_node_ids_during_embedding[:max_ids_to_log]
            if len(failed_node_ids_during_embedding) > max_ids_to_log:
                log_message += f". First {max_ids_to_log} Failed node IDs: {failed_ids_snippet}... (and {len(failed_node_ids_during_embedding) - max_ids_to_log} more)"
            else:
                log_message += f". Failed node IDs: {failed_ids_snippet}"
        self.logger.info(log_message)
        
        return {
            "status": "success", 
            "message": "Snapshot processed.",
            "processed_tags": len(processed_tags_for_storage), 
            "processed_nodes": successfully_embedded_nodes_count, 
            "failed_embedding_count": len(failed_node_ids_during_embedding),
            "deleted_nodes_count": nodes_deleted_count, # Added deleted count
            "failed_node_ids": failed_node_ids_during_embedding 
        } 
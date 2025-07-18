
"""
Unified Knowledge Graph Service
"""
import logging
import sqlite3
import json
import threading
import time
from typing import Dict, List, Any, Optional, Tuple, TYPE_CHECKING
from dataclasses import dataclass, field
from enum import Enum
from datetime import datetime

# Ensure numpy is available
try:
    import numpy as np
    if TYPE_CHECKING:
        from numpy import ndarray
except ImportError:
    np = None
    if TYPE_CHECKING:
        from typing import Any as ndarray
    logging.getLogger("GraphService").error("NumPy library not found. Vector operations will be disabled.")

# Configure logger for this module
logger = logging.getLogger(__name__)

# --- Enums for Node and Relation Types ---
class NodeType(Enum):
    TEXT = "TEXT"
    ENTITY = "ENTITY"
    PERSON = "PERSON"
    ORGANIZATION = "ORGANIZATION"
    PROJECT = "PROJECT"
    CONCEPT = "CONCEPT"
    EVENT = "EVENT"
    TECHNOLOGY = "TECHNOLOGY"
    ALIAS = "ALIAS"
    TAG = "TAG"

class RelationType(Enum):
    HAS_ENTITY = "HAS_ENTITY"
    REF_TO = "REF_TO"
    IS_ALIAS_OF = "IS_ALIAS_OF"
    MENTIONS = "MENTIONS"
    AUTHORED_BY = "AUTHORED_BY"
    IMPLEMENTS = "IMPLEMENTS"
    CAUSES = "CAUSES"
    PART_OF = "PART_OF"
    IS_A = "IS_A"
    RELATED_TO = "RELATED_TO"

class GraphService:
    """
    Manages the storage and querying of the org-supertag knowledge graph,
    acting as a unified interface to the underlying SQLite database with vector support.
    """

    def __init__(self, db_path: str, config=None):
        """
        Initializes the GraphService.
        """
        self.db_path = db_path
        self.config = config
        self.logger = logging.getLogger(__name__)
        self.has_vector_ext = False
        self.local = threading.local()
        
        self._init_connection()
        
        try:
            self._init_db()
        except sqlite3.OperationalError as e:
            if "database is locked" in str(e).lower():
                self.logger.warning("Database locked during initialization, attempting to force unlock...")
                if self.force_unlock_database():
                    self.logger.info("Database unlocked, retrying initialization...")
                    self._init_connection()
                    self._init_db()
                else:
                    raise Exception("Failed to unlock database during initialization") from e
            else:
                raise

    # --- Connection and Initialization ---

    def _init_connection(self):
        """Initializes a database connection for the current thread."""
        if not hasattr(self.local, 'conn') or self.local.conn is None:
            self.logger.debug(f"Creating new database connection for thread {threading.get_ident()}")
            self.local.conn = self._create_connection()
        return self.local.conn

    def _create_connection(self) -> sqlite3.Connection:
        """Creates and configures a new SQLite connection."""
        conn = sqlite3.connect(self.db_path, check_same_thread=False, timeout=30.0)
        
        conn.execute("PRAGMA journal_mode=WAL")
        conn.execute("PRAGMA synchronous=NORMAL")
        
        conn.enable_load_extension(True)
        try:
            import sqlite_vec
            sqlite_vec.load(conn)
            self.has_vector_ext = True
            version = conn.execute('SELECT vec_version()').fetchone()[0]
            self.logger.info(f"sqlite-vec extension v{version} loaded successfully.")
        except (ImportError, sqlite3.OperationalError) as e:
            self.logger.warning(f"Failed to load sqlite-vec extension: {e}. Vector search will be disabled.")
            self.has_vector_ext = False
        conn.enable_load_extension(False)
        return conn

    def _get_connection(self) -> sqlite3.Connection:
        """Retrieves the database connection for the current thread."""
        return self._init_connection()

    def _init_db(self):
        """Initializes the database schema, being idempotent."""
        max_retries = 5
        retry_delay = 1.0
        
        for attempt in range(max_retries):
            try:
                conn = self._get_connection()
                with conn:
                    cursor = conn.cursor()
                    cursor.execute("PRAGMA journal_mode=WAL")
                    
                    # Nodes Table
                    cursor.execute("""
                    CREATE TABLE IF NOT EXISTS nodes (
                        node_id TEXT PRIMARY KEY,
                        type TEXT NOT NULL,
                        name TEXT,
                        content TEXT,
                        title TEXT,
                        file_path TEXT,
                        pos INTEGER,
                        olp TEXT,
                        level INTEGER,
                        scheduled TEXT,
                        deadline TEXT,
                        todo TEXT,
                        priority TEXT,
                        modified_at TEXT,
                        knowledge_status TEXT,
                        properties TEXT,
                        raw_value TEXT,
                        hash TEXT,
                        content_hash TEXT,
                        document_date TEXT,
                        relations_inferred_at TEXT,
                        aliases TEXT,
                        priority_score REAL
                    )
                    """)
                    cursor.execute("CREATE INDEX IF NOT EXISTS idx_nodes_type ON nodes (type)")
                    cursor.execute("CREATE INDEX IF NOT EXISTS idx_nodes_name ON nodes (name)")
                    # 如果数据库已存在于旧版本，确保 knowledge_status 列存在
                    cursor.execute("PRAGMA table_info(nodes)")
                    existing_cols = [row[1] for row in cursor.fetchall()]
                    if 'knowledge_status' not in existing_cols:
                        self.logger.info("Adding missing column 'knowledge_status' to existing nodes table…")
                        cursor.execute("ALTER TABLE nodes ADD COLUMN knowledge_status TEXT")

                    # 添加索引（在确保列存在之后）
                    cursor.execute("CREATE INDEX IF NOT EXISTS idx_nodes_knowledge_status ON nodes (knowledge_status)")

                    # Relations Table
                    cursor.execute("""
                    CREATE TABLE IF NOT EXISTS relations (
                        relation_id TEXT PRIMARY KEY,
                        source_id TEXT NOT NULL,
                        target_id TEXT NOT NULL,
                        type TEXT NOT NULL,
                        weight REAL DEFAULT 1.0,
                        properties TEXT,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        FOREIGN KEY (source_id) REFERENCES nodes (node_id) ON DELETE CASCADE,
                        FOREIGN KEY (target_id) REFERENCES nodes (node_id) ON DELETE CASCADE
                    )
                    """)
                    cursor.execute("CREATE INDEX IF NOT EXISTS idx_relations_source_id ON relations (source_id)")
                    cursor.execute("CREATE INDEX IF NOT EXISTS idx_relations_target_id ON relations (target_id)")
                    
                    # VSS Table
                    if self.has_vector_ext:
                        vector_dim = self.config.get_vector_dimension_for_model()
                        cursor.execute(f"""
                        CREATE VIRTUAL TABLE IF NOT EXISTS node_embeddings_vss USING vec0(
                            embedding FLOAT[{vector_dim}],
                            node_id TEXT
                        )
                        """
                        )
                        cursor.execute(f"""
                        CREATE VIRTUAL TABLE IF NOT EXISTS entity_embeddings_vss USING vec0(
                            embedding FLOAT[{vector_dim}],
                            entity_id TEXT
                        )
                        """
                        )
                    
                self.logger.info("Database schema initialized.")
                break
                
            except sqlite3.OperationalError as e:
                if "database is locked" in str(e).lower() and attempt < max_retries - 1:
                    self.logger.warning(f"Database locked, retrying... ({attempt + 1})")
                    time.sleep(retry_delay)
                    retry_delay *= 2
                    continue
                else:
                    self.logger.error(f"Failed to initialize database: {e}", exc_info=True)
                    raise

    # --- Schema Helpers ---

    def get_table_columns(self, table_name: str) -> List[str]:
        """Gets a list of column names for a given table."""
        conn = self._get_connection()
        cursor = conn.cursor()
        cursor.execute(f"PRAGMA table_info({table_name})")
        return [row[1] for row in cursor.fetchall()]

    # --- Knowledge-status helpers ---

    def update_node_status(self, node_id: str, status: str):
        """Updates the knowledge_status of a node."""
        conn = self._get_connection()
        with conn:
            conn.execute(
                "UPDATE nodes SET knowledge_status = ?, modified_at = ? WHERE node_id = ?",
                (status, datetime.now().isoformat(), node_id),
            )

    def get_nodes_by_status(self, status: str, limit: int = 10) -> List[str]:
        """Returns up to <limit> node IDs with the given knowledge_status."""
        conn = self._get_connection()
        cursor = conn.cursor()
        cursor.execute(
            "SELECT node_id FROM nodes WHERE knowledge_status = ? ORDER BY priority_score DESC, modified_at DESC LIMIT ?",
            (status, limit),
        )
        return [row[0] for row in cursor.fetchall()]

    # --- Node and Relation Operations ---
    
    def upsert_entity(self, entity_data: Dict[str, Any]):
        """
        Inserts or updates an entity (node or tag), handling both dict and list-of-pairs input,
        including nested structures like the 'properties' field.
        """
        # --- Robust Data Normalization (Level 1: Top-level entity object) ---
        if isinstance(entity_data, list):
            try:
                entity_data = {str(item[0]): item[1] for item in entity_data if isinstance(item, (list, tuple)) and len(item) == 2}
            except (TypeError, IndexError) as e:
                self.logger.error(f"Failed to convert top-level entity alist to dict: {entity_data}. Error: {e}")
                return

        if not isinstance(entity_data, dict):
            self.logger.error(f"upsert_entity expects a dictionary, but received type {type(entity_data)}.")
            return

        # --- Robust Data Normalization (Level 2: Nested 'properties' field) ---
        props = entity_data.get('properties')
        if isinstance(props, list):
            try:
                entity_data['properties'] = {str(item[0]): item[1] for item in props if isinstance(item, (list, tuple)) and len(item) == 2}
            except (TypeError, IndexError) as e:
                self.logger.error(f"Failed to convert nested properties alist to dict: {props}. Error: {e}")
                # Continue with empty properties instead of failing the whole entity
                entity_data['properties'] = {}

        # Map 'id' to 'node_id' for consistency
        if 'id' in entity_data and 'node_id' not in entity_data:
            entity_data['node_id'] = entity_data['id']
            
        node_id = entity_data.get('node_id')
        if not node_id:
            self.logger.warning(f"Skipping entity upsert due to missing 'node_id'. Data: {entity_data}")
            return

        # --- Default knowledge_status handling ---
        # If the incoming entity represents a node (not a tag, etc.) and the caller
        # 没有显式设置 knowledge_status，则默认将其标记为 "PENDING"，
        # 这样它会被后台知识提取流程自动拾取。
        if entity_data.get('type') == 'node' and 'knowledge_status' not in entity_data:
            entity_data['knowledge_status'] = 'PENDING'

        conn = self._get_connection()
        with conn:
            cursor = conn.cursor()
            cursor.execute("SELECT properties FROM nodes WHERE node_id = ?", (node_id,))
            existing = cursor.fetchone()

            if existing:
                # UPDATE
                existing_props = json.loads(existing[0] or '{}')
                new_props = entity_data.get('properties', {})

                # --- Final Data Integrity Check ---
                # Ensure both existing and new props are dictionaries before merging.
                # This handles historical data that might have been stored incorrectly.
                if isinstance(existing_props, list):
                    try:
                        existing_props = {str(item[0]): item[1] for item in existing_props if isinstance(item, (list, tuple)) and len(item) == 2}
                    except (TypeError, IndexError):
                        existing_props = {} # Default to empty dict on failure
                
                if isinstance(new_props, list):
                    try:
                        new_props = {str(item[0]): item[1] for item in new_props if isinstance(item, (list, tuple)) and len(item) == 2}
                    except (TypeError, IndexError):
                        new_props = {} # Default to empty dict on failure

                # Merge descriptions if both exist
                new_desc = new_props.get('description')
                if new_desc and existing_props.get('description'):
                    new_props['description'] = f"{existing_props.get('description')}\n---\n{new_desc}"
                
                merged_props = {**existing_props, **new_props}
                entity_data['properties'] = json.dumps(merged_props)

                valid_columns = self.get_table_columns('nodes')
                update_clauses = [f"`{k}` = ?" for k in entity_data if k != 'node_id' and k in valid_columns]
                
                update_values = []
                for k, v in entity_data.items():
                    if k != 'node_id' and k in valid_columns:
                        if isinstance(v, (dict, list)) and k != 'properties':
                             update_values.append(json.dumps(v))
                        else:
                            update_values.append(v)
                
                if not update_clauses: return
                update_values.append(node_id)
                sql = f"UPDATE nodes SET {', '.join(update_clauses)} WHERE node_id = ?"
                cursor.execute(sql, tuple(update_values))

            else:
                # INSERT
                props = entity_data.get('properties', {})
                if isinstance(props, list):
                    try:
                        entity_data['properties'] = {str(item[0]): item[1] for item in props if isinstance(item, (list, tuple)) and len(item) == 2}
                    except (TypeError, IndexError):
                        entity_data['properties'] = {}
                
                valid_columns = self.get_table_columns('nodes')
                cols = [c for c in valid_columns if c in entity_data]
                vals = []
                for c in cols:
                    v = entity_data.get(c)
                    if isinstance(v, (dict, list)):
                        vals.append(json.dumps(v))
                    else:
                        vals.append(v)
                
                placeholders = ', '.join(['?' for _ in cols])
                sql = f"INSERT INTO nodes ({', '.join(f'`{c}`' for c in cols)}) VALUES ({placeholders})"
                cursor.execute(sql, tuple(vals))

    def upsert_relationship(self, source_id: str, target_id: str, type: str, properties: Optional[Dict[str, Any]] = None):
        """Inserts or updates a relationship, merging properties."""
        properties = properties or {}
        conn = self._get_connection()
        relation_id = f"{source_id}-{type}-{target_id}"
        properties.setdefault('weight', 1.0)
        
        with conn:
            cursor = conn.cursor()
            cursor.execute("SELECT properties, weight FROM relations WHERE relation_id = ?", (relation_id,))
            existing = cursor.fetchone()

            if existing:
                # UPDATE
                existing_props = json.loads(existing[0] or '{}')
                existing_weight = existing[1]
                
                final_weight = existing_weight + properties.get('weight', 1.0)
                merged_props = {**existing_props, **properties}

                sql = "UPDATE relations SET weight = ?, properties = ? WHERE relation_id = ?"
                cursor.execute(sql, (final_weight, json.dumps(merged_props, indent=2), relation_id))

            else:
                # INSERT
                sql = "INSERT INTO relations (relation_id, source_id, target_id, type, weight, properties) VALUES (?, ?, ?, ?, ?, ?)"
                props_json = json.dumps(properties, indent=2)
                cursor.execute(sql, (relation_id, source_id, target_id, type, properties['weight'], props_json))

            # --- Tag Incremental Update ---
            if type == "HAS_TAG":
                cursor.execute("SELECT type FROM nodes WHERE node_id = ?", (target_id,))
                row = cursor.fetchone()
                if row and row[0] == "TAG":
                    self.update_node_status(target_id, "STALE")

    def mark_node_relations_inferred(self, node_id: str):
        """Marks a node to indicate that its relations have been inferred."""
        conn = self._get_connection()
        with conn:
            cursor = conn.cursor()
            cursor.execute("UPDATE nodes SET relations_inferred_at = ? WHERE node_id = ?", (datetime.now().isoformat(), node_id))

    def get_nodes_needing_relation_inference(self, limit: int = 5) -> List[str]:
        """Fetches node IDs that haven't had their relations inferred yet."""
        conn = self._get_connection()
        cursor = conn.cursor()
        query = "SELECT node_id FROM nodes WHERE relations_inferred_at IS NULL ORDER BY priority_score DESC, modified_at DESC LIMIT ?"
        cursor.execute(query, (limit,))
        return [row[0] for row in cursor.fetchall()]

    def _format_node_output(self, node_row: sqlite3.Row) -> Optional[Dict[str, Any]]:
        """Formats a database row into a node dictionary."""
        if not node_row: return None
        node_dict = dict(node_row)
        if node_dict.get('properties'):
            try:
                node_dict['properties'] = json.loads(node_dict['properties'])
            except (json.JSONDecodeError, TypeError):
                node_dict['properties'] = {}
        return node_dict

    def get_node_by_id(self, node_id: str) -> Optional[Dict[str, Any]]:
        """Retrieves a single node by its ID."""
        conn = self._get_connection()
        conn.row_factory = sqlite3.Row
        cursor = conn.cursor()
        cursor.execute("SELECT * FROM nodes WHERE node_id = ?", (node_id,))
        node_row = cursor.fetchone()
        conn.row_factory = None
        return self._format_node_output(node_row)

    def get_nodes_by_ids(self, node_ids: List[str]) -> List[Dict[str, Any]]:
        """Retrieves a list of nodes by their IDs."""
        if not node_ids: return []
        conn = self._get_connection()
        conn.row_factory = sqlite3.Row
        placeholders = ','.join('?' for _ in node_ids)
        cursor = conn.cursor()
        cursor.execute(f"SELECT * FROM nodes WHERE node_id IN ({placeholders})", node_ids)
        results = [self._format_node_output(row) for row in cursor.fetchall()]
        conn.row_factory = None
        return results

    def delete_nodes_by_ids(self, node_ids: List[str]):
        """Deletes one or more nodes and their associated data."""
        if not node_ids: return
        conn = self._get_connection()
        with conn:
            placeholders = ','.join('?' for _ in node_ids)
            conn.execute(f"DELETE FROM nodes WHERE node_id IN ({placeholders})", node_ids)
            if self.has_vector_ext:
                # 清理两个嵌入表以确保完整性
                conn.execute(f"DELETE FROM node_embeddings_vss WHERE node_id IN ({placeholders})", node_ids)
                conn.execute(f"DELETE FROM entity_embeddings_vss WHERE entity_id IN ({placeholders})", node_ids)

    # --- Vector Operations ---

    def get_node_embedding_by_id(self, node_id: str) -> Optional["ndarray"]:
        """Retrieves the vector embedding for a single node."""
        if not self.has_vector_ext or not np: return None
        conn = self._get_connection()
        cursor = conn.cursor()
        cursor.execute("SELECT embedding FROM node_embeddings_vss WHERE node_id = ?", (node_id,))
        result = cursor.fetchone()
        return np.frombuffer(result[0], dtype=np.float32) if result and result[0] and isinstance(result[0], bytes) else None

    def find_similar_nodes(self, query_embedding: List[float], top_k=10) -> List[Dict[str, Any]]:
        """Finds nodes with embeddings similar to the query embedding."""
        if not self.has_vector_ext or not np: return []
        query_embedding_np = np.array(query_embedding, dtype=np.float32)
        conn = self._get_connection()
        cursor = conn.cursor()
        # 使用 entity_embeddings_vss 表进行统一搜索
        try:
            # 使用正确的 sqlite-vec KNN 查询语法
            cursor.execute(
                "SELECT entity_id, distance FROM entity_embeddings_vss WHERE embedding MATCH ? AND k = ?",
                (query_embedding_np.tobytes(), top_k)
            )
        except sqlite3.OperationalError as e:
            self.logger.error(f"Vector search failed: {e}")
            return []
        
        similar_entities_with_distance = cursor.fetchall()
        similar_entity_ids = [row[0] for row in similar_entities_with_distance]
        # 修复：使用 entity_id 作为键，因为在统一嵌入系统中，entity_id 就是 node_id
        full_entity_data_map = {entity['node_id']: entity for entity in self.get_nodes_by_ids(similar_entity_ids)}
        results = []
        for entity_id, distance in similar_entities_with_distance:
            # 修复：在统一嵌入系统中，entity_id 就是 node_id
            if entity_id in full_entity_data_map:
                entity_data = full_entity_data_map[entity_id]
                entity_data['distance'] = distance
                results.append(entity_data)
        return results

    def upsert_node_embedding(self, node_id: str, embedding: "ndarray"):
        """Inserts or updates a node's vector embedding."""
        if not self.has_vector_ext or not np: return
        conn = self._get_connection()
        with conn:
            cursor = conn.cursor()
            cursor.execute("REPLACE INTO node_embeddings_vss (node_id, embedding) VALUES (?, ?)", (node_id, embedding.tobytes()))

    def get_entity_embedding_by_id(self, entity_id: str) -> Optional["ndarray"]:
        """Retrieves the vector embedding for a single entity."""
        if not self.has_vector_ext or not np: return None
        conn = self._get_connection()
        cursor = conn.cursor()
        cursor.execute("SELECT embedding FROM entity_embeddings_vss WHERE entity_id = ?", (entity_id,))
        result = cursor.fetchone()
        return np.frombuffer(result[0], dtype=np.float32) if result and result[0] and isinstance(result[0], bytes) else None

    def upsert_entity_embedding(self, entity_id: str, embedding: "ndarray"):
        """Inserts or updates an entity's description vector embedding."""
        if not self.has_vector_ext or not np: return
        conn = self._get_connection()
        with conn:
            cursor = conn.cursor()
            cursor.execute("REPLACE INTO entity_embeddings_vss (entity_id, embedding) VALUES (?, ?)", (entity_id, embedding.tobytes()))

    def find_similar_entities(self, query_embedding: List[float], top_k=5) -> List[Dict[str, Any]]:
        """Finds entities with descriptions similar to the query embedding."""
        if not self.has_vector_ext or not np: return []
        query_embedding_np = np.array(query_embedding, dtype=np.float32)
        conn = self._get_connection()
        cursor = conn.cursor()
        try:
            # 使用正确的 sqlite-vec KNN 查询语法
            cursor.execute(
                "SELECT entity_id, distance FROM entity_embeddings_vss WHERE embedding MATCH ? AND k = ?",
                (query_embedding_np.tobytes(), top_k)
            )
        except sqlite3.OperationalError as e:
            self.logger.error(f"Vector search failed: {e}")
            return []
        
        similar_entities_with_distance = cursor.fetchall()
        similar_entity_ids = [row[0] for row in similar_entities_with_distance]
        # 获取完整的实体数据并添加距离信息
        full_entity_data_map = {entity['node_id']: entity for entity in self.get_nodes_by_ids(similar_entity_ids)}
        results = []
        for entity_id, distance in similar_entities_with_distance:
            if entity_id in full_entity_data_map:
                entity_data = full_entity_data_map[entity_id]
                entity_data['distance'] = distance
                results.append(entity_data)
        return results

    # --- Graph Traversal and Querying ---



    def get_neighbors(self, node_id: str, relation_type: Optional[str] = None) -> List[Dict[str, Any]]:
        """Retrieves neighboring nodes connected by a specific relation type."""
        conn = self._get_connection()
        cursor = conn.cursor()
        if relation_type:
            cursor.execute("SELECT target_id FROM relations WHERE source_id = ? AND type = ?", (node_id, relation_type))
        else:
            cursor.execute("SELECT target_id FROM relations WHERE source_id = ?", (node_id,))
        neighbor_ids = [row[0] for row in cursor.fetchall()]
        return self.get_nodes_by_ids(neighbor_ids)

    def get_nodes_linked_to_tag(self, tag_id: str) -> List[Dict[str, Any]]:
        """
        Retrieves all nodes that are linked from a specific tag.
        This is primarily used to gather context for embedding a tag concept.
        """
        conn = self._get_connection()
        # This query finds all nodes that are the 'source' of a link where the tag is the 'target'.
        # This corresponds to a node having a tag.
        cursor = conn.cursor()
        cursor.execute("""
            SELECT n.*
            FROM nodes n
            JOIN relations r ON n.node_id = r.source_id
            WHERE r.target_id = ? AND r.type = 'HAS_TAG'
        """, (tag_id,))
        
        node_rows = cursor.fetchall()
        
        # Since the schema is unified, we need to re-establish the row factory to parse rows into dicts.
        conn.row_factory = sqlite3.Row
        cursor = conn.cursor()
        cursor.execute("""
            SELECT n.*
            FROM nodes n
            JOIN relations r ON n.node_id = r.source_id
            WHERE r.target_id = ? AND r.type = 'HAS_TAG'
        """, (tag_id,))
        
        results = [self._format_node_output(row) for row in cursor.fetchall()]
        conn.row_factory = None # Reset row factory
        return results

    def search_nodes_by_title_content(self, search_query: str, limit: int = 10) -> List[Dict[str, Any]]:
        """Performs a full-text search on node titles and content. Also finds nodes linked to matching tags."""
        conn = self._get_connection()
        conn.row_factory = sqlite3.Row  # Enable row -> dict conversion
        like_query = f"%{search_query}%"
        cursor = conn.cursor()
        # 1) direct match on title / content
        cursor.execute(
            "SELECT * FROM nodes WHERE title LIKE ? OR content LIKE ? ORDER BY modified_at DESC LIMIT ?",
            (like_query, like_query, limit)
        )
        rows = cursor.fetchall()
        results: List[Dict[str, Any]] = [self._format_node_output(row) for row in rows if row]

        # 2) if not enough, search by tag linkage
        if len(results) < limit:
            collected_ids = {n['node_id'] for n in results}
            tag_limit = limit * 2  # fetch extra for deduplication buffer
            cursor.execute(
                """
                SELECT n.*
                FROM nodes n
                JOIN relations r ON n.node_id = r.source_id
                JOIN nodes t ON t.node_id = r.target_id
                WHERE r.type = 'HAS_TAG'
                  AND (t.title LIKE ? OR t.name LIKE ? OR t.node_id LIKE ?)
                LIMIT ?
                """,
                (like_query, like_query, like_query, tag_limit)
            )
            tag_rows = cursor.fetchall()
            for row in tag_rows:
                node = self._format_node_output(row)
                if node and node['node_id'] not in collected_ids:
                    results.append(node)
                    collected_ids.add(node['node_id'])
                    if len(results) >= limit:
                        break
        conn.row_factory = None  # Reset row factory
        return results

    def get_node_with_neighbors(self, node_id: str) -> Optional[Dict[str, Any]]:
        """Retrieves a node along with its neighbors."""
        node = self.get_node_by_id(node_id)
        if not node: return None
        conn = self._get_connection()
        cursor = conn.cursor()
        cursor.execute("SELECT source_id, type FROM relations WHERE target_id = ?", (node_id,))
        parents = [{"node_id": row[0], "relation_type": row[1]} for row in cursor.fetchall()]
        cursor.execute("SELECT target_id, type FROM relations WHERE source_id = ?", (node_id,))
        children = [{"node_id": row[0], "relation_type": row[1]} for row in cursor.fetchall()]
        node['relations'] = {
            'parents': self.get_nodes_by_ids([p['node_id'] for p in parents]),
            'children': self.get_nodes_by_ids([c['node_id'] for c in children]),
        }
        return node
        
    # --- Database Maintenance and Stats ---

    def get_stats(self) -> Dict[str, Any]:
        """Retrieves statistics about the graph."""
        conn = self._get_connection()
        cursor = conn.cursor()
        stats = {}
        cursor.execute("SELECT COUNT(*) FROM nodes")
        stats['node_count'] = cursor.fetchone()[0]
        cursor.execute("SELECT COUNT(*) FROM relations")
        stats['relation_count'] = cursor.fetchone()[0]
        cursor.execute("SELECT type, COUNT(*) FROM nodes GROUP BY type")
        stats['nodes_by_type'] = dict(cursor.fetchall())
        cursor.execute("SELECT type, COUNT(*) FROM relations GROUP BY type")
        stats['relations_by_type'] = dict(cursor.fetchall())
        return stats



    def close(self):
        """Safely closes the database connection for the current thread."""
        if hasattr(self, 'local') and hasattr(self.local, 'conn') and self.local.conn:
            self.logger.debug(f"Closing database connection for thread {threading.get_ident()}")
            self.local.conn.close()
            self.local.conn = None

    def force_unlock_database(self):
        """
        Forces the database to unlock by clearing the WAL journal file.
        WARNING: This is a last resort and can lead to data corruption if the
        database is actively being written to by another process.
        """
        try:
            wal_path = f"{self.db_path}-wal"
            shm_path = f"{self.db_path}-shm"

            self.close()

            import os
            if os.path.exists(wal_path):
                os.remove(wal_path)
                self.logger.info(f"Removed WAL file: {wal_path}")
            if os.path.exists(shm_path):
                os.remove(shm_path)
                self.logger.info(f"Removed SHM file: {shm_path}")
                
            return True
        except Exception as e:
            self.logger.error(f"Failed to force unlock database: {e}", exc_info=True)
            return False

    def __del__(self):
        """Ensures the connection is closed when the object is destroyed."""
        self.close()

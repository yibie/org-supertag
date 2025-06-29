"""
Unified Knowledge Graph Service
"""
import logging
import sqlite3
import numpy as np
import json
import threading
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass, field
from enum import Enum
from datetime import datetime

# Ensure numpy is available
NP_AVAILABLE = False
try:
    import numpy as np
    NP_AVAILABLE = True
except ImportError:
    logging.getLogger("GraphService").error("NumPy library not found. Vector operations will be disabled.")

# Configure logger for this module
logger = logging.getLogger(__name__)

# --- Data Classes (can be moved to a separate file later) ---

class TagStatus(Enum):
    """Tag governance status types."""
    DRAFT = "draft"
    REVIEW = "review"
    ACTIVE = "active"
    DEPRECATED = "deprecated"
    MERGED = "merged"
    SPLIT = "split"
    ARCHIVED = "archived"

@dataclass
class Rule:
    """Represents a governance rule."""
    type: str # RuleType enum can be used here
    rule: Dict[str, Any]
    description: Optional[str] = None
    created_at: datetime = field(default_factory=datetime.now)

@dataclass
class HistoryEntry:
    """Represents a history entry for status or type changes."""
    timestamp: datetime
    old_value: Any
    new_value: Any
    reason: Optional[str] = None

class GraphService:
    """
    Manages the storage and querying of the org-supertag knowledge graph,
    acting as a unified interface to the underlying SQLite database with vector support.
    """

    def __init__(self, db_path: str, config=None):
        """
        Initializes the GraphService.

        Args:
            db_path: The path to the SQLite database file.
            config: Optional configuration object.
        """
        self.db_path = db_path
        self.config = config
        self.logger = logging.getLogger(__name__)
        self.has_vector_ext = False
        self.local = threading.local()
        
        self._init_connection()
        
        # Try to initialize database, with automatic unlock if needed
        try:
            self._init_db()
        except sqlite3.OperationalError as e:
            if "database is locked" in str(e).lower():
                self.logger.warning("Database locked during initialization, attempting to force unlock...")
                if self.force_unlock_database():
                    self.logger.info("Database unlocked, retrying initialization...")
                    self._init_connection()  # Reconnect
                    self._init_db()  # Retry initialization
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
        
        # Configure SQLite for better concurrency
        conn.execute("PRAGMA journal_mode=WAL")
        conn.execute("PRAGMA synchronous=NORMAL")
        conn.execute("PRAGMA cache_size=10000")
        conn.execute("PRAGMA temp_store=memory")
        conn.execute("PRAGMA mmap_size=268435456")  # 256MB
        
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

    def _add_column_if_not_exists(self, cursor, table_name, column_name, column_type):
        """Adds a column to a table if it doesn't already exist."""
        cursor.execute(f"PRAGMA table_info({table_name})")
        columns = [info[1] for info in cursor.fetchall()]
        if column_name not in columns:
            cursor.execute(f"ALTER TABLE {table_name} ADD COLUMN {column_name} {column_type}")
            self.logger.info(f"Added column '{column_name}' to table '{table_name}'.")

    def _init_db(self):
        """
        Initializes the database schema, creating tables if they don't exist.
        This method is designed to be idempotent.
        """
        import time
        max_retries = 5
        retry_delay = 1.0
        
        for attempt in range(max_retries):
            try:
                conn = self._get_connection()
                cursor = conn.cursor()
                
                # Enable WAL mode if not already enabled
                cursor.execute("PRAGMA journal_mode=WAL")
                
                # 1. Create Nodes Table if not exists
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
                    properties TEXT,
                    raw_value TEXT,
                    hash TEXT,
                    content_hash TEXT,
                    document_date TEXT,
                    relations_inferred_at TEXT
                    -- UNIQUE(name, type) -- This constraint might be too restrictive for entities, remove for now
                )
                """)
                cursor.execute("CREATE INDEX IF NOT EXISTS idx_nodes_type ON nodes (type)")
                cursor.execute("CREATE INDEX IF NOT EXISTS idx_nodes_name ON nodes (name)")

                # 2. Add new columns to nodes table if they don't exist (Migration)
                # Add 'aliases' column
                self._add_column_if_not_exists(cursor, "nodes", "aliases", "TEXT")
                # Add 'priority_score' column
                self._add_column_if_not_exists(cursor, "nodes", "priority_score", "REAL")

                # 3. Create Relations Table if not exists
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
                cursor.execute("CREATE INDEX IF NOT EXISTS idx_relations_type ON relations (type)")

                # 4. Create VSS Table for node embeddings if not exists
                if self.has_vector_ext:
                    vector_dim = self.config.get_vector_dimension_for_model()
                    
                    cursor.execute(f"""
                    CREATE VIRTUAL TABLE IF NOT EXISTS node_embeddings_vss USING vec0(
                        node_id TEXT PRIMARY KEY,
                        embedding FLOAT[{vector_dim}]
                    )
                    """)
                
                conn.commit()
                self.logger.info("Database schema initialized/migrated for heterogenous graph structure.")
                break  # Success, exit retry loop
                
            except sqlite3.OperationalError as e:
                if "database is locked" in str(e).lower() and attempt < max_retries - 1:
                    self.logger.warning(f"Database locked, retrying in {retry_delay}s (attempt {attempt + 1}/{max_retries})")
                    time.sleep(retry_delay)
                    retry_delay *= 2  # Exponential backoff
                    continue
                else:
                    self.logger.error(f"Failed to initialize database schema after {max_retries} attempts: {e}")
                    raise
            except Exception as e:
                self.logger.error(f"Unexpected error during database initialization: {e}")
                raise

    def _add_column_if_not_exists(self, cursor, table_name, column_name, column_type):
        """Adds a column to a table if it doesn't already exist."""
        cursor.execute(f"PRAGMA table_info({table_name})")
        columns = [info[1] for info in cursor.fetchall()]
        if column_name not in columns:
            cursor.execute(f"ALTER TABLE {table_name} ADD COLUMN {column_name} {column_type}")
            self.logger.info(f"Added column '{column_name}' to table '{table_name}'.")

    def mark_node_relations_inferred(self, node_id: str):
        """Marks a node to indicate that relation inference has been performed."""
        conn = self._get_connection()
        try:
            timestamp = datetime.now().isoformat()
            with conn:
                conn.execute(
                    "UPDATE nodes SET relations_inferred_at = ? WHERE node_id = ?",
                    (timestamp, node_id)
                )
            logger.debug(f"Marked node {node_id} as having relations inferred at {timestamp}.")
        except Exception as e:
            logger.error(f"Failed to mark node {node_id} for relation inference: {e}", exc_info=True)

    def get_nodes_needing_relation_inference(self, limit: int = 5, order_by: Optional[str] = None, order_direction: str = "ASC", min_priority_score: Optional[float] = None) -> List[str]:
        """
        Retrieves a list of node IDs that have embeddings but have not yet
        had relation inference performed on them.
        """
        conn = self._get_connection()
        try:
            with conn:
                cursor = conn.cursor()
                query = """
                    SELECT node_id FROM nodes
                    WHERE type = 'TEXT'
                      AND content IS NOT NULL AND content != ''
                      AND relations_inferred_at IS NULL
                """
                params = []

                if min_priority_score is not None:
                    query += " AND priority_score >= ?"
                    params.append(min_priority_score)

                if order_by:
                    # Basic validation to prevent SQL injection
                    if order_by not in ["node_id", "modified_at", "document_date", "priority_score"]:
                        self.logger.warning(f"Invalid order_by column: {order_by}. Ignoring order_by.")
                        order_by = None
                    if order_direction.upper() not in ["ASC", "DESC"]:
                        self.logger.warning(f"Invalid order_direction: {order_direction}. Defaulting to ASC.")
                        order_direction = "ASC"

                    if order_by:
                        query += f" ORDER BY {order_by} {order_direction}"

                query += " LIMIT ?"
                params.append(limit)

                cursor.execute(query, params)
                rows = cursor.fetchall()
                node_ids = [row[0] for row in rows]
                logger.info(f"Found {len(node_ids)} nodes needing relation inference (limit: {limit}).")
                return node_ids
        except Exception as e:
            logger.error(f"Failed to get nodes needing relation inference: {e}", exc_info=True)
            return []

    # --- Public API ---

    # --- Node Operations ---

    def upsert_text_node(self, node_data: Dict[str, Any]):
        """
        Upserts a single text node, its associated entities (tags), and its references.
        This is the primary method for adding content to the graph.
        """
        conn = self._get_connection()
        try:
            # 1. Prepare and separate data
            text_node_id = node_data['node_id']
            tags = node_data.pop('tags', []) # Extract tags
            references_to_raw = node_data.pop('ref_to', []) # Extract references

            # The 'ref_to' data might be a JSON string, so we need to parse it.
            references_to = []
            if isinstance(references_to_raw, str):
                try:
                    parsed_refs = json.loads(references_to_raw)
                    if isinstance(parsed_refs, list):
                        references_to = parsed_refs
                except json.JSONDecodeError:
                    self.logger.warning(f"Could not parse 'ref_to' JSON string for node {text_node_id}: {references_to_raw}")
            elif isinstance(references_to_raw, list):
                references_to = references_to_raw
            
            node_to_upsert = {**node_data, 'type': 'TEXT'}
            self._upsert_nodes_internal([node_to_upsert])

            # 2. Process entities (tags)
            if tags:
                entity_nodes = []
                for tag_name in tags:
                    entity_id, entity_node = self._prepare_entity_node(tag_name)
                    entity_nodes.append(entity_node)
                
                # Upsert entity nodes
                self._upsert_nodes_internal(entity_nodes)

                # Create tag relations
                tag_relations = []
                for tag_name in tags:
                    entity_id = self._get_entity_id_by_name(tag_name)
                    if entity_id:
                        tag_relations.append({
                            'source_id': text_node_id,
                            'target_id': entity_id,
                            'type': 'HAS_ENTITY'
                        })
                
                if tag_relations:
                    self.bulk_upsert_relations(tag_relations)
            
            # 3. Process direct references (links)
            if references_to:
                ref_relations = []
                for target_id in references_to:
                    if isinstance(target_id, str) and target_id:
                        ref_relations.append({
                            'source_id': text_node_id,
                            'target_id': target_id,
                            'type': 'REF_TO'
                        })
                if ref_relations:
                    self.bulk_upsert_relations(ref_relations)

            conn.commit()
            self.logger.info(f"Successfully upserted node {text_node_id} with {len(tags)} entities and {len(references_to)} references.")

        except Exception as e:
            conn.rollback()
            self.logger.error(f"Failed to upsert text node {node_data.get('node_id')}: {e}", exc_info=True)
            raise
        finally:
            # Committing should happen here after all operations for the node are successful.
            # However, since this function is now part of a larger transaction in NodeProcessor,
            # we let the caller manage the commit/rollback.
            # For standalone use, a commit would be here.
            pass

    def bulk_upsert_entity_nodes(self, entities_data: List[Dict[str, str]]):
        """
        Bulk upserts entity nodes from a list of dictionaries containing name and description.
        """
        if not entities_data:
            return

        conn = self._get_connection()
        try:
            entity_nodes_to_upsert = []
            for entity_info in entities_data:
                name = entity_info.get('name')
                description = entity_info.get('description')
                if not name:
                    continue

                _, entity_node = self._prepare_entity_node(name)
                entity_node['properties'] = json.dumps({'description': description})
                entity_nodes_to_upsert.append(entity_node)
            
            if entity_nodes_to_upsert:
                self._upsert_nodes_internal(entity_nodes_to_upsert)
                # The commit is handled by the calling context (NodeProcessor)
                
        except Exception as e:
            self.logger.error(f"Failed during bulk entity node upsert: {e}", exc_info=True)
            # No rollback here, let the higher-level transaction manager handle it.
            raise

    def _prepare_entity_node(self, name: str) -> Tuple[str, Dict[str, Any]]:
        """Prepares a dictionary representing an ENTITY node."""
        if not isinstance(name, str) or not name.strip():
            # Return a placeholder for invalid input to avoid downstream errors
            invalid_name = f"INVALID_NAME_{datetime.now().isoformat()}"
            self.logger.warning(f"Invalid entity name provided (type: {type(name)}). Using placeholder: {invalid_name}")
            name = invalid_name

        # Normalize name to create a consistent ID
        normalized_name = name.strip().upper()
        entity_id = f"ENTITY_{normalized_name}"
        return entity_id, {
            "node_id": entity_id,
            "type": "ENTITY",
            "name": normalized_name,
            "title": name, # Store original casing for display
            "modified_at": datetime.now().isoformat()
        }

    def _get_entity_id_by_name(self, name: str) -> Optional[str]:
        """Gets an entity's node_id by its normalized name."""
        if not isinstance(name, str):
            self.logger.warning(f"_get_entity_id_by_name called with non-string argument: {name} (type: {type(name)}). This may indicate an issue with the LLM output. Returning None.")
            return None
        normalized_name = name.strip().upper()
        conn = self._get_connection()
        cursor = conn.cursor()
        cursor.execute("SELECT node_id FROM nodes WHERE type='ENTITY' AND name=?", (normalized_name,))
        result = cursor.fetchone()
        return result[0] if result else None

    def _format_node_output(self, node_row: sqlite3.Row) -> Optional[Dict[str, Any]]:
        """Formats a raw database row into a structured node dictionary."""
        if not node_row:
            return None
        
        node_dict = dict(node_row)
        
        # Deserialize JSON fields
        node_dict['properties'] = json.loads(node_dict.get('properties', '{}') or '{}')
        
        # For TEXT nodes, dynamically fetch associated tags from relations
        if node_dict.get('type') == 'TEXT':
            tags = self._get_tags_for_node(node_dict['node_id'])
            node_dict['tags'] = tags
        
        return node_dict

    def _get_tags_for_node(self, node_id: str) -> List[str]:
        """Retrieves all tag names associated with a given text node."""
        conn = self._get_connection()
        cursor = conn.cursor()
        sql = """
            SELECT n.title FROM relations r
            JOIN nodes n ON r.target_id = n.node_id
            WHERE r.source_id = ? AND r.type = 'HAS_ENTITY' AND n.type = 'ENTITY'
        """
        cursor.execute(sql, (node_id,))
        return [row[0] for row in cursor.fetchall()]

    def _upsert_nodes_internal(self, nodes_data: List[Dict[str, Any]]):
        """Helper to bulk upsert nodes, ensuring all required fields are present."""
        if not nodes_data:
            return

        conn = self._get_connection()
        cursor = conn.cursor()

        # Define all columns to ensure order
        columns = [
            'node_id', 'type', 'name', 'content', 'title', 'file_path', 'pos',
            'olp', 'level', 'scheduled', 'deadline', 'todo', 'priority',
            'modified_at', 'properties', 'raw_value', 'hash', 'content_hash',
            'document_date'
        ]

        nodes_to_upsert = []
        for node in nodes_data:
            # Ensure all columns have a value (default to None if missing)
            values = []
            for col in columns:
                val = node.get(col)
                # Convert list types to string for datetime fields
                if col in ['scheduled', 'deadline', 'modified_at', 'created_at', 'document_date'] and isinstance(val, list):
                    # Elisp time lists are typically [high low microsecs picosecs]
                    # For now, just convert to string representation
                    val = str(val) if val else None
                # Ensure olp is serialized as string if it's a list
                elif col == 'olp' and isinstance(val, list):
                    val = json.dumps(val)
                # Ensure properties is serialized as string
                elif col == 'properties' and isinstance(val, dict):
                    val = json.dumps(val)
                # Handle any other list types by converting to string
                elif isinstance(val, list):
                    val = json.dumps(val) if val else None
                values.append(val)
            nodes_to_upsert.append(tuple(values))

        # SQL for upserting
        placeholders = ', '.join(['?'] * len(columns))
        sql = f"""
            INSERT INTO nodes ({', '.join(columns)})
            VALUES ({placeholders})
            ON CONFLICT(node_id) DO UPDATE SET
                type=excluded.type,
                name=excluded.name,
                content=excluded.content,
                title=excluded.title,
                file_path=excluded.file_path,
                pos=excluded.pos,
                olp=excluded.olp,
                level=excluded.level,
                scheduled=excluded.scheduled,
                deadline=excluded.deadline,
                todo=excluded.todo,
                priority=excluded.priority,
                modified_at=excluded.modified_at,
                properties=excluded.properties,
                raw_value=excluded.raw_value,
                hash=excluded.hash,
                content_hash=excluded.content_hash,
                document_date=excluded.document_date
        """
        cursor.executemany(sql, nodes_to_upsert)

    def get_node_by_id(self, node_id: str) -> Optional[Dict[str, Any]]:
        """Retrieves a single node by its ID, including its tags."""
        conn = self._get_connection()
        conn.row_factory = sqlite3.Row
        cursor = conn.cursor()
        cursor.execute("SELECT * FROM nodes WHERE node_id = ?", (node_id,))
        row = cursor.fetchone()
        conn.row_factory = None # Reset row factory
        return self._format_node_output(row)

    def get_nodes_by_ids(self, node_ids: List[str]) -> List[Dict[str, Any]]:
        """Retrieves multiple nodes' metadata by their IDs."""
        if not node_ids:
            return []
        
        placeholders = ','.join('?' for _ in node_ids)
        query = f"SELECT * FROM nodes WHERE node_id IN ({placeholders})"
        
        conn = self._get_connection()
        cursor = conn.cursor()
        cursor.execute(query, node_ids)
        
        return [self._format_node_output(row) for row in cursor.fetchall()]

    def delete_nodes_by_ids(self, node_ids: List[str]):
        """
        Deletes a list of nodes and their associated VSS embeddings by their IDs.
        This operation is performed within a single transaction.
        Relationships are handled by the 'ON DELETE CASCADE' foreign key constraint.
        """
        if not node_ids:
            self.logger.info("delete_nodes_by_ids called with an empty list.")
            return

        conn = self._get_connection()
        cursor = conn.cursor()

        try:
            # 1. Get rowids of nodes to be deleted BEFORE deleting them.
            # This is crucial for cleaning up the VSS table.
            id_placeholders = ','.join('?' for _ in node_ids)
            rowid_query = f"SELECT rowid FROM nodes WHERE node_id IN ({id_placeholders})"
            cursor.execute(rowid_query, node_ids)
            rowids_to_delete = [row[0] for row in cursor.fetchall()]
            
            # 2. Delete from the main 'nodes' table.
            # The 'ON DELETE CASCADE' on the 'relations' table will automatically
            # clean up any relationships pointing to or from these nodes.
            delete_nodes_query = f"DELETE FROM nodes WHERE node_id IN ({id_placeholders})"
            cursor.execute(delete_nodes_query, node_ids)
            deleted_node_count = cursor.rowcount
            self.logger.info(f"Deleted {deleted_node_count} records from the 'nodes' table.")

            # 3. Delete from the VSS table using the collected rowids.
            if self.has_vector_ext and rowids_to_delete:
                vss_placeholders = ','.join('?' for _ in rowids_to_delete)
                delete_vss_query = f"DELETE FROM node_embeddings_vss WHERE rowid IN ({vss_placeholders})"
                cursor.execute(delete_vss_query, rowids_to_delete)
                self.logger.info(f"Deleted {cursor.rowcount} embeddings from the VSS table.")

            # 4. Commit the transaction.
            conn.commit()
            self.logger.info(f"Successfully deleted {deleted_node_count} nodes and their related data.")

        except Exception as e:
            conn.rollback()
            self.logger.error(f"Failed to delete nodes: {e}", exc_info=True)
            raise

    # --- Tag Operations ---
    def bulk_upsert_tags(self, tags_data: List[Dict[str, Any]]):
        """Bulk inserts or updates tag metadata."""
        if not tags_data:
            return

        conn = self._get_connection()
        cursor = conn.cursor()

        sql = """
            INSERT INTO tags (tag_id, name, description, modified_at)
            VALUES (?, ?, ?, ?)
            ON CONFLICT(tag_id) DO UPDATE SET
                name=excluded.name,
                description=excluded.description,
                modified_at=excluded.modified_at;
        """

        tags_to_upsert = [
            (
                tag.get('tag_id'), tag.get('name'), tag.get('description'),
                tag.get('modified_at', datetime.now().isoformat())
            )
            for tag in tags_data
        ]

        try:
            cursor.executemany(sql, tags_to_upsert)
            conn.commit()
            self.logger.info(f"Successfully upserted {len(tags_to_upsert)} tags.")
        except Exception as e:
            conn.rollback()
            self.logger.error(f"Failed to bulk upsert tags: {e}", exc_info=True)
            raise

    def get_tag_by_name(self, name: str) -> Optional[Dict[str, Any]]:
        """Retrieves a single tag by its name (case-insensitive)."""
        conn = self._get_connection()
        conn.row_factory = sqlite3.Row
        cursor = conn.cursor()
        cursor.execute("SELECT * FROM tags WHERE name = ?", (name,))
        row = cursor.fetchone()
        conn.row_factory = None
        return dict(row) if row else None

    # --- Relation Operations ---
    def bulk_upsert_relations(self, relations_data: List[Dict[str, Any]]):
        """Bulk inserts or updates relations."""
        if not relations_data:
            return

        conn = self._get_connection()
        cursor = conn.cursor()

        sql = """
            INSERT INTO relations (relation_id, source_id, target_id, type, weight, properties)
            VALUES (?, ?, ?, ?, ?, ?)
            ON CONFLICT(relation_id) DO UPDATE SET
                source_id=excluded.source_id,
                target_id=excluded.target_id,
                type=excluded.type,
                weight=excluded.weight,
                properties=excluded.properties;
        """

        relations_to_upsert = [
            (
                rel.get('relation_id'), rel.get('source_id'), rel.get('target_id'),
                rel.get('type'), rel.get('weight', 1.0),
                json.dumps(rel.get('properties', {}))
            )
            for rel in relations_data
        ]

        try:
            cursor.executemany(sql, relations_to_upsert)
            conn.commit()
            self.logger.info(f"Successfully upserted {len(relations_to_upsert)} relations.")
        except Exception as e:
            conn.rollback()
            self.logger.error(f"Failed to bulk upsert relations: {e}", exc_info=True)
            raise

    # --- Embedding and Vector Search Operations ---

    def get_node_embedding_by_id(self, node_id: str) -> Optional[np.ndarray]:
        """
        Retrieves a node's embedding from the VSS table by its node_id.
        """
        if not self.has_vector_ext:
            self.logger.warning("Vector extension not available.")
            return None
        
        conn = self._get_connection()
        cursor = conn.cursor()
        
        try:
            cursor.execute("SELECT embedding FROM node_embeddings_vss WHERE node_id = ?", (node_id,))
            row = cursor.fetchone()
            if row and row[0]:
                # The embedding is stored as a JSON string, convert it back to numpy array
                return np.array(json.loads(row[0]), dtype=np.float32)
            return None
        except Exception as e:
            self.logger.error(f"Could not retrieve node embedding for id {node_id}: {e}", exc_info=True)
            return None

    def find_similar_nodes(self, query_vector, top_k=10):
        if not self.has_vector_ext:
            logger.warning("Vector extension not available, cannot find similar nodes.")
            return []
        
        # The query_vector is now an EmbeddingResult object.
        # We need to extract the embedding data from it.
        if not query_vector or not query_vector.success:
            logger.error("Invalid or failed EmbeddingResult received.")
            return []
        
        embedding_list = query_vector.embedding
        if not embedding_list:
            logger.error("EmbeddingResult contains no embedding data.")
            return []

        sql = """
            SELECT
                n.node_id,
                t.distance
            FROM
                (SELECT node_id, distance FROM node_embeddings_vss WHERE embedding MATCH ? LIMIT ?) t
            JOIN
                nodes n ON n.node_id = t.node_id
        """
        try:
            # The C-extension expects a JSON string of a list of floats.
            query_vector_json = json.dumps(embedding_list)
            
            conn = self._get_connection()
            cursor = conn.cursor()
            cursor.execute(sql, (query_vector_json, top_k))
            return cursor.fetchall()
        except Exception as e:
            logger.error(f"Failed to find similar nodes: {e}", exc_info=True)
            return []

    def upsert_node_embedding(self, node_id: str, embedding: np.ndarray):
        """
        Upserts an embedding for a node into the VSS table.
        """
        if not self.has_vector_ext or not NP_AVAILABLE:
            self.logger.warning(f"Cannot upsert embedding for node {node_id}, vector support is disabled.")
            return

        conn = self._get_connection()
        cursor = conn.cursor()
        
        # Convert numpy array to JSON string for sqlite-vec
        embedding_json = json.dumps(embedding.tolist())
        
        try:
            # Use INSERT OR REPLACE with the explicit node_id primary key
            cursor.execute(
                "INSERT OR REPLACE INTO node_embeddings_vss (node_id, embedding) VALUES (?, ?)",
                (node_id, embedding_json)
            )
            conn.commit()
            self.logger.info(f"Successfully upserted embedding for node {node_id}.")
        except Exception as e:
            conn.rollback()
            self.logger.error(f"Failed to upsert embedding for node {node_id}: {e}", exc_info=True)
            raise

    # --- Graph Traversal ---

    def get_neighbors(self, node_id: str, link_type: Optional[str] = None) -> List[Dict[str, Any]]:
        """Retrieves neighbor nodes of a given node, based on stored relations."""
        conn = self._get_connection()
        cursor = conn.cursor()
        
        placeholders = '?'
        params = [node_id]
        if link_type:
            sql += " AND type = ?"
            params.append(link_type)
        
        cursor.execute(sql, params)
        neighbor_ids = [row[0] for row in cursor.fetchall()]

        if not neighbor_ids:
            return []
        
        # Get full node details for the neighbors
        return self.get_nodes_by_ids(neighbor_ids)

    def search_nodes_by_title_content(self, search_query: str, limit: int = 10) -> List[Dict[str, Any]]:
        """
        Performs a LIKE search on node titles and content.
        """
        if not search_query.strip():
            return []

        conn = self._get_connection()
        cursor = conn.cursor()
        
        search_pattern = f"%{search_query.strip()}%"
        query = """
            SELECT *
            FROM nodes 
            WHERE title LIKE ? OR content LIKE ?
            ORDER BY 
                CASE 
                    WHEN title LIKE ? THEN 1 
                    ELSE 2 
                END,
                LENGTH(title) ASC
            LIMIT ?
        """
        
        try:
            conn.row_factory = sqlite3.Row
            cursor.execute(query, (search_pattern, search_pattern, search_pattern, limit))
            rows = cursor.fetchall()
            conn.row_factory = None

            results = []
            for row in rows:
                node = dict(row)
                node['tags'] = json.loads(node.get('tags', '[]'))
                node['properties'] = json.loads(node.get('properties', '{}'))
                results.append(node)
            return results
        except Exception as e:
            self.logger.error(f"Error searching nodes by content: {e}", exc_info=True)
            return []

    def search_expand(self, query_vector: np.ndarray, top_k: int = 5, expansion_hops: int = 1) -> Dict[str, List[Dict[str, Any]]]:
        """
        Performs topology-enhanced search.
        1. Find initial seed nodes via vector search.
        2. Expand from seed nodes to find related entities.
        3. Expand from entities to find other related text nodes.
        """
        self.logger.info(f"Starting search_expand with top_k={top_k}, expansion_hops={expansion_hops}")
        
        # 1. Initial vector search for seed nodes
        initial_nodes_tuples = self.find_similar_nodes(query_vector, top_k=top_k)
        if not initial_nodes_tuples:
            self.logger.info("No initial nodes found in vector search.")
            return {"initial": [], "expanded": []}

        initial_node_ids = [item[0] for item in initial_nodes_tuples]
        initial_nodes = self.get_nodes_by_ids(initial_node_ids)
        self.logger.debug(f"Found {len(initial_nodes)} initial seed nodes.")

        # Use a set to keep track of all node IDs found to avoid duplicates
        all_found_node_ids = set(initial_node_ids)
        
        # 2. Graph Expansion
        current_expansion_front = set(initial_node_ids)
        
        for hop in range(expansion_hops):
            self.logger.debug(f"Expansion hop {hop + 1}/{expansion_hops}")
            
            # Find related entities from the current front of text nodes
            related_entity_ids = set()
            for node_id in current_expansion_front:
                neighbors = self.get_neighbors(node_id, relation_type='HAS_ENTITY')
                for neighbor in neighbors:
                    related_entity_ids.add(neighbor['node_id'])
            
            if not related_entity_ids:
                self.logger.debug("No related entities found to expand from.")
                break # Stop if no further expansion is possible
            
            self.logger.debug(f"Found {len(related_entity_ids)} related entities.")
            
            # Find new text nodes related to these entities
            newly_found_text_ids = set()
            for entity_id in related_entity_ids:
                # We need a method to get neighbors that are sources of a relation
                source_neighbors = self._get_source_neighbors(entity_id, relation_type='HAS_ENTITY')
                for neighbor in source_neighbors:
                    if neighbor['node_id'] not in all_found_node_ids:
                        newly_found_text_ids.add(neighbor['node_id'])
            
            if not newly_found_text_ids:
                self.logger.debug("Entities did not lead to any new text nodes.")
                break

            self.logger.debug(f"Found {len(newly_found_text_ids)} new text nodes in hop {hop + 1}.")
            all_found_node_ids.update(newly_found_text_ids)
            current_expansion_front = newly_found_text_ids

        # 3. Aggregate results
        expanded_node_ids = list(all_found_node_ids - set(initial_node_ids))
        expanded_nodes = self.get_nodes_by_ids(expanded_node_ids) if expanded_node_ids else []
        
        self.logger.info(f"Search expanded from {len(initial_nodes)} to {len(all_found_node_ids)} total nodes.")

        return {
            "initial": initial_nodes,
            "expanded": expanded_nodes
        }

    def _get_source_neighbors(self, node_id: str, relation_type: Optional[str] = None) -> List[Dict[str, Any]]:
        """Helper to get neighbors where the given node_id is the target."""
        conn = self._get_connection()
        cursor = conn.cursor()
        
        sql = "SELECT source_id FROM relations WHERE target_id = ?"
        params = [node_id]
        if relation_type:
            sql += " AND type = ?"
            params.append(relation_type)
            
        cursor.execute(sql, params)
        source_ids = [row[0] for row in cursor.fetchall()]
        
        if not source_ids:
            return []
            
        return self.get_nodes_by_ids(source_ids)

    def get_stats(self) -> Dict[str, Any]:
        """Returns statistics about the database."""
        conn = self._get_connection()
        cursor = conn.cursor()
        
        total_nodes = cursor.execute("SELECT COUNT(*) FROM nodes").fetchone()[0]
        text_nodes = cursor.execute("SELECT COUNT(*) FROM nodes WHERE type = 'TEXT'").fetchone()[0]
        entity_nodes = cursor.execute("SELECT COUNT(*) FROM nodes WHERE type = 'ENTITY'").fetchone()[0]
        total_relations = cursor.execute("SELECT COUNT(*) FROM relations").fetchone()[0]
        
        stats = {
            'total_nodes': total_nodes,
            'text_nodes': text_nodes,
            'entity_nodes': entity_nodes,
            'total_relations': total_relations,
            'db_path': self.db_path,
            'vector_extension': 'sqlite-vec' if self.has_vector_ext else 'Not loaded'
        }
        return stats

    def close(self):
        """Closes the database connection for the current thread."""
        if hasattr(self.local, 'conn') and self.local.conn is not None:
            self.local.conn.close()
            self.local.conn = None
            self.logger.debug(f"Closed database connection for thread {threading.get_ident()}") 
    
    def force_unlock_database(self):
        """Force unlock the database by closing all connections and clearing WAL files if needed."""
        try:
            # Close current connection
            self.close()
            
            # Try to connect and run a simple query to check if database is accessible
            test_conn = sqlite3.connect(self.db_path, timeout=5.0)
            test_conn.execute("SELECT 1").fetchone()
            test_conn.close()
            
            self.logger.info("Database is accessible, no force unlock needed.")
            return True
            
        except sqlite3.OperationalError as e:
            if "database is locked" in str(e).lower():
                self.logger.warning("Database is locked, attempting to force unlock...")
                
                # Remove WAL and SHM files if they exist
                import os
                wal_file = self.db_path + "-wal"
                shm_file = self.db_path + "-shm"
                
                try:
                    if os.path.exists(wal_file):
                        os.remove(wal_file)
                        self.logger.info(f"Removed WAL file: {wal_file}")
                    if os.path.exists(shm_file):
                        os.remove(shm_file)
                        self.logger.info(f"Removed SHM file: {shm_file}")
                        
                    # Try to connect again
                    test_conn = sqlite3.connect(self.db_path, timeout=5.0)
                    test_conn.execute("SELECT 1").fetchone()
                    test_conn.close()
                    
                    self.logger.info("Database unlocked successfully.")
                    return True
                    
                except Exception as cleanup_error:
                    self.logger.error(f"Failed to force unlock database: {cleanup_error}")
                    return False
            else:
                self.logger.error(f"Database error (not lock-related): {e}")
                return False
        except Exception as e:
            self.logger.error(f"Unexpected error during database unlock: {e}")
            return False
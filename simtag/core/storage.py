"""
Vector Storage Abstraction Layer - SQLite Implementation
"""
import sqlite3
import numpy as np
import json
import logging
import threading
from typing import List, Dict, Any, Tuple, Optional
import traceback

# Ensure numpy is available, otherwise log an error and continue without vector ops
NP_AVAILABLE = False
try:
    import numpy as np
    NP_AVAILABLE = True
except ImportError:
    logging.getLogger("VectorStorage").error("NumPy library not found. Vector operations will be disabled.")

class VectorStorage:
    """SQLite vector storage implementation, thread-safe version"""
    
    def __init__(self, db_path: str):
        self.db_path = db_path
        self.logger = logging.getLogger("storage")
        self.has_vector_ext = False
        
        # Use thread local storage to save connection for each thread
        self.local = threading.local()
        
        # Initialize main thread connection
        self._init_connection()
        self._init_db()
    
    def _init_connection(self):
        """Initialize database connection for current thread"""
        if not hasattr(self.local, 'conn') or self.local.conn is None:
            self.logger.debug(f"Creating new database connection for thread {threading.get_ident()}")
            self.local.conn = self._create_connection()
        return self.local.conn
    
    def _create_connection(self) -> sqlite3.Connection:
        """Create database connection"""
        conn = sqlite3.connect(self.db_path)
        conn.enable_load_extension(True)
        
        try:
            # Try loading sqlite-vec Python package
            import sqlite_vec
            sqlite_vec.load(conn)
            self.has_vector_ext = True
            self.logger.info(f"Vector extension available: {conn.execute('SELECT vec_version()').fetchone()[0]}")
        except ImportError:
            self.logger.warning("sqlite-vec package not available, trying to load extension file directly")
            try:
                # Fallback to loading extension file directly
                conn.load_extension("vec0")
                self.has_vector_ext = True
                self.logger.info("SQLite vector extension(vec0) loaded successfully")
            except sqlite3.OperationalError as e:
                self.logger.warning(f"SQLite vector extension loading failed: {e}")
                self.has_vector_ext = False
        except Exception as e:
            self.logger.warning(f"Failed to load vector extension: {e}")
            self.has_vector_ext = False
        
        conn.enable_load_extension(False)
        return conn
    
    def _get_connection(self):
        """Get current thread's connection, create if not exists"""
        return self._init_connection()
    
    def _init_db(self):
        """Initialize database table structure"""
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            
            # --- tag_metadata and tag_embeddings_vss tables ---
            self.logger.debug("Ensuring tag metadata and VSS table structure")
            
            # Expected columns for tag_metadata table (excluding vector if VSS is used)
            expected_tag_metadata_columns = [
                'tag_id', 'name', 'fields', 'tag_type', 'relation_type_hint', 
                'description', 'icon', 'color', 'modified_at', 'behaviors', 'created_at'
            ]

            cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='tag_metadata'")
            tag_metadata_table_exists = cursor.fetchone() is not None

            if tag_metadata_table_exists:
                cursor.execute("PRAGMA table_info(tag_metadata)")
                existing_tag_meta_columns = [col[1] for col in cursor.fetchall()]
                
                # Recreate if schema fundamentally changed (e.g., vector columns present/absent inappropriately)
                needs_recreate = False
                if self.has_vector_ext and ('vector' in existing_tag_meta_columns or 'vector_dim' in existing_tag_meta_columns):
                    self.logger.warning("'tag_metadata' table exists with vector columns, but sqlite-vec is active. Recreating for metadata only.")
                    needs_recreate = True
                elif not self.has_vector_ext and not ('vector' in existing_tag_meta_columns and 'vector_dim' in existing_tag_meta_columns):
                    self.logger.warning("'tag_metadata' table exists WITHOUT vector columns, but sqlite-vec is NOT active. Recreating with vector columns.")
                    needs_recreate = True
                
                if not needs_recreate:
                    current_expected_cols = set(expected_tag_metadata_columns)
                    if not self.has_vector_ext: # if no VSS, vector/vector_dim are expected
                        current_expected_cols.update(['vector', 'vector_dim'])
                    missing_tag_meta_columns = current_expected_cols - set(existing_tag_meta_columns)
                    if missing_tag_meta_columns:
                        self.logger.warning(f"tag_metadata table missing columns: {missing_tag_meta_columns}. Recreating table.")
                        needs_recreate = True

                if needs_recreate:
                    cursor.execute("DROP TABLE IF EXISTS tag_metadata")
                    conn.commit()
                    tag_metadata_table_exists = False
            
            # Drop old 'tag_vectors' table if it exists, as we are migrating to 'tag_metadata'
            cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='tag_vectors'")
            if cursor.fetchone() is not None:
                self.logger.info("Found old 'tag_vectors' table. Dropping it in favor of 'tag_metadata' and VSS structure.")
                cursor.execute("DROP TABLE IF EXISTS tag_vectors")
                conn.commit()

            if not tag_metadata_table_exists:
                if self.has_vector_ext:
                    self.logger.info("Creating 'tag_metadata' table for METADATA (sqlite-vec is active).")
                    cursor.execute("""
                    CREATE TABLE tag_metadata (
                        tag_id TEXT PRIMARY KEY,
                        name TEXT,
                        fields TEXT,
                        tag_type TEXT,
                        relation_type_hint TEXT,
                        description TEXT,
                        icon TEXT,
                        color TEXT,
                        modified_at TEXT,
                        behaviors TEXT,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    )
                    """)
                else: # Fallback: tag_metadata includes vector columns
                    self.logger.info("Creating 'tag_metadata' table with embedded vectors (sqlite-vec NOT active).")
                    cursor.execute("""
                    CREATE TABLE tag_metadata (
                        tag_id TEXT PRIMARY KEY,
                        vector BLOB,
                        vector_dim INTEGER,
                        name TEXT,
                        fields TEXT,
                        tag_type TEXT,
                        relation_type_hint TEXT,
                        description TEXT,
                        icon TEXT,
                        color TEXT,
                        modified_at TEXT,
                        behaviors TEXT,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    )
                    """)
                self.logger.info("Created 'tag_metadata' table structure.")
            else:
                self.logger.debug("'tag_metadata' table already exists and schema was checked.")

            # Create VIRTUAL TABLE for tag embeddings if sqlite-vec is active
            if self.has_vector_ext:
                self.logger.info("Ensuring 'tag_embeddings_vss' sqlite-vec VIRTUAL table structure.")
                vector_dimension = self.config.get_vector_dimension_for_model() if hasattr(self, 'config') and hasattr(self.config, 'get_vector_dimension_for_model') else 768
                cursor.execute(f"""
                CREATE VIRTUAL TABLE IF NOT EXISTS tag_embeddings_vss USING vec0(
                    embedding FLOAT[{vector_dimension}],
                    tag_id_ref TEXT HIDDEN     -- Reference to tag_metadata.tag_id
                )
                """)
                self.logger.info(f"Ensured 'tag_embeddings_vss' VIRTUAL table with dimension {vector_dimension}.")


            # --- node_vectors table ---
            self.logger.debug("Ensuring 'node_vectors' table structure (potentially as standard or VSS-linked table)")
            
            # --- NEW: node_chunks table ---
            self.logger.debug("Ensuring 'node_chunks' table structure")
            cursor.execute("""
            CREATE TABLE IF NOT EXISTS node_chunks (
                chunk_id TEXT PRIMARY KEY,
                node_id TEXT NOT NULL,
                text_chunk TEXT NOT NULL,
                embedding BLOB,
                headline TEXT,
                original_tags TEXT,     -- JSON string of tags from the original node
                file_path TEXT,
                node_modified_at TEXT,  -- ISO timestamp of the original node's modification
                chunk_created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                FOREIGN KEY (node_id) REFERENCES node_vectors (node_id) ON DELETE CASCADE
            )
            """)
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_node_chunks_node_id ON node_chunks (node_id)")
            self.logger.info("Ensured 'node_chunks' table and index.")
            
            expected_node_metadata_columns = [
                'node_id', # 'vector', 'vector_dim' are removed for metadata-only table if VSS is used
                'content', 'title', 'tags', 'created_at',
                'file_path', 'pos', 'olp', 'level', 'scheduled', 'deadline', 'todo', 
                'priority', 'modified_at', 'properties', 'raw_value', 'hash', 'content_hash',
                'document_date' # Added for Knowledge Archaeology
            ]

            cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='node_vectors'")
            node_table_exists = cursor.fetchone() is not None
            
            if node_table_exists:
                # Check if it needs restructuring (e.g. removing vector columns if VSS is now active)
                # This is a simplified check. A more robust migration would handle existing data.
                cursor.execute("PRAGMA table_info(node_vectors)")
                existing_node_columns = [col[1] for col in cursor.fetchall()]
                if self.has_vector_ext and ('vector' in existing_node_columns or 'vector_dim' in existing_node_columns):
                    self.logger.warning("'node_vectors' table exists with vector columns, but sqlite-vec is active. Consider migrating and altering table.")
                    # For now, we won't drop it, but new inserts should go to VSS table for vectors.
                    # Ideally, a migration script would handle this.
                elif not self.has_vector_ext and not ('vector' in existing_node_columns and 'vector_dim' in existing_node_columns):
                    self.logger.warning("'node_vectors' table exists WITHOUT vector columns, but sqlite-vec is NOT active. Table might be incomplete. Recreating.")
                    cursor.execute("DROP TABLE IF EXISTS node_vectors")
                    conn.commit()
                    node_table_exists = False
                else:
                    # Check for other missing metadata columns if table structure is as expected for its role
                    current_expected_cols = set(expected_node_metadata_columns)
                    if not self.has_vector_ext: # if no VSS, vector/vector_dim are expected
                        current_expected_cols.update(['vector', 'vector_dim'])
                    missing_node_columns = current_expected_cols - set(existing_node_columns)
                    if missing_node_columns:
                        self.logger.warning(f"node_vectors table missing columns: {missing_node_columns}. Recreating table.")
                        cursor.execute("DROP TABLE IF EXISTS node_vectors")
                        conn.commit()
                        node_table_exists = False
            
            if not node_table_exists:
                if self.has_vector_ext:
                    # Create node_vectors for METADATA ONLY
                    self.logger.info("Creating 'node_vectors' table for METADATA (sqlite-vec is active).")
                    cursor.execute(f"""
                    CREATE TABLE node_vectors (
                        node_id TEXT PRIMARY KEY,
                        content TEXT,
                        title TEXT,
                        tags TEXT,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
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
                        document_date TEXT
                    )
                    """)
                else:
                    # Create node_vectors with embedded vector columns (FALLBACK)
                    self.logger.info("Creating 'node_vectors' table with embedded vectors (sqlite-vec NOT active).")
                    cursor.execute("""
                    CREATE TABLE node_vectors (
                        node_id TEXT PRIMARY KEY,
                        content TEXT,
                        title TEXT,
                        tags TEXT,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
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
                        vector BLOB,
                        vector_dim INTEGER,
                        document_date TEXT
                    )
                    """)
                self.logger.info("Created node_vectors table structure.")
            else:
                self.logger.debug("node_vectors table already exists and schema was checked.")

            # --- Create VIRTUAL TABLE for node embeddings if sqlite-vec is active ---
            if self.has_vector_ext:
                self.logger.info("Ensuring 'node_embeddings_vss' sqlite-vec VIRTUAL table structure.")
                vector_dimension = self.config.get_vector_dimension_for_model() if hasattr(self, 'config') and hasattr(self.config, 'get_vector_dimension_for_model') else 768 # Default or get from config
                cursor.execute(f"""
                CREATE VIRTUAL TABLE IF NOT EXISTS node_embeddings_vss USING vec0(
                    embedding FLOAT[{vector_dimension}],
                    node_id_ref TEXT HIDDEN     -- Reference to node_vectors.node_id
                )
                """)
                self.logger.info(f"Ensured 'node_embeddings_vss' VIRTUAL table with dimension {vector_dimension}.")

            # --- elisp_links table ---
            self.logger.debug("Ensuring 'elisp_links' table structure")
            cursor.execute("""
            CREATE TABLE IF NOT EXISTS elisp_links (
                link_id TEXT PRIMARY KEY,
                link_type TEXT,
                from_id TEXT,
                to_id TEXT,
                properties TEXT,
                created_at TEXT,
                modified_at TEXT
            )
            """)
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_elisp_links_link_type ON elisp_links (link_type)")
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_elisp_links_from_id ON elisp_links (from_id)")
            cursor.execute("CREATE INDEX IF NOT EXISTS idx_elisp_links_to_id ON elisp_links (to_id)")

            # --- elisp_metadata table ---
            self.logger.debug("Ensuring 'elisp_metadata' table structure")
            cursor.execute("""
            CREATE TABLE IF NOT EXISTS elisp_metadata (
                meta_key TEXT PRIMARY KEY,
                meta_value TEXT,
                last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
            """)
            
            conn.commit()
            self.logger.info("Database table structure initialization/verification complete")
            
            # Check vector extension
            if self.has_vector_ext:
                self.check_vec_extension()
        except Exception as e:
            self.logger.error(f"Failed to initialize database table structure: {e}")
            raise
    
    def save_tag_vector(self, tag_id: str, vector: list, vector_dim: int = None, tag_name: Optional[str] = None):
        """Save tag vector and minimal metadata.
        If VSS is active, vector goes to VSS table, metadata to tag_metadata.
        Otherwise, vector and metadata go to tag_metadata.
        """
        try:
            if not isinstance(vector, np.ndarray):
                vector_np = np.array(vector, dtype=np.float32)
            else:
                vector_np = vector.astype(np.float32)
            
            if vector_dim is None:
                vector_dim = len(vector_np)
            
            if np.isnan(vector_np).any() or np.isinf(vector_np).any():
                self.logger.warning(f"Vector for tag {tag_id} contains NaN or Inf values. Clamping.")
                vector_np = np.nan_to_num(vector_np, nan=0.0, posinf=1.0, neginf=-1.0)
            
            conn = self._get_connection()
            cursor = conn.cursor()

            # Upsert minimal metadata (tag_id, name if provided)
            # Other metadata fields are typically handled by bulk_insert_tag_vectors
            if tag_name:
                cursor.execute("INSERT OR IGNORE INTO tag_metadata (tag_id, name) VALUES (?, ?)", (tag_id, tag_name))
                cursor.execute("UPDATE tag_metadata SET name = ? WHERE tag_id = ? AND name IS NULL", (tag_name, tag_id))
            else:
                cursor.execute("INSERT OR IGNORE INTO tag_metadata (tag_id) VALUES (?)", (tag_id,))
            
            if self.has_vector_ext:
                # Save to VSS table
                # sqlite-vec expects embedding as JSON string list for float arrays
                embedding_json = json.dumps(vector_np.tolist())
                # Upsert: Delete existing then insert
                cursor.execute("DELETE FROM tag_embeddings_vss WHERE tag_id_ref = ?", (tag_id,))
                cursor.execute("INSERT INTO tag_embeddings_vss (tag_id_ref, embedding) VALUES (?, ?)", (tag_id, embedding_json))
                self.logger.debug(f"Saved tag vector to VSS: {tag_id}, dimension: {vector_dim}")
            else:
                # Save to tag_metadata table (fallback)
                vector_blob = vector_np.tobytes()
                cursor.execute("""
                UPDATE tag_metadata 
                SET vector = ?, vector_dim = ?
                WHERE tag_id = ?
                """, (vector_blob, vector_dim, tag_id))
                # Ensure the row exists from INSERT OR IGNORE above. If it was truly new, this update sets the vector.
                self.logger.debug(f"Saved tag vector to tag_metadata (blob): {tag_id}, dimension: {vector_dim}")
            
            conn.commit()
            return True
        except Exception as e:
            self.logger.error(f"Failed to save tag vector for {tag_id}: {e}\n{traceback.format_exc()}")
            return False
    
    def delete_tag_vector(self, tag_id: str) -> bool:
        """Delete tag vector and its metadata."""
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            
            # Delete from metadata table (will cascade if FKs were set up, but here manual)
            cursor.execute("DELETE FROM tag_metadata WHERE tag_id = ?", (tag_id,))
            deleted_metadata_count = cursor.rowcount

            if self.has_vector_ext:
                # Delete from VSS table
                cursor.execute("DELETE FROM tag_embeddings_vss WHERE tag_id_ref = ?", (tag_id,))
                deleted_vss_count = cursor.rowcount
                self.logger.debug(f"Deleted tag from VSS: {tag_id} (count: {deleted_vss_count})")
            
            conn.commit()
            self.logger.debug(f"Deleted tag metadata for: {tag_id} (count: {deleted_metadata_count})")
            return deleted_metadata_count > 0 # Successful if metadata was found and deleted
        except Exception as e:
            self.logger.error(f"Failed to delete tag vector {tag_id}: {e}")
            return False
    
    def list_tag_ids(self) -> List[str]:
        """Get list of all tag IDs from tag_metadata."""
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            
            cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='tag_metadata'")
            if cursor.fetchone() is None:
                self.logger.warning("'tag_metadata' table does not exist. Initializing DB.")
                self._init_db() # Attempt to initialize
                return [] 
            
            cursor.execute("SELECT tag_id FROM tag_metadata")
            result = [row[0] for row in cursor.fetchall()]
            self.logger.debug(f"Retrieved {len(result)} tag IDs from tag_metadata")
            return result
        except Exception as e:
            self.logger.error(f"Failed to get tag ID list from tag_metadata: {e}")
            return []
    
    def has_tag_vector(self, tag_id: str) -> bool:
        """Check if tag has a vector.
        If VSS is active, checks tag_embeddings_vss.
        Otherwise, checks for non-NULL vector in tag_metadata.
        """
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            if self.has_vector_ext:
                cursor.execute("SELECT 1 FROM tag_embeddings_vss WHERE tag_id_ref = ? LIMIT 1", (tag_id,))
                return cursor.fetchone() is not None
            else:
                # Check for row existence AND non-null vector column in fallback table
                cursor.execute("SELECT 1 FROM tag_metadata WHERE tag_id = ? AND vector IS NOT NULL LIMIT 1", (tag_id,))
                return cursor.fetchone() is not None
        except Exception as e:
            self.logger.error(f"Failed to check tag vector for {tag_id}: {e}")
            return False
    
    def get_tag_vector(self, tag_id: str) -> Optional[np.ndarray]:
        """Get tag vector.
        If VSS active, retrieves from tag_embeddings_vss.
        Otherwise, from tag_metadata (blob).
        """
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            
            if self.has_vector_ext:
                cursor.execute("SELECT embedding FROM tag_embeddings_vss WHERE tag_id_ref = ?", (tag_id,))
                row = cursor.fetchone()
                if not row or row[0] is None:
                    self.logger.debug(f"No VSS embedding found for tag_id: {tag_id}")
                    return None
                
                embedding_data = row[0]
                if isinstance(embedding_data, bytes):
                    # This is now an expected case if sqlite-vec FLOAT[] returns bytes
                    self.logger.debug(f"VSS embedding for tag_id: {tag_id} is BYTES. Processing with np.frombuffer.")
                    try:
                        # If count is not specified, frombuffer reads until end of buffer.
                        # This should be fine if sqlite-vec returns only the vector bytes.
                        vector = np.frombuffer(embedding_data, dtype=np.float32)
                        self.logger.debug(f"Successfully parsed VSS BYTES embedding for tag_id: {tag_id}, dim: {len(vector)}")
                        return vector
                    except Exception as ex_bytes:
                        self.logger.error(f"Failed to parse VSS BYTES embedding for {tag_id}: {ex_bytes}\n{traceback.format_exc()}")
                        return None
                elif isinstance(embedding_data, str):
                    # This path remains for cases where it might actually be a JSON string
                    self.logger.debug(f"VSS embedding for tag_id: {tag_id} is STRING. Processing with json.loads.")
                    try:
                        embedding_list = json.loads(embedding_data)
                        vector = np.array(embedding_list, dtype=np.float32)
                        self.logger.debug(f"Successfully retrieved and parsed VSS JSON STRING embedding for tag_id: {tag_id}")
                        return vector
                    except json.JSONDecodeError as jde:
                        self.logger.error(f"Failed to decode JSON for VSS embedding of tag {tag_id}: {jde}. Data: '{embedding_data[:100]}...'")
                        return None
                    except Exception as ex_json_conv: # Catch other potential errors during array conversion
                        self.logger.error(f"Error converting VSS JSON embedding for tag {tag_id}: {ex_json_conv}")
                        return None
                else:
                    self.logger.error(f"Unexpected data type for VSS embedding of tag {tag_id}: {type(embedding_data)}. Expected str.")
                    return None
            else: # Fallback: Get from tag_metadata (blob)
                cursor.execute("SELECT vector, vector_dim FROM tag_metadata WHERE tag_id = ?", (tag_id,))
                row = cursor.fetchone()
                if not row or row[0] is None: # Check if vector blob itself is NULL
                    self.logger.debug(f"No vector blob found in tag_metadata for tag: {tag_id}")
                    return None
                
                vector_blob, vector_dim = row
                if not vector_dim: # If vector_dim is 0 or None, it's problematic
                     self.logger.warning(f"Vector dimension is 0 or missing for tag: {tag_id}. Cannot parse vector.")
                     return None

                try:
                    vector = np.frombuffer(vector_blob, dtype=np.float32)
                    # Basic validation, though vector_dim should be the source of truth
                    if len(vector) != vector_dim:
                        self.logger.warning(f"Tag vector dimension mismatch for {tag_id}: stored={vector_dim}, actual from blob={len(vector)}. Attempting to reshape or trust stored dim.")
                        # This part might need more robust handling if mismatches are common
                        if vector_dim > 0 and len(vector) >= vector_dim : # If blob is larger or equal
                             vector = vector[:vector_dim]
                        elif vector_dim > 0 and len(vector) < vector_dim: # If blob is smaller, this is problematic
                             self.logger.error(f"Cannot reconstruct vector for {tag_id} due to insufficient blob data for stored dimension.")
                             return None
                        # If vector_dim is somehow wrong, but vector has content, this might be an issue.
                    
                    self.logger.debug(f"Retrieved blob vector from tag_metadata for tag: {tag_id}")
                    return vector.astype(np.float32)
                except Exception as e:
                    self.logger.warning(f"Failed to parse vector blob for tag {tag_id}: {e}")
                    return None
        except Exception as e:
            self.logger.error(f"Error getting tag vector for {tag_id}: {e}")
            return None
    
    def _cosine_similarity(self, vec1: np.ndarray, vec2: np.ndarray) -> float:
        """计算余弦相似度"""
        dot_product = np.dot(vec1, vec2)
        norm1 = np.linalg.norm(vec1)
        norm2 = np.linalg.norm(vec2)
        if norm1 == 0 or norm2 == 0:
            return 0.0
        return dot_product / (norm1 * norm2)
    
    def find_similar_tags(self, query_vector: np.ndarray, top_k: int = 5) -> List[Tuple[str, float]]:
        """查找相似标签. Uses VSS if active, otherwise logs error as manual search is not implemented yet."""
        if not NP_AVAILABLE:
            self.logger.error("find_similar_tags: NumPy is not available.")
            return []
        if query_vector is None or query_vector.size == 0:
            self.logger.warning("find_similar_tags: Query vector is None or empty.")
            return []

        try:
            if self.has_vector_ext:
                conn = self._get_connection()
                cursor = conn.cursor()
                query_vector_json = json.dumps(query_vector.tolist())

                sql = """
                    SELECT tag_id_ref, distance
                    FROM tag_embeddings_vss
                    WHERE embedding MATCH ? 
                    ORDER BY distance
                    LIMIT ?;
                """
                self.logger.debug(f"Executing VSS search for similar tags: top_k={top_k}")
                cursor.execute(sql, (query_vector_json, top_k))
                results = cursor.fetchall()
                # Assuming distance is cosine distance (lower is better).
                # Convert to similarity if needed, or return distance.
                # For now, (tag_id, distance)
                similar = [(row[0], float(row[1])) for row in results]
                self.logger.info(f"VSS tag search found {len(similar)} tags.")
                return similar
            else:
                self.logger.error("SQLite vector extension not available. Finding similar tags via manual DB scan is not implemented for performance reasons.")
                # Placeholder for potential manual implementation:
                # 1. Get all tag_ids and vectors from tag_metadata.
                # 2. For each, compute similarity with query_vector.
                # 3. Sort and return top_k.
                # This would be slow.
                return []
        except Exception as e:
            self.logger.error(f"Failed to find similar tags: {e}\n{traceback.format_exc()}")
            return []
    
    def save_node_vector(self, node_id: str, vector: np.ndarray, content: str = "", title: str = "", tags: List[str] = None):
        """保存节点向量"""
        # Similar to save_tag_vector, this is primarily for the vector and associated direct metadata.
        # Other new fields will be handled by the mirroring ingestion logic.
        try:
            vector_blob = vector.tobytes()
            tags_json = json.dumps(tags) if tags else "[]"
            conn = self._get_connection()
            cursor = conn.cursor()
            cursor.execute("""
            INSERT OR REPLACE INTO node_vectors (node_id, vector, vector_dim, content, title, tags)
            VALUES (?, ?, ?, ?, ?, ?)
            """, (node_id, vector_blob, len(vector), content, title, tags_json))
            # This only updates specific columns. A full upsert for all columns
            # is better handled by dedicated mirroring data ingestion.
            conn.commit()
            return True
        except Exception as e:
            self.logger.error(f"保存节点向量失败: {e}")
            return False
    
    def get_all_tags(self) -> List[str]:
        """获取所有标签ID"""
        conn = self._get_connection()
        cursor = conn.cursor()
        cursor.execute("SELECT tag_id FROM tag_metadata")
        return [row[0] for row in cursor.fetchall()]
    
    def get_stats(self) -> Dict[str, Any]:
        """获取存储统计信息"""
        conn = self._get_connection()
        cursor = conn.cursor()
        
        tag_count = 0
        try:
            cursor.execute("SELECT COUNT(*) FROM tag_metadata")
            tag_count = cursor.fetchone()[0]
        except sqlite3.OperationalError:
             self.logger.warning("'tag_metadata' table not found for stats, likely needs init.")


        # 节点向量数量
        cursor.execute("SELECT COUNT(*) FROM node_vectors")
        node_count = cursor.fetchone()[0]
        
        return {
            "has_vector_extension": self.has_vector_ext,
            "tag_vectors": tag_count,
            "node_vectors": node_count,
            "db_path": self.db_path
        }

    def check_vec_extension(self) -> str:
        """检查sqlite-vec扩展的版本
        
        Returns:
            str: 版本号，如果扩展不可用则返回None
        """
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            cursor.execute("SELECT vec_version()")
            version = cursor.fetchone()[0]
            self.logger.info(f"sqlite-vec 版本: {version}")
            return version
        except Exception as e:
            self.logger.warning(f"无法获取sqlite-vec版本: {e}")
            return None

    def clear_all_mirrored_data(self):
        """Clears all data from tables populated by the Elisp mirroring process."""
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            
            self.logger.info("Clearing all mirrored data from tables: node_vectors, tag_metadata, elisp_links, elisp_metadata")
            
            # It's generally safer to delete from tables rather than dropping and recreating,
            # especially if other parts of the application might hold references or expect tables to exist.
            cursor.execute("DELETE FROM node_vectors")
            cursor.execute("DELETE FROM tag_metadata") # Changed from tag_vectors
            if self.has_vector_ext:
                # Also clear VSS tables if they exist
                # These might not exist if init_db hasn't run or failed for VSS part
                try:
                    cursor.execute("DELETE FROM node_embeddings_vss")
                    self.logger.info("Cleared 'node_embeddings_vSS' table.")
                except sqlite3.OperationalError:
                    self.logger.warning("'node_embeddings_vss' table not found for clearing, might be normal if VSS was not active or initialized.")
                try:
                    cursor.execute("DELETE FROM tag_embeddings_vss")
                    self.logger.info("Cleared 'tag_embeddings_vss' table.")
                except sqlite3.OperationalError:
                    self.logger.warning("'tag_embeddings_vss' table not found for clearing, might be normal if VSS was not active or initialized.")

            cursor.execute("DELETE FROM elisp_links")
            cursor.execute("DELETE FROM elisp_metadata")
            
            conn.commit()
            self.logger.info("Successfully cleared all mirrored data.")
            return True
        except Exception as e:
            self.logger.error(f"Failed to clear mirrored data: {e}")
            # Depending on atomicity requirements, the caller should handle transactions.
            # If this is part of a larger transaction, it might be rolled back.
            return False

    def bulk_insert_tag_vectors(self, tags_data: List[Dict[str, Any]]):
        """Bulk inserts data into the tag_metadata table.
        Vector data itself is not handled here; it should be generated and saved via save_tag_vector or a separate batch process.
        Assumes input data is a list of dictionaries, each matching the tag_metadata schema (excluding vector/vector_dim if VSS active).
        """
        if not tags_data:
            return
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            
            # Define columns for tag_metadata based on VSS status
            base_columns = [
                'tag_id', 'name', 'fields', 'tag_type', 'relation_type_hint', 
                'description', 'icon', 'color', 'modified_at', 'behaviors', 'created_at'
            ]
            if self.has_vector_ext:
                columns = base_columns
            else:
                columns = base_columns + ['vector', 'vector_dim'] # Add vector columns if no VSS
            
            placeholders = ', '.join(['?'] * len(columns))
            sql = f"INSERT OR REPLACE INTO tag_metadata ({', '.join(columns)}) VALUES ({placeholders})"
            
            data_to_insert = []
            for row_dict in tags_data:
                # For non-VSS, ensure vector and vector_dim are handled (e.g., convert list to blob)
                if not self.has_vector_ext:
                    vector_list = row_dict.get('vector')
                    if vector_list is not None and isinstance(vector_list, list):
                        vector_np = np.array(vector_list, dtype=np.float32)
                        row_dict['vector'] = sqlite3.Binary(vector_np.tobytes())
                        row_dict['vector_dim'] = len(vector_np)
                    elif vector_list is not None: # Already bytes or other format, or None
                        # Assuming if 'vector' is provided and not list, it might be pre-formatted blob or None
                        pass 
                    else: # vector is None
                        row_dict['vector'] = None
                        row_dict['vector_dim'] = 0
                        
                row_tuple = tuple(row_dict.get(col) for col in columns)
                data_to_insert.append(row_tuple)

            if not data_to_insert: return

            cursor.executemany(sql, data_to_insert)
            # conn.commit() # Commit should be handled by the calling transaction manager
            self.logger.info(f"Bulk inserted/replaced {len(data_to_insert)} rows into tag_metadata.")
        except Exception as e:
            self.logger.error(f"Failed to bulk insert into tag_metadata: {e}\n{traceback.format_exc()}")
            raise # Re-raise to allow transaction rollback

    def bulk_insert_node_vectors(self, nodes_data: List[Dict[str, Any]]):
        """Bulk inserts data into the node_vectors table.
        Assumes input data is a list of dictionaries, each matching the table schema.
        """
        if not nodes_data:
            return
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            
            columns = [
                'node_id', 'content', 'title', 'tags', 'file_path', 'pos', 'olp', 'level',
                'scheduled', 'deadline', 'todo', 'priority', 'modified_at', 'properties',
                'raw_value', 'hash', 'content_hash',
                'vector', 'vector_dim', 'created_at' # Vector related fields might be null initially
            ]
            placeholders = ', '.join(['?'] * len(columns))
            sql = f"INSERT OR REPLACE INTO node_vectors ({', '.join(columns)}) VALUES ({placeholders})"

            data_to_insert = []
            for row_dict in nodes_data:
                row_tuple = tuple(row_dict.get(col) for col in columns)
                data_to_insert.append(row_tuple)
                
            cursor.executemany(sql, data_to_insert)
            # conn.commit() # Commit should be handled by the calling transaction manager
            self.logger.info(f"Bulk inserted {len(data_to_insert)} rows into node_vectors.")
        except Exception as e:
            self.logger.error(f"Failed to bulk insert into node_vectors: {e}")
            raise # Re-raise

    def bulk_insert_elisp_links(self, links_data: List[Dict[str, Any]]):
        """Bulk inserts data into the elisp_links table."""
        if not links_data:
            return
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            
            columns = ['link_id', 'link_type', 'from_id', 'to_id', 'properties', 'created_at', 'modified_at']
            placeholders = ', '.join(['?'] * len(columns))
            sql = f"INSERT OR REPLACE INTO elisp_links ({', '.join(columns)}) VALUES ({placeholders})"

            data_to_insert = []
            for row_dict in links_data:
                # Ensure the 'properties' field is stored as a JSON string
                properties_val = row_dict.get('properties')
                if properties_val is not None and not isinstance(properties_val, str):
                    row_dict['properties'] = json.dumps(properties_val)
                elif properties_val is None: # Ensure NULL is stored if properties are not provided or explicitly null
                    row_dict['properties'] = None 
                # else, it's already a string (less likely for complex objects) or correctly None
                
                row_tuple = tuple(row_dict.get(col) for col in columns)
                data_to_insert.append(row_tuple)

            cursor.executemany(sql, data_to_insert)
            # conn.commit() # Commit should be handled by the calling transaction manager
            self.logger.info(f"Bulk inserted {len(data_to_insert)} rows into elisp_links.")
        except Exception as e:
            self.logger.error(f"Failed to bulk insert into elisp_links: {e}")
            raise # Re-raise

    def bulk_insert_elisp_metadata(self, metadata_items: List[Dict[str, Any]]):
        """Bulk inserts data into the elisp_metadata table."""
        if not metadata_items:
            return
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            
            columns = ['meta_key', 'meta_value', 'last_updated']
            placeholders = ', '.join(['?'] * len(columns))
            sql = f"INSERT OR REPLACE INTO elisp_metadata ({', '.join(columns)}) VALUES ({placeholders})"

            data_to_insert = []
            for row_dict in metadata_items:
                row_tuple = tuple(row_dict.get(col) for col in columns)
                data_to_insert.append(row_tuple)
                
            cursor.executemany(sql, data_to_insert)
            # conn.commit() # Commit should be handled by the calling transaction manager
            self.logger.info(f"Bulk inserted {len(data_to_insert)} rows into elisp_metadata.")
        except Exception as e:
            self.logger.error(f"Failed to bulk insert into elisp_metadata: {e}")
            raise # Re-raise

    # --- NEW: Method for bulk upserting node chunks with embeddings ---
    def bulk_upsert_node_chunks(self, chunks_data: List[Dict[str, Any]]):
        """Bulk inserts or replaces data into the node_chunks table."""
        if not chunks_data:
            self.logger.info("bulk_upsert_node_chunks: No data provided.")
            return
        if not NP_AVAILABLE:
            self.logger.error("bulk_upsert_node_chunks: NumPy is not available, cannot process embeddings.")
            return

        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            
            # Define the columns for the node_chunks table
            # Ensure these match the table schema and the keys in your chunks_data dictionaries
            columns = [
                'chunk_id', 'node_id', 'text_chunk', 'embedding', 
                'headline', 'original_tags', 'file_path', 'node_modified_at'
                # 'chunk_created_at' uses DEFAULT CURRENT_TIMESTAMP
            ]
            placeholders = ', '.join(['?'] * len(columns))
            sql = f"INSERT OR REPLACE INTO node_chunks ({', '.join(columns)}) VALUES ({placeholders})"

            data_to_insert = []
            for row_dict in chunks_data:
                # Convert embedding list to blob
                embedding_list = row_dict.get('embedding')
                if embedding_list is not None and isinstance(embedding_list, list):
                    vector = np.array(embedding_list, dtype=np.float32)
                    row_dict['embedding'] = sqlite3.Binary(vector.tobytes())
                elif embedding_list is not None: # Not a list, maybe already bytes or wrong type
                    self.logger.warning(f"Embedding for chunk {row_dict.get('chunk_id')} is not a list, attempting to use as is or skip.")
                    if not isinstance(embedding_list, bytes):
                         row_dict['embedding'] = None # Or handle error
                else:
                    row_dict['embedding'] = None # Ensure embedding is None if not provided
                
                # Ensure original_tags is a JSON string if it's a list/dict
                tags_val = row_dict.get('original_tags')
                if tags_val is not None and not isinstance(tags_val, str):
                    row_dict['original_tags'] = json.dumps(tags_val)
                
                row_tuple = tuple(row_dict.get(col) for col in columns)
                data_to_insert.append(row_tuple)
                
            cursor.executemany(sql, data_to_insert)
            # conn.commit() # Commit should be handled by the calling transaction manager
            self.logger.info(f"Bulk inserted/updated {len(data_to_insert)} rows into node_chunks.")
        except Exception as e:
            self.logger.error(f"Failed to bulk upsert into node_chunks: {e}\n{traceback.format_exc()}")
            raise # Re-raise to allow transaction rollback
    # --- END NEW ---

    def get_db_connection(self):
        conn = self._get_connection()
        conn.commit()
        # logger.info(f"Bulk inserted/updated {len(data_to_insert)} tags.") # data_to_insert is not defined here
        self.logger.info("Committed transaction on current thread's connection.")

    def bulk_upsert_nodes(self, nodes_data: list):
        """
        Bulk inserts or updates nodes into the node_vectors table.
        'nodes_data' is a list of dictionaries, where each dict represents a node.
        """
        if not nodes_data:
            return

        conn = self._get_connection()
        cursor = conn.cursor()

        # Prepare data for upsert, ensuring all keys are present
        nodes_to_upsert = []
        for node in nodes_data:
            nodes_to_upsert.append((
                node.get('node_id'),
                node.get('content'),
                node.get('title'),
                json.dumps(node.get('tags', [])), # Tags as JSON string
                node.get('file_path'),
                node.get('pos'),
                node.get('olp'),
                node.get('level'),
                node.get('scheduled'),
                node.get('deadline'),
                node.get('todo'),
                node.get('priority'),
                node.get('modified_at'),
                json.dumps(node.get('properties', {})), # Properties as JSON string
                node.get('raw_value'),
                node.get('hash'),
                node.get('content_hash'),
                node.get('document_date') # Added for Knowledge Archaeology
            ))
        
        try:
            self.logger.info(f"💾 开始存储 {len(nodes_to_upsert)} 个节点到数据库")
            self.logger.info(f"🔧 VSS状态: {'激活' if self.has_vector_ext else '未激活'}")
            
            if self.has_vector_ext:
                # VSS is active, so node_vectors does NOT have vector columns
                sql = """
                    INSERT INTO node_vectors (
                        node_id, content, title, tags, file_path, pos, olp, level, scheduled,
                        deadline, todo, priority, modified_at, properties, raw_value, hash, content_hash, document_date
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    ON CONFLICT(node_id) DO UPDATE SET
                        content=excluded.content, title=excluded.title, tags=excluded.tags,
                        file_path=excluded.file_path, pos=excluded.pos, olp=excluded.olp,
                        level=excluded.level, scheduled=excluded.scheduled, deadline=excluded.deadline,
                        todo=excluded.todo, priority=excluded.priority, modified_at=excluded.modified_at,
                        properties=excluded.properties, raw_value=excluded.raw_value, hash=excluded.hash,
                        content_hash=excluded.content_hash, document_date=excluded.document_date;
                """
            else:
                # VSS not active, so we expect vector data to be managed here (or be NULL)
                # The bulk_upsert_node_embeddings should be used for vectors, so we assume NULL here
                self.logger.warning("bulk_upsert_nodes called without VSS. Vector data will not be handled in this call.")
                sql = """
                    INSERT INTO node_vectors (
                        node_id, content, title, tags, file_path, pos, olp, level, scheduled,
                        deadline, todo, priority, modified_at, properties, raw_value, hash, content_hash, document_date
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    ON CONFLICT(node_id) DO UPDATE SET
                        content=excluded.content, title=excluded.title, tags=excluded.tags,
                        file_path=excluded.file_path, pos=excluded.pos, olp=excluded.olp,
                        level=excluded.level, scheduled=excluded.scheduled, deadline=excluded.deadline,
                        todo=excluded.todo, priority=excluded.priority, modified_at=excluded.modified_at,
                        properties=excluded.properties, raw_value=excluded.raw_value, hash=excluded.hash,
                        content_hash=excluded.content_hash, document_date=excluded.document_date;
                """

            # 记录第一个节点的数据格式以供调试
            if nodes_to_upsert:
                first_node = nodes_to_upsert[0]
                self.logger.info(f"📋 样本节点数据: node_id={first_node[0]}, content_len={len(str(first_node[1])) if first_node[1] else 0}")
            
            cursor.executemany(sql, nodes_to_upsert)
            conn.commit()
            self.logger.info(f"✅ 成功存储 {len(nodes_to_upsert)} 个节点元数据到数据库")
        except Exception as e:
            self.logger.error(f"❌ bulk_upsert_nodes 存储失败: {e}", exc_info=True)
            self.logger.error(f"📊 失败的数据: {len(nodes_to_upsert)} 个节点")
            if nodes_to_upsert:
                self.logger.error(f"🔍 第一个节点ID: {nodes_to_upsert[0][0]}")
            conn.rollback()
            raise  # 重新抛出异常，确保调用者知道失败了

    def bulk_upsert_node_embeddings(self, node_embeddings_data: List[Dict[str, Any]]):
        """
        Bulk inserts or updates node embeddings.
        Only operates if self.has_vector_ext is True.
        Expects each dict in list to have 'node_id_ref' and 'embedding' (list of floats).
        """
        if not self.has_vector_ext:
            self.logger.info("bulk_upsert_node_embeddings: sqlite-vec extension not active. Skipping.")
            return

        if not node_embeddings_data:
            self.logger.info("bulk_upsert_node_embeddings: No embeddings data provided.")
            return

        conn = self._get_connection()
        cursor = conn.cursor()

        # For sqlite-vec, it's often an INSERT. If node_id_ref needs to be unique for replacement,
        # we might need a DELETE followed by INSERT, or rely on VSS table's rowid behavior.
        # Assuming node_id_ref is a direct reference and we want to replace if exists.
        # A common pattern for VSS is to delete old entries if updating.
        # DELETE FROM node_embeddings_vss WHERE node_id_ref = ?;
        # INSERT INTO node_embeddings_vss (node_id_ref, embedding) VALUES (?, ?);

        # Given `node_id_ref TEXT HIDDEN`, standard SQL for direct insert/upsert on node_id_ref is tricky.
        # sqlite-vec typically uses rowid for external mapping if a reference column isn't directly settable.
        # Let's assume `node_embeddings_vss` allows direct insert on `node_id_ref` if it's not truly hidden
        # for insert purposes or we map `node_id` to `rowid` in the calling code.
        # The `node_embeddings_vss` table has `node_id_ref TEXT HIDDEN`.
        # This means we likely cannot directly set `node_id_ref` on INSERT in plain SQL.
        # We might need to DELETE first based on `node_id_ref` if it's indexed and searchable.
        # Then INSERT the new embedding. Sqlite-vec docs mention:
        # "inserting a new vector into the table returns the rowid of the new vector"
        # "Data can be inserted using standard SQL INSERT statements. The only special column is the vector column itself"
        # Let's try a simple INSERT first and see. If node_id_ref is HIDDEN it means it is usually managed by a foreign key from vec0's perspective
        # or it's a column that is not returned by SELECT * but can be used in WHERE.

        # Safest approach: Delete then Insert if replacing.
        # For now, let's go with direct INSERT and assume node_id_ref can be set.
        # If an embedding for a node_id_ref already exists, this will add a duplicate unless
        # node_id_ref has a UNIQUE constraint (not specified in our VIRTUAL TABLE DDL for node_id_ref).
        # To ensure upsert-like behavior on node_id_ref, we'd first DELETE.
        
        self.logger.info(f"bulk_upsert_node_embeddings: Preparing to insert {len(node_embeddings_data)} node embeddings into VSS table.")
        
        sql_delete = "DELETE FROM node_embeddings_vss WHERE node_id_ref = ?;"
        sql_insert = "INSERT INTO node_embeddings_vss (node_id_ref, embedding) VALUES (?, ?);"
        
        embeddings_to_insert_tuples = []
        node_ids_to_delete = []

        for item in node_embeddings_data:
            node_id = item.get('node_id_ref')
            embedding = item.get('embedding')
            if node_id and embedding:
                node_ids_to_delete.append((node_id,))
                embeddings_to_insert_tuples.append((node_id, json.dumps(embedding))) # sqlite-vec often takes JSON for arrays if not direct list
            else:
                self.logger.warning(f"bulk_upsert_node_embeddings: Skipping item with missing node_id_ref or embedding: {item}")

        if not embeddings_to_insert_tuples:
            self.logger.info("bulk_upsert_node_embeddings: No valid embeddings data to insert after filtering.")
            return
            
        try:
            self.logger.info(f"🔢 开始存储 {len(embeddings_to_insert_tuples)} 个向量到VSS")
            
            # Perform DELETEs first
            if node_ids_to_delete:
                cursor.executemany(sql_delete, node_ids_to_delete)
                self.logger.info(f"🗑️ 删除了 {len(node_ids_to_delete)} 个旧向量")

            # Perform INSERTs
            cursor.executemany(sql_insert, embeddings_to_insert_tuples)
            conn.commit()
            self.logger.info(f"✅ 成功存储 {len(embeddings_to_insert_tuples)} 个向量到VSS")
            
            # 记录第一个向量的信息
            if embeddings_to_insert_tuples:
                first_embedding = embeddings_to_insert_tuples[0]
                self.logger.info(f"📊 样本向量: node_id={first_embedding[0]}, embedding_size={len(first_embedding[1])}")
                
        except Exception as e:
            self.logger.error(f"❌ bulk_upsert_node_embeddings 失败: {e}", exc_info=True)
            self.logger.error(f"📊 失败的向量数据: {len(embeddings_to_insert_tuples)} 个向量")
            if embeddings_to_insert_tuples:
                self.logger.error(f"🔍 第一个向量node_id: {embeddings_to_insert_tuples[0][0]}")
            conn.rollback()
            raise  # 重新抛出异常

    def bulk_upsert_tag_embeddings(self, tag_embeddings_data: List[Dict[str, Any]]):
        """Bulk INSERT for tag embeddings into the VSS table (tag_embeddings_vss).
        Only operates if self.has_vector_ext is True.
        Expects each dict in list to have 'tag_id_ref' and 'embedding' (list of floats).
        """
        if not self.has_vector_ext:
            self.logger.info("bulk_upsert_tag_embeddings: sqlite-vec extension not active. Skipping.")
            return

        if not tag_embeddings_data:
            self.logger.info("bulk_upsert_tag_embeddings: No embeddings data provided.")
            return

        conn = self._get_connection()
        cursor = conn.cursor()
        
        self.logger.info(f"bulk_upsert_tag_embeddings: Preparing to insert {len(tag_embeddings_data)} tag embeddings into VSS table.")
        
        sql_delete = "DELETE FROM tag_embeddings_vss WHERE tag_id_ref = ?;"
        sql_insert = "INSERT INTO tag_embeddings_vss (tag_id_ref, embedding) VALUES (?, ?);"
        
        embeddings_to_insert_tuples = []
        tag_ids_to_delete = []

        for item in tag_embeddings_data:
            tag_id = item.get('tag_id_ref')
            embedding = item.get('embedding') # Should be a list of floats
            if tag_id and embedding and isinstance(embedding, list):
                tag_ids_to_delete.append((tag_id,))
                # sqlite-vec expects embedding as a JSON string of a list for float arrays
                embeddings_to_insert_tuples.append((tag_id, json.dumps(embedding)))
            else:
                self.logger.warning(f"bulk_upsert_tag_embeddings: Skipping item with missing tag_id_ref, non-list embedding, or empty embedding: {item}")

        if not embeddings_to_insert_tuples:
            self.logger.info("bulk_upsert_tag_embeddings: No valid embeddings data to insert after filtering.")
            return
            
        try:
            # Perform DELETEs first to ensure idempotency for upsert behavior
            if tag_ids_to_delete:
                cursor.executemany(sql_delete, tag_ids_to_delete)
                self.logger.info(f"bulk_upsert_tag_embeddings: Executed DELETE for {len(tag_ids_to_delete)} tag_id_refs.")

            # Perform INSERTs
            cursor.executemany(sql_insert, embeddings_to_insert_tuples)
            conn.commit()
            self.logger.info(f"bulk_upsert_tag_embeddings: Successfully inserted {len(embeddings_to_insert_tuples)} tag embeddings.")
        except Exception as e:
            self.logger.error(f"bulk_upsert_tag_embeddings: Error during bulk upsert of tag embeddings: {e}\n{traceback.format_exc()}", exc_info=True)
            # conn.rollback() # Consider if rollback should be managed by caller

    def get_tag_by_id(self, tag_id):
        conn = self._get_connection()
        cursor = conn.cursor()
        # Select from tag_metadata. Vector is handled by get_tag_vector separately.
        # Ensure all desired metadata columns are selected.
        cursor.execute("SELECT tag_id, name, description, created_at, modified_at FROM tag_metadata WHERE tag_id = ?", (tag_id,))
        return cursor.fetchone()

    def get_all_node_ids(self) -> set[str]:
        """Retrieves all node_ids from the node_vectors table."""
        conn = self._get_connection()
        cursor = conn.cursor()
        try:
            cursor.execute("SELECT node_id FROM node_vectors")
            rows = cursor.fetchall()
            return {row[0] for row in rows}
        except Exception as e:
            self.logger.error(f"Failed to get all node IDs: {e}\n{traceback.format_exc()}", exc_info=True)
            return set()

    def delete_nodes_by_ids(self, node_ids: List[str]):
        """Deletes nodes by their IDs from node_vectors and node_embeddings_vss (if VSS active)."""
        if not node_ids:
            self.logger.info("delete_nodes_by_ids: No node IDs provided for deletion.")
            return

        conn = self._get_connection()
        cursor = conn.cursor()
        
        try:
            # Create placeholders for IN clause: (?, ?, ...)
            placeholders = ', '.join(['?'] * len(node_ids))
            
            # Delete from node_vectors (metadata table)
            sql_delete_metadata = f"DELETE FROM node_vectors WHERE node_id IN ({placeholders})"
            cursor.execute(sql_delete_metadata, node_ids)
            self.logger.info(f"Deleted {cursor.rowcount} rows from node_vectors for {len(node_ids)} IDs.")

            # If VSS is active, delete from node_embeddings_vss
            if self.has_vector_ext:
                sql_delete_vss = f"DELETE FROM node_embeddings_vss WHERE node_id_ref IN ({placeholders})"
                cursor.execute(sql_delete_vss, node_ids)
                self.logger.info(f"Deleted {cursor.rowcount} rows from node_embeddings_vss for {len(node_ids)} IDs.")
            
            conn.commit()
            self.logger.info(f"Successfully deleted data for {len(node_ids)} node IDs.")
        except Exception as e:
            self.logger.error(f"delete_nodes_by_ids: Error during bulk deletion: {e}\n{traceback.format_exc()}", exc_info=True)
            conn.rollback() # Rollback on error

    def delete_tags_by_ids(self, tag_ids: List[str]):
        """Deletes tags by their IDs from tag_metadata and tag_embeddings_vss (if VSS active)."""
        if not tag_ids:
            self.logger.info("delete_tags_by_ids: No tag IDs provided for deletion.")
            return

        conn = self._get_connection()
        cursor = conn.cursor()
        
        try:
            placeholders = ', '.join(['?'] * len(tag_ids))
            
            # Delete from tag_metadata
            sql_delete_metadata = f"DELETE FROM tag_metadata WHERE tag_id IN ({placeholders})"
            cursor.execute(sql_delete_metadata, tag_ids)
            self.logger.info(f"Deleted {cursor.rowcount} rows from tag_metadata for {len(tag_ids)} IDs.")

            if self.has_vector_ext:
                sql_delete_vss = f"DELETE FROM tag_embeddings_vss WHERE tag_id_ref IN ({placeholders})"
                cursor.execute(sql_delete_vss, tag_ids)
                self.logger.info(f"Deleted {cursor.rowcount} rows from tag_embeddings_vss for {len(tag_ids)} IDs.")
            
            conn.commit()
            self.logger.info(f"Successfully deleted data for {len(tag_ids)} tag IDs.")
        except Exception as e:
            self.logger.error(f"delete_tags_by_ids: Error during bulk deletion of tags: {e}\n{traceback.format_exc()}", exc_info=True)
            conn.rollback() # Rollback on error

    def find_similar_node_embeddings(self, query_vector: np.ndarray, top_k: int = 10) -> List[Tuple[str, float]]:
        """Finds similar node embeddings using the VSS table.

        Args:
            query_vector: The numpy array of the query vector.
            top_k: The number of similar nodes to return.

        Returns:
            A list of tuples, where each tuple is (node_id_ref, similarity_score).
            Returns empty list if VSS is not active or an error occurs.
        """
        if not self.has_vector_ext:
            self.logger.warning("find_similar_node_embeddings: sqlite-vec extension not active. Cannot perform VSS search.")
            return []
        
        if not NP_AVAILABLE:
            self.logger.error("find_similar_node_embeddings: NumPy is not available, cannot process query vector.")
            return []

        if query_vector is None or query_vector.size == 0:
            self.logger.warning("find_similar_node_embeddings: Query vector is None or empty.")
            return []

        conn = self._get_connection()
        cursor = conn.cursor()

        try:
            # sqlite-vec expects the query vector as a JSON string of a list, or a bytes blob.
            # Let's use JSON string to be consistent with how embeddings are inserted by bulk_upsert_node_embeddings.
            query_vector_json = json.dumps(query_vector.tolist())

            # The 'distance' column in sqlite-vec is cosine distance (0.0 to 2.0).
            # To get cosine similarity (1.0 to -1.0), we can use: 1 - distance / 2 if vectors are normalized,
            # or more generally, just return the distance and let the caller interpret.
            # For now, let's return distance, as it's the direct output. Higher score = less similar.
            # The column name `node_id_ref` is what we defined in `node_embeddings_vss`
            sql = """
                SELECT node_id_ref, distance
                FROM node_embeddings_vss
                WHERE embedding MATCH ? 
                ORDER BY distance
                LIMIT ?;
            """
            
            self.logger.debug(f"Executing VSS search: top_k={top_k}")
            cursor.execute(sql, (query_vector_json, top_k))
            results = cursor.fetchall()
            
            # Convert results: distance to a similarity score if desired, or just return distance.
            # For now, returning (node_id, distance). Caller should be aware lower distance = more similar.
            # If we want actual cosine similarity, and if vectors are L2 normalized, similarity = 1 - distance.
            # Let's assume for now client wants distance.
            similar_nodes = [(row[0], float(row[1])) for row in results]
            self.logger.info(f"VSS search found {len(similar_nodes)} similar nodes (top_k={top_k}).")
            return similar_nodes

        except Exception as e:
            self.logger.error(f"find_similar_node_embeddings: Error during VSS search: {e}\n{traceback.format_exc()}", exc_info=True)
            return []

    def get_node_embedding_by_id(self, node_id: str) -> Optional[np.ndarray]:
        """Retrieves a node's embedding from the VSS table by its ID.

        Args:
            node_id: The ID of the node whose embedding is to be retrieved.

        Returns:
            A NumPy array of the embedding, or None if not found, VSS inactive, or error.
        """
        if not self.has_vector_ext:
            self.logger.debug(f"get_node_embedding_by_id: sqlite-vec extension not active. Cannot retrieve embedding for node {node_id}.")
            return None
        
        if not NP_AVAILABLE:
            self.logger.error("get_node_embedding_by_id: NumPy is not available, cannot process embedding.")
            return None

        conn = self._get_connection()
        cursor = conn.cursor()
        sql = "SELECT embedding FROM node_embeddings_vss WHERE node_id_ref = ? LIMIT 1;"

        try:
            self.logger.debug(f"Attempting to retrieve embedding for node_id: {node_id}")
            cursor.execute(sql, (node_id,))
            row = cursor.fetchone()

            if row and row[0]:
                embedding_data = row[0]
                if isinstance(embedding_data, bytes):
                    self.logger.debug(f"Node embedding for node_id: {node_id} is BYTES. Processing with np.frombuffer.")
                    try:
                        embedding_np = np.frombuffer(embedding_data, dtype=np.float32)
                        self.logger.debug(f"Successfully parsed VSS BYTES embedding for node_id: {node_id}, dim: {len(embedding_np)}")
                        return embedding_np
                    except Exception as ex_bytes:
                        self.logger.error(f"Failed to parse VSS BYTES embedding for node {node_id}: {ex_bytes}\n{traceback.format_exc()}")
                        return None
                elif isinstance(embedding_data, str):
                    self.logger.debug(f"Node embedding for node_id: {node_id} is STRING. Processing with json.loads.")
                    try:
                        embedding_list = json.loads(embedding_data)
                        embedding_np = np.array(embedding_list, dtype=np.float32)
                        self.logger.debug(f"Successfully retrieved and parsed VSS JSON STRING embedding for node_id: {node_id}")
                        return embedding_np
                    except json.JSONDecodeError as jde:
                        self.logger.error(f"Failed to decode JSON for embedding of node {node_id}: {jde}")
                        return None
                    except Exception as ex_json_conv: # Catch other potential errors during array conversion
                        self.logger.error(f"Error converting VSS JSON embedding for node {node_id}: {ex_json_conv}")
                        return None
                else:
                    self.logger.error(f"Unexpected data type for VSS embedding of node {node_id}: {type(embedding_data)}. Expected str.")
                    return None
            else:
                self.logger.debug(f"No embedding found in VSS table for node_id: {node_id}")
                return None
        except Exception as e:
            self.logger.error(f"get_node_embedding_by_id: Error retrieving embedding for node {node_id}: {e}\n{traceback.format_exc()}", exc_info=True)
            return None

    def get_node_details_by_ids(self, node_ids: List[str]) -> List[Dict[str, Any]]:
        """
        Retrieves metadata (id, title, document_date) for a list of node_ids.
        Returns a list of dictionaries, each containing 'node_id', 'title', and 'document_date'.
        """
        if not node_ids:
            return []
        
        conn = self._get_connection()
        cursor = conn.cursor()
        
        # Using placeholders to prevent SQL injection
        placeholders = ','.join('?' for _ in node_ids)
        query = f"SELECT node_id, title, document_date FROM node_vectors WHERE node_id IN ({placeholders})"
        
        self.logger.debug(f"Executing get_node_details_by_ids with query: {query} and IDs: {node_ids}")
        
        try:
            cursor.execute(query, node_ids)
            rows = cursor.fetchall()
            # Convert rows (tuples) to list of dictionaries
            results = [
                {'node_id': row[0], 'title': row[1], 'document_date': row[2]}
                for row in rows
            ]
            return results
        except Exception as e:
            self.logger.error(f"Error fetching node details: {e}\n{traceback.format_exc()}")
            return []

    def get_node_content_by_ids(self, node_ids: List[str]) -> Dict[str, str]:
        """
        获取节点内容用于用户友好显示
        
        Args:
            node_ids: 节点ID列表
            
        Returns:
            字典格式 {node_id: content, ...}
        """
        if not node_ids:
            return {}
        
        conn = self._get_connection()
        cursor = conn.cursor()
        
        # Using placeholders to prevent SQL injection
        placeholders = ','.join('?' for _ in node_ids)
        query = f"SELECT node_id, content FROM node_vectors WHERE node_id IN ({placeholders})"
        
        self.logger.debug(f"Executing get_node_content_by_ids with query: {query} and IDs: {node_ids}")
        
        try:
            cursor.execute(query, node_ids)
            rows = cursor.fetchall()
            # Convert to dictionary {node_id: content}
            content_dict = {row[0]: row[1] or "" for row in rows}
            return content_dict
        except Exception as e:
            self.logger.error(f"Error fetching node content: {e}\n{traceback.format_exc()}")
            return {}

    def search_nodes_by_title_content(self, search_query: str, limit: int = 10) -> List[Dict[str, Any]]:
        """
        根据标题和内容搜索节点
        
        Args:
            search_query: 搜索查询
            limit: 返回结果限制
            
        Returns:
            包含节点信息的列表
        """
        if not search_query.strip():
            return []
        
        conn = self._get_connection()
        cursor = conn.cursor()
        
        # 使用 LIKE 进行模糊搜索，同时搜索标题和内容
        search_pattern = f"%{search_query.strip()}%"
        query = """
            SELECT node_id, title, content, document_date
            FROM node_vectors 
            WHERE title LIKE ? OR content LIKE ?
            ORDER BY 
                CASE 
                    WHEN title LIKE ? THEN 1 
                    ELSE 2 
                END,
                LENGTH(title) ASC
            LIMIT ?
        """
        
        self.logger.debug(f"Executing search_nodes_by_title_content with pattern: {search_pattern}")
        
        try:
            cursor.execute(query, (search_pattern, search_pattern, search_pattern, limit))
            rows = cursor.fetchall()
            
            results = []
            for row in rows:
                node_id, title, content, document_date = row
                results.append({
                    'node_id': node_id,
                    'title': title or f"节点 {node_id[:8]}...",
                    'content': content or "",
                    'document_date': document_date
                })
            
            self.logger.info(f"搜索 '{search_query}' 找到 {len(results)} 个结果")
            return results
            
        except Exception as e:
            self.logger.error(f"Error searching nodes: {e}\n{traceback.format_exc()}")
            return []

    def close(self):
        """Close the database connection for the current thread."""
        self.local.conn.close()
        self.local.conn = None

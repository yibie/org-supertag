"""
Vector Storage Abstraction Layer - SQLite Implementation
"""
import sqlite3
import numpy as np
import json
import logging
import threading
from typing import List, Dict, Any, Tuple

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
            
            # Check if table exists
            cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='tag_vectors'")
            table_exists = cursor.fetchone() is not None
            
            self.logger.info(f"Database initialization: tag_vectors table existence = {table_exists}")
            
            # Create tag vectors table
            self.logger.debug("Creating tag vectors table")
            cursor.execute("""
            CREATE TABLE IF NOT EXISTS tag_vectors (
                tag_id TEXT PRIMARY KEY,
                vector BLOB,
                vector_dim INTEGER,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
            """)
            
            # Validate table structure
            cursor.execute("PRAGMA table_info(tag_vectors)")
            columns = cursor.fetchall()
            column_names = [col[1] for col in columns]
            self.logger.debug(f"tag_vectors table structure: {columns}")
            
            # Check if required columns exist
            if 'tag_id' not in column_names or 'vector' not in column_names:
                self.logger.warning(f"tag_vectors table missing required columns, current columns: {column_names}")
                self.logger.info("Attempting to recreate table structure")
                cursor.execute("DROP TABLE IF EXISTS tag_vectors")
                conn.commit()
                # Recreate table
                cursor.execute("""
                CREATE TABLE tag_vectors (
                    tag_id TEXT PRIMARY KEY,
                    vector BLOB,
                    vector_dim INTEGER,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
                """)
            
            # Create node vectors table
            self.logger.debug("Creating node vectors table")
            cursor.execute("""
            CREATE TABLE IF NOT EXISTS node_vectors (
                node_id TEXT PRIMARY KEY,
                vector BLOB,
                vector_dim INTEGER,
                content TEXT,
                title TEXT,
                tags TEXT,  
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
            """)
            
            conn.commit()
            self.logger.info("Database table structure initialization complete")
            
            # Check vector extension
            if self.has_vector_ext:
                self.check_vec_extension()
        except Exception as e:
            self.logger.error(f"Failed to initialize database table structure: {e}")
            raise
    
    def save_tag_vector(self, tag_id: str, vector: list, vector_dim: int = None):
        """Save tag vector (supports list input)
        
        Args:
            tag_id: Tag ID
            vector: Vector data (list or numpy array)
            vector_dim: Vector dimension (if not provided, uses vector length)
        """
        try:
            # Ensure vector is numpy array
            if not isinstance(vector, np.ndarray):
                vector = np.array(vector, dtype=np.float32)
            else:
                # Ensure vector is float32 type
                vector = vector.astype(np.float32)
            
            # If dimension not provided, use vector length
            if vector_dim is None:
                vector_dim = len(vector)
            
            # Check if vector has valid values
            if np.isnan(vector).any() or np.isinf(vector).any():
                self.logger.warning(f"Vector contains NaN or Inf values: {tag_id}")
                # Replace NaN and Inf with 0
                vector = np.nan_to_num(vector, nan=0.0, posinf=1.0, neginf=-1.0)
            
            # Serialize vector data
            vector_blob = vector.tobytes()
            
            conn = self._get_connection()
            cursor = conn.cursor()
            cursor.execute("""
            INSERT OR REPLACE INTO tag_vectors (tag_id, vector, vector_dim)
            VALUES (?, ?, ?)
            """, (tag_id, vector_blob, vector_dim))
            conn.commit()
            self.logger.debug(f"Saved tag vector: {tag_id}, dimension: {vector_dim}")
            return True
        except Exception as e:
            self.logger.error(f"Failed to save tag vector: {e}")
            return False
    
    def delete_tag_vector(self, tag_id: str) -> bool:
        """Delete tag vector
        
        Args:
            tag_id: Tag ID
            
        Returns:
            Whether deletion was successful
        """
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            cursor.execute("DELETE FROM tag_vectors WHERE tag_id = ?", (tag_id,))
            conn.commit()
            self.logger.debug(f"Deleted tag vector: {tag_id}")
            return True
        except Exception as e:
            self.logger.error(f"Failed to delete tag vector: {e}")
            return False
    
    def list_tag_ids(self) -> List[str]:
        """Get list of all tag IDs
        
        Returns:
            List of tag IDs
        """
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            
            # First check if table exists
            cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='tag_vectors'")
            if cursor.fetchone() is None:
                self.logger.warning("tag_vectors table does not exist, creating...")
                self._init_db()
                return []
            
            # Check if columns exist
            cursor.execute("PRAGMA table_info(tag_vectors)")
            columns = [col[1] for col in cursor.fetchall()]
            
            if 'tag_id' not in columns:
                self.logger.error(f"tag_vectors table missing tag_id column, current columns: {columns}")
                # Try to rebuild table
                cursor.execute("DROP TABLE IF EXISTS tag_vectors")
                conn.commit()
                self._init_db()
                return []
            
            # Query tag IDs
            cursor.execute("SELECT tag_id FROM tag_vectors")
            result = [row[0] for row in cursor.fetchall()]
            self.logger.debug(f"Retrieved {len(result)} tag IDs")
            return result
        except Exception as e:
            self.logger.error(f"Failed to get tag ID list: {e}")
            # Return empty list instead of raising exception to ensure caller can continue
            return []
    
    def has_tag_vector(self, tag_id: str) -> bool:
        """Check if tag has vector
        
        Args:
            tag_id: Tag ID
            
        Returns:
            Whether vector exists for this tag
        """
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            cursor.execute("SELECT 1 FROM tag_vectors WHERE tag_id = ? LIMIT 1", (tag_id,))
            return cursor.fetchone() is not None
        except Exception as e:
            self.logger.error(f"Failed to check tag vector: {e}")
            return False
    
    def get_tag_vector(self, tag_id: str) -> np.ndarray:
        """Get tag vector
        
        Args:
            tag_id: Tag ID
            
        Returns:
            Tag vector (numpy array), returns None if not found
        """
        try:
            conn = self._get_connection()
            cursor = conn.cursor()
            cursor.execute("SELECT vector, vector_dim FROM tag_vectors WHERE tag_id = ?", (tag_id,))
            row = cursor.fetchone()
            
            if not row:
                self.logger.debug(f"未找到标签向量: {tag_id}")
                return None
                
            vector_blob, vector_dim = row
            
            # 尝试解析向量数据
            try:
                # 首先尝试float32格式
                vector = np.frombuffer(vector_blob, dtype=np.float32)
                
                # 验证维度
                if len(vector) != vector_dim:
                    self.logger.warning(f"向量维度不匹配: 存储={vector_dim}, 实际={len(vector)}")
                    
                    # 尝试float64格式
                    vector = np.frombuffer(vector_blob, dtype=np.float64)
                    
                    if len(vector) != vector_dim:
                        self.logger.warning(f"所有尝试都失败，返回原始向量并截断/填充到正确维度")
                        # 如果仍然不匹配，返回截断/填充后的向量
                        if len(vector) > vector_dim:
                            vector = vector[:vector_dim]
                        elif len(vector) < vector_dim:
                            padding = np.zeros(vector_dim - len(vector), dtype=vector.dtype)
                            vector = np.concatenate([vector, padding])
            except Exception as e:
                self.logger.warning(f"解析向量数据失败: {tag_id}, 错误: {e}")
                return None
            
            # 确保返回float32类型
            return vector.astype(np.float32)
        except Exception as e:
            self.logger.error(f"获取标签向量失败: {e}")
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
        """查找相似标签"""
        try:
            if not self.has_vector_ext:
                self.logger.error("向量扩展不可用，无法执行相似度查询")
                return []
                
            # 使用向量扩展进行查询
            query_blob = query_vector.tobytes()
            conn = self._get_connection()
            cursor = conn.cursor()
            
            # 尝试使用向量扩展的各种函数
            try:
                cursor.execute("""
                SELECT tag_id, vec_distance_cosine(vector, ?) as dist
                FROM tag_vectors
                ORDER BY dist ASC
                LIMIT ?
                """, (query_blob, top_k))
                return [(row[0], row[1]) for row in cursor.fetchall()]
            except sqlite3.OperationalError as e:
                self.logger.warning(f"向量扩展查询失败: {e}")
                return []
                
        except Exception as e:
            self.logger.error(f"相似标签查询失败: {e}")
            return []
    
    def save_node_vector(self, node_id: str, vector: np.ndarray, content: str = "", title: str = "", tags: List[str] = None):
        """保存节点向量"""
        try:
            vector_blob = vector.tobytes()
            tags_json = json.dumps(tags) if tags else "[]"
            conn = self._get_connection()
            cursor = conn.cursor()
            cursor.execute("""
            INSERT OR REPLACE INTO node_vectors (node_id, vector, vector_dim, content, title, tags)
            VALUES (?, ?, ?, ?, ?, ?)
            """, (node_id, vector_blob, len(vector), content, title, tags_json))
            conn.commit()
            return True
        except Exception as e:
            self.logger.error(f"保存节点向量失败: {e}")
            return False
    
    def get_all_tags(self) -> List[str]:
        """获取所有标签ID"""
        conn = self._get_connection()
        cursor = conn.cursor()
        cursor.execute("SELECT tag_id FROM tag_vectors")
        return [row[0] for row in cursor.fetchall()]
    
    def get_stats(self) -> Dict[str, Any]:
        """获取存储统计信息"""
        conn = self._get_connection()
        cursor = conn.cursor()
        
        # 标签向量数量
        cursor.execute("SELECT COUNT(*) FROM tag_vectors")
        tag_count = cursor.fetchone()[0]
        
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

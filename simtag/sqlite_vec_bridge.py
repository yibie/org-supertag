"""
SQLite-vec Bridge Module - 为Emacs提供向量搜索功能
通过EPC调用sqlite-vec，实现高性能向量搜索
"""

import logging
import sqlite3
import json
import os
import traceback
from typing import List, Dict, Any, Optional, Tuple
import numpy as np

try:
    import sqlite_vec
    SQLITE_VEC_AVAILABLE = True
except ImportError:
    SQLITE_VEC_AVAILABLE = False

class SqliteVecBridge:
    """SQLite-vec桥接器，为Emacs提供向量搜索功能"""
    
    def __init__(self, db_path: Optional[str] = None):
        """初始化SQLite-vec桥接器
        
        Args:
            db_path: 数据库文件路径，如果为None则使用内存数据库
        """
        self.logger = logging.getLogger("simtag.sqlite_vec_bridge")
        self.db_path = db_path or ":memory:"
        self.db = None
        self.logger.info(f"初始化SQLite-vec桥接器，数据库: {self.db_path}")
        
        # 检查sqlite-vec可用性
        if SQLITE_VEC_AVAILABLE:
            self.logger.info("✅ sqlite-vec包可用")
        else:
            self.logger.warning("sqlite-vec未安装，某些功能将不可用")
    
    def connect(self) -> bool:
        """连接到数据库并加载sqlite-vec扩展
        
        Returns:
            bool: 连接是否成功
        """
        try:
            self.db = sqlite3.connect(self.db_path)
            self.db.enable_load_extension(True)
            
            if SQLITE_VEC_AVAILABLE:
                sqlite_vec.load(self.db)
                self.logger.info("✅ 成功加载sqlite-vec扩展")
            else:
                self.logger.warning("⚠️ sqlite-vec不可用，仅支持基本功能")
            
            return True
            
        except Exception as e:
            self.logger.error(f"❌ 连接数据库失败: {e}")
            return False
    
    def disconnect(self):
        """断开数据库连接"""
        if self.db:
            self.db.close()
            self.db = None
            self.logger.info("数据库连接已关闭")
    
    def create_vector_table(self, table_name: str, vector_dim: int, 
                          metadata_columns: Optional[Dict[str, str]] = None) -> bool:
        """创建向量表
        
        Args:
            table_name: 表名
            vector_dim: 向量维度
            metadata_columns: 元数据列定义，格式: {"column_name": "column_type"}
            
        Returns:
            bool: 创建是否成功
        """
        try:
            if not self.db:
                raise Exception("数据库未连接")
            
            if not SQLITE_VEC_AVAILABLE:
                raise Exception("sqlite-vec不可用")
            
            # 构建CREATE TABLE语句
            columns = [f"embedding float[{vector_dim}]"]
            if metadata_columns:
                for col_name, col_type in metadata_columns.items():
                    columns.append(f"{col_name} {col_type}")
            
            sql = f"CREATE VIRTUAL TABLE IF NOT EXISTS {table_name} USING vec0({', '.join(columns)})"
            
            self.db.execute(sql)
            self.db.commit()
            
            self.logger.info(f"✅ 成功创建向量表: {table_name}")
            return True
            
        except Exception as e:
            self.logger.error(f"❌ 创建向量表失败: {e}")
            return False
    
    def insert_vector(self, table_name: str, embedding: List[float], 
                     metadata: Optional[Dict[str, Any]] = None, 
                     rowid: Optional[int] = None) -> bool:
        """插入向量数据
        
        Args:
            table_name: 表名
            embedding: 向量数据
            metadata: 元数据
            rowid: 行ID（可选）
            
        Returns:
            bool: 插入是否成功
        """
        try:
            if not self.db:
                raise Exception("数据库未连接")
            
            if not SQLITE_VEC_AVAILABLE:
                raise Exception("sqlite-vec不可用")
            
            # 构建INSERT语句
            columns = ["embedding"]
            values = [json.dumps(embedding)]
            placeholders = ["?"]
            
            if rowid is not None:
                columns.insert(0, "rowid")
                values.insert(0, rowid)
                placeholders.insert(0, "?")
            
            if metadata:
                for key, value in metadata.items():
                    columns.append(key)
                    values.append(value)
                    placeholders.append("?")
            
            sql = f"INSERT INTO {table_name}({', '.join(columns)}) VALUES ({', '.join(placeholders)})"
            
            self.db.execute(sql, values)
            self.db.commit()
            
            self.logger.debug(f"✅ 成功插入向量到 {table_name}")
            return True
            
        except Exception as e:
            self.logger.error(f"❌ 插入向量失败: {e}")
            return False
    
    def vector_search(self, table_name: str, query_embedding: List[float], 
                     k: int = 10, where_clause: Optional[str] = None,
                     where_params: Optional[List] = None) -> List[Dict[str, Any]]:
        """执行向量搜索
        
        Args:
            table_name: 表名
            query_embedding: 查询向量
            k: 返回结果数量
            where_clause: WHERE条件（不包含向量搜索）
            where_params: WHERE条件参数
            
        Returns:
            List[Dict]: 搜索结果
        """
        try:
            if not self.db:
                raise Exception("数据库未连接")
            
            if not SQLITE_VEC_AVAILABLE:
                raise Exception("sqlite-vec不可用")
            
            # 构建查询语句
            base_sql = f"SELECT rowid, distance FROM {table_name}"
            
            # 构建WHERE条件
            where_conditions = [f"embedding MATCH ? AND k = {k}"]
            params = [json.dumps(query_embedding)]
            
            if where_clause:
                where_conditions.append(where_clause)
                if where_params:
                    params.extend(where_params)
            
            sql = f"{base_sql} WHERE {' AND '.join(where_conditions)} ORDER BY distance"
            
            cursor = self.db.execute(sql, params)
            results = cursor.fetchall()
            
            # 转换为字典格式
            formatted_results = []
            for row in results:
                formatted_results.append({
                    "rowid": row[0],
                    "distance": float(row[1])
                })
            
            self.logger.info(f"✅ 向量搜索完成，返回 {len(formatted_results)} 个结果")
            return formatted_results
            
        except Exception as e:
            self.logger.error(f"❌ 向量搜索失败: {e}")
            return []
    
    def vector_search_with_metadata(self, table_name: str, query_embedding: List[float],
                                  metadata_table: str, join_column: str,
                                  k: int = 10, select_columns: Optional[List[str]] = None) -> List[Dict[str, Any]]:
        """执行带元数据的向量搜索
        
        Args:
            table_name: 向量表名
            query_embedding: 查询向量
            metadata_table: 元数据表名
            join_column: 连接列名
            k: 返回结果数量
            select_columns: 要选择的列
            
        Returns:
            List[Dict]: 搜索结果
        """
        try:
            if not self.db:
                raise Exception("数据库未连接")
            
            if not SQLITE_VEC_AVAILABLE:
                raise Exception("sqlite-vec不可用")
            
            # 构建查询语句
            if select_columns:
                select_clause = ", ".join([f"m.{col}" for col in select_columns])
            else:
                select_clause = "m.*"
            
            sql = f"""
                SELECT v.rowid, v.distance, {select_clause}
                FROM {table_name} v
                JOIN {metadata_table} m ON v.rowid = m.{join_column}
                WHERE v.embedding MATCH ? AND k = {k}
                ORDER BY v.distance
            """
            
            params = [json.dumps(query_embedding)]
            cursor = self.db.execute(sql, params)
            results = cursor.fetchall()
            
            # 获取列名
            column_names = [description[0] for description in cursor.description]
            
            # 转换为字典格式
            formatted_results = []
            for row in results:
                result_dict = {}
                for i, value in enumerate(row):
                    result_dict[column_names[i]] = value
                formatted_results.append(result_dict)
            
            self.logger.info(f"✅ 带元数据的向量搜索完成，返回 {len(formatted_results)} 个结果")
            return formatted_results
            
        except Exception as e:
            self.logger.error(f"❌ 带元数据的向量搜索失败: {e}")
            return []
    
    def get_vector_by_rowid(self, table_name: str, rowid: int) -> Optional[Dict[str, Any]]:
        """根据rowid获取向量数据
        
        Args:
            table_name: 表名
            rowid: 行ID
            
        Returns:
            Optional[Dict]: 向量数据
        """
        try:
            if not self.db:
                raise Exception("数据库未连接")
            
            sql = f"SELECT * FROM {table_name} WHERE rowid = ?"
            cursor = self.db.execute(sql, [rowid])
            result = cursor.fetchone()
            
            if result:
                column_names = [description[0] for description in cursor.description]
                result_dict = {}
                for i, value in enumerate(result):
                    result_dict[column_names[i]] = value
                return result_dict
            
            return None
            
        except Exception as e:
            self.logger.error(f"❌ 获取向量数据失败: {e}")
            return None
    
    def status(self) -> Dict[str, Any]:
        """获取桥接器状态
        
        Returns:
            Dict: 状态信息
        """
        return {
            "sqlite_vec_available": SQLITE_VEC_AVAILABLE,
            "connected": self.db is not None,
            "db_path": self.db_path
        }

# EPC服务函数
def create_sqlite_vec_service():
    """创建SQLite-vec EPC服务"""
    bridge = SqliteVecBridge()
    
    def epc_connect(db_path=None):
        """EPC: 连接数据库"""
        if db_path:
            bridge.db_path = db_path
        return bridge.connect()
    
    def epc_disconnect():
        """EPC: 断开连接"""
        bridge.disconnect()
        return True
    
    def epc_create_vector_table(table_name, vector_dim, metadata_columns=None):
        """EPC: 创建向量表"""
        return bridge.create_vector_table(table_name, vector_dim, metadata_columns)
    
    def epc_insert_vector(table_name, embedding, metadata=None, rowid=None):
        """EPC: 插入向量"""
        return bridge.insert_vector(table_name, embedding, metadata, rowid)
    
    def epc_vector_search(table_name, query_embedding, k=10, where_clause=None, where_params=None):
        """EPC: 向量搜索"""
        return bridge.vector_search(table_name, query_embedding, k, where_clause, where_params)
    
    def epc_vector_search_with_metadata(table_name, query_embedding, metadata_table, 
                                      join_column, k=10, select_columns=None):
        """EPC: 带元数据的向量搜索"""
        return bridge.vector_search_with_metadata(table_name, query_embedding, metadata_table,
                                                join_column, k, select_columns)
    
    def epc_get_vector_by_rowid(table_name, rowid):
        """EPC: 根据rowid获取向量"""
        return bridge.get_vector_by_rowid(table_name, rowid)
    
    def epc_status():
        """EPC: 获取状态"""
        return bridge.status()
    
    # 返回EPC服务函数字典
    return {
        "sqlite_vec_connect": epc_connect,
        "sqlite_vec_disconnect": epc_disconnect,
        "sqlite_vec_create_table": epc_create_vector_table,
        "sqlite_vec_insert": epc_insert_vector,
        "sqlite_vec_search": epc_vector_search,
        "sqlite_vec_search_with_metadata": epc_vector_search_with_metadata,
        "sqlite_vec_get_by_rowid": epc_get_vector_by_rowid,
        "sqlite_vec_status": epc_status
    }

def _test():
    """测试SQLite-vec桥接器"""
    logging.basicConfig(level=logging.DEBUG)
    logger = logging.getLogger("test")
    
    try:
        logger.info("🚀 开始测试SQLite-vec桥接器...")
        
        # 创建桥接器
        bridge = SqliteVecBridge("test_bridge.db")
        
        # 连接数据库
        if not bridge.connect():
            logger.error("❌ 连接失败")
            return False
        
        # 创建向量表
        if not bridge.create_vector_table("test_vectors", 4, {"content": "TEXT", "tag": "TEXT"}):
            logger.error("❌ 创建表失败")
            return False
        
        # 插入测试数据
        test_data = [
            ([0.1, 0.2, 0.3, 0.4], {"content": "Python编程", "tag": "programming"}),
            ([0.5, 0.6, 0.7, 0.8], {"content": "机器学习", "tag": "ai"}),
            ([0.9, 0.1, 0.2, 0.3], {"content": "数据库", "tag": "database"}),
        ]
        
        for i, (embedding, metadata) in enumerate(test_data, 1):
            if not bridge.insert_vector("test_vectors", embedding, metadata, i):
                logger.error(f"❌ 插入数据 {i} 失败")
                return False
        
        # 测试向量搜索
        query = [0.2, 0.3, 0.4, 0.5]
        results = bridge.vector_search("test_vectors", query, k=2)
        logger.info(f"✅ 搜索结果: {results}")
        
        # 断开连接
        bridge.disconnect()
        
        logger.info("🎉 测试完成！")
        return True
        
    except Exception as e:
        logger.error(f"❌ 测试失败: {e}")
        traceback.print_exc()
        return False

if __name__ == "__main__":
    _test() 
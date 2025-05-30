#!/usr/bin/env python3
import os
import sys
import sqlite3
import sqlite_vec

# 添加当前目录到 Python 路径
script_dir = os.path.dirname(os.path.realpath(__file__))
if script_dir not in sys.path:
    sys.path.insert(0, script_dir)

from simtag.tag_vectors import TagVectorEngine

# 创建引擎实例并检查配置
engine = TagVectorEngine()
print(f"SQLite-vec available: {engine.use_sqlite_vec}")
print(f"Database path: {engine.sqlite_db_path}")

# 直接连接数据库并检查内容
db_paths = [
    "tag_vectors.db",  # 当前目录
    os.path.expanduser("~/.emacs.d/org-supertag/tag_vectors.db"),  # Emacs 目录
]

for db_path in db_paths:
    if os.path.exists(db_path):
        print(f"\nChecking database: {db_path}")
        print(f"File size: {os.path.getsize(db_path)} bytes")
        
        try:
            conn = sqlite3.connect(db_path)
            conn.enable_load_extension(True)
            sqlite_vec.load(conn)
            
            # 检查表结构
            cursor = conn.execute("SELECT sql FROM sqlite_master WHERE type='table' AND name='tag_vectors'")
            schema = cursor.fetchone()
            if schema:
                print(f"Schema: {schema[0]}")
            
            # 获取第一个向量的维度
            cursor = conn.execute("SELECT embedding FROM tag_vectors LIMIT 1")
            vector = cursor.fetchone()
            if vector:
                print(f"First vector dimension: {len(vector[0])}")
            
            conn.close()
        except Exception as e:
            print(f"Error checking {db_path}: {e}") 
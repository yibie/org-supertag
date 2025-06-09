#!/usr/bin/env python3
"""
向量维度修复工具
用于更新数据库表结构以支持新的向量维度
"""

import sqlite3
import json
import os
import shutil
from pathlib import Path
import sqlite_vec

def get_database_path():
    """获取数据库路径"""
    # 从环境变量或配置获取路径
    storage_dir = os.path.expanduser("~/.emacs.d/org-supertag")
    return os.path.join(storage_dir, "supertag_vector.db")

def backup_database(db_path):
    """备份数据库"""
    backup_path = f"{db_path}.backup"
    shutil.copy2(db_path, backup_path)
    print(f"✅ 数据库已备份到: {backup_path}")
    return backup_path

def check_current_dimension(db_path):
    """检查当前向量维度"""
    # 启用扩展加载
    conn = sqlite3.connect(db_path)
    conn.enable_load_extension(True)
    cursor = conn.cursor()
    
    try:
        # 加载sqlite-vec扩展
        sqlite_vec.load(conn)
        has_vec = True
        print("SQLite-vec 扩展: ✅ 可用")
    except Exception as e:
        print(f"SQLite-vec 扩展: ❌ 不可用 ({e})")
        has_vec = False
    
    # 检查node_embeddings_vss表结构
    try:
        cursor.execute("PRAGMA table_info(node_embeddings_vss)")
        table_info = cursor.fetchall()
        
        if table_info:
            print("📋 node_embeddings_vss 表结构:")
            for col in table_info:
                print(f"  - {col}")
        else:
            print("❌ node_embeddings_vss 表不存在")
    except Exception as e:
        print(f"❌ 检查表结构失败: {e}")
    
    # 检查现有数据的维度
    try:
        cursor.execute("SELECT embedding FROM node_embeddings_vss LIMIT 1")
        row = cursor.fetchone()
        if row and row[0]:
            if isinstance(row[0], str):
                # JSON格式
                vector = json.loads(row[0])
                print(f"📊 现有向量维度: {len(vector)} (JSON格式)")
            else:
                print(f"📊 现有向量格式: {type(row[0])}")
        else:
            print("📊 表中无数据")
    except Exception as e:
        print(f"📊 检查现有数据失败: {e}")
    
    conn.close()

def recreate_tables_for_1024d(db_path):
    """重新创建表以支持1024维向量"""
    # 启用扩展加载
    conn = sqlite3.connect(db_path)
    conn.enable_load_extension(True)
    cursor = conn.cursor()
    
    try:
        # 加载sqlite-vec扩展
        sqlite_vec.load(conn)
        print("✅ 成功加载 sqlite-vec 扩展")
    except Exception as e:
        print(f"❌ 无法加载 sqlite-vec 扩展: {e}")
        return False
    
    try:
        # 备份现有数据（如果有的话）
        cursor.execute("SELECT COUNT(*) FROM node_embeddings_vss")
        existing_count = cursor.fetchone()[0]
        print(f"📊 现有嵌入数据: {existing_count} 条")
        
        if existing_count > 0:
            print("⚠️  警告：这将删除所有现有的嵌入数据！")
            response = input("确认继续？(y/N): ")
            if response.lower() != 'y':
                print("❌ 操作已取消")
                return False
        
        # 删除旧表
        cursor.execute("DROP TABLE IF EXISTS node_embeddings_vss")
        cursor.execute("DROP TABLE IF EXISTS tag_embeddings_vss")
        print("🗑️  已删除旧的向量表")
        
        # 创建新的1024维表
        cursor.execute("""
        CREATE VIRTUAL TABLE node_embeddings_vss USING vec0(
            embedding FLOAT[1024],
            node_id_ref TEXT HIDDEN
        )
        """)
        
        cursor.execute("""
        CREATE VIRTUAL TABLE tag_embeddings_vss USING vec0(
            embedding FLOAT[1024],
            tag_id_ref TEXT HIDDEN
        )
        """)
        
        conn.commit()
        print("✅ 成功创建新的1024维向量表")
        
        # 验证表结构
        cursor.execute("PRAGMA table_info(node_embeddings_vss)")
        print("📋 新表结构确认:")
        for col in cursor.fetchall():
            print(f"  - {col}")
            
        return True
        
    except Exception as e:
        print(f"❌ 重创建表失败: {e}")
        conn.rollback()
        return False
    finally:
        conn.close()

def main():
    """主函数"""
    print("=" * 60)
    print("🔧 org-supertag 向量维度修复工具")
    print("=" * 60)
    
    db_path = get_database_path()
    print(f"📍 数据库路径: {db_path}")
    
    if not os.path.exists(db_path):
        print("❌ 数据库文件不存在")
        return
    
    print("\n🔍 检查当前状态...")
    check_current_dimension(db_path)
    
    print(f"\n{'='*40}")
    print("💡 解决方案选项:")
    print("  1. 重新创建为1024维表 (删除现有数据)")
    print("  2. 仅查看状态，不修改")
    print("  3. 退出")
    
    choice = input("\n请选择 (1/2/3): ").strip()
    
    if choice == "1":
        print(f"\n{'='*40}")
        print("🔧 开始修复...")
        
        # 备份数据库
        backup_path = backup_database(db_path)
        
        # 重新创建表
        if recreate_tables_for_1024d(db_path):
            print("✅ 修复完成！")
            print("📝 现在需要重新生成所有嵌入向量")
            print("💡 建议：运行 org-supertag 的重新索引功能")
        else:
            print("❌ 修复失败，可以从备份恢复")
            
    elif choice == "2":
        print("👀 仅查看模式，未进行修改")
        
    else:
        print("👋 退出")

if __name__ == "__main__":
    main() 
#!/usr/bin/env python3
"""
Vector Dimension Management Tool
Automatically detects and resolves vector dimension mismatch issues
"""

import sqlite3
import sqlite_vec
import os
import asyncio
import shutil
from typing import Dict, Any, Optional, List, Tuple
from simtag.config import Config
from simtag.services.embedding_service import get_embedding_service

class DimensionManager:
    """Vector Dimension Manager"""
    
    def __init__(self):
        self.config = Config()
        self.db_path = self.config.vector_db_path
        self.embedding_service = get_embedding_service()
    
    def get_database_info(self) -> Dict[str, Any]:
        """Get database information"""
        if not os.path.exists(self.db_path):
            return {"exists": False, "error": "Database file does not exist"}
        
        try:
            # Enable extension loading
            conn = sqlite3.connect(self.db_path)
            conn.enable_load_extension(True)
            sqlite_vec.load(conn)
            cursor = conn.cursor()
            
            info = {"exists": True, "tables": {}}
            
            # Check vector tables
            for table_name in ["node_embeddings_vss", "tag_embeddings_vss"]:
                try:
                    # Get table structure information
                    cursor.execute(f"PRAGMA table_info({table_name})")
                    table_info = cursor.fetchall()
                    
                    if table_info:
                        # Get data count
                        cursor.execute(f"SELECT COUNT(*) FROM {table_name}")
                        count = cursor.fetchone()[0]
                        
                        # Try to get a sample vector to detect dimension
                        dimension = None
                        if count > 0:
                            cursor.execute(f"SELECT embedding FROM {table_name} LIMIT 1")
                            row = cursor.fetchone()
                            if row and row[0]:
                                if isinstance(row[0], (bytes, str)):
                                    try:
                                        import json
                                        import numpy as np
                                        if isinstance(row[0], bytes):
                                            # sqlite-vec C extension returns bytes
                                            vector = np.frombuffer(row[0], dtype=np.float32)
                                        else:
                                            # JSON string format
                                            vector = json.loads(row[0])
                                        dimension = len(vector)
                                    except:
                                        dimension = "unknown format"
                        
                        info["tables"][table_name] = {
                            "exists": True,
                            "count": count,
                            "dimension": dimension,
                            "structure": table_info
                        }
                    else:
                        info["tables"][table_name] = {"exists": False}
                        
                except Exception as e:
                    info["tables"][table_name] = {"exists": False, "error": str(e)}
            
            conn.close()
            return info
            
        except Exception as e:
            return {"exists": True, "error": f"Cannot access database: {e}"}
    
    async def detect_model_dimension(self) -> Optional[int]:
        """Detect current embedding model dimension"""
        try:
            print("🔍 Detecting current embedding model dimension...")
            test_text = "Test text"
            result = await self.embedding_service.get_embedding(test_text)
            
            if result.success and result.embedding:
                dimension = len(result.embedding)
                print(f"✅ Current model dimension: {dimension}")
                return dimension
            else:
                print(f"❌ Cannot get embedding vector: {result.error_message}")
                return None
                
        except Exception as e:
            print(f"❌ Failed to detect model dimension: {e}")
            return None
    
    def check_dimension_compatibility(self) -> Dict[str, Any]:
        """Check dimension compatibility"""
        print("🔍 Checking vector dimension compatibility...")
        
        db_info = self.get_database_info()
        
        if not db_info.get("exists"):
            return {
                "compatible": False,
                "issue": "database_missing",
                "message": "Database file does not exist",
                "action": "create_database"
            }
        
        if "error" in db_info:
            return {
                "compatible": False,
                "issue": "database_error",
                "message": db_info["error"],
                "action": "fix_database"
            }
        
        # Check dimensions of each table
        dimensions = {}
        for table_name, table_info in db_info.get("tables", {}).items():
            if table_info.get("exists") and table_info.get("dimension"):
                dimensions[table_name] = table_info["dimension"]
        
        if not dimensions:
            return {
                "compatible": True,
                "issue": "no_data",
                "message": "Database is empty, can be used directly",
                "action": "none"
            }
        
        # Check if dimensions are consistent
        unique_dimensions = set(d for d in dimensions.values() if isinstance(d, int))
        
        if len(unique_dimensions) > 1:
            return {
                "compatible": False,
                "issue": "inconsistent_dimensions",
                "message": f"Table dimensions are inconsistent: {dimensions}",
                "action": "recreate_tables",
                "dimensions": dimensions
            }
        
        return {
            "compatible": True,
            "current_dimension": list(unique_dimensions)[0] if unique_dimensions else None,
            "tables": dimensions,
            "message": "Dimension check passed"
        }
    
    async def auto_fix_dimension_mismatch(self, target_dimension: Optional[int] = None) -> bool:
        """Automatically fix dimension mismatch"""
        print("🔧 Starting automatic dimension mismatch fix...")
        
        # If target dimension is not specified, detect current model dimension
        if target_dimension is None:
            target_dimension = await self.detect_model_dimension()
            if target_dimension is None:
                print("❌ Cannot detect model dimension, fix failed")
                return False
        
        # Check compatibility
        compatibility = self.check_dimension_compatibility()
        
        if compatibility["compatible"]:
            current_dim = compatibility.get("current_dimension")
            if current_dim and current_dim != target_dimension:
                print(f"⚠️ Database dimension({current_dim}) does not match model dimension({target_dimension})")
            else:
                print("✅ Dimensions are already compatible, no fix needed")
                return True
        
        # Execute fix
        return await self._recreate_tables_with_dimension(target_dimension)
    
    async def _recreate_tables_with_dimension(self, dimension: int) -> bool:
        """Recreate tables to support specified dimension"""
        try:
            print(f"🔧 Recreating tables to support {dimension}-dimensional vectors...")
            
            # Backup database
            backup_path = f"{self.db_path}.backup_{int(asyncio.get_event_loop().time())}"
            shutil.copy2(self.db_path, backup_path)
            print(f"✅ Database backed up to: {backup_path}")
            
            # Connect to database
            conn = sqlite3.connect(self.db_path)
            conn.enable_load_extension(True)
            sqlite_vec.load(conn)
            cursor = conn.cursor()
            
            # Check existing data
            existing_data = {}
            for table_name in ["node_embeddings_vss", "tag_embeddings_vss"]:
                try:
                    cursor.execute(f"SELECT COUNT(*) FROM {table_name}")
                    count = cursor.fetchone()[0]
                    existing_data[table_name] = count
                except:
                    existing_data[table_name] = 0
            
            total_existing = sum(existing_data.values())
            if total_existing > 0:
                print(f"⚠️ Warning: Will delete {total_existing} existing vector data entries")
                response = input("Confirm to continue? (y/N): ")
                if response.lower() != 'y':
                    print("❌ Operation cancelled by user")
                    return False
            
            # Delete old tables
            cursor.execute("DROP TABLE IF EXISTS node_embeddings_vss")
            cursor.execute("DROP TABLE IF EXISTS tag_embeddings_vss")
            print("🗑️ Old vector tables deleted")
            
            # Create new tables
            cursor.execute(f"""
            CREATE VIRTUAL TABLE node_embeddings_vss USING vec0(
                embedding FLOAT[{dimension}],
                node_id_ref TEXT HIDDEN
            )
            """)
            
            cursor.execute(f"""
            CREATE VIRTUAL TABLE tag_embeddings_vss USING vec0(
                embedding FLOAT[{dimension}],
                tag_id_ref TEXT HIDDEN
            )
            """)
            
            conn.commit()
            conn.close()
            
            print(f"✅ Successfully created {dimension}-dimensional vector tables")
            return True
            
        except Exception as e:
            print(f"❌ Failed to recreate tables: {e}")
            return False
    
    def get_recovery_suggestions(self, compatibility_result: Dict[str, Any]) -> List[str]:
        """Get recovery suggestions"""
        suggestions = []
        
        issue = compatibility_result.get("issue")
        
        if issue == "database_missing":
            suggestions.extend([
                "🔧 Create new vector database",
                "📝 Run org-supertag initialization command",
                "🔄 Regenerate all embedding vectors"
            ])
        
        elif issue == "database_error":
            suggestions.extend([
                "🔧 Check sqlite-vec extension installation",
                "📁 Check database file permissions",
                "🔄 Reinstall sqlite-vec package"
                "🔧 检查 sqlite-vec 扩展安装",
                "📁 检查数据库文件权限",
                "🔄 重新安装 sqlite-vec 包"
            ])
        
        elif issue == "inconsistent_dimensions":
            suggestions.extend([
                "🔧 运行自动维度修复工具",
                "🗑️ 清理并重新创建向量表",
                "🔄 重新生成所有嵌入向量",
                "💾 确保使用一致的嵌入模型"
            ])
        
        else:
            suggestions.append("✅ 当前配置正常")
        
        return suggestions

async def main():
    """主函数"""
    print("=" * 60)
    print("🔧 向量维度管理工具")
    print("=" * 60)
    
    manager = DimensionManager()
    
    # 1. 检测当前状态
    print("🔍 检测当前状态...")
    compatibility = manager.check_dimension_compatibility()
    
    print(f"\n📊 兼容性检查结果:")
    print(f"   兼容状态: {'✅ 兼容' if compatibility['compatible'] else '❌ 不兼容'}")
    print(f"   状态信息: {compatibility['message']}")
    
    if not compatibility["compatible"]:
        print(f"   问题类型: {compatibility.get('issue', '未知')}")
    
    # 2. 检测模型维度
    model_dimension = await manager.detect_model_dimension()
    
    if model_dimension:
        current_db_dim = compatibility.get("current_dimension")
        if current_db_dim and current_db_dim != model_dimension:
            print(f"⚠️ 维度不匹配: 数据库({current_db_dim}) vs 模型({model_dimension})")
    
    # 3. 获取建议
    suggestions = manager.get_recovery_suggestions(compatibility)
    print(f"\n💡 建议操作:")
    for suggestion in suggestions:
        print(f"   {suggestion}")
    
    # 4. 交互式修复
    if not compatibility["compatible"] or (model_dimension and compatibility.get("current_dimension") != model_dimension):
        print(f"\n🔧 修复选项:")
        print("   1. 自动修复维度不匹配")
        print("   2. 仅显示详细信息")
        print("   3. 退出")
        
        choice = input("\n请选择 (1-3): ").strip()
        
        if choice == "1":
            success = await manager.auto_fix_dimension_mismatch(model_dimension)
            if success:
                print("✅ 维度修复完成！")
                print("📝 建议：重新生成所有嵌入向量")
                print("💡 运行：M-x org-supertag-sync-all")
            else:
                print("❌ 维度修复失败")
        
        elif choice == "2":
            # 显示详细信息
            db_info = manager.get_database_info()
            print(f"\n📋 详细数据库信息:")
            for table_name, table_info in db_info.get("tables", {}).items():
                print(f"   {table_name}:")
                print(f"     存在: {table_info.get('exists', False)}")
                print(f"     数据量: {table_info.get('count', 0)}")
                print(f"     维度: {table_info.get('dimension', '未知')}")
        
        else:
            print("👋 退出")
    
    else:
        print("✅ 当前配置正常，无需修复")

if __name__ == "__main__":
    asyncio.run(main()) 
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
快速测试多核心功能 - 不依赖外部服务
只测试多核心管理器本身的逻辑
"""

def test_multicore_manager():
    """测试多核心管理器的基本功能"""
    print("🔧 测试多核心管理器...")
    
    try:
        from multicore_manager import MultiCoreManager
        
        # 测试配置
        config = {
            "enabled": True,
            "max_workers": 4,
            "embedding_batch_threshold": 10,
            "memory_threshold_gb": 2
        }
        
        # 初始化
        manager = MultiCoreManager(config)
        print(f"✅ 初始化成功 - workers: {manager.max_workers}")
        
        # 测试判断逻辑
        should_use_small = manager.should_use_multicore(5, "embedding")
        should_use_large = manager.should_use_multicore(20, "embedding")
        
        print(f"📊 小批量(5个): {'使用多核心' if should_use_small else '使用单线程'}")
        print(f"📊 大批量(20个): {'使用多核心' if should_use_large else '使用单线程'}")
        
        # 测试内存监控
        memory_info = manager.get_memory_info()
        print(f"💾 内存使用: {memory_info['used_gb']:.1f}GB / {memory_info['available_gb']:.1f}GB")
        
        # 清理
        manager.cleanup()
        print("✅ 清理完成")
        
        return True
        
    except Exception as e:
        print(f"❌ 错误: {e}")
        return False

def test_batch_processing_logic():
    """测试批量处理逻辑（模拟）"""
    print("\n🎯 测试批量处理逻辑...")
    
    # 模拟不同大小的文本批量
    test_cases = [
        ("小批量", ["text1", "text2", "text3"]),
        ("中批量", [f"text{i}" for i in range(50)]),
        ("大批量", [f"text{i}" for i in range(500)])
    ]
    
    try:
        from multicore_manager import MultiCoreManager
        
        config = {
            "enabled": True,
            "max_workers": 8,
            "embedding_batch_threshold": 20,
            "memory_threshold_gb": 4
        }
        
        manager = MultiCoreManager(config)
        
        for name, texts in test_cases:
            should_use = manager.should_use_multicore(len(texts), "embedding")
            recommended_workers = min(manager.max_workers, len(texts) // 10 + 1)
            
            print(f"📝 {name} ({len(texts)}个文本):")
            print(f"   推荐: {'多核心处理' if should_use else '单线程处理'}")
            if should_use:
                print(f"   建议workers: {recommended_workers}")
        
        manager.cleanup()
        return True
        
    except Exception as e:
        print(f"❌ 错误: {e}")
        return False

def main():
    """主测试函数"""
    print("🚀 快速多核心功能测试")
    print("=" * 40)
    
    # 测试1: 基础功能
    success1 = test_multicore_manager()
    
    # 测试2: 批量处理逻辑
    success2 = test_batch_processing_logic()
    
    print(f"\n📊 测试结果:")
    print(f"   基础功能: {'✅ 通过' if success1 else '❌ 失败'}")
    print(f"   批量逻辑: {'✅ 通过' if success2 else '❌ 失败'}")
    
    if success1 and success2:
        print("\n🎉 多核心管理器功能正常！")
        print("💡 可以集成到实际服务中使用")
    else:
        print("\n⚠️  需要检查配置或依赖")

if __name__ == "__main__":
    main() 
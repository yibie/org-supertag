#!/usr/bin/env python3
"""
NER 速度优化工具
用于诊断和优化命名实体识别的性能
"""

import asyncio
import time
import psutil
import os
from typing import Dict, List, Any
from simtag.config import Config
from simtag.services.entity_extractor import EntityExtractor
from simtag.services.llm_client import LLMClient

class NEROptimizer:
    """NER性能优化器"""
    
    def __init__(self):
        self.config = Config()
        self.llm_client = LLMClient(self.config.llm_client_config)
        
    async def diagnose_performance(self) -> Dict[str, Any]:
        """诊断当前NER性能"""
        print("🔍 诊断 NER 性能...")
        
        # 测试文本
        test_texts = [
            "张三是北京大学的教授，研究人工智能。",
            "苹果公司在加利福尼亚州库比蒂诺市设有总部。",
            "机器学习是人工智能的一个重要分支，包括深度学习和神经网络。"
        ]
        
        # 系统信息
        system_info = {
            "cpu_count": psutil.cpu_count(),
            "memory_gb": round(psutil.virtual_memory().total / (1024**3), 2),
            "current_model": self.config.entity_extractor_config.get("llm_model_override"),
            "max_workers": self.config.multicore_config.get("max_workers"),
            "batch_threshold": self.config.multicore_config.get("ner_batch_threshold")
        }
        
        print(f"💻 系统信息:")
        print(f"   CPU 核心数: {system_info['cpu_count']}")
        print(f"   内存: {system_info['memory_gb']} GB")
        print(f"   当前模型: {system_info['current_model']}")
        
        # 测试当前配置性能
        extractor = EntityExtractor(self.config.entity_extractor_config, self.llm_client)
        
        start_time = time.time()
        try:
            await extractor.extract_entities_from_text(test_texts[0])
            processing_time = time.time() - start_time
            success = True
        except Exception as e:
            processing_time = time.time() - start_time
            success = False
            print(f"❌ 测试失败: {e}")
        
        return {
            "system_info": system_info,
            "test_time": round(processing_time, 2),
            "success": success,
            "tokens_per_second": round(50 / processing_time, 2) if success else 0
        }
    
    def get_optimization_recommendations(self, diagnosis: Dict[str, Any]) -> List[str]:
        """根据诊断结果提供优化建议"""
        recommendations = []
        
        # 性能评估
        if diagnosis["test_time"] > 10:
            recommendations.append("🐌 处理速度较慢，建议优化")
        elif diagnosis["test_time"] > 5:
            recommendations.append("⚡ 处理速度中等，可以优化")
        else:
            recommendations.append("🚀 处理速度良好")
        
        # 模型建议
        current_model = diagnosis["system_info"]["current_model"]
        fast_models = Config.get_fast_ner_models()
        
        if current_model not in fast_models:
            recommendations.append(f"💡 建议切换到更快的模型：{list(fast_models.keys())[:3]}")
        
        # 多核心建议
        cpu_count = diagnosis["system_info"]["cpu_count"]
        max_workers = diagnosis["system_info"]["max_workers"]
        
        if max_workers is None or max_workers > cpu_count // 2:
            recommendations.append(f"⚙️ 建议设置 max_workers 为 {min(4, cpu_count // 2)}")
        
        # 内存建议
        memory_gb = diagnosis["system_info"]["memory_gb"]
        if memory_gb < 8:
            recommendations.append("💾 内存较少，建议使用更小的模型和减少并行度")
        
        return recommendations
    
    async def apply_optimizations(self, optimization_level: str = "balanced"):
        """应用优化配置
        
        Args:
            optimization_level: "fast" | "balanced" | "quality"
        """
        print(f"🔧 应用 {optimization_level} 优化模式...")
        
        if optimization_level == "fast":
            # 极速模式
            optimizations = {
                "llm_model_override": "qwen2.5:0.5b",
                "max_gleaning_rounds": 1,
                "max_entities_per_extraction": 5,
                "max_relations_per_extraction": 3,
                "llm_timeout": 30,
                "max_retries": 1
            }
            multicore_opts = {
                "max_workers": 2,
                "ner_batch_threshold": 2,
                "chunk_size_factor": 2
            }
            
        elif optimization_level == "balanced":
            # 平衡模式
            optimizations = {
                "llm_model_override": "qwen2.5:0.5b",
                "max_gleaning_rounds": 1,
                "max_entities_per_extraction": 8,
                "max_relations_per_extraction": 5,
                "llm_timeout": 60,
                "max_retries": 1
            }
            multicore_opts = {
                "max_workers": 3,
                "ner_batch_threshold": 3,
                "chunk_size_factor": 4
            }
            
        else:  # quality
            # 质量优先模式
            optimizations = {
                "llm_model_override": "qwen2.5:1.5b",
                "max_gleaning_rounds": 2,
                "max_entities_per_extraction": 12,
                "max_relations_per_extraction": 8,
                "llm_timeout": 120,
                "max_retries": 2
            }
            multicore_opts = {
                "max_workers": 4,
                "ner_batch_threshold": 5,
                "chunk_size_factor": 6
            }
        
        # 应用配置
        self.config.entity_extractor_config.update(optimizations)
        self.config.multicore_config.update(multicore_opts)
        
        print("✅ 优化配置已应用")
        
        # 显示变更
        print(f"📝 模型: {optimizations['llm_model_override']}")
        print(f"📝 精化轮次: {optimizations['max_gleaning_rounds']}")
        print(f"📝 最大实体数: {optimizations['max_entities_per_extraction']}")
        print(f"📝 并行worker: {multicore_opts['max_workers']}")
    
    async def benchmark_models(self, models: List[str]) -> Dict[str, float]:
        """对比测试不同模型的性能"""
        print("🏁 开始模型性能对比测试...")
        
        test_text = "张三是北京大学的教授，专门研究人工智能和机器学习。"
        results = {}
        
        original_model = self.config.entity_extractor_config.get("llm_model_override")
        
        for model in models:
            print(f"🧪 测试模型: {model}")
            
            # 更新模型配置
            self.config.entity_extractor_config["llm_model_override"] = model
            extractor = EntityExtractor(self.config.entity_extractor_config, self.llm_client)
            
            start_time = time.time()
            try:
                await extractor.extract_entities_from_text(test_text)
                elapsed = time.time() - start_time
                results[model] = round(elapsed, 2)
                print(f"   ✅ {elapsed:.2f}秒")
            except Exception as e:
                results[model] = float('inf')
                print(f"   ❌ 失败: {e}")
        
        # 恢复原始模型
        self.config.entity_extractor_config["llm_model_override"] = original_model
        
        return results

async def main():
    """主函数"""
    print("=" * 60)
    print("🚀 NER 速度优化工具")
    print("=" * 60)
    
    optimizer = NEROptimizer()
    
    # 1. 诊断当前性能
    diagnosis = await optimizer.diagnose_performance()
    
    print(f"\n📊 性能诊断结果:")
    print(f"   处理时间: {diagnosis['test_time']}秒")
    print(f"   成功率: {'✅' if diagnosis['success'] else '❌'}")
    if diagnosis['success']:
        print(f"   估算速度: {diagnosis['tokens_per_second']} tokens/秒")
    
    # 2. 获取建议
    recommendations = optimizer.get_optimization_recommendations(diagnosis)
    print(f"\n💡 优化建议:")
    for rec in recommendations:
        print(f"   {rec}")
    
    # 3. 交互式优化
    print(f"\n🔧 优化选项:")
    print("   1. 快速模式 (最快速度，基础质量)")
    print("   2. 平衡模式 (平衡速度和质量)")
    print("   3. 质量模式 (最佳质量，较慢速度)")
    print("   4. 模型对比测试")
    print("   5. 退出")
    
    choice = input("\n请选择 (1-5): ").strip()
    
    if choice == "1":
        await optimizer.apply_optimizations("fast")
    elif choice == "2":
        await optimizer.apply_optimizations("balanced")
    elif choice == "3":
        await optimizer.apply_optimizations("quality")
    elif choice == "4":
        fast_models = list(Config.get_fast_ner_models().keys())[:3]
        results = await optimizer.benchmark_models(fast_models)
        print(f"\n🏆 模型性能排名:")
        sorted_results = sorted(results.items(), key=lambda x: x[1])
        for i, (model, time_taken) in enumerate(sorted_results, 1):
            if time_taken == float('inf'):
                print(f"   {i}. {model}: 失败")
            else:
                print(f"   {i}. {model}: {time_taken}秒")
    else:
        print("👋 退出")
        return
    
    # 4. 重新测试
    if choice in ["1", "2", "3"]:
        print(f"\n🔄 重新测试优化后性能...")
        new_diagnosis = await optimizer.diagnose_performance()
        improvement = diagnosis['test_time'] - new_diagnosis['test_time']
        print(f"📈 性能提升: {improvement:.2f}秒 ({improvement/diagnosis['test_time']*100:.1f}%)")

if __name__ == "__main__":
    asyncio.run(main()) 
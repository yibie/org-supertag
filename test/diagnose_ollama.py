#!/usr/bin/env python3
"""
Ollama服务诊断工具
用于检查Ollama服务状态、模型可用性和性能
"""

import asyncio
import requests
import time
import json
import logging
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
import httpx

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class ModelInfo:
    name: str
    size: int
    digest: str
    modified_at: str

@dataclass
class DiagnosticResult:
    service_available: bool
    models: List[ModelInfo]
    model_test_results: Dict[str, Any]
    performance_metrics: Dict[str, float]
    recommendations: List[str]

class OllamaDiagnostic:
    """Ollama诊断器"""
    
    def __init__(self, base_url: str = "http://localhost:11434"):
        self.base_url = base_url
        
    def check_service_availability(self) -> bool:
        """检查Ollama服务是否可用"""
        try:
            response = requests.get(f"{self.base_url}/api/tags", timeout=10)
            response.raise_for_status()
            logger.info("✅ Ollama service is available")
            return True
        except Exception as e:
            logger.error(f"❌ Ollama service unavailable: {e}")
            return False
    
    def get_models(self) -> List[ModelInfo]:
        """获取可用模型列表"""
        try:
            response = requests.get(f"{self.base_url}/api/tags", timeout=10)
            response.raise_for_status()
            data = response.json()
            
            models = []
            for model in data.get("models", []):
                models.append(ModelInfo(
                    name=model.get("name", ""),
                    size=model.get("size", 0),
                    digest=model.get("digest", ""),
                    modified_at=model.get("modified_at", "")
                ))
            
            logger.info(f"📦 Found {len(models)} models")
            for model in models:
                size_mb = model.size / (1024 * 1024)
                logger.info(f"  - {model.name}: {size_mb:.1f}MB")
            
            return models
        except Exception as e:
            logger.error(f"❌ Failed to get models: {e}")
            return []
    
    async def test_model_generation(self, model_name: str, timeout: int = 60) -> Dict[str, Any]:
        """测试模型生成能力"""
        test_prompt = "Hello, how are you today?"
        
        try:
            start_time = time.time()
            
            # 使用requests进行同步调用，在线程中执行
            def make_request():
                payload = {
                    "model": model_name,
                    "prompt": test_prompt,
                    "stream": False,
                    "options": {"temperature": 0.1}
                }
                response = requests.post(
                    f"{self.base_url}/api/generate",
                    json=payload,
                    timeout=timeout
                )
                response.raise_for_status()
                return response.json()
            
            # 在线程中执行同步请求
            response_data = await asyncio.to_thread(make_request)
            
            end_time = time.time()
            response_time = end_time - start_time
            
            response_text = response_data.get("response", "")
            
            result = {
                "success": True,
                "response_time": response_time,
                "response_length": len(response_text),
                "response_preview": response_text[:100] + "..." if len(response_text) > 100 else response_text,
                "tokens_per_second": len(response_text.split()) / response_time if response_time > 0 else 0
            }
            
            logger.info(f"✅ {model_name} test successful: {response_time:.2f}s, {result['tokens_per_second']:.1f} tokens/s")
            return result
            
        except asyncio.TimeoutError:
            logger.error(f"⏰ {model_name} test timeout ({timeout}s)")
            return {"success": False, "error": "timeout"}
        except Exception as e:
            logger.error(f"❌ {model_name} test failed: {e}")
            return {"success": False, "error": str(e)}
    
    async def test_embedding_model(self, model_name: str, timeout: int = 60) -> Dict[str, Any]:
        """测试嵌入模型"""
        test_text = "This is a test sentence for embedding generation."
        
        try:
            start_time = time.time()
            
            def make_request():
                payload = {
                    "model": model_name,
                    "prompt": test_text
                }
                response = requests.post(
                    f"{self.base_url}/api/embeddings",
                    json=payload,
                    timeout=timeout
                )
                response.raise_for_status()
                return response.json()
            
            response_data = await asyncio.to_thread(make_request)
            
            end_time = time.time()
            response_time = end_time - start_time
            
            embedding = response_data.get("embedding", [])
            
            result = {
                "success": True,
                "response_time": response_time,
                "embedding_dimension": len(embedding),
                "sample_values": embedding[:5] if len(embedding) >= 5 else embedding
            }
            
            logger.info(f"✅ {model_name} embedding test successful: {response_time:.2f}s, dim={len(embedding)}")
            return result
            
        except Exception as e:
            logger.error(f"❌ {model_name} embedding test failed: {e}")
            return {"success": False, "error": str(e)}
    
    def get_system_info(self) -> Dict[str, Any]:
        """获取系统信息"""
        try:
            # 尝试获取Ollama版本信息
            response = requests.get(f"{self.base_url}/api/version", timeout=10)
            if response.status_code == 200:
                version_info = response.json()
            else:
                version_info = {"version": "unknown"}
            
            # 检查资源使用情况（基本信息）
            import psutil
            cpu_percent = psutil.cpu_percent(interval=1)
            memory = psutil.virtual_memory()
            disk = psutil.disk_usage('/')
            
            return {
                "ollama_version": version_info.get("version", "unknown"),
                "cpu_usage": cpu_percent,
                "memory_usage": memory.percent,
                "memory_available": memory.available / (1024**3),  # GB
                "disk_usage": disk.percent,
                "disk_free": disk.free / (1024**3)  # GB
            }
        except Exception as e:
            logger.warning(f"Failed to get system info: {e}")
            return {}
    
    async def run_full_diagnostic(self) -> DiagnosticResult:
        """运行完整诊断"""
        logger.info("🔍 Starting Ollama diagnostic...")
        
        # 1. 检查服务可用性
        service_available = self.check_service_availability()
        
        if not service_available:
            return DiagnosticResult(
                service_available=False,
                models=[],
                model_test_results={},
                performance_metrics={},
                recommendations=["Ollama service is not running. Please start it with 'ollama serve'"]
            )
        
        # 2. 获取模型列表
        models = self.get_models()
        
        # 3. 测试模型
        model_test_results = {}
        
        # 测试生成模型
        generation_models = [m.name for m in models if not any(embed_name in m.name.lower() 
                           for embed_name in ['embed', 'embedding', 'nomic-embed'])]
        
        if generation_models:
            test_model = generation_models[0]  # 测试第一个生成模型
            logger.info(f"🧪 Testing generation model: {test_model}")
            model_test_results[test_model] = await self.test_model_generation(test_model)
        
        # 测试嵌入模型
        embedding_models = [m.name for m in models if any(embed_name in m.name.lower() 
                          for embed_name in ['embed', 'embedding', 'nomic-embed'])]
        
        if embedding_models:
            test_embed_model = embedding_models[0]  # 测试第一个嵌入模型
            logger.info(f"🧪 Testing embedding model: {test_embed_model}")
            model_test_results[f"{test_embed_model}_embedding"] = await self.test_embedding_model(test_embed_model)
        
        # 4. 性能指标
        performance_metrics = {}
        system_info = self.get_system_info()
        
        if system_info:
            performance_metrics.update(system_info)
        
        # 5. 生成建议
        recommendations = self._generate_recommendations(models, model_test_results, system_info)
        
        return DiagnosticResult(
            service_available=service_available,
            models=models,
            model_test_results=model_test_results,
            performance_metrics=performance_metrics,
            recommendations=recommendations
        )
    
    def _generate_recommendations(self, models: List[ModelInfo], 
                                test_results: Dict[str, Any], 
                                system_info: Dict[str, Any]) -> List[str]:
        """生成建议"""
        recommendations = []
        
        # 检查模型数量
        if not models:
            recommendations.append("No models found. Install models with 'ollama pull <model_name>'")
        
        # 检查嵌入模型
        embedding_models = [m for m in models if 'embed' in m.name.lower()]
        if not embedding_models:
            recommendations.append("No embedding models found. Consider installing 'ollama pull nomic-embed-text'")
        
        # 检查小模型用于实体提取
        small_models = [m for m in models if any(size in m.name.lower() 
                       for size in ['1b', '2b', '0.5b', 'mini'])]
        if not small_models:
            recommendations.append("Consider installing smaller models for faster entity extraction: 'ollama pull qwen2.5:1.5b'")
        
        # 检查性能
        for model_name, result in test_results.items():
            if result.get("success"):
                response_time = result.get("response_time", 0)
                if response_time > 30:
                    recommendations.append(f"Model {model_name} is slow ({response_time:.1f}s). Consider using a smaller model.")
        
        # 检查系统资源
        if system_info:
            memory_usage = system_info.get("memory_usage", 0)
            cpu_usage = system_info.get("cpu_usage", 0)
            
            if memory_usage > 90:
                recommendations.append("High memory usage detected. Consider closing other applications.")
            
            if cpu_usage > 90:
                recommendations.append("High CPU usage detected. System may be under load.")
        
        if not recommendations:
            recommendations.append("System appears to be functioning well!")
        
        return recommendations

def print_diagnostic_report(result: DiagnosticResult):
    """打印诊断报告"""
    print("\n" + "="*60)
    print("🔍 OLLAMA DIAGNOSTIC REPORT")
    print("="*60)
    
    # 服务状态
    status_emoji = "✅" if result.service_available else "❌"
    print(f"{status_emoji} Service Status: {'Available' if result.service_available else 'Unavailable'}")
    
    if not result.service_available:
        print("\n❌ Cannot proceed with further diagnostics.")
        print("\n💡 RECOMMENDATIONS:")
        for rec in result.recommendations:
            print(f"  • {rec}")
        print("="*60)
        return
    
    # 模型信息
    print(f"\n📦 MODELS ({len(result.models)} total):")
    if result.models:
        for model in result.models:
            size_gb = model.size / (1024**3)
            print(f"  • {model.name}: {size_gb:.2f}GB")
    else:
        print("  No models found")
    
    # 测试结果
    print(f"\n🧪 MODEL TESTS:")
    if result.model_test_results:
        for model_name, test_result in result.model_test_results.items():
            if test_result.get("success"):
                response_time = test_result.get("response_time", 0)
                tokens_per_sec = test_result.get("tokens_per_second", 0)
                if "embedding" in model_name:
                    dim = test_result.get("embedding_dimension", 0)
                    print(f"  ✅ {model_name}: {response_time:.2f}s (dim={dim})")
                else:
                    print(f"  ✅ {model_name}: {response_time:.2f}s ({tokens_per_sec:.1f} tokens/s)")
            else:
                error = test_result.get("error", "unknown")
                print(f"  ❌ {model_name}: {error}")
    else:
        print("  No tests performed")
    
    # 性能指标
    if result.performance_metrics:
        print(f"\n📊 PERFORMANCE:")
        metrics = result.performance_metrics
        if "cpu_usage" in metrics:
            print(f"  CPU Usage: {metrics['cpu_usage']:.1f}%")
        if "memory_usage" in metrics:
            print(f"  Memory Usage: {metrics['memory_usage']:.1f}%")
        if "memory_available" in metrics:
            print(f"  Memory Available: {metrics['memory_available']:.1f}GB")
        if "ollama_version" in metrics:
            print(f"  Ollama Version: {metrics['ollama_version']}")
    
    # 建议
    print(f"\n💡 RECOMMENDATIONS:")
    for rec in result.recommendations:
        print(f"  • {rec}")
    
    print("="*60)

async def main():
    client = httpx.AsyncClient(timeout=120)
    api_url = "http://localhost:11434/api/chat"
    model_name = "hf.co/unsloth/gemma-3-4b-it-GGUF"  # <--- THE MODEL IN QUESTION
    messages = [{"role": "user", "content": "Why is the sky blue?"}]

    payload = {
        "model": model_name,
        "messages": messages,
        "stream": False,
        "options": {
            "temperature": 0.5
        }
    }

    print(f"Sending request to {api_url} with model {model_name} and options...")
    try:
        response = await client.post(api_url, json=payload, headers={"Content-Type": "application/json"})
        response.raise_for_status()
        print("Success! Response:")
        print(response.json())
    except httpx.HTTPStatusError as e:
        print(f"HTTP Error! Status: {e.response.status_code}")
        print("Response body:")
        print(e.response.text)
    except Exception as e:
        print(f"An unexpected error occurred: {e}")
    finally:
        await client.aclose()

if __name__ == "__main__":
    asyncio.run(main()) 
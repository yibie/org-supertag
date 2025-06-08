"""
真正的多核心处理管理器 - 使用多进程实现真正的并行
专为M4 Max等多核心CPU优化
"""
import os
import sys
import psutil
import logging
import multiprocessing as mp
import concurrent.futures
import time
import json
import pickle
from typing import List, Dict, Any, Callable, Optional, Union, Tuple
from dataclasses import dataclass
from functools import partial
import asyncio
import threading
import re

logger = logging.getLogger(__name__)

# 导入日志监控
try:
    from .services.log_monitor import get_log_monitor
except ImportError:
    # Fallback 如果导入失败
    def get_log_monitor():
        return None

@dataclass
class ProcessingTask:
    """处理任务"""
    task_id: str
    task_type: str  # "embedding", "ner", "llm_call"
    data: Any
    config: Dict[str, Any] = None
    
@dataclass 
class ProcessingResult:
    """处理结果"""
    task_id: str
    success: bool
    result: Any = None
    error: str = None
    processing_time: float = 0
    worker_id: int = 0

# ============================================================================
# 多进程工作函数 - 在独立进程中运行
# ============================================================================

def worker_init(worker_id: int):
    """工作进程初始化"""
    # 设置进程级别的日志
    logging.basicConfig(
        level=logging.INFO,
        format=f'%(asctime)s - WORKER_{worker_id} - %(levelname)s - %(message)s'
    )
    logger = logging.getLogger(f"worker_{worker_id}")
    
    # 设置环境变量避免tokenizer警告
    os.environ["TOKENIZERS_PARALLELISM"] = "false"
    os.environ["CUDA_VISIBLE_DEVICES"] = ""  # 强制使用CPU
    
    logger.info(f"🚀 Worker {worker_id} 初始化完成 (PID: {os.getpid()})")

def process_embedding_task(task: ProcessingTask, worker_id: int) -> ProcessingResult:
    """处理嵌入任务"""
    start_time = time.time()
    
    try:
        # 设置worker级别日志
        import logging
        logger = logging.getLogger(f"worker_{worker_id}")
        
        # 获取嵌入配置
        embedding_backend = task.config.get("backend", "local")
        
        logger.info(f"🔧 Worker {worker_id} 开始处理嵌入任务: {task.task_id} (backend: {embedding_backend})")
        
        if embedding_backend == "local":
            # 本地嵌入模型
            from sentence_transformers import SentenceTransformer
            import numpy as np
            
            # 从配置中获取本地模型名称
            model_name = task.config.get("local_model", "sentence-transformers/all-MiniLM-L6-v2")
            
            logger.debug(f"📦 Worker {worker_id} 正在加载嵌入模型: {model_name}")
            
            # 在每个进程中加载自己的模型实例
            model = SentenceTransformer(model_name)
            
            # 处理文本（可能是单个文本或文本列表）
            texts = task.data if isinstance(task.data, list) else [task.data]
            text_count = len(texts) if isinstance(texts, list) else 1
            
            logger.debug(f"🔢 Worker {worker_id} 正在生成 {text_count} 个文本的嵌入向量...")
            
            # 生成嵌入
            embeddings = model.encode(texts, show_progress_bar=False)
            
            # 转换为列表格式
            if isinstance(task.data, list):
                result = [emb.tolist() for emb in embeddings]
            else:
                result = embeddings[0].tolist()
                
        elif embedding_backend == "ollama":
            # Ollama嵌入模型
            import requests
            import json
            
            # 从配置中获取Ollama设置
            base_url = task.config.get("base_url", "http://localhost:11434")
            model_name = task.config.get("ollama_model", "nomic-embed-text")
            timeout = task.config.get("timeout", 300)
            
            # 处理文本
            texts = task.data if isinstance(task.data, list) else [task.data]
            
            # 构建请求
            api_url = f"{base_url}/api/embeddings"
            
            results = []
            for text in texts:
                payload = {
                    "model": model_name,
                    "prompt": text
                }
                
                response = requests.post(
                    api_url,
                    json=payload,
                    headers={"Content-Type": "application/json"},
                    timeout=timeout
                )
                response.raise_for_status()
                
                # 解析响应
                embedding_data = response.json().get("embedding", [])
                results.append(embedding_data)
            
            # 返回结果格式
            if isinstance(task.data, list):
                result = results
            else:
                result = results[0] if results else []
                
        else:
            raise ValueError(f"Unsupported embedding backend: {embedding_backend}")
            
        processing_time = time.time() - start_time
        logger.info(f"✅ Worker {worker_id} 嵌入任务完成: {task.task_id} ({processing_time:.3f}s)")
        
        return ProcessingResult(
            task_id=task.task_id,
            success=True,
            result=result,
            processing_time=processing_time,
            worker_id=worker_id
        )
        
    except Exception as e:
        processing_time = time.time() - start_time
        logger.error(f"❌ Worker {worker_id} 嵌入任务失败: {task.task_id} - {str(e)} ({processing_time:.3f}s)")
        return ProcessingResult(
            task_id=task.task_id,
            success=False,
            error=str(e),
            processing_time=processing_time,
            worker_id=worker_id
        )

def process_llm_task(task: ProcessingTask, worker_id: int) -> ProcessingResult:
    """处理LLM任务"""
    start_time = time.time()
    
    try:
        import requests
        import json
        
        # 获取LLM配置
        base_url = task.config.get("base_url", "http://localhost:11434")
        model = task.config.get("model", "qwen2.5:1.5b")
        timeout = task.config.get("timeout", 120)
        
        # 构建请求
        api_url = f"{base_url}/api/generate"
        payload = {
            "model": model,
            "prompt": task.data,
            "stream": False,
            "options": {
                "temperature": 0.2,
                "num_predict": 2048
            }
        }
        
        # 发送请求
        response = requests.post(
            api_url,
            json=payload,
            headers={"Content-Type": "application/json"},
            timeout=timeout
        )
        response.raise_for_status()
        
        # 解析响应
        result = response.json().get("response", "")
        
        return ProcessingResult(
            task_id=task.task_id,
            success=True,
            result=result,
            processing_time=time.time() - start_time,
            worker_id=worker_id
        )
        
    except Exception as e:
        return ProcessingResult(
            task_id=task.task_id,
            success=False,
            error=str(e),
            processing_time=time.time() - start_time,
            worker_id=worker_id
        )

def process_ner_task(task: ProcessingTask, worker_id: int) -> ProcessingResult:
    """处理NER任务"""
    start_time = time.time()
    
    try:
        import requests
        import json
        import logging
        import re
        
        # 设置worker级别日志
        logger = logging.getLogger(f"worker_{worker_id}")
        
        # 获取配置
        base_url = task.config.get("base_url", "http://localhost:11434")
        model = task.config.get("model", "qwen3:1.7b")
        timeout = task.config.get("timeout", 120)
        
        # 解析任务数据
        content = task.data["content"]
        existing_tags = task.data.get("existing_tags", [])
        
        logger.info(f"🔧 Worker {worker_id} 开始处理NER任务: {task.task_id} (model: {model}, content: {len(content)} chars)")
        
        # 构建更简单的提示词，减少对JSON格式的依赖
        existing_tags_str = ", ".join(f"'{t}'" for t in existing_tags) if existing_tags else "None"
        
        prompt = f"""分析以下文本，提取适合作为标签的关键概念、主题、工具或名称。

文本内容：
---
{content[:4000]}
---

已有标签：[{existing_tags_str}]

请建议新的标签，不要重复已有标签。为每个标签提供：
- 标签名称
- 置信度(0.0-1.0) 
- 原因

你可以使用以下两种格式之一回复：

格式1（JSON数组）：
[
  {{"tag_name": "Python", "confidence": 0.9, "reasoning": "文本明确提到Python编程语言"}},
  {{"tag_name": "编程", "confidence": 0.8, "reasoning": "内容涉及编程相关概念"}}
]

格式2（简单文本）：
标签: Python | 置信度: 0.9 | 原因: 文本明确提到Python编程语言
标签: 编程 | 置信度: 0.8 | 原因: 内容涉及编程相关概念

请开始分析：
"""
        
        # 发送请求
        api_url = f"{base_url}/api/generate"
        payload = {
            "model": model,
            "prompt": prompt,
            "stream": False,
            "options": {
                "temperature": 0.3,
                "num_predict": 1024
            }
        }
        
        response = requests.post(
            api_url,
            json=payload,
            headers={"Content-Type": "application/json"},
            timeout=timeout
        )
        response.raise_for_status()
        
        # 解析响应
        response_text = response.json().get("response", "")
        
        # 先尝试JSON解析
        result = []
        json_parsed = False
        
        # 清理响应
        cleaned_response = response_text.strip()
        if cleaned_response.startswith('```json'):
            cleaned_response = cleaned_response[7:]
        elif cleaned_response.startswith('```'):
            cleaned_response = cleaned_response[3:]
        if cleaned_response.endswith('```'):
            cleaned_response = cleaned_response[:-3]
        cleaned_response = cleaned_response.strip()
        
        # 尝试JSON解析
        try:
            # 查找JSON数组
            json_match = re.search(r'\[.*?\]', cleaned_response, re.DOTALL)
            if json_match:
                json_str = json_match.group(0)
                result = json.loads(json_str)
                if isinstance(result, list):
                    json_parsed = True
                    logger.debug(f"✅ Worker {worker_id} JSON解析成功: {len(result)} tags")
                else:
                    result = []
            else:
                # 尝试解析整个响应
                result = json.loads(cleaned_response)
                if isinstance(result, list):
                    json_parsed = True
                    logger.debug(f"✅ Worker {worker_id} 完整JSON解析成功: {len(result)} tags")
                else:
                    result = []
        except json.JSONDecodeError:
            result = []
        
        # 如果JSON解析失败，使用文本解析
        if not json_parsed:
            logger.debug(f"🔄 Worker {worker_id} JSON解析失败，尝试文本解析")
            result = []
            
            # 解析简单文本格式
            lines = response_text.split('\n')
            for line in lines:
                line = line.strip()
                if not line:
                    continue
                    
                # 匹配格式：标签: XXX | 置信度: X.X | 原因: XXX
                pattern = r'标签[:：]\s*([^|]+)\s*[|｜]\s*置信度[:：]\s*([0-9.]+)\s*[|｜]\s*原因[:：]\s*(.+)'
                match = re.search(pattern, line)
                if match:
                    tag_name = match.group(1).strip()
                    try:
                        confidence = float(match.group(2).strip())
                    except ValueError:
                        confidence = 0.5
                    reasoning = match.group(3).strip()
                    
                    if tag_name and tag_name not in existing_tags:
                        result.append({
                            "tag_name": tag_name,
                            "confidence": confidence,
                            "reasoning": reasoning
                        })
                        continue
                
                # 尝试更宽松的解析：寻找可能的标签
                # 如：Python (0.9): 编程语言
                pattern2 = r'([^\s(]+)\s*\(([0-9.]+)\)\s*[:：]\s*(.+)'
                match2 = re.search(pattern2, line)
                if match2:
                    tag_name = match2.group(1).strip()
                    try:
                        confidence = float(match2.group(2).strip())
                    except ValueError:
                        confidence = 0.5
                    reasoning = match2.group(3).strip()
                    
                    if tag_name and tag_name not in existing_tags:
                        result.append({
                            "tag_name": tag_name,
                            "confidence": confidence,
                            "reasoning": reasoning
                        })
                        continue
                
                # 最后尝试：直接提取可能的关键词
                if len(result) < 3:  # 只在找到的标签较少时使用
                    # 寻找中文词汇或英文单词
                    words = re.findall(r'[\u4e00-\u9fff]+|[A-Za-z]+', line)
                    for word in words:
                        if (len(word) >= 2 and 
                            word not in existing_tags and 
                            word.lower() not in ['标签', 'tag', 'confidence', '置信度', '原因', 'reasoning']):
                            result.append({
                                "tag_name": word,
                                "confidence": 0.5,
                                "reasoning": f"从文本中提取的关键词: {word}"
                            })
                            break  # 每行最多提取一个
            
            if result:
                logger.debug(f"✅ Worker {worker_id} 文本解析成功: {len(result)} tags")
            else:
                logger.warning(f"⚠️ Worker {worker_id} 文本解析也未找到标签")
        
        # 验证和清理结果
        final_result = []
        for item in result:
            if (isinstance(item, dict) and 
                'tag_name' in item and 
                item['tag_name'] and 
                item['tag_name'] not in existing_tags):
                
                # 确保必要字段存在
                if 'confidence' not in item:
                    item['confidence'] = 0.5
                if 'reasoning' not in item:
                    item['reasoning'] = f"提取的标签: {item['tag_name']}"
                
                final_result.append(item)
        
        processing_time = time.time() - start_time
        tag_count = len(final_result)
        parse_method = "JSON" if json_parsed else "文本"
        logger.info(f"✅ Worker {worker_id} NER任务完成: {task.task_id} -> {tag_count} tags ({processing_time:.3f}s, {parse_method}解析)")
        
        return ProcessingResult(
            task_id=task.task_id,
            success=True,
            result=final_result,
            processing_time=processing_time,
            worker_id=worker_id
        )
        
    except Exception as e:
        processing_time = time.time() - start_time
        logger.error(f"❌ Worker {worker_id} NER任务失败: {task.task_id} - {str(e)} ({processing_time:.3f}s)")
        return ProcessingResult(
            task_id=task.task_id,
            success=False,
            error=str(e),
            processing_time=processing_time,
            worker_id=worker_id
        )

# ============================================================================
# 主要的多核心管理器类
# ============================================================================

class TrueMultiCoreManager:
    """真正的多核心处理管理器"""
    
    def __init__(self, config: Dict[str, Any] = None):
        self.config = config or {}
        
        # 检测硬件配置
        self.cpu_count = os.cpu_count() or 4
        self.memory_gb = psutil.virtual_memory().total / (1024**3)
        
        # 为M4 Max优化工作进程数
        max_workers = self.config.get("max_workers")
        if max_workers is None:
            if self.cpu_count >= 16:  # M4 Max
                self.max_workers = min(self.cpu_count - 2, 14)
            elif self.cpu_count >= 8:
                self.max_workers = min(self.cpu_count - 2, 8)
            else:
                self.max_workers = max(self.cpu_count - 1, 2)
        else:
            self.max_workers = min(max_workers, self.cpu_count)
        
        # 配置
        self.enabled = self.config.get("enabled", True)
        self.process_timeout = self.config.get("timeout", 300)
        
        # 进程池
        self.executor = None
        self.is_started = False
        
        # 统计信息
        self.stats = {
            "tasks_processed": 0,
            "total_time": 0,
            "successful_tasks": 0,
            "failed_tasks": 0
        }
        
        # 日志监控
        self.log_monitor = get_log_monitor()
        self.current_session_id = None
        
        logger.info(f"TrueMultiCoreManager initialized:")
        logger.info(f"  - CPU cores: {self.cpu_count}")
        logger.info(f"  - Max workers: {self.max_workers}")
        logger.info(f"  - Memory: {self.memory_gb:.1f} GB")
        logger.info(f"  - Enabled: {self.enabled}")
        logger.info(f"  - Log monitor: {'✅' if self.log_monitor else '❌'}")
    
    def start(self):
        """启动进程池"""
        if self.is_started or not self.enabled:
            return
            
        try:
            # 使用spawn方法（macOS兼容）
            mp_context = mp.get_context('spawn')
            
            self.executor = concurrent.futures.ProcessPoolExecutor(
                max_workers=self.max_workers,
                mp_context=mp_context,
                initializer=worker_init,
                initargs=(0,)  # worker_id will be set by the executor
            )
            
            self.is_started = True
            logger.info(f"Process pool started with {self.max_workers} workers")
            
        except Exception as e:
            logger.error(f"Failed to start process pool: {e}")
            self.enabled = False
    
    def stop(self):
        """停止进程池"""
        if self.executor and self.is_started:
            logger.info("Shutting down process pool...")
            self.executor.shutdown(wait=True)
            self.executor = None
            self.is_started = False
            logger.info("Process pool shut down")
    
    def should_use_multicore(self, task_count: int, task_type: str = "general") -> bool:
        """判断是否应该使用多核心处理"""
        if not self.enabled or task_count < 2:
            return False
            
        # 检查内存使用
        memory_percent = psutil.virtual_memory().percent
        if memory_percent > 85:
            logger.warning(f"High memory usage ({memory_percent:.1f}%), skipping multicore")
            return False
        
        # 根据任务类型设置阈值
        thresholds = {
            "embedding": 3,    # 嵌入任务相对轻量，3个以上就用多核心
            "ner": 2,          # NER任务较重，2个以上就用多核心
            "llm_call": 2,     # LLM调用，2个以上就用多核心
            "general": 5
        }
        
        threshold = thresholds.get(task_type, 5)
        use_multicore = task_count >= threshold
        
        logger.info(f"Multi-core decision: {task_count} {task_type} tasks >= {threshold} = {use_multicore}")
        return use_multicore
    
    async def process_embeddings_parallel(self, 
                                        texts: List[str], 
                                        model_config: Dict[str, Any] = None) -> List[List[float]]:
        """并行处理嵌入生成"""
        if not texts:
            return []
            
        if not self.should_use_multicore(len(texts), "embedding"):
            logger.info(f"Using sequential processing for {len(texts)} embeddings")
            return await self._process_embeddings_sequential(texts, model_config)
        
        logger.info(f"🚀 Using multi-process parallel processing for {len(texts)} embeddings")
        
        if not self.is_started:
            self.start()
            
        if not self.executor:
            return await self._process_embeddings_sequential(texts, model_config)
        
        # 启动监控会话
        if self.log_monitor:
            self.current_session_id = f"embedding_{int(time.time())}"
            self.log_monitor.start_session(
                session_id=self.current_session_id,
                session_type="embedding",
                total_tasks=len(texts),
                worker_count=self.max_workers
            )
        
        start_time = time.time()
        
        # 创建任务
        tasks = []
        for i, text in enumerate(texts):
            task = ProcessingTask(
                task_id=f"embedding_{i}",
                task_type="embedding",
                data=text,
                config=model_config or {"model_name": "sentence-transformers/all-MiniLM-L6-v2"}
            )
            tasks.append(task)
        
        # 提交任务到进程池
        future_to_task = {}
        for i, task in enumerate(tasks):
            future = self.executor.submit(process_embedding_task, task, i % self.max_workers)
            future_to_task[future] = task
        
        logger.info(f"📤 已提交 {len(tasks)} 个嵌入任务到 {self.max_workers} 个工作进程")
        
        # 收集结果
        results = [None] * len(texts)
        success_count = 0
        completed_count = 0
        unfinished_futures = set(future_to_task.keys())
        
        try:
            # 不设置超时限制，让任务自然完成
            for future in concurrent.futures.as_completed(future_to_task):
                unfinished_futures.discard(future)
            task = future_to_task[future]
            try:
                result = future.result()
                task_index = int(task.task_id.split('_')[1])
                completed_count += 1
                
                if result.success:
                    results[task_index] = result.result
                    success_count += 1
                    logger.debug(f"✅ 嵌入任务 {task_index+1} 完成 (worker_{result.worker_id}, {result.processing_time:.3f}s)")
                    
                    # 记录到监控器
                    if self.log_monitor:
                        self.log_monitor.log_event(
                            event_type="task_complete",
                            worker_id=result.worker_id,
                            task_id=result.task_id,
                            message=f"Embedding task completed successfully",
                            metadata={"processing_time": result.processing_time}
                        )
                else:
                    results[task_index] = []
                    logger.error(f"❌ 嵌入任务 {task_index+1} 失败: {result.error}")
                    
                    # 记录到监控器
                    if self.log_monitor:
                        self.log_monitor.log_event(
                            event_type="task_error",
                            worker_id=result.worker_id,
                            task_id=result.task_id,
                            message=f"Embedding task failed: {result.error}",
                            metadata={"error": result.error}
                        )
                
                # 实时进度报告
                if completed_count % 5 == 0 or completed_count == len(tasks):
                    progress_percent = (completed_count / len(tasks)) * 100
                    logger.info(f"📊 嵌入处理进度: {completed_count}/{len(tasks)} ({progress_percent:.1f}%) - 成功: {success_count}")
                    
            except Exception as e:
                task_index = int(task.task_id.split('_')[1])
                results[task_index] = []
                completed_count += 1
                logger.error(f"❌ 嵌入任务 {task_index+1} 异常: {e}")
                
                # 实时进度报告
                if completed_count % 5 == 0 or completed_count == len(tasks):
                    progress_percent = (completed_count / len(tasks)) * 100
                    logger.info(f"📊 嵌入处理进度: {completed_count}/{len(tasks)} ({progress_percent:.1f}%) - 成功: {success_count}")
        
        except Exception as e:
            logger.error(f"❌ 嵌入批处理过程中发生异常: {e}")
            # 尝试收集已完成的结果
            logger.info(f"🔄 尝试收集已完成的任务结果...")
            
        # 检查是否有未完成的任务
        if unfinished_futures:
            unfinished_count = len(unfinished_futures)
            logger.error(f"❌ 嵌入并行处理失败: {unfinished_count} (of {len(tasks)}) futures unfinished")
            
            # 尝试取消未完成的任务
            cancelled_count = 0
            for future in unfinished_futures:
                if future.cancel():
                    cancelled_count += 1
            
            if cancelled_count > 0:
                logger.info(f"🚫 已取消 {cancelled_count} 个未完成的嵌入任务")
        
        total_time = time.time() - start_time
        
        # 更新统计
        self.stats["tasks_processed"] += len(texts)
        self.stats["total_time"] += total_time
        self.stats["successful_tasks"] += success_count
        self.stats["failed_tasks"] += len(texts) - success_count
        
        logger.info(f"✅ Multi-process embedding completed:")
        logger.info(f"   📊 Total: {len(texts)} texts")
        logger.info(f"   ⏱️ Time: {total_time:.2f}s")
        logger.info(f"   🚄 Throughput: {len(texts)/total_time:.1f} texts/sec")
        logger.info(f"   ✅ Success: {success_count}/{len(texts)} ({success_count/len(texts)*100:.1f}%)")
        
        # 结束监控会话
        if self.log_monitor and self.current_session_id:
            self.log_monitor.end_session(self.current_session_id)
            self.current_session_id = None
        
        return results
    
    async def process_ner_parallel(self, 
                                 note_contents: List[str],
                                 existing_tags_list: List[List[str]] = None,
                                 llm_config: Dict[str, Any] = None) -> List[List[Dict[str, Any]]]:
        """并行处理NER任务"""
        if not note_contents:
            return []
            
        if existing_tags_list is None:
            existing_tags_list = [[] for _ in note_contents]
            
        if not self.should_use_multicore(len(note_contents), "ner"):
            logger.info(f"Using sequential processing for {len(note_contents)} NER tasks")
            return await self._process_ner_sequential(note_contents, existing_tags_list, llm_config)
        
        logger.info(f"🚀 Using multi-process parallel processing for {len(note_contents)} NER tasks")
        
        if not self.is_started:
            self.start()
            
        if not self.executor:
            return await self._process_ner_sequential(note_contents, existing_tags_list, llm_config)
        
        # 启动监控会话
        if self.log_monitor:
            self.current_session_id = f"ner_{int(time.time())}"
            self.log_monitor.start_session(
                session_id=self.current_session_id,
                session_type="ner",
                total_tasks=len(note_contents),
                worker_count=self.max_workers
            )
        
        start_time = time.time()
        
        # 创建任务
        tasks = []
        for i, (content, existing_tags) in enumerate(zip(note_contents, existing_tags_list)):
            task = ProcessingTask(
                task_id=f"ner_{i}",
                task_type="ner",
                data={"content": content, "existing_tags": existing_tags},
                config=llm_config or {
                    "base_url": "http://localhost:11434",
                    "model": "qwen2.5:1.5b",  # 使用配置文件中的正确模型
                    "timeout": 300  # 增加单个任务超时时间到5分钟
                }
            )
            tasks.append(task)
        
        # 提交任务到进程池
        future_to_task = {}
        for i, task in enumerate(tasks):
            future = self.executor.submit(process_ner_task, task, i % self.max_workers)
            future_to_task[future] = task
        
        logger.info(f"📤 已提交 {len(tasks)} 个NER任务到 {self.max_workers} 个工作进程")
        
        # 收集结果
        results = [[] for _ in note_contents]
        success_count = 0
        completed_count = 0
        unfinished_futures = set(future_to_task.keys())
        
        try:
            # 不设置超时限制，让任务自然完成
            for future in concurrent.futures.as_completed(future_to_task):
                unfinished_futures.discard(future)
                task = future_to_task[future]
                try:
                    result = future.result()
                    task_index = int(task.task_id.split('_')[1])
                    completed_count += 1
                    
                    if result.success:
                        results[task_index] = result.result
                        if result.result:  # 有标签建议
                            success_count += 1
                            logger.debug(f"✅ NER任务 {task_index+1} 完成 (worker_{result.worker_id}, {result.processing_time:.3f}s, {len(result.result)} tags)")
                            
                            # 记录到监控器
                            if self.log_monitor:
                                self.log_monitor.log_event(
                                    event_type="task_complete",
                                    worker_id=result.worker_id,
                                    task_id=result.task_id,
                                    message=f"NER task completed with {len(result.result)} tags",
                                    metadata={"processing_time": result.processing_time, "tag_count": len(result.result)}
                                )
                        else:
                            logger.debug(f"⚪ NER任务 {task_index+1} 完成但无标签 (worker_{result.worker_id}, {result.processing_time:.3f}s)")
                            
                            # 记录到监控器
                            if self.log_monitor:
                                self.log_monitor.log_event(
                                    event_type="task_complete",
                                    worker_id=result.worker_id,
                                    task_id=result.task_id,
                                    message=f"NER task completed with no tags",
                                    metadata={"processing_time": result.processing_time, "tag_count": 0}
                                )
                    else:
                        logger.error(f"❌ NER任务 {task_index+1} 失败: {result.error}")
                        
                        # 记录到监控器
                        if self.log_monitor:
                            self.log_monitor.log_event(
                                event_type="task_error",
                                worker_id=result.worker_id,
                                task_id=result.task_id,
                                message=f"NER task failed: {result.error}",
                                metadata={"error": result.error}
                            )
                    
                    # 实时进度报告
                    if completed_count % 3 == 0 or completed_count == len(tasks):
                        progress_percent = (completed_count / len(tasks)) * 100
                        logger.info(f"📊 NER处理进度: {completed_count}/{len(tasks)} ({progress_percent:.1f}%) - 有结果: {success_count}")
                        
                except Exception as e:
                    task_index = int(task.task_id.split('_')[1])
                    completed_count += 1
                    logger.error(f"❌ NER任务 {task_index+1} 异常: {e}")
                    
                    # 实时进度报告
                    if completed_count % 3 == 0 or completed_count == len(tasks):
                        progress_percent = (completed_count / len(tasks)) * 100
                        logger.info(f"📊 NER处理进度: {completed_count}/{len(tasks)} ({progress_percent:.1f}%) - 有结果: {success_count}")
        
        except Exception as e:
            logger.error(f"❌ 批处理过程中发生异常: {e}")
            # 尝试收集已完成的结果
            logger.info(f"🔄 尝试收集已完成的任务结果...")
            
        # 检查是否有未完成的任务
        if unfinished_futures:
            unfinished_count = len(unfinished_futures)
            logger.error(f"❌ 多进程并行处理失败: {unfinished_count} (of {len(tasks)}) futures unfinished")
            
            # 尝试取消未完成的任务
            cancelled_count = 0
            for future in unfinished_futures:
                if future.cancel():
                    cancelled_count += 1
            
            if cancelled_count > 0:
                logger.info(f"🚫 已取消 {cancelled_count} 个未完成的任务")
        
        total_time = time.time() - start_time
        
        # 更新统计
        self.stats["tasks_processed"] += len(note_contents)
        self.stats["total_time"] += total_time
        self.stats["successful_tasks"] += success_count
        self.stats["failed_tasks"] += len(note_contents) - success_count
        
        logger.info(f"✅ Multi-process NER completed:")
        logger.info(f"   📊 Total: {len(note_contents)} tasks")
        logger.info(f"   ⏱️ Time: {total_time:.2f}s")
        logger.info(f"   🚄 Throughput: {len(note_contents)/total_time:.1f} tasks/sec")
        logger.info(f"   ✅ Success: {success_count}/{len(note_contents)} ({success_count/len(note_contents)*100:.1f}%)")
        
        # 结束监控会话
        if self.log_monitor and self.current_session_id:
            self.log_monitor.end_session(self.current_session_id)
            self.current_session_id = None
        
        return results
    
    async def _process_embeddings_sequential(self, texts: List[str], model_config: Dict[str, Any] = None) -> List[List[float]]:
        """顺序处理嵌入（回退方案）"""
        try:
            from sentence_transformers import SentenceTransformer
            model_name = (model_config or {}).get("model_name", "sentence-transformers/all-MiniLM-L6-v2")
            model = SentenceTransformer(model_name)
            
            embeddings = await asyncio.to_thread(model.encode, texts)
            return [emb.tolist() for emb in embeddings]
        except Exception as e:
            logger.error(f"Sequential embedding failed: {e}")
            return [[] for _ in texts]
    
    async def _process_ner_sequential(self, 
                                    note_contents: List[str],
                                    existing_tags_list: List[List[str]],
                                    llm_config: Dict[str, Any] = None) -> List[List[Dict[str, Any]]]:
        """顺序处理NER（回退方案）"""
        results = []
        for content, existing_tags in zip(note_contents, existing_tags_list):
            # 这里应该调用原有的单个NER处理逻辑
            # 暂时返回空结果
            results.append([])
        return results
    
    def get_stats(self) -> Dict[str, Any]:
        """获取统计信息"""
        memory = psutil.virtual_memory()
        cpu_percent = psutil.cpu_percent(interval=0.1)
        
        return {
            "manager": {
                "enabled": self.enabled,
                "started": self.is_started,
                "max_workers": self.max_workers,
                "tasks_processed": self.stats["tasks_processed"],
                "total_time": self.stats["total_time"],
                "successful_tasks": self.stats["successful_tasks"],
                "failed_tasks": self.stats["failed_tasks"],
                "avg_time_per_task": self.stats["total_time"] / max(self.stats["tasks_processed"], 1)
            },
            "system": {
                "cpu_count": self.cpu_count,
                "cpu_usage": cpu_percent,
                "memory_total_gb": memory.total / (1024**3),
                "memory_used_gb": memory.used / (1024**3),
                "memory_percent": memory.percent
            }
        }
    
    def __enter__(self):
        """上下文管理器入口"""
        self.start()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """上下文管理器出口"""
        self.stop()

# ============================================================================
# 保持兼容性的别名
# ============================================================================

# 为了保持向后兼容
MultiCoreManager = TrueMultiCoreManager 
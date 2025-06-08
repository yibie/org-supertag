#!/usr/bin/env python3
"""
LlamaCpp Embedding Service
使用 llama.cpp 的 llama-embedding 命令行工具提供嵌入服务
"""

import os
import subprocess
import tempfile
import logging
import json
import asyncio
from typing import List, Optional, Dict, Any
from dataclasses import dataclass
from pathlib import Path

logger = logging.getLogger(__name__)

@dataclass
class EmbeddingResult:
    """嵌入结果"""
    success: bool
    embedding: Optional[List[float]] = None
    dimension: int = 0
    error_message: Optional[str] = None
    processing_time: float = 0.0

class LlamaCppEmbeddingService:
    """
    LlamaCpp 嵌入服务
    封装 llama-embedding 命令行工具
    """
    
    def __init__(self, 
                 model_path: str,
                 binary_path: str = "llama-embedding",
                 pooling: str = "mean",
                 n_threads: Optional[int] = None,
                 n_ctx: int = 4096):
        """
        初始化 LlamaCpp 嵌入服务
        
        Args:
            model_path: GGUF模型文件路径
            binary_path: llama-embedding 二进制文件路径
            pooling: 池化策略 ("mean", "cls", "last", etc.)
            n_threads: 线程数
            n_ctx: 上下文长度
        """
        self.model_path = Path(model_path).expanduser()
        self.binary_path = binary_path
        self.pooling = pooling
        self.n_threads = n_threads or os.cpu_count()
        self.n_ctx = n_ctx
        
        # 验证模型文件
        if not self.model_path.exists():
            raise FileNotFoundError(f"Model file not found: {self.model_path}")
        
        # 验证二进制文件
        try:
            subprocess.run([self.binary_path, "--help"], 
                         capture_output=True, check=True, timeout=10)
        except (subprocess.CalledProcessError, FileNotFoundError, subprocess.TimeoutExpired) as e:
            raise RuntimeError(f"llama-embedding binary not found or not working: {e}")
        
        logger.info(f"LlamaCpp embedding service initialized:")
        logger.info(f"  Model: {self.model_path}")
        logger.info(f"  Binary: {self.binary_path}")
        logger.info(f"  Pooling: {self.pooling}")
        logger.info(f"  Threads: {self.n_threads}")
    
    async def get_embedding(self, text: str) -> EmbeddingResult:
        """
        获取单个文本的嵌入向量
        
        Args:
            text: 输入文本
            
        Returns:
            EmbeddingResult: 嵌入结果
        """
        import time
        start_time = time.time()
        
        if not text or not text.strip():
            return EmbeddingResult(
                success=False,
                error_message="Empty text provided"
            )
        
        try:
            # 使用临时文件避免管道问题
            with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as tmp_file:
                tmp_file.write(text.strip())
                tmp_file_path = tmp_file.name
            
            try:
                # 构建命令
                cmd = [
                    self.binary_path,
                    "-m", str(self.model_path),
                    "-f", tmp_file_path,
                    "--pooling", self.pooling,
                    "-t", str(self.n_threads),
                    "-c", str(self.n_ctx),
                    "--embd-output-format", "json",
                    "--embd-normalize", "2",  # L2标准化
                    "--no-warmup"  # 跳过预热
                ]
                
                logger.debug(f"Running command: {' '.join(cmd)}")
                
                # 运行命令
                result = await asyncio.create_subprocess_exec(
                    *cmd,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE
                )
                
                stdout, stderr = await result.communicate()
                
                if result.returncode != 0:
                    error_msg = f"llama-embedding failed with code {result.returncode}: {stderr.decode()}"
                    logger.error(error_msg)
                    return EmbeddingResult(
                        success=False,
                        error_message=error_msg,
                        processing_time=time.time() - start_time
                    )
                
                # 解析输出
                output = stdout.decode().strip()
                if not output:
                    return EmbeddingResult(
                        success=False,
                        error_message="No output from llama-embedding",
                        processing_time=time.time() - start_time
                    )
                
                # 尝试解析JSON格式输出
                try:
                    # 查找JSON部分（可能混有其他日志）
                    lines = output.split('\n')
                    
                    # 寻找包含JSON数据的部分
                    json_text = ""
                    in_json = False
                    brace_count = 0
                    
                    for line in lines:
                        line = line.strip()
                        if line.startswith('{'):
                            in_json = True
                            json_text = line
                            brace_count = line.count('{') - line.count('}')
                            if brace_count == 0:
                                break
                        elif in_json:
                            json_text += line
                            brace_count += line.count('{') - line.count('}')
                            if brace_count == 0:
                                break
                    
                    if json_text:
                        try:
                            json_data = json.loads(json_text)
                            
                            # 处理OpenAI风格的嵌入格式
                            if isinstance(json_data, dict) and 'data' in json_data:
                                data_array = json_data['data']
                                if isinstance(data_array, list) and len(data_array) > 0:
                                    first_item = data_array[0]
                                    if isinstance(first_item, dict) and 'embedding' in first_item:
                                        embedding = first_item['embedding']
                                    else:
                                        raise ValueError("No embedding field in data item")
                                else:
                                    raise ValueError("Empty data array")
                            elif isinstance(json_data, dict) and 'embedding' in json_data:
                                embedding = json_data['embedding']
                            elif isinstance(json_data, list):
                                if len(json_data) > 0 and isinstance(json_data[0], list):
                                    embedding = json_data[0]
                                else:
                                    embedding = json_data
                            else:
                                raise ValueError("Unexpected JSON structure")
                                
                        except json.JSONDecodeError as e:
                            logger.debug(f"JSON parsing failed, trying alternative parsing: {e}")
                            # 如果JSON解析失败，尝试提取嵌入数组
                            # 查找 "embedding": [数字列表] 模式
                            import re
                            pattern = r'"embedding":\s*\[([\d\.\-,\s]+)\]'
                            match = re.search(pattern, output, re.DOTALL)
                            if match:
                                numbers_text = match.group(1)
                                # 分割并转换为浮点数
                                numbers = re.findall(r'-?\d+\.?\d*', numbers_text)
                                embedding = [float(n) for n in numbers if n]
                            else:
                                raise ValueError("Could not extract embedding from output")
                    else:
                        # 如果没有找到JSON，尝试解析原始数字输出
                        # 查找包含数字的行
                        number_lines = []
                        for line in lines:
                            if any(char.isdigit() or char == '.' or char == '-' for char in line):
                                number_lines.append(line)
                        
                        if number_lines:
                            # 将所有数字行合并并解析
                            numbers_text = ' '.join(number_lines)
                            # 提取浮点数
                            import re
                            numbers = re.findall(r'-?\d+\.?\d*', numbers_text)
                            embedding = [float(n) for n in numbers if n]
                        else:
                            raise ValueError("No numeric data found in output")
                    
                    # 验证嵌入向量
                    if not embedding or not isinstance(embedding, list):
                        raise ValueError("Invalid embedding format")
                    
                    # 检查是否全为零（可能表示错误）
                    if all(abs(x) < 1e-10 for x in embedding):
                        logger.warning("Generated embedding contains all zeros, this might indicate an issue")
                    
                    processing_time = time.time() - start_time
                    logger.debug(f"Generated embedding: dimension={len(embedding)}, time={processing_time:.3f}s")
                    
                    return EmbeddingResult(
                        success=True,
                        embedding=embedding,
                        dimension=len(embedding),
                        processing_time=processing_time
                    )
                    
                except (json.JSONDecodeError, ValueError, IndexError) as e:
                    error_msg = f"Failed to parse embedding output: {e}\nOutput: {output[:500]}..."
                    logger.error(error_msg)
                    return EmbeddingResult(
                        success=False,
                        error_message=error_msg,
                        processing_time=time.time() - start_time
                    )
            
            finally:
                # 清理临时文件
                try:
                    os.unlink(tmp_file_path)
                except:
                    pass
                    
        except Exception as e:
            error_msg = f"Unexpected error in get_embedding: {e}"
            logger.error(error_msg, exc_info=True)
            return EmbeddingResult(
                success=False,
                error_message=error_msg,
                processing_time=time.time() - start_time
            )
    
    async def get_embeddings_batch(self, texts: List[str]) -> List[EmbeddingResult]:
        """
        批量获取嵌入向量
        
        Args:
            texts: 文本列表
            
        Returns:
            List[EmbeddingResult]: 嵌入结果列表
        """
        logger.info(f"Processing batch of {len(texts)} texts")
        
        # 简单实现：逐个处理
        # TODO: 优化为真正的批量处理
        results = []
        for i, text in enumerate(texts):
            logger.debug(f"Processing text {i+1}/{len(texts)}")
            result = await self.get_embedding(text)
            results.append(result)
            
            # 如果连续失败太多，提前退出
            if i > 5 and all(not r.success for r in results[-5:]):
                logger.error("Too many consecutive failures, stopping batch processing")
                # 为剩余文本添加失败结果
                for j in range(i+1, len(texts)):
                    results.append(EmbeddingResult(
                        success=False,
                        error_message="Batch processing stopped due to consecutive failures"
                    ))
                break
        
        success_count = sum(1 for r in results if r.success)
        logger.info(f"Batch processing completed: {success_count}/{len(texts)} successful")
        
        return results
    
    async def health_check(self) -> bool:
        """
        健康检查
        
        Returns:
            bool: 服务是否正常
        """
        try:
            result = await self.get_embedding("Health check test")
            return result.success
        except Exception as e:
            logger.error(f"Health check failed: {e}")
            return False
    
    def get_model_info(self) -> Dict[str, Any]:
        """
        获取模型信息
        
        Returns:
            Dict: 模型信息
        """
        return {
            "model_path": str(self.model_path),
            "model_exists": self.model_path.exists(),
            "model_size_mb": round(self.model_path.stat().st_size / (1024*1024), 1) if self.model_path.exists() else 0,
            "binary_path": self.binary_path,
            "pooling": self.pooling,
            "n_threads": self.n_threads,
            "n_ctx": self.n_ctx
        }

def create_qwen3_embedding_service(
    model_path: Optional[str] = None,
    binary_path: Optional[str] = None
) -> LlamaCppEmbeddingService:
    """
    创建 Qwen3-Embedding 服务的便捷函数
    
    Args:
        model_path: 模型路径，默认使用环境变量或标准位置
        binary_path: 二进制路径，默认自动查找
        
    Returns:
        LlamaCppEmbeddingService: 配置好的服务实例
    """
    # 确定模型路径
    if not model_path:
        # 尝试环境变量
        model_path = os.getenv("QWEN3_MODEL_PATH")
        
        # 尝试标准位置
        if not model_path:
            standard_locations = [
                "~/.models/Qwen3-Embedding-0.6B-GGUF/Qwen3-Embedding-0.6B-Q8_0.gguf",
                "~/.models/Qwen3-Embedding-0.6B-GGUF/Qwen3-Embedding-0.6B-f16.gguf",
                "./models/Qwen3-Embedding-0.6B-Q8_0.gguf",
                "./models/Qwen3-Embedding-0.6B-f16.gguf"
            ]
            
            for location in standard_locations:
                path = Path(location).expanduser()
                if path.exists():
                    model_path = str(path)
                    logger.info(f"Found Qwen3 model at: {model_path}")
                    break
    
    if not model_path:
        raise FileNotFoundError(
            "Qwen3-Embedding model not found. Please specify model_path or set QWEN3_MODEL_PATH environment variable"
        )
    
    # 确定二进制路径
    if not binary_path:
        # 尝试环境变量
        binary_path = os.getenv("LLAMA_CPP_PATH", "llama-embedding")
        
        # 检查常见位置
        common_binaries = ["llama-embedding", "llama-cpp", "main"]
        for binary in common_binaries:
            try:
                subprocess.run([binary, "--help"], capture_output=True, check=True, timeout=5)
                binary_path = binary
                logger.info(f"Found llama.cpp binary: {binary_path}")
                break
            except:
                continue
    
    # 创建服务
    return LlamaCppEmbeddingService(
        model_path=model_path,
        binary_path=binary_path,
        n_threads=os.cpu_count()
    ) 
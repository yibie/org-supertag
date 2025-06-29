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
import shutil
import re
import time

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
                 n_ctx: int = 4096,
                 config_dict: Optional[Dict[str, Any]] = None):
        """
        初始化 LlamaCpp 嵌入服务
        
        Args:
            model_path: GGUF模型文件路径
            binary_path: llama-embedding 二进制文件路径
            pooling: 池化策略 ("mean", "cls", "last", etc.)
            n_threads: 线程数
            n_ctx: 上下文长度
            config_dict: 配置字典，包含长文本处理选项
        """
        self.model_path = Path(model_path).expanduser()
        self.binary_path = binary_path
        self.pooling = pooling
        self.n_threads = n_threads or os.cpu_count()
        self.n_ctx = n_ctx
        
        # 配置（移除了 chunking 相关配置，使用 node-based 预处理）
        self.config = config_dict or {}
        
        # 验证模型文件
        if not self.model_path.exists():
            raise FileNotFoundError(f"Model file not found: {self.model_path}")
        
        # 验证二进制文件
        try:
            subprocess.run([self.binary_path, "--help"], 
                         capture_output=True, check=True, timeout=10)
        except (subprocess.CalledProcessError, FileNotFoundError, subprocess.TimeoutExpired) as e:
            raise RuntimeError(f"llama-embedding binary not found or not working: {e}")
        
        logger.info("LlamaCpp embedding service initialized:")
        logger.info(f"  Model: {self.model_path}")
        logger.info(f"  Binary: {self.binary_path}")
        logger.info(f"  Pooling: {self.pooling}")
        logger.info(f"  Threads: {self.n_threads}")
    
    # 移除了 _split_text_into_chunks 和 _aggregate_embeddings 方法
    # 这些 chunking 相关的功能已被 node-based 预处理策略替代

    def _preprocess_text(self, text: str) -> str:
        """智能文本预处理，处理可能导致嵌入失败的问题"""
        if not text:
            return text
        
        # 保存原始文本用于调试
        original_text = text
        
        # 1. 规范化换行符和空白字符
        # 将多个连续换行符替换为单个空格
        text = re.sub(r'\n{2,}', ' ', text)  # 多个换行 -> 单个空格
        text = re.sub(r'\n', ' ', text)      # 单个换行 -> 空格
        text = re.sub(r'\r', ' ', text)      # 回车 -> 空格
        text = re.sub(r'\t', ' ', text)      # 制表符 -> 空格
        
        # 2. 处理重复的空格
        text = re.sub(r' {2,}', ' ', text)   # 多个空格 -> 单个空格
        
        # 3. 移除首尾空白
        text = text.strip()
        
        # 4. 处理过短的文本 - 添加一些上下文
        if len(text.strip()) < 3:
            if text.strip():
                # 对于很短的文本，添加描述性前缀
                text = f"文本内容：{text.strip()}"
            
        # 5. 处理重复内容（如"点评点评"）
        # 检测重复模式并简化
        if len(text) < 20:  # 只对短文本做重复检测
            # 检查是否是简单重复（如："abc abc"）
            words = text.split()
            if len(words) >= 2:
                # 检查是否有相邻的重复词
                unique_words = []
                prev_word = None
                for word in words:
                    if word != prev_word:
                        unique_words.append(word)
                        prev_word = word
                if len(unique_words) < len(words):
                    text = ' '.join(unique_words)
                    logger.debug(f"Removed adjacent duplicates: {repr(original_text)} -> {repr(text)}")
        
        # 6. 确保文本不会太短导致tokenizer问题
        if len(text.strip()) < 5:
            text = f"这是关于'{text.strip()}'的内容"
            logger.debug(f"Expanded short text: {repr(original_text)} -> {repr(text)}")
        
        # 记录预处理变化
        if text != original_text:
            logger.debug(f"Text preprocessed: {repr(original_text)} -> {repr(text)}")
        
        return text

    def _analyze_text(self, text: str) -> Dict[str, Any]:
        """分析文本特征，帮助识别可能导致问题的内容"""
        analysis = {
            "length": len(text),
            "stripped_length": len(text.strip()),
            "is_empty": not text.strip(),
            "has_special_chars": bool(any(ord(c) > 127 for c in text)),
            "has_control_chars": bool(any(ord(c) < 32 and c not in '\t\n\r' for c in text)),
            "starts_with_whitespace": text != text.lstrip(),
            "ends_with_whitespace": text != text.rstrip(),
            "preview": repr(text[:50]) + ("..." if len(text) > 50 else "")
        }
        return analysis

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
        
        # 更严格的空文本检查
        if not text or not text.strip():
            text_analysis = self._analyze_text(text)
            logger.warning(f"Empty or whitespace-only text detected: {text_analysis['preview']}")
            return EmbeddingResult(
                success=False,
                error_message=f"Empty or whitespace-only text provided: {text_analysis['preview']}"
            )
        
        # 智能文本预处理
        original_text = text
        text = self._preprocess_text(text)
        
        # 如果预处理后文本变空，记录并返回错误
        if not text.strip():
            logger.warning(f"Text became empty after preprocessing: {repr(original_text)}")
            return EmbeddingResult(
                success=False,
                error_message=f"Text became empty after preprocessing: {repr(original_text)}"
            )
        
        # 简单的长度检查和截断（node-based 预处理应该已经处理了长度）
        # 使用基础的字符长度检查作为安全保障
        char_length = len(text.strip())
        safe_char_length = int(self.n_ctx * 0.6)  # 更保守的字符长度限制
        
        if char_length > safe_char_length:
            # 如果字符长度仍然超长，进行截断并警告
            logger.warning(f"Text still too long after preprocessing ({char_length} chars, max {safe_char_length}), truncating. This suggests node-based preprocessing needs adjustment.")
            text = text[:safe_char_length]
            logger.info(f"Truncated text to {len(text)} characters")
        
        # 尝试不同的池化策略来避免llama.cpp的bug
        pooling_strategies = [self.pooling, "cls", "last", "mean"]
        # 去重并保持顺序
        pooling_strategies = list(dict.fromkeys(pooling_strategies))
        
        for i, pooling_strategy in enumerate(pooling_strategies):
            logger.debug(f"Trying pooling strategy: {pooling_strategy} (attempt {i+1}/{len(pooling_strategies)})")
            
            result = await self._try_embedding_with_pooling(text, pooling_strategy, start_time)
            
            if result.success:
                if i > 0:  # 如果不是第一次尝试成功的
                    logger.info(f"Successfully generated embedding using fallback pooling strategy: {pooling_strategy}")
                return result
            else:
                # 检查是否是GGML_ASSERT错误
                if "GGML_ASSERT" in str(result.error_message) or "seq_id" in str(result.error_message):
                    text_preview = repr(text[:30]) + ("..." if len(text) > 30 else "")
                    logger.warning(f"Pooling strategy '{pooling_strategy}' failed with sequence ID issue for text {text_preview}, trying next strategy")
                    continue
                else:
                    # 其他类型的错误，可能不是池化策略问题
                    logger.error(f"Non-pooling related error with strategy '{pooling_strategy}': {result.error_message}")
                    return result
        
        # 所有池化策略都失败 - 记录详细的文本分析信息
        text_analysis = self._analyze_text(text)
        
        error_details = [
            "All pooling strategies failed for text:",
            f"  Text preview: {text_analysis['preview']}",
            f"  Length: {text_analysis['length']} (stripped: {text_analysis['stripped_length']})",
            f"  Is empty: {text_analysis['is_empty']}",
            f"  Has special chars: {text_analysis['has_special_chars']}",
            f"  Has control chars: {text_analysis['has_control_chars']}",
            f"  Has whitespace padding: start={text_analysis['starts_with_whitespace']}, end={text_analysis['ends_with_whitespace']}"
        ]
        
        error_message = "\n".join(error_details)
        logger.error(error_message)
        
        return EmbeddingResult(
            success=False,
            error_message=error_message,
            processing_time=time.time() - start_time
        )
    
    async def _try_embedding_with_pooling(self, text: str, pooling_strategy: str, start_time: float) -> EmbeddingResult:
        """尝试使用指定的池化策略生成嵌入"""
        import time
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
                    "--pooling", pooling_strategy,
                    "-t", str(self.n_threads),
                    "-c", str(self.n_ctx),
                    "-b", "1",  # 强制批处理大小为1，避免溢出
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
                
                # 过滤掉已知的、无害的警告信息
                stderr_lines = stderr.decode().split('\n')
                filtered_stderr = []
                for line in stderr_lines:
                    if not any(warning in line for warning in [
                        "tokenizer.ggml.add_eos_token",
                        "last token in the prompt is not SEP",
                        "setting batch size to",
                        "using device Metal",
                        "llama_model_loader:",
                        "print_info:",
                        "load_tensors:",
                        "llama_context:",
                        "ggml_metal_init:",
                        "llama_kv_cache_unified:",
                        "system_info:",
                        "load: special tokens cache size",
                        "load: token to piece cache size",
                        "common_init_from_params:"
                    ]):
                        filtered_stderr.append(line)
                
                filtered_stderr_text = '\n'.join(filtered_stderr).strip()
                
                if result.returncode != 0:
                    # 只记录过滤后的错误信息
                    if filtered_stderr_text:
                        error_msg = f"llama-embedding failed with code {result.returncode}: {filtered_stderr_text}"
                    else:
                        error_msg = f"llama-embedding failed with code {result.returncode} (no significant error details)"
                    
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
    
    # 移除了 _get_embedding_chunked 方法
    # chunking 功能已被 node-based 预处理策略完全替代
    
    async def get_embeddings_batch(self, texts: List[str]) -> List[EmbeddingResult]:
        """
        串行批量获取嵌入向量 - 针对 llama-embedding 的特性优化
        
        由于 llama-embedding 每次调用都需要加载模型，并发反而会增加开销
        因此采用优化的串行处理策略
        
        Args:
            texts: 文本列表
            
        Returns:
            List[EmbeddingResult]: 嵌入结果列表
        """
        logger.info(f"Processing batch of {len(texts)} texts (optimized serial)")
        start_time = time.time()
        
        results = []
        for i, text in enumerate(texts):
            logger.debug(f"Processing text {i+1}/{len(texts)}")
            result = await self.get_embedding(text)
            results.append(result)
            
            # 提供进度反馈
            if (i + 1) % 5 == 0 or i == len(texts) - 1:
                elapsed = time.time() - start_time
                rate = (i + 1) / elapsed if elapsed > 0 else 0
                remaining = (len(texts) - i - 1) / rate if rate > 0 else 0
                logger.info(f"Progress: {i+1}/{len(texts)} ({rate:.1f} texts/s, ~{remaining:.0f}s remaining)")
            
            # 如果连续失败太多，提前退出
            if i >= 5 and all(not r.success for r in results[-5:]):
                logger.error("Too many consecutive failures, stopping batch processing")
                # 为剩余文本添加失败结果
                for j in range(i+1, len(texts)):
                    results.append(EmbeddingResult(
                        success=False,
                        error_message="Batch processing stopped due to consecutive failures"
                    ))
                break
        
        success_count = sum(1 for r in results if r.success)
        total_time = time.time() - start_time
        
        logger.info(f"Batch processing completed: {success_count}/{len(texts)} successful in {total_time:.2f}s")
        logger.info(f"Average time per text: {total_time/len(texts):.3f}s")
        
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
    binary_path: Optional[str] = None,
    config_dict: Optional[Dict[str, Any]] = None
) -> 'LlamaCppEmbeddingService':
    """
    创建配置好的 Qwen3-Embedding 服务
    
    Args:
        model_path: 模型文件路径，如果为None则使用默认路径
        binary_path: llama-embedding 二进制文件路径，如果为None则使用默认
        config_dict: 配置字典，用于获取额外的配置参数
        
    Returns:
        配置好的 LlamaCppEmbeddingService 实例
    """
    # 如果没有提供config_dict，创建一个默认的
    if config_dict is None:
        config_dict = {}
    
    # 默认模型路径
    if model_path is None:
        model_path = config_dict.get(
            "llama_cpp_model_path",
            "~/.models/Qwen3-Embedding-0.6B-GGUF/Qwen3-Embedding-0.6B-Q8_0.gguf"
        )
    
    # 展开用户路径
    model_path = os.path.expanduser(model_path)
    
    # 检查模型文件是否存在
    if not os.path.exists(model_path):
        raise FileNotFoundError(f"Qwen3-Embedding model not found at: {model_path}")
    
    # 默认二进制路径
    if binary_path is None:
        binary_path = config_dict.get("llama_cpp_binary", "llama-embedding")
    
    # 检查二进制文件
    if not shutil.which(binary_path):
        raise FileNotFoundError(f"llama-embedding binary not found: {binary_path}")
    
    # 从config获取参数
    pooling = config_dict.get("llama_cpp_pooling", "mean")
    n_threads = config_dict.get("llama_cpp_threads")
    n_ctx = config_dict.get("llama_cpp_max_context", 512)  # 使用较小的上下文避免批处理问题
    
    logger.info("Creating Qwen3-Embedding service with:")
    logger.info(f"  Model: {model_path}")
    logger.info(f"  Binary: {binary_path}")
    logger.info(f"  Pooling: {pooling}")
    logger.info(f"  Threads: {n_threads}")
    logger.info(f"  Context: {n_ctx}")
    
    # 直接使用LlamaCppEmbeddingService构造函数
    return LlamaCppEmbeddingService(
        model_path=model_path,
        binary_path=binary_path,
        pooling=pooling,
        n_threads=n_threads,
        n_ctx=n_ctx,
        config_dict=config_dict
    ) 
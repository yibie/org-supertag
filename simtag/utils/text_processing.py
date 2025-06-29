#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
from typing import Dict, Any

logger = logging.getLogger(__name__)

def prepare_node_text_for_embedding(node_data: Dict[str, Any], max_total_tokens: int = 280) -> str:
    """
    为 org-supertag node 准备向量化文本
    
    策略：标题 + 内容前N字，充分利用 org-mode 的结构化优势
    专门针对 LlamaCpp 的 512 token 限制进行优化
    
    Args:
        node_data: Node 数据字典
        max_total_tokens: 总的最大 token 数，默认280（留出安全余量）
        
    Returns:
        str: 准备好的文本
    """
    text_parts = []
    used_tokens = 0
    
    # 1. 优先添加标题（最重要的语义信息）
    title = node_data.get(':title', '').strip()
    if title:
        title_text = f"标题: {title}"
        title_tokens = _estimate_tokens(title_text)
        if used_tokens + title_tokens < max_total_tokens:
            text_parts.append(title_text)
            used_tokens += title_tokens
    
    # 2. 添加关键上下文（如果空间允许）
    olp = node_data.get(':olp', [])
    if olp and len(olp) > 0 and used_tokens < max_total_tokens * 0.5:  # 只在token使用率<50%时添加
        context = " > ".join(olp[-2:])  # 只取最近的2级父标题
        context_text = f"上下文: {context}"
        context_tokens = _estimate_tokens(context_text)
        if used_tokens + context_tokens < max_total_tokens * 0.7:  # 上下文不超过总量的70%
            text_parts.append(context_text)
            used_tokens += context_tokens
    
    # 3. 添加标签（如果空间允许且标签数量合理）
    tags = node_data.get(':tags', [])
    if tags and len(tags) <= 5 and used_tokens < max_total_tokens * 0.6:
        tags_text = f"标签: {', '.join(tags[:5])}"  # 最多5个标签
        tags_tokens = _estimate_tokens(tags_text)
        if used_tokens + tags_tokens < max_total_tokens * 0.8:  # 标签不超过总量的80%
            text_parts.append(tags_text)
            used_tokens += tags_tokens
    
    # 4. 用剩余空间添加内容
    content = node_data.get(':content', '').strip()
    if content:
        remaining_tokens = max_total_tokens - used_tokens - 20  # 保留20 token的安全边界
        if remaining_tokens > 50:  # 至少要有50 token才添加内容
            # 根据剩余token数动态计算内容字符数限制
            max_content_chars = int(remaining_tokens / 1.2)  # 保守估算
            truncated_content = _smart_truncate(content, max_content_chars)
            content_text = f"内容: {truncated_content}"
            text_parts.append(content_text)
    
    final_text = " | ".join(text_parts)
    
    # 最终检查：确保不超过限制
    final_tokens = _estimate_tokens(final_text)
    if final_tokens > max_total_tokens:
        # 如果还是超过，优先保留标题和少量内容
        title_only = node_data.get(':title', '').strip()
        content = node_data.get(':content', '').strip()
        if content:
            remaining_chars = max_total_tokens * 0.6  # 大约60%给内容
            truncated = _smart_truncate(content, int(remaining_chars))
            final_text = f"标题: {title_only} | 内容: {truncated}"
        else:
            final_text = f"标题: {title_only}"
    
    return final_text

def _estimate_tokens(text: str) -> float:
    """
    实用的 token 数量估算
    
    基于实际测试优化，平衡精度和安全性
    
    Args:
        text: 输入文本
        
    Returns:
        float: 估算的 token 数量
    """
    # 基于实际观察调整：中文字符约1.2 tokens，英文单词约0.8 tokens
    chinese_chars = len([c for c in text if '\u4e00' <= c <= '\u9fff'])
    other_chars = len(text) - chinese_chars
    
    # 更贴近实际的估算
    estimated = chinese_chars * 1.2 + other_chars * 0.6
    
    # 适度的安全边界（10%）
    return estimated * 1.1

def _smart_truncate(text: str, max_chars: int) -> str:
    """
    在句子边界智能截断文本
    
    Args:
        text: 原文本
        max_chars: 最大字符数
        
    Returns:
        str: 截断后的文本
    """
    if len(text) <= max_chars:
        return text
    
    # 在最大长度处向前查找句子边界
    truncated = text[:max_chars]
    
    # 查找句号、感叹号、问号等句子结束标记
    sentence_endings = ['。', '！', '？', '.', '!', '?', '\n\n']
    
    best_cut = 0
    for ending in sentence_endings:
        pos = truncated.rfind(ending)
        if pos > max_chars * 0.7:  # 确保截断位置不要太靠前
            best_cut = max(best_cut, pos + 1)
    
    if best_cut > 0:
        return text[:best_cut].strip()
    else:
        # 如果没找到合适的句子边界，就在单词边界截断
        words = truncated.split()
        return ' '.join(words[:-1]) + '...' if len(words) > 1 else truncated + '...'

#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
from typing import Dict, Any

logger = logging.getLogger(__name__)

def prepare_node_text_for_embedding(node_data: Dict[str, Any], max_total_tokens: int = 450) -> str:
    """
    Prepare vectorization text for org-supertag node
    
    Strategy: Title + first N characters of content, fully utilizing org-mode's structural advantages
    Specifically optimized for LlamaCpp's 512 token limit
    
    Args:
        node_data: Node data dictionary
        max_total_tokens: Maximum total tokens, default 280 (leaving safety margin)
        
    Returns:
        str: Prepared text
    """
    text_parts = []
    used_tokens = 0
    
    # 1. Prioritize adding title (most important semantic information)
    title = (node_data.get('title') or node_data.get(':title') or node_data.get('name') or '').strip()
    if title:
        title_text = f"Title: {title}"
        title_tokens = _estimate_tokens(title_text)
        if used_tokens + title_tokens < max_total_tokens:
            text_parts.append(title_text)
            used_tokens += title_tokens
    
    # 2. Add key context (if space allows)
    olp = node_data.get('olp') or node_data.get(':olp', [])
    if olp and len(olp) > 0 and used_tokens < max_total_tokens * 0.5:  # Only add when token usage < 50%
        context = " > ".join(olp[-2:])  # Only take the last 2 levels of parent titles
        context_text = f"Context: {context}"
        context_tokens = _estimate_tokens(context_text)
        if used_tokens + context_tokens < max_total_tokens * 0.7:  # Context not exceeding 70% of total
            text_parts.append(context_text)
            used_tokens += context_tokens
    
    # 3. Add tags (if space allows and tag count is reasonable)
    tags = node_data.get('tags') or node_data.get(':tags', [])
    if tags and len(tags) <= 5 and used_tokens < max_total_tokens * 0.6:
        tags_text = f"Tags: {', '.join(tags[:5])}"  # Maximum 5 tags
        tags_tokens = _estimate_tokens(tags_text)
        if used_tokens + tags_tokens < max_total_tokens * 0.8:  # Tags not exceeding 80% of total
            text_parts.append(tags_text)
            used_tokens += tags_tokens
    
    # 4. Add content with remaining space
    content = (node_data.get('content') or node_data.get(':content') or '').strip()
    if content:
        remaining_tokens = max_total_tokens - used_tokens - 20  # Reserve 20 token safety boundary
        if remaining_tokens > 50:  # Need at least 50 tokens to add content
            # Dynamically calculate content character limit based on remaining tokens
            max_content_chars = int(remaining_tokens / 1.2)  # Conservative estimate
            truncated_content = _smart_truncate(content, max_content_chars)
            content_text = f"Content: {truncated_content}"
            text_parts.append(content_text)
    
    final_text = " | ".join(text_parts)
    
    # Final check: ensure not exceeding limit
    final_tokens = _estimate_tokens(final_text)
    if final_tokens > max_total_tokens:
        # If still exceeding, prioritize title and small amount of content
        title_only = (node_data.get('title') or node_data.get(':title') or node_data.get('name') or '').strip()
        content = (node_data.get('content') or node_data.get(':content') or '').strip()
        if content:
            remaining_chars = max_total_tokens * 0.6  # About 60% for content
            truncated = _smart_truncate(content, int(remaining_chars))
            final_text = f"Title: {title_only} | Content: {truncated}"
        else:
            final_text = f"Title: {title_only}"
    
    return final_text

def _estimate_tokens(text: str) -> float:
    """
    Practical token count estimation
    
    Optimized based on actual testing, balancing accuracy and safety
    
    Args:
        text: Input text
        
    Returns:
        float: Estimated token count
    """
    # Adjusted based on actual observations: Chinese characters ~1.2 tokens, English words ~0.8 tokens
    chinese_chars = len([c for c in text if '\u4e00' <= c <= '\u9fff'])
    other_chars = len(text) - chinese_chars
    
    # More realistic estimation
    estimated = chinese_chars * 1.5 + other_chars * 0.8
    
    # Moderate safety boundary (10%)
    return estimated * 1.1

def _smart_truncate(text: str, max_chars: int) -> str:
    """
    Intelligently truncate text at sentence boundaries
    
    Args:
        text: Original text
        max_chars: Maximum character count
        
    Returns:
        str: Truncated text
    """
    if len(text) <= max_chars:
        return text
    
    # Look forward from maximum length to find sentence boundaries
    truncated = text[:max_chars]
    
    # Look for sentence ending markers like periods, exclamation marks, question marks
    sentence_endings = ['。', '！', '？', '.', '!', '?', '\n\n']
    
    best_cut = 0
    for ending in sentence_endings:
        pos = truncated.rfind(ending)
        if pos > max_chars * 0.7:  # Ensure truncation position is not too far forward
            best_cut = max(best_cut, pos + 1)
    
    if best_cut > 0:
        return text[:best_cut].strip()
    else:
        # If no suitable sentence boundary found, truncate at word boundary
        return text[:max_chars]

def generate_semantic_id(node_data: Dict[str, Any]) -> str:
    """
    Generate semantic identifier for node, facilitating RAG retrieval
    
    Format: uuid:title_summary or uuid:content_summary
    This maintains uniqueness while providing semantic information
    
    Args:
        node_data: Node data dictionary
        
    Returns:
        str: Semantic identifier
    """
    # Get original UUID
    node_id = node_data.get('node_id') or node_data.get('id', '')
    
    # Get summary of title or content
    title = (node_data.get('title') or node_data.get(':title') or node_data.get('name') or '').strip()
    content = (node_data.get('content') or node_data.get(':content') or '').strip()
    
    # Generate summary text
    summary = ""
    if title:
        # Use title as summary (length limited)
        summary = _smart_truncate(title, 50)
    elif content:
        # If no title, use first 50 characters of content as summary
        summary = _smart_truncate(content, 50)
    
    # Clean special characters from summary to ensure it can be used as identifier
    if summary:
        summary = summary.replace('\n', ' ').replace('\r', ' ').replace('|', '-')
        summary = ''.join(c for c in summary if c.isprintable()).strip()
    
    # Generate composite identifier
    if summary:
        return f"{node_id}:{summary}"
    else:
        return node_id

def extract_uuid_from_semantic_id(semantic_id: str) -> str:
    """
    Extract original UUID from semantic identifier
    
    Args:
        semantic_id: Semantic identifier
        
    Returns:
        str: Original UUID
    """
    if ':' in semantic_id:
        return semantic_id.split(':', 1)[0]
    return semantic_id

def extract_summary_from_semantic_id(semantic_id: str) -> str:
    """
    Extract summary part from semantic identifier
    
    Args:
        semantic_id: Semantic identifier
        
    Returns:
        str: Summary part
    """
    if ':' in semantic_id:
        return semantic_id.split(':', 1)[1]
    return ""
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from hashlib import md5

def compute_node_hash(node_id: str, text: str) -> str:
    """Compute stable hash value for a node.
    
    Args:
        node_id: Node ID
        text: Node content
        
    Returns:
        str: Stable hash value
    """
    content = f"{node_id}:{text}"
    return f"md5_{md5(content.encode()).hexdigest()}"

def compute_content_hash(text: str) -> str:
    """Compute stable hash value for content text.
    
    Args:
        text: Content text
        
    Returns:
        str: Stable hash value
    """
    return f"md5_{md5(text.encode()).hexdigest()}" 
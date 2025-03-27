"""
SimTag 序列化工具模块
提供数据的序列化和反序列化功能
"""

import json
import numpy as np
from typing import Any, Dict, List, Union
from datetime import datetime

def to_serializable(obj: Any) -> Any:
    """将对象转换为可序列化的格式
    
    Args:
        obj: 需要序列化的对象
        
    Returns:
        可序列化的对象
    """
    if isinstance(obj, dict):
        return {k: to_serializable(v) for k, v in obj.items()}
    elif isinstance(obj, list):
        return [to_serializable(v) for v in obj]
    elif isinstance(obj, tuple):
        return tuple(to_serializable(v) for v in obj)
    elif isinstance(obj, np.ndarray):
        return obj.tolist()
    elif isinstance(obj, (np.int32, np.int64)):
        return int(obj)
    elif isinstance(obj, (np.float32, np.float64)):
        return float(obj)
    elif isinstance(obj, datetime):
        return obj.isoformat()
    return obj

def serialize_to_json(obj: Any) -> str:
    """将对象序列化为JSON字符串
    
    Args:
        obj: 需要序列化的对象
        
    Returns:
        JSON字符串
    """
    return json.dumps(to_serializable(obj), ensure_ascii=False, indent=2)

def save_to_json_file(obj: Any, file_path: str) -> None:
    """将对象保存为JSON文件
    
    Args:
        obj: 需要序列化的对象
        file_path: JSON文件路径
    """
    with open(file_path, 'w', encoding='utf-8') as f:
        json.dump(to_serializable(obj), f, ensure_ascii=False, indent=2)

def load_from_json_file(file_path: str) -> Any:
    """从JSON文件加载对象
    
    Args:
        file_path: JSON文件路径
        
    Returns:
        加载的对象
    """
    with open(file_path, 'r', encoding='utf-8') as f:
        return json.load(f)

def normalize_response(data: Any, status: str = "success", message: str = None) -> Dict[str, Any]:
    """生成标准化的响应格式
    
    Args:
        data: 响应数据
        status: 响应状态，"success"或"error"
        message: 状态消息，通常用于错误说明
        
    Returns:
        标准化的响应字典
    """
    response = {
        "status": status,
        "result": to_serializable(data)
    }
    
    if message:
        response["message"] = message
        
    return response 
"""
SimTag Serialization Utility Module
Provides data serialization and deserialization functionality
"""

import json
import numpy as np
from typing import Any, Dict
from datetime import datetime

def to_serializable(obj: Any) -> Any:
    """Converts an object to a serializable format
    
    Args:
        obj: The object to be serialized
        
    Returns:
        The serialized object
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
    """Serializes an object to a JSON string
    
    Args:
        obj: The object to be serialized
        
    Returns:
        The JSON string
    """
    return json.dumps(to_serializable(obj), ensure_ascii=False, indent=2)

def save_to_json_file(obj: Any, file_path: str) -> None:
    """Saves an object to a JSON file
    
    Args:
        obj: The object to be serialized
        file_path: The path of the JSON file
    """
    with open(file_path, 'w', encoding='utf-8') as f:
        json.dump(to_serializable(obj), f, ensure_ascii=False, indent=2)

def load_from_json_file(file_path: str) -> Any:
    """Loads an object from a JSON file
    
    Args:
        file_path: The path of the JSON file
        
    Returns:
        The loaded object
    """
    with open(file_path, 'r', encoding='utf-8') as f:
        return json.load(f)

def normalize_response(data: Any, status: str = "success", message: str = None) -> Dict[str, Any]:
    """Generates a standardized response format
    
    Args:
        data: The response data
        status: The response status, "success" or "error"
        message: The status message, usually used for error explanation
        
    Returns:
        The standardized response dictionary
    """
    response = {
        "status": status,
        "result": to_serializable(data)
    }
    
    if message:
        response["message"] = message
        
    return response 
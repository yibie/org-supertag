#!/usr/bin/env python3
"""
统一标签处理器
===============

本模块负责统一所有 Elisp <-> Python 数据传输的格式和转换逻辑。
所有跨语言数据交互都必须遵循此文件中定义的约定。

数据传输约定 (Elisp -> Python)
---------------------------------
为了确保跨语言通信的稳定性和一致性，所有从 Elisp 发送到 Python 的数据
都必须遵循以下统一格式：

1.  **Elisp 端**:
    - 所有数据必须被构建成一个 **关联列表 (alist)**，例如 
      `'(("nodes" . nodes-list) ("config" . config-list))`。
      这是最可靠的序列化格式，必须取代 hash-table。
    - 在调用 `org-supertag-bridge` 的任何函数时，此 alist 必须被包裹在一个
      **列表中**，例如 `(list payload-alist)`。

2.  **Python 端**:
    - `normalize_payload` 函数是所有 Python 端 EPC 方法处理数据的
      **唯一入口点**。
    - 它期望接收一个列表 `[...]`，并智能地处理两种情况：
      a.  理想情况：列表只包含一个元素 `[alist]`。
      b.  兼容情况：由于EPC行为，列表可能被解包，直接收到裸的 `alist`。
    - 无论哪种情况，它都会将 alist 可靠地转换为一个 Python **字典 (dict)**。

此约定避免了 EPC 桥接层对不同数据类型进行不可预测的序列化，
确保了数据格式的绝对统一。

数据传输约定 (Python -> Elisp)
---------------------------------
从 Python 返回数据到 Elisp 时，也应遵循统一格式：

1.  **Python 端**:
    - 所有函数返回值都应是一个标准的 Python **字典 (dict)**。

2.  **Elisp 端**:
    - EPC 桥接层会将 Python 字典自动反序列化为一个 **属性列表 (plist)**。
    - 所有 Elisp 回调函数都应期望接收一个 plist，并使用 `(plist-get result :key)`
      来安全地访问数据。
"""

import json
import logging
import re
from typing import List, Dict, Any, Optional, Union
from dataclasses import dataclass, field
from sexpdata import Symbol 

logger = logging.getLogger(__name__)

# ====== 标准数据格式定义 ======

@dataclass
class TagResult:
    """标准标签结果格式"""
    tag_name: str
    confidence: float  # 0.0-1.0
    reasoning: str
    source: str = "llm"  # "llm" | "preprocessor" | "manual"

@dataclass
class NoteResult:
    """单个笔记的处理结果"""
    note_id: str
    tags: List[TagResult]
    error: Optional[str] = None # To store any processing errors

@dataclass
class BatchResult:
    """批量处理的完整结果"""
    notes: List[NoteResult]
    total_time: float
    stats: Dict[str, Any]

@dataclass
class TagGovernanceData:
    """标签治理数据格式"""
    tag_id: str
    tag_name: str
    tag_status: Optional[str] = None
    tag_rules: Dict[str, Any] = field(default_factory=dict)
    tag_history: List[Dict[str, Any]] = field(default_factory=list)

@dataclass
class TagRelationData:
    """标签关系数据格式"""
    from_tag: str
    to_tag: str
    rel_type: Optional[str] = None
    rel_rules: Dict[str, Any] = field(default_factory=dict)
    rel_history: List[Dict[str, Any]] = field(default_factory=list)

# ====== 最终的、健壮的数据转换逻辑 (V7) ======

def _is_alist(data: list) -> bool:
    """
    Deterministically checks if a list has the structure of an Elisp alist.
    An alist is a list of lists/tuples.
    """
    if not data:
        return True
    return all(isinstance(item, (list, tuple)) for item in data)

def _parse_elisp_data(data: Any) -> Any:
    """
    Recursively and robustly parses Elisp data structures into Python equivalents.
    """
    if not isinstance(data, (list, tuple)):
        if isinstance(data, Symbol):
            val = data.value()
            if val == 't': return True
            if val == 'nil': return None
            # Handle keywords by stripping the leading ':'
            if val.startswith(':'):
                return val[1:]
            return val
        return data

    if not data:
        return []

    # Check if it's an alist
    if _is_alist(data):
        # This check is to differentiate a list of alists (like nodes) from a single alist
        if any(isinstance(item[0], (list, tuple)) for item in data if item):
             return [_parse_elisp_data(item) for item in data]
        
        result_dict = {}
        for item in data:
            if not item: continue
            
            key = _parse_elisp_data(item[0])
            
            # Handle dotted pair `(key . val)`
            if len(item) == 3 and isinstance(item[1], Symbol) and item[1].value() == '.':
                value = _parse_elisp_data(item[2])
            else:
                value_parts = item[1:]
                value = _parse_elisp_data(value_parts[0]) if len(value_parts) == 1 else _parse_elisp_data(value_parts)
            
            result_dict[key] = value
        return result_dict

    # Otherwise, it's a plain list
    return [_parse_elisp_data(item) for item in data]

def parse_llm_json_response(response_text: str) -> Optional[Union[Dict, List]]:
    """
    Robustly parses a JSON response from an LLM, handling potential markdown fences
    and other text noise by extracting the first valid JSON object or array.

    Args:
        response_text: The raw string output from the LLM.

    Returns:
        A parsed Python dictionary or list if successful, otherwise None.
    """
    if not response_text or not isinstance(response_text, str):
        return None

    try:
        # Regex to find a JSON object {...} or array [...]
        # It's important to try to find the most specific match first.
        # A common failure is a JSON object wrapped in ```json ... ```
        match = re.search(r'```json\s*(\{.*\}|\[.*\])\s*```', response_text, re.DOTALL)
        if match:
            json_str = match.group(1)
        else:
            # If not in a marked code block, find the first JSON object or array
            match = re.search(r'(\{.*\}|\[.*\])', response_text, re.DOTALL)
            if not match:
                logger.error(f"No JSON object or array found in LLM response: {response_text}")
                return None
            json_str = match.group(0)

        return json.loads(json_str)

    except json.JSONDecodeError:
        logger.error(f"Failed to decode extracted LLM JSON response: {json_str}")
        return None
    except Exception as e:
        logger.error(f"An unexpected error occurred while parsing LLM response: {e}")
        return None


def normalize_payload(payload: Any) -> Dict:
    """
    The single, robust entry point for normalizing any payload from Elisp.
    """
    logger.debug(f"normalize_payload received raw payload of type: {type(payload)}")

    elisp_data = payload
    if isinstance(payload, (list, tuple)) and len(payload) == 1:
        elisp_data = payload[0]

    parsed_data = _parse_elisp_data(elisp_data)

    if isinstance(parsed_data, dict):
        return parsed_data
    
    # NEW: If the result is a list with a single dictionary, unwrap it.
    # This handles the case where a single alist is parsed into [{}].
    if isinstance(parsed_data, list) and len(parsed_data) == 1 and isinstance(parsed_data[0], dict):
        logger.debug("Unwrapping single dictionary from list.")
        return parsed_data[0]
    
    logger.warning(f"Parsed data is not a dict ({type(parsed_data)}), wrapping.")
    return {'data': parsed_data}

# ====== 旧的、复杂的数据转换逻辑 (将被删除) ======
# All old functions like _convert_simple_sexp, _convert_sexp_to_dict, 
# normalize_autotag_payload etc. are now obsolete and removed.

class UnifiedTagProcessor:
    """
    统一的标签处理器 - 标准化LLM响应的解析。
    """
    
    def clean_llm_response(self, response_str: str) -> str:
        """
        Cleans the LLM response string to extract only the valid JSON part.
        It finds the first occurrence of a JSON array or object.
        """
        # Regex to find a JSON object {...} or array [...]
        json_match = re.search(r'(\{.*\}|\[.*\])', response_str, re.DOTALL)
        
        if json_match:
            json_str = json_match.group(0)
            logger.debug(f"Extracted JSON string from LLM response: {json_str[:300]}...")
            return json_str
        else:
            logger.warning("No JSON object or array found in the LLM response.")
            # Fallback: try to remove markdown backticks as a last resort
            cleaned = response_str.strip()
            if cleaned.startswith('```json'):
                cleaned = cleaned[7:].strip()
            elif cleaned.startswith('```'):
                cleaned = cleaned[3:].strip()
            if cleaned.endswith('```'):
                cleaned = cleaned[:-3].strip()
            return cleaned

    def process_llm_response(self, response_str: str, note_ids: List[str]) -> List[NoteResult]:
        """
        处理 LLM 响应并返回标准化的 NoteResult 列表。
        
        Args:
            response_str: LLM 的原始响应字符串
            note_ids: 对应的笔记ID列表
            
        Returns:
            List[NoteResult]: 处理结果列表
        """
        import json
        
        results = []
        
        try:
            # Clean the response to get only the JSON part
            json_string = self.clean_llm_response(response_str)
            
            try:
                # Attempt to parse the cleaned JSON string
                parsed_data = json.loads(json_string)
            except json.JSONDecodeError as e:
                logger.error(f"Failed to parse extracted JSON response: {e}. JSON string was: '{json_string}'")
                # Create error results for all note_ids
                return [NoteResult(note_id=note_id, tags=[], error=f"JSON parse error: {e}") 
                        for note_id in note_ids]
            
            # Process the parsed data
            if isinstance(parsed_data, list):
                # Response is a list of tags
                tags = self._parse_tag_list(parsed_data)
                note_id = note_ids[0] if note_ids else "unknown"
                results.append(NoteResult(note_id=note_id, tags=tags))
                
            elif isinstance(parsed_data, dict):
                # Response is a dictionary, might contain tags
                if 'tags' in parsed_data and isinstance(parsed_data['tags'], list):
                    tags = self._parse_tag_list(parsed_data['tags'])
                else:
                    # Try to parse the whole dict as a single tag
                    tags = self._parse_tag_list([parsed_data])
                
                note_id = note_ids[0] if note_ids else "unknown"
                results.append(NoteResult(note_id=note_id, tags=tags))
            else:
                logger.warning(f"Unexpected JSON format after parsing: {type(parsed_data)}")
                return [NoteResult(note_id=note_id, tags=[], error="Unexpected JSON format") 
                        for note_id in note_ids]
                
        except Exception as e:
            logger.error(f"Error processing LLM response: {e}", exc_info=True)
            return [NoteResult(note_id=note_id, tags=[], error=str(e)) for note_id in note_ids]
        
        # 如果结果数量不匹配 note_ids 数量，补齐空结果
        while len(results) < len(note_ids):
            results.append(NoteResult(note_id=note_ids[len(results)], tags=[]))
            
        return results
    
    def _parse_tag_list(self, tag_data: List[Dict[str, Any]]) -> List[TagResult]:
        """
        解析标签数据列表为 TagResult 对象列表。
        
        Args:
            tag_data: 标签数据列表，每个元素是包含标签信息的字典
            
        Returns:
            List[TagResult]: 解析后的标签结果列表
        """
        tags = []
        
        if not isinstance(tag_data, list):
            logger.warning(f"Expected list for tag_data, got {type(tag_data)}")
            return tags
        
        for item in tag_data:
            try:
                if isinstance(item, dict):
                    # 标准格式：{"tag_name": "...", "confidence": ..., "reasoning": "..."}
                    tag_name = item.get('tag_name', item.get('name', ''))
                    confidence = float(item.get('confidence', 0.0))
                    reasoning = item.get('reasoning', item.get('reason', ''))
                    
                    if tag_name:
                        tag_result = TagResult(
                            tag_name=tag_name,
                            confidence=confidence,
                            reasoning=reasoning,
                            source="llm"
                        )
                        tags.append(tag_result)
                    else:
                        logger.warning(f"Tag item missing name: {item}")
                        
                elif isinstance(item, str):
                    # 简单字符串格式
                    tag_result = TagResult(
                        tag_name=item,
                        confidence=0.5,  # 默认置信度
                        reasoning="Extracted as simple string",
                        source="llm"
                    )
                    tags.append(tag_result)
                else:
                    logger.warning(f"Unexpected tag item format: {type(item)}")
                    
            except Exception as e:
                logger.error(f"Error parsing tag item {item}: {e}")
                continue
        
        return tags
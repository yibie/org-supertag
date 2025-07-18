#!/usr/bin/env python3
"""
Unify data Processer
===============

Data Contract Specification
==========================================

### 1. Elisp 端数据准备规范

Elisp 端必须按照以下规范准备数据，确保数据结构符合 sync_handler.py 的期望：

#### 1.1 实体 (Entity) 数据格式
```elisp
;; 每个实体必须包含以下字段：
'((id . "entity-id")           ; 必需：实体唯一标识
  (type . "node")              ; 必需：实体类型 ("node" | "tag")
  (title . "实体标题")         ; 可选：实体标题
  (content . "实体内容")       ; 可选：实体内容
  (properties . properties-alist) ; 可选：属性列表
  (file_path . "/path/to/file")    ; 可选：文件路径
  (pos . 123)                      ; 可选：位置信息
  ;; ... 其他字段
  )
```

#### 1.2 快照 (Snapshot) 数据格式
```elisp
;; 完整的同步快照数据：
'((entities . (entity1 entity2 ...))     ; 要更新的实体列表
  (links . (link1 link2 ...))           ; 要更新的链接列表
  (ids_to_delete . ("id1" "id2" ...))   ; 要删除的实体ID列表
  )
```

#### 1.3 链接 (Link) 数据格式
```elisp
;; 链接数据：
'((source . "source-id")       ; 必需：源实体ID
  (target . "target-id")       ; 必需：目标实体ID
  (type . "REF_TO")           ; 可选：链接类型
  ;; ... 其他属性
  )
```

### 2. Python 端处理规范

#### 2.1 normalize_payload 函数职责
- 接收 Elisp 传输的数据 (通过 EPC 协议)
- 将 Elisp alist 转换为 Python dict
- 处理 Symbol 类型和特殊值 (t, nil)
- 递归处理嵌套结构

#### 2.2 期望的输出格式
```python
{
    "entities": [
        {
            "id": "entity-id",
            "type": "node",
            "title": "实体标题",
            "content": "实体内容",
            "properties": {...},
            # ... 其他字段
        },
        # ... 更多实体
    ],
    "links": [
        {
            "source": "source-id",
            "target": "target-id",
            "type": "REF_TO",
            # ... 其他属性
        },
        # ... 更多链接
    ],
    "ids_to_delete": ["id1", "id2", ...]
}
```

### 3. 数据验证规则

#### 3.1 必需字段验证
- 实体必须有 'id' 和 'type' 字段
- 链接必须有 'source' 和 'target' 字段

#### 3.2 数据类型验证
- ID 字段必须是字符串
- type 字段必须是 "node" 或 "tag"
- properties 必须是字典或能转换为字典的结构

数据传输约定 (Elisp -> Python)
---------------------------------
为了确保跨语言通信的稳定性和一致性，所有从 Elisp 发送到 Python 的数据
都必须遵循以下统一格式：

1.  **Elisp 端**:
    - 所有数据必须被构建成一个 **关联列表 (alist)**，例如 
      `'(("entities" . entities-list) ("links" . links-list))`。
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


def validate_entity_data(entity: Dict[str, Any]) -> bool:
    """
    Validate entity data
    
    Args:
        entity: Entity data dictionary
        
    Returns:
        bool: Validation passed
    """
    # Required fields check
    if not entity.get('id'):
        logger.error(f"Entity missing required 'id' field: {entity}")
        return False
    
    if not entity.get('type'):
        logger.error(f"Entity missing required 'type' field: {entity}")
        return False
    
    # Type validation
    if entity.get('type') not in ['node', 'tag']:
        logger.error(f"Invalid entity type '{entity.get('type')}', must be 'node' or 'tag': {entity}")
        return False
    
    # ID type check
    if not isinstance(entity.get('id'), str):
        logger.error(f"Entity 'id' must be a string, got {type(entity.get('id'))}: {entity}")
        return False
    
    return True

def validate_link_data(link: Dict[str, Any]) -> bool:
    """
    Validate link data
    
    Args:
        link: Link data dictionary
        
    Returns:
        bool: Validation passed
    """
    # Required fields check
    if not link.get('source'):
        logger.error(f"Link missing required 'source' field: {link}")
        return False
    
    if not link.get('target'):
        logger.error(f"Link missing required 'target' field: {link}")
        return False
    
    # Type check
    if not isinstance(link.get('source'), str):
        logger.error(f"Link 'source' must be a string, got {type(link.get('source'))}: {link}")
        return False
    
    if not isinstance(link.get('target'), str):
        logger.error(f"Link 'target' must be a string, got {type(link.get('target'))}: {link}")
        return False
    
    return True

def validate_snapshot_data(data: Dict[str, Any]) -> Dict[str, Any]:
    """
    Validate and clean snapshot data.
    
    Args:
        data: Snapshot data dictionary
        
    Returns:
        Dict: Validated data, invalid data will be filtered out
    """
    validated_data = {
        'entities': [],
        'links': [],
        'ids_to_delete': []
    }
    
    # Validate entity data
    entities = data.get('entities', [])
    if not isinstance(entities, list):
        logger.error(f"'entities' must be a list, got {type(entities)}")
        entities = []
    
    for entity in entities:
        if validate_entity_data(entity):
            validated_data['entities'].append(entity)
    
    # Validate link data
    links = data.get('links', [])
    if not isinstance(links, list):
        logger.error(f"'links' must be a list, got {type(links)}")
        links = []
    
    for link in links:
        if validate_link_data(link):
            validated_data['links'].append(link)
    
    # Validate delete ID list
    ids_to_delete = data.get('ids_to_delete', [])
    if not isinstance(ids_to_delete, list):
        logger.error(f"'ids_to_delete' must be a list, got {type(ids_to_delete)}")
        ids_to_delete = []
    
    valid_ids = []
    for id_val in ids_to_delete:
        if isinstance(id_val, str) and id_val.strip():
            valid_ids.append(id_val.strip())
        else:
            logger.warning(f"Invalid ID in ids_to_delete: {id_val}")
    
    validated_data['ids_to_delete'] = valid_ids
    
    logger.info(f"Validation completed: {len(validated_data['entities'])} entities, "
                f"{len(validated_data['links'])} links, {len(validated_data['ids_to_delete'])} deletions")
    
    return validated_data

def normalize_payload(payload: Any) -> Dict:
    """
    The single, robust entry point for normalizing any payload from Elisp.
    It recursively unwraps tuples and lists until it finds the first
    dictionary-like structure (an alist), then converts it to a dict.
    
    Enhanced with data validation and error handling.
    """
    logger.debug(f"normalize_payload received raw payload of type: {type(payload)}")

    current_data = payload
    # Elisp often wraps data in a single-element list or tuple
    while isinstance(current_data, (list, tuple)) and len(current_data) == 1:
        current_data = current_data[0]
        logger.debug(f"Unwrapped payload, current type: {type(current_data)}")

    # After unwrapping, we should have the core alist (as a list/tuple of lists/tuples)
    if isinstance(current_data, (list, tuple)):
        parsed_dict = _parse_elisp_data(current_data)
        if isinstance(parsed_dict, dict):
            logger.debug(f"Successfully parsed payload to dict: {list(parsed_dict.keys())}")
            # Validate and clean data
            validated_data = validate_snapshot_data(parsed_dict)
            logger.debug("Data validation completed successfully")
            # === Preserve additional top-level keys that are not part of the standard snapshot schema ===
            for k, v in parsed_dict.items():
                if k not in validated_data:
                    validated_data[k] = v
            return validated_data
        else:
            logger.error(f"Parsed data is not a dict, but {type(parsed_dict)}. Returning empty dict.")
            return {'entities': [], 'links': [], 'ids_to_delete': []}
    
    if isinstance(current_data, dict):
        # Validate and clean data
        validated_data = validate_snapshot_data(current_data)
        # === Preserve additional top-level keys that are not part of the standard snapshot schema ===
        for k, v in current_data.items():
            if k not in validated_data:
                validated_data[k] = v
        return validated_data

    logger.error(f"Could not normalize payload. Final type was {type(current_data)}. Returning empty dict.")
    return {'entities': [], 'links': [], 'ids_to_delete': []}

# ====== UnifiedTagProcessor (restored) ======
class UnifiedTagProcessor:
    """Provides helper methods to clean and parse LLM tag-extraction responses into
    structured TagResult / NoteResult objects. This class was accidentally
    removed; restoring it ensures downstream imports function correctly."""

    # ------------------------------------------------------------
    # Public helpers
    # ------------------------------------------------------------
    def clean_llm_response(self, response_str: str) -> str:
        """Extract the first valid JSON object/array from the LLM response."""
        json_match = re.search(r'(\{.*\}|\[.*\])', response_str, re.DOTALL)
        if json_match:
            return json_match.group(0)

        # Fallback – strip markdown fences
        cleaned = response_str.strip()
        if cleaned.startswith('```json'):
            cleaned = cleaned[7:].strip()
        elif cleaned.startswith('```'):
            cleaned = cleaned[3:].strip()
        if cleaned.endswith('```'):
            cleaned = cleaned[:-3].strip()
        return cleaned

    def process_llm_response(self, response_str: str, note_ids: List[str]) -> List[NoteResult]:
        """Parse LLM output string and return a list of NoteResult objects."""
        results: List[NoteResult] = []
        try:
            json_string = self.clean_llm_response(response_str)
            parsed = json.loads(json_string)
        except Exception as e:
            logger.error(f"Failed to parse LLM response: {e}")
            return [NoteResult(note_id=nid, tags=[], error=str(e)) for nid in note_ids]

        # Helper to convert raw list/dict to TagResult list
        def _to_tag_results(raw) -> List[TagResult]:
            tag_list: List[TagResult] = []
            if not isinstance(raw, list):
                raw = [raw]
            for item in raw:
                try:
                    if isinstance(item, dict):
                        tag_name = item.get('tag_name') or item.get('name') or ''
                        conf = float(item.get('confidence', 0.0))
                        reasoning = item.get('reasoning', item.get('reason', ''))
                    else:
                        tag_name = str(item)
                        conf = 0.5
                        reasoning = 'Extracted as simple string'
                    if tag_name:
                        tag_list.append(TagResult(tag_name=tag_name, confidence=conf, reasoning=reasoning, source='llm'))
                except Exception as ex:
                    logger.warning(f"Failed to parse tag item {item}: {ex}")
            return tag_list

        try:
            if isinstance(parsed, list):
                tags = _to_tag_results(parsed)
                results.append(NoteResult(note_id=note_ids[0] if note_ids else 'unknown', tags=tags))
            elif isinstance(parsed, dict):
                raw_tags = parsed.get('tags', parsed)
                tags = _to_tag_results(raw_tags)
                results.append(NoteResult(note_id=note_ids[0] if note_ids else 'unknown', tags=tags))
            else:
                logger.warning(f"Unexpected JSON root type: {type(parsed)}")
        except Exception as err:
            logger.error(f"Error constructing NoteResult: {err}")

        # Ensure one result per note_id
        while len(results) < len(note_ids):
            results.append(NoteResult(note_id=note_ids[len(results)], tags=[]))
        return results
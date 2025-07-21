#!/usr/bin/env python3
"""
Unify data Processer
===============

Data Contract Specification
==========================================

1. Elisp Side Data Preparation Specification

Elisp Side must prepare data according to the following specification to ensure the data structure matches the expectations of sync_handler.py:

1.1 Entity Data Format
```elisp
;; Each entity must contain the following fields:
'((id . "entity-id")           ; Required: Entity unique identifier
  (type . "node")              ; Required: Entity type ("node" | "tag")
  (title . "entity title")     ; Optional: Entity title
  (content . "entity content") ; Optional: Entity content
  (properties . properties-alist) ; Optional: Properties list
  (file_path . "/path/to/file")    ; Optional: File path
  (pos . 123)                      ; Optional: Position information
  ;; ... other fields
  )
```

1.2 Snapshot Data Format
```elisp
;; Complete snapshot data:
'((entities . (entity1 entity2 ...))     ; List of entities to update
  (links . (link1 link2 ...))           ; List of links to update
  (ids_to_delete . ("id1" "id2" ...))   ; List of entity IDs to delete
  )
```

1.3 Link Data Format
```elisp
;; Link data:
'((source . "source-id")       ; Required: Source entity ID
  (target . "target-id")       ; Required: Target entity ID
  (type . "REF_TO")           ; Optional: Link type
  ;; ... other fields
  )
```

2. Python Side Processing Specification

2.1 normalize_payload Function Responsibilities
- Receive data from Elisp (via EPC protocol)
- Convert Elisp alist to Python dict
- Handle Symbol type and special values (t, nil)
- Recursively process nested structures

2.2 Expected Output Format
```python
{
    "entities": [
        {
            "id": "entity-id",
            "type": "node",
            "title": "entity title",
            "content": "entity content",
            "properties": {...},
            # ... other fields
        },
        # ... more entities
    ],
    "links": [
        {
            "source": "source-id",
            "target": "target-id",
            "type": "REF_TO",
            # ... other fields
        },
        # ... more links
    ],
    "ids_to_delete": ["id1", "id2", ...]
}
```

3. Data Validation Rules

3.1 Required Field Validation
- Entity must have 'id' and 'type' fields
- Link must have 'source' and 'target' fields

3.2 Data Type Validation
- ID field must be a string
- type field must be "node" or "tag"
- properties must be a dictionary or a structure that can be converted to a dictionary

Data Transmission Agreement (Elisp -> Python)
---------------------------------
To ensure stability and consistency of cross-language communication, all data sent from Elisp to Python
must follow the following unified format:

1.  **Elisp Side**:
    - All data must be constructed as an **alist**, e.g.
      `'(("entities" . entities-list) ("links" . links-list))`.
      This is the most reliable serialization format, and must replace hash-table.
    - When calling any function of `org-supertag-bridge`, this alist must be wrapped in a
      **list**, e.g. `(list payload-alist)`.

2.  **Python Side**:
    - `normalize_payload` function is the **only entry point** for all Python EPC methods to process data.
    - It expects a list `[...]`, and intelligently handles two cases:
      a.  Ideal case: The list only contains one element `[alist]`.
      b.  Compatible case: Due to EPC behavior, the list may be unpacked, and the raw `alist` may be received directly.
    - In both cases, it will reliably convert the alist to a Python **dictionary (dict)**.

This agreement avoids unpredictable serialization of different data types by the EPC bridge layer,
ensures absolute uniformity of data formats.

Data Transmission Agreement (Python -> Elisp)
---------------------------------
When returning data from Python to Elisp, it should also follow the unified format:

1.  **Python Side**:
    - All function return values should be a standard Python **dictionary (dict)**.

2.  **Elisp Side**:
    - The EPC bridge layer will automatically deserialize the Python dictionary into a **property list (plist)**.
    - All Elisp callback functions should expect to receive a plist, and use `(plist-get result :key)`
      to safely access data.
"""

import json
import logging
import re
from typing import List, Dict, Any, Optional, Union
from dataclasses import dataclass, field
from sexpdata import Symbol 

logger = logging.getLogger(__name__)

# ====== Standard Data Format Definitions ======
@dataclass
class TagResult:
    """Standard Tag Result Format"""
    tag_name: str
    confidence: float  # 0.0-1.0
    reasoning: str
    source: str = "llm"  # "llm" | "preprocessor" | "manual"

@dataclass
class NoteResult:
    """Single Note Processing Result"""
    note_id: str
    tags: List[TagResult]
    error: Optional[str] = None # To store any processing errors

@dataclass
class BatchResult:
    """Complete Batch Processing Result"""
    notes: List[NoteResult]
    total_time: float
    stats: Dict[str, Any]

@dataclass
class TagGovernanceData:
    """Tag Governance Data Format"""
    tag_id: str
    tag_name: str
    tag_status: Optional[str] = None
    tag_rules: Dict[str, Any] = field(default_factory=dict)
    tag_history: List[Dict[str, Any]] = field(default_factory=list)

@dataclass
class TagRelationData:
    """Tag Relation Data Format"""
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
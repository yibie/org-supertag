# SQLite List Type Error 修复总结

## 问题描述
在同步节点数据时出现错误：
```
sqlite3.ProgrammingError: Error binding parameter 10: type 'list' is not supported
```

错误发生在尝试插入节点数据到 SQLite 数据库时，`deadline` 字段（参数 10）接收了一个 list 类型的值。

## 问题原因
Emacs/Elisp 使用内部时间格式，通常是一个包含 3-4 个整数的列表：
- `[high low microsecs]` 或
- `[high low microsecs picosecs]`

当这些时间数据从 Elisp 传递到 Python 时，它们保持为列表格式，而 SQLite 的 TEXT 字段不能直接存储列表。

## 解决方案

### 1. 数据规范化阶段（推荐）
在 `simtag/utils/unified_tag_processor.py` 的 `_parse_elisp_data` 函数中添加了对 Emacs 时间列表的特殊处理：

```python
# Special case: Emacs time list format
if (len(data) in [3, 4] and 
    all(isinstance(x, (int, float)) for x in data) and
    len([x for x in data if isinstance(x, int) and x >= 0]) == len(data)):
    return f"emacs-time:{data}"
```

这会将时间列表转换为字符串格式，例如：`"emacs-time:[26709, 58437, 123456]"`

### 2. 数据库插入阶段（备用）
在 `simtag/core/graph_service.py` 的 `_upsert_nodes_internal` 函数中添加了类型检查和转换：

```python
for col in columns:
    val = node.get(col)
    # Convert list types to string for datetime fields
    if col in ['scheduled', 'deadline'] and isinstance(val, list):
        val = str(val) if val else None
    # Ensure olp is serialized as string if it's a list
    elif col == 'olp' and isinstance(val, list):
        val = json.dumps(val)
    # Ensure properties is serialized as string
    elif col == 'properties' and isinstance(val, dict):
        val = json.dumps(val)
    values.append(val)
```

这确保了即使数据规范化阶段遗漏了某些列表，在插入数据库前也会被转换为字符串。

## 影响的字段
- `deadline`: Org-mode 的 DEADLINE 时间戳
- `scheduled`: Org-mode 的 SCHEDULED 时间戳
- `olp`: Outline path (大纲路径)，通常是一个字符串列表
- `properties`: 属性字典，需要序列化为 JSON 字符串

## 未来改进
可以考虑将 Emacs 时间列表转换为标准的 ISO 8601 时间字符串格式，这样更易于在 Python 端处理和查询。 
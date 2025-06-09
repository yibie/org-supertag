# org-supertag 性能优化指南

## 🚀 NER 速度优化

### 问题诊断

如果您发现 NER（命名实体识别）处理速度较慢，可以使用我们的优化工具：

```bash
python optimize_ner_speed.py
```

### 快速优化方案

#### 1. 切换到更快的模型

在 `simtag/config.py` 中修改：

```python
entity_extractor_config: Dict[str, Any] = field(default_factory=lambda: {
    "llm_model_override": "qwen2.5:0.5b",  # 使用最轻量级模型
    "max_gleaning_rounds": 1,              # 减少精化轮次
    "max_entities_per_extraction": 8,      # 减少实体数量
    "max_relations_per_extraction": 5,     # 减少关系数量
})
```

#### 2. 推荐的快速模型

| 模型 | 大小 | 速度 | 质量 | 适用场景 |
|------|------|------|------|----------|
| `qwen2.5:0.5b` | 0.5B | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | 极速处理 |
| `llama3.2:1b` | 1B | ⭐⭐⭐⭐ | ⭐⭐⭐ | 平衡选择 |
| `gemma2:2b` | 2B | ⭐⭐⭐ | ⭐⭐⭐⭐ | 质量优先 |

#### 3. 优化多核心处理

```python
multicore_config: Dict[str, Any] = field(default_factory=lambda: {
    "max_workers": 3,           # 限制worker数量
    "ner_batch_threshold": 3,   # 降低批处理阈值
    "chunk_size_factor": 4,     # 减少每个worker处理量
})
```

### 一键优化

使用配置类的快速模式：

```python
from simtag.config import Config

config = Config()
config.enable_fast_ner_mode()  # 一键启用快速模式
```

## 🔧 向量维度管理

### 维度不匹配问题

当切换嵌入模型时，可能出现维度不匹配错误：

```
Dimension mismatch for inserted vector for the 'embedding' column. 
Expected 768 dimensions but received 1024
```

### 自动修复工具

使用我们的维度管理工具：

```bash
python dimension_manager.py
```

该工具会：
1. 🔍 自动检测当前模型维度
2. 📊 检查数据库现有维度
3. ⚠️ 识别维度不匹配问题
4. 🔧 提供自动修复方案

### 手动修复步骤

如果需要手动处理：

1. **备份数据库**
   ```bash
   cp ~/.emacs.d/org-supertag/supertag_vector.db ~/.emacs.d/org-supertag/supertag_vector.db.backup
   ```

2. **运行修复脚本**
   ```bash
   python fix_vector_dimension.py
   ```

3. **重新生成嵌入向量**
   ```elisp
   M-x org-supertag-sync-all
   ```

### 预防维度问题

#### 1. 模型切换前检查

在切换嵌入模型前，运行兼容性检查：

```python
from simtag.services.embedding_service import get_embedding_service

service = get_embedding_service()
compatibility = await service.check_dimension_compatibility()
print(compatibility)
```

#### 2. 常见模型维度

| 模型 | 维度 | 说明 |
|------|------|------|
| `nomic-embed-text` | 768 | Ollama 默认嵌入模型 |
| `Qwen3-Embedding-0.6B` | 1024 | llama.cpp 推荐模型 |
| `all-MiniLM-L6-v2` | 384 | 轻量级本地模型 |

## 📊 性能监控

### 系统要求检查

```python
import psutil

print(f"CPU 核心数: {psutil.cpu_count()}")
print(f"内存: {psutil.virtual_memory().total / (1024**3):.1f} GB")
```

### 性能基准

| 系统配置 | NER 处理时间 | 嵌入生成时间 | 推荐配置 |
|----------|--------------|--------------|----------|
| M1/M2 Mac | < 3秒 | < 1秒 | 平衡模式 |
| Intel Mac | < 5秒 | < 2秒 | 快速模式 |
| 低配置机 | < 10秒 | < 5秒 | 极速模式 |

## 🛠️ 故障排除

### 常见问题

#### 1. NER 超时

**症状**: `Entity extraction timeout`

**解决方案**:
```python
entity_extractor_config = {
    "llm_timeout": 30,        # 减少超时时间
    "max_retries": 1,         # 减少重试次数
    "llm_model_override": "qwen2.5:0.5b"  # 切换更快模型
}
```

#### 2. 内存不足

**症状**: `Out of memory` 或系统卡顿

**解决方案**:
```python
multicore_config = {
    "max_workers": 2,         # 减少并行度
    "chunk_size_factor": 2,   # 减少批处理大小
}
```

#### 3. 维度错误

**症状**: `Dimension mismatch`

**解决方案**:
```bash
# 方法1: 自动修复
python dimension_manager.py

# 方法2: 重新创建数据库
rm ~/.emacs.d/org-supertag/supertag_vector.db
M-x org-supertag-sync-all
```

### 日志调试

启用详细日志：

```python
import logging
logging.getLogger("simtag").setLevel(logging.DEBUG)
```

## 📝 最佳实践

### 1. 模型选择策略

- **开发测试**: 使用 `qwen2.5:0.5b` 快速迭代
- **生产环境**: 使用 `qwen2.5:1.5b` 平衡质量和速度
- **高质量需求**: 使用 `gemma2:2b` 或更大模型

### 2. 数据管理

- 定期备份向量数据库
- 模型切换前检查维度兼容性
- 使用增量更新减少重复处理

### 3. 系统优化

- 确保有足够内存运行多个模型
- 使用 SSD 存储提升数据库访问速度
- 定期清理缓存和临时文件

## 🔄 版本更新指南

### 更新步骤

1. **备份现有数据**
   ```bash
   cp -r ~/.emacs.d/org-supertag ~/.emacs.d/org-supertag.backup
   ```

2. **检查兼容性**
   ```bash
   python dimension_manager.py
   ```

3. **更新配置**
   - 查看新的配置选项
   - 应用性能优化设置

4. **重新同步**
   ```elisp
   M-x org-supertag-sync-all
   ```

通过这些优化措施，您可以显著提升 org-supertag 的性能表现！ 
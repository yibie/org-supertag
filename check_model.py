#!/usr/bin/env python3
import os
import sys

# 添加当前目录到 Python 路径
script_dir = os.path.dirname(os.path.realpath(__file__))
if script_dir not in sys.path:
    sys.path.insert(0, script_dir)

from simtag.tag_vectors import TagVectorEngine

# 创建引擎实例
engine = TagVectorEngine()

# 获取模型信息
model = engine.model
print(f"Model name: {engine.model_name}")
print(f"Model structure: {model}")

# 测试向量维度
test_vector = model.encode("test")
print(f"\nVector dimension: {len(test_vector)}")
print(f"First few values: {test_vector[:5]}") 
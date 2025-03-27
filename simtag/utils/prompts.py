"""
提示词管理模块
集中管理所有用于LLM的提示词
"""

# 实体提取相关提示词
ENTITY_SYSTEM_PROMPT = """你是一个命名实体识别专家。请分析给定的文本，识别其中的命名实体。
要求：
1. 识别以下类型的实体：
   - PERSON: 人名
   - ORG: 组织、公司、机构
   - PRODUCT: 产品、软件、技术
   - CONCEPT: 技术概念、方法论
   - TECH: 编程语言、框架、工具
2. 返回格式为 JSON 数组，每个实体包含：
   - text: 实体文本
   - type: 实体类型（必须是上述之一）
   - confidence: 置信度（0-1之间的浮点数）
3. 只返回 JSON 数组，不要有其他解释"""

ENTITY_USER_PROMPT = """分析以下文本中的命名实体：

{text}

只返回 JSON 数组，格式如下：
[
  {{"text": "实体文本", "type": "实体类型", "confidence": 0.95}},
  ...
]"""

# 标签生成相关提示词
TAG_SYSTEM_PROMPT = """You are a professional indexer. Your task is to identify key concepts from text that would be useful for searching and organizing content.

Guidelines:
- Focus on core concepts and themes
- Use simple, clear terms (prefer 1-2 words)
- Format with lowercase and underscores for multi-word concepts
- Prefer shorter terms when possible
- Return only comma-separated tags, no explanations
- Each tag should be meaningful on its own"""

TAG_USER_PROMPT = """Please identify {limit} key concepts from this text:

{text}

Return only comma-separated tags in lowercase, using underscores for multi-word concepts.""" 
"""
SimTag 标签关系分析模块
分析标签之间的语义关系
"""

import logging
from typing import List, Dict, Any, Optional

# 预定义关系类型
RELATIONS = {
    "CONTRAST": "比较或对比关系",
    "RELATE": "一般关联关系",
    "INFLUENCE": "影响关系",
    "CONTAIN": "包含关系(父)",
    "BELONG": "从属关系(子)",
    "PARALLEL": "并行关系",
    "DEPENDENCY": "依赖关系",
    "CAUSE": "因果关系(因)",
    "EFFECT": "因果关系(果)",
    "COOCCURRENCE": "共现关系"
}

class TagRelationAnalyzer:
    """标签关系分析器"""
    
    def __init__(self, ollama_bridge: Any = None):
        """初始化标签关系分析器
        
        Args:
            ollama_bridge: Ollama桥接对象，用于关系分析
        """
        self.logger = logging.getLogger("simtag.tag_relation")
        self.ollama = ollama_bridge
        
    def analyze_relations(self, tag: str, tags: List[str]) -> List[Dict[str, Any]]:
        """分析标签间的关系
        
        Args:
            tag: 目标标签
            tags: 待分析的标签列表
            
        Returns:
            标签关系列表，每个元素包含:
                - tag: 相关标签
                - relation: 关系类型
                - reason: 关系说明
        """
        if not self.ollama:
            self.logger.error("未提供Ollama实例，无法分析标签关系")
            return []
            
        system = """You are a tag relationship analyzer. Your task is to determine the relationship between two tags.

Available relationship types:
CONTRAST   - Tags represent contrasting or comparable concepts
RELATE     - Tags have a general association
INFLUENCE  - First tag has significant impact on second tag
CONTAIN    - First tag is a broader category that includes second tag
BELONG     - First tag is a subset or member of second tag
PARALLEL   - Tags represent similar-level concepts
DEPENDENCY - First tag requires or depends on second tag
CAUSE      - First tag leads to or causes second tag
EFFECT     - First tag is a result of second tag
COOCCURRENCE - Tags commonly appear together

Response format requirements:
1. Use EXACTLY this format (including newline):
   RELATION: <TYPE>
   REASON: <brief explanation>
2. Choose only ONE relationship type from the list above
3. Provide a clear, concise reason (1-2 sentences)
4. Use technical language when appropriate
5. Be specific about the relationship direction

Example response:
RELATION: BELONG
REASON: Python is a specific programming language, making it a subset of programming.

DO NOT include any other text or explanations."""

        results = []
        for related_tag in tags:
            prompt = f"""How is "{related_tag}" related to "{tag}"?

Choose ONE relationship type and explain why.
Use EXACTLY this format (including newline):
RELATION: <TYPE>
REASON: <explanation>"""


            try:
                response = self.ollama.run(prompt, system)
                response = response.strip()
                
                # 验证响应格式
                if not ('\nREASON:' in response and response.startswith('RELATION:')):
                    self.logger.warning(f"标签'{related_tag}'的响应格式无效，重试...")
                    response = self.ollama.run(prompt, system)
                    if not ('\nREASON:' in response and response.startswith('RELATION:')):
                        self.logger.warning(f"重试后仍然格式无效，跳过标签'{related_tag}'")
                        continue
                
                # 解析响应
                lines = response.strip().split('\n')
                relation_line = next(line for line in lines if line.startswith('RELATION:'))
                reason_line = next(line for line in lines if line.startswith('REASON:'))
                
                relation = relation_line.split(':', 1)[1].strip().upper()
                reason = reason_line.split(':', 1)[1].strip()
                
                # 验证关系类型
                if relation not in RELATIONS:
                    self.logger.warning(f"标签'{related_tag}'的关系类型'{relation}'无效")
                    continue
                
                results.append({
                    'tag': related_tag,
                    'relation': relation.lower(),
                    'reason': reason
                })
                
            except Exception as e:
                self.logger.error(f"解析标签'{related_tag}'的响应时出错: {e}")
                self.logger.debug(f"原始响应:\n{response}")
                continue
                
        return results

# 全局单例实例
_analyzer_instance = None

def analyze_tag_relations(tag: str, tags: List[str]) -> List[Dict[str, Any]]:
    """分析标签关系的全局函数，供EPC服务器调用"""
    global _analyzer_instance
    
    if _analyzer_instance is None:
        _analyzer_instance = TagRelationAnalyzer()
        
    return _analyzer_instance.analyze_relations(tag, tags) 
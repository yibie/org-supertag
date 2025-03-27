#!/usr/bin/env python3
"""测试标签生成功能"""

import logging
from simtag.ollama_bridge import OllamaBridge
from simtag.tag_generator import TagGenerator

# 设置日志
logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s [%(levelname)s] %(name)s: %(message)s'
)

def main():
    """主函数"""
    try:
        # 创建 OllamaBridge 实例
        print("创建 OllamaBridge 实例...")
        bridge = OllamaBridge()
        
        # 创建 TagGenerator 实例
        print("创建 TagGenerator 实例...")
        generator = TagGenerator(bridge)
        
        # 测试文本
        test_text = """
        Python是一种流行的编程语言，以其简洁的语法和丰富的生态系统而闻名。
        它广泛应用于Web开发、数据分析、人工智能等领域。
        Python的设计哲学强调代码的可读性，其语法允许程序员用更少的代码行数表达概念。
        """
        
        print("\n测试文本:")
        print(test_text)
        
        # 生成标签
        print("\n生成标签...")
        tags = generator.suggest_tags(test_text)
        
        print("\n生成的标签:")
        for i, tag in enumerate(tags, 1):
            print(f"{i}. {tag}")
            
    except Exception as e:
        print(f"测试失败: {e}")
        return False
        
    return True

if __name__ == "__main__":
    success = main()
    exit(0 if success else 1) 
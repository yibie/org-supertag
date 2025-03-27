"""
测试标签生成功能
"""

import logging
from simtag.ollama_bridge import OllamaBridge
from simtag.tag_generator import TagGenerator

# 设置日志
logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s [%(levelname)s] %(name)s: %(message)s'
)

def test_tag_generation():
    """测试标签生成"""
    # 初始化 Ollama
    print("初始化 Ollama...")
    ollama = OllamaBridge(model="hf.co/unsloth/gemma-3-4b-it-GGUF")
    
    # 初始化标签生成器
    print("初始化标签生成器...")
    generator = TagGenerator(ollama)
    
    # 测试文本
    text = """
    构建 Github 自动发布 Release 工作流
    
    使用 GitHub Actions 自动化发布流程，包括：
    1. 版本号管理
    2. 更新日志生成
    3. Release 创建
    4. 二进制文件打包
    """
    
    print("\n测试文本:\n")
    print(text)
    print("\n生成标签...")
    
    # 生成标签
    tags = generator.suggest_tags(text)
    
    print("\n生成的标签:")
    for tag in tags:
        print(f"- {tag}")

if __name__ == "__main__":
    test_tag_generation() 
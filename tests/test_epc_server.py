"""
最小化的 EPC 服务器测试

按照 python-epc 文档实现：
https://python-epc.readthedocs.io/en/latest/
"""

import os
import sys
import logging

# 添加项目根目录到 Python 路径
project_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, project_root)
logging.info(f"添加项目根目录到 Python 路径: {project_root}")

from epc.server import ThreadingEPCServer
from simtag.ollama_bridge import OllamaBridge
from simtag.tag_generator import TagGenerator

# 设置日志
logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s [%(levelname)s] %(name)s: %(message)s'
)

logger = logging.getLogger(__name__)

class TagGeneratorServer(ThreadingEPCServer):
    """标签生成服务器"""
    
    def __init__(self, address=('localhost', 0)):
        """初始化服务器"""
        super().__init__(address)
        self._generator = None
        
    def init_tag_generator(self):
        """初始化标签生成器"""
        try:
            logger.info("初始化 Ollama Bridge...")
            ollama = OllamaBridge(model="hf.co/unsloth/gemma-3-4b-it-GGUF")
            
            logger.info("初始化标签生成器...")
            self._generator = TagGenerator(ollama)
            return True
        except Exception as e:
            logger.error(f"初始化失败: {e}")
            return False
            
    @property
    def generator(self):
        """获取标签生成器实例"""
        if self._generator is None:
            self.init_tag_generator()
        return self._generator

def main():
    """主函数"""
    # 创建服务器
    server = TagGeneratorServer(('localhost', 0))
    
    # 注册方法
    @server.register_function
    def suggest_tags(text):
        """生成标签建议"""
        logger.info(f"收到文本: {text[:100]}...")
        try:
            if server.generator is None:
                return []
            tags = server.generator.suggest_tags(text)
            logger.info(f"生成标签: {tags}")
            return tags
        except Exception as e:
            logger.error(f"生成标签失败: {e}")
            return []
    
    # 初始化标签生成器
    if not server.init_tag_generator():
        logger.error("标签生成器初始化失败")
        return
    
    # 启动服务器
    logger.info("启动 EPC 服务器...")
    server.print_port()
    server.serve_forever()

if __name__ == "__main__":
    main() 
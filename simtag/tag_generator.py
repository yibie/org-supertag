"""
标签生成器模块
负责从文本内容生成标签建议
"""

import re
import logging
from typing import List, Dict, Any, Optional
import traceback

# 全局单例实例
_generator_instance = None

def suggest_tags(text: str, limit: int = 5) -> List[str]:
    """从文本中提取标签的全局函数，供EPC服务器调用
    
    Args:
        text: 需要分析的文本内容
        limit: 返回的标签数量限制（默认5个）
        
    Returns:
        提取的标签列表
    """
    global _generator_instance
    
    if _generator_instance is None:
        _generator_instance = TagGenerator(None)
    
    # 固定使用5个标签    
    return _generator_instance.suggest_tags(text)  # 不传递 limit 参数

class TagGenerator:
    """标签生成器类"""
    
    def __init__(self, ollama_bridge):
        """初始化标签生成器
        
        Args:
            ollama_bridge: LLM接口对象
        """
        self.logger = logging.getLogger("simtag.tag_generator")
        self.ollama = ollama_bridge
        
    def suggest_tags(self, text: str, limit: int = 5) -> List[str]:
        """生成标签建议"""
        try:
            print(f"[DEBUG] 开始生成标签建议，文本长度: {len(text)}")
            self.logger.info(f"开始生成标签建议，文本长度: {len(text)}")
            
            # 如果文本为空，返回空列表
            if not text or len(text.strip()) == 0:
                print("[WARNING] 输入文本为空或只包含空白字符")
                self.logger.warning("输入文本为空或只包含空白字符")
                return []
            
            # 显示文本内容以便调试
            preview = text[:100] + "..." if len(text) > 100 else text
            print(f"[DEBUG] 文本预览: {preview}")
            self.logger.info(f"文本预览: {preview}")
            
            # 检查 Ollama 是否初始化
            if not self.ollama:
                print("[ERROR] Ollama 客户端未初始化")
                self.logger.error("Ollama 客户端未初始化")
                return []



            prompt = f"""As a professional indexer, identify 5 significant concepts from this text that a reader would likely search for:

{text}

Remember:
- Focus on the core concepts, not just keywords
- Consider what concepts would be most useful for retrieval
- Return ONLY a comma-separated list of concepts as tags
- Format with lowercase and underscores for multi-word concepts"""

            # 尝试直接调用 Ollama
            print("[DEBUG] 准备调用 Ollama API...")
            self.logger.info("准备调用 Ollama API...")
            
            # 检查 Ollama 状态并添加额外的测试
            try:
                # 直接使用 ollama 命令行测试
                import subprocess
                result = subprocess.run(["ollama", "list"], capture_output=True, text=True)
                print(f"[DEBUG] Ollama 命令行测试: {result.stdout.strip()}")
                
                # 检查 API 可用性
                import requests
                test_response = requests.get("http://127.0.0.1:11434/api/tags")
                print(f"[DEBUG] Ollama API 测试: {test_response.status_code}")
                
                # 检查对象状态
                if hasattr(self.ollama, 'status'):
                    status = self.ollama.status()
                    print(f"[DEBUG] Ollama 状态: {status}")
                    self.logger.info(f"Ollama 状态: {status}")
            except Exception as e:
                print(f"[ERROR] 检查 Ollama 状态失败: {e}")
                self.logger.error(f"检查 Ollama 状态失败: {e}")
            
            # 调用 Ollama
            try:
                print("[DEBUG] 调用 Ollama 生成标签")
                self.logger.info("调用 Ollama 生成标签")
                
                # 打印完整请求
                print(f"[DEBUG] 系统提示: {system}")
                print(f"[DEBUG] 用户提示: {prompt[:100]}...")
                
                response = self.ollama.run(prompt, system=system)
                print(f"[DEBUG] Ollama 响应: '{response}'")
                self.logger.info(f"Ollama 响应: '{response}'")
                
                # 如果响应中包含错误提示或请求补充信息的语句
                if response and ("please provide" in response.lower() or 
                                "i need" in response.lower() or
                                "please give" in response.lower()):
                    print(f"[WARNING] Ollama 返回了请求而非标签: {response}")
                    self.logger.warning(f"Ollama 返回了请求而非标签: {response}")
                    return []
                    
                # 检查是否为空响应
                if not response:
                    print("[WARNING] Ollama 返回空响应")
                    self.logger.warning("Ollama 返回空响应")
                    return []
                    
                # 尝试分割标签
                raw_tags = [tag.strip() for tag in response.split(',')]
                print(f"[DEBUG] 分割后原始标签: {raw_tags}")
                self.logger.info(f"分割后原始标签: {raw_tags}")
                
                # 清理和筛选标签
                valid_tags = []
                for tag in raw_tags:
                    if tag and len(tag) <= 10:  # 放宽长度限制以便调试
                        valid_tags.append(tag)
                    else:
                        print(f"[DEBUG] 忽略无效标签: '{tag}'")
                        self.logger.debug(f"忽略无效标签: '{tag}'")
                
                if limit and valid_tags:
                    valid_tags = valid_tags[:limit]
                    
                print(f"[DEBUG] 最终生成的有效标签({len(valid_tags)}): {valid_tags}")
                self.logger.info(f"最终生成的有效标签({len(valid_tags)}): {valid_tags}")
                return valid_tags
                
            except Exception as e:
                print(f"[ERROR] Ollama 调用失败: {e}")
                print(traceback.format_exc())
                self.logger.error(f"Ollama 调用失败: {e}")
                self.logger.error(traceback.format_exc())
                return []
                
        except Exception as e:
            print(f"[ERROR] 标签生成过程出错: {e}")
            print(traceback.format_exc())
            self.logger.error(f"标签生成过程出错: {e}")
            self.logger.error(traceback.format_exc())
            return []  # 返回空列表而不是抛出异常 
"""
SimTag Ollama桥接模块 - 提供与Ollama模型的交互功能
"""

import logging
from typing import Any, Optional, Dict
import subprocess
import traceback
import requests
import json
import sys

class OllamaBridge:
    """Ollama API集成，提供基础的LLM调用功能"""
    
    def __init__(self, model: str = "hf.co/unsloth/gemma-3-4b-it-GGUF:latest"):
        """初始化 Ollama 客户端
        
        Args:
            model: 使用的模型名称
        """
        self.logger = logging.getLogger("simtag.ollama_bridge")
        if not model:
            model = "hf.co/unsloth/gemma-3-4b-it-GGUF:latest"  # 确保有默认值
        self.model = str(model)  # 确保是字符串类型
        self.logger.info(f"初始化 OllamaBridge，使用模型: {self.model}")

    def run(self, prompt: str, system: str = None) -> str:
        """运行 Ollama 命令
        
        Args:
            prompt: 提示文本
            system: 系统提示
            
        Returns:
            模型输出文本
        """
        try:
            self.logger.info("准备调用 Ollama API")
            
            # 确保模型名称有效
            if not self.model:
                self.logger.error("模型名称未设置")
                raise Exception("模型名称未设置")
            
            self.logger.debug(f"使用模型: {self.model}")
            self.logger.debug(f"系统提示: {system}")
            self.logger.debug(f"用户提示: {prompt[:100]}...")
            
            # 构建请求数据
            data = {
                "model": self.model,
                "prompt": prompt,
                "stream": False  # 不使用流式响应
            }
            if system:
                data["system"] = system
            
            self.logger.info("发送 API 请求...")
            response = requests.post(
                "http://127.0.0.1:11434/api/generate",
                json=data
            )
            
            if response.status_code == 200:
                response_data = response.json()
                result = response_data.get('response', '').strip()
                
                # 记录生成统计信息
                if 'eval_duration' in response_data:
                    eval_duration = response_data['eval_duration']
                    eval_count = response_data.get('eval_count', 0)
                    tokens_per_second = eval_count / (eval_duration / 1e9) if eval_duration > 0 else 0
                    self.logger.info(f"生成速度: {tokens_per_second:.2f} tokens/s")
                
                self.logger.info("Ollama API 调用成功")
                self.logger.debug(f"响应结果: {result[:100]}...")  # 只记录前100个字符
                return result
                
            else:
                error_msg = f"Ollama API 调用失败: {response.text}"
                self.logger.error(error_msg)
                raise Exception(error_msg)
                
        except requests.exceptions.ConnectionError as e:
            error_msg = f"无法连接到 Ollama 服务: {str(e)}"
            self.logger.error(error_msg)
            raise Exception(error_msg)
            
        except Exception as e:
            error_msg = f"Ollama 执行异常: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            raise Exception(error_msg)

    def status(self) -> Dict[str, Any]:
        """获取 Ollama 状态"""
        try:
            self.logger.info("检查 Ollama 状态")
            result = subprocess.run(
                ["ollama", "list"],
                capture_output=True,
                text=True
            )
            
            if result.returncode == 0:
                self.logger.info("Ollama 状态检查成功")
                return {
                    "available": True,
                    "model": self.model,
                    "models": result.stdout.strip()
                }
            else:
                self.logger.error(f"Ollama 状态检查失败: {result.stderr}")
                return {
                    "available": False,
                    "error": result.stderr
                }
                
        except Exception as e:
            error_msg = f"Ollama 状态检查异常: {str(e)}"
            self.logger.error(error_msg)
            return {
                "available": False,
                "error": error_msg
            }

def _test():
    """测试 Ollama Bridge 功能"""
    # 设置日志
    logging.basicConfig(
        level=logging.DEBUG,
        format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S'
    )
    logger = logging.getLogger("ollama_bridge_test")
    
    try:
        logger.info("开始测试 Ollama Bridge")
        
        # 1. 测试初始化
        bridge = OllamaBridge()
        logger.info(f"创建 OllamaBridge 实例成功，使用模型: {bridge.model}")
        
        # 2. 测试状态检查
        logger.info("测试状态检查...")
        status = bridge.status()
        logger.info(f"Ollama 状态: {status}")
        
        # 3. 测试简单对话
        logger.info("测试简单对话...")
        prompt = "你好，请用一句话介绍自己。"
        response = bridge.run(prompt)
        logger.info(f"简单对话响应: {response}")
        
        # 4. 测试带系统提示的对话
        logger.info("测试带系统提示的对话...")
        system = "你是一个简洁的助手，回答要简短。"
        prompt = "解释什么是人工智能。"
        response = bridge.run(prompt, system=system)
        logger.info(f"带系统提示的对话响应: {response}")
        
        # 5. 测试标签生成场景
        logger.info("测试标签生成场景...")
        system = """你是一个标签生成专家。请分析给定的文本，生成最相关的标签。
要求：
1. 每个标签应该简洁、准确
2. 标签应该反映文本的主要主题和概念
3. 返回格式为逗号分隔的标签列表
4. 不要解释，只返回标签列表"""
        
        test_text = """
        Python是一种流行的编程语言，以其简洁的语法和丰富的生态系统而闻名。
        它广泛应用于Web开发、数据分析、人工智能等领域。
        """
        response = bridge.run(test_text, system=system)
        logger.info(f"标签生成响应: {response}")
        
        logger.info("所有测试完成")
        
    except Exception as e:
        logger.error(f"测试过程出错: {e}")
        logger.error(traceback.format_exc())
        return False
        
    return True

if __name__ == "__main__":
    success = _test()
    sys.exit(0 if success else 1) 
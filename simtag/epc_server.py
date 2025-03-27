"""
SimTag EPC服务器模块
提供统一的EPC接口，连接Emacs与Python后端功能
"""

import os
import sys
import json
import logging
import traceback
from typing import Dict, Any, List
import requests

from epc.server import EPCServer
from .config import Config
from .entity_extractor import EntityExtractor
from .ollama_bridge import OllamaBridge
from .tag_vectors import TagVectorEngine
from .tag_generator import TagGenerator
from .tag_relation_analyzer import TagRelationAnalyzer
from .utils.serialization import normalize_response

logger = logging.getLogger("simtag.epc_server")

class SimTagServer:
    """SimTag EPC服务器类"""
    
    def __init__(self, config: Config):
        """初始化服务器"""
        self.logger = logging.getLogger("simtag.epc_server")
        self.config = config
        self._initialized = False
        
        # 初始化基础组件
        self.ollama = None
        self.tag_generator = None
        self.entity_extractor = None
        self.vector_engine = None
        
        # 初始化EPC服务器
        self.server = EPCServer((self.config.host, self.config.port))
        self._register_methods()
        
    def _register_methods(self):
        """注册EPC方法"""
        methods = [
            ('echo', self.echo),
            ('initialize', self.initialize),
            ('suggest_tags', self.suggest_tags),
            ('extract_entities', self.extract_entities),
            ('find_similar', self.find_similar),
            ('test_ollama_connection', self.test_ollama_connection),
            ('generate_text', self.generate_text),
        ]
        
        for name, method in methods:
            self.server.register_function(method)
            
    def start(self):
        """启动服务器"""
        try:
            port = self.server.server_address[1]
            print(f"{port}", flush=True)
            self.server.serve_forever()
        except Exception as e:
            self.logger.error(f"服务器启动失败: {e}")
            raise
        
    def echo(self, message: str) -> str:
        """回显测试方法"""
        return f"Echo: {message}"
        
    def initialize(self, vector_file: str = None, db_file: str = None) -> Dict[str, Any]:
        """初始化服务器组件"""
        try:
            if vector_file:
                self.config.vector_file = vector_file
            
            if db_file:
                self.config.db_file = db_file
            
            # 初始化组件
            self.ollama = OllamaBridge(model=self.config.model_name)
            self.tag_generator = TagGenerator(self.ollama)
            self.entity_extractor = EntityExtractor(self.ollama)
            self.vector_engine = TagVectorEngine(vector_file=self.config.vector_file)
            
            self._initialized = True
            
            return normalize_response({
                "status": "success",
                "vector_file": self.config.vector_file,
                "db_file": self.config.db_file,
            })
            
        except Exception as e:
            self._initialized = False
            self.logger.error(f"初始化失败: {e}")
            return normalize_response(None, "error", str(e))
            
    def find_similar(self, tag_name: str, content: str = "", top_k: int = 5) -> Dict[str, Any]:
        """查找相似标签"""
        try:
            if not self._initialized or not self.vector_engine:
                return normalize_response(None, "error", "服务未初始化")
            
            results = self.vector_engine.find_similar(tag_name, top_k)
            return normalize_response(results)
            
        except Exception as e:
            return normalize_response(None, "error", str(e))

    def suggest_tags(self, text: str, limit: int = 5) -> Dict[str, Any]:
        """生成标签建议"""
        try:
            if not self._initialized:
                self.initialize()
            
            if not self.tag_generator:
                return normalize_response(None, "error", "标签生成器未初始化")
            
            tags = self.tag_generator.suggest_tags(text)
            
            if not tags:
                return normalize_response([])
            
            valid_tags = [str(tag).strip() for tag in tags if tag]
            return normalize_response(valid_tags)
            
        except Exception as e:
            return normalize_response(None, "error", str(e))
            
    def extract_entities(self, text: str, options: Dict[str, Any] = None) -> Dict[str, Any]:
        """提取实体"""
        try:
            # 检查初始化状态，如果未初始化则自动初始化
            if not self._initialized:
                self.logger.info("服务未初始化，自动初始化...")
                init_result = self.initialize()
                if init_result.get("status") != "success":
                    return normalize_response(None, "error", "服务器自动初始化失败")
            
            # 确保ollama已初始化
            if not self.ollama:
                self.logger.info("Ollama未初始化，正在创建...")
                self.ollama = OllamaBridge(model=self.config.model_name)
            
            # 再次检查实体提取器是否可用
            if not self.entity_extractor:
                self.logger.error("实体提取器初始化失败")
                # 尝试重新创建实体提取器
                try:
                    self.entity_extractor = EntityExtractor(self.ollama)
                except Exception as creation_err:
                    return normalize_response(None, "error", f"创建实体提取器失败: {str(creation_err)}")
            
            # 直接调用模块级extract_entities函数，并传递ollama实例
            from .entity_extractor import extract_entities
            entities = extract_entities(text, self.ollama)
            
            # 记录提取结果
            self.logger.info(f"成功提取到 {len(entities) if entities else 0} 个实体")
            return normalize_response(entities)
            
        except Exception as e:
            self.logger.error(f"提取实体失败: {str(e)}")
            return normalize_response(None, "error", str(e))

    def test_ollama_connection(self) -> Dict[str, Any]:
        """测试与Ollama的连接
        
        Returns:
            包含连接状态和模型列表的响应
        """
        try:
            self.logger.info("测试Ollama连接...")
            
            # 确保ollama已初始化
            if not self.ollama:
                self.logger.info("Ollama未初始化，尝试初始化...")
                try:
                    self.ollama = OllamaBridge(model=self.config.model_name)
                except Exception as e:
                    self.logger.error(f"Ollama初始化失败: {str(e)}")
                    return normalize_response(None, "error", f"Ollama初始化失败: {str(e)}")
            
            # 获取Ollama状态
            try:
                # 首先通过API检查模型列表
                response = requests.get("http://localhost:11434/api/tags")
                if response.status_code != 200:
                    return normalize_response(None, "error", f"Ollama API调用失败: {response.text}")
                
                models_data = response.json()
                available_models = []
                
                if "models" in models_data:
                    models = models_data["models"]
                    # 根据返回结构解析模型列表
                    if isinstance(models, list):
                        for model_info in models:
                            if isinstance(model_info, dict) and "name" in model_info:
                                available_models.append(model_info["name"])
                    # 如果是向量形式
                    elif isinstance(models, (tuple, list)) or hasattr(models, "__iter__"):
                        for i in range(len(models)):
                            model_info = models[i]
                            if isinstance(model_info, dict) and "name" in model_info:
                                available_models.append(model_info["name"])
                
                # 检查当前模型是否在列表中
                current_model = self.config.model_name
                model_available = current_model in available_models
                
                # 简单的ping测试
                test_result = self.ollama.run("测试连接", system="简短回复")
                connection_success = test_result is not None and len(test_result) > 0
                
                if connection_success:
                    self.logger.info(f"Ollama连接测试成功，找到 {len(available_models)} 个模型")
                    return normalize_response({
                        "connected": True,
                        "models": available_models,
                        "current_model": current_model,
                        "model_available": model_available
                    })
                else:
                    self.logger.error("Ollama连接测试失败: 无法获取响应")
                    return normalize_response(None, "error", "无法获取Ollama响应")
                    
            except requests.exceptions.ConnectionError as e:
                self.logger.error(f"Ollama连接失败: {str(e)}")
                return normalize_response(None, "error", f"无法连接到Ollama服务: {str(e)}")
                
            except Exception as e:
                self.logger.error(f"Ollama状态检查出错: {str(e)}")
                return normalize_response(None, "error", f"Ollama状态检查出错: {str(e)}")
                
        except Exception as e:
            self.logger.error(f"测试Ollama连接时出现异常: {str(e)}")
            return normalize_response(None, "error", f"测试出错: {str(e)}")

    def generate_text(self, prompt: str, system: str = None) -> Dict[str, Any]:
        """使用Ollama生成文本，用于测试
        
        Args:
            prompt: 提示文本
            system: 可选的系统提示
            
        Returns:
            包含生成文本的响应
        """
        try:
            self.logger.info("测试文本生成...")
            
            # 确保ollama已初始化
            if not self.ollama:
                self.logger.info("Ollama未初始化，尝试初始化...")
                try:
                    self.ollama = OllamaBridge(model=self.config.model_name)
                except Exception as e:
                    self.logger.error(f"Ollama初始化失败: {str(e)}")
                    return normalize_response(None, "error", f"Ollama初始化失败: {str(e)}")
            
            # 使用默认系统提示如果未提供
            if not system:
                system = "你是一个简洁的助手，回答要简短明了。"
                
            # 调用Ollama生成文本
            try:
                text = self.ollama.run(prompt, system=system)
                
                if text:
                    self.logger.info(f"成功生成文本: {text[:50]}...")
                    return normalize_response({
                        "text": text
                    })
                else:
                    self.logger.error("文本生成失败: 返回空结果")
                    return normalize_response(None, "error", "生成结果为空")
                    
            except Exception as e:
                self.logger.error(f"文本生成出错: {str(e)}")
                return normalize_response(None, "error", f"文本生成出错: {str(e)}")
                
        except Exception as e:
            self.logger.error(f"文本生成过程中出现异常: {str(e)}")
            return normalize_response(None, "error", f"生成过程出错: {str(e)}")

def main(config: Config):
    """主函数"""
    try:
        server = SimTagServer(config)
        server.start()
    except Exception as e:
        logger.error(f"服务器启动失败: {e}")
        sys.exit(1)

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description='SimTag EPC服务器')
    parser.add_argument('--vector-file', help='向量文件路径')
    parser.add_argument('--db-file', help='数据库文件路径')
    parser.add_argument('--model', help='模型名称')
    parser.add_argument('--debug', action='store_true', help='启用调试模式')
    parser.add_argument('--log-file', help='日志文件路径')
    parser.add_argument('--host', default='127.0.0.1', help='服务器地址')
    parser.add_argument('--port', type=int, default=0, help='服务器端口')
    args = parser.parse_args()

    config = Config(
        vector_file=args.vector_file,
        db_file=args.db_file,
        model_name=args.model,
        debug=args.debug,
        log_file=args.log_file,
        host=args.host,
        port=args.port
    )
    
    main(config) 
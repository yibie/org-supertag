"""
SimTag EPC服务器模块
提供统一的EPC接口，连接Emacs与Python后端功能
"""

import os
import sys
import json
import logging
import traceback
import argparse
import subprocess
from typing import List, Dict, Any, Optional, Tuple

from epc.server import EPCServer
from .config import Config
from .entity_extractor import EntityExtractor
from .ollama_bridge import OllamaBridge
from .tag_vectors import TagVectorEngine
from .utils.logging import setup_logging
from .utils.serialization import normalize_response
from .tag_generator import TagGenerator
from .tag_relation_analyzer import TagRelationAnalyzer, analyze_tag_relations

logger = logging.getLogger("simtag.epc_server")

class SimTagServer:
    """SimTag EPC服务器类"""
    
    def __init__(self, config: Config):
        """初始化服务器
        
        Args:
            config: 配置对象
        """
        self.logger = logging.getLogger("simtag.epc_server")
        self.config = config
        self._initialized = False  # 添加初始化标志
        
        # 初始化基础组件为 None
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
            ('status', self.status),
            ('initialize', self.initialize),
            ('find_similar', self.find_similar),
            ('suggest_tags', self.suggest_tags),
            ('suggest_tags_base64', self.suggest_tags_base64),
            ('suggest_tags_json', self.suggest_tags_json),
            ('extract_entities', self.extract_entities),
            ('check_imports', self.check_imports),
            ('get_config', self.get_config),
            ('test_engine', self.test_engine),
            ('analyze_tag_relations', self.analyze_tag_relations),
            ('run_ollama', self.run_ollama),
        ]
        
        for name, method in methods:
            self.server.register_function(method)
            
    def start(self):
        """启动服务器"""
        try:
            port = self.server.server_address[1]
            self.logger.info(f"获取到服务器端口: {port}")
            
            # 确保标准输出是干净的
            sys.stdout.flush()  # 清空缓冲区
            
            # 重要：单独一行输出端口号
            print(f"{port}", flush=True)
            self.logger.info(f"已输出端口号到标准输出: {port}")
            
            # 启动服务器
            self.logger.info("开始serve_forever()...")
            self.server.serve_forever()
        except Exception as e:
            self.logger.error(f"服务器启动失败: {e}")
            raise
        
    def echo(self, message: str) -> str:
        """回显测试方法"""
        self.logger.info(f"Echo测试: {message}")
        return f"Echo: {message}"
        
    def status(self) -> Dict[str, Any]:
        """获取服务器状态
        
        Returns:
            状态信息字典
        """
        status = {
            "server": {
                "running": True,
                "port": self.server.server_address[1]
            },
            "components": {
                "vector_engine": self.vector_engine.status() if self.vector_engine else None,
                "ollama": self.ollama.status() if self.ollama else None
            },
            "config": self.config.to_dict()
        }
        return normalize_response(status)

    """def initialize 初始化函数不要破坏性更新"""   
    def initialize(self, vector_file: str = None, db_file: str = None) -> Dict[str, Any]:
        """初始化服务器组件"""
        try:
            self.logger.info("初始化服务器组件...")
            
            # 更新并验证文件路径
            if vector_file:
                self.logger.info(f"使用指定的向量文件: {vector_file}")
                if not os.path.exists(vector_file):
                    self.logger.error(f"指定的向量文件不存在: {vector_file}")
                    return normalize_response(None, "error", f"向量文件不存在: {vector_file}")
                self.config.vector_file = vector_file
            
            if db_file:
                self.logger.info(f"使用指定的数据库文件: {db_file}")
                if not os.path.exists(db_file):
                    self.logger.error(f"指定的数据库文件不存在: {db_file}")
                    return normalize_response(None, "error", f"数据库文件不存在: {db_file}")
                self.config.db_file = db_file
            
            # 确保Ollama可用
            if not self.config.ensure_ollama():
                raise Exception("Ollama未安装或不可用")
            
            # 1. 初始化 Ollama
            self.logger.info("初始化 Ollama...")
            self.ollama = OllamaBridge(model=self.config.model_name)
            
            # 2. 初始化标签生成器
            self.logger.info("初始化标签生成器...")
            self.tag_generator = TagGenerator(self.ollama)
            
            # 3. 初始化其他组件
            self.logger.info("初始化其他组件...")
            self.entity_extractor = EntityExtractor(self.ollama)
            self.vector_engine = TagVectorEngine(vector_file=self.config.vector_file)
            
            # 标记初始化完成
            self._initialized = True
            self.logger.info("所有组件初始化完成")
            
            return normalize_response({
                "status": "success",
                "vector_file": self.config.vector_file,
                "db_file": self.config.db_file,
                "model": self.config.model_name
            })
            
        except Exception as e:
            self._initialized = False  # 确保失败时标记为未初始化
            self.logger.error(f"初始化失败: {e}")
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", str(e))
            
    def find_similar(self, tag_name: str, content: str = "", top_k: int = 5) -> Dict[str, Any]:
        """查找相似标签
        
        Args:
            tag_name: 标签名称
            content: 相关内容
            top_k: 返回结果数量
            
        Returns:
            相似标签列表
        """
        try:
            # 检查初始化状态
            if not self._initialized:
                self.logger.error("服务未初始化，请先调用 initialize")
                return normalize_response(None, "error", "服务未初始化，请先调用 initialize")
            
            # 检查向量引擎
            if not self.vector_engine:
                self.logger.error("向量引擎未初始化")
                return normalize_response(None, "error", "向量引擎未初始化")
            
            # 使用混合搜索
            self.logger.info(f"查找与 '{tag_name}' 相似的标签...")
            results = self.vector_engine.find_similar(tag_name, top_k)
            
            return normalize_response(results)
            
        except Exception as e:
            error_msg = f"查找相似标签失败: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", error_msg)

    def suggest_tags(self, text: str, limit: int = 5) -> Dict[str, Any]:
        """生成标签建议"""
        try:
            # 如果未初始化，尝试自动初始化
            if not self._initialized:
                self.logger.info("服务未初始化，尝试自动初始化...")
                init_result = self.initialize()
                if init_result.get("status") != "success":
                    self.logger.error("自动初始化失败")
                    return normalize_response(None, "error", "服务初始化失败")
            
            if not self.tag_generator:
                self.logger.error("标签生成器未初始化")
                return normalize_response(None, "error", "标签生成器未初始化")
            
            # 获取标签列表
            self.logger.info("开始生成标签...")
            self.logger.debug(f"输入文本预览: {text[:100]}...")  # 添加输入文本日志
            
            tags = self.tag_generator.suggest_tags(text)
            
            # 验证标签列表
            if not tags:
                self.logger.warning("未生成任何标签")
                return normalize_response([])  # 返回空列表而不是 None
            
            if not isinstance(tags, list):
                self.logger.error(f"标签生成器返回了非列表类型: {type(tags)}")
                return normalize_response(None, "error", "标签格式错误")
            
            # 确保所有标签都是字符串
            valid_tags = [str(tag).strip() for tag in tags if tag]
            
            self.logger.info(f"成功生成 {len(valid_tags)} 个标签: {valid_tags}")
            
            # 使用 normalize_response 返回
            return normalize_response(valid_tags)
            
        except Exception as e:
            self.logger.error(f"生成标签失败: {e}")
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", str(e))
            
    def extract_entities(self, text: str) -> Dict[str, Any]:
        """提取实体（完整版）
        
        Args:
            text: 文本内容
            
        Returns:
            实体列表
        """
        try:
            if not self.entity_extractor:
                raise Exception("实体提取器未初始化")
                
            entities = self.entity_extractor.extract(text)
            return normalize_response(entities)
            
        except Exception as e:
            error_msg = f"提取实体失败: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", error_msg)
            

    def check_imports(self):
        """测试必要模块是否已正确导入."""
        try:
            import numpy
            import torch
            import sentence_transformers
            import requests
            return {
                "status": "success",
                "imports": {
                    "numpy": numpy.__version__,
                    "torch": torch.__version__,
                    "sentence_transformers": sentence_transformers.__version__,
                    "requests": requests.__version__
                }
            }
        except ImportError as e:
            return {
                "status": "error",
                "message": str(e)
            }

    def get_config(self):
        """返回当前配置信息."""
        return {
            "vector_file": self.config.vector_file,
            "db_file": self.config.db_file,
            "model_name": self.config.model_name,
            "debug": self.config.debug
        }

    def test_engine(self, test_text: str) -> Dict[str, Any]:
        """测试文本向量引擎功能
        
        Args:
            test_text: 测试文本
            
        Returns:
            向量数据
        """
        try:
            if not self.vector_engine:
                raise Exception("向量引擎未初始化")
                
            # 生成文本向量
            self.logger.info(f"开始生成文本向量: {test_text}")
            vector = self.vector_engine.model.encode(test_text)
            
            # 详细记录向量信息
            self.logger.info(f"向量类型: {type(vector)}")
            self.logger.info(f"向量形状: {vector.shape if hasattr(vector, 'shape') else len(vector)}")
            
            # 获取向量数据
            vector_data = vector.tolist() if hasattr(vector, 'tolist') else vector
            self.logger.info(f"向量数据长度: {len(vector_data)}")
            
            # 返回结果
            result = {
                "vector": vector_data,
                "dimensions": len(vector_data),
                "model": self.vector_engine.model_name if hasattr(self.vector_engine, 'model_name') else None
            }
            
            return normalize_response(result)
            
        except Exception as e:
            error_msg = f"引擎测试失败: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", error_msg)

    def analyze_tag_relations(self, tag: str, tags: list) -> Dict[str, Any]:
        """分析标签关系
        
        Args:
            tag: 目标标签
            tags: 待分析的标签列表
            
        Returns:
            标签关系列表
        """
        try:
            relations = self.tag_analyzer.analyze_relations(tag, tags)
            return {
                "status": "success",
                "result": relations
            }
        except Exception as e:
            return {
                "status": "error",
                "message": f"分析标签关系失败: {str(e)}"
            }

    def run_ollama(self, prompt, system=None):
        """向Ollama发送消息并获取响应
        
        Args:
            prompt: 用户提示文本
            system: 可选的系统提示文本
            
        Returns:
            Dict: 包含处理结果的字典
        """
        self.logger.info(f"接收到Ollama交互请求，prompt长度: {len(prompt)}")
        
        # 检查初始化状态
        if not self._initialized:
            self.logger.error("尝试在初始化前使用Ollama")
            return normalize_response(None, "error", "服务未初始化，请先调用initialize")
        
        # 检查Ollama实例
        if not self.ollama:
            self.logger.error("Ollama实例未初始化")
            return normalize_response(None, "error", "Ollama实例未初始化")
        
        try:
            # 记录进一步的信息，但避免记录过长的prompt
            prompt_preview = prompt[:100] + "..." if len(prompt) > 100 else prompt
            self.logger.info(f"发送请求到Ollama，prompt预览: {prompt_preview}")
            
            # 调用Ollama
            response = self.ollama.run(prompt, system=system)
            
            # 检查响应
            if not response:
                return normalize_response(None, "error", "Ollama返回空响应")
            
            # 记录响应（只记录部分以避免日志过大）
            response_preview = response[:100] + "..." if len(response) > 100 else response
            self.logger.info(f"接收到Ollama响应，长度: {len(response)}，预览: {response_preview}")
            
            # 返回成功响应
            return normalize_response(response, "success")
            
        except Exception as e:
            # 捕获并记录异常
            error_message = f"Ollama交互出错: {str(e)}"
            trace = traceback.format_exc()
            self.logger.error(f"{error_message}\n{trace}")
            return normalize_response(None, "error", error_message)

    def suggest_tags_base64(self, base64_text: str, limit: int = 5) -> Dict[str, Any]:
        """处理Base64编码的文本并生成标签建议
        
        Args:
            base64_text: Base64编码的文本
            limit: 返回结果数量限制
            
        Returns:
            标签列表
        """
        try:
            # 记录接收到的Base64文本长度
            self.logger.info(f"收到Base64编码文本，长度: {len(base64_text)}")
            
            # 解码Base64文本
            import base64
            try:
                decoded_text = base64.b64decode(base64_text).decode('utf-8')
                self.logger.info(f"Base64解码成功，解码后文本长度: {len(decoded_text)}")
                
                # 添加文本预览日志
                if decoded_text:
                    preview = decoded_text[:200] + "..." if len(decoded_text) > 200 else decoded_text
                    self.logger.info(f"解码后文本预览: {preview}")
            except Exception as e:
                self.logger.error(f"Base64解码失败: {e}")
                return normalize_response(None, "error", f"Base64解码失败: {e}")
            
            # 调用常规的标签生成方法处理解码后的文本
            return self.suggest_tags(decoded_text, limit)
            
        except Exception as e:
            self.logger.error(f"Base64文本处理失败: {e}")
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", str(e))
            
    def suggest_tags_json(self, json_data: str, limit: int = 5) -> Dict[str, Any]:
        """使用JSON格式处理标签生成请求
        
        Args:
            json_data: JSON格式的请求数据，包含要分析的文本内容
            limit: 返回结果数量限制
            
        Returns:
            标签列表
        """
        try:
            # 记录接收到的JSON数据长度
            self.logger.info(f"收到JSON格式请求，长度: {len(json_data)}")
            
            # 解析JSON数据
            try:
                import json
                request = json.loads(json_data)
                
                # 确保JSON格式正确，包含content字段
                if not isinstance(request, dict):
                    self.logger.error(f"JSON数据不是字典格式: {type(request)}")
                    return normalize_response(None, "error", "无效的请求格式，应为JSON对象")
                
                text = request.get("content")
                
                if not text:
                    self.logger.error("请求中缺少content字段或为空")
                    return normalize_response(None, "error", "请求中缺少文本内容")
                
                self.logger.info(f"从JSON提取的文本长度: {len(text)}")
                text_preview = text[:100] + "..." if len(text) > 100 else text
                self.logger.info(f"文本预览: {text_preview}")
                
            except json.JSONDecodeError as e:
                self.logger.error(f"JSON解析失败: {e}")
                self.logger.error(f"接收到的JSON数据: {json_data[:200]}..." if len(json_data) > 200 else json_data)
                return normalize_response(None, "error", f"JSON解析失败: {e}")
            
            # 使用标准的suggest_tags方法处理文本
            return self.suggest_tags(text, limit)
            
        except Exception as e:
            self.logger.error(f"JSON请求处理失败: {e}")
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", str(e))

def run_ollama_model(text, model_name="gemma-3b-it"):
    """直接运行ollama命令"""
    try:
        cmd = ["ollama", "run", model_name, text]
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        return result.stdout.strip()
    except subprocess.CalledProcessError as e:
        logger.error(f"运行ollama命令失败: {e}")
        return None

def main(config: Config):
    """主函数"""
    try:
        # 初始化日志
        log_level = logging.DEBUG if config.debug else logging.INFO
        setup_logging(config.log_file, log_level)
        
        # 记录配置信息
        logger.info("SimTag EPC 服务器配置:")
        logger.info(f"向量文件: {config.vector_file}")
        logger.info(f"数据库文件: {config.db_file}")
        logger.info(f"日志文件: {config.log_file}")
        logger.info(f"调试模式: {config.debug}")
        
        # 创建服务器实例
        server = SimTagServer(config)
        
        # 启动服务器
        server.start()
        
    except Exception as e:
        logger.error(f"服务器启动失败: {e}")
        logger.error(traceback.format_exc())
        sys.exit(1)

if __name__ == "__main__":
    # 解析命令行参数
    parser = argparse.ArgumentParser(description='SimTag EPC服务器')
    parser.add_argument('--vector-file', help='向量文件路径')
    parser.add_argument('--db-file', help='数据库文件路径')
    parser.add_argument('--model', help='模型名称')
    parser.add_argument('--debug', action='store_true', help='启用调试模式')
    parser.add_argument('--log-file', help='日志文件路径')
    parser.add_argument('--host', default='127.0.0.1', help='服务器地址')
    parser.add_argument('--port', type=int, default=0, help='服务器端口')
    args = parser.parse_args()

    # 创建配置对象
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
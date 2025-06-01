"""
EPC 服务器入口
提供与 Emacs 通信的接口，基于更稳定的实现
参考了 Blink-Search 的架构设计
"""
import sys
import logging
import traceback
import os
import socket
import socketserver
import threading
from epc.server import ThreadingEPCServer
from simtag.core.storage import VectorStorage
from simtag.core.tagging import TaggingEngine

# 配置日志
def setup_logging():
    # 获取数据目录
    data_dir = os.environ.get('ORG_SUPERTAG_DATA_DIRECTORY', 
                            os.path.expanduser('~/.emacs.d/org-supertag'))
    
    # 确保目录存在
    os.makedirs(data_dir, exist_ok=True)
    
    # 设置日志文件路径
    log_file = os.path.join(data_dir, 'simtag_epc.log')
    
    # 设置日志
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(sys.stderr),
            logging.FileHandler(log_file, mode='a', encoding='utf-8')
        ]
    )
    
    return logging.getLogger("main"), data_dir, log_file

# 主服务类
class SimTagEPCServer:
    def __init__(self, config):
        """初始化EPC服务器
        
        Args:
            config: 配置对象
        """
        self.config = config
        self.logger = logging.getLogger("epc_server")
        
        # 创建向量存储对象
        self.storage = VectorStorage(config.vector_db_path)
        
        # 检查sqlite-vec扩展
        vec_version = self.storage.check_vec_extension()
        if vec_version:
            self.logger.info(f"成功加载sqlite-vec扩展，版本: {vec_version}")
        else:
            self.logger.warning("sqlite-vec扩展未加载，向量搜索性能可能受到影响")
        
        # 创建标签引擎
        self.engine = TaggingEngine(config, self.storage)
        
        # 创建 EPC 服务器
        self.server = ThreadingEPCServer(('127.0.0.1', 0), log_traceback=True)
        self.server.allow_reuse_address = True
        
        # 注册方法
        self._register_methods()
        
        # 创建服务线程
        self.server_thread = None
        
        # 简化状态管理 - 移除复杂的消息队列机制
        self.state = {}

    def _register_methods(self):
        """注册 EPC 方法"""
        methods = [
            ('echo', self.echo),  # 添加 echo 方法用于测试
            ('ping', self.ping),  # 添加心跳检测方法
            ('initialize', self.initialize),  # 添加初始化方法
            ('suggest_tags', self.suggest_tags),
            ('suggest_tags_json', self.suggest_tags_json),  # 添加JSON格式的标签推荐方法
            ('extract_entities', self.extract_entities),
            ('find_similar', self.find_similar_tags),  # 修正方法名匹配
            ('find_similar_tags', self.find_similar_tags),  # 保持兼容性
            ('analyze_tag_relations', self.analyze_tag_relations),
            ('sync_library', self.sync_library),  # 添加同步库方法
            ('update_tag', self.update_tag),  # 添加更新单个标签方法
            ('remove_tag', self.remove_tag),  # 添加删除标签方法
            ('get_status', self.get_status),
            ('check_imports', self.check_imports),  # 添加导入检查方法
            ('get_config', self.get_config),  # 添加配置获取方法
            ('update_state', self.update_state)  # 添加状态更新方法
        ]
        for name, method in methods:
            self.server.register_function(method, name)
            
    def update_state(self, state_name, value):
        """更新服务器状态 - 简化版本，不使用消息队列"""
        try:
            self.state[state_name] = value
            self.logger.info(f"状态更新: {state_name} = {value}")
            return {"status": "success"}
        except Exception as e:
            self.logger.error(f"状态更新失败: {e}")
            return {"status": "error", "message": str(e)}
    
    def echo(self, message):
        """回声测试方法 - 极简版本，确保快速响应"""
        try:
            self.logger.info(f"Echo收到消息: {message}")
            # 直接更新状态，不使用消息队列
            self.state["last_echo"] = message
            # 返回简单字符串，确保兼容性
            response = f"Echo: {message}"
            self.logger.info(f"Echo处理完成，返回: {response}")
            return response
        except Exception as e:
            self.logger.error(f"Echo方法出错: {e}")
            return f"Error in Python echo: {str(e)}"
    
    def ping(self):
        """心跳检测方法 - 简化版本"""
        try:
            self.logger.info("收到ping请求，开始处理...")
            # 直接更新状态，不使用消息队列
            self.state["last_ping"] = "active"
            # 临时返回简单字符串测试EPC序列化
            response = "pong"
            self.logger.info(f"ping处理完成，返回: {response}")
            return response
        except Exception as e:
            self.logger.error(f"Ping方法出错: {e}")
            return f"Error: {str(e)}"
    
    def initialize(self, vector_file_path, db_file_path):
        """初始化方法，更新向量文件路径"""
        try:
            self.logger.info(f"Initializing with vector_file: {vector_file_path}, db_file: {db_file_path}")
            
            # 更新配置
            if vector_file_path and os.path.isabs(vector_file_path):
                self.config.vector_db_path = vector_file_path
                # 需要重新创建存储对象来应用新路径
                self.storage = VectorStorage(vector_file_path)
                # 重新初始化引擎
                self.engine = TaggingEngine(self.config, self.storage)
                self.logger.info(f"Vector database path updated to: {vector_file_path}")
            else:
                self.logger.warning(f"Invalid vector file path: {vector_file_path}, using default: {self.config.vector_db_path}")
            
            return {"status": "success", "result": {
                "vector_db_path": self.config.vector_db_path,
                "db_file_path": db_file_path
            }}
        except Exception as e:
            self.logger.error(f"Initialization failed: {e}")
            return {"status": "error", "message": str(e)}
    
    def check_imports(self):
        """检查模块导入状态"""
        try:
            import numpy
            import requests
            import epc
            return {"status": "success", "modules": ["numpy", "requests", "epc"]}
        except Exception as e:
            return {"status": "error", "message": str(e)}
    
    def get_config(self):
        """获取配置信息"""
        return {
            "status": "success", 
            "config": {
                "vector_db_path": self.config.vector_db_path,
                "ollama_model": getattr(self.config, 'ollama_model', 'default')
            }
        }
    
    def suggest_tags(self, text, limit=5, use_ai=True):
        """标签推荐方法 - 统一返回格式"""
        try:
            result = self.engine.generate_tags(text, limit, use_ai)
            return {"status": "success", "result": result}
        except Exception as e:
            error_msg = f"标签推荐失败: {str(e)}"
            self.logger.error(error_msg)
            return {"status": "error", "message": error_msg}
    
    def suggest_tags_json(self, json_data):
        """使用JSON格式处理标签推荐请求
        
        Args:
            json_data: JSON格式的请求数据，应包含content字段
            
        Returns:
            标签推荐结果
        """
        try:
            # 解析JSON数据
            import json
            if isinstance(json_data, str):
                data = json.loads(json_data)
            else:
                data = json_data
                
            # 提取参数
            content = data.get('content', '')
            limit = data.get('limit', 5)
            use_ai = data.get('use_ai', True)
            
            # 参数校验
            if not content:
                return {"status": "error", "message": "Content is empty"}
            
            # 调用标签生成
            self.logger.info(f"JSON请求标签推荐: 内容长度={len(content)}, limit={limit}")
            tags = self.engine.generate_tags(content, limit, use_ai)
            
            # 返回结果
            return {"status": "success", "result": tags}
        except Exception as e:
            error_msg = f"JSON标签推荐失败: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            return {"status": "error", "message": error_msg}
    
    def extract_entities(self, text, use_ai=True):
        """实体提取方法 - 统一返回格式"""
        try:
            result = self.engine.extract_entities(text, use_ai)
            return {"status": "success", "result": result}
        except Exception as e:
            error_msg = f"实体提取失败: {str(e)}"
            self.logger.error(error_msg)
            return {"status": "error", "message": error_msg}
    
    def find_similar_tags(self, tag, limit=10):
        """查找相似标签
        
        Args:
            tag: 标签名称或ID
            limit: 返回结果数量上限
            
        Returns:
            带状态的结果字典
        """
        try:
            # 调用引擎查找相似标签
            self.logger.info(f"开始查找相似标签: {tag}, limit={limit}")
            similar_tags = self.engine.find_similar_tags(tag, limit)
            
            # 记录找到的标签数量和原始数据
            self.logger.info(f"找到 {len(similar_tags)} 个相似标签")
            self.logger.info(f"原始数据: {similar_tags}")
            
            # 确保结果格式正确，每个元素都是[tag_name, score]格式的列表
            result = []
            for tag_id, score in similar_tags:
                self.logger.info(f"处理标签: {tag_id}, 相似度: {score}")
                # 使用列表而非元组，更易于JSON序列化
                result.append([tag_id, float(score)])
            
            # 检查结果格式
            self.logger.info(f"最终结果结构: {type(result)}")
            self.logger.info(f"第一个结果样例: {result[0] if result else 'None'}")
            self.logger.info(f"第一个结果类型: {type(result[0]) if result else 'None'}")
            
            # 构建完整的响应
            response = {
                "status": "success", 
                "result": result
            }
            
            self.logger.info(f"返回响应: status={response['status']}, 结果数量={len(response['result'])}")
            self.logger.info(f"完整响应: {response}")
            return response
        except Exception as e:
            error_msg = f"查找相似标签失败: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            return {"status": "error", "message": error_msg}
    
    def analyze_tag_relations(self, tag, related_tags, use_ai=True):
        """标签关系分析方法 - 统一返回格式"""
        try:
            result = self.engine.analyze_relations(tag, related_tags, use_ai)
            return {"status": "success", "result": result}
        except Exception as e:
            error_msg = f"标签关系分析失败: {str(e)}"
            self.logger.error(error_msg)
            return {"status": "error", "message": error_msg}
    
    def get_status(self):
        """获取状态方法 - 简化版本，移除消息队列操作"""
        try:
            status = {
                "ollama_available": self.engine.ollama.available,
                "storage_ready": True,
                "storage_stats": self.storage.get_stats(),
                "server_running": True,
                "config": {
                    "vector_db_path": self.config.vector_db_path,
                    "ollama_model": self.config.ollama_model
                },
                "state": self.state  # 添加当前状态
            }
            # 直接更新状态，不使用消息队列
            self.state["last_status"] = status
            return {"status": "success", "result": status}
        except Exception as e:
            error_msg = f"获取状态失败: {str(e)}"
            self.logger.error(error_msg)
            return {"status": "error", "message": error_msg}
    
    def sync_library(self, db_file, tag_data):
        """同步标签向量库
        
        Args:
            db_file: 数据库文件路径
            tag_data: 标签数据列表，每个元素是包含id和name的词典
            
        Returns:
            同步结果
        """
        try:
            # 记录收到的原始数据类型和大小，以便排查问题
            self.logger.info(f"同步标签库请求: 数据库文件={db_file}, 标签数据类型={type(tag_data)}, 数据大小={len(tag_data) if isinstance(tag_data, (list, tuple)) else 'unknown'}")
            
            # 数据安全性检查
            if not isinstance(tag_data, (list, tuple)):
                self.logger.warning(f"标签数据不是列表类型: {type(tag_data)}")
                tag_data = []
            
            # 记录前几个标签的数据格式，用于调试
            if tag_data and len(tag_data) > 0:
                sample_tags = tag_data[:min(3, len(tag_data))]
                self.logger.info(f"标签数据样例: {sample_tags}")
            
            # 调用引擎进行同步
            result = self.engine.sync_tags(db_file, tag_data)
            self.logger.info(f"Sync embedding tag result: {result}")
            return {"status": "success", "result": result}
        except Exception as e:
            error_msg = f"同步标签库失败: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            return {"status": "error", "message": error_msg}
    
    def update_tag(self, tag_info):
        """更新单个标签的向量
        
        Args:
            tag_info: 包含id和name的标签信息，可以是列表/元组 [id, name] 或字典 {"id": id, "name": name}
            
        Returns:
            更新结果
        """
        try:
            self.logger.info(f"更新标签向量: 数据类型={type(tag_info)}, 值={tag_info}")
            
            # 兼容不同的数据格式
            if isinstance(tag_info, (list, tuple)) and len(tag_info) >= 2:
                # 列表/元组格式 [id, name]
                tag_id = tag_info[0]
                tag_name = tag_info[1]
            elif isinstance(tag_info, dict) and "id" in tag_info and "name" in tag_info:
                # 字典格式 {"id": id, "name": name}
                tag_id = tag_info["id"]
                tag_name = tag_info["name"]
            else:
                self.logger.error(f"无法识别的标签数据格式: {tag_info}")
                return {"status": "error", "message": "无法识别的标签数据格式"}
            
            self.logger.info(f"解析标签数据: id={tag_id}, name={tag_name}")
            result = self.engine.update_tag_vector(tag_id, tag_name)
            return {"status": "success", "result": result}
        except Exception as e:
            error_msg = f"更新标签向量失败: {str(e)}"
            self.logger.error(error_msg)
            return {"status": "error", "message": error_msg}
    
    def remove_tag(self, tag_id):
        """删除标签向量
        
        Args:
            tag_id: 标签ID
            
        Returns:
            删除结果
        """
        try:
            self.logger.info(f"删除标签向量: id={tag_id}")
            result = self.engine.remove_tag_vector(tag_id)
            return {"status": "success", "result": result}
        except Exception as e:
            error_msg = f"删除标签向量失败: {str(e)}"
            self.logger.error(error_msg)
            return {"status": "error", "message": error_msg}
    
    def start(self):
        """启动 EPC 服务器 - 简化版本"""
        try:
            # 获取分配的端口
            port = self.server.server_address[1]
            self.logger.info(f"服务器已创建，端口号: {port}")
            
            # 输出端口信息到标准输出和错误输出，以便 Emacs 端捕获
            print(f"EPC_PORT:{port}", flush=True)
            print(f"EPC_PORT:{port}", file=sys.stderr, flush=True)
            self.logger.info("端口信息已输出")
            
            # 记录环境变量
            self.logger.info("环境变量:")
            for key in ["ORG_SUPERTAG_DATA_DIRECTORY", "PYTHONPATH", "VIRTUAL_ENV", "PATH"]:
                value = os.environ.get(key, "未设置")
                self.logger.info(f"  {key} = {value}")
            
            # 简化服务器启动 - 直接在主线程运行
            self.logger.info("开始运行服务...")
            self.server.serve_forever()
            
        except KeyboardInterrupt:
            self.logger.info("收到中断信号，服务器关闭中...")
            self.shutdown()
        except Exception as e:
            error_msg = f"服务启动失败: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            print(f"❌ {error_msg}", file=sys.stderr, flush=True)
            raise
    
    def shutdown(self):
        """安全关闭服务器 - 简化版本"""
        if self.server:
            self.logger.info("关闭 EPC 服务器...")
            self.server.shutdown()
            self.server.server_close()
            self.logger.info("EPC 服务器已关闭")


def main():
    """主入口点"""
    try:
        # 设置日志
        logger, data_dir, log_file = setup_logging()
        logger.info("SimTag EPC Server 启动中...")
        logger.info(f"使用日志文件路径: {log_file}")
        
        vector_db_path = os.path.join(data_dir, 'supertag_vector.db')
        logger.info(f"使用向量数据库路径: {vector_db_path}")
        
        # 创建配置
        from simtag.config import Config
        config = Config(vector_db_path=vector_db_path)
        
        # 创建并启动服务器
        server = SimTagEPCServer(config)
        logger.info("EPC服务器已创建，开始启动...")
        server.start()
        
    except Exception as e:
        error_msg = f"启动失败: {str(e)}"
        print(f"❌ {error_msg}", file=sys.stderr, flush=True)
        logging.error(error_msg)
        logging.error(traceback.format_exc())
        sys.exit(1)


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""
SimTag EPC服务器 - 提供标签相似度、提取实体和生成标签的常驻服务
使用EPC (Emacs RPC)与Emacs通信
"""

import os
import sys
import argparse

# 添加当前目录到Python路径
script_dir = os.path.dirname(os.path.realpath(__file__))
if script_dir not in sys.path:
    sys.path.insert(0, script_dir)

# 尝试导入SimTag模块
try:
    from simtag.config import Config
    from simtag.epc_server import main as server_main
except ImportError as e:
    # 如果导入失败，尝试展示更友好的错误信息
    print(f"导入SimTag模块失败: {e}")
    print("请确保已安装所有依赖:")
    print("uv pip install epc sentence-transformers torch numpy requests")
    sys.exit(1)

def main():
    """主函数"""
    try:
        # 设置环境变量表明是EPC模式
        os.environ["SIMTAG_EPC_MODE"] = "1"
        
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
        
        # 确保所有输出都被刷新
        sys.stdout.flush()
        sys.stderr.flush()
        
        # 调用服务器主函数
        server_main(config)
        
    except Exception as e:
        # 确保错误信息写入stderr
        print(f"启动服务器失败: {e}", file=sys.stderr, flush=True)
        sys.exit(1)

if __name__ == "__main__":
    main()
#!/usr/bin/env python3
"""
配置管理工具
用于动态调整 org-supertag 的配置选项
"""

import sys
from typing import Dict, Any
from simtag.config import Config

def show_current_config():
    """显示当前配置"""
    config = Config()
    embedding_config = config.embedding_config
    
    print("=== 当前嵌入配置 ===")
    print(f"后端: {embedding_config.get('backend', 'llama_cpp')}")
    print(f"长文本策略: {embedding_config.get('long_text_strategy', 'truncate')}")
    print(f"分块大小: {embedding_config.get('chunk_size', 400)} 字符")
    print(f"分块重叠: {embedding_config.get('chunk_overlap', 50)} 字符")
    print(f"聚合方法: {embedding_config.get('chunk_aggregation', 'mean')}")
    print(f"最大分块数: {embedding_config.get('max_chunks', 10)}")
    print(f"上下文长度: {embedding_config.get('llama_cpp_max_context', 512)}")
    print(f"池化策略: {embedding_config.get('llama_cpp_pooling', 'cls')}")

def set_long_text_strategy(strategy: str):
    """设置长文本处理策略"""
    if strategy not in ['truncate', 'chunk']:
        print(f"错误: 无效的策略 '{strategy}'. 可选: 'truncate', 'chunk'")
        return False
    
    config = Config()
    config.embedding_config['long_text_strategy'] = strategy
    
    print(f"✅ 长文本策略已设置为: {strategy}")
    
    if strategy == 'chunk':
        print("📝 分块策略说明:")
        print("  - 长文本会被分割成多个块")
        print("  - 每个块单独生成嵌入")
        print("  - 最终结果通过聚合得到")
        print("  - 保留完整信息但处理时间较长")
    else:
        print("📝 截断策略说明:")
        print("  - 长文本会被截断到上下文长度")
        print("  - 处理速度快但可能丢失信息")
    
    return True

def set_chunk_config(size: int = None, overlap: int = None, aggregation: str = None, max_chunks: int = None):
    """设置分块配置"""
    config = get_config()
    
    if size is not None:
        if size < 100 or size > 1000:
            print("错误: 分块大小应在 100-1000 之间")
            return False
        config.embedding_config['chunk_size'] = size
        print(f"✅ 分块大小设置为: {size}")
    
    if overlap is not None:
        if overlap < 0 or overlap > 200:
            print("错误: 分块重叠应在 0-200 之间")
            return False
        config.embedding_config['chunk_overlap'] = overlap
        print(f"✅ 分块重叠设置为: {overlap}")
    
    if aggregation is not None:
        if aggregation not in ['mean', 'weighted_mean', 'max_pool']:
            print("错误: 聚合方法应为 'mean', 'weighted_mean', 'max_pool' 之一")
            return False
        config.embedding_config['chunk_aggregation'] = aggregation
        print(f"✅ 聚合方法设置为: {aggregation}")
    
    if max_chunks is not None:
        if max_chunks < 1 or max_chunks > 50:
            print("错误: 最大分块数应在 1-50 之间")
            return False
        config.embedding_config['max_chunks'] = max_chunks
        print(f"✅ 最大分块数设置为: {max_chunks}")
    
    return True

def main():
    """主函数"""
    if len(sys.argv) < 2:
        print("使用方法:")
        print("  python config_manager.py show                    # 显示当前配置")
        print("  python config_manager.py strategy <truncate|chunk>  # 设置长文本策略")
        print("  python config_manager.py chunk --size 400 --overlap 50 --aggregation mean --max-chunks 10")
        print("")
        print("示例:")
        print("  python config_manager.py strategy chunk         # 启用分块策略")
        print("  python config_manager.py strategy truncate      # 启用截断策略")
        print("  python config_manager.py chunk --size 300       # 设置分块大小为300字符")
        return
    
    command = sys.argv[1]
    
    if command == "show":
        show_current_config()
    
    elif command == "strategy":
        if len(sys.argv) < 3:
            print("错误: 请指定策略 (truncate 或 chunk)")
            return
        strategy = sys.argv[2]
        set_long_text_strategy(strategy)
    
    elif command == "chunk":
        # 解析参数
        args = sys.argv[2:]
        size = None
        overlap = None
        aggregation = None
        max_chunks = None
        
        i = 0
        while i < len(args):
            if args[i] == "--size" and i + 1 < len(args):
                size = int(args[i + 1])
                i += 2
            elif args[i] == "--overlap" and i + 1 < len(args):
                overlap = int(args[i + 1])
                i += 2
            elif args[i] == "--aggregation" and i + 1 < len(args):
                aggregation = args[i + 1]
                i += 2
            elif args[i] == "--max-chunks" and i + 1 < len(args):
                max_chunks = int(args[i + 1])
                i += 2
            else:
                print(f"未知参数: {args[i]}")
                return
        
        set_chunk_config(size, overlap, aggregation, max_chunks)
    
    else:
        print(f"未知命令: {command}")

if __name__ == "__main__":
    main() 
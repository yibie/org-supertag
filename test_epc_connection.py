#!/usr/bin/env python3
"""
测试EPC服务器连接脚本
用于检查SimTag EPC服务器是否正常运行并能被连接
"""

import sys
import os
import time
import socket
from epc.client import EPCClient

# 配置
PORT = 21278  # 默认端口，可通过命令行参数覆盖
TIMEOUT = 5  # 连接超时时间（秒）

def check_port_open(port, host='127.0.0.1'):
    """检查端口是否开放"""
    print(f"检查端口 {port} 是否开放...")
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(2)
        result = sock.connect_ex((host, port))
        sock.close()
        if result == 0:
            print(f"端口 {port} 已开放")
            return True
        else:
            print(f"端口 {port} 未开放 (错误码: {result})")
            return False
    except socket.error as e:
        print(f"检查端口时发生错误: {e}")
        return False

def test_echo(port, message="test", host='127.0.0.1'):
    """测试EPC服务器的echo功能"""
    print(f"连接到 {host}:{port}...")
    try:
        # 创建EPC客户端
        client = EPCClient((host, port))
        
        print(f"连接成功，尝试调用echo...")
        start_time = time.time()
        
        # 调用echo方法
        response = client.call_sync("echo", [message])
        
        elapsed = time.time() - start_time
        print(f"调用echo成功，耗时: {elapsed:.2f}秒")
        print(f"响应: {response}")
        
        # 关闭连接
        client.close()
        return True
    except Exception as e:
        print(f"连接或调用失败: {e}")
        return False

def test_extract_entities(port, text="测试文本", host='127.0.0.1'):
    """测试实体提取功能"""
    print(f"测试实体提取...")
    try:
        # 创建EPC客户端
        client = EPCClient((host, port))
        
        # 调用extract_entities方法
        start_time = time.time()
        response = client.call_sync("extract_entities", [text])
        elapsed = time.time() - start_time
        
        print(f"调用成功，耗时: {elapsed:.2f}秒")
        
        if isinstance(response, dict) and "data" in response:
            entities = response["data"]
            print(f"提取到 {len(entities)} 个实体:")
            for entity in entities[:3]:  # 只打印前3个
                print(f"  - {entity}")
            if len(entities) > 3:
                print(f"  - ... 等 {len(entities)-3} 个")
        else:
            print(f"响应格式不符合预期: {response}")
        
        # 关闭连接
        client.close()
        return True
    except Exception as e:
        print(f"调用实体提取功能失败: {e}")
        return False

def get_server_status(port, host='127.0.0.1'):
    """获取服务器状态"""
    print(f"获取服务器状态...")
    try:
        client = EPCClient((host, port))
        response = client.call_sync("get_server_status", [])
        client.close()
        
        if isinstance(response, dict) and "data" in response:
            status = response["data"]
            print(f"服务器状态:")
            
            if "server" in status:
                server_info = status["server"]
                print(f"  - 端口: {server_info.get('port')}")
                print(f"  - 地址: {server_info.get('address')}")
            
            if "system" in status:
                system_info = status["system"]
                print(f"  - 平台: {system_info.get('platform')}")
                print(f"  - Python版本: {system_info.get('python_version')}")
            
            if "process" in status:
                process_info = status["process"]
                print(f"  - 进程ID: {process_info.get('pid')}")
            
            return True
        else:
            print(f"获取状态失败，响应格式不符合预期: {response}")
            return False
    except Exception as e:
        print(f"获取服务器状态失败: {e}")
        return False

def main():
    """主函数"""
    # 解析命令行参数
    if len(sys.argv) > 1:
        try:
            port = int(sys.argv[1])
        except ValueError:
            print(f"错误: 无效的端口号 '{sys.argv[1]}'")
            print(f"使用默认端口 {PORT}")
            port = PORT
    else:
        port = PORT
    
    print(f"\n{'='*50}")
    print(f"SimTag EPC 连接测试")
    print(f"{'='*50}\n")
    
    print(f"使用端口: {port}")
    
    # 检查端口是否开放
    if not check_port_open(port):
        print("端口未开放，服务器可能未启动")
        return False
    
    # 测试echo功能
    if not test_echo(port):
        print("Echo测试失败，服务器可能未正常运行")
        return False
    
    # 获取服务器状态
    if not get_server_status(port):
        print("获取服务器状态失败")
    
    # 测试实体提取功能
    if not test_extract_entities(port, "这是一个测试文本，用于测试实体提取功能。"):
        print("实体提取测试失败")
    
    print("\n所有测试完成！")
    return True

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1) 
#!/usr/bin/env python3
"""
嵌入调试工具
用于分析和识别导致嵌入失败的文本内容
"""

import asyncio
import logging
import sys
from typing import List, Dict, Any
from simtag.services.embedding_service import get_embedding_service

# 设置日志格式
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

def analyze_text_properties(text: str) -> Dict[str, Any]:
    """分析文本属性"""
    return {
        "length": len(text),
        "stripped_length": len(text.strip()),
        "is_empty": not text.strip(),
        "has_special_chars": any(ord(c) > 127 for c in text),
        "has_control_chars": any(ord(c) < 32 and c not in '\t\n\r' for c in text),
        "has_whitespace_padding": text != text.strip(),
        "starts_with_whitespace": text != text.lstrip(),
        "ends_with_whitespace": text != text.rstrip(),
        "char_types": {
            "ascii": sum(1 for c in text if ord(c) < 128),
            "unicode": sum(1 for c in text if ord(c) >= 128),
            "control": sum(1 for c in text if ord(c) < 32 and c not in '\t\n\r'),
            "whitespace": sum(1 for c in text if c.isspace())
        },
        "preview": repr(text[:100]) + ("..." if len(text) > 100 else "")
    }

async def debug_single_text(text: str, service) -> Dict[str, Any]:
    """调试单个文本"""
    analysis = analyze_text_properties(text)
    
    # 尝试获取嵌入
    result = await service.get_embedding(text)
    
    return {
        "text_analysis": analysis,
        "embedding_result": {
            "success": result.success,
            "dimension": result.dimension if result.success else None,
            "backend_used": getattr(result, 'backend_used', 'unknown'),
            "error_message": result.error_message if not result.success else None,
            "processing_time": result.processing_time
        }
    }

async def debug_text_batch(texts: List[str]) -> None:
    """调试一批文本"""
    service = get_embedding_service()
    
    print("=" * 80)
    print("嵌入调试报告")
    print("=" * 80)
    
    successful = 0
    failed = 0
    problems = []
    
    for i, text in enumerate(texts):
        print(f"\n--- 文本 {i+1} ---")
        print(f"内容预览: {repr(text[:50])}{'...' if len(text) > 50 else ''}")
        
        debug_info = await debug_single_text(text, service)
        analysis = debug_info["text_analysis"]
        result = debug_info["embedding_result"]
        
        # 显示分析结果
        print(f"长度: {analysis['length']} (去空格后: {analysis['stripped_length']})")
        print(f"字符类型: ASCII={analysis['char_types']['ascii']}, Unicode={analysis['char_types']['unicode']}, 控制字符={analysis['char_types']['control']}")
        
        if result["success"]:
            successful += 1
            print(f"✅ 成功: 后端={result['backend_used']}, 维度={result['dimension']}, 时间={result['processing_time']:.3f}s")
        else:
            failed += 1
            print(f"❌ 失败: {result['error_message']}")
            
            # 收集问题文本信息
            problems.append({
                "index": i + 1,
                "text_preview": analysis["preview"],
                "analysis": analysis,
                "error": result["error_message"]
            })
    
    # 总结报告
    print("\n" + "=" * 80)
    print("总结报告")
    print("=" * 80)
    print(f"总计: {len(texts)} 个文本")
    print(f"成功: {successful} 个 ({successful/len(texts)*100:.1f}%)")
    print(f"失败: {failed} 个 ({failed/len(texts)*100:.1f}%)")
    
    if problems:
        print(f"\n--- 问题文本分析 ---")
        
        # 分类问题
        empty_texts = [p for p in problems if p["analysis"]["is_empty"]]
        control_char_texts = [p for p in problems if p["analysis"]["has_control_chars"]]
        long_texts = [p for p in problems if p["analysis"]["length"] > 400]
        other_texts = [p for p in problems if p not in empty_texts + control_char_texts + long_texts]
        
        if empty_texts:
            print(f"\n空/空白文本 ({len(empty_texts)} 个):")
            for p in empty_texts[:3]:  # 只显示前3个
                print(f"  - 文本 {p['index']}: {p['text_preview']}")
        
        if control_char_texts:
            print(f"\n包含控制字符的文本 ({len(control_char_texts)} 个):")
            for p in control_char_texts[:3]:
                print(f"  - 文本 {p['index']}: {p['text_preview']}")
        
        if long_texts:
            print(f"\n超长文本 ({len(long_texts)} 个):")
            for p in long_texts[:3]:
                print(f"  - 文本 {p['index']}: 长度 {p['analysis']['length']}")
        
        if other_texts:
            print(f"\n其他问题 ({len(other_texts)} 个):")
            for p in other_texts[:3]:
                print(f"  - 文本 {p['index']}: {p['error']}")

def main():
    """主函数"""
    if len(sys.argv) < 2:
        print("使用方法:")
        print("  python debug_embedding.py '要测试的文本'")
        print("  python debug_embedding.py '文本1' '文本2' '文本3' ...")
        print("  echo '文本内容' | python debug_embedding.py")
        return
    
    if sys.argv[1:]:
        # 从命令行参数读取文本
        texts = sys.argv[1:]
    else:
        # 从标准输入读取
        texts = [line.rstrip('\n\r') for line in sys.stdin]
    
    asyncio.run(debug_text_batch(texts))

if __name__ == "__main__":
    main() 
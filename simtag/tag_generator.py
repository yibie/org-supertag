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
            
            # 检查文本编码，确保是有效的UTF-8编码
            try:
                # 尝试编码/解码文本，确保文本是有效的 UTF-8
                if isinstance(text, str):
                    text_bytes = text.encode('utf-8')
                    text = text_bytes.decode('utf-8')
            except UnicodeError as e:
                print(f"[WARNING] 文本编码有问题: {e}")
                self.logger.warning(f"文本编码有问题: {e}")
                # 尝试修复编码问题
                try:
                    if isinstance(text, str):
                        text = text.encode('utf-8', errors='replace').decode('utf-8')
                except Exception as e:
                    print(f"[ERROR] 无法修复文本编码: {e}")
                    self.logger.error(f"无法修复文本编码: {e}")
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

            # 确保文本是一个有效的字符串，并删除可能导致问题的特殊字符
            cleaned_text = text.strip()
            if len(cleaned_text) < 10:  # 如果文本太短，可能不足以提取有用的标签
                print(f"[WARNING] 文本内容太短: '{cleaned_text}'")
                self.logger.warning(f"文本内容太短: '{cleaned_text}'")
                # 对于短文本，直接返回其本身作为标签
                if cleaned_text:
                    return [cleaned_text]
                return []

            # 修改提示，确保文本内容被明确标记
            prompt = f"""Extract 5 significant tags from the following text:

TEXT START
{cleaned_text}
TEXT END

Return ONLY a comma-separated list of tags, with no explanations or other text.
Example format: tag1, tag2, tag3, tag4, tag5"""

            # 添加system变量的定义
            system = """You are a tag generation expert. Your task is to generate relevant tags for the given text.
Guidelines:
1. Each tag should be concise and accurate
2. Tags should reflect the main topics and concepts in the text
3. Return ONLY a comma-separated list of tags
4. Do not include any explanations or other text in your response"""

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
                # 避免打印过长的提示，只打印前200个字符和最后100个字符
                if len(prompt) > 300:
                    print(f"[DEBUG] 用户提示(截断): {prompt[:200]}...{prompt[-100:]}")
                else:
                    print(f"[DEBUG] 用户提示: {prompt}")
                self.logger.info(f"用户提示长度: {len(prompt)} 字符")
                
                # 确认文本是否实际包含在提示中
                text_in_prompt = "TEXT START" in prompt and "TEXT END" in prompt
                print(f"[DEBUG] 文本已正确包含在提示中: {text_in_prompt}")
                self.logger.info(f"文本已正确包含在提示中: {text_in_prompt}")
                
                # 调用Ollama并记录详细信息
                print(f"[DEBUG] 开始调用Ollama.run()，提示长度: {len(prompt)}")
                response = self.ollama.run(prompt, system=system)
                print(f"[DEBUG] Ollama.run()完成，响应长度: {len(response) if response else 0}")
                
                # 打印原始响应以便调试
                raw_response = response if response else "无响应"
                if len(raw_response) > 200:
                    print(f"[DEBUG] Ollama原始响应(截断): {raw_response[:200]}...")
                else:
                    print(f"[DEBUG] Ollama原始响应: '{raw_response}'")
                self.logger.info(f"收到Ollama响应，长度: {len(raw_response)}")
                
                # 识别并处理特殊响应情况
                lower_response = raw_response.lower() if raw_response else ""
                special_phrases = [
                    "please provide", "i need", "please give", 
                    "the text is empty", "no text provided", "i don't see any text",
                    "cannot generate", "unable to generate"
                ]
                
                is_error_response = any(phrase in lower_response for phrase in special_phrases)
                if is_error_response:
                    print(f"[WARNING] Ollama 返回了错误响应: '{raw_response}'")
                    self.logger.warning(f"Ollama 返回了错误响应: '{raw_response}'")
                    
                    # 尝试再次请求，提供更明确的说明
                    print("[INFO] 尝试使用备用提示再次请求...")
                    self.logger.info("尝试使用备用提示再次请求...")
                    
                    # 备用提示更加简单直接
                    backup_prompt = f"Generate 5 tags for this text: {cleaned_text[:1000]}"
                    print(f"[DEBUG] 备用提示: {backup_prompt[:200]}...")
                    
                    try:
                        backup_response = self.ollama.run(backup_prompt, system=system)
                        if backup_response and not any(phrase in backup_response.lower() for phrase in special_phrases):
                            print(f"[INFO] 备用请求成功: '{backup_response}'")
                            self.logger.info("备用请求成功")
                            response = backup_response
                        else:
                            print("[WARNING] 备用请求也失败了")
                            self.logger.warning("备用请求也失败了")
                            return []
                    except Exception as e:
                        print(f"[ERROR] 备用请求失败: {e}")
                        self.logger.error(f"备用请求失败: {e}")
                        return []
                    
                # 如果仍然没有有效响应，返回空列表
                if not response:
                    print("[WARNING] Ollama 返回空响应")
                    self.logger.warning("Ollama 返回空响应")
                    return []
                
                # 增强的标签提取逻辑
                # 首先尝试直接按逗号分割
                raw_tags = [tag.strip() for tag in response.split(',')]
                
                # 如果只有一个元素，可能是响应格式不正确
                if len(raw_tags) <= 1:
                    print(f"[WARNING] 响应格式可能不正确，尝试其他分割方式")
                    self.logger.warning("响应格式可能不正确，尝试其他分割方式")
                    
                    # 首先尝试按换行符分割
                    line_tags = []
                    for line in response.split('\n'):
                        line = line.strip()
                        # 跳过空行和明显的非标签行
                        if not line or len(line) > 100:
                            continue
                        # 检查是否是列表项（以数字或连字符开头）
                        if line.startswith(('-', '*', '1.', '2.', '3.', '4.', '5.')):
                            # 移除列表标记
                            line = line.lstrip('-*0123456789. ')
                        # 如果行里有逗号，可能是多个标签
                        if ',' in line:
                            line_tags.extend([t.strip() for t in line.split(',')])
                        else:
                            line_tags.append(line)
                    
                    if line_tags:
                        print(f"[DEBUG] 使用换行分割提取到 {len(line_tags)} 个标签")
                        self.logger.info(f"使用换行分割提取到 {len(line_tags)} 个标签")
                        raw_tags = line_tags
                    else:
                        # 如果换行分割也失败了，尝试更激进的分割方法
                        # 寻找可能的标签模式，如带引号的内容或冒号后的内容
                        # import re  # 移除此处的re导入，确保使用全局导入的re
                        # 寻找引号中的内容或冒号后的内容作为可能的标签
                        potential_tags = re.findall(r'"([^"]+)"|\'([^\']+)\'|:\s*([^,\n]+)', response)
                        
                        extracted_tags = []
                        for tag_tuple in potential_tags:
                            # findall 会返回元组，取非空值
                            tag = next((t for t in tag_tuple if t), None)
                            if tag:
                                extracted_tags.append(tag.strip())
                        
                        if extracted_tags:
                            print(f"[DEBUG] 使用正则表达式提取到 {len(extracted_tags)} 个标签")
                            self.logger.info(f"使用正则表达式提取到 {len(extracted_tags)} 个标签")
                            raw_tags = extracted_tags
                        elif raw_tags[0]:  # 如果只有一个元素且不为空
                            # 使用原始单一元素作为唯一标签
                            print(f"[DEBUG] 使用原始响应作为单一标签: {raw_tags[0]}")
                            self.logger.info(f"使用原始响应作为单一标签: {raw_tags[0]}")
                        else:
                            print("[WARNING] 无法从响应中提取标签")
                            self.logger.warning("无法从响应中提取标签")
                            return []
                
                print(f"[DEBUG] 原始标签列表 ({len(raw_tags)}): {raw_tags}")
                self.logger.info(f"原始标签列表 ({len(raw_tags)}): {raw_tags}")
                
                # 清理和筛选标签
                valid_tags = []
                for tag in raw_tags:
                    # 跳过明显无效的标签
                    if not tag or len(tag) > 50:
                        print(f"[DEBUG] 跳过无效标签: '{tag}'")
                        continue
                        
                    # 标准化标签 (小写，移除多余空格和标点)
                    clean_tag = tag.strip().lower()
                    # 移除引号和括号等可能的标记
                    clean_tag = re.sub(r'[\'"`\(\)\[\]\{\}]', '', clean_tag)
                    # 移除开头的数字和点
                    clean_tag = re.sub(r'^[\d\.\-\s]+', '', clean_tag)
                    
                    if clean_tag and clean_tag not in valid_tags:
                        valid_tags.append(clean_tag)
                    else:
                        print(f"[DEBUG] 忽略重复或空标签: '{tag}'")
                
                # 确保我们有标签
                if not valid_tags:
                    print("[WARNING] 没有提取到有效标签")
                    self.logger.warning("没有提取到有效标签")
                    # 如果无法提取标签，尝试使用第一行作为标签
                    first_line = response.split('\n')[0].strip()
                    if first_line and len(first_line) <= 50:
                        print(f"[INFO] 使用响应第一行作为标签: '{first_line}'")
                        valid_tags = [first_line.lower()]
                    else:
                        return []
                
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
#!/usr/bin/env python3
import requests
import json
import os
import sys
import platform
import subprocess
from typing import List, Dict, Any, Optional
import time
import re

class OllamaServiceManager:
    """管理 Ollama 服务的类"""
    
    # 添加类变量记录已验证的模型
    _verified_models = set()
    
    def __init__(self):
        """初始化服务管理器"""
        self.model = "hf.co/unsloth/gemma-3-4b-it-GGUF:latest"
        # 默认静默模式
        self.quiet = os.environ.get("OLLAMA_SKIP_OUTPUT", "1") == "1"
    
    def ensure_service_ready(self) -> bool:
        """确保 Ollama 服务已准备就绪
        
        Returns:
            服务是否就绪
        """
        # 如果模型已验证过，直接返回成功
        if self.model in OllamaServiceManager._verified_models:
            # 不再输出验证消息
            return True
            
        try:
            if not self.quiet:
                print(f"正在下载模型 {self.model}...")
                
            # 尝试下载模型
            response = requests.post(
                "http://localhost:11434/api/pull",
                json={"name": self.model},
                stream=True
            )
            
            for line in response.iter_lines():
                if line and not self.quiet:
                    data = json.loads(line)
                    if "status" in data:
                        print(data["status"], end=" ")
                        if "completed" in data:
                            print()
                    if "error" in data:
                        print(f"\nError: {data['error']}")
                        return False
            
            # 添加到已验证模型集合
            OllamaServiceManager._verified_models.add(self.model)
            if not self.quiet:
                print("Ollama 服务就绪")
            return True
            
        except Exception as e:
            if not self.quiet:
                print(f"Error: {e}")
            return False
    
    @staticmethod
    def check_ollama_installed() -> bool:
        """检查 Ollama 是否已安装"""
        if platform.system() == "Windows":
            # Windows 下检查多个可能的位置
            possible_paths = [
                r"C:\Program Files\Ollama\ollama.exe",
                r"C:\Program Files (x86)\Ollama\ollama.exe",
                os.path.expanduser("~\\AppData\\Local\\Programs\\Ollama\\ollama.exe"),
                os.path.expanduser("~\\scoop\\apps\\ollama\\current\\ollama.exe"),
            ]
            
            # 通过 PATH 环境变量查找
            if os.environ.get("PATH"):
                for path in os.environ["PATH"].split(os.pathsep):
                    possible_paths.append(os.path.join(path, "ollama.exe"))
                    
            # 检查所有可能的路径
            return any(os.path.exists(path) for path in possible_paths)
        else:
            # Unix 系统检查 PATH
            return bool(subprocess.run(
                ["which", "ollama"], 
                capture_output=True
            ).returncode == 0)
    
    @staticmethod
    def get_install_command() -> str:
        """获取安装命令"""
        system = platform.system().lower()
        if system == "darwin":  # macOS
            return "curl -fsSL https://ollama.com/install.sh | sh"
        elif system == "linux":
            return "curl -fsSL https://ollama.com/install.sh | sh"
        elif system == "windows":
            return """Windows 安装选项:
1. 使用 winget (推荐):
   winget install Ollama.Ollama

2. 使用 Scoop:
   scoop bucket add main
   scoop install ollama

3. 直接下载安装包:
   访问 https://ollama.com/download
"""
        else:
            raise NotImplementedError(f"Unsupported system: {system}")
            
    @staticmethod
    def is_service_running() -> bool:
        """检查 Ollama 服务是否运行"""
        try:
            response = requests.get("http://127.0.0.1:11434/api/tags")
            return response.status_code == 200
        except:
            return False
            
    @staticmethod
    def start_service():
        """启动 Ollama 服务"""
        system = platform.system().lower()
        if system in ["darwin", "linux"]:
            subprocess.Popen(
                ["ollama", "serve"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE
            )
        elif system == "windows":
            subprocess.Popen(
                ["ollama.exe", "serve"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE
            )
            
    @staticmethod
    def ensure_model_exists(model_name: str) -> bool:
        """确保模型已下载"""
        # 如果模型已验证过，直接返回成功
        if model_name in OllamaServiceManager._verified_models:
            return True
            
        try:
            response = requests.get(f"http://127.0.0.1:11434/api/show?name={model_name}")
            result = response.status_code == 200
            
            # 如果成功验证，添加到已验证模型集合
            if result:
                OllamaServiceManager._verified_models.add(model_name)
                
            return result
        except:
            return False
            
    @staticmethod
    def pull_model(model_name: str):
        """下载模型"""
        subprocess.run(["ollama", "pull", model_name], check=True)

class OllamaBridge:
    """Ollama API 集成，用于标签推荐"""
    
    # 添加类变量用于缓存初始化状态
    _initialized = False
    
    def __init__(self, model: str = "hf.co/unsloth/gemma-3-4b-it-GGUF:latest", host: str = "http://127.0.0.1:11434"):
        """初始化 Ollama 客户端
        
        Args:
            model: 使用的模型名称
            host: Ollama 服务地址
        """
        self.model = model
        self.host = host
        # 默认静默模式
        self.quiet = os.environ.get("OLLAMA_SKIP_OUTPUT", "1") == "1"
        
        # 仅在首次初始化时检查服务
        if not OllamaBridge._initialized:
            self._ensure_service()
            OllamaBridge._initialized = True
        
    def _ensure_service(self):
        """确保 Ollama 服务可用"""
        service_mgr = OllamaServiceManager()
        service_mgr.quiet = self.quiet  # 传递静默设置
        service_mgr.model = self.model  # 设置相同的模型
        
        # 检查是否已安装
        if not service_mgr.check_ollama_installed():
            install_cmd = service_mgr.get_install_command()
            print(f"Ollama 未安装，请运行以下命令安装:\n{install_cmd}")
            raise RuntimeError(f"Ollama 未安装，请运行以下命令安装:\n{install_cmd}")
            
        # 检查服务是否运行
        if not service_mgr.is_service_running():
            print("Ollama 服务未运行，正在启动...")
            service_mgr.start_service()
            # 等待服务启动
            for i in range(5):
                print(f"等待服务启动... 尝试 {i+1}/5")
                time.sleep(2)
                if service_mgr.is_service_running():
                    print("服务已启动")
                    break
            else:
                print("服务启动失败")
                raise RuntimeError("Ollama 服务启动失败")
                
        # 检查模型是否存在 - 使用优化后的快速检查
        print(f"检查模型 {self.model} 是否存在...")
        if not service_mgr.ensure_model_exists(self.model):
            print(f"模型 {self.model} 不存在，正在下载...")
            try:
                service_mgr.ensure_service_ready()
                print(f"模型 {self.model} 已准备就绪")
            except Exception as e:
                print(f"模型准备失败: {e}")
                raise RuntimeError(f"模型准备失败: {e}")
                
        print("Ollama 服务就绪")
            
    def generate(self, prompt: str, system: str = "", **kwargs) -> str:
        """生成文本
        
        Args:
            prompt: 提示词
            system: 系统提示词
            **kwargs: 其他参数
            
        Returns:
            生成的文本
        """
        url = f"{self.host}/api/generate"
        data = {
            "model": self.model,
            "prompt": prompt,
            "system": system,
            **kwargs
        }
        
        try:
            response = requests.post(url, json=data, stream=True)
            response.raise_for_status()
            
            # 收集所有响应
            full_response = ""
            for line in response.iter_lines():
                if line:
                    try:
                        json_response = json.loads(line)
                        if "response" in json_response:
                            full_response += json_response["response"]
                    except json.JSONDecodeError:
                        print(f"Warning: Failed to parse response line: {line}")
                        continue
                        
            return full_response.strip()
            
        except Exception as e:
            print(f"Error generating text: {e}")
            return ""
            
    def suggest_tags(self, text: str) -> List[str]:
        """根据文本内容提取关键词作为标签
        
        Args:
            text: 需要分析的文本内容
            
        Returns:
            提取的标签列表
        """
        system = """You are a professional book indexer with expertise in extracting meaningful concepts from text. Your task is to identify the most significant concepts and terms from the given text that readers would likely search for.

Approach indexing as a professional would:
1. Identify key concepts, not just surface-level keywords
2. Consider hierarchical relationships between concepts
3. Recognize different expressions of the same concept
4. Focus on what readers would want to find
5. Include proper nouns, technical terms, and domain-specific vocabulary

Guidelines for creating tags:
1. Extract 3-5 most significant concepts from the text
2. Preserve the meaning and integrity of conceptual phrases
3. Use lowercase letters, numbers, and underscores for formatting
4. Connect multi-word concepts with underscores (e.g. cognitive_dissonance)
5. Prioritize conceptual significance over word frequency

Output format:
- Return ONLY a comma-separated list of tags
- No other text or explanations
- Example: quantum_mechanics, heisenberg_uncertainty, wave_particle_duality"""

        prompt = f"""As a professional indexer, identify 3-5 significant concepts from this text that a reader would likely search for:

{text}

Remember:
- Focus on the core concepts, not just keywords
- Consider what concepts would be most useful for retrieval
- Return ONLY a comma-separated list of concepts as tags
- Format with lowercase and underscores for multi-word concepts"""

        try:
            response = self.generate(prompt, system)
            
            # 清理和格式化标签
            tags = []
            for tag in response.strip().split(','):
                # 清理标签
                tag = tag.strip().lower()
                # 移除特殊字符（保留下划线）
                tag = re.sub(r'[^\w\s_]', '', tag)
                # 将空格和连字符替换为下划线
                tag = re.sub(r'[\s-]+', '_', tag)
                # 移除多余的下划线
                tag = re.sub(r'_+', '_', tag)
                # 移除首尾的下划线
                tag = tag.strip('_')
                
                # 验证标签长度和格式
                if tag and len(tag) <= 50 and re.match(r'^[a-z0-9_]+$', tag):
                    tags.append(tag)
            
            # 如果没有有效标签，重试一次
            if not tags:
                print("No valid tags found, retrying...")
                return self.suggest_tags(text)
                
            return list(set(tags))  # 移除重复
            
        except Exception as e:
            print(f"Error suggesting tags: {e}")
            return []
        
    def analyze_tag_relations(self, tag: str, tags: List[str]) -> List[Dict[str, Any]]:
        """分析标签间的关系
        
        Args:
            tag: 目标标签
            tags: 待分析的标签列表
            
        Returns:
            标签关系列表，每个元素包含标签名和关系类型
        """
        # 预定义关系类型
        RELATIONS = {
            "CONTRAST": "比较或对比关系",
            "RELATE": "一般关联关系",
            "INFLUENCE": "影响关系",
            "CONTAIN": "包含关系(父)",
            "BELONG": "从属关系(子)",
            "PARALLEL": "并行关系",
            "DEPENDENCY": "依赖关系",
            "CAUSE": "因果关系(因)",
            "EFFECT": "因果关系(果)",
            "COOCCURRENCE": "共现关系"
        }
        
        system = """You are a tag relationship analyzer. Your task is to determine the relationship between two tags.

Available relationship types:
CONTRAST   - Tags represent contrasting or comparable concepts
RELATE     - Tags have a general association
INFLUENCE  - First tag has significant impact on second tag
CONTAIN    - First tag is a broader category that includes second tag
BELONG     - First tag is a subset or member of second tag
PARALLEL   - Tags represent similar-level concepts
DEPENDENCY - First tag requires or depends on second tag
CAUSE      - First tag leads to or causes second tag
EFFECT     - First tag is a result of second tag
COOCCURRENCE - Tags commonly appear together

Response format requirements:
1. Use EXACTLY this format (including newline):
   RELATION: <TYPE>
   REASON: <brief explanation>
2. Choose only ONE relationship type from the list above
3. Provide a clear, concise reason (1-2 sentences)
4. Use technical language when appropriate
5. Be specific about the relationship direction

Example response:
RELATION: BELONG
REASON: Python is a specific programming language, making it a subset of programming.

DO NOT include any other text or explanations."""

        results = []
        for related_tag in tags:
            prompt = f"""How is "{related_tag}" related to "{tag}"?

Choose ONE relationship type and explain why.
Use EXACTLY this format (including newline):
RELATION: <TYPE>
REASON: <explanation>"""

            response = self.generate(prompt, system)
            
            try:
                # 清理响应
                response = response.strip()
                
                # 检查格式
                if not ('\nREASON:' in response and response.startswith('RELATION:')):
                    print(f"Warning: Invalid response format for tag '{related_tag}', retrying...")
                    # 重试一次
                    response = self.generate(prompt, system)
                    if not ('\nREASON:' in response and response.startswith('RELATION:')):
                        print(f"Warning: Still invalid format after retry, skipping tag '{related_tag}'")
                        continue
                
                # 解析响应
                lines = response.strip().split('\n')
                relation_line = next(line for line in lines if line.startswith('RELATION:'))
                reason_line = next(line for line in lines if line.startswith('REASON:'))
                
                # 提取关系类型和原因
                relation = relation_line.split(':', 1)[1].strip().upper()
                reason = reason_line.split(':', 1)[1].strip()
                
                # 验证关系类型
                if relation not in RELATIONS:
                    print(f"Warning: Invalid relation type '{relation}' for tag '{related_tag}', retrying...")
                    continue
                
                results.append({
                    'tag': related_tag,
                    'relation': relation.lower(),  # 转换为小写以匹配 Emacs 中的符号
                    'reason': reason
                })
            except Exception as e:
                print(f"Warning: Failed to parse response for tag '{related_tag}': {e}")
                print(f"Response was:\n{response}")
                continue
                
        return results
        
    def direct_tag_generation(self, text: str) -> List[str]:
        """直接使用LLM为文本生成标签，优化中文处理
        
        Args:
            text: 待分析文本
            
        Returns:
            标签列表
        """
        if not self._initialized:
            return []
            
        try:
            # 更新提示，增强关键概念识别能力
            system = """你是一位专业的索引编纂专家，擅长为学术文献和技术文档创建精确的主题索引。你的任务是识别文本中最重要的关键术语、概念和主题，特别注意以下几点：

1. 识别专业领域的核心术语和概念，尤其是标题或章节标题中出现的关键术语
2. 提取完整的技术术语和专业概念，保持其完整性
3. 捕捉具有特定领域意义的名词短语
4. 识别文本讨论的主要对象、工具、方法和理论
5. 不要漏掉文本中明确出现的关键术语，尤其是重复出现的专业术语

提取标准：
1. 从文本中提取3-5个最关键的术语或概念
2. 准确提取原文中实际出现的完整术语
3. 保持专业术语的完整性和准确性
4. 如果是中文术语，转换为对应的英文术语，保持专业准确性
5. 重点关注标题、小标题中出现的术语，这些通常是文档的核心概念

输出格式：
- 仅返回逗号分隔的标签列表
- 多词概念用下划线连接
- 不要包含其他文本或解释
- 示例: search_engine, search_intent, information_retrieval"""

            prompt = f"""作为专业索引编纂专家，从以下文本中提取3-5个最重要的专业术语或概念作为检索标签：

{text}

请特别注意：
1. 准确提取标题或文本中明确出现的关键术语和专业概念
2. 保持术语的完整性，不要简化专业术语
3. 将多词概念用下划线连接
4. 只返回逗号分隔的标签列表，不要有其他文本"""

            response = self.generate(prompt, system)
            
            # 解析响应
            tags = []
            if response:
                # 分割并清理标签
                for tag in response.strip().split(','):
                    tag = tag.strip().lower()
                    # 清理标签，只保留字母、数字和下划线
                    tag = re.sub(r'[^\w\s_]', '', tag)
                    # 将空格和连字符替换为下划线
                    tag = re.sub(r'[\s-]+', '_', tag)
                    # 将连续多个下划线替换为单个
                    tag = re.sub(r'_+', '_', tag)
                    # 移除首尾下划线
                    tag = tag.strip('_')
                    if tag:
                        tags.append(tag)
                        
            return tags
            
        except Exception as e:
            if not self.quiet:
                print(f"Error in direct tag generation: {e}")
            return []
            
    def generate_tags(self, text: str) -> List[str]:
        """综合方法：从文本中提取关键词作为标签
        
        Args:
            text: 待分析文本
            
        Returns:
            标签列表
        """
        # 首先使用中文优化的标签提取
        chinese_tags = self.direct_tag_generation(text)
        
        # 如果标签数量足够，则直接返回
        if len(chinese_tags) >= 3:  # 最少需要3个标签
            return chinese_tags
            
        # 如果标签数量不足，使用英文方法补充
        try:
            standard_tags = self.suggest_tags(text)
            
            # 合并去重
            all_tags = list(set(chinese_tags + standard_tags))
            
            # 如果合并后的标签数量仍然不足3个，则保留所有标签
            # 否则，限制最多返回5个标签
            if len(all_tags) > 5:  # 最多返回5个标签
                return all_tags[:5]
            else:
                return all_tags
                
        except Exception as e:
            if not self.quiet:
                print(f"Error getting standard tags: {e}")
            # 如果补充标签失败，则返回原有标签
            return chinese_tags

    def extract_entities(self, text: str) -> List[Dict[str, Any]]:
        """从文本中提取命名实体
        
        Args:
            text: 待分析文本
            
        Returns:
            实体列表，每个实体包含文本、类型和置信度
        """
        system = """You are an expert in Named Entity Recognition (NER). Your task is to identify and classify named entities in the given text. Focus on these entity types:

1. PERSON - Names of people
2. ORG - Organizations, companies, institutions
3. PRODUCT - Products, software, technologies
4. CONCEPT - Technical concepts, methodologies
5. TECH - Programming languages, frameworks, tools

For each entity:
1. Extract the exact text as it appears
2. Classify its type from the above categories
3. Assign a confidence score (0.0-1.0)

Output format:
Return a JSON array of entities, each with:
- text: The exact entity text
- type: Entity type (from above list)
- confidence: Score between 0.0 and 1.0

Example output:
[
  {"text": "Python", "type": "TECH", "confidence": 0.95},
  {"text": "TensorFlow", "type": "PRODUCT", "confidence": 0.9}
]"""

        prompt = f"""Identify and classify named entities in this text:

{text}

Remember:
- Extract exact text as it appears
- Use only the specified entity types
- Assign realistic confidence scores
- Return valid JSON array of entities"""

        try:
            response = self.generate(prompt, system)
            
            # 尝试解析JSON响应
            try:
                entities = json.loads(response)
                if not isinstance(entities, list):
                    raise ValueError("Response is not a list")
                    
                # 验证和清理每个实体
                valid_entities = []
                for entity in entities:
                    if not isinstance(entity, dict):
                        continue
                        
                    # 确保所需字段存在
                    if not all(k in entity for k in ['text', 'type', 'confidence']):
                        continue
                        
                    # 验证类型
                    if entity['type'] not in ['PERSON', 'ORG', 'PRODUCT', 'CONCEPT', 'TECH']:
                        continue
                        
                    # 验证置信度
                    try:
                        conf = float(entity['confidence'])
                        if not (0 <= conf <= 1):
                            continue
                        entity['confidence'] = conf
                    except (ValueError, TypeError):
                        continue
                        
                    valid_entities.append(entity)
                    
                return valid_entities
                
            except json.JSONDecodeError:
                print(f"Warning: Invalid JSON response: {response}")
                return []
                
        except Exception as e:
            print(f"Error extracting entities: {e}")
# 文件操作函数，用于与 sim_tag.py 集成
def extract_entities_from_file(input_file, output_file, quiet=True):
    """从文件提取命名实体（为兼容性保留，但返回空结果）
    
    Args:
        input_file: 输入文件路径（包含文本）
        output_file: 输出文件路径
        quiet: 是否静默模式
    """
    try:
        # 读取输入文件
        if not quiet:
            print(f"Reading input file: {input_file}")
        with open(input_file, 'r') as f:
            input_data = json.load(f)
            
        # 解析输入数据
        if isinstance(input_data, list) and len(input_data) >= 1:
            # 保存空结果（此功能已弃用）
            with open(output_file, 'w') as f:
                json.dump([], f)
            if not quiet:
                print(f"Empty result saved to: {output_file}")
                
        else:
            if not quiet:
                print(f"Error: Invalid input data format: {input_data}")
            
    except Exception as e:
        if not quiet:
            print(f"Error in file processing: {e}")
        # 确保创建输出文件，即使发生错误
        try:
            with open(output_file, 'w') as f:
                json.dump([], f)
        except:
            pass

def suggest_tags_from_file(input_file, output_file, quiet=True):
    """从文件生成标签建议
    
    Args:
        input_file: 输入文件路径（包含文本）
        output_file: 输出文件路径
        quiet: 是否静默模式
    """
    try:
        # 读取输入文件
        if not quiet:
            print(f"Reading input file: {input_file}")
        with open(input_file, 'r') as f:
            input_data = json.load(f)
            
        # 解析输入数据
        if isinstance(input_data, list) and len(input_data) >= 1:
            text = input_data[0]  # 待分析文本
            
            # 生成标签
            bridge = OllamaBridge()
            bridge.quiet = quiet
            tags = bridge.generate_tags(text)
            
            # 保存结果
            with open(output_file, 'w') as f:
                json.dump(tags, f, indent=2)
            if not quiet:
                print(f"Tags saved to: {output_file}")
                
        else:
            if not quiet:
                print(f"Error: Invalid input data format: {input_data}")
            
    except Exception as e:
        if not quiet:
            print(f"Error suggesting tags: {e}")
        # 确保创建输出文件，即使发生错误
        try:
            with open(output_file, 'w') as f:
                json.dump([], f)
        except:
            pass

def main():
    """测试 Ollama 集成"""
    try:
        # 设置静默模式
        quiet_mode = os.environ.get("OLLAMA_SKIP_OUTPUT", "1") == "1"
        
        # 处理命令行参数
        if len(sys.argv) > 1:
            # 直接从命令行获取测试文本并生成标签
            test_text = sys.argv[1]
            bridge = OllamaBridge()
            bridge.quiet = quiet_mode
            tags = bridge.generate_tags(test_text)
            print(", ".join(tags))
            return
        
        # 初始化 Ollama 服务
        service = OllamaServiceManager()
        service.quiet = quiet_mode
        if not service.ensure_service_ready():
            print("无法启动 Ollama 服务")
            sys.exit(1)
        
        # 创建 bridge 实例
        bridge = OllamaBridge()
        bridge.quiet = quiet_mode
        
        # 测试示例 1：技术内容
        tech_text = """Python is a popular programming language for machine learning projects.
        TensorFlow and PyTorch are two common frameworks used by data scientists."""
        
        print("\n=== 测试示例 1: 技术内容 ===")
        print(f"输入: {tech_text}")
        tech_tags = bridge.generate_tags(tech_text)
        print(f"提取标签: {tech_tags}")
        
        # 测试示例 2：中文内容
        chinese_text = "输入任何关键词，只要能够在 Google 趋势里，12 月内找到它的起始点，就是新词"
        print("\n=== 测试示例 2: 中文内容 ===")
        print(f"输入: {chinese_text}")
        chinese_tags = bridge.generate_tags(chinese_text)
        print(f"提取标签: {chinese_tags}")
        
        # 测试示例 3：具有明显名词的句子
        entity_text = "Tesla CEO Elon Musk announced that SpaceX will launch Starship to Mars by 2026."
        print("\n=== 测试示例 3: 包含专有名词的内容 ===")
        print(f"输入: {entity_text}")
        entity_tags = bridge.generate_tags(entity_text)
        print(f"提取标签: {entity_tags}")
        
        # 测试标签关系分析
        if not quiet_mode:
            print("\n=== 测试标签关系分析 ===")
            main_tag = "python"
            related_tags = ["machine_learning", "tensorflow", "programming"]
            print(f"主标签: {main_tag}")
            print(f"相关标签: {related_tags}")
            
            relations = bridge.analyze_tag_relations(main_tag, related_tags)
            if relations:
                print("\n分析结果:")
                for relation in relations:
                    print(f"- {relation['tag']}: {relation['relation']} ({relation['reason']})")
            else:
                print("无法获取标签关系分析结果")
        
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main() 
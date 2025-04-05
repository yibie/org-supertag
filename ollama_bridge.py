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
    """Manage Ollama services"""
    
    # Add class variables to record verified models
    _verified_models = set()
    
    def __init__(self):
        """Initialize the service manager"""
        self.model = "hf.co/unsloth/gemma-3-4b-it-GGUF:latest"
        # Default silent mode
        self.quiet = os.environ.get("OLLAMA_SKIP_OUTPUT", "1") == "1"
    
    def ensure_service_ready(self) -> bool:
        """Ensure Ollama service is ready
        
        Returns:
            Whether the service is ready
        """
        # If the model has been verified, return success directly
        if self.model in OllamaServiceManager._verified_models:
            # No longer output verification messages
            return True
            
        try:
            if not self.quiet:
                print(f"Checking model {self.model}...")
                
            # First check if model already exists
            response = requests.get(
                "http://localhost:11434/api/tags",
                timeout=10
            )
            
            model_exists = False
            if response.status_code == 200:
                models = response.json().get("models", [])
                model_exists = any(m.get('name') == self.model for m in models)
            
            # If model doesn't exist, download it
            if not model_exists:
                if not self.quiet:
                    print(f"Downloading model {self.model}...")
                    
                # Try to download the model
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
            else:
                if not self.quiet:
                    print(f"Model {self.model} already exists")
            
            # Add to the verified model set
            OllamaServiceManager._verified_models.add(self.model)
            if not self.quiet:
                print("Ollama service ready")
            return True
            
        except Exception as e:
            if not self.quiet:
                print(f"Error: {e}")
            return False
    
    @staticmethod
    def check_ollama_installed() -> bool:
        """Check if Ollama is installed"""
        if platform.system() == "Windows":
            # Check multiple possible locations under Windows
            possible_paths = [
                r"C:\Program Files\Ollama\ollama.exe",
                r"C:\Program Files (x86)\Ollama\ollama.exe",
                os.path.expanduser("~\\AppData\\Local\\Programs\\Ollama\\ollama.exe"),
                os.path.expanduser("~\\scoop\\apps\\ollama\\current\\ollama.exe"),
            ]
            
            # Find through the PATH environment variable
            if os.environ.get("PATH"):
                for path in os.environ["PATH"].split(os.pathsep):
                    possible_paths.append(os.path.join(path, "ollama.exe"))
                    
            # Check all possible paths
            return any(os.path.exists(path) for path in possible_paths)
        else:
            # Unix system check PATH
            return bool(subprocess.run(
                ["which", "ollama"], 
                capture_output=True
            ).returncode == 0)
    
    @staticmethod
    def get_install_command() -> str:
        """Get the installation command"""
        system = platform.system().lower()
        if system == "darwin":  # macOS
            return """Please install Ollama using the following secure method:

1. Visit the official website to download: https://ollama.com/download
2. Use Homebrew: brew install ollama
3. Manual installation:
   - Download the installation script to local: curl -fsSL https://ollama.com/install.sh -o ollama_install.sh
   - Check the script content: cat ollama_install.sh
   - Confirm security and execute: sh ollama_install.sh
"""
        elif system == "linux":
            return """Please install Ollama using the following secure method:

1. Visit the official website: https://ollama.com/download
2. Use package manager (if available): apt install ollama or dnf install ollama
3. Manual installation:
   - Download the installation script to local: curl -fsSL https://ollama.com/install.sh -o ollama_install.sh
   - Check the script content: cat ollama_install.sh
   - Confirm security and execute: sh ollama_install.sh
"""
        elif system == "windows":
            return """Windows installation options:

1. Use winget (recommended):
   winget install Ollama.Ollama

2. Use Scoop:
   scoop bucket add main
   scoop install ollama

3. Directly download the installation package:
   Visit https://ollama.com/download
"""
        else:
            raise NotImplementedError(f"Unsupported system: {system}")
            
    @staticmethod
    def is_service_running() -> bool:
        """Check if the Ollama service is running"""
        try:
            response = requests.get("http://127.0.0.1:11434/api/tags")
            return response.status_code == 200
        except:
            return False
            
    @staticmethod
    def start_service():
        """Start the Ollama service"""
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
        """Ensure the model is downloaded"""
        # If the model has been verified, return success directly
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
        """Download the model"""
        subprocess.run(["ollama", "pull", model_name], check=True)

class OllamaBridge:
    """Ollama API integration, used for tag recommendation"""
    
    # Add class variables to cache initialization status
    _initialized = False
    
    def __init__(self, model: str = "hf.co/unsloth/gemma-3-4b-it-GGUF:latest", host: str = "http://127.0.0.1:11434"):
        """Initialize the Ollama client
        
        Args:
            model: The name of the model used
            host: Ollama Service Address
        """
        self.model = model
        self.host = host
        # Default quiet mode
        self.quiet = os.environ.get("OLLAMA_SKIP_OUTPUT", "1") == "1"
        
        # Only check the service when first initialized
        if not OllamaBridge._initialized:
            self._ensure_service()
            OllamaBridge._initialized = True
        
    def _ensure_service(self):
        """Ensure the Ollama service is available"""
        service_mgr = OllamaServiceManager()
        service_mgr.quiet = self.quiet  # Pass the silent settings
        service_mgr.model = self.model  # Set the same model
        
        # Check if it is installed
        if not service_mgr.check_ollama_installed():
            install_cmd = service_mgr.get_install_command()
            print(f"Ollama is not installed. Please install Ollama first:")
            print(install_cmd)
            print("\nOllama can be downloaded from: https://ollama.com/download")
            raise RuntimeError("Ollama is not installed")
            
        # Check if the service is running
        if not service_mgr.is_service_running():
            print("Ollama service is not running, starting...")
            service_mgr.start_service()
            # Wait for the service to start
            for i in range(5):
                print(f"Waiting for the service to start... Try {i+1}/5")
                time.sleep(2)
                if service_mgr.is_service_running():
                    print("Service started")
                    break
            else:
                print("Service startup failed")
                raise RuntimeError("Ollama service failed to start")
                
        # Check if the model exists - use the optimized fast check
        print(f"Check the model {self.model} Does it exist...")
        if not service_mgr.ensure_model_exists(self.model):
            print(f"Model {self.model} Doesn't exist, downloading...")
            try:
                service_mgr.ensure_service_ready()
                print(f"Model {self.model} is ready")
            except Exception as e:
                print(f"Model preparation failed: {e}")
                raise RuntimeError(f"Model preparation failed: {e}")
                
        print("Ollama service is ready")
            
    def generate(self, prompt: str, system: str = "", **kwargs) -> str:
        """Generate text
        
        Args:
            prompt: Prompt
            system: System prompt
            **kwargs: Other parameters
            
        Returns:
            Generated text
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
            
            # Collect all responses
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
        """Extract keywords from text content as tags
        
        Args:
            text: Text content to analyze
            
        Returns:
            Extracted tag list
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
            
            # Clean and format tags
            tags = []
            for tag in response.strip().split(','):
                tag = tag.strip().lower()
                tag = re.sub(r'[^\w\s_]', '', tag)
                tag = re.sub(r'[\s-]+', '_', tag)
                tag = re.sub(r'_+', '_', tag)
                tag = tag.strip('_')
                if tag and len(tag) <= 50 and re.match(r'^[a-z0-9_]+$', tag):
                    tags.append(tag)
            
            # If no valid tags are found, retry once
            if not tags:
                print("No valid tags found, retrying...")
                return self.suggest_tags(text)
                
            return list(set(tags))  # Remove duplicates
            
        except Exception as e:
            print(f"Error suggesting tags: {e}")
            return []
        
    def analyze_tag_relations(self, tag: str, tags: List[str]) -> List[Dict[str, Any]]:
        """Analyze the relationship between tags
        
        Args:
            tag: Target tag
            tags: List of tags to analyze
            
        Returns:
            List of tag relationships, each element contains tag name and relationship type
        """
        # Predefined relationship types
        RELATIONS = {
            "CONTRAST": "Comparative or contrasting relationship",
            "RELATE": "General association relationship",
            "INFLUENCE": "Influence relationship",
            "CONTAIN": "Parent-child relationship",
            "BELONG": "Subordinate relationship",
            "PARALLEL": "Parallel relationship",
            "DEPENDENCY": "Dependency relationship",
            "CAUSE": "Causal relationship",
            "EFFECT": "Causal relationship",
            "COOCCURRENCE": "Co-occurrence relationship"
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
                response = response.strip()
                
                if not ('\nREASON:' in response and response.startswith('RELATION:')):
                    print(f"Warning: Invalid response format for tag '{related_tag}', retrying...")
                    response = self.generate(prompt, system)
                    if not ('\nREASON:' in response and response.startswith('RELATION:')):
                        print(f"Warning: Still invalid format after retry, skipping tag '{related_tag}'")
                        continue
                
                lines = response.strip().split('\n')
                relation_line = next(line for line in lines if line.startswith('RELATION:'))
                reason_line = next(line for line in lines if line.startswith('REASON:'))
                
                relation = relation_line.split(':', 1)[1].strip().upper()
                reason = reason_line.split(':', 1)[1].strip()
                
                if relation not in RELATIONS:
                    print(f"Warning: Invalid relation type '{relation}' for tag '{related_tag}', retrying...")
                    continue
                
                results.append({
                    'tag': related_tag,
                    'relation': relation.lower(),  # Convert to lowercase to match Emacs symbols
                    'reason': reason
                })
            except Exception as e:
                print(f"Warning: Failed to parse response for tag '{related_tag}': {e}")
                print(f"Response was:\n{response}")
                continue
                
        return results
        
    def direct_tag_generation(self, text: str) -> List[str]:
        """Direct use of LLM to generate tags for text, optimized for Chinese processing
        
        Args:
            text: Text to analyze
            
        Returns:
            Tag list
        """
        if not self._initialized:
            return []
            
        try:
            system = """You are a professional indexer, good at creating precise topic indexes for academic literature and technical documents. Your task is to identify the most important key terms, concepts, and themes in the text, especially pay attention to the following points:

1. Identify core terms and concepts in the professional field, especially key terms in titles or chapter titles
2. Extract complete technical terms and professional concepts, maintaining their completeness
3. Capture specific domain-specific noun phrases
4. Identify the main objects, tools, methods, and theories discussed in the text
5. Do not miss key terms that are clearly mentioned in the text, especially repeated professional terms

Extraction standards:
1. Extract 3-5 of the most critical terms or concepts from the text
2. Accurately extract the complete terms that appear in the original text
3. Maintain the completeness and accuracy of professional terms
4. If the term is Chinese, convert it to the corresponding English term, maintaining professionalism and accuracy
5. Focus on terms appearing in titles and subtitles, which are usually the core concepts of the document

Output format:
- Return ONLY a comma-separated list of tags
- Use underscores to connect multi-word concepts
- Do not include any other text or explanations
- Example: search_engine, search_intent, information_retrieval"""

            prompt = f"""As a professional indexer, extract 3-5 of the most critical terms or concepts from the following text as retrieval tags:

{text}

Please pay special attention to:
1. Accurately extract the key terms and professional concepts that are clearly mentioned in the titles or text
2. Maintain the completeness of the terms, do not simplify professional terms
3. Use underscores to connect multi-word concepts
4. Return ONLY a comma-separated list of tags, do not include any other text"""

            response = self.generate(prompt, system)
            
            # Parse the response
            tags = []
            if response:
                for tag in response.strip().split(','):
                    tag = tag.strip().lower()
                    tag = re.sub(r'[^\w\s_]', '', tag)
                    tag = re.sub(r'[\s-]+', '_', tag)
                    tag = re.sub(r'_+', '_', tag)
                    tag = tag.strip('_')
                    if tag:
                        tags.append(tag)
                        
            return tags
            
        except Exception as e:
            if not self.quiet:
                print(f"Error in direct tag generation: {e}")
            return []
            
    def generate_tags(self, text: str) -> List[str]:
        """hybrid method: extract keywords as tags from text
        
        Args:
            text: Text to analyze
            
        Returns:
            Tag list
        """
        # First use the Chinese-optimized tag extraction
        chinese_tags = self.direct_tag_generation(text)
        
        # If the number of tags is sufficient, return directly
        if len(chinese_tags) >= 3:  # At least 3 tags are needed
            return chinese_tags
            
        # If the number of tags is insufficient, use the English method to supplement
        try:
            standard_tags = self.suggest_tags(text)
            
            # Merge and remove duplicates
            all_tags = list(set(chinese_tags + standard_tags))
            
            # If the number of tags is still insufficient, keep all tags
            # Otherwise, limit the maximum return to 5 tags
            if len(all_tags) > 5:  # Maximum return 5 tags
                return all_tags[:5]
            else:
                return all_tags
                
        except Exception as e:
            if not self.quiet:
                print(f"Error getting standard tags: {e}")
            # If the tag supplement fails, return the original tags
            return chinese_tags

    def extract_entities(self, text: str) -> List[Dict[str, Any]]:
        """Extract named entities from text
        
        Args:
            text: Text to analyze
            
        Returns:
            Entity list, each entity contains text, type, and confidence
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
            
            try:
                entities = json.loads(response)
                if not isinstance(entities, list):
                    raise ValueError("Response is not a list")
                    
                valid_entities = []
                for entity in entities:
                    if not isinstance(entity, dict):
                        continue
                        
                    if not all(k in entity for k in ['text', 'type', 'confidence']):
                        continue
                        
                    if entity['type'] not in ['PERSON', 'ORG', 'PRODUCT', 'CONCEPT', 'TECH']:
                        continue
                        
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

def extract_entities_from_file(input_file, output_file, quiet=True):
    """Extract named entities from file (for compatibility, but returns empty results)
    
    Args:
        input_file: input file path (including text)
        output_file: output file path
        quiet: Whether to silent mode
    """
    try:
        if not quiet:
            print(f"Reading input file: {input_file}")
        with open(input_file, 'r') as f:
            input_data = json.load(f)
            
        if isinstance(input_data, list) and len(input_data) >= 1:
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
        try:
            with open(output_file, 'w') as f:
                json.dump([], f)
        except:
            pass

def suggest_tags_from_file(input_file, output_file, quiet=True):
    """Generate tag suggestions from files
    
    Args:
        input_file: input file path (including text)
        output_file: output file path
        quiet: Whether to silent mode
    """
    try:
        if not quiet:
            print(f"Reading input file: {input_file}")
        with open(input_file, 'r') as f:
            input_data = json.load(f)
            
        if isinstance(input_data, list) and len(input_data) >= 1:
            text = input_data[0]  # text to analyze
            
            bridge = OllamaBridge()
            bridge.quiet = quiet
            tags = bridge.generate_tags(text)
            
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
        try:
            with open(output_file, 'w') as f:
                json.dump([], f)
        except:
            pass

def main():
    """Test Ollama integration"""
    try:
        quiet_mode = os.environ.get("OLLAMA_SKIP_OUTPUT", "1") == "1"

        if len(sys.argv) > 1:
            test_text = sys.argv[1]
            bridge = OllamaBridge()
            bridge.quiet = quiet_mode
            tags = bridge.generate_tags(test_text)
            print(", ".join(tags))
            return
        

        service = OllamaServiceManager()
        service.quiet = quiet_mode
        if not service.ensure_service_ready():
            print("Failed to start Ollama service")
            sys.exit(1)
        

        bridge = OllamaBridge()
        bridge.quiet = quiet_mode
        
        tech_text = """Python is a popular programming language for machine learning projects.
        TensorFlow and PyTorch are two common frameworks used by data scientists."""
        
        print("\n=== Test example 1: Technical content ===")
        print(f"Input: {tech_text}")
        tech_tags = bridge.generate_tags(tech_text)
        print(f"Extracted tags: {tech_tags}")
        
        chinese_text = "Input any keyword, as long as it can be found in Google Trends within 12 months, it is a new word"
        print("\n=== Test example 2: Chinese content ===")
        print(f"Input: {chinese_text}")
        chinese_tags = bridge.generate_tags(chinese_text)
        print(f"Extracted tags: {chinese_tags}")
        
        entity_text = "Tesla CEO Elon Musk announced that SpaceX will launch Starship to Mars by 2026."
        print("\n=== Test example 3: Content with obvious proper nouns ===")
        print(f"Input: {entity_text}")
        entity_tags = bridge.generate_tags(entity_text)
        print(f"Extracted tags: {entity_tags}")
        
        if not quiet_mode:
            print("\n=== Test example 4: Tag relation analysis ===")
            main_tag = "python"
            related_tags = ["machine_learning", "tensorflow", "programming"]
            print(f"Main tag: {main_tag}")
            print(f"Related tags: {related_tags}")
            
            relations = bridge.analyze_tag_relations(main_tag, related_tags)
            if relations:
                print("\nAnalysis results:")
                for relation in relations:
                    print(f"- {relation['tag']}: {relation['relation']} ({relation['reason']})")
            else:
                print("Failed to get tag relation analysis results")
        
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main() 
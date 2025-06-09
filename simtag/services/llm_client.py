"""
LLM Client Service
Provides a standardized interface for interacting with various LLM providers.
Initially supports Ollama, designed for extensibility.
"""
import httpx # Using httpx for async requests
import json
import logging
from typing import Optional, List, Dict, Any, Union
import requests       # NEW: Import for requests library
import asyncio        # For running sync code in async
import time

# from simtag.config import Config # LLMClient will expect an llm_config dict

# 导入多核心管理器
try:
    from ..multicore_manager import MultiCoreManager
except ImportError:
    # 当作为脚本直接运行时的fallback
    import sys
    import os
    sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    from multicore_manager import MultiCoreManager

logger = logging.getLogger(__name__)

class LLMClient:
    """
    LLM客户端，支持通过嵌入服务的多后端架构
    """
    
    def __init__(self, provider: str, config: Dict[str, Any]):
        """
        Initializes the LLMClient.

        Args:
            provider (str): The LLM provider.
            config (Dict[str, Any]): Configuration dictionary for the LLM provider.
                Expected keys:
                - 'base_url': str (API base URL)
                - 'default_model': str (Default model name to use)
                - 'timeout': Optional[int] (Request timeout in seconds, defaults to 300)
                - 'multicore_config': Optional[Dict] (Multi-core configuration)
        """
        self.provider = provider
        self.config = config
        self.base_url = config.get('base_url', 'http://localhost:11434')
        self.default_model = config.get('default_model', 'gemma3:4b')  # Changed default
        self.timeout = config.get('timeout', 300)  # Increased timeout to 5 minutes
        
        # 初始化HTTP客户端
        self._client = httpx.AsyncClient(timeout=self.timeout)
        
        # 初始化多核心管理器
        self._init_multicore_manager()

    def _init_multicore_manager(self):
        """初始化多核心管理器"""
        multicore_config = self.config.get('multicore_config', {
            "enabled": True,
            "max_workers": None,  # 自动检测
            "embedding_batch_threshold": 300,  # 300个文本以上使用多核心
            "memory_threshold_gb": 4
        })
        
        try:
            self.multicore_manager = MultiCoreManager(multicore_config)
            logger.info("Multi-core manager initialized successfully")
        except Exception as e:
            logger.warning(f"Failed to initialize multi-core manager: {e}, will use single-threaded processing")
            self.multicore_manager = None

    async def check_availability(self) -> bool:
        """Checks if the configured LLM service is available."""
        if self.provider == 'ollama':
            try:
                # This is the only remaining use of httpx
                response = await self._client.get(f"{self.base_url}/api/tags")
                response.raise_for_status() # Raise an exception for HTTP errors
                logger.info(f"Ollama service available at {self.base_url}")
                return True
            except httpx.RequestError as e:
                logger.warning(f"Ollama service unavailable at {self.base_url}: {e}")
                return False
            except httpx.HTTPStatusError as e:
                logger.warning(f"Ollama service returned error status at {self.base_url}: {e.response.status_code} - {e.response.text}")
                return False
        # TODO: Implement availability checks for other providers
        logger.warning(f"Availability check not implemented for provider: {self.provider}")
        return False # Default to false if provider unknown or check not implemented

    async def generate(self, 
                       prompt: str, 
                       system_prompt: Optional[str] = None, 
                       model: Optional[str] = None,
                       format_json: bool = False,
                       options: Optional[Dict[str, Any]] = None,
                       **kwargs: Any
                      ) -> str:
        """
        Generates text using the configured LLM provider.
        使用更安全的直接调用方式，避免线程池问题。
        """
        target_model = model if model else self.config.get('default_model', self.default_model)
        
        if self.provider == 'ollama':
            # 直接调用同步方法，避免 asyncio.to_thread 的线程池问题
            try:
                return self._call_ollama_generate_sync(
                    prompt=prompt,
                    system_prompt=system_prompt,
                    model=target_model,
                    format_json=format_json,
                    options=options,
                    **kwargs
                )
            except Exception as e:
                logger.error(f"Direct Ollama generate call failed: {e}")
                # 如果直接调用失败，尝试异步HTTP客户端
                return await self._call_ollama_generate_async(
                    prompt=prompt,
                    system_prompt=system_prompt,
                    model=target_model,
                    format_json=format_json,
                    options=options,
                    **kwargs
                )
        # TODO: Add dispatch for other providers like 'openai'
        else:
            logger.error(f"LLM provider '{self.provider}' not supported.")
            return ""

    def _call_ollama_generate_sync(self,
                                    prompt: str,
                                    system_prompt: Optional[str],
                                    model: str,
                                    format_json: bool,
                                    options: Optional[Dict[str, Any]],
                                    **kwargs: Any
                                   ) -> str:
        """
        Synchronous version using requests instead of httpx to avoid potential issues.
        """
        api_url = f"{self.base_url}/api/generate"
        
        payload_dict: Dict[str, Any] = {
            "model": model,
            "prompt": prompt,
            "stream": True,  # Always use streaming to be consistent
            "format": "json" if format_json else ""
        }
        
        if system_prompt:
            payload_dict["system"] = system_prompt
        
        # Add options if provided
        if options:
            payload_dict["options"] = options
        
        logger.debug(f"Making generate request to {api_url} with model {model}")
        
        try:
            response = requests.post(
                api_url, 
                json=payload_dict, 
                headers={"Content-Type": "application/json"},
                timeout=self.timeout,
                stream=True
            )
            
            response.raise_for_status()
            
            # Parse streaming response
            full_response = ""
            for line in response.iter_lines():
                if line:
                    try:
                        chunk = json.loads(line.decode('utf-8'))
                        if 'response' in chunk:
                            full_response += chunk['response']
                        if chunk.get('done', False):
                            break
                    except json.JSONDecodeError:
                        logger.warning(f"Failed to parse JSON chunk: {line}")
                        continue
            
            logger.debug(f"Successfully generated response with model {model}")
            return full_response
            
        except requests.exceptions.RequestException as e:
            logger.error(f"Ollama generate request failed for model {model}: {e}")
            raise

    async def _call_ollama_generate_async(self,
                                        prompt: str,
                                        system_prompt: Optional[str],
                                        model: str,
                                        format_json: bool,
                                        options: Optional[Dict[str, Any]],
                                        **kwargs: Any
                                       ) -> str:
        """
        Asynchronous version using httpx to avoid potential issues.
        """
        api_url = f"{self.base_url}/api/generate"
        
        payload_dict: Dict[str, Any] = {
            "model": model,
            "prompt": prompt,
            "stream": True,  # Always use streaming to be consistent
            "format": "json" if format_json else ""
        }
        
        if system_prompt:
            payload_dict["system"] = system_prompt
        
        # Add options if provided
        if options:
            payload_dict["options"] = options
        
        logger.debug(f"Making generate request to {api_url} with model {model}")
        
        try:
            response = await self._client.post(
                api_url, 
                json=payload_dict, 
                headers={"Content-Type": "application/json"},
                timeout=self.timeout
            )
            
            response.raise_for_status()
            
            # Parse streaming response
            full_response = ""
            async for line in response.aiter_lines():
                if line:
                    try:
                        chunk = json.loads(line.decode('utf-8'))
                        if 'response' in chunk:
                            full_response += chunk['response']
                        if chunk.get('done', False):
                            break
                    except json.JSONDecodeError:
                        logger.warning(f"Failed to parse JSON chunk: {line}")
                        continue
            
            logger.debug(f"Successfully generated response with model {model}")
            return full_response
            
        except httpx.RequestError as e:
            logger.error(f"Ollama generate request failed for model {model}: {e}")
            raise

    async def get_embedding(self, 
                            text: str, 
                            model: Optional[str] = None,
                            **kwargs: Any
                           ) -> List[float]:
        """
        Async version of get_embedding using EmbeddingService.
        """
        from .embedding_service import get_embedding_service
        
        embedding_service = get_embedding_service()
        result = await embedding_service.get_embedding(text, model=model)
        
        if result.success:
            return result.embedding or []
        else:
            logger.error(f"Embedding failed: {result.error_message}")
            return []

    async def get_embeddings_batch(self,
                                 texts: List[str],
                                 model: Optional[str] = None,
                                 use_multicore: bool = True,
                                 **kwargs: Any
                                ) -> List[List[float]]:
        """
        Async batch embedding using EmbeddingService.
        """
        from .embedding_service import get_embedding_service
        
        logger.info(f"🚀 批量处理 {len(texts)} 个文本的嵌入")
        
        embedding_service = get_embedding_service()
        results = await embedding_service.get_embeddings_batch(texts, model=model)
        
        # 转换为List[List[float]]格式
        embeddings = []
        success_count = 0
        for result in results:
            if result.success and result.embedding:
                embeddings.append(result.embedding)
                success_count += 1
            else:
                embeddings.append([])  # 失败时返回空列表
        
        logger.info(f"✅ 批量嵌入处理完成: {success_count}/{len(texts)} 成功")
        return embeddings

    async def close(self):
        """Closes the underlying HTTP client and cleans up resources."""
        if hasattr(self, '_client') and self._client and not self._client.is_closed:
            await self._client.aclose()
            logger.info("LLMClient HTTP client closed.")
        
        # 清理多核心管理器资源
        if hasattr(self, 'multicore_manager') and self.multicore_manager:
            # 使用正确的清理方法（如果有的话）
            if hasattr(self.multicore_manager, 'close'):
                self.multicore_manager.close()
            elif hasattr(self.multicore_manager, 'shutdown'):
                self.multicore_manager.shutdown()
            logger.info("LLMClient multi-core manager cleaned up.")


# Example Usage (Illustrative - requires an Ollama server running for full test)
async def main_test_llm_client():
    logging.basicConfig(level=logging.INFO) # Changed to INFO for less verbose default

    # Example llm_config (this would typically come from a main Config object)
    ollama_config = {
        'provider': 'ollama',
        'base_url': 'http://localhost:11434', # Ensure your Ollama is running here
        'default_model': 'gemma3:4b', # A small model for quick testing
        'default_embedding_model': 'nomic-embed-text', # Common embedding model
        'timeout': 60 
    }

    client = LLMClient(provider='ollama', config=ollama_config)

    # --- Test Availability (Ollama specific part) ---
    available = await client.check_availability()
    print(f"Ollama service available: {available}\n")

    if not available:
        print("Ollama not available, skipping further tests that require it.")
        await client.close()
        return

    # --- Test Generation ---
    print("--- Testing Text Generation ---")
    prompt_text = "Explain the concept of a Large Language Model in one sentence."
    try:
        generated_response = await client.generate(prompt_text, model="gemma3:4b") # Explicit model
        print(f"Prompt: {prompt_text}")
        print(f"Generated Response: {generated_response}\n")
        assert generated_response is not None and len(generated_response) > 0 and not generated_response.startswith("Error:")
    except Exception as e:
        print(f"Error during generation test: {e}\n")
    
    # --- Test JSON Generation ---
    print("--- Testing JSON Generation ---")
    json_prompt = "Provide a JSON object with two keys: 'name' and 'type', describing a cat."
    try:
        json_response_str = await client.generate(json_prompt, format_json=True, system_prompt="You are a helpful assistant that only responds in valid JSON.", model="gemma3:4b")
        print(f"Prompt: {json_prompt}")
        print(f"Generated JSON Response (string): {json_response_str}")
        # Try parsing the JSON string
        parsed_json = json.loads(json_response_str) # This will fail if error message returned
        print(f"Parsed JSON: {parsed_json}")
        assert 'name' in parsed_json and 'type' in parsed_json
    except json.JSONDecodeError as e:
        print(f"Failed to parse JSON string from LLM (likely an error message was returned instead of JSON): {json_response_str}. Error: {e}")
    except Exception as e:
        print(f"Error during JSON generation test: {e}\n")


    # --- Test Embeddings ---
    print("--- Testing Embeddings ---")
    embedding_text = "This is a test sentence for embeddings."
    try:
        embedding_vector = await client.get_embedding(embedding_text) # Uses default_embedding_model
        print(f"Text for embedding: {embedding_text}")
        if embedding_vector:
            print(f"Embedding Vector (first 5 dims): {embedding_vector[:5]}... Length: {len(embedding_vector)}")
            assert isinstance(embedding_vector, list) and len(embedding_vector) > 0
        else:
            print("Embedding vector was empty, check Ollama logs or model availability ('nomic-embed-text').")
            assert False # Fail test if no embedding
    except Exception as e:
        print(f"Error during embedding test: {e}\n")
    
    # --- Test with a non-existent model (example of error handling) ---
    print("--- Testing Non-Existent Model ---")
    try:
        error_response = await client.generate("Hello", model="nonexistent-model:latest")
        print(f"Response for non-existent model: {error_response}") 
        assert error_response.startswith("Error:")
    except Exception as e: # Should not typically raise exception, but return error string
        print(f"Error (unexpected exception) for non-existent model: {e}\n")

    await client.close()

if __name__ == '__main__':
    import asyncio
    # To run this example:
    # 1. Ensure Ollama is running (e.g., `ollama serve`)
    # 2. Ensure you have pulled the models used (e.g., `ollama pull gemma3:4b`, `ollama pull nomic-embed-text`)
    # 3. Run this script (ensure httpx and requests are installed: pip install httpx requests)
    # asyncio.run(main_test_llm_client())
    pass 
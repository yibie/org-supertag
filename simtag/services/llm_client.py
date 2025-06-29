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
        """
        self.provider = provider
        self.config = config
        self.base_url = config.get('base_url', 'http://localhost:11434')
        self.default_model = config.get('default_model', 'gemma3:4b')
        self.timeout = config.get('timeout', 300)
        
        self._client: Optional[httpx.AsyncClient] = None # Initialize to None

    async def _get_httpx_client(self) -> httpx.AsyncClient:
        """Lazily initializes and returns the httpx.AsyncClient."""
        if self._client is None or self._client.is_closed:
            logger.info("Creating new httpx.AsyncClient instance.")
            self._client = httpx.AsyncClient(timeout=self.timeout)
        return self._client

    async def check_availability(self) -> bool:
        """Checks if the configured LLM service is available."""
        if self.provider == 'ollama':
            try:
                client = await self._get_httpx_client()
                response = await client.get(f"{self.base_url}/api/tags")
                response.raise_for_status()
                logger.info(f"Ollama service available at {self.base_url}")
                return True
            except httpx.RequestError as e:
                logger.warning(f"Ollama service unavailable at {self.base_url}: {e}")
                return False
            except httpx.HTTPStatusError as e:
                logger.warning(f"Ollama service returned error status at {self.base_url}: {e.response.status_code} - {e.response.text}")
                return False
        logger.warning(f"Availability check not implemented for provider: {self.provider}")
        return False

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
        """
        target_model = model if model else self.config.get('default_model', self.default_model)

        if self.provider == 'ollama':
            return await self._call_ollama_generate_async(
                prompt=prompt,
                system_prompt=system_prompt,
                model=target_model,
                format_json=format_json,
                options=options,
                **kwargs
            )
        logger.warning(f"Generation not implemented for provider: {self.provider}")
        return f"Error: Generation not implemented for provider {self.provider}"

    async def _call_ollama_generate_async(self,
                                        prompt: str,
                                        system_prompt: Optional[str],
                                        model: str,
                                        format_json: bool,
                                        options: Optional[Dict[str, Any]],
                                        **kwargs: Any
                                       ) -> str:
        """
        Asynchronous version using httpx.
        """
        api_url = f"{self.base_url}/api/generate"

        payload_dict: Dict[str, Any] = {
            "model": model,
            "prompt": prompt,
            "stream": True,
            "format": "json" if format_json else ""
        }

        if system_prompt:
            payload_dict["system"] = system_prompt

        if options:
            payload_dict["options"] = options

        logger.debug(f"Making generate request to {api_url} with model {model}")

        try:
            client = await self._get_httpx_client()
            response = await client.post(
                api_url,
                json=payload_dict,
                headers={"Content-Type": "application/json"},
                timeout=self.timeout
            )

            response.raise_for_status()

            full_response = ""
            async for line in response.aiter_lines():
                if line:
                    try:
                        chunk = json.loads(line)
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
            logger.error(f"Async Ollama request failed for model {model}: {e}")
            return f"Error: Failed to connect to Ollama service at {self.base_url}"
        except httpx.HTTPStatusError as e:
            logger.error(f"Async Ollama request failed for model {model} with status {e.response.status_code}: {e.response.text}")
            return f"Error: Ollama service returned status {e.response.status_code} - {e.response.text}"
        except Exception as e:
            logger.error(f"An unexpected error occurred during async Ollama call for model {model}: {e}")
            return f"Error: An unexpected error occurred: {e}"

    

    async def close(self):
        """Closes the underlying HTTP client."""
        if hasattr(self, '_client') and self._client and not self._client.is_closed:
            await self._client.aclose()
            logger.info("LLMClient HTTP client closed.")


# Example Usage (Illustrative - requires an Ollama server running for full test)
async def main_test_llm_client():
    """测试LLMClient的功能"""
    
    test_config = {
        'provider': 'ollama',
        'base_url': 'http://localhost:11434',
        'default_model': 'qwen2.5:1.5b',
        'timeout': 120
    }
    
    client = LLMClient(provider='ollama', config=test_config)
    
    is_available = await client.check_availability()
    print(f"Ollama service available: {is_available}")
    if not is_available:
        return

    print("\n--- Testing Text Generation ---")
    prompt = "Why is the sky blue? Be concise."
    response = await client.generate(prompt)
    print(f"Prompt: {prompt}")
    print(f"Response: {response}")

    print("\n--- Testing JSON Generation ---")
    json_prompt = "Provide a user profile for 'John Doe', age 30, city 'New York'. Respond in JSON format."
    json_response_str = await client.generate(json_prompt, format_json=True)
    print(f"Prompt: {json_prompt}")
    try:
        json_response = json.loads(json_response_str)
        print(f"Parsed JSON Response: {json_response}")
        assert isinstance(json_response, dict)
    except json.JSONDecodeError:
        print(f"Failed to parse JSON response: {json_response_str}")

    await client.close()

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
    asyncio.run(main_test_llm_client()) 
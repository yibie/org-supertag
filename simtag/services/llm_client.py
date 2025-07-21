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
from abc import ABC, abstractmethod
from dataclasses import dataclass, field

# from simtag.config import Config # LLMClient will expect an llm_config dict
from ..config import LLMConfig

logger = logging.getLogger(__name__)

# =============================================================================
# Standardized Data Structures
# =============================================================================

@dataclass
class LLMResult:
    """Standardized result for all LLM operations."""
    success: bool
    content: str = ""
    error_message: Optional[str] = None
    provider_name: Optional[str] = None
    model_used: Optional[str] = None
    response_time: Optional[float] = None

# =============================================================================
# Backend Abstraction
# =============================================================================

class LLMBackend(ABC):
    """Abstract Base Class for all LLM provider backends."""
    
    @property
    @abstractmethod
    def provider_name(self) -> str:
        """The name of the backend provider (e.g., 'ollama', 'openai')."""
        pass
    
    @abstractmethod
    async def generate(self,
                     prompt: str,
                     model: str,
                     format_json: bool,
                     options: Optional[Dict[str, Any]],
                     system_prompt: Optional[str] = None,
                     **kwargs: Any
                    ) -> LLMResult:
        """Abstract method for single text generation."""
        pass

    @abstractmethod
    async def chat(self,
                   messages: List[Dict[str, str]],
                   model: str,
                   format_json: bool,
                   options: Optional[Dict[str, Any]],
                   **kwargs: Any
                  ) -> LLMResult:
        """Abstract method for chat completions."""
        pass

# =============================================================================
# Ollama Backend Implementation
# =============================================================================

class OllamaBackend(LLMBackend):
    """LLM Backend implementation for Ollama."""
    
    def __init__(self, base_url: str, default_model: str, timeout: int):
        self.base_url = base_url
        self.default_model = default_model
        self.timeout = timeout

    @property
    def provider_name(self) -> str:
        return "ollama"

    async def generate(self,
                     prompt: str,
                     model: str,
                     format_json: bool,
                     options: Optional[Dict[str, Any]],
                     system_prompt: Optional[str] = None,
                     **kwargs: Any
                    ) -> LLMResult:
        api_url = f"{self.base_url}/api/generate"
        payload: Dict[str, Any] = {
            "model": model,
            "prompt": prompt,
            "stream": False, # Ensure no streaming
        }
        if format_json:
            payload["format"] = "json"
        if system_prompt:
            payload["system"] = system_prompt
        if options:
            payload["options"] = options

        #logger.debug(f"Ollama payload: {json.dumps(payload)}")
        logger.debug(f"Making Ollama generate request to {api_url} with model {model} using requests")
        start_time = time.time()

        def do_request():
            # This function will be executed in a separate thread
            try:
                return requests.post(api_url, json=payload, timeout=self.timeout)
            except requests.RequestException as e:
                return e # Return exception to be re-raised in main thread

        try:
            loop = asyncio.get_running_loop()
            response = await loop.run_in_executor(None, do_request)

            if isinstance(response, requests.RequestException):
                raise response # Re-raise exception from the thread

            response.raise_for_status()
            response_json = response.json()

            # Ollama's non-streaming response aggregates the 'response' field.
            content = response_json.get('response', '').strip()

            return LLMResult(
                success=True,
                content=content,
                provider_name=self.provider_name,
                model_used=model,
                response_time=time.time() - start_time
            )
        except requests.RequestException as e:
            msg = f"Failed to connect to Ollama service using requests: {e}"
            logger.error(f"Ollama generate request failed for model {model}: {msg}")
            return LLMResult(success=False, error_message=msg, provider_name=self.provider_name, model_used=model)
        except requests.HTTPError as e:
            # Use e.response.text to get the body, which might have more info
            response_text = e.response.text if e.response else "(No response text)"
            msg = f"Ollama service returned status {e.response.status_code}: {response_text}"
            logger.error(f"Ollama generate request failed for model {model} with status {e.response.status_code}: {response_text}")
            return LLMResult(success=False, error_message=msg, provider_name=self.provider_name, model_used=model)
        except Exception as e:
            msg = f"An unexpected error occurred: {e}"
            logger.error(f"An unexpected error occurred during Ollama generate call for model {model}: {msg}")
            return LLMResult(success=False, error_message=msg, provider_name=self.provider_name, model_used=model)

    async def chat(self,
                   messages: List[Dict[str, str]],
                   model: str,
                   format_json: bool,
                   options: Optional[Dict[str, Any]],
                   **kwargs: Any
                  ) -> LLMResult:
        api_url = f"{self.base_url}/api/chat"
        payload: Dict[str, Any] = {
            "model": model,
            "messages": messages,
            "stream": False,
        }
        if format_json:
            payload["format"] = "json"
        if options:
            payload["options"] = options

        logger.debug(f"Making Ollama chat request to {api_url} with model {model} using requests")
        start_time = time.time()

        def do_request():
            try:
                return requests.post(api_url, json=payload, timeout=self.timeout)
            except requests.RequestException as e:
                return e

        try:
            loop = asyncio.get_running_loop()
            response = await loop.run_in_executor(None, do_request)

            if isinstance(response, requests.RequestException):
                raise response

            response.raise_for_status()
            response_json = response.json()
            content = response_json.get('message', {}).get('content', '').strip()

            return LLMResult(
                success=True,
                content=content,
                provider_name=self.provider_name,
                model_used=model,
                response_time=time.time() - start_time
            )
        except requests.RequestException as e:
            msg = f"Failed to connect to Ollama service using requests: {e}"
            logger.error(f"Ollama chat request failed for model {model}: {msg}")
            return LLMResult(success=False, error_message=msg, provider_name=self.provider_name, model_used=model)
        except requests.HTTPError as e:
            response_text = e.response.text if e.response else "(No response text)"
            msg = f"Ollama service returned status {e.response.status_code}: {response_text}"
            logger.error(f"Ollama chat request failed for model {model} with status {e.response.status_code}: {response_text}")
            return LLMResult(success=False, error_message=msg, provider_name=self.provider_name, model_used=model)
        except Exception as e:
            msg = f"An unexpected error occurred: {e}"
            logger.error(f"An unexpected error occurred during Ollama chat call for model {model}: {msg}")
            return LLMResult(success=False, error_message=msg, provider_name=self.provider_name, model_used=model)

# =============================================================================
# Unified LLM Client
# =============================================================================

class LLMClient:
    """
    Unified LLM Client that manages multiple backends.
    """
    
    def __init__(self, config: Union[Dict, 'LLMConfig']):
        if not isinstance(config, dict):
            config = LLMConfig()

        self.config = config
        self.backends: Dict[str, LLMBackend] = {}
        self.primary_backend: Optional[str] = None
        self._init_backends()

    def _init_backends(self):
        """Initializes all configured LLM backends."""
        backends = getattr(self.config, 'backends', {})
        if "ollama" in backends:
            ollama_config = backends["ollama"]
            self.backends["ollama"] = OllamaBackend(
                base_url=ollama_config.base_url,
                default_model=ollama_config.default_model,
                timeout=ollama_config.timeout
            )
            logger.info("Initialized 'ollama' LLM backend.")
        
        self.primary_backend = getattr(self.config, 'primary_backend', 'ollama')
        if not self.primary_backend or self.primary_backend not in self.backends:
            if self.backends:
                self.primary_backend = list(self.backends.keys())[0]
                logger.warning(f"Primary LLM backend not configured or available. Falling back to '{self.primary_backend}'.")
            else:
                logger.error("No LLM backends are available or configured.")

    async def generate(self,
                       prompt: str,
                       system_prompt: Optional[str] = None,
                       model: Optional[str] = None,
                       format_json: bool = False,
                       options: Optional[Dict[str, Any]] = None,
                       use_chat_endpoint: bool = False,
                       **kwargs: Any
                      ) -> LLMResult:
        if use_chat_endpoint:
            messages = []
            if system_prompt:
                messages.append({"role": "system", "content": system_prompt})
            messages.append({"role": "user", "content": prompt})
            return await self.chat(messages=messages, model=model, format_json=format_json, options=options, **kwargs)

        backend_name = self.primary_backend
        if not backend_name or backend_name not in self.backends:
            return LLMResult(success=False, error_message="No suitable LLM backend is available.")

        llm_backend = self.backends[backend_name]
        
        target_model = model
        if not target_model and isinstance(llm_backend, OllamaBackend):
             target_model = llm_backend.default_model

        if not target_model:
            return LLMResult(success=False, error_message="No model specified and backend has no default.", provider_name=llm_backend.provider_name)

        return await llm_backend.generate(
            prompt=prompt,
            system_prompt=system_prompt,
            model=target_model,
            format_json=format_json,
            options=options,
            **kwargs
        )
    async def chat(self,
                   messages: List[Dict[str, str]],
                   model: Optional[str] = None,
                   format_json: bool = False,
                   options: Optional[Dict[str, Any]] = None,
                   backend: Optional[str] = None,
                   **kwargs: Any
                  ) -> LLMResult:
        backend_name = backend or self.primary_backend
        if not backend_name or backend_name not in self.backends:
            return LLMResult(success=False, error_message="No suitable LLM backend is available.")

        llm_backend = self.backends[backend_name]
        
        # Use the backend's default model if no specific model is requested
        target_model = model
        if not target_model and isinstance(llm_backend, OllamaBackend): # Example of backend-specific logic
             target_model = llm_backend.default_model

        if not target_model:
            return LLMResult(success=False, error_message="No model specified and backend has no default.", provider_name=llm_backend.provider_name)

        return await llm_backend.chat(
            messages=messages,
            model=target_model,
            format_json=format_json,
            options=options,
            **kwargs
        )

    async def check_availability(self) -> bool:
        """Checks if the configured LLM service is available."""
        if self.primary_backend in self.backends:
            backend = self.backends[self.primary_backend]
            if isinstance(backend, OllamaBackend):
                try:
                    async with httpx.AsyncClient(timeout=10) as client:
                        response = await client.get(f"{backend.base_url}/api/tags")
                        response.raise_for_status()
                        logger.info(f"LLM service available at {backend.base_url}")
                        return True
                except httpx.RequestError as e:
                    logger.warning(f"LLM service unavailable at {backend.base_url}: {e}")
                    return False
                except httpx.HTTPStatusError as e:
                    logger.warning(f"LLM service returned error status at {backend.base_url}: {e.response.status_code} - {e.response.text}")
                    return False
        logger.warning(f"Availability check not implemented for primary backend: {self.primary_backend}")
        return False

    async def close(self):
        """Closes the underlying HTTP client. (No-op now)"""
        logger.info("LLMClient.close() called, but client is now managed per-request.")
        pass

    async def get_available_models(self) -> List[str]:
        """Gets the available models from the LLM API."""
        if self.primary_backend in self.backends:
            backend = self.backends[self.primary_backend]
            if isinstance(backend, OllamaBackend):
                try:
                    async with httpx.AsyncClient(timeout=10) as client:
                        response = await client.get(f"{backend.base_url}/api/tags")
                        response.raise_for_status()
                        models_data = response.json()
                        return [model['name'] for model in models_data.get('models', [])]
                except Exception as e:
                    logger.error(f"Failed to get available models from LLM: {e}")
                    return []
        return []

    def get_default_model(self) -> str:
        """Gets the default model from the config."""
        if self.primary_backend in self.backends:
            backends = getattr(self.config, 'backends', {})
            backend_config = backends.get(self.primary_backend)
            if backend_config:
                return backend_config.default_model
        return ''


# Example Usage (Illustrative - requires an LLM server running for full test)
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
    print(f"LLM service available: {is_available}")
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
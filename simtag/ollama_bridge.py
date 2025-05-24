"""
SimTag Ollama Bridge Module - Provides interaction with the Ollama model
"""

import logging
from typing import Any, Optional, Dict, Literal
import subprocess
import traceback
import requests
import json
import sys

class OllamaBridge:
    """Ollama API integration, providing basic LLM call functionality"""
    
    def __init__(self, model: Optional[str] = None):
        """Initialize the Ollama client
        
        Args:
            model: The name of the model to use
        """
        self.logger = logging.getLogger("simtag.ollama_bridge")
        if not model:
            model = "hf.co/unsloth/gemma-3-4b-it-GGUF:latest"
        # Clean up model name - remove any quotes and extra whitespace
        self.model = str(model).strip().replace('"', '').replace("'", '')
        self.logger.info(f"Initialized OllamaBridge, default model: {self.model}")

    def run(self, prompt: str, system: Optional[str] = None, model: Optional[str] = None) -> str:
        """Execute interaction with Ollama model.

        Args:
            prompt: User prompt text.
            system: Optional system prompt text.
            model: Optional model name to override the default for this call.

        Returns:
            The response content string from the model.

        Raises:
            Exception: If the Ollama interaction fails.
        """
        try:
            # Determine the model to use for this specific call
            model_to_use = model.strip().replace('"', '').replace("'", '') if model and model.strip() else self.model
            self.logger.info(f"Executing Ollama call. Model: {model_to_use}. Prompt length: {len(prompt)}")
            if system:
                self.logger.info(f"System prompt provided (length: {len(system)}).")

            # Build request data
            data = {
                "model": model_to_use,
                "prompt": prompt,
                "stream": False,  # Do not use streaming response
                "options": {
                    "temperature": 0.7,  # Control the randomness of the output
                    "num_predict": 1024,  # Maximum output length
                    "stop": []  # Stop markers
                }
            }
            
            # Add system prompt
            if system:
                data["system"] = system
            
            # Log the generated request data (excluding sensitive content)
            self.logger.info(f"Sending API request to model: {model_to_use}")
            
            # Send the request
            try:
                response = requests.post(
                    "http://127.0.0.1:11434/api/generate",
                    json=data,
                    headers={"Content-Type": "application/json"},
                    timeout=60  # Add timeout setting
                )
            except requests.RequestException as e:
                self.logger.error(f"Request exception: {e}")
                raise Exception(f"Request exception: {e}")
            
            # Check the response status code
            if response.status_code == 200:
                try:
                    response_data = response.json()
                    result = response_data.get('response', '').strip()
                    
                    # Log the generation statistics
                    if 'eval_duration' in response_data:
                        eval_duration = response_data['eval_duration']
                        eval_count = response_data.get('eval_count', 0)
                        tokens_per_second = eval_count / (eval_duration / 1e9) if eval_duration > 0 else 0
                        self.logger.info(f"Generation speed: {tokens_per_second:.2f} tokens/s")
                    
                    if not result:
                        self.logger.warning("Ollama returned an empty response")
                        
                    self.logger.info("Ollama API call successful")
                    self.logger.debug(f"Response result: {result[:100]}..." if len(result) > 100 else f"Response result: {result}")
                    return result
                except json.JSONDecodeError as e:
                    self.logger.error(f"Failed to parse JSON response: {e}")
                    self.logger.error(f"Original response content: {response.text[:200]}...")
                    raise Exception(f"Failed to parse JSON response: {e}")
            else:
                error_msg = f"Ollama API call failed: HTTP {response.status_code} - {response.text}"
                self.logger.error(error_msg)
                raise Exception(error_msg)
                
        except requests.exceptions.ConnectionError as e:
            error_msg = f"Failed to connect to Ollama service: {str(e)}"
            self.logger.error(error_msg)
            raise Exception(error_msg)
            
        except Exception as e:
            error_msg = f"Ollama execution exception: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            raise Exception(error_msg)

    def status(self) -> Dict[str, Any]:
        """Get Ollama status"""
        try:
            self.logger.info("Checking Ollama status")
            result = subprocess.run(
                ["ollama", "list"],
                capture_output=True,
                text=True
            )
            
            if result.returncode == 0:
                self.logger.info("Ollama status check successful")
                return {
                    "available": True,
                    "model": self.model,
                    "models": result.stdout.strip()
                }
            else:
                self.logger.error(f"Ollama status check failed: {result.stderr}")
                return {
                    "available": False,
                    "error": result.stderr
                }
                
        except Exception as e:
            error_msg = f"Ollama status check exception: {str(e)}"
            self.logger.error(error_msg)
            return {
                "available": False,
                "error": error_msg
            }

def _test():
    """Test Ollama Bridge functionality"""
    # Set up logging
    logging.basicConfig(
        level=logging.DEBUG,
        format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S'
    )
    logger = logging.getLogger("ollama_bridge_test")
    
    try:
        logger.info("Starting Ollama Bridge test")
        
        # 1. Test initialization
        bridge = OllamaBridge()
        logger.info(f"OllamaBridge instance created successfully, using model: {bridge.model}")
        
        # 2. Test status check
        logger.info("Testing status check...")
        status = bridge.status()
        logger.info(f"Ollama status: {status}")
        
        # 3. Test simple dialogue with generate mode
        logger.info("Testing simple dialogue (generate mode)...")
        prompt = "Hello, please introduce yourself in one sentence."
        response = bridge.run(prompt, mode="generate")
        logger.info(f"Simple dialogue response (generate): {response}")
        
        # 4. Test simple dialogue with chat mode
        logger.info("Testing simple dialogue (chat mode)...")
        response = bridge.run(prompt, mode="chat")
        logger.info(f"Simple dialogue response (chat): {response}")
        
        # 5. Test dialogue with system prompt (generate mode)
        logger.info("Testing dialogue with system prompt (generate mode)...")
        system = "You are a concise assistant, answer should be short."
        prompt = "Explain what is artificial intelligence."
        response = bridge.run(prompt, system=system, mode="generate")
        logger.info(f"Dialogue response with system prompt (generate): {response}")
        
        # 6. Test dialogue with system prompt (chat mode)
        logger.info("Testing dialogue with system prompt (chat mode)...")
        response = bridge.run(prompt, system=system, mode="chat")
        logger.info(f"Dialogue response with system prompt (chat): {response}")
        
        # 7. Test tag generation scenario
        logger.info("Testing tag generation scenario...")
        system = """You are a tag generation expert. Please analyze the given text and generate the most relevant tags.
Requirements:
1. Each tag should be concise and accurate
2. Tags should reflect the main theme and concepts of the text
3. Return the format as a comma-separated list of tags
4. Do not explain, just return the list of tags"""
        
        test_text = """
        Python is a popular programming language, known for its concise syntax and rich ecosystem.
        It is widely used in fields such as web development, data analysis, and artificial intelligence.
        """
        # Try both modes
        response_gen = bridge.run(test_text, system=system, mode="generate")
        logger.info(f"Tag generation response (generate): {response_gen}")
        response_chat = bridge.run(test_text, system=system, mode="chat")
        logger.info(f"Tag generation response (chat): {response_chat}")
        
        logger.info("All tests completed")
        
    except Exception as e:
        logger.error(f"Error during testing: {e}")
        logger.error(traceback.format_exc())
        return False
        
    return True

if __name__ == "__main__":
    success = _test()
    sys.exit(0 if success else 1) 
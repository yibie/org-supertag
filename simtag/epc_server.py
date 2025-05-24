"""
SimTag EPC Server Module
Provides a unified EPC interface to connect Emacs with Python backend functionality
"""

import os
import sys
import json
import logging
import traceback
import argparse
import subprocess
from typing import List, Dict, Any, Optional, Tuple

from epc.server import EPCServer
from .config import Config
from .entity_extractor import EntityExtractor
from .ollama_bridge import OllamaBridge
from .tag_vectors import TagVectorEngine
from .utils.logging import setup_logging
from .utils.serialization import normalize_response
from .tag_generator import TagGenerator
from .tag_relation_analyzer import TagRelationAnalyzer, analyze_tag_relations

logger = logging.getLogger("simtag.epc_server")

class SimTagServer:
    """SimTag EPC Server Class"""
    
    def __init__(self, config: Config):
        """Initialize server
        
        Args:
            config: Configuration object
        """
        self.logger = logging.getLogger("simtag.epc_server")
        self.config = config
        self._initialized = False  # Add initialization flag
        
        # Initialize base components as None
        self.ollama = None
        self.tag_generator = None
        self.entity_extractor = None
        self.vector_engine = None
        
        # Initialize EPC server
        self.server = EPCServer((self.config.host, self.config.port))
        self._register_methods()
        
    def _register_methods(self):
        """Register EPC methods"""
        methods = [
            ('echo', self.echo),
            ('status', self.status),
            ('initialize', self.initialize),
            ('find_similar', self.find_similar),
            ('suggest_tags', self.suggest_tags),
            ('suggest_tags_json', self.suggest_tags_json),
            ('extract_entities', self.extract_entities),
            ('check_imports', self.check_imports),
            ('get_config', self.get_config),
            ('test_engine', self.test_engine),
            ('analyze_tag_relations', self.analyze_tag_relations),
            ('run_ollama', self.run_ollama),
            ('sync_library', self.sync_library),
            ('add_tag', self.add_tag),
            ('update_tag', self.update_tag),
            ('remove_tag', self.remove_tag)
        ]
        
        for name, method in methods:
            self.server.register_function(method)
            
    def start(self):
        """Start server"""
        try:
            port = self.server.server_address[1]
            self.logger.info(f"Server port obtained: {port}")
            
            # Ensure clean stdout
            sys.stdout.flush()  # Clear buffer
            
            # Important: Output port number on a separate line
            print(f"{port}", flush=True)
            self.logger.info(f"Port number output to stdout: {port}")
            
            # Start server
            self.logger.info("Starting serve_forever()...")
            self.server.serve_forever()
        except Exception as e:
            self.logger.error(f"Server startup failed: {e}")
            raise
        
    def echo(self, message: str) -> str:
        """Echo test method"""
        self.logger.info(f"Echo test: {message}")
        return f"Echo: {message}"
        
    def status(self) -> Dict[str, Any]:
        """Get server status
        
        Returns:
            Status information dictionary
        """
        status = {
            "server": {
                "running": True,
                "port": self.server.server_address[1]
            },
            "components": {
                "vector_engine": self.vector_engine.status() if self.vector_engine else None,
                "ollama": self.ollama.status() if self.ollama else None
            },
            "config": self.config.to_dict()
        }
        return normalize_response(status)

    """def initialize initialization function do not break update"""   
    def initialize(self, vector_file: str = None, db_file: str = None) -> Dict[str, Any]:
        """Initialize server components"""
        try:
            self.logger.info("Initializing server components...")
            
            # Update and validate file paths
            if vector_file:
                self.logger.info(f"Using specified vector file: {vector_file}")
                # For vector file, we don't require it to exist - it can be created during initialization
                self.config.vector_file = vector_file
                # Ensure the directory exists
                os.makedirs(os.path.dirname(vector_file), exist_ok=True)
            
            if db_file:
                self.logger.info(f"Using specified database file: {db_file}")
                if not os.path.exists(db_file):
                    self.logger.error(f"Specified database file does not exist: {db_file}")
                    return normalize_response(None, "error", f"Database file does not exist: {db_file}")
                self.config.db_file = db_file
            
            # Ensure Ollama is available
            if not self.config.ensure_ollama():
                raise Exception("Ollama is not installed or not available")
            
            # 1. Initialize Ollama
            self.logger.info("Initializing Ollama...")
            self.ollama = OllamaBridge(model=self.config.model_name)
            
            # 2. Initialize tag generator
            self.logger.info("Initializing tag generator...")
            self.tag_generator = TagGenerator(self.ollama)
            
            # 3. Initialize other components
            self.logger.info("Initializing other components...")
            self.entity_extractor = EntityExtractor(self.ollama)
            self.vector_engine = TagVectorEngine(vector_file=self.config.vector_file)
            
            # 4. Initialize the vector library with tag data
            self.logger.info("Initializing vector library...")
            vector_init_result = self.vector_engine.initialize(
                self.config.db_file, 
                self.config.vector_file
            )
            
            if vector_init_result.get("status") != "success":
                error_msg = f"Vector library initialization failed: {vector_init_result.get('message', 'Unknown error')}"
                self.logger.error(error_msg)
                return normalize_response(None, "error", error_msg)
            
            # Mark initialization complete
            self._initialized = True
            self.logger.info("All components initialized successfully")
            
            return normalize_response({
                "status": "success",
                "vector_file": self.config.vector_file,
                "db_file": self.config.db_file,
                "model": self.config.model_name,
                "vector_stats": vector_init_result.get("result", {})
            })
            
        except Exception as e:
            self._initialized = False  # Ensure marked as uninitialized on failure
            self.logger.error(f"Initialization failed: {e}")
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", str(e))
            
    def find_similar(self, tag_name: str, content: str = "", top_k: int = 5) -> Dict[str, Any]:
        """Find similar tags
        
        Args:
            tag_name: Tag name
            content: Related content
            top_k: Number of results to return
            
        Returns:
            List of similar tags
        """
        try:
            # Check initialization status
            if not self._initialized:
                self.logger.error("Service not initialized, please call initialize first")
                return normalize_response(None, "error", "Service not initialized, please call initialize first")
            
            # Check vector engine
            if not self.vector_engine:
                self.logger.error("Vector engine not initialized")
                return normalize_response(None, "error", "Vector engine not initialized")
            
            # Use hybrid search
            self.logger.info(f"Finding tags similar to '{tag_name}'...")
            results = self.vector_engine.find_similar(tag_name, top_k)
            
            return normalize_response(results)
            
        except Exception as e:
            error_msg = f"Failed to find similar tags: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", error_msg)

    def suggest_tags(self, text: str, limit: int = 5) -> Dict[str, Any]:
        """Generate tag suggestions"""
        try:
            # If not initialized, try auto-initialization
            if not self._initialized:
                self.logger.info("Service not initialized, attempting auto-initialization...")
                init_result = self.initialize()
                if init_result.get("status") != "success":
                    self.logger.error("Auto-initialization failed")
                    return normalize_response(None, "error", "Service initialization failed")
            
            if not self.tag_generator:
                self.logger.error("Tag generator not initialized")
                return normalize_response(None, "error", "Tag generator not initialized")
            
            # Get tag list
            self.logger.info("Starting tag generation...")
            self.logger.debug(f"Input text preview: {text[:100]}...")  # Add input text logging
            
            tags = self.tag_generator.suggest_tags(text)
            
            # Validate tag list
            if not tags:
                self.logger.warning("No tags generated")
                return normalize_response([])  # Return empty list instead of None
            
            if not isinstance(tags, list):
                self.logger.error(f"Tag generator returned non-list type: {type(tags)}")
                return normalize_response(None, "error", "Invalid tag format")
            
            # Ensure all tags are strings
            valid_tags = [str(tag).strip() for tag in tags if tag]
            
            self.logger.info(f"Successfully generated {len(valid_tags)} tags: {valid_tags}")
            
            # Return using normalize_response
            return normalize_response(valid_tags)
            
        except Exception as e:
            self.logger.error(f"Tag generation failed: {e}")
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", str(e))
            
    def extract_entities(self, text: str) -> Dict[str, Any]:
        """Extract entities (full version)
        
        Args:
            text: Text content
            
        Returns:
            List of entities
        """
        try:
            if not self.entity_extractor:
                raise Exception("Entity extractor not initialized")
                
            entities = self.entity_extractor.extract(text)
            return normalize_response(entities)
            
        except Exception as e:
            error_msg = f"Failed to extract entities: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", error_msg)
            

    def check_imports(self):
        """Check if required modules are properly imported."""
        try:
            import numpy
            import torch
            import sentence_transformers
            import requests
            return {
                "status": "success",
                "imports": {
                    "numpy": numpy.__version__,
                    "torch": torch.__version__,
                    "sentence_transformers": sentence_transformers.__version__,
                    "requests": requests.__version__
                }
            }
        except ImportError as e:
            return {
                "status": "error",
                "message": str(e)
            }

    def get_config(self):
        """Return current configuration information."""
        return {
            "vector_file": self.config.vector_file,
            "db_file": self.config.db_file,
            "model_name": self.config.model_name,
            "debug": self.config.debug
        }

    def test_engine(self, test_text: str) -> Dict[str, Any]:
        """Test text vector engine functionality
        
        Args:
            test_text: Test text
            
        Returns:
            Vector data
        """
        try:
            if not self.vector_engine:
                raise Exception("Vector engine not initialized")
                
            # Generate text vector
            self.logger.info(f"Starting text vector generation: {test_text}")
            vector = self.vector_engine.model.encode(test_text)
            
            # Record vector details
            self.logger.info(f"Vector type: {type(vector)}")
            self.logger.info(f"Vector shape: {vector.shape if hasattr(vector, 'shape') else len(vector)}")
            
            # Get vector data
            vector_data = vector.tolist() if hasattr(vector, 'tolist') else vector
            self.logger.info(f"Vector data length: {len(vector_data)}")
            
            # Return result
            result = {
                "vector": vector_data,
                "dimensions": len(vector_data),
                "model": self.vector_engine.model_name if hasattr(self.vector_engine, 'model_name') else None
            }
            
            return normalize_response(result)
            
        except Exception as e:
            error_msg = f"Engine test failed: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", error_msg)

    def analyze_tag_relations(self, tag: str, tags: list) -> Dict[str, Any]:
        """Analyze tag relationships
        
        Args:
            tag: Target tag
            tags: List of tags to analyze
            
        Returns:
            List of tag relationships
        """
        try:
            relations = self.tag_analyzer.analyze_relations(tag, tags)
            return {
                "status": "success",
                "result": relations
            }
        except Exception as e:
            return {
                "status": "error",
                "message": f"Failed to analyze tag relationships: {str(e)}"
            }

    def run_ollama(self, prompt, system=None):
        """Send message to Ollama and get response
        
        Args:
            prompt: User prompt text
            system: Optional system prompt text
            
        Returns:
            Dict: Dictionary containing processing results
        """
        self.logger.info(f"Received Ollama interaction request, prompt length: {len(prompt)}")
        
        # Check initialization status
        if not self._initialized:
            self.logger.error("Attempting to use Ollama before initialization")
            return normalize_response(None, "error", "Service not initialized, please call initialize first")
        
        # Check Ollama instance
        if not self.ollama:
            self.logger.error("Ollama instance not initialized")
            return normalize_response(None, "error", "Ollama instance not initialized")
        
        try:
            # Record additional information, avoiding logging long prompts
            prompt_preview = prompt[:100] + "..." if len(prompt) > 100 else prompt
            self.logger.info(f"Sending request to Ollama, prompt preview: {prompt_preview}")
            
            # Call Ollama
            response = self.ollama.run(prompt, system=system)
            
            # Check response
            if not response:
                return normalize_response(None, "error", "Ollama returned empty response")
            
            # Record response (partial to avoid large logs)
            response_preview = response[:100] + "..." if len(response) > 100 else response
            self.logger.info(f"Received Ollama response, length: {len(response)}, preview: {response_preview}")
            
            # Return success response
            return normalize_response(response, "success")
            
        except Exception as e:
            # Catch and log exception
            error_message = f"Ollama interaction error: {str(e)}"
            trace = traceback.format_exc()
            self.logger.error(f"{error_message}\n{trace}")
            return normalize_response(None, "error", error_message)
            
    def suggest_tags_json(self, json_data: str, limit: int = 5) -> Dict[str, Any]:
        """Process tag generation request using JSON format
        
        Args:
            json_data: JSON format request data containing text content to analyze
            limit: Result count limit
            
        Returns:
            List of tags
        """
        try:
            # Record received JSON data length
            self.logger.info(f"Received JSON format request, length: {len(json_data)}")
            
            # Parse JSON data
            try:
                import json
                request = json.loads(json_data)
                
                # Ensure JSON format is correct, contains content field
                if not isinstance(request, dict):
                    self.logger.error(f"JSON data is not dictionary format: {type(request)}")
                    return normalize_response(None, "error", "Invalid request format, should be JSON object")
                
                text = request.get("content")
                
                if not text:
                    self.logger.error("Request missing content field or empty")
                    return normalize_response(None, "error", "Request missing text content")
                
                self.logger.info(f"Text length extracted from JSON: {len(text)}")
                text_preview = text[:100] + "..." if len(text) > 100 else text
                self.logger.info(f"Text preview: {text_preview}")
                
            except json.JSONDecodeError as e:
                self.logger.error(f"JSON parsing failed: {e}")
                self.logger.error(f"Received JSON data: {json_data[:200]}..." if len(json_data) > 200 else json_data)
                return normalize_response(None, "error", f"JSON parsing failed: {e}")
            
            # Process text using standard suggest_tags method
            return self.suggest_tags(text, limit)
            
        except Exception as e:
            self.logger.error(f"JSON request processing failed: {e}")
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", str(e))

    def sync_library(self, db_file: str, tag_data: List[Dict[str, str]]) -> Dict[str, Any]:
        """Synchronize the vector library with the provided tag data.

        Args:
            db_file: The path to the source database file (for logging/reference).
            tag_data: A list of dictionaries, each containing 'id' and 'name' of a tag.

        Returns:
            A dictionary indicating the status of the operation.
        """
        self.logger.info(f"Received sync_library request. DB file: {db_file}, {len(tag_data)} tags received.")
        
        try:
            # Check initialization status
            if not self._initialized:
                self.logger.error("Service not initialized, please call initialize first")
                return normalize_response(None, "error", "Service not initialized, please call initialize first")

            # Check vector engine
            if not self.vector_engine:
                self.logger.error("Vector engine not initialized")
                return normalize_response(None, "error", "Vector engine not initialized")

            # Perform the synchronization
            # Assuming vector_engine has a method like sync_from_tags
            # You might need to adjust this method name based on your TagVectorEngine implementation
            result = self.vector_engine.sync_from_tags(tag_data)
            
            self.logger.info(f"Library synchronization completed. Result: {result}")
            return normalize_response({"status": "success", "details": result})

        except Exception as e:
            error_msg = f"Failed to synchronize library: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", error_msg)

    def update_tag(self, tag_data: List[Dict[str, str]]) -> Dict[str, Any]:
        """Update the name of a tag
        
        Args:
            tag_data: List containing tag information, format: [{"id": tag_id, "name": tag_name}]
            
        Returns:
            Dictionary indicating the status of the operation
        """
        try:
            # Check initialization status
            if not self._initialized:
                self.logger.error("Service not initialized, please call initialize first")
                return normalize_response(None, "error", "Service not initialized, please call initialize first")
            
            # Check vector engine
            if not self.vector_engine:
                self.logger.error("Vector engine not initialized")
                return normalize_response(None, "error", "Vector engine not initialized")
            
            # Extract tag information
            if not tag_data or not isinstance(tag_data, list) or len(tag_data) == 0:
                self.logger.error("Invalid tag data format")
                return normalize_response(None, "error", "Invalid tag data format")
            
            # Get the first tag data item
            tag_info = tag_data[0]
            if not isinstance(tag_info, dict):
                # Handle case where it's passed as a list of tuples
                if isinstance(tag_info, list):
                    tag_dict = {}
                    for item in tag_info:
                        if isinstance(item, tuple) and len(item) == 2:
                            key, value = item
                            tag_dict[key] = value
                    tag_info = tag_dict
                else:
                    self.logger.error(f"Invalid tag info format: {type(tag_info)}")
                    return normalize_response(None, "error", f"Invalid tag info format: {type(tag_info)}")
            
            tag_id = tag_info.get("id")
            tag_name = tag_info.get("name")
            
            if not tag_id:
                self.logger.error("Missing tag ID")
                return normalize_response(None, "error", "Missing tag ID")
            
            self.logger.info(f"Updating tag: {tag_id} -> '{tag_name}'")
            
            # Perform the update
            result = self.vector_engine.update_tag(tag_id, tag_name)
            
            if result:
                return normalize_response({"status": "success", "tag_id": tag_id, "updated": True})
            else:
                return normalize_response(None, "error", "Tag update failed")
            
        except Exception as e:
            error_msg = f"Failed to update tag: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", error_msg)

    def remove_tag(self, tag_id: str) -> Dict[str, Any]:
        """Remove a tag from the vector library
        
        Args:
            tag_id: ID of the tag to remove
            
        Returns:
            Dictionary indicating the status of the operation
        """
        try:
            # Check initialization status
            if not self._initialized:
                self.logger.error("Service not initialized, please call initialize first")
                return normalize_response(None, "error", "Service not initialized, please call initialize first")
            
            # Check vector engine
            if not self.vector_engine:
                self.logger.error("Vector engine not initialized")
                return normalize_response(None, "error", "Vector engine not initialized")
            
            # Perform the removal
            result = self.vector_engine.remove_tag(tag_id)
            
            if result:
                return normalize_response({"status": "success", "details": result})
            else:
                return normalize_response(None, "error", "Tag removal failed")
            
        except Exception as e:
            error_msg = f"Failed to remove tag: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", error_msg)

    def add_tag(self, tag_data: List[Dict[str, str]]) -> Dict[str, Any]:
        """Add a new tag to the vector library
        
        Args:
            tag_data: List containing tag information, format: [{"id": tag_id, "name": tag_name}]
            
        Returns:
            Dictionary indicating the status of the operation
        """
        try:
            # Check initialization status
            if not self._initialized:
                self.logger.error("Service not initialized, please call initialize first")
                return normalize_response(None, "error", "Service not initialized, please call initialize first")
            
            # Check vector engine
            if not self.vector_engine:
                self.logger.error("Vector engine not initialized")
                return normalize_response(None, "error", "Vector engine not initialized")
            
            # Extract tag information
            if not tag_data or not isinstance(tag_data, list) or len(tag_data) == 0:
                self.logger.error("Invalid tag data format")
                return normalize_response(None, "error", "Invalid tag data format")
            
            # Get the first tag data item
            tag_info = tag_data[0]
            if not isinstance(tag_info, dict):
                # Handle case where it's passed as a list of tuples
                if isinstance(tag_info, list):
                    tag_dict = {}
                    for item in tag_info:
                        if isinstance(item, tuple) and len(item) == 2:
                            key, value = item
                            tag_dict[key] = value
                    tag_info = tag_dict
                else:
                    self.logger.error(f"Invalid tag info format: {type(tag_info)}")
                    return normalize_response(None, "error", f"Invalid tag info format: {type(tag_info)}")
            
            tag_id = tag_info.get("id")
            tag_name = tag_info.get("name")
            
            if not tag_id:
                self.logger.error("Missing tag ID")
                return normalize_response(None, "error", "Missing tag ID")
            
            self.logger.info(f"Adding tag: {tag_id} -> '{tag_name}'")
            
            # Perform the addition
            result = self.vector_engine.add_tag(tag_id, tag_name)
            
            if result:
                return normalize_response({"status": "success", "tag_id": tag_id, "added": True})
            else:
                return normalize_response(None, "error", "Tag addition failed")
            
        except Exception as e:
            error_msg = f"Failed to add tag: {str(e)}"
            self.logger.error(error_msg)
            self.logger.error(traceback.format_exc())
            return normalize_response(None, "error", error_msg)

def run_ollama_model(text, model_name="gemma-3b-it"):
    """Run ollama command directly"""
    try:
        cmd = ["ollama", "run", model_name, text]
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        return result.stdout.strip()
    except subprocess.CalledProcessError as e:
        logger.error(f"Failed to run ollama command: {e}")
        return None

def main(config: Config):
    """Main function"""
    try:
        # Initialize logging
        log_level = logging.DEBUG if config.debug else logging.INFO
        setup_logging(config.log_file, log_level)
        
        # Record configuration information
        logger.info("SimTag EPC Server Configuration:")
        logger.info(f"Vector file: {config.vector_file}")
        logger.info(f"Database file: {config.db_file}")
        logger.info(f"Log file: {config.log_file}")
        logger.info(f"Debug mode: {config.debug}")
        
        # Create server instance
        server = SimTagServer(config)
        
        # Start server
        server.start()
        
    except Exception as e:
        logger.error(f"Server startup failed: {e}")
        logger.error(traceback.format_exc())
        sys.exit(1)

if __name__ == "__main__":
    # Parse command line arguments
    parser = argparse.ArgumentParser(description='SimTag EPC Server')
    parser.add_argument('--vector-file', help='Vector file path')
    parser.add_argument('--db-file', help='Database file path')
    parser.add_argument('--model', help='Model name')
    parser.add_argument('--debug', action='store_true', help='Enable debug mode')
    parser.add_argument('--log-file', help='Log file path')
    parser.add_argument('--host', default='127.0.0.1', help='Server address')
    parser.add_argument('--port', type=int, default=0, help='Server port')
    args = parser.parse_args()

    # Create configuration object
    config = Config(
        vector_file=args.vector_file,
        db_file=args.db_file,
        model_name=args.model,
        debug=args.debug,
        log_file=args.log_file,
        host=args.host,
        port=args.port
    )
    
    main(config) 
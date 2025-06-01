#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart (Original Author)
# Adapted for Org SuperTag
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com> (Original)
# Maintainer: Your Name
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import sys
import os
import logging # Ensure logging is imported early for diagnostics

# --- Early Diagnostic Logging --- 
# Get a basic logger for diagnostics before full setup
diag_logger = logging.getLogger("simtag_bridge_diag")
diag_handler = logging.StreamHandler(sys.stderr) # Log to stderr initially
diag_formatter = logging.Formatter('%(asctime)s - DIAG - %(levelname)s - %(message)s')
diag_handler.setFormatter(diag_formatter)
diag_logger.addHandler(diag_handler)
diag_logger.setLevel(logging.INFO)

diag_logger.info(f"Python Executable: {sys.executable}")
diag_logger.info(f"Initial sys.path: {sys.path}")
# --- End Early Diagnostic Logging ---

# Add the project root (parent of 'simtag' directory) to sys.path
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(SCRIPT_DIR)
if PROJECT_ROOT not in sys.path:
    sys.path.insert(0, PROJECT_ROOT)
diag_logger.info(f"Modified sys.path: {sys.path}")

import traceback
import threading
import argparse
import sexpdata # Added for _eval_in_emacs helper

from epc.server import ThreadingEPCServer
from epc.client import EPCClient # Added for self.emacs_client

from simtag.core.storage import VectorStorage
from simtag.core.tagging import TaggingEngine
from simtag.config import Config

# Removed: from .python_bridge.utils import init_epc_client, eval_in_emacs, logger as bridge_logger, close_epc_client

# Configure package-level logger
logger = logging.getLogger("simtag_bridge")
# We can customize this logger further if needed, for now, it will inherit root logger settings

def setup_simtag_logging(data_dir):
    """Sets up logging for the SimTag bridge."""
    log_file = os.path.join(data_dir, 'simtag_bridge.log')
    
    # Use existing "simtag_bridge" logger
    logger.setLevel(logging.INFO) # Ensure level is set
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    
    # Clear existing handlers to avoid duplication if this is called multiple times
    # or if the logger was already configured by basicConfig elsewhere.
    if logger.hasHandlers():
        logger.handlers.clear()

    # Stream handler for stderr
    stream_handler = logging.StreamHandler(sys.stderr)
    stream_handler.setFormatter(formatter)
    logger.addHandler(stream_handler)
    
    # File handler
    file_handler = logging.FileHandler(log_file, mode='a', encoding='utf-8')
    file_handler.setFormatter(formatter)
    logger.addHandler(file_handler)
    
    logger.info(f"SimTag Bridge logging initialized. Log file: {log_file}")
    # Return logger for convenience, though it's globally accessible
    return logger


class SimTagBridge:
    def __init__(self, emacs_epc_port, data_directory, server_host='127.0.0.1'):
        # Setup logging first, so all initialization steps are logged properly.
        # Note: setup_simtag_logging is called here, ensure it's idempotent or called only once.
        # If logger is global, this might reconfigure it. Consider passing data_directory to a global setup.
        # For now, assume this is the primary setup point for this specific logger.
        # setup_simtag_logging(data_directory) # Called in main, if bridge instance created there.
                                            # If bridge created elsewhere, ensure logging is set up.
                                            # For safety, ensure logger has at least a basic config if not yet set up.
        if not logger.hasHandlers():
            logging.basicConfig(level=logging.INFO, stream=sys.stderr, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
            logger.warning("SimTagBridge logger was not pre-configured; basicConfig used as fallback.")


        logger.info(f"Initializing SimTagBridge. Emacs EPC port: {emacs_epc_port}, Data directory: {data_directory}")
        
        self.emacs_client: EPCClient | None = None
        self._init_emacs_client(int(emacs_epc_port))

        self.data_dir = data_directory
        vector_db_path = os.path.join(self.data_dir, 'supertag_vector.db')
        
        # If setup_simtag_logging wasn't called before __init__ or if instance is created independently of main()
        # it might be better to ensure it's called here, or that main() definitely calls it before creating SimTagBridge.
        # For now, we assume main() handles the primary logging setup.
        
        logger.info(f"Using vector database path: {vector_db_path}")
        
        self.config = Config(vector_db_path=vector_db_path)
        
        self.storage = VectorStorage(self.config.vector_db_path)
        vec_version = self.storage.check_vec_extension()
        if vec_version:
            logger.info(f"Successfully loaded sqlite-vec extension, version: {vec_version}")
        else:
            logger.warning("sqlite-vec extension not loaded, vector search performance may be affected")
            
        self.engine = TaggingEngine(self.config, self.storage)
        
        self.server = ThreadingEPCServer((server_host, 0), log_traceback=True)
        self.server.allow_reuse_address = True
        
        self.server.register_instance(self)
        logger.info("Registered SimTagBridge instance methods with EPC server.")

        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.daemon = True
        self.server_thread.start()
        logger.info("SimTagBridge EPC server started in a new thread.")

        server_port = self.server.server_address[1]
        logger.info(f"SimTagBridge EPC server listening on port: {server_port}. Informing Emacs.")
        self._eval_in_emacs('org-supertag-bridge--handle-python-server-ready-signal', server_port)
        logger.info("Informed Emacs about the SimTagBridge EPC server port.")

    def _init_emacs_client(self, emacs_server_port):
        if self.emacs_client is None:
            try:
                logger.info(f"Initializing EPC client to Emacs on 127.0.0.1:{emacs_server_port}")
                self.emacs_client = EPCClient(("127.0.0.1", emacs_server_port), log_traceback=True)
                logger.info("EPC client to Emacs initialized.")
            except ConnectionRefusedError:
                logger.error(f"Connection refused when initializing EPC client to Emacs: {traceback.format_exc()}")
            except Exception as e:
                logger.error(f"An unexpected error occurred during EPC client initialization: {e}\n{traceback.format_exc()}")


    def _close_emacs_client(self):
        if self.emacs_client is not None:
            try:
                logger.info("Closing EPC client connection to Emacs.")
                self.emacs_client.close()
                self.emacs_client = None
                logger.info("EPC client to Emacs closed.")
            except Exception as e:
                logger.error(f"Error closing EPC client to Emacs: {e}\n{traceback.format_exc()}")


    def _handle_arg_types_for_eval(self, arg):
        # Helper for _eval_in_emacs
        if type(arg) is str and arg.startswith("'"): # Check if it's a quoted symbol
            try:
                # Attempt to create a symbol, removing the initial quote
                return sexpdata.Symbol(arg[1:])
            except Exception as e:
                logger.warning(f"Failed to convert '{arg}' to sexpdata.Symbol, using as raw string: {e}")
                return arg # Fallback to raw string if Symbol creation fails
        # For other types, or strings not starting with a quote, wrap them.
        # However, symbols and numbers might not need quoting for simple `eval-in-emacs` calls.
        # The original `handle_arg_types` wrapped everything in `sexpdata.Quoted(arg)`.
        # This might be too aggressive if `method_name` is already a symbol and args are simple types.
        # Let's reconsider based on typical `eval_in_emacs` usage.
        # If `eval-in-emacs` in Elisp expects (func 'arg1 'arg2), then args need quoting.
        # If it expects (func arg1 arg2) where args are already values, no quoting needed for values.
        # For now, let's stick closer to quoting arguments passed to the target function.
        if isinstance(arg, (sexpdata.Symbol, int, float, bool)): # Don't quote symbols or basic numerics/bools
             return arg
        return sexpdata.Quoted(arg) # Quote strings and other complex types


    def _eval_in_emacs(self, method_name_str, *args):
        if self.emacs_client is None:
            logger.error("EPC client to Emacs is not initialized. Cannot eval in Emacs.")
            return

        try:
            # Convert method_name_str to a Symbol for the function call
            method_symbol = sexpdata.Symbol(method_name_str)
            
            # Process arguments
            processed_args = [self._handle_arg_types_for_eval(arg) for arg in args]
            
            # Construct the S-expression: (method_symbol arg1 arg2 ...)
            s_expression_parts = [method_symbol] + processed_args
            sexp_to_eval = sexpdata.dumps(s_expression_parts)

            logger.debug(f"Evaluating in Emacs: {sexp_to_eval}")
            # The 'eval-in-emacs' Elisp function is expected to take a single string argument,
            # which is the S-expression to evaluate.
            self.emacs_client.call("eval-in-emacs", [sexp_to_eval])
        except Exception as e:
            logger.error(f"Error evaluating in Emacs ('{method_name_str}' with args {args}): {e}\n{traceback.format_exc()}")


    # --- Methods to be exposed via EPC ---
    
    def echo(self, message):
        logger.info(f"echo received: {message}")
        return f"SimTagBridge echoes: {message}"

    def ping(self):
        logger.info("ping received")
        return "pong"

    def initialize_simtag(self, vector_file_path, db_file_path):
        logger.info(f"initialize_simtag called with vector_file: {vector_file_path}, db_file: {db_file_path}")
        try:
            if vector_file_path and os.path.isabs(vector_file_path):
                self.config.vector_db_path = vector_file_path
                self.storage = VectorStorage(vector_file_path)
                self.engine = TaggingEngine(self.config, self.storage)
                logger.info(f"Vector database path updated to: {vector_file_path}")
            else:
                logger.warning(f"Invalid or relative vector file path provided: {vector_file_path}. Using existing: {self.config.vector_db_path}")
            
            return {"status": "success", "result": {
                "vector_db_path": self.config.vector_db_path,
                "db_file_path": db_file_path 
            }}
        except Exception as e:
            logger.error(f"SimTag initialization failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def suggest_tags(self, text, limit=5, use_ai=True):
        logger.info(f"suggest_tags called with text (len: {len(text)}), limit: {limit}, use_ai: {use_ai}")
        try:
            result = self.engine.generate_tags(text, limit, use_ai)
            return {"status": "success", "result": result}
        except Exception as e:
            logger.error(f"Tag suggestion failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def suggest_tags_json(self, json_data):
        logger.info(f"suggest_tags_json called with data: {json_data}")
        try:
            import json
            if isinstance(json_data, str):
                data = json.loads(json_data)
            else:
                data = json_data
            
            content = data.get('content', '')
            limit = data.get('limit', 5)
            use_ai = data.get('use_ai', True)
            
            if not content:
                return {"status": "error", "message": "Content is empty"}
            
            tags = self.engine.generate_tags(content, limit, use_ai)
            return {"status": "success", "result": tags}
        except Exception as e:
            logger.error(f"JSON tag suggestion failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def extract_entities(self, text, use_ai=True):
        logger.info(f"extract_entities called with text (len: {len(text)}), use_ai: {use_ai}")
        try:
            result = self.engine.extract_entities(text, use_ai)
            return {"status": "success", "result": result}
        except Exception as e:
            logger.error(f"Entity extraction failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def find_similar_tags(self, tag, limit=10):
        logger.info(f"find_similar_tags called for tag: {tag}, limit: {limit}")
        try:
            similar_tags = self.engine.find_similar_tags(tag, limit)
            result = [[tag_id, float(score)] for tag_id, score in similar_tags]
            return {"status": "success", "result": result}
        except Exception as e:
            logger.error(f"Finding similar tags failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def analyze_tag_relations(self, tag, related_tags, use_ai=True):
        logger.info(f"analyze_tag_relations for tag: {tag}, related_tags: {related_tags}, use_ai: {use_ai}")
        try:
            result = self.engine.analyze_relations(tag, related_tags, use_ai)
            return {"status": "success", "result": result}
        except Exception as e:
            logger.error(f"Tag relation analysis failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def sync_library(self, db_file, tag_data):
        logger.info(f"sync_library called with db_file: {db_file}, num_tags: {len(tag_data) if isinstance(tag_data, list) else 'N/A'}")
        try:
            if not isinstance(tag_data, (list, tuple)):
                logger.warning(f"Tag data is not a list or tuple: {type(tag_data)}")
                tag_data = []
            result = self.engine.sync_tags(db_file, tag_data)
            return {"status": "success", "result": result}
        except Exception as e:
            logger.error(f"Sync library failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def update_tag(self, tag_info):
        logger.info(f"update_tag called with tag_info: {tag_info}")
        try:
            if isinstance(tag_info, (list, tuple)) and len(tag_info) >= 2:
                tag_id, tag_name = tag_info[0], tag_info[1]
            elif isinstance(tag_info, dict) and "id" in tag_info and "name" in tag_info:
                tag_id, tag_name = tag_info["id"], tag_info["name"]
            else:
                raise ValueError(f"Unrecognized tag_info format: {tag_info}")
            
            result = self.engine.update_tag_vector(tag_id, tag_name)
            return {"status": "success", "result": result}
        except Exception as e:
            logger.error(f"Update tag failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def remove_tag(self, tag_id):
        logger.info(f"remove_tag called for tag_id: {tag_id}")
        try:
            result = self.engine.remove_tag_vector(tag_id)
            return {"status": "success", "result": result}
        except Exception as e:
            logger.error(f"Remove tag failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def get_status(self):
        logger.info("get_status called")
        try:
            status = {
                "ollama_available": self.engine.ollama.available,
                "storage_ready": True, 
                "storage_stats": self.storage.get_stats(),
                "server_running": True, 
                "emacs_client_connected": self.emacs_client is not None,
                "config": {
                    "vector_db_path": self.config.vector_db_path,
                    "ollama_model": getattr(self.config, 'ollama_model', 'default')
                }
            }
            return {"status": "success", "result": status}
        except Exception as e:
            logger.error(f"Get status failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def get_config(self):
        logger.info("get_config called")
        return {
            "status": "success", 
            "config": {
                "vector_db_path": self.config.vector_db_path,
                "ollama_model": getattr(self.config, 'ollama_model', 'default')
            }
        }
        
    def check_imports(self):
        logger.info("check_imports called")
        missing_modules = []
        try:
            import numpy
        except ImportError:
            missing_modules.append("numpy")
        try:
            import requests
        except ImportError:
            missing_modules.append("requests")
        
        if not missing_modules:
            return {"status": "success", "modules_ok": ["numpy", "requests", "epc"]}
        else:
            msg = f"Missing critical modules: {', '.join(missing_modules)}"
            logger.error(msg)
            return {"status": "error", "message": msg, "missing": missing_modules}

    def cleanup(self):
        logger.info("SimTagBridge cleanup method called.")
        self._close_emacs_client() # Close client connection to Emacs
        
        if self.server:
            logger.info("Shutting down SimTagBridge EPC server...")
            try:
                self.server.shutdown() 
                self.server.server_close() 
            except Exception as e:
                logger.error(f"Error shutting down SimTagBridge EPC server: {e}\n{traceback.format_exc()}")
            finally:
                logger.info("SimTagBridge EPC server shut down attempt finished.")


def main():
    parser = argparse.ArgumentParser(description="Org SuperTag EPC Bridge Server (Python)")
    parser.add_argument("emacs_epc_port", type=int, help="Port number of the Emacs-side EPC server.")
    parser.add_argument("data_directory", type=str, help="Path to the data directory for org-supertag.")
    parser.add_argument("--host", type=str, default="127.0.0.1", help="Host for this Python EPC server to listen on (default: 127.0.0.1).")
    parser.add_argument("--profile", action="store_true", help="Enable cProfile for performance analysis.")
    
    args = parser.parse_args()

    # Crucial: Setup logging right at the start, using the data_directory for log file placement.
    # This ensures all subsequent operations, including SimTagBridge initialization, are logged.
    setup_simtag_logging(args.data_directory)

    logger.info(f"Starting SimTagBridge with args: Port={args.emacs_epc_port}, DataDir='{args.data_directory}', Host='{args.host}', Profile={args.profile}")

    bridge_instance = None # Define bridge_instance to ensure it's in scope for finally block

    if args.profile:
        import cProfile
        profiler = cProfile.Profile()
        try:
            # Note: We need an instance for cleanup, so profiling SimTagBridge class directly is tricky
            # if we want to call instance methods like cleanup in finally.
            # A common pattern is to profile a function that creates and runs the instance.
            def profiled_run():
                nonlocal bridge_instance
                bridge_instance = SimTagBridge(args.emacs_epc_port, args.data_directory, server_host=args.host)
                if bridge_instance and bridge_instance.server_thread:
                     bridge_instance.server_thread.join()
            
            profiler.runcall(profiled_run)

        finally:
            profile_path = os.path.join(args.data_directory, "simtag_bridge.prof")
            profiler.dump_stats(profile_path)
            logger.info(f"Profiling data saved to {profile_path}")
            if bridge_instance: # If instance was created
                bridge_instance.cleanup() # Explicitly call cleanup for profiled run
    else:
        try:
            bridge_instance = SimTagBridge(args.emacs_epc_port, args.data_directory, server_host=args.host)
            if bridge_instance.server_thread:
                bridge_instance.server_thread.join() 
        except KeyboardInterrupt:
            logger.info("KeyboardInterrupt received, shutting down SimTagBridge.")
        finally:
            if bridge_instance: # If instance was created
                logger.info("SimTagBridge main process initiating cleanup...")
                bridge_instance.cleanup() # Call cleanup on the instance
            else:
                logger.info("SimTagBridge main process exiting (no instance for cleanup).")

    logger.info("SimTagBridge main process finished.")


if __name__ == "__main__":
    main() 
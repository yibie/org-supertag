#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import os
import logging
import traceback
import threading
import argparse
import asyncio
import time
import json

from epc.server import ThreadingEPCServer

# --- Project Setup ---
# Add project root to sys.path to allow for absolute imports
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(SCRIPT_DIR)
if PROJECT_ROOT not in sys.path:
    sys.path.insert(0, PROJECT_ROOT)

from simtag.context import context
from simtag.utils.unified_tag_processor import normalize_payload
from typing import Dict, Any

# --- Logging ---
# Note: Full logging to file is initialized inside SimTagBridge
# after the data_directory is known.
logger = logging.getLogger("simtag_bridge")

class SimTagBridge:
    def __init__(self, port_file: str, data_directory: str, server_port: int, server_host: str = '127.0.0.1'):
        # --- Logging Setup ---
        log_file = os.path.join(data_directory, 'simtag_bridge.log')
        if not os.path.exists(data_directory):
            os.makedirs(data_directory)
            
        package_logger = logging.getLogger("simtag")
        package_logger.setLevel(logging.DEBUG)
        formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        
        if package_logger.hasHandlers():
            package_logger.handlers.clear()
            
        stream_handler = logging.StreamHandler(sys.stderr)
        stream_handler.setFormatter(formatter)
        package_logger.addHandler(stream_handler)
        
        file_handler = logging.FileHandler(log_file, mode='a', encoding='utf-8')
        file_handler.setFormatter(formatter)
        package_logger.addHandler(file_handler)
        
        logging.getLogger("simtag.bridge_setup").info(f"SimTag package logging initialized. Log file: {log_file}")
        
        # --- Core Initialization ---
        self.server_host = server_host
        self.port = server_port
        self.port_file = port_file
        self.shutdown_event = threading.Event()
        
        # --- Request Tracking ---
        self.running_tasks = {}  # request_id -> task_info
        self.task_lock = threading.Lock()
        self.request_counter = 0
        
        # --- Component Setup ---
        self._init_event_loop()
        
        
        # 1. Initialize the EPC Server
        self.server = ThreadingEPCServer((self.server_host, self.port), log_traceback=True)
        self.server.allow_reuse_address = True
        
        # 2. Initialize all application services via the global context
        # This must be done *before* registering methods that use these services.
        context.initialize(port=self.port, emacs_client=None) # emacs_client can be set later if needed
        
        # 3. Register methods with the server
        self._register_methods()
        
        # 4. Start the server in a separate thread
        self.server_thread = threading.Thread(target=self._run_server, daemon=True)
        self.server_thread.start()
        
        # 5. Write the actual port number to the port file for Emacs to read
        actual_port = self.server.server_address[1]
        logger.info(f"SimTagBridge EPC server listening on port: {actual_port}. Writing to port file: {self.port_file}")
        try:
            with open(self.port_file, 'w') as f:
                f.write(str(actual_port))
            logger.info("Successfully wrote port to file.")
        except IOError as e:
            logger.error(f"FATAL: Could not write to port file {self.port_file}. Error: {e}", exc_info=True)
            self.shutdown()

    def _init_event_loop(self):
        """Initializes the asyncio event loop and starts it in a separate thread."""
        self.loop = asyncio.new_event_loop()
        self.loop_thread = threading.Thread(target=self._run_event_loop, daemon=True)
        self.loop_thread.start()
        logger.info("Dedicated asyncio event loop thread started.")

    def _run_server(self):
        """Runs the EPC server loop until a shutdown is requested."""
        logger.info("EPC server thread started.")
        self.server.serve_forever()
        logger.info("EPC server thread has shut down.")

    def _run_event_loop(self):
        """Runs the asyncio event loop and logs its lifecycle."""
        asyncio.set_event_loop(self.loop)
        logger.info("Event loop thread started and running.")
        try:
            self.loop.run_forever()
        except Exception as e:
            logger.error(f"""Event loop thread crashed with an exception: {e}
{traceback.format_exc()}""")
        finally:
            self.loop.close()
            logger.info("Event loop has been closed and thread is terminating.")

    def _register_methods(self):
        """Register all available EPC methods by referencing handlers from the global context."""
        logger.info("Registering EPC methods...")
        
        # Core
        self.server.register_function(self.ping, 'ping')
        self.server.register_function(self.echo, 'echo')
        self.server.register_function(self.cleanup, 'cleanup')

        # Diagnostics
        self.server.register_function(self.get_status, 'diagnostics/get_status')
        self.server.register_function(self.get_config, 'diagnostics/get_config')
        self.server.register_function(self.check_imports, 'diagnostics/check_imports')
        self.server.register_function(self.test_embedding_retrieval, 'diagnostics/test_embedding_retrieval')
        self.server.register_function(self.get_processing_status, 'diagnostics/get_processing_status')
        self.server.register_function(self.print_processing_report, 'diagnostics/print_processing_report')
        self.server.register_function(self.get_available_models, 'diagnostics/get_available_models')
        self.server.register_function(self.get_default_model, 'diagnostics/get_default_model')

        # Sync
        self.server.register_function(self.bulk_process_snapshot, 'sync/bulk_process')
        
        # Query
        self.server.register_function(context.query_handler.get_similar_nodes, 'query/get_similar_nodes')
        self.server.register_function(self.get_similar_entities, 'query/get_similar_entities')
        
        # Autotag
        self.server.register_function(self.batch_generate_tags, 'autotag/batch_generate_tags')
        self.server.register_function(self.generate_single_node_tags, 'autotag/generate_single_node_tags')
         
        # RAG Assistant
        self.server.register_function(self.query, 'rag/query')
        
        # AI Commands (dedicated interfaces for chat commands)
        self.server.register_function(self.suggest_tags, 'ai/suggest-tags')
        self.server.register_function(self.find_connections, 'ai/find-connections')
        self.server.register_function(self.expand_content, 'ai/expand')

        # Embedding maintenance
        self.server.register_function(self.refresh_stale_tags, 'embedding/refresh_stale_tags')
        
        # Reasoning
        # New KnowledgeHandler cycle (replaces old reasoning/run_cycle)
        self.server.register_function(self.run_knowledge_cycle, 'knowledge/run_cycle')
        # Maintain backward compatibility with older Emacs versions
        self.server.register_function(self.run_inference_cycle, 'reasoning/run_cycle')
        # New queue status interface
        self.server.register_function(self.get_knowledge_queue_status, 'knowledge/get_queue_status')
        # Backward compatibility
        self.server.register_function(self.get_reasoning_queue_status, 'reasoning/get_queue_status')

        # Feedback
        self.server.register_function(self.submit_feedback, 'feedback/submit')
        self.server.register_function(self.debug_payload, 'debug/payload')
        
        # Smart Companion
        self.server.register_function(self.analyze_tag_context, 'smart_companion/analyze_tag_context')
        
        # Task Management
        self.server.register_function(self._get_running_tasks, 'diagnostics/get_running_tasks')
        
        logger.info("All EPC methods registered successfully.")

    def echo(self, message):
        """Echoes back the received message."""
        logger.info(f"Received echo request: {message}")
        return message

    def ping(self):
        """A simple method to check if the server is alive."""
        logger.info("Received ping.")
        return "pong"

    def cleanup(self):
        """Shuts down the bridge and its resources gracefully."""
        logger.info("Received shutdown request. Cleaning up resources...")
        self.shutdown_event.set()
        if self.loop.is_running():
            asyncio.run_coroutine_threadsafe(self.final_async_cleanup(), self.loop)
        logger.info("Graceful shutdown complete.")

    async def final_async_cleanup(self):
        tasks = [t for t in asyncio.all_tasks(loop=self.loop) if t is not asyncio.current_task()]
        if tasks:
            logger.info(f"Cancelling {len(tasks)} outstanding async tasks...")
            for task in tasks:
                task.cancel()
            await asyncio.gather(*tasks, return_exceptions=True)
            logger.info("All outstanding async tasks cancelled.")
        self.loop.stop()

    # --- Method Implementations ---
    # All methods now delegate to the handlers stored in the global `context`.

    def get_similar_entities(self, *args):
        """Run get_similar_entities in the async event loop."""
        return self._run_async(context.query_handler.get_similar_entities(*args))

    def get_status(self):
        return context.diagnostics_handler.get_status()

    def get_config(self):
        return context.diagnostics_handler.get_config()

    def check_imports(self):
        return context.diagnostics_handler.check_imports()

    def test_embedding_retrieval(self, text: str):
        return self._run_async(context.diagnostics_handler.test_embedding_retrieval(text))

    def get_processing_status(self):
        return self._run_async(context.diagnostics_handler.get_processing_status())

    def print_processing_report(self):
        return self._run_async(context.diagnostics_handler.print_processing_report())



    def get_available_models(self):
        return self._run_async(context.diagnostics_handler.get_available_models())

    def get_default_model(self):
        return self._run_async(context.diagnostics_handler.get_default_model())

    

    def bulk_process_snapshot(self, *args) -> Dict[str, Any]:
        try:
            logger.info("Bridge: Received call for bulk_process_snapshot.")
            payload = normalize_payload(args)
            logger.info(f"Bridge: Payload normalized. Processing {len(payload.get('entities', []))} entities.")

            # --- Ad-hoc Data Transformation (Temporary) ---
            # Standardize link types from Elisp symbols to Python strings
            if 'links' in payload and isinstance(payload['links'], list):
                for link in payload['links']:
                    # Elisp sends links as alists, which EPC converts to Python lists of lists.
                    # e.g., [['type', '.', ':node-tag'], ['from', '.', 'id1'], ...]
                    type_found = False
                    for i, pair in enumerate(link):
                        if isinstance(pair, (list, tuple)) and len(pair) > 0 and pair[0] == 'type':
                            # Check for Elisp symbol `:'node-tag`
                            if len(pair) > 2 and pair[2] == "':node-tag":
                                link[i] = ['type', '.', '"HAS_TAG"']
                                type_found = True
                                logger.debug(f"Transformed link type for link from {link[1][2]}")
                            break
                    if not type_found:
                        # If no type specified, assume it's a node-tag link for now
                        pass # Let the handler deal with it, or insert a default.

            # The coroutine to be executed
            coro = context.sync_handler.bulk_process_snapshot(payload)

            # Run the async task and get the result
            result = self._run_async(coro)

            logger.info(f"Bridge: Async task finished. Result before returning to Emacs: {result}")

            # Ensure we always return a dictionary to Emacs
            if not isinstance(result, dict):
                logger.error(f"Bridge: Result is not a dictionary (type: {type(result)}). Returning error state.")
                return {"status": "error", "message": "Internal bridge error: result was not a dictionary."}

            return result

        except Exception as e:
            logger.error(f"Bridge: Unhandled exception in bulk_process_snapshot: {e}", exc_info=True)
            return {"status": "error", "message": f"Unhandled bridge exception: {e}"}

    # --- Placeholder Governance Methods ---
    # def sync_tag_event(self, event_data: Dict[str, Any]) -> Dict[str, Any]:
    #     return self._run_async(context.sync_handler.sync_tag_event(event_data))
    # def update_tag_status(self, tag_data: Dict[str, Any]) -> Dict[str, Any]:
    #     return self._run_async(context.sync_handler.update_tag_status(tag_data))
    # def update_tag_rules(self, tag_data: Dict[str, Any]) -> Dict[str, Any]:
    #     return self._run_async(context.sync_handler.update_tag_rules(tag_data))
    # def update_relation_type(self, relation_data: Dict[str, Any]) -> Dict[str, Any]:
    #     return self._run_async(context.sync_handler.update_relation_type(relation_data))
    # def update_relation_rules(self, relation_data: Dict[str, Any]) -> Dict[str, Any]:
    #     return context.sync_handler.update_relation_rules(relation_data)

    # ------------------------------------------------------------------
    # AI Commands (Dedicated interfaces)
    # ------------------------------------------------------------------

    def suggest_tags(self, *args):
        """Generate intelligent tag suggestions for content."""
        return self._run_async(context.ai_handler.suggest_tags(*args), method_name="ai/suggest-tags", args=args)

    def find_connections(self, *args):
        """Find knowledge connections for a given tag."""
        return self._run_async(context.ai_handler.find_connections(*args), method_name="ai/find-connections", args=args)

    def expand_content(self, *args):
        """Expand and elaborate on given content or topic."""
        return self._run_async(context.ai_handler.expand_content(*args), timeout=240, method_name="ai/expand", args=args)

    def batch_generate_tags(self, *args):
        try:
            payload = normalize_payload(args)
            coro = context.autotag_handler.batch_generate_tags(payload)
            return self._run_async(coro)
        except Exception as e:
            logger.error(f"""Error in BATCH_GENERATE_TAGS bridge call: {e}\n{traceback.format_exc()}""")
            return {"error": "batch_generate_tags_failed", "message": str(e)}

    def generate_single_node_tags(self, *args) -> Dict[str, Any]:
        try:
            payload = normalize_payload(args)
            coro = context.autotag_handler.suggest_tags_for_single_node_elisp(payload)
            return self._run_async(coro)
        except Exception as e:
            logger.error(f"""Error calling async generate_single_node_tags handler: {e}\n{traceback.format_exc()}""")
            return {"status": "error", "message": str(e)}

    

    # ------------------------------------------------------------------
    # RAG Query (New unified chat endpoint)
    # ------------------------------------------------------------------

    def query(self, *args):
        """Forward generic chat/query requests to RAGHandler."""
        return self._run_async(context.rag_handler.query(*args), method_name="rag/query", args=args)
        
    # ------------------------------------------------------------------
    # Knowledge processing cycles
    # ------------------------------------------------------------------

    def run_knowledge_cycle(self, limit: int = 5):
        """Preferred new entry point for background knowledge extraction."""
        return self._run_async(context.knowledge_handler.run_extraction_cycle(limit))

    # Backward-compat alias
    def run_inference_cycle(self, limit: int = 5):
        return self.run_knowledge_cycle(limit)

    def get_reasoning_queue_status(self):
        return self._run_async(context.knowledge_handler.get_queue_status())

    def get_knowledge_queue_status(self):
        return self._run_async(context.knowledge_handler.get_queue_status())

    def submit_feedback(self, *args):
        return self._run_async(context.feedback_handler.submit_feedback(*args))
        
    def debug_payload(self, *args):
        logger.info(f"--- DEBUG PAYLOAD ---\nType: {type(args)}\nContent: {args}\n--- END DEBUG ---")
        return args

    # ------------------------------------------------------------------
    # Smart Companion Methods
    # ------------------------------------------------------------------

    def analyze_tag_context(self, *args):
        """Analyze tag context for smart companion suggestions."""
        return self._run_async(context.smart_companion_handler.analyze_tag_context(*args))

    def refresh_stale_tags(self, batch_size: int = 20):
        """RPC: Refresh embeddings for all STALE tags (knowledge_status='STALE')."""
        logger.info(f"RPC call: embedding/refresh_stale_tags (batch_size={batch_size})")
        async def _run():
            await context.embedding_service.refresh_stale_tags(batch_size=batch_size)
            return {"status": "success"}
        # Run in event loop and wait for result synchronously for EPC
        future = asyncio.run_coroutine_threadsafe(_run(), self.loop)
        return future.result()

    def _generate_request_id(self, method_name: str) -> str:
        """Generate unique request ID for tracking."""
        with self.task_lock:
            self.request_counter += 1
            return f"{method_name}_{int(time.time())}_{self.request_counter}"
    
    def _register_task(self, request_id: str, method_name: str, args: Any) -> None:
        """Register a running task."""
        with self.task_lock:
            self.running_tasks[request_id] = {
                "method": method_name,
                "start_time": time.time(),
                "args": str(args)[:200],  # Truncate for logging
                "status": "running"
            }
            logger.debug(f"Task registered: {request_id} - {method_name}")
    
    def _unregister_task(self, request_id: str) -> None:
        """Unregister a completed task."""
        with self.task_lock:
            if request_id in self.running_tasks:
                task_info = self.running_tasks.pop(request_id)
                duration = time.time() - task_info["start_time"]
                logger.debug(f"Task completed: {request_id} - duration: {duration:.2f}s")
    
    def _get_running_tasks(self) -> Dict[str, Any]:
        """Get current running tasks."""
        with self.task_lock:
            return dict(self.running_tasks)
    
    def _run_async(self, coro, timeout=300, method_name="unknown", args=None):
        if self.shutdown_event.is_set():
            logger.warning("Shutdown in progress. Rejecting new async task.")
            return {"error": "shutdown_in_progress"}
        if not self.loop_thread.is_alive() or not self.loop.is_running():
            logger.error("CRITICAL: Event loop is not running. Cannot schedule task.")
            return {"error": "event_loop_dead"}
        
        request_id = self._generate_request_id(method_name)
        self._register_task(request_id, method_name, args)
        
        async def tracked_coro():
            try:
                result = await coro
                return {"result": result, "request_id": request_id}
            except asyncio.CancelledError:
                logger.warning(f"Task cancelled: {request_id}")
                raise
            finally:
                self._unregister_task(request_id)
        
        future = asyncio.run_coroutine_threadsafe(tracked_coro(), self.loop)
        try:
            response = future.result(timeout=timeout)
            if isinstance(response, dict) and "request_id" in response:
                return response["result"]
            return response
        except asyncio.TimeoutError:
            logger.error(f"Async task timed out: {request_id}")
            self._unregister_task(request_id)
            return {"error": "timeout", "request_id": request_id}
        except Exception as e:
            logger.error(f"Error running async task {request_id}: {e}\n{traceback.format_exc()}")
            self._unregister_task(request_id)
            return {"error": "async_execution_error", "message": str(e), "request_id": request_id}

def main():
    parser = argparse.ArgumentParser(description="SimTag EPC Bridge for Emacs.")
    parser.add_argument("--port-file", type=str, required=True, help="File to write the dynamic port number to.")
    parser.add_argument("--data-directory", type=str, required=True, help="Path to the org-supertag data directory.")
    parser.add_argument("--port", type=int, default=0, help="Port for the Python EPC server. 0 means dynamic.")
    parser.add_argument("--profile", action='store_true', help="Enable profiling.")
    args = parser.parse_args()

    # Initialize the bridge. This will also initialize the context and all services.
    bridge = SimTagBridge(
        port_file=args.port_file,
        data_directory=args.data_directory,
        server_port=args.port
    )

    # Profiling setup
    profiler = None
    if args.profile:
        import cProfile
        profiler = cProfile.Profile()
        
        def save_profile():
            profile_dir = os.path.join(args.data_directory, "profiles")
            if not os.path.exists(profile_dir):
                os.makedirs(profile_dir)
            timestamp = time.strftime("%Y%m%d-%H%M%S")
            profile_file = os.path.join(profile_dir, f"simtag_bridge_{timestamp}.prof")
            profiler.dump_stats(profile_file)
            logging.getLogger("simtag_bridge").info(f"Profiling data saved to {profile_file}")

        def profiled_run():
            try:
                profiler.enable()
                bridge.shutdown_event.wait()
            finally:
                profiler.disable()
                save_profile()
        
        profiled_thread = threading.Thread(target=profiled_run, daemon=True)
        profiled_thread.start()
    else:
        bridge.shutdown_event.wait()

    logging.getLogger("simtag_bridge").info("Shutdown event received. Exiting main thread.")

if __name__ == '__main__':
    main() 

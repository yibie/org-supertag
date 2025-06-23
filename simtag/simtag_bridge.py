#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import os
import logging
import traceback
import threading
import argparse
import sexpdata
import asyncio

from epc.server import ThreadingEPCServer
from epc.client import EPCClient

from dependency_injector.wiring import inject, Provide
from simtag.container import AppContainer
from simtag.module.sync_handler import SyncHandler
from simtag.module.query_handler import QueryHandler
from simtag.module.diagnostics_handler import DiagnosticsHandler
from simtag.module.autotag_handler import AutotagHandler
from simtag.module.resonance_handler import ResonanceHandler
from simtag.config import Config
from typing import Dict, List, Any, Optional

from simtag.utils.unified_tag_processor import normalize_payload

# --- Logging Setup ---
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(SCRIPT_DIR)
if PROJECT_ROOT not in sys.path:
    sys.path.insert(0, PROJECT_ROOT)

logger = logging.getLogger("simtag_bridge")

def setup_simtag_logging(data_dir):
    """Sets up logging for the SimTag bridge."""
    log_file = os.path.join(data_dir, 'simtag_bridge.log')
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
    return package_logger

class SimTagBridge:
    def __init__(self, container: AppContainer, emacs_epc_port: int, server_host: str = '127.0.0.1'):
        logger.info(f"Initializing SimTagBridge. Emacs EPC port: {emacs_epc_port}")
        
        self.container = container
        self.emacs_client: Optional[EPCClient] = None
        self._init_emacs_client(emacs_epc_port)

        self.container.config.emacs_client.from_value(self.emacs_client)

        self.server = ThreadingEPCServer((server_host, 0), log_traceback=True)
        self.server.allow_reuse_address = True
        
        self._register_methods()
        logger.info("Registered SimTagBridge instance methods with EPC server.")

        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.daemon = True
        self.server_thread.start()
        logger.info("SimTagBridge EPC server started in a new thread.")

        server_port = self.server.server_address[1]
        logger.info(f"SimTagBridge EPC server listening on port: {server_port}. Informing Emacs.")
        self._eval_in_emacs('org-supertag-bridge--handle-python-server-ready-signal', server_port)
        logger.info("Informed Emacs about the SimTagBridge EPC server port.")

    def _register_methods(self):
        """Register all available EPC methods."""
        logger.info("Registering EPC methods...")
        
        # Core
        self.server.register_function(self.ping, 'ping')
        self.server.register_function(self.echo, 'echo')
        self.server.register_function(self.cleanup, 'cleanup')

        # Diagnostics
        self.server.register_function(self.get_status, 'get_status')
        self.server.register_function(self.get_config, 'get_config')
        self.server.register_function(self.check_imports, 'check_imports')
        self.server.register_function(self.test_embedding_retrieval, 'test_embedding_retrieval')
        self.server.register_function(self.get_processing_status, 'get_processing_status')
        self.server.register_function(self.print_processing_report, 'print_processing_report')

        # Sync
        self.server.register_function(self.sync_library, 'sync_library')
        self.server.register_function(self.bulk_process_snapshot, 'bulk_process_snapshot')
        
        # Query
        self.server.register_function(self.get_similar_nodes, 'get_similar_nodes')
        self.server.register_function(self.find_similar_nodes_friendly, 'find_similar_nodes_friendly')
        self.server.register_function(self.search_nodes_by_content, 'search_nodes_by_content')
        self.server.register_function(self.get_node_context_friendly, 'get_node_context_friendly')
        
        # Autotag
        self.server.register_function(self.extract_entities_for_tagging, 'extract_entities_for_tagging')
        self.server.register_function(self.force_extract_entities_for_tagging, 'force_extract_entities_for_tagging')
        self.server.register_function(self.batch_extract_entities_for_tagging, 'batch_extract_entities_for_tagging')
        self.server.register_function(self.rag_extract_tag_patterns, 'rag_extract_tag_patterns')
        self.server.register_function(self.get_tag_similarity_suggestions, 'get_tag_similarity_suggestions')
        
        # Tag Governance
        self.server.register_function(self.sync_tag_event, 'sync_tag_event')
        self.server.register_function(self.update_tag_status, 'update_tag_status')
        self.server.register_function(self.update_tag_rules, 'update_tag_rules')
        self.server.register_function(self.update_relation_type, 'update_relation_type')
        self.server.register_function(self.update_relation_rules, 'update_relation_rules')
        
        # Batch Tag Processing
        self.server.register_function(self.batch_generate_tags, 'batch_generate_tags')
        self.server.register_function(self.generate_single_node_tags, 'generate_single_node_tags')
        
        # Proactive Features
        self.server.register_function(self.proactive_get_resonance, 'proactive_get_resonance')
        
        logger.info("All EPC methods registered successfully.")

    def _init_emacs_client(self, emacs_server_port):
        if self.emacs_client is None:
            try:
                self.emacs_client = EPCClient(("127.0.0.1", emacs_server_port), log_traceback=True)
            except Exception as e:
                logger.error(f"Error initializing EPC client: {e}\n{traceback.format_exc()}")

    def _close_emacs_client(self):
        if self.emacs_client is not None:
            try:
                self.emacs_client.close()
                self.emacs_client = None
            except Exception as e:
                logger.error(f"Error closing EPC client: {e}\n{traceback.format_exc()}")

    def _eval_in_emacs(self, method_name_str, *args):
        if self.emacs_client is None:
            logger.error("EPC client to Emacs is not initialized. Cannot eval in Emacs.")
            return None
        try:
            method_symbol = sexpdata.Symbol(method_name_str)
            processed_args = [sexpdata.Quoted(arg) if isinstance(arg, str) else arg for arg in args]
            s_expression_parts = [method_symbol] + processed_args
            sexp_to_eval = sexpdata.dumps(s_expression_parts)
            self.emacs_client.call("eval-in-emacs", [sexp_to_eval])
        except Exception as e:
            logger.error(f"Error evaluating in Emacs: {e}\n{traceback.format_exc()}")
    
    def _parse_elisp_to_dict(self, elisp_data: Any) -> Any:
        """
        Recursively converts complex Elisp-style data structures from EPC into
        standard Python dictionaries and lists. It specifically handles:
        1.  The special EPC serialization for a LIST of hash-tables:
            `[Symbol('#s'), <hash_list1>, Symbol('#s'), <hash_list2>, ...]`
        2.  s-expressions for single hash-tables: `{'#s': [Symbol('hash-table'), ...]}`
        3.  Standard alists and plists.
        """
        # Case 1: A list, which could be various Elisp structures
        if isinstance(elisp_data, list):
            # Subcase 1.1: EPC serialization of a list of hash-tables
            if len(elisp_data) >= 2 and elisp_data[0] == sexpdata.Symbol('#s'):
                logger.debug("Parsing EPC-serialized list of hash-tables.")
                nodes = []
                it = iter(elisp_data)
                try:
                    while True:
                        s_symbol = next(it)
                        if s_symbol != sexpdata.Symbol('#s'):
                            logger.warning(f"Expected '#s' symbol, but got {s_symbol}. List may be malformed.")
                            break
                        
                        hash_content_list = next(it)
                        reconstructed_hash_dict = {'#s': hash_content_list}
                        nodes.append(self._parse_elisp_to_dict(reconstructed_hash_dict))
                except StopIteration:
                    pass # Consumed all items
                return nodes

            # Subcase 1.2: Standard alist (dotted or simple)
            is_alist_dotted = all(isinstance(i, list) and len(i) == 3 and isinstance(i[1], sexpdata.Symbol) and i[1].value() == '.' for i in elisp_data)
            if is_alist_dotted:
                logger.debug("Parsing Elisp alist (dotted).")
                return {item[0]: self._parse_elisp_to_dict(item[2]) for item in elisp_data}
            
            is_alist_simple = all(isinstance(i, list) and len(i) == 2 for i in elisp_data)
            if is_alist_simple:
                logger.debug("Parsing Elisp alist (simple).")
                return {item[0]: self._parse_elisp_to_dict(item[1]) for item in elisp_data}
            
            # Subcase 1.3: Fallback for a regular list of other items
            return [self._parse_elisp_to_dict(item) for item in elisp_data]

        # Case 2: s-expression for a SINGLE Elisp hash-table
        if isinstance(elisp_data, dict) and '#s' in elisp_data:
            s_exp = elisp_data['#s']
            if isinstance(s_exp, list) and s_exp and s_exp[0] == sexpdata.Symbol('hash-table'):
                logger.debug("Parsing single Elisp hash-table s-expression.")
                res_dct = {}
                try:
                    data_symbol = sexpdata.Symbol('data')
                    data_index = s_exp.index(data_symbol)
                    kv_list = s_exp[data_index + 1]
                    
                    it = iter(kv_list)
                    while True:
                        try:
                            key = next(it)
                            value = next(it)
                            key_str = key.value() if isinstance(key, sexpdata.Symbol) else str(key)
                            res_dct[key_str] = self._parse_elisp_to_dict(value)
                        except StopIteration:
                            break
                    return res_dct
                except (ValueError, IndexError, StopIteration, TypeError) as e:
                    logger.error(f"Failed to parse hash-table s-expression content: {e}", exc_info=True)
                    return {}
            return elisp_data

        # Case 3: A standard Python dictionary
        if isinstance(elisp_data, dict):
            return {self._parse_elisp_to_dict(k): self._parse_elisp_to_dict(v) for k, v in elisp_data.items()}

        # Case 4: A primitive value or a Symbol
        if isinstance(elisp_data, sexpdata.Symbol):
            return elisp_data.value()

        return elisp_data

    def echo(self, message):
        logger.info(f"Received message from Emacs to echo: {message}")
        return message

    def ping(self):
        return "pong"

    def cleanup(self):
        logger.info("Closing EPC server and client connection.")
        self._close_emacs_client()
        if self.server:
            self.server.shutdown()

    @inject
    def sync_library(self, db_file: str, db_snapshot_json_str: str, handler: SyncHandler = Provide[AppContainer.sync_handler]) -> Dict[str, Any]:
        return handler.sync_library_from_snapshot_json(db_file, db_snapshot_json_str)

    @inject
    def get_similar_nodes(self, query_input: str, top_k: int = 10, handler: QueryHandler = Provide[AppContainer.query_handler]):
        return handler.get_similar_nodes(query_input, top_k)

    @inject
    def find_similar_nodes_friendly(self, query_input: str, top_k: int = 10, handler: QueryHandler = Provide[AppContainer.query_handler]):
        return handler.find_similar_nodes_friendly(query_input, top_k)

    @inject
    def search_nodes_by_content(self, search_query: str, top_k: int = 10, fuzzy_match: bool = True, handler: QueryHandler = Provide[AppContainer.query_handler]):
        return handler.search_nodes_by_content(search_query, top_k, fuzzy_match)

    @inject
    def get_node_context_friendly(self, node_identifier: str, handler: QueryHandler = Provide[AppContainer.query_handler]):
        return handler.get_node_context_friendly(node_identifier)

    @inject
    def get_status(self, handler: DiagnosticsHandler = Provide[AppContainer.diagnostics_handler]):
        return handler.get_status()

    @inject
    def get_config(self, handler: DiagnosticsHandler = Provide[AppContainer.diagnostics_handler]):
        return handler.get_config()

    @inject
    def check_imports(self, handler: DiagnosticsHandler = Provide[AppContainer.diagnostics_handler]):
        return handler.check_imports()
        
    def _run_async(self, coro):
        """Helper to run a coroutine from a synchronous context."""
        try:
            loop = asyncio.get_running_loop()
        except RuntimeError:  # No running loop in this thread
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
        
        return loop.run_until_complete(coro)

    @inject
    def proactive_get_resonance(self, payload: Dict[str, Any], handler: ResonanceHandler = Provide[AppContainer.resonance_handler]) -> Dict[str, Any]:
        logger.debug(f"proactive_get_resonance received payload for node: {payload.get('node_id')}")
        
        # This handler method is async
        coro = handler.get_resonance(payload)
        return self._run_async(coro)

    @inject
    def test_embedding_retrieval(self, text: str, handler: DiagnosticsHandler = Provide[AppContainer.diagnostics_handler]):
        return handler.test_embedding_retrieval(text)

    @inject
    def bulk_process_snapshot(self, *args, handler: SyncHandler = Provide[AppContainer.sync_handler]) -> Dict[str, Any]:
        """
        Processes a snapshot of nodes and links from Elisp.
        Uses *args to handle flexible payload structures from EPC.
        """
        logger.debug(f"bulk_process_snapshot received {len(args)} raw args.")

        # Defensive coding: The entire payload is expected from Elisp.
        # EPC seems to be unpacking our list, so we treat all args as part of the payload.
        # The contract from Elisp is (list HASH-TABLE), which becomes the single argument.
        if not args:
            logger.error("bulk_process_snapshot received no arguments.")
            return {"error": "No data received"}

        # The payload from Elisp is the first element of the args tuple.
        raw_payload = args[0]
        
        try:
            # The handler expects the raw payload as passed from Elisp.
            return handler.bulk_process_snapshot(raw_payload)
        except Exception as e:
            logger.error(f"Error in bulk_process_snapshot: {e}\\n{traceback.format_exc()}")
            return {"error": str(e), "traceback": traceback.format_exc()}

    @inject
    def get_processing_status(self, handler: DiagnosticsHandler = Provide[AppContainer.diagnostics_handler]) -> Dict[str, Any]:
        return handler.get_processing_status()

    @inject
    def print_processing_report(self, handler: DiagnosticsHandler = Provide[AppContainer.diagnostics_handler]) -> Dict[str, Any]:
        return handler.print_processing_report()

    @inject
    def extract_entities_for_tagging(self, payload_list: List[Dict[str, Any]], handler: AutotagHandler = Provide[AppContainer.autotag_handler]) -> Dict[str, Any]:
        # Pass the raw payload_list directly to AutotagHandler, which now handles Elisp alist format
        return handler.extract_entities_for_tagging(payload_list)

    @inject
    def force_extract_entities_for_tagging(self, payload_list: List[Dict[str, Any]], handler: AutotagHandler = Provide[AppContainer.autotag_handler]) -> Dict[str, Any]:
        # Pass the raw payload_list directly to AutotagHandler, which now handles Elisp alist format
        return handler.force_extract_entities_for_tagging(payload_list)

    @inject
    def batch_extract_entities_for_tagging(self, payload_list: List[Dict[str, Any]], handler: AutotagHandler = Provide[AppContainer.autotag_handler]) -> Dict[str, Any]:
        # Pass the raw payload_list directly to AutotagHandler, which now handles Elisp alist format
        return handler.batch_extract_entities_for_tagging(payload_list)

    @inject
    def rag_extract_tag_patterns(self, payload_list: List[Dict[str, Any]], handler: AutotagHandler = Provide[AppContainer.autotag_handler]) -> Dict[str, Any]:
        # Pass the raw payload_list directly to AutotagHandler, which now handles Elisp alist format
        return handler.rag_extract_tag_patterns(payload_list)

    @inject
    def get_tag_similarity_suggestions(self, payload_list: List[Dict[str, Any]], handler: AutotagHandler = Provide[AppContainer.autotag_handler]) -> Dict[str, Any]:
        # Pass the raw payload_list directly to AutotagHandler, which now handles Elisp alist format
        return handler.get_tag_similarity_suggestions(payload_list)

    @inject
    def sync_tag_event(self, event_data: Dict[str, Any], handler: SyncHandler = Provide[AppContainer.sync_handler]) -> Dict[str, Any]:
        """
        Handles various tag-related events from Elisp.
        'event_type': 'add', 'remove', 'rename', 'merge'
        'data': { ... event specific data ... }
        """
        event_type = event_data.get('event_type')
        data = event_data.get('data')
        
        # Here you can call different methods on SyncHandler based on event_type
        # e.g., handler.add_tag(data), handler.remove_tag(data), etc.
        logger.info(f"Received tag event: {event_type} with data: {data}")
        return {"status": "received", "event": event_type}

    @inject
    def update_tag_status(self, tag_data: Dict[str, Any], handler: SyncHandler = Provide[AppContainer.sync_handler]) -> Dict[str, Any]:
        """
        Updates the status of a tag (e.g., 'active', 'deprecated').
        'tag_name': name of the tag
        'status': new status
        """
        tag_name = tag_data.get('tag_name')
        status = tag_data.get('status')
        # Call handler.update_tag_status(tag_name, status)
        logger.info(f"Updating tag '{tag_name}' to status '{status}'")
        return {"status": "updated", "tag_name": tag_name, "new_status": status}

    @inject
    def update_tag_rules(self, tag_data: Dict[str, Any], handler: SyncHandler = Provide[AppContainer.sync_handler]) -> Dict[str, Any]:
        """
        Updates inference or application rules for a tag.
        'tag_name': name of the tag
        'rules': { ... new rules ... }
        """
        tag_name = tag_data.get('tag_name')
        rules = tag_data.get('rules')
        # Call handler.update_tag_rules(tag_name, rules)
        logger.info(f"Updating rules for tag '{tag_name}': {rules}")
        return {"status": "rules_updated", "tag_name": tag_name}

    @inject
    def update_relation_type(self, relation_data: Dict[str, Any], handler: SyncHandler = Provide[AppContainer.sync_handler]) -> Dict[str, Any]:
        """
        Updates a relationship type (e.g., rename, add properties).
        'type_name': name of the relation type
        'updates': { ... changes to apply ... }
        """
        type_name = relation_data.get('type_name')
        updates = relation_data.get('updates')
        # Call handler.update_relation_type(type_name, updates)
        logger.info(f"Updating relation type '{type_name}': {updates}")
        return {"status": "relation_type_updated", "type_name": type_name}

    @inject
    def update_relation_rules(self, relation_data: Dict[str, Any], handler: SyncHandler = Provide[AppContainer.sync_handler]) -> Dict[str, Any]:
        logger.debug(f"update_relation_rules received data for relation: {relation_data.get('type')}")
        return handler.update_relation_rules(relation_data)

    @inject
    def batch_generate_tags(self, *args, handler: AutotagHandler = Provide[AppContainer.autotag_handler]):
        """
        Processes a batch of items to generate tags by running the async handler.
        """
        logger.debug(f"Bridge batch_generate_tags received {len(args)} arguments")
        
        # 详细打印每个参数的信息
        for i, arg in enumerate(args):
            logger.debug(f"Argument {i}: type={type(arg)}, content={arg}")
            if hasattr(arg, 'value') and callable(arg.value):
                logger.debug(f"Argument {i} is Symbol with value: {arg.value()}")
        
        try:
            # According to unified data contract, pass the arguments as a single parameter
            # If multiple args are received, take the first one as the payload
            payload = args[0] if args else None
            coro = handler.batch_generate_tags(payload)
            return self._run_async(coro)
        except Exception as e:
            logger.error(f"Error running async batch_generate_tags: {e}", exc_info=True)
            return {"error": "async_execution_error", "message": str(e)}

    @inject
    def generate_single_node_tags(self, *args, handler: AutotagHandler = Provide[AppContainer.autotag_handler]) -> Dict[str, Any]:
        """
        Processes a single node to generate tags by running the async handler.
        """
        logger.debug(f"Bridge received generate_single_node_tags, forwarding to async handler.")
        try:
            # According to unified data contract, pass the arguments as a single parameter
            # If multiple args are received, take the first one as the payload
            payload = args[0] if args else None
            coro = handler.suggest_tags_for_single_node_elisp(payload)
            return self._run_async(coro)
        except Exception as e:
            logger.error(f"Error calling async generate_single_node_tags handler: {e}", exc_info=True)
            return {"status": "error", "message": str(e)}

def main():
    parser = argparse.ArgumentParser(description="Org SuperTag EPC Bridge Server (Python)")
    parser.add_argument("--emacs-epc-port", type=int, required=True, help="EPC port of the Emacs server")
    parser.add_argument("--data-directory", type=str, required=True, help="Data directory for org-supertag")
    parser.add_argument("--config-file", type=str, help="Path to the TOML configuration file")
    parser.add_argument("--debug", action="store_true", help="Enable debug mode")

    args = parser.parse_args()

    # --- Config Loading ---
    config = Config()
    if args.config_file:
        try:
            config = Config.from_toml(args.config_file)
            print(f"Configuration loaded from {args.config_file}", file=sys.stderr)
        except FileNotFoundError:
            print(f"Warning: Config file not found at {args.config_file}. Using default settings.", file=sys.stderr)
        except Exception as e:
            print(f"Error loading config file: {e}. Using default settings.", file=sys.stderr)

    # Override config with command line arguments
    if args.data_directory:
        config.data_directory = args.data_directory
    if args.debug:
        config.debug = args.debug
    
    # --- Logging Setup ---
    logger = setup_simtag_logging(config.data_directory)

    # --- Dependency Injection Container Setup ---
    try:
        container = AppContainer()
        container.config.from_dict(config.to_dict())
        container.wire(modules=[__name__])
        logger.info("Dependency injection container wired successfully.")
    except Exception as e:
        logger.error(f"Failed to wire dependency injection container: {e}\n{traceback.format_exc()}")
        sys.exit(1)

    # --- Start EPC Server ---
    try:
        bridge = SimTagBridge(container, args.emacs_epc_port)
        logger.info(f"SimTagBridge initialized. Python backend is ready.")
        
        # Keep the main thread alive for the server thread
        while True:
            # Use a mechanism that allows for graceful shutdown if needed
            # For now, just sleep.
            import time
            time.sleep(1)

    except KeyboardInterrupt:
        logger.info("Shutting down SimTagBridge due to KeyboardInterrupt.")
    except Exception as e:
        logger.error(f"An unexpected error occurred: {e}\n{traceback.format_exc()}")
    finally:
        if 'bridge' in locals() and bridge:
            bridge.cleanup()
        logger.info("SimTagBridge has been shut down.")

if __name__ == "__main__":
    # To enable profiled run:
    # Set environment variable: `export ENABLE_PROFILING=1`
    # Then run the script. The profile `simtag_bridge.prof` will be saved on exit.
    if os.environ.get("ENABLE_PROFILING"):
        import cProfile
        import atexit
        
        print("Profiling is enabled. Profile will be saved to 'simtag_bridge.prof'.", file=sys.stderr)
        
        profiler = cProfile.Profile()
        
        def save_profile():
            profiler.dump_stats("simtag_bridge.prof")
            print("Profiling data saved.", file=sys.stderr)

        atexit.register(save_profile)

        def profiled_run():
            try:
                profiler.enable()
                main()
            finally:
                profiler.disable()
        
        profiled_run()
    else:
        main() 
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

import sys, os
print("sys.path:", sys.path)
print("cwd:", os.getcwd())

import logging # Ensure logging is imported early for diagnostics
import time # Added for timestamping in new EPC methods
import asyncio
import json # Added for JSON processing
import sqlite3 # Added to catch sqlite3.OperationalError

from simtag.core.tagging import TaggingEngine # Added to run async EPC methods

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

from simtag.config import Config

# Removed: from simtag.core.storage import VectorStorage
# Removed: from simtag.core.tagging import TaggingEngine

# New imports for the Living Documentation System
from simtag.core.storage import VectorStorage # Keep VectorStorage
from simtag.services.llm_client import LLMClient
from simtag.core.memory_engine import MemoryEngine, MemoryItem, MemoryItemType, UserPreference, DialogueHistory, DialogueTurn, ContextSnapshot # Added specific MemoryItem types
from simtag.core.graph_store import OrgSupertagKnowledgeGraph, Entity as GraphEntity, Relation as GraphRelation # Added as per graph_store.py
from simtag.core.entity_extractor import OrgSupertagEntityExtractor, ExtractedEntity, ExtractedRelation # 使用统一的实体提取器
from simtag.core.rag_engine import OrgSupertagRAGEngine
import dataclasses # For converting dataclasses to dicts
from datetime import datetime, timezone # Added for UTC timestamps
import time

# New import for NER Service
from simtag.services.ner_service import NERService

# New import for Memory Synthesizer
from simtag.services.memory_synthesizer import MemorySynthesizer, CandidateMemory
# Removed: using unified entity extractor from core module
from simtag.services.content_processor import ContentProcessor, ProcessingMode, ProcessingConfig, ContentItem, ProcessingResult, NERResult
from simtag.services.embedding_service import EmbeddingResult
from simtag.core.entity_extractor import ExtractedEntity, ExtractedRelation

# Removed: from .python_bridge.utils import init_epc_client, eval_in_emacs, logger as bridge_logger, close_epc_client

# Configure package-level logger
logger = logging.getLogger("simtag_bridge")
# We can customize this logger further if needed, for now, it will inherit root logger settings

# --- Define types for EPC interface ---
from typing import Dict, List, Any, Optional, Tuple # Ensure Tuple is imported

# Placeholder for actual data structures, will be refined later
# Based on living-doc-features.org section 10.4.A
InputData = Dict[str, Any]
ResponseData = Optional[Dict[str, Any]]
QuestionData = Dict[str, Any]
AnswerData = Dict[str, Any]
ContextData = Dict[str, Any]
SuggestionData = List[Dict[str, Any]]
MemoryData = Dict[str, Any]
MemoryUpdateData = Dict[str, Any]

def setup_simtag_logging(data_dir):
    """Sets up logging for the SimTag bridge."""
    log_file = os.path.join(data_dir, 'simtag_bridge.log')
    
    # Configure the parent logger for the 'simtag' package
    # This will allow loggers like 'simtag.services.llm_client' or 'simtag.core.tagging'
    # to inherit this configuration.
    package_logger = logging.getLogger("simtag") # Get the parent logger for the package
    package_logger.setLevel(logging.INFO) # Ensure level is set on the parent
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    
    # Clear existing handlers from the package_logger to avoid duplication
    if package_logger.hasHandlers():
        package_logger.handlers.clear()

    # Stream handler for stderr
    stream_handler = logging.StreamHandler(sys.stderr)
    stream_handler.setFormatter(formatter)
    package_logger.addHandler(stream_handler)
    
    # File handler
    file_handler = logging.FileHandler(log_file, mode='a', encoding='utf-8')
    file_handler.setFormatter(formatter)
    package_logger.addHandler(file_handler)
    
    # Log the initialization using a child logger of "simtag", e.g., "simtag.bridge_setup"
    # Or use the main "simtag_bridge" logger if that's preferred for this specific message.
    # For consistency, let's use a specific logger for this setup message, or just use package_logger.
    setup_logger = logging.getLogger("simtag.bridge_setup") # Child logger for this message
    setup_logger.info(f"SimTag package logging initialized. Log file: {log_file}")
    
    return package_logger # Return the configured parent logger

from hashlib import md5

def compute_node_hash(node_id: str, text: str) -> str:
    """计算节点的稳定哈希值。
    
    Args:
        node_id: 节点ID
        text: 节点内容
        
    Returns:
        str: 稳定的哈希值
    """
    content = f"{node_id}:{text}"
    return f"md5_{md5(content.encode()).hexdigest()}"

def compute_content_hash(text: str) -> str:
    """计算内容的稳定哈希值。
    
    Args:
        text: 内容文本
        
    Returns:
        str: 稳定的哈希值
    """
    return f"md5_{md5(text.encode()).hexdigest()}"

class SimTagBridge:
    def __init__(self, emacs_epc_port, data_directory, server_host='127.0.0.1'):
        # Setup logging first, so all initialization steps are logged properly.
        # Note: setup_simtag_logging is called here, ensure it's idempotent or called only once.
        # If logger is global, this might reconfigure it. Consider passing data_directory to a global setup.
        # For now, assume this is the primary setup point for this specific logger.
        # setup_simtag_logging(data_directory) # Called in main, if bridge instance created there.
                                            # If bridge created elsewhere, ensure logging is set up.
                                            # For safety, ensure logger has at least a basic config if not yet set up.
        if not logging.getLogger("simtag").hasHandlers(): # Check the parent logger we now configure
            # This fallback might still be useful if main() somehow fails to call setup_simtag_logging
            logging.basicConfig(level=logging.INFO, stream=sys.stderr, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
            # Use the module-level logger for this warning
            logger.warning("SimTag parent logger ('simtag') was not pre-configured by setup_simtag_logging; basicConfig used as fallback.")


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
        
        # --- Enhanced Logging for LLMClient Initialization ---
        logger.info(f"SimTagBridge:llm_client_config type: {type(self.config.llm_client_config)}")
        logger.info(f"SimTagBridge:llm_client_config content: {self.config.llm_client_config}")
        logger.info("SimTagBridge: Attempting to initialize LLMClient...")
        # --- End Enhanced Logging ---
        
        # Initialize new services and engines FIRST if others depend on them
        self.llm_client = LLMClient(
            provider=self.config.llm_client_config.get('provider', 'ollama'),
            config=self.config.llm_client_config
        )
        logger.info("LLMClient initialized.")

        # Initialize core components that might use LLMClient
        self.storage = VectorStorage(self.config.vector_db_path)
        self.engine = TaggingEngine(self.config, self.storage, llm_client=self.llm_client) # Pass llm_client
        logger.info("Legacy TaggingEngine initialized for compatibility, now with LLMClient.")

        # Initialize other services and engines
        self.ner_service = NERService(llm_client=self.llm_client, config=self.config)
        logger.info("NERService initialized with config.")
        self.memory_engine = MemoryEngine(config=self.config, llm_client=self.llm_client)
        logger.info("MemoryEngine initialized.")

        # Initialize the MemorySynthesizer, it needs access to the memory_engine and llm_client
        self.memory_synthesizer = MemorySynthesizer(
            config=self.config,
            memory_engine=self.memory_engine,
            llm_client=self.llm_client
        )
        logger.info("MemorySynthesizer initialized.")

        self.knowledge_graph = OrgSupertagKnowledgeGraph(config=self.config) # Add storage_backend if configurable
        logger.info("OrgSupertagKnowledgeGraph initialized.")
        
        # Configure entity types for the extractor, potentially from config
        # Example: entity_types_config_key = 'entity_extractor_entity_types'
        # default_entity_types = OrgSupertagEntityExtractor.DEFAULT_ENTITY_TYPES # Access default if needed
        # entity_types_for_extractor = getattr(self.config, entity_types_config_key, default_entity_types)
        # For now, using default or None which will make extractor use its own default
        # entity_types_for_extractor = getattr(self.config, 'entity_extractor_entity_types', None) # Old way

        # Initialize unified entity extractor with async callable
        async def llm_async_callable(prompt: str, model: Optional[str] = None) -> str:
            if model:
                return await self.llm_client.generate(prompt, model=model)
            else:
                return await self.llm_client.generate(prompt)
        
        self.entity_extractor = OrgSupertagEntityExtractor(
            llm_async_callable=llm_async_callable,
            entity_extractor_config=self.config.entity_extractor_config
        )
        logger.info("OrgSupertagEntityExtractor initialized.")

        # Initialize the new ContentProcessor that handles both embedding and NER
        processing_config = ProcessingConfig(
            mode=ProcessingMode.FULL_PROCESSING,
            embedding_enabled=True,
            ner_enabled=True,
            continue_on_embedding_failure=True,
            continue_on_ner_failure=True
        )
        self.content_processor = ContentProcessor(processing_config)
        logger.info("ContentProcessor initialized.")

        self.rag_engine = OrgSupertagRAGEngine(
            knowledge_graph=self.knowledge_graph,
            vector_storage=self.storage,
            llm_client=self.llm_client,
            config=self.config
        )
        logger.info("OrgSupertagRAGEngine initialized.")

        # Initialize UserInterfaceService for user-friendly interactions
        from .services.user_interface import UserInterfaceService
        self.user_interface = UserInterfaceService(storage=self.storage)
        logger.info("UserInterfaceService initialized.")

        # Deprecated: self.engine = TaggingEngine(self.config, self.storage)
        # logger.info("Old TaggingEngine initialized (to be phased out).")
            
        self.server = ThreadingEPCServer((server_host, 0), log_traceback=True)
        self.server.allow_reuse_address = True
        
        self._register_methods() # Call new registration method
        logger.info("Registered SimTagBridge instance methods with EPC server.")

        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.daemon = True
        self.server_thread.start()
        logger.info("SimTagBridge EPC server started in a new thread.")

        server_port = self.server.server_address[1]
        logger.info(f"SimTagBridge EPC server listening on port: {server_port}. Informing Emacs.")
        self._eval_in_emacs('org-supertag-bridge--handle-python-server-ready-signal', server_port)
        logger.info("Informed Emacs about the SimTagBridge EPC server port.")
        
        # Removed periodic sync initialization
        # self.last_sync_time = None
        # self.sync_interval = 900  # 15分钟
        # self._schedule_sync()
        # logger.info("Initialized sync scheduler.")

    def _register_methods(self):
        """Register all available EPC methods."""
        logger.info("Registering EPC methods...")
        
        # === Core Functionality ===
        self.server.register_function(self.echo, 'echo')
        self.server.register_function(self.ping, 'ping')
        self.server.register_function(self.initialize, 'initialize')
        
        # === Tag Operations (Now Deprecated or Handled by NER/RAG) ===
        # self.server.register_function(self.suggest_tags, 'suggest_tags')
        # self.server.register_function(self.suggest_tags_json, 'suggest_tags_json')
        # self.server.register_function(self.extract_entities, 'extract_entities')
        # self.server.register_function(self.find_similar, 'find_similar')
        # self.server.register_function(self.analyze_tag_relations, 'analyze_tag_relations')
        
        # === Database Operations ===
        self.server.register_function(self.sync_library, 'sync_library')
        self.server.register_function(self.get_similar_nodes, 'get_similar_nodes')
        # self.server.register_function(self.update_tag, 'update_tag')
        # self.server.register_function(self.remove_tag, 'remove_tag')
        
        # === System Status ===
        self.server.register_function(self.get_status, 'get_status')
        self.server.register_function(self.get_config, 'get_config')
        self.server.register_function(self.check_imports, 'check_imports')
        self.server.register_function(self.cleanup, 'cleanup')
        
        # === Living Documentation System ===
        self.server.register_function(self.analyze_input, 'analyze_input')
        self.server.register_function(self.ask_question, 'ask_question')
        self.server.register_function(self.get_suggestions, 'get_suggestions')
        self.server.register_function(self.set_dialogue_mode, 'set_dialogue_mode')
        self.server.register_function(self.get_memory_dashboard, 'get_memory_dashboard')
        self.server.register_function(self.update_memory_epc, 'update_memory_epc')
        
        # === Conceptual Resonance (New) ===
        self.server.register_function(self.analyze_conceptual_resonance, 'analyze_conceptual_resonance')
        
        # === Memory Synthesis (New) ===
        self.server.register_function(self.trigger_memory_synthesis_for_session, 'trigger_memory_synthesis_for_session')
        self.server.register_function(self.get_candidate_memories, 'get_candidate_memories')
        self.server.register_function(self.process_candidate_memory, 'process_candidate_memory')
        
        # === Knowledge Archaeology ===
        self.server.register_function(self.knowledge_archaeology_dig, 'knowledge_archaeology_dig')
        
        # === NER-based Features (New) ===
        self.server.register_function(self.get_ner_tag_suggestions, 'get_ner_tag_suggestions_for_note')
        self.server.register_function(self.discover_inferred_relationships, 'get_inferred_tag_relationships')
        
        # === Generic LLM Access (for behaviors) ===
        self.server.register_function(self.generate_text, 'generate_text')
        
        # === NEW Node Sync Method ===
        self.server.register_function(self.sync_node_from_elisp, 'sync_node_from_elisp')
        
        # === User-Friendly Interface Methods ===
        self.server.register_function(self.find_similar_nodes_friendly, 'find_similar_nodes_friendly')
        self.server.register_function(self.search_nodes_by_content, 'search_nodes_by_content')
        self.server.register_function(self.get_node_context_friendly, 'get_node_context_friendly')
        
        # New MemoryEngine specific EPC methods
        self.server.register_function(self.epc_get_user_preference, 'epc_get_user_preference')
        self.server.register_function(self.epc_add_dialogue_turn, 'epc_add_dialogue_turn')
        self.server.register_function(self.epc_get_dialogue_history, 'epc_get_dialogue_history')
        self.server.register_function(self.epc_summarize_dialogue_history, 'epc_summarize_dialogue_history')
        
        # === Proactive Engine Method ===
        self.server.register_function(self.analyze_node_context, 'analyze_node_context')
        # === Bulk Sync Method ===
        self.server.register_function(self.bulk_process_snapshot, 'bulk_process_snapshot')
        
        # === Test Methods ===
        self.server.register_function(self.test_embedding_retrieval, 'test_embedding_retrieval')
        
        logger.info("All EPC methods registered successfully.")

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
            return None # Modified to return None

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

    def initialize(self, vector_file_path, db_file_path):
        logger.info(f"initialize called with vector_file: {vector_file_path}, db_file: {db_file_path}")
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

    def sync_library(self, db_file, db_snapshot_json_str):
        logger.info(f"sync_library called with db_file: {db_file}, receiving DB snapshot as JSON string.")
        
        num_tags = "N/A"
        num_nodes = "N/A"
        parsed_snapshot = None

        try:
            if not isinstance(db_snapshot_json_str, str):
                logger.error(f"DB snapshot is not a JSON string as expected (type: {type(db_snapshot_json_str)}). Aborting sync.")
                return {"status": "error", "message": "Invalid snapshot format (not a JSON string)"}

            logger.debug(f"Attempting to parse JSON snapshot string (length: {len(db_snapshot_json_str)} chars). Preview: {db_snapshot_json_str[:200]}...")
            parsed_snapshot = json.loads(db_snapshot_json_str) # Parse the JSON string
            logger.info("Successfully parsed DB snapshot JSON string.")

            if isinstance(parsed_snapshot, dict):
                num_tags = len(parsed_snapshot.get("tags", []))
                num_nodes = len(parsed_snapshot.get("nodes", []))
                logger.info(f"Parsed snapshot contains: tags={num_tags}, nodes={num_nodes}.")
            else:
                logger.error(f"Parsed snapshot is not a dictionary (type: {type(parsed_snapshot)}). Aborting sync.")
                return {"status": "error", "message": "Parsed snapshot is not a dictionary"}

            # Call the engine method with the parsed Python dictionary
            result = self.engine.sync_full_snapshot(parsed_snapshot) 
            
            return {"status": "success", "result": result}
        except json.JSONDecodeError as je:
            logger.error(f"JSON decoding failed for DB snapshot: {je}\nFull string was (approx first 500 chars): {db_snapshot_json_str[:500]}")
            return {"status": "error", "message": f"JSON decoding error: {je}"}
        except Exception as e:
            logger.error(f"Sync library with full snapshot failed: {e}\n{traceback.format_exc()}")
            # Include details about the snapshot if it was parsed, to help debug if error is in engine
            parsed_info = f"Parsed snapshot (tags: {num_tags}, nodes: {num_nodes})" if parsed_snapshot else "Snapshot not parsed."
            return {"status": "error", "message": f"General error during sync: {e}. {parsed_info}"}

    def get_similar_nodes(self, query_input: str, top_k: int = 10):
        """EPC method to find similar nodes.

        Args:
            query_input: A node_id (str) or a text string (str) to find similar nodes for.
            top_k: The number of similar nodes to return.

        Returns:
            A dictionary with status and results or error message.
        """
        logger.info(f"[SimTagBridge.get_similar_nodes] called for query_input: '{query_input[:100]}...', top_k: {top_k}")
        start_time = time.time()
        try:
            if not hasattr(self, 'engine') or self.engine is None:
                logger.error("[SimTagBridge.get_similar_nodes] TaggingEngine (self.engine) is not initialized.")
                return {"status": "error", "message": "TaggingEngine not initialized"}
            
            similar_nodes_result = self.engine.find_similar_nodes(query_input, top_k)
            
            # The result from find_similar_nodes is List[Tuple[str, float]] (node_id, distance)
            # This format is fine for EPC.
            logger.info(f"[SimTagBridge.get_similar_nodes] for '{query_input[:100]}...' completed in {time.time() - start_time:.4f} seconds. Found {len(similar_nodes_result)} nodes.")
            return {"status": "success", "result": similar_nodes_result}
        except Exception as e:
            logger.error(f"[SimTagBridge.get_similar_nodes] for '{query_input[:100]}...' failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    # === 用户友好的接口方法 ===
    
    def find_similar_nodes_friendly(self, query_input: str, top_k: int = 10):
        """
        用户友好的相似节点查找方法
        
        Args:
            query_input: 可以是节点UUID或者用户输入的文本内容
            top_k: 返回结果数量
            
        Returns:
            包含标题、内容片段等用户友好信息的结果
        """
        logger.info(f"[find_similar_nodes_friendly] 用户友好查询: '{query_input[:100]}...', top_k: {top_k}")
        start_time = time.time()
        
        try:
            if not hasattr(self, 'user_interface') or self.user_interface is None:
                logger.error("[find_similar_nodes_friendly] UserInterfaceService not initialized.")
                return {"status": "error", "message": "UserInterfaceService not initialized"}
            
            if not hasattr(self, 'engine') or self.engine is None:
                logger.error("[find_similar_nodes_friendly] TaggingEngine not initialized.")
                return {"status": "error", "message": "TaggingEngine not initialized"}
            
            # 首先尝试解析用户输入为UUID（如果需要）
            resolved_query = self.user_interface.resolve_user_input_to_uuid(query_input) or query_input
            
            # 使用现有的引擎查找相似节点（返回UUID格式）
            uuid_results = self.engine.find_similar_nodes(resolved_query, top_k)
            
            # 转换为用户友好格式
            friendly_results = self.user_interface.convert_similar_nodes_to_user_friendly(uuid_results)
            
            logger.info(f"[find_similar_nodes_friendly] 完成查询 '{query_input[:100]}...' 用时 {time.time() - start_time:.4f} 秒. 找到 {len(friendly_results)} 个节点.")
            
            return {
                "status": "success", 
                "result": friendly_results,
                "query_info": {
                    "original_query": query_input,
                    "resolved_query": resolved_query,
                    "total_results": len(friendly_results)
                }
            }
            
        except Exception as e:
            logger.error(f"[find_similar_nodes_friendly] 查询失败 '{query_input[:100]}...': {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}
    
    def search_nodes_by_content(self, search_query: str, top_k: int = 10, fuzzy_match: bool = True):
        """
        根据内容搜索节点（用户友好接口）
        
        Args:
            search_query: 搜索查询（标题片段、内容关键词等）
            top_k: 返回结果数量
            fuzzy_match: 是否启用模糊匹配
            
        Returns:
            用户友好的搜索结果
        """
        logger.info(f"[search_nodes_by_content] 内容搜索: '{search_query}', top_k: {top_k}, fuzzy: {fuzzy_match}")
        start_time = time.time()
        
        try:
            if not hasattr(self, 'user_interface') or self.user_interface is None:
                logger.error("[search_nodes_by_content] UserInterfaceService not initialized.")
                return {"status": "error", "message": "UserInterfaceService not initialized"}
            
            # 使用用户接口服务进行搜索
            search_results = self.user_interface.search_nodes_by_title_or_content(
                query=search_query,
                top_k=top_k,
                fuzzy_match=fuzzy_match
            )
            
            logger.info(f"[search_nodes_by_content] 搜索完成 '{search_query}' 用时 {time.time() - start_time:.4f} 秒. 找到 {len(search_results)} 个结果.")
            
            return {
                "status": "success",
                "result": search_results,
                "search_info": {
                    "query": search_query,
                    "total_results": len(search_results),
                    "fuzzy_match_enabled": fuzzy_match
                }
            }
            
        except Exception as e:
            logger.error(f"[search_nodes_by_content] 搜索失败 '{search_query}': {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}
    
    def get_node_context_friendly(self, node_identifier: str):
        """
        获取节点的完整上下文信息（用户友好格式）
        
        Args:
            node_identifier: 节点标识符（可以是UUID、标题片段等）
            
        Returns:
            包含节点详细信息和相关上下文的用户友好格式
        """
        logger.info(f"[get_node_context_friendly] 获取节点上下文: '{node_identifier}'")
        start_time = time.time()
        
        try:
            if not hasattr(self, 'user_interface') or self.user_interface is None:
                logger.error("[get_node_context_friendly] UserInterfaceService not initialized.")
                return {"status": "error", "message": "UserInterfaceService not initialized"}
            
            # 解析用户输入为UUID
            node_uuid = self.user_interface.resolve_user_input_to_uuid(node_identifier)
            if not node_uuid:
                return {"status": "error", "message": f"无法解析节点标识符: {node_identifier}"}
            
            # 获取节点上下文
            context_info = self.user_interface.get_node_context_by_uuid(node_uuid)
            if not context_info:
                return {"status": "error", "message": f"未找到节点: {node_identifier}"}
            
            logger.info(f"[get_node_context_friendly] 获取上下文完成 '{node_identifier}' 用时 {time.time() - start_time:.4f} 秒.")
            
            return {
                "status": "success",
                "result": context_info,
                "node_info": {
                    "identifier": node_identifier,
                    "resolved_uuid": node_uuid
                }
            }
            
        except Exception as e:
            logger.error(f"[get_node_context_friendly] 获取上下文失败 '{node_identifier}': {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def _format_timestamp_for_db(self, elisp_time_val):
        """Helper to convert a potential Elisp timestamp to an ISO string or None."""
        if isinstance(elisp_time_val, str):
            # Assume it's already a valid ISO string if it's a string
            # TODO: Add validation if strict format is needed
            return elisp_time_val
        if isinstance(elisp_time_val, (list, tuple)) and len(elisp_time_val) == 4:
            # Tentative: Elisp time (high low micro pico) from current-time
            # This requires a more complex conversion to a standard datetime object first,
            # then formatting. For now, this is a placeholder for robust conversion.
            # Python's datetime uses POSIX timestamp (seconds since epoch).
            # Emacs time: (HIGHER LOWER MICRO PICO)
            # HIGHER is (Lisp_Object_Header / 2^16). LOWER is (Lisp_Object_Header % 2^16).
            # This is complex. A simpler approach might be to ensure Elisp sends ISO strings.
            try:
                # This is a very rough approximation and likely incorrect for Emacs internal time.
                # Emacs Lisp: (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)) gives ISO string.
                # If Elisp sends this tuple, it needs proper decoding.
                # For now, if it's a list/tuple, log a warning and return None.
                logger.warning(f"Received Elisp time tuple {elisp_time_val}, needs proper conversion. Storing as NULL.")
                # A more robust solution is for Elisp to format time to string before sending.
                # Example conversion if it were a datetime.datetime object (which it isn't here):
                # from datetime import datetime
                # if isinstance(elisp_time_val, datetime):
                #    return elisp_time_val.isoformat()
                return None # Or some default string like '1970-01-01T00:00:00Z'
            except Exception as te:
                logger.error(f"Error converting timestamp {elisp_time_val}: {te}")
                return None
        if isinstance(elisp_time_val, (int, float)):
             # If it's a simple number, assume it could be a Unix timestamp (seconds since epoch)
            try:
                from datetime import datetime, timezone
                return datetime.fromtimestamp(elisp_time_val, tz=timezone.utc).isoformat()
            except Exception as te:
                logger.error(f"Error converting numeric timestamp {elisp_time_val}: {te}")
                return None
        return None # Default to None if not recognizable

    def get_status(self):
        logger.info("get_status called")
        try:
            # Check LLM client availability instead of engine.ollama
            llm_available = False
            llm_model = "unknown"
            try:
                if self.llm_client:
                    # Try to get LLM status - this might need adjustment based on LLMClient implementation
                    llm_available = True
                    llm_model = getattr(self.config, 'llm_client_config', {}).get('default_model', 'unknown')
            except Exception as e:
                logger.warning(f"Could not check LLM status: {e}")

            # Check storage stats
            storage_stats = {}
            try:
                if self.storage:
                    storage_stats = self.storage.get_stats()
            except Exception as e:
                logger.warning(f"Could not get storage stats: {e}")

            status = {
                "llm_available": llm_available,
                "llm_model": llm_model,
                "storage_ready": self.storage is not None, 
                "storage_stats": storage_stats,
                "memory_engine_ready": self.memory_engine is not None,
                "knowledge_graph_ready": self.knowledge_graph is not None,
                "entity_extractor_ready": self.entity_extractor is not None,
                "rag_engine_ready": self.rag_engine is not None,
                "server_running": True, 
                "emacs_client_connected": self.emacs_client is not None,
                "config": {
                    "vector_db_path": self.config.vector_db_path,
                    "data_directory": self.data_dir,
                    "llm_model": llm_model
                }
            }
            return {"status": "success", "result": status}
        except Exception as e:
            logger.error(f"Get status failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def get_config(self):
        """获取配置信息"""
        # This should now return more comprehensive config
        # For now, it can return what's available in self.config
        logger.info("EPC Call: get_config")
        return {
            "status": "success", 
            "config": self.config.to_dict() # Assuming Config has a to_dict method
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
        # Removed sync_timer cancellation
        # if hasattr(self, 'sync_timer') and self.sync_timer:
        #     logger.info("Cancelling sync timer...")
        #     self.sync_timer.cancel()
        self._close_emacs_client()
        if self.server:
            logger.info("Shutting down SimTagBridge EPC server.")
            self.server.shutdown() # Gracefully shutdown the server
            self.server.server_close() # Close the server socket
            logger.info("SimTagBridge EPC server shut down.")
        if self.server_thread and self.server_thread.is_alive():
            self.server_thread.join(timeout=5) # Wait for thread to finish
            if self.server_thread.is_alive():
                logger.warning("SimTagBridge EPC server thread did not terminate cleanly.")
        logger.info("SimTagBridge cleanup complete.")

    # Removed _schedule_sync method entirely
    # def _schedule_sync(self):
    #     ... (entire method removed) ...

    # --- New EPCInterface Method Stubs ---
    # analyze_input, ask_question, etc. are now implemented with async logic
    async def _async_analyze_input(self, input_data: InputData) -> ResponseData:
        """
        Asynchronous core logic for analyzing input.
        """
        try:
            logger.info(f"_async_analyze_input called with data keys: {list(input_data.keys())}")
            text = input_data.get('text')
            node_id = input_data.get('node_id')
            org_tags = input_data.get('tags') # Optional list of strings

            if not text or not node_id:
                logger.warning("analyze_input: 'text' or 'node_id' missing from input_data.")
                return {'status': 'error', 'message': "'text' and 'node_id' are required."}

            # 1. Extract entities and relations
            extracted_entities, extracted_relations = await self.entity_extractor.extract_from_org_node(
                node_content=text, 
                node_id=node_id, 
                org_tags=org_tags
            )
            logger.info(f"Extracted {len(extracted_entities)} entities and {len(extracted_relations)} relations from node {node_id}.")

            # 2. Convert to format expected by KnowledgeGraph and upsert
            #    ExtractedEntity/Relation are dataclasses, KG expects List[Dict]
            if extracted_entities:
                entities_for_kg = [dataclasses.asdict(e) for e in extracted_entities]
                 # Ensure source_node_id is correctly passed if KG expects it at top level not in attributes
                for entity_dict, orig_entity in zip(entities_for_kg, extracted_entities):
                    entity_dict['source_nodes'] = {orig_entity.source_node_id} if orig_entity.source_node_id else set()
                    # KG might expect 'name', 'type', 'description', 'id' directly
                await self.knowledge_graph.upsert_entities(entities_for_kg)
            
            if extracted_relations:
                relations_for_kg = [dataclasses.asdict(r) for r in extracted_relations]
                # Ensure source_node_id is correctly passed for relations too
                for rel_dict, orig_rel in zip(relations_for_kg, extracted_relations):
                    rel_dict['source_nodes'] = {orig_rel.source_node_id} if orig_rel.source_node_id else set()
                await self.knowledge_graph.upsert_relations(relations_for_kg)
            
            logger.info(f"Upserted entities and relations for node {node_id} into Knowledge Graph.")

            # 3. Record analysis in memory (optional, can be detailed)
            # For now, let's just log a system state or a simple memory item
            analysis_summary = {
                'node_id': node_id,
                'num_entities_extracted': len(extracted_entities),
                'num_relations_extracted': len(extracted_relations),
                'tags_provided': org_tags
            }
            mem_item = MemoryItem(
                id=f"analysis_{node_id}_{int(time.time())}", 
                type=MemoryItemType.SYSTEM_STATE, # Or a more specific type like "NODE_ANALYSIS"
                content=analysis_summary,
                metadata={'source': 'analyze_input'}
            )
            await self.memory_engine.add_memory_item(mem_item)

            return {
                'status': 'success', 
                'message': f"Analyzed node {node_id}.",
                'num_entities': len(extracted_entities), 
                'num_relations': len(extracted_relations)
            }

        except Exception as e:
            logger.error(f"Error in _async_analyze_input: {e}\n{traceback.format_exc()}")
            return {'status': 'error', 'message': str(e)}

    def analyze_input(self, input_data: InputData) -> ResponseData:
        """
        EPC method: Analyzes input text (e.g., from an org-node), extracts entities and relations,
        updates the knowledge graph, and stores the analysis in memory.
        """
        return asyncio.run(self._async_analyze_input(input_data))

    async def _async_ask_question(self, question_data: QuestionData) -> AnswerData:
        """
        Asynchronous core logic for asking a question, now with session-aware conversational modes.
        """
        try:
            query_text = question_data.get('query_text')
            session_id = question_data.get('session_id', f"session_{int(time.time())}")
            logger.info(f"_async_ask_question called for session {session_id} with query: '{query_text[:50]}...'")

            if not query_text:
                logger.warning("ask_question: 'query_text' missing.")
                return {'answer': "Error: 'query_text' is required.", 'session_id': session_id, 'status': 'error', 'metadata': {}}

            # 1. Retrieve dialogue mode for the current session from MemoryEngine
            dialogue_mode = await self.memory_engine.get_user_preference(key=f"session_mode_{session_id}", default='normal')
            logger.info(f"Using dialogue mode '{dialogue_mode}' for session {session_id}")

            # 2. Get query embedding using ContentProcessor
            # 创建临时配置用于仅嵌入模式
            embedding_config = ProcessingConfig(mode=ProcessingMode.EMBEDDING_ONLY)
            embedding_processor = ContentProcessor(embedding_config)
            
            content_item = ContentItem(
                id=f"query_{session_id}_{int(time.time())}",
                text=query_text
            )
            processing_result = await embedding_processor.process_single(content_item)
            
            if (processing_result.success and 
                processing_result.embedding_result and 
                processing_result.embedding_result.success):
                query_embedding = processing_result.embedding_result.embedding
            else:
                logger.error("Failed to get query embedding for ask_question using ContentProcessor.")
                return {'answer': "Error: Could not generate query embedding.", 'session_id': session_id, 'status': 'error', 'metadata': {}}

            # 3. Extract entities from the query to aid RAG
            query_extracted_entities, _ = await self.entity_extractor.extract_from_org_node(
                node_content=query_text, 
                node_id=f"query_epc_{session_id}_{int(time.time())}"
            )

            # 4. Determine RAG strategy and other parameters from config based on dialogue_mode
            active_mode_config = self.config.rag_mode_presets.get(dialogue_mode, self.config.rag_mode_presets.get('normal', {}))
            rag_strategy_name = active_mode_config.get('rag_retrieval_mode', getattr(self.config, 'rag_retrieval_mode', 'light'))
            rag_top_k = active_mode_config.get('rag_vector_results', getattr(self.config, 'rag_vector_results', 10))

            # 5. Retrieve RAG context documents
            logger.info(f"Using RAG strategy: '{rag_strategy_name}' for mode '{dialogue_mode}'")
            rag_context_docs = await self.rag_engine.retrieve_context(
                query_text=query_text,
                query_embedding=query_embedding,
                extracted_entities=query_extracted_entities,
                strategy=rag_strategy_name,
                top_k=rag_top_k,
            )

            # 6. Generate answer based on the dialogue mode
            prompt_template_override = active_mode_config.get('prompt_template')
            final_answer_text: str
            response_metadata = {'mode': dialogue_mode}

            if dialogue_mode == 'socratic':
                logger.info(f"Applying Socratic mode prompt for session {session_id}")
                # Use a specific prompt template that instructs the LLM to return a JSON object.
                socratic_prompt_template = prompt_template_override or """
You are a Socratic tutor. Your goal is to help the user think more deeply about their topic.
Your response MUST be a JSON object with two keys: "question" and "hint".
- The "question" should be an insightful, guiding question based on the user's query and the provided context.
- The "hint" should be a brief summary of the most relevant points from the context, to help the user answer your question. Do not invent new information.

User's Query: "{query}"

Context from user's notes:
---
{context}
---

Now, generate the JSON response.
"""
                socratic_response_str = await self.rag_engine.generate_response(
                    query_text=query_text, 
                    context_docs=rag_context_docs,
                    prompt_template=socratic_prompt_template
                )
                
                # Attempt to parse the structured JSON response from the LLM
                try:
                    socratic_data = json.loads(socratic_response_str)
                    final_answer_text = socratic_data.get("question", "I'm not sure what to ask next. Could you elaborate?")
                    response_metadata['hint'] = socratic_data.get("hint", "")
                    logger.debug(f"Socratic mode parsed successfully. Hint: {response_metadata['hint'][:100]}...")
                except (json.JSONDecodeError, TypeError):
                    logger.error(f"Socratic mode failed to generate valid JSON. LLM raw response: {socratic_response_str}")
                    final_answer_text = socratic_response_str # Fallback to showing raw response
                    response_metadata['hint'] = "Error: The AI response was not in the expected structured format."

            else: # For 'normal' mode or any other mode without special handling
                final_answer_text = await self.rag_engine.generate_response(
                    query_text=query_text, 
                    context_docs=rag_context_docs,
                    prompt_template=prompt_template_override
                )

            # 7. Record interaction in dialogue history
            await self.memory_engine.add_dialogue_turn(session_id, "user", query_text, metadata={'mode': dialogue_mode})
            # Add the hint to the AI's turn metadata for a complete record
            ai_turn_metadata = {'mode': dialogue_mode, 'rag_docs_count': len(rag_context_docs)}
            if 'hint' in response_metadata and response_metadata['hint']:
                ai_turn_metadata['hint'] = response_metadata['hint']
            await self.memory_engine.add_dialogue_turn(session_id, "ai", final_answer_text, metadata=ai_turn_metadata)
            
            # 8. Record context snapshot for diagnostics and future learning
            snapshot_content = {
                'query': query_text,
                'dialogue_mode': dialogue_mode,
                'rag_strategy': rag_strategy_name,
                'retrieved_docs_count': len(rag_context_docs),
                'retrieved_docs_preview': [dataclasses.asdict(doc) if not isinstance(doc, dict) else doc for doc in rag_context_docs[:3]],
            }
            await self.memory_engine.record_context_snapshot(
                triggering_query=query_text,
                context_content=snapshot_content,
                llm_response=final_answer_text
            )

            return {'answer': final_answer_text, 'session_id': session_id, 'status': 'success', 'metadata': response_metadata}

        except Exception as e:
            logger.error(f"Error in _async_ask_question: {e}\n{traceback.format_exc()}")
            return {'answer': f"An error occurred: {str(e)}", 'session_id': question_data.get('session_id', 'error_session'), 'status': 'error', 'metadata': {}}

    def ask_question(self, question_data: QuestionData) -> AnswerData:
        """
        EPC method: Handles a question from the user, retrieves context using RAG,
        generates an answer using LLM, and updates dialogue memory.
        This implementation is now session-aware and supports different conversational modes.
        """
        return asyncio.run(self._async_ask_question(question_data))

    async def _async_get_suggestions(self, context_data: ContextData) -> SuggestionData:
        """
        Asynchronous core logic for getting suggestions.
        """
        try:
            # Hotfix for Elisp sending data as a single-element list
            if isinstance(context_data, list) and len(context_data) == 1 and isinstance(context_data[0], dict):
                logger.warning(f"Received context_data as a single-element list. Unpacking it. This indicates a client-side packaging issue that should be fixed in Elisp.")
                context_data = context_data[0]

            logger.info(f"_async_get_suggestions called with context keys: {list(context_data.keys())}")
            node_content = context_data.get('current_node_content')
            node_id = context_data.get('current_node_id')
            # dialogue_mode = context_data.get('dialogue_mode', 'normal') # Mode might influence suggestions

            suggestions: SuggestionData = []

            if not node_content or not node_id:
                logger.info("No node_content or node_id for suggestions, returning empty list.")
                return []

            # 1. Extract entities and get embedding for the current node content using ContentProcessor
            # 创建临时配置用于仅嵌入模式
            embedding_config = ProcessingConfig(mode=ProcessingMode.EMBEDDING_ONLY)
            embedding_processor = ContentProcessor(embedding_config)
            
            content_item = ContentItem(
                id=node_id,
                text=node_content
            )
            processing_result = await embedding_processor.process_single(content_item)
            
            if (processing_result.success and 
                processing_result.embedding_result and 
                processing_result.embedding_result.success):
                content_embedding = processing_result.embedding_result.embedding
            else:
                logger.warning(f"Could not get embedding for node_content of {node_id} using ContentProcessor.")
                return []
            
            extracted_node_entities, _ = await self.entity_extractor.extract_from_org_node(
                node_content=node_content, node_id=node_id
            )

            # 2. Use RAG engine to find related items (e.g., using 'light' or a custom 'exploratory' strategy)
            # For suggestions, the "query" is implicitly the current context.
            # We can use the full node_content as query_text or prominent entities.
            # Let's use the node_content as the query for broader similarity.
            
            # Suggestion-specific RAG config (could be from main config or hardcoded)
            suggestion_rag_strategy = getattr(self.config, 'suggestion_rag_strategy', 'light') 
            suggestion_top_k = getattr(self.config, 'suggestion_top_k', 5)

            related_docs = await self.rag_engine.retrieve_context(
                query_text=node_content[:1000], # Use a snippet of content as query
                query_embedding=content_embedding,
                extracted_entities=extracted_node_entities,
                strategy=suggestion_rag_strategy,
                top_k=suggestion_top_k 
            )

            # 3. Format retrieved items into SuggestionData
            for doc in related_docs:
                if doc.get('id') == node_id: continue # Don't suggest the node itself (if it's in vector DB)

                suggestions.append({
                    'type': doc.get('source', 'related_document'), # e.g., 'vector_search', 'graph_neighbor', 'reasoning_path'
                    'id': str(doc.get('id', '')),
                    'title': doc.get('text', '')[:80] + "..." if len(doc.get('text', '')) > 80 else doc.get('text', ''),
                    'snippet': doc.get('text', '')[:200] + "..." if len(doc.get('text', '')) > 200 else doc.get('text', ''),
                    'score': float(doc.get('score', 0.0)),
                    'metadata': {'entity_type': doc.get('entity_type')} if doc.get('entity_type') else {}
                })
            
            # Example: Suggest related concepts from the knowledge graph directly (not via RAG docs)
            # for entity in extracted_node_entities[:3]: # Limit to a few prominent entities
            #     neighbors = await self.knowledge_graph.get_neighbors(entity.id, k=1) # Direct 1-hop neighbors
            #     for neighbor_entity in neighbors:
            #         suggestions.append({
            #             'type': 'related_concept',
            #             'id': neighbor_entity.id,
            #             'title': neighbor_entity.name,
            #             'snippet': neighbor_entity.description or f"Concept of type {neighbor_entity.type}",
            #             'score': 0.75 # Assign a default score for direct graph links
            #         })
            
            # Sort suggestions by score (descending) if not already
            suggestions.sort(key=lambda s: s.get('score', 0.0), reverse=True)
            
            logger.info(f"Generated {len(suggestions)} suggestions for node {node_id}.")
            return suggestions[:suggestion_top_k] # Return top N suggestions

        except Exception as e:
            logger.error(f"Error in _async_get_suggestions: {e}\n{traceback.format_exc()}")
            return []

    def get_suggestions(self, context_data: ContextData) -> SuggestionData:
        """
        EPC method: Provides suggestions based on the current context (e.g., current org-node).
        Suggestions can be related documents, concepts, or actions.
        """
        return asyncio.run(self._async_get_suggestions(context_data))

    async def _async_set_dialogue_mode(self, session_id: str, mode_name: str, mode_config: Optional[Dict[str, Any]] = None) -> bool:
        """
        Asynchronous core logic for setting dialogue mode for a specific session.
        """
        try:
            logger.info(f"_async_set_dialogue_mode called for session '{session_id}' with mode: '{mode_name}'")
            if mode_config:
                # Store general config for a mode type, not session-specific
                await self.memory_engine.update_user_preference(
                    key=f"dialogue_mode_custom_config_{mode_name}", 
                    value=mode_config
                )
                logger.info(f"Stored/Updated custom config for dialogue mode type: {mode_name}")

            # Store the active mode for the specific session using a structured key
            session_mode_key = f"session_mode_{session_id}"
            await self.memory_engine.update_user_preference(key=session_mode_key, value=mode_name)
            
            logger.info(f"Active dialogue mode for session '{session_id}' set to: '{mode_name}'")
            return True
        except Exception as e:
            logger.error(f"Error in _async_set_dialogue_mode for session '{session_id}': {e}\n{traceback.format_exc()}")
            return False

    def set_dialogue_mode(self, session_id: str, mode_name: str, mode_config: Optional[Dict[str, Any]] = None) -> bool:
        """
        EPC method: Sets the active dialogue mode for a specific session.
        The `session_id` is crucial for distinguishing between concurrent dialogue windows.
        """
        return asyncio.run(self._async_set_dialogue_mode(session_id, mode_name, mode_config))

    async def _async_get_memory_dashboard(self) -> MemoryData:
        """
        Asynchronous core logic for getting the memory dashboard.
        """
        try:
            logger.info("_async_get_memory_dashboard called.")
            dashboard_data = await self.memory_engine.get_memory_dashboard_data()
            return dashboard_data
        except Exception as e:
            logger.error(f"Error in _async_get_memory_dashboard: {e}\n{traceback.format_exc()}")
            return {'status': 'error', 'message': str(e)} # Or a more structured error dict

    def get_memory_dashboard(self) -> MemoryData:
        """
        EPC method: Retrieves data for the memory management dashboard.
        """
        return asyncio.run(self._async_get_memory_dashboard())

    async def _async_update_memory_epc(self, update_data: MemoryUpdateData) -> bool: 
        """
        Asynchronous core logic for updating memory via EPC.
        """
        try:
            # Hotfix for Elisp sending data as a single-element list
            if isinstance(update_data, list) and len(update_data) == 1 and isinstance(update_data[0], dict):
                logger.warning(f"Received update_data as a single-element list. Unpacking it. This indicates a client-side packaging issue that should be fixed in Elisp.")
                update_data = update_data[0]

            logger.info(f"_async_update_memory_epc called with data: {update_data}")
            item_type_str = update_data.get('item_type')
            
            if not item_type_str:
                logger.warning("update_memory_epc: 'item_type' missing.")
                return False

            if item_type_str == MemoryItemType.USER_PREFERENCE.value:
                key = update_data.get('key')
                value = update_data.get('value')
                if key is not None: # Value can be None to unset/clear
                    await self.memory_engine.update_user_preference(key, value)
                    logger.info(f"Updated user preference via EPC: {key} = {value}")
                    return True
                else:
                    logger.warning("update_memory_epc: 'key' missing for user_preference.")
                    return False
            
            elif item_type_str == MemoryItemType.USER_FEEDBACK.value:
                feedback_content = update_data.get('content') # e.g., {'target_item_id': 'id', 'rating': 'good', 'comment': '...'}
                if feedback_content and isinstance(feedback_content, dict):
                    feedback_item = MemoryItem(
                        id=f"feedback_{int(time.time())}",
                        type=MemoryItemType.USER_FEEDBACK,
                        content=feedback_content,
                        metadata=update_data.get('metadata', {})
                    )
                    await self.memory_engine.add_memory_item(feedback_item)
                    logger.info(f"Added user feedback via EPC: {feedback_content}")
                    return True
                else:
                    logger.warning("update_memory_epc: 'content' (dict) missing or invalid for user_feedback.")
                    return False
            
            # Add other updatable memory types here (e.g., clearing specific dialogue histories)
            else:
                logger.warning(f"update_memory_epc: Unsupported item_type '{item_type_str}'.")
                return False

        except Exception as e:
            logger.error(f"Error in _async_update_memory_epc: {e}\n{traceback.format_exc()}")
            return False

    def update_memory_epc(self, update_data: MemoryUpdateData) -> bool: 
        """
        EPC method: Allows Emacs to update specific memory items, like user preferences or feedback.
        """
        return asyncio.run(self._async_update_memory_epc(update_data))

    # --- Memory Synthesis ---

    async def _async_trigger_memory_synthesis(self, session_id: str) -> Dict[str, Any]:
        """Triggers the memory synthesis process for a given session."""
        logger.info(f"EPC call received to trigger memory synthesis for session: {session_id}")
        try:
            candidates = await self.memory_synthesizer.synthesize_from_dialogue(session_id)
            msg = f"Memory synthesis triggered for session {session_id}. Found {len(candidates)} new candidates."
            logger.info(msg)
            # We can optionally return the new candidates immediately
            return {"status": "success", "message": msg, "new_candidate_count": len(candidates)}
        except Exception as e:
            logger.error(f"Error during memory synthesis trigger for session {session_id}: {e}", exc_info=True)
            return {"status": "error", "message": str(e)}

    def trigger_memory_synthesis_for_session(self, session_id: str) -> Dict[str, Any]:
        """EPC Method: Triggers memory synthesis for a session."""
        return asyncio.run(self._async_trigger_memory_synthesis(session_id))

    def get_candidate_memories(self) -> Dict[str, Any]:
        """Returns all pending candidate memories, formatted for EPC."""
        logger.info("EPC call received to get all pending candidate memories.")
        try:
            candidates = self.memory_synthesizer.get_pending_candidates()
            # Convert dataclasses to dicts for serialization
            serializable_candidates = [dataclasses.asdict(c) for c in candidates]
            # MemoryItem within proposed_item is also a dataclass, so we need to convert it too
            for cand_dict in serializable_candidates:
                if 'proposed_item' in cand_dict and isinstance(cand_dict['proposed_item'], MemoryItem):
                     cand_dict['proposed_item'] = dataclasses.asdict(cand_dict['proposed_item'])

            return {"status": "success", "result": serializable_candidates}
        except Exception as e:
            logger.error(f"Error getting candidate memories: {e}", exc_info=True)
            return {"status": "error", "message": str(e)}
            
    async def _async_process_candidate_memory(self, candidate_id: str, action: str) -> Dict[str, Any]:
        """Processes a user's decision on a candidate memory."""
        logger.info(f"EPC call received to process candidate {candidate_id} with action: {action}")
        candidate = self.memory_synthesizer.get_candidate_by_id(candidate_id)

        if not candidate:
            return {"status": "error", "message": f"Candidate with ID {candidate_id} not found."}

        try:
            if action == "accept":
                # Add the proposed item to the memory engine
                await self.memory_engine.add_memory_item(candidate.proposed_item)
                # Remove from candidates
                self.memory_synthesizer.discard_candidate(candidate_id)
                msg = f"Accepted and stored candidate {candidate_id} as a new memory."
                logger.info(msg)
                return {"status": "success", "message": msg}
            
            elif action == "reject":
                # Just remove from candidates
                self.memory_synthesizer.discard_candidate(candidate_id)
                msg = f"Rejected and discarded candidate {candidate_id}."
                logger.info(msg)
                return {"status": "success", "message": msg}
            
            else:
                return {"status": "error", "message": f"Invalid action '{action}'. Must be 'accept' or 'reject'."}
        except Exception as e:
            logger.error(f"Error processing candidate memory {candidate_id}: {e}", exc_info=True)
            return {"status": "error", "message": str(e)}

    def process_candidate_memory(self, candidate_id: str, action: str) -> Dict[str, Any]:
        """EPC Method: Processes user decision on a candidate memory."""
        return asyncio.run(self._async_process_candidate_memory(candidate_id, action))

    # --- Knowledge Archaeology ---
    async def _async_knowledge_archaeology_dig(self, query_text: str, top_k: int = 50) -> Dict[str, Any]:
        """
        Asynchronous core logic for the Knowledge Archaeology feature.
        Finds nodes related to a query and returns them sorted by date.
        """
        logger.info(f"Knowledge Archaeology dig started for query: '{query_text}'")
        try:
            # 1. Find similar nodes using vector search
            # We use the TaggingEngine's find_similar_nodes, which should handle embedding generation.
            similar_nodes_result = await asyncio.to_thread(
                self.engine.find_similar_nodes, query_text, top_k
            )
            
            if not similar_nodes_result:
                logger.info("No similar nodes found for the query.")
                return {"status": "success", "result": []}

            node_ids = [node_id for node_id, score in similar_nodes_result]
            logger.debug(f"Found {len(node_ids)} potentially relevant nodes.")

            # 2. Fetch metadata for these nodes from the storage layer
            # This is a synchronous call but should be very fast.
            node_details = self.storage.get_node_details_by_ids(node_ids)
            
            if not node_details:
                logger.warning("Found similar node IDs but could not fetch their details.")
                return {"status": "success", "result": []}

            # 3. Filter out nodes without a valid date and sort the rest
            valid_nodes = [n for n in node_details if n.get('document_date')]
            
            # Sort by document_date (assuming ISO format YYYY-MM-DD...)
            # The 'reverse=True' can be changed if older items should appear first.
            sorted_nodes = sorted(valid_nodes, key=lambda x: x['document_date'], reverse=True)
            
            logger.info(f"Returning {len(sorted_nodes)} nodes sorted by date for archaeology dig.")
            return {"status": "success", "result": sorted_nodes}

        except Exception as e:
            logger.error(f"Error during Knowledge Archaeology dig: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def knowledge_archaeology_dig(self, query_text: str, top_k: int = 50) -> Dict[str, Any]:
        """
        EPC method for the Knowledge Archaeology feature.
        """
        return asyncio.run(self._async_knowledge_archaeology_dig(query_text, top_k))

    # --- NER-based Features ---

    async def _async_get_ner_tag_suggestions(self, payload: Dict) -> Dict[str, Any]:
        """Asynchronous wrapper for NERService tag suggestion."""
        logger.info(f"Received request for NER tag suggestions. Payload keys: {payload.keys()}")
        try:
            # Hotfix for Elisp sending data as a single-element list
            if isinstance(payload, list) and len(payload) == 1 and isinstance(payload[0], dict):
                logger.warning(f"Received payload as a single-element list. Unpacking it. This indicates a client-side packaging issue that should be fixed in Elisp.")
                payload = payload[0]

            note_content = payload.get("note_content")
            existing_tags = payload.get("existing_tags")
            
            if not note_content:
                return {"status": "error", "message": "note_content is required."}

            suggestions = await self.ner_service.suggest_tags_for_note(note_content, existing_tags)
            
            return {"status": "success", "result": suggestions}
        except Exception as e:
            logger.error(f"Error during NER tag suggestion: {e}", exc_info=True)
            return {"status": "error", "message": str(e)}
        
    def get_ner_tag_suggestions(self, payload):
    # 兼容 Elisp 端传递 [dict] 或 dict
        if isinstance(payload, list):
            if len(payload) == 1 and isinstance(payload[0], dict):
                payload = payload[0]    
            else:
                return {"status": "error", "message": "Payload must be dict or [dict]."}
        if not isinstance(payload, dict):
            return {"status": "error", "message": "Payload must be a dict."}
        return asyncio.run(self._async_get_ner_tag_suggestions(payload))

    async def _async_discover_inferred_relationships(self, payload: Dict) -> Dict[str, Any]:
        """Asynchronous wrapper for NERService relationship discovery."""
        logger.info(f"Received request for inferred relationship discovery. Payload keys: {payload.keys()}")
        try:
            # Hotfix for Elisp sending data as a single-element list
            if isinstance(payload, list) and len(payload) == 1 and isinstance(payload[0], dict):
                logger.warning(f"Received payload as a single-element list. Unpacking it. This indicates a client-side packaging issue that should be fixed in Elisp.")
                payload = payload[0]

            note_content = payload.get("note_content")
            if not note_content:
                return {"status": "error", "message": "note_content is required."}
            
            relationships = await self.ner_service.discover_tag_relationships(note_content)
            
            return {"status": "success", "result": relationships}
        except Exception as e:
            logger.error(f"Error during inferred relationship discovery: {e}", exc_info=True)
            return {"status": "error", "message": str(e)}

    def discover_inferred_relationships(self, payload: Dict) -> Dict[str, Any]:
        """EPC method to discover inferred tag relationships from note content."""
        return asyncio.run(self._async_discover_inferred_relationships(payload))


    # --- Generic Text Generation ---
    async def _async_generate_text(self, prompt: str) -> Dict[str, Any]:
        """Asynchronous wrapper for generic LLM text generation."""
        logger.info(f"Received request for generic text generation. Prompt length: {len(prompt)}")
        try:
            if not prompt:
                return {"status": "error", "message": "Prompt cannot be empty."}
            
            response_text = await self.llm_client.generate(prompt)
            
            return {"status": "success", "result": response_text}
        except Exception as e:
            logger.error(f"Error during generic text generation: {e}", exc_info=True)
            return {"status": "error", "message": str(e)}

    def generate_text(self, prompt: str) -> Dict[str, Any]:
        """EPC method for generic text generation, called by behaviors."""
        return asyncio.run(self._async_generate_text(prompt))

    async def _async_sync_node_from_elisp(self, node_props: Dict[str, Any]) -> Dict[str, Any]:
        """
        Asynchronously processes a single node from Elisp, handling vectorization and storage.
        This is the new, preferred method for syncing individual nodes.
        """
        # Hotfix for Elisp sending data as a single-element list
        if isinstance(node_props, list) and len(node_props) == 1 and isinstance(node_props[0], dict):
            logger.warning(f"Received node_props as a single-element list. Unpacking it. This indicates a client-side packaging issue that should be fixed in Elisp.")
            node_props = node_props[0]

        node_id = node_props.get("id")
        if not node_id:
            logger.error("sync_node_from_elisp: Received node properties without an 'id'.")
            return {"status": "error", "message": "Node 'id' is required."}

        logger.info(f"sync_node_from_elisp: Processing node {node_id}.")

        try:
            # 1. Extract content for vectorization
            # The content is a combination of title and the main content body.
            title = node_props.get("title", "")
            content_body = node_props.get("content", "")
            text_to_embed = f"{title}\n\n{content_body}".strip()

            # 2. Generate embedding for the content using ContentProcessor
            embedding = []
            if text_to_embed:
                # 创建临时配置用于仅嵌入模式
                embedding_config = ProcessingConfig(mode=ProcessingMode.EMBEDDING_ONLY)
                embedding_processor = ContentProcessor(embedding_config)
                
                content_item = ContentItem(
                    id=node_id,
                    text=text_to_embed
                )
                processing_result = await embedding_processor.process_single(content_item)
                
                if (processing_result.success and 
                    processing_result.embedding_result and 
                    processing_result.embedding_result.success):
                    embedding = processing_result.embedding_result.embedding
                    logger.info(f"Generated embedding for node {node_id} using ContentProcessor")
                else:
                    logger.warning(f"Could not generate embedding for node {node_id} using ContentProcessor. It will be stored without a vector.")
            else:
                logger.info(f"Node {node_id} has no content to embed.")
            
            # 3. Prepare data for storage
            # The storage layer expects a list of dictionaries.
            node_data_for_storage = {
                "node_id": node_id,
                "vector": embedding,
                "content": text_to_embed, # Store the combined text used for embedding
                "title": title,
                "tags": node_props.get("tags", []),
                "file_path": node_props.get("file-path"), # Note the key transformation from elisp
                "modified_at": node_props.get("modified-at")
                # Add any other relevant props from Elisp here
            }
            
            # 4. Upsert the node vector and metadata into storage
            # The bulk_upsert_nodes method can handle a single node in a list.
            await asyncio.to_thread(self.storage.bulk_upsert_nodes, [node_data_for_storage])

            logger.info(f"Successfully synced and vectorized node {node_id}.")
            return {"status": "success", "node_id": node_id}

        except Exception as e:
            logger.error(f"Error syncing node {node_id}: {e}", exc_info=True)
            return {"status": "error", "message": str(e), "node_id": node_id}

    def sync_node_from_elisp(self, node_props: Dict[str, Any]) -> Dict[str, Any]:
        """
        Synchronous EPC wrapper for _async_sync_node_from_elisp.
        This method is designed to be called from Emacs Lisp to sync a single node.
        """
        return asyncio.run(self._async_sync_node_from_elisp(node_props))

    # --- New EPC Methods for MemoryEngine ---

    def epc_get_user_preference(self, key: str, default: Optional[Any] = None) -> Optional[Any]:
        """EPC wrapper for MemoryEngine.get_user_preference."""
        logger.debug(f"epc_get_user_preference called for key: {key}")
        try:
            return asyncio.run(self.memory_engine.get_user_preference(key, default))
        except Exception as e:
            logger.error(f"Error in epc_get_user_preference for key '{key}': {e}", exc_info=True)
            return default # Or raise, or return specific error structure

    def epc_add_dialogue_turn(self, session_id: str, speaker: str, text: str, metadata: Optional[Dict[str, Any]]=None) -> Dict[str, Any]:
        """EPC wrapper for MemoryEngine.add_dialogue_turn."""
        logger.debug(f"epc_add_dialogue_turn called for session: {session_id}, speaker: {speaker}")
        if metadata is None:
            metadata = {}
        try:
            dialogue_history_obj = asyncio.run(self.memory_engine.add_dialogue_turn(session_id, speaker, text, metadata))
            if dialogue_history_obj:
                return {
                    "status": "success",
                    "session_id": dialogue_history_obj.session_id,
                    "session_db_id": dialogue_history_obj.id, # The unique ID in dialogue_sessions table
                    "turn_count": len(dialogue_history_obj.turns),
                    "latest_timestamp": dialogue_history_obj.timestamp
                }
            else:
                return {"status": "error", "message": "Failed to add dialogue turn (no history object returned)"}
        except Exception as e:
            logger.error(f"Error in epc_add_dialogue_turn for session '{session_id}': {e}", exc_info=True)
            return {"status": "error", "message": str(e)}

    def epc_get_dialogue_history(self, session_id: str, max_turns: Optional[int] = None) -> Optional[Dict[str, Any]]:
        """EPC wrapper for MemoryEngine.get_dialogue_history. Returns serializable dict."""
        logger.debug(f"epc_get_dialogue_history called for session: {session_id}, max_turns: {max_turns}")
        try:
            history_obj = asyncio.run(self.memory_engine.get_dialogue_history(session_id, max_turns))
            if history_obj:
                # Convert DialogueHistory and DialogueTurn objects to dictionaries for EPC
                serializable_turns = []
                for turn in history_obj.turns:
                    serializable_turns.append({
                        "speaker": turn.speaker,
                        "text": turn.text,
                        "timestamp": turn.timestamp,
                        "metadata": turn.metadata
                    })
                return {
                    "id": history_obj.id,
                    "session_id": history_obj.session_id,
                    "turns": serializable_turns,
                    "summary": history_obj.summary,
                    "timestamp": history_obj.timestamp,
                    "metadata": history_obj.metadata
                }
            return None
        except Exception as e:
            logger.error(f"Error in epc_get_dialogue_history for session '{session_id}': {e}", exc_info=True)
            return None # Or specific error structure

    def epc_summarize_dialogue_history(self, dialogue_history_id: str) -> Dict[str, Any]:
        """EPC wrapper for MemoryEngine.summarize_dialogue_history."""
        logger.debug(f"epc_summarize_dialogue_history called for history_id: {dialogue_history_id}")
        try:
            success = asyncio.run(self.memory_engine.summarize_dialogue_history(dialogue_history_id))
            if success:
                return {"status": "success", "message": f"Summarization initiated/completed for {dialogue_history_id}"}
            else:
                return {"status": "failure", "message": f"Summarization failed or history not found for {dialogue_history_id}"}
        except Exception as e:
            logger.error(f"Error in epc_summarize_dialogue_history for ID '{dialogue_history_id}': {e}", exc_info=True)
            return {"status": "error", "message": str(e)}

    async def _async_analyze_conceptual_resonance(self, context_data: ContextData) -> Optional[Dict[str, Any]]:
        """
        Asynchronous core logic for analyzing conceptual resonance.
        It finds related notes and uses an LLM to generate an insightful connection.
        """
        # Hotfix for Elisp sending data as a single-element list
        if isinstance(context_data, list) and len(context_data) == 1 and isinstance(context_data[0], dict):
            logger.warning(f"Received context_data as a single-element list. Unpacking it. This indicates a client-side packaging issue that should be fixed in Elisp.")
            context_data = context_data[0]

        logger.info(f"Analyzing conceptual resonance for node_id: {context_data.get('current_node_id')}")
        
        # 1. Reuse the suggestion logic to find related documents.
        # We get more than needed and then filter to find the most interesting one.
        context_data_for_suggestions = context_data.copy()
        suggestions = await self._async_get_suggestions(context_data_for_suggestions)
        
        # We need at least one related note to find resonance.
        if not suggestions:
            logger.info("No relevant suggestions found, cannot analyze for resonance.")
            return None

        # 2. Select the most promising suggestion for resonance analysis.
        # For now, let's pick the top suggestion that is not from the same file.
        # A more advanced strategy could check for cross-domain tags.
        current_node_props = self.storage.get_node_details_by_ids([context_data.get('current_node_id')])[0]
        current_file = current_node_props.get('file_path')

        resonant_note_suggestion = None
        for suggestion in suggestions:
            try:
                # Fetch details of the suggested node to check its file path
                suggested_node_props_list = self.storage.get_node_details_by_ids([suggestion['id']])
                if suggested_node_props_list:
                    suggested_node_props = suggested_node_props_list[0]
                    if suggested_node_props.get('file_path') != current_file:
                        resonant_note_suggestion = suggestion
                        break # Found a suitable note
            except Exception as e:
                logger.warning(f"Could not fetch details for suggested node {suggestion['id']}: {e}")
                continue
        
        if not resonant_note_suggestion:
             logger.info("Found suggestions, but none were suitable for cross-note resonance analysis.")
             return None

        # 3. Prepare the context for the LLM to generate the insight.
        current_note_content = context_data.get('current_node_content', '')
        resonant_note_content = resonant_note_suggestion.get('snippet', '')
        
        prompt = f"""
You are an AI assistant with a deep understanding of personal knowledge graphs. Your task is to find and articulate an insightful, non-obvious connection between two notes provided by the user.

Here is the user's NEW note:
---
{current_note_content}
---

Here is a related OLD note from their knowledge base:
---
{resonant_note_content}
---

Your task:
1.  Analyze both notes to understand their core ideas.
2.  Identify a "conceptual resonance" - a surprising or interesting link, structural similarity, or underlying principle that connects them.
3.  Write a brief, insightful message (starting with "💡 Interesting!...") that explains this connection to the user, sparking a new thought.
4.  Do NOT just summarize the notes. Focus on the *connection*.
5.  If you cannot find a meaningful connection, you must return an empty string.

Now, generate the insightful message.
"""
        
        try:
            insightful_message = await self.llm_client.generate(prompt)
            
            if insightful_message and insightful_message.strip():
                logger.info(f"Generated conceptual resonance insight: {insightful_message[:150]}...")
                return {
                    "status": "success",
                    "insight": insightful_message,
                    "source_note_id": context_data.get('current_node_id'),
                    "resonant_note_id": resonant_note_suggestion.get('id'),
                    "resonant_note_title": resonant_note_suggestion.get('title')
                }
            else:
                logger.info("LLM could not find a meaningful resonance.")
                return None
                
        except Exception as e:
            logger.error(f"Error generating conceptual resonance insight with LLM: {e}", exc_info=True)
            return None

    def analyze_conceptual_resonance(self, context_data: ContextData) -> Optional[Dict[str, Any]]:
        """
        EPC method: Analyzes the conceptual resonance for a given node content.
        Finds related notes and uses an LLM to generate an insightful connection.
        """
        return asyncio.run(self._async_analyze_conceptual_resonance(context_data))

    def test_embedding_retrieval(self, text: str):
        """
        A test method callable from Emacs to check the embedding service.
        """
        logger.info(f"Received test_embedding_retrieval request with text: '{text}'")
        if not self.content_processor:
            logger.error("ContentProcessor is not initialized.")
            return "Error: ContentProcessor not available."
        
        try:
            # Use the async content processor in a sync wrapper
            import asyncio
            # 创建临时配置用于仅嵌入模式
            embedding_config = ProcessingConfig(mode=ProcessingMode.EMBEDDING_ONLY)
            embedding_processor = ContentProcessor(embedding_config)
            
            content_item = ContentItem(
                id=f"test_{int(time.time())}",
                text=text
            )
            processing_result = asyncio.run(embedding_processor.process_single(content_item))
            
            if (processing_result.success and 
                processing_result.embedding_result and 
                processing_result.embedding_result.success):
                embedding = processing_result.embedding_result.embedding
                dimension = len(embedding)
                logger.info(f"Successfully generated embedding with dimension: {dimension}")
                return f"Success! Got embedding of dimension {dimension}. First 5 values: {embedding[:5]}"
            else:
                error_msg = processing_result.error or "Unknown error"
                logger.error(f"ContentProcessor failed to generate embedding: {error_msg}")
                return f"Error: Failed to get embedding using ContentProcessor: {error_msg}"
                
        except Exception as e:
            logger.error(f"Exception in test_embedding_retrieval: {e}", exc_info=True)
            return f"Error: Exception occurred during embedding test: {e}"

    async def _process_nodes_batch(self, nodes: List[List], total_items: int = None) -> List[bool]:
        """
        🚀 OPTIMIZED: Batch process multiple nodes with multicore support
        Separates embedding and NER processing for optimal performance
        """
        start_time = time.time()
        
        # 🔍 DEBUG: 详细检查输入数据结构
        logger.info(f"🔍 数据结构调试:")
        logger.info(f"   nodes类型: {type(nodes)}")
        logger.info(f"   nodes长度: {len(nodes)}")
        if nodes:
            logger.info(f"   第一个元素类型: {type(nodes[0])}")
            logger.info(f"   第一个元素内容: {nodes[0]}")
            if len(nodes) > 1:
                logger.info(f"   第二个元素类型: {type(nodes[1])}")
                logger.info(f"   第二个元素内容: {nodes[1]}")
        
        # 🔧 HOTFIX: 处理可能的数据格式问题
        original_nodes_length = len(nodes) if nodes else 0
        
        # 情况1: 检查是否所有节点都被包装在额外的列表中
        if nodes and all(isinstance(node, list) and len(node) == 1 for node in nodes):
            logger.warning("🔧 检测到节点数据被额外包装，正在展开...")
            nodes = [node[0] if isinstance(node[0], list) else node for node in nodes]
            logger.info(f"🔧 展开后的第一个节点: {nodes[0] if nodes else 'None'}")
        
        # 情况2: 检查是否整个nodes数组被包装在一个额外的列表中
        elif (nodes and len(nodes) == 1 and isinstance(nodes[0], list) 
              and all(isinstance(item, list) for item in nodes[0])):
            logger.warning("🔧 检测到整个nodes数组被额外包装，正在提取...")
            nodes = nodes[0]  # 提取实际的节点列表
            logger.info(f"🔧 提取后的nodes长度: {len(nodes)}")
            logger.info(f"🔧 提取后的第一个节点: {nodes[0] if nodes else 'None'}")
        
        # 情况3: 检查是否每个节点是一个字符串，需要特殊处理
        elif (nodes and all(isinstance(node, str) for node in nodes)):
            logger.warning("🔧 检测到节点数据为字符串格式，正在分析...")
            
            # 🔍 检查是否是字符分解问题
            if all(len(node) == 1 for node in nodes):
                combined_string = ''.join(nodes)
                logger.warning(f"🔧 检测到字符分解问题！重新组合字符串: '{combined_string}'")
                
                # 如果组合后的字符串看起来像关键字或数据标识符，这可能是EPC传输错误
                if combined_string.startswith(':') or 'objects' in combined_string or 'nodes' in combined_string:
                    logger.error(f"🚨 严重的EPC传输错误：数据被错误分解为单个字符！")
                    logger.error(f"🚨 原始数据可能是: '{combined_string}'")
                    logger.error(f"🚨 这表明Elisp端的数据序列化或EPC传输有问题")
                    
                    # 返回空结果，但记录详细错误信息
                    return [False] * len(nodes)
                else:
                    logger.info(f"🔍 组合后的字符串: '{combined_string}'")
            
            # 首先检查字符串内容
            for i in range(min(3, len(nodes))):  # 只检查前3个
                node_str = nodes[i]
                logger.info(f"🔍 字符串节点 {i}: 长度={len(node_str)}, 内容='{node_str[:100]}...'" if len(node_str) > 100 else f"🔍 字符串节点 {i}: 内容='{node_str}'")
                
                # 检查是否是空字符串或只有空白字符
                if not node_str or not node_str.strip():
                    logger.warning(f"🔍 节点 {i} 是空字符串或只有空白字符")
                    continue
                    
                # 检查是否包含特殊字符
                if '\x00' in node_str:
                    logger.warning(f"🔍 节点 {i} 包含null字符")
                if not node_str.isprintable():
                    logger.warning(f"🔍 节点 {i} 包含不可打印字符")
            
            # 尝试JSON解析
            try:
                import json
                parsed_nodes = []
                for i, node_str in enumerate(nodes):
                    if not node_str or not node_str.strip():
                        logger.warning(f"跳过空字符串节点 {i}")
                        continue
                        
                    try:
                        parsed_node = json.loads(node_str)
                        if isinstance(parsed_node, list):
                            parsed_nodes.append(parsed_node)
                        else:
                            logger.warning(f"JSON解析的节点 {i} 不是列表: {type(parsed_node)}")
                    except json.JSONDecodeError as e:
                        logger.error(f"节点 {i} JSON解析失败: {e} (内容预览: '{node_str[:50]}...')")
                
                if parsed_nodes:
                    nodes = parsed_nodes
                    logger.info(f"🔧 JSON解析成功，节点数量: {len(nodes)}")
                    logger.info(f"🔧 解析后的第一个节点: {nodes[0] if nodes else 'None'}")
                else:
                    logger.error("🔧 JSON解析失败，无有效节点")
                    # 如果JSON解析失败，可能是其他格式，尝试直接使用
                    logger.warning("🔧 尝试将字符串节点视为单字段节点...")
                    
                    # 检查是否可能是Elisp的打印格式，尝试简单分割
                    try:
                        test_node = nodes[0] if nodes else ""
                        if test_node and ("(" in test_node or "[" in test_node or test_node.startswith('"')):
                            logger.warning("🔧 检测到可能的Elisp格式数据，需要特殊解析")
                            # 这里可以添加Elisp格式解析逻辑
                        else:
                            logger.warning("🔧 字符串格式未知，将作为错误数据处理")
                    except Exception as parse_e:
                        logger.error(f"🔧 字符串格式分析失败: {parse_e}")
            except Exception as e:
                logger.error(f"🔧 JSON解析异常: {e}")
        
        # 情况4: 如果所有修复都失败，记录详细信息
        elif nodes:
            logger.error("🔧 未能识别的数据格式:")
            for i in range(min(3, len(nodes))):
                node = nodes[i]
                logger.error(f"   节点 {i}: 类型={type(node)}, 内容={str(node)[:100]}...")
        else:
            logger.warning("🔧 nodes为空或None")
        
        logger.info(f"🔧 数据修复完成: 原始长度={original_nodes_length}, 修复后长度={len(nodes) if nodes else 0}")
        
        # Parse nodes and prepare content items
        content_items = []
        node_info = []
        
        # 统计过滤信息
        format_errors = 0
        empty_content_count = 0
        valid_nodes = 0
        
        for i, node_list in enumerate(nodes):
            # 🔍 DEBUG: 检查每个节点的结构
            if i < 3:  # 只对前3个节点详细调试，避免日志过多
                logger.info(f"🔍 节点 {i}: 类型={type(node_list)}, 长度={len(node_list) if hasattr(node_list, '__len__') else 'N/A'}")
                logger.info(f"🔍 节点 {i}: 内容={node_list}")
            
            if len(node_list) == 6:
                node_id, title, content, tags, _, _ = node_list
                
                # 计算哈希值
                node_hash = compute_node_hash(node_id, f"{title}\n\n{content}".strip())
                content_hash = compute_content_hash(content)
                
                # 检查内容是否为空或过短
                combined_text = f"{title}\n\n{content}".strip() if title else content
                if not combined_text or len(combined_text.strip()) < 3:
                    empty_content_count += 1
                    logger.debug(f"Skipping node {i} with empty/short content: {node_id}")
                    node_info.append(None)
                    continue
                
                content_items.append(ContentItem(
                    id=node_id,
                    text=combined_text,
                    title=title,
                    hash=node_hash,
                    content_hash=content_hash
                ))
                node_info.append({
                    'node_id': node_id,
                    'title': title,
                    'content': content,
                    'tags': tags,
                    'hash': node_hash,
                    'content_hash': content_hash
                })
                valid_nodes += 1
            else:
                format_errors += 1
                logger.warning(f"Skipping malformed node data: {len(node_list)} fields (expected 6)")
                node_info.append(None)
        
        # 记录详细的过滤统计
        logger.info(f"📊 节点解析统计:")
        logger.info(f"   📥 输入节点总数: {len(nodes)}")
        logger.info(f"   ✅ 有效节点: {valid_nodes}")
        logger.info(f"   ❌ 格式错误: {format_errors}")
        logger.info(f"   📄 空内容: {empty_content_count}")
        logger.info(f"   🔄 最终处理: {len(content_items)} 个文本")
        
        # Filter out None entries
        valid_items = [(item, info) for item, info in zip(content_items, node_info) if info is not None]
        
        if not valid_items:
            logger.warning("No valid nodes to process in batch")
            return [False] * len(nodes)
        
        content_items, node_info = zip(*valid_items)
        
        # 🔥 STEP 1: TRUE Multi-core concurrent processing
        logger.info(f"🚀 Starting TRUE multi-core concurrent processing for {len(content_items)} nodes")
        
        try:
            # 1. 强制使用多核心并发嵌入处理
            texts = [item.text for item in content_items]
            node_ids = [item.id for item in content_items]
            
            logger.info(f"🔄 Starting multi-core embedding for {len(texts)} texts...")
            embedding_start = time.time()
            
            # 🎯 更新进度 - 开始嵌入
            total_for_progress = total_items if total_items else len(content_items) * 2  # 估算总数
            try:
                self._eval_in_emacs('org-supertag-background-sync--update-progress', 
                                  int(len(content_items) * 0.1), total_for_progress)
            except Exception as e:
                logger.warning(f"Progress update failed: {e}")
            
            # 强制使用多核心，降低阈值以确保启用
            embeddings = await self.llm_client.get_embeddings_batch(
                texts, 
                use_multicore=True  # 强制启用多核心
            )
            embedding_time = time.time() - embedding_start
            logger.info(f"⚡ Multi-core embedding completed in {embedding_time:.2f}s ({len(texts)/embedding_time:.1f} texts/sec)")
            
            # 2. 强制使用多核心并发NER处理（已改为真正的asyncio并发）
            logger.info(f"🔄 Starting multi-core concurrent NER for {len(texts)} texts...")
            
            # 🎯 更新进度 - 开始NER
            try:
                self._eval_in_emacs('org-supertag-background-sync--update-progress', 
                                  int(len(content_items) * 0.5), total_for_progress)
            except Exception as e:
                logger.warning(f"Progress update failed: {e}")
                
            ner_start = time.time()
            # NER服务现在已经改为真正的asyncio并发处理，不再使用批量LLM调用
            batch_ner_results = await self.ner_service.suggest_tags_batch(texts)
            ner_time = time.time() - ner_start
            logger.info(f"⚡ Multi-core concurrent NER completed in {ner_time:.2f}s ({len(texts)/ner_time:.1f} texts/sec)")
            
            # 3. 组装处理结果
            processing_results = []
            for i, (item, embedding, ner_tag_suggestions) in enumerate(zip(content_items, embeddings, batch_ner_results)):
                # 创建嵌入结果
                embedding_result = EmbeddingResult(
                    success=embedding is not None,
                    embedding=embedding,
                    error_message=None if embedding is not None else "Batch embedding failed"
                )
                
                # 处理NER标签建议结果（现在是标签建议列表，不是实体关系）
                tag_suggestions = ner_tag_suggestions if isinstance(ner_tag_suggestions, list) else []
                
                # 将标签建议转换为简单的实体对象（保持兼容性）
                entity_objects = []
                for j, suggestion in enumerate(tag_suggestions):
                    if isinstance(suggestion, dict) and 'tag_name' in suggestion:
                        entity_objects.append(ExtractedEntity(
                            id=f"{item.id}_tag_entity_{j}",
                            name=suggestion.get('tag_name', ''),
                            type='suggested_tag',
                            description=suggestion.get('reasoning', ''),
                            attributes={
                                'confidence': suggestion.get('confidence', 0.0),
                                'is_tag_suggestion': True
                            }
                        ))
                
                # 暂时没有关系数据（因为改为标签建议）
                relation_objects = []
                
                # 创建处理结果
                result = ProcessingResult(
                    content_id=item.id,
                    success=embedding is not None or bool(tag_suggestions),
                    embedding_result=embedding_result,
                    extracted_entities=entity_objects,
                    extracted_relations=relation_objects,
                    ner_result=NERResult(
                        success=bool(tag_suggestions),
                        entities=entity_objects,
                        relations=relation_objects
                    )
                )
                processing_results.append(result)
            
            batch_time = time.time() - start_time
            logger.info(f"🎉 TRUE batch processing completed in {batch_time:.2f}s")
            logger.info(f"🚄 Overall processing speed: {len(content_items)/batch_time:.1f} nodes/sec")
            logger.info(f"📊 Breakdown: Embedding {embedding_time:.1f}s, NER {ner_time:.1f}s, Assembly {batch_time-embedding_time-ner_time:.1f}s")
            
        except Exception as e:
            logger.error(f"TRUE batch processing failed: {e}")
            return [False] * len(valid_items)
        
        # 🔥 STEP 2: Process results and store in knowledge graph
        success_results = []
        
        # 计算进度更新频率
        total_nodes = len(processing_results)
        update_freq = max(1, total_nodes // 10)  # 至少更新10次
        
        for i, (processing_result, info) in enumerate(zip(processing_results, node_info)):
            node_id = info['node_id']
            tags = info['tags']
            # 从content_items中获取对应的item对象
            item = content_items[i]
            
            # 🎯 中间进度更新 - 知识图谱存储进度
            if i % update_freq == 0 or i == total_nodes - 1:
                # 计算总体进度：批量处理已完成，现在在存储阶段
                nodes_completed = len(content_items)  # 批量处理完成的节点数
                storage_progress = int((i + 1) / total_nodes * 50)  # 存储进度
                current_total_progress = nodes_completed + storage_progress
                
                try:
                    # 更新 Emacs 进度  
                    self._eval_in_emacs('org-supertag-background-sync--update-progress', 
                                      current_total_progress, total_for_progress)
                    logger.info(f"📊 Knowledge graph storage: {i+1}/{total_nodes} nodes processed")
                except Exception as e:
                    logger.warning(f"Progress update failed: {e}")
            
            try:
                success = True
                
                # Handle embedding results and store to database
                if processing_result.embedding_result and processing_result.embedding_result.success:
                    embedding = processing_result.embedding_result.embedding
                    
                    # 准备完整的节点数据用于存储 - 匹配 storage.py 期望的格式
                    from datetime import datetime
                    current_time = datetime.now().isoformat()
                    
                    node_vector_data = {
                        "node_id": node_id,
                        "content": item.text,
                        "title": item.title or "",
                        "tags": tags if isinstance(tags, list) else [],  # 确保是列表
                        "file_path": "",  # 后续可以从原始节点数据获取
                        "pos": 0,  # 设置默认值而不是 None
                        "olp": "",  # 设置默认值
                        "level": 1,  # 设置默认值
                        "scheduled": "",
                        "deadline": "",
                        "todo": "",
                        "priority": "",
                        "modified_at": current_time,
                        "properties": {},
                        "raw_value": item.text,
                        "hash": f"hash_{node_id}_{hash(item.text)}",  # 生成简单哈希
                        "content_hash": f"content_{hash(item.text)}",
                        "document_date": current_time
                    }
                    
                    try:
                        logger.info(f"💾 开始存储节点数据到数据库: {node_id}")
                        
                        # 1. 存储节点元数据
                        logger.info(f"📝 存储节点元数据: {node_id}")
                        await asyncio.to_thread(self.storage.bulk_upsert_nodes, [node_vector_data])
                        logger.info(f"✅ 节点元数据存储成功: {node_id}")
                        
                        # 2. 存储向量数据（如果VSS激活）
                        if self.storage.has_vector_ext:
                            logger.info(f"🔢 存储向量数据到VSS: {node_id} (维度: {len(embedding)})")
                            embedding_data = [{
                                "node_id_ref": node_id,
                                "embedding": embedding
                            }]
                            await asyncio.to_thread(self.storage.bulk_upsert_node_embeddings, embedding_data)
                            logger.info(f"✅ 向量数据存储成功: {node_id}")
                        else:
                            logger.info(f"⚠️ VSS未激活，向量数据未存储: {node_id}")
                        
                        logger.info(f"🎉 节点完整存储成功: {node_id} (dim={len(embedding)})")
                        
                        # 将节点本身作为实体存储到knowledge graph
                        node_entity = {
                            "id": node_id,
                            "name": item.title or f"Node {node_id}",
                            "type": "node",
                            "description": item.text[:200] + "..." if len(item.text) > 200 else item.text,
                            "attributes": {
                                "is_org_node": True,
                                "content_length": len(item.text),
                                "has_embedding": True
                            }
                        }
                        await self.knowledge_graph.upsert_entities([node_entity])
                        logger.info(f"📝 节点实体已存储到knowledge graph: {node_id}")
                        
                    except Exception as storage_error:
                        logger.error(f"❌ 存储失败 {node_id}: {storage_error}", exc_info=True)
                        success = False
                else:
                    logger.warning(f"⚠️ No embedding for node {node_id}")
                    
                    # 即使没有embedding，也要将节点作为实体存储
                    try:
                        node_entity = {
                            "id": node_id,
                            "name": item.title or f"Node {node_id}",
                            "type": "node",
                            "description": item.text[:200] + "..." if len(item.text) > 200 else item.text,
                            "attributes": {
                                "is_org_node": True,
                                "content_length": len(item.text),
                                "has_embedding": False
                            }
                        }
                        await self.knowledge_graph.upsert_entities([node_entity])
                        logger.info(f"📝 节点实体已存储到knowledge graph (无embedding): {node_id}")
                    except Exception as kg_error:
                        logger.error(f"❌ 节点实体存储失败: {kg_error}", exc_info=True)
                
                # Handle NER results
                if processing_result.ner_result and processing_result.ner_result.success:
                    entities = processing_result.ner_result.entities
                    relations = processing_result.ner_result.relations
                    
                    if entities:
                        entities_for_kg = []
                        for entity in entities:
                            entities_for_kg.append({
                                "id": entity.id,
                                "name": entity.name,
                                "type": entity.type,
                                "description": entity.description,
                                "source_node_id": node_id,
                                "attributes": entity.attributes
                            })
                        await self.knowledge_graph.upsert_entities(entities_for_kg)
                        
                    if relations:
                        relations_for_kg = []
                        for relation in relations:
                            relations_for_kg.append({
                                "id": relation.id,
                                "source_entity_id": relation.source_entity_id,
                                "target_entity_id": relation.target_entity_id,
                                "type": relation.type,
                                "description": relation.description,
                                "attributes": relation.attributes
                            })
                        await self.knowledge_graph.upsert_relations(relations_for_kg)
                        
                    logger.debug(f"✅ Stored {len(entities)} entities, {len(relations)} relations for {node_id}")
                
                # Process tags as entities (using tag name as ID to match Elisp format)
                if tags:
                    tag_entities_data = []
                    for tag in tags:
                        tag_entity = {
                            "id": tag,  # 使用tag名称本身作为ID，与Elisp保持一致
                            "name": tag,
                            "type": "tag",
                            "description": f"Tag: {tag}",
                            "source_node_id": node_id,
                            "attributes": {"is_org_tag": True}
                        }
                        tag_entities_data.append(tag_entity)
                    
                    await self.knowledge_graph.upsert_entities(tag_entities_data)
                    
                    # Create tag-to-node relations
                    tag_relations_data = []
                    for tag in tags:
                        tag_relation = {
                            "id": f"TAG_REL_{tag}_{node_id}",
                            "source_entity_id": tag,  # 使用tag名称作为实体ID
                            "target_entity_id": node_id,
                            "type": "tags",
                            "description": f"Tag '{tag}' is associated with node {node_id}",
                            "attributes": {"relationship_type": "org_tag_association"}
                        }
                        tag_relations_data.append(tag_relation)
                    
                    await self.knowledge_graph.upsert_relations(tag_relations_data)
                
                success_results.append(success)
                
            except Exception as e:
                logger.error(f"Error processing results for node {node_id}: {e}")
                success_results.append(False)
        
        # Fill in False for malformed nodes - 详细调试版本
        logger.info(f"🔍 准备组装最终结果: node_info长度={len(node_info)}, success_results长度={len(success_results)}")
        
        final_results = []
        valid_idx = 0
        valid_nodes_count = sum(1 for info in node_info if info is not None)
        logger.info(f"🔍 有效节点数量: {valid_nodes_count}")
        
        for i, info in enumerate(node_info):
            if info is not None:
                # 确保索引不越界
                if valid_idx < len(success_results):
                    result = success_results[valid_idx]
                    final_results.append(result)
                    logger.debug(f"📋 节点 {i}: valid_idx={valid_idx}, result={result}")
                    valid_idx += 1
                else:
                    logger.error(f"❌ 索引越界: valid_idx={valid_idx}, success_results长度={len(success_results)}, 节点索引={i}")
                    final_results.append(False)
            else:
                logger.debug(f"📋 跳过格式错误的节点 {i}")
                final_results.append(False)
        
        logger.info(f"🔍 最终结果组装完成: {len(final_results)} 个结果")
        
        total_time = time.time() - start_time
        successful_count = sum(final_results)
        logger.info(f"🎉 Batch processing complete: {successful_count}/{len(nodes)} nodes successful in {total_time:.2f}s")
        
        return final_results
    
    async def _process_single_node_data(self, node_id: str, content: str, tags: List[str], title: str = "") -> bool:
        """
        Internal helper to process a single node: extract entities, build graph, and vectorize.
        This is the unified logic used by both real-time and bulk sync.
        Returns True on success, False on failure.
        """
        logger.info(f"Processing node data for: {node_id}")
        try:
            # 🔧 CRITICAL FIX: Create the node entity first before processing relations
            node_entity = {
                "id": node_id,
                "name": title or f"Node {node_id[:8]}...",
                "type": "node", 
                "description": f"Org-mode node: {title}" if title else f"Org-mode node {node_id}",
                "attributes": {
                    "content_preview": content[:200] + "..." if len(content) > 200 else content,
                    "has_title": bool(title),
                    "content_length": len(content),
                    "tags_count": len(tags) if tags else 0
                }
            }
            
            # Create node entity in knowledge graph BEFORE any relations
            await self.knowledge_graph.upsert_entities([node_entity])
            logger.info(f"✅ Created node entity for {node_id}: {title or 'No title'}")
            
            # Use new ContentProcessor for both embedding and NER processing
            combined_text = f"{title}\n\n{content}".strip() if title else content
            
            content_item = ContentItem(
                id=node_id,
                text=combined_text,
                title=title
            )
            processing_result = await self.content_processor.process_single(content_item)
            
            if not processing_result.success:
                error_details = "; ".join(processing_result.errors) if processing_result.errors else "Unknown error"
                logger.error(f"ContentProcessor failed for node {node_id}: {error_details}")
                return False
            
            # 1. Handle embedding result if available
            if processing_result.embedding_result and processing_result.embedding_result.success:
                logger.info(f"✅ Embedding completed for node {node_id}")
            else:
                logger.warning(f"❌ Embedding processing failed for node {node_id}")

            # 2. Handle NER results if available  
            if processing_result.ner_result and processing_result.ner_result.success:
                entities = processing_result.ner_result.entities
                relations = processing_result.ner_result.relations
                
                if entities:
                    # Convert entities to format expected by knowledge graph
                    entities_for_kg = []
                    for entity in entities:
                        entities_for_kg.append({
                            "id": entity.id,
                            "name": entity.name,
                            "type": entity.type,
                            "description": entity.description,
                            "source_node_id": node_id,
                            "attributes": entity.attributes
                        })
                    await self.knowledge_graph.upsert_entities(entities_for_kg)
                    logger.info(f"Upserted {len(entities)} extracted entities for node {node_id}")
                
                if relations:
                    # Convert relations to format expected by knowledge graph
                    relations_for_kg = []
                    for relation in relations:
                        relations_for_kg.append({
                            "id": relation.id,
                            "source_entity_id": relation.source_entity_id,
                            "target_entity_id": relation.target_entity_id,
                            "type": relation.type,
                            "description": relation.description,
                            "source_node_id": node_id
                        })
                    await self.knowledge_graph.upsert_relations(relations_for_kg)
                    logger.info(f"Upserted {len(relations)} extracted relations for node {node_id}")
            else:
                logger.info(f"NER processing not performed or failed for node {node_id}")
            
            return True
        except Exception as e:
            logger.error(f"Error in _process_single_node_data for node {node_id}: {e}", exc_info=True)
            return False

    async def _async_analyze_node_context(self, context_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Main entry point for real-time proactive analysis.
        """
        # Hotfix for Elisp sending data as a single-element list
        if isinstance(context_data, list) and len(context_data) == 1 and isinstance(context_data[0], dict):
            logger.warning(f"Received context_data as a single-element list. Unpacking it. This indicates a client-side packaging issue that should be fixed in Elisp.")
            context_data = context_data[0]

        node_id = context_data.get("node_id")
        content = context_data.get("buffer_content")
        tags = context_data.get("current_tags", [])

        if not node_id or content is None: # content can be empty string
            msg = "Incomplete context. 'node_id' and 'buffer_content' are required."
            logger.error(msg)
            return {"status": "error", "message": msg}

        success = await self._process_single_node_data(node_id, content, tags)

        if success:
            # Placeholder for conceptual resonance logic
            insight_result = {"insight": "Node context processed successfully.", "resonant_note_title": "N/A"}
            return {"status": "success", **insight_result}
        else:
            return {"status": "error", "message": f"Failed to process node {node_id}."}

    def analyze_node_context(self, context_data: Dict[str, Any]) -> Dict[str, Any]:
        """Synchronous EPC wrapper for _async_analyze_node_context."""
        return asyncio.run(self._async_analyze_node_context(context_data))

    async def _async_bulk_process_snapshot(self, snapshot_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Main entry point for periodic background sync.
        """
        try:
            # 🔍 DEBUG: 详细检查快照数据结构
            logger.info(f"🔍 快照数据结构调试:")
            logger.info(f"   snapshot_data类型: {type(snapshot_data)}")
            if hasattr(snapshot_data, '__len__'):
                logger.info(f"   snapshot_data长度: {len(snapshot_data)}")
            logger.info(f"   snapshot_data内容预览: {str(snapshot_data)[:500]}...")
            
            # Handle different argument formats from Elisp
            if isinstance(snapshot_data, list):
                if len(snapshot_data) == 3:
                    logger.info("Converting 3-element list to expected dict format")
                    nodes, links, sync_timestamp = snapshot_data
                    snapshot_data = {
                        "nodes": nodes,
                        "links": links, 
                        "sync_timestamp": sync_timestamp
                    }
                elif len(snapshot_data) == 6:
                    logger.info("Converting 6-element list to expected dict format")
                    nodes, links, sync_timestamp, _, _, _ = snapshot_data  # Take first 3, ignore the rest
                    snapshot_data = {
                        "nodes": nodes,
                        "links": links, 
                        "sync_timestamp": sync_timestamp
                    }
                elif len(snapshot_data) == 1 and isinstance(snapshot_data[0], dict):
                    logger.warning("Unpacking single dict from list wrapper")
                    snapshot_data = snapshot_data[0]
                else:
                    logger.error(f"Unexpected list format: {len(snapshot_data)} elements")
                    return {"status": "error", "message": f"Invalid list format with {len(snapshot_data)} elements"}
            elif isinstance(snapshot_data, dict):
                # Check if this is the old :objects format from org-supertag-bridge-sync.el
                if ":objects" in snapshot_data or "objects" in snapshot_data:
                    logger.info("🔄 检测到旧的:objects格式，转换为新格式...")
                    objects_data = snapshot_data.get(":objects") or snapshot_data.get("objects", {})
                    links_data = snapshot_data.get(":links") or snapshot_data.get("links", {})
                    
                    # Convert hash tables to lists
                    nodes = []
                    links = []
                    
                    # Convert objects (nodes) from hash table to list format
                    if isinstance(objects_data, dict):
                        for node_id, node_info in objects_data.items():
                            if isinstance(node_info, dict) and node_info.get(":type") == ":node":
                                title = node_info.get(":title", "")
                                content = node_info.get(":content", node_info.get(":raw-value", ""))
                                tags = node_info.get(":tags", [])
                                file_path = node_info.get(":file-path", "")
                                modified_at = node_info.get(":modified-at", "")
                                nodes.append([node_id, title, content, tags, file_path, modified_at])
                    
                    # Convert links from hash table to list format
                    if isinstance(links_data, dict):
                        for link_id, link_info in links_data.items():
                            if isinstance(link_info, dict) and link_info.get(":type") == ":link":
                                link_type = link_info.get(":link-type", "unknown")
                                from_id = link_info.get(":from-id", "")
                                to_id = link_info.get(":to-id", "")
                                properties = link_info.get(":properties", {})
                                modified_at = link_info.get(":modified-at", "")
                                links.append([link_id, link_type, from_id, to_id, properties, modified_at])
                    
                    # Update snapshot_data to new format
                    from datetime import datetime
                    snapshot_data = {
                        "nodes": nodes,
                        "links": links,
                        "sync_timestamp": datetime.now().isoformat()
                    }
                    logger.info(f"✅ 旧格式转换完成: {len(nodes)}个节点, {len(links)}个链接")
            else:
                logger.error(f"Expected dict or list, got {type(snapshot_data)}")
                return {"status": "error", "message": f"Invalid data format"}

            nodes = snapshot_data.get("nodes", [])
            links = snapshot_data.get("links", [])
            new_timestamp = snapshot_data.get("sync_timestamp")
            
            # 🔍 DEBUG: 检查解析后的nodes结构
            logger.info(f"🔍 解析后的数据结构:")
            logger.info(f"   nodes类型: {type(nodes)}, 长度: {len(nodes) if nodes else 0}")
            logger.info(f"   links类型: {type(links)}, 长度: {len(links) if links else 0}")
            if nodes:
                logger.info(f"   第一个node类型: {type(nodes[0])}")
                logger.info(f"   第一个node内容: {nodes[0]}")
                
                # 🚨 检查是否是EPC传输错误（字符分解）
                if (isinstance(nodes, list) and all(isinstance(node, str) and len(node) == 1 for node in nodes)):
                    combined = ''.join(nodes)
                    logger.error(f"🚨 检测到EPC传输错误！nodes被分解为单个字符: '{combined}'")
                    logger.error(f"🚨 这可能是Elisp端bulk_process_snapshot调用的参数传递问题")
                    logger.error(f"🚨 建议检查Elisp端的EPC调用格式和参数序列化")
                    
                    return {
                        "status": "error",
                        "message": f"EPC传输错误：数据被错误分解为单个字符 ('{combined}')",
                        "error_type": "epc_transmission_error",
                        "suggestion": "检查Elisp端的bulk_process_snapshot调用参数格式"
                    }
            
            processed_nodes = 0
            failed_nodes = 0

            total_items = len(nodes) + len(links)
            processed_items = 0
            
            logger.info(f"🚀 开始批量处理快照:")
            logger.info(f"   📊 总对象数: {total_items}")
            logger.info(f"   📝 节点数: {len(nodes)} (需要文本处理)")  
            logger.info(f"   🔗 链接数: {len(links)} (关系处理)")
            logger.info(f"   🏷️  估算标签数: {total_items - len(nodes) - len(links)} (元数据处理)")
        
        except Exception as e:
            logger.error(f"❌ 批量处理初始化失败: {e}", exc_info=True)
            return {"status": "error", "message": f"初始化失败: {str(e)}"}
        
        # 🚀 OPTIMIZED: Batch processing with multicore support
        try:
            if len(nodes) >= 3:  # Use batch processing for 3+ nodes
                logger.info(f"🔥 使用优化批量处理 {len(nodes)} 个节点")
                
                # 🎯 批量处理进度更新 - 开始处理
                try:
                    self._eval_in_emacs('org-supertag-background-sync--update-progress', 
                                      0, total_items)
                except Exception as e:
                    logger.warning(f"Failed to update progress in Emacs: {e}")
                
                logger.info(f"🔥 正在批量处理 {len(nodes)} 个节点...")
                
                success_results = await self._process_nodes_batch(nodes, total_items)
                processed_nodes = sum(success_results)
                failed_nodes = len(nodes) - processed_nodes
                processed_items = len(nodes)
                
                # 🎯 批量处理完成进度更新
                try:
                    self._eval_in_emacs('org-supertag-background-sync--update-progress', 
                                      processed_items, total_items)
                except Exception as e:
                    logger.warning(f"Failed to update progress in Emacs: {e}")
                
                logger.info(f"✅ 已完成 {processed_nodes} 个节点的批量处理")
            else:
                # Fallback to serial processing for small batches
                logger.info("Using serial processing for small batch")
                for i, node_list in enumerate(nodes):
                    # Elisp snapshot format: [ID, TITLE, CONTENT, TAGS, FILE_PATH, MODIFIED_AT]
                    if len(node_list) == 6:
                        node_id, title, content, tags, _, _ = node_list
                        if await self._process_single_node_data(node_id, content, tags, title):
                            processed_nodes += 1
                        else:
                            failed_nodes += 1
                    else:
                        logger.warning(f"Skipping malformed node in bulk sync: {node_list}")
                    
                    processed_items += 1
                    
                    # 🎯 串行处理进度更新
                    if processed_items % 10 == 0 or processed_items == len(nodes):
                        try:
                            self._eval_in_emacs('org-supertag-background-sync--update-progress', 
                                              processed_items, total_items)
                            logger.info(f"📊 已处理 {processed_items}/{len(nodes)} 个节点")
                        except Exception as e:
                            logger.warning(f"Failed to update progress in Emacs: {e}")
        
        except Exception as e:
            logger.error(f"❌ 节点批量处理失败: {e}", exc_info=True)
            # 即使失败也要返回部分结果，确保增量同步不受影响
            failed_nodes = len(nodes) - processed_nodes
        
        # 🔗 Process links AFTER all nodes are processed (relations need entities to exist first)
        try:
            if links:
                logger.info(f"🔗 开始处理关系链接 (在所有节点处理完成后)")
                relations_to_upsert = []
                for i, link_list in enumerate(links):
                     # Elisp snapshot format: [LINK_ID, TYPE, FROM_ID, TO_ID, PROPERTIES, MODIFIED_AT]
                    if len(link_list) == 6:
                        link_id, link_type, from_id, to_id, props, _ = link_list
                        
                        # 确保ID是字符串类型，防止unhashable type错误
                        link_id = str(link_id) if link_id is not None else f"link_{i}"
                        from_id = str(from_id) if from_id is not None else f"unknown_source_{i}"
                        to_id = str(to_id) if to_id is not None else f"unknown_target_{i}"
                        link_type = str(link_type) if link_type is not None else "unknown"
                        props = props if isinstance(props, dict) else {}
                        
                        relations_to_upsert.append({
                            "id": link_id, 
                            "source_entity_id": from_id, 
                            "target_entity_id": to_id,
                            "type": link_type, 
                            "attributes": props
                        })
                    
                    processed_items += 1
                    
                    # 🎯 链接处理进度更新
                    if processed_items % 10 == 0 or i == len(links) - 1:
                        try:
                            self._eval_in_emacs('org-supertag-background-sync--update-progress', 
                                              processed_items, total_items)
                            logger.info(f"🔗 已处理 {i+1}/{len(links)} 个链接")
                        except Exception as e:
                            logger.warning(f"Failed to update progress in Emacs: {e}")
                
                # 批量插入所有关系（在所有节点处理完成后）
                if relations_to_upsert:
                    logger.info(f"🔗 开始批量插入 {len(relations_to_upsert)} 个关系")
                    await self.knowledge_graph.upsert_relations(relations_to_upsert)
                    logger.info(f"✅ 批量插入关系成功: {len(relations_to_upsert)} 个")
                else:
                    logger.info("🔗 没有关系需要插入")
        
        except Exception as e:
            logger.error(f"❌ 链接处理失败: {e}", exc_info=True)

        logger.info(f"🎉 批量处理完成. 节点处理: {processed_nodes}, 失败: {failed_nodes}.")
        
        # 确保总是返回成功状态，这样Emacs端能继续增量同步
        return {
            "status": "success",
            "message": "快照处理完成",
            "nodes_processed": processed_nodes,
            "nodes_failed": failed_nodes,
            "links_processed": len(links) if links else 0,
            "new_timestamp": new_timestamp
        }

    def bulk_process_snapshot(self, snapshot_data: Dict[str, Any]) -> Dict[str, Any]:
        """Synchronous EPC wrapper for _async_bulk_process_snapshot."""
        return asyncio.run(self._async_bulk_process_snapshot(snapshot_data))
    
    def get_processing_status(self) -> Dict[str, Any]:
        """获取当前处理状态和日志"""
        try:
            # 导入日志监控
            from .services.log_monitor import get_log_monitor
            
            monitor = get_log_monitor()
            if not monitor:
                return {
                    "status": "error",
                    "message": "Log monitor not available"
                }
            
            # 获取当前状态
            current_status = monitor.get_current_status()
            
            # 获取最近事件
            recent_events = monitor.get_recent_events(count=20)
            
            # 获取全局统计
            global_stats = monitor.get_global_stats()
            
            return {
                "status": "success",
                "current_status": current_status,
                "recent_events": recent_events,
                "global_stats": global_stats,
                "timestamp": time.time()
            }
            
        except Exception as e:
            logger.error(f"Failed to get processing status: {e}")
            return {
                "status": "error",
                "message": str(e)
            }
    
    def print_processing_report(self) -> Dict[str, Any]:
        """打印处理状态报告"""
        try:
            from .services.log_monitor import get_log_monitor
            
            monitor = get_log_monitor()
            if not monitor:
                return {
                    "status": "error",
                    "message": "Log monitor not available"
                }
            
            # 打印状态报告
            monitor.print_status_report()
            
            return {
                "status": "success",
                "message": "Status report printed to console"
            }
            
        except Exception as e:
            logger.error(f"Failed to print processing report: {e}")
            return {
                "status": "error",
                "message": str(e)
            }


def main():
    parser = argparse.ArgumentParser(description="Org SuperTag EPC Bridge Server (Python)")
    parser.add_argument("emacs_epc_port", type=int, help="Port number of the Emacs-side EPC server.")
    parser.add_argument("data_directory", type=str, help="Path to the data directory for org-supertag.")
    parser.add_argument("--host", type=str, default="127.0.0.1", help="Host for this Python EPC server to listen on (default: 127.0.0.1).")
    parser.add_argument("--profile", action="store_true", help="Enable cProfile for performance analysis.")
    
    args = parser.parse_args()

    # Crucial: Setup logging right at the start, using the data_directory for log file placement.
    # This ensures all subsequent operations, including SimTagBridge initialization, are logged.
    data_dir_for_log = args.data_directory if args.data_directory else os.path.expanduser("~/.emacs.d/org-supertag")
    if not os.path.exists(data_dir_for_log):
        os.makedirs(data_dir_for_log, exist_ok=True)
    
    # Call the centralized logging setup
    # This now configures the "simtag" parent logger.
    setup_simtag_logging(data_dir_for_log)
    
    # Use a logger instance for subsequent info messages.
    # The global 'logger' is logging.getLogger("simtag_bridge").
    # Messages to it will now be handled by the "simtag" parent logger's handlers.
    main_logger = logging.getLogger("simtag_bridge.main") # Or just use the global 'logger'
    main_logger.info(f"Starting SimTagBridge with args: Port={args.emacs_epc_port}, DataDir='{data_dir_for_log}', Host='{args.host}', Profile={args.profile}")

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
                bridge_instance = SimTagBridge(args.emacs_epc_port, data_dir_for_log, server_host=args.host)
                if bridge_instance and bridge_instance.server_thread:
                     bridge_instance.server_thread.join()
            
            profiler.runcall(profiled_run)

        finally:
            profile_path = os.path.join(data_dir_for_log, "simtag_bridge.prof")
            profiler.dump_stats(profile_path)
            main_logger.info(f"Profiling data saved to {profile_path}")
            if bridge_instance: # If instance was created
                bridge_instance.cleanup() # Explicitly call cleanup for profiled run
    else:
        try:
            bridge_instance = SimTagBridge(args.emacs_epc_port, data_dir_for_log, server_host=args.host)
            if bridge_instance.server_thread:
                bridge_instance.server_thread.join() 
        except KeyboardInterrupt:
            main_logger.info("KeyboardInterrupt received, shutting down SimTagBridge.")
        finally:
            if bridge_instance: # If instance was created
                main_logger.info("SimTagBridge main process initiating cleanup...")
                bridge_instance.cleanup() # Call cleanup on the instance
            else:
                main_logger.info("SimTagBridge main process exiting (no instance for cleanup).")

    main_logger.info("SimTagBridge main process finished.")


if __name__ == "__main__":
    main() 
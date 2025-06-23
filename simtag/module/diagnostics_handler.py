#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
import traceback
import time
from typing import Dict, Any
import asyncio

from simtag.core.graph_service import GraphService

logger = logging.getLogger(__name__)

class DiagnosticsHandler:
    def __init__(self, config, llm_client, graph_service: GraphService, memory_engine, entity_extractor, rag_engine, emacs_client, data_dir, content_processor):
        self.config = config
        self.llm_client = llm_client
        self.graph_service = graph_service
        self.memory_engine = memory_engine
        self.entity_extractor = entity_extractor
        self.rag_engine = rag_engine
        self.emacs_client = emacs_client
        self.data_dir = data_dir
        self.content_processor = content_processor
        logger.info("DiagnosticsHandler initialized with extended dependencies.")

    def get_status(self) -> Dict[str, Any]:
        """
        Gets the system status. Moved from SimTagBridge.
        """
        logger.info("get_status called")
        try:
            # Check LLM client availability
            llm_available = False
            llm_model = "unknown"
            try:
                if self.llm_client:
                    llm_available = True
                    llm_model = getattr(self.config, 'llm_client_config', {}).get('default_model', 'unknown')
            except Exception as e:
                logger.warning(f"Could not check LLM status: {e}")

            # Check storage stats
            storage_stats = {}
            try:
                if self.graph_service:
                    storage_stats = self.graph_service.get_stats()
            except Exception as e:
                logger.warning(f"Could not get storage stats: {e}")

            status = {
                "llm_available": llm_available,
                "llm_model": llm_model,
                "graph_service_ready": self.graph_service is not None,
                "storage_stats": storage_stats,
                "memory_engine_ready": self.memory_engine is not None,
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

    def get_config(self) -> Dict[str, Any]:
        """
        Gets the current configuration. Moved from SimTagBridge.
        """
        logger.info("EPC Call: get_config")
        try:
            return {
                "status": "success",
                "config": self.config.to_dict()  # Assuming Config has a to_dict method
            }
        except Exception as e:
            logger.error(f"Get config failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def check_imports(self) -> Dict[str, Any]:
        """
        Checks for the presence of critical Python modules. Moved from SimTagBridge.
        """
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

    def test_embedding_retrieval(self, text: str):
        from simtag.services.content_processor import ProcessingConfig, ProcessingMode, ContentItem
        import time

        logger.info(f"Received test_embedding_retrieval request with text: '{text}'")
        if not self.content_processor:
            return "Error: ContentProcessor not available."
        
        try:
            embedding_config = ProcessingConfig(mode=ProcessingMode.EMBEDDING_ONLY)
            embedding_processor = self.content_processor.__class__(embedding_config)
            
            content_item = ContentItem(id=f"test_{int(time.time())}", text=text)
            processing_result = asyncio.run(embedding_processor.process_single(content_item))
            
            if processing_result.success and processing_result.embedding_result and processing_result.embedding_result.success:
                embedding = processing_result.embedding_result.embedding
                return f"Success! Got embedding of dimension {len(embedding)}."
            else:
                return f"Error: {processing_result.error or 'Unknown error'}"
        except Exception as e:
            return f"Error: Exception occurred: {e}"

    def get_processing_status(self) -> Dict[str, Any]:
        return {"status": "error", "message": "Log monitor not available in this version."}
    
    def print_processing_report(self) -> Dict[str, Any]:
        return {"status": "error", "message": "Log monitor not available in this version."} 
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
    def __init__(self, config, graph_service: GraphService, llm_client, embedding_service):
        self.config = config
        self.graph_service = graph_service
        self.llm_client = llm_client
        self.embedding_service = embedding_service
        logger.info("DiagnosticsHandler initialized with modern dependencies.")

    def get_status(self) -> Dict[str, Any]:
        """
        Gets the system status.
        """
        logger.info("get_status called")
        try:
            llm_available = self.llm_client is not None
            llm_model = "unknown"
            if llm_available:
                try:
                    llm_model = self.llm_client.get_default_model()
                except Exception:
                    pass

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
                "embedding_service_ready": self.embedding_service is not None,
                "storage_stats": storage_stats,
                "server_running": True,
                "config": {
                    "data_directory": self.config.data_directory,
                    "db_path": self.config.db_path,
                }
            }
            return {"status": "success", "result": status}
        except Exception as e:
            logger.error(f"Get status failed: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)}

    def get_config(self) -> Dict[str, Any]:
        """
        Gets the current configuration.
        """
        logger.info("EPC Call: get_config")
        try:
            return {
                "status": "success",
                "config": self.config.model_dump()
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

    def get_processing_status(self) -> Dict[str, Any]:
        return {"status": "error", "message": "Log monitor not available in this version."}
    
    def print_processing_report(self) -> Dict[str, Any]:
        return {"status": "error", "message": "Log monitor not available in this version."}



    async def test_embedding_retrieval(self, text: str) -> Dict[str, Any]:
        """Tests the full embedding retrieval pipeline for a given text."""
        logger.info(f"Running embedding test for text: '{text[:30]}...'")
        if not self.embedding_service:
            return {"status": "error", "message": "EmbeddingService is not available."}
        
        start_time = time.time()
        result = await self.embedding_service.get_embedding(text)
        end_time = time.time()
        
        if result.success:
            return {
                "status": "success",
                "embedding_length": len(result.embedding) if result.embedding else 0,
                "model_used": result.model_used,
                "backend_used": result.backend_used,
                "processing_time_ms": (end_time - start_time) * 1000
            }
        else:
            return {
                "status": "error",
                "message": result.error_message,
                "model_used": result.model_used,
                "backend_used": result.backend_used,
            }



    async def get_available_models(self) -> Dict[str, Any]:
        """
        Gets the available models from the LLM client.
        """
        try:
            if self.llm_client:
                # This assumes the llm_client has a method to get available models.
                # We will need to add this to the LLMClient class.
                models = await self.llm_client.get_available_models()
                return {"status": "success", "models": models}
            return {"status": "error", "message": "LLM client not available."}
        except Exception as e:
            logger.error(f"Failed to get available models: {e}\n{traceback.format_exc()}")
            return {"status": "error", "message": str(e)} 
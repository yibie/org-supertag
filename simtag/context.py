import logging
from typing import Optional

from .config import Config
from .core.graph_service import GraphService
from .services.embedding_service import EmbeddingService
from .services.llm_client import LLMClient
from .services.rag_service import RAGService
from simtag.module.rag_handler import RAGHandler
from simtag.module.sync_handler import SyncHandler
from simtag.module.autotag_handler import AutotagHandler
from simtag.module.knowledge_handler import KnowledgeHandler
from simtag.module.diagnostics_handler import DiagnosticsHandler
from simtag.module.feedback_handler import FeedbackHandler
from simtag.module.query_handler import QueryHandler
from simtag.module.smart_companion_handler import SmartCompanionHandler
from simtag.module.ai_handler import AIHandler

logger = logging.getLogger(__name__)

class AppContext:
    """
    A simple singleton container for holding and providing access to all
    application-level services. This replaces the dependency-injector container
    for a more explicit and straightforward approach to service management.
    """
    def __init__(self):
        self.config: Optional[Config] = None
        self.embedding_service: Optional[EmbeddingService] = None
        self.llm_client: Optional[LLMClient] = None
        self.graph_service: Optional[GraphService] = None
        self.rag_service: Optional[RAGService] = None
        self.sync_handler: Optional[SyncHandler] = None
        self.autotag_handler: Optional[AutotagHandler] = None
        self.diagnostics_handler: Optional[DiagnosticsHandler] = None
        self.feedback_handler: Optional[FeedbackHandler] = None
        self.query_handler: Optional[QueryHandler] = None
        self.rag_handler: Optional[RAGHandler] = None
        self.knowledge_handler: Optional[KnowledgeHandler] = None
        self.smart_companion_handler: Optional[SmartCompanionHandler] = None
        self.ai_handler: Optional[AIHandler] = None
        self.emacs_client: Optional[any] = None # Will be set from SimTagBridge
        self.port: Optional[int] = None # Will be set from SimTagBridge

    def initialize(self, port: int, emacs_client: any):
        """
        Initializes all services in the correct dependency order.
        """
        logger.info("Initializing application context and services...")
        
        self.port = port
        self.emacs_client = emacs_client

        # Level 0: No dependencies
        self.config = Config()
        logger.info("Config loaded.")

        # Level 1: Depends on Config
        self.llm_client = LLMClient(config=self.config.llm)
        logger.info("Core services (LLM) initialized.")

        # Level 2: Depends on Level 1 services
        self.graph_service = GraphService(
            db_path=self.config.vector_db_path,
            config=self.config
        )
        # EmbeddingService depends on both config and graph_service
        self.embedding_service = EmbeddingService(
            config=self.config,
            graph_service=self.graph_service
        )
        logger.info("GraphService and EmbeddingService initialized.")
        
        # Level 3: Depends on Level 2 services (RAG services)
        self.rag_service = RAGService(
            llm_client=self.llm_client,
            graph_service=self.graph_service,
            embedding_service=self.embedding_service,
            config=self.config
        )
        logger.info("RAG and Orchestration services initialized.")
        
        # Level 4: Handlers (depend on various services)
        self.sync_handler = SyncHandler(
            graph_service=self.graph_service,
            embedding_service=self.embedding_service,
            rag_service=self.rag_service,
            config=self.config,
        )
        self.autotag_handler = AutotagHandler(
            llm_client=self.llm_client,
        )
        self.diagnostics_handler = DiagnosticsHandler(
            config=self.config,
            graph_service=self.graph_service,
            llm_client=self.llm_client,
            embedding_service=self.embedding_service
        )
        self.feedback_handler = FeedbackHandler(
            config=self.config
        )
        self.query_handler = QueryHandler(
            graph_service=self.graph_service
        )
        self.rag_handler = RAGHandler(
            rag_service=self.rag_service
        )
        self.knowledge_handler = KnowledgeHandler(
            config=self.config,
            graph_service=self.graph_service,
            embedding_service=self.embedding_service,
            rag_service=self.rag_service,
            llm_client=self.llm_client
        )
        self.smart_companion_handler = SmartCompanionHandler(
            autotag_handler=self.autotag_handler
        )
        self.ai_handler = AIHandler(
            rag_service=self.rag_service,
            graph_service=self.graph_service
        )
        # 向后兼容：旧代码可能仍访问 reasoning_handler
        self.reasoning_handler = self.knowledge_handler
        logger.info("All handlers initialized.")
        logger.info("Application context fully initialized.")

# Global context instance
context = AppContext() 
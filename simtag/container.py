import logging
import asyncio

logger = logging.getLogger(__name__)

from dependency_injector import containers, providers
from simtag.config import Config
from simtag.core.graph_service import GraphService
from simtag.services.llm_client import LLMClient
from simtag.core.tagging import TaggingEngine
from simtag.services.ner_service import NERService
from simtag.services.embedding_service import EmbeddingService
from simtag.core.memory_engine import MemoryEngine
from simtag.services.memory_synthesizer import MemorySynthesizer
from simtag.core.entity_extractor import LLMEntityExtractor
from simtag.services.content_processor import ContentProcessor
from simtag.core.rag_engine import OrgSupertagRAGEngine
from simtag.services.user_interface import UserInterfaceService
from simtag.module.node_processor import NodeProcessor
from simtag.module.sync_handler import SyncHandler
from simtag.module.query_handler import QueryHandler
from simtag.module.diagnostics_handler import DiagnosticsHandler
from simtag.module.autotag_handler import AutotagHandler
from simtag.services.smart_ner_service import SmartNERService
from simtag.module.resonance_handler import ResonanceHandler
from simtag.module.rag_handler import RAGHandler
from simtag.module.reasoning_handler import ReasoningHandler

class AppContainer(containers.DeclarativeContainer):
    config = providers.Configuration()
    
    config_obj = providers.Factory(
        lambda config_dict: Config(**{
            key: config_dict[key]
            for key in list(Config.model_fields.keys())
            if key in config_dict
        }),
        config_dict=config
    )

    # --- Unified Data Service ---
    graph_service = providers.Singleton(
        GraphService,
        db_path=config.vector_db_path,
        config=config_obj
    )

    # Core Services
    llm_client = providers.Singleton(
        lambda config_dict: LLMClient(
            provider=config_dict.get('llm_client_config', {}).get('provider', 'ollama'),
            config=config_dict.get('llm_client_config', {})
        ),
        config_dict=config
    )
    embedding_service = providers.Singleton(
        EmbeddingService,
        config_dict=config
    )
    smart_ner_service = providers.Singleton(
        SmartNERService,
        llm_async_callable=llm_client.provided.generate,
        config=config
    )
    ner_service = providers.Singleton(
        NERService,
        config=config.ner,
        llm_client=llm_client,
        smart_ner_service=smart_ner_service
    )
    
    entity_extractor = providers.Singleton(
        LLMEntityExtractor,
        llm_client=llm_client,
        config=config.analysis_config
    )
    
    # Processing Services
    content_processor = providers.Singleton(ContentProcessor, config=config_obj)
    
    # Engines
    rag_engine = providers.Singleton(OrgSupertagRAGEngine, graph_service=graph_service, llm_client=llm_client, embedding_service=embedding_service, config=config_obj)
    memory_engine = providers.Singleton(MemoryEngine, config=config_obj, llm_client=llm_client)
    memory_synthesizer = providers.Singleton(MemorySynthesizer, config=config_obj, memory_engine=memory_engine, llm_client=llm_client)
    engine = providers.Singleton(TaggingEngine, config=config_obj, graph_service=graph_service, llm_client=llm_client)
    
    # UI Service
    user_interface = providers.Singleton(UserInterfaceService, graph_service=graph_service)
    
    # Handlers
    node_processor = providers.Factory(NodeProcessor, config=config_obj, llm_client=llm_client, ner_service=ner_service, graph_service=graph_service, content_processor=content_processor, emacs_client=config.emacs_client, embedding_service=embedding_service, entity_extractor=entity_extractor)
    sync_handler = providers.Factory(SyncHandler, node_processor=node_processor, engine=engine, emacs_client=config.emacs_client)
    query_handler = providers.Factory(QueryHandler, engine=engine, user_interface=user_interface, graph_service=graph_service, emacs_client=config.emacs_client)
    diagnostics_handler = providers.Factory(DiagnosticsHandler, config=config_obj, llm_client=llm_client, graph_service=graph_service, memory_engine=memory_engine, entity_extractor=entity_extractor, rag_engine=rag_engine, emacs_client=config.emacs_client, data_dir=config.data_dir, content_processor=content_processor)
    autotag_handler = providers.Factory(AutotagHandler, llm_client=llm_client, ner_service=ner_service)
    resonance_handler = providers.Factory(ResonanceHandler, graph_service=graph_service, llm_client=llm_client, config=config_obj)
    rag_handler = providers.Factory(RAGHandler, rag_engine=rag_engine, llm_client=llm_client)
    reasoning_handler = providers.Factory(ReasoningHandler, config=config_obj, graph_service=graph_service, entity_extractor=entity_extractor, embedding_service=embedding_service, rag_engine=rag_engine)

    async def shutdown_services(self):
        """Gracefully shuts down singleton services.
        
        This is a coroutine and must be awaited.
        """
        logger.info("Shutting down services...")
        # Get the instances of the services
        llm_client_instance = await self.llm_client()
        embedding_service_instance = await self.embedding_service()
        graph_service_instance = await self.graph_service()

        # Create a list of shutdown tasks
        shutdown_tasks = []
        if hasattr(llm_client_instance, 'close') and asyncio.iscoroutinefunction(llm_client_instance.close):
            shutdown_tasks.append(llm_client_instance.close())
        
        if hasattr(embedding_service_instance, 'close') and asyncio.iscoroutinefunction(embedding_service_instance.close):
            shutdown_tasks.append(embedding_service_instance.close())

        if hasattr(graph_service_instance, 'close') and not asyncio.iscoroutinefunction(graph_service_instance.close):
            # Assuming graph_service.close() is synchronous
            graph_service_instance.close()

        # Run async shutdown tasks concurrently
        if shutdown_tasks:
            await asyncio.gather(*shutdown_tasks)
        
        logger.info("All services have been shut down.") 
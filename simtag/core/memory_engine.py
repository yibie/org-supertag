"""
Memory Engine for Org SuperTag

This module implements the MemoryEngine responsible for managing user preferences, 
behavioral patterns, dialogue history, and constructing context snapshots for the LLM.
Based on living-doc-features.org sections 10.7.A.4 and 10.8.F.
"""
import asyncio
import logging
import time
from typing import Dict, List, Any, Optional, Union, Callable, Awaitable
from dataclasses import dataclass, field
from enum import Enum
import sqlite3 # Added for SQLite persistence
import json # Added for JSON serialization

# Assuming Config and other services will be imported when needed
# from simtag.config import Config
# from ..services.llm_client import LLMClient 
# from .storage import StorageService # If we use a unified storage service

logger = logging.getLogger(__name__)

class MemoryItemType(Enum):
    USER_PREFERENCE = "user_preference"
    BEHAVIORAL_PATTERN = "behavioral_pattern"
    DIALOGUE_TURN = "dialogue_turn"
    CONTEXT_SNAPSHOT = "context_snapshot" # A snapshot of what was sent to LLM
    USER_FEEDBACK = "user_feedback" # Explicit feedback on AI responses
    SYSTEM_STATE = "system_state" # e.g., current dialogue mode

@dataclass
class MemoryItem:
    """Generic container for a piece of memory."""
    id: str 
    type: MemoryItemType
    content: Any
    timestamp: Optional[float] = None
    metadata: Optional[Dict[str, Any]] = None

    def __post_init__(self):
        if self.timestamp is None:
            self.timestamp = time.time()
        if self.metadata is None:
            self.metadata = {}

@dataclass
class UserPreference(MemoryItem):
    """Stores explicit user preferences."""
    key: str = "" # Default value to avoid field order issues
    # content field from MemoryItem will store the value of the preference
    # type will be MemoryItemType.USER_PREFERENCE
    def __post_init__(self):
        super().__post_init__()  # Call parent's __post_init__ first
        if not self.key:  # Validate that key was actually provided
            raise ValueError("UserPreference requires a non-empty 'key' field")
        self.type = MemoryItemType.USER_PREFERENCE
        if not self.id: # Auto-generate ID if not provided
            self.id = f"pref_{self.key}_{self.timestamp}"

@dataclass
class BehavioralPattern(MemoryItem):
    """Stores inferred user behavioral patterns."""
    pattern_type: str = "" # Default value to avoid field order issues
    confidence: float = 0.0 # Confidence in the inferred pattern
    def __post_init__(self):
        super().__post_init__()  # Call parent's __post_init__ first
        if not self.pattern_type:  # Validate that pattern_type was actually provided
            raise ValueError("BehavioralPattern requires a non-empty 'pattern_type' field")
        self.type = MemoryItemType.BEHAVIORAL_PATTERN
        if not self.id:
            self.id = f"pattern_{self.pattern_type}_{self.timestamp}"
        self.metadata['confidence'] = self.confidence


@dataclass
class DialogueTurn:
    """Represents a single turn in a dialogue."""
    speaker: str = "" # Default value to avoid field order issues
    text: str = "" # Default value to avoid field order issues
    timestamp: float = field(default_factory=time.time)
    metadata: Dict[str, Any] = field(default_factory=dict) # e.g., associated node_id, embeddings
    
    def __post_init__(self):
        if not self.speaker or not self.text:  # Validate required fields
            raise ValueError("DialogueTurn requires non-empty 'speaker' and 'text' fields")

@dataclass
class DialogueHistory(MemoryItem):
    """Stores a sequence of dialogue turns for a session or topic."""
    session_id: str = "" # Default value to avoid field order issues
    turns: List[DialogueTurn] = field(default_factory=list)
    summary: Optional[str] = None # Optional summary of the dialogue
    def __post_init__(self):
        super().__post_init__()  # Call parent's __post_init__ first
        if not self.session_id:  # Validate that session_id was actually provided
            raise ValueError("DialogueHistory requires a non-empty 'session_id' field")
        self.type = MemoryItemType.DIALOGUE_TURN # Or a new type like DIALOGUE_SESSION
        if not self.id:
            self.id = f"dialogue_{self.session_id}_{self.timestamp}"
        self.content = self.turns # Store turns in content for generic access

@dataclass
class ContextSnapshot(MemoryItem):
    """
    Represents the context provided to the LLM at a specific point in time.
    This helps in understanding LLM behavior and for debugging.
    """
    triggering_query: str = "" # Default value to avoid field order issues
    # content will be a dict of context elements: 
    # e.g. {'prompt': str, 'retrieved_docs': List[str], 'user_prefs': Dict}
    # type will be MemoryItemType.CONTEXT_SNAPSHOT
    llm_response: Optional[str] = None # The response generated from this context
    def __post_init__(self):
        super().__post_init__()  # Call parent's __post_init__ first
        if not self.triggering_query:  # Validate that triggering_query was actually provided
            raise ValueError("ContextSnapshot requires a non-empty 'triggering_query' field")
        self.type = MemoryItemType.CONTEXT_SNAPSHOT
        if not self.id:
            self.id = f"snapshot_{self.timestamp}"


class MemoryEngine:
    """
    Manages various forms of memory for the org-supertag system.
    Handles storage, retrieval, and processing of memory items.
    """

    def __init__(self, 
                 config: Any, # simtag.config.Config
                 llm_client: Optional[Any] = None, # LLMClient for summarization etc.
                 # storage_service: Optional[Any] = None # For persistent storage - replacing with direct SQLite
                 db_path: Optional[str] = None # New parameter for DB path
                ):
        """
        Initializes the MemoryEngine.

        Args:
            config: Configuration object.
            llm_client: Optional LLM client for tasks like summarization.
            db_path: Optional path to the SQLite database file. If None, uses in-memory.
        """
        self.config = config
        self.llm_client = llm_client
        self.db_path = db_path if db_path else getattr(config, 'memory_db_path', None)
        self.db_conn: Optional[sqlite3.Connection] = None

        # In-memory storage for now; will be augmented/replaced by storage_service
        self._memory_items: Dict[str, MemoryItem] = {}
        
        if self.db_path:
            logger.info(f"MemoryEngine initializing with SQLite persistence at: {self.db_path}")
            self._init_db()
        else:
            logger.info("MemoryEngine initializing with in-memory storage only.")

        self.retention_period = getattr(config, 'memory_retention_period_days', 30) * 86400 # seconds
        self.max_elements = getattr(config, 'memory_max_elements', 10000)
        self.auto_summary_interval = getattr(config, 'memory_auto_summary_interval_hours', 24) * 3600 # seconds
        
        logger.info(f"MemoryEngine initialized. Retention: {self.retention_period/86400} days, Max elements: {self.max_elements}.")
        # TODO: Load existing memory from storage_service if provided and implemented.
        # If using DB, load from DB here.
        if self.db_conn:
            self._load_memory_from_db() # Placeholder for method to load data on startup

    def _init_db(self):
        """Initializes the SQLite database connection and creates tables if they don't exist."""
        if not self.db_path:
            return
        try:
            self.db_conn = sqlite3.connect(self.db_path)
            self.db_conn.row_factory = sqlite3.Row # Access columns by name
            cursor = self.db_conn.cursor()
            self._create_db_tables(cursor)
            self.db_conn.commit()
            logger.info(f"SQLite database initialized successfully at {self.db_path}")
        except sqlite3.Error as e:
            logger.error(f"Error initializing SQLite database at {self.db_path}: {e}", exc_info=True)
            self.db_conn = None # Ensure connection is None if setup failed

    def _create_db_tables(self, cursor: sqlite3.Cursor):
        """Creates the necessary SQLite tables for memory persistence."""
        # User Preferences Table
        cursor.execute("""
        CREATE TABLE IF NOT EXISTS user_preferences (
            id TEXT PRIMARY KEY,
            key TEXT UNIQUE NOT NULL,
            content_json TEXT,
            timestamp REAL,
            metadata_json TEXT
        )
        """)
        logger.debug("Table 'user_preferences' checked/created.")

        # Behavioral Patterns Table
        cursor.execute("""
        CREATE TABLE IF NOT EXISTS behavioral_patterns (
            id TEXT PRIMARY KEY,
            pattern_type TEXT NOT NULL,
            content_json TEXT,
            confidence REAL,
            timestamp REAL,
            metadata_json TEXT 
        )
        """)
        logger.debug("Table 'behavioral_patterns' checked/created.")

        # Dialogue Sessions Table
        cursor.execute("""
        CREATE TABLE IF NOT EXISTS dialogue_sessions (
            id TEXT PRIMARY KEY, 
            session_id TEXT UNIQUE NOT NULL,
            summary TEXT,
            timestamp REAL,
            metadata_json TEXT
        )
        """)
        logger.debug("Table 'dialogue_sessions' checked/created.")

        # Dialogue Turns Table
        cursor.execute("""
        CREATE TABLE IF NOT EXISTS dialogue_turns (
            turn_id TEXT PRIMARY KEY,
            session_db_id TEXT NOT NULL, 
            speaker TEXT NOT NULL,
            text TEXT NOT NULL,
            timestamp REAL,
            metadata_json TEXT,
            turn_order INTEGER, 
            FOREIGN KEY (session_db_id) REFERENCES dialogue_sessions(id) ON DELETE CASCADE
        )
        """)
        logger.debug("Table 'dialogue_turns' checked/created.")
        cursor.execute("CREATE INDEX IF NOT EXISTS idx_dialogue_turns_session_db_id ON dialogue_turns (session_db_id);")

        # Context Snapshots Table
        cursor.execute("""
        CREATE TABLE IF NOT EXISTS context_snapshots (
            id TEXT PRIMARY KEY,
            triggering_query TEXT,
            content_json TEXT, 
            llm_response TEXT,
            timestamp REAL,
            metadata_json TEXT
        )
        """)
        logger.debug("Table 'context_snapshots' checked/created.")

        # Optional: A generic memory_items table if you want to store all item types generically
        # For now, specific tables are clearer based on distinct dataclasses.

    def _load_memory_from_db(self):
        """Loads memory items from DB into in-memory cache on startup."""
        if not self.db_conn:
            return

        logger.info("Loading initial memory items from database...")
        loaded_count = 0
        # Load User Preferences
        try:
            cursor = self.db_conn.cursor()
            cursor.execute("SELECT id, key, content_json, timestamp, metadata_json FROM user_preferences")
            for row in cursor.fetchall():
                try:
                    pref = UserPreference(
                        id=row['id'], 
                        key=row['key'], 
                        content=json.loads(row['content_json']) if row['content_json'] else None,
                        timestamp=row['timestamp'],
                        metadata=json.loads(row['metadata_json']) if row['metadata_json'] else {}
                    )
                    # Add to in-memory cache, typically self._memory_items
                    # This ensures that items loaded from DB are also in the working cache.
                    self._memory_items[pref.id] = pref
                    loaded_count +=1
                except (json.JSONDecodeError, ValueError, TypeError) as e: # Catch errors during object creation
                    logger.error(f"Error reconstructing UserPreference from DB row ID {row['id']} (key: {row['key']}): {e}")
            if loaded_count > 0:
                logger.info(f"Loaded {loaded_count} user preferences from DB into cache.")
        except sqlite3.Error as e:
            logger.error(f"Error loading user preferences from DB: {e}", exc_info=True)
        
        # Load Behavioral Patterns
        loaded_bp_count = 0
        try:
            cursor = self.db_conn.cursor()
            cursor.execute("SELECT id, pattern_type, content_json, confidence, timestamp, metadata_json FROM behavioral_patterns")
            for row in cursor.fetchall():
                try:
                    pattern = BehavioralPattern(
                        id=row['id'],
                        pattern_type=row['pattern_type'],
                        content=json.loads(row['content_json']) if row['content_json'] else None,
                        confidence=row['confidence'],
                        timestamp=row['timestamp'],
                        metadata=json.loads(row['metadata_json']) if row['metadata_json'] else {}
                    )
                    self._memory_items[pattern.id] = pattern # Add to cache
                    loaded_bp_count += 1
                except (json.JSONDecodeError, ValueError, TypeError) as e:
                    logger.error(f"Error reconstructing BehavioralPattern from DB row ID {row['id']}: {e}")
            if loaded_bp_count > 0:
                logger.info(f"Loaded {loaded_bp_count} behavioral patterns from DB into cache.")
        except sqlite3.Error as e:
            logger.error(f"Error loading behavioral patterns from DB: {e}", exc_info=True)

        # Load Dialogue History (Sessions and Turns)
        loaded_dh_count = 0
        if self.db_conn: # Ensure db_conn is checked before use
            try:
                cursor = self.db_conn.cursor()
                # First, load all dialogue sessions
                cursor.execute("SELECT id, session_id, summary, timestamp, metadata_json FROM dialogue_sessions")
                sessions_data = cursor.fetchall()
                for session_row in sessions_data:
                    try:
                        # Fetch turns for this session
                        turns_cursor = self.db_conn.cursor()
                        turns_cursor.execute("SELECT turn_id, speaker, text, timestamp, metadata_json, turn_order FROM dialogue_turns WHERE session_db_id = ? ORDER BY turn_order ASC", (session_row['id'],))
                        dialogue_turns: List[DialogueTurn] = []
                        for turn_row in turns_cursor.fetchall():
                            turn_metadata = json.loads(turn_row['metadata_json']) if turn_row['metadata_json'] else {}
                            # The DialogueTurn dataclass does not have turn_id or turn_order as direct fields.
                            # We can store them in metadata if needed, or adjust DialogueTurn if it's only for DB.
                            # For now, let's assume they are not part of the reconstructed DialogueTurn object directly, unless put in metadata.
                            dt = DialogueTurn(
                                speaker=turn_row['speaker'],
                                text=turn_row['text'],
                                timestamp=turn_row['timestamp'],
                                metadata=turn_metadata
                            )
                            dialogue_turns.append(dt)
                        
                        session_metadata = json.loads(session_row['metadata_json']) if session_row['metadata_json'] else {}
                        dh = DialogueHistory(
                            id=session_row['id'],
                            session_id=session_row['session_id'],
                            turns=dialogue_turns,
                            summary=session_row['summary'],
                            timestamp=session_row['timestamp'],
                            metadata=session_metadata
                        )
                        self._memory_items[dh.id] = dh # Cache the reconstructed DialogueHistory
                        loaded_dh_count += 1
                    except (json.JSONDecodeError, ValueError, TypeError) as e:
                        logger.error(f"Error reconstructing DialogueHistory from DB for session_id {session_row['session_id']}: {e}")
                if loaded_dh_count > 0:
                    logger.info(f"Loaded {loaded_dh_count} dialogue histories (sessions with turns) from DB into cache.")
            except sqlite3.Error as e:
                logger.error(f"Error loading dialogue histories from DB: {e}", exc_info=True)

        # Load Context Snapshots
        loaded_cs_count = 0
        if self.db_conn:
            try:
                cursor = self.db_conn.cursor()
                cursor.execute("SELECT id, triggering_query, content_json, llm_response, timestamp, metadata_json FROM context_snapshots")
                for row in cursor.fetchall():
                    try:
                        snapshot = ContextSnapshot(
                            id=row['id'],
                            triggering_query=row['triggering_query'],
                            content=json.loads(row['content_json']) if row['content_json'] else None,
                            llm_response=row['llm_response'],
                            timestamp=row['timestamp'],
                            metadata=json.loads(row['metadata_json']) if row['metadata_json'] else {}
                        )
                        self._memory_items[snapshot.id] = snapshot # Add to cache
                        loaded_cs_count += 1
                    except (json.JSONDecodeError, ValueError, TypeError) as e:
                        logger.error(f"Error reconstructing ContextSnapshot from DB row ID {row['id']}: {e}")
                if loaded_cs_count > 0:
                    logger.info(f"Loaded {loaded_cs_count} context snapshots from DB into cache.")
            except sqlite3.Error as e:
                logger.error(f"Error loading context snapshots from DB: {e}", exc_info=True)

    async def add_memory_item(self, item: MemoryItem) -> bool:
        """
        Adds a new memory item to the store.
        Manages max elements constraint and persists to DB if applicable.
        """
        # In-memory cache management (optional, can be removed if DB is primary source)
        if len(self._memory_items) >= self.max_elements:
            # Pruning logic might need to be DB-aware if _memory_items is just a cache
            # For now, assume _memory_items is a cache that can be pruned independently.
            # await self.prune_memory(target_count=self.max_elements - 1) # Make space
            # Simplification: If DB is source of truth, cache eviction is different.
            # Let's assume for now the cache has its own pruning not directly tied to DB table limits.
            logger.warning(f"In-memory cache limit ({self.max_elements}) reached. Pruning not fully implemented with DB.")

        self._memory_items[item.id] = item # Keep in-memory cache for now
        logger.debug(f"Added/Updated memory item in cache: {item.id} (Type: {item.type.value}). Total cached items: {len(self._memory_items)}")
        
        # Persistence logic
        if self.db_conn:
            if item.type == MemoryItemType.USER_PREFERENCE and isinstance(item, UserPreference):
                try:
                    cursor = self.db_conn.cursor()
                    cursor.execute("""
                        INSERT INTO user_preferences (id, key, content_json, timestamp, metadata_json)
                        VALUES (?, ?, ?, ?, ?)
                        ON CONFLICT(key) DO UPDATE SET
                            id = excluded.id, 
                            content_json = excluded.content_json,
                            timestamp = excluded.timestamp,
                            metadata_json = excluded.metadata_json
                    """, (item.id, item.key, json.dumps(item.content), item.timestamp, json.dumps(item.metadata)))
                    self.db_conn.commit()
                    logger.debug(f"UserPreference '{item.key}' (ID: {item.id}) upserted into DB.")
                except sqlite3.Error as e:
                    logger.error(f"Error upserting UserPreference '{item.key}' to DB: {e}", exc_info=True)
                    return False
            elif item.type == MemoryItemType.BEHAVIORAL_PATTERN and isinstance(item, BehavioralPattern):
                try:
                    cursor = self.db_conn.cursor()
                    # Assuming BehavioralPattern.id is unique and primary key for its table.
                    # ON CONFLICT(id) DO UPDATE for general upsert behavior if items can be re-added/updated by id.
                    cursor.execute("""
                        INSERT INTO behavioral_patterns (id, pattern_type, content_json, confidence, timestamp, metadata_json)
                        VALUES (?, ?, ?, ?, ?, ?)
                        ON CONFLICT(id) DO UPDATE SET
                            pattern_type = excluded.pattern_type,
                            content_json = excluded.content_json,
                            confidence = excluded.confidence,
                            timestamp = excluded.timestamp,
                            metadata_json = excluded.metadata_json
                    """, (
                        item.id, 
                        item.pattern_type, 
                        json.dumps(item.content), 
                        item.confidence, 
                        item.timestamp, 
                        json.dumps(item.metadata)
                    ))
                    self.db_conn.commit()
                    logger.debug(f"BehavioralPattern '{item.pattern_type}' (ID: {item.id}) upserted into DB.")
                except sqlite3.Error as e:
                    logger.error(f"Error upserting BehavioralPattern '{item.pattern_type}' to DB: {e}", exc_info=True)
                    return False # Indicate failure
            elif item.type == MemoryItemType.CONTEXT_SNAPSHOT and isinstance(item, ContextSnapshot):
                try:
                    cursor = self.db_conn.cursor()
                    cursor.execute("""
                        INSERT INTO context_snapshots (id, triggering_query, content_json, llm_response, timestamp, metadata_json)
                        VALUES (?, ?, ?, ?, ?, ?)
                        ON CONFLICT(id) DO UPDATE SET
                            triggering_query = excluded.triggering_query,
                            content_json = excluded.content_json,
                            llm_response = excluded.llm_response,
                            timestamp = excluded.timestamp,
                            metadata_json = excluded.metadata_json
                    """, (
                        item.id, 
                        item.triggering_query,
                        json.dumps(item.content),
                        item.llm_response,
                        item.timestamp, 
                        json.dumps(item.metadata)
                    ))
                    self.db_conn.commit()
                    logger.debug(f"ContextSnapshot (ID: {item.id}) for query '{item.triggering_query[:50]}...' upserted into DB.")
                except sqlite3.Error as e:
                    logger.error(f"Error upserting ContextSnapshot (ID: {item.id}) to DB: {e}", exc_info=True)
                    return False # Indicate failure
            elif item.type == MemoryItemType.DIALOGUE_TURN and isinstance(item, DialogueHistory):
                # This branch is for updating an existing DialogueHistory session, e.g., after summarization.
                # add_dialogue_turn handles new turns and initial session creation/update.
                try:
                    cursor = self.db_conn.cursor()
                    cursor.execute("""
                        UPDATE dialogue_sessions 
                        SET summary = ?, timestamp = ?, metadata_json = ?
                        WHERE id = ?
                    """, (
                        item.summary,
                        item.timestamp,
                        json.dumps(item.metadata),
                        item.id
                    ))
                    if cursor.rowcount == 0:
                        logger.warning(f"Attempted to update DialogueHistory (ID: {item.id}) summary/metadata in DB, but no matching session found. It might be a new session not yet fully persisted by add_dialogue_turn, or an ID mismatch.")
                        # If it was meant to be a new session, it should have gone via add_dialogue_turn.
                        # For safety, we could try an INSERT here if rowcount is 0, but it might indicate a logic flaw elsewhere.
                        # For now, just log if no update happened.
                    else:
                        self.db_conn.commit()
                        logger.debug(f"DialogueHistory (ID: {item.id}) summary/metadata updated in DB.")
                except sqlite3.Error as e:
                    logger.error(f"Error updating DialogueHistory (ID: {item.id}) summary/metadata in DB: {e}", exc_info=True)
                    return False
            # TODO: Add similar persistence for other future MemoryItemTypes if any.
        return True

    async def get_memory_item(self, item_id: str) -> Optional[MemoryItem]:
        """Retrieves a specific memory item by ID. (Primarily from cache for now)"""
        cached_item = self._memory_items.get(item_id)
        if cached_item:
            return cached_item
        
        # Example: Fetch UserPreference from DB if not in cache
        if self.db_conn: # Check if db_conn is not None
            # Determine table and type based on item_id prefix or other conventions if possible
            # This is a simplified lookup. A more robust system might require type hint or separate getters.
            if item_id.startswith("pref_"):
                table_name = "user_preferences"
                ItemClass = UserPreference
                logger.debug(f"Attempting to fetch UserPreference ID '{item_id}' from DB.")
                try:
                    cursor = self.db_conn.cursor()
                    cursor.execute(f"SELECT * FROM {table_name} WHERE id = ?", (item_id,))
                    row = cursor.fetchone()
                    if row:
                        item = ItemClass(
                            id=row['id'], 
                            key=row['key'], 
                            content=json.loads(row['content_json']) if row['content_json'] else None,
                            timestamp=row['timestamp'],
                            metadata=json.loads(row['metadata_json']) if row['metadata_json'] else {}
                        )
                        self._memory_items[item.id] = item # Cache it
                        return item
                except sqlite3.Error as e:
                    logger.error(f"Error fetching UserPreference by id '{item_id}' from DB: {e}")
            elif item_id.startswith("pattern_"):
                # Similar logic for BehavioralPattern if needed by ID
                logger.debug(f"Attempting to fetch BehavioralPattern ID '{item_id}' from DB.")
                # ... implementation to fetch BehavioralPattern by ID ...
                pass # Placeholder
            elif item_id.startswith("dialogue_"):
                logger.debug(f"Attempting to fetch DialogueHistory ID '{item_id}' from DB.")
                try:
                    cursor = self.db_conn.cursor()
                    cursor.execute("SELECT id, session_id, summary, timestamp, metadata_json FROM dialogue_sessions WHERE id = ?", (item_id,))
                    session_row = cursor.fetchone()
                    if session_row:
                        # Found the session, now fetch its turns
                        turns_cursor = self.db_conn.cursor()
                        turns_query = "SELECT speaker, text, timestamp, metadata_json FROM dialogue_turns WHERE session_db_id = ? ORDER BY turn_order ASC"
                        turns_cursor.execute(turns_query, (session_row['id'],))
                        
                        dialogue_turns_from_db: List[DialogueTurn] = []
                        for turn_row in turns_cursor.fetchall():
                            dialogue_turns_from_db.append(DialogueTurn(
                                speaker=turn_row['speaker'], 
                                text=turn_row['text'], 
                                timestamp=turn_row['timestamp'], 
                                metadata=json.loads(turn_row['metadata_json']) if turn_row['metadata_json'] else {}
                            ))
                        
                        history_obj = DialogueHistory(
                            id=session_row['id'], 
                            session_id=session_row['session_id'], 
                            turns=dialogue_turns_from_db,
                            summary=session_row['summary'], 
                            timestamp=session_row['timestamp'],
                            metadata=json.loads(session_row['metadata_json']) if session_row['metadata_json'] else {}
                        )
                        self._memory_items[history_obj.id] = history_obj # Cache it
                        return history_obj
                except sqlite3.Error as e:
                    logger.error(f"Error fetching DialogueHistory by id '{item_id}' from DB: {e}")
            # Add logic for other types (ContextSnapshot) if get_memory_item by ID is needed for them

        return None # Not found in cache or DB for the types handled above

    async def get_memory_items(self, 
                               item_type: Optional[MemoryItemType] = None,
                               max_count: Optional[int] = None,
                               time_window_seconds: Optional[float] = None,
                               sort_by_time: bool = True # True for descending (most recent first)
                              ) -> List[MemoryItem]:
        """
        Retrieves memory items, with optional filtering and sorting.
        Prioritizes DB for UserPreference if available.
        """
        # Special handling for UserPreference to query DB directly if available
        if item_type == MemoryItemType.USER_PREFERENCE and self.db_conn:
            logger.debug(f"Fetching UserPreferences from DB with filters: window={time_window_seconds}s, max={max_count}, sort_desc={sort_by_time}")
            results: List[MemoryItem] = []
            try:
                query = "SELECT id, key, content_json, timestamp, metadata_json FROM user_preferences"
                params = []
                conditions = []

                if time_window_seconds:
                    min_timestamp = time.time() - time_window_seconds
                    conditions.append("timestamp >= ?")
                    params.append(min_timestamp)
                
                if conditions:
                    query += " WHERE " + " AND ".join(conditions)
                
                query += " ORDER BY timestamp " + ("DESC" if sort_by_time else "ASC")
                
                if max_count is not None:
                    query += " LIMIT ?"
                    params.append(max_count)

                cursor = self.db_conn.cursor()
                cursor.execute(query, tuple(params))
                
                for row in cursor.fetchall():
                    try:
                        pref = UserPreference(
                            id=row['id'], 
                            key=row['key'], 
                            content=json.loads(row['content_json']) if row['content_json'] else None,
                            timestamp=row['timestamp'],
                            metadata=json.loads(row['metadata_json']) if row['metadata_json'] else {}
                        )
                        results.append(pref)
                        # Update in-memory cache with items retrieved from DB
                        self._memory_items[pref.id] = pref 
                    except (json.JSONDecodeError, ValueError, TypeError) as e:
                        logger.error(f"Error reconstructing UserPreference from DB row during get_memory_items (ID: {row['id']}): {e}")
                logger.debug(f"Retrieved {len(results)} UserPreferences from DB.")
                return results
            except sqlite3.Error as e:
                logger.error(f"DB error fetching UserPreferences in get_memory_items: {e}", exc_info=True)
                # Fall through to in-memory cache if DB query fails
        
        elif item_type == MemoryItemType.BEHAVIORAL_PATTERN and self.db_conn:
            logger.debug(f"Fetching BehavioralPatterns from DB with filters: window={time_window_seconds}s, max={max_count}, sort_desc={sort_by_time}")
            results: List[MemoryItem] = []
            try:
                query = "SELECT id, pattern_type, content_json, confidence, timestamp, metadata_json FROM behavioral_patterns"
                params = []
                conditions = []

                if time_window_seconds:
                    min_timestamp = time.time() - time_window_seconds
                    conditions.append("timestamp >= ?")
                    params.append(min_timestamp)
                
                if conditions:
                    query += " WHERE " + " AND ".join(conditions)
                
                query += " ORDER BY timestamp " + ("DESC" if sort_by_time else "ASC")
                
                if max_count is not None:
                    query += " LIMIT ?"
                    params.append(max_count)

                cursor = self.db_conn.cursor()
                cursor.execute(query, tuple(params))
                
                for row in cursor.fetchall():
                    try:
                        pattern = BehavioralPattern(
                            id=row['id'],
                            pattern_type=row['pattern_type'],
                            content=json.loads(row['content_json']) if row['content_json'] else None,
                            confidence=row['confidence'],
                            timestamp=row['timestamp'],
                            metadata=json.loads(row['metadata_json']) if row['metadata_json'] else {}
                        )
                        results.append(pattern)
                        self._memory_items[pattern.id] = pattern # Update cache
                    except (json.JSONDecodeError, ValueError, TypeError) as e:
                        logger.error(f"Error reconstructing BehavioralPattern from DB row during get_memory_items (ID: {row['id']}): {e}")
                logger.debug(f"Retrieved {len(results)} BehavioralPatterns from DB.")
                return results
            except sqlite3.Error as e:
                logger.error(f"DB error fetching BehavioralPatterns in get_memory_items: {e}", exc_info=True)
                # Fall through to in-memory cache if DB query fails

        elif item_type == MemoryItemType.DIALOGUE_TURN and self.db_conn: # Assuming this means get DialogueHistory objects
            logger.debug(f"Fetching DialogueHistory sessions from DB with filters: window={time_window_seconds}s, max={max_count}, sort_desc={sort_by_time}")
            results: List[MemoryItem] = [] # Will store DialogueHistory objects
            try:
                query = "SELECT id, session_id FROM dialogue_sessions" # Select session_id to pass to get_dialogue_history
                params = []
                conditions = []

                if time_window_seconds:
                    min_timestamp = time.time() - time_window_seconds
                    conditions.append("timestamp >= ?")
                    params.append(min_timestamp)
                
                if conditions:
                    query += " WHERE " + " AND ".join(conditions)
                
                query += " ORDER BY timestamp " + ("DESC" if sort_by_time else "ASC")
                
                if max_count is not None:
                    query += " LIMIT ?"
                    params.append(max_count)

                cursor = self.db_conn.cursor()
                cursor.execute(query, tuple(params))
                
                session_rows = cursor.fetchall()
                for session_row in session_rows:
                    session_id_from_db = session_row['session_id']
                    # Fetch the full DialogueHistory object using the existing method
                    # This reuses its logic for fetching turns and caching.
                    # max_turns for get_dialogue_history itself is not specified here, so it fetches all turns for the session.
                    # The max_count in this method (get_memory_items) applies to the number of sessions.
                    history_obj = await self.get_dialogue_history(session_id_from_db, max_turns=None) 
                    if history_obj:
                        results.append(history_obj)
                        # self._memory_items[history_obj.id] = history_obj # get_dialogue_history already caches it
                
                logger.debug(f"Retrieved {len(results)} DialogueHistory sessions from DB query.")
                return results # Returns List[DialogueHistory] which are MemoryItem compatible
            except sqlite3.Error as e:
                logger.error(f"DB error fetching DialogueHistory session IDs in get_memory_items: {e}", exc_info=True)
                # Fall through to in-memory cache if DB query fails

        elif item_type == MemoryItemType.CONTEXT_SNAPSHOT and self.db_conn:
            logger.debug(f"Fetching ContextSnapshots from DB with filters: window={time_window_seconds}s, max={max_count}, sort_desc={sort_by_time}")
            results: List[MemoryItem] = []
            try:
                query = "SELECT id, triggering_query, content_json, llm_response, timestamp, metadata_json FROM context_snapshots"
                params = []
                conditions = []

                if time_window_seconds:
                    min_timestamp = time.time() - time_window_seconds
                    conditions.append("timestamp >= ?")
                    params.append(min_timestamp)
                
                if conditions:
                    query += " WHERE " + " AND ".join(conditions)
                
                query += " ORDER BY timestamp " + ("DESC" if sort_by_time else "ASC")
                
                if max_count is not None:
                    query += " LIMIT ?"
                    params.append(max_count)

                cursor = self.db_conn.cursor()
                cursor.execute(query, tuple(params))
                
                for row in cursor.fetchall():
                    try:
                        snapshot = ContextSnapshot(
                            id=row['id'],
                            triggering_query=row['triggering_query'],
                            content=json.loads(row['content_json']) if row['content_json'] else None,
                            llm_response=row['llm_response'],
                            timestamp=row['timestamp'],
                            metadata=json.loads(row['metadata_json']) if row['metadata_json'] else {}
                        )
                        results.append(snapshot)
                        self._memory_items[snapshot.id] = snapshot # Update cache
                    except (json.JSONDecodeError, ValueError, TypeError) as e:
                        logger.error(f"Error reconstructing ContextSnapshot from DB row during get_memory_items (ID: {row['id']}): {e}")
                logger.debug(f"Retrieved {len(results)} ContextSnapshots from DB.")
                return results
            except sqlite3.Error as e:
                logger.error(f"DB error fetching ContextSnapshots in get_memory_items: {e}", exc_info=True)
                # Fall through to in-memory cache if DB query fails

        # Fallback to in-memory cache for other types or if DB fails
        logger.debug(f"Fetching memory items from in-memory cache (type: {item_type}, window: {time_window_seconds}s, max: {max_count}).")
        candidate_items = list(self._memory_items.values())
        
        if item_type:
            candidate_items = [item for item in candidate_items if item.type == item_type]
            
        if time_window_seconds:
            current_time = time.time()
            candidate_items = [item for item in candidate_items if (current_time - item.timestamp) <= time_window_seconds]
            
        if sort_by_time:
            candidate_items.sort(key=lambda item: item.timestamp, reverse=True)
            
        if max_count is not None:
            candidate_items = candidate_items[:max_count]
            
        logger.debug(f"Retrieved {len(candidate_items)} memory items (type: {item_type}, window: {time_window_seconds}s, max: {max_count}).")
        return candidate_items

    async def update_user_preference(self, key: str, value: Any) -> UserPreference:
        """Creates or updates a user preference and persists it to the database."""
        timestamp = time.time()
        pref_id = f"pref_{key}_{timestamp}" # Generate a potentially new ID
        
        # Try to fetch existing from DB to see if it's an update or new
        # This also serves to get the most current ID if key exists
        if self.db_conn:
            try:
                cursor = self.db_conn.cursor()
                cursor.execute("SELECT id, content_json, metadata_json, timestamp FROM user_preferences WHERE key = ?", (key,))
                row = cursor.fetchone()
                if row:
                    logger.info(f"Updating existing preference '{key}' in DB.")
                    # Use existing ID if found, update content and timestamp
                    pref_id = row['id'] 
                    # Create a UserPreference object for consistency, then it will be added via add_memory_item
                    updated_pref = UserPreference(
                        id=pref_id, 
                        key=key, 
                        content=value, 
                        timestamp=timestamp, # New timestamp for update
                        metadata=json.loads(row['metadata_json']) if row['metadata_json'] else {} # Preserve old metadata or update?
                                                                                             # For now, let's assume metadata is not changed by this simple update value func
                    )
                else:
                    logger.info(f"Creating new preference '{key}' in DB.")
                    updated_pref = UserPreference(id=pref_id, key=key, content=value, timestamp=timestamp)
                
                # Let add_memory_item handle the actual upsert and caching
                await self.add_memory_item(updated_pref)
                return updated_pref # Return the object that was (or would be) upserted

            except sqlite3.Error as e:
                logger.error(f"DB error during update_user_preference for key '{key}': {e}", exc_info=True)
                # Fallback to in-memory only if DB fails, or re-raise?
                # For now, create object and it won't be persisted if add_memory_item fails on DB part.
                # This behavior might need refinement.
                # Create an in-memory version, it won't be persisted if db_conn is None or add_memory_item fails db part
                temp_pref_for_failure = UserPreference(id=pref_id, key=key, content=value, timestamp=timestamp)
                # Manually add to cache if DB op failed before add_memory_item call
                self._memory_items[temp_pref_for_failure.id] = temp_pref_for_failure
                return temp_pref_for_failure
        else:
            # No DB connection, purely in-memory behavior (similar to original but using add_memory_item for cache)
            logger.info(f"No DB connection. Handling preference '{key}' in-memory.")
            # Check in-memory cache for existing preference by key
            existing_cached_pref: Optional[UserPreference] = None
            for item_id, item_obj in self._memory_items.items():
                if isinstance(item_obj, UserPreference) and item_obj.key == key:
                    existing_cached_pref = item_obj
                    break
            
            if existing_cached_pref:
                existing_cached_pref.content = value
                existing_cached_pref.timestamp = timestamp
                # add_memory_item will update the cache again, which is fine.
                await self.add_memory_item(existing_cached_pref) # Ensures it's in cache via a single path
                return existing_cached_pref
            else:
                new_pref = UserPreference(id=pref_id, key=key, content=value, timestamp=timestamp)
                await self.add_memory_item(new_pref)
                return new_pref

    async def get_user_preference(self, key: str, default: Optional[Any] = None) -> Optional[Any]:
        """Retrieves the latest value for a user preference key from DB or cache."""
        # Try cache first for speed
        for item_id, item_obj in reversed(list(self._memory_items.items())): # Check recent cache items first
            if isinstance(item_obj, UserPreference) and item_obj.key == key:
                logger.debug(f"Retrieved preference '{key}' from cache.")
                return item_obj.content

        if self.db_conn:
            try:
                cursor = self.db_conn.cursor()
                cursor.execute("SELECT id, content_json, metadata_json, timestamp FROM user_preferences WHERE key = ? ORDER BY timestamp DESC LIMIT 1", (key,))
                row = cursor.fetchone()
                if row:
                    logger.debug(f"Retrieved preference '{key}' from DB.")
                    # Create a UserPreference object to cache it and return content
                    pref = UserPreference(
                        id=row['id'], 
                        key=key, 
                        content=json.loads(row['content_json']) if row['content_json'] else None,
                        timestamp=row['timestamp'],
                        metadata=json.loads(row['metadata_json']) if row['metadata_json'] else {}
                    )
                    self._memory_items[pref.id] = pref # Update cache
                    return pref.content
            except sqlite3.Error as e:
                logger.error(f"DB error during get_user_preference for key '{key}': {e}", exc_info=True)
        
        logger.debug(f"Preference '{key}' not found in cache or DB. Returning default.")
        return default

    async def infer_behavioral_pattern(self, pattern_type: str, data: Any, confidence: float = 0.5) -> BehavioralPattern:
        """
        Infers and stores a behavioral pattern.
        This is a placeholder; real inference would be more complex.
        """
        pattern = BehavioralPattern(
            pattern_type=pattern_type, 
            content=data, 
            confidence=confidence,
            id=f"pattern_{pattern_type}_{time.time()}" # Ensure ID is unique
        )
        await self.add_memory_item(pattern)
        logger.info(f"Inferred behavioral pattern: {pattern_type} with data {data} (Confidence: {confidence})")
        return pattern

    async def add_dialogue_turn(self, session_id: str, speaker: str, text: str, metadata: Optional[Dict]=None) -> DialogueHistory:
        """Adds a dialogue turn to a session, creating or updating it in memory and DB."""
        if metadata is None:
            metadata = {}
        
        current_time = time.time()
        new_turn = DialogueTurn(speaker=speaker, text=text, timestamp=current_time, metadata=metadata)

        session_history: Optional[DialogueHistory] = None

        # 1. Try to find existing DialogueHistory in cache by session_id
        cached_histories = [item for item in self._memory_items.values() if isinstance(item, DialogueHistory) and item.session_id == session_id]
        if cached_histories:
            cached_histories.sort(key=lambda h: h.timestamp, reverse=True)
            session_history = cached_histories[0]
            logger.debug(f"Found DialogueHistory for session '{session_id}' in cache (ID: {session_history.id}).")
        
        # 2. If not in cache, try to find in DB by session_id
        if not session_history and self.db_conn:
            try:
                cursor = self.db_conn.cursor()
                cursor.execute("SELECT id, summary, timestamp, metadata_json FROM dialogue_sessions WHERE session_id = ? ORDER BY timestamp DESC LIMIT 1", (session_id,))
                row = cursor.fetchone()
                if row:
                    logger.debug(f"Found DialogueHistory for session '{session_id}' in DB (DB ID: {row['id']}). Reconstructing.")
                    # Reconstruct session. For turns, we might load them all or defer to get_dialogue_history.
                    # For add_dialogue_turn, we primarily need the session object to append to.
                    # Let's load recent turns to make the cached object somewhat useful.
                    turns_cursor = self.db_conn.cursor()
                    turns_cursor.execute("SELECT speaker, text, timestamp, metadata_json FROM dialogue_turns WHERE session_db_id = ? ORDER BY turn_order DESC LIMIT 20", (row['id'],)) # Load recent 20 for cache
                    loaded_turns: List[DialogueTurn] = []
                    for turn_row in reversed(turns_cursor.fetchall()): # Reverse to maintain chronological order
                        loaded_turns.append(DialogueTurn(
                            speaker=turn_row['speaker'], text=turn_row['text'], timestamp=turn_row['timestamp'], 
                            metadata=json.loads(turn_row['metadata_json']) if turn_row['metadata_json'] else {}
                        ))
                    session_history = DialogueHistory(
                        id=row['id'], session_id=session_id, turns=loaded_turns, 
                        summary=row['summary'], timestamp=row['timestamp'], 
                        metadata=json.loads(row['metadata_json']) if row['metadata_json'] else {}
                    )
                    self._memory_items[session_history.id] = session_history # Update cache
            except sqlite3.Error as e:
                logger.error(f"DB error looking up session '{session_id}': {e}", exc_info=True)

        # 3. Append turn and update/create session object
        if session_history:
            session_history.turns.append(new_turn)
            session_history.timestamp = current_time # Update session timestamp for recent activity
            self._memory_items[session_history.id] = session_history # Re-cache if updated
            logger.info(f"Appended turn to DialogueHistory for session '{session_id}'. Cache ID: {session_history.id}. New turn count: {len(session_history.turns)}")
        else:
            # Truly new session (not in cache, not in DB)
            session_history_id = f"dialogue_{session_id}_{current_time}"
            session_history = DialogueHistory(id=session_history_id, session_id=session_id, turns=[new_turn], timestamp=current_time)
            self._memory_items[session_history.id] = session_history # Add to cache
            logger.info(f"Created new DialogueHistory for session '{session_id}'. Cache ID: {session_history.id}")

        # 4. Persist session and turn to DB
        if self.db_conn and session_history: 
            try:
                cursor = self.db_conn.cursor()
                # Upsert DialogueSession. Use session_history.id as the definitive ID.
                # ON CONFLICT on session_id handles cases where another instance might have created it with a different primary ID.
                cursor.execute("""
                    INSERT INTO dialogue_sessions (id, session_id, summary, timestamp, metadata_json)
                    VALUES (?, ?, ?, ?, ?)
                    ON CONFLICT(session_id) DO UPDATE SET
                        id = excluded.id, -- Prefer the ID from the current session_history object if conflict on session_id
                        summary = excluded.summary, 
                        timestamp = excluded.timestamp, 
                        metadata_json = excluded.metadata_json
                    WHERE excluded.timestamp > timestamp; -- Only update if new data is fresher for the same session_id
                    -- If the primary key (id) conflicts, it implies we are updating an existing known session by its definitive ID.
                    -- No, the above ON CONFLICT(session_id) needs to be primary. If ID matches, it's an update. If session_id matches but ID is different, we update the existing row.
                    -- Let's simplify: Upsert on ID. If new session ID, it will insert. If existing ID, it will update.
                    -- The lookup logic above should ensure we get the correct session_history.id if session_id exists.
                """)
                # Revised UPSERT for dialogue_sessions to be safer:
                # First, try to insert with the specific ID. If that session_id already exists with a *different* ID, this could fail
                # or create issues if session_id is unique. The previous lookup should handle this.
                # The most robust way is to ensure session_id is the key for finding existing, then use its known DB ID.
                cursor.execute("""
                    INSERT INTO dialogue_sessions (id, session_id, summary, timestamp, metadata_json)
                    VALUES (?, ?, ?, ?, ?)
                    ON CONFLICT(id) DO UPDATE SET
                        summary = excluded.summary, 
                        timestamp = excluded.timestamp, 
                        metadata_json = excluded.metadata_json
                """, (
                    session_history.id, 
                    session_history.session_id, 
                    session_history.summary, 
                    session_history.timestamp, 
                    json.dumps(session_history.metadata)
                ))
                
                turn_db_id = f"turn_{session_history.id}_{new_turn.timestamp}_{speaker[:10].replace(" ", "_")}" 
                turn_order = len(session_history.turns) 

                cursor.execute("""
                    INSERT INTO dialogue_turns (turn_id, session_db_id, speaker, text, timestamp, metadata_json, turn_order)
                    VALUES (?, ?, ?, ?, ?, ?, ?)
                    ON CONFLICT(turn_id) DO UPDATE SET -- In case a turn with exact same generated ID exists (unlikely but good practice)
                        speaker=excluded.speaker, text=excluded.text, timestamp=excluded.timestamp, metadata_json=excluded.metadata_json, turn_order=excluded.turn_order
                """, (
                    turn_db_id, session_history.id, new_turn.speaker, new_turn.text, 
                    new_turn.timestamp, json.dumps(new_turn.metadata), turn_order
                ))
                self.db_conn.commit()
                logger.debug(f"Persisted dialogue session '{session_history.session_id}' (ID: {session_history.id}) and turn (DB ID: {turn_db_id}) to DB.")
            except sqlite3.Error as e:
                logger.error(f"Error persisting dialogue turn for session '{session_id}' to DB: {e}", exc_info=True)
        
        return session_history

    async def get_dialogue_history(self, session_id: str, max_turns: Optional[int] = None) -> Optional[DialogueHistory]:
        """Retrieves a specific dialogue history by session_id, prioritizing DB."""
        logger.debug(f"Attempting to retrieve dialogue history for session_id: {session_id}")

        # Try in-memory cache first, but be mindful it might be partially loaded by add_dialogue_turn
        # If a full load is needed and DB exists, DB should be the source of truth.
        cached_history: Optional[DialogueHistory] = None
        for item_id, item_obj in self._memory_items.items():
            if isinstance(item_obj, DialogueHistory) and item_obj.session_id == session_id:
                # If max_turns is not specified, and cached item seems complete enough, consider returning it.
                # However, for definitive full history, DB is better.
                # Let's assume if we have it in cache, it was placed there by a previous full load or add_dialogue_turn.
                # If max_turns is specified, we might need to re-fetch or filter turns from DB anyway.
                cached_history = item_obj
                if max_turns is None or len(cached_history.turns) >= max_turns: # Simple check
                    logger.debug(f"Found potentially suitable DialogueHistory for session '{session_id}' in cache.")
                    # Return a copy with potentially sliced turns if max_turns is set
                    if max_turns is not None:
                        # Ensure turns are sorted by their timestamp or inherent order if not already
                        # DialogueTurn objects from cache are already ordered as they were appended.
                        final_turns = cached_history.turns[-max_turns:] # Get the last N turns
                        # Create a new DialogueHistory instance to avoid modifying the cached one directly if turns are sliced
                        return DialogueHistory(
                            id=cached_history.id, session_id=cached_history.session_id, turns=final_turns,
                            summary=cached_history.summary, timestamp=cached_history.timestamp, metadata=cached_history.metadata
                        )
                    return cached_history # Return the cached one as is
                break # Found a candidate, but may need DB for specific max_turns

        if self.db_conn:
            try:
                cursor = self.db_conn.cursor()
                cursor.execute("SELECT id, summary, timestamp, metadata_json FROM dialogue_sessions WHERE session_id = ? ORDER BY timestamp DESC LIMIT 1", (session_id,))
                session_row = cursor.fetchone()

                if session_row:
                    session_db_id = session_row['id']
                    logger.debug(f"Found dialogue session in DB (ID: {session_db_id}) for session_id: {session_id}. Fetching turns.")
                    
                    turns_query = "SELECT speaker, text, timestamp, metadata_json, turn_order FROM dialogue_turns WHERE session_db_id = ? ORDER BY turn_order ASC"
                    turn_params = [session_db_id]
                    
                    # If max_turns is specified, we adjust the query to get the latest N turns.
                    # SQLite doesn't directly support LIMIT on a subquery sorted differently easily.
                    # So, we fetch all, then slice, or fetch N turns sorted descending then reverse in Python.
                    # Let's fetch N turns ordered DESC then reverse in Python for simplicity if max_turns is given.
                    if max_turns is not None:
                        turns_query = "SELECT speaker, text, timestamp, metadata_json, turn_order FROM dialogue_turns WHERE session_db_id = ? ORDER BY turn_order DESC LIMIT ?"
                        turn_params.extend([max_turns])
                    
                    turns_cursor = self.db_conn.cursor()
                    turns_cursor.execute(turns_query, tuple(turn_params))
                    
                    dialogue_turns_from_db: List[DialogueTurn] = []
                    db_rows = turns_cursor.fetchall()
                    if max_turns is not None: # If we fetched DESC, reverse to get chronological for DialogueHistory
                        db_rows.reverse()

                    for turn_row in db_rows:
                        dialogue_turns_from_db.append(DialogueTurn(
                            speaker=turn_row['speaker'], 
                            text=turn_row['text'], 
                            timestamp=turn_row['timestamp'], 
                            metadata=json.loads(turn_row['metadata_json']) if turn_row['metadata_json'] else {}
                        ))
                    
                    reconstructed_history = DialogueHistory(
                        id=session_db_id, 
                        session_id=session_id, 
                        turns=dialogue_turns_from_db,
                        summary=session_row['summary'], 
                        timestamp=session_row['timestamp'],
                        metadata=json.loads(session_row['metadata_json']) if session_row['metadata_json'] else {}
                    )
                    self._memory_items[reconstructed_history.id] = reconstructed_history # Update cache with fully loaded history
                    logger.info(f"Reconstructed DialogueHistory for session '{session_id}' from DB with {len(dialogue_turns_from_db)} turns.")
                    return reconstructed_history
                else:
                    logger.debug(f"Dialogue session_id '{session_id}' not found in DB.")
                    return None # Not found in DB
            except sqlite3.Error as e:
                logger.error(f"DB error fetching dialogue history for session_id '{session_id}': {e}", exc_info=True)
                # If DB error, and we had a cached_history, maybe return that? Or None?
                # For now, if DB fails, we don't fall back to a potentially stale/partial cache entry if full load was intended.
                return cached_history # Fallback to (potentially partial) cache if DB fails after cache check
        else:
            # No DB, rely on cache if it was found earlier and met criteria
            if cached_history: # This means it passed the initial cache check (e.g. max_turns satisfied)
                logger.debug(f"Returning DialogueHistory for session '{session_id}' from cache (no DB connection).")
                return cached_history

        logger.debug(f"DialogueHistory for session '{session_id}' not found.")
        return None

    async def summarize_dialogue_history(self, dialogue_history_id: str) -> bool:
        """
        Summarizes a dialogue history using the LLM. (Placeholder)
        Requires self.llm_client to be set.
        """
        if not self.llm_client:
            logger.warning("LLM client not available for summarizing dialogue history.")
            return False
            
        item = await self.get_memory_item(dialogue_history_id)
        if not isinstance(item, DialogueHistory):
            logger.warning(f"Dialogue history {dialogue_history_id} not found or not a DialogueHistory item.")
            return False

        if not item.turns:
            logger.info(f"No turns to summarize for dialogue {dialogue_history_id}.")
            item.summary = "Empty dialogue."
            return True

        # Construct prompt for summarization
        dialogue_text = "\n".join([f"{turn.speaker}: {turn.text}" for turn in item.turns])
        prompt = f"""
        Summarize the following dialogue concisely. Capture the main topics and outcomes.
        If the dialogue is very short, a brief phrase is sufficient.

        Dialogue:
        ---
        {dialogue_text}
        ---

        Summary:
        """
        try:
            summary_text = await self.llm_client.generate(prompt=prompt)
            item.summary = summary_text
            item.timestamp = time.time() # Update timestamp as it's modified
            logger.info(f"Summarized dialogue {dialogue_history_id}. Summary: {summary_text[:100]}...")
            # TODO: Persist updated item if using storage_service
            return True
        except Exception as e:
            logger.error(f"Error summarizing dialogue {dialogue_history_id}: {e}")
            return False

    async def record_context_snapshot(self, triggering_query: str, context_content: Dict[str, Any], llm_response: Optional[str]=None) -> ContextSnapshot:
        """Records a snapshot of the context provided to the LLM."""
        snapshot = ContextSnapshot(
            triggering_query=triggering_query, 
            content=context_content,
            llm_response=llm_response,
            id=f"snap_{time.time()}" # Ensure unique ID
        )
        await self.add_memory_item(snapshot)
        logger.debug(f"Recorded context snapshot for query: {triggering_query[:50]}")
        return snapshot

    async def build_context_for_llm(self, 
                                    session_id: Optional[str] = None, 
                                    user_query: Optional[str] = None,
                                    max_dialogue_turns: int = 5,
                                    include_preferences_keys: Optional[List[str]] = None
                                   ) -> Dict[str, Any]:
        """
        Builds a context dictionary to be used for LLM prompts.
        Combines relevant dialogue history, user preferences, behavioral patterns.

        This is a simplified version. Token budget allocation as per 10.8.F needs careful implementation.
        """
        context = {}
        
        # 1. Dialogue History (if session_id provided)
        if session_id:
            history = await self.get_dialogue_history(session_id, max_turns=max_dialogue_turns)
            if history:
                context['dialogue_history'] = {
                    'summary': history.summary,
                    'recent_turns': [{'speaker': t.speaker, 'text': t.text} for t in history.turns]
                }
        
        # 2. User Preferences (selectively, based on keys or general config)
        prefs_to_include = {}
        default_pref_keys = getattr(self.config, 'memory_context_default_pref_keys', ['llm_model', 'rag_strategy'])
        keys_to_fetch = include_preferences_keys if include_preferences_keys is not None else default_pref_keys
        
        for key in keys_to_fetch:
            value = await self.get_user_preference(key)
            if value is not None:
                prefs_to_include[key] = value
        if prefs_to_include:
            context['user_preferences'] = prefs_to_include
            
        # 3. Behavioral Patterns (e.g., most recent relevant ones)
        # This is highly dependent on what patterns are stored and how they are deemed relevant.
        # For now, let's fetch a few recent generic ones.
        # patterns = await self.get_memory_items(item_type=MemoryItemType.BEHAVIORAL_PATTERN, max_count=2, sort_by_time=True)
        # if patterns:
        #     context['behavioral_patterns'] = [{'type': p.pattern_type, 'details': p.content, 'confidence': p.metadata.get('confidence')} for p in patterns if isinstance(p, BehavioralPattern)]

        # 4. Current Query (if provided)
        if user_query:
            context['current_query'] = user_query

        # TODO: Implement token budget allocation (section 10.8.F)
        # This would involve:
        # - Calculating token counts for each context component.
        # - Prioritizing components based on config (e.g., user_prefs 15%, current_content 30%, graph_info 20%, etc.).
        # - Truncating or summarizing components to fit the budget.
        # For now, this method just gathers potential context elements. The RAG engine or LLM caller
        # would be responsible for final prompt construction and fitting it to token limits.

        logger.debug(f"Built context: { {k: type(v) for k,v in context.items()} }")
        return context

    async def prune_memory(self, force_prune_all: bool = False, target_count: Optional[int] = None) -> int:
        """
        Prunes old memory items from the database based on retention period.
        Also, ensures the in-memory cache reflects these changes.

        Args:
            force_prune_all: If True, ignores retention and attempts to clear (not implemented yet for DB).
            target_count: If specified, tries to prune towards this count (primarily for cache, complex for DB).

        Returns:
            The number of items effectively pruned from the DB (approximate).
        """
        if force_prune_all:
            logger.warning("force_prune_all=True is not fully implemented for DB backend. Will only prune by retention.")
            # If we wanted to clear tables: self._memory_items.clear(); cursor.execute("DELETE FROM table") etc.

        if not self.db_conn:
            logger.info("No DB connection, skipping DB pruning. In-memory cache pruning (if any) would happen here.")
            # Original in-memory pruning logic would go here if self._memory_items was the sole source of truth
            # and max_elements was to be enforced on it directly without a DB.
            # For now, this method primarily focuses on DB pruning when db_conn is available.
            return 0

        pruned_count_total = 0
        cutoff_timestamp = time.time() - self.retention_period
        logger.info(f"Pruning memory items older than timestamp: {cutoff_timestamp} (Retention: {self.retention_period/86400:.2f} days)")

        tables_to_prune_by_timestamp = [
            "user_preferences",
            "behavioral_patterns",
            "context_snapshots",
            "dialogue_sessions" # Deleting sessions will cascade to dialogue_turns
        ]

        try:
            cursor = self.db_conn.cursor()
            for table_name in tables_to_prune_by_timestamp:
                # Get count before deleting for logging
                # Using a direct execute for count first might be slightly less efficient but good for logging.
                # Alternatively, check cursor.rowcount after DELETE if the DB driver supports it reliably for DELETE.
                # For SQLite, cursor.rowcount after DELETE should work.
                
                # Let's get IDs to be deleted for more accurate cache invalidation later, if we don't full reload
                # For now, we do full reload, so direct DELETE is fine.
                
                initial_rowcount_query = f"SELECT COUNT(*) FROM {table_name} WHERE timestamp < ?"
                cursor.execute(initial_rowcount_query, (cutoff_timestamp,))
                num_to_delete = cursor.fetchone()[0]

                if num_to_delete > 0:
                    delete_query = f"DELETE FROM {table_name} WHERE timestamp < ?"
                    cursor.execute(delete_query, (cutoff_timestamp,))
                    deleted_this_table = cursor.rowcount # Number of rows affected by the DELETE
                    self.db_conn.commit()
                    logger.info(f"Pruned {deleted_this_table} old records from table '{table_name}'. (Initially expected: {num_to_delete})")
                    pruned_count_total += deleted_this_table
                else:
                    logger.debug(f"No old records to prune from table '{table_name}' based on timestamp < {cutoff_timestamp}.")
            
            if pruned_count_total > 0:
                logger.info(f"DB pruned {pruned_count_total} records. Reloading in-memory cache to reflect changes.")
                self._memory_items.clear() # Clear the entire cache
                self._load_memory_from_db() # Reload all types of memory items from DB
                logger.info(f"In-memory cache reloaded. Current cache size: {len(self._memory_items)}.")
            else:
                logger.info("No records were pruned from the database based on retention period.")

        except sqlite3.Error as e:
            logger.error(f"Error during DB pruning: {e}", exc_info=True)
            # Potentially rollback if transactions were started explicitly, but commit is per statement here.
            return -1 # Indicate error

        logger.info(f"Memory pruning (retention-based) complete. Total DB records affected: {pruned_count_total}.")
        return pruned_count_total

    async def get_memory_dashboard_data(self) -> Dict[str, Any]:
        """
        Provides data for a user-facing dashboard to manage memory. (Stub)
        As per 10.8.F "Memory Management Dashboard".
        """
        stats_by_type = {mem_type.value: 0 for mem_type in MemoryItemType}
        total_items = len(self._memory_items)
        for item in self._memory_items.values():
            stats_by_type[item.type.value] += 1
        
        recent_items_summary = []
        # Get a few recent items as examples
        recent_items = await self.get_memory_items(max_count=5, sort_by_time=True)
        for item in recent_items:
            summary = f"ID: {item.id}, Type: {item.type.value}, Timestamp: {time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(item.timestamp))}"
            if isinstance(item, UserPreference):
                summary += f", Key: {item.key}, Value: {str(item.content)[:50]}"
            elif isinstance(item, DialogueHistory):
                summary += f", Session: {item.session_id}, Turns: {len(item.turns)}"
            elif isinstance(item, ContextSnapshot):
                summary += f", Query: {item.triggering_query[:50]}"
            recent_items_summary.append(summary)

        return {
            "total_items": total_items,
            "items_by_type": stats_by_type,
            "retention_period_days": self.retention_period / 86400,
            "max_elements_limit": self.max_elements,
            "auto_summary_interval_hours": self.auto_summary_interval / 3600,
            "recent_items_preview": recent_items_summary
            # Add more data as needed: e.g., oldest item, newest item, specific preferences list
        }

# Example Usage / Test function
async def main_test_memory_engine():
    import asyncio
    import time
    from simtag.config import Config
    from simtag.services.llm_client import LLMClient # For mocking or real use
    # MemoryItem, UserPreference, DialogueHistory, DialogueTurn, ContextSnapshot, MemoryItemType are already imported in this file

    # Basic logger for test output
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    test_logger = logging.getLogger("memory_engine_test")
    test_logger.info("Starting Memory Engine Test")

    # 1. Setup Config and Mock LLMClient
    config = Config()
    # Ensure a default llm_client_config for the test if not fully set up elsewhere
    if not config.llm_client_config.get('base_url'): 
        config.llm_client_config['base_url'] = "http://localhost:11434"
        config.llm_client_config['default_model'] = "gemma:2b"
        config.llm_client_config['default_embedding_model'] = "nomic-embed-text"
    
    config.memory_dialogues_limit = 5 # Keep test dialogue history small
    config.memory_preferences_limit = 3

    # Mock LLMClient methods (only summarize_history needs it for now)
    async def mock_summarize_history(prompt: str, model: Optional[str] = None) -> str:
        test_logger.info(f"Mock LLM summarize_history called with prompt (first 100): {prompt[:100]}...")
        return f"Mocked summary of history: {prompt.splitlines()[-1][:50]}..."

    mock_llm_client = LLMClient(llm_config=config.llm_client_config)
    # For MemoryEngine, only `generate` (used by summarize_history) might be needed.
    # If summarize_history directly calls generate, then mock that.
    # Assuming summarize_history might become more complex, we mock a specific method or the generic one.
    # Let's assume summarize_history in MemoryEngine internally calls self.llm_client.generate
    mock_llm_client.generate = mock_summarize_history

    # 2. Initialize MemoryEngine
    memory_engine = MemoryEngine(config=config, llm_client=mock_llm_client)

    # 3. Test adding and retrieving memory items
    test_logger.info("\n--- Testing User Preferences ---")
    pref1 = UserPreference(key="theme", value="dark")
    pref2 = UserPreference(key="language", value="Python")
    pref3 = UserPreference(key="verbosity", value="high")
    pref4_overwrite = UserPreference(key="theme", value="light") # Overwrites pref1
    
    await memory_engine.update_user_preference(pref1.key, pref1.value)
    await memory_engine.update_user_preference(pref2.key, pref2.value)
    await memory_engine.update_user_preference(pref3.key, pref3.value)
    all_prefs = await memory_engine.get_user_preferences()
    test_logger.info(f"  Initial preferences ({len(all_prefs)}): {all_prefs}")
    assert len(all_prefs) == 3

    await memory_engine.update_user_preference(pref4_overwrite.key, pref4_overwrite.value)
    updated_pref = await memory_engine.get_user_preference("theme")
    test_logger.info(f"  Updated preference 'theme': {updated_pref.value if updated_pref else None}")
    assert updated_pref and updated_pref.value == "light"
    all_prefs_after_update = await memory_engine.get_user_preferences()
    test_logger.info(f"  All preferences after update ({len(all_prefs_after_update)}): {all_prefs_after_update}")
    assert len(all_prefs_after_update) == 3 # Should still be 3 due to overwrite and limit

    test_logger.info("\n--- Testing Dialogue History ---")
    session_id_1 = "test_session_001"
    await memory_engine.add_dialogue_turn(session_id_1, "user", "Hello AI!", metadata={'timestamp': time.time() - 10})
    await memory_engine.add_dialogue_turn(session_id_1, "ai", "Hello User!", metadata={'timestamp': time.time() - 9})
    await memory_engine.add_dialogue_turn(session_id_1, "user", "How are you?", metadata={'timestamp': time.time() - 8})
    await memory_engine.add_dialogue_turn(session_id_1, "ai", "I am fine.", metadata={'timestamp': time.time() - 7})
    await memory_engine.add_dialogue_turn(session_id_1, "user", "What is 1+1?", metadata={'timestamp': time.time() - 6})
    await memory_engine.add_dialogue_turn(session_id_1, "ai", "It is 2.", metadata={'timestamp': time.time() - 5}) # 6th turn, will exceed limit of 5

    history_session_1 = await memory_engine.get_dialogue_history(session_id_1)
    test_logger.info(f"  Dialogue history for {session_id_1} ({len(history_session_1.turns if history_session_1 else [])} turns):")
    if history_session_1:
        for turn in history_session_1.turns:
            test_logger.info(f"    {turn.role}: {turn.content} (ts: {turn.timestamp})")
        assert len(history_session_1.turns) == config.memory_dialogues_limit
    else:
        test_logger.warning("  No history found for session_id_1")

    test_logger.info("\n--- Testing Context Snapshots ---")
    snapshot1 = ContextSnapshot(
        id="snap1", 
        triggering_query="What is X?", 
        context_content={"key_entities": ["X"], "retrieved_docs_count": 2},
        llm_response="X is a variable."
    )
    await memory_engine.record_context_snapshot(snapshot1.triggering_query, snapshot1.context_content, snapshot1.llm_response, snapshot1.id)
    # Test retrieving (though MemoryEngine doesn't have a direct get_snapshot by id yet)
    # For now, we can see it in the dashboard or general memory items list.

    test_logger.info("\n--- Testing build_context_for_llm ---")
    context_for_llm = await memory_engine.build_context_for_llm(session_id_1, user_query="Tell me more.")
    test_logger.info(f"  Built context for LLM: {context_for_llm}")
    assert "preferences_summary" in context_for_llm
    assert "dialogue_history_summary" in context_for_llm # As summarize_history is mocked, it should run

    test_logger.info("\n--- Testing Memory Dashboard ---")
    dashboard_data = await memory_engine.get_memory_dashboard_data()
    test_logger.info(f"  Memory dashboard data: User Prefs: {len(dashboard_data.get('user_preferences',[]))}, Dialogues: {len(dashboard_data.get('recent_dialogue_sessions',[]))}, Snapshots: {len(dashboard_data.get('context_snapshots',[]))}")
    test_logger.info(f"  Dashboard detail: {dashboard_data}")
    assert len(dashboard_data.get('user_preferences', [])) == 3
    assert len(dashboard_data.get('recent_dialogue_sessions', {}).get(session_id_1, {}).get('turns', [])) == config.memory_dialogues_limit
    assert len(dashboard_data.get('context_snapshots', [])) == 1

    await mock_llm_client.close()
    test_logger.info("Memory Engine Test Finished")

if __name__ == '__main__':
    asyncio.run(main_test_memory_engine()) 
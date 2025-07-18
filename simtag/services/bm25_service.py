"""bm25_service.py
Lightweight single-file BM25 index service for SimTag.
Stores an in-memory BM25Okapi index and serializes it to `index_path` with pickle so that it can be re-loaded quickly on next startup.
The index is built over the (title + content) of every node in the graph.
"""

from __future__ import annotations

import os
import pickle
import re
import logging
from typing import List, Tuple, Optional

try:
    # rank_bm25 is a pure-python dependency
    from rank_bm25 import BM25Okapi
except ImportError as e:  # pragma: no cover – handled at runtime
    raise ImportError("rank_bm25 library is required. Please add `rank_bm25` to your requirements.") from e

logger = logging.getLogger(__name__)

# Very naive whitespace/regex tokenizer. For production consider jieba/icu for CJK.
_tokenizer_re = re.compile(r"[\w\-]+", re.UNICODE)

def _tokenize(text: str) -> List[str]:
    return _tokenizer_re.findall(text.lower())


class BM25Service:
    """A minimal BM25 retrieval service backed by rank_bm25 and a pickle file."""

    def __init__(self, graph_service, index_path: str, rebuild_if_missing: bool = True):
        from simtag.core.graph_service import GraphService  # Local import to avoid circular dep
        if not isinstance(graph_service, GraphService):
            raise TypeError("graph_service must be an instance of GraphService")
        self.graph_service = graph_service
        self.index_path = index_path
        self.bm25: Optional[BM25Okapi] = None
        self.node_ids: List[str] = []
        if os.path.exists(self.index_path):
            try:
                self._load()
            except Exception as e:
                logger.warning(f"Failed to load BM25 index: {e}. Rebuilding…")
                self._build_and_save()
        elif rebuild_if_missing:
            logger.info("BM25 index not found. Building a new one…")
            self._build_and_save()
        else:
            logger.warning("BM25 index missing and rebuild_if_missing=False – service disabled.")

    # ---------------------------------------------------------------------
    # Public API
    # ---------------------------------------------------------------------

    def query(self, query_text: str, top_k: int = 10) -> List[Tuple[str, float]]:
        """Return a list of ``(node_id, score)`` sorted by descending BM25 score."""
        if not self.bm25:
            return []
        tokens = _tokenize(query_text)
        if not tokens:
            return []
        scores = self.bm25.get_scores(tokens)
        # rank_bm25 returns scores aligned with corpus order
        top_idx = sorted(range(len(scores)), key=lambda i: scores[i], reverse=True)[:top_k]
        return [(self.node_ids[i], float(scores[i])) for i in top_idx if scores[i] > 0]

    def rebuild(self):
        """Force rebuild the index from graph data."""
        self._build_and_save()

    # ---------------------------------------------------------------------
    # Internal helpers
    # ---------------------------------------------------------------------

    def _load(self):
        logger.info(f"Loading BM25 index from {self.index_path}")
        with open(self.index_path, "rb") as f:
            data = pickle.load(f)
        self.bm25 = data["bm25"]
        self.node_ids = data["node_ids"]
        logger.info(f"Loaded BM25 index with {len(self.node_ids)} documents.")

    def _build_and_save(self):
        logger.info("Building BM25 index …")
        conn = self.graph_service._get_connection()  # Use internal method for efficiency
        cursor = conn.cursor()
        cursor.execute("SELECT node_id, COALESCE(title, ''), COALESCE(content, '') FROM nodes")
        rows = cursor.fetchall()
        self.node_ids = []
        corpus_tokens: List[List[str]] = []
        for node_id, title, content in rows:
            text = f"{title} {content}"
            corpus_tokens.append(_tokenize(text))
            self.node_ids.append(node_id)
        self.bm25 = BM25Okapi(corpus_tokens)
        # Persist
        os.makedirs(os.path.dirname(self.index_path), exist_ok=True)
        with open(self.index_path, "wb") as f:
            pickle.dump({"bm25": self.bm25, "node_ids": self.node_ids}, f)
        logger.info(f"BM25 index built with {len(self.node_ids)} documents and saved to {self.index_path}") 
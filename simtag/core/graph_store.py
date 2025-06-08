"""
Knowledge Graph Storage and Management Module for Org SuperTag
"""
import logging
from typing import Dict, List, Any, Optional, Set, Tuple
from dataclasses import dataclass, field
import os

# Configure logger for this module
logger = logging.getLogger(__name__)

# Forward declaration for Config type, if needed later
# from simtag.config import Config 

@dataclass
class Entity:
    """Represents an entity in the knowledge graph."""
    id: str
    name: str
    type: str
    description: Optional[str] = None
    source_nodes: Set[str] = field(default_factory=set) # org-node IDs where this entity was found
    attributes: Dict[str, Any] = field(default_factory=dict) # Other properties

@dataclass
class Relation:
    """Represents a relationship between two entities in the knowledge graph."""
    id: str
    source_entity_id: str
    target_entity_id: str
    type: str # e.g., IS_A, PART_OF, RELATED_TO, TAGS, SEMANTICALLY_RELATED
    description: Optional[str] = None
    weight: float = 1.0
    source_nodes: Set[str] = field(default_factory=set) # org-node IDs relevant to this relation
    attributes: Dict[str, Any] = field(default_factory=dict) # Other properties
    bidirectional: bool = False


class OrgSupertagKnowledgeGraph:
    """
    Manages the storage and querying of the org-supertag knowledge graph.
    This implementation starts with in-memory storage and can be extended
    to use a persistent backend like SQLite.
    """

    def __init__(self, config: Optional[Any] = None, storage_backend: str = 'memory'):
        """
        Initializes the knowledge graph.

        Args:
            config: Optional configuration object (e.g., simtag.config.Config).
            storage_backend: The type of storage to use ('memory', 'sqlite'). Defaults to 'memory'.
        """
        self.config = config
        self.storage_backend = storage_backend
        
        # In-memory storage structures
        self.entity_storage: Dict[str, Entity] = {}  # entity_id -> Entity object
        self.relation_storage: Dict[str, Relation] = {} # relation_id -> Relation object
        
        # Adjacency list for graph traversal (stores entity IDs)
        # For a directed graph: source_entity_id -> Set of target_entity_ids
        self.adj: Dict[str, Set[str]] = {} 
        # For undirected/bidirectional relations, ensure reverse edges are also added or handled during traversal.
        
        # Reverse adjacency list (target_entity_id -> Set of source_entity_ids) - useful for some queries
        self.rev_adj: Dict[str, Set[str]] = {}

        # Index for entities by type: entity_type -> Set of entity_ids
        self.entity_type_index: Dict[str, Set[str]] = {}

        # Index for entities by name (lowercase for case-insensitive lookup): entity_name_lower -> Set of entity_ids
        self.entity_name_index: Dict[str, Set[str]] = {}
        
        logger.info(f"OrgSupertagKnowledgeGraph initialized with {storage_backend} backend.")

    async def upsert_entities(self, entities_data: List[Dict[str, Any]]):
        """
        Batch inserts or updates entities in the graph.
        Data is provided as a list of dictionaries, each representing an entity.
        Example entity_data:
        {
            'id': 'ENTITY_ID_1', 
            'name': 'Machine Learning', 
            'type': 'concept', 
            'description': 'A field of AI.',
            'source_nodes': {'node_A', 'node_B'}, # Optional
            'attributes': {'domain': 'AI'} # Optional
        }
        """
        logger.debug(f"Upserting {len(entities_data)} entities.")
        for entity_data in entities_data:
            entity_id = entity_data['id']
            
            if entity_id in self.entity_storage:
                # Update existing entity
                existing_entity = self.entity_storage[entity_id]
                logger.debug(f"Updating entity: {entity_id}")
                
                # Update fields if provided
                existing_entity.name = entity_data.get('name', existing_entity.name)
                new_type = entity_data.get('type', existing_entity.type)
                if new_type != existing_entity.type:
                    # Remove from old type index
                    if existing_entity.type in self.entity_type_index:
                        self.entity_type_index[existing_entity.type].discard(entity_id)
                    existing_entity.type = new_type
                
                existing_entity.description = entity_data.get('description', existing_entity.description)
                
                if 'source_nodes' in entity_data:
                    existing_entity.source_nodes.update(entity_data['source_nodes'])
                if 'attributes' in entity_data:
                    existing_entity.attributes.update(entity_data['attributes'])
            else:
                # Create new entity
                logger.debug(f"Creating new entity: {entity_id}")
                new_entity = Entity(
                    id=entity_id,
                    name=entity_data['name'],
                    type=entity_data['type'],
                    description=entity_data.get('description'),
                    source_nodes=set(entity_data.get('source_nodes', [])),
                    attributes=entity_data.get('attributes', {})
                )
                self.entity_storage[entity_id] = new_entity

            # Update indices for the entity (new or updated)
            current_entity = self.entity_storage[entity_id]
            
            # Update type index
            self.entity_type_index.setdefault(current_entity.type, set()).add(entity_id)
            
            # Update name index (case-insensitive)
            name_lower = current_entity.name.lower()
            self.entity_name_index.setdefault(name_lower, set()).add(entity_id)
        logger.info(f"Upserted {len(entities_data)} entities. Total entities: {len(self.entity_storage)}")

    async def upsert_relations(self, relations_data: List[Dict[str, Any]]):
        """
        Batch inserts or updates relations in the graph.
        Data is provided as a list of dictionaries, each representing a relation.
        Example relation_data:
        {
            'id': 'REL_ID_1', 
            'source_entity_id': 'ENTITY_ID_1', 
            'target_entity_id': 'ENTITY_ID_2', 
            'type': 'RELATED_TO', 
            'description': 'Entity 1 is related to Entity 2',
            'weight': 0.85, # Optional, default 1.0
            'source_nodes': {'node_A'}, # Optional
            'attributes': {'context': 'discussion_xyz'}, # Optional
            'bidirectional': False # Optional, default False
        }
        """
        logger.debug(f"Upserting {len(relations_data)} relations.")
        for rel_data in relations_data:
            rel_id = rel_data['id']
            src_id = rel_data['source_entity_id']
            tgt_id = rel_data['target_entity_id']

            # Auto-create missing entities with type detection
            entities_to_create = []
            
            def is_node_id(entity_id: str) -> bool:
                """检测是否是节点ID（UUID格式）"""
                import re
                # UUID pattern: 8-4-4-4-12 hex digits
                uuid_pattern = r'^[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}$'
                return bool(re.match(uuid_pattern, entity_id))
            
            if src_id not in self.entity_storage:
                # 根据ID格式判断实体类型
                if is_node_id(src_id):
                    entity_type = "node"
                    description = f"Auto-created node entity: {src_id}"
                    logger.warning(f"Auto-creating missing node entity (should exist): {src_id}")
                else:
                    entity_type = "tag"
                    description = f"Auto-created tag entity: {src_id}"
                    logger.info(f"Auto-creating missing tag entity: {src_id}")
                
                auto_entity = Entity(
                    id=src_id,
                    name=src_id,
                    type=entity_type,
                    description=description,
                    attributes={"auto_created": True, "created_for_relation": rel_id}
                )
                entities_to_create.append(auto_entity)
            
            if tgt_id not in self.entity_storage:
                # 根据ID格式判断实体类型
                if is_node_id(tgt_id):
                    entity_type = "node"
                    description = f"Auto-created node entity: {tgt_id}"
                    logger.warning(f"Auto-creating missing node entity (should exist): {tgt_id}")
                else:
                    entity_type = "tag"
                    description = f"Auto-created tag entity: {tgt_id}"
                    logger.info(f"Auto-creating missing tag entity: {tgt_id}")
                
                auto_entity = Entity(
                    id=tgt_id,
                    name=tgt_id,
                    type=entity_type,
                    description=description,
                    attributes={"auto_created": True, "created_for_relation": rel_id}
                )
                entities_to_create.append(auto_entity)
            
            # 批量创建缺失的实体
            if entities_to_create:
                for entity in entities_to_create:
                    self.entity_storage[entity.id] = entity
                    # Update name index
                    name_key = entity.name.lower()
                    self.entity_name_index.setdefault(name_key, set()).add(entity.id)
                    # Update type index
                    self.entity_type_index.setdefault(entity.type, set()).add(entity.id)
                
                logger.info(f"Auto-created {len(entities_to_create)} entities for relation {rel_id}")

            if rel_id in self.relation_storage:
                # Update existing relation
                existing_relation = self.relation_storage[rel_id]
                logger.debug(f"Updating relation: {rel_id}")
                existing_relation.type = rel_data.get('type', existing_relation.type)
                existing_relation.description = rel_data.get('description', existing_relation.description)
                existing_relation.weight = rel_data.get('weight', existing_relation.weight)
                if 'source_nodes' in rel_data:
                    existing_relation.source_nodes.update(rel_data['source_nodes'])
                if 'attributes' in rel_data:
                    existing_relation.attributes.update(rel_data['attributes'])
                existing_relation.bidirectional = rel_data.get('bidirectional', existing_relation.bidirectional)
            else:
                # Create new relation
                logger.debug(f"Creating new relation: {rel_id}")
                new_relation = Relation(
                    id=rel_id,
                    source_entity_id=src_id,
                    target_entity_id=tgt_id,
                    type=rel_data['type'],
                    description=rel_data.get('description'),
                    weight=rel_data.get('weight', 1.0),
                    source_nodes=set(rel_data.get('source_nodes', [])),
                    attributes=rel_data.get('attributes', {}),
                    bidirectional=rel_data.get('bidirectional', False)
                )
                self.relation_storage[rel_id] = new_relation
            
            # Update adjacency lists for the relation (new or updated)
            current_relation = self.relation_storage[rel_id]
            self.adj.setdefault(current_relation.source_entity_id, set()).add(current_relation.target_entity_id)
            self.rev_adj.setdefault(current_relation.target_entity_id, set()).add(current_relation.source_entity_id)

            if current_relation.bidirectional:
                self.adj.setdefault(current_relation.target_entity_id, set()).add(current_relation.source_entity_id)
                self.rev_adj.setdefault(current_relation.source_entity_id, set()).add(current_relation.target_entity_id)
        logger.info(f"Upserted {len(relations_data)} relations. Total relations: {len(self.relation_storage)}")

    async def get_entity(self, entity_id: str) -> Optional[Entity]:
        """Retrieves an entity by its ID."""
        return self.entity_storage.get(entity_id)

    async def get_relation(self, relation_id: str) -> Optional[Relation]:
        """Retrieves a relation by its ID."""
        return self.relation_storage.get(relation_id)

    async def get_entities_by_name(self, name: str, case_sensitive: bool = False) -> List[Entity]:
        """Retrieves entities by name."""
        lookup_name = name if case_sensitive else name.lower()
        entity_ids = self.entity_name_index.get(lookup_name, set())
        return [self.entity_storage[eid] for eid in entity_ids if eid in self.entity_storage]

    async def get_entities_by_type(self, entity_type: str) -> List[Entity]:
        """Retrieves entities by type."""
        entity_ids = self.entity_type_index.get(entity_type, set())
        return [self.entity_storage[eid] for eid in entity_ids if eid in self.entity_storage]

    async def get_neighbors(self, entity_id: str, relation_type: Optional[str] = None, direction: str = "outgoing") -> List[Entity]:
        """
        Retrieves neighbor entities of a given entity.
        
        Args:
            entity_id: The ID of the source entity.
            relation_type: Optional filter for relation type.
            direction: 'outgoing', 'incoming', or 'both'.
        
        Returns:
            A list of neighbor Entity objects.
        """
        neighbors = set()
        
        # Determine which adjacency list to use
        adj_to_search = set()
        if direction == "outgoing" or direction == "both":
            adj_to_search.update(self.adj.get(entity_id, set()))
        if direction == "incoming" or direction == "both":
             adj_to_search.update(self.rev_adj.get(entity_id, set()))


        for neighbor_id in adj_to_search:
            # Check relation type if specified
            if relation_type:
                # This requires checking all relations between entity_id and neighbor_id
                # This is inefficient with current structure if relation_type is very specific.
                # For now, we will iterate. A more optimized way would be to index relations by type and (src,tgt).
                found_relation_of_type = False
                for rel in self.relation_storage.values():
                    if ((rel.source_entity_id == entity_id and rel.target_entity_id == neighbor_id) or \
                        (rel.source_entity_id == neighbor_id and rel.target_entity_id == entity_id and rel.bidirectional)) and \
                       rel.type == relation_type:
                        found_relation_of_type = True
                        break
                if not found_relation_of_type:
                    continue # Skip if relation type doesn't match

            if neighbor_id in self.entity_storage:
                neighbors.add(self.entity_storage[neighbor_id])
        
        return list(neighbors)

    async def get_neighbors_within_k_hops(self, entity_id: str, k: int, relation_types: Optional[List[str]] = None) -> List[Entity]:
        """
        Gets all unique neighbor entities within k hops (BFS traversal).
        Optionally filters by relation types along the paths.
        """
        if entity_id not in self.entity_storage:
            logger.warning(f"Entity ID '{entity_id}' not found for k-hop search.")
            return []

        visited_entities: Set[str] = {entity_id}
        # Queue stores (entity_id, current_depth)
        queue: List[Tuple[str, int]] = [(entity_id, 0)]
        result_entities: Set[str] = set() # Store IDs of entities within k hops, excluding start_entity_id

        head = 0
        while head < len(queue):
            current_eid, depth = queue[head]
            head += 1

            if depth >= k:
                continue

            # Get neighbors based on adjacency (outgoing relations)
            # If relation_types is None, consider all relations.
            # If relation_types is specified, this simple BFS needs adjustment to check relation types on edges.
            # For now, this basic k-hop considers connectivity, not path-specific relation types.
            # TODO: Enhance to filter by relation_types along the path during BFS if critical.
            
            potential_neighbors = self.adj.get(current_eid, set())
            # If bidirectional matters for k-hop, also consider rev_adj for incoming that are bidirectional.
            # This gets complex. For now, assume standard directed k-hop or fully connected if all relations are bidirectional.

            for neighbor_eid in potential_neighbors:
                if neighbor_eid not in visited_entities:
                    # Here, one could check if the relation (current_eid -> neighbor_eid) matches relation_types.
                    # This requires finding that specific relation.
                    is_valid_path_segment = True
                    if relation_types:
                        # Check if *any* relation of the specified types exists
                        is_valid_path_segment = False
                        for rel_id, rel_obj in self.relation_storage.items():
                            if rel_obj.source_entity_id == current_eid and rel_obj.target_entity_id == neighbor_eid and rel_obj.type in relation_types:
                                is_valid_path_segment = True
                                break
                            if rel_obj.bidirectional and rel_obj.source_entity_id == neighbor_eid and rel_obj.target_entity_id == current_eid and rel_obj.type in relation_types:
                                is_valid_path_segment = True
                                break
                    
                    if is_valid_path_segment:
                        visited_entities.add(neighbor_eid)
                        result_entities.add(neighbor_eid)
                        queue.append((neighbor_eid, depth + 1))
        
        return [self.entity_storage[eid] for eid in result_entities if eid in self.entity_storage]

    async def get_reasoning_paths(self, 
                                 start_entity_ids: List[str], 
                                 target_entity_types: List[str], 
                                 max_depth: int = 3,
                                 max_paths_per_start_node: int = 5) -> Dict[str, List[Dict[str, Any]]]:
        """
        Builds reasoning paths from start entities to entities of target types.
        A path is a list of entity IDs.
        Returns a dictionary: {start_entity_id: [path_info_dict, ...]}
        path_info_dict: {'path': [entity_id, ...], 'target_entity_id': str, 'target_entity_type': str, 'depth': int, 'score': float}
        """
        all_found_paths: Dict[str, List[Dict[str, Any]]] = {}

        for start_id in start_entity_ids:
            if start_id not in self.entity_storage:
                logger.warning(f"Start entity '{start_id}' for pathfinding not found.")
                all_found_paths[start_id] = []
                continue

            paths_for_current_start: List[Dict[str, Any]] = []
            # Queue stores (current_entity_id, current_path_list_of_ids, current_depth)
            queue: List[Tuple[str, List[str], int]] = [(start_id, [start_id], 0)]
            
            # To avoid re-visiting entities within the *same path* to prevent cycles.
            # visited_in_path for BFS is tricky if paths can share nodes but not cycle.
            # A simple BFS explores shortest paths. For all paths up to max_depth, DFS is more natural.
            # Let's use DFS for finding multiple paths up to a certain depth.
            
            # Using DFS: (current_entity_id, current_path_list_of_ids)
            # Need to manage visited nodes PER PATH to avoid cycles.
            
            dfs_stack: List[Tuple[str, List[str]]] = [(start_id, [start_id])]
            
            while dfs_stack:
                current_eid, current_path = dfs_stack.pop()
                current_depth = len(current_path) - 1

                # Check if current_eid is of a target type (and not the start node itself if depth 0)
                current_entity_obj = self.entity_storage.get(current_eid)
                if current_entity_obj and current_entity_obj.type in target_entity_types and current_depth > 0:
                    path_info = {
                        'path': list(current_path), # Ensure copy
                        'target_entity_id': current_eid,
                        'target_entity_type': current_entity_obj.type,
                        'depth': current_depth,
                        'score': await self._calculate_path_score(current_path) 
                    }
                    paths_for_current_start.append(path_info)

                if current_depth >= max_depth:
                    continue

                # Explore neighbors
                for neighbor_eid in self.adj.get(current_eid, set()):
                    if neighbor_eid not in current_path: # Avoid cycles in this specific path
                        new_path = list(current_path) + [neighbor_eid]
                        dfs_stack.append((neighbor_eid, new_path))
            
            # Sort paths by score (descending) and take top N
            paths_for_current_start.sort(key=lambda p: p['score'], reverse=True)
            all_found_paths[start_id] = paths_for_current_start[:max_paths_per_start_node]
            
        return all_found_paths

    async def _calculate_path_score(self, path_entity_ids: List[str]) -> float:
        """
        Calculates a score for a given path (list of entity IDs).
        Score can be based on relation weights, path length, entity relevance, etc.
        """
        if len(path_entity_ids) < 2:
            return 0.0

        total_weight = 0.0
        num_edges = 0
        for i in range(len(path_entity_ids) - 1):
            src_id = path_entity_ids[i]
            tgt_id = path_entity_ids[i+1]
            
            # Find relations between src_id and tgt_id
            # This is simplified; a robust version might average multiple relation weights or pick the strongest.
            edge_weight = 0.1 # Default low weight if no direct relation found (or consider it an invalid path segment)
            
            # Iterate through all relations to find one matching src -> tgt
            # This is inefficient. Adjacency list should ideally store relation IDs or weights.
            for rel in self.relation_storage.values():
                if rel.source_entity_id == src_id and rel.target_entity_id == tgt_id:
                    edge_weight = rel.weight
                    break
                # if rel.bidirectional and rel.source_entity_id == tgt_id and rel.target_entity_id == src_id:
                #     edge_weight = rel.weight # Consider reverse if bidirectional
                #     break 
            
            total_weight += edge_weight
            num_edges += 1
        
        if num_edges == 0:
            return 0.0
            
        avg_weight = total_weight / num_edges
        length_penalty = 1.0 / (len(path_entity_ids) -1) # Penalize longer paths

        # Example scoring: average weight * length penalty
        # Adjust score to be higher for better paths.
        # If weights are confidences (0-1), avg_weight is fine.
        # If weights are costs, then inverse. Assume weights are "strength" or "relevance".
        score = avg_weight * length_penalty 
        return score
        
    async def _get_relations_on_path(self, entity_ids_on_path: List[str]) -> List[Relation]:
        """
        Retrieves all Relation objects that form segments of a given path of entity IDs.
        A path is an ordered list of entity_ids [e1, e2, e3, ...].
        This will find relations (e1,e2), (e2,e3), etc.
        """
        path_relations: List[Relation] = []
        if len(entity_ids_on_path) < 2:
            return []

        for i in range(len(entity_ids_on_path) - 1):
            src_id = entity_ids_on_path[i]
            tgt_id = entity_ids_on_path[i+1]
            
            # Find relations connecting src_id to tgt_id
            # This iterates all stored relations. Could be optimized with an index later 
            # (e.g., Dict[(src_id, tgt_id), List[relation_id]])
            found_direct_relation = False
            for rel_id, rel_obj in self.relation_storage.items():
                if rel_obj.source_entity_id == src_id and rel_obj.target_entity_id == tgt_id:
                    path_relations.append(rel_obj)
                    found_direct_relation = True
                # Consider bidirectional if the stored relation is marked as such and matches in reverse
                elif rel_obj.bidirectional and rel_obj.source_entity_id == tgt_id and rel_obj.target_entity_id == src_id:
                    # We might want to represent this differently or ensure the RAG engine understands
                    # that a path segment was traversed via a reverse bidirectional edge.
                    # For now, just add the relation as is.
                    path_relations.append(rel_obj)
                    found_direct_relation = True
            
            if not found_direct_relation:
                logger.debug(f"No direct relation found in storage for path segment: {src_id} -> {tgt_id}")
                # Optionally, create a temporary or implicit "follows" relation if paths can exist without explicit stored relations
                # For now, if no relation, that segment isn't explicitly added.

        return path_relations

    async def find_reasoning_paths_with_details(
        self,
        start_entity_names: List[str], # Changed from IDs to names for initial lookup
        target_entity_types: List[str],
        max_paths: int = 5, # Corresponds to top_k_reasoning_paths from RAG engine
        max_depth: int = 3
    ) -> Tuple[List[Entity], List[Relation], List[Dict[str, Any]]]:
        """
        Finds reasoning paths and returns detailed Entity, Relation objects 
        and path description documents for the RAG engine.
        """
        logger.debug(f"find_reasoning_paths_with_details called: starts={start_entity_names}, targets={target_entity_types}")
        
        all_path_entities: Dict[str, Entity] = {}
        all_path_relations: Dict[str, Relation] = {}
        path_description_documents: List[Dict[str, Any]] = []

        # Convert start_entity_names to start_entity_ids
        start_entity_ids: List[str] = []
        for name in start_entity_names:
            # Assuming get_entities_by_name returns a list, take the first one or handle multiple if necessary
            entities_found = await self.get_entities_by_name(name, case_sensitive=False)
            if entities_found:
                start_entity_ids.append(entities_found[0].id) # Use the ID of the first match
            else:
                logger.warning(f"Could not find entity ID for start name: {name}")
        
        if not start_entity_ids:
            logger.warning("No valid start entity IDs found for pathfinding.")
            return [], [], []

        # 1. Get basic paths (lists of entity IDs) from get_reasoning_paths
        # get_reasoning_paths expects max_paths_per_start_node.
        # We can pass max_paths // len(start_entity_ids) or adjust get_reasoning_paths behavior.
        # For now, let's assume max_paths is the total desired paths across all start nodes.
        # So, we might need to collect more and then trim, or adjust max_paths_per_start_node.
        # Let max_paths_per_start_node be roughly max_paths / len(start_entity_ids) or at least 1.
        effective_max_paths_per_start = max(1, max_paths // len(start_entity_ids) if start_entity_ids else max_paths)

        raw_paths_map = await self.get_reasoning_paths(
            start_entity_ids=start_entity_ids,
            target_entity_types=target_entity_types,
            max_depth=max_depth,
            max_paths_per_start_node=effective_max_paths_per_start
        )

        # Collect all paths from the map and sort them by score to take top overall `max_paths`
        collected_raw_paths_with_start_id = []
        for start_node_id, paths_info_list in raw_paths_map.items():
            for path_info in paths_info_list:
                collected_raw_paths_with_start_id.append(path_info) # path_info already has score
        
        collected_raw_paths_with_start_id.sort(key=lambda p: p['score'], reverse=True)
        top_overall_raw_paths = collected_raw_paths_with_start_id[:max_paths]

        # 2. For each top path, get detailed entities, relations, and create description doc
        for i, path_info in enumerate(top_overall_raw_paths):
            entity_ids_on_this_path: List[str] = path_info['path']
            path_score = path_info['score']
            path_doc_text_parts = []
            path_doc_text_parts.append(f"Path {i+1} (Score: {path_score:.2f}):")

            current_path_entities_temp: List[Entity] = []
            for entity_id in entity_ids_on_this_path:
                entity_obj = await self.get_entity(entity_id)
                if entity_obj:
                    all_path_entities[entity_id] = entity_obj # Store unique entities
                    current_path_entities_temp.append(entity_obj)
                    path_doc_text_parts.append(f"  - Entity: {entity_obj.name} (ID: {entity_obj.id}, Type: {entity_obj.type})")
                else:
                    path_doc_text_parts.append(f"  - Entity ID: {entity_id} (Details not found)")
            
            relations_on_this_path = await self._get_relations_on_path(entity_ids_on_this_path)
            if relations_on_this_path:
                path_doc_text_parts.append(f"  Relations on path:")
                for rel_obj in relations_on_this_path:
                    all_path_relations[rel_obj.id] = rel_obj # Store unique relations
                    # Try to get names for relation description
                    src_name = all_path_entities.get(rel_obj.source_entity_id, Entity(id=rel_obj.source_entity_id, name=rel_obj.source_entity_id, type='Unknown')).name
                    tgt_name = all_path_entities.get(rel_obj.target_entity_id, Entity(id=rel_obj.target_entity_id, name=rel_obj.target_entity_id, type='Unknown')).name
                    path_doc_text_parts.append(f"    * {src_name} --[{rel_obj.type} (ID: {rel_obj.id}, W: {rel_obj.weight:.2f})]--> {tgt_name}. Desc: {rel_obj.description or 'N/A'}")
            
            path_description_documents.append({
                "id": f"reasoning_path_{path_info.get('target_entity_id', 'unknown')}_{i}",
                "text": "\n".join(path_doc_text_parts),
                "score": path_score,
                "retrieval_source_type": "reasoning_path"
            })

        return list(all_path_entities.values()), list(all_path_relations.values()), path_description_documents

    # --- Helper methods for merging and data integrity (stubs) ---
    async def _merge_entity_data(self, existing_entity_dict: Dict[str, Any], new_entity_dict: Dict[str, Any]) -> Dict[str, Any]:
        """Merges data from a new entity dictionary into an existing one."""
        # This was used in the org-mode example, but with dataclasses, we update fields directly.
        # If using dicts for self.entity_storage:
        merged = existing_entity_dict.copy()
        merged.update(new_entity_dict) # Simple dict update, or more complex logic
        
        # Example: combine descriptions or source_nodes if they are lists/sets
        if 'description' in new_entity_dict and 'description' in existing_entity_dict:
            if new_entity_dict['description'] not in existing_entity_dict['description']: # Avoid duplicates
                 merged['description'] = f"{existing_entity_dict['description']}; {new_entity_dict['description']}"
        
        if 'source_nodes' in new_entity_dict and 'source_nodes' in existing_entity_dict:
            merged_sources = set(existing_entity_dict.get('source_nodes', []))
            merged_sources.update(new_entity_dict.get('source_nodes', []))
            merged['source_nodes'] = list(merged_sources) # or keep as set

        return merged

    async def _merge_relation_data(self, existing_relation_dict: Dict[str, Any], new_relation_dict: Dict[str, Any]) -> Dict[str, Any]:
        """Merges data from a new relation dictionary into an existing one."""
        # Similar to _merge_entity_data, if using dicts for self.relation_storage
        merged = existing_relation_dict.copy()
        merged.update(new_relation_dict)
        # Example: update weight (e.g., average, sum, max)
        if 'weight' in new_relation_dict and 'weight' in existing_relation_dict:
            # For simplicity, let's say we take the new weight or average
            merged['weight'] = (existing_relation_dict['weight'] + new_relation_dict['weight']) / 2 
        return merged

    def clear_graph(self):
        """Clears all entities and relations from the graph."""
        self.entity_storage.clear()
        self.relation_storage.clear()
        self.adj.clear()
        self.rev_adj.clear()
        self.entity_type_index.clear()
        self.entity_name_index.clear()
        logger.info("Knowledge graph cleared.")

    def get_graph_stats(self) -> Dict[str, int]:
        """Returns statistics about the graph."""
        return {
            "num_entities": len(self.entity_storage),
            "num_relations": len(self.relation_storage),
            "num_entity_types": len(self.entity_type_index),
            "num_indexed_names": len(self.entity_name_index)
        }

    async def get_node_vector(self, node_id: str) -> Optional[Dict[str, Any]]:
        """获取节点的向量表示。

        Args:
            node_id: 节点ID

        Returns:
            Optional[Dict[str, Any]]: 包含向量和元数据的字典，如果不存在则返回 None
        """
        try:
            import sqlite3
            import json
            
            # 连接数据库
            db_path = os.path.join(self.config.data_directory, "supertag_vector.db")
            conn = sqlite3.connect(db_path)
            cursor = conn.cursor()
            
            # 查询向量数据
            cursor.execute("""
                SELECT vector, hash, content_hash, model, backend
                FROM node_vectors
                WHERE node_id = ?
            """, (node_id,))
            
            row = cursor.fetchone()
            if row:
                vector_data = json.loads(row[0])  # 向量数据是 JSON 字符串
                return {
                    'vector': vector_data,
                    'hash': row[1],
                    'content_hash': row[2],
                    'model': row[3],
                    'backend': row[4]
                }
            return None
            
        except Exception as e:
            logger.error(f"Error getting node vector for {node_id}: {e}")
            return None
        finally:
            if 'conn' in locals():
                conn.close()

    async def get_node_entities(self, node_id: str) -> Optional[Dict[str, Any]]:
        """获取节点的实体和关系。

        Args:
            node_id: 节点ID

        Returns:
            Optional[Dict[str, Any]]: 包含实体、关系和元数据的字典，如果不存在则返回 None
        """
        try:
            import sqlite3
            import json
            
            # 连接数据库
            db_path = os.path.join(self.config.data_directory, "supertag_graph.db")
            conn = sqlite3.connect(db_path)
            cursor = conn.cursor()
            
            # 查询实体和关系数据
            cursor.execute("""
                SELECT entities, relations, hash, content_hash
                FROM node_entities
                WHERE node_id = ?
            """, (node_id,))
            
            row = cursor.fetchone()
            if row:
                entities = json.loads(row[0])  # 实体数据是 JSON 字符串
                relations = json.loads(row[1])  # 关系数据是 JSON 字符串
                return {
                    'entities': entities,
                    'relations': relations,
                    'hash': row[2],
                    'content_hash': row[3]
                }
            return None
            
        except Exception as e:
            logger.error(f"Error getting node entities for {node_id}: {e}")
            return None
        finally:
            if 'conn' in locals():
                conn.close()

    async def store_node_vector(self, node_id: str, vector_data: Dict[str, Any]) -> bool:
        """存储节点的向量表示。

        Args:
            node_id: 节点ID
            vector_data: 包含向量和元数据的字典

        Returns:
            bool: 是否成功存储
        """
        try:
            import sqlite3
            import json
            
            # 连接数据库
            db_path = os.path.join(self.config.data_directory, "supertag_vector.db")
            conn = sqlite3.connect(db_path)
            cursor = conn.cursor()
            
            # 创建表（如果不存在）
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS node_vectors (
                    node_id TEXT PRIMARY KEY,
                    vector TEXT NOT NULL,
                    hash TEXT,
                    content_hash TEXT,
                    model TEXT,
                    backend TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            # 准备数据
            vector_json = json.dumps(vector_data['vector'])
            
            # 更新或插入数据
            cursor.execute("""
                INSERT OR REPLACE INTO node_vectors 
                (node_id, vector, hash, content_hash, model, backend, updated_at)
                VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
            """, (
                node_id,
                vector_json,
                vector_data.get('hash'),
                vector_data.get('content_hash'),
                vector_data.get('model'),
                vector_data.get('backend')
            ))
            
            conn.commit()
            return True
            
        except Exception as e:
            logger.error(f"Error storing node vector for {node_id}: {e}")
            return False
        finally:
            if 'conn' in locals():
                conn.close()

    async def store_node_entities(self, node_id: str, entity_data: Dict[str, Any]) -> bool:
        """存储节点的实体和关系。

        Args:
            node_id: 节点ID
            entity_data: 包含实体、关系和元数据的字典

        Returns:
            bool: 是否成功存储
        """
        try:
            import sqlite3
            import json
            
            # 连接数据库
            db_path = os.path.join(self.config.data_directory, "supertag_graph.db")
            conn = sqlite3.connect(db_path)
            cursor = conn.cursor()
            
            # 创建表（如果不存在）
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS node_entities (
                    node_id TEXT PRIMARY KEY,
                    entities TEXT NOT NULL,
                    relations TEXT NOT NULL,
                    hash TEXT,
                    content_hash TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            # 准备数据
            entities_json = json.dumps(entity_data['entities'])
            relations_json = json.dumps(entity_data['relations'])
            
            # 更新或插入数据
            cursor.execute("""
                INSERT OR REPLACE INTO node_entities 
                (node_id, entities, relations, hash, content_hash, updated_at)
                VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
            """, (
                node_id,
                entities_json,
                relations_json,
                entity_data.get('hash'),
                entity_data.get('content_hash')
            ))
            
            conn.commit()
            return True
            
        except Exception as e:
            logger.error(f"Error storing node entities for {node_id}: {e}")
            return False
        finally:
            if 'conn' in locals():
                conn.close()

# Example Usage (for testing, can be removed or moved to a test file)
async def main_test():
    logging.basicConfig(level=logging.DEBUG)
    graph = OrgSupertagKnowledgeGraph()

    # Test entity upsert
    entities_to_add = [
        {'id': 'E1', 'name': 'Machine Learning', 'type': 'Concept', 'description': 'A field of AI.'},
        {'id': 'E2', 'name': 'Artificial Intelligence', 'type': 'Concept', 'description': 'Broader field.'},
        {'id': 'E3', 'name': 'Python', 'type': 'Tool', 'description': 'Programming language.'},
        {'id': 'E4', 'name': 'Ollama', 'type': 'Tool'},
    ]
    await graph.upsert_entities(entities_to_add)
    
    # Test entity update
    await graph.upsert_entities([{'id': 'E1', 'name': 'ML', 'description': 'Updated desc.', 'source_nodes': {'node1'}}])
    
    print(f"Entity E1: {await graph.get_entity('E1')}")
    print(f"Entities by type 'Concept': {await graph.get_entities_by_type('Concept')}")
    print(f"Entities by name 'Python': {await graph.get_entities_by_name('Python')}")


    # Test relation upsert
    relations_to_add = [
        {'id': 'R1', 'source_entity_id': 'E1', 'target_entity_id': 'E2', 'type': 'IS_SUBFIELD_OF', 'weight': 0.9},
        {'id': 'R2', 'source_entity_id': 'E1', 'target_entity_id': 'E3', 'type': 'USES_TOOL', 'weight': 0.7},
        {'id': 'R3', 'source_entity_id': 'E2', 'target_entity_id': 'E4', 'type': 'RELATED_TO', 'bidirectional': True},
    ]
    await graph.upsert_relations(relations_to_add)
    print(f"Relation R1: {await graph.get_relation('R1')}")
    print(f"Graph Stats: {graph.get_graph_stats()}")

    # Test neighbors
    print(f"Neighbors of E1: {await graph.get_neighbors('E1', direction='outgoing')}")
    print(f"Neighbors of E4 (bidirectional test): {await graph.get_neighbors('E4', direction='both')}")


    # Test k-hops
    print(f"1-hop neighbors of E1: {[e.name for e in await graph.get_neighbors_within_k_hops('E1', 1)]}")
    print(f"2-hop neighbors of E1: {[e.name for e in await graph.get_neighbors_within_k_hops('E1', 2)]}")

    # Test reasoning paths
    paths = await graph.get_reasoning_paths(start_entity_ids=['E1'], target_entity_types=['Tool'], max_depth=2)
    print(f"Reasoning paths from E1 to Tool: {paths}")
    
    paths_to_concept = await graph.get_reasoning_paths(start_entity_ids=['E3'], target_entity_types=['Concept'], max_depth=3)
    print(f"Reasoning paths from E3 to Concept: {paths_to_concept}")


if __name__ == '__main__':
    import asyncio
    # asyncio.run(main_test()) # Comment out if not running directly
    pass 
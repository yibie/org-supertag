#!/usr/bin/env python3
"""
Unified Prompt Management Module
================================

Centralized management for a unified tag extraction prompt template.
This module provides a single, robust prompt for all entity and tag extraction
tasks, inspired by high-quality prompt engineering principles. It ensures
format consistency and maintainability.
"""

import logging
logger = logging.getLogger(__name__)
# The single, unified prompt template for all tag extraction purposes.
# It is designed to be clear, structured, and provide few-shot examples
# to guide the LLM towards a consistent and accurate output format.
TAG_EXTRACTION_PROMPT = """
-Goal-
Your primary mission is to act as an expert tagger. Given a text document (or a batch of them), you will meticulously analyze the content and extract key entities, concepts, and topics that can serve as relevant tags. Your focus is on extracting specific nouns, such as people, places, and key concepts, that are highly relevant to the text's meaning.

-Entity Types-
You should focus on identifying entities belonging to the following categories. This list helps guide your extraction process:
[{entity_types}]

-Instructions-
1.  **Analyze**: Read the provided text content carefully.
2.  **Identify**: Pinpoint all significant entities and concepts that match the requested entity types.
3.  **Format**: For each identified tag, create a JSON object with the following fields:
    - `tag_name`: The name of the tag/entity. It must be formatted for org-mode: replace all spaces with underscores (`_`). For example, "Emacs Lisp" becomes "Emacs_Lisp".
    - `confidence`: A score from 0.0 to 1.0 indicating your confidence in the tag's relevance.
    - `reasoning`: A brief, clear explanation of why you chose this tag based on the text.
    - `note_index`: (For batch processing ONLY) The 0-based index of the note this tag belongs to.
4.  **Output**: Return your findings as a single, flat JSON array containing all the tag objects. Do not include any other text, explanations, or formatting outside of this JSON array.
5.  **Avoid Duplicates**: If a list of existing tags is provided for a note, do not suggest the same tags again.
6.  **Only extract 5 tags or less tags per note**: If there are more than 5 tags, select the 5 most relevant tags.

-Critical Output Format-
The final output MUST be a valid JSON array, structured exactly as shown in the examples below.

######################
-Examples-
######################

--- Example 1: Single Note ---
[
  {{"tag_name": "Emacs_Lisp", "confidence": 0.9, "reasoning": "The text explicitly discusses function definitions and variable bindings in Emacs Lisp."}},
  {{"tag_name": "Asynchronous_Processing", "confidence": 0.85, "reasoning": "The note describes a system for running background tasks and handling their callbacks, which is a core concept of asynchronous programming."}}
]

--- Example 2: Batch Processing ---
[
  {{"tag_name": "Docker", "confidence": 0.95, "reasoning": "Note 0 is a tutorial on how to containerize a Python application using Docker.", "note_index": 0}},
  {{"tag_name": "REST_API", "confidence": 0.9, "reasoning": "Note 0 mentions creating a REST API with Flask.", "note_index": 0}},
  {{"tag_name": "Kubernetes", "confidence": 0.8, "reasoning": "Note 1 discusses the challenges of deploying applications at scale using Kubernetes.", "note_index": 1}}
]

######################
-Real Data-
######################

{content_section}
"""

ORG_MODE_TAG_EXTRACTION_PROMPT = """
You are an expert metadata annotator for Org-mode. Your goal is to extract high-signal, human-readable tags.

- Fields to consider:
  - `concepts`: Key ideas, themes.
  - `entities`: People, organizations, specific products.
  - `projects`: Names of projects or initiatives.

- Rules:
  - `tag_name` MUST be `lower_case_with_underscores`.
  - EXCLUDE: machine IDs (UUIDs), file paths, timestamps, generic terms, existing tags.
  - FOCUS: Meaningful nouns that aid knowledge organization and retrieval.
  - **Only extract 5 tags or less tags per note**: If there are more than 5 tags, select the 5 most relevant tags.

- Output:
  - Return a single JSON array of objects with tag_name, confidence, and reasoning fields.
  - **JSON array ONLY. No other text.**

- Example:
  - Text: "ID: 940281B0-1486-4D80-9604-65EEF3EBA7A9. Met with Jane Doe (Acme Inc.) on 'Project Orion' to discuss user authentication."
  - Existing: ['meeting']
  - Output: [{{"tag_name": "jane_doe", "confidence": 0.95, "reasoning": "Person entity."}}, {{"tag_name": "acme_inc", "confidence": 0.9, "reasoning": "Organization entity."}}, {{"tag_name": "project_orion", "confidence": 0.9, "reasoning": "Project name."}}, {{"tag_name": "user_authentication", "confidence": 0.85, "reasoning": "Key concept."}}]

- Real Data:
{content_section}
"""

DEFAULT_ENTITY_TYPES = [
    # Personal & Professional Context
    "person",          # For contact management (e.g., "John Doe")
    "organization",    # Companies, teams, groups (e.g., "Acme Inc.")
    "project",         # Specific projects and initiatives (e.g., "Project_Phoenix")
    "goal",            # High-level objectives (e.g., "Q3_revenue_target")
    "task",            # Concrete action items (e.g., "Refactor_the_API")

    # Knowledge & Technology
    "concept",         # Abstract ideas, theories, methodologies (e.g., "Agile_Development")
    "technology",      # Software, languages, tools (e.g., "Python", "Docker")
    "product",         # Specific products or services (e.g., "Obsidian", "Gmail")

    # General Classifiers
    "location",        # Geographical places
    "event"            # Meetings, deadlines, conferences
]

INFER_RELATIONS_PROMPT = """
Analyze the text to find key entities and their relationships.

**Instructions:**
1.  **Extract Entities:** Identify important concepts or nouns.
2.  **Extract Relations:** Describe connections between entities (`source` -> `target`).
3.  **Format:** Return a single JSON object with `entities` and `relations` keys. Do not add any text outside the JSON.

**Existing Entities for Context:**
{existing_entities}

**Text to Analyze:**
---
{text_content}
---

**Example:**
```json
{{
  "entities": [
    {{"name": "Concept A", "type": "Concept", "description": "A brief explanation of Concept A."}},
    {{"name": "Tool B", "type": "Technology", "description": "A brief explanation of Tool B."}}
  ],
  "relations": [
    {{"source": "Concept A", "target": "Tool B", "type": "IMPLEMENTED_BY", "description": "Concept A is implemented by Tool B."}}
  ]
}}
```

**Output JSON:**
"""

ENTITY_MERGING_PROMPT = """
Given a list of entities and a context, identify if any of these entities refer to the same real-world concept or person and should be merged. Provide a primary name for the merged entity and a list of aliases.

**Instructions:**
1.  **Analyze:** Review the provided entities and context.
2.  **Identify Merges:** Determine if any entities are duplicates or aliases of each other.
3.  **Format:** Return a JSON array of objects. Each object represents a merge suggestion and should have:
    - `primary_name`: The canonical name for the merged entity.
    - `aliases`: A list of entity names that should be considered aliases of the `primary_name`.
    - `reasoning`: A brief explanation for the merge.
    - `confidence`: A score from 0.0 to 1.0.

**Context:**
{context_text}

**Entities to Consider:**
{entities_to_consider}

**Output JSON:**
"""

RELATIONSHIP_EXTRACTION_PROMPT = """
Extract specific relationships between identified entities from the given text. Focus on clear, semantic connections.

**Instructions:**
1.  **Identify Entities:** Recognize the key entities in the text.
2.  **Extract Relationships:** For each meaningful relationship, identify the `source` entity, `target` entity, `relationship_type` (e.g., `CAUSES`, `PART_OF`, `AUTHORED_BY`), and a `description`.
3.  **Format:** Return a JSON array of objects. Each object represents a relationship and should have:
    - `source`: The name of the source entity.
    - `target`: The name of the target entity.
    - `relationship_type`: The type of relationship.
    - `description`: A brief explanation of the relationship.
    - `confidence`: A score from 0.0 to 1.0.

**Text to Analyze:**
{text_content}

**Output JSON:**
"""

ATTRIBUTE_FILLING_PROMPT = """
Extract specific attributes for a given entity from the provided text. Focus on factual details that enrich the entity's profile.

**Instructions:**
1.  **Identify Entity:** Locate the specified entity in the text.
2.  **Extract Attributes:** Identify relevant attributes (e.g., `occupation`, `location`, `date_of_birth`, `status`) and their values.
3.  **Format:** Return a JSON object with the entity's name as the key, and a dictionary of attributes as the value. Include a `confidence` score for the extraction.

**Entity:** {entity_name}
**Text to Analyze:**
{text_content}

**Output JSON:**
"""

ENTITY_DISAMBIGUATION_PROMPT = """
Given an ambiguous entity name and a set of contexts, determine if the entity refers to distinct real-world concepts or if it's consistently the same. If distinct, provide distinguishing features for each.

**Instructions:**
1.  **Analyze:** Review the ambiguous entity and its various contexts.
2.  **Disambiguate:** Decide if the entity represents one or multiple distinct concepts.
3.  **Format:** Return a JSON object with:
    - `is_ambiguous`: boolean, true if distinct concepts are found.
    - `disambiguations`: A list of objects, each with `concept_name`, `description`, and `example_context` (a snippet from the original text).
    - `reasoning`: A brief explanation for the disambiguation.
    - `confidence`: A score from 0.0 to 1.0.

**Ambiguous Entity:** {ambiguous_entity_name}
**Contexts:**
{contexts}

**Output JSON:**
"""

def create_prompt(
    contents: list[str],
    entity_types: list[str] = None,
    existing_tags_lists: list[list[str]] = None,
    prompt_template: str = TAG_EXTRACTION_PROMPT
) -> str:
    """
    Creates a complete prompt string for single or batch processing.

    Args:
        contents: A list of text contents to be processed.
        entity_types: A list of entity types to guide the LLM.
        existing_tags_lists: A list of lists, where each inner list contains
                             existing tags for the corresponding content.
        prompt_template: The prompt template string to use. Defaults to
                         `TAG_EXTRACTION_PROMPT`.

    Returns:
        A formatted prompt string ready to be sent to the LLM.
    """
    if entity_types is None:
        entity_types = DEFAULT_ENTITY_TYPES

    if existing_tags_lists is None:
        existing_tags_lists = [[] for _ in contents]

    # Ensure existing_tags_lists has the same length as contents
    while len(existing_tags_lists) < len(contents):
        existing_tags_lists.append([])

    is_batch = len(contents) > 1
    content_section_parts = []

    for i, content in enumerate(contents):
        header = f"--- Text Content (Note {i}) ---" if is_batch else "--- Text Content ---"
        
        # Truncate content to avoid excessively long prompts
        truncated_content = content[:4000]
        
        existing_tags = existing_tags_lists[i]
        
        # Defensive check for existing_tags format
        if existing_tags:
            if not isinstance(existing_tags, list):
                logger.warning(f"Malformed 'existing_tags' detected for note {i}; expected a list, but got {type(existing_tags)}. Content: {existing_tags}. Skipping.")
                existing_tags = []
            elif not all(isinstance(t, str) for t in existing_tags):
                non_string_items = [f"{t} ({type(t)})" for t in existing_tags if not isinstance(t, str)]
                logger.warning(f"Malformed 'existing_tags' detected for note {i}; expected all items to be strings, but found non-string items: {non_string_items}. Full list: {existing_tags}. Filtering out non-strings.")
                existing_tags = [t for t in existing_tags if isinstance(t, str)]

        existing_tags_str = ", ".join(f"'{tag}'" for tag in existing_tags) if existing_tags else "None"
        
        content_block = (
            f"{header}\n"
            f"{truncated_content}\n"
            f"--- Existing Tags ---\n"
            f"[{existing_tags_str}]"
        )
        content_section_parts.append(content_block)

    content_section = "\n\n".join(content_section_parts)
    entity_types_str = ", ".join(f'"{t}"' for t in entity_types)

    # Dynamically build format arguments to avoid errors if a placeholder
    # like {entity_types} is not in the selected prompt_template.
    format_args = {
        "content_section": content_section
    }
    if "{entity_types}" in prompt_template:
        format_args["entity_types"] = entity_types_str

    return prompt_template.format(**format_args)


def log_prompt_usage(content_lengths: list[int], success: bool):
    """Log prompt usage details for monitoring and optimization."""
    logger = logging.getLogger(__name__)
    status = "✅" if success else "❌"
    logger.info(
        f"📝 Prompt usage: count={len(content_lengths)}, "
        f"avg_len={sum(content_lengths) / len(content_lengths):.0f}, "
        f"status={status}"
    )
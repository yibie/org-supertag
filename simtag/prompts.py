#!/usr/bin/env python3
"""
Unified Prompt Management Module
================================

Centralized management for all prompt templates.
"""

import logging
logger = logging.getLogger(__name__)

# ==============================================================================
# The single, definitive prompt for all advanced entity, relation,
# and keyword extraction purposes, based on the high-quality MiniRAG prompt.
# ==============================================================================

# Delimiters for the custom tuple-based format
TUPLE_DELIMITER = "<|>"
RECORD_DELIMITER = "##"
COMPLETION_DELIMITER = "<|COMPLETE|>"

# Default entity types from MiniRAG
DEFAULT_ENTITY_TYPES = ["organization", "person", "location", "event"]

ENTITY_EXTRACTION_PROMPT = """
Task: From the input text, output ONE JSON object with keys:
  entities        – list of {{name, type, description}}
  relationships   – list of {{source, target, type, description, strength}}
  keywords        – list of strings

Rules:
- Use only the entity types: {entity_types}
- relationship.type MUST be UPPER_SNAKE_CASE.
- strength is a number between 1.0 and 10.0.
- NO extra text, markdown, or code fences.

Text:
{input_text}

JSON example:
{{
  "entities": [
    {{"name": "Alice", "type": "PERSON", "description": "..."}}
  ],
  "relationships": [
    {{"source": "Alice", "target": "Bob", "type": "COLLABORATES_WITH", "description": "...", "strength": 7.5}}
  ],
  "keywords": ["project"]
}}

Your JSON:
"""

# ==============================================================================
# Prompt for Relation Inference (Stage B)
# ==============================================================================

RELATION_INFERENCE_PROMPT = """-Role-
You are a relationship extractor working with a predefined list of entities.

-Input-
Entities: {entities}
Text: {input_text}

-Goal-
Identify all direct relationships between **only** the provided entities. Ignore mentions of entities outside the list.

-Output Format-
Return **one** JSON object.
Key: "relationships"
Value: list of objects with keys:
    source  – entity name (string, must exist in Entities)
    target  – entity name (string, must exist in Entities)
    type    – UPPER_SNAKE_CASE relationship label (string, e.g., "MENTIONS", "USES")

Example Output:
{{"relationships": [
  {{"source": "Python", "target": "FastAPI", "type": "USES"}},
  {{"source": "FastAPI", "target": "Pydantic", "type": "DEPENDS_ON"}}
]}}

**Do not** add any explanation, markdown fences, or extra keys.
"""

# ==============================================================================
# Prompt for Auto-Tagging
# ==============================================================================

AUTOTAG_SUGGESTION_PROMPT = """
You are an AI assistant specializing in analyzing text to suggest relevant tags.

**Instructions (EN)**
1. **Text Analysis**
   • Focus on core themes, key entities (concepts/ people / organizations / places), technologies, and domain-specific terms.
   • Prioritize terms representing the main subject rather than peripheral details.
2. **Tag Generation Rules**
   • Generate **up to 5** tags; fewer is acceptable when content is narrow.
   • Each tag must be short, no more than 1-3 words.
   • Convert internal spaces to **snake_case** — e.g. "natural language processing" → "natural_language_processing".
   • For non-English terms, translate to English first, then apply snake_case rule.
   • Avoid overly broad terms. Example: use "ai_safety" instead of "technology".
3. **Output Format**
   • Return **only** a JSON object with key "tags" whose value is a list of strings.
   • Do NOT wrap in Markdown, code fences, or add extra keys.

**示例 (Example)**
Input Summary: "Python in machine learning and knowledge graph"
Expected Output:
{{
    "tags": ["python", "machine_learnning", "knowledge_graph"]
}}

---

**Text to Analyze:**
---
{text_content}
---

**Your JSON Output:**
"""

# ==============================================================================
# Prompts for specific, advanced tasks (Graph building, Querying)
# ==============================================================================

GRAPH_INFERENCE_PROMPT = """
Analyze the text to find key entities and their relationships for knowledge graph construction.

**Instructions:**
1.  **Extract Entities:** Identify important concepts or nouns. For each, provide a `name`, `type`, and `description`.
2.  **Extract Relations:** Describe connections between entities (`source` -> `target`). For each, provide `source`, `target`, `type`, and `description`.
3.  **Format:** Return a single JSON object with `entities` and `relations` keys. Do not add any text outside the JSON.

**Existing Entities for Context:**
{existing_entities}

**Text to Analyze:**
---
{text_content}
---

**Example:**
```json
{
  "entities": [
    {"name": "Concept A", "type": "Concept", "description": "A brief explanation of Concept A."},
    {"name": "Tool B", "type": "Technology", "description": "A brief explanation of Tool B."}
  ],
  "relations": [
    {"source": "Concept A", "target": "Tool B", "type": "IMPLEMENTED_BY", "description": "Concept A is implemented by Tool B."}
  ]
}
```

**Output JSON:**
"""

MINIRAG_QUERY_TO_KEYWORDS_PROMPT = """
---Role---
You are a helpful assistant tasked with identifying both answer-type and low-level keywords in the user's query.
---Goal---
Given the query, list both answer-type and low-level keywords.
answer_type_keywords focus on the type of the answer to the certain query, while low-level keywords focus on specific entities, details, or concrete terms.
The answer_type_keywords must be selected from Answer type pool.
This pool is in the form of a dictionary, where the key represents the Type you should choose from and the value represents the example samples.
---Instructions---
- Output the keywords in JSON format.
- The JSON should have three keys:
  - "answer_type_keywords" for the types of the answer. In this list, the types with the highest likelihood should be placed at the forefront. No more than 3.
  - "entities_from_query" for specific entities or details. It must be extracted from the query.

######################
-Example-
######################

Query: "How does international trade influence global economic stability?"
Answer type pool: {answer_type_pool}
################
Output:
{{
  "answer_type_keywords": ["STRATEGY", "CONCEPT"],
  "entities_from_query": ["international trade", "global economic stability", "trade agreements", "tariffs", "currency exchange"]
}}
#############################

---Real Data---
Query: {query}
Answer type pool: {answer_type_pool}
################
Output:
"""

MINIRAG_RAG_RESPONSE_PROMPT = """
---Role---
You are an expert AI assistant. Your goal is to provide a comprehensive and accurate answer to the user's query.

---Instructions---
1. **Use Context + Knowledge**: Base your answer primarily on the provided context, but also use your general knowledge when appropriate to provide more comprehensive insights.
2. **Be Creative When Needed**: For creative tasks (like generating questions, brainstorming, or analysis), don't limit yourself to just the context. Use your knowledge to provide deeper insights.
3. **Synthesize and Structure**: Do not simply list the retrieved information. Synthesize the context into a coherent, well-structured answer. Use Markdown formatting (like bullet points, bolding, and italics) to improve readability.
4. **Cite Sources**: If the context contains source IDs (e.g., `[source_id: 123]`), you MUST cite the relevant sources at the end of the sentences or paragraphs they support. This is critical for traceability.
5. **Answer Directly**: Begin by directly answering the user's query, then provide supporting details and synthesis from the context.

---Context---
{context}

---Query---
{query}

---Answer---
"""

QUERY_ANALYSIS_PROMPT = """
---Role---
You are an expert query analyzer. Your task is to extract key entities and keywords from the user's query to facilitate a precise search within a knowledge graph.

---Goal---
Given the user's query, identify the main subjects, concepts, or entities being discussed.

---Instructions---
- Output a JSON object.
- The JSON should have one key: "entities_from_query".
- The value of "entities_from_query" should be a list of strings.
- Each string should be a noun or noun phrase that represents a specific person, place, organization, concept, or technology.
- Focus on terms that are essential to understanding the user's intent. Avoid generic verbs, adjectives, or filler words.

######################
-Examples-
######################
Example 1:
Query: "What is the relationship between langchain and asyncio in the context of building conversational AI?"
################
Output:
{
    "entities_from_query": ["langchain", "asyncio", "conversational AI"]
}
######################
Example 2:
Query: "Tell me about the software architecture of the apollo project."
################
Output:
{
    "entities_from_query": ["apollo project", "software architecture"]
}
######################
Example 3:
Query: "Summarize the latest advancements in quantum computing."
################
Output:
{
    "entities_from_query": ["quantum computing", "advancements"]
}
######################

---Real Data---
Query: {query}
################
Output:
"""

QA_PROMPT = """
You are a helpful AI assistant. Answer the user's question based on the context provided below.
If the context does not contain the answer, state that you don't know.

Context:
{context}

Question:
{question}

Answer:
"""

# ==============================================================================
# Prompt for Entity Extraction Only (Stage A)
# ==============================================================================

ENTITY_ONLY_PROMPT = """
Task: Extract entities from the input text and output ONE JSON object only.

Rules:
- Use only the entity types: {entity_types}
- Do NOT include relationships or keywords.
- No explanation, no markdown, no code fences.

Text:
{input_text}

JSON example:
{{"entities": [{{"name": "Alice", "type": "PERSON", "description": "..."}}]}}

Your JSON:
"""

# ==============================================================================
# Prompts for Multi-Language Chat
# ==============================================================================

CHAT_LANGUAGE_INSTRUCTIONS = {
    "English": "Answer the following question in English: {question}",
    "Chinese": "请用中文回答以下问题: {question}",
    "Spanish": "Por favor responda a la siguiente pregunta en español: {question}",
}

def generate_query_with_language_instruction(language: str, question: str) -> str:
    """
    Generates a query string that includes a language instruction.

    Args:
        language: The target language (e.g., "Chinese").
        question: The user's original question.

    Returns:
        A formatted query string for the LLM.
    """
    template = CHAT_LANGUAGE_INSTRUCTIONS.get(language, CHAT_LANGUAGE_INSTRUCTIONS["English"])
    logger.debug(f"Generated prompt for language '{language}': {template.format(question='...')}")
    return template.format(question=question)
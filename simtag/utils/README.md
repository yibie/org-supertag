# SimTag Utils - General Utilities Package

A general utilities package for the SimTag project, providing standardized functionality across components.

## Unified Tag Processor

Solves the problem of chaotic format conversion in tag processing by providing unified data formats and processing workflows.

### Core Features

- 🎯 **Unified Data Format**: Standardized `TagResult`, `NoteResult`, `BatchResult`
- 🚀 **Smart Format Conversion**: Automatic detection and repair of various LLM response formats
- 📝 **Standard Prompt Template**: Ensures consistent format for all LLM calls
- 🔧 **Error Handling**: Unified exception handling and fallback strategies

### Quick Start

```python
from simtag.utils import UnifiedTagProcessor, TagResult, NoteResult

# Create processor
processor = UnifiedTagProcessor()

# Generate standard prompt
notes = ["Python performance optimization note", "Docker deployment guide"]
existing_tags = [["performance"], ["docker"]]
prompt = processor.create_standard_prompt(notes, existing_tags)

# Process LLM response
llm_response = "..."  # LLM 返回的原始文本
results = processor.process_llm_response(
    llm_response, 
    len(notes),
    ["note_1", "note_2"]
)

# Convert to legacy format
legacy_format = processor.convert_to_legacy_format(results)
```

### Data Structures

#### TagResult
```python
@dataclass
class TagResult:
    tag_name: str          # Tag name
    confidence: float      # Confidence (0.0-1.0)
    reasoning: str         # Reasoning
    source: str = "llm"    # Source: "llm" | "preprocessor" | "manual"
```

#### NoteResult
```python
@dataclass
class NoteResult:
    note_id: str                    # Note ID
    tags: List[TagResult]           # Tag list
    processing_time: float = 0.0    # Processing time
    method: str = "unknown"         # Processing method
```

#### BatchResult
```python
@dataclass
class BatchResult:
    notes: List[NoteResult]     # Note result list
    total_time: float           # Total processing time
    stats: Dict[str, Any]       # Statistics
```

### Usage Examples

#### Using in new components

```python
from simtag.utils import UnifiedTagProcessor, TagResult, NoteResult

class MyTagService:
    def __init__(self, llm_client):
        self.llm_client = llm_client
        self.processor = UnifiedTagProcessor()
    
    async def process_documents(self, documents):
        # Use unified prompt generation
        prompt = self.processor.create_standard_prompt(
            documents, 
            [[] for _ in documents]  # 无现有标签
        )
        
        # Call LLM
        response = await self.llm_client.generate(prompt)
        
        # Unified response processing
        results = self.processor.process_llm_response(
            response, 
            len(documents),
            [f"doc_{i}" for i in range(len(documents))]
        )
        
        return results
```

#### Merge multi-source results

```python
# Merge LLM and preprocessor results
llm_results = [...] # List[NoteResult]
preprocessor_results = [...] # List[NoteResult]

merged_results = processor.merge_results(llm_results, preprocessor_results)
```

#### Format conversion

```python
# Convert to legacy API compatible format
note_results = [...]  # List[NoteResult]
legacy_format = processor.convert_to_legacy_format(note_results)
# Return: List[List[Dict[str, Any]]]

# Create new format from legacy format
legacy_data = [[{"tag_name": "python", "confidence": 0.8, "reasoning": "..."}]]
# Need to manually convert to NoteResult
```

### Performance monitoring

```python
# Get processing statistics
stats = processor.get_stats()
print(f"Successful conversions: {stats['successful_conversions']}")
print(f"Failed conversions: {stats['failed_conversions']}")
print(f"Format fixes applied: {stats['format_fixes_applied']}")

# Reset statistics
processor.reset_stats()
```

### Advanced usage

#### Custom prompt template

```python
# Although standard template is recommended, custom prompt can be used
custom_prompt = """
Custom analysis of {note_count} notes...
{note_content_section}
Return format: [...]
"""

# Manually build content section
note_content_section = "..."
formatted_prompt = custom_prompt.format(
    note_count=len(notes),
    note_content_section=note_content_section
)
```

#### Error handling

```python
try:
    results = processor.process_llm_response(response, note_count)
except Exception as e:
    # Unified processor will automatically return empty results, no exception will be raised
    # But statistics can be checked to understand failure cases
    if processor.get_stats()['failed_conversions'] > 0:
        print("Processing failed, returning empty results")
```

## Integration guide

### Integrate into existing services

1. **Import unified processor**
   ```python
   from simtag.utils import UnifiedTagProcessor, TagResult, NoteResult
   ```

2. **Replace existing format conversion logic**
   - Remove scattered JSON cleanup code
   - Use `processor.clean_llm_response()`
   - Use `processor.process_llm_response()`

3. **Standardize prompt**
   - Use `processor.create_standard_prompt()`
   - Ensure all LLM calls use consistent format

4. **Maintain backward compatibility**
   - Use `processor.convert_to_legacy_format()` to convert output
   - Keep existing API interface unchanged

### Migration checklist

- [ ] Import unified processor
- [ ] Replace prompt generation logic
- [ ] Replace response parsing logic  
- [ ] Update error handling
- [ ] Add performance monitoring
- [ ] Test backward compatibility

## Best practices

1. **Always use unified processor**: Avoid duplicate format conversion logic
2. **Monitor statistics**: Check conversion success rate and format fix count regularly
3. **Maintain backward compatibility**: Ensure existing API works during migration
4. **Unified error handling**: Use built-in error handling mechanism of processor
5. **Performance optimization**: Reuse processor instance, avoid duplicate creation 
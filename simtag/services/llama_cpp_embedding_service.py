#!/usr/bin/env python3
"""
LlamaCpp Embedding Service
Provides embedding service using llama.cpp's llama-embedding command-line tool
"""

import os
import subprocess
import tempfile
import logging
import json
import asyncio
from typing import List, Optional, Dict, Any
from dataclasses import dataclass
from pathlib import Path
import shutil
import re
import time

# Import from the main embedding service to ensure consistency
from .embedding_service import EmbeddingBackend, EmbeddingResult

logger = logging.getLogger(__name__)

def smart_truncate(text: str, max_length: int) -> str:
    """
    Truncates text by preserving the beginning and end, joining them with '...'.
    This is a token-aware (by words) truncation.
    """
    if len(text) <= max_length:
        return text

    # Simple word-based tokenization
    words = text.split()
    
    # Estimate average characters per word
    if not words:
        return text[:max_length]
        
    avg_chars_per_word = len(text) / len(words)
    max_words = int(max_length / avg_chars_per_word)
    
    if max_words < 2: # Need at least one word from start and one from end
        return text[:max_length]

    # Preserve half of the words from the start, half from the end
    keep_words_each_side = max_words // 2
    
    start_words = words[:keep_words_each_side]
    end_words = words[-keep_words_each_side:]
    
    start_text = " ".join(start_words)
    end_text = " ".join(end_words)
    
    return f"{start_text} ... {end_text}"

class LlamaCppEmbeddingService(EmbeddingBackend):
    """
    LlamaCpp Embedding Service
    Wraps the llama-embedding command-line tool and implements the EmbeddingBackend interface.
    """
    
    def __init__(self, 
                 model_path: str,
                 binary_path: str = "llama-embedding",
                 pooling: str = "mean",
                 n_threads: Optional[int] = None,
                 n_ctx: int = 4096,
                 config_dict: Optional[Dict[str, Any]] = None):
        """
        Initializes the LlamaCpp Embedding Service
        
        Args:
            model_path: Path to the GGUF model file
            binary_path: Path to the llama-embedding binary
            pooling: Pooling strategy ("mean", "cls", "last", etc.)
            n_threads: Number of threads
            n_ctx: Context length
            config_dict: Configuration dictionary, including options for long text processing
        """
        self.model_path = Path(model_path).expanduser()
        self.binary_path = binary_path
        self.pooling = pooling
        self.n_threads = n_threads or os.cpu_count()
        self.n_ctx = n_ctx
        self._dimension: Optional[int] = None
        
        # Configuration (removed chunking related config, using node-based preprocessing)
        self.config = config_dict or {}
        
        # Validate model file
        if not self.model_path.exists():
            raise FileNotFoundError(f"Model file not found: {self.model_path}")
        
        # Validate binary file
        try:
            subprocess.run([self.binary_path, "--help"], 
                         capture_output=True, check=True, timeout=10)
        except (subprocess.CalledProcessError, FileNotFoundError, subprocess.TimeoutExpired) as e:
            raise RuntimeError(f"llama-embedding binary not found or not working: {e}")
        
        logger.info("LlamaCpp embedding service initialized:")
        logger.info(f"  Model: {self.model_path}")
        logger.info(f"  Binary: {self.binary_path}")
        logger.info(f"  Pooling: {self.pooling}")
        logger.info(f"  Threads: {self.n_threads}")
        
        # Determine the embedding dimension on initialization
        try:
            self._dimension = self._get_embedding_dimension()
            logger.info(f"  Embedding Dimension: {self._dimension}")
        except Exception as e:
            logger.error(f"Failed to determine embedding dimension during initialization: {e}")
            # We can either raise an exception or allow it to fail later.
            # For robustness, we'll let it proceed and fail on the first get_embedding call.
            pass
    
    @property
    def backend_name(self) -> str:
        return "llama_cpp"
        
    @property
    def default_dimension(self) -> int:
        if self._dimension is None:
            # Try to determine dimension again if it failed during init
            try:
                self._dimension = self._get_embedding_dimension()
            except Exception as e:
                logger.error(f"Could not determine embedding dimension: {e}")
                return 0 # Return 0 or a sensible default if it fails
        return self._dimension
    
    def _get_embedding_dimension(self) -> int:
        """
        Runs a test embedding to determine the model's output dimension.
        This is a synchronous, one-off call during initialization.
        """
        test_text = "dimension check"
        # Use a synchronous subprocess call for this one-time check
        cmd = [
            self.binary_path,
            "--model", str(self.model_path),
            "--pooling", self.pooling,
            "--threads", str(self.n_threads),
            "--ctx", str(self.n_ctx)
        ]
        
        process = subprocess.run(
            cmd, 
            input=test_text, 
            capture_output=True, 
            text=True, 
            check=True,
            timeout=30
        )
        
        output = process.stdout.strip()
        embedding_list = json.loads(output)
        return len(embedding_list)

    def _preprocess_text(self, text: str) -> str:
        """Smart text preprocessing, handles issues that might cause embedding failure"""
        if not text:
            return text
        
        # Save original text for debugging
        original_text = text
        
        # 1. Normalize newlines and whitespace
        # Replace multiple consecutive newlines with a single space
        text = re.sub(r'\n{2,}', ' ', text)  # multiple newlines -> single space
        text = re.sub(r'\n', ' ', text)      # single newline -> space
        text = re.sub(r'\r', ' ', text)      # carriage return -> space
        text = re.sub(r'\t', ' ', text)      # tab -> space
        
        # 2. Handle repeated spaces
        text = re.sub(r' {2,}', ' ', text)   # multiple spaces -> single space
        
        # 3. Remove leading/trailing whitespace
        text = text.strip()
        
        # 4. Handle overly short text - add some context
        if len(text.strip()) < 3:
            if text.strip():
                # For very short text, add a descriptive prefix
                text = f"Text content: {text.strip()}"
            
        # 5. Handle repeated content (e.g., 'comment comment')
        # Detect and simplify repeated patterns
        if len(text) < 20:  # Only check short texts for repetition
            # Check for simple repetition (e.g., "abc abc")
            words = text.split()
            if len(words) >= 2:
                # Check for adjacent repeated words
                unique_words = []
                prev_word = None
                for word in words:
                    if word != prev_word:
                        unique_words.append(word)
                        prev_word = word
                if len(unique_words) < len(words):
                    text = ' '.join(unique_words)
                    logger.debug(f"Removed adjacent duplicates: {repr(original_text)} -> {repr(text)}")
        
        # 6. Ensure text is not too short to cause tokenizer issues
        if len(text.strip()) < 5:
            text = f"This is content about '{text.strip()}'"
            logger.debug(f"Expanded short text: {repr(original_text)} -> {repr(text)}")
        
        # Log preprocessing changes
        if text != original_text:
            logger.debug(f"Text preprocessed: {repr(original_text)} -> {repr(text)}")
        
        return text

    def _analyze_text(self, text: str) -> Dict[str, Any]:
        """Analyze text features to help identify content that might cause issues"""
        analysis = {
            "length": len(text),
            "stripped_length": len(text.strip()),
            "is_empty": not text.strip(),
            "has_special_chars": bool(any(ord(c) > 127 for c in text)),
            "has_control_chars": bool(any(ord(c) < 32 and c not in '\t\n\r' for c in text)),
            "starts_with_whitespace": text != text.lstrip(),
            "ends_with_whitespace": text != text.rstrip(),
            "preview": repr(text[:50]) + ("..." if len(text) > 50 else "")
        }
        return analysis

    async def get_embedding(self, text: str) -> EmbeddingResult:
        """
        Get embedding vector for a single text
        
        Args:
            text: Input text
            
        Returns:
            EmbeddingResult: Embedding result
        """
        import time
        start_time = time.time()
        
        # More strict empty text check
        if not text or not text.strip():
            text_analysis = self._analyze_text(text)
            logger.warning(f"Empty or whitespace-only text detected: {text_analysis['preview']}")
            return EmbeddingResult(
                success=False,
                error_message=f"Empty or whitespace-only text provided: {text_analysis['preview']}"
            )
        
        # Smart text preprocessing
        original_text = text
        text = self._preprocess_text(text)
        
        # If text becomes empty after preprocessing, log and return error
        if not text.strip():
            logger.warning(f"Text became empty after preprocessing: {repr(original_text)}")
            return EmbeddingResult(
                success=False,
                error_message=f"Text became empty after preprocessing: {repr(original_text)}"
            )
        
        # Simple length check and truncation (node-based preprocessing should have handled length)
        # Use basic character length check as a safeguard
        char_length = len(text.strip())
        safe_char_length = int(self.n_ctx * 0.75) # Increase ratio to accommodate longer context
        
        if char_length > safe_char_length:
            # If character length is still too long, perform smart truncation and warn
            logger.warning(f"Text still too long after preprocessing ({char_length} chars, max {safe_char_length}), performing smart truncation.")
            text = smart_truncate(text, safe_char_length)
            logger.info(f"Smart-truncated text to {len(text)} characters: '{text[:100]}...'")
        
        # Try different pooling strategies to avoid llama.cpp bugs
        pooling_strategies = [self.pooling, "cls", "last", "mean"]
        # Remove duplicates and preserve order
        pooling_strategies = list(dict.fromkeys(pooling_strategies))
        
        for i, pooling_strategy in enumerate(pooling_strategies):
            logger.debug(f"Trying pooling strategy: {pooling_strategy} (attempt {i+1}/{len(pooling_strategies)})")
            
            result = await self._try_embedding_with_pooling(text, pooling_strategy, start_time)
            
            if result.success:
                if i > 0:  # If not successful on the first attempt
                    logger.info(f"Successfully generated embedding using fallback pooling strategy: {pooling_strategy}")
                return result
            else:
                # Check for GGML_ASSERT error
                if "GGML_ASSERT" in str(result.error_message) or "seq_id" in str(result.error_message):
                    text_preview = repr(text[:30]) + ("..." if len(text) > 30 else "")
                    logger.warning(f"Pooling strategy '{pooling_strategy}' failed with sequence ID issue for text {text_preview}, trying next strategy")
                    continue
                else:
                    # Other types of errors, possibly not a pooling strategy issue
                    logger.error(f"Non-pooling related error with strategy '{pooling_strategy}': {result.error_message}")
                    return result
        
        # All pooling strategies failed - log detailed text analysis information
        text_analysis = self._analyze_text(text)
        
        error_details = [
            "All pooling strategies failed for text:",
            f"  Text preview: {text_analysis['preview']}",
            f"  Length: {text_analysis['length']} (stripped: {text_analysis['stripped_length']})",
            f"  Is empty: {text_analysis['is_empty']}",
            f"  Has special chars: {text_analysis['has_special_chars']}",
            f"  Has control chars: {text_analysis['has_control_chars']}",
            f"  Has whitespace padding: start={text_analysis['starts_with_whitespace']}, end={text_analysis['ends_with_whitespace']}"
        ]
        
        error_message = "\n".join(error_details)
        logger.error(error_message)
        
        return EmbeddingResult(
            success=False,
            error_message=error_message,
            processing_time=time.time() - start_time
        )
    
    async def _try_embedding_with_pooling(self, text: str, pooling_strategy: str, start_time: float) -> EmbeddingResult:
        """Attempt to generate embedding using the specified pooling strategy"""
        import time
        try:
            # Use temporary file to avoid pipe issues
            with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as tmp_file:
                tmp_file.write(text.strip())
                tmp_file_path = tmp_file.name
            
            try:
                # Build command
                cmd = [
                    self.binary_path,
                    "-m", str(self.model_path),
                    "-f", tmp_file_path,
                    "--pooling", pooling_strategy,
                    "-t", str(self.n_threads),
                    "-c", str(self.n_ctx),
                    "-b", "1",  # Force batch size to 1 to avoid overflow
                    "--embd-output-format", "json",
                    "--embd-normalize", "2",  # L2 normalization
                    "--no-warmup"  # Skip warmup
                ]
                
                logger.debug(f"Running command: {' '.join(cmd)}")
                
                # Run command
                result = await asyncio.create_subprocess_exec(
                    *cmd,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE
                )
                
                stdout, stderr = await result.communicate()
                
                # Filter out known, harmless warning messages
                stderr_lines = stderr.decode().split('\n')
                filtered_stderr = []
                for line in stderr_lines:
                    if not any(warning in line for warning in [
                        "tokenizer.ggml.add_eos_token",
                        "last token in the prompt is not SEP",
                        "setting batch size to",
                        "using device Metal",
                        "llama_model_loader:",
                        "print_info:",
                        "load_tensors:",
                        "llama_context:",
                        "ggml_metal_init:",
                        "llama_kv_cache_unified:",
                        "system_info:",
                        "load: special tokens cache size",
                        "load: token to piece cache size",
                        "common_init_from_params:"
                    ]):
                        filtered_stderr.append(line)
                
                filtered_stderr_text = '\n'.join(filtered_stderr).strip()
                
                if result.returncode != 0:
                    # Only log filtered error messages
                    if filtered_stderr_text:
                        error_msg = f"llama-embedding failed with code {result.returncode}: {filtered_stderr_text}"
                    else:
                        error_msg = f"llama-embedding failed with code {result.returncode} (no significant error details)"
                    
                    return EmbeddingResult(
                        success=False,
                        error_message=error_msg,
                        processing_time=time.time() - start_time
                    )
                
                # Parse output
                output = stdout.decode().strip()
                if not output:
                    return EmbeddingResult(
                        success=False,
                        error_message="No output from llama-embedding",
                        processing_time=time.time() - start_time
                    )
                
                # Attempt to parse JSON format output
                try:
                    # Find JSON part (may be mixed with other logs)
                    lines = output.split('\n')
                    
                    # Look for the section containing JSON data
                    json_text = ""
                    in_json = False
                    brace_count = 0
                    
                    for line in lines:
                        line = line.strip()
                        if line.startswith('{'):
                            in_json = True
                            json_text = line
                            brace_count = line.count('{') - line.count('}')
                            if brace_count == 0:
                                break
                        elif in_json:
                            json_text += line
                            brace_count += line.count('{') - line.count('}')
                            if brace_count == 0:
                                break
                    
                    if json_text:
                        try:
                            json_data = json.loads(json_text)
                            
                            # Handle OpenAI-style embedding format
                            if isinstance(json_data, dict) and 'data' in json_data:
                                data_array = json_data['data']
                                if isinstance(data_array, list) and len(data_array) > 0:
                                    first_item = data_array[0]
                                    if isinstance(first_item, dict) and 'embedding' in first_item:
                                        embedding = first_item['embedding']
                                    else:
                                        raise ValueError("No embedding field in data item")
                                else:
                                    raise ValueError("Empty data array")
                            elif isinstance(json_data, dict) and 'embedding' in json_data:
                                embedding = json_data['embedding']
                            elif isinstance(json_data, list):
                                if len(json_data) > 0 and isinstance(json_data[0], list):
                                    embedding = json_data[0]
                                else:
                                    embedding = json_data
                            else:
                                raise ValueError("Unexpected JSON structure")
                                
                        except json.JSONDecodeError as e:
                            logger.debug(f"JSON parsing failed, trying alternative parsing: {e}")
                            # If JSON parsing fails, attempt to extract embedding array
                            # Look for the "embedding": [list of numbers] pattern
                            pattern = r'"embedding":\s*\[([\d\.\-,\s]+)\]'
                            match = re.search(pattern, output, re.DOTALL)
                            if match:
                                numbers_text = match.group(1)
                                # Split and convert to float numbers
                                numbers = re.findall(r'-?\d+\.?\d*', numbers_text)
                                embedding = [float(n) for n in numbers if n]
                            else:
                                raise ValueError("Could not extract embedding from output")
                    else:
                        # If JSON is not found, attempt to parse raw numeric output
                        # Find lines containing numbers
                        number_lines = []
                        for line in lines:
                            if any(char.isdigit() or char == '.' or char == '-' for char in line):
                                number_lines.append(line)
                        
                        if number_lines:
                            # Combine and parse all numeric lines
                            numbers_text = ' '.join(number_lines)
                            # Extract float numbers
                            numbers = re.findall(r'-?\d+\.?\d*', numbers_text)
                            embedding = [float(n) for n in numbers if n]
                        else:
                            raise ValueError("No numeric data found in output")
                    
                    # Validate embedding vector
                    if not embedding or not isinstance(embedding, list):
                        raise ValueError("Invalid embedding format")
                    
                    # Check if all elements are zero (may indicate an error)
                    if all(abs(x) < 1e-10 for x in embedding):
                        logger.warning("Generated embedding contains all zeros, this might indicate an issue")
                    
                    processing_time = time.time() - start_time
                    logger.debug(f"Generated embedding: dimension={len(embedding)}, time={processing_time:.3f}s")
                    
                    return EmbeddingResult(
                        success=True,
                        embedding=embedding,
                        dimension=len(embedding),
                        processing_time=processing_time
                    )
                    
                except (json.JSONDecodeError, ValueError, IndexError) as e:
                    error_msg = f"Failed to parse embedding output: {e}\nOutput: {output[:500]}..."
                    logger.error(error_msg)
                    return EmbeddingResult(
                        success=False,
                        error_message=error_msg,
                        processing_time=time.time() - start_time
                    )
            
            finally:
                # Clean up temporary file
                try:
                    os.unlink(tmp_file_path)
                except:
                    pass
                    
        except Exception as e:
            error_msg = f"Unexpected error in get_embedding: {e}"
            logger.error(error_msg, exc_info=True)
            return EmbeddingResult(
                success=False,
                error_message=error_msg,
                processing_time=time.time() - start_time
            )
    
    async def get_embeddings_batch(self, texts: List[str], model: Optional[str] = None) -> List[EmbeddingResult]:
        """
        Get embedding vectors for a batch of texts.
        The `model` parameter is ignored as this service is tied to a single model instance.
        """
        start_time = time.time()
        
        # 1. Preprocess all texts
        processed_texts = [self._preprocess_text(text) for text in texts]
        
        # 2. Filter out texts that became empty after preprocessing
        valid_texts = []
        original_indices = []
        for i, text in enumerate(processed_texts):
            if text and text.strip():
                valid_texts.append(text)
                original_indices.append(i)

        if not valid_texts:
            return [EmbeddingResult(success=False, error_message="All texts were empty after preprocessing.")] * len(texts)

        # 3. Create a temporary file to pass all valid texts to the binary
        with tempfile.NamedTemporaryFile(mode='w+', delete=True, suffix=".txt", encoding='utf-8') as tmpfile:
            # Each text must be on a new line
            tmpfile.write("\n".join(valid_texts))
            tmpfile.flush() # Ensure all data is written to disk
            
            cmd = [
                self.binary_path,
                "--model", str(self.model_path),
                "--pooling", self.pooling,
                "--threads", str(self.n_threads),
                "--ctx", str(self.n_ctx),
                "--file", tmpfile.name
            ]
            
            try:
                # 4. Run the llama-embedding process asynchronously
                process = await asyncio.create_subprocess_exec(
                    *cmd,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE
                )
                
                stdout, stderr = await process.communicate()
                
                if process.returncode != 0:
                    error_msg = f"Llama-embedding failed with code {process.returncode}: {stderr.decode('utf-8', 'ignore')}"
                    logger.error(error_msg)
                    # Return errors for all original texts
                    return [EmbeddingResult(success=False, error_message=error_msg)] * len(texts)
                
                # 5. Parse the output
                output_str = stdout.decode('utf-8').strip()
                # The output for batch is one JSON array per line
                lines = output_str.splitlines()
                batch_embeddings = [json.loads(line) for line in lines]
                
                if len(batch_embeddings) != len(valid_texts):
                    raise ValueError(f"Mismatch in batch embedding results. Expected {len(valid_texts)}, got {len(batch_embeddings)}.")

                # 6. Reconstruct the results list in the original order
                final_results = [EmbeddingResult(success=False, error_message="Text was empty after preprocessing.")] * len(texts)
                processing_time = (time.time() - start_time) / len(valid_texts)

                for i, embedding in enumerate(batch_embeddings):
                    original_idx = original_indices[i]
                    final_results[original_idx] = EmbeddingResult(
                        success=True,
                        embedding=embedding,
                        dimension=len(embedding),
                        processing_time=processing_time
                    )
                
                return final_results

            except Exception as e:
                logger.error(f"Batch embedding failed: {e}", exc_info=True)
                return [EmbeddingResult(success=False, error_message=str(e))] * len(texts)

    async def health_check(self) -> bool:
        """
        Performs a health check on the service
        """
        try:
            result = await self.get_embedding("health check")
            return result.success
        except Exception:
            return False

    def get_model_info(self) -> Dict[str, Any]:
        """Returns information about the model and service configuration."""
        return {
            "model_path": str(self.model_path),
            "binary_path": self.binary_path,
            "pooling_strategy": self.pooling,
            "threads": self.n_threads,
            "context_length": self.n_ctx,
            "dimension": self._dimension
        } 
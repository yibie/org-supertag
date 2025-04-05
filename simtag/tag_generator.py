"""
Tag Generator Module
Responsible for generating tag suggestions from text content
"""

import re
import logging
from typing import List, Dict, Any, Optional
import traceback

# Global singleton instance
_generator_instance = None

def suggest_tags(text: str, limit: int = 5) -> List[str]:
    """Extract tags from text.
    
    Args:
        text: The text to analyze
        limit: The maximum number of tags to return (default 5)
        
    Returns:
        A list of tags
    """
    global _generator_instance
    
    if _generator_instance is None:
        _generator_instance = TagGenerator(None)
    
    return _generator_instance.suggest_tags(text)  # Don't pass limit parameter

class TagGenerator:
    """Tag generator class"""

    def __init__(self, ollama_bridge):
        """Initialize tag generator
        
        Args:
            ollama_bridge: LLM interface object
        """
        self.logger = logging.getLogger("simtag.tag_generator")
        self.ollama = ollama_bridge
        
    def suggest_tags(self, text: str, limit: int = 5) -> List[str]:
        """Generate tag suggestions"""
        try:
            self.logger.debug(f"Starting tag generation, text length: {len(text)}")
            
            if not text or len(text.strip()) == 0:
                self.logger.warning("The input text is empty or only contains whitespace")
                return []
            
            try:
                if isinstance(text, str):
                    text_bytes = text.encode('utf-8')
                    text = text_bytes.decode('utf-8')
            except UnicodeError as e:
                self.logger.warning(f"Text encoding issue: {e}")
                try:
                    if isinstance(text, str):
                        text = text.encode('utf-8', errors='replace').decode('utf-8')
                except Exception as e:
                    self.logger.error(f"I can't fix the text encoding: {e}")
                    return []
            
            preview = text[:100] + "..." if len(text) > 100 else text
            self.logger.debug(f"Preview: {preview}")
            
            if not self.ollama:
                self.logger.error("Ollama client not initialized")
                return []

            cleaned_text = text.strip()
            if len(cleaned_text) < 10:
                self.logger.warning(f"The text is too short: '{cleaned_text}'")
                if cleaned_text:
                    return [cleaned_text]
                return []

            prompt = f"""Extract 5 significant tags from the following text:

TEXT START
{cleaned_text}
TEXT END

Return ONLY a comma-separated list of tags, with no explanations or other text.
Example format: tag1, tag2, tag3, tag4, tag5"""

            # Add system variable definition
            system = """You are a tag generation expert. Your task is to generate relevant tags for the given text.
Guidelines:
1. Each tag should be concise and accurate
2. Tags should reflect the main topics and concepts in the text
3. Return ONLY a comma-separated list of tags
4. Do not include any explanations or other text in your response"""

            # Try to call Ollama directly
            self.logger.debug("Preparing to call Ollama API...")
            
            # Check Ollama status and add additional tests
            try:
                # Test using ollama command line directly
                import subprocess
                result = subprocess.run(["ollama", "list"], capture_output=True, text=True)
                self.logger.debug(f"Ollama command line test: {result.stdout.strip()}")
                
                # Check API availability
                import requests
                test_response = requests.get("http://127.0.0.1:11434/api/tags")
                self.logger.debug(f"Ollama API test: {test_response.status_code}")
                
                # Check object status
                if hasattr(self.ollama, 'status'):
                    status = self.ollama.status()
                    self.logger.debug(f"Ollama status: {status}")
            except Exception as e:
                self.logger.error(f"Failed to check Ollama status: {e}")
            
            # Call Ollama
            try:
                self.logger.debug("Calling Ollama to generate tags")
                
                # Log request details
                self.logger.debug(f"System prompt: {system}")
                # Avoid logging long prompts, only log first 200 and last 100 characters
                if len(prompt) > 300:
                    self.logger.debug(f"User prompt (truncated): {prompt[:200]}...{prompt[-100:]}")
                else:
                    self.logger.debug(f"User prompt: {prompt}")
                self.logger.info(f"User prompt length: {len(prompt)} characters")
                
                # Verify if text is actually included in prompt
                text_in_prompt = "TEXT START" in prompt and "TEXT END" in prompt
                self.logger.debug(f"Text correctly included in prompt: {text_in_prompt}")
                
                # Call Ollama and log details
                self.logger.debug(f"Starting Ollama.run(), prompt length: {len(prompt)}")
                response = self.ollama.run(prompt, system=system)
                self.logger.debug(f"Ollama.run() completed, response length: {len(response) if response else 0}")
                
                # Log raw response
                raw_response = response if response else "No response"
                if len(raw_response) > 200:
                    self.logger.debug(f"Ollama raw response (truncated): {raw_response[:200]}...")
                else:
                    self.logger.debug(f"Ollama raw response: '{raw_response}'")
                self.logger.info(f"Received Ollama response, length: {len(raw_response)}")
                
                # Identify and handle special response cases
                lower_response = raw_response.lower() if raw_response else ""
                special_phrases = [
                    "please provide", "i need", "please give", 
                    "the text is empty", "no text provided", "i don't see any text",
                    "cannot generate", "unable to generate"
                ]
                
                is_error_response = any(phrase in lower_response for phrase in special_phrases)
                if is_error_response:
                    self.logger.warning(f"Ollama returned error response: '{raw_response}'")
                    
                    # Try again with clearer instructions
                    self.logger.info("Attempting with backup prompt...")
                    
                    # Backup prompt is simpler and more direct
                    backup_prompt = f"Generate 5 tags for this text: {cleaned_text[:1000]}"
                    self.logger.debug(f"Backup prompt: {backup_prompt[:200]}...")
                    
                    try:
                        backup_response = self.ollama.run(backup_prompt, system=system)
                        if backup_response and not any(phrase in backup_response.lower() for phrase in special_phrases):
                            self.logger.info(f"Backup request successful: '{backup_response}'")
                            response = backup_response
                        else:
                            self.logger.warning("Backup request also failed")
                            return []
                    except Exception as e:
                        self.logger.error(f"Backup request failed: {e}")
                        return []
                    
                # If still no valid response, return empty list
                if not response:
                    self.logger.warning("Ollama returned empty response")
                    return []
                
                # Enhanced tag extraction logic
                # First try direct comma separation
                raw_tags = [tag.strip() for tag in response.split(',')]
                
                # If only one element, response format might be incorrect
                if len(raw_tags) <= 1:
                    self.logger.warning("Response format may be incorrect, trying alternative splitting methods")
                    
                    # First try splitting by newlines
                    line_tags = []
                    for line in response.split('\n'):
                        line = line.strip()
                        # Skip empty lines and obvious non-tag lines
                        if not line or len(line) > 100:
                            continue
                        # Check if it's a list item (starts with number or hyphen)
                        if line.startswith(('-', '*', '1.', '2.', '3.', '4.', '5.')):
                            # Remove list markers
                            line = line.lstrip('-*0123456789. ')
                        # If line has commas, might be multiple tags
                        if ',' in line:
                            line_tags.extend([t.strip() for t in line.split(',')])
                        else:
                            line_tags.append(line)
                    
                    if line_tags:
                        self.logger.debug(f"Extracted {len(line_tags)} tags using newline splitting")
                        raw_tags = line_tags
                    else:
                        # If newline splitting also failed, try more aggressive splitting
                        # Look for possible tag patterns like quoted content or content after colons
                        potential_tags = re.findall(r'"([^"]+)"|\'([^\']+)\'|:\s*([^,\n]+)', response)
                        
                        extracted_tags = []
                        for tag_tuple in potential_tags:
                            # findall returns tuples, get non-empty value
                            tag = next((t for t in tag_tuple if t), None)
                            if tag:
                                extracted_tags.append(tag.strip())
                        
                        if extracted_tags:
                            self.logger.debug(f"Extracted {len(extracted_tags)} tags using regex")
                            raw_tags = extracted_tags
                        elif raw_tags[0]:  # If there's only one non-empty element
                            # Use the original single element as the only tag
                            self.logger.debug(f"Using original response as single tag: {raw_tags[0]}")
                        else:
                            self.logger.warning("Unable to extract tags from response")
                            return []
                
                self.logger.debug(f"Raw tag list ({len(raw_tags)}): {raw_tags}")
                
                # Clean and filter tags
                valid_tags = []
                for tag in raw_tags:
                    # Skip obviously invalid tags
                    if not tag or len(tag) > 50:
                        self.logger.debug(f"Skipping invalid tag: '{tag}'")
                        continue
                        
                    # Normalize tag (lowercase, remove extra spaces and punctuation)
                    clean_tag = tag.strip().lower()
                    # Remove quotes and brackets
                    clean_tag = re.sub(r'[\'"`\(\)\[\]\{\}]', '', clean_tag)
                    # Remove leading numbers and dots
                    clean_tag = re.sub(r'^[\d\.\-\s]+', '', clean_tag)
                    # Remove backslash characters
                    clean_tag = clean_tag.replace('\\', '')
                    # Replace internal spaces with underscores
                    clean_tag = re.sub(r'\s+', '_', clean_tag)
                    # No longer truncate tag length
                    
                    if clean_tag and clean_tag not in valid_tags:
                        valid_tags.append(clean_tag)
                    else:
                        self.logger.debug(f"Ignoring duplicate or empty tag: '{tag}'")
                
                # Ensure we have tags
                if not valid_tags:
                    self.logger.warning("No valid tags extracted")
                    # If unable to extract tags, try using first line as tag
                    first_line = response.split('\n')[0].strip()
                    if first_line and len(first_line) <= 50:
                        self.logger.info(f"Using first line of response as tag: '{first_line}'")
                        valid_tags = [first_line.lower()]
                    else:
                        return []
                
                if limit and valid_tags:
                    valid_tags = valid_tags[:limit]
                    
                self.logger.info(f"Final valid tags ({len(valid_tags)}): {valid_tags}")
                return valid_tags
                
            except Exception as e:
                self.logger.error(f"Ollama call failed: {e}")
                self.logger.error(traceback.format_exc())
                return []
                
        except Exception as e:
            self.logger.error(f"Tag generation process failed: {e}")
            self.logger.error(traceback.format_exc())
            return []  # Return empty list instead of raising exception
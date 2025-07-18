#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
from typing import Dict, Any

logger = logging.getLogger(__name__)

class FeedbackHandler:
    """
    Handles user feedback. This is a simplified version after the removal of MemoryEngine.
    Currently, it only logs the feedback received.
    """
    def __init__(self, config):
        """
        Initializes the simplified FeedbackHandler.
        """
        self.config = config
        logger.info("FeedbackHandler initialized (simplified).")

    async def submit_feedback(self, feedback_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Receives and logs user feedback. Does not store it permanently.

        Args:
            feedback_data: A dictionary containing feedback details.

        Returns:
            A dictionary confirming the receipt of the feedback.
        """
        try:
            item_id = feedback_data.get("item_id")
            feedback_type = feedback_data.get("feedback_type")

            if not item_id or not feedback_type:
                logger.warning("Received feedback with missing 'item_id' or 'feedback_type'.")
                return {"status": "error", "message": "Missing 'item_id' or 'feedback_type'."}

            # In this simplified version, we just log the feedback.
            # In the future, this could be stored in the graph DB or another system.
            logger.info(f"Received feedback for item '{item_id}': {feedback_data}")
            
            return {"status": "success", "message": "Feedback received and logged."}

        except Exception as e:
            logger.error(f"Error processing feedback: {e}", exc_info=True)
            return {"status": "error", "message": str(e)}

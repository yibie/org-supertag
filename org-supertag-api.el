;;; org-supertag-api.el --- API for org-supertag Python backend communication  -*- lexical-binding: t; -*- 

;;; Commentary:
;; This file provides a centralized API for interacting with the Python
;; backend of org-supertag. It handles EPC calls for features like
;; RAG (Retrieval Augmented Generation), memory management, and other
;; AI-driven functionalities.
;; It relies on `org-supertag-bridge.el` for the underlying EPC communication.

;;; Code:

(require 'epc)
(require 'json)
(require 'org-supertag-bridge)

(defvar org-supertag-api--session-id nil
  "The current session ID for the conversation.")



;; --- Helper for Async Calls (Optional, for consistent error/result handling) ---
;; (defun org-supertag-api--handle-async-response (response callback-fn)
;;   \"Generic handler for async responses from the bridge.
;; RESPONSE is the data from `org-supertag-bridge-call-async`.
;; CALLBACK-FN is the user-provided function to call with the actual result.\"\n;; (if (and (listp response) (eq (car response) :error))\n;; (progn\n;; (message \"Org SuperTag API Error: %s\" (plist-get response :message))\n;; ;; Potentially call callback-fn with nil or an error indicator\n;; (when callback-fn (funcall callback-fn nil)))\n;; (when callback-fn (funcall callback-fn response))))

;; --- RAG Functions ---

(defun org-supertag-api-rag-query (query-text session-id &optional callback-fn)
  "Send a query to the RAG system and retrieve results.
QUERY-TEXT is the user's query string.
SESSION-ID is the unique identifier for the current dialogue session.
If CALLBACK-FN is provided, the call is asynchronous, and CALLBACK-FN
will be called with the result. Otherwise, the call is synchronous.
The Python method called is \"ask_question\"."
  (interactive "sQuery: ")
  (org-supertag-bridge-ensure-ready) ; Ensure connection is up
  (let ((method-name "ask_question")
        (question-data `(:query_text ,query-text :session_id ,session-id))) ; Constructing QuestionData
    (if callback-fn
        (org-supertag-bridge-call-async method-name callback-fn question-data)
      (org-supertag-bridge-call-sync method-name 30 question-data)))) ; 30s timeout for sync

(defun org-supertag-api-set-dialogue-mode (session-id mode-name &optional callback-fn)
  "Set the dialogue mode for a specific session.
SESSION-ID is the unique identifier for the dialogue session.
MODE-NAME is the name of the mode to set (e.g., \"socratic\").
If CALLBACK-FN is provided, the call is asynchronous.
The Python method called is \"set_dialogue_mode\"."
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "set_dialogue_mode"))
    (if callback-fn
        (org-supertag-bridge-call-async method-name callback-fn session-id mode-name)
      (org-supertag-bridge-call-sync method-name 10 session-id mode-name)))) ; 10s timeout

;; --- Memory Functions ---

(defun org-supertag-api-memory-retrieve (aspect &optional callback-fn)
  "Retrieve memory from the Python backend (currently fetches the whole dashboard).
ASPECT is the conceptual aspect to retrieve (e.g., 'user_preferences').
If CALLBACK-FN is provided, the call is asynchronous.
The Python method called is \"get_memory_dashboard\"."
  ;; ASPECT is currently ignored in the call to Python, 
  ;; but kept for potential client-side filtering or future backend changes.
  (interactive "sRetrieve memory aspect: ")
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "get_memory_dashboard"))
    (if callback-fn
        (org-supertag-bridge-call-async method-name callback-fn)
      (org-supertag-bridge-call-sync method-name 10)))) ; 10s timeout

(defun org-supertag-api-memory-store (aspect data &optional callback-fn)
  "Store data for a specific aspect in the memory system.
ASPECT is the category of memory (e.g., 'user_preference').
DATA is the content to store.
If CALLBACK-FN is provided, the call is asynchronous.
The Python method called is \"update_memory_epc\"."
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "update_memory_epc")
        (update-data `(:action "store" :aspect ,aspect :payload ,data)))
    (if callback-fn
        (org-supertag-bridge-call-async method-name callback-fn update-data)
      (org-supertag-bridge-call-sync method-name 10 update-data)))) ; 10s timeout

(defun org-supertag-api-memory-clear (aspect &optional item-id callback-fn)
  "Clear a specific aspect or an item within an aspect from memory.
ASPECT is the category (e.g., 'dialogue_history').
ITEM-ID (optional) is the specific item to clear.
If CALLBACK-FN is provided, the call is asynchronous.
The Python method called is \"update_memory_epc\"."
  (interactive "sClear memory aspect: \\nP(Optional) Item ID: ")
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "update_memory_epc")
        (update-data `(:action "clear" :aspect ,aspect :item-id ,item-id)))
    (if callback-fn
        (org-supertag-bridge-call-async method-name callback-fn update-data)
      (org-supertag-bridge-call-sync method-name 10 update-data)))) ; 10s timeout

;; --- Dialogue History Functions ---

(defun org-supertag-api-get-dialogue-history (session-id &optional count callback-fn)
  "Retrieve the recent dialogue history for a specific session.
SESSION-ID is the unique identifier for the dialogue session.
COUNT (optional) specifies the number of turns to retrieve.
If CALLBACK-FN is provided, the call is asynchronous.
The Python method called is \"epc_get_dialogue_history\"."
  (interactive "P")
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "epc_get_dialogue_history")
        (max-turns (if (integerp count) count nil)))
    (if callback-fn
        (org-supertag-bridge-call-async method-name callback-fn session-id max-turns)
      (org-supertag-bridge-call-sync method-name 10 session-id max-turns)))) ; 10s timeout

(defun org-supertag-api-log-dialogue (session-id user-query assistant-response &optional callback-fn)
  "Log a user query and assistant response to a specific dialogue history.
SESSION-ID is the unique identifier for the dialogue session.
If CALLBACK-FN is provided, the call is asynchronous (fire and forget for user query,
callback attached to assistant response logging).
The Python method called is \"epc_add_dialogue_turn\"."
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "epc_add_dialogue_turn")
        (user-speaker "USER")
        (ai-speaker "AI"))

    ;; Log user query (fire-and-forget or with its own error reporting if needed)
    (org-supertag-bridge-call-async method-name 
                                    (lambda (response) ; Minimal error reporting for user query log
                                      (when (and (listp response) (eq (car response) :error))
                                        (message "Error logging user query: %s" (plist-get response :message))))
                                    session-id user-speaker user-query nil)

    ;; Log assistant response (attach the main callback-fn here if provided)
    (if callback-fn
        (org-supertag-bridge-call-async method-name callback-fn session-id ai-speaker assistant-response nil)
      (org-supertag-bridge-call-async method-name 
                                      (lambda (response) ; Minimal error reporting for AI response log
                                        (when (and (listp response) (eq (car response) :error))
                                          (message "Error logging AI response: %s" (plist-get response :message))))
                                      session-id ai-speaker assistant-response nil))))

;; --- Tag Similarity Functions ---

(defun org-supertag-api-get-similar-tags (&optional target-tag callback-fn)
  "Fetch similar tags for TARGET-TAG from the Python backend.
TARGET-TAG can be a string. If nil, backend might use current context.
If CALLBACK-FN is provided, the call is asynchronous.
Python method called: \"find_similar\"."
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "find_similar"))
    (if callback-fn
        (org-supertag-bridge-call-async method-name callback-fn target-tag 10) ; Assuming limit 10
      (org-supertag-bridge-call-sync method-name 15 target-tag 10)))) ; 15s timeout, limit 10

;; --- NER-based Tag Suggestion Functions ---

(defun org-supertag-api-get-tag-suggestions-for-note (note-content &optional existing-tags callback-fn)
  "Fetch NER-based tag suggestions for NOTE-CONTENT from the Python backend.
EXISTING-TAGS is an optional list of current tags on the note.
If CALLBACK-FN is provided, the call is asynchronous.
Anticipated Python method: \"get_ner_tag_suggestions_for_note\"."
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "get_ner_tag_suggestions_for_note")
        (payload `(:note_content ,note-content :existing_tags ,existing-tags)))
    (if callback-fn
        (org-supertag-bridge-call-async method-name callback-fn payload)
      (org-supertag-bridge-call-sync method-name 20 payload)))) ; 20s timeout for NER might be needed

;; --- Tag Relationship Suggestion Functions ---

(defun org-supertag-api-get-tag-relationship-suggestions (target-tag &optional desired-types context callback-fn)
  "Fetch inferred tag relationship suggestions for TARGET-TAG.
DESIRED-TYPES is an optional list of relationship types (strings).
CONTEXT is optional additional context for the backend.
If CALLBACK-FN is provided, the call is asynchronous.
Anticipated Python method: \"get_inferred_tag_relationships\"."
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "get_inferred_tag_relationships")
        (payload `(:target_tag ,target-tag 
                     :desired_types ,desired-types 
                     :context ,context)))
    (if callback-fn
        (org-supertag-bridge-call-async method-name callback-fn payload)
      (org-supertag-bridge-call-sync method-name 20 payload)))) ; 20s for inference

;; --- Associated Nodes Functions ---

(defun org-supertag-api-get-associated-nodes (&optional context-id callback-fn)
  "Fetch associated/related nodes for CONTEXT-ID from the Python backend.
CONTEXT-ID could be a node ID, tag, or nil for general context.
If CALLBACK-FN is provided, the call is asynchronous.
Python method called: \"get_similar_nodes\"."
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "get_similar_nodes"))
    (if callback-fn
        (org-supertag-bridge-call-async method-name callback-fn (or context-id "") 10) ; Pass context, default top_k 10
      (org-supertag-bridge-call-sync method-name 15 (or context-id "") 10)))) ; 15s timeout, default top_k 10

;; --- Knowledge Archaeology Functions ---

(defun org-supertag-api-knowledge-archaeology-dig (query-text &optional top-k)
  "Perform a knowledge archaeology dig for a given query.
QUERY-TEXT is the search term.
TOP-K is the number of results to return."
  (let ((top-k (or top-k 50)))
    (org-supertag-bridge-call-sync "knowledge_archaeology_dig" 30 query-text top-k)))

;; --- Sync version for direct tag suggestion (no callback, direct result) ---
(defun org-supertag-api-get-tag-suggestions-for-note-sync (note-content &optional existing-tags)
  "[SYNC] Get NER-based tag suggestions for the given note content.
NOTE-CONTENT is the string content of the note.
EXISTING-TAGS is an optional list of tags already on the note."
  (let ((payload `((note_content . ,note-content)
                   (existing_tags . ,(or existing-tags [])))))
    (org-supertag-bridge-call-sync "get_ner_tag_suggestions_for_note" 20 payload)))

(defun org-supertag-api-get-tag-relationship-suggestions (note-content)
  "Discover inferred tag relationships from the given note content.
NOTE-CONTENT is the string content of the note."
  (let ((payload `((note_content . ,note-content))))
    (org-supertag-bridge-call-sync "get_inferred_tag_relationships" 20 payload)))

(defun org-supertag-api-generate-text (prompt &optional callback-fn)
  "Call the generic text generation endpoint.
PROMPT is the string to send to the LLM.
If CALLBACK-FN is provided, the call is asynchronous."
  (if callback-fn
      (org-supertag-bridge-call-async "generate_text" callback-fn prompt)
    (org-supertag-bridge-call-sync "generate_text" 120 prompt)))

;;; Memory Synthesis

(defun org-supertag-api-trigger-memory-synthesis (session-id &optional callback-fn)
  "Trigger the memory synthesis process for a given dialogue session.
SESSION-ID is the ID of the conversation to analyze."
  (if callback-fn
      (org-supertag-bridge-call-async "trigger_memory_synthesis_for_session" callback-fn session-id)
    (org-supertag-bridge-call-sync "trigger_memory_synthesis_for_session" 10 session-id)))

(defun org-supertag-api-get-candidate-memories ()
  "Fetch all pending candidate memories awaiting user review."
  (org-supertag-bridge-call-sync "get_candidate_memories" 10))

(defun org-supertag-api-process-candidate-memory (candidate-id action)
  "Process a user's decision on a candidate memory.
CANDIDATE-ID is the ID of the memory candidate.
ACTION is a string, either \"accept\" or \"reject\"."
  (org-supertag-bridge-call-sync "process_candidate_memory" 10 candidate-id action))

;; --- Proactive Engine Functions ---

(defun org-supertag-api-analyze-node-context (context-data &optional callback-fn)
  "Send the current node's context to the backend for full analysis.
This includes entity extraction, graph building, vectorization, and checking for conceptual resonance.
CONTEXT-DATA is an alist containing information like node ID, content, etc.
If CALLBACK-FN is provided, the call is asynchronous.
The Python method called is \"analyze_node_context\"."
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "analyze_node_context"))
    (if callback-fn
        (org-supertag-bridge-call-async method-name callback-fn context-data)
      (org-supertag-bridge-call-sync method-name 60 context-data)))) ; 60s timeout for potentially long analysis

;; --- Bulk Sync Functions ---

(defun org-supertag-api-bulk-process-snapshot (snapshot-data &optional callback-fn)
  "Send a snapshot of incrementally changed nodes and links to the backend.
SNAPSHOT-DATA is an alist containing :nodes and :links lists.
If CALLBACK-FN is provided, the call is asynchronous.
The Python method called is \"bulk_process_snapshot\"."
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "bulk_process_snapshot"))
    (if callback-fn
        (org-supertag-bridge-call-async method-name callback-fn snapshot-data)
      (org-supertag-bridge-call-sync method-name 300 snapshot-data)))) ; 300s (5min) timeout for potentially large snapshots

(provide 'org-supertag-api)

;;; org-supertag-api.el ends here 

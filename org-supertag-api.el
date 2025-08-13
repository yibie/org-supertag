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
(require 'cl-lib)

(defvar org-supertag-api--session-id nil
  "The current session ID for the conversation.")

(defvar org-supertag-api-tagging-provider "default"
  "The provider to use for auto-tagging, e.g., 'ollama', 'openai'.")

(defvar org-supertag-api-tagging-model "default"
  "The specific model to use for auto-tagging.")

(defvar org-supertag-api-tagging-temperature 0.7
  "The temperature setting for the tagging model.")

(defvar org-supertag-api-tagging-max-tokens 2048
  "The max tokens setting for the tagging model.")

;; --- Helper for Async Calls (Optional, for consistent error/result handling) ---
;; (defun org-supertag-api--handle-async-response (response callback-fn)
;;   \"Generic handler for async responses from the bridge.
;; RESPONSE is the data from `org-supertag-bridge-call-async`.
;; CALLBACK-FN is the user-provided function to call with the actual result.\"\n;; (if (and (listp response) (eq (car response) :error))\n;; (progn\n;; (message \"Org SuperTag API Error: %s\" (plist-get response :message))\n;; ;; Potentially call callback-fn with nil or an error indicator\n;; (when callback-fn (funcall callback-fn nil)))\n;; (when callback-fn (funcall callback-fn response))))rtag-api-get-tag-relationship-suggestions (target-tag &optional desired-types context callback-fn)
;;   ...)

;; --- Query Functions ---

(defun org-supertag-api-query-similar-nodes (query-text &optional top-k callback-fn)
  "Fetch similar nodes for QUERY-TEXT from the Python backend.
QUERY-TEXT can be a node ID or any string.
TOP-K is the number of results to return.
If CALLBACK-FN is provided, the call is asynchronous.
Python method called: \"query/get_similar_nodes\"."
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "query/get_similar_nodes")
        (k (or top-k 10)))
    (if callback-fn
        (org-supertag-bridge-call-async method-name (list query-text k) callback-fn)
      (org-supertag-bridge-call-sync method-name (list query-text k) 15))))

;; --- Unified Entity Extraction for Auto-Tagging ---

(defun org-supertag-api-batch-generate-tags (payload callback)
  "Sends a batch of nodes to the backend for tag suggestion generation.
PAYLOAD is an alist containing nodes and model configuration.
CALLBACK is the function to call with the results."
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "autotag/generate_tags"))
    (org-supertag-bridge--log "API: Calling autotag/generate_tags with payload.")
    ;; The payload from the caller is already a hash-table.
    ;; We wrap it in a list to conform to the data contract.
    (org-supertag-bridge-call-async method-name (list payload) callback)))

;; --- Background Sync Functions ---

(defun org-supertag-api-sync-bulk-process (payload callback)
  "Sends a bulk snapshot of database changes to the Python backend for processing.
This is used by the background sync system.
PAYLOAD is an alist containing nodes, tags, links to upsert, and IDs to delete.
CALLBACK is the function to call with the results.
Python method called: \"sync/bulk_process\"."
  (org-supertag-bridge-ensure-ready)
  (let ((method-name "sync/bulk_process"))
    (org-supertag-bridge--log "API: Calling sync/bulk_process with payload.")
    (org-supertag-bridge-call-async method-name (list payload) callback)))

;; --- Generic Text Generation ---

(defun org-supertag-api-generate-text (prompt &optional callback-fn)
  "Call the generic text generation endpoint.
This is intended for simple, one-off generation tasks and likely maps
to a reasoning or query handler in the backend.
PROMPT is the string to send to the LLM.
If CALLBACK-FN is provided, the call is asynchronous.
Assumed Python method: \"reasoning/run_cycle\" or a similar generic generator."
  (org-supertag-bridge-ensure-ready)
  ;; NOTE: The backend method for generic text generation is not clearly
  ;; defined post-refactor. After refactor, use 'knowledge/run_cycle'.
  (let ((method-name "knowledge/run_cycle"))
    (if callback-fn
        (org-supertag-bridge-call-async method-name (list prompt) callback-fn)
      (org-supertag-bridge-call-sync method-name (list prompt) 120))))

;; --- Simple Tag Suggestions ---

;;;###autoload
(defun org-supertag-suggest-tags-here ()
  "Suggest tags for current node and let user select which to apply.
Simple, manual trigger - no background processing."
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (message "Can only suggest tags in org-mode")
    (let* ((node-id (org-id-get-create))
           (node-data (org-supertag-db-get node-id))
           (title (plist-get node-data :title))
           (content (plist-get node-data :content))
           (combined-content (concat (or title "") "\n\n" (or content ""))))
      
      (if (< (length (string-trim combined-content)) 20)
          (message "Node content too short for meaningful suggestions")
        (progn
          (message "Analyzing content for tag suggestions...")
          (let* ((payload `(("nodes" . (((("id" . ,node-id)
                                         ("content" . ,combined-content)))))
                           ("model_config" . (("provider" . "ollama")
                                            ("model" . "hf.co/unsloth/gemma-3-4b-it-GGUF:latest")
                                            ("temperature" . 0.7)
                                            ("max_tokens" . 1024)))))
                 (result (org-supertag-bridge-call-sync "autotag/generate_tags" (list payload) 30)))
            
            (if (and result (> (length result) 0))
                (let* ((suggestion (car result))
                       (tags (plist-get suggestion :tags)))
                  (if tags
                      (let ((selected-tags (completing-read-multiple
                                           "Select tags to apply: " tags nil nil)))
                        (when selected-tags
                          (dolist (tag-name selected-tags)
                            (let ((tag-id (org-supertag-sanitize-tag-name tag-name)))
                              (unless (org-supertag-tag-get tag-id)
                                (org-supertag-tag--create tag-id))
                              (org-supertag-node-db-add-tag node-id tag-id)
                              (org-supertag-inline-add tag-name)))
                          (message "Applied %d tags" (length selected-tags))))
                    (message "No tag suggestions available")))
              (message "Failed to get tag suggestions"))))))))

;;;###autoload
(defun org-supertag-chat-with-node ()
  "Open chat interface with current node as context."
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (message "Can only chat with nodes in org-mode")
    (let ((node-id (org-id-get-create)))
      (require 'org-supertag-view-chat)
      (org-supertag-view-chat-open)
      (message "Chat opened with current node context"))))

;; --- Utility Functions ---

(defun org-supertag-api-get-all-tags ()
  "Get all tags from the database."
  (org-supertag-db-get-all-tags))

(provide 'org-supertag-api)

;;; org-supertag-api.el ends here

;;; supertag-rag.el --- The ultimate, LLM-driven RAG component -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Supertag Authors

;; Author: Supertag Team
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (gptel "0.5") (org "9.0"))
;; Keywords: outlines, hypermedia, calendar, wp
;; URL: https://github.com/supertag/org-supertag

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides RAG (Retrieval-Augmented Generation) functionality
;; for org-supertag, leveraging gptel for LLM interaction and the existing
;; supertag query system for high-performance local retrieval.

;;; Code:

(require 'gptel)
(require 'supertag-core-store)
(require 'supertag-services-query)
(require 'json) ;; For safer parsing of LLM responses

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup supertag-rag nil
  "Configuration for the Supertag RAG component."
  :group 'org-supertag)

(defcustom supertag-rag-max-context-length 10000
  "Maximum length of context to send to the LLM. Longer contexts will be truncated."
  :type 'integer
  :group 'supertag-rag)

(defcustom supertag-rag-max-document-excerpt 2000
  "Maximum length of each document excerpt included in the context."
  :type 'integer
  :group 'supertag-rag)

(defcustom supertag-rag-display-buffer-on-start t
  "Whether to display the answer buffer when the first chunk arrives."
  :type 'boolean
  :group 'supertag-rag)

  (defcustom supertag-rag-sexp-generation-prompt
    "You are an expert programmer who translates natural language into a Lisp-based query language for Org Supertag.

Your task is to convert the user's question into a valid S-expression for the `supertag-query-sexp` function.
**BEFORE returning your query, validate that ALL string values are properly quoted with double quotes**

Supported Query Operators Table:
| Operator | Parameters          | Example                          | Description                     |
|----------|---------------------|----------------------------------|---------------------------------|
| (and ...) | Any number of sub-expressions | (and (tag \"work\") (after \"-7d\")) | All conditions must match       |
| (or ...)  | Any number of sub-expressions | (or (tag \"urgent\") (tag \"important\")) | Any condition can match         |
| (not ...) | 1 sub-expression    | (not (tag \"archived\"))          | The condition must not match    |
| (tag \"X\") | 1 tag name (string) | (tag \"work\")                    | Find notes with specific tag    |
| (field \"K\" \"V\") | Field name + value (2 strings) | (field \"status\" \"done\")       | Find by custom field value      |
| (after \"D\") | 1 date string       | (after \"-7d\")                   | Date-based queries              |
| (before \"D\") | 1 date string       | (before \"2024-01-01\")           | Date-based queries              |
| (between \"S\" \"E\") | Start + end dates (2 strings) | (between \"2024-01-01\" \"2024-01-31\") | Date range queries              |
| (term \"X\") | 1 keyword (string)  | (term \"projectx\")               | Search for keyword in content   |

Date Formats:
- Relative: \"-7d\" (7 days ago), \"+2w\" (2 weeks later), \"-1m\" (1 month ago)
- Absolute: \"2024-01-01\" (YYYY-MM-DD)  
- Special: \"now\", \"today\", \"yesterday\"

Critical Rules:
1. ALL string values MUST be enclosed in double quotes
2. Focus on core intent: keywords, tags, and date ranges
3. Prefer tags over terms when possible
4. Ignore generic nouns like \"notes\", \"tasks\", \"list\", etc.
5. Ignore action phrases like \"help me\", \"please\", \"show\", etc.
6. Ignore question words like \"what\", \"how\", \"where\", etc.
7. For time-related questions, ALWAYS use relative date formats (\"-1d\", \"-7d\", \"-1m\") instead of \"now\" or absolute dates
8. Combine multiple conditions using 'and' when relevant
9. ALWAYS enclose ALL string values in double quotes, including date strings

Examples:
User: \"find notes about projectx from last week\"
Query: (and (term \"projectx\") (after \"-7d\"))

User: \"show me articles tagged #urgent\"
Query: (tag \"urgent\")

User: \"find all urgent tasks from this week\"
Query: (and (tag \"urgent\") (tag \"task\") (after \"-7d\"))

User: \"recent notes from last 3 days\"
Query: (after \"-3d\")

User: \"what are my new ideas\"
Query: (and (tag \"idea\") (after \"-7d\"))

INCORRECT: (and (tag idea) (after -7d))
CORRECT: (and (tag \"idea\") (after \"-7d\"))

User's Question: \"%s\"

Return ONLY the raw Lisp S-expression. Ensure all string values are properly quoted with double quotes. Do not include any other text or explanations."
    "The prompt template used to ask the LLM to generate a query S-expression."
    :type 'string
    :group 'supertag-rag)

(defcustom supertag-rag-final-answer-prompt
  (string-join
   '("You are an assistant helping a user understand their personal notes and ideas."
     "The user has a knowledge base of personal notes, and they're asking a question about what they've recorded."
     "Your task is to answer their question based EXCLUSIVELY on the provided context."
     ""
     "CRITICAL INSTRUCTIONS:"
     "1. The context contains REAL notes the user recorded in their knowledge base"
     "2. You MUST find and use relevant information from the context to answer the question"
     "3. If the context contains information related to the question, you MUST use it"
     "4. Only if the context is completely empty or has no relevant information should you say 'no relevant notes were found'"
     "5. DO NOT say the context is insufficient or that you need more information"
     "6. DO NOT make up information that is not in the context"
     "7. Answer in the same language as the user's question"
     "8. Be specific and cite information from the notes when possible"
     "9. Format your answer using Markdown"
     "10. Provide detailed explanations and do not give overly brief responses"
     "11. If you find relevant information, explain it thoroughly and provide examples from the context"
     "12. Structure your response with clear headings and subheadings when appropriate"
     "13. Use bullet points or numbered lists to organize information when helpful"
     ""
     "IMPORTANT: The user's question is in {detected_language}. Your answer must also be in {detected_language}."
     ""
     "Here is the context from the user's notes:"
     "--- CONTEXT ---"
     "%s"
     "--- END CONTEXT ---"
     ""
     "The user's question is: %s"
     ""
     "Please provide a detailed answer based on the context above. If relevant information exists in the context, be sure to use it to provide a comprehensive response.")
   "\n")
  "The prompt template used to generate the final answer."
  :type 'string
  :group 'supertag-rag)

(defcustom supertag-rag-model nil
  "The model to use for RAG queries. If nil, gptel's default is used."
  :type '(or nil string)
  :group 'supertag-rag)

(defcustom supertag-rag-general-answer-prompt
  (string-join
   '("You are a helpful assistant. Please provide a clear and comprehensive answer to the user's question."
     "Format your answer using Markdown."
     "The user's question is in Chinese. Your answer must also be in Chinese."
     ""
     "User's question: %s")
   "\n")
  "The prompt template used for general questions when no local context is found."
  :type 'string
  :group 'supertag-rag)

;;-----------------------------------------------------------------------------
;;; 1. User-facing command
;;-----------------------------------------------------------------------------

;;;###autoload
(defun supertag-rag-ask (question &rest args)
  "Ask a question using an LLM-driven query planning RAG process.

QUESTION is the user's natural language query.

ARGS is a property list that can contain the following keys:
:callback - An optional function to be called with the final answer string.
            This is called only ONCE when the entire response is complete.
:stream-callback - An optional function called with each chunk of the
                   streaming response.
:feedback-callback - An optional function called with status strings
                      during the process.
:mode - Determines the behavior of the RAG engine:
- :smart (default): Attempts to find local notes. If none are found,
  it gracefully degrades to a general-purpose Q&A.
- :rag-only: Strictly uses local notes. If none are found, it
  returns a message indicating so and does not call the LLM for a
  general answer.
- :general-only: Skips local search and directly asks the LLM."
  (interactive "sAsk Supertag RAG: ")
  (let* ((mode (or (plist-get args :mode) :smart))
         (stream-callback (plist-get args :stream-callback))
         (callback (plist-get args :callback))
         (feedback-callback (plist-get args :feedback-callback)))

    (unless (and question (not (string-empty-p question)))
      (message "Supertag RAG: Please provide a valid question.")
      (cl-return-from supertag-rag-ask))

    (if (eq mode :general-only)
        (progn
          (when feedback-callback (funcall feedback-callback "Asking assistant directly..."))
          (supertag-rag--generate-answer (format supertag-rag-general-answer-prompt question) callback stream-callback))
      (when feedback-callback (funcall feedback-callback "Generating query plan..."))
      (supertag-rag--generate-sexp-from-question
       question
       (lambda (s-exp-query)
         (let ((final-query (or (when (and (listp s-exp-query) (supertag-rag--validate-sexp s-exp-query))
                                  (-> s-exp-query
                                      supertag-rag--post-process-query
                                      supertag-rag--optimize-query))
                                (supertag-rag--generate-default-query question))))
           (when feedback-callback (funcall feedback-callback (format "Query plan received, executing: %s" final-query)))
           (condition-case err
               (let* ((node-ids (supertag-query-sexp final-query))
                      (context (supertag-rag--retrieve-context-from-ids node-ids)))
                 (if (or (not context) (string-empty-p context))
                     (cond
                      ((eq mode :rag-only)
                       (when feedback-callback (funcall feedback-callback "No relevant notes found."))
                       (when callback (funcall callback "在您的笔记中未找到相关内容。")))
                      ((eq mode :smart)
                       (when feedback-callback (funcall feedback-callback "No relevant notes found, asking assistant directly..."))
                       (supertag-rag--generate-answer (format supertag-rag-general-answer-prompt question) callback stream-callback)))
                   (when feedback-callback
                     (funcall feedback-callback
                              (format "Found %d notes, generating final answer..." (length node-ids))))
                   (let ((prompt (format supertag-rag-final-answer-prompt
                                         (supertag-rag--enhance-context-with-metadata context)
                                         question)))
                     (supertag-rag--generate-answer prompt callback stream-callback))))
             (error
              (message "Supertag RAG: Error executing query: %s" err)))))))))

;;-----------------------------------------------------------------------------
;;; 2. Helper Functions
;;-----------------------------------------------------------------------------

(defun supertag-rag--generate-sexp-from-question (question callback)
  "Ask LLM to translate a natural language question into a supertag S-expression."
  (let ((prompt (format supertag-rag-sexp-generation-prompt question)))
    (apply #'gptel-request prompt
           (append `(:callback
                     ,(lambda (s-exp-string &rest _)
                        (condition-case err
                            (if (and s-exp-string (not (string-empty-p s-exp-string)))
                                (let ((s-exp (car (read-from-string s-exp-string))))
                                  (funcall callback s-exp))
                              (funcall callback nil))
                          (error 
                           (message "Supertag RAG: Failed to parse LLM response: %s. Response: %s" err s-exp-string)
                           (funcall callback nil)))))
                   (if supertag-rag-model `(:model ,supertag-rag-model) nil)))))

(defun supertag-rag--generate-default-query (question)
  "Generate a default query based on common patterns in the question."
  (let ((lower-question (downcase question)))
    (cond
     ;; If question mentions recent days/week/month
     ((string-match "\\(最近\\|近期\\|最近几天\\|最近一周\\|最近一个月\\|last\\|recent\\|past\\|previous\\)" lower-question)
      (cond
       ((string-match "\\(天\\|days\\)" lower-question) '(after "-1d"))
       ((string-match "\\(周\\|week\\)" lower-question) '(after "-7d"))
       ((string-match "\\(月\\|month\\)" lower-question) '(after "-1m"))
       (t '(after "-7d"))))
     ;; If question mentions yesterday
     ((string-match "\\(昨天\\|yesterday\\)" lower-question)
      '(after "-1d"))
     ;; If question mentions today or related terms
     ((string-match "\\(今天\\|today\\|今日\\|当天\\)" lower-question)
      '(after "-1d"))
     ;; If question mentions summary/summarize
     ((string-match "\\(总结\\|summary\\|summarize\\)" lower-question)
      '(after "-7d"))
     ;; Default to last week
     (t '(after "-7d")))))

(defun supertag-rag-debug-node-data (node-id)
  "Debug function to inspect the data structure of a specific node."
  (interactive "sNode ID to inspect: ")
  (let ((node-data (supertag-store-direct-get :nodes node-id)))
    (message "Node data for %s: %s" node-id node-data)
    (if (null node-data)
        (message "Node with ID %s not found in store" node-id)
      (let ((title (plist-get node-data :title))
            (content (plist-get node-data :content))
            (tags (plist-get node-data :tags))
            (properties (plist-get node-data :properties))
            (created-at (plist-get node-data :created-at))
            (modified-at (plist-get node-data :modified-at)))
        (message "Title: %s" title)
        (message "Content type: %s, length: %d" (type-of content) (if content (length content) 0))
        (message "Content value: %s" (if (and content (stringp content)) 
                                         (if (> (length content) 100) 
                                             (concat (substring content 0 100) "...") 
                                           content) 
                                       "Not a string or nil"))
        (message "Tags: %s" tags)
        (message "Properties: %s" properties)
        (message "Created at: %s" created-at)
        (message "Modified at: %s" modified-at)))))

(defun supertag-rag-debug-list-all-nodes ()
  "Debug function to list all nodes in the store with basic information."
  (interactive)
  (let ((nodes-collection (supertag-get '(:nodes)))
        (node-count 0))
    (message "Supertag RAG Debug: Listing all nodes in store...")
    (when (hash-table-p nodes-collection)
      (maphash
       (lambda (id node-data)
         (setq node-count (1+ node-count))
         (let ((title (plist-get node-data :title))
               (content (plist-get node-data :content))
               (created-at (plist-get node-data :created-at))
               (tags (plist-get node-data :tags)))
           (message "Node ID: %s, Title: %s, Content length: %d, Created: %s, Tags: %s" 
                    id (or title "No title") (if content (length content) 0)
                    (if created-at (format-time-string "%Y-%m-%d %H:%M:%S" created-at) "Unknown")
                    (if tags (mapconcat 'identity tags ", ") "None"))))
       nodes-collection))
    (message "Supertag RAG Debug: Total nodes in store: %d" node-count)))

(defun supertag-rag--retrieve-context-from-ids (node-ids)
  "Retrieve the content of nodes based on a list of node IDs."
  (if (not node-ids)
      ""
    (let ((context-parts '())
          (max-context-length supertag-rag-max-context-length))
      (dolist (id node-ids)
        (let ((node-data (supertag-store-direct-get :nodes id)))
          ;; Debug: Print node data to messages buffer
          (message "Supertag RAG Debug: Node ID %s data: %s" id node-data)
          (if (null node-data)
              (message "Supertag RAG Debug: Node ID %s not found in store" id)
            (let* ((raw-title (plist-get node-data :title))
                   (raw-content (plist-get node-data :content))
                   (created-at (plist-get node-data :created-at))
                   (tags (plist-get node-data :tags))
                   (title (cond
                          ((stringp raw-title) raw-title)
                          ((null raw-title) "Untitled")
                          (t (format "%s" raw-title))))
                   (content (cond
                            ((stringp raw-content) raw-content)
                            ((null raw-content) "")
                            (t (format "%s" raw-content)))))
              ;; Debug: Print content and title
              (message "Supertag RAG Debug: Node ID %s title: %s, content type: %s, content length: %d" 
                       id title (type-of raw-content) (length content))
              (if (and content (> (length content) 0))
                  (let ((excerpt (if (> (length content) supertag-rag-max-document-excerpt)
                                    (substring content 0 supertag-rag-max-document-excerpt)
                                  content)))
                    (push (format "--- Document: %s ---\nTags: %s\nCreated: %s\n%s"
                                  title
                                  (if tags (mapconcat #'identity tags ", ") "None")
                                  (if created-at (format-time-string "%Y-%m-%d %H:%M" created-at) "Unknown")
                                  excerpt)
                          context-parts))
                (message "Supertag RAG Debug: Node ID %s has no content or empty content" id))))))
      (let ((context (mapconcat #'identity (nreverse context-parts) "\n\n")))
        ;; Debug: Print total context length
        (message "Supertag RAG Debug: Total context length: %d" (length context))
        (message "Supertag RAG Debug: Final context content:\n%s" context)
        ;; Ensure total context doesn't exceed limit
        (if (> (length context) max-context-length)
            (substring context 0 max-context-length)
          context)))))

(defun supertag-rag--enhance-context-with-metadata (context)
  "Enhance the context with metadata to help the LLM understand the relevance."
  (format "The following notes were retrieved from the user's personal knowledge base in response to their question. These notes are RELEVANT and RECENTLY recorded by the user. Pay special attention to them when answering the question.\n\n%s" context))

(defun supertag-rag--create-streaming-display-callback ()
  "Create a callback function that handles streaming responses."
  (let* ((buffer (get-buffer-create "*Supertag RAG Answer*"))
         (first-chunk-p t)
         (error-occurred nil))
    (with-current-buffer buffer
      (when (fboundp 'markdown-mode) (markdown-mode)))

    (lambda (content metadata)
      ;; Ensure content is a string or nil
      (let ((content-str (if (stringp content) content (if content (format "%s" content) ""))))
        ;; Handle errors in the stream
        (when (and metadata (plist-get metadata :error))
          (setq error-occurred t)
          (message "Supertag RAG: Error in response stream: %s" (plist-get metadata :error)))
        
        (let ((is-done (and metadata (plist-get metadata :done))))
          (when (and (not (string-empty-p content-str)) (not error-occurred))
            (with-current-buffer buffer
              (when first-chunk-p
                (erase-buffer)
                (when supertag-rag-display-buffer-on-start
                  (display-buffer buffer))
                (setq first-chunk-p nil))
              (goto-char (point-max))
              (insert content-str)
              ;; Ensure the buffer is visible
              (when-let* ((win (get-buffer-window buffer)))
                (set-window-point win (point-max)))))
          
          (when (and is-done (not error-occurred))
            (message "Supertag RAG: Answer stream complete."))
          
          (when (and is-done error-occurred)
            (message "Supertag RAG: Answer stream completed with errors.")))))))

(defun supertag-rag--generate-answer (prompt callback stream-callback)
  "Generate an answer using gptel, correctly handling its streaming callback."
  (message "Supertag RAG Debug: Using dedicated buffer for gptel request.")
  (let ((buffer (get-buffer-create "*supertag-rag-temp-request*"))
        (response-parts '()))
    (with-current-buffer buffer
      (erase-buffer)
      (text-mode)
      (insert prompt))
    (apply #'gptel-request
           nil ; Prompt is nil, gptel reads from buffer
           (append `(:buffer ,buffer
                     :stream t
                     :callback ,(lambda (response-or-signal &rest _)
                                 "Handle both string chunks from the stream and the final `t` signal from the sentinel."
                                 (if (stringp response-or-signal)
                                     ;; Case 1: It's a string chunk from the stream filter.
                                     (progn
                                       (when stream-callback (funcall stream-callback response-or-signal))
                                       (push response-or-signal response-parts))
                                   ;; Case 2: It's the final signal (t) from the stream sentinel.
                                   (when (eq response-or-signal t)
                                     (when callback
                                       (let ((final-response (string-join (nreverse response-parts) "")))
                                         (funcall callback final-response)))
                                     (kill-buffer buffer))))
                     )
                   (if supertag-rag-model `(:model ,supertag-rag-model) nil)))))

(defun supertag-rag--display-answer (answer)
  "Display the final answer in a dedicated buffer."
  (let ((buffer (get-buffer-create "*Supertag RAG Answer*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert answer)
      ;; For better viewing of Markdown
      (when (fboundp 'markdown-mode)
        (markdown-mode)))
    (display-buffer buffer)))

(defun supertag-rag--validate-sexp (sexp)
  "Validate the generated S-expression to ensure it follows the correct format."
  (cond
   ((null sexp) nil)
   ((not (listp sexp)) nil)
   (t
    (catch 'invalid
      (supertag-rag--validate-sexp-helper sexp)
      t))))

(defun supertag-rag--validate-sexp-helper (sexp)
  "Helper function to recursively validate S-expression."
  (when (listp sexp)
    (let ((op (car sexp))
          (generic-nouns '("note" "notes" "document" "documents" "article" "articles" 
                           "task" "tasks" "entry" "entries" "list" "show" "find" "provide" 
                           "give" "access" "where" "which" "what" "how" "can" "could" 
                           "would" "should" "will" "do" "does" "did" "may" "might" 
                           "why" "when" "whom" "whose" "笔记" "文章" "任务" "条目" 
                           "列表" "显示" "查找" "提供" "给我" "访问" "哪里" "哪些" 
                           "什么" "如何" "能够" "可以" "为什么" "什么时候" "谁" "怎样" 
                           "是否" "帮忙" "帮助" "请" "总结" "帮我总结" "总结一下" 
                           "summarize" "summarize for me" "help me" "help" "please" 
                           "give me a summary" "provide summary" "summary")))
      (cond
       ((eq op 'and)
        (dolist (subexpr (cdr sexp))
          (supertag-rag--validate-sexp-helper subexpr)))
       ((eq op 'or)
        (dolist (subexpr (cdr sexp))
          (supertag-rag--validate-sexp-helper subexpr)))
       ((eq op 'not)
        (when (> (length sexp) 2)
          (throw 'invalid nil))
        (supertag-rag--validate-sexp-helper (cadr sexp)))
       ((eq op 'term)
        (when (or (null (cadr sexp)) (not (stringp (cadr sexp))))
          (throw 'invalid nil))
        ;; Additional check: ensure term is not a generic noun or action phrase
        (when (or (member (downcase (cadr sexp)) generic-nouns)
                  (supertag-rag--contains-action-phrase (cadr sexp) generic-nouns))
          (throw 'invalid nil)))
       ((eq op 'tag)
        (when (or (null (cadr sexp)) (not (stringp (cadr sexp))))
          (throw 'invalid nil)))
       ((eq op 'after)
        (when (or (null (cadr sexp)) (not (stringp (cadr sexp))))
          (throw 'invalid nil)))
       ((eq op 'before)
        (when (or (null (cadr sexp)) (not (stringp (cadr sexp))))
          (throw 'invalid nil)))
       ((eq op 'between)
        (when (or (null (cadr sexp)) (not (stringp (cadr sexp)))
                  (null (caddr sexp)) (not (stringp (caddr sexp))))
          (throw 'invalid nil)))
       ((null op) nil)
       (t (throw 'invalid nil))))))

(defun supertag-rag--remove-generic-terms (expr generic-nouns)
  "Recursively remove generic terms from the query expression."
  (cond
   ((null expr) nil)
   ((atom expr) expr)
   ((eq (car expr) 'term)
    (let ((term-value (cadr expr)))
      (if (and (stringp term-value)
               (or (member (downcase term-value) generic-nouns)
                   (supertag-rag--contains-action-phrase term-value generic-nouns)))
          nil  ; Remove this term entirely
        expr)))
   ((eq (car expr) 'and)
    (let ((filtered-terms (cl-remove-if-not 
                          (lambda (x) (not (null x)))
                          (mapcar (lambda (x) (supertag-rag--remove-generic-terms x generic-nouns))
                                  (cdr expr)))))
      (cond
       ((null filtered-terms) nil)
       ((= (length filtered-terms) 1) (car filtered-terms))
       (t (cons 'and filtered-terms)))))
   ((eq (car expr) 'or)
    (let ((filtered-terms (cl-remove-if-not 
                          (lambda (x) (not (null x)))
                          (mapcar (lambda (x) (supertag-rag--remove-generic-terms x generic-nouns))
                                  (cdr expr)))))
      (cond
       ((null filtered-terms) nil)
       ((= (length filtered-terms) 1) (car filtered-terms))
       (t (cons 'or filtered-terms)))))
   (t
    (cons (car expr) 
          (mapcar (lambda (x) (supertag-rag--remove-generic-terms x generic-nouns))
                  (cdr expr))))))

(defun supertag-rag--contains-action-phrase (term-value generic-nouns)
  "Check if the term contains any action phrases."
  (catch 'found
    (dolist (phrase generic-nouns)
      (when (and (stringp phrase) (string-match (regexp-quote phrase) (downcase term-value)))
        (throw 'found t)))
    nil))

(defun supertag-rag--optimize-query (query)
  "Optimize the query by converting inappropriate term queries to tag queries."
  (supertag-rag--convert-terms-to-tags query))

(defun supertag-rag--convert-terms-to-tags (expr)
  "Convert term expressions to tag expressions when appropriate."
  (let ((common-tags '("work" "urgent" "important" "todo" "task" "project" 
                       "meeting" "note" "document" "article" "plan" "idea"
                       "question" "answer" "solution" "bug" "feature" "fix"
                       "reminder" "schedule" "event" "goal" "objective"
                       "工作" "紧急" "重要" "待办" "任务" "项目" "会议" 
                       "笔记" "文档" "文章" "计划" "想法" "问题" "答案" 
                       "解决方案" "错误" "功能" "修复" "提醒" "日程" "事件" 
                       "目标" "目的")))
    (cond
     ((null expr) nil)
     ((atom expr) expr)
     ((eq (car expr) 'term)
      (let ((term-value (cadr expr)))
        (if (and (stringp term-value)
                 (member (downcase term-value) common-tags))
            (list 'tag term-value)  ; Convert to tag query
          expr)))
     ((eq (car expr) 'and)
      (cons 'and 
            (mapcar (lambda (x) (supertag-rag--convert-terms-to-tags x))
                    (cdr expr))))
     ((eq (car expr) 'or)
      (cons 'or 
            (mapcar (lambda (x) (supertag-rag--convert-terms-to-tags x))
                    (cdr expr))))
     ((eq (car expr) 'not)
      (cons 'not 
            (list (supertag-rag--convert-terms-to-tags (cadr expr)))))
     (t
      (cons (car expr) 
            (mapcar (lambda (x) (supertag-rag--convert-terms-to-tags x))
                    (cdr expr)))))))

(defun supertag-rag-test-prompt (context question)
  "Test the RAG prompt with given context and question."
  (interactive "sContext: \nsQuestion: ")
  (let ((prompt (format supertag-rag-final-answer-prompt context question)))
    (message "Prompt:\n%s" prompt)
    (when (yes-or-no-p "Send this prompt to LLM for testing? ")
      (supertag-rag--generate-final-answer question context nil))))

(provide 'supertag-rag)
;;; supertag-rag.el ends here

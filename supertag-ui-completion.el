;;; supertag-ui-completion.el --- Enhanced tag completion for org-supertag -*- lexical-binding: t; -*-

;; Enhanced, robust tag completion system for org-supertag.
;; Features: intelligent caching, optimized triggers, robust boundary detection.

;;; Code:

(require 'org)
(require 'org-id nil t)
(require 'cl-lib)

;; Optional requires
(require 'supertag-ops-tag nil t)
(require 'supertag-ops-node nil t)
(require 'supertag-services-query nil t)

;;; --- Caching System ---

(defvar supertag-completion--cache
  (make-hash-table :test 'eq)
  "Cache for completion candidates by type.")

(defvar supertag-completion--cache-timestamp
  (make-hash-table :test 'eq)
  "Timestamp for cache entries.")

(defvar supertag-completion--cache-ttl 30
  "Cache time-to-live in seconds.")

(defun supertag-completion--cache-valid-p (type)
  "Check if cache for TYPE is still valid."
  (when-let ((timestamp (gethash type supertag-completion--cache-timestamp)))
    (< (float-time (time-subtract (current-time) timestamp)) 
       supertag-completion--cache-ttl)))

(defun supertag-completion--get-cached-candidates (type)
  "Get cached candidates for TYPE, refreshing if needed."
  (unless (supertag-completion--cache-valid-p type)
    (supertag-completion--refresh-cache type))
  (gethash type supertag-completion--cache '()))

(defun supertag-completion--refresh-cache (type)
  "Refresh cache for TYPE with fresh data."
  (let ((candidates (supertag-completion--fetch-candidates type)))
    (puthash type candidates supertag-completion--cache)
    (puthash type (current-time) supertag-completion--cache-timestamp)
    candidates))

(defun supertag-completion--fetch-candidates (type)
  "Fetch fresh candidates for TYPE from the store using multiple strategies."
  (condition-case err
      (pcase type
        (:tag
         ;; Strategy 1: Try supertag-query first
         (or (when (fboundp 'supertag-query)
               (let ((result (supertag-query '(:tags))))
                 (mapcar #'car result)))
             ;; Strategy 2: Scan all nodes and collect unique tags
             (supertag-completion--get-tags-from-nodes)
             ;; Strategy 3: Provide helpful fallback suggestions
             '("project" "task" "idea" "note" "todo" "important" "urgent" "work" "personal")))
        (:node
         ;; Strategy 1: Try supertag-query first
         (or (when (fboundp 'supertag-query)
               (let ((result (supertag-query '(:nodes))))
                 (delq nil (mapcar (lambda (node-pair)
                                    (plist-get (cdr node-pair) :title))
                                   result))))
             ;; Strategy 2: Direct store access
             (supertag-completion--get-nodes-direct)
             ;; Strategy 3: Provide fallback
             '()))
        (_ '()))
    (error
     (message "supertag-completion: Failed to fetch %s candidates: %S" type err)
     '())))

(defun supertag-completion--get-tags-from-nodes ()
  "Extract all unique tag names from nodes by scanning the store."
  (let ((nodes-ht (supertag-get '(:nodes)))
        (all-tags '()))
    (when (hash-table-p nodes-ht)
      (maphash (lambda (_node-id node-data)
                 (when-let ((tags (plist-get node-data :tags)))
                   (setq all-tags (append tags all-tags))))
               nodes-ht))
    (delete-dups all-tags)))

(defun supertag-completion--get-nodes-direct ()
  "Get all node titles by direct store access."
  (let ((nodes-ht (supertag-get '(:nodes)))
        (all-titles '()))
    (when (hash-table-p nodes-ht)
      (maphash (lambda (_node-id node-data)
                 (when-let ((title (plist-get node-data :title)))
                   (push title all-titles)))
               nodes-ht))
    all-titles))

(defun supertag-completion--invalidate-cache (&optional type)
  "Invalidate cache for TYPE, or all types if TYPE is nil."
  (if type
      (progn
        (remhash type supertag-completion--cache)
        (remhash type supertag-completion--cache-timestamp))
    (clrhash supertag-completion--cache)
    (clrhash supertag-completion--cache-timestamp)))

;;; --- Robust Boundary Detection ---

(defun supertag-completion--tag-bounds ()
  "Return (START . END) for tag name after `#`, with robust detection."
  (save-excursion
    (let ((end (point))
          (start nil))
      ;; Look backward for # character
      (when (re-search-backward "#\\([a-zA-Z0-9_-]*\\)" (line-beginning-position) t)
        (let ((hash-pos (match-beginning 0))
              (tag-start (match-beginning 1))
              (tag-end (match-end 1)))
          ;; Ensure we're still within the tag being typed
          (when (and (<= hash-pos end) (<= end tag-end))
            (setq start (if (= end tag-end) tag-start (1+ hash-pos)))
            (cons start end)))))))

(defun supertag-completion--node-bounds ()
  "Return (START . END) for node link after `[[`, with robust detection."
  (save-excursion
    (let ((end (point)))
      ;; Look for [[ pattern before current point
      (when (re-search-backward "\\[\\[\\([^]]*\\)" (line-beginning-position) t)
        (let ((bracket-pos (match-beginning 0))
              (content-start (match-beginning 1))
              (content-end (match-end 1)))
          ;; Ensure we're still within the link being typed and no closing ]]
          (when (and (<= bracket-pos end) 
                     (<= end content-end)
                     (not (re-search-forward "\\]\\]" end t)))
            (cons content-start end)))))))

;;; --- Smart Triggering ---

(defvar supertag-completion--last-trigger-time 0
  "Timestamp of last completion trigger.")

(defvar supertag-completion--trigger-debounce 0.1
  "Minimum seconds between completion triggers.")

(defun supertag-completion--should-trigger-p ()
  "Check if completion should be triggered based on context and timing."
  (and (derived-mode-p 'org-mode)
       (let ((now (float-time)))
         (when (> (- now supertag-completion--last-trigger-time) 
                  supertag-completion--trigger-debounce)
           (setq supertag-completion--last-trigger-time now)
           t))))

(defun supertag-completion--smart-trigger ()
  "Smart completion trigger with debouncing and context awareness."
  (when (supertag-completion--should-trigger-p)
    (let ((char-before (char-before))
          (context-valid nil))
      
      ;; Check for tag completion context
      (when (eq char-before ?#)
        (setq context-valid t))
      
      ;; Check for node completion context  
      (when (and (eq char-before ?\[)
                 (eq (char-before (1- (point))) ?\[))
        (setq context-valid t))
      
      ;; Trigger completion if context is valid
      (when context-valid
        (completion-at-point)))))

;;; --- Enhanced CAPF ---

(defun supertag-completion--capf ()
  "Enhanced completion-at-point function with caching and error handling."
  (or
   ;; Node completion
   (when-let ((bounds (supertag-completion--node-bounds)))
     (list (car bounds) (cdr bounds)
           (supertag-completion--get-cached-candidates :node)
           ;; Use a custom action function to override default insertion.
           :action-function #'supertag-completion--node-action-function
           :category 'supertag-node
           :annotation-function (lambda (_) " [node]")))
   
   ;; Tag completion
   (when-let ((bounds (supertag-completion--tag-bounds)))
     (list (car bounds) (cdr bounds)
           (supertag-completion--get-cached-candidates :tag)
           :exit-function #'supertag-completion--tag-exit-function
           :category 'supertag-tag
           :annotation-function (lambda (_) " [tag]")))))

;;; --- Exit Functions ---

(defun supertag-completion--get-node-id-at-point ()
  "Find the node ID for the current context, with error handling."
  (condition-case err
      (save-excursion
        (unless (org-at-heading-p)
          (org-back-to-heading t))
        (unless (org-at-heading-p)
          (user-error "Not within an Org heading's scope"))
        (org-id-get-create))
    (error
     (message "supertag-completion: Failed to get node ID: %S" err)
     nil)))

(defun supertag-completion--tag-exit-function (candidate _status)
  "Enhanced tag exit function with proper error handling."
  (condition-case err
      (when-let ((node-id (supertag-completion--get-node-id-at-point)))
        (when (fboundp 'supertag-ops-add-tag-to-node)
          (supertag-ops-add-tag-to-node node-id candidate :create-if-needed t)
          (message "Tag '%s' added to node %s" candidate node-id))
        ;; Add space after tag for better UX
        (unless (looking-at-p "\\s-")
          (insert " "))
        ;; Invalidate cache since we added a new tag relationship
        (supertag-completion--invalidate-cache :tag))
    (error
     (message "supertag-completion: Tag completion error: %S" err))))

(defun supertag-completion--node-action-function (candidate)
  "Custom action function for node completion.
Replaces the completion prefix with a full org-id link."
  (let ((bounds (supertag-completion--node-bounds)))
    (when bounds
      (let ((start (car bounds))
            (end (cdr bounds)))
        ;; 1. Delete the text being completed (e.g., "my-no")
        (delete-region start end)
        ;; 2. Also delete the `[[` prefix
        (save-excursion
          (goto-char start)
          (when (re-search-backward "\\[\\[" (line-beginning-position) t)
            (delete-region (match-beginning 0) start)))
        
        ;; 3. Find the node-id for the selected candidate
        (let* ((nodes (supertag-query '(:nodes)))
               (node-pair (cl-find candidate nodes
                                   :key (lambda (p) (plist-get (cdr p) :title))
                                   :test #'string=))
               (node-id (car node-pair))
               (from-node-id (supertag-completion--get-node-id-at-point)))
          
          (if node-id
              (progn
                ;; 4. Insert the full link
                (insert (format "[[id:%s][%s]]" node-id candidate))
                
                ;; 5. Add relation automatically if inside a node
                (when (and from-node-id node-id (fboundp 'supertag-ops-add-relation))
                  (supertag-ops-add-relation from-node-id node-id '("references"))
                  (message "Reference added from %s to %s" from-node-id node-id)))
            ;; Fallback: if node not found (should be rare), just insert candidate
            (insert candidate)
            (message "supertag-completion: Could not find node ID for '%s'" candidate)))))))

;;; --- Minor Mode ---

;;;###autoload
(define-minor-mode supertag-ui-completion-mode
  "Enhanced tag completion for org-supertag with intelligent caching."
  :lighter " ST-C+"
  (if supertag-ui-completion-mode
      (progn
        (add-hook 'completion-at-point-functions #'supertag-completion--capf nil t)
        (add-hook 'post-self-insert-hook #'supertag-completion--smart-trigger nil t)
        ;; Initialize cache
        (supertag-completion--invalidate-cache))
    (remove-hook 'completion-at-point-functions #'supertag-completion--capf t)
    (remove-hook 'post-self-insert-hook #'supertag-completion--smart-trigger t)
    ;; Clear cache
    (supertag-completion--invalidate-cache)))

;;;###autoload
(defun supertag-ui-completion-enable ()
  "Enable enhanced tag completion in org-mode buffers."
  (when (derived-mode-p 'org-mode)
    (supertag-ui-completion-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-supertag-ui-completion-mode
  supertag-ui-completion-mode
  supertag-ui-completion-enable)

;;; --- Manual Cache Management ---

;;;###autoload
(defun supertag-completion-refresh-cache ()
  "Manually refresh completion cache."
  (interactive)
  (supertag-completion--invalidate-cache)
  (message "supertag-completion: Cache refreshed"))

;;;###autoload
(defun supertag-completion-show-cache-stats ()
  "Show completion cache statistics."
  (interactive)
  (let ((tag-count (length (gethash :tag supertag-completion--cache '())))
        (node-count (length (gethash :node supertag-completion--cache '())))
        (tag-age (when-let ((ts (gethash :tag supertag-completion--cache-timestamp)))
                   (float-time (time-subtract (current-time) ts))))
        (node-age (when-let ((ts (gethash :node supertag-completion--cache-timestamp)))
                    (float-time (time-subtract (current-time) ts)))))
    (message "Cache: %d tags (%.1fs old), %d nodes (%.1fs old)"
             tag-count (or tag-age 0)
             node-count (or node-age 0))))

(provide 'supertag-ui-completion)

;;; supertag-ui-completion.el ends here
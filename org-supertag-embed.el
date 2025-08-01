;;; org-supertag-embed.el --- Embed blocks for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides embed block functionality for org-supertag:
;; - Embed node content into other files
;; - Embed query results into files
;; - Bidirectional synchronization
;; - Automatic refresh on source changes
;; - Direct buffer parsing for block identification (no text properties)

;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'org-supertag-db)
(require 'org-supertag-node)
(require 'org-supertag-util)

;;------------------------------------------------------------------------------
;; New Helper Functions for Direct Buffer Parsing
;;------------------------------------------------------------------------------

(defun org-supertag-embed--find-block-by-id (embed-id)
  "Find embed block by its ID using regex search.
EMBED-ID: The embed block identifier to find.
Returns (start . end) for the entire block (including begin/end markers), or nil if not found."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+begin_embed_node: \\(.*\\) %s" 
                                    (regexp-quote embed-id)) nil t)
      (let ((start (match-beginning 0)))
        (when (re-search-forward "^#\\+end_embed_node" nil t)
          (cons start (match-end 0)))))))

(defun org-supertag-embed--get-id-at-point ()
  "Identify which embed block the cursor is currently inside.
Returns the embed-id if inside a block, nil otherwise."
  (save-excursion
    (let ((original-point (point))
          (found-id nil))
      ;; First try: Search backwards for the nearest begin marker
      ;; More flexible regex to handle quoted and unquoted source-id/query
      (when (re-search-backward "^#\\+begin_embed_node: \\(.*?\\) \\([a-zA-Z0-9_-]+\\)" nil t)
        (let ((block-start (match-beginning 0))
              (embed-id (match-string 2))
              (block-type (match-string 1)))
          ;; Search forward for the corresponding end marker
          (when (re-search-forward "^#\\+end_embed_node" nil t)
            (let ((block-end-pos (match-end 0)))
              ;; More lenient boundary check - allow cursor on the markers themselves
              (when (and (>= original-point block-start) ; Original point is after or at begin tag
                        (<= original-point block-end-pos))
                (setq found-id embed-id))))))
      
      ;; Second try: If not found, search forward from beginning of buffer
      (unless found-id
        (goto-char (point-min))
        (while (and (not found-id) 
                    (re-search-forward "^#\\+begin_embed_node: \\(.*?\\) \\([a-zA-Z0-9_-]+\\)" nil t))
          (let ((block-start (match-beginning 0))
                (embed-id (match-string 2)))
            (when (re-search-forward "^#\\+end_embed_node" nil t)
              (let ((block-end-pos (match-end 0)))
                (when (and (>= original-point block-start)
                          (<= original-point block-end-pos))
                  (setq found-id embed-id)))))))
      
      found-id)))

;;------------------------------------------------------------------------------
;; Independent Embed Sync Database Structure
;;------------------------------------------------------------------------------

;; Main embed sync database structure
(cl-defstruct (org-supertag-embed-sync-db
               (:constructor org-supertag-embed-sync-db-create)
               (:copier org-supertag-embed-sync-db-copy))
  "Embed synchronization database structure."
  (version "1.0")
  (timestamp (current-time))
  (embeds (make-hash-table :test 'equal))        ; Embed entries
  (sync-states (make-hash-table :test 'equal))   ; Sync state tracking
  (file-mappings (make-hash-table :test 'equal)) ; File to embed mappings
  (modified nil)
  (dirty nil)
  (id-counter 0))

;; Embed entry structure
(cl-defstruct (org-supertag-embed-entry
               (:constructor org-supertag-embed-entry-create)
               (:copier org-supertag-embed-entry-copy))
  "Embed entry structure for synchronization."
  (id nil :type string)                           ; Unique embed ID
  (source-type nil :type symbol)                  ; :node or :query
  (source-id nil :type string)                    ; Source node ID or query string
  (embedded-file nil :type string)                ; File containing embed
  (embedded-pos nil :type number)                 ; Position in embedded file
  (source-file nil :type string)                  ; Source file path (for nodes)
  (source-pos nil :type number)                   ; Source position (for nodes)
  (content-hash nil :type string)                 ; Hash of current content
  (source-hash nil :type string)                  ; Hash of source content
  (created nil)                                   ; Creation time
  (modified nil)                                  ; Last modification time
  (sync-status 'synced :type symbol)             ; :synced, :dirty, :conflict
  (user-data nil))                                ; User data for extensions

;; Global embed sync database instance
(defvar org-supertag-embed-sync-db nil "The embed sync database.")

(defvar org-supertag-embed-sync-db-file
  (expand-file-name "org-supertag-embed-sync.el" org-supertag-data-directory)
  "The embed sync database file.")

(defun org-supertag-embed--save-db ()
  "Save the embed sync database to its file."
  (with-temp-file org-supertag-embed-sync-db-file
    (prin1 org-supertag-embed-sync-db (current-buffer))))

(defun org-supertag-embed--load-db ()
  "Load the embed sync database from its file."
  (when (file-exists-p org-supertag-embed-sync-db-file)
    (with-temp-buffer
      (insert-file-contents org-supertag-embed-sync-db-file)
      (condition-case nil
          (read (current-buffer))
        (end-of-file nil)))))

(defun org-supertag-embed-init-db ()
  "Initialize the embed sync database."
  (if (file-exists-p org-supertag-embed-sync-db-file)
      (setq org-supertag-embed-sync-db (or (org-supertag-embed--load-db)
                                           (org-supertag-embed-sync-db-create)))
    (setq org-supertag-embed-sync-db (org-supertag-embed-sync-db-create)))
  
  ;; Ensure the embeds field is properly initialized as a hash table
  (when (and org-supertag-embed-sync-db
             (not (hash-table-p (org-supertag-embed-sync-db-embeds org-supertag-embed-sync-db))))
    (setf (org-supertag-embed-sync-db-embeds org-supertag-embed-sync-db)
          (make-hash-table :test 'equal)))
  
  ;; Also ensure other hash table fields are properly initialized
  (when org-supertag-embed-sync-db
    (when (not (hash-table-p (org-supertag-embed-sync-db-sync-states org-supertag-embed-sync-db)))
      (setf (org-supertag-embed-sync-db-sync-states org-supertag-embed-sync-db)
            (make-hash-table :test 'equal)))
    (when (not (hash-table-p (org-supertag-embed-sync-db-file-mappings org-supertag-embed-sync-db)))
      (setf (org-supertag-embed-sync-db-file-mappings org-supertag-embed-sync-db)
            (make-hash-table :test 'equal)))))

(defun org-supertag-embed-sync-db-save ()
  "Save the embed sync database."
  (org-supertag-embed--save-db))

(defun org-supertag-embed-sync-add-entry (embed-id source-id &optional options)
  "Add a new embed entry to the sync database.
EMBED-ID: Unique identifier for this embed block
SOURCE-ID: ID of the source node
OPTIONS: Optional plist of additional options"
  (let* ((embedded-file (buffer-file-name))
         (embedded-pos (point))
         (now (current-time))
         ;; Get source node info from database
         (node-props (org-supertag-db-get source-id))
         (source-file (plist-get node-props :file-path))
         (source-pos (plist-get node-props :pos))
         (entry (org-supertag-embed-entry-create
                :id embed-id
                :source-type :node
                :source-id source-id
                :embedded-file embedded-file
                :embedded-pos embedded-pos
                :source-file source-file
                :source-pos source-pos
                :created now
                :modified now
                :sync-status 'synced)))
    
    ;; Store in database
    (puthash embed-id entry (org-supertag-embed-sync-db-embeds org-supertag-embed-sync-db))
    
    ;; Update file mappings
    (let ((file-embeds (gethash embedded-file (org-supertag-embed-sync-db-file-mappings org-supertag-embed-sync-db))))
      (unless file-embeds
        (setq file-embeds '()))
      (push embed-id file-embeds)
      (puthash embedded-file file-embeds (org-supertag-embed-sync-db-file-mappings org-supertag-embed-sync-db)))
    
    ;; Mark database as modified
    (setf (org-supertag-embed-sync-db-modified org-supertag-embed-sync-db) t)
    (setf (org-supertag-embed-sync-db-dirty org-supertag-embed-sync-db) t)
    
    (org-supertag-embed-sync-db-save)
    
    entry))

(defun org-supertag-embed-sync-get-entry (embed-id)
  "Get embed entry from the sync database.
EMBED-ID: The embed block identifier"
  (gethash embed-id (org-supertag-embed-sync-db-embeds org-supertag-embed-sync-db)))

(defun org-supertag-embed-sync-update-entry (embed-id &rest updates)
  "Update embed entry in the sync database.
EMBED-ID: The embed block identifier
UPDATES: Property-value pairs to update"
  (let ((entry (org-supertag-embed-sync-get-entry embed-id)))
    (when entry
      ;; Apply updates
      (while updates
        (let ((key (pop updates))
              (value (pop updates)))
          (pcase key
            (:id (setf (org-supertag-embed-entry-id entry) value))
            (:source-type (setf (org-supertag-embed-entry-source-type entry) value))
            (:source-id (setf (org-supertag-embed-entry-source-id entry) value))
            (:embedded-file (setf (org-supertag-embed-entry-embedded-file entry) value))
            (:embedded-pos (setf (org-supertag-embed-entry-embedded-pos entry) value))
            (:source-file (setf (org-supertag-embed-entry-source-file entry) value))
            (:source-pos (setf (org-supertag-embed-entry-source-pos entry) value))
            (:content-hash (setf (org-supertag-embed-entry-content-hash entry) value))
            (:source-hash (setf (org-supertag-embed-entry-source-hash entry) value))
            (:created (setf (org-supertag-embed-entry-created entry) value))
            (:modified (setf (org-supertag-embed-entry-modified entry) value))
            (:sync-status (setf (org-supertag-embed-entry-sync-status entry) value))
            (:user-data (setf (org-supertag-embed-entry-user-data entry) value))
            (_ (error "Unknown slot: %s" key)))))
      
      ;; Always update modified time
      (setf (org-supertag-embed-entry-modified entry) (current-time))
      
      ;; Mark database as modified
      (setf (org-supertag-embed-sync-db-modified org-supertag-embed-sync-db) t)
      (setf (org-supertag-embed-sync-db-dirty org-supertag-embed-sync-db) t)
      
      entry)))

(defun org-supertag-embed-sync-remove-entry (embed-id)
  "Remove embed entry from the sync database.
EMBED-ID: The embed block identifier to remove"
  (let ((entry (org-supertag-embed-sync-get-entry embed-id)))
    (when entry
      ;; Remove from file mappings
      (let* ((embedded-file (org-supertag-embed-entry-embedded-file entry))
             (file-embeds (gethash embedded-file (org-supertag-embed-sync-db-file-mappings org-supertag-embed-sync-db))))
        (when file-embeds
          (setq file-embeds (delete embed-id file-embeds))
          (if file-embeds
              (puthash embedded-file file-embeds (org-supertag-embed-sync-db-file-mappings org-supertag-embed-sync-db))
            (remhash embedded-file (org-supertag-embed-sync-db-file-mappings org-supertag-embed-sync-db)))))
      
      ;; Remove from main database
      (remhash embed-id (org-supertag-embed-sync-db-embeds org-supertag-embed-sync-db))
      
      ;; Mark database as modified
      (setf (org-supertag-embed-sync-db-modified org-supertag-embed-sync-db) t)
      (setf (org-supertag-embed-sync-db-dirty org-supertag-embed-sync-db) t))))

(defun org-supertag-embed-sync-get-file-embeds (file-path)
  "Get all embed IDs for a specific file.
FILE-PATH: The file path to look up"
  (gethash file-path (org-supertag-embed-sync-db-file-mappings org-supertag-embed-sync-db)))

;;------------------------------------------------------------------------------
;; Content Hash Management
;;------------------------------------------------------------------------------

(defun org-supertag-embed-sync-calculate-content-hash (content)
  "Calculate hash for embed content.
CONTENT: The content string to hash"
  (md5 content))

;;------------------------------------------------------------------------------
;; Safe Maphash Helper
;;------------------------------------------------------------------------------

(defun org-supertag-embed--get-embeds-table ()
  "Get the embeds table from the sync database with proper initialization."
  (unless org-supertag-embed-sync-db
    (org-supertag-embed-init-db))
  (when (and org-supertag-embed-sync-db
             (org-supertag-embed-sync-db-embeds org-supertag-embed-sync-db)
             (hash-table-p (org-supertag-embed-sync-db-embeds org-supertag-embed-sync-db)))
    (org-supertag-embed-sync-db-embeds org-supertag-embed-sync-db)))

(defun org-supertag-embed--safe-map-embeds (func)
  "Safely map over the embeds table with proper error checking.
FUNC: Function that takes two arguments (key entry)

Returns t if successful, nil if embeds table is invalid or empty."
    (let ((table (org-supertag-embed--get-embeds-table)))
      (unless (functionp func)
      (error "First argument must be a function, got: %s" func))
    (when (and table 
               (hash-table-p table) 
               (> (hash-table-count table) 0))
      (maphash (lambda (key value)
                  (funcall func key value))
                table)
      t)))

(cl-defun org-supertag-embed-sync-calculate-source-hash (entry)
  "Calculate hash for source content.
ENTRY: The embed entry"
  (unless entry
    (cl-return-from org-supertag-embed-sync-calculate-source-hash nil))
  (let ((source-id (org-supertag-embed-entry-source-id entry)))
    (when-let ((source-node (org-supertag-db-get source-id)))
      (org-supertag-node-content-hash source-node))))

(defun org-supertag-embed-sync-update-hashes (embed-id content-hash source-hash)
  "Update hash values for embed entry.
EMBED-ID: The embed block identifier
CONTENT-HASH: Hash of embed content
SOURCE-HASH: Hash of source content"
  (let ((entry (org-supertag-embed-sync-get-entry embed-id)))
    (when entry
      (setf (org-supertag-embed-entry-content-hash entry) content-hash)
      (setf (org-supertag-embed-entry-source-hash entry) source-hash)
      (setf (org-supertag-embed-entry-modified entry) (current-time))
      (setf (org-supertag-embed-sync-db-modified org-supertag-embed-sync-db) t)
      (setf (org-supertag-embed-sync-db-dirty org-supertag-embed-sync-db) t))))

;;------------------------------------------------------------------------------
;; Sync State Detection
;;------------------------------------------------------------------------------

(cl-defun org-supertag-embed-sync-detect-changes (embed-id &optional buffer)
  "Detect changes for the given embed block.
EMBED-ID: The embed block identifier
BUFFER: Optional buffer to operate on. If nil, uses current buffer.

Returns a plist with :content-changed and :source-changed flags."
  (let* ((entry (org-supertag-embed-sync-get-entry embed-id)))

    (unless entry
      (cl-return-from org-supertag-embed-sync-detect-changes nil))
    (with-current-buffer (or buffer (current-buffer))
      (let* ((current-content-hash (org-supertag-embed-sync-calculate-content-hash
                                    (org-supertag-embed-get-content-at-point embed-id)))
             (current-source-hash (org-supertag-embed-sync-calculate-source-hash entry))
             (last-content-hash (org-supertag-embed-entry-content-hash entry))
             (last-source-hash (org-supertag-embed-entry-source-hash entry)))

        (let ((content-changed (and current-content-hash last-content-hash
                                    (not (equal current-content-hash last-content-hash))))
              (source-changed (and current-source-hash last-source-hash
                                   (not (equal current-source-hash last-source-hash)))))

          (list :content-changed content-changed
                :source-changed source-changed
                :current-content-hash current-content-hash
                :current-source-hash current-source-hash))))))

;;------------------------------------------------------------------------------
;; Content Extraction Helper
;;------------------------------------------------------------------------------

(defun org-supertag-embed--get-inner-block-region (embed-id)
  "Get the inner content region of an embed block, excluding begin/end markers.
EMBED-ID: The embed block identifier
Returns (start . end) for the inner content region, or nil if not found."
  (let ((block-region (org-supertag-embed--find-block-by-id embed-id)))
    (when block-region
      (save-excursion
        (goto-char (car block-region)) ; Go to the start of the #+begin_embed line
        (forward-line 1) ; Move past the #+begin_embed line
        (let ((inner-start (point)))
          (goto-char (cdr block-region)) ; Go to the end of the #+end_embed line
          (re-search-backward "^#\\+end_embed_node" (car block-region) t) ; Find the start of the #+end_embed line
          (let ((inner-end (point)))
            (when (> inner-end inner-start)
              (cons inner-start inner-end))))))))

(defun org-supertag-embed-get-content-at-point (embed-id)
  "Get embed content at current point.
EMBED-ID: The embed block identifier"
  (let ((inner-block-region (org-supertag-embed--get-inner-block-region embed-id)))
    (if inner-block-region
        (buffer-substring-no-properties (car inner-block-region) (cdr inner-block-region))
      "")))

;;------------------------------------------------------------------------------
;; Embed Block Management
;;------------------------------------------------------------------------------

(defun org-supertag-embed-create-id ()
  "Generate a unique embed ID for a new embed block."
  (format "embed_%s_%s" 
          (format-time-string "%Y%m%d_%H%M%S")
          (random 10000)))

;;------------------------------------------------------------------------------
;; Content Filtering
;;------------------------------------------------------------------------------

(defun org-supertag-embed--filter-embed-markers (content)
  "Remove embed block markers and ID properties from content to prevent nested embeds and ID conflicts.
CONTENT: The content string to filter"
  (let ((filtered-content content))
    ;; Remove embed block begin markers
    (setq filtered-content 
          (replace-regexp-in-string "^#\\+begin_embed_node:.*$" "" filtered-content))
    ;; Remove embed block end markers
    (setq filtered-content 
          (replace-regexp-in-string "^#\\+end_embed_node.*$" "" filtered-content))
    ;; Remove ID properties to prevent conflicts
    (setq filtered-content 
          (replace-regexp-in-string "^[ \t]*:ID:[ \t]+.*$" "" filtered-content))
    ;; Remove empty PROPERTIES drawers
    (setq filtered-content 
          (replace-regexp-in-string "^[ \t]*:PROPERTIES:\n[ \t]*:END:\n" "" filtered-content))
    ;; Clean up multiple consecutive newlines
    (setq filtered-content 
          (replace-regexp-in-string "\n\n\n+" "\n\n" filtered-content))
    ;; Trim the result
    (string-trim filtered-content)))

;;------------------------------------------------------------------------------
;; Embed Content Generation
;;------------------------------------------------------------------------------

(cl-defun org-supertag-embed-generate-node-content (node-id)
  "Generate embed content for a single node.
NODE-ID: The node identifier to embed"
  (let* ((node-props (org-supertag-db-get node-id))
         (file-path (plist-get node-props :file-path))
         (_pos (plist-get node-props :pos))) ; pos is likely stale, don't use
    (unless node-props
      (cl-return-from org-supertag-embed-generate-node-content
        "** Error: Node not found in DB **"))

    ;; Get original text content directly from source file
    (if file-path
        (with-current-buffer (find-file-noselect file-path)
          ;; Force buffer to be up-to-date
          (revert-buffer t t t)
          (save-excursion
            (goto-char (point-min))
            ;; Search for the node by ID instead of relying on a stored position
            ;; which can become stale.
            (if (re-search-forward (format ":ID:[ \t]+%s" (regexp-quote node-id)) nil t)
                (progn
                  (org-back-to-heading t)
                  ;; Get only the content part, excluding PROPERTIES and ID
                  (let* ((heading-start (point))
                         (heading-line (buffer-substring-no-properties 
                                       (line-beginning-position) 
                                       (line-end-position)))
                         (content-start (progn
                                         ;; Skip the heading line
                                         (forward-line 1)
                                         ;; Skip PROPERTIES drawer if present
                                         (when (looking-at "[ \t]*:PROPERTIES:")
                                           (search-forward ":END:" nil t)
                                           (forward-line 1))
                                         (point)))
                         (content-end (save-excursion
                                       (org-end-of-subtree t t)
                                       (skip-chars-backward "\n\r\t ")
                                       (point))))
                    
                    ;; Construct the embedded content: heading + content (no properties)
                    (let* ((content-text (if (> content-end content-start)
                                            (buffer-substring-no-properties content-start content-end)
                                          ""))
                           (full-content (concat heading-line "\n" content-text))
                           (normalized-content
                            (replace-regexp-in-string "\n*\\'" "\n" full-content))
                           (filtered-content (org-supertag-embed--filter-embed-markers normalized-content))
                           (clean-content (string-trim-right filtered-content)))
                      
                      ;; Only show debug info when debugging is enabled
                      ;; (message "DEBUG: Content boundaries: %d to %d" content-start content-end)
                      ;; (message "DEBUG: Heading: %s" heading-line)
                      ;; (message "DEBUG: Content length: %d" (length content-text))
                      ;; (message "DEBUG: Clean content length: %d" (length clean-content))
                      ;; (message "DEBUG: Generated content: %s" clean-content)
                      clean-content)))
              (format "** Error: Could not find node with ID %s in file %s **" node-id file-path))))
      "** Error: Node file path not available in DB **")))

(defun org-supertag-embed-generate-content (embed-id)
  "Generate content for the embed block with the given ID."
  (let* ((entry (org-supertag-embed-sync-get-entry embed-id))
         (source-id (org-supertag-embed-entry-source-id entry)))
    
    (unless entry
      (error "Embed block %s not found" embed-id))
    
    (org-supertag-embed-generate-node-content source-id)))

;;------------------------------------------------------------------------------
;; Embed Block Parsing and Insertion
;;------------------------------------------------------------------------------

(cl-defun org-supertag-embed-parse-block (&optional pos)
  "Parse the embed block at POS (defaults to point) and return its metadata.
POS: Position to start parsing from (defaults to current point)

Returns a plist with :source-type, :source-id, :embed-id, :block-start, :block-end, etc.
Returns nil if no valid embed block is found."
  (save-excursion
    (when pos (goto-char pos))
    
    (let ((block-start nil)
          (block-end nil)
          (source-id nil)
          (embed-id nil)
          (block-content nil))
      
      ;; Find the beginning of the embed block
      (unless (re-search-backward "^#\\+begin_embed_node: \\(.*?\\) \\([a-zA-Z0-9_-]+\\)" nil t)
        (cl-return-from org-supertag-embed-parse-block nil))
      
      (setq block-start (match-beginning 0))
      (setq source-id (match-string 1))
      (setq embed-id (match-string 2))
      
      ;; Find the end of the embed block
      (unless (re-search-forward "^#\\+end_embed_node" nil t)
        (cl-return-from org-supertag-embed-parse-block nil))
      
      (setq block-end (match-end 0))
      
      ;; Extract block content (excluding begin/end markers)
      (setq block-content (buffer-substring-no-properties 
                           (save-excursion
                             (goto-char block-start)
                             (forward-line 1)
                             (point))
                           (save-excursion
                             (goto-char block-end)
                             (forward-line -1)
                             (point))))
      
      ;; Validate embed-id exists in database
      (unless (org-supertag-embed-sync-get-entry embed-id)
        (message "Warning: Embed block %s not found in database" embed-id))
      
      (list :block-start block-start
            :block-end block-end
            :source-type :node
            :source-id source-id
            :embed-id embed-id
            :block-content block-content))))

(defun org-supertag-embed-insert-block (embed-id source-id &optional options)
  "Insert a new embed block at point.
  
EMBED-ID: Unique identifier for this embed block
SOURCE-ID: ID of the source node
OPTIONS: Optional plist of additional options"
  (interactive
   (let* ((node-id (let* ((nodes (org-supertag-node--collect-nodes))
                          (candidates (mapcar #'cdr nodes))
                          (choice (completing-read "Select node to embed: " candidates nil t))
                          (node-id (car (rassoc choice nodes))))
                     (if node-id node-id (user-error "No node selected"))))
          (embed-id (org-supertag-embed-create-id))
          (opts (read-string "Options (lisp plist, 可留空): ")))
     (list embed-id node-id
           (if (string-empty-p opts) nil (read opts)))))
  
  (let* ((inner-content (org-supertag-embed-generate-node-content source-id))
         (block-start (point)))
    
    ;; Register the embed in the sync database
    (org-supertag-embed-sync-add-entry embed-id source-id options)
    
    ;; Insert the block with new format (temporarily disable auto-sync to prevent ID generation)
    (let ((org-supertag-sync-auto-create-node nil))
      (insert (format "#+begin_embed_node: %s %s\n"
                     source-id embed-id))
      (insert inner-content)
      ;; Ensure proper formatting with newlines
      (unless (string-suffix-p "\n" inner-content)
        (insert "\n"))
      (insert "#+end_embed_node\n"))
    
    ;; Update hashes in the new sync database
    (let* ((entry (org-supertag-embed-sync-get-entry embed-id))
           (source-hash (org-supertag-embed-sync-calculate-source-hash entry))
           (content-hash (org-supertag-embed-sync-calculate-content-hash inner-content)))
      (org-supertag-embed-sync-update-hashes embed-id content-hash source-hash))
    
    (org-supertag-embed-sync-db-save)))

;;------------------------------------------------------------------------------
;; Embed Block Refresh and Change Detection
;;------------------------------------------------------------------------------

(defun org-supertag-embed-refresh-block (embed-id &optional buffer)
  "Refresh the content of the embed block with the given ID.
EMBED-ID: The embed block identifier
BUFFER: Optional buffer to operate on (defaults to current buffer)

Returns t if the block was refreshed, nil otherwise."
  (let ((entry (org-supertag-embed-sync-get-entry embed-id))
        (refreshed nil))
    (when entry
      (with-current-buffer (or buffer (current-buffer))
        (when-let ((inner-block-region
                   (org-supertag-embed--get-inner-block-region embed-id)))
          (let* ((inner-start (car inner-block-region))
                 (inner-end (cdr inner-block-region))
                 (current-inner-content (buffer-substring-no-properties
                                       inner-start inner-end))
                 (new-inner-content (org-supertag-embed-generate-content
                                   embed-id)))
            
            (unless (equal (string-trim current-inner-content) (string-trim
                                                               new-inner-content))
              (delete-region inner-start inner-end)
              (goto-char inner-start)
              (insert new-inner-content)
              
              ;; Ensure there's exactly one newline before the end marker
              (unless (looking-at-p "\n")
                (insert "\n"))
              
              (let* ((source-hash (org-supertag-embed-sync-calculate-source-hash
                                  entry))
                     (content-hash
                      (org-supertag-embed-sync-calculate-content-hash new-inner-content)))
                (org-supertag-embed-sync-update-hashes embed-id content-hash
                                                      source-hash))
              
              (org-supertag-embed-sync-db-save)
              (setq refreshed t)
              (message "Refreshed embed block %s" embed-id))))))
    refreshed))

(defun org-supertag-embed-refresh-all ()
  "Refresh all embed blocks in the current buffer."
  (interactive)
  (let ((refreshed-count 0))
    ;; Find all embed blocks in current buffer by scanning the buffer directly
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+begin_embed_node: .*? \\([a-zA-Z0-9_-]+\\)\\s-*$" nil t)
        (let ((embed-id (match-string 1)))
          (condition-case err
              (progn
                (when (org-supertag-embed-refresh-block embed-id)
                  (setq refreshed-count (1+ refreshed-count))))
            (error (message "ERROR refreshing embed %s: %s" embed-id (error-message-string err)))))))
    
    ;; If any blocks were refreshed, run the hook
    (when (> refreshed-count 0)
      (run-hooks 'org-supertag-embed-after-refresh-hook))
    
    (when (called-interactively-p 'interactive)
      (message "Refreshed %d embed blocks in current buffer" refreshed-count))
    refreshed-count))

;;------------------------------------------------------------------------------
;; Interactive Commands
;;------------------------------------------------------------------------------

(defun org-supertag-embed-node (node-id)
  "Embed a node at the current point.
Prompts user to select a node from a list of all available nodes."
  (interactive
   (let* ((nodes (org-supertag-node--collect-nodes))
          (candidates (mapcar #'cdr nodes))
          (choice (completing-read "Select node to embed: " candidates nil t))
          (node-id (car (rassoc choice nodes))))
     (list (if node-id node-id (user-error "No node selected")))))
  (let ((embed-id (org-supertag-embed-create-id)))
    (org-supertag-embed-insert-block embed-id node-id)))

;;------------------------------------------------------------------------------
;; Auto-refresh functionality
;;------------------------------------------------------------------------------

(defvar org-supertag-embed-after-refresh-hook nil
  "Hook run after one or more embed blocks in the current buffer have been
refreshed.")

(defun org-supertag-embed-sync-on-save ()
  "Hook function to sync embed blocks when a file is saved.
This function handles bidirectional synchronization:
1. If current file contains embed blocks with local changes, sync them to source
2. If current file is a source file, refresh embed blocks that reference it"
  (let ((current-file (buffer-file-name))
        (refreshed-count 0)
        (synced-count 0)
        (cleaned-count 0))
    (when current-file
      ;; First, clean up orphaned entries in the current file
      (setq cleaned-count (org-supertag-embed-cleanup-orphaned-entries))
      
      ;; Check for modified embed blocks in current buffer and sync them first
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^#\\+begin_embed_node: .*? \\([a-zA-Z0-9_-]+\\)\\s-*$" nil t)
          (let* ((embed-id (match-string 1))
                 (changes (org-supertag-embed-sync-detect-changes embed-id)))
            (when (and changes (plist-get changes :content-changed))
              (message "DEBUG: Auto-syncing modified embed block %s to source" embed-id)
              (condition-case err
                  (progn
                    (org-supertag-sync-embed-block-by-id embed-id)
                    (setq synced-count (1+ synced-count)))
                (error (message "ERROR syncing embed %s: %s" embed-id (error-message-string err))))))))
      
      ;; Check if this is a source file that has embed blocks referencing it
      (let ((source-refreshed-count (org-supertag-embed-auto-refresh-from-source current-file)))
        (setq refreshed-count source-refreshed-count))
      
      ;; Only refresh embed blocks in current buffer if we didn't sync any
      ;; (to avoid overwriting changes we just synced)
      (when (= synced-count 0)
        (let ((current-buffer-refreshed (org-supertag-embed-refresh-all)))
          (setq refreshed-count (+ refreshed-count current-buffer-refreshed))))
      
      (cond
       ((and (> cleaned-count 0) (> synced-count 0) (> refreshed-count 0))
        (message "Org SuperTag Embed: Cleaned %d orphaned, synced %d to source, refreshed %d from source" 
                 cleaned-count synced-count refreshed-count))
       ((and (> synced-count 0) (> refreshed-count 0))
        (message "Org SuperTag Embed: Synced %d to source, refreshed %d from source" 
                 synced-count refreshed-count))
       ((> synced-count 0)
        (message "Org SuperTag Embed: Synced %d embed blocks to source" synced-count))
       ((> cleaned-count 0)
        (message "Org SuperTag Embed: Cleaned up %d orphaned embed entries" cleaned-count))
       ((> refreshed-count 0)
        (message "Org SuperTag Embed: Auto-refreshed %d embed blocks from source" refreshed-count))
       (t
        (message "Org SuperTag Embed: No embed blocks needed refreshing on save."))))))

(add-hook 'org-supertag-embed-after-refresh-hook 'save-buffer)

(defun org-supertag-embed-auto-refresh-from-source (source-file)
  "Automatically refresh all embed blocks that reference the given source file.
SOURCE-FILE: The path to the source file that has changed"
  (let ((refreshed-count 0))
    ;; Ensure database is initialized
    (unless org-supertag-embed-sync-db
      (org-supertag-embed-init-db))
    
    ;; Map over embeds table safely
    (org-supertag-embed--safe-map-embeds
     (lambda (embed-id entry)
       (let ((entry-source-file (org-supertag-embed-entry-source-file entry)))
         (when (and entry-source-file (string= entry-source-file source-file))
           (let ((embedded-file (org-supertag-embed-entry-embedded-file entry)))
             (when (and embedded-file (file-exists-p embedded-file))
               (with-current-buffer (find-file-noselect embedded-file)
                 (org-supertag-embed-refresh-block embed-id)
                 (setq refreshed-count (1+ refreshed-count)))))))))
    
    (when (> refreshed-count 0)
      (message "Org SuperTag Embed: Auto-refreshed %d embed blocks from source file %s" 
               refreshed-count (file-name-nondirectory source-file)))
    refreshed-count))

(defun org-supertag-embed-check-source-changes ()
  "Check for changes in source files and refresh affected embed blocks.
This function can be called manually or as part of a periodic check."
  (interactive)
  (let ((total-refreshed 0))
    (org-supertag-embed--safe-map-embeds
     (lambda (embed-id entry)
       (let* ((source-file (org-supertag-embed-entry-source-file entry))
              (source-id (org-supertag-embed-entry-source-id entry))
              (last-source-hash (org-supertag-embed-entry-source-hash entry)))
         (when (and source-file source-id last-source-hash)
           (let ((current-source-hash (org-supertag-node-content-hash source-id)))
             (when (and current-source-hash 
                        (not (equal current-source-hash last-source-hash)))
               (org-supertag-embed-auto-refresh-from-source source-file)
               (setq total-refreshed (1+ total-refreshed))))))))
    ;; 确保返回 total-refreshed，而不是 safe-map-embeds 的返回值
    (when (> total-refreshed 0)
      (message "Org SuperTag Embed: Checked source changes, refreshed %d embed blocks" total-refreshed))
    total-refreshed))

(defun org-supertag-embed-cleanup-orphaned-entries ()
  "Clean up orphaned embed entries that no longer exist in their embedded files.
This function scans all embed entries and removes those whose embed blocks
no longer exist in their respective files."
  (interactive)
  (let ((cleaned-count 0)
        (total-entries 0))
    ;; Map over embeds table safely
    (org-supertag-embed--safe-map-embeds
     (lambda (embed-id entry)
       (setq total-entries (1+ total-entries))
       (let ((embedded-file (org-supertag-embed-entry-embedded-file entry)))
         (when (and embedded-file (file-exists-p embedded-file))
           (with-current-buffer (find-file-noselect embedded-file)
             (let ((block-region (org-supertag-embed--find-block-by-id embed-id)))
               (unless block-region
                 ;; Embed block no longer exists, remove from database
                 (org-supertag-embed-sync-remove-entry embed-id)
                 (setq cleaned-count (1+ cleaned-count)))))))))
    ;; 确保返回 cleaned-count，而不是 safe-map-embeds 的返回值
    (when (> cleaned-count 0)
      (org-supertag-embed-sync-db-save)
      (message "Org SuperTag Embed: Cleaned up %d orphaned embed entries (total: %d)" 
               cleaned-count total-entries))
    cleaned-count))

(defun org-supertag-embed-cleanup-all-files ()
  "Clean up orphaned embed entries across all files in the database.
This function checks all embed entries and removes those whose embed blocks
no longer exist in their respective files."
  (interactive)
  (let ((cleaned-count 0)
        (processed-files '()))
    ;; Map over embeds table safely
    (org-supertag-embed--safe-map-embeds
     (lambda (embed-id entry)
       (let ((embedded-file (org-supertag-embed-entry-embedded-file entry)))
         (when (and embedded-file 
                    (not (member embedded-file processed-files)))
           (push embedded-file processed-files)
           (when (file-exists-p embedded-file)
             (with-current-buffer (find-file-noselect embedded-file)
               (let ((block-region (org-supertag-embed--find-block-by-id embed-id)))
                 (unless block-region
                   ;; Embed block no longer exists, remove from database
                   (org-supertag-embed-sync-remove-entry embed-id)
                   (setq cleaned-count (1+ cleaned-count))))))))))
    ;; 确保返回 cleaned-count，而不是 safe-map-embeds 的返回值
    (when (> cleaned-count 0)
      (org-supertag-embed-sync-db-save)
      (message "Org SuperTag Embed: Cleaned up %d orphaned embed entries across %d files" 
               cleaned-count (length processed-files)))
    cleaned-count))

;;------------------------------------------------------------------------------
;; Bidirectional Synchronization
;;------------------------------------------------------------------------------

(cl-defun org-supertag-sync-embed-block ()
  "Sync changes from the current embed block back to its source.
This function allows users to edit embed block content and sync those changes
back to the original source node using the independent sync database."
  (interactive)
  (let* ((embed-id (org-supertag-embed--get-id-at-point))
         (entry (when embed-id (org-supertag-embed-sync-get-entry embed-id))))
    
    (cond
     ((null embed-id)
      (user-error "No embed block found at current position. Use `org-supertag-embed-debug-id-at-point' to diagnose."))
     ((null entry)
      (user-error "Embed block %s found but not registered in sync database. Use `org-supertag-embed-debug-id-at-point' to diagnose." embed-id))
     (t
      (org-supertag-sync-embed-block-by-id embed-id)))))

(cl-defun org-supertag-sync-embed-block-by-id (embed-id)
  "Sync an embed block by its ID."
  (cl-block org-supertag-sync-embed-block-by-id
    (let* ((entry (org-supertag-embed-sync-get-entry embed-id)))
      
      (unless entry
        (user-error "No embed block found at current position"))
      
      (let* ((source-type (org-supertag-embed-entry-source-type entry))
             (source-id (org-supertag-embed-entry-source-id entry))
             (source-file (org-supertag-embed-entry-source-file entry))
             (source-pos (org-supertag-embed-entry-source-pos entry))
             (block-content (org-supertag-embed-get-content-at-point embed-id))
             (changes (org-supertag-embed-sync-detect-changes embed-id))
             (content-changed (when changes (plist-get changes :content-changed)))
             (source-changed (when changes (plist-get changes :source-changed))))
        
        (unless (eq source-type :node)
          (user-error "Sync is only supported for node-type embed blocks"))
        
        ;; Check for conflicts before modifying source file
        (when source-changed
          (unless (y-or-n-p "Source file has changed since last sync. Overwrite?")
            (error "Sync cancelled due to source file changes")))
        
        ;; Check if content has actually changed
        (unless content-changed
          (message "No changes detected in embed block")
          (cl-return-from org-supertag-sync-embed-block-by-id nil))
    
        ;; Smart synchronization: preserve source node structure, update content only
        (with-current-buffer (find-file-noselect source-file)
          (save-excursion
            ;; Find the source node by ID instead of relying on stale position
            (goto-char (point-min))
            (if (re-search-forward (format ":ID:[ \t]+%s" (regexp-quote source-id)) nil t)
                (progn
                  (org-back-to-heading t)
                  (let* ((heading-start (point))
                         (heading-end (line-end-position))
                         (original-heading (buffer-substring-no-properties heading-start heading-end))
                         ;; Find content region (after PROPERTIES drawer)
                         (content-start (progn
                                        (forward-line 1)
                                        (when (looking-at "[ \t]*:PROPERTIES:")
                                          (search-forward ":END:" nil t)
                                          (forward-line 1))
                                        (point)))
                         (content-end (save-excursion
                                      (org-end-of-subtree t t)
                                      (skip-chars-backward "\n\r\t ")
                                      (point))))
                    
                    ;; Parse the embed content to extract new heading and content
                    (let* ((embed-lines (split-string block-content "\n"))
                           (embed-heading (car embed-lines))
                           (embed-content-lines (cdr embed-lines))
                           (embed-content (string-trim (string-join embed-content-lines "\n"))))
                      
                      ;; Update heading if it changed
                      (unless (string= original-heading embed-heading)
                        (goto-char heading-start)
                        (delete-region heading-start heading-end)
                        (insert embed-heading)
                        (message "Updated heading: %s" embed-heading))
                      
                      ;; Update content (preserve PROPERTIES drawer)
                      (delete-region content-start content-end)
                      (goto-char content-start)
                      (when (and embed-content (not (string-empty-p (string-trim embed-content))))
                        (insert embed-content))
                      
                      (message "Successfully synced embed block %s to source node %s" embed-id source-id))))
              
              (error "Could not find source node with ID %s" source-id))
           
           ;; Update sync database with new hashes
           (let* ((new-content-hash (org-supertag-embed-sync-calculate-content-hash block-content))
                  (new-source-hash (org-supertag-embed-sync-calculate-source-hash entry)))
             (org-supertag-embed-sync-update-hashes embed-id new-content-hash new-source-hash))
           
           ;; Update sync status
           (org-supertag-embed-sync-update-entry embed-id :sync-status 'synced)
           (org-supertag-embed-sync-db-save)
           
           ;; Save the source file
           (save-buffer)))))))

(cl-defun org-supertag-embed-detect-conflicts (embed-id)
  "Detect potential conflicts for the given embed block.
EMBED-ID: The embed block identifier

Returns a plist with conflict information."
  (let* ((entry (org-supertag-embed-sync-get-entry embed-id))
         (source-type (org-supertag-embed-entry-source-type entry))
         (source-id (org-supertag-embed-entry-source-id entry))
         (last-synced-hash (org-supertag-embed-entry-source-hash entry)))
    
    (unless entry
      (cl-return-from org-supertag-embed-detect-conflicts nil))
    
    ;; Only support :node type for conflict detection
    (unless (eq source-type :node)
      (cl-return-from org-supertag-embed-detect-conflicts nil))
    
    ;; Calculate current source hash
    (let* ((current-source-hash (org-supertag-node-content-hash source-id))
           (source-changed (and current-source-hash last-synced-hash
                                (not (equal current-source-hash last-synced-hash)))))
      
      (list :has-conflict source-changed
            :source-changed source-changed
            :current-source-hash current-source-hash
            :last-synced-hash last-synced-hash))))

;;------------------------------------------------------------------------------
;; Debug and Status Functions
;;------------------------------------------------------------------------------

(defun org-supertag-embed-sync-show-status (embed-id)
  "Show detailed status for the given embed block.
EMBED-ID: The embed block identifier"
  (interactive "sEmbed ID: ")
  (let* ((entry (org-supertag-embed-sync-get-entry embed-id))
         (changes (org-supertag-embed-sync-detect-changes embed-id))
         (block-content (org-supertag-embed-get-content-at-point embed-id)))
    
    (with-output-to-temp-buffer "*Org SuperTag Embed Sync Status*"
      (princ (format "Sync Status for Embed Block: %s\n" embed-id))
      (princ (make-string 50 ?=))
      (princ "\n\n")
      
      (if entry
          (progn
            (princ "Entry Information:\n")
            (princ (format "  Source Type: %s\n" (org-supertag-embed-entry-source-type entry)))
            (princ (format "  Source ID: %s\n" (org-supertag-embed-entry-source-id entry)))
            (princ (format "  Embedded File: %s\n" (org-supertag-embed-entry-embedded-file entry)))
            (princ (format "  Source File: %s\n" (org-supertag-embed-entry-source-file entry)))
            (princ (format "  Source Pos: %s\n" (org-supertag-embed-entry-source-pos entry)))
            (princ (format "  Created: %s\n" (org-supertag-embed-entry-created entry)))
            (princ (format "  Modified: %s\n" (org-supertag-embed-entry-modified entry)))
            (princ (format "  Sync Status: %s\n" (org-supertag-embed-entry-sync-status entry)))
            (princ (format "  Content Hash: %s\n" (org-supertag-embed-entry-content-hash entry)))
            (princ (format "  Source Hash: %s\n" (org-supertag-embed-entry-source-hash entry)))
            
            (princ "\nChange Detection:\n")
            (if changes
                (progn
                  (princ (format "  Content Changed: %s\n" (plist-get changes :content-changed)))
                  (princ (format "  Source Changed: %s\n" (plist-get changes :source-changed)))
                  (princ (format "  Current Content Hash: %s\n" (plist-get changes :current-content-hash)))
                  (princ (format "  Current Source Hash: %s\n" (plist-get changes :current-source-hash))))
              (princ "  Unable to detect changes\n"))
            
            (princ "\nContent Information:\n")
            (if block-content
                (princ (format "  Content Length: %d characters\n" (length block-content)))
              (princ "  No content found in buffer\n")))
        (princ "Entry not found in sync database\n")))))

(defun org-supertag-embed-sync-show-all-embeds ()
  "Show all embed blocks in the sync database."
  (interactive)
  (let ((embeds (org-supertag-embed-sync-db-embeds org-supertag-embed-sync-db)))
    (with-output-to-temp-buffer "*Org SuperTag Embed Sync Database*"
      (princ "Embed Sync Database Contents\n")
      (princ (make-string 50 ?=))
      (princ "\n\n")
      
      (if (and embeds (> (hash-table-count embeds) 0))
          (maphash (lambda (embed-id entry)
                     (princ (format "Embed ID: %s\n" embed-id))
                     (princ (format "  Source Type: %s\n" (org-supertag-embed-entry-source-type entry)))
                     (princ (format "  Source ID: %s\n" (org-supertag-embed-entry-source-id entry)))
                     (princ (format "  Embedded File: %s\n" (org-supertag-embed-entry-embedded-file entry)))
                     (princ (format "  Sync Status: %s\n" (org-supertag-embed-entry-sync-status entry)))
                     (princ "\n"))
                   embeds)
        (princ "No embed blocks found in sync database\n")))))

;;------------------------------------------------------------------------------
;; Integration with org-supertag
;;------------------------------------------------------------------------------

(defun org-supertag-embed-setup ()
  "Setup embed block functionality."
  (add-hook 'org-mode-hook #'org-supertag-embed-mode))

(defun org-supertag-embed-save-metadata ()
  "Save embed metadata to persistent storage.
This function is called when the org-supertag database is saved."
  (when (and org-supertag-db--embeds (> (hash-table-count org-supertag-db--embeds) 0))
    (message "Saving %d embed block metadata entries" (hash-table-count org-supertag-db--embeds))))

(defun org-supertag-embed-load-metadata ()
  "Load embed metadata from persistent storage.
This function is called when the org-supertag database is loaded."
  (when org-supertag-db--embeds
    (message "Loaded %d embed block metadata entries" (hash-table-count org-supertag-db--embeds))))

(define-minor-mode org-supertag-embed-mode
  "Minor mode for Org SuperTag embed blocks."
  :lighter " SuperTag-Embed"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-e n") #'org-supertag-embed-node)
            (define-key map (kbd "C-c C-e e") #'org-supertag-edit-embed-block)
            (define-key map (kbd "C-c C-e s") #'org-supertag-sync-embed-block)
            (define-key map (kbd "C-c C-e i") #'org-supertag-embed-insert-block)
            (define-key map (kbd "C-c C-e r") #'org-supertag-embed-refresh-all)
            (define-key map (kbd "C-c C-e c") #'org-supertag-embed-check-source-changes)
            (define-key map (kbd "C-c C-e d") #'org-supertag-embed-debug-id-at-point)
            (define-key map (kbd "C-c C-e l") #'org-supertag-embed-cleanup-orphaned-entries)
            (define-key map (kbd "C-c C-e L") #'org-supertag-embed-cleanup-all-files)
            map))

;;------------------------------------------------------------------------------
;; Error Handling and User Feedback
;;------------------------------------------------------------------------------

(defun org-supertag-embed-debug-id-at-point ()
  "Debug function to show what embed-id is found at current point."
  (interactive)
  (let* ((embed-id (org-supertag-embed--get-id-at-point))
         (entry (when embed-id (org-supertag-embed-sync-get-entry embed-id)))
         (current-line (line-number-at-pos))
         (current-char (char-after)))
    (with-output-to-temp-buffer "*Org SuperTag Embed Debug ID*"
      (princ (format "Debug: Embed ID at current point\n"))
      (princ (make-string 50 ?=))
      (princ "\n\n")
      
      (princ (format "Current point: %d (line %d, char: %s)\n" (point) current-line (char-to-string (or current-char ?\s))))
      (princ (format "Embed ID found: %s\n" (or embed-id "nil")))
      
      ;; Show context around current point
      (princ "\nContext around current point:\n")
      (save-excursion
        (let ((start (max (point-min) (- (point) 200)))
              (end (min (point-max) (+ (point) 200))))
          (princ (format "  %s\n" (buffer-substring-no-properties start end)))))
      
      (if entry
          (progn
            (princ "\nEntry found in database:\n")
            (princ (format "  Source Type: %s\n" (org-supertag-embed-entry-source-type entry)))
            (princ (format "  Source ID: %s\n" (org-supertag-embed-entry-source-id entry)))
            (princ (format "  Embedded File: %s\n" (org-supertag-embed-entry-embedded-file entry))))
        (princ "\nNo entry found in database for this embed ID.\n"))
      
      (princ "\nAll embed blocks in current buffer:\n")
      (save-excursion
        (goto-char (point-min))
        (let ((count 0)) ; Counter for found blocks
          (while (re-search-forward "^#\\+begin_embed_node: \\(.*?\\) \\([a-zA-Z0-9_-]+\\)" nil t)
            (setq count (1+ count))
            (let ((block-start (match-beginning 0))
                  (block-end (save-excursion
                               (goto-char (match-end 0))
                               (when (re-search-forward "^#\\+end_embed_node" nil t)
                                 (match-end 0)))))
              (princ (format "  %d. Source: %s, ID: %s (pos: %d-%d)\n" 
                            count (match-string 1) (match-string 2)
                            block-start (or block-end -1)))))))
      
      (princ "\nAll entries in sync database:\n")
      (let ((embeds-table (org-supertag-embed-sync-db-embeds org-supertag-embed-sync-db)))
        (when embeds-table
          (maphash (lambda (id entry)
                     (princ (format "  ID: %s, Type: %s, Source: %s\n" 
                                   id 
                                   (org-supertag-embed-entry-source-type entry)
                                   (org-supertag-embed-entry-source-id entry))))
                   embeds-table))))))

(defun org-supertag-embed-debug-info (embed-id)
  "Show debug information for the given embed block.
EMBED-ID: The embed block identifier"
  (interactive (list (org-supertag-embed--get-id-at-point)))
  (let* ((entry (org-supertag-embed-sync-get-entry embed-id))
         (block-region (org-supertag-embed--find-block-by-id embed-id))
         (changes (org-supertag-embed-sync-detect-changes embed-id)))
    
    (with-output-to-temp-buffer "*Org SuperTag Embed Debug*"
      (princ (format "Debug Information for Embed Block: %s\n" embed-id))
      (princ (make-string 50 ?=))
      (princ "\n\n")
      
      (if entry
          (progn
            (princ "Entry Information:\n")
            (princ (format "  Source Type: %s\n" (org-supertag-embed-entry-source-type entry)))
            (princ (format "  Source ID: %s\n" (org-supertag-embed-entry-source-id entry)))
            (princ (format "  Embedded File: %s\n" (org-supertag-embed-entry-embedded-file entry)))
            (princ (format "  Source File: %s\n" (org-supertag-embed-entry-source-file entry)))
            (princ (format "  Created: %s\n" (org-supertag-embed-entry-created entry)))
            (princ (format "  Modified: %s\n" (org-supertag-embed-entry-modified entry)))
            (princ (format "  Sync Status: %s\n" (org-supertag-embed-entry-sync-status entry)))
            (princ (format "  Content Hash: %s\n" (org-supertag-embed-entry-content-hash entry)))
            (princ (format "  Source Hash: %s\n" (org-supertag-embed-entry-source-hash entry))))
        (princ "Entry not found in sync database\n"))
      
      (princ "\nBlock Region:\n")
      (if block-region
          (princ (format "  Start: %d, End: %d\n" (car block-region) (cdr block-region)))
        (princ "  Not found in current buffer\n"))
      
      (princ "\nChange Detection:\n")
      (if changes
          (progn
            (princ (format "  Content Changed: %s\n" (plist-get changes :content-changed)))
            (princ (format "  Source Changed: %s\n" (plist-get changes :source-changed)))
            (princ (format "  Current Content Hash: %s\n" (plist-get changes :current-content-hash)))
            (princ (format "  Current Source Hash: %s\n" (plist-get changes :current-source-hash))))
        (princ "  Unable to detect changes\n")))))

;;------------------------------------------------------------------------------
;; Debug Functions
;;------------------------------------------------------------------------------

(defun org-supertag-embed-debug-source-context (node-id)
  "Debug function to show the context around a source node.
+NODE-ID: The node identifier to debug"
  (interactive "sNode ID: ")
  (let* ((node-props (org-supertag-db-get node-id))
         (file-path (plist-get node-props :file-path)))
    (if file-path
        (with-current-buffer (find-file-noselect file-path)
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward (format ":ID:[ \t]+%s" (regexp-quote node-id)) nil t)
                (let* ((found-pos (point))
                       (context-start (max (point-min) (- found-pos 500)))
                       (context-end (min (point-max) (+ found-pos 500)))
                       (context (buffer-substring-no-properties context-start context-end)))
                  (with-output-to-temp-buffer "*Source Node Context*"
                    (princ (format "Context around node %s in file %s:\n\n" node-id file-path))
                    (princ context)))
              (message "Node %s not found in file %s" node-id))))
             (message "No file path for node %s" node-id))))

(defun org-supertag-embed-fix-database-entry (embed-id)
  "Fix the database entry with correct source file information.
EMBED-ID: The embed ID to fix"
  (interactive "sEmbed ID: ")
  (let* ((entry (org-supertag-embed-sync-get-entry embed-id))
         (source-id (org-supertag-embed-entry-source-id entry))
         (node-props (org-supertag-db-get source-id))
         (correct-source-file (plist-get node-props :file-path)))
    
    (when entry
      (message "Current source file: %s" (org-supertag-embed-entry-source-file entry))
      (message "Correct source file: %s" correct-source-file)
      
      ;; Update with correct source file
      (setf (org-supertag-embed-entry-source-file entry) correct-source-file)
      (setf (org-supertag-embed-entry-source-pos entry) (plist-get node-props :pos))
      
      (org-supertag-embed-sync-db-save)
      (message "Fixed database entry for embed %s" embed-id))))

;; Initialize the embed sync database
(org-supertag-embed-init-db)

;; Add hooks for automatic refresh
(add-hook 'after-save-hook #'org-supertag-embed-sync-on-save)

;; Optional: Add periodic cleanup (uncomment if you want automatic cleanup)
;; (run-with-idle-timer 300 t #'org-supertag-embed-cleanup-orphaned-entries)

;; Manual commands for testing:
;; M-x org-supertag-embed-refresh-all - manually refresh all embeds in current buffer
;; M-x org-supertag-embed-cleanup-orphaned-entries - clean up orphaned entries

(provide 'org-supertag-embed)
;;; org-supertag-embed.el ends here

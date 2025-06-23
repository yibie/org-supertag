;;; debug-type-distribution.el --- Analyze object type distribution

(message "=== DETAILED TYPE DISTRIBUTION ANALYSIS ===")

;; 1. 统计所有对象类型
(message "\n1. Object type distribution:")
(let ((type-counts (make-hash-table :test 'equal))
      (total-objects 0))
  
  ;; 统计对象表中的类型
  (maphash (lambda (_id props)
             (cl-incf total-objects)
             (let ((type (plist-get props :type)))
               (if type
                   (let ((type-str (if (symbolp type) (symbol-name type) (format "%S" type))))
                     (puthash type-str (1+ (gethash type-str type-counts 0)) type-counts))
                 (puthash "NO-TYPE" (1+ (gethash "NO-TYPE" type-counts 0)) type-counts))))
           org-supertag-db--object)
  
  (message "   Total objects: %d" total-objects)
  (message "   Type breakdown:")
  (maphash (lambda (type count)
             (message "     - %s: %d" type count))
           type-counts))

;; 2. 统计链接类型
(message "\n2. Link type distribution:")
(let ((link-type-counts (make-hash-table :test 'equal))
      (total-links 0))
  
  (maphash (lambda (_id props)
             (cl-incf total-links)
             (let ((type (plist-get props :type)))
               (if type
                   (let ((type-str (if (symbolp type) (symbol-name type) (format "%S" type))))
                     (puthash type-str (1+ (gethash type-str link-type-counts 0)) link-type-counts))
                 (puthash "NO-TYPE" (1+ (gethash "NO-TYPE" link-type-counts 0)) link-type-counts))))
           org-supertag-db--link)
  
  (message "   Total links: %d" total-links)
  (message "   Link type breakdown:")
  (maphash (lambda (type count)
             (message "     - %s: %d" type count))
           link-type-counts))

;; 3. 检查节点对象的详细信息
(message "\n3. Detailed node analysis:")
(let ((node-count 0)
      (sample-nodes '()))
  
  (maphash (lambda (id props)
             (when (eq (plist-get props :type) :node)
               (cl-incf node-count)
               (when (< (length sample-nodes) 3)
                 (push (list :id id
                           :title (plist-get props :title)
                           :raw-value (plist-get props :raw-value)
                           :file-path (plist-get props :file-path)
                           :content (let ((content (plist-get props :content)))
                                     (if (and content (> (length content) 50))
                                         (concat (substring content 0 50) "...")
                                       content)))
                       sample-nodes))))
           org-supertag-db--object)
  
  (message "   Found %d :node type objects" node-count)
  (when sample-nodes
    (message "   Sample nodes:")
    (dolist (node sample-nodes)
      (message "     ID: %s" (plist-get node :id))
      (message "       Title: %s" (plist-get node :title))
      (message "       Raw-value: %s" (plist-get node :raw-value))
      (message "       File: %s" (plist-get node :file-path))
      (message "       Content: %s" (plist-get node :content))
      (message ""))))

;; 4. 检查是否有嵌入向量相关的对象
(message "\n4. Embedding-related objects:")
(let ((embedding-count 0)
      (vector-count 0)
      (embedding-samples '()))
  
  (maphash (lambda (id props)
             (let ((id-str (format "%S" id))
                   (type (plist-get props :type)))
               (when (or (string-match-p "embed" id-str)
                        (string-match-p "vector" id-str)
                        (string-match-p "embed" (format "%S" type))
                        (string-match-p "vector" (format "%S" type)))
                 (cl-incf embedding-count)
                 (when (< (length embedding-samples) 3)
                   (push (list :id id :type type) embedding-samples)))))
           org-supertag-db--object)
  
  (maphash (lambda (id props)
             (let ((id-str (format "%S" id))
                   (type (plist-get props :type)))
               (when (or (string-match-p "embed" id-str)
                        (string-match-p "vector" id-str)
                        (string-match-p "embed" (format "%S" type))
                        (string-match-p "vector" (format "%S" type)))
                 (cl-incf vector-count)
                 (when (< (length embedding-samples) 3)
                   (push (list :id id :type type) embedding-samples)))))
           org-supertag-db--link)
  
  (message "   Embedding-related objects: %d" embedding-count)
  (message "   Embedding-related links: %d" vector-count)
  (when embedding-samples
    (message "   Samples:")
    (dolist (sample embedding-samples)
      (message "     ID: %s, Type: %s" (plist-get sample :id) (plist-get sample :type)))))

;; 5. 查找可能的原始节点数据
(message "\n5. Looking for original node data patterns:")
(let ((uuid-pattern "[0-9A-F]\\{8\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{4\\}-[0-9A-F]\\{12\\}")
      (uuid-objects 0)
      (uuid-samples '()))
  
  (maphash (lambda (id props)
             (let ((id-str (format "%S" id)))
               (when (string-match-p uuid-pattern id-str)
                 (cl-incf uuid-objects)
                 (when (< (length uuid-samples) 5)
                   (push (list :id id :type (plist-get props :type)) uuid-samples)))))
           org-supertag-db--object)
  
  (message "   Objects with UUID-like IDs: %d" uuid-objects)
  (when uuid-samples
    (message "   UUID samples:")
    (dolist (sample uuid-samples)
      (message "     ID: %s, Type: %s" (plist-get sample :id) (plist-get sample :type)))))

(message "\n=== ANALYSIS COMPLETED ===") 
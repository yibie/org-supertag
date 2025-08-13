;;; org-supertag-view-table.el --- Table view for org-supertag -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'cl-macs)
(require 'org-supertag-view-utils nil t)

;;;----------------------------------------------------------------------
;;; Char-Grid Image Rendering Engine (Phase 2 Refactor)
;;;----------------------------------------------------------------------

(defcustom org-supertag-grid-image-target-char-height 5
  "Image target display height in table cells (in character lines)."
  :type 'integer
  :group 'org-supertag)


(defcustom org-supertag-grid-image-max-width-ratio 0.8
  "Maximum ratio of image width to cell content space."
  :type 'float
  :group 'org-supertag)

(defcustom org-supertag-grid-image-column-width 14
  "Default display width of image column (in characters)."
  :type 'integer
  :group 'org-supertag)

(defcustom org-supertag-grid-per-tag-image-widths nil
  "Image column width settings for each tag, format: ((tag1 . width1) (tag2 . width2) ...)"
  :type '(alist :key-type string :value-type integer)
  :group 'org-supertag)

(defvar-local org-supertag-grid--grid-data nil
  "Buffer-local variable to hold the entire grid data (list of lists of grid-cells).")


(defface org-supertag-grid-highlight-face
  '((t :background "gray20"))
  "Face for highlighting the current grid cell."
  :group 'org-supertag)

(defvar-local org-supertag-grid--highlight-overlay nil)

;;----------------------------------------------------------------------
;; Helpful functions
;;----------------------------------------------------------------------

(defun org-supertag-view-table-get-field-info ()
  "Get all relevant information about the cell currently under the cursor."
  (when-let* ((coords (org-supertag-grid-get-cell-at-point))
              (row-idx (plist-get coords :row))
              (col-idx (plist-get coords :col))
              (grid-data org-supertag-grid--grid-data)
              (cell (when (and (>= row-idx 0) (< row-idx (length grid-data)))
                      (let ((row-data (nth row-idx grid-data)))
                        (when (and (>= col-idx 0) (< col-idx (length row-data)))
                          (nth col-idx row-data)))))
              (tag (org-supertag-view--get-current-tag)))
    (let* ((all-fields (org-supertag-get-all-fields-for-tag tag))
           (field-idx (- col-idx 1)) ; Only Node is a fixed column now
           (field-def (when (>= field-idx 0) (nth field-idx all-fields)))
           (node-cell (nth 0 (nth row-idx grid-data)))
           (node-title (grid-cell-raw-input node-cell))
           (node-id (org-supertag-view--find-node-by-title node-title))) ; Get the actual node ID
      (list :coords coords
            :cell cell
            :node-id node-id ; This needs a more robust way to get the node's internal ID
            :tag tag
            :field-def field-def))))

(defun org-supertag-view-table-get-line ()
  "Get all fields in the current table row as a list.
Returns a list where each element is the content of a cell in the current row."
  (save-excursion
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position)))
          (fields '()))
      ;; Ensure we are on a table row
      (when (string-match "^[ \t]*|" line)
        ;; Remove leading and trailing |
        (setq line (replace-regexp-in-string "^[ \t]*|" "" line))
        (setq line (replace-regexp-in-string "|[ \t]*$" "" line))
        ;; Split fields and remove leading/trailing whitespace
        (setq fields (mapcar #'string-trim (split-string line "|"))))
      fields)))

(defun org-supertag-view--find-node-by-title (title)
  "Find node ID by title.
TITLE is the title of the node.
Returns the found node ID, or nil if not found.
Performs exact and fuzzy matching, prioritizing exact matches."
  (when (and title (not (string-empty-p title)))
    (let ((cleaned-title (string-trim title))
          (exact-match nil)
          (fuzzy-matches '()))
      
      ;; Normalize title (remove extra spaces)
      (setq cleaned-title (replace-regexp-in-string "\\s-+" " " cleaned-title))
      
      ;; Search all nodes in the database
      (maphash
       (lambda (id props)
         (when (eq (plist-get props :type) :node)
           (let ((node-title (plist-get props :title))
                 (node-org-id (plist-get props :id)))
             (when node-title
               ;; Normalize node title
               (setq node-title (replace-regexp-in-string "\\s-+" " " (string-trim node-title)))
               
               ;; Check for exact match
               (if (string= cleaned-title node-title)
                   (setq exact-match id)
                 ;; Check for fuzzy match (containment relationship)
                 (when (or (string-match-p (regexp-quote cleaned-title) node-title)
                           (string-match-p (regexp-quote node-title) cleaned-title))
                   (push (cons id (length node-title)) fuzzy-matches)))))))
       org-supertag-db--object)
      
      ;; Prioritize exact match
      (or exact-match
          ;; Otherwise, return the closest fuzzy match (choose the one with the closest title length)
          (when fuzzy-matches
            (caar (sort fuzzy-matches
                        (lambda (a b)
                          (< (abs (- (cdr a) (length cleaned-title)))
                             (abs (- (cdr b) (length cleaned-title))))))))))))

(defun org-supertag-view--find-node-by-id (file-path org-id)
  "Locate the node in the file by ID.
FILE-PATH is the file path, ORG-ID is the ID of the node.
Returns t if the node is successfully found."
  (when (and file-path (file-exists-p file-path) org-id)
    (find-file file-path)
    (widen)
    ;; Disable org-element-cache to avoid parsing errors
    (when (boundp 'org-element-use-cache)
      (setq-local org-element-use-cache nil))
    
    (goto-char (point-min))
    ;; Locate node by ID property
    (when (re-search-forward (format ":ID:\\s-*%s\\s-*$"
                                    (regexp-quote org-id))
                            nil t)
      (org-back-to-heading t)
      (org-fold-show-entry)
      (org-fold-show-children)
      (recenter 1)
      t)))

(defun org-supertag-view-table-node-at-point ()
  "View the node at point in the SuperTag table view.
Strategy:
1. Get node title from the table row
2. Find node ID and file location in the database by title
3. Locate the node by ID"
  (interactive)
  (when (org-at-table-p)
    (let* ((row-data (org-supertag-view-table-get-line))
           (node-text (nth 0 row-data))
           ;; Clean up node title (remove [v] and [N/A] markers)
           (node-title (when node-text
                        (string-trim
                         (replace-regexp-in-string "\\[\\(v\\|N/A\\)\\]\\s-*" ""
                                                 node-text))))
           (node-id (org-supertag-view--find-node-by-title node-title)))
      
      (if node-id
          (let* ((node-props (gethash node-id org-supertag-db--object))
                 (file-path (plist-get node-props :file-path))
                 (org-id (plist-get node-props :id)))
            (if (org-supertag-view--find-node-by-id file-path org-id)
                (message "Successfully located node: %s" node-title)
              (message "Could not find node in file: %s" node-title)))
        (message "Node not found: %s" node-title)))))

;;----------------------------------------------------------------------
;; Sorting and Moving Functions
;;----------------------------------------------------------------------

(defun org-supertag-grid--compare-cell-values (cell1 cell2)
  "Compare two cell values for sorting. Returns t if cell1 < cell2."
  (let ((val1 (grid-cell-computed-value cell1))
        (val2 (grid-cell-computed-value cell2)))
    ;; Handle empty/nil values
    (cond
     ((and (or (null val1) (string-empty-p val1))
           (or (null val2) (string-empty-p val2))) nil)
     ((or (null val1) (string-empty-p val1)) t)
     ((or (null val2) (string-empty-p val2)) nil)
     ;; Try numeric comparison first
     ((and (string-match-p "^-?[0-9]+\\.?[0-9]*$" val1)
           (string-match-p "^-?[0-9]+\\.?[0-9]*$" val2))
      (< (string-to-number val1) (string-to-number val2)))
     ;; Fall back to string comparison
     (t (string< val1 val2)))))



;;----------------------------------------------------------------------
;; Single Tag View Mode (Table View)
;;----------------------------------------------------------------------

(defun org-supertag-view--show-content-table (tag &optional node-id-to-restore col-to-restore)
  "Show content table for TAG in a dedicated full-screen buffer."
  (let ((buffer (get-buffer-create (format "*Org SuperTag Table View: %s*" tag))))
    (with-current-buffer buffer
      (org-mode)
      (org-supertag-view-table-mode)
      (org-supertag-view-mode 1) ; Enable the minor mode for correct dispatching
      (setq-local org-supertag-view-current-tag tag)

      (org-supertag-grid--setup-perfect-display-environment)

      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Use plain text instead of org heading
        (insert (format "Tag: #%s\n\n" tag))
        (insert "Instructions:\n")
        (insert " [q] - Quit    [g] - Refresh    [v] - View Node\n")
        (insert " [e] - Edit Value    [w] - Adjust Image Column Width\n")
        (insert " [a] - Add Field    [D] - Delete Field    [E] - Edit Field Definition\n")
        (insert " [Tab] - Next Field    [S-Tab] - Previous Field    [n/p] - Move Up/Down\n")
        (insert " Click the [v] button before a node to directly view its content\n")
        (insert " Editing fields will automatically save changes\n")
        (insert "\n")
        ;; Insert table content
        (org-supertag-view--insert-content-table tag))

      (if (and node-id-to-restore col-to-restore)
          ;; TODO: Implement cursor restoration functionality.
          ;; The function org-supertag-view-table--restore-cursor is not defined.
          (goto-char (point-min))
        (goto-char (point-min)))
      (setq buffer-read-only t))

    ;; Use full-screen display instead of sidebar window
    (switch-to-buffer buffer)
    (delete-other-windows)))

(defun org-supertag-view--get-related-nodes (tag)
  "Get nodes related to TAG.
Returns a list of plists with properties :node, :id and field values."
  (let* ((nodes '())
         (tag-def (org-supertag-tag-get tag))
         (fields (org-supertag-get-all-fields-for-tag tag)))

    (maphash
     (lambda (link-id link-props)
       ;; First find all nodes related to this tag
       (when (and (string-match ":node-tag:\\(.+\\)->\\(.+\\)$" link-id)
                  (equal (match-string 2 link-id) tag))
         (when-let* ((node-id (plist-get link-props :from))
                     (node-props (gethash node-id org-supertag-db--object)))
           ;; For each node, get the values of all its fields
           (let ((field-values
                  (mapcar
                   (lambda (field)
                     (let* ((field-name (plist-get field :name))
                            (value (org-supertag-field-get-value node-id field-name tag)))
                       (cons field-name value)))
                   fields)))
             
             ;; Build node information
             (push (list :node (or (plist-get node-props :title) (format "Node %s" node-id))
                         :id node-id
                         :fields field-values)
                   nodes)))))
     org-supertag-db--link)

    (nreverse nodes)))

;; Grid cell structure - using simple functions instead of defstruct
(defun make-grid-cell (&rest properties)
  "Create a grid cell with PROPERTIES."
  (apply #'list properties))

(defun grid-cell-raw-input (cell)
  "Get the raw input of CELL."
  (plist-get cell :raw-input))

(defun grid-cell-row (cell)
  "Get the row index of CELL."
  (plist-get cell :row))

(defun grid-cell-col (cell)
  "Get the column index of CELL."
  (plist-get cell :col))

(defun grid-cell-computed-value (cell)
  "Get the computed value of CELL."
  (plist-get cell :computed-value))

(defun set-grid-cell-raw-input (cell value)
  "Set the raw input of CELL to VALUE."
  (setf (plist-get cell :raw-input) value))

(defun set-grid-cell-computed-value (cell value)
  "Set the computed value of CELL to VALUE."
  (setf (plist-get cell :computed-value) value))

(defun grid-cell-data-type (cell)
  "Get the data type of CELL."
  (plist-get cell :data-type))

(defun grid-cell-dependencies (cell)
  "Get the dependencies of CELL."
  (plist-get cell :dependencies))

(defun grid-cell-formula (cell)
  "Get the formula of CELL."
  (plist-get cell :formula))

(defun grid-cell-error-msg (cell)
  "Get the error message of CELL."
  (plist-get cell :error-msg))

(defun org-supertag-grid--calculate-column-widths (headers data-rows)
  "Calculate the optimal column widths based on headers and data.
HEADERS is a list of strings for the column titles.
DATA-ROWS is a list of lists, where each inner list represents a row.
Returns a list of integers representing the calculated width for each column."
  (when (or headers data-rows)
    (let* ((num-columns (length headers))
           (min-width 5) ; Minimum width for any column
           (max-width 30) ; Maximum width for any column
           (widths (make-list num-columns 0))
           (current-tag (org-supertag-view--get-current-tag))
           (image-width (org-supertag-grid--get-image-column-width-for-tag current-tag)))

      ;; 1. Calculate initial widths from headers
      (dotimes (i num-columns)
        (setf (nth i widths) (max min-width (string-width (nth i headers)))))

      ;; 2. Expand widths based on data rows
      (dolist (row data-rows)
        (dotimes (i num-columns)
          (when (< i (length row))
            (let* ((cell-content (format "%s" (or (nth i row) "")))
                   (content-width 
                    (if (org-supertag-grid--is-image-path-p cell-content)
                        ;; Use current tag's configured width for images
                        image-width
                      ;; Use character width for text
                      (string-width cell-content))))
              (setf (nth i widths) (max (nth i widths) content-width))))))

      ;; 3. Apply max width constraint
      (dotimes (i num-columns)
        (setf (nth i widths) (min max-width (nth i widths))))

      ;; 4. (Future) Adjust total width to fit window size.
      ;; For now, we just return the calculated widths.

      widths)))

(defun org-supertag-grid--wrap-text (text width)
  "Wrap TEXT into a list of strings, each fitting within WIDTH."
  (if (<= (string-width text) width)
      (list text)
    (let ((lines '())
          (remaining-text text))
      (while (> (string-width remaining-text) width)
        (let* ((break-pos (or (cl-loop for i from (min (length remaining-text) width) downto 1
                                       when (<= (string-width remaining-text 0 i) width)
                                       return i)
                              1))
               ;; Try to find a natural break point (space)
               (space-pos (or (cl-position ?\s remaining-text :from-end t :end break-pos)
                              break-pos)))
          (push (substring remaining-text 0 space-pos) lines)
          (setq remaining-text (string-trim (substring remaining-text space-pos)))))
      (push remaining-text lines)
      (nreverse lines))))

(defun org-supertag-grid--pad-string (text width)
  "Pad TEXT with spaces to fit WIDTH."
  (let* ((text-str (format "%s" text))
         (padding (- width (string-width text-str))))
    (if (> padding 0)
        (concat text-str (make-string padding ?\s))
      text-str)))

(defun org-supertag-grid--is-image-path-p (text)
  "Return t if TEXT is a string that points to an existing image file."
  (and (stringp text)
       (file-exists-p text)
       (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|svg\\|webp\\|bmp\\)$" text)))

(defun org-supertag-grid--get-exact-line-height ()
  "获取当前环境的精确行高（像素），包括frame级别的line-spacing"
  (let* ((char-height (frame-char-height))
         (buffer-line-spacing (or line-spacing 0))
         (frame-line-spacing (or (frame-parameter nil 'line-spacing) 0))
         ;; 使用更大的line-spacing值
         (effective-line-spacing (max buffer-line-spacing frame-line-spacing))
         (total-line-height (+ char-height effective-line-spacing)))
    ;; 确保切片高度完全等于实际行高，不留空隙
    total-line-height))



(defun org-supertag-grid--propertize-org-markup (text)
  "Parse a string containing org-mode markup and return a propertized string."
  (if (or (not (stringp text)) (string-empty-p text))
      ""
    (with-temp-buffer
      (org-mode)
      ;; Insert text and ensure it's treated as a paragraph
      (insert text "\n\n")
      (goto-char (point-min))
      (font-lock-fontify-region (point-min) (point-max))
      ;; Extract the propertized string, removing the extra newlines
      (buffer-substring-no-properties (point-min) (- (point-max) 2)))))

(defun org-supertag-grid--setup-perfect-display-environment ()
  "设置完美显示环境，确保行高一致性"
  ;; 强制行间距为0 - 这是关键设置
  (setq-local line-spacing 0)
  
  ;; 确保没有额外的行高因子
  (setq-local line-height-factor 1.0)
  
  ;; 禁用可能影响行高的特性
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  
  (message "Display environment optimized for perfect height matching"))



(defun org-supertag-grid--show-markup-help ()
  "Show help information about org-mode markup syntax and image support in table cells."
  (interactive)
  (let ((help-text "
=== Org Mode Markup Syntax & Image Support ===

The table view supports org-mode markup syntax and image rendering.

Supported Text Markup:
- *bold* - Bold text
- /italic/ - Italic text (may not display in all configurations)
- _underline_ - Underlined text
- +strikethrough+ - Strikethrough text
- ~code~ - Code text
- [[link][description]] - Links

Image Support:
- Supported formats: PNG, JPG, JPEG, GIF, SVG, WebP, BMP
- Simply enter the full path to an image file in any cell
- Images will be automatically scaled to fit the column width
- Example: /path/to/image.png

Usage:
- Type org-mode markup in any cell for rich text
- Enter image file paths to display images
- All content is automatically rendered"))
    (with-current-buffer (get-buffer-create "*Org Mode Markup Help*")
      (erase-buffer)
      (insert help-text)
      (org-mode)
      (view-mode 1)
      (pop-to-buffer (current-buffer)))))


;;----------------------------------------------------------------------
;; Draw Row
;;----------------------------------------------------------------------

(defun org-supertag-grid--enforce-line-height (string)
  "为 STRING 中的每一行强制应用一个固定的 'line-height' 文本属性."
  (propertize string 'line-height (org-supertag-grid--get-exact-line-height)))

(defun org-supertag-grid--get-loadable-image-source (file)
  "寻找一个能被安全加载的图片源。
它会检查原始文件，如果 Emacs 不支持，则在 macOS 上尝试用 'sips' 工具将其转换为 PNG。
成功时返回一个列表 '(FILE-PATH TYPE)'
失败时返回 nil。"
  ;; 首先检查文件是否存在且可读
  (if (not (and (stringp file) (file-exists-p file)))
      (progn
        (message "!!! ERROR: File does not exist or is not accessible: %s" file)
        nil)
    ;; 处理文件路径编码问题（特别是中文文件名）
    (let* ((normalized-file (expand-file-name file))
           (original-type (image-type-from-file-name normalized-file)))
      
      ;; 调试信息
      (message "DEBUG: Processing image file: %s" normalized-file)
      (message "DEBUG: Detected image type: %s" original-type)
      
      (cond
       ;; 策略 1: 检查 Emacs 是否原生支持该图片类型
       ((and original-type (image-type-available-p original-type))
        (message "DEBUG: Using native image support for type: %s" original-type)
        (list normalized-file original-type))
       
       ;; 策略 2: 如果类型检测失败，尝试基于文件扩展名推断类型
       ((null original-type)
        (message "DEBUG: Image type detection failed, trying extension-based detection")
        (let ((extension (downcase (file-name-extension normalized-file))))
          (cond
           ((member extension '("jpg" "jpeg"))
            (if (image-type-available-p 'jpeg)
                (progn
                  (message "DEBUG: Using JPEG based on file extension")
                  (list normalized-file 'jpeg))
              (message "DEBUG: JPEG not available, will try conversion")
              nil))
           ((member extension '("png"))
            (if (image-type-available-p 'png)
                (progn
                  (message "DEBUG: Using PNG based on file extension")
                  (list normalized-file 'png))
              (message "DEBUG: PNG not available, will try conversion")
              nil))
           ((member extension '("gif"))
            (if (image-type-available-p 'gif)
                (progn
                  (message "DEBUG: Using GIF based on file extension")
                  (list normalized-file 'gif))
              (message "DEBUG: GIF not available, will try conversion")
              nil))
           (t
            (message "DEBUG: Unknown file extension: %s" extension)
            nil))))
       
       ;; 策略 3: 如果不支持且在 macOS 上，则调用 sips 进行转换
       ((eq system-type 'darwin)
        (message "DEBUG: Image type '%s' not directly supported, trying conversion with 'sips'..." original-type)
        (let* ((temp-file (make-temp-file "ost-img-" nil ".png"))
               (exit-code (call-process "sips" nil nil nil "-s" "format" "png" normalized-file "--out" temp-file)))
          (if (and (zerop exit-code) (file-exists-p temp-file))
              (progn
                (message "DEBUG: External conversion to PNG successful: %s" temp-file)
                (list temp-file 'png))
            (progn
              (message "!!! ERROR: External conversion (sips) failed for %s (exit code: %s)" normalized-file exit-code)
              nil))))
       
       ;; 策略 4: 在其他系统上，如果不支持则直接失败
       (t
        (message "!!! ERROR: Image type '%s' not supported and no conversion available on this system" original-type)
        nil)))))

(defvar org-supertag-grid--debug-slices nil
  "Output each slice's position and size when non-nil, for debugging.")



(defun org-supertag-grid--get-image-column-width-for-tag (tag)
  "Get image column width settings for a specific TAG."
  (or (cdr (assoc tag org-supertag-grid-per-tag-image-widths))
      org-supertag-grid-image-column-width))

(defun org-supertag-grid--set-image-column-width-for-tag (tag width)
  "Set image column width for a specific TAG."
  (let ((existing (assoc tag org-supertag-grid-per-tag-image-widths)))
    (if existing
        (setcdr existing width)
      (push (cons tag width) org-supertag-grid-per-tag-image-widths))
    ;; Save to customize system
    (customize-save-variable 'org-supertag-grid-per-tag-image-widths 
                            org-supertag-grid-per-tag-image-widths)))

(defun org-supertag-grid--adjust-image-column-width ()
  "Interactively adjust the image column width for the current tag and save the setting."
  (interactive)
  (let* ((current-tag (org-supertag-view--get-current-tag))
         (current-width (org-supertag-grid--get-image-column-width-for-tag current-tag))
         (new-width (read-number (format "Image column width (tag: %s, current: %d characters): " 
                                        current-tag current-width) 
                                current-width)))
    (when (and (numberp new-width) (> new-width 0))
      ;; Set and save
      (org-supertag-grid--set-image-column-width-for-tag current-tag new-width)
      
      ;; Refresh current table
      (when (derived-mode-p 'org-supertag-view-table-mode)
        (org-supertag-view-table-refresh))
      
      (message "Image column width for tag '%s' set to %d characters and saved" current-tag new-width))))

(defun org-supertag-grid--debug-image-alignment ()
  "Debug image alignment issues."
  (interactive)
  (let* ((char-h (frame-char-height))
         (buffer-line-spacing (or line-spacing 0))
         (frame-line-spacing (or (frame-parameter nil 'line-spacing) 0))
         (exact-line-height (org-supertag-grid--get-exact-line-height))
         (font-info (font-info (face-font 'default))))
    (message "=== Image Alignment Debug Info ===")
    (message "Character height: %d pixels" char-h)
    (message "Buffer line spacing: %s" buffer-line-spacing)
    (message "Frame line spacing: %s" frame-line-spacing)
    (message "Exact line height (used for images): %d pixels" exact-line-height)
    (message "Font info: %s" font-info)
    (message "===================================")))

(defun org-supertag-grid--draw-row (rowData widths)
  "Draw a table row, supporting image mosaics and text, with uniform line height."
  (let* ((row-idx (grid-cell-row (car rowData))) ; Get row index from the first cell
         ;; 1. Process all cells into a list of lists (a list of lines for each cell).
         (processed-cells (cl-loop for cell in rowData for width in widths
                                   collect (let ((content (grid-cell-computed-value cell)))
                                             (if (org-supertag-grid--is-image-path-p content)
                                                 ;; Use the built-in insert-sliced-image function
                                                 (with-temp-buffer
                                                   (let* ((source-info (org-supertag-grid--get-loadable-image-source content))
                                                          (source-file (car source-info))
                                                          (source-type (cadr source-info))
                                                          (target-px-w (* width (frame-char-width)))
                                                          (target-px-h (* org-supertag-grid-image-target-char-height (org-supertag-grid--get-exact-line-height)))
                                                          (image (create-image source-file source-type nil
                                                                               :width target-px-w
                                                                               :height target-px-h
                                                                               :ascent 'center)))
                                                     (when image
                                                       (insert-sliced-image image " " nil org-supertag-grid-image-target-char-height width)
                                                       (split-string (buffer-string) "\n" t))))
                                               (org-supertag-grid--wrap-text
                                                (org-supertag-grid--propertize-org-markup content)
                                                width)))))
         (row-height (apply #'max (cons 1 (mapcar #'length processed-cells)))) ; Ensure height is at least 1
         (output-lines '()))

    ;; 3. Build the output line by line.
    (dotimes (line-idx row-height)
      (let ((line-parts (cl-loop for processed-content in processed-cells
                                 for width in widths
                                 for col-idx from 0
                                 collect
                                 (let* ((content-part (or (nth line-idx processed-content) ""))
                                        (padded (org-supertag-grid--pad-string content-part width)))
                                   (propertize (format " %s " padded)
                                               'grid-cell t
                                               'grid-row-idx row-idx
                                               'grid-col-idx col-idx)))))
        (push (format "│%s│" (string-join line-parts "│")) output-lines)))

    ;; 4. Join all generated lines into a single string.
    (string-join (nreverse output-lines) "\n")))


(defun org-supertag-grid--draw-separator (widths &optional left mid right)
  "Draw a horizontal separator line.
LEFT, MID, RIGHT are the characters for left-end, middle, and right-end."
  (let* ((left (or left "├"))
         (mid (or mid "┼"))
         (right (or right "┤"))
         (segments (mapcar (lambda (w) (make-string (+ w 2) ?─)) widths)))
    (concat left (string-join segments mid) right)))

(defun org-supertag-grid-get-cell-at-point ()
  "Get the logical coordinates of the cell at the current point.
Robustly searches the current line if point is not directly on cell text."
  (interactive)
  (let ((props-at-point (get-text-property (point) 'grid-cell)))
    (if props-at-point
        ;; Fast path: Point is directly on cell text.
        (list :row (get-text-property (point) 'grid-row-idx)
              :col (get-text-property (point) 'grid-col-idx))
      ;; Slow path: Point is on a border or padding. Search the line.
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((next-cell-pos (next-single-property-change (point) 'grid-cell nil (line-end-position))))
          (when next-cell-pos
            (goto-char next-cell-pos)
            (list :row (get-text-property (point) 'grid-row-idx)
                  :col (get-text-property (point) 'grid-col-idx))))))))

(defun org-supertag-grid--move-to-cell (row col)
  "Move point to the beginning of the cell at logical ROW and COL."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (let ((props (text-properties-at (point))))
        (when (and (plist-get props 'grid-cell)
                   (eq (plist-get props 'grid-row-idx) row)
                   (eq (plist-get props 'grid-col-idx) col))
          (throw 'found t)))
      (forward-char 1))
    ;; If not found, maybe do nothing or move to a default position
    (message "Cell (%s, %s) not found." row col)
    nil))

(defun org-supertag-grid--get-cell-bounds (row col)
  "Get the buffer start and end positions of the cell at logical ROW and COL."
  (let (bounds)
    (save-excursion
      (when (org-supertag-grid--move-to-cell row col)
        (let ((start (point))
              (end (next-single-property-change (point) 'grid-cell nil (point-max))))
          ;; If no next property change point found, use line end
          (unless end
            (setq end (line-end-position)))
          (setq bounds (cons start end)))))
    bounds))

(defun org-supertag-grid--highlight-cell (row col)
  "Highlight the cell at the given ROW and COL using an overlay."
  (unless org-supertag-grid--highlight-overlay
    (setq org-supertag-grid--highlight-overlay (make-overlay (point-min) (point-min)))
    (overlay-put org-supertag-grid--highlight-overlay 'face 'org-supertag-grid-highlight-face)
    (overlay-put org-supertag-grid--highlight-overlay 'priority -1))

  (if-let ((bounds (org-supertag-grid--get-cell-bounds row col)))
      (move-overlay org-supertag-grid--highlight-overlay (car bounds) (cdr bounds))
    ;; If cell not found, hide the overlay
    (move-overlay org-supertag-grid--highlight-overlay (point-min) (point-min))))

(defun org-supertag-grid--redraw-cell (row-idx col-idx)
  "Redraw a single cell at ROW-IDX and COL-IDX based on the current grid data."
  (when-let* ((grid-data org-supertag-grid--grid-data)
              (row-data (nth row-idx grid-data))
              (cell (nth col-idx row-data))
              (cell-bounds (org-supertag-grid--get-cell-bounds row-idx col-idx)))
    (let ((start-pos (car cell-bounds))
          (end-pos (cdr cell-bounds)))
      (delete-region start-pos end-pos)
      (goto-char start-pos)
      ;; Insert the updated cell content with proper text properties
      (let* ((column-widths (org-supertag-grid--calculate-column-widths
                             (cons "Node" (mapcar (lambda (f) (plist-get f :name))
                                                  (org-supertag-get-all-fields-for-tag (org-supertag-view--get-current-tag))))
                             (mapcar (lambda (row) (mapcar #'grid-cell-computed-value row))
                                     grid-data)))
             (cell-width (nth col-idx column-widths))
             (cell-content (org-supertag-grid--pad-string 
                           (org-supertag-grid--propertize-org-markup (grid-cell-computed-value cell)) 
                           cell-width)))
        (insert (propertize cell-content
                           'grid-cell t
                           'grid-row-idx row-idx
                           'grid-col-idx col-idx))))))

;;----------------------------------------------------------------------
;; Navigation
;;----------------------------------------------------------------------

(defun org-supertag-grid--find-and-move-to-cell (row-idx col-idx)
  "Find and move to cell at ROW-IDX and COL-IDX using a more robust search method."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (not (eobp))
        (let ((props (text-properties-at (point))))
          (when (and (plist-get props 'grid-cell)
                     (eq (plist-get props 'grid-row-idx) row-idx)
                     (eq (plist-get props 'grid-col-idx) col-idx))
            (throw 'found t)))
        (forward-char 1))
      ;; If not found, try to move to a reasonable position
      (goto-char (point-min))
      (when (search-forward "│" nil t)
        (forward-char 1)))))

(defun org-supertag-grid--move-to-first-empty-cell ()
  "Move cursor to the first cell that has no value (empty or whitespace only)."
  (when-let* ((grid-data org-supertag-grid--grid-data)
              (fields (org-supertag-get-all-fields-for-tag (org-supertag-view--get-current-tag))))
    (let ((found nil))
      ;; Skip the first column (Node column) and search for empty cells in user-defined fields
      (cl-loop for row-idx from 0 below (length grid-data) until found
               do (cl-loop for col-idx from 1 below (length (nth row-idx grid-data)) until found
                           do (let* ((cell (nth col-idx (nth row-idx grid-data)))
                                     (cell-value (grid-cell-raw-input cell)))
                                (when (or (string-empty-p cell-value)
                                         (string-blank-p cell-value))
                                  (setq found t)
                                  (org-supertag-grid--move-to-cell row-idx col-idx)
                                  (org-supertag-grid--highlight-cell row-idx col-idx))))
      ;; If no empty cell found, move to the first editable cell (first row, first field column)
      (unless found
        (when (and (> (length grid-data) 0) (> (length (car grid-data)) 1))
          (org-supertag-grid--move-to-cell 0 1)
          (org-supertag-grid--highlight-cell 0 1)))))))

(defun org-supertag-grid-next-line ()
  "Move to the cell in the next row, same column."
  (interactive)
  (let ((coords (org-supertag-grid-get-cell-at-point)))
    (when coords
      (let* ((current-row (plist-get coords :row))
             (current-col (plist-get coords :col))
             (grid-data org-supertag-grid--grid-data)
             (num-rows (length grid-data)))
        (if (and grid-data (> num-rows 0))
            (let ((target-row (1+ current-row)))
              ;; If we're at the end of the table, wrap to the first row
              (when (>= target-row num-rows)
                (setq target-row 0))
              ;; Move to the target cell
              (when (and (>= target-row 0) (< target-row num-rows))
                (org-supertag-grid--move-to-cell target-row current-col)
                (org-supertag-grid--highlight-cell target-row current-col)))
          ;; If no grid data, try to move to a reasonable position
          (message "No grid data available"))))))

(defun org-supertag-grid-previous-line ()
  "Move to the cell in the previous row, same column."
  (interactive)
  (let ((coords (org-supertag-grid-get-cell-at-point)))
    (when coords
      (let* ((current-row (plist-get coords :row))
             (current-col (plist-get coords :col))
             (grid-data org-supertag-grid--grid-data)
             (num-rows (length grid-data)))
        (if (and grid-data (> num-rows 0))
            (let ((target-row (1- current-row)))
              ;; If we're at the beginning of the table, wrap to the last row
              (when (< target-row 0)
                (setq target-row (1- num-rows)))
              ;; Move to the target cell
              (when (and (>= target-row 0) (< target-row num-rows))
                (org-supertag-grid--move-to-cell target-row current-col)
                (org-supertag-grid--highlight-cell target-row current-col)))
          ;; If no grid data, try to move to a reasonable position
          (message "No grid data available"))))))

(defun org-supertag-grid-next-cell ()
  "Move to the next cell in the table, wrapping to the next row."
  (interactive)
  (let ((coords (org-supertag-grid-get-cell-at-point)))
    (when coords
      (let* ((current-row (plist-get coords :row))
             (current-col (plist-get coords :col))
             (grid-data org-supertag-grid--grid-data)
             (num-rows (length grid-data))
             (num-cols (when grid-data (length (car grid-data)))))
        (if (and grid-data num-cols)
            (let ((target-row current-row)
                  (target-col (1+ current-col)))
              ;; If we're at the end of a row, wrap to the next row
              (when (>= target-col num-cols)
                (setq target-row (1+ target-row))
                (setq target-col 0))
              ;; If we're at the end of the table, wrap to the first row
              (when (>= target-row num-rows)
                (setq target-row 0))
              ;; Move to the target cell
              (when (and (>= target-row 0) (< target-row num-rows)
                         (>= target-col 0) (< target-col num-cols))
                (org-supertag-grid--move-to-cell target-row target-col)
                (org-supertag-grid--highlight-cell target-row target-col)))
          ;; If no grid data, try to move to a reasonable position
          (message "No grid data available"))))))

(defun org-supertag-grid-previous-cell ()
  "Move to the previous cell in the table, wrapping to the previous row."
  (interactive)
  (let ((coords (org-supertag-grid-get-cell-at-point)))
    (when coords
      (let* ((current-row (plist-get coords :row))
             (current-col (plist-get coords :col))
             (grid-data org-supertag-grid--grid-data)
             (num-rows (length grid-data))
             (num-cols (when grid-data (length (car grid-data)))))
        (if (and grid-data num-cols)
            (let ((target-row current-row)
                  (target-col (1- current-col)))
              ;; If we're at the beginning of a row, wrap to the previous row
              (when (< target-col 0)
                (setq target-row (1- target-row))
                (setq target-col (1- num-cols)))
              ;; If we're at the beginning of the table, wrap to the last row
              (when (< target-row 0)
                (setq target-row (1- num-rows)))
              ;; Move to the target cell
              (when (and (>= target-row 0) (< target-row num-rows)
                         (>= target-col 0) (< target-col num-cols))
                (org-supertag-grid--move-to-cell target-row target-col)
                (org-supertag-grid--highlight-cell target-row target-col)))
          ;; If no grid data, try to move to a reasonable position
          (message "No grid data available"))))))

(defun org-supertag-view-smart-edit ()
  "Smart edit function - calls the centralized field editor.
Immediately saves the field value to the database after editing."
  (interactive)
  (let ((inhibit-read-only t)) ; Allow editing in the read-only buffer
    (when-let* ((field-info (org-supertag-view-table-get-field-info)))
      (let* ((coords (plist-get field-info :coords))
             (row-idx (plist-get coords :row))
             (col-idx (plist-get coords :col))
             (cell (plist-get field-info :cell))
             (field-def (plist-get field-info :field-def))
             (node-id (plist-get field-info :node-id))
             (tag (plist-get field-info :tag)))
        (unless field-def
          (error "Cannot edit this column. Not a user-defined field."))

        (let ((new-value (org-supertag-field-read-and-validate-value
                          field-def (grid-cell-raw-input cell))))
          (when new-value
            (org-supertag-field-set-value node-id (plist-get field-def :name) new-value tag)
            ;; 1. Update the data model in memory
            (set-grid-cell-raw-input cell new-value)
            (set-grid-cell-computed-value cell new-value) ; For now, computed is same as raw
            ;; 2. Redraw only the affected cell
            (org-supertag-grid--redraw-cell row-idx col-idx)
            ;; 3. Move cursor to the cell below the edited cell (if it exists)
            (let ((grid-data org-supertag-grid--grid-data))
              (if (and grid-data (< (1+ row-idx) (length grid-data)))
                  (org-supertag-grid--move-to-cell (1+ row-idx) col-idx)
                ;; If no cell below, move to the same cell
                (org-supertag-grid--move-to-cell row-idx col-idx)))
            ;; If moving to the target cell fails, try to move to a reasonable position
            (unless (get-text-property (point) 'grid-cell)
              (message "Debug: moving to target cell failed, trying reasonable position")
              (goto-char (point-min))
              (search-forward "│" nil t)
              (forward-char 1))
            (message "Field updated: %s" new-value)))))))

(defun org-supertag-view--insert-content-table (tag)
  "Insert content related to TAG in current buffer using the new Grid View."
  (let* ((fields (org-supertag-get-all-fields-for-tag tag))
         (header-names (cons "Node" (mapcar (lambda (f) (plist-get f :name)) fields)))
          (header-cells (cl-loop for name in header-names for col from 0
                                 collect (make-grid-cell :raw-input name :computed-value name :row -1 :col col)))
         (content (org-supertag-view--get-related-nodes tag)))
    
    (if (not content)
        (insert (format "No content found related to tag #%s" tag))
      ;; Process content and create grid
      (let* ((grid-data (cl-loop for item in content for row-idx from 0
                                collect (let* ((node-id (plist-get item :id))
                                               (base-values (list (plist-get item :node)))
                                               (field-values
                                                 (mapcar (lambda (field)
                                                           (let* ((field-name (plist-get field :name))
                                                                  (value (org-supertag-field-get-value node-id field-name tag)))
                                                             (or value "")))
                                                         fields))
                                               (all-values (append base-values field-values)))
                                          (cl-loop for val in all-values for col-idx from 0
                                                   collect (make-grid-cell :raw-input val :computed-value val
                                                                          :row row-idx :col col-idx)))))
             ;; Calculate column widths
             (column-widths (org-supertag-grid--calculate-column-widths
                            header-names
                            (mapcar (lambda (row) (mapcar #'grid-cell-computed-value row))
                                    grid-data))))
          
          ;; Store grid data for later use
          (setq org-supertag-grid--grid-data grid-data)

          ;; Draw the table
          (insert (org-supertag-grid--draw-separator column-widths "┌" "┬" "┐") "\n")
          (insert (org-supertag-grid--draw-row header-cells column-widths) "\n")
          (insert (org-supertag-grid--draw-separator column-widths "├" "┼" "┤") "\n")
          (dolist (row grid-data)
            (insert (org-supertag-grid--draw-row row column-widths) "\n")
            (unless (eq row (car (last grid-data)))
              (insert (org-supertag-grid--draw-separator column-widths "├" "┼" "┤") "\n")))
          (insert (org-supertag-grid--draw-separator column-widths "└" "┴" "┘") "\n")
          
          ;; Auto-position cursor to the first empty cell
          (org-supertag-grid--move-to-first-empty-cell)))))


(defun org-supertag-view--get-field-index (col)
  "Get field index based on column position COL."
  (let ((pos 0)
        (current 0))
    (while (< current col)
      (setq current (+ current (if (= pos 0) 20 15)))
      (setq pos (1+ pos)))
    pos))

(defun org-supertag-view--get-current-tag ()
  "Get current tag from buffer name or buffer local variable."
  (let ((tag (if (boundp 'org-supertag-view-current-tag)
                 ;; First try to get from local variable
                 org-supertag-view-current-tag
               ;; Then try to extract from buffer name
               (when (string-match "\\*Org SuperTag Table View: \\([^*]+\\)\\*" (buffer-name))
                 (match-string 1 (buffer-name))))))
    ;; Normalize the tag by removing # prefix if present
    (when tag
      (if (string-prefix-p "#" tag)
          (substring tag 1)
        tag))))

(defun org-supertag-view-table-get-all-rows ()
  "Get all data rows in the table.
Returns a list, each element is a list of row data."
  (let ((rows '()))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "|Node|Type|Date" nil t)
        (forward-line 2) ;; Skip header and separator line
        
        ;; Collect each row
        (while (org-at-table-p)
          (let ((row-data (org-supertag-view-table-get-line)))
            (when row-data
              (push row-data rows)))
          (forward-line 1))))
    (nreverse rows)))

(defun org-supertag-view-table-update-field (node-id field-name field-value tag-id)
  "Update field value in database.
NODE-ID is the node identifier
FIELD-NAME is the field name
FIELD-VALUE is the new value
TAG-ID is the tag identifier
Returns t if update successful, nil if failed."
  (condition-case err
      (progn
        ;; 1. Update database by calling the centralized field function
        (org-supertag-field-set-value node-id field-name field-value tag-id)

        ;; The file update part is removed to decouple from org-properties.
        ;; The database is now the single source of truth.

        ;; 2. Mark database as dirty and schedule save
        ;; This is now handled by org-supertag-field-set-value,
        ;; so it can be removed from here.
        t)  ;; Return success
    (error
     (message "Error updating field %s: %s" field-name (error-message-string err))
     nil)))

(defun org-supertag-view-cancel-edit ()
  "Cancel editing and restore original view."
  (interactive)
  (when (yes-or-no-p "Cancel editing? Changes will be lost.")
    (let ((tag (org-supertag-view--get-current-tag)))
      (quit-window)
      (org-supertag-view--show-content-table tag))))


(defun org-supertag-view-table-delete-field ()
  "Delete the field at the current column from the tag definition and refresh.
Only allows deletion of fields that belong to the current tag, not inherited fields."
  (interactive)
  (let* ((field-info (org-supertag-view-table-get-field-info))
         (tag-id (plist-get field-info :tag))
         (field-def (plist-get field-info :field-def)))
    (if-let ((field-name (and field-def (plist-get field-def :name))))
        ;; Check if this is an inherited field
        (if (plist-get field-def :projected-from)
            (message "Cannot delete inherited field '%s'. It belongs to parent tag '%s'." 
                     field-name (plist-get field-def :projected-from))
          (when (yes-or-no-p (format "Are you sure you want to delete the field '%s'?" field-name))
            (org-supertag-tag--remove-field tag-id field-name)
            (message "Field '%s' deleted. Refreshing..." field-name)
            (org-supertag-view-table-refresh)))
      (message "Cannot delete. Please place the cursor on a valid field column."))))

(defun org-supertag-view-table-add-field ()
  "Add a new field to the current tag and refresh the table view."
  (interactive)
  (let* ((tag-id (org-supertag-view--get-current-tag)))
    (when tag-id
      (when-let* ((field-def (org-supertag-tag--create-field-definition)))
        (org-supertag-tag-add-field tag-id field-def)
        (message "Field '%s' added successfully. Refreshing..." (plist-get field-def :name))
        ;; A full, data-refetching refresh is needed as the table structure has changed.
        (org-supertag-view-table-refresh)))))

(defun org-supertag-view-table-edit-field-definition ()
  "Edit the definition of the field at the current column and refresh."
  (interactive)
  (let* ((field-info (org-supertag-view-table-get-field-info))
         (tag-id (plist-get field-info :tag))
         (field-def (plist-get field-info :field-def)))
    (if field-def
        (let* ((field-name (plist-get field-def :name))
               (current-type (plist-get field-def :type))
               (current-options (plist-get field-def :options))
               (action (completing-read "Edit: " '("Name" "Type and Options") nil t)))
          (cond
           ((string= action "Name")
            (let ((new-name (read-string (format "New name for field '%s': " field-name) nil nil field-name)))
              (when (and new-name (not (string-empty-p new-name)) (not (string= new-name field-name)))
                (org-supertag-tag-rename-field tag-id field-name new-name)
                (message "Field renamed to '%s'. Refreshing..." new-name)
                (org-supertag-view-table-refresh))))
           ((string= action "Type and Options")
            (let* ((field-type-choices (org-supertag-get-field-types))
                   (field-type-str (completing-read "Field type: "
                                                    (mapcar #'car field-type-choices)
                                                    nil t nil nil (symbol-name current-type)))
                   (new-type (cdr (assoc field-type-str field-type-choices)))
                   (new-field-def (list :name field-name :type new-type)))
              ;; If it's options type, ask for options
              (when (eq new-type 'options)
                (let* ((current-options-str (if (and (eq current-type 'options) current-options)
                                                (mapconcat #'identity current-options ", ")
                                              ""))
                       (options-input (read-string "Options (comma separated): " current-options-str))
                       (new-options (split-string options-input "," t "[ \t\n\r]+")))
                  (setq new-field-def (plist-put new-field-def :options new-options))))
              (org-supertag-tag--update-field-definition tag-id field-name new-field-def)
              (message "Field '%s' updated. Refreshing..." field-name)
              (org-supertag-view-table-refresh)))
          (t t))
      (message "Cannot edit. Please place the cursor on a valid field column.")))))

;;----------------------------------------------------------------------
;; Mode definitions
;;----------------------------------------------------------------------

(defun org-supertag-view-table-refresh (&optional node-id-to-restore col-to-restore)
  "Refresh the table view by re-generating its content."
  (interactive)
  (let ((tag (org-supertag-view--get-current-tag)))
    (when tag
      (org-supertag-view--show-content-table tag node-id-to-restore col-to-restore))))


;; New: Insert image path into a cell
(defun org-supertag-grid--insert-image-path ()
  "Insert an image path into the current cell."
  (interactive)
  (when-let* ((coords (org-supertag-grid-get-cell-at-point))
              (image-file (read-file-name "Select image: " nil nil t nil
                                        (lambda (f) (string-match-p "\\.\\(png\\|jpe?g\\|gif\\)$" 
                                                                  (downcase f))))))
    (let* ((row-idx (plist-get coords :row))
           (col-idx (plist-get coords :col))
           (cell (nth col-idx (nth row-idx org-supertag-grid--grid-data))))
      
      ;; Update cell data
      (set-grid-cell-raw-input cell image-file)
      (set-grid-cell-computed-value cell image-file)
      
      ;; Redraw cell
      (let ((inhibit-read-only t))
        (org-supertag-grid--redraw-cell row-idx col-idx))
      
      (message "Image path inserted: %s" (file-name-nondirectory image-file)))))

(defun org-supertag-grid--test-image-loading (file-path)
  "Test image loading functionality for debugging purposes.
FILE-PATH is the path to the image file to test.
This function will test all the image loading strategies and provide detailed output."
  (interactive "fSelect image file to test: ")
  (message "=== Testing Image Loading for: %s ===" file-path)
  
  ;; Test file existence
  (message "1. File existence check:")
  (if (file-exists-p file-path)
      (message "   ✓ File exists")
    (message "   ✗ File does not exist")
    (message "=== Test Complete - File not found ===")
    (return))
  
  ;; Test file readability
  (message "2. File readability check:")
  (if (file-readable-p file-path)
      (message "   ✓ File is readable")
    (message "   ✗ File is not readable")
    (message "=== Test Complete - File not readable ===")
    (return))
  
  ;; Test image type detection
  (message "3. Image type detection:")
  (let ((detected-type (image-type-from-file-name file-path)))
    (if detected-type
        (message "   ✓ Detected type: %s" detected-type)
      (message "   ✗ Could not detect image type"))
    
    ;; Test if type is available
    (if (and detected-type (image-type-available-p detected-type))
        (message "   ✓ Type is available in Emacs")
      (message "   ✗ Type is not available in Emacs")))
  
  ;; Test the main function
  (message "4. Testing main image loading function:")
  (let ((result (org-supertag-grid--get-loadable-image-source file-path)))
    (if result
        (message "   ✓ Success: %s" result)
      (message "   ✗ Failed to load image")))
  
  (message "=== Test Complete ==="))

(define-derived-mode org-supertag-view-table-mode org-mode "SuperTag-Table"
  "Major mode for displaying tag content in table format.
This mode is based on org-mode to ensure compatibility with org table functions."
  :group 'org-supertag
  (setq-local org-element-use-cache nil)
  (setq truncate-lines nil) ; Allow multi-line content to avoid truncation
  (setq buffer-read-only t)
  (setq header-line-format
        (propertize " Org-Supertag Table View" 'face '(:weight bold)))
  (let ((map (make-sparse-keymap)))
    ;; For debugging highlight
    (define-key map (kbd "h") #'org-supertag-grid-get-cell-at-point)
    ;; Define the most critical editing keys first to ensure they have the highest priority
    (define-key map (kbd "e") (lambda ()
                               (interactive)
                               (let ((inhibit-read-only t))
                                 (call-interactively 'org-supertag-view-smart-edit))))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'org-supertag-view-table-refresh)
    (define-key map (kbd "v") 'org-supertag-view-table-node-at-point)
    (define-key map (kbd "V") 'org-supertag-view-table-view-all-nodes)
    ;; (define-key map (kbd "m") 'org-supertag-view-manage-relations) ; Removed
    ;; New keys for field management
    (define-key map (kbd "a") 'org-supertag-view-table-add-field)
    (define-key map (kbd "D") 'org-supertag-view-table-delete-field)
    (define-key map (kbd "E") 'org-supertag-view-table-edit-field-definition)
    ;; Help function for org-mode markup
    (define-key map (kbd "?") 'org-supertag-grid--show-markup-help)
    ;; Debug function for testing image loading
    (define-key map (kbd "C-c C-t") 'org-supertag-grid--test-image-loading)
    ;; Debug function for image alignment
    (define-key map (kbd "C-c C-d") 'org-supertag-grid--debug-image-alignment)
    ;; Test simple image slicing
    (define-key map (kbd "C-c C-x") 'org-supertag-grid--test-simple-slice)
    ;; Adjust image column width
    (define-key map (kbd "w") 'org-supertag-grid--adjust-image-column-width)
    

    (define-key map (kbd "C-c C-i") 'org-supertag-grid--insert-image-path)
    (define-key map (kbd "C-c C-k") 'org-supertag-view-cancel-edit)
    (define-key map (kbd "<tab>") #'org-supertag-grid-next-cell)
    (define-key map (kbd "<backtab>") #'org-supertag-grid-previous-cell)
    (define-key map (kbd "M-p") 'org-table-copy-down)
    (define-key map (kbd "n") #'org-supertag-grid-next-line)
    (define-key map (kbd "p") #'org-supertag-grid-previous-line)
    (define-key map (kbd "2") 'org-supertag-view-switch-to-discover)
    (define-key map (kbd "3") 'org-supertag-view-switch-to-columns)
    (set-keymap-parent map org-mode-map)
    (use-local-map map)))

(defun org-supertag-view-table-setup-keys ()
  "Set up key bindings for table view mode."
  (interactive)
  ;; Set up table view and edit functions
  (define-key org-supertag-view-table-mode-map (kbd "e")
              (lambda ()
                (interactive)
                (let ((inhibit-read-only t))
                  (call-interactively 'org-supertag-view-smart-edit))))
  (define-key org-supertag-view-table-mode-map (kbd "v")
              'org-supertag-view-table-node-at-point)
  (define-key org-supertag-view-table-setup-keys (kbd "g")
              'org-supertag-view-table-refresh)
  (define-key org-supertag-view-table-setup-keys (kbd "q")
              'quit-window))


;;;###autoload
(defun org-supertag-view-table (&optional tag)
  "Show content related to a tag in table view format.
If TAG is provided, show content for that tag.
If point is on a tag, show content for that tag.
Otherwise, prompt for a tag using completion.

The table view displays:
- Tag properties and description
- A table of related nodes with their properties
- Operation instructions for navigation and management"
  (interactive)
  (let ((tag-to-use (or tag
                       (org-supertag-view--get-tag-name)
                       (completing-read "View tag in table format: "
                                      (org-supertag-view--get-all-tags)
                                      nil t))))
    (org-supertag-view--show-content-table tag-to-use)))

(define-obsolete-function-alias 'org-supertag-view-tag-only 'org-supertag-view-table "1.0")

(provide 'org-supertag-view-table) 


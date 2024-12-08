;;; org-supertag-ui.el --- UI components for org-supertag -*- lexical-binding: t; -*-

(require 'org)
(require 'org-supertag-api)
(require 'org-supertag-field)
(require 'org-supertag-base)
(require 'transient)

;; 模板数据结构
(cl-defstruct org-supertag-template
  id              ; 模板ID
  tag-name        ; 关联的标签名
  display-name    ; 显示名称
  description     ; 描述
  fields          ; 字段定义
  icon            ; 图标(可选)
  color)          ; 颜色(可选)

(defcustom org-supertag-tag-template-file
  (expand-file-name "templates/tag-templates.el"
                   org-supertag-data-directory)
  "标签模板数据文件路径."
  :type 'file
  :group 'org-supertag)

;; 模板数据存储
(defvar org-supertag--tag-templates (make-hash-table :test 'equal)
  "标签模板数据存储 template-id -> plist.")

(defun org-supertag-template-ensure-directory ()
  "确保模板目录存在."
  (let ((dir (file-name-directory org-supertag-tag-template-file)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

;;; 模板数据访问辅助函数
(defun org-supertag-get-template (id)
  "获取模板数据.
ID 是模板的唯一标识符."
  (when-let ((plist (org-supertag-db-get id)))
    (org-supertag-template-from-plist plist)))

(defun org-supertag-find-templates ()
  "获取所有模板."
  (let ((template-plists (org-supertag-db-find :type :template)))
    (mapcar #'org-supertag-template-from-plist template-plists)))

(defun org-supertag-get-template-tag (template-id)
  "获取模板关联的标签.
TEMPLATE-ID 是模板ID."
  (org-supertag-db-get-linked template-id :template-tag))

(defun org-supertag-link-template-tag (template-id tag-name)
  "建立模板和标签的关联.
TEMPLATE-ID 是模板ID
TAG-NAME 是标签名称."
  (org-supertag-db-link :template-tag template-id tag-name)
  (org-supertag-db-link :tag-template tag-name template-id))

;;; 模板数据保存和加载
(defun org-supertag-template-save-all ()
  "Save all templates to file."
  (message "Saving templates to %s" org-supertag-tag-template-file)
  (org-supertag-template-ensure-directory)
  (with-temp-buffer
    (let* ((print-level nil)
           (print-length nil)
           (templates-alist (org-supertag--hash-to-alist org-supertag--tag-templates))
           (data `(setq org-supertag--tag-templates
                       ',(org-supertag--hash-to-alist org-supertag--tag-templates))))
      (message "Templates alist: %S" templates-alist)
      (message "Saving data: %S" data)
      (print data (current-buffer))
      (write-region (point-min) (point-max)
                   org-supertag-tag-template-file nil 'silent))))

(defun org-supertag-template-load ()
  "从文件加载模板数据."
  (when (file-exists-p org-supertag-tag-template-file)
    (load org-supertag-tag-template-file)))

(defun org-supertag-template-initialize ()
  "初始化模板系统."
  (org-supertag-template-ensure-directory)
  (unless (file-exists-p org-supertag-tag-template-file)
    (with-temp-file org-supertag-tag-template-file
      (insert ";; Org Supertag Tag Templates\n\n"))))

;; 在包加载时初始化模板系统
(org-supertag-template-initialize)

(defvar org-supertag-template-mode-map
  (let ((map (make-sparse-keymap)))
    ;; 基础命令
    (define-key map (kbd "q") 'org-supertag-template-quit)
    (define-key map (kbd "g") 'org-supertag-template-refresh)
    (define-key map (kbd "?") 'org-supertag-template-edit-dispatch)
    (define-key map (kbd "s") 'org-supertag-template-save)
    (define-key map (kbd "C-c C-c") 'org-supertag-template-save-and-exit)
    (define-key map (kbd "C-c C-k") 'org-supertag-template-quit)
    
    ;; 撤销/重做
    (define-key map (kbd "u") 'org-supertag-template-undo)
    (define-key map [remap undo] 'org-supertag-template-undo)
    (define-key map (kbd "M-u") 'org-supertag-template-redo)
    
    ;; 保存
    (define-key map (kbd "s") 'org-supertag-template-save)
    (define-key map (kbd "C-c C-c") 'org-supertag-template-save-and-exit)
    
    ;; 标签名设置
    (define-key map (kbd "t") 'org-supertag-template-set-tag-name)

    ;; 字段添加命令
    (define-key map (kbd "a") 'org-supertag-template-add-field-command)
    (define-key map (kbd "d") 'org-supertag-template-remove-field-command)
    (define-key map (kbd "e") 'org-supertag-template-edit-field-command)
    (define-key map (kbd "M-p") 'org-supertag-template-move-field-up)
    (define-key map (kbd "M-n") 'org-supertag-template-move-field-down)
    
    ;; 模板属性编辑命令
    (define-key map (kbd "T r") 'org-supertag-template-rename-command)
    (define-key map (kbd "T C") 'org-supertag-template-copy-command)
    (define-key map (kbd "T d") 'org-supertag-template-delete-command)
    
    ;; 快速编辑功能
    (define-key map (kbd "A") 'org-supertag-template-quick-add-field)
    (define-key map (kbd "E") 'org-supertag-template-quick-edit-field)
    (define-key map (kbd "M") 'org-supertag-template-move-field-to)
    (define-key map (kbd "B") 'org-supertag-template-bulk-add-fields)
    (define-key map (kbd "I") 'org-supertag-template-infer-fields)
    (define-key map (kbd "W") 'org-supertag-template-copy-field)
    (define-key map (kbd "Y") 'org-supertag-template-paste-field)
    
    ;; 扩展功能
    (define-key map (kbd "s") 'org-supertag-template-sort-fields)
    (define-key map (kbd "d") 'org-supertag-template-duplicate-field)
    (define-key map (kbd "G") 'org-supertag-template-group-fields)
    (define-key map (kbd "v") 'org-supertag-template-toggle-field-visibility)
    (define-key map (kbd "c") 'org-supertag-template-set-field-condition)
    (define-key map (kbd "t") 'org-supertag-template-set-field-transform)
    
    map)
  "Keymap for `org-supertag-template-mode'.")

(define-derived-mode org-supertag-template-mode magit-section-mode "Supertag"
  "Major mode for editing org-supertag templates."
  :group 'org-supertag
  ;; 基础设置
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq-local truncate-lines t)
  
  ;; magit-section 钩子
  (add-hook 'magit-section-highlight-hook #'magit-section-highlight nil t)
  (add-hook 'magit-section-movement-hook #'magit-section-movement-hook nil t)
  (add-hook 'magit-section-update-hook #'magit-section-update nil t)
  
  ;; 确保 section 可以正确高亮
  (setq-local magit-section-highlight-overlays nil)
  (setq-local magit-section-highlighted-section nil)
  (setq-local magit-section-highlighted-sections nil)
  (setq-local magit-section-unhighlight-sections nil))

(defun org-supertag-template-refresh ()
  "刷新模板编辑器buffer."
  (interactive)
  (let ((inhibit-read-only t)
        ;; 保存当前位置和section状态
        (old-point (point))
        (old-sections (magit-section-get-visibility-cache))
        (window-start (window-start)))
    
    (condition-case err
        (progn
          ;; 清空buffer
          (erase-buffer)
          
          ;; 重新插入内容
          (magit-insert-section (template)
            (org-supertag-template--insert-header)
            (org-supertag-template--insert-fields))
          
          ;; 恢复section状态
          (magit-section-set-visibility-cache old-sections)
          
          ;; 恢复光标位置
          (goto-char (min old-point (point-max)))
          (set-window-start (selected-window) window-start))
      
      ;; 错误处理
      (error
       (message "Error refreshing template buffer: %s" (error-message-string err))
       ;; 如果刷新失败，尝试重置到基本状态
       (erase-buffer)
       (insert "Error refreshing template. Press 'g' to try again.\n")
       (insert (format "Error: %s" (error-message-string err)))))))

;; 辅助函数：遍历所有sections
(defun org-supertag-section-map (fn)
  "遍历所有sections并对每个section执行FN函数."
  (when magit-root-section
    (let ((stack (list magit-root-section)))
      (while stack
        (let ((section (pop stack)))
          (funcall fn section)
          ;; 将子section加入到stack中
          (when-let ((children (oref section children)))
            (setq stack (append children stack))))))))

;; 辅助函数：获取section可见性状态
(defun org-supertag-section-get-visibility-cache ()
  "返回section可见性状态的alist."
  (let (cache)
    (org-supertag-section-map
     (lambda (section)
       (push (cons (oref section value)
                  (oref section hidden))
             cache)))
    cache))

;; 辅助函数：恢复section可见性状态
(defun magit-section-set-visibility-cache (cache)
  "Restore section visibility from CACHE."
  (org-supertag-section-map
   (lambda (section)
     (when-let ((status (assoc (oref section value) cache)))
       (oset section hidden (cdr status))))))

(defun org-supertag-section-set-visibility-cache (cache)
  "从CACHE恢复section可见性."
  (org-supertag-section-map
   (lambda (section)
     (when-let ((status (assoc (oref section value) cache)))
       (oset section hidden (cdr status))))))

(defun org-supertag-template--insert-header ()
  "插入模板编辑器的头部信息."
  (magit-insert-section (header)
    (let* ((template org-supertag--current-template)
           (tag-name (org-supertag-template-tag-name template))
           (display-name (org-supertag-template-display-name template)))
      ;; 显示模板名称和修改状态
      (insert (format "Template: %s %s\n"
                     (propertize (org-supertag-template-id template)
                               'face 'magit-section-heading)
                     (if (org-supertag-template--modified-p)
                         (propertize "[*]" 'face 'warning)
                       "")))
      
      ;; 显示标签预览
      (insert (format "Tag: %s (%s)\n"
                     (propertize tag-name 'face 'magit-section-secondary-heading)
                     display-name))
      
      ;; 显示字段数量
      (insert (format "Fields: %d\n"
                     (length (org-supertag-template-fields template))))
      
      ;; 显示撤销/重做状态
      (when (or org-supertag--undo-list org-supertag--redo-list)
        (insert (format "[Undo: %d] [Redo: %d]  "
                       (length org-supertag--undo-list)
                       (length org-supertag--redo-list))))
      
      ;; 显示最后一次操作
      (when org-supertag--undo-list
        (insert (format "Last: %s"
                       (org-supertag-template-change-description
                        (car org-supertag--undo-list)))))
      
      (insert "\n\n"))))

(defun org-supertag-template--insert-fields ()
  "插入字段列表."
  (magit-insert-section (fields)
    (insert "Fields:\n")
    (let ((fields (plist-get org-supertag--current-template :fields)))
      (seq-do-indexed
       (lambda (field-def index)
         (let ((type (plist-get field-def :type))
               (name (plist-get field-def :name))
               (required (plist-get field-def :required))
               (default (plist-get field-def :default)))
           ;; 创建字段section
           (magit-insert-section (field index)
             ;; 显示字段类型标识
             (insert (format "  %c %-8s  "
                           (pcase type
                             (:property ?P)
                             (:drawer ?D)
                             (:todo ?T)
                             (:string ?S)
                             (:number ?N)
                             (:date ?D)
                             (:planning ?P)
                             (:priority ?R)
                             (:tags ?T)
                             (:enum ?E)
                             (_ ??))
                           type))
             ;; 显示字段名称
             (insert (format "%-10s" name))
             ;; 显示必填状态
             (insert (if required
                        (propertize "required" 'face 'magit-section-highlight)
                      "optional"))
             ;; 显示默认值
             (insert (format "    default: %S" default))
             ;; 如果是最近修改的字段，显示标记
             (when (and org-supertag--undo-list
                       (eq index (cdr (org-supertag-template-change-data
                                     (car org-supertag--undo-list)))))
               (insert (propertize "  [changed]" 'face 'magit-section-highlight)))
             (insert "\n"))))
       fields))))

(defun org-supertag-template--modified-p ()
  "返回模板是否被修改."
  (and org-supertag--current-template
       org-supertag--original-template
       (not (equal org-supertag--current-template
                  org-supertag--original-template))))

(defun org-supertag-template-quit ()
  "退出模板编辑器."
  (interactive)
  (when (or (not (org-supertag-template--modified-p))
            (yes-or-no-p "Template modified. Quit anyway? "))
    (quit-window)))

(defun org-supertag-template-help ()
  "显示帮助信息."
  (interactive)
  (message "Help: press 'q' to quit, 'g' to refresh"))

(defun org-supertag-tag-template-edit (template-name)
  "编辑已有的标签模板.
TEMPLATE-NAME 是要编辑的模板名称."
  (interactive
   (list (completing-read "选择模板: "
                         (mapcar #'car (org-supertag-db-find :type :template))
                         nil t)))
  (let ((template (org-supertag-db-get template-name)))
    (unless template
      (error "Template not found: %s" template-name))
    (let ((buf (get-buffer-create "*Org Supertag Template*")))
      (with-current-buffer buf
        (org-supertag-template-mode)
        (setq org-supertag--current-template template)
        (setq org-supertag--original-template (copy-tree template))
        (setq org-supertag--undo-list nil)
        (setq org-supertag--redo-list nil)
        (org-supertag-template-refresh))
      (switch-to-buffer buf))))


;;; 数据结构和状态管理

(defvar-local org-supertag--current-template nil
  "当前正在编辑的模板数据.")

(defvar-local org-supertag--original-template nil
  "原始模板数据，用于比较修改.")

(defvar-local org-supertag--undo-list nil
  "撤销历史列表.")

(defvar-local org-supertag--redo-list nil
  "重做历史列表.")

(cl-defstruct (org-supertag-template-change (:constructor org-supertag-template-change-create)
                                            (:copier org-supertag-template-change-copy))
  type               ; 改动类型 (:add-field :remove-field :modify-field :rename )
  description        ; 改动描述
  data              ; 改动数据
  undo-func         ; 撤销函数
  redo-func)        ; 重做函数

(defun org-supertag-template--record-change (change)
  "记录一个改动.
CHANGE 是 org-supertag-template-change 结构体"
  (push change org-supertag--undo-list)
  (setq org-supertag--redo-list nil))

(defun org-supertag-template--modified-p ()
  "检查模板是否被修改."
  (not (equal org-supertag--current-template
              org-supertag--original-template)))

(defun org-supertag-template--init (template-name)
  "初始化模板编辑器的状态.
TEMPLATE-NAME 是要编辑的模板名称"
  (let ((template (org-supertag-db-get template-name)))
    (setq org-supertag--current-template template)
    (setq org-supertag--original-template (copy-tree template))
    (setq org-supertag--undo-list nil)
    (setq org-supertag--redo-list nil)))

(defun org-supertag-template-undo ()
  "撤销最后一次改动."
  (interactive)
  (when org-supertag--undo-list
    (let ((change (pop org-supertag--undo-list)))
      (funcall (org-supertag-template-change-undo-func change))
      (push change org-supertag--redo-list)
      (org-supertag-template-refresh))))

(defun org-supertag-template-redo ()
  "重做最后一次撤销的改动."
  (interactive)
  (when org-supertag--redo-list
    (let ((change (pop org-supertag--redo-list)))
      (funcall (org-supertag-template-change-redo-func change))
      (push change org-supertag--undo-list)
      (org-supertag-template-refresh))))

;; 示例改动记录函数
(defun org-supertag-template--add-field (field-def)
  "添加一个字段.
FIELD-DEF 是字段定义"
  (unless (org-supertag-template-field-validate field-def)
    (error "Invalid field definition"))
  (let* ((fields (org-supertag-template-fields org-supertag--current-template))
         (new-fields (vconcat fields (vector field-def)))
         (change (org-supertag-template-change-create
                 :type :add-field
                 :description (format "Added field %s" (plist-get field-def :name))
                 :data field-def
                 :undo-func (lambda ()
                            (setf (org-supertag-template-fields org-supertag--current-template)
                                  fields))
                 :redo-func (lambda ()
                            (setf (org-supertag-template-fields org-supertag--current-template)
                                  new-fields)))))
    (setf (org-supertag-template-fields org-supertag--current-template) new-fields)
    (org-supertag-template--record-change change)))

;; 字段操作函数
(defun org-supertag-template--remove-field (index)
  "删除指定索引的字段.
INDEX 是字段的索引"
  (let* ((fields (org-supertag-template-fields org-supertag--current-template))
         (field-def (aref fields index))
         (new-fields (vconcat (seq-subseq fields 0 index)
                             (seq-subseq fields (1+ index))))
         (change (org-supertag-template-change-create
                 :type :remove-field
                 :description (format "Removed field %s" (plist-get field-def :name))
                 :data (cons index field-def)
                 :undo-func (lambda ()
                            (setf (org-supertag-template-fields org-supertag--current-template)
                                  fields))
                 :redo-func (lambda ()
                            (setf (org-supertag-template-fields org-supertag--current-template)
                                  new-fields)))))
    (setf (org-supertag-template-fields org-supertag--current-template) new-fields)
    (org-supertag-template--record-change change)))

(defun org-supertag-template--modify-field (field-index new-field-def)
  "修改指定索引的字段.
FIELD-INDEX 是字段在列表中的索引
NEW-FIELD-DEF 是新的字段定义"
  (let* ((fields (plist-get org-supertag--current-template :fields))
         (old-field-def (aref fields field-index))
         (new-fields (copy-sequence fields))
         (change (org-supertag-template-change-create
                 :type :modify-field
                 :description (format "Modified field %s"
                                    (plist-get old-field-def :name))
                 :data (cons old-field-def new-field-def)
                 :undo-func (lambda ()
                            (aset new-fields field-index old-field-def)
                            (setq org-supertag--current-template
                                  (plist-put org-supertag--current-template
                                           :fields new-fields)))
                 :redo-func (lambda ()
                            (aset new-fields field-index new-field-def)
                            (setq org-supertag--current-template
                                  (plist-put org-supertag--current-template
                                           :fields new-fields))))))
    (aset new-fields field-index new-field-def)
    (setq org-supertag--current-template
          (plist-put org-supertag--current-template :fields new-fields))
    (org-supertag-template--record-change change)))

(defun org-supertag-template--move-field (field-index new-index)
  "移动字段到新位置.
FIELD-INDEX 是字段当前的索引
NEW-INDEX 是字段的目标索引"
  (let* ((fields (plist-get org-supertag--current-template :fields))
         (field-def (aref fields field-index))
         (new-fields (make-vector (length fields) nil))
         (change (org-supertag-template-change-create
                 :type :move-field
                 :description (format "Moved field %s"
                                    (plist-get field-def :name))
                 :data (cons field-index new-index)
                 :undo-func (lambda ()
                            (setq org-supertag--current-template
                                  (plist-put org-supertag--current-template
                                           :fields fields)))
                 :redo-func (lambda ()
                            (setq org-supertag--current-template
                                  (plist-put org-supertag--current-template
                                           :fields new-fields))))))
    ;; 构建新的字段序列
    (dotimes (i (length fields))
      (cond
       ;; 新位置放入移动的字段
       ((= i new-index)
        (aset new-fields i field-def))
       ;; 向上移动时的字段重排
       ((and (< field-index new-index)
             (>= i field-index)
             (< i new-index))
        (aset new-fields i (aref fields (1+ i))))
       ;; 向下移动时的字段重排
       ((and (> field-index new-index)
             (>= i new-index)
             (< i field-index))
        (aset new-fields i (aref fields (1- i))))
       ;; 其他位置保持不变
       (t
        (aset new-fields i (aref fields i)))))
    
    (setq org-supertag--current-template
          (plist-put org-supertag--current-template :fields new-fields))
    (org-supertag-template--record-change change)))

;; Set tag name
(defun org-supertag-template-set-tag-name (name)
  "设置模板的标签名."
  (interactive "sEnter tag name: ")
  (let ((change (org-supertag-template-change-create
                 :type :rename
                 :description (format "Changed tag name to %s" name)
                 :data (cons (org-supertag-template-tag-name org-supertag--current-template) name)
                 :undo-func (lambda ()
                            (setf (org-supertag-template-tag-name org-supertag--current-template)
                                  (car (org-supertag-template-change-data change))))
                 :redo-func (lambda ()
                            (setf (org-supertag-template-tag-name org-supertag--current-template)
                                  name)))))
    (setf (org-supertag-template-tag-name org-supertag--current-template) name)
    (org-supertag-template--record-change change)
    (org-supertag-template-refresh)))

;; 字段添加命令
(defun org-supertag-template-add-field-command ()
  "添加字段的命令菜单."
  (interactive)
  (let* ((type-char (read-char-choice
                     "Field type [p]roperty [d]rawer [t]odo [s]tring [n]umber [D]ate [P]lanning [r]priority [T]ags [e]num: "
                     '(?p ?d ?t ?s ?n ?D ?P ?r ?T ?e)))
         (type (pcase type-char
                (?p :property)
                (?d :drawer)
                (?t :todo)
                (?s :string)
                (?n :number)
                (?D :date)
                (?P :planning)
                (?r :priority)
                (?T :tags)
                (?e :enum)))
         (name (read-string "Field name: "))
         (required (y-or-n-p "Required? "))
         (default (read-string "Default value: "))
         ;; 创建一个正确的字段定义列表
         (field-def `(:type ,type
                     :name ,name
                     :required ,required
                     :default ,default)))
    ;; 添加到字段向量中
    (org-supertag-template--add-field field-def)
    (org-supertag-template-refresh)))

;; 字段删除命令
(defun org-supertag-template-remove-field-command ()
  "删除当前字段."
  (interactive)
  (when-let* ((section (magit-current-section))
              (field-index (oref section value)))
    (org-supertag-template--remove-field field-index)
    (org-supertag-template-refresh)))

;; 字段编辑命令
(defun org-supertag-template-edit-field-command ()
  "编辑当前字段."
  (interactive)
  (when-let* ((section (magit-current-section))
              (field-index (oref section value))
              (fields (plist-get org-supertag--current-template :fields))
              (field-def (aref fields field-index)))
    (let* ((type (plist-get field-def :type))
           (name (read-string "Field name: " (plist-get field-def :name)))
           (required (y-or-n-p "Required? " (plist-get field-def :required)))
           (default (read-string "Default value: " (plist-get field-def :default)))
           (new-field-def (list :type type
                               :name name
                               :required required
                               :default default)))
      (org-supertag-template--modify-field field-index new-field-def)
      (org-supertag-template-refresh))))

;; 字段移动命令
(defun org-supertag-template-move-field-up ()
  "向上移动当前字段."
  (interactive)
  (when-let* ((section (magit-current-section))
              (field-index (oref section value)))
    (unless (zerop field-index)
      (org-supertag-template--move-field field-index (1- field-index))
      (org-supertag-template-refresh))))

(defun org-supertag-template-move-field-down ()
  "向下移动当前字段."
  (interactive)
  (when-let* ((section (magit-current-section))
              (field-index (oref section value))
              (fields (plist-get org-supertag--current-template :fields)))
    (unless (= field-index (1- (length fields)))
      (org-supertag-template--move-field field-index (1+ field-index))
      (org-supertag-template-refresh))))

(defun org-supertag-template-sort-fields ()
  "对模板中的字段进行排序."
  (interactive)
  (when-let* ((fields (org-supertag-template-fields org-supertag--current-template))
              (sort-key (completing-read "Sort by: "
                                       '("name" "type" "required")
                                       nil t)))
    (let* ((sorted-fields
            (cl-sort (append fields nil) ; 转换为列表再排序
                    (pcase sort-key
                      ("name" #'string<)
                      ("type" (lambda (a b)
                              (string< (symbol-name (plist-get a :type))
                                     (symbol-name (plist-get b :type)))))
                      ("required" (lambda (a b)
                                  (and (plist-get a :required)
                                       (not (plist-get b :required))))))
                    :key (pcase sort-key
                          ("name" (lambda (f) (plist-get f :name)))
                          (_ #'identity)))))
      (setf (org-supertag-template-fields org-supertag--current-template)
            (vconcat sorted-fields))
      (org-supertag-template-refresh))))

(defun org-supertag-template-duplicate-field ()
  "复制当前字段并添加到模板中."
  (interactive)
  (when-let* ((field-def (org-supertag-ui--get-current-field)))
    (let* ((new-name (read-string "New field name: " 
                                 (concat (plist-get field-def :name) "-copy")))
           (new-field (plist-put (copy-sequence field-def) :name new-name)))
      (org-supertag-template--add-field new-field)
      (org-supertag-template-refresh))))

(defun org-supertag-template-group-fields ()
  "将选中的字段组合成一个组."
  (interactive)
  (let* ((fields (org-supertag-template-fields org-supertag--current-template))
         (field-names (mapcar (lambda (f) (plist-get f :name)) (append fields nil)))
         (selected (completing-read-multiple "Select fields to group: " field-names))
         (group-name (read-string "Group name: ")))
    (when (and selected group-name)
      ;; TODO: 实现字段分组功能
      (message "Field grouping will be implemented soon"))))

(defun org-supertag-template-toggle-field-visibility ()
  "切换字段的可见性."
  (interactive)
  (when-let* ((field-info (org-supertag-ui--field-at-point))
              (field-index (car field-info))
              (field-def (cdr field-info)))
    (let* ((new-field-def (plist-put (copy-sequence field-def)
                                    :hidden
                                    (not (plist-get field-def :hidden)))))
      (org-supertag-template--modify-field field-index new-field-def)
      (org-supertag-template-refresh))))

(defun org-supertag-template-set-field-condition ()
  "设置字段的显示条件."
  (interactive)
  (when-let* ((field-info (org-supertag-ui--field-at-point))
              (field-index (car field-info))
              (field-def (cdr field-info)))
    (let* ((fields (org-supertag-template-fields org-supertag--current-template))
           (field-names (mapcar (lambda (f) (plist-get f :name))
                               (append fields nil)))
           (dep-field (completing-read "Depends on field: " field-names))
           (condition (read-string "Show when (elisp expression): "
                                 (or (plist-get field-def :condition) "")))
           (new-field-def (plist-put (copy-sequence field-def)
                                   :condition
                                   (cons dep-field condition))))
      (org-supertag-template--modify-field field-index new-field-def)
      (org-supertag-template-refresh))))

(defun org-supertag-template-set-field-transform ()
  "设置字段值的转换函数."
  (interactive)
  (when-let* ((field-info (org-supertag-ui--field-at-point))
              (field-index (car field-info))
              (field-def (cdr field-info)))
    (let* ((transform-type (completing-read "Transform type: "
                                          '("input" "output" "validate")
                                          nil t))
           (transform (read-string (format "%s transform (elisp expression): "
                                         transform-type)
                                 (or (plist-get field-def
                                              (intern (concat ":" transform-type "-transform")))
                                     "")))
           (new-field-def (plist-put (copy-sequence field-def)
                                   (intern (concat ":" transform-type "-transform"))
                                   transform)))
      (org-supertag-template--modify-field field-index new-field-def)
      (org-supertag-template-refresh))))

;; 模板操作函数
(defun org-supertag-template-rename-command ()
  "重命名模板."
  (interactive)
  (let* ((current-name (org-supertag-template-id org-supertag--current-template))
         (new-name (read-string "New template name: " current-name)))
    (unless (equal new-name current-name)
      (let* ((template (copy-org-supertag-template org-supertag--current-template)))
        (setf (org-supertag-template-id template) new-name)
        ;; 使用 API 更新模板
        (org-supertag-update-template template)
        ;; 更新编辑器状态
        (setq org-supertag--current-template template)
        (org-supertag-template-refresh)))))

(defun org-supertag-template-copy-command ()
  "复制当前模板."
  (interactive)
  (let* ((current-id (org-supertag-template-id org-supertag--current-template))
         (new-id (read-string "New template ID: " (format "%s-copy" current-id)))
         (new-template (copy-org-supertag-template org-supertag--current-template)))
    (setf (org-supertag-template-id new-template) new-id)
    ;; 使用 API 创建新模板
    (org-supertag-create-template new-template)
    (message "Template copied as '%s'" new-id)))

(defun org-supertag-template-save ()
  "保存当前模板和对应的标签."
  (interactive)
  (message "Current template before save: %S" org-supertag--current-template)
  (let ((template org-supertag--current-template))
    (condition-case err
        (progn
          (org-supertag-create-template template)  ;; 使用新的 API
          (setq org-supertag--original-template (copy-org-supertag-template template))
          (setq org-supertag--undo-list nil)
          (setq org-supertag--redo-list nil)
          (message "Template saved successfully")
          (org-supertag-template-refresh))
      (error (message "Failed to save template: %s" (error-message-string err))))))


(defun org-supertag-template-save-and-exit ()
  "保存当前模板并退出编辑器."
  (interactive)
  (org-supertag-template-save)
  (quit-window))

;; 添加删除模板的命令
(defun org-supertag-template-delete-command ()
  "删除当前正在编辑的模板."
  (interactive)
  (let ((template-id (org-supertag-template-id org-supertag--current-template)))
    (when (and template-id
               (yes-or-no-p (format "确定要删除模板 '%s' 吗？" template-id)))
      (org-supertag-delete-template template-id)  ;; 使用 API 删除模板
      (quit-window)
      (message "模板 '%s' 已删除." template-id))))

(defun org-supertag-delete-template ()
  "从列表中选择并删除一个模板."
  (interactive)
  (let* ((templates (org-supertag-db-find :type :template))
         (template-name (completing-read "选择要删除的模板: " templates nil t)))
    (when (and template-name
               (yes-or-no-p (format "确定要删除模板 '%s' 吗？" template-name)))
      ;; 如果正在编辑这个模板，关闭编辑buffer
      (when-let ((buf (get-buffer "*Org Supertag Template*")))
        (with-current-buffer buf
          (when (equal template-name
                      (plist-get org-supertag--current-template :name))
            (kill-buffer buf))))
      
      ;; 使用数据库API删除模板
      (org-supertag-db-remove template-name)
      (message "模板 '%s' 已删除." template-name))))

;; 添加模板操作的辅助函数
(defun org-supertag-template-exists-p (template-name)
  "检查模板是否存在.
TEMPLATE-NAME 是模板名称"
  (when-let ((template (org-supertag-db-get template-name)))
    (eq (plist-get template :type) :template)))


(defun org-supertag-template-field-validate (field-def)
  "验证字段定义的有效性.
FIELD-DEF 是要验证的字段定义"
  (and (plist-get field-def :type)
       (stringp (plist-get field-def :name))
       (memq (plist-get field-def :required) '(t nil))
       (stringp (plist-get field-def :default))))

(defun org-supertag-template-field-get (template field-name)
  "从模板中获取指定名称的字段定义.
TEMPLATE 是模板数据
FIELD-NAME 是字段名称"
  (let ((fields (plist-get template :fields)))
    (seq-find (lambda (field)
                (string= (plist-get field :name) field-name))
              fields)))

;; 2. 增强创建模板函数
(defun org-supertag-create-tag-template (template)
  "创建标签模板并关联标签."
  (when (org-supertag-template-validate template)
    ;; 保存模板
    (org-supertag-db-put (org-supertag-template-id template) template)
    
    ;; 创建关联的标签
    (let* ((tag-name (org-supertag-template-tag-name template))
           (tag-props (list :type :tag
                          :name (org-supertag-template-display-name template)
                          :template (org-supertag-template-id template))))
      ;; 保存标签
      (org-supertag-db-put tag-name tag-props)
      
      ;; 建立双向关联
      (org-supertag-link :tag-template tag-name (org-supertag-template-id template))
      (org-supertag-link :template-tag (org-supertag-template-id template) tag-name))))


(defun org-supertag-tag-template-new ()
  "创建新的标签模板."
  (interactive)
  (let ((template (make-org-supertag-template
                  :id ""
                  :tag-name ""
                  :display-name ""
                  :description ""
                  :fields (vector))))
    (let ((buf (get-buffer-create "*Org Supertag Template*")))
      (save-current-buffer
        (set-buffer buf)
        (org-supertag-template-mode)
        (setq org-supertag--current-template template)
        (setq org-supertag--original-template (copy-org-supertag-template template))
        (setq org-supertag--undo-list nil)
        (setq org-supertag--redo-list nil)
        (org-supertag-template-refresh))
      (switch-to-buffer buf))))

(defun org-supertag-edit-tag-template (template-id)
  "编辑指定的标签模板."
  (interactive (list (completing-read "Template ID: "
                                    (mapcar #'org-supertag-template-id
                                            (org-supertag-find-templates)))))
  (when-let ((template (org-supertag-get-template template-id)))  ;; 使用 API 获取模板
    (let ((buf (get-buffer-create "*Org Supertag Template*")))
      (with-current-buffer buf
        (org-supertag-template-mode)
        (setq org-supertag--current-template template)
        (setq org-supertag--original-template (copy-org-supertag-template template))
        (setq org-supertag--undo-list nil)
        (setq org-supertag--redo-list nil)
        (org-supertag-template-refresh))
      (switch-to-buffer buf))))

;; ----------------------------------------------------------------------
;; 字段类型 UI 组件
;; ----------------------------------------------------------------------

(defun org-supertag-ui--read-field-value (field-def)
  "读取字段值.
FIELD-DEF 是字段定义"
  (let* ((type (plist-get field-def :type))
         (type-spec (org-supertag-get-field-type type))
         (validator (plist-get type-spec :validator))
         (current-value (plist-get field-def :value)))
    (pcase type
      ;; 基本类型
      ('string
       (read-string "String: " current-value))
      
      ;; 数值类型
      ('number
       (read-number "Number: " (or (and current-value (string-to-number current-value)) 0)))
      
      ;; 整数类型
      ('integer
       (let ((num (read-number "Integer: " (or (and current-value (string-to-number current-value)) 0)))
         (floor num)))
      
      ;; 日期类型
      ('date
       (org-read-date nil t nil "Date: "))
      
      ;; 时间类型
      ('time
       (let ((time (read-string "Time (HH:MM): " 
                               (or current-value 
                                   (format-time-string "%H:%M")))))
         (if (funcall validator time)
             time
           (user-error "Invalid time format. Please use HH:MM"))))
      
      ;; 日期时间类型
      ('datetime
       (let ((datetime (read-string "DateTime (YYYY-MM-DDTHH:MM:SSZ): "
                                  (or current-value
                                      (format-time-string "%Y-%m-%dT%H:%M:%SZ")))))
         (if (funcall validator datetime)
             datetime
           (user-error "Invalid datetime format"))))
      
      ;; 持续时间类型
      ('duration
       (read-string "Duration (e.g. 1h30m): " current-value))
      
      ;; 列表类型
      ('list
       (let ((items (split-string (read-string "Items (comma separated): " 
                                             (if (listp current-value)
                                                 (string-join current-value ",")
                                               current-value))
                                ",")))
         (mapcar #'string-trim items)))
      
      ;; 选项类型
      ('options
       (let ((choices (plist-get field-def :options)))
         (unless choices
           (user-error "No options defined for options field"))
         (completing-read "Choose option: " choices nil t current-value)))
      
      ;; 选择类型
      ('choice
       (let ((choices (plist-get field-def :choices)))
         (unless choices
           (user-error "No choices defined for choice field"))
         (completing-read "Choose value: " choices nil t current-value)))
      
      ;; 复选框类型
      ('checkbox
       (if (y-or-n-p "Check this box? ")
           t
         nil))
      
      ;; 评分类型
      ('rating
       (let ((rating (read-number "Rating (1-5): " (or current-value 3))))
         (if (funcall validator rating)
             rating
           (user-error "Invalid rating. Please enter a number between 1 and 5"))))
      
      ;; 进度类型
      ('progress
       (let ((progress (read-number "Progress (0-100): " (or current-value 0))))
         (if (funcall validator progress)
             progress
           (user-error "Invalid progress. Please enter a number between 0 and 100"))))
      
      ;; 文件类型
      ('file
       (let ((file (read-file-name "Choose file: " nil current-value t)))
         (if (funcall validator file)
             file
           (user-error "Invalid file path"))))
      
      ;; 目录类型
      ('directory
       (let ((dir (read-directory-name "Choose directory: " nil current-value t)))
         (if (funcall validator dir)
             dir
           (user-error "Invalid directory path"))))
      
      ;; 颜色类型
      ('color
       (let ((color (read-string "Color (#RRGGBB): " (or current-value "#000000"))))
         (if (funcall validator color)
             color
           (user-error "Invalid color format. Please use #RRGGBB"))))
      
      ;; 人员类型
      ('person
       (let ((person (read-string "Person name (FirstName LastName): " current-value)))
         (if (funcall validator person)
             person
           (user-error "Invalid person name format. Use 'FirstName LastName'"))))
      
      ;; 电话号码类型
      ('tel
       (let ((tel (read-string "Telephone: " current-value)))
         (if (funcall validator tel)
             tel
           (user-error "Invalid telephone number"))))
      
      ;; 邮箱类型
      ('email
       (let ((email (read-string "Email: " current-value)))
         (if (funcall validator email)
             email
           (user-error "Invalid email address"))))
      
      ;; URL类型
      ('url
       (let ((url (read-string "URL: " current-value)))
         (if (funcall validator url)
             url
           (user-error "Invalid URL"))))
      
      ;; 引用类型
      ('reference
       (let* ((ref-tag (plist-get field-def :ref-tag))
              (candidates (org-supertag-get-entries-by-tag ref-tag)))
         (unless ref-tag
           (user-error "No reference tag specified"))
         (completing-read "Select reference: " candidates nil t current-value)))
      
      ;; Org链接类型
      ('org-link
       (let ((link (read-string "Org link: " current-value)))
         (if (funcall validator link)
             link
           (user-error "Invalid Org link format"))))
      
      ;; 密码类型
      ('password
       (let ((pass (read-passwd "Password: " t current-value)))
         (if (funcall validator pass)
             pass
           (user-error "Password must be at least 8 characters"))))
      
      ;; 位置类型
      ('location
       (let* ((lat (read-number "Latitude (-90 to 90): " 
                               (or (car (and current-value (org-supertag-parse-location current-value))) 0)))
              (lon (read-number "Longitude (-180 to 180): "
                              (or (cdr (and current-value (org-supertag-parse-location current-value))) 0))))
         (format "%f,%f" lat lon)))
      
      ;; 数值类型
      ('number
       (propertize (number-to-string (read-number "Number: " (or (and current-value (string-to-number current-value)) 0)))
                 'face 'org-table))
      
      ;; 整数类型
      ('integer
       (propertize (number-to-string (read-number "Integer: " (or (and current-value (string-to-number current-value)) 0)))
                 'face 'org-table))
      
      ;; 默认情况
      (_ (read-string (format "%s: " (or (plist-get field-def :name) type))
                     (or current-value "")))))))

(defun org-supertag-ui--format-field-value-for-display (field-def)
  "格式化字段值用于显示.
FIELD-DEF 是字段定义"
  (let* ((type (plist-get field-def :type))
         (value (plist-get field-def :value))
         (type-spec (org-supertag-get-field-type type)))
    (cond
     ;; 空值处理
     ((null value)
      (propertize "<empty>" 'face 'shadow))
     
     ;; 日期类型
     ((eq type 'date)
      (propertize value 'face 'org-date))
     
     ;; 时间类型
     ((eq type 'time)
      (propertize value 'face 'org-time))
     
     ;; 日期时间类型
     ((eq type 'datetime)
      (propertize value 'face 'org-date))
     
     ;; 持续时间类型
     ((eq type 'duration)
      (propertize value 'face 'org-special-keyword))
     
     ;; 列表类型
     ((eq type 'list)
      (if (listp value)
          (concat "["
                  (propertize (string-join value ", ") 'face 'org-list)
                  "]")
        value))
     
     ;; 选项和选择类型
     ((or (eq type 'options) (eq type 'choice))
      (propertize value 'face 'org-tag))
     
     ;; 复选框类型
     ((eq type 'checkbox)
      (propertize (if value "[X]" "[ ]")
                 'face (if value 'success 'shadow)))
     
     ;; 评分类型
     ((eq type 'rating)
      (let ((rating (if (numberp value) value (string-to-number value))))
        (concat (propertize (make-string rating ?★) 'face 'org-priority)
                (propertize (make-string (- 5 rating) ?☆) 'face 'shadow))))
     
     ;; 进度类型
     ((eq type 'progress)
      (let* ((progress (if (numberp value) value (string-to-number value)))
             (width 10)
             (filled (round (* width (/ progress 100.0))))
             (empty (- width filled)))
        (concat "["
                (propertize (make-string filled ?=) 'face 'success)
                (propertize (make-string empty ?-) 'face 'shadow)
                "]"
                (propertize (format " %d%%" progress)
                           'face 'org-special-keyword))))
     
     ;; 文件类型
     ((eq type 'file)
      (let ((filename (file-name-nondirectory value)))
        (propertize filename 'face 'org-link
                    'help-echo value)))
     
     ;; 目录类型
     ((eq type 'directory)
      (propertize (abbreviate-file-name value)
                 'face 'org-link
                 'help-echo value))
     
     ;; 颜色类型
     ((eq type 'color)
      (propertize (concat "■ " value)
                 'face `(:foreground ,value)))
     
     ;; 人员类型
     ((eq type 'person)
      (propertize value 'face 'org-agenda-done))
     
     ;; 电话号码类型
     ((eq type 'tel)
      (propertize value 'face 'org-special-keyword))
     
     ;; 邮箱类型
     ((eq type 'email)
      (propertize value 'face 'org-link))
     
     ;; URL类型
     ((eq type 'url)
      (propertize value 'face 'org-link))
     
     ;; 引用类型
     ((eq type 'reference)
      (propertize value 'face 'org-ref))
     
     ;; Org链接类型
     ((eq type 'org-link)
      (propertize value 'face 'org-link))
     
     ;; 密码类型
     ((eq type 'password)
      (propertize (make-string (length value) ?•) 'face 'shadow))
     
     ;; 位置类型
     ((eq type 'location)
      (if (string-match "^\\([0-9.-]+\\),\\([0-9.-]+\\)$" value)
          (format "📍 %s" value)
        value))
     
     ;; 数值类型
     ((eq type 'number)
      (propertize (number-to-string value) 'face 'org-table))
     
     ;; 整数类型
     ((eq type 'integer)
      (propertize (number-to-string value) 'face 'org-table))
     
     ;; 默认情况
     (t (or value "")))))

(defun org-supertag-ui--insert-field (field-def)
  "在当前位置插入字段.
FIELD-DEF 是字段定义"
  (let* ((name (plist-get field-def :name))
         (type (plist-get field-def :type))
         (description (plist-get field-def :description))
         (required (plist-get field-def :required))
         (formatted-value (org-supertag-ui--format-field-value-for-display field-def)))
    ;; 插入字段名和类型
    (insert (format "%-20s "
                   (concat (propertize name 'face 'font-lock-variable-name-face)
                          (if required
                              (propertize "*" 'face 'error)
                            "")))
    (insert (format "%-12s "
                   (propertize (symbol-name type)
                             'face 'font-lock-type-face)))
    
    ;; 插入值
    (insert formatted-value)
    (insert "\n")
    
    ;; 如果有描述，在下一行显示
    (when description
      (insert (propertize (format "%20s %s\n" "" description)
                         'face 'font-lock-comment-face))))))

;; ----------------------------------------------------------------------
;; 模板操作命令
;; ----------------------------------------------------------------------

(defun org-supertag-template-add-field ()
  "添加新字段到当前模板."
  (interactive)
  (let* ((name (read-string "Field name: "))
         (type-candidates (org-supertag--field-type-candidates))
         (type-choice (completing-read 
                      "Field type: "
                      (mapcar #'car type-candidates)
                      nil t))
         (type (cdr (assoc type-choice type-candidates)))
         (required (y-or-n-p "Required? "))
         (default (read-string "Default value (optional): "))
         (field (list :name name
                     :type type
                     :required required
                     :default default)))
    (setf (org-supertag-template-fields org-supertag--current-template)
          (vconcat (org-supertag-template-fields org-supertag--current-template)
                   (vector field)))
    (org-supertag-template-refresh)))

(defun org-supertag-template-edit-field (index)
  "编辑指定索引的字段.
INDEX 是字段的索引"
  (interactive "nField index: ")
  (let* ((fields (org-supertag-template-fields org-supertag--current-template))
         (field (aref fields index))
         (name (read-string "Field name: " (plist-get field :name)))
         (type-candidates (org-supertag--field-type-candidates))
         (current-type (symbol-name (plist-get field :type)))
         (type-choice (completing-read 
                      "Field type: "
                      (mapcar #'car type-candidates)
                      nil t
                      (concat current-type " - ")))
         (type (cdr (assoc type-choice type-candidates)))
         (required (y-or-n-p "Required? "))
         (default (read-string "Default value: " (plist-get field :default)))
         (new-field (list :name name
                         :type type
                         :required required
                         :default default))
         (new-fields (vconcat (seq-subseq fields 0 index)
                             (vector new-field)
                             (seq-subseq fields (1+ index)))))
    (setf (org-supertag-template-fields org-supertag--current-template)
          new-fields)
    (org-supertag-template-refresh)))

;; ----------------------------------------------------------------------
;; 快捷键绑定
;; ----------------------------------------------------------------------

(define-key org-supertag-template-mode-map (kbd "?") 'org-supertag-template-edit-dispatch)
(define-key org-supertag-template-mode-map (kbd "h") 'org-supertag-template-edit-dispatch)

;; ----------------------------------------------------------------------
;; 全局快捷键绑定
;; ----------------------------------------------------------------------

(defcustom org-supertag-template-keymap-prefix "C-c C-x t"
  "The prefix for org-supertag-template keymap."
  :type 'string
  :group 'org-supertag)

(defvar org-supertag-template-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") 'org-supertag-template-dispatch)
    (define-key map (kbd "n") 'org-supertag-tag-template-new)
    (define-key map (kbd "e") 'org-supertag-edit-tag-template)
    (define-key map (kbd "l") 'org-supertag-list-tag-templates)
    map)
  "Keymap for org-supertag-template commands.")

(when org-supertag-template-keymap-prefix
  (global-set-key (kbd org-supertag-template-keymap-prefix)
                 org-supertag-template-command-map))


;;; 字段解析和验证

(defun org-supertag-ui--parse-field-spec (spec)
  "解析字段规范字符串.
SPEC 格式: name:type[*][=default]
返回解析后的字段定义 plist"
  (if (string-match "^\\([^:]+\\):\\([^=*]+\\)\\(\\*\\)?\\(?:=\\(.+\\)\\)?$" spec)
      (let* ((name (match-string 1 spec))
             (type-str (match-string 2 spec))
             (required (match-string 3 spec))
             (default (match-string 4 spec))
             (type (intern type-str)))
        `(:name ,name
          :type ,type
          :required ,(not (null required))
          ,@(when default
              (list :default default))))
    (user-error "Invalid field spec: %s" spec)))

(defun org-supertag-ui--validate-field-spec (field-def)
  "验证字段定义是否有效.
FIELD-DEF 是字段定义 plist"
  (let* ((type (plist-get field-def :type))
         (type-spec (org-supertag-get-field-type type)))
    (unless type-spec
      (user-error "Unknown field type: %s" type))
    ;; 验证默认值
    (when-let ((default (plist-get field-def :default)))
      (let ((validation (org-supertag-tag-validate-field field-def default)))
        (unless (car validation)
          (user-error "Invalid default value: %s" (cdr validation)))))
    t))

(defun org-supertag-ui--infer-field-specs (template-name)
  "从模板名称推断可能的字段规范.
使用 org-supertag-tag 中的推断逻辑"
  (let ((fields (org-supertag--infer-fields-from-tag template-name)))
    (mapcar #'org-supertag-ui--parse-field-spec fields)))

(defun org-supertag-ui--field-to-spec (field-def)
  "将字段定义转换为规范字符串.
FIELD-DEF 是字段定义 plist"
  (concat (plist-get field-def :name)
          ":"
          (symbol-name (plist-get field-def :type))
          (when (plist-get field-def :required) "*")
          (when-let ((default (plist-get field-def :default)))
            (concat "=" default))))

;;; 字段编辑历史

(defvar-local org-supertag-ui--field-history (make-hash-table :test 'equal)
  "字段编辑历史记录.
键是字段类型，值是历史值列表.")

(defun org-supertag-ui--add-to-history (type value)
  "添加值到字段类型的历史记录中.
TYPE 是字段类型
VALUE 是字段值"
  (let ((history (gethash type org-supertag-ui--field-history)))
    (unless (member value history)
      (puthash type
               (cons value (delete value history))
               org-supertag-ui--field-history))))

(defun org-supertag-ui--get-history (type)
  "获取字段类型的历史记录.
TYPE 是字段类型"
  (gethash type org-supertag-ui--field-history nil))

;;; 字段编辑状态

(defvar-local org-supertag-ui--last-field nil
  "最后编辑的字段信息.")

(defun org-supertag-ui--remember-field (field-def)
  "记住字段编辑状态.
FIELD-DEF 是字段定义"
  (setq org-supertag-ui--last-field field-def))

(defun org-supertag-ui--get-last-field ()
  "获取最后编辑的字段信息."
  org-supertag-ui--last-field)

;;; 字段操作辅助函数

(defun org-supertag-ui--get-current-field ()
  "获取当前光标所在的字段定义."
  (when-let* ((section (magit-current-section))
              (field-index (oref section value))
              (fields (plist-get org-supertag--current-template :fields)))
    (aref fields field-index)))

(defun org-supertag-ui--field-at-point ()
  "获取光标处的字段信息."
  (when-let* ((section (magit-current-section))
              (field-index (oref section value))
              (fields (plist-get org-supertag--current-template :fields)))
    (cons field-index (aref fields field-index))))

;;; 快速编辑功能

(defun org-supertag-template-quick-add-field ()
  "快速添加字段，使用简化语法.
示例：'title:string*' 表示必填的字符串字段
'rating:number=3' 表示默认值为3的数字字段"
  (interactive)
  (let* ((spec (read-string "Quick add field (name:type[*][=default]): "
                           nil 'org-supertag-field-history))
         (field-def (org-supertag-ui--parse-field-spec spec)))
    (when (org-supertag-ui--validate-field-spec field-def)
      (org-supertag-template--add-field field-def)
      (org-supertag-template-refresh))))

(defun org-supertag-template-quick-edit-field ()
  "快速编辑字段的单个属性."
  (interactive)
  (when-let* ((field-info (org-supertag-ui--field-at-point))
              (field-index (car field-info))
              (field-def (cdr field-info)))
    (let* ((attr (completing-read "Edit attribute: "
                               '("name" "type" "required" "default" "description")
                               nil t))
           (current-value (plist-get field-def (intern (concat ":" attr))))
           (new-value
            (pcase attr
              ("name" (read-string "New name: " (or current-value "")))
              ("type" (completing-read "New type: "
                                     (mapcar #'car org-supertag-field-types)
                                     nil t
                                     (and current-value (symbol-name current-value))))
              ("required" (y-or-n-p "Required? "))
              ("default" (read-string "New default: "
                                    (or current-value "")))
              ("description" (read-string "New description: "
                                        (or current-value "")))))
           (new-field-def (plist-put (copy-sequence field-def)
                                   (intern (concat ":" attr))
                                   (if (equal attr "type")
                                       (intern new-value)
                                     new-value))))
      (when (org-supertag-ui--validate-field-spec new-field-def)
        (org-supertag-template--modify-field field-index new-field-def)
        (org-supertag-template-refresh)))))

(defun org-supertag-template-move-field-to ()
  "移动字段到指定位置."
  (interactive)
  (when-let* ((field-info (org-supertag-ui--field-at-point))
              (field-index (car field-info))
              (fields (plist-get org-supertag--current-template :fields))
              (max-pos (1- (length fields)))
              (new-pos (read-number (format "Move to position (0-%d): " max-pos))))
    (when (and (>= new-pos 0) (<= new-pos max-pos))
      (org-supertag-template--move-field field-index new-pos)
      (org-supertag-template-refresh))))

(defun org-supertag-template-bulk-add-fields ()
  "批量添加多个字段.
使用简化语法，每行一个字段定义."
  (interactive)
  (let* ((input (read-string "Add fields (one per line):\n")
         (field-specs (split-string input "\n" t "[ \t\n\r]+")))
    (dolist (spec field-specs)
      (condition-case err
          (let ((field-def (org-supertag-ui--parse-field-spec spec)))
            (when (org-supertag-ui--validate-field-spec field-def)
              (org-supertag-template--add-field field-def)))
        (error
         (message "Error adding field '%s': %s" spec (error-message-string err)))))
    (org-supertag-template-refresh)))

(defun org-supertag-template-infer-fields ()
  "从模板名称推断并添加字段."
  (interactive)
  (when-let* ((template-name (plist-get org-supertag--current-template :name))
              (fields (org-supertag-ui--infer-field-specs template-name)))
    (when (yes-or-no-p (format "Add %d inferred fields? " (length fields)))
      (dolist (field-def fields)
        (org-supertag-template--add-field field-def))
      (org-supertag-template-refresh))))

(defun org-supertag-template-copy-field ()
  "复制当前字段到剪贴板."
  (interactive)
  (when-let* ((field-def (org-supertag-ui--get-current-field)))
    (let ((spec (org-supertag-ui--field-to-spec field-def)))
      (kill-new spec)
      (message "Copied field spec: %s" spec))))

(defun org-supertag-template-paste-field ()
  "从剪贴板粘贴字段."
  (interactive)
  (when-let* ((spec (current-kill 0 t)))
    (condition-case err
        (let ((field-def (org-supertag-ui--parse-field-spec spec)))
          (when (org-supertag-ui--validate-field-spec field-def)
            (org-supertag-template--add-field field-def)
            (org-supertag-template-refresh)))
      (error
       (message "Invalid field spec in clipboard: %s" (error-message-string err))))))

;;; 键绑定

(defvar org-supertag-template-mode-map
  (let ((map (make-sparse-keymap)))
    ;; 字段操作
    (define-key map (kbd "a") 'org-supertag-template-quick-add-field)
    (define-key map (kbd "e") 'org-supertag-template-quick-edit-field)
    (define-key map (kbd "m") 'org-supertag-template-move-field-to)
    (define-key map (kbd "b") 'org-supertag-template-bulk-add-fields)
    (define-key map (kbd "i") 'org-supertag-template-infer-fields)
    (define-key map (kbd "w") 'org-supertag-template-copy-field)
    (define-key map (kbd "y") 'org-supertag-template-paste-field)
    ;; 导航
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "g") 'org-supertag-template-refresh)
    ;; 其他
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `org-supertag-template-mode'.")

(provide 'org-supertag-ui)
;;; org-supertag-ui.el ends here 

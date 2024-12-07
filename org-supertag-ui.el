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

;; 辅助函数：获取section可见性状态
(defun magit-section-get-visibility-cache ()
  "Return an alist of section visibility status."
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
                         (mapcar #'car (org-supertag-db-find)
                                      (lambda (_id props)
                                        (eq (plist-get props :type) :template)))
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

(cl-defstruct (org-supertag-template-change (:constructor org-supertag-template-change-create))
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
  (let* ((templates (org-supertag-db-find-entities :template))
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

(defun org-supertag-template-new-cancel ()
  "Cancel template creation."
  (interactive)
  (when (yes-or-no-p "Cancel template creation? ")
    (quit-window)))

(defun org-supertag-template-new-collect-values ()
  "Collect values from the new template form."
  (let (values)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((overlay (car (overlays-at (point)))))
                   (field (overlay-get overlay 'template-field))
                   (end (overlay-end overlay))
                   (value (buffer-substring-no-properties (point) end))
          (push (cons field (string-trim value)) values))
        (forward-line)))
    (nreverse values)))

;; 定义模板操作的快捷键
(transient-define-prefix org-supertag-template-dispatch ()
  "Dispatch template operations."
  ["Template Operations"
   ["Create/Edit"
    ("n" "New template" org-supertag-tag-template-new)
    ("e" "Edit template" org-supertag-edit-tag-template)
    ("c" "Copy template" org-supertag-template-copy-command)]
   ["Manage"
    ("d" "Delete template" org-supertag-delete-template)
    ("r" "Rename template" org-supertag-template-rename-command)
    ("l" "List templates" org-supertag-list-tag-templates)]])

(transient-define-prefix org-supertag-template-edit-dispatch ()
  "Dispatch template editing operations."
  ["Template Edit"
   ["Basic"
    ("s" "Save" org-supertag-template-save)
    ("q" "Quit" org-supertag-template-quit)
    ("u" "Undo" org-supertag-template-undo)
    ("r" "Redo" org-supertag-template-redo)]
   ["Fields"
    ("f a" "Add field" org-supertag-template-add-field-command)
    ("f e" "Edit field" org-supertag-template-edit-field-command)
    ("f d" "Delete field" org-supertag-template-remove-field-command)
    ("f m" "Move field" org-supertag-template-move-field-dispatch)]])


(transient-define-prefix org-supertag-template-move-field-dispatch ()
  "Dispatch field movement operations."
  ["Move Field"
   [("u" "Move up" org-supertag-template-move-field-up)
    ("d" "Move down" org-supertag-template-move-field-down)]])

;; 修改现有的模板编辑模式按键绑定
(define-key org-supertag-template-mode-map (kbd "?") 'org-supertag-template-edit-dispatch)
(define-key org-supertag-template-mode-map (kbd "h") 'org-supertag-template-edit-dispatch)

;; 添加全局快捷键建议
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

(provide 'org-supertag-ui)
;;; org-supertag-ui.el ends here 

# org-supertag 表格视图编辑功能实现方案

## 工作计划表

| ID | 任务 | 状态 | 优先级 | 依赖 |
|----|------|------|--------|------|
| 1 | 修改 `org-supertag-view--show-content-table` 实现全屏显示 | 已完成 | 高 | - |
| 2 | 修改 `org-supertag-view--insert-content-table` 使用 org 表格语法并移除 Type 列 | 已完成 | 高 | - |
| 3 | 创建 `org-supertag-view-smart-edit` 函数 | 已完成 | 高 | - |
| 4 | 创建 `org-supertag-view-edit-table` 函数 | 已完成 | 高 | - |
| 5 | 创建 `org-supertag-view-edit-date-field` 函数 | 已完成 | 中 | 4 |
| 6 | 创建 `org-supertag-view-edit-options-field` 函数 | 已完成 | 中 | 4 |
| 7 | 创建 `org-supertag-view-edit-reference-field` 函数 | 已完成 | 中 | 4 |
| 8 | 创建 `org-supertag-view-edit-list-field` 函数 | 已完成 | 中 | 4 |
| 9 | 创建 `org-supertag-view-edit-range-field` 函数 | 已完成 | 中 | 4 |
| 10 | 创建 `org-supertag-view-edit-timestamp-field` 函数 | 已完成 | 中 | 4 |
| 11 | 创建 `org-supertag-view-validate-field` 函数 | 已完成 | 中 | - |
| 12 | 修改 `org-supertag-view-save-table` 函数 | 已完成 | 高 | 2 |
| 13 | 更新 `org-supertag-view-table-mode-map` 添加编辑按键绑定 | 未开始 | 高 | 3-10 |

## 背景与目标

在 org-supertag 的表格视图中实现直接编辑字段值的功能，提供更便捷的数据管理体验。当前表格视图仅支持查看功能，需要扩展以支持编辑操作。

## 实现方案

我们选择**基于 org-mode 表格**的实现方案，这种方案可以最大限度利用 org-mode 的现有功能，同时保持实现的简洁性。

### 核心思路

1. 将现有的格式化表格替换为真正的 org-mode 表格（使用 `|` 分隔符）
2. 提供编辑模式切换功能，允许用户进入/退出编辑模式
3. 实现保存功能，将编辑后的表格数据回写到数据库
4. 为不同字段类型提供适当的验证和格式化
5. 实现智能字段识别，通过单键触发合适的编辑功能
6. 使用全屏显示表格视图，提供更好的编辑体验

### 详细设计

#### 1. 修改表格生成函数

当前的 `org-supertag-view--insert-content-table` 函数使用格式化字符串和固定宽度列来创建表格。需要修改为使用 org 表格语法，并且使用全屏显示而非侧边窗口：

```elisp
(defun org-supertag-view--insert-content-table (tag)
  "Insert content related to TAG in current buffer using org table format."
  (insert "* Related Nodes\n\n")
  (let* ((content (org-supertag-view--get-related-nodes tag))
         (tag-def (org-supertag-tag-get tag))
         (fields (plist-get tag-def :fields)))
    (if content
        (progn
          ;; 表头
          (insert "|Node|Type|Date|")
          (dolist (field fields)
            (insert (format "%s|" (plist-get field :name))))
          (insert "\n")
          
          ;; 分隔线
          (insert "|-|-|-|")
          (dolist (field fields)
            (insert "-|"))
          (insert "\n")
          
          ;; 数据行
          (dolist (item content)
            (let ((field-values (plist-get item :fields)))
              (insert (format "|%s|%s|%s|"
                            (plist-get item :node)
                            (or (plist-get item :type) "")
                            (plist-get item :date)))
              ;; 字段值
              (cl-loop for field in fields
                       do (let* ((field-name (plist-get field :name))
                                (value (cdr (assoc field-name field-values))))
                            (insert (format "%s|" (or value "")))))
              (insert "\n"))))
      (insert (format "No content found for tag #%s" tag)))))

(defun org-supertag-view--show-content-table (tag)
  "Show content table for TAG in a dedicated full-screen buffer."
  (let ((buffer (get-buffer-create (format "*Org SuperTag Table View: %s*" tag))))
    (with-current-buffer buffer
      ;; 设置主模式
      (org-mode)  ;; 先启用 org-mode
      (org-supertag-view-table-mode)  ;; 然后启用自定义模式
      (setq-local org-supertag-view-current-tag tag)

      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; 插入简洁的标题和操作说明 (使用 org 标题格式)
        (insert (format "* Tag: #%s\n\n" tag))
        (insert "** Operations:\n")
        (insert " [q] - Quit    [g] - Refresh    [v] - View Node    [m] - Manage Relations\n")
        (insert " [e] - Edit Field    [2] - Switch to Discover View    [3] - Switch to Columns View\n\n")
        ;; 插入表格内容
        (org-supertag-view--insert-content-table tag))

      (setq buffer-read-only t)
      (goto-char (point-min)))
    
    ;; 使用全屏显示，而非侧边窗口
    (switch-to-buffer buffer)
    (delete-other-windows)))
```

#### 2. 智能编辑功能

实现一个智能编辑功能，根据当前光标所在单元格的字段类型自动选择合适的编辑方式：

```elisp
(defun org-supertag-view-smart-edit ()
  "智能编辑功能 - 根据当前字段类型选择合适的编辑方式。
在表格视图中，按下'e'键将自动：
1. 如果在标准字段上，进入普通编辑模式
2. 如果在日期字段上，调用日期选择器
3. 如果在选项字段上，提供选项列表
4. 如果在引用字段上，提供节点选择器"
  (interactive)
  (if (not (org-at-table-p))
      ;; 不在表格上，切换整体编辑模式
      (org-supertag-view-edit-table)
    ;; 在表格上，检查字段类型
    (let* ((col (org-table-current-column))
           (tag org-supertag-view-current-tag)
           (tag-def (org-supertag-tag-get tag))
           (fields (plist-get tag-def :fields)))
      (if (< col 4)
          ;; 前三列是固定列 (Node/Type/Date)
          (if (= col 3) ; Date列
              (org-supertag-view-edit-date-field)
            ;; 其他固定列使用普通编辑
            (org-supertag-view-edit-table))
        ;; 位于字段列，检查字段类型
        (let* ((field-idx (- col 4))
               (field (when (and (>= field-idx 0) (< field-idx (length fields)))
                        (nth field-idx fields)))
               (field-type (when field (plist-get field :type))))
          (cond
           ((eq field-type 'date)
            (org-supertag-view-edit-date-field))
           ((eq field-type 'timestamp)
            (org-supertag-view-edit-timestamp-field))
           ((eq field-type 'options)
            (org-supertag-view-edit-options-field))
           ((eq field-type 'tag-reference)
            (org-supertag-view-edit-reference-field))
           ((eq field-type 'list)
            (org-supertag-view-edit-list-field))
           ((eq field-type 'range)
            (org-supertag-view-edit-range-field))
           (t
            ;; 对于普通字段类型或未知类型，使用普通编辑
            (when buffer-read-only
              (org-supertag-view-edit-table)))))))))
```

#### 3. 特殊字段类型辅助编辑函数

为各种字段类型提供专门的编辑功能：

```elisp
(defun org-supertag-view-edit-date-field ()
  "Edit date field at current cell using org-read-date."
  (interactive)
  (when (org-at-table-p)
    (let* ((col (org-table-current-column))
           (row-data (org-table-get-line))
           (tag org-supertag-view-current-tag)
           (tag-def (org-supertag-tag-get tag))
           (fields (plist-get tag-def :fields))
           (field-idx (- col 4))  ; 调整列索引以匹配字段索引
           (field (when (and (>= field-idx 0) (< field-idx (length fields)))
                    (nth field-idx fields))))
      (when (or (= col 3) ; Date列
                (and field (eq (plist-get field :type) 'date)))
        ;; 如果当前是只读模式，先切换到编辑模式
        (when buffer-read-only
          (org-supertag-view-edit-table))
        (let* ((current-val (org-table-get nil col))
               (date (org-read-date nil t nil "Enter date" 
                                   (when (and current-val (not (string-empty-p current-val)))
                                     current-val)))
               (formatted-date (format-time-string "<%Y-%m-%d %a>" date)))
          (org-table-put nil col formatted-date)
          (message "Date field updated: %s" formatted-date))))))

;; 其他特殊字段类型的编辑函数也需要类似的调整...
```

#### 4. 更新保存函数

修改 `org-supertag-view-save-table` 函数，调整索引以适应新的表格结构：

```elisp
(defun org-supertag-view-save-table ()
  "Save all changes in the table with validation."
  (interactive)
  (let* ((tag org-supertag-view-current-tag)
         (tag-def (org-supertag-tag-get tag))
         (fields (plist-get tag-def :fields))
         (errors nil))
    
    ;; 保存逻辑需要调整列索引计算，这里省略完整代码...
    
    ;; 示例：字段值读取时的索引调整
    (cl-loop for field in fields
             for i from 3  ; 之前是从第4列开始，现在是从第3列开始
             do (let* ((field-name (plist-get field :name))
                      (value (string-trim (or (nth i row-data) ""))))
                  ;; 处理字段值...
                  ))
    
    (message "Changes saved for tag #%s" tag)))
```

## 用户体验优化

为了提高操作效率：

1. **全屏显示**：表格视图使用全屏显示，提供更舒适的编辑体验和更宽敞的视觉空间
2. **单键智能编辑**：按 `e` 键智能识别当前字段类型并启动相应的编辑功能
3. **上下文感知**：自动检测当前单元格的字段类型，无需用户指定
4. **自动模式切换**：从只读模式自动切换到编辑模式
5. **视觉反馈**：通过模式行和标题行的状态指示器，清晰显示当前模式
6. **保留专用快捷键**：保留 `d`/`o`/`r` 等键作为备用，便于习惯这些键的用户

## 字段类型处理

对于不同的字段类型，系统会自动识别并提供适当的编辑方式：

1. **String 类型**：基本的文本处理，按 `e` 进入普通编辑
2. **Number 类型**：按 `e` 进入普通编辑，保存时验证数字格式
3. **Date 类型**：按 `e` 自动启动日期选择器
4. **Options 类型**：按 `e` 自动显示选项列表
5. **Tag-reference 类型**：按 `e` 自动启动节点选择功能
6. **List 类型**：按 `e` 启动列表编辑功能
7. **Range 类型**：按 `e` 启动范围编辑功能
8. **Timestamp 类型**：按 `e` 启动时间戳编辑功能

## 实现步骤

1. 修改 `org-supertag-view--insert-content-table` 函数，使用 org 表格语法
2. 更新 `org-supertag-view--show-content-table` 函数，使用全屏显示而非侧边窗口
3. 实现 `org-supertag-view-smart-edit` 函数，提供智能字段识别和编辑功能
4. 修改现有的特殊字段编辑函数，使其能处理固定列
5. 更新 `org-supertag-view-save-table` 函数，确保验证和保存逻辑正确
6. 添加 `org-supertag-view-cancel-edit` 函数，处理取消编辑的情况
7. 更新键映射，以 `e` 键为主要编辑入口
8. 添加编辑模式下的用户提示信息

## 维护表格结构完整性

表格结构（列数、行数）应该是固定的，因为每列对应特定的字段，每行对应特定的节点。为了确保表格结构不被破坏：

1. **禁用结构修改操作**：禁用添加/删除行列的快捷键和命令
2. **仅允许内容编辑**：用户只能修改单元格内容，不能改变表格结构
3. **编辑辅助功能**：为复杂字段类型提供专用的辅助编辑功能
4. **状态指示器**：在模式行和标题行显示当前的编辑状态和可用操作

## 优点

1. **全屏操作**：使用全屏显示表格，提供更舒适的编辑体验
2. **极简操作**：只需一个 `e` 键即可进行大多数编辑操作
3. **智能识别**：自动识别字段类型，减少用户决策负担
4. **一致体验**：统一的编辑入口，用户不需要记忆多个快捷键
5. **利用现有功能**：充分利用 org-mode 的表格编辑功能
7. **实现简单**：相比其他方案，代码改动较小
8. **安全编辑**：通过限制编辑操作保护表格结构
9. **向后兼容**：保留原有的专用快捷键，不影响已熟悉的用户习惯

## 潜在问题与解决方案

1. **字段类型识别**：
   - 问题：在某些情况下可能无法正确识别字段类型
   - 解决：保留专用快捷键作为备用，确保用户可以直接选择特定编辑方式

2. **字段验证**：
   - 问题：表格编辑模式难以实现实时验证
   - 解决：在保存时进行验证，出错时提供明确反馈

3. **长文本编辑**：
   - 问题：表格单元格不适合编辑长文本
   - 解决：对于长文本，在表格里截断显示，检测字段长度并提供扩展编辑器（单独在 minibuffer 里列出）

4. **用户错误操作**：
   - 问题：用户可能误操作破坏表格结构
   - 解决：禁用可能破坏表格结构的键绑定，提供明确的用户界面提示

## 后续扩展可能性

1. 添加表格排序功能
2. 为更多特殊字段类型提供定制化编辑器
3. 实现表格过滤功能
4. 支持批量编辑操作
5. 添加字段说明和提示信息
6. 实现表格单元格级别的高亮和格式化
7. 添加字段类型提示图标，使用不同的视觉指示区分字段类型


1. **sim-tag.py 新增功能**：
   - `init_tag_library`：从 org-supertag 导入初始标签库
   - `update_tag_library`：增量更新标签
   - `remove_from_library`：删除标签
   - 持久化存储：
     - 标签->向量的映射
     - 向量数据库状态
     - 最后更新时间戳

2. **org-supertag 端的改动**：
   ```lisp
   ;; 1. 初始化同步
   (defun org-supertag-sim-init ()
     ;; 收集所有标签
     ;; 调用 sim-tag.py 初始化库)

   ;; 2. 监听变更
   (org-supertag-db-on 'entity:created ...)
   (org-supertag-db-on 'entity:removed ...)
   ```

3. **工作流程**：
   ```
   启动时：
   org-supertag ---(导出标签列表)---> sim-tag.py [init_tag_library]
   
   添加标签时：
   org-supertag ---(新标签)---> sim-tag.py [update_tag_library]
   
   删除标签时：
   org-supertag ---(标签ID)---> sim-tag.py [remove_from_library]
   ```

4. **数据一致性**：
   - 定期全量同步检查
   - 记录同步状态和时间戳
   - 错误恢复机制

您觉得这个实现计划如何？我们可以开始逐步实现这些功能。

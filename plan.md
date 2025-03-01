# org-supertag-relation 实现计划

让我们基于 @org-supertag-relation.el 添加，标签关系管理的功能：

1. 标签关系应当基于 @org-supertag-db.el 中的 (defconst org-supertag-db-link-type (defconst org-supertag-db-link-structure 创建
1.1 用户可以为标签之间设定关系，关系可以有多种类型，例如：
- 辩证关系：正题、反题和合题
- 因果关系：原因和结果
- 时间关系：之前、之后和期间
- 空间关系：包含、属于和邻接

2. 标签关系的管理在可视化面板里管理。需要改造当前的  org-supertag-relation-show-related 和 org-supertag-relation-analyze-network。
2.1 org-supertag-relation-show-related 除了显示基于同现的标签之外，还可以显示存在关系的标签
2.2 org-supertag-relation-analyze-network 为它赋予管理标签关系的功能。

3. 多个标签关系可以成立标签组。
3.1 用户在为标签设定关系的时候，可以依据组别选择（或者在关系名旁边，同时可以显示组名）
3.2 用户可以创建新的关系组，也可以修改和删除已有的关系组。



预设关系组：(defcustom org-supertag-relation-group-types
  '((knowledge . ((corresponds . "对应")
                 (relates . "相关")
                 (influences . "影响"))))
  "预定义的标签关系组类型。
每个关系组类型定义了一组特定的角色及其中文描述。")





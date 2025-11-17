# Org-SuperTag 5.0: 纯 Emacs Lisp 知识管理

[English](./README.md) | [中文](./README_CN.md)

## ⚡ 5.0 新变化

- **删除 44% 代码** - 完全移除 Python
- **5 倍性能提升** - 无 EPC 通信开销  
- **一键安装** - 仅需 Emacs

## 🚀 30 秒上手

```emacs-lisp
;; 安装
(straight-use-package '(org-supertag :host github :repo "yibie/org-supertag"))

;; 配置目录
(setq org-supertag-sync-directories '("~/org/"))

;; 初始化（仅一次）
M-x supertag-sync-full-initialize
```

## 🎯 核心概念

传统标签：`#tag`  
SuperTag：`#tag` + 结构化数据

```org
* 项目计划 #project
  - status: planning
  - priority: high
  - due: 2024-12-31
```

## 📋 常用命令

- `M-x supertag-add-tag` - 添加标签
- `M-x supertag-view-node` - 查看节点详情  
- `M-x supertag-search` - 智能搜索
- `M-x supertag-capture` - 快速捕获
- `M-x supertag-view-kanban` - 看板视图

## 🔍 查询示例

```lisp
;; 高优先级项目
(supertag-search '(and (tag "project") 
                       (field "priority" "high")))

;; 未读论文
(supertag-search '(and (tag "paper") 
                       (field "status" "unread")))
```

## 🔄 4.x 迁移

1. `M-x load-file RET supertag-migration.el RET`
2. `M-x supertag-migrate-database-to-new-arch RET`  
3. 重启 Emacs

## ⚙️ 配置

```emacs-lisp
(setq org-supertag-sync-directories '("~/notes/"))
```

---

*为 Emacs 用户设计的 Notion 级知识管理*

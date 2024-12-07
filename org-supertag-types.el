;;; org-supertag-types.el --- Type definitions for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 定义核心实体类型和关系类型
;; 实体类型包括：节点、标签、字段、标签组
;; 关系类型包括：节点-标签、节点-字段、标签-字段、标签-组、标签继承、标签组合、标签引用、标签互斥
;; 关系类型中，冒号前的符号表示关系的起点，冒号后的符号表示关系的终点
;; 通过关系连接实体 - 使用 type、from、to 来表达关系

;;; Code:

(defconst org-supertag-entity-types
  '(:node    ; org 节点
    :tag     ; 标签
    :field   ; 字段
    :group)) ; 标签组

(defconst org-supertag-relation-types
  '(:node-tag     ; 节点-标签关系
    :node-field   ; 节点-字段关系
    :tag-field    ; 标签-字段关系
    :tag-group    ; 标签-组关系
    :tag-extend   ; 标签继承关系
    :tag-contain  ; 标签组合关系
    :tag-ref      ; 标签引用关系
    :tag-excl))   ; 标签互斥关系


(provide 'org-supertag-types)
;;; org-supertag-types.el ends here
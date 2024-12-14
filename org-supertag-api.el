;;; Commentary:
;; 提供高层 API 接口
;; 不直接修改数据库，而是通过 API 访问和操作数据库
;; API 提供数据类型转换
;; 所有函数以 org-supertag-api- 开头
;; 本文件不支持【破坏性更新】，如固有函数不足以支持功能，则实现新函数

;;; Code:
(require 'cl-lib)
(require 'org-supertag-db)
(require 'org-supertag-types)

;;; 提供数据类型转换函数
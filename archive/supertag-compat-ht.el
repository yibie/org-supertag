;;; ht.el --- Minimal `ht` compatibility layer -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a tiny subset of the `ht` API used by Org-Supertag so
;; that the package can run in environments where the external `ht` library
;; is not installed.  When the real library is available, it will be used
;; instead of this shim.

;;; Code:

(unless (require 'ht nil 'noerror)
  ;; Basic constructor
  (defun ht-create (&rest _args)
    "Create a new hash-table with `equal' test.
_ARGS are ignored but kept for compatibility."
    (make-hash-table :test 'equal))

  ;; Accessors
  (defun ht-set (table key value)
    "Set in TABLE the entry KEY to VALUE and return VALUE."
    (puthash key value table)
    value)

  (defun ht-get (table key &optional default)
    "Get from TABLE the value for KEY or DEFAULT."
    (gethash key table default))

  (defun ht-remove (table key)
    "Remove KEY from TABLE and return non-nil if key existed."
    (remhash key table))

  (defun ht-keys (table)
    "Return a list of keys stored in TABLE."
    (let (keys)
      (maphash (lambda (key _value)
                 (push key keys))
               table)
      keys))

  (defun ht-values (table)
    "Return a list of values stored in TABLE."
    (let (values)
      (maphash (lambda (_key value)
                 (push value values))
               table)
      values))

  (provide 'ht))

(provide 'ht)

;;; ht.el ends here

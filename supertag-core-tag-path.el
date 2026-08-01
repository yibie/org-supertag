;;; supertag-core-tag-path.el --- Pure slash-path semantics for tags -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete slash paths are canonical tag IDs.  This module only derives
;; namespace relationships; it never creates parent tags or :extends links.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun supertag-tag-path-valid-p (path)
  "Return non-nil when PATH has no empty slash-delimited segment."
  (and (stringp path)
       (not (string-empty-p path))
       (not (string-prefix-p "/" path))
       (not (string-suffix-p "/" path))
       (not (string-match-p "//" path))))

(defun supertag-tag-path-parent (path)
  "Return PATH's namespace parent, or nil for a root or malformed path."
  (when (supertag-tag-path-valid-p path)
    (when-let* ((slash (string-match "/[^/]+\\'" path)))
      (substring path 0 slash))))

(defun supertag-tag-path-leaf (path)
  "Return PATH's final segment, preserving malformed historical IDs."
  (if-let* ((parent (supertag-tag-path-parent path)))
      (substring path (1+ (length parent)))
    path))

(defun supertag-tag-path-descendant-p (candidate parent)
  "Return non-nil when CANDIDATE is a strict path descendant of PARENT."
  (and (supertag-tag-path-valid-p candidate)
       (supertag-tag-path-valid-p parent)
       (> (length candidate) (length parent))
       (string-prefix-p (concat parent "/") candidate)))

(defun supertag-tag-path-rebase (path old-root new-root)
  "Move PATH from OLD-ROOT to NEW-ROOT while preserving its suffix."
  (unless (and (supertag-tag-path-valid-p old-root)
               (supertag-tag-path-valid-p new-root)
               (or (equal path old-root)
                   (supertag-tag-path-descendant-p path old-root)))
    (error "Cannot rebase tag path '%s' from '%s' to '%s'"
           path old-root new-root))
  (concat new-root (substring path (length old-root))))

(defun supertag-tag-path-namespace-prefixes (paths)
  "Return sorted unique namespace ancestors derived from valid PATHS."
  (let ((seen (make-hash-table :test 'equal))
        prefixes)
    (dolist (path paths)
      (let ((parent (supertag-tag-path-parent path)))
        (while parent
          (unless (gethash parent seen)
            (puthash parent t seen)
            (push parent prefixes))
          (setq parent (supertag-tag-path-parent parent)))))
    (sort prefixes #'string<)))

(defun supertag-tag-path-direct-candidates (paths namespace)
  "Return real tags and namespaces directly below NAMESPACE in PATHS.
NAMESPACE is either the empty string for roots or a slash-terminated
path such as `emacs/package/'.  Returned namespace candidates keep
their trailing slash; real Tag candidates keep their complete IDs.
Malformed historical IDs remain selectable at the root but never
participate in namespace derivation."
  (unless (or (string-empty-p namespace)
              (and (string-suffix-p "/" namespace)
                   (supertag-tag-path-valid-p
                    (string-remove-suffix "/" namespace))))
    (error "Invalid tag namespace '%s'" namespace))
  (let ((seen (make-hash-table :test 'equal))
        candidates)
    (dolist (path paths)
      (let ((candidate
             (cond
              ((and (supertag-tag-path-valid-p path)
                    (string-prefix-p namespace path))
               (let ((remainder (substring path (length namespace))))
                 (unless (string-empty-p remainder)
                   (if-let* ((slash (string-match "/" remainder)))
                       (concat namespace (substring remainder 0 (1+ slash)))
                     path))))
              ((string-empty-p namespace) path))))
        (when (and candidate (not (gethash candidate seen)))
          (puthash candidate t seen)
          (push candidate candidates))))
    (sort candidates #'string<)))

(defun supertag-tag-path-has-descendants-p (path paths)
  "Return non-nil when one of PATHS is below PATH."
  (cl-some (lambda (candidate)
             (supertag-tag-path-descendant-p candidate path))
           paths))

(provide 'supertag-core-tag-path)
;;; supertag-core-tag-path.el ends here

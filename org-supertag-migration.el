;;; org-supertag-migration.el --- One-time migration scripts for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains functions for performing one-time data migrations on the
;; org-supertag database. These are intended to be run manually by the user
;; to upgrade data structures to new formats.

;;; Code:

(require 'org-supertag-db)
(require 'ht)

(defun org-supertag-migration--canonicalize-props (data)
  "Recursively canonicalize any Lisp DATA into a standard plist format.
This function is a copy of the new logic intended for org-supertag-db.el,
placed here to be self-contained for the migration process.
It converts alists to plists and cleans up keys and values."
  (cond
   ;; Atoms are returned as-is.
   ((or (not (consp data)) (keywordp data)) data)

   ;; An alist (e.g., from properties drawer) is converted to a plist.
   ((and (listp data) (consp (car data)) (not (keywordp (caar data))))
    (let (plist)
      (dolist (pair data)
        (let ((key (car pair))
              (val (cdr pair)))
          (push (org-supertag-migration--canonicalize-props val) plist)
          ;; Ensure key is a valid string/symbol before creating keyword
          (let ((key-str (format "%s" key)))
            (unless (string-empty-p key-str)
              (push (intern (format ":%s" key-str)) plist)))))
      (nreverse plist)))

   ;; A regular list (or a plist already) is processed element by element.
   ((listp data)
    (mapcar #'org-supertag-migration--canonicalize-props data))

   ;; A dotted pair or other cons cell.
   (t (cons (org-supertag-migration--canonicalize-props (car data))
            (org-supertag-migration--canonicalize-props (cdr data))))))

(defun org-supertag-migrate-database-to-canonical-format ()
  "Run a one-time migration to convert the entire database to the new canonical plist format.
This process will:
1. Ask for user confirmation.
2. Back up the existing database file.
3. Load the old database.
4. Create a new, clean database in memory by converting every object.
5. Overwrite the old database file with the new, clean data."
  (interactive)
  (when (y-or-n-p "Really migrate database to the new canonical format? This will create a backup.")
    ;; 1. Back up the existing database
    (let ((db-file org-supertag-db-file)
          (backup-file (concat org-supertag-db-file ".bak")))
      (message "Backing up current database to %s" backup-file)
      (copy-file db-file backup-file t))

    ;; 2. Load the old database
    (message "Loading existing database...")
    (org-supertag-db-load)

    ;; 3. Create new, clean hash tables
    (let ((new-objects (ht-create))
          (new-links (ht-create))
          (processed-count 0))

      (message "Starting migration of %d objects and %d links..."
               (hash-table-count org-supertag-db--object)
               (hash-table-count org-supertag-db--link))

      ;; 4. Migrate all objects
      (maphash (lambda (id props)
                 (let ((clean-props (org-supertag-migration--canonicalize-props props)))
                   (ht-set! new-objects id clean-props))
                 (cl-incf processed-count))
               org-supertag-db--object)

      ;; 5. Migrate all links
      (maphash (lambda (id props)
                 (let ((clean-props (org-supertag-migration--canonicalize-props props)))
                   (ht-set! new-links id clean-props))
                 (cl-incf processed-count))
               org-supertag-db--link)

      (message "Migration complete. Processed %d total records." processed-count)

      ;; 6. Overwrite the live database variables with the new, clean data
      (setq org-supertag-db--object new-objects)
      (setq org-supertag-db--link new-links)

      ;; 7. Save the new database to disk
      (message "Saving new, canonicalized database to %s..." org-supertag-db-file)
      (org-supertag-db--mark-dirty) ; Mark as dirty to force save
      (org-supertag-db-save)
      (message "Database migration successful!"))))

(provide 'org-supertag-migration)
;;; org-supertag-migration.el ends here 